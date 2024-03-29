;;; org-an.el --- Org notes synchronizations with Anki.  -*- lexical-binding: t; -*-


;;; Commentary:
;; Dependencies: Anki, AnkiConnect should be installed and running.

;; TODO:
;; - Use plz.el queue for uploading media and add message in queue :finally
;;   to notify that all media was uploaded.
;;
;; - Add "protected tags" that won't be deleted from Anki even when they're
;;   absent in Org entries, useful for special tags like "marked" and "leech".
;;
;; - When the deck is changed, updating note should move the note to new deck.
;;
;; - Add ability to set the front field different from the title, e.g. inherent
;;   parent heading.
;;
;; - Maybe use width size from image attributes (e.g. "#+ATTR_ORG: :width
;;   830px") to resize images before uploading to the Anki media folder.
;;
;; - Guard ankiConnect and Anki versions.
;;
;; - Make HTML export async. Check out `org-export-async-start', for an example
;;   of async elisp based on processes, or see (info "(elisp) Threads") for an
;;   alternative based on threads.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-attach)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'outline)
(require 'plz)


;;; User options

(defgroup org-an nil
  "Synchronize entries in org files with Anki."
  :group 'comm
  :group 'org)

(defcustom org-an-default-deck nil
  "Default deck if deck name is not specified in the properties."
  :type '(choice
          (const :tag "Disable" nil)
          (string :tag "Deck name")))

(defcustom org-an-default-note-type "Basic (org-an)"
  "Default note type if note type is not specified in the properties."
  :type 'string)

(defcustom org-an-ankiconnect-host "http://127.0.0.1:8765"
  "The network address AnkiConnect is listening on."
  :type 'string)

(defcustom org-an-not-exported-tags
  (append org-export-select-tags org-export-exclude-tags '("ATTACH"))
  "A list of Org tags that are ignored when constructing notes."
  :type '(repeat string))


;;; Internal variables

;; Check out the Anki source code for the up-to-date supported formats.
;; https://github.com/ankitects/anki/blob/ba6325b47f1cc001873147c7d9430d5a35de0e6b/rslib/src/image_occlusion/imagedata.rs#L144
;; or
;; https://github.com/ankitects/anki/blob/1be9c368635651da072254d690fc4a6ed30fdae1/qt/aqt/editor.py#L60
(defconst org-an--image-extensions-regexp
  (format "\\.%s\\'"
	  (regexp-opt
	   '("jpg" "jpeg" "png" "tif" "tiff" "gif" "svg" "webp" "ico")
	   t))
  "Image extensions supported by Anki.")

(defconst org-an--sound-extensions-regexp
  (format "\\.%s\\'"
	  (regexp-opt
           '("3gp" "aac" "avi" "flac" "flv" "m4a" "mkv" "mov" "mp3" "mp4" "mpeg"
             "mpg" "oga" "ogg" "ogv" "ogx" "opus" "spx" "swf" "wav" "webm")
           t))
  "Sound extensions supported by Anki.")

(defconst org-an--html-inline-image-rules
  `(("file" . ,org-an--image-extensions-regexp)
    ("http" . ,org-an--image-extensions-regexp)
    ("https" . ,org-an--image-extensions-regexp))
  "Patched `org-html-inline-image-rules'.
Rules cover image extensions supported by Anki.")

(defconst org-an--ox-anki-backend
  (org-export-create-backend
   :name 'anki
   :parent 'html
   :transcoders '((latex-fragment . org-an--ox-latex-fragment)
                  (latex-environment . org-an--ox-latex-environment)
                  (link . org-an--ox-link))
   :options
   '(;; Inline images.
     (:html-inline-images nil nil t nil)
     (:html-inline-image-rules nil nil org-an--html-inline-image-rules nil)
     ;; Always export latex
     (:with-latex nil nil t nil)
     ;; Do not export table of contents, properties, planning line.
     (:with-toc nil nil nil nil)
     (:with-properties nil nil nil nil)
     (:with-planning nil nil nil nil)
     (:with-title nil nil nil nil nil)))
  "Org export backend for Anki HTML syntax.")


;;; Commands

;;;###autoload
(defun org-an-push-entry-at-point ()
  "Push entry at point to Anki.

The command updates an existing note in Anki if the corresponding
note exists, otherwise, it creates a new Anki note.

The note is considered to exist if the \"ANKI_NOTE_ID\" property
is set."
  (interactive)
  (let ((note (org-an--compile-note)))
    (if (org-an--note-id note)
        (org-an--update-note note)
      (org-an--create-note note))))

;;;###autoload
(defun org-an-push-subtree-at-point ()
  "Push subtree at point to Anki.

The command updates an existing note in Anki if the corresponding
note exists, otherwise, it creates a new Anki note.

The note is considered to exist if the \"ANKI_NOTE_ID\" property
is set."
  (interactive)
  (let ((note (org-an--compile-note 'subtree)))
    (if (org-an--note-id note)
        (org-an--update-note note)
      (org-an--create-note note))))

;;;###autoload
(defun org-an-delete-note ()
  "Delete Anki note corresponding to entry at point if there is one.

The note is considered to exist if the \"ANKI_NOTE_ID\" property
is set."
  (interactive)
  (org-an--delete-note))


;;; Entry parsing

(cl-defstruct org-an--note
  "Note structure resembling the AnkiConnect Note object."
  (id nil
      :type (or nil integer)
      :documentation "Anki note ID.")
  (deck nil
        :type string
        :documentation "Anki deck name.")
  (type nil
        :type string
        :documentation "Anki note type.")
  (tags nil
        :type list
        :documentation "List of note tags.")
  (fields nil
          :type list
          :documentation "Alist of note fields.")
  (media nil
         :type list
         :documentation
         "List of local media files, represented as (PATH . ENCODED-NAME), in the note."))

(cl-defstruct org-an--media-file
  path encoded-name)

(defun org-an--compile-note (&optional subtree)
  "Compile note from the entry (or the subtree if SUBTREE is not nil) at point.

Returns `org-an--note' object where NOTE-ID is nil if the
\"ANKI_NOTE_ID\" property is not set."
  (let ((note-id (org-entry-get (point) "ANKI_NOTE_ID"))
        (deck-name (org-an--get-deck))
        (note-type (or (org-entry-get (point) "ANKI_NOTE_TYPE" t)
                       org-an-default-note-type))
        (tags (mapcar #'substring-no-properties (org-get-tags)))
        (window-position (window-start))
        subheadings-as-fields?
        media
        fields)

    ;; Narrow the buffer to make collecting media files and fields easier.
    (save-excursion
      (save-restriction
        (cond
         ;; Note = subtree?
         (subtree (org-narrow-to-subtree))

         ;; Note = entry with fields as subheadings?
         ((org-an--subheadings-as-fields-p note-type)
          (setq subheadings-as-fields? t)
          (org-narrow-to-subtree))

         ;; Note = entry.
         (t (narrow-to-region (org-entry-beginning-position)
                              (org-entry-end-position))))

        (setq media (org-an--encode-local-media-filenames))
        (setq fields
              (if subheadings-as-fields?
                  (org-an--compile-fields-from-subheadings media)
                (org-an--compile-note-fields note-type media)))))

    ;; Restore the exact scroll position as it was before parsing the note.
    (set-window-start (get-buffer-window) window-position)

    (make-org-an--note :id note-id
                       :deck deck-name
                       :type note-type
                       ;; Filter ignored tags (see `org-an-not-exported-tags').
                       :tags (seq-filter
                              (lambda (tag) (not (member tag org-an-not-exported-tags)))
                              tags)
                       :fields fields
                       :media media)))

(defun org-an--get-deck ()
  "Return Anki deck name for the entry at point."
  (cond
   ((org-entry-get (point) "ANKI_DECK" t))
   (org-an-default-deck)
   ;; Signal an error because Anki deck is not specified: neither the
   ;; "ANKI_DECK" property nor the default deck is set.
   (t (error
       (substitute-command-keys "Deck name is nil.
Set \"ANKI_DECK\" property. If you define it using a top level property, \
do not forget to activate the change using \\[org-ctrl-c-ctrl-c]")))))

(defun org-an--subheadings-as-fields-p (note-type)
  "Non-nil when the entry contains NOTE-TYPE fields as subheadings."
  (let ((subheadings (org-an--get-subheadings)))
    (pcase note-type
      ("Basic (org-an)"
       (equal '("Front" "Back") subheadings))
      (_
       (error "Unsupported note type: %s" note-type)))))

(defun org-an--get-subheadings ()
  "Return list of the current entry subheadings."
  (let ((window-position (window-start))
        subheadings)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (outline-next-heading)
        (while (< (point) (point-max))
          (push (org-get-heading t t t t) subheadings)
          (outline-next-heading))))
    (set-window-start (get-buffer-window) window-position)

    ;; Return subheadings.
    (reverse subheadings)))

(defun org-an--encode-local-media-filenames ()
  "Encode filenames of local media files.

Returns list of local media files with encoded filenames,
a file is represented as (PATH . ENCODED-NAME).

Encoding is needed for Anki which stores all files in the root of
the media folders, so there is a need to uniquely name files. The
method uses MD5 digest for encoded name: \"MD5.ORIGINAL_EXTENSION\"."
  (let ((local-media-paths (org-an--find-local-media-paths)))
    (delete-dups local-media-paths)
    (mapcar (lambda (path) (cons path (org-an--encode-path path)))
            local-media-paths)))

(defun org-an--find-local-media-paths ()
  "Find and return list of local media files paths in the buffer."
  ;; Map over all links in the current buffer and collect only local media.
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link)))
        ;; Expand attachment link.
        (when (string= type "attachment")
          (setq type "file")
          (save-excursion
            ;; Move point to the link, to make sure that point is at correct
            ;; entry for `org-attach-expand'. E.g., when there are multiple
            ;; entries, like during export of the entire subtree, this step is
            ;; mandatory.
            (goto-char (org-element-property :begin link))
            (setq path (org-attach-expand path))))
        ;; If the link is to a local media file, return it, else return
        ;; nil (which will be ignored and skipped by `org-element-map').
        (when (org-an--local-media-link-p type path)
          path)))))

(defun org-an--local-media-link-p (type path)
  "Non-nil when link (TYPE PATH) is to a local media file.

The method assumes that \"attachment\" links are expanded and
replaced with \"file\" links."
  (and (string= type "file")
       (or (string-match-p org-an--image-extensions-regexp path)
           (string-match-p org-an--sound-extensions-regexp path))))

(defun org-an--encode-path (path)
  "Return \"md5_digest.file_extension\" for PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (concat (md5 (current-buffer)) (file-name-extension path t))))

(defun org-an--compile-note-fields (note-type local-media)
  "Return note fields in the HTML format from the entry according to NOTE-TYPE.

LOCAL-MEDIA, list of files, represented as (PATH . ENCODED-NAME),
is used to adjust links to media files. Original file paths are
replaced by ENCODED-NAME that stand for files in the Anki media
folder."
  (let ((export-options `(:encoded-paths ,local-media)))
    (pcase note-type
      ("Basic (org-an)"
       ;; Basic type has two fields "Front" and "Back".
       ;; - Bind Front to the entry heading,
       ;; - bind Back to the entry body text.
       (let ((heading (org-export-string-as (org-get-heading t t t t)
                                            org-an--ox-anki-backend
                                            t
                                            export-options))
             body)
         (save-restriction
           (org-end-of-meta-data)
           (narrow-to-region (point) (point-max))
           (setq body (org-export-as org-an--ox-anki-backend nil nil t export-options)))
         `((Front . ,heading)
           (Back . ,body))))
      (_
       (error "Unsupported note type: %s" note-type)))))

(defun org-an--compile-fields-from-subheadings (local-media)
  "Return note fields compiled from subheadings.

LOCAL-MEDIA, list of files, represented as (PATH . ENCODED-NAME),
is used to adjust links to media files. Original file paths are
replaced by ENCODED-NAME that stand for files in the Anki media
folder."
  (outline-next-heading)
  (let ((export-options `(:encoded-paths ,local-media))
        fields)
    (while (< (point) (point-max))
      (let ((heading (substring-no-properties (org-get-heading t t t t)))
            body)
        (save-restriction
          (org-end-of-meta-data)
          (narrow-to-region (point) (org-entry-end-position))
          (setq body (org-export-as org-an--ox-anki-backend
                                    nil
                                    nil
                                    t
                                    export-options)))
        (push (cons (intern heading) body) fields))
      (outline-next-heading))
    (reverse fields)))


;;; Anki export transcoders

(defun org-an--ox-link (link description info)
  "Transcode a LINK object from Org to Anki HTML.

DESCRIPTION is the description part of the link, or the empty
string. INFO is a plist holding export options.

The method replaces paths to local media by corresponding
ENCODED-NAME according to `:encoded-paths', alist of cons cells
\(PATH . ENCODED-NAME), from the export options.

If LINK is to a local file that is not media, then the method
just returns DESCRIPTION.

If LINK is non-local, then the method invokes `org-html-link' and
returns its output.

Returns string, transcoded LINK."
  (if (string= (org-element-property :type link) "file")
    (let ((encoded-name (alist-get
                         (org-element-property :path link)
                         (plist-get info :encoded-paths)
                         nil
                         nil
                         #'equal)))
      (if encoded-name
          (if (string-match-p
               org-an--image-extensions-regexp
               (org-element-property :path link))
              (format "<img src=\"%s\">" encoded-name)
            (format "[sound:%s]" encoded-name))
        description))
    (org-html-link link description info)))

(defun org-an--ox-latex-fragment (latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object from Org to Anki HTML.
CONTENTS is nil. INFO is a plist holding export options."
  (let ((value (org-element-property :value latex-fragment)))
    (cond
     ((string-prefix-p "$$" value) (format "\\[%s\\]" (substring value 2 -2)))
     ((string-prefix-p "$" value) (format "\\(%s\\)" (substring value 1 -1)))
     ;; Leave \(...\) and \[...\] as is.
     (t value))))

(defun org-an--ox-latex-environment (latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT object from Org to Anki HTML.
CONTENTS is nil. INFO is a plist holding export options."
  (format "\\[%s\\]" (string-trim-right
                      (org-element-property :value latex-environment))))


;;; AnkiConnect HTTP calls

(defun org-an--create-note (note)
  "Create new Anki note based on the NOTE.

The point must be located on the entry from which NOTE was
constructed in order to correctly store \"ANKI_NOTE_ID\" property."
  (let* ((payload `( :action "addNote"
                     :version 6
                     :params
                     (:note
                      ( :deckName ,(org-an--note-deck note)
                        :modelName ,(org-an--note-type note)
                        :fields ,(org-an--note-fields note)
                        :tags ,(vconcat (org-an--note-tags note))
                        :options ( :allowDuplicate :false
                                   :duplicateScope "deck")
                        ;; HACK: use the "picture" parameter to upload all
                        ;;       media, may break in the future. Benefit of this
                        ;;       hack is that we can create Anki Note and upload
                        ;;       media in one HTTP request.
                        :picture
                        ,(vconcat (mapcar (lambda (media) `( :path ,(file-truename (car media))
                                                             :filename ,(cdr media)
                                                             :fields ["hack: do not insert in any field"]))
                                          (org-an--note-media note)))))))
         (body (json-serialize payload))
         ;; Store the entry ID so we can later find the entry and put the
         ;; "ANKI_NOTE_ID" property. In the case the entry ID is nil, remember
         ;; that and create one.
         (entry-id-was-nil (org-entry-get (point) "ID"))
         (entry-id (org-id-get-create))
         (file (buffer-file-name)))

    (plz 'post org-an-ankiconnect-host
      :body body
      :as (lambda () (json-parse-buffer :object-type 'plist))
      :then (lambda (response)
              (pcase (plist-get response :error)
                ;; If success, then set ANKI_NOTE_ID property.
                (:null (let ((note-id (plist-get response :result)))
                         (org-an--put-note-id file entry-id note-id)
                         (message "Successfully created note %d" note-id)))
                ;; If error, then message it.
                (err (message "AnkiConnect addNote failed with \"%s\"" err))))
      :finally (lambda ()
                 "Clean entry id if it was nil."
                 (unless entry-id-was-nil
                   (save-mark-and-excursion
                     (let ((where (org-id-find-id-in-file entry-id file 'marker)))
                       (unless where (error "ID %s not found" entry-id))
                       (org-entry-delete where "ID"))))))))

(defun org-an--update-note (note)
  "Update Anki note based on the NOTE."
  (let* ((note-id (string-to-number (org-an--note-id note)))
         (payload `( :action "updateNote"
                     :version 6
                     :params
                     (:note
                      ( :id ,note-id
                        :fields ,(org-an--note-fields note)
                        :tags ,(vconcat (org-an--note-tags note))))))
         (body (json-serialize payload))
         (media (org-an--note-media note)))

    (plz 'post org-an-ankiconnect-host
      :body body
      :as (lambda () (json-parse-buffer :object-type 'plist))
      :then (lambda (response)
              (pcase (plist-get response :error)
                ;; If success, then upload media if there is any.
                (:null (if (null media)
                           (message "Successfully updated note %d" note-id)
                         (message "Successfully updated note %d fields and tags, uploading media now..."
                                  note-id)
                         (mapcar #'org-an--upload-media media)))
                (err (message "AnkiConnect updateNote failed with \"%s\"" err)))))))

(defun org-an--upload-media (media)
  "Upload media file MEDIA, represented as (PATH . ENCODED-NAME), to Anki."
  (let* ((path (car media))
         (encoded-name (cdr media))
         (payload `( :action "storeMediaFile"
                     :version 6
                     :params ( :filename ,encoded-name
                               :path ,(file-truename path))))
         (body (json-serialize payload)))
    (plz 'post org-an-ankiconnect-host
      :body body
      :as (lambda () (json-parse-buffer :object-type 'plist))
      :then (lambda (response)
              (pcase (plist-get response :error)
                (:null (message "Successfully uploaded %s" path))
                (err (message
                      "AnkiConnect storeMediaFile for %s failed with %s."
                      path err)))))))

(defun org-an--delete-note ()
  "Delete Anki note corresponding to entry at point if there is one."
  (let ((note-id (org-entry-get (point) "ANKI_NOTE_ID")))
    (if (not note-id)
        (message "\"ANKI_NOTE_ID\" property is nil in the entry.")
      (let* ((payload `( :action "deleteNotes"
                         :version 6
                         :params (:notes [,(string-to-number note-id)])))
             (body (json-serialize payload))
             ;; Store the entry ID so we can later find the entry and delete the
             ;; "ANKI_NOTE_ID" property. In the case the entry ID is nil,
             ;; remember that and create one.
             (entry-id-was-nil (org-entry-get (point) "ID"))
             (entry-id (org-id-get-create))
             (file (buffer-file-name)))
        (plz 'post org-an-ankiconnect-host
          :body body
          :as (lambda () (json-parse-buffer :object-type 'plist))
          :then (lambda (response)
                  (pcase (plist-get response :error)
                    (:null (save-mark-and-excursion
                             (let ((where (org-id-find-id-in-file entry-id file 'marker)))
                               (unless where (error "ID %s not found" entry-id))
                               (org-entry-delete where "ANKI_NOTE_ID")))
                           (message "Successfully deleted note %s" note-id))
                    (err (message "AnkiConnect deleteNotes failed with %s." err))))
          :finally (lambda ()
                     "Clean entry id if it was nil."
                     (unless entry-id-was-nil
                       (save-mark-and-excursion
                         (let ((where (org-id-find-id-in-file entry-id file 'marker)))
                           (unless where (error "ID %s not found" entry-id))
                           (org-entry-delete where "ID"))))))))))


(defun org-an--put-note-id (file entry-id note-id)
  "Put NOTE-ID to the \"ANKI_NOTE_ID\" property in FILE.

Entry is identified by the ID property value (ENTRY-ID)."
  (let ((where (org-id-find-id-in-file entry-id file 'marker)))
    (unless where (error "ID %s not found" entry-id))
    (org-entry-put where "ANKI_NOTE_ID" (number-to-string note-id))))

(provide 'org-an)

;;; org-an.el ends here
