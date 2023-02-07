;;; org-anki-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-anki" "org-anki.el" (0 0 0 0))
;;; Generated autoloads from org-anki.el

(autoload 'org-anki-sync-entry "org-anki" "\
Synchronize entry at point." t nil)

(autoload 'org-anki-sync-all "org-anki" "\
Syncronize all entries in optional BUFFER.

\(fn &optional BUFFER)" t nil)

(autoload 'org-anki-update-all "org-anki" "\
Updates all entries in optional BUFFER.

Updates all entries that have ANKI_NOTE_ID property set.

\(fn &optional BUFFER)" t nil)

(autoload 'org-anki-delete-entry "org-anki" "\
Delete org entry under cursor." t nil)

(autoload 'org-anki-cloze-dwim "org-anki" "\
Convert current active region or word under cursor to Cloze
syntax.

\(fn &optional ARG HINT)" t nil)

(autoload 'org-anki-browse-entry "org-anki" "\
Browse entry at point on anki's browser dialog with searching nid" t nil)

(autoload 'org-anki-import-deck "org-anki" "\
Import deck with NAME to current buffer, or to BUFFER when provided.

This is a best-effort command which doesn't support all of Anki's features. Its use case is to import a deck to an .org which from then on will be used as source-of-truth for the notes.

Pandoc is required to be installed.

\(fn NAME &optional BUFFER)" t nil)

(register-definition-prefixes "org-anki" '("org-anki-" "plist-to-assoc"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-anki-autoloads.el ends here
