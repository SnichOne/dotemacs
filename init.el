;; TODO:
;; - [ ] refactor init.el:
;;    + [ ] remove commented code,
;;    + [ ] use (use-package emacs) for general settings?
;; - [ ] corfu: configure tab to expand to longest common prefix of candidates if
;;   current input is a proper prefix of the longest common prefix.
;; - [ ] vertico: disable for the `M-x man' command.
;; - [ ] maybe use GNU hyperbole?

;; The file is divided by sections enclosed by lines of dashes.

;; General rule: avoid using 'setq', 'setq-default' and use
;; 'customize-set-variable' for variables defined using 'defcustom'.
;;
;; Rationale: 'customize-set-variable' triggers setter method which does all
;; needed initialization, and this method seems to be better integrated with
;; Easy Customization UI, e.g. you can see all customized variables with the
;; 'customize-unsaved' command.

(require 'mode-local)                   ; provides 'setq-mode-local'
(require 'package)                      ; package manager

;; ---------------------------------------------------------------------------
;; General settings

;; Keep Working Directory Tidy. Emacs creates a number of temporary files to
;; ensure that we do not inadvertently lose our work while editing files. Let's
;; write backup files to a separate directory, since these files can clutter our
;; working directories.
(customize-set-variable 'backup-directory-alist
                        '(("." . "~/.local/state/emacs/backup/")))

;; Create backup files by copying our files, not moving our files. Everytime
;; Emacs has to create a backup file, it moves our file to the backup location,
;; then creates a new file at the same location as that of the one we are
;; editing, copies our content to this new file, and then resumes editing our
;; file. This causes any hard link referring to the original file to be now
;; referring to the backup file.
;; https://github.com/susam/emfy#keep-working-directory-tidy
(customize-set-variable 'backup-by-copying t)

;; Decrease delay before showing eldoc (documentation in minibuffer).
;; delay.
(customize-set-variable 'eldoc-idle-delay 0.3)

;; Even if you avoid using the customization UI, some settings may cause
;; customization variables to be added to your init.el file. Let's change that
;; and move customization variables to a separate file and load it.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Ask before exiting Emacs
(customize-set-variable 'confirm-kill-emacs 'y-or-n-p)

;; On saving a file, check if the file contains a shebang and makes it
;; executable. This little thing that might come handy when editing bash,
;; Python, etc. scripts.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; HACK: On macOS, set `treesit-extra-load-path' to nil.
;;
;; Reason: MacPorts installs broken python language grammar that hangs Emacs and
;; the default value of `treesit-extra-load-path' points to this grammar. I
;; installed the python grammar using `M-x treesit-install-language-grammar' and
;; it works, and the change is required to tell Emacs to use the correct
;; grammar.
(when (eq system-type 'darwin)
  (setq treesit-extra-load-path nil))

;; When emacs-mac is used, bind option to meta.
(when (boundp 'mac-carbon-version-string)
  (customize-set-variable 'mac-option-modifier
                          '(:ordinary meta :function meta :mouse meta)))
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Emacs basic look.

;; Disable scroll bar, scroll bar is nice but for some reason Emacs does not
;; allow you to resize the window when you try to drag the window by scroll bar,
;; so you have to resize only by dragging the area on modeline.
;; (scroll-bar-mode -1)

;; When emacs-mac is used, disable tool bar since it's just several buttons for
;; the most common operations but takes too much space.
(when (boundp 'mac-carbon-version-string)
  (tool-bar-mode -1))

;; Highlight current line in all buffers.
(global-hl-line-mode 1)

;; Inhibit the startup screen with the "Welcome to GNU Emacs" message from
;; appearing.
(customize-set-variable 'inhibit-startup-message t)

;; Maximize/fullscreen Emacs on startup.
;; There are two ways to do it: using 'default-frame-alist' or
;; 'initial-frame-alist':
;; 1. Maximize all frames.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 2. Should only the first frame be maximized, use 'initial-frame-alist',
;; ((un)comment the following(previous) line)
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Enable the leuven theme, a nice light theme available by default, e.g.
;; provides awesome Org-mode support by highlighting headers and changing title
;; font size.
;; (load-theme 'leuven-high-contrast t)
;; I am bit tired of the leuven theme — want to try modus-themes. Leuven,
;; modus-operandi and default are the best light themes.

;; Display column number in the mode line.
(column-number-mode 1)

;; Explicitly define a width to reduce the cost of on-the-fly computation.
(customize-set-variable 'display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(customize-set-variable 'display-line-numbers-widen t)

;; Enable line numbers in programming modes. Not
;; 'global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want line numbering.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Show the end of buffer in Text mode with a special glyph in the left fringe.
(setq-mode-local text-mode indicate-empty-lines t)

;; Make calendar begin weeks on Monday.
(customize-set-variable 'calendar-week-start-day 1)

;; Make Emacs mark out URLs specially in the current buffer. When the
;; `goto-address-mode' buffer-local minor mode is enabled, it finds all the URLs
;; in the buffer, highlights them, and turns them into clickable buttons. You
;; can follow the URL by typing ‘C-c <RET>’ (‘goto-address-at-point’) while
;; point is on its text; or by clicking with ‘mouse-2’, or by clicking ‘mouse-1’
;; quickly.
;; NOTE: evil mode has a default keybinding `gx' to follow a link at point, that
;; works regardless weather the mode is on or off.
(global-goto-address-mode 1)

;; Scroll horizontally with touchpad.
;; Correct left-right scroll direction for OS X.
(customize-set-variable 'mouse-wheel-flip-direction t)
;; Enable left-right scroll from trackpad.
(customize-set-variable 'mouse-wheel-tilt-scroll t)

;; Do not recenter point during scroll.
(customize-set-variable 'scroll-conservatively 101)

;; If not emacs-mac is used, enable pixel scrolling mode. Emacs-mac supports
;; pixel scrolling without the mode, and enabling the mode makes scrolling way
;; too fast.
(pixel-scroll-precision-mode (if (boundp 'mac-carbon-version-string) -1 1))

;; Hide cursor in image buffers
(defun hide-cursor ()
  ;; HACK: nothing else worked for me.
  ;; source: https://github.com/emacs-evil/evil/issues/592#issuecomment-1034443444.
  (setq-local evil-default-cursor #'ignore))
(add-hook 'image-mode-hook #'hide-cursor)
(add-hook 'doc-view-mode-hook #'hide-cursor)

;; Enable color output in `proced'
(customize-set-variable 'proced-enable-color-flag t)

;; He-he, games inside Emacs? Make them look nicer on modern monitors.
(when (eq system-type 'darwin)
  (setq gamegrid-glyph-height-mm 7.3))
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Buffer management.

;; Bind IBuffer to 'C-x C-b', the shortcut executes 'list-buffer' by default but
;; IBuffer is superior. IBuffer is a major mode for viewing a list of buffers
;; and operating on them in a way analogous to that of Dired, including
;; filtering, marking, sorting in various ways, and acting on buffers.
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Echo area.

;; Echo multi-character keys in the echo area after 0.01 second. The variable
;; ‘echo-keystrokes’ controls that, its value is the number of seconds of pause
;; required to cause echoing to start, or zero, meaning don’t echo at all.
(customize-set-variable 'echo-keystrokes '0.01)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Editing settings.

;; Enable auto insert of closing parentheses, brackets, etc.
;; Additionally, If the region is active, the parentheses (brackets, etc.) are
;; inserted around the region instead.
(electric-pair-mode 1)

;; Set line width: configure 'fill-paragraph' (binded to M-q by default) to wrap
;; lines at 80 characters.
(customize-set-variable 'fill-column 80)


;; NOTE: Commented out flyspell mode and flyspell prog mode in favor of speed
;; and snappiness of Emacs: https://github.com/minad/corfu/issues/163.
;; Probably, use `jinx' instead: https://www.reddit.com/r/emacs/comments/1231t0t/jinx_enchanted_justintime_spellchecker_gnu_elpa/

;; Enable Flyspell mode in Text mode. Flyspell is a minor mode that performs
;; automatic spell-checking of the text you type as you type it. Note that, as
;; Flyspell mode needs to check each word across which you move, it will slow
;; down cursor motion and scrolling commands. It also doesn't automatically
;; check the text you didn't type or move across; use 'flyspell-region' or
;; 'flyspell-buffer' for that.
;;
;; Prerequisites.
;; - Ispell and Flyspell commands only work if one of spell
;; checkers programs: Hunspell, Aspell, Ispell or Enchant, is installed on your
;; OS.
;; - 'ispell-complete-word' only works if you have a plain word-list dictionary
;; (sorted, case insensitive). You can install the 'words' package on Arch Linux
;; to get such dictionaries. Emacs configures 'ispell-alternate-dictionary' to
;; have the right path on Linux and macOS. But on windows, you need to set
;; 'ispell-complete-word-dict' manually.

;; NOTE: Uncomment here to enable flyspell.
;; (add-hook 'text-mode-hook #'flyspell-mode)

;; Enable Flyspell Prog mode in comments in Prog mode. Flyspell Prog mode only
;; checks words in comments and string constants.
;; NOTE: Uncomment here to enable flyspell-prog-mode.
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Important configuration of flyspell: you need to disable the M-<TAB> (C-M-i,
;; <ESC> <TAB>) binding for flyspell, because it's already used for the
;; 'complete-symbol' or 'ispell-complete-word' commands, e.g. this binding
;; overrides 'complete-symbol' in org-mode.
;; By default, flyspell corrects word using M-<TAB>.
(customize-set-variable 'flyspell-use-meta-tab nil)

;; Let a period followed by a single space be treated as end of sentence. By
;; default, Emacs uses the rather old-fashioned convention of treating a period
;; followed by double spaces as end of sentence.
(customize-set-variable 'sentence-end-double-space nil)

;; Remember and restore the last cursor position of opened files.
(save-place-mode 1)

;; Auto refresh buffers when the underlying file has changed outside of Emacs.
;; It's called Auto Revert mode in Emacs.
(global-auto-revert-mode 1)
;; When Auto Revert mode reverts a buffer that is under version control, it
;; updates the version control information in the mode line. However, Auto
;; Revert mode may not properly update this information if the version control
;; status changes without changes to the work file, from outside the current
;; Emacs session. If you set ‘auto-revert-check-vc-info’ to ‘t’, Auto Revert
;; mode updates the version control status information every
;; ‘auto-revert-interval’ seconds, even if the work file itself is unchanged.
;; The resulting CPU usage depends on the version control system, but is usually
;; not excessive.
;; NOTE: setting `auto-revert-check-vc-info' to `t' also makes
;; `first-change-hook' unusable: the hook runs periodically besides cases when
;; you modify unmodified buffer.
(customize-set-variable 'auto-revert-check-vc-info t)

;; Auto refresh Dired and other similar buffers.
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Highlight trailing whitespace at the end of lines: any stray trailing
;; whitespace at the end of lines is highlighted (usually with a red
;; background).
;;
;; It is possible to set it globally using
;; (customize-set-variable 'show-trailing-whitespace t)
;; but it is annoying to see trailing spaces in buffers like *Choices*, *Dired*,
;; etc.
(defun highlight-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'highlight-trailing-whitespace)
(add-hook 'conf-mode-hook #'highlight-trailing-whitespace)
(add-hook 'text-mode-hook #'highlight-trailing-whitespace)

;; Always add a newline automatically at the end of a file while saving. Many
;; Emacs major modes do this by default, but some do not, let's change it.
(customize-set-variable 'require-final-newline t)

;; Use spaces, not tabs, for indentation.
(customize-set-variable 'indent-tabs-mode nil)

;; Enable highlighting of matching pair of parentheses. By default, there is a
;; small delay between the movement of a cursor and the highlighting of the
;; matching pair of parentheses. The following line of code gets rid of this
;; delay.
(customize-set-variable 'show-paren-delay 0)

;; Make it easier to follow Git's 50/72 rule in 'VC' (Emacs built-in package,
;; an interface to version control systems).
(setq-mode-local vc-git-log-edit-mode fill-column 72)

;; Make Emacs hotkeys work in Russian layout.
;; http://reangdblog.blogspot.com/2015/05/emacs.html.
;; LOR suggests that method does not always work:
;; https://www.linux.org.ru/forum/general/13857712
(defun cfg:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
(cfg:reverse-input-method 'russian-computer)
;; NOTE: 'cfg:reverse-input-method' causes Emacs to open the *Quail Completions*
;; buffer, let's auto-close it.
(kill-buffer "*Quail Completions*")
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Elisp mode

;; Update: commented out the code below, cz it sucks sometimes and defaults are
;; reasonable. Emacs contains a lot of Elisp, I bet Emacs developers have such
;; defaults for a reason.

;; Configure better Elisp indentation, e.g. Emacs doesn't
;; indent plist correctly by default, this configuration fixes it.
;;
;; Taken from: https://stackoverflow.com/a/22167050.
;;
;; (customize-set-variable 'lisp-indent-function 'common-lisp-indent-function)
;; (put 'cl-flet 'common-lisp-indent-function
;;      (get 'flet 'common-lisp-indent-function))
;; (put 'cl-labels 'common-lisp-indent-function
;;      (get 'labels 'common-lisp-indent-function))
;; (put 'if 'common-lisp-indent-function 2)
;; (put 'dotimes-protect 'common-lisp-indent-function
;;      (get 'when 'common-lisp-indent-function))
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Packages

;; Do package configuration and installation using use-package.
;; NOTE: Try to lazy-load packages as much as possible to reduce the startup time:
;; - use the ':commands' block to specify commands that will serve as a trigger
;;   to load the package,
;; - use the ':bind' and ':mode' blocks for the same "trigger" purpose,
;; - use ':defer' as a last resource to lazy-load the package.

;; Initialize package manager.
;; The following commands are taken from the tutorial:
;; https://github.com/susam/emfy#install-packages.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Install use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Configure use-package to install packages with package.el if they are not
;; installed.
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)


;; Display scroll via nyan cat. Alternative — the 'poke-line' package.
(use-package nyan-mode
  :custom
  (nyan-mode t)
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  (nyan-bar-length 16))


;; Add parrot — no more lonely nyancat in the mode-line.
(use-package parrot
  :after evil
  :config
  (parrot-mode 1)
  (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
  (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
  (parrot-set-parrot-type 'science)

  (defun parrot-start-animation+ (&rest args)
    (parrot-start-animation))

  ;; Animate parrot when switching to a different window.
  ;; NOTE: works ok-ish: evil ex command input triggers the hook.
  (add-hook 'window-selection-change-functions #'parrot-start-animation+)

  ;; Animate parrot when switching focus to the Emacs frame.
  (add-function
   :after after-focus-change-function
   (lambda ()
     (when (frame-focus-state) (parrot-start-animation)))))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-checker-simple-format t)
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  ;; NOTE: AFAIU the left bar is changed to bar with little scroll indicator.
  (setq doom-modeline-hud t)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (setq doom-modeline-window-width-limit 135)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerd-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
  (setq doom-modeline-lsp-icon t)

  ;; Whether display the time icon. It respects option `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether display the live icons of time.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-time-icon'.
  (setq doom-modeline-time-live-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

  ;; When non-nil, mode line displays column numbers zero-based.
  ;; See `column-number-indicator-zero-based'.
  (setq doom-modeline-column-zero-based t)

  (setq doom-modeline-percent-position nil)

  ;; Format used to display line numbers in the mode line.
  ;; See `mode-line-position-line-format'.
  (setq doom-modeline-position-line-format '("L%l"))

  ;; Format used to display column numbers in the mode line.
  ;; See `mode-line-position-column-format'.
  (setq doom-modeline-position-column-format '("C%c"))

  ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
  (setq doom-modeline-position-column-line-format '("%l:%3c"))

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; Whether display the total line number。
  (setq doom-modeline-total-line-number nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github t)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;; Whether display the modern icons for modals: showing letter inside.
  (setq doom-modeline-modal-modern-icon t)

  ;; When non-nil, always show the register name when recording an evil macro.
  (setq doom-modeline-always-show-macro-register nil)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; The function to handle `buffer-file-name'.
  (setq doom-modeline-buffer-file-name-function #'identity)

  ;; The function to handle `buffer-file-truename'.
  (setq doom-modeline-buffer-file-truename-function #'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)

  ;; Change the executables to use for the language version string
  ;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  ;; (setq doom-modeline-env-ruby-executable "ruby")
  ;; (setq doom-modeline-env-perl-executable "perl")
  ;; (setq doom-modeline-env-go-executable "go")
  ;; (setq doom-modeline-env-elixir-executable "iex")
  ;; (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  ;; (setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)

  (doom-modeline-def-modeline 'default-but-buffer-position-on-right-side
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host word-count selection-info)
    '(buffer-position parrot compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

  ;; Set default mode-line
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'default-but-buffer-position-on-right-side 'default))))


;; === Minibuffer ============================================================


;; Do not allow the cursor in the minibuffer prompt, e.g. in the prompt for
;; `set-fill-column'.
;; Source: https://github.com/minad/vertico?tab=readme-ov-file#configuration.
(setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Display the default argument as ‘[DEFAULT-ARG]’ instead of ‘(default
;; DEFAULT-ARG)’, saving some screen space.
(customize-set-variable 'minibuffer-default-prompt-format " [%s]")



;; Configure minibuffer completion using the built-in Fido mode.
;; (use-package icomplete                  ; built-in
;;   :ensure nil
;;   :custom
;;   ;; Enable fido-mode. It's really just icomplete with slightly different
;;   ;; defaults that emulate ido mode as close as possible.
;;   (fido-mode t)
;;   ;; Make Fido/Icomplete mode display the possible completions on the same line
;;   ;; as the prompt by default.
;;   (icomplete-vertical-mode t)
;;   :config
;;   ;; Bind TAB to complete selected candidate.
;;   ;; NOTE: I used 'define-key' instead of ':bind' because the latter creates
;;   ;; autoloads for the command (see use-package docs for more info).
;;   (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete))


;; Vertico — Vertico provides a performant and minimalistic vertical completion
;; UI based on the default completion system.
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Configure how minibuffer suggest default input.
(use-package minibuf-eldef              ; built-in
  :ensure nil
  :custom
  ;; Hide the default argument as soon as you modify the contents of the
  ;; minibuffer (since typing <RET> would no longer submit that default).
  (minibuffer-electric-default-mode t))


;; Add colorful annotations placed at the margin of the minibuffer for
;; completion candidates.
(use-package marginalia
  ;; Either bind 'marginalia-cycle' globally or only in the minibuffer.
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!).
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  :config
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq truncate-lines t))))

;; ===========================================================================


;; === Isearch ===============================================================

;; Better isearch defaults. Isearch stands for incremental search. This means
;; that search results are updated and highlighted while you are typing your
;; query, incrementally.
(use-package isearch                    ; built-in
  :ensure nil
  :custom
  ;; Allow scroll while isearch is active.
  (isearch-allow-scroll 'unlimited)
  :hook
  ;; Leave the cursor at the beginning of match after isearch is completed.
  (isearch-mode-end . isearch-exit-at-start)

  :config

  ;; Source: https://github.com/oantolin/emacs-config/blob/856179ae1095cfe1211424f1dd1416960702a8a4/my-lisp/isearch-extras.el#L3
  (defun isearch-exit-at-start ()
    "Leave the cursor at the beginning of match after isearch is completed."
    (unless (or isearch-mode-end-hook-quit
                (bound-and-true-p isearch-suspended)
                (not isearch-forward)
                (not isearch-other-end)
                (and (boundp 'avy-command)
                     (eq avy-command 'avy-isearch)))
      (goto-char isearch-other-end))))

;; ===========================================================================


;; Enable the minor mode that indicates which buffer is currently active by
;; dimming the faces in the other buffers.
;;
;; The value of the dimmer-fraction has been selected empirically. Users might
;; prefer to tweak it further (increasing it makes the dim effect more
;; pronounced).
;;
;; Taken from:
;; https://protesilaos.com/emacs/modus-themes#h:8eb4b758-d318-4480-9ead-357a571beb93
;;
;; Update: commented it out, since it dims lsp signature in the minibuffer or
;; echo area, do not know what it is and fixing it is not a priority.
;; (use-package dimmer
;;   :defer 1
;;   :config
;;   (setq dimmer-fraction 0.3)
;;   (setq dimmer-adjustment-mode :foreground)
;;   (setq dimmer-use-colorspace :rgb)

;;   (dimmer-mode 1))


;; Load Modus theme.
(use-package modus-themes
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes.

  ;; Adjust modus-vivendi colors.
  ;; TODO review whether the following customization works in the 4.0 version
  ;; (setq modus-themes-vivendi-color-overrides
  ;;       '((fg-main . "#f0fff0")))       ; Default face is white on black, dim white a bit.

  ;; TODO (change the following code since such customizations are deprecated in
  ;; the 4.0 version.
  ;; (setq modus-themes-mode-line '(borderless accented)
  ;;       modus-themes-syntax '(yellow-comments))
  ;;
  ;; E.g. to make mode line borderless use:
  ;; (setq modus-themes-common-palette-overrides
  ;;       '((border-mode-line-active unspecified)
  ;;       (border-mode-line-inactive unspecified)))

  ;; Maybe define some palette overrides (there are interesting presets:
  ;; 'modus-themes-preset-overrides-faint'
  ;; and 'modus-themes-preset-overrides-intense'
  (setq modus-themes-common-palette-overrides
        '(;; remove mode line border
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; remove fringe background
          ;; (fringe unspecified)

          ;; make org properties and attributes (e.g., lines like "#+title: ...")
          ;; distinguishable from comments.
          (prose-metadata cyan-warmer)
          (prose-metadata-value magenta)))
          ;; (comment yellow-faint)
          ;; (string green-warmer)))

  ;; Fix fzf colors in term.
  (custom-set-faces
   `(term-color-yellow ((t :foreground "light goldenrod"
                           :background "light goldenrod"))))

  ;; Load the theme of your choice:
  (load-theme 'modus-operandi :no-confirm) ;; OR (load-theme 'modus-vivendi :no-confirm).
  :bind ("<f5>" . modus-themes-toggle))


;; Olivetti lets you center your buffer for aesthetics and focus.
;; NOTE: I tried it mostly for org mode, but it needs some configuration:
;; 1. disable conflicting with org-cdlatex keybind: "C-c {",
;; 2. customize initial width for org-mode, since I have large columns than then
;;    fill-column value.
;; (use-package olivetti
;;   :bind
;;   ("<left-margin> <mouse-1>" . ignore)
;;   ("<right-margin> <mouse-1>" . ignore))


;; Enable the minor mode for Emacs that displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings). This includes
;; prefixes like C-x 8 which are shown in a different face.
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.4))


;; Collapse all minor modes in modeline, but whitelist flycheck and flymake.
;; Probably will switch to doom-modeline, if I ever use evil-mode.
(use-package minions
  :hook
  (after-init . minions-mode)
  :custom
  (minions-mode-line-lighter ";")
  (minions-prominent-modes '(flycheck-mode flymake-mode envrc-mode)))


;; Add icons.
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Hack Nerd Font Mono"))
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   (dirvish-directory-view-mode . diredfl-mode)))
  ;; Show dirnames in bold.
  ;; :config
  ;; (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Improved dired.
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")))

  ;; Commands which ask for a destination directory, such as those which copy
  ;; and rename files or create links for them, try to guess the default target
  ;; directory for the operation. Normally, they suggest the Dired buffer’s
  ;; default directory, but if the option 'dired-dwim-target' is non-nil, and if
  ;; there is another Dired buffer displayed in some window, that other buffer’s
  ;; directory is suggested instead.

  ;; The 'dired-dwim-target-next' value makes Dired to prefer the next windows
  ;; on the same frame. Default is nil.
  (dired-dwim-target 'dired-dwim-target-next)

  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state))
  (setq delete-by-moving-to-trash t)
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/local/bin/gls"))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; Emacs 29 added mouse drag-and-drop support for Dired, the following settings will enable it:
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


;; Convenient completion popup.
;; (use-package company
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0)         ; Default is 0.2.
;;   :commands (completion-at-point)
;;   :hook
;;   (after-init . global-company-mode)
;;   ;; There is company-tng-mode which behaves similar to Vim YCM or coc, but it's
;;   ;; not working exactly with lsp-mode and pylsp server: when you cycle through
;;   ;; with TAB it outputs "virtual" arguments which disappear when you start
;;   ;; typing anything.
;;   (after-init . company-tng-mode)
;;   :bind
;;   ( :map prog-mode-map ("<tab>" . company-indent-or-complete-common))
;;   ;;         :map company-active-map
;;   ;;         ("<tab>" . company-complete-common-or-cycle)
;;   ;;         ("<backtab>" . company-select-previous))
;;   )

;; Corfu. Completion popup.
;; TODO: configure word completion in Org mode, e.g., 'ispell-complete-word'.
(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  ;; TAB-and-Go completion customizations
  (corfu-auto t)
  ;; (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  ;; Do not preselect anything, this way hitting tab will select the first
  ;; candidate.
  (corfu-preselect 'prompt)
  ;; Display candidate documentation or source in a popup next to the candidate
  ;; menu.
  (corfu-popupinfo-mode t)
  ;; Use TAB for cycling, default is 'corfu-complete'.
  :custom-face
  ;; Set candidate documentation font size in the popup documentation.
  (corfu-popupinfo ((t (:height 0.9))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (setq tab-always-indent 'complete))

;; Add extensions to corfu: additional backends.
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!

  ;; Dabbrev is nice, but it can become laggy when a lot of large files are
  ;; opened.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Enable Flycheck globally. Check for errors on the fly. Flycheck has better
;; integration with lsp-mode than built-in Flymake, e.g. lsp-ui sideline does
;; not show diagnostics with Flymake.
;; (use-package flycheck
;;   :defer 1
;;   :config
;;   (setq flycheck-display-errors-delay 0)
;;   (global-flycheck-mode))

;; Enable flycheck-status-emoji, it replaces the standard Flycheck mode-line
;; status indicators with cute, compact emoji that convey the corresponding
;; information.
;; (use-package flycheck-status-emoji
;;   :after flycheck
;;   :config
;;   (flycheck-status-emoji-mode))

;; LSP support.
;; (use-package lsp-mode
;;   :init
;;   ;; Set prefix for lsp-command-keymap (few alternatives - "s-l", "C-c l").
;;   (setq lsp-enable-snippet nil
;;         lsp-completion-enable-additional-text-edit nil
;;         lsp-completion-show-detail nil
;;         lsp-completion-show-kind nil
;;         lsp-pylsp-plugins-jedi-completion-include-params nil
;;         lsp-pylsp-plugins-jedi-completion-include-class-objects nil
;;         lsp-keymap-prefix "C-c l")
;;   :hook ((python-mode . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration)) ; shows keymap names, e.g. +goto, +refactor, etc.
;;   :commands (lsp lsp-deferred))

;; Optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;; Enable Flymake in prog-mode. Flymake is a universal on-the-fly syntax
;; checker.
(use-package flymake                    ; built-in
  :ensure nil
  :hook prog-mode
  :bind (("C-c e [" . flymake-goto-prev-error)
         ("C-c e ]" . flymake-goto-next-error)
         ("C-c e b" . flymake-show-buffer-diagnostics)
         ("C-c e p" . flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; LSP support.
(use-package eglot
  :custom
  ;; This stops eglot from logging the json events of lsp server and improves
  ;; the performance.
  (eglot-events-buffer-size 0)

  ;; Disable elgot freeze the UI for up to 3s when you open file maybe large
  ;; one.
  (eglot-sync-connect nil)

  ;; If and M-. (xref-find-definitions) lands you in a file outside of your
  ;; project, such as a system-installed library or header file, transiently
  ;; consider that file as managed by the same language server. That file is
  ;; still outside your project (i.e. project-find-file won’t find it), but
  ;; Eglot and the server will consider it to be part of the workspace.
  ;; By default, eglot starts new server, e.g., each time you press M-. on
  ;; an imported library outside the project.
  (eglot-extend-to-xref t)
  :hook ((python-mode python-ts-mode) . eglot-ensure))
;; (use-package eldoc-box
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))

(use-package eldoc
  :ensure nil
  :custom
  eldoc-echo-area-prefer-doc-buffer t)


;; Dumb Jump is an Emacs "jump to definition" package with support for 50+
;; programming languages that favors "just working". This means minimal — and
;; ideally zero — configuration with absolutely no stored indexes (TAGS) or
;; persistent background processes..
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Highlight TODO and similar keywords in comments and strings.
(use-package hl-todo
  :hook (prog-mode conf-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))


;; TODO: Load 'setq-mode-local' using use-package, after refactoring the
;; sections above without use-package: should I rewrite them  use-package.
;; (use-package mode-local
;;   :ensure nil)


;; Org mode.
(use-package org
  :custom
  ;; Fontify (e.g., highlight with a background color) the whole line for
  ;; headings. Looks nice with the leuven theme.
  ;; (org-fontify-whole-heading-line t)

  ;; Display the buffer in the indented view, this also hides leading heading
  ;; stars, only one star (the rightmost) at each heading is visible, the rest are
  ;; masked with the same font color as background.
  (org-startup-indented t)

  ;; Customize org-M-RET-may-split-line to make M-RET not split the line, but
  ;; allow splitting the line at the cursor position when creating a new list
  ;; item. Useful when inserting new heading. Doom Emacs doesn't split the line
  ;; in all contexts by default. Source:
  ;; https://orgmode.org/manual/Structure-Editing.html#index-M_002dRET.
  (org-M-RET-may-split-line '((default . nil)
                              (item . t)))

  ;; Scale LaTeX preview.
  ;; Alternative to customize-set-variable one can use plist-put:
  ;; (plist-put org-format-latex-options :scale 1.5)
  (org-format-latex-options '( :foreground default
                               :background default
                               :scale 1.8
                               :html-foreground "Black"
                               :html-background "Transparent"
                               :html-scale 1.0
                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; Configure LaTeX hyperref package.
  ;; 1. Better table of contents (TOC), list of figures (LOF) and list of
  ;;    tables (LOT): make both section and page number be links,
  ;; 2. Enable color for links,
  ;; 3. Enable "backlink" text at the end of each item in the bibliography,
  ;;    as a list of page numbers. This can only work properly if there is
  ;;    a blank line after each \bibitem.
  ;;    TODO: probably org-mode should be configured additionally to create
  ;;    such blank lines.
  (org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n linktoc=all,\n colorlinks=true,\n pagebackref=true}\n")

  ;; Enable syntax highlighting for latex fragments.
  ;; NOTE: I didn't understood
  (org-highlight-latex-and-related '(native))

  ;; Enable syntax higlighting of source blocks in LaTeX export.
  (org-latex-src-block-backend 'engraved)

  ;; Prevent inadvertently edit an invisible part of the buffer and be confused
  ;; on what has been edited and how to undo the mistake. Setting
  ;; 'org-catch-invisible-edits' to 'error' makes Emacs throw an error and do
  ;; nothing when inserting or deleting a character in an invisible region.
  ;; TODO: for some reason, when I hit C-RET on a collapsed tree, newly created
  ;; headline is created and Emacs shows three dots, as if it is collapsed, and
  ;; therefore does not let you edit it unless you "expand" it.
  ;; Disabled this option for that reason until further investigation.
  ;; (org-catch-invisible-edits 'error)

  ;; Store quick notes (C-c C-z) in the drawer (LOGBOOK by default). It is also
  ;; possible to arrange for state change notes and clock times to be stored in
  ;; a similar way.
  (org-log-into-drawer t)

  ;; Insert a line ‘CLOSED: [timestamp]’ just after the headline each time you
  ;; turn an entry from a TODO (not-done) state into any of the DONE states.
  (org-log-done 'time)

  ;; Configure default target file for notes (org-capture).
  (org-default-notes-file "~/org/notes.org")

  ;; It seems, that you should set 'org-link-search-must-match-exact-headline'
  ;; to nil to make links like ‘file:projects.org::some words’ perform text
  ;; search in the file. By default ('query-to-create'), clicking on such links
  ;; will fuzzy? search for match with headlines and offer to create a new
  ;; headline when none matched.
  (org-link-search-must-match-exact-headline nil)

  ;; Customize refile targets: allow refilling entries ('C-c C-w' or 'M-x
  ;; org-refile') to any headline in the current buffer with level <= 4.
  (org-refile-targets '((nil :maxlevel . 4)))
  ;; Make refile use outline path (including filename without directory) for
  ;; targets as paths. So a level 3 headline will be available as
  ;; filename/level1/level2/level3. By default, refile accepts only the headline
  ;; without it's parent(s).
  (org-refile-use-outline-path 'file)
  ;; Disable refiling in steps ('org-outline-path-complete-in-steps') since it
  ;; does not work with fido-mode (and maybe with all other completion
  ;; packages).
  (org-outline-path-complete-in-steps nil)

  ;; Configure capture templates:
  ;; - life.org task template,
  ;; - life_journal.org daily plan template: datetree structure,
  ;; - life_journal.org weekly review template: datetree structure,
  ;; - work.org task template,
  ;; - work_journal.org daily plan template: datetree structure.
  ;; TODO ⮷
  ;; - book template,
  ;; - movie template.
  (org-capture-templates
   '(("l" "Life")
     ("lt" "Life task" entry (file "life.org")
      "* TODO %^{Name}\n\n%?"
      :empty-lines 1
      :prepend t)
     ("li" "Life daily plan (checkbox items)" checkitem (file+olp+datetree "life_journal.org")
      nil
      :empty-lines 1
      :jump-to-captured 1)
     ("lr" "Life weekly review" checkitem (file+olp+datetree "life_journal.org")
      (file "templates/weekly_review.org")
      :empty-lines 1)

     ("w" "Work")
     ("wt" "Work task" entry (file "work.org")
      "* TODO %^{Name}\n\n%?"
      :empty-lines 1
      :prepend t)
     ("wi" "Work daily plan (checkbox items)" checkitem (file+olp+datetree "work_journal.org")
      nil
      :empty-lines 1
      :jump-to-captured t)))

  ;; Disable automatic bookmarks creation.
  ;; NOTE: Documentation for the 9.5 version says you should change
  ;; 'org-capture-last-stored', but the documentation is incorrect, the
  ;; up-to-date way is to customize 'org-bookmark-names-plist'.
  (org-bookmark-names-plist nil)

  ;; Configure 'org-image-actual-width': make Org mode try to get the width from
  ;; any "#+ATTR.*# keyword if it matches a width specification like:
  ;;
  ;;     #+ATTR_HTML: :width 300px
  ;;
  ;; and fall back on the original width if none is found.
  ;; NOTE: One can use the "#+ATTR_ORG: :width" option to control the width of
  ;; the image displayed inline in Org mode, and leave other "#+ATTR.*# keywords
  ;; for their primal purpose.
  (org-image-actual-width nil)

  ;; Download and display remote images: only for ssh connections, images linked
  ;; to external URL (http, https) are not affected by this setting).
  (org-display-remote-inline-images 'download)

  ;; Configure how much levels should be exported as a headline, inferior levels
  ;; will usually produce itemize or enumerate lists when exported, but back-end
  ;; behavior may differ.
  (org-export-headline-levels 7)

  ;; Configure org-id to create ID only if 'org-store-link' is called directly
  ;; and CUSTOM_ID is not present.
  ;; Reason: to avoid proliferation of unwanted IDs, just because you happen to
  ;; be in an Org file when you call ‘org-capture’ that automatically and
  ;; preemptively creates a link.
  ;; NOTE: 'org-id.el' should be loaded, e.g., "(require 'org-id)".
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Configure org mode to use timestamp slugs for the ID property instead of
  ;; UUID.
  (org-id-method 'ts)
  ;; This will set up our unique IDs as timestamp, but we need to configure
  ;; org-attach to use it. Because org-mode is set up to use UUIDs by default,
  ;; org-attach is set to create directories that are meant to work with UUID.
  ;; The functions that create directories for 'org-attach' are defined in
  ;; another function, 'org-attach-id-to-path-function-list'. Specifically, it
  ;; points to two functions: 'org-attach-id-uuid-folder-format' and
  ;; 'org-attach-id-ts-folder-format'. You can go into 'org-attach.el' and see
  ;; that they break down the folder structure in a pretty straightforward way:
  ;; the UUID function (which is the one used by default) takes the first two
  ;; characters of the UUID and makes a parent folder out of those (as seen
  ;; above), while the ts function takes the first six. The first six characters
  ;; make sense because they include the year and the month.

  ;; By default, if we use the example of 20220315T083403.413614 as a timestamp,
  ;; we will get the following directory structure:
  ;; /home/user/org/data/20/220315T083403.413614. Not very useful: you will need
  ;; to keep using org-mode until the year 2100 for a new sub-folder to be
  ;; created! What needs to be changed is org-attach-id-to-path-function-list.
  ;; It is as simple as changing the order of the functions on this list, so
  ;; org-attach will know to use the function first.

  ;; Source: https://taonaw-blog.netlify.app/2022-03-13/
  (org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                         org-attach-id-uuid-folder-format))

  ;; Configure languages which can be evaluated in Org buffers.
  (org-babel-load-languages '((R . t)
                              (python . t)))
  ;; Disable the confirmation prompt asking permission to evaluate the source
  ;; code.
  (org-confirm-babel-evaluate nil)

  :bind
  ;; For a better experience, the three Org commands ‘org-store-link’,
  ;; ‘org-capture’ and ‘org-agenda’ ought to be accessible anywhere in Emacs,
  ;; not just in Org buffers. To that effect, you need to bind them to globally
  ;; available keys.
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link)
   ("C-c o s" . +org-quick-attach-image))

  :config
  ;; Org mode sets 'truncate-lines' to 't', so each line of text has just one
  ;; scree line. But there is a problem with it: you can be left with horizontal
  ;; scroll after you invoke 'org-fill-paragraph' on a long line and you will
  ;; have to manually scroll to adjust the view. You can change
  ;; 'auto-hscholl-mode' to f'current-line' locally to 'org-mode' to avoid the
  ;; problem. But setting this option makes inconvenient table editing of tables
  ;; that do not fit on screen.
  ;; So instead of setting 'auto-hscroll-mode' to 'current-line' I better remap
  ;; org-fill-paragraph to scroll horizontally to left after filling the
  ;; paragraph.
  (defun set-selected-window-hscroll-to-0 ()
    (interactive)
    (set-window-hscroll (selected-window) 0))
  (advice-add 'org-fill-paragraph :after #'set-selected-window-hscroll-to-0)

  ;; Configure 'fill-paragraph' to wrap lines at 72 characters in org-mode.
  (setq-mode-local org-mode fill-column 72)

  ;; Set the path to default bibliography files:
  ;; '<org-directory>/bibligoraphy.bib'.
  (setq org-cite-global-bibliography
        (list (file-name-concat org-directory "bibliography.bib")))

  ;; Load org-id. This library will automatically create the ID property
  ;; if 'org-id-link-to-org-use-id' is set.
  (require 'org-id)

  ;; Improve equation numbering in LaTeX fragments in org-mode.
  ;; By default, equation numbers, generated with latex preview in Org Mode do
  ;; not increment above (1).
  ;; Source: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
  (defun org-renumber-environment (orig-func &rest args)
    (let ((results '())
          (counter -1)
          (numberp))

      (setq results
            (cl-loop for (begin .  env) in
                     (org-element-map (org-element-parse-buffer) 'latex-environment
                       (lambda (env)
                         (cons
                          (org-element-property :begin env)
                          (org-element-property :value env))))
                     collect
                     (cond
                      ((and (string-match "\\\\begin{equation}" env)
                            (not (string-match "\\\\tag{" env)))
                       (cl-incf counter)
                       (cons begin counter))
                      ((string-match "\\\\begin{align}" env)
                       (prog2
                           (cl-incf counter)
                           (cons begin counter)
                         (with-temp-buffer
                           (insert env)
                           (goto-char (point-min))
                           ;; \\ is used for a new line. Each one leads to a number
                           (cl-incf counter (count-matches "\\\\$"))
                           ;; unless there are nonumbers.
                           (goto-char (point-min))
                           (cl-decf counter (count-matches "\\nonumber")))))
                      (t
                       (cons begin nil)))))

      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))

    (apply orig-func args))

  (advice-add 'org-create-formula-image :around #'org-renumber-environment)

  ;; Add by default additional LaTeX packages for Russian language support.
  ;; NOTE: I changed my mind, these settings should be better placed using
  ;; headers per file basis.
  ;; (add-to-list 'org-latex-packages-alist '("" "cmap" t))
  ;; (add-to-list 'org-latex-packages-alist '("english,russian" "babel" t))

  (define-minor-mode +org-pretty-mode
    "Hides emphasis markers."
    :init-value nil
    :lighter " *"
    :group 'org
    (setq org-hide-emphasis-markers +org-pretty-mode)
    ;; (with-silent-modifications
    ;;   ;; In case the above un-align tables
    ;;   (org-table-map-tables 'org-table-align t))
    )

  (+org-pretty-mode 1)

  (defun +org-quick-attach-image (url width)
    "Download an image from a URL and insert a link to the image.

Also,
- convert the downloaded file to GIF if it is an MP4 video;
- insert \"#+ATTR_HTML :width WIDTH\" before the link.

The image is downloaded to the attach directory."
    (interactive "*sURL: \nnInline width (in px): ")
    (org-attach-url url)
    (insert (format "#+ATTR_HTML: :width %dpx" width))
    (newline-and-indent)
    (let ((link-beginning (point))
          (file (file-name-nondirectory url)))
      (when (string= (file-name-extension file) "mp4")
        ;; Convert the video to GIF.
        (let ((default-directory (org-attach-dir))
              (gif-file-name (concat (file-name-sans-extension file) ".gif")))
          (call-process "ffmpeg" nil nil nil
                        "-y"
                        "-i" file
                        gif-file-name)
          (delete-file file)
          (setq file gif-file-name)))
      (insert (format "[[attachment:%s]]" file))
      (org-display-inline-images nil nil link-beginning (point)))
    (newline-and-indent))

  (defun +org-refresh-images ()
    (interactive)
    "TODO: refresh/show images that are visible on the screen."))

;; Install Org mode export dependencies: better code syntax highlighting.
;; 1. Htmlize. Convert buffer text and decorations to HTML.
(use-package htmlize
  :after org)
;; 2. Engrave-faces. Convert font-lock faces to other formats.
(use-package engrave-faces
  :after org)

;; Org-an — my anki synchronization plugin.
(use-package org-an
  :ensure nil
  :load-path "lisp/"
  :after org                            ; required, because of the binding to
                                        ; org-mode-map which is created only
                                        ; after org package is loaded
  :commands (org-an-push-entry-at-point
             org-an-delete-note)
  :bind
  ;; Bind the sync entry command ('org-anki-sync-entry') to "C-c a s".
  ( :map org-mode-map
    ("C-c a" . org-an-push-entry-at-point)))

;; Org-cliplink. A simple command that takes a URL from the clipboard and
;; inserts an org-mode link with a title of a page found by the URL into the
;; current buffer.
(use-package org-cliplink
  :after org                            ; required, because of the binding to
                                        ; org-mode-map which is created only
                                        ; after org package is loaded
  :bind
  ( :map org-mode-map
    ("C-c o i" . org-cliplink)))

;; Org-modern. Modern Org Style: Org Mode prettifier.
;; To have a vertical line, that visually shows the span of the bock, to the
;; left of the blocks (e.g., source blocks), it seems, that you need to disable
;; `org-startup-indented' and add a frame margin: internal-border-width. But I
;; find `org-indent-mode' more useful.
(use-package org-modern
  :after org
  :custom
  ;; Disable org-modern for tables, otherwise table borders have messed alignment.
  (org-modern-table nil)
  :hook (org-mode . org-modern-mode))

;; Org-inline-anim. Inline playback of animated GIF/PNG for Org.
;;
;; You can play an animation by M-x org-inline-anim-animate (or C-c C-x m) when
;; the point is on the image.
;;
;; `M-x org-inline-anim-animate-all' (or C-c C-x M) plays all the animatable
;; images in the buffer at once. You can stop it with a double prefix (C-u C-u)
;; or a numeric arg 0 (C-u 0 or C-0). The former shows the last frame after
;; playback is stopped.
(use-package org-inline-anim
  :after org
  :custom
  (org-inline-anim-loop t)
  :hook (org-mode . org-inline-anim-mode))

;; Org-roam
;; (use-package org-roam
;;   :custom
;;   (org-roam-directory "~/org/roam notes/"))

;; Xeft. Full-text search for notes. Based on Xapian.
;; NOTE. Xeft on the first run will ask to download or install Xapian dynamic
;; module. It seems that the download option is just fine.
(use-package xeft
  :commands xeft
  :custom
  ;; Set path to notes.
  (xeft-directory "~/org/")
  ;; Set path to Xeft database.
  (xeft-database "~/.config/xeft")
  ;; Ignore not org files.
  (xeft-ignore-extension '("iimg" "png" "gif" "jpg" "html" "pdf" "tex" "log"))
  ;; Make Xeft search recursively, by default search works only for first level
  ;; files in 'xeft-directory'
  (xeft-recursive t))


;; Better LaTeX input.
;; CDLaTeX.
(use-package cdlatex
  :after org
  :hook (org-mode . turn-on-org-cdlatex))

;; AUCTeX.
(use-package tex
  :ensure auctex
  :defer t)

;; Org-fragtog. Automatically toggle Org mode LaTeX fragment previews as the
;; cursor enters and exits them
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))


;; Dired (built-in file manager).
;; (use-package dired
;;   :ensure nil
;;   :custom
;;   ;; Make Dired show directories first, works on Linux, MacOS.
;;   (dired-listing-switches "-ahl --group-directories-first")

;;   ;; Commands which ask for a destination directory, such as those which copy
;;   ;; and rename files or create links for them, try to guess the default target
;;   ;; directory for the operation. Normally, they suggest the Dired buffer’s
;;   ;; default directory, but if the option 'dired-dwim-target' is non-nil, and if
;;   ;; there is another Dired buffer displayed in some window, that other buffer’s
;;   ;; directory is suggested instead.

;;   ;; The 'dired-dwim-target-next' value makes Dired to prefer the next windows
;;   ;; on the same frame. Default is nil.
;;   (dired-dwim-target 'dired-dwim-target-next)
;;   :hook
;;   ;; Hide details: file properties, owner, size, modified time.
;;   (dired-mode . dired-hide-details-mode)

;;   :config
;;   (when (eq system-type 'darwin)
;;     (setq dired-use-ls-dired t
;;           insert-directory-program "/opt/local/bin/gls")))

;; Icons for dired
;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; Magit. Complete text-based user interface to Git.
;; "A Git Porcelain inside Emacs".
(use-package magit
  :commands (magit-status magit-project-status)
  :custom
  (magit-view-git-manual-method 'man)
  :init
  ;; Project.el integration: bind 'magit-project-status' to "m" in the project
  ;; switch menu (when hitting 'C-x p p').
  ;; NOTE: Magit already has such integration, but it's placed in 'magit-extras.el'
  ;; and it is not executed until 'magit-extras' is evaluated. I decided to take
  ;; the following two lines from 'magit-extras' and put it here, in the :init
  ;; block.
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Magit Forge. Forge allows you to work with Git forges, such as Github and
;; Gitlab, from the comfort of Magit and the rest of Emacs.
(use-package forge
  :after magit
  :custom
  (auth-sources '("~/.authinfo")))


;; Emacs Speaks Statistics (ESS). It is designed to support editing of scripts
;; and interaction with various statistical analysis programs such as R, S-Plus,
;; SAS, Stata and OpenBUGS/JAGS.
(use-package ess
  ;; Configuration for R language
  :commands R
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.[rR]nw\\'" . Rnw-mode))
  :interpreter ("R" . R-mode))


;; Python (built-in python.el).
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python[0-9.]*" . python-ts-mode)
  :custom

  (python-shell-interpreter "python")
  (python-shell-completion-native-enable nil) ; otherwise there is warning
                                              ; related to readline.

  ;; Configure multiplier applied to indentation inside multi-line def blocks,
  ;; i.e. defines how function parameters should be indented.
  (python-indent-def-block-scale 1)
  ;; Use symmetric style to fill docstrings.
  (python-fill-docstring-style 'symmetric)
  :config
  ;; Configure 'fill-paragraph' to wrap lines at 72 characters in python-mode.
  (setq-mode-local python-mode fill-column 72)

  ;; Configure python to auto-indent line when you insert closing parenthesis or
  ;; bracket. Hah, I wrote this clutch.
  (defun python-indent-closing-paren-or-bracket ()
    ;; Check if the the last inserted char is ")", "]" or "}" and if the line
    ;; starts with ")", "]" or "}".

    ;; NOTE: this can be improved by matching the line with the regex
    ;; "^\w*[\)\]]\w*$"
    (when (and (member last-command-event '(?\) ?\] ?\}))
               (member (char-after (+ (line-beginning-position) (current-indentation))) '(?\) ?\] ?\})))
      (python-indent-line)))

  (defun fix-python-self-insert-hook ()
    (add-hook 'post-self-insert-hook
              #'python-indent-closing-paren-or-bracket
              0
              'local))

  ;; Add the hook (the fix to auto indenting closing parenthesis or bracket) to
  ;; both python-mode as well as to python-ts-mode (treesitter counterpart).
  ;;
  ;; NOTE: it may be a temporary requiremnt for treesitter mode and may change
  ;; in the future.
  (add-hook 'python-mode-hook #'fix-python-self-insert-hook)
  (add-hook 'python-ts-mode-hook #'fix-python-self-insert-hook))

(use-package python-black
  :after python
  :commands (python-black-buffer python-black-region))


;; Markdown mode. A major mode for editing Markdown-formatted text.
(use-package markdown-mode
  :custom
  (markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))


;; Yaml mode
(use-package yaml-mode
  :mode "\\.yml\\'")


;; Javascript
(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))


;; Dockerfile mode
(use-package dockerfile-mode)

;; Expand region increases the selected region by semantic units. Just keep
;; pressing the key until it selects what you want.
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; TRAMP. TRAMP stands for Transparent Remote Access, Multiple Protocol. In
;; brief, it’s a lovely way to wander around outside your local filesystem.
(use-package tramp                      ; built-in
  :ensure nil
  :config
  ;; Configure TRAMP to respect the PATH variable on the remote machine (e.g.,
  ;; for remote eshell sessions) by adding 'tramp-own-remote-path' to the list
  ;; `tramp-remote-path'.
  ;; NOTE: conda PATH is not set correctly, maybe it's the problem with my
  ;; .zshrc or using zsh in general.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Enable search for '.dir-locals.el' for remote files.
  (setq enable-remote-dir-locals t))


;; Evil — Vim emulation.
(use-package evil
  :init
  ;; Evil uses "C-z" and "C-M-z" to switch to Emacs state, hence unbind "C-z"
  ;; which is bound to 'suspend-emacs' by default.
  (global-unset-key (kbd "C-z"))

  :hook
  (after-init . evil-mode)

  :bind (:map evil-motion-state-map
         ("C-e" . evil-switch-to-windows-last-buffer)

         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         ("gj" . evil-next-line)
         ("gk" . evil-previous-line)

         ("] e" . flymake-goto-next-error)
         ("[ e" . flymake-goto-prev-error)

         ("C-." . embark-act)

         :map evil-insert-state-map
         ("C-y" . yank))

  :custom
  (evil-undo-system 'undo-redo)

  :config
  (dolist (mode '(Info-mode
                  help-mode
                  shortdoc-mode
                  flymake-mode-buffer-mode
                  flymake-project-diagnostics-mode
                  flymake-diagnostics-buffer-mode
                  occur-mode
                  grep-mode
                  forge-pullreq-mode
                  xeft-mode
                  xref--xref-buffer-mode
                  diff-mode
                  dired-mode
                  wdired-mode
                  Man-mode
                  image-mode
                  doc-view-mode
                  term-mode
                  eshell-mode
                  shell-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Redefine <tab> to `org-cycle' in Normal State in Org mode.
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  ;; Make word motions operate as symbol motions as well as make the * and #
  ;; searches use symbols instead of words.
  ;;
  ;; Explanation. An underscore _ is a word character in Vim. This means that
  ;; word motions like w skip over underlines in a sequence of letters as if it
  ;; was a letter itself. In contrast, in Evil the underscore is often a
  ;; non-word character like operators, e.g. +. Thus aliasing word to symbol
  ;; (for Evil) should make all motions and text objects implemented in terms of
  ;; symbols.
  ;;
  ;; For more details, see
  ;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; Make consecutive calls of forward-page work in Evil mode.
  (defun evil-forward-page-nudge (arg)
    "Move forward by a character if ARG > 0 and Evil mode is enabled.

Moving forward by a character is required if we want consecutive
calls to `forward-page' work as in Emacs or insert state. This is
dictated by the difference between cursor position in some of
Vim (Evil) modes and in Emacs: before the character and after the
character correspondingly."
    (when (and evil-mode (> arg 0))
      ;; Move forward by the length of `page-delimeter'
      (forward-char (+ (length page-delimiter)))))
  (advice-add 'forward-page :after #'evil-forward-page-nudge))

;; Evil-escape allows to map a chord for escape.
(use-package evil-escape
  :after evil
  :hook
  (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk"))


;; Breadcrumb — headerline indication of where you are in a large project.
;; Local file, since the plugin is not in Melpa.
(use-package breadcrumb
  :hook prog-mode)


;; Exec-path-from-shell — A GNU Emacs library to ensure environment variables
;; inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


;; Consult — Consult provides search and navigation commands based.
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; TODO: decide if I want this.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)


;; Orderless — Completion style that divides the pattern into space-separated
;; components, and matches candidates that match all of the components in any
;; order. Each component can match in any one of several ways: literally, as a
;; regexp, as an initialism, in the flex style, or as multiple word prefixes. By
;; default, regexp and literal matches are enabled.
(use-package orderless
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Embark — it's sort of a contextual menu for an object similar to the one you
;; find in other GUI programs that popups when you right click on something. But
;; Embark is a bit more than simple contextual menu.
;;
;; The "target" (for the contextual menu) depends on the case: it can be the
;; current item in a completion list, the symbol at point, or some URL at point.
;; Your conduit to this mode of operation is the `embark-act' command, which you
;; should bind to a convienient key ("C-." in my case).
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding (also, specify
                                ;; the binding for `embark-act' in :bind
                                ;; sections of packages with wich you want an
                                ;; integration, e.g. Evil, Icomplete, etc.)

   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark consult integration.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Wgrep — allows you to edit a grep buffer and apply those changes to the file
;; buffer like sed interactively. No need to learn sed script, just learn Emacs.
(use-package wgrep
  :commands (wgrep-toggle-readonly-area))


;; Envrc.el — buffer-local direnv integration for Emacs.

;; NOTE: The documentation states the following.
;; "It's probably wise to load envrc.el late in your startup sequence: you
;; normally want envrc-mode to be initialized in each buffer before other minor
;; modes like flycheck-mode which might look for executable.
;; Counter-intuitively, this means that envrc-global-mode should be enabled
;; after other global minor modes, since each prepends itself to various hooks."

;; I'm not sure how use-packages should be used in this case. But it seems that
;; I do not enable any other global minor modes that can interfere.
(use-package envrc
  ;; Only load envrc if direnv is installed.
  :if (executable-find "direnv")
  :config
  ;; Enable envrc globally. There is a local minor mode envrc-mode, but you
  ;; should not try to enable this granularly, e.g. for certain modes or
  ;; projects, because compilation and other buffers might not get set up with
  ;; the right environment.
  (envrc-global-mode))
;; ---------------------------------------------------------------------------
