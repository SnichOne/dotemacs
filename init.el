;; The file is divided by sections enclosed by lines of dashes.

;; General rule: avoid using 'setq', 'setq-default' and use
;; 'customize-set-variable' for variables defined using 'defcustom'.
;;
;; Rationale: 'customize-set-variable' triggers setter method which does all
;; needed initialization, and this method seems to be better integrated with
;; Easy Customization UI, e.g. you can see all customized variables with the
;; 'customized-unsaved' command.


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

;; Better isearch defaults. Isearch stands for incremental search. This means
;; that search results are updated and highlighted while you are typing your
;; query, incrementally.
(customize-set-variable 'lazy-highlight-initial-delay 0)
;; I tested scroll while isearch is active, it works awfully and doesn't let you
;; to scroll more than a screen away from the active match. Do not enable it.
;; (customize-set-variable 'isearch-allow-scroll nil)
;; Highlighting full buffer does not make a lot of sense without scroll.
;; (customize-set-variable 'lazy-highlight-buffer t)

;; Even if you avoid using the customization UI, some settings may cause
;; customization variables to be added to your init.el file. Let's change that
;; and move customization variables to a separate file and load it.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Emacs basic look.

;; TODO Do not disable menu bar for the time being to see if it's useful. It's
;; not disabled by default in the "prelude" distribution.
;; (menu-bar-mode -1)

;; Disable tool bar since it's just several buttons for the most common
;; operations but takes too much space.
(tool-bar-mode -1)

;; Disable scroll bar to save screen space and since it hinders the ability to
;; resize windows using mouse: it's not allowed to resize the window by clicking
;; and dragging on the scroll bar, only clicking and dragging on the minibuffer
;; border works (tested in Emacs 28.1).
(scroll-bar-mode -1)

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
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in programming modes. Not
;; 'global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want line numbering.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Show the end of buffer with a special glyph in the left fringe.
(setq-mode-local text-mode indicate-empty-lines t)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Buffer management.

;; Bind IBuffer to 'C-x C-b', the shortcut executes `list-buffer` by default but
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
;; Minibuffer.

;; Minibuffer completion.
;; Enable fido-mode. It's really just icomplete with slightly different defaults
;; that emulate ido mode as close as possible.
(fido-mode 1)
;; Make Fido/Icomplete mode display the possible completions on the same line as
;; the prompt by default.
(icomplete-vertical-mode 1)

;; Display the default argument as ‘[DEFAULT-ARG]’ instead of ‘(default
;; DEFAULT-ARG)’, saving some screen space.
;;
;; Note: use 'customize-set-variable' or plac before enabling
;; 'minibuffer-electric-default-mode'.
(customize-set-variable 'minibuffer-eldef-shorten-default t)

;; Hide the default argument as soon as you modify the contents of the
;; minibuffer (since typing <RET> would no longer submit that default).
(minibuffer-electric-default-mode 1)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Editing settings.

;; Set column beyond which automatic line-wrapping should happen (e.g. by
;; hitting M-q).
(customize-set-variable 'fill-column 80)

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
(add-hook 'text-mode-hook #'flyspell-mode)
;; Enable Flyspell Prog mode in comments in Prog mode. Flyspell Prog mode only
;; checks words in comments and string constants.
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Important configuration of flyspell: you need to disable the M-<TAB> (C-M-i,
;; <ESC> <TAB>) binding for flyspell, because it's already used for the
;; 'complete-symbol' or 'ispell-complete-word' commands, e.g. this binding
;; overrides 'complete-symbol' in org-mode.
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
;; When Auto Revert mode a buffer that is under version control, it updates the
;; version control information in the mode line. However, Auto Revert mode may
;; not properly update this information if the version control status changes
;; without changes to the work file, from outside the current Emacs session. If
;; you set ‘auto-revert-check-vc-info’ to ‘t’, Auto Revert mode updates the
;; version control status information every ‘auto-revert-interval’ seconds, even
;; if the work file itself is unchanged. The resulting CPU usage depends on the
;; version control system, but is usually not excessive.
;; (customize-set-variable 'auto-revert-check-vc-info t)

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
;; Note. 'cfg:reverse-input-method' causes Emacs to open the *Quail Completions*
;; buffer, let's auto-close it.
(kill-buffer "*Quail Completions*")
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Org mode settings.

;; Fontify (e.g., highlight with a background color) the whole line for
;; headings. Looks nice with the leuven theme.
;; (customize-set-variable 'org-fontify-whole-heading-line t)

;; Display the buffer in the indented view, this also hides leading heading
;; stars, only one star (the rightmost) at each heading is visible, the rest are
;; masked with the same font color as background.
(customize-set-variable 'org-startup-indented t)

;; Customize org-M-RET-may-split-line to make M-RET not split the line. Doom
;; Emacs sets it to nil by default.
;; Source: https://orgmode.org/manual/Structure-Editing.html#index-M_002dRET.
(customize-set-variable 'org-M-RET-may-split-line nil)

;; Scale LaTeX preview.
;; Alternative to customize-set-variable one can use plist-put
;; (plist-put org-format-latex-options :scale 1.5)
(customize-set-variable 'org-format-latex-options
                        '( :foreground default
                           :background default
                           :scale 1.5
                           :html-foreground "Black"
                           :html-background "Transparent"
                           :html-scale 1.0
                           :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Enable syntax highlighting for latex fragments.
;; Note: I didn't understood
(customize-set-variable 'org-highlight-latex-and-related '(native))

;; Add by default additional LaTeX packages for Russian language support.
;; (require 'org)
;; (add-to-list 'org-latex-packages-alist '("" "cmap" t))
;; (add-to-list 'org-latex-packages-alist '("english,russian" "babel" t))

;; Org mode sets 'truncate-lines' to 't', so each line of text has just one
;; scree line. But there is a problem with it: you can be left with horizontal
;; scroll after you invoke 'org-fill-paragraph' on a long line and you will have
;; to manually scroll to adjust the view. Let's change 'auto-hscholl-mode' to
;; f'current-line' locally to 'org-mode' to avoid the problem.
;;
;; BTW, maybe instead of setting 'auto-hscroll-mode' to 'current-line' it's
;; better to remap org-fill-paragraph to scroll horizontally to left after
;; filling the paragraph.
(setq-mode-local org-mode auto-hscroll-mode 'current-line)
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

;; Initialize package manager.
;; Following commands are taken from the tutorial:
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
(setq use-package-always-ensure t)


;; Display scroll via nyan cat. Alternative — the 'poke-line' package.
(use-package nyan-mode
  :defer 1
  :config (nyan-mode 1))


;; Enable rich annotations using the Marginalia package.
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer.
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
;;
;; TODO, modify the 'shadow' face, to make org properties (e.g., lines like
;; "#+title: ...") distinguishable from comments.
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes.

  ;; Adjust modus-vivendi colors.
  (setq modus-themes-vivendi-color-overrides
        '((fg-main . "#f0fff0")))       ; Default face is white on black, dim white a bit.

  (setq modus-themes-mode-line '(borderless accented)
        modus-themes-syntax '(yellow-comments))

  ;; Load the theme files before enabling a theme.
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi).
  :bind ("<f5>" . modus-themes-toggle))


;; Enable the minor mode for Emacs that displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings). This includes
;; prefixes like C-x 8 which are shown in a different face.
(use-package which-key
  :defer 2
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode))


;; Collapse minor modes in modeline.
;; Probably will switch to doom-modeline, if I ever use evil-mode.
(use-package minions
  :config
  (setq minions-mode-line-lighter ";")
  :defer 1
  :config
  (minions-mode 1))


;; Convenient completion popup.
(use-package company
  :defer 1
  :commands (completion-at-point)
  :bind ( :map prog-mode-map
          ("<tab>" . company-indent-or-complete-common)
          :map company-active-map
          ("<tab>" . company-complete-common-or-cycle)
          ("<backtab>" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)         ; Default is 0.2.
  ;; There is company-tng-mode which behaves similar to Vim YCM or coc, but not
  ;; exactly: when you cycle through with TAB it outputs "virtual" arguments
  ;; which disappear when you start typing anyhting.
  ;; (company-tng-mode)
  (global-company-mode))


;; Enable Flycheck globally. Check for errors on the fly. Flycheck has better
;; integration with lsp-mode than built-in Flymake, e.g. lsp-ui sideline does
;; not show diagnostics with Flymake.
(use-package flycheck
  :defer 1
  :config (global-flycheck-mode))

;; LSP support.
(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "s-l", "C-c l").
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)) ; shows keymap names, e.g. +goto, +refactor, etc.
  :commands (lsp lsp-deferred))

;; Optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;; Evil — Vim emulation.
;; (use-package evil
;;   :init
;;   ;; Evil uses "C-z" and "C-M-z" to switch to Emacs state, hence unbind "C-z"
;;   ;; which is bound to 'suspend-emacs' by default.
;;   (global-unset-key (kbd "C-z"))
;;   (evil-mode))
;; ---------------------------------------------------------------------------
