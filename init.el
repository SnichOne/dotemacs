;; The file is divided by sections enclosed by lines of dashes.

;; General rule: avoid using 'setq', 'setq-default' and use
;; 'customize-set-variable' for variables defined using 'defcustom'.
;;
;; Rationale: 'customize-set-variable' triggers setter method which does all
;; needed initialization, and this method seems to be better integrated with
;; Easy Customization UI, e.g. you can see all customized variables with the
;; 'customized-unsaved' command.

;; TODO:
;; - rewrite use-package :config to :custom where applicable.

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

;; Show eldoc (documentation in minibuffer) as soon as possible without any
;; delay.
(customize-set-variable 'eldoc-idle-delay 0)

;; Even if you avoid using the customization UI, some settings may cause
;; customization variables to be added to your init.el file. Let's change that
;; and move customization variables to a separate file and load it.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Emacs basic look.

;; Disable scroll bar, scroll bar is nice but for some reason Emacs does not
;; allow you to resize the window when you try to drag the window by scroll bar,
;; so you have to resize only by dragging the area on modeline.
;; (scroll-bar-mode -1)

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

;; Show the end of buffer with a special glyph in the left fringe.
(setq-mode-local text-mode indicate-empty-lines t)

;; Make calendar begin weeks on Monday.
(customize-set-variable 'calendar-week-start-day 1)

;; Scroll horizontally with touchpad.
;; Correct left-right scroll direction for OS X.
(customize-set-variable 'mouse-wheel-flip-direction t)
;; Enable left-right scroll from trackpad.
(customize-set-variable 'mouse-wheel-tilt-scroll t)
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
;; When Auto Revert mode a buffer that is under version control, it updates the
;; version control information in the mode line. However, Auto Revert mode may
;; not properly update this information if the version control status changes
;; without changes to the work file, from outside the current Emacs session. If
;; you set ‘auto-revert-check-vc-info’ to ‘t’, Auto Revert mode updates the
;; version control status information every ‘auto-revert-interval’ seconds, even
;; if the work file itself is unchanged. The resulting CPU usage depends on the
;; version control system, but is usually not excessive.
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
;; that is interface to version control systems).
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
;; Note. 'cfg:reverse-input-method' causes Emacs to open the *Quail Completions*
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
(require 'bind-key)
(setq use-package-always-ensure t)


;; Display scroll via nyan cat. Alternative — the 'poke-line' package.
(use-package nyan-mode
  :custom
  (nyan-mode t))


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
;; TODO: modify the 'shadow' face, to make org properties (e.g., lines like
;; "#+title: ...") distinguishable from comments.
(use-package modus-themes
  :demand
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
  ;; `modus-themes-preset-overrides-faint`
  ;; and `modus-themes-preset-overrides-intense`
  (setq modus-themes-common-palette-overrides
        '(;; remove mode line border
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; remove fringe background
          (fringe unspecified)

          ;; make org properties and attributes (e.g., lines like "#+title: ...")
          ;; distinguishable from comments.
          (prose-metadata cyan-warmer)
          (prose-metadata-value magenta)))
          ;; (comment yellow-faint)
          ;; (string green-warmer)))

  ;; Load the theme of your choice:
  (load-theme 'modus-operandi :no-confirm) ;; OR (load-theme 'modus-vivendi :no-confirm).
  :bind ("<f5>" . modus-themes-toggle))


;; Enable the minor mode for Emacs that displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings). This includes
;; prefixes like C-x 8 which are shown in a different face.
(use-package which-key
  :defer 1
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode))


;; Collapse all minor modes in modeline, but whitelist flycheck and flymake.
;; Probably will switch to doom-modeline, if I ever use evil-mode.
(use-package minions
  :defer 1
  :config
  (setq minions-mode-line-lighter ";")
  (setq minions-prominent-modes '(flycheck-mode flymake-mode))
  (minions-mode 1))


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
;; TODO: configure word completion in Org mode, e.g., `ispell-complete-word`.
(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :custom
  ;; TAB-and-Go completion customizations
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  ;; TODO: corfu-preselect-first will be deprecated.
  (corfu-preselect-first nil)
  ;; Display candidate documentation or source in a popup next to the candidate
  ;; menu.
  (corfu-popupinfo-mode t)
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :config
  (setq tab-always-indent 'complete))


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
(use-package flymake
  :ensure nil
  :hook prog-mode
  :bind (("C-c [ e" . flymake-goto-prev-error)
         ("C-c ] e" . flymake-goto-next-error)
         ("C-c e b" . flymake-show-buffer-diagnostics)
         ("C-c e p" . flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; LSP support.
(use-package eglot
  :custom
  ;; This stops eglot from logging the json events of lsp server.
  (eglot-events-buffer-size 0)
  :hook (python-mode . eglot-ensure))
;; (use-package eldoc-box
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))


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


;; Org mode (use built-in version).
(use-package org
  :ensure nil

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

  ;; Enable syntax highlighting for latex fragments.
  ;; Note: I didn't understood
  (org-highlight-latex-and-related '(native))

  ;; Prevent inadvertently edit an invisible part of the buffer and be confused
  ;; on what has been edited and how to undo the mistake. Setting
  ;; 'org-catch-invisible-edits' to 'error' makes Emacs throw an error and do
  ;; nothing when inserting or deleting a character in an invisible region.
  (org-catch-invisible-edits 'error)

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
  ;; 1. life.org task template,
  ;; 2. life_journal.org daily plan template: datetree structure,
  ;; 3. work.org task template,
  ;; 4. work_journal.org daily plan template: datetree structure.
  ;; TODO ⮷
  ;; 5. book template
  ;; 6. movie template
  (org-capture-templates
   '(("l" "Life")
     ("lt" "Life task" entry (file "life.org")
      "* TODO %^{Name}\n\n%?"
      :empty-lines 1
      :prepend t)
     ("li" "Life daily plan (checkbox items)" checkitem (file+olp+datetree "life_journal.org")
      nil
      :empty-lines 1)

     ("w" "Work")
     ("wt" "Work task" entry (file "work.org")
      "* TODO %^{Name}\n\n%?"
      :empty-lines 1
      :prepend t)
     ("wi" "Work daily plan (checkbox items)" checkitem (file+olp+datetree "life_journal.org")
      nil
      :empty-lines 1)))

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
  ;; NOTE: One case use "#+ATTR_ORG: :width", and leave other "#+ATTR.*#
  ;; keywords for their primal purpose.
  (org-image-actual-width nil)

  ;; Configure how much levels should be exported as a headline, inferior levels
  ;; will usually produce itemize or enumerate lists when exported, but back-end
  ;; behavior may differ.
  (org-export-headline-levels 7)

  :bind
  ;; For a better experience, the three Org commands ‘org-store-link’,
  ;; ‘org-capture’ and ‘org-agenda’ ought to be accessible anywhere in Emacs,
  ;; not just in Org buffers. To that effect, you need to bind them to globally
  ;; available keys.
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link))

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
  (advice-add 'org-fill-paragraph :after #'scroll-right)

  ;; Add by default additional LaTeX packages for Russian language support.
  ;; Note: I changed my mind, these settings better should be better placed
  ;; using headers per file basis.
  ;; (add-to-list 'org-latex-packages-alist '("" "cmap" t))
  ;; (add-to-list 'org-latex-packages-alist '("english,russian" "babel" t))
)

;; Org-roam
;; (use-package org-roam
;;   :custom
;;   (org-roam-directory "~/org/roam notes/"))

;; Xeft. Full-text search for notes. Based on Xapian.
(use-package xeft
  :commands xeft
  :custom
  ;; Set path to notes.
  (xeft-directory "~/org/")
  ;; Set path to Xeft database.
  (xeft-database "~/.config/xeft")
  ;; Ignore not org files.
  (xeft-ignore-extension '("iimg" "png" "html" "pdf" "tex" "log"))
  ;; Make Xeft search recursively, by default search works only for first level
  ;; files in `xeft-directory`
  (xeft-recursive t))


;; Dired (built-in file manager).
(use-package dired
  :ensure nil
  :custom
  ;; Make Dired show directories first, works on Linux, MacOS.
  (dired-listing-switches "-al --group-directories-first")
  :hook
  ;; Hide details: file properties, owner, size, modified time.
  (dired-mode . dired-hide-details-mode))


;; Magit. Complete text-based user interface to Git.
;; "A Git Porcelain inside Emacs".
(use-package magit
  :commands (magit-status magit-project-status)
  :custom
  (magit-view-git-manual-method 'man)
  :init
  ;; Project.el integration: bind `magit-project-status` to "m" in the project
  ;; switch menu (when hitting `C-x p p`).
  ;; NOTE: Magit already has such integration, but it's placed in `magit-extras.el`
  ;; and it is not executed until `magit-extras` is evaluated. I decided to take
  ;; the following two lines from `magit-extras` and put it here, in the :init
  ;; block.
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))


;; Evil — Vim emulation.
;; (use-package evil
;;   :init
;;   ;; Evil uses "C-z" and "C-M-z" to switch to Emacs state, hence unbind "C-z"
;;   ;; which is bound to 'suspend-emacs' by default.
;;   (global-unset-key (kbd "C-z"))
;;   :custom
;;   ;; Set default Evil state to "emacs".
;;   (evil-default-state 'emacs)
;;   (evil-motion-state-modes nil)
;;   (evil-insert-state-modes nil)
;;   :config
;;   (evil-mode 1))
;; ---------------------------------------------------------------------------
