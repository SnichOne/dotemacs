;; NOTE: Do not disable menu bar for the time being to see if it's useful.
;; It's not disabled by default in the "prelude" distribution.
;; (menu-bar-mode -1)

;; Unless emacs-mac is used, disable tool bar since it's just several buttons
;; for the most common operations but takes too much space.
(unless (boundp 'mac-carbon-version-string) (tool-bar-mode -1))
;; (tool-bar-mode -1)

;; Disable scroll bar to save screen space and since it hinders the ability
;; to resize windows using mouse: it's not allowed to resize the window by
;; clicking and dragging on the scroll bar, only clicking and dragging on the
;; minibuffer border works (tested in Emacs 28.1).
(scroll-bar-mode -1)

;; Maximize/fullscreen Emacs on startup.
;; There are two ways to do it: using 'default-frame-alist' or
;; 'initial-frame-alist':
;; 1. Maximize all frames.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 2. Should only the first frame be maximized, use 'initial-frame-alist',
;; ((un)comment the following(previous) line)
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set default font on macOS
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono-12")))


;; Garbage collector https://gitlab.com/koral/gcmh trick
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'gcmh)
(gcmh-mode 1)



;; Fix for Native Comp (AOT) linker errors on macOS GUI launch
(let ((brew-prefix "/opt/homebrew/bin"))
  (when (file-directory-p brew-prefix)
    (setenv "PATH" (concat brew-prefix ":" (getenv "PATH")))
    (add-to-list 'exec-path brew-prefix)))


;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
