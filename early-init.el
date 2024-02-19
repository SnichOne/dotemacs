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
