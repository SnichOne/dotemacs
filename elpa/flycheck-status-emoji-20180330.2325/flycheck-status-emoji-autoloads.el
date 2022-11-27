;;; flycheck-status-emoji-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-status-emoji" "flycheck-status-emoji.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-status-emoji.el

(defvar flycheck-status-emoji-mode nil "\
Non-nil if Flycheck-Status-Emoji mode is enabled.
See the `flycheck-status-emoji-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flycheck-status-emoji-mode'.")

(custom-autoload 'flycheck-status-emoji-mode "flycheck-status-emoji" nil)

(autoload 'flycheck-status-emoji-mode "flycheck-status-emoji" "\
Toggle Flycheck status emoji mode.

This is a minor mode.  If called interactively, toggle the
`Flycheck-Status-Emoji mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='flycheck-status-emoji-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode; any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, while `toggle' toggles the state.

When enabled, this mode replaces the standard Flycheck mode-line
status indicators with cute, compact emoji that convey the
corresponding information.  For example, a buffer shows status
“😔” while being checked, then “😱” to report errors, “😟” to report
warnings, or “😌” if no problems were found.

See <https://github.com/liblit/flycheck-status-emoji#readme> for
additional documentation.  Visit
<https://github.com/liblit/flycheck-status-emoji/issues> or use
command `flycheck-status-emoji-submit-bug-report' to report bugs
or offer suggestions for improvement.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "flycheck-status-emoji" '("flycheck-status-emoji-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-status-emoji-autoloads.el ends here
