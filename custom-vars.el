;; ---------------------------------------------------------------------------
;; Use Easy Customization to tweak theme.
;; (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; )
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(info-menu-star ((t (:foreground "red"))))
 ;; '(mode-line-highlight ((t (:foreground "dark orange"))))
 ;; )
;; ---------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/Users/snich/org/agenda.org"))
 '(package-selected-packages
   '(lsp-ui lsp-mode inf-ruby evil-matchit rubocop gptel jinx rspec-mode rbenv es-mode ef-themes olivetti swift-mode embark-consult embark orderless org-modern closql emacsql f ghub jsonrpc xref pdf-tools jsonnet-mode ox-gfm wgrep breadcrumb doom-modeline parrot dirvish diredfl nerd-icons vertico consult forge exec-path-from-shell xeft eglot python-black dockerfile-mode yaml-mode org-inline-anim org-fragtog plz evil-escape evil project cape envrc markdown-mode org engrave-faces expand-region ess org-cliplink htmlize auctex cdlatex magit corfu hl-todo dumb-jump use-package minions which-key modus-themes marginalia nyan-mode))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")))
 '(safe-local-variable-values
   '((lsp-enabled-clients quote ruby-lsp-ls)
     (lsp-enabled-clients . ruby-lsp-ls)
     (python-black-extra-args "--line-length" "120")
     (eval add-to-list 'tramp-remote-path "/home/ubuntu_ssm/.cache/pypoetry/virtualenvs/guide-ml-research-_5OzJJ7u-py3.10/bin/")))
 '(tetris-buffer-name "*config*")
 '(tetris-x-colors
   [[0.392 0.584 0.941]
    [0.7 0 1]
    [1 1 0]
    [1 0 1]
    [0 1 1]
    [0 1 0]
    [1 0 0]])
 '(warning-suppress-types '((org-element-cache))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:background "#000000" :foreground "#000000"))))
 '(term-color-green ((t (:background "#00AE00" :foreground "#00AE00"))))
 '(term-color-yellow ((t :foreground "light goldenrod" :background "light goldenrod"))))
