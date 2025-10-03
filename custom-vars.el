(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("modified files" (visiting-file) (modified))
     ("programming"
      (or (derived-mode . prog-mode) (mode . ess-mode) (mode . compilation-mode)))
     ("text document" (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX"
      (or (derived-mode . tex-mode) (mode . latex-mode) (mode . context-mode)
          (mode . ams-tex-mode) (mode . bibtex-mode)))
     ("web"
      (or (derived-mode . sgml-mode) (derived-mode . css-base-mode)
          (derived-mode . js-base-mode) (derived-mode . typescript-ts-base-mode)
          (mode . js2-mode) (derived-mode . haml-mode) (mode . sass-mode)))
     ("gnus"
      (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode)
          (mode . gnus-summary-mode) (mode . gnus-article-mode)))))
 '(org-agenda-files '("/Users/snich/org/agenda.org"))
 '(package-selected-packages
   '(auctex breadcrumb cape cdlatex closql consult corfu diredfl dirvish
            dockerfile-mode doom-modeline dumb-jump ef-themes eglot emacsql
            embark embark-consult engrave-faces envrc es-mode ess evil
            evil-escape evil-matchit exec-path-from-shell expand-region f forge
            ghub gptel hl-todo htmlize inf-ruby jinx jsonnet-mode jsonrpc
            lsp-mode lsp-ui magit marginalia markdown-mode minions modus-themes
            nerd-icons nyan-mode olivetti orderless org org-cliplink org-fragtog
            org-inline-anim org-modern ox-gfm parrot pdf-tools plz project
            python-black rbenv rspec-mode rubocop swift-mode use-package vertico
            vterm wgrep which-key xeft xref yaml-mode))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")))
 '(tetris-buffer-name "*config*")
 '(tetris-x-colors
   [[0.392 0.584 0.941] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rbenv-active-ruby-face ((t (:inherit warning))))
 '(term-color-black ((t (:background "#000000" :foreground "#000000"))))
 '(term-color-green ((t (:background "#00AE00" :foreground "#00AE00"))))
 '(term-color-yellow ((t :foreground "light goldenrod" :background "light goldenrod"))))
