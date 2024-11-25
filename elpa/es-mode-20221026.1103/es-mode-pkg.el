;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "es-mode" "20221026.1103"
  "A major mode for editing and executing Elasticsearch queries."
  '((dash    "2.11.0")
    (cl-lib  "0.5")
    (spark   "1.0")
    (s       "1.11.0")
    (request "0.3.0"))
  :url "https://github.com/dakrone/es-mode"
  :commit "e82465fd785688bb58918ea62ca4de06a2a23a1e"
  :revdesc "e82465fd7856"
  :keywords '("elasticsearch")
  :authors '(("Lee Hinman" . "lee@writequit.org"))
  :maintainers '(("Lee Hinman" . "lee@writequit.org")))
