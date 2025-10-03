;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250720.1322"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "1.0.0")
    (magit-section "4.3.8")
    (seq           "2.24")
    (transient     "0.9.3")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "acf71f7eb422c37a83d30673df88c66166a44e96"
  :revdesc "acf71f7eb422"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
