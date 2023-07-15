(setq package-list '(package use-package spacemacs-theme ispell yasnippet rust-mode magit
                     tide company flycheck web-mode multiple-cursors expand-region xclip))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (cpp "https://github.com/uyha/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (regex "https://github.com/tree-sitter/tree-sitter-regex")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (scala "https://github.com/tree-sitter/tree-sitter-scala")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq treesit-grammar-list '(c css elisp go html java javascript json python rust toml
                             tsx typescript yaml))


(dolist (grammar treesit-grammar-list)
  (treesit-install-language-grammar grammar)))
