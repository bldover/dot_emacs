;;; install-pkgs.el --- First-time package installation

;;; Commentary:

;; This file is intended to be used as an all-in-one installation utility
;; when using Emacs in a new environment.  It will install all packages
;; required by init.el along with a number of common treesitter language
;; grammars.

;; Which packages and grammars are installed can be modified by updating
;; local variables \"package-list\" and \"treesit-grammar-list\", respectively.

;; It might be useful to expose these lists at some point to call these
;; functions from init.el or another config file.

;;; Code:

(unless package-archive-contents
  (package-refresh-contents))

;; List to modify to change which packages are installed
(let ((package-list '(package use-package spacemacs-theme
                              rust-mode json-mode
                              tide web-mode typescript-mode
                              magit yasnippet helm
                              company company-box flycheck ispell
                              multiple-cursors expand-region xclip
                              org-autolist org-bullets
                              projectile helm-projectile
                              lsp-mode lsp-ui lsp-treemacs helm-lsp dap-mode
                              lsp-java)))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))




(defun already-installed (grammar)
  "Check if the language grammar for GRAMMAR has been previously installed.

It will check for both Windows (.dll) or Linux installations (.so).

Returns t if the grammar exists, nil otherwise"
  (when (or (file-exists-p (get-file-path grammar ".so"))
            (file-exists-p (get-file-path grammar ".dll")))
    t))

(defun get-file-path (grammar ext)
  "Build the file path to the installation location of the grammar GRAMMAR.

The function assumes grammars are installed to the default tree-sitter directory
in the folder specified by the variable `user-emacs-directory'.

The EXT extension should have the \".\" included"
  (expand-file-name (concat "tree-sitter/libtree-sitter-" (prin1-to-string grammar) ext) user-emacs-directory))

(let ((treesit-grammar-list '(c css elisp go html java javascript json python sql
                                rust toml tsx typescript yaml))
      (treesit-language-source-alist
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
         (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
         (toml "https://github.com/tree-sitter/tree-sitter-toml")
         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
         (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
  (dolist (grammar treesit-grammar-list)
    (unless (already-installed grammar)
      (message "Installing treesitter grammar for %s" grammar)
      (treesit-install-language-grammar grammar))))

;;; install-pkgs.el ends here
