;;; install-pkgs.el --- First-time package installation

;;; Commentary:

;; This file is intended to be used as an all-in-one installation utility
;; when using Emacs in a new environment.  It will install all packages
;; required by init.el

;; Which packages are installed can be modified by updating \"package-list\"

;; It might be useful to expose this at some point to call it from
;; init.el or another config file.

;;; Code:

(unless package-archive-contents
  (package-refresh-contents))

;; List to modify to change which packages are installed
(let ((package-list '(package
                      use-package
                      spacemacs-theme
                      rust-mode
                      json-mode
                      tide
                      web-mode
                      typescript-mode
                      magit
                      yasnippet
                      helm
                      company
                      company-box
                      flycheck
                      ispell
                      multiple-cursors
                      expand-region
                      xclip
                      org-autolist
                      org-bullets
                      projectile
                      helm-projectile
                      tree-sitter-langs
                      lsp-mode
                      lsp-ui
                      lsp-treemacs
                      helm-lsp
                      dap-mode
                      lsp-java)))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;; install-pkgs.el ends here
