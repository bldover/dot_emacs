;; Debug
;; (setq debug-on-error t)

;; Debugging commands
;; trace-function -> func-name

;; Package management
(setq native-comp-async-report-warnings-errors :silent)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(require 'use-package)
(setq use-package-always-demand t)

;; Basics
(setq-default visible-bell 1)
(setq-default show-paren-mode 1)
(setq-default word-wrap t)
(setq-default column-number-mode t)
(setq use-dialog-box nil)
(setq-default cursor-type 'box)
(setq inhibit-startup-screen t)
(setq completion-styles '(flex))
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time)
(scroll-bar-mode -1)
(show-paren-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(xclip-mode 1)
(which-key-mode)
(setq gc-cons-threshold 100000000)

;; Enable non-default commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Execute region in shell
(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

(defun shell-region-and-jsonify-output (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end))
  (other-window -1)
  (json-mode)
  (json-pretty-print-buffer-ordered))

;; Movement keybindings
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-o") "\C-u\-1\C-x\o") ; reverse window switch

;; Execute in shell keybinds; useful for curling
(global-set-key (kbd "C-c M-|") 'shell-region)
(global-set-key (kbd "C-c C-M-|") 'shell-region-and-jsonify-output) ; doesn't work on terminal due to key interpretation

;; Lisp eval rebinds - make keymap?
(define-key global-map (kbd "C-x C-e") 'eval-expression) ; redefine from eval-last-sexp
(define-key emacs-lisp-mode-map (kbd "C-c C-e") nil)     ; was eval-region-or-buffer
(define-key emacs-lisp-mode-map (kbd "C-M-x") nil)       ; was eval-defun
(define-key emacs-lisp-mode-map (kbd "C-x C-e e") 'eval-expression)
(define-key emacs-lisp-mode-map (kbd "C-x C-e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-x C-e s") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-x C-e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-x C-e d") 'eval-defun)

;; Easily open scratch buffer ; doesn't work :(
(defun switch-to-scratch-buffer ()
  (switch-to-buffer (get-scratch-buffer-create)))
;; (global-set-key (kbd "C-c b s") 'switch-to-scratch-buffer)

;; Tabbing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Buffer history
(savehist-mode 1)
(setq history-length 20)

;; Unmodified buffers should automatically reload from disk
(use-package autorevert
  :config
  (global-auto-revert-mode 1))

;; Line numbering
(use-package display-line-numbers
  :defines
  (display-line-numbers-type display-line-numbers-exempt-modes)
  :config
  ;;   (defcustom display-line-numbers-exempt-modes
  ;;     '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-mode)
  ;;     "Major modes on which to disable line numbers."
  ;;     :group 'display-line-numbers
  ;;     :type 'list
  ;;     :version "green")

  ;;   (defun display-line-numbers--turn-on ()
  ;;     "Turn on line numbers except for certain major modes.
  ;; Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  ;;     (unless (or (minibufferp)
  ;;                 (member major-mode display-line-numbers-exempt-modes))
  ;;       (display-line-numbers-mode)))
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-width-start t))

;; Variable vs fixed widths
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Backup and autosave file locations
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t)))

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default scroll-step 1) ;; keyboard scroll one line at a time

;; Window setup
;; (add-to-list 'initial-frame-alist '(width . 88))
;; (add-to-list 'default-frame-alist '(width . 88))
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))

;; Helm
(use-package helm
  :defines
  helm-command-prefix-key helm-google-suggest-use-curl-p helm-M-x-fuzzy-match
  helm-buffers-fuzzy-matching helm-recentf-fuzzy-match helm-semantic-fuzzy-match
  helm-imenu-fuzzy-match helm-locate-fuzzy-match helm-apropos-fuzzy-match
  helm-lisp-fuzzy-completion helm-move-to-line-cycle-in-source helm-ff-search-library-in-sexp
  helm-ff-file-name-history-use-recentf
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-c h o" . helm-occur)
   ("C-c h x" . helm-register)
   ("C-h SPC" . helm-all-mark-rings))
  :init
  (setq helm-command-prefix-key "C-c h")
  :config
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-ff-file-name-history-use-recentf t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-mode 1))

;; Projectile
(use-package projectile
  :bind
  (("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Magit
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c g" . magit-dispatch)
   ("C-c f" . magit-file-dispatch)
   ))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("M-p" . mc/mark-previous-like-this)
   ("M-n" . mc/mark-next-like-this)
   ("C-c m a" . mc/mark-all-like-this)
   ("C-c m d" . mc/mark-all-dwim)
   ("C-c m l" . mc/edit-lines)))

;; Expand region
(use-package expand-region
  :bind
  ("M-m" . er/expand-region))


;; Org mode prettify
;; partially stolen from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(use-package org
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160))))))

(use-package org-indent
  :hook
  (org-mode . org-indent-mode))

;; Org mode better lists
(use-package org-autolist
  :hook
  (org-mode . org-autolist-mode))

(use-package typescript-mode)

;; Web - TSX major mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;; Company - autocomplete
(use-package company
  :defines lsp-completion-provider
  :config
  (setq company-tooltip-align-annotations t)
  (setq lsp-completion-provider :capf)
  (global-company-mode)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; Company front-end - prettier box
(use-package company-box
  :after
  (company)
  :hook (company-mode . company-box-mode))

;; Flycheck - autolinter
(use-package flycheck
  :after
  (web-mode)
  :functions
  (flycheck-add-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (global-flycheck-mode))

;; ktlint https://github.com/pinterest/ktlint
(use-package flycheck-kotlin
  :config (flycheck-kotlin-setup))

;; Eldoc - Documention in buffer
(use-package eldoc
  :after
  (web-mode typescript-mode)
  :hook
  ((typescript-mode . eldoc-mode)
   (tsx-ts-mode . eldoc-mode)
   (web-mode . eldoc-mode)))

;; Treesitter Grammars
(use-package tree-sitter-langs)
(tree-sitter-require 'bash)
(tree-sitter-require 'c)
(tree-sitter-require 'cmake)
(tree-sitter-require 'cpp)
(tree-sitter-require 'css)
(tree-sitter-require 'elisp)
(tree-sitter-require 'go)
(tree-sitter-require 'html)
(tree-sitter-require 'java)
(tree-sitter-require 'javascript)
(tree-sitter-require 'json)
(tree-sitter-require 'kotlin)
(tree-sitter-require 'make)
(tree-sitter-require 'markdown)
(tree-sitter-require 'python)
(tree-sitter-require 'rust)
(tree-sitter-require 'sql)
(tree-sitter-require 'toml)
(tree-sitter-require 'tsx)
(tree-sitter-require 'typescript)
(tree-sitter-require 'yaml)
;; (add-to-list 'load-path "~/.emacs.d/treesitter/")

;; Use ts modes
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-dot-mod-mode . go-mod-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; not good do something else idk
(defun init-json-mode-indent ()
  (setq js-indent-level 4))

;; json mode
(use-package json-mode)

;; lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((java-ts-mode . lsp-deferred)
   (java-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (go-dot-mod-mode . lsp-deferred)
   (go-ts-mode . lsp-deferred)
   (go-mod-ts-mode . lsp-deferred)
   (kotlin-mode . lsp-deferred)) ;; https://github.com/fwcd/kotlin-language-server
  :commands lsp lsp-deferred
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :defines lsp-java-java-path lsp-java-configuration-runtimes
  :init
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java")
  :config
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-11"
                                                 :path
                                                 "/usr/lib/jvm/java-11-openjdk-amd64"
                                                 :default t)
    			                          (:name "JavaSE-1.8"
                                                 :path
                                                 "/usr/lib/jvm/java-8-openjdk-amd64")])
  )

;; (use-package lsp-java-boot)

;; go mode
(defun go-config ()
  (setq-local compile-command "go install")
  (setq-local indent-tabs-mode t)
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (add-hook 'before-save-hook 'lsp-organize-imports nil t))

(use-package go-ts-mode
  :hook
  (go-ts-mode . go-config)
  (go-mod-ts-mode . go-config)
  :defines go-ts-mode-indent-offset
  :config
  (setq go-ts-mode-indent-offset 4))


;; dap mode debugger
(use-package dap-mode)
(use-package dap-java)
(use-package dap-dlv-go)

;; Javascript Mode
(use-package js
  :hook
  (json-mode . init-json-mode-indent)
  :config
  (setq js-indent-level 2))

;; Tide - Typescript
(defun setup-tide ()
  (tide-setup)
  (tide-hl-identifier-mode t)
  (setq tab-width 2))

(use-package tide
  :after
  (typescript-mode web-mode company eldoc)
  :bind
  ("C-%" . tide-rename-symbol)
  :hook
  ((typescript-mode . setup-tide)
   (tsx-ts-mode . setup-tide)
   (web-mode . setup-tide)))

;; Shell mode
(defun shell-mode-hook-setup ()
  "Set up `shell-mode'."

  (setq-local company-backends '((company-files company-native-complete)))
  ;; `company-native-complete' is better than `completion-at-point'
  (local-set-key (kbd "TAB") 'company-complete)

  ;; @see https://github.com/redguardtoo/emacs.d/issues/882
  (setq-local company-idle-delay 1))

(add-hook 'shell-mode-hook 'shell-mode-hook-setup)

;; Rust
(autoload 'rust-mode "rust-mode" nil t)

;; LLM - gptel
(use-package gptel
  :bind
  (("C-c a s" . gptel-send)
   ("C-c a m" . gptel-menu)
   ("C-c a b" . gptel)
   ("C-c a r" . gptel-rewrite))
  :config
  (setq ;; default backend
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key gptel-api-key)) ;; .authinfo source
  ;; extra backends
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:32b)))

;; System-specific configs
(when (string-equal "gnu/linux" system-type)
  (load-file "~/.emacs.d/wsl-init.el"))

(when (string-equal "windows-nt" system-type)
  (load-file "~/.emacs.d/win-init.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c7ee965589b6049e4d0fd811eb9f61f6d9b212d1af27bf13721b937de7def9ba"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac"
     default))
 '(org-fontify-todo-headline t)
 '(package-selected-packages
   '(company company-box dap-mode expand-region flycheck-kotlin go-mode
             go-projectile gptel helm helm-ag helm-lsp helm-projectile
             helm-rg json-mode kotlin-mode kotlin-ts-mode lsp-go
             lsp-java lsp-treemacs lsp-ui magit mixed-pitch
             multiple-cursors org-autolist org-bullets projectile
             rust-mode spacemacs-theme tide tree-sitter-langs
             typescript-mode web-mode xclip yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit (bold variable-width) :extend nil :foreground "#4f97d7" :height 1.3))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))
