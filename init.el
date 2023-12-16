;; Debug
;; (setq debug-on-error t)

;; Debugging commands
;; trace-function -> func-name

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
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

;; Convenience keybindings
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-o") "\C-u\-1\C-x\o") ; reverse window switch

;; Execute in shell keybinds; useful for curling
(global-set-key (kbd "C-c M-|") 'shell-region)
(global-set-key (kbd "C-c C-M-|") 'shell-region-and-jsonify-output) ; doesn't work on terminal

;; Easily open scratch buffer ; doesn't work
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
  (("C-c m p" . mc/mark-previous-like-this)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m a" . mc/mark-all-like-this)
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
  (global-company-mode))

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

;; Eldoc - Documention in buffer
(use-package eldoc
  :after
  (web-mode typescript-mode)
  :hook
  ((typescript-mode . eldoc-mode)
   (tsx-ts-mode . eldoc-mode)
   (web-mode . eldoc-mode)))

;; Treesitter Grammars
(add-to-list 'load-path "~/.emacs.d/treesitter/")

;; Use ts modes
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))

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
   (java-mode . lsp-deferred))
  :commands lsp lsp-deferred)

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

;; dap mode debugger
(use-package dap-mode)
(use-package dap-java)

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
 '(org-fontify-todo-headline t)
 '(package-selected-packages
   '(lsp-java dap-mode helm-lsp lsp-treemacs lsp-ui lsp-mode helm-projectile projectile helm org-bullets json-mode company-box mixed-pitch org-autolist typescript-mode xclip expand-region multiple-cursors web-mode company tide magit rust-mode yasnippet spacemacs-theme)))
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
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))
