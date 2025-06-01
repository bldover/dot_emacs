;;; init.el --- Initialization file -*- lexical-binding: t -*-
;;; Commentary:
;;  File that executes at Emacs startup

;;; Code:

;; --- One time installs ---
;; (nerd-icons-install-fonts) ; used by doom-modeline

;; --- Package management ---
(require 'package)
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setopt use-package-always-ensure t)

;; --- Emacs behavior ---
(setopt native-comp-async-report-warnings-errors 'silent)
(setopt gc-cons-threshold 100000000)
(setopt inhibit-startup-screen t)
(setopt custom-file "~/.emacs.d/init-custom.el")
(setq-default read-process-output-max (* 1024 1024)) ;; 1MB
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-emacs)

;; History tracking
(recentf-mode t)
(setopt recentf-max-saved-items 500)
(savehist-mode t)
(setopt history-length 500)
(setopt history-delete-duplicates t)
(setopt savehist-save-minibuffer-history t)
(setopt savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Backup and autosave file locations
(setopt backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setopt auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t)))

;; Unmodified buffers should automatically reload from disk
(use-package autorevert
  :config
  (global-auto-revert-mode 1))

;; Projectile - project management
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'helm-projectile)
  :config
  (projectile-mode 1)
  (helm-projectile-on))


;; Improved help documentation
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h o" . helpful-symbol)
  ("C-h C-a" . helpful-at-point))

;; --- UI setup ---
;; Window size
(add-to-list 'initial-frame-alist '(width . 88))
(add-to-list 'default-frame-alist '(width . 88))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Window navigation
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :custom
  (aw-keys '(?s ?n ?t ?h ?u ?e ?o ?a)))

;; --- UI elements ---
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setopt column-number-mode t)
(setopt visible-bell t)

;; Font variable vs fixed widths
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  (doom-themes-visual-bell-config t)
  :config
  (load-theme 'doom-snazzy t))

;; --- Modeline ---
(display-time)

;; modeline themes
;; (use-package spaceline
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-helm-mode))
(use-package doom-modeline
  :demand t
  :init
  (doom-modeline-mode 1))

;; --- Tabline ---
(use-package tab-bar
  :demand t
  :after doom-modeline ;; so we inherit the theme
  :bind ("C-x t g" . tab-select)
  :custom-face
  (tab-bar ((t (:inherit mode-line))))
  (tab-bar-tab ((t (:inherit mode-line :foreground "white"))))
  (tab-bar-tab-inactive ((t (:inherit mode-line-inactive :foreground "black"))))
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :config
  (tab-bar-mode 1))

;; --- Minibuffer ---
(fset 'yes-or-no-p 'y-or-n-p)
(setopt completion-styles '(basic partial-completion flex))

;; Which key - show possible key completions
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;; Helm
;; Consider vertico, marginalia, consult, embark, orderless instead ?
(use-package helm
  :defer 5
  ;; :defines
  ;; helm-command-prefix-key
  ;; helm-google-suggest-use-curl-p
  ;; helm-M-x-fuzzy-match
  ;; helm-buffers-fuzzy-matching
  ;; helm-recentf-fuzzy-match
  ;; helm-semantic-fuzzy-match
  ;; helm-imenu-fuzzy-match
  ;; helm-locate-fuzzy-match
  ;; helm-apropos-fuzzy-match
  ;; helm-lisp-fuzzy-completion
  ;; helm-move-to-line-cycle-in-source
  ;; helm-ff-search-library-in-sexp
  ;; helm-ff-file-name-history-use-recentf
  :init
  (setopt helm-command-prefix-key "C-c h")
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-h a" . helm-apropos)
   ("C-h SPC" . helm-all-mark-rings)
   (:map helm-command-map
         ("x" . helm-register)
         ("o" . helm-occur)
         ("C-o" . helm-outline)))
  :custom
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-locate-fuzzy-match t)
  (helm-apropos-fuzzy-match t)
  (helm-lisp-fuzzy-completion t)
  (helm-scroll-amount 8)
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-M-x-show-short-doc t)
  :config
  (global-unset-key (kbd "C-x c"))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-mode 1))

;; --- Editor visuals ---
(setopt show-paren-mode t)
(setopt word-wrap t)
(setopt cursor-type 'box)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Line numbering
(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start t)
  :config
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-exempt-modes '(vterm-mode
                                            eshell-mode
                                            shell-mode
                                            term-mode
                                            ansi-term-mode
                                            org-mode))
  (defun disable-line-numbers-mode ()
    (display-line-numbers-mode 0))
  (dolist (mode display-line-numbers-exempt-modes)
    (add-hook 'mode 'disable-line-numbers-mode)))

;; --- Editor behavior ---
(delete-selection-mode 1)
(xclip-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setopt indent-tabs-mode nil)
(setopt tab-width 4)
(setq-default indent-line-function 'indent-relative)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Scroll one line at a time (less "jumpy" than defaults)
(setopt mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setopt mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setopt mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setopt scroll-step 1) ;; keyboard scroll one line at a time

;; Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; Flycheck - autolinter
(use-package flycheck
  :after
  (web-mode)
  :functions
  (flycheck-add-mode)
  :custom
  (flycheck-idle-change-delay 0.5)
  (flycheck-check-syntax-automatically '(save
										 idle-change
										 mode-enabled))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (global-flycheck-mode))

;; ktlint https://github.com/pinterest/ktlint
(use-package flycheck-kotlin
  :hook
  (kotlin-mode . flycheck-mode)
  :config
  (flycheck-kotlin-setup))

;; Eldoc - Documention in buffer
(use-package eldoc
  :after
  (web-mode typescript-mode)
  :hook
  ((typescript-ts-mode . eldoc-mode)
   (tsx-ts-mode . eldoc-mode)
   (js-ts-mode . eldoc-mode)
   (web-mode . eldoc-mode)))

;; --- Completions ---
;; yasnippet - completions
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Company - autocomplete
(use-package company
  :defines lsp-completion-provider backend-with-yas
  :custom
  (company-tooltip-align-annotations t)
  (lsp-completion-provider :capf)
  :config
  (defvar enable-yas t "Enable yasnippet for all backends.")
  (defun backend-with-yas (backend)
	(if (or (not enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setopt company-backends (mapcar #'backend-with-yas company-backends))
  (global-company-mode))

;; prettier frontend for company
(use-package company-box
  :after
  (company)
  :hook (company-mode . company-box-mode))

;; defun stolen from gptel
(defun claude-api-key-from-auth-source ()
  "Lookup Claude api key in the auth source."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host "api.anthropic.com"
                    :user "apikey"
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No Claude API key found in the auth source")))

;; minuet - in-buffer AI suggestions
;; really slow, need to tune the parameters
(use-package minuet
  :bind
  (("C-c a m m" . #'minuet-complete-with-minibuffer)
   ("C-c a m s" . #'minuet-show-suggestion)
   ("C-c a m c" . #'minuet-configure-provider)

   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("M-A" . #'minuet-accept-suggestion)
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))
  :init
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :custom
  (minuet-provider 'claude)
  (minuet-context-window 16000)
  (minuet-context-ratio 85)
  (minuet-request-timeout 30)
  :config
  (plist-put minuet-claude-options :api-key 'claude-api-key-from-auth-source)
  (plist-put minuet-claude-options :model "claude-3-5-haiku-20241022")
  (plist-put minuet-claude-options :max_tokens 512))

;; LLM - gptel
(use-package gptel
  :bind
  (("C-c a g s" . gptel-send)
   ("C-c a g m" . gptel-menu)
   ("C-c a g b" . gptel)
   ("C-c a g r" . gptel-rewrite)
   ("C-c a g c d" . gptel-context-add)
   ("C-c a g c f" . gptel-context-add-file)
   ("C-c a g c b" . gptel--infix-context-add-buffer)
   ("C-c a g c r" . gptel--infix-context-add-region))
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  :config
  ;; default backend
  ;;   a bug in gptel causes this to fail due to recursive requires if set
  ;;   using custom -- https://github.com/karthink/gptel/issues/556
  (setq gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key gptel-api-key)) ;; .authinfo source
  ;; extra backends
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:32b)))

;; --- Keybindings ---
;; Movement keybinds
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-o") "\C-u\-1\C-x\o") ; reverse window switch

;; Comments
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-x M-;") 'comment-dwim)

;; Find and replace
(global-set-key (kbd "M-s r") 'replace-string)
(global-set-key (kbd "M-s C-r") 'replace-regexp)

;; Execute in shell keybinds; useful for curling
(defun shell-region (start end)
  "Execute region in an inferior shell."
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))
(defun shell-region-and-jsonify-output (start end)
  "Execute region in an inferior shell and format output as JSON."
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end))
  (other-window -1)
  (json-ts-mode)
  (json-pretty-print-buffer-ordered))
(global-set-key (kbd "C-c M-|") 'shell-region)
(global-set-key (kbd "C-c C-M-|") 'shell-region-and-jsonify-output) ; doesn't work on terminal due to key interpretation

;; Quickly open scratch buffer
(defun switch-to-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-c b s") 'switch-to-scratch-buffer)

;; Lisp eval rebinds - make keymap?
(define-key global-map (kbd "C-x C-e") 'eval-expression) ; redefine from eval-last-sexp
(define-key emacs-lisp-mode-map (kbd "C-c C-e") nil)     ; was eval-region-or-buffer
(define-key emacs-lisp-mode-map (kbd "C-M-x") nil)       ; was eval-defun
(define-key emacs-lisp-mode-map (kbd "C-x C-e e") 'eval-expression)
(define-key emacs-lisp-mode-map (kbd "C-x C-e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-x C-e s") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-x C-e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-x C-e d") 'eval-defun)

;; --- Version Control ---
;; Magit
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c g d" . magit-dispatch)
   ("C-c g f" . magit-file-dispatch)))

;; --- Org Mode ---
;; Org mode prettify
;; partially stolen from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(use-package org
  :hook
  (org-mode . org-indent-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-fontify-todo-headline t)
  :custom-face
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info ((t (:foreground "dark orange"))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-level-1 ((t (:inherit (bold variable-width) :extend nil :foreground "#4f97d7" :height 1.3))))
  (org-link ((t (:foreground "royal blue" :underline t))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-value ((t (:inherit fixed-pitch))) t)
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  (variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
  (fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

;; Org mode better lists
(use-package org-autolist
  :hook
  (org-mode . org-autolist-mode))

;; automatically map prog-modes to the ts-mode variant if available
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; --- Programming Languages ---
;; HTML
(use-package html-ts-mode
  :custom (html-indent-level 4))

;; CSS
(use-package css-mode
  :custom (css-indent-level 4))

;; Javascript
(use-package js
  :custom (js-indent-level 4))

;; Typescript
(use-package typescript-ts-mode
  :custom (typescript-ts-mode-indent-offset 4))

;; JSON
(use-package json-ts-mode
  :custom (json-ts-mode-indent-offset 4))

;; Shell mode
(defun shell-mode-hook-setup ()
  "Set up `shell-mode'."
  (setq-local company-backends '((company-files company-native-complete)))
  ;; `company-native-complete' is better than `completion-at-point'
  ;; @see https://github.com/redguardtoo/emacs.d/issues/882
  (setq-local company-idle-delay 2))
(add-hook 'shell-mode-hook 'shell-mode-hook-setup)

;; Rust
(use-package rust-mode)

;; Go
(defun go-config ()
  (setq-local compile-command "go install")
  (add-hook 'before-save-hook 'lsp-organize-imports nil t))

(use-package go-ts-mode
  :hook
  (go-ts-mode . go-config)
  (go-mod-ts-mode . go-config)
  :defines go-ts-mode-indent-offset
  :custom
  (go-ts-mode-indent-offset 4))

;; --- LSP ---
(use-package lsp-mode
  :functions lsp-register-custom-settings
  :init
  (setopt lsp-keymap-prefix "C-c l")
  :hook
  ((java-ts-mode . lsp-deferred)
   (java-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (go-dot-mod-mode . lsp-deferred)
   (go-ts-mode . lsp-deferred)
   (go-mod-ts-mode . lsp-deferred)
   (kotlin-mode . lsp-deferred) ;; https://github.com/fwcd/kotlin-language-server
   (js-ts-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (tsx-ts-mode . lsp-deferred)
   (html-ts-mode . lsp-deferred)
   (css-ts-mode . lsp-deferred)
   (json-ts-mode . lsp-deferred)
   (web-mode . lsp-deferred))
  :custom
  (lsp-format-buffer-on-save t)
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui)
(use-package helm-lsp)
(use-package lsp-treemacs)

(use-package lsp-java
  :defines lsp-java-java-path lsp-java-configuration-runtimes
  :init
  (setopt lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java")
  :custom
  (lsp-java-configuration-runtimes '[(:name "JavaSE-11"
                                            :path
                                            "/usr/lib/jvm/java-11-openjdk-amd64"
                                            :default t)
    								 (:name "JavaSE-1.8"
                                            :path
                                            "/usr/lib/jvm/java-8-openjdk-amd64")]))
;; (use-package lsp-java-boot)

;; --- Debugging ---
;; (setq debug-on-error t)
;; trace-function -> func-name

;; Dap debugger
(use-package dap-mode)
;; (use-package dap-dlv-go
;;   :after dap-mode)
;; (use-package dap-java
;;   :after lsp-java)

;; --- Customize ---
(load custom-file)

;; --- System-specific configs ---
(when (string-equal "gnu/linux" system-type)
  (load-file "~/.emacs.d/wsl-init.el"))

(when (string-equal "windows-nt" system-type)
  (load-file "~/.emacs.d/win-init.el"))

(provide 'init)

;;; init.el ends here
