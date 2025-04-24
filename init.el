;;; init.el --- Initialization file -*- lexical-binding: t -*-
;;; Commentary:
;;  File that executes at Emacs startup

;;; Code:

;; --- Package management ---
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --- Emacs behavior ---
(setq-default native-comp-async-report-warnings-errors :silent)
(setq gc-cons-threshold 100000000)
 '(inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/init-custom.el")
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-emacs)

;; History tracking
(recentf-mode t)
(setq-default recentf-max-saved-items 500)
(savehist-mode t)
(setq-default history-length 500)
(setq-default history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history t)
(setq-default savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Backup and autosave file locations
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t)))

;; Unmodified buffers should automatically reload from disk
(use-package autorevert
  :config (global-auto-revert-mode 1))

;; Projectile - project management
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq-default projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq-default projectile-switch-project-action 'helm-projectile))

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
;; GUI window setup ---
(add-to-list 'initial-frame-alist '(width . 88))
(add-to-list 'default-frame-alist '(width . 88))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; --- UI elements ---
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default column-number-mode t)
(setq-default visible-bell t)

;; Font variable vs fixed widths
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :config
  (setq-default doom-themes-enable-italic nil)
  (setq-default doom-themes-visual-bell-config t)
  (load-theme 'doom-snazzy t))

;; --- Editor visuals ---
(setq-default show-paren-mode t)
(setq-default word-wrap t)
(setq-default cursor-type 'box)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Line numbering
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode 1)
  (setq-default display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width-start t)
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


;; --- Modeline ---
(display-time)

;; modeline themes
;; (use-package spaceline
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-helm-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; --- Minibuffer ---
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default completion-styles '(basic flex))

;; Which key - show possible key completions
(use-package which-key
  :init (which-key-mode)
  :config
  ;; should set before init WK mode?
  (setq-default which-key-idle-delay 0.5))

;; Helm
;; Consider vertico, marginalia, consult, embark, orderless instead ?
(use-package helm
  :defer 5
  :defines
  helm-command-prefix-key
  helm-google-suggest-use-curl-p
  helm-M-x-fuzzy-match
  helm-buffers-fuzzy-matching
  helm-recentf-fuzzy-match
  helm-semantic-fuzzy-match
  helm-imenu-fuzzy-match
  helm-locate-fuzzy-match
  helm-apropos-fuzzy-match
  helm-lisp-fuzzy-completion
  helm-move-to-line-cycle-in-source
  helm-ff-search-library-in-sexp
  helm-ff-file-name-history-use-recentf
  :init
  (setq-default helm-command-prefix-key "C-c h")
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
  :config
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "curl")
    (setq-default helm-google-suggest-use-curl-p t))
  (setq-default helm-M-x-fuzzy-match t)
  (setq-default helm-buffers-fuzzy-matching t)
  (setq-default helm-recentf-fuzzy-match t)
  (setq-default helm-semantic-fuzzy-match t)
  (setq-default helm-imenu-fuzzy-match t)
  (setq-default helm-locate-fuzzy-match t)
  (setq-default helm-apropos-fuzzy-match t)
  (setq-default helm-lisp-fuzzy-completion t)
  (setq-default helm-scroll-amount 8)
  (setq-default helm-move-to-line-cycle-in-source t)
  (setq-default helm-ff-search-library-in-sexp t)
  (setq-default helm-ff-file-name-history-use-recentf t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (setq helm-M-x-show-short-doc t)
  (helm-mode 1))

;; --- Editor behavior ---
(delete-selection-mode 1)
(xclip-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default indent-line-function 'indent-relative)

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
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (global-flycheck-mode))

;; ktlint https://github.com/pinterest/ktlint
(use-package flycheck-kotlin
  :hook (kotlin-mode . flycheck-mode)
  :config (flycheck-kotlin-setup))

;; Eldoc - Documention in buffer
(use-package eldoc
  :after
  (web-mode typescript-mode)
  :hook
  ((typescript-mode . eldoc-mode)
   (tsx-ts-mode . eldoc-mode)
   (web-mode . eldoc-mode)))

;; --- Completions ---
;; yasnippet - completions
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Company - autocomplete
(use-package company
  :defines lsp-completion-provider backend-with-yas
  :config
  (setq company-tooltip-align-annotations t)
  (setq lsp-completion-provider :capf)
  (global-company-mode)
  (defvar enable-yas t
    "Enable yasnippet for all backends.")
  (defun backend-with-yas (backend)
    (if (or (not enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'backend-with-yas company-backends)))
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
    :config
    (setq minuet-provider 'claude)
    (setq minuet-context-window 16000)
    (setq minuet-context-ratio 85)
    (setq minuet-request-timeout 30)

    (plist-put minuet-claude-options :api-key 'claude-api-key-from-auth-source)
    (plist-put minuet-claude-options :model "claude-3-5-haiku-20241022")
    (plist-put minuet-claude-options :max_tokens 512))

;; LLM - gptel
(use-package gptel
  :bind
  (("C-c a g s" . gptel-send)
   ("C-c a g m" . gptel-menu)
   ("C-c a g b" . gptel)
   ("C-c a g r" . gptel-rewrite))
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

;; --- Keybindings ---
;; Movement keybinds
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-o") "\C-u\-1\C-x\o") ; reverse window switch

;; Commenting
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-x M-;") 'comment-dwim)

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
  (json-mode)
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

;; Org mode better lists
(use-package org-autolist
  :hook
  (org-mode . org-autolist-mode))

;; --- Treesitter ---
;; Grammer installation
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

;; --- Programming Languages ---
;; Javascript/Typescript
(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;; Javascript Mode
(use-package js
  :config
  (setq js-indent-level 2))

;; Tide - Typescript
(defun setup-tide ()
  (tide-setup)
  (tide-hl-identifier-mode t)
  (setq tab-width 2))
(use-package typescript-mode)
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
  ;; @see https://github.com/redguardtoo/emacs.d/issues/882
  (setq-local company-idle-delay 2))
(add-hook 'shell-mode-hook 'shell-mode-hook-setup)

;; Rust
(use-package rust-mode)

;; Go
(defun go-config ()
  (setq-local compile-command "go install")
  (setq-local lsp-format-buffer-on-save t)
  (add-hook 'before-save-hook 'lsp-organize-imports nil t))
(use-package go-ts-mode
  :hook
  (go-ts-mode . go-config)
  (go-mod-ts-mode . go-config)
  :defines go-ts-mode-indent-offset
  :config
  (setq go-ts-mode-indent-offset 4))

;; JSON
;; not good do something else idk
(defun init-json-mode-indent ()
  (setq-default js-indent-level 4))
(use-package json-mode
  :hook (json-mode . init-json-mode-indent))

;; --- LSP ---
(use-package lsp-mode
  :defines
  lsp-register-custom-settings
  :init
  (setq-default lsp-keymap-prefix "C-c l")
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
  ()
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
