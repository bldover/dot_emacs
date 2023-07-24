;; Debug
(setq debug-on-error t)

;; Debugging commands
;; trace-function -> func-name

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

;; Basics
(setq-default visible-bell 1)
(setq-default show-paren-mode 1)
(setq-default word-wrap t)
(setq-default column-number-mode t)
(setq use-dialog-box nil)
(setq-default cursor-type 'box)
(setq inhibit-startup-screen t)
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

;; Some keybindings
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x M-o") "\C-u\-1\C-x\o") ; reverse window switch
(global-set-key (kbd "C-c M-|") 'shell-region)
(global-set-key (kbd "C-c C-M-|") 'shell-region-and-jsonify-output)

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
(add-to-list 'initial-frame-alist '(width . 88))
(add-to-list 'default-frame-alist '(width . 88))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))

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
  (("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-<" . mc/edit-lines)
   ))

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
  :config
  (setq company-tooltip-align-annotations t)
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

;; not good do something else idk
(defun init-json-mode-indent ()
  (setq js-indent-level 4))

;; json mode
(use-package json-mode)

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
  (typescript-mode web-mode company flycheck eldoc)
  :bind
  ("C-%" . tide-rename-symbol)
  :hook
  ((typescript-mode . setup-tide)
   (tsx-ts-mode . setup-tide)
   (web-mode . setup-tide)))

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
   '(json-mode company-box mixed-pitch org-autolist typescript-mode xclip expand-region multiple-cursors web-mode company tide magit rust-mode yasnippet spacemacs-theme)))
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
