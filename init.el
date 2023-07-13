; Package management
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

(tool-bar-mode 0)
(scroll-bar-mode -1)
(show-paren-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)

(display-time)

(fset 'yes-or-no-p 'y-or-n-p)

;; Tabbing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq indent-line-function 'insert-tab)

;; Font size
(set-face-attribute 'default nil :height 120)

;; Buffer history
(savehist-mode 1)
(setq history-length 20)

;; Unmodified buffers should automatically reload from disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Line numbering
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;; Backup and autosave file locations
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t)))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'spacemacs-dark t)

;; Spellchecker
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict/en_US.multi")
(use-package ispell)

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Rust Mode
(autoload 'rust-mode "rust-mode" nil t)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Some keybindings
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-g") 'goto-line)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq-default scroll-step 1) ;; keyboard scroll one line at a time

;; Account for size of gutter and fringes
(add-to-list 'initial-frame-alist '(width . 88))
(add-to-list 'default-frame-alist '(width . 88))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)
         ))

;; Typescript
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq tab-width 2))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;;(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)

;; BQ SQL
(add-to-list 'load-path
             "~/.emacs.d/plugins/bigquery-mode")
(use-package sql-indent)
(use-package bigquery-mode)
(use-package bqm-names)

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-<" . mc/edit-lines)
         ))

;; Expand Region
(use-package expand-region
  :bind ("M-m" . er/expand-region))

;; Org Mode Bullets
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("b1141a3ef7cc05b835906998a85ff6720194a67b2c24772e057a44a1d7b31579" "fdede5d990532aab1913a232d6efa8b33c21e7cd5ba19153fd21e41ddc508a9a" "2713006a5288b4d89a0b31ea884caca55bfd9ca0a0cc0fc2ef129e90818b9f62" "863aada9cdfb23601603b629cb9c86fc2c7a1104410bb63e7e2210cf563e988b" "906db9cfd526eb24babe19db6d4eb0dd5eb513f2653bd3aaa3112219e709ca3e" "7ef2884658a1fed818a11854c232511fa25721c60083a2695e6ea34ce14777ee" default))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(rust-mode yasnippet magit org-autolist org-bullets sql-indent expand-region multiple-cursors spacemacs-theme web-mode flycheck tide use-package company))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#000000")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-module 2)
 '(visual-line-fringe-indicators '(left-curly-arrow nil))
 '(weechat-color-list
   (unspecified "#000000" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
