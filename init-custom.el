;;; init-custom.el --- Customize initialization file -*- lexical-binding: t -*-
;;; Commentary:
;;  File that executes at Emacs startup for variables set with the customize interface

;;; Code:
(print 'custom-init)
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
   '(company-box diminish doom-modeline doom-themes expand-region
				 flycheck-kotlin go-projectile gptel helm-ag helm-lsp
				 helm-projectile helm-rg helpful info+ json-mode
				 kotlin-mode kotlin-ts-mode lsp-java lsp-ui magit
				 marginalia minuet mixed-pitch multiple-cursors
				 org-autolist org-bullets rust-mode spaceline
				 spaceline-config spacemacs-theme tide
				 tree-sitter-langs typescript-mode web-mode xclip
				 yasnippet)))
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

;;; init-custom.el ends here
