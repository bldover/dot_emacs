;; Font size
(set-face-attribute 'default nil :height 210)
(set-face-attribute 'mode-line nil :height 160)
(normal-erase-is-backspace-mode 0)

(use-package xclip
  :config
  (xclip-mode 1))
