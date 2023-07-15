;; Font size
(set-face-attribute 'default nil :height 120)

;; Spellchecker
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict/en_US.multi")
(use-package ispell)
