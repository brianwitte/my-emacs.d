;;; -*- lexical-binding: t -*-
;;;
;;; base/evil.el --- Description


(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)  ; required if I start using which-key
  :config
  (evil-mode 1))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :after evil)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(provide 'emx-evil)
