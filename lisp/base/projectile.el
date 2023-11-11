;;; -*- lexical-binding: t -*-
;;;
;;; core.el --- Description

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  )

(provide 'emx-projectile)
