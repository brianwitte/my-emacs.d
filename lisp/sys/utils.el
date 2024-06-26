;;; -*- lexical-binding: t -*-
;;

(use-package vterm
  :straight t
  )

(use-package multi-vterm
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "ot" 'multi-vterm)
  )

(use-package emamux
  :straight t
  )

(use-package restclient
  :straight t)

(provide 'emx-utils)
