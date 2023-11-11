;;; -*- lexical-binding: t -*-
;;

(use-package vterm
  :straight t
  :config
  (my-leader-def
    :keymaps 'normal
    "ot" 'vterm))

(provide 'emx-utils)
