;;; -*- lexical-binding: t -*-
;;;
;;; org.el --- Description

(defun setup-org-mode-keys ()
  (my-local-leader-def
    :keymaps 'org-mode-map
    "o a" 'org-agenda))

(setup-org-mode-keys)

(provide 'emx-org)
