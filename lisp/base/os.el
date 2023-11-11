;;; -*- lexical-binding: t -*-
;;;
;;; os.el --- Description

(use-package exec-path-from-shell
  :straight t
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package sudo-edit
  :straight t
  :commands (sudo-edit))

(provide 'emx-os)
