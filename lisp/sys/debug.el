;;; -*- lexical-binding: t -*-
;;

;;; debug.el --- Description

(setq gdb-many-windows t
      gdb-use-separate-io-buffer t)

(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

(defconst gud-window-register 123456)

(defun gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

(add-hook 'gud-mode-hook
          (lambda ()
            (gud-tooltip-mode)
            (window-configuration-to-register gud-window-register)
            (local-set-key (kbd "C-q") 'gud-quit)))

(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (jump-to-register gud-window-register)
                (bury-buffer))))


(provide 'emx-debug)
