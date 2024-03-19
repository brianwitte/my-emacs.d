;;; -*- lexical-binding: t -*-
;;
;;; python.el --- Description

(use-package pyenv-mode
  :straight t
  :config
  (pyenv-mode)
)
; install lsp mode
(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(provide 'emx-python)
