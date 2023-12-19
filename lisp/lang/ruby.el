;;; -*- lexical-binding: t -*-
;;

;;; ruby.el --- Description

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((ruby-mode . lsp-deferred))
  :init
  (setq lsp-enable-snippet nil  ;; Disable snippets
        lsp-enable-symbol-highlighting nil  ;; Disable symbol highlighting
        lsp-enable-text-document-color nil  ;; Disable text document color
        lsp-enable-on-type-formatting nil  ;; Disable auto formatting
        lsp-enable-indentation nil  ;; Disable indentation
        lsp-diagnostics-provider :none))  ;; Disable diagnostics

(use-package rspec-mode
  :straight t
  :defer t
  :init
  ;; Autoload RSpec minor mode in ruby buffers
  (add-hook 'ruby-mode-hook 'rspec-mode)
  :config
  ;; Use rspec instead of rake
  (setq rspec-use-rake-when-possible nil)
  ;; Configurations, keybindings, etc.
  )

(use-package reformatter
  :straight t)

(use-package inf-ruby
  :straight t)

(use-package rbenv
  :straight t
  :config
  (global-rbenv-mode))

(defun rufo-success-p (retcode)
  (member retcode '(0 3)))

(reformatter-define rufo
		    :program "rufo"
		    :lighter " Rufo"
		    :exit-code-success-p rufo-success-p)

(defun my-inf-ruby-console ()
  "Open inf-ruby-console and return its buffer."
  (inf-ruby-console-auto)
  (get-buffer "*inf-ruby*"))

(defun my-ruby-scratch-buffer ()
  "Open or get a Ruby scratch buffer."
  (or (get-buffer "*ruby-scratch*")
      (generate-new-buffer "*ruby-scratch*")))

(defun my-rails-console ()
  "Open Rails console."
  (interactive)
  (projectile-rails-console)
  (get-buffer "*Rails*"))

(provide 'emx-ruby)
