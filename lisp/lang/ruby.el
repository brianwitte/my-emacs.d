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
  :straight t
  )

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

(defun my-setup-ruby-environment ()
  "Setup Ruby or Rails interactive environment."
  (interactive)
  (let ((inf-ruby-buf (my-inf-ruby-console))
        (ruby-scratch-buf (my-ruby-scratch-buffer)))
    (delete-other-windows)
    (split-window-below)
    (set-window-buffer (selected-window) inf-ruby-buf)
    (set-window-dedicated-p (selected-window) t)
    (read-only-mode 1)
    (other-window 1)
    (set-window-buffer (selected-window) ruby-scratch-buf)
    (ruby-mode)
    (local-set-key (kbd "C-c C-c") 'my-send-region-to-inf-ruby)))

(defun my-send-region-to-inf-ruby ()
  "Send selected region to *inf-ruby* buffer."
  (interactive)
  (let ((code (buffer-substring (region-beginning) (region-end))))
    (comint-send-string (get-buffer-process "*inf-ruby*") (concat code "\n"))))

(defun my-rails-console ()
  "Open Rails console."
  (interactive)
  (projectile-rails-console)
  (get-buffer "*Rails*"))

(defun my-setup-rails-environment ()
  "Setup Rails interactive environment."
  (interactive)
  (let ((rails-buf (my-rails-console))
        (ruby-scratch-buf (my-ruby-scratch-buffer)))
    (delete-other-windows)
    (split-window-below)
    (set-window-buffer (selected-window) rails-buf)
    (set-window-dedicated-p (selected-window) t)
    (read-only-mode 1)
    (other-window 1)
    (set-window-buffer (selected-window) ruby-scratch-buf)
    (ruby-mode)
    (local-set-key (kbd "C-c C-c") 'my-send-region-to-inf-ruby)))

(provide 'emx-ruby)
