;;; -*- lexical-binding: t -*-
;;

;;; ruby.el --- Description

;;(use-package lsp-mode
;;  :straight t
;;  :commands (lsp lsp-deferred)
;;  :hook ((ruby-mode . lsp-deferred))
;;  :init
;;  (setq lsp-enable-snippet nil  ;; Disable snippets
;;        lsp-enable-symbol-highlighting nil  ;; Disable symbol highlighting
;;        lsp-enable-text-document-color nil  ;; Disable text document color
;;        lsp-enable-on-type-formatting nil  ;; Disable auto formatting
;;        lsp-enable-indentation nil  ;; Disable indentation
;;        lsp-diagnostics-provider :none))  ;; Disable diagnostics


(use-package inf-ruby
  :straight t
  :config
  (defun inf-ruby-start-pry ()
    "Run an inferior Ruby process with Pry in a buffer.
If there is a Ruby process running in an existing buffer with Pry, switch
to that buffer. Otherwise create a new buffer with Pry."
    (interactive)
    (let* ((impl "pry")
           (command (cdr (assoc impl inf-ruby-implementations))))
      (run-ruby command impl))))


(use-package robe
  :straight t
  :config
  (defun my-ruby-mode-robe-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(ruby-mode-map robe-mode-map)
      ;; from ruby-mode
      " i e" #'ruby-indent-exp
      " s d" #'smie-down-list
      " b b" #'ruby-beginning-of-block
      " b e" #'ruby-end-of-block
      " b t" #'ruby-toggle-block
      " b q" #'ruby-toggle-string-quotes
      " f l" #'ruby-find-library-file

      ;; from robe-mode
      " g"   #'robe-jump
      " d"   #'robe-doc
      " r r" #'robe-rails-refresh

      ;; from inf-ruby
      " e d" #'ruby-send-definition
      ;;#'ruby-send-definition-and-go
      " e l" #'ruby-send-last-stmt
      " e s" #'ruby-send-block
      ;;#'ruby-send-block-and-go
      " e r" #'ruby-send-region
      ;;#'ruby-send-region-and-go
      ;;#'ruby-switch-to-inf
      " r F" #'ruby-load-file
      " r f" #'ruby-load-current-file
      " '" #'inf-ruby-start-pry))

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'my-ruby-mode-robe-keybindings))

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

(require 'reformatter)
(reformatter-define rubyfmt
  :program "rubyfmt"
  :lighter " Rubyfmt")

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

;; load local chruby package
(load (expand-file-name
       ".config/emacs/lisp/packages/chruby"
       gnus-home-directory))

;; then require it
(require 'chruby)


(provide 'emx-ruby)
