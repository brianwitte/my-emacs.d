;;; -*- lexical-binding: t -*-
;;

;;; cc.el --- Description

(use-package lsp-mode
  :hook (c-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  ;; Configure lsp-mode to use clangd, assuming it's already installed on your system
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-formatting nil)

  (setq lsp-clients-clangd-executable "clangd"))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook
 'c-mode-hook
 (lambda ()
   (when (and buffer-file-name
              (string-prefix-p (expand-file-name "~/proj/ntxx/")
                               (file-name-directory buffer-file-name)))
     (setq-local c-basic-offset 8)
     (setq-local c-label-minimum-indentation 0)
     (setq-local indent-tabs-mode t)
     (setq-local show-trailing-whitespace t)
     (setq-local c-offsets-alist
                 '((arglist-close         . c-lineup-arglist-tabs-only)
                   (arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))
                   (arglist-intro         . +)
                   (brace-list-intro      . +)
                   (c                     . c-lineup-C-comments)
                   (case-label            . 0)
                   (comment-intro         . c-lineup-comment)
                   (cpp-define-intro      . +)
                   (cpp-macro             . (lambda (ignored) -1000))
                   (cpp-macro-cont        . +)
                   (defun-block-intro     . +)
                   (else-clause           . 0)
                   (func-decl-cont        . +)
                   (inclass               . +)
                   (inher-cont            . c-lineup-multi-inher)
                   (knr-argdecl-intro     . 0)
                   (label                 . (lambda (ignored) -1000))
                   (statement             . 0)
                   (statement-block-intro . +)
                   (statement-case-intro  . +)
                   (statement-cont        . +)
                   (substatement          . +))))))

(provide 'emx-cc)
