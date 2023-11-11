;;; -*- lexical-binding: t -*-
;;
;;; sexp.el -- s-expression editing

(defun my-sexp-keybindings ()
  (my-leader-def
    :keymaps 'normal
    "k="  'sp-reindent
    "k-"  'sp-reindent
    "kW"  'sp-unwrap-sexp
    "kb"  'sp-forward-barf-sexp
    "kB"  'sp-backward-barf-sexp
    "kc"  'sp-convolute-sexp
    "kdx" 'sp-kill-sexp
    "kr"  'sp-raise-sexp
    "ks"  'sp-forward-slurp-sexp
    "kS"  'sp-backward-slurp-sexp
    "kt"  'sp-transpose-sexp
    "kw"  'sp-wrap-sexp
    "ky"  'sp-copy-sexp))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)  ; optional, but recommended
  (smartparens-global-mode t)    ; enable smartparens everywhere

  (add-hook 'smartparens-mode-hook 'my-sexp-keybindings))

(provide 'emx-sexp)
