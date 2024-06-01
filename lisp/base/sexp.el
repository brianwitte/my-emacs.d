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

(provide 'emx-sexp)
