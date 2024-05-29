;;; -*- lexical-binding: t -*-
;;;
;;; elisp.el --- Description

(use-package aggressive-indent
  :straight t
  :after evil)

(defun eval-last-sexp-and-show-temporarily ()
  "Evaluate the sexp before the point and momentarily display the result."
  (interactive)
  ;; Evaluate the preceding sexp and get the result.
  (let* ((result (eval-last-sexp nil))
         (result-string (format "\n;=> %S" result))
         (display-pos (1+ (point)))) ; The position to display the message at.
    ;; Use `momentary-string-display` to show the result temporarily.
    ;; By default, it will disappear with the next keystroke.
    (momentary-string-display result-string display-pos)))

(defun setup-elisp-mode-keys ()
  (my-local-leader-def :keymaps 'emacs-lisp-mode-map
    "m" 'macrostep-expand
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-last-sexp-and-show-temporarily
    "er" 'eval-region
    "el" 'load-library
    "gf" 'find-function
    "gv" 'find-variable
    "gl" 'find-library))



(add-hook 'emacs-lisp-mode-hook 'setup-elisp-mode-keys)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'emx-elisp)
