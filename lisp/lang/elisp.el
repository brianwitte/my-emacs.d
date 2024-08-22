;;; -*- lexical-binding: t -*-
;;;
;;; elisp.el --- Description


(defun eval-last-sexp-and-show-temporarily ()
  "Evaluate the sexp before the point and momentarily display the result."
  (interactive)
  ;; Evaluate the preceding sexp and get the result.
  (let*
    (
      (result (eval-last-sexp nil))
      (result-string (format "\n;=> %S" result))
      ;; The position to display the message at.
      (display-pos (1+ (point))))
    ;; Use `momentary-string-display` to show the result temporarily.
    ;; By default, it will disappear with the next keystroke.
    (momentary-string-display result-string display-pos)))

(defun setup-elisp-mode-keys ()
  (my-local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "m"
    'macrostep-expand
    "eb"
    'eval-buffer
    "ed"
    'eval-defun
    "ee"
    'eval-last-sexp-and-show-temporarily
    "er"
    'eval-region
    "el"
    'load-library
    "gf"
    'find-function
    "gv"
    'find-variable
    "gl"
    'find-library))

(add-to-list 'exec-path (expand-file-name "fmt-bin" user-emacs-directory))

(require 'reformatter)
(reformatter-define elatto
  :program (expand-file-name "elatto" (locate-user-emacs-file "fmt-bin"))
  :args `("--stdin"
          "--stdout"
          "--fmt-defs-dir"
          ,(expand-file-name "fmt-bin" user-emacs-directory)
          "--fmt-defs"
          "elatto.overrides.json"
          "--fmt-style"
          "fixed")
  :lighter " Elatto")


(provide 'emx-elisp)
