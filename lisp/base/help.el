;;; -*- lexical-binding: t -*-
;;;
;;; base/help.el --- Description

;; The best resource within Emacs for learning about basic Emacs Lisp (Elisp)
;; syntax, including how to perform loops, is the Emacs Lisp Reference
;; Manual. You can access it by running `M-x info` and then selecting "Emacs
;; Lisp".
;;
;; For loops specifically, navigate to the section that talks about iteration,
;; which will provide you with details on using `while`, `dolist`, `dotimes`,
;; and other loop constructs. You'll get in-depth explanations, examples, and
;; even some best practices.
;;
;; To go directly to the Emacs Lisp Reference Manual from within Emacs, you can
;; also execute `M-x info-emacs-lisp-reference`, provided that the `info` pages
;; for Emacs Lisp are installed on your system.
;;
;; For quicker, context-based help, you can use the following:
;;
;; - `M-x describe-function`: Describe a specific function.
;; - `M-x describe-variable`: Describe a variable.  `C-h k` (or
;; - `M-x describe-key`: Describe what a specific keybinding does,
;;    and shows the Elisp function that it calls.
;;
;; These are great for learning what a specific function or variable does, but
;; they are less suitable for understanding larger programming constructs like
;; loops.
;;
;; The combination of the Emacs Lisp Reference Manual for in-depth learning and
;; `describe-function`, `describe-variable`, and `describe-key` for quick
;; lookups covers most of what you'll need to become proficient in Elisp.


(defun my-custom-help-setup ()
  "Open *info* in new window below current focused window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (info-emacs-manual)  ;; Initialize the apropos command prompt
  )


(my-leader-def
  :keymaps 'normal
  "hh" 'my-custom-help-setup)

(my-local-leader-def
  :keymaps 'Info-mode-map
  "n" 'Info-next
  "p" 'Info-prev
  "u" 'Info-up
  "g" 'Info-goto-node
  "S" 'Info-search
  "s" 'swiper
  "i" 'Info-index
  "l" 'Info-history-back
  "f" 'Info-follow-nearest-node
  "m" 'Info-menu
  "q" 'delete-window)

(add-hook 'Info-mode-hook
          (lambda () (define-key Info-mode-map (kbd "SPC") nil)))

(provide 'emx-help)
