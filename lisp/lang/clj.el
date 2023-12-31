;;; -*- lexical-binding: t -*-

;;; clj.el --- Clojure Development Configuration

;; Ensure use-package is available
(eval-when-compile
  (require 'use-package))

(use-package cider
  :straight t
  :config
  (defun my-clojure-mode-cider-keybindings ()
    (my-local-leader-def
      :states 'normal
      :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
      " '"  #'cider-jack-in-clj
      " \"" #'cider-jack-in-cljs
      " c"  #'cider-connect-clj
      " C"  #'cider-connect-cljs
      " m"  #'cider-macroexpand-1
      " M"  #'cider-macroexpand-all
      ;; Debug
      " d d" #'cider-debug-defun-at-point
      ;; Eval
      " e b" #'cider-eval-buffer
      " e d" #'cider-eval-defun-at-point
      " e D" #'cider-insert-defun-in-repl
      " e e" #'cider-eval-last-sexp
      " e E" #'cider-insert-last-sexp-in-repl
      " e r" #'cider-eval-region
      " e R" #'cider-insert-region-in-repl
      " e u" #'cider-undef
      ;; Goto
      " g b" #'cider-pop-back
      " g g" #'cider-find-var
      " g n" #'cider-find-ns
      ;; Help
      " h a" #'cider-apropos
      " h c" #'cider-clojuredocs
      " h d" #'cider-doc
      " h j" #'cider-javadoc
      " h w" #'cider-clojuredocs-web
      ;; Inspect
      " i e" #'cider-enlighten-mode
      " i i" #'cider-inspect
      " i r" #'cider-inspect-last-result
      ;; Namespace
      " n n" #'cider-browse-ns
      " n N" #'cider-browse-ns-all
      " n r" #'cider-ns-refresh
      ;; Print
      " p p" #'cider-pprint-eval-last-sexp
      " p P" #'cider-pprint-eval-last-sexp-to-comment
      " p d" #'cider-pprint-eval-defun-at-point
      " p D" #'cider-pprint-eval-defun-to-comment
      " p r" #'cider-pprint-eval-last-sexp-to-repl
      ;; REPL
      " r n" #'cider-repl-set-ns
      " r q" #'cider-quit
      " r r" #'cider-ns-refresh
      " r R" #'cider-restart
      " r b" #'cider-switch-to-repl-buffer
      " r B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
      " r c" #'cider-find-and-clear-repl-output
      " r f" #'cider-load-file
      " r l" #'cider-load-buffer
      " r L" #'cider-load-buffer-and-switch-to-repl-buffer
      ;; Test
      " t a" #'cider-test-rerun-test
      " t l" #'cider-test-run-loaded-tests
      " t n" #'cider-test-run-ns-tests
      " t p" #'cider-test-run-project-tests
      " t r" #'cider-test-rerun-failed-tests
      " t s" #'cider-test-run-ns-tests-with-filters
      " t t" #'cider-test-run-test))

  (add-hook 'clojure-mode-hook 'my-clojure-mode-cider-keybindings)
  (add-hook 'clojurescript-mode-hook 'my-clojure-mode-cider-keybindings)
  (add-hook 'clojurec-mode-hook 'my-clojure-mode-cider-keybindings))

(provide 'emx-clj)

;;; emx-clj.el ends here
