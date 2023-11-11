;;; -*- lexical-binding: t -*-
;;;
;;; core.el --- Description


(setq custom-file (expand-file-name "custom.el" my-emacs-local-dir))

;; Load custom file. Doesn't throw an error if the file doesn't exist.
(when (file-exists-p custom-file)
  (load custom-file))

;; use-package check for redundancy
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; order matters below
(require 'emx-evil
         "base/evil.el")              ;; evil, collections, etc.

(require 'emx-general
         "base/general.el")           ;; general & core keymaps

(require 'emx-projectile
         "base/projectile.el")        ;; projectile base config

(require 'emx-sexp   "base/sexp.el")
(require 'emx-os     "base/os.el")    ;; freebsd, macos, windows, etc. compat
(require 'emx-perf   "base/perf.el")  ;; perf tuning for emacs/packages
(require 'emx-nav    "base/nav.el")   ;; corfu, vertico, swiper
(require 'emx-ui     "base/ui.el")    ;; hl-todo, popup, treemacs
(require 'emx-org    "base/org.el")   ;; get org'd
(require 'emx-help   "base/help.el")  ;; get help'd

;; autoload file
(require 'emx-base-autoload "base/autoload.el")

(provide 'emx-base)
;;; core.el ends here
