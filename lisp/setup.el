;;; -*- lexical-binding: t -*-
;;
;; Bootstrap straight.el

(defvar my-emacs-local-dir "~/.emacs-local.d/"
  "The root directory for all Emacs local, stateful files.")

(unless (file-exists-p my-emacs-local-dir)
  (make-directory my-emacs-local-dir t))

(setq initial-major-mode 'emacs-lisp-mode)

;; ascii art by Brian MacDonald
;; https://www.asciiart.eu/computers/computers
(setq scratch-ascii-art
      (concat ";;      _________\n"
              ";;     / ======= \\\n"
              ";;    / __________\\\n"
              ";;   | ___________ |\n"
              ";;   | |e        | |\n"
              ";;   | |         | |\n"
              ";;   | |_________| |______________________________\n"
              ";;   \\_____________/   ==== _  _  __   __  __     )\n"
              ";;   /\\\\\\\\\\\\\\\\\\\\\\\\\\\\   |__  |\\/| |__| |   [__    \/\n"
              ";;  / ::::::::::::: \\  |    |||| |  | |__  __]  /\n"
              ";; (_________________) ====                    /\n"
              ";;                                         =D-'\n"))

(defun gpl-v3-text ()
  (concat
   ";; This document is free software: you can redistribute it and/or modify\n"
   ";; it under the terms of the GNU General Public License as published by\n"
   ";; the Free Software Foundation, either version 3 of the License, or\n"
   ";; (at your option) any later version.\n"
   ";;\n"
   ";; This program is distributed in the hope that it will be useful,\n"
   ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   ";; GNU General Public License for more details.\n"
   ";;\n"
   ";; You should have received a copy of the GNU General Public License\n"
   ";; along with this program.  If not, see <https://www.gnu.org/licenses/>.\n"
   ";;\n"
   ))

(setq initial-scratch-message
      (concat
       scratch-ascii-art
       ";; Emacs Version: " emacs-version "\n"
       ";;\n"
       (gpl-v3-text)
       "\n"
       "\n"
       "(message \"eval me\")\n"
       "\n"
       ))

(setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" my-emacs-local-dir)))

(let ((eln-cache-dir (expand-file-name "eln-cache/" my-emacs-local-dir)))
  (unless (file-exists-p eln-cache-dir)
    (make-directory eln-cache-dir)))


;; transient is dep of many useful packages
(setq transient-history-file (concat my-emacs-local-dir "transient/history.el"))

(setq straight-base-dir my-emacs-local-dir)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" my-emacs-local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)      ; integrate with use-package
(setq straight-use-package-by-default t) ; use straight.el for every package by default

(provide 'setup)
