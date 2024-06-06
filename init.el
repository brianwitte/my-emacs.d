;;; -*- lexical-binding: t -*-
;;
;;; init.el -- my init

;; load path
(add-to-list 'load-path
             "~/.config/emacs/lisp")

;; pre-init setup
(require 'setup
         "setup.el")  ;; dirs, scratch, straight bootstrap, etc.

;; current main theme
(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven-dark))

;; configuration entrypoints -- subdir == *.el within dir
;; -----------------------------------------------------------------------------

(require 'emx-base   "base/core.el")  ;; subdir - ui, org, general, etc.

(require 'emx-build  "bld/build.el")  ;; compilation & build systems
(require 'emx-ci     "bld/ci.el")     ;; ci/cd
(require 'emx-virt   "bld/virt.el")   ;; virtualization - kvm, qemu, etc.
(require 'emx-pods   "bld/pods.el")   ;; working w/ containers

(require 'emx-kernel "wf/kernel.el")  ;; linux kernel development
(require 'emx-patch  "wf/patch.el")   ;; patch/diff management

(require 'emx-utils  "sys/utils.el")  ;; tried & true - vterm, awk, etc.
(require 'emx-db     "sys/db.el")     ;; sql, datalog, pg, sqlite, etc.
(require 'emx-debug  "sys/debug.el")  ;; gdb & friends
(require 'emx-git    "sys/git.el")    ;; (づ ᴗ _ᴗ)づ  ─=≡ΣO)) MAGIT

(require 'emx-browse "web/browse.el") ;; surf ze web
(require 'emx-mail   "web/mail.el")   ;; mu4e, msmtp
(require 'emx-bots   "web/bots.el")   ;; talk to machines

;; files that are bound to general.el keybindings for quick navigation
(require 'file-keymaps
         "file_keymaps.el")

;; main personal config
(require 'my-config
         "config.el")

;; primary text editing config -- always enabled
(require 'emx-editor      "lang/editor.el")      ;; global settings, lsp, etc.
(require 'emx-config-fmts "lang/config_fmts.el") ;; ini, yaml, edn, etc.
(require 'emx-lsp         "lang/lsp.el")
(require 'emx-tree-sitter "lang/tree_sitter.el")

;; lang -- comment/uncomment
(require 'emx-elisp  "lang/elisp.el") ;; subdir - elisp config
(require 'emx-cc     "lang/cc.el")
(require 'emx-clj    "lang/clj.el")
(require 'emx-fnl    "lang/fnl.el")
;;(require 'emx-java   "lang/java.el")
;;(require 'emx-kotlin "lang/kotlin.el")
(require 'emx-lua    "lang/lua.el")
(require 'emx-ocaml  "lang/ocaml.el")
(require 'emx-poke    "lang/poke.el")
;;(require 'emx-python "lang/python.el")
(require 'emx-ruby   "lang/ruby.el")
;;(require 'emx-zig    "lang/zig.el")

;; not yet categorized
(require 'emx-misc
         "wip/misc.el")

;; test framework for config -- unit, functional, perf, etc.
(require 'emx-test
         "test.el")
