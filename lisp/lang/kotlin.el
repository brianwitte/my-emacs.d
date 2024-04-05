;;; -*- lexical-binding: t -*-
;;
;;; kotlin.el --- Kotlin Development Configuration

(use-package lsp-mode
  :straight t
  :hook (kotlin-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  ;; Set the prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; Ensure the exec-path includes the directory where kotlin-language-server is located
  ;; This line is optional if kotlin-language-server is already in a directory within exec-path
  (add-to-list 'exec-path
               "~/language-servers/kotlin-language-server/server/build/install/server/bin")
  :config
  (use-package lsp-ui)
  ;; If kotlin-language-server is in exec-path, you don't need to set lsp-clients-kotlin-server-executable explicitly
  ;; Additional configuration...

  ;; Kotlin Language Server Executable Path
  ;; (setq lsp-clients-kotlin-server-executable "kotlin-language-server")

  ;; JVM Target Version for Kotlin Compiler
  ;; (setq lsp-kotlin-compiler-jvm-target "1.8")

  ;; Enable/Disable Completion Snippets
  ;; (setq lsp-kotlin-completion-snippets-enabled t)

  ;; Enable/Disable Debug Adapter
  ;; (setq lsp-kotlin-debug-adapter-enabled t)

  ;; Custom Path to Debug Adapter Executable
  ;; (setq lsp-kotlin-debug-adapter-path "")

  ;; Auto-Convert Decompiled/External Classes to Kotlin
  ;; (setq lsp-kotlin-external-sources-auto-convert-to-kotlin t)

  ;; Use kls-scheme for URIs Inside JARs
  ;; (setq lsp-kotlin-external-sources-use-kls-scheme t)

  ;; Enable Chained Hints (Requires lsp-inlay-hints-mode)
  ;; (setq lsp-kotlin-inlayhints-enable-chainedhints t)

  ;; Enable Parameter Hints (Requires lsp-inlay-hints-mode)
  ;; (setq lsp-kotlin-inlayhints-enable-parameterhints t)

  ;; Enable Type Hints (Requires lsp-inlay-hints-mode)
  ;; (setq lsp-kotlin-inlayhints-enable-typehints t)

  ;; Debounce Time for Linting (ms)
  ;; (setq lsp-kotlin-linting-debounce-time 250)

  ;; Enable/Disable Ondisk Cache
  ;; (setq lsp-kotlin-ondisk-cache-enabled nil)

  ;; Path to Ondisk Cache
  ;; (setq lsp-kotlin-ondisk-cache-path nil)

  ;; URL for Language Server Download
  ;; (setq lsp-kotlin-server-download-url "https://github.com/fwcd/kotlin-language-server/releases/latest/download/server.zip")

  ;; Trace Server Communication
  ;; (setq lsp-kotlin-trace-server 'off)

  ;; LSP Kotlin Workspace Cache Directory
  ;; (setq lsp-kotlin-workspace-cache-dir "~/.emacs.d/workspace/.cache/")

  ;; LSP Kotlin Workspace Directory
  ;; (setq lsp-kotlin-workspace-dir "~/.emacs.d/workspace/")
  )

(use-package kotlin-mode
  :straight t
  :mode "\\.kt\\'"
  ;; This hook automatically starts lsp-mode in Kotlin buffers
  :hook (kotlin-mode . lsp-deferred)
  )


(provide 'emx-kotlin)
