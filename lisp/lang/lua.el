;;; -*- lexical-binding: t -*-
;;
;;; lua.el --- Description

(setq lsp-clients-lua-language-server-install-dir
      "/home/bkz/lang-servers/lua-language-server/")

(use-package lua-mode
  :straight t
  :init
  ;; lua-indent-level defaults to 3. let's keep it? idk
  (setq lua-indent-level 4)
  :config

  (add-hook 'lua-mode-local-vars-hook #'lsp 'append)
  (add-hook 'lua-mode-local-vars-hook #'tree-sitter 'append))

(provide 'emx-lua)
