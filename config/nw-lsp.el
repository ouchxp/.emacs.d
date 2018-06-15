
(eval-when-compile
  (require 'use-package))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-imenu
  :defines (lsp-ui-imenu-colors)
  :commands (lsp-enable-imenu)
  :config
  (progn
    (setq lsp-ui-imenu-colors '("#e99ce8" "#bbbbff" "#ffbbff"))
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)))

(provide 'nw-lsp)
