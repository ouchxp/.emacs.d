
(eval-when-compile
  (require 'use-package))

(require 'dash)


(use-package nw-lsp-python
  :after (python)
  :config
  (progn
    (autoload 'flycheck-mode "flycheck")
    (autoload 'python-mode-hook "python")
    (nw-lsp-python--setup)
    (add-hook 'python-mode-hook #'lsp-python-enable)
    (add-hook 'lsp-python-mode-hook 'flycheck-mode)
    ))

(provide 'nw-python)
