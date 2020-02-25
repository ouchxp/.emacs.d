
(require 'nw-spacemacs-keys)

(use-package ivy
  :straight t
  ;:bind
  ;("M-m r" . ivy-resume)

  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))

; (use-package ivy-hydra)


(use-package counsel
  :straight t
  :commands (counsel-M-x
             counsel-descbinds
             counsel-describe-face
             counsel-describe-function
             counsel-describe-variable
             counsel-minibuffer-history
             counsel-find-file
             counsel-imenu
             counsel-faces
             counsel-colors-emacs
             counsel-list-processes
             counsel-colors-web
             counsel-command-history
             counsel-file-jump
             counsel-recentf
             counsel-yank-pop
             counsel-grep-or-swiper
             counsel-load-theme
             counsel-up-directory)
  :defines (counsel-rg-base-command)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel"))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "SPC" #'counsel-M-x
      "?"   #'counsel-descbinds
      "f f" #'counsel-find-file
      "f j" #'counsel-file-jump
      "f r" #'counsel-recentf
      "k r" #'counsel-yank-pop
      "i i" #'counsel-imenu
      "i f" #'counsel-faces
      "i e" #'counsel-colors-emacs
      "i w" #'counsel-colors-web
      "i c" #'counsel-command-history
      "i m" #'counsel-minibuffer-history
      "i p" #'counsel-list-processes
      "i t" #'counsel-load-theme
      "h d f" #'counsel-describe-function
      "h d v" #'counsel-describe-variable
      "h d c" #'counsel-describe-face)

    (evil-global-set-key 'motion "/" #'counsel-grep-or-swiper)
    (evil-global-set-key 'normal "/" #'counsel-grep-or-swiper)
    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-h f" #'counsel-describe-function))

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)
    (define-key ivy-minibuffer-map (kbd "C-h") #'counsel-up-directory)

    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command "rg -i -g '!.git/*' --no-heading --line-number --hidden --max-columns 120 --color never %s .")
    (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))

    (counsel-mode +1)))

(provide 'nw-ivy)
