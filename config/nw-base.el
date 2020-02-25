(eval-when-compile
  (require 'use-package))

(defconst nw-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

;; better scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

(delete-selection-mode 1)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(use-package nw-look
  :config
  (progn
    (setq nw-default-font "Menlo-12")
    (setq nw-variable-pitch-font "Lucida Grande-12")
    (setq nw-fixed-pitch-font "Menlo-12")))

(use-package all-the-icons
  :straight t)


(use-package doom-themes
  :straight t
  :config
  (progn
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    ;;(doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    ;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)

    ;; Nicer colors for org heading level 3 to 6. Colors are taken
    ;; from doom-one theme.
    (eval-after-load "org"
      '(progn
         (set-face-attribute 'org-level-3 nil :foreground "#DA8548")
         (set-face-attribute 'org-level-4 nil :foreground "#ECBE7B")
         (set-face-attribute 'org-level-5 nil :foreground "#ECBE7B")
         (set-face-attribute 'org-level-6 nil :foreground "#ECBE7B")))))

(use-package nw-modeline)

;; which-key
(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.2)
    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC a"   "applications"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC h d" "describe"
      "SPC h f" "find"
      "SPC k"   "kill"
      "SPC l"   "layout"
      "SPC n"   "narrow"
      "SPC o"   "org"
      "SPC p"   "project"
      "SPC q"   "quit"
      "SPC w"   "window"
      "SPC s"   "search/edit"
      "SPC t"   "toggles"
      "SPC SPC" "M-x"
      "SPC m"   '("major-mode-cmd" . "Major mode commands")
    )
  (which-key-mode))

(autoload 'counsel-rg "counsel")
(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile")
(autoload 'projectile-project-p "projectile")

;; switch buffer only shows current project files
(defun nw-switch-buffer (global-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not global-p))
      (counsel-projectile-switch-to-buffer)
    (ivy-switch-buffer)))

(use-package nw-spacemacs-keys
  :preface
  (progn
    (autoload 'evil-window-next "evil-commands")
    (autoload 'evil-window-split "evil-commands")
    (autoload 'evil-window-vsplit "evil-commands")

    (defun nw-leader-keys/reload-file ()
      "Revisit the current file."
      (interactive)
      (when-let (path (buffer-file-name))
        (find-alternate-file path))))

  :config
  (progn
    (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

    (spacemacs-keys-set-leader-keys
      "u"   #'universal-argument
      "SPC" #'execute-extended-command
      ;; "TAB" #'rk/alternate-buffer
      ;; "|"   #'rk/toggle-window-split

      "!"   #'shell-command

      "b d" #'kill-this-buffer
      "b b" #'nw-switch-buffer
      "b v" #'nw-leader-keys/reload-file

      ;; "C" #'compile

      ;; "c r" #'comment-or-uncomment-region

      ;; "f D" #'rk/delete-current-buffer-and-file
      "f F" #'find-file-other-window
      ;; "f R" #'rk/rename-file-and-buffer
      ;; "f e" #'rk/sudo-edit
      "f f" #'find-file
      "f s" #'save-buffer
      "f S" #'save-some-buffers
      "f W" #'write-file
      ;; "f v" #'rk-leader-keys/reload-file
      ;; "f y" #'rk/copy-buffer-path

      ;; "g i" #'rk-goto-init-file
      ;; "g m" #'rk-goto-messages
      ;; "g p" #'rk-goto-personal-config

      "h d c" #'describe-face
      "h d k" #'describe-key
      "h d m" #'describe-mode
      "h d v" #'describe-variable
      "h f c" #'find-face-definition
      "h f f" #'find-function
      "h f l" #'find-library
      "h f v" #'find-variable
      "h i"   #'info

      "k b" #'kill-this-buffer
      "k w" #'delete-window

      "n d" #'narrow-to-defun
      "n f" #'narrow-to-defun
      "n r" #'narrow-to-region
      "n s" #'org-narrow-to-subtree
      "n w" #'widen

      "q w" #'delete-window
      "q q" #'kill-emacs

      "t F" #'toggle-frame-fullscreen

      "w =" #'balance-windows
      "w w" #'evil-window-next
      "w o" #'delete-other-windows
      "w d" #'delete-window
      "w r" #'evil-window-rotate-downwards
      "w -" #'evil-window-split
      "w /" #'evil-window-vsplit)))

;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'nw-base)