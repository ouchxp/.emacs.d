(eval-when-compile
  (require 'use-package))

(use-package nw-emacs)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (exec-path-from-shell-initialize)))

(use-package nw-look
  :config
  (progn
    (setq nw-default-font "Menlo-12")
    (setq nw-variable-pitch-font "Lucida Grande-12")
    (setq nw-fixed-pitch-font "Menlo-12")))

;;; why is this not working
;(use-package all-the-icons)


(use-package doom-themes
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
  :config
  (setq which-key-idle-delay 0.2)
  ;; (which-key-add-key-based-replacements
  ;;   "M-m g" "git"
  ;;   "M-m l" "layouts"
  ;;   "M-m m" "major-mode-hydra"
  ;;   "M-m p" "projects"
  ;;   "M-m t" "toggles"
  ;;   "M-m w" "workspace")
  (which-key-mode))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'nw-base)