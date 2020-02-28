(eval-when-compile
  (require 'use-package))

;; enable desktop mode, so it reopen previous sessions
(desktop-save-mode 1)

;; maximized by default
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defconst nw-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

;; better scrolling
;; (use-package yascroll
;;   :straight t)

;; (global-yascroll-bar-mode 1)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

;; setup global keys
(global-set-key (kbd "A-<f10>") 'toggle-frame-maximized)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; ensure that emacs can find shell binaries e.g. ag
(use-package exec-path-from-shell
  :straight t
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
    (load-theme 'doom-dracula t)

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

(defun nw-hydra/title-with-faicon (name icon)
  (format "%s %s"
          (all-the-icons-faicon icon :v-adjust 0.01 :height 1.1)
          (propertize name 'face '(:height 1.1 :weight bold))))

(defun nw-hydra/remove-heads-prefix (prefix heads-plist)
  (if (stringp prefix)
      (->> heads-plist
           (--map-when (listp it)
                       (-map (-lambda ((key . rest))
                               (cons (s-trim (s-chop-prefix prefix key)) rest))
                             it)))
    heads-plist))

(defun nw-hydra/maybe-add-exit-head (heads-plist)
  (if (->> heads-plist
           (-partition 2)
           (-mapcat #'cadr)
           (-none? (-compose #'null #'cadr)))
      (--map-first (listp it)
                   (-concat it '(("q"        nil nil)
                                 ("<escape>" nil nil)))
                   heads-plist)
    heads-plist))

(defun nw-hydra/plist-remove-props (plist props)
  (->> plist
       (-partition 2)
       (--filter (not (-contains? props (car it))))
       (-flatten-n 1)))

(defmacro nw-hydra/define (name body heads-plist)
  (declare (indent defun))
  (let* ((icon   (or (plist-get body :icon) "cogs"))
         (title  (nw-hydra/title-with-faicon (plist-get body :title) icon))
         (prefix (plist-get body :prefix))
         (heads  (--> heads-plist
                      (nw-hydra/remove-heads-prefix prefix it)
                      (nw-hydra/maybe-add-exit-head it)))
         (body   (nw-hydra/plist-remove-props body '(:title :icon :prefix))))
    `(pretty-hydra-define ,name (,@body :hint nil :title ,title) ,heads)))

(use-package pretty-hydra
  :straight t
  :after (hydra)

  :config
  (nw-hydra/define nw-main-hydra--window
    (:color teal :title "Window Hydra" :icon "windows" :prefix "w")
    ("Basic"
     (("w SPC" nw/toggle-current-window-dedication "dedicate")
      ("wp" ivy-push-view "push view")
      ("wP" ivy-switch-view "switch view")
      ("wo" ace-select-window "select")
      ("w0" (text-scale-set 0) "scale reset")
      ("w+" text-scale-increase "scale ↑" :color pink)
      ("w_" text-scale-decrease "scale ↓" :color pink))

     "Split"
     (("w-" split-window-vertically "vertical")
      ("w/" split-window-horizontally "horizontal")
      ("wR" ace-swap-window "swap")
      ("wr" evil-window-rotate-downwards "rotate"))

     "Resize"
     (("w=" balance-windows "balance")
      ("wm" nw/toggle-maximize-window "maximize")
      ("w," shrink-window "resize ←" :color pink)
      ("w." enlarge-window "resize ↑" :color pink)
      ("w<" shrink-window-horizontally "resize ←" :color pink)
      ("w>" enlarge-window-horizontally "resize →" :color pink))

     "Killer"
     (("wd" delete-window "kill")
      ("wD" delete-other-windows "close others")
      ("wk" ace-delete-window "ace kill")
      ("wK" ace-delete-other-windows "ace close others")
      ("wQ" zc/kill-emacs-or-frame "kill frame"))))


  (nw-hydra/define nw-main-hydra
    (:hint nil :color teal :title "Main Hydra")
    ("Basic"
     (("SPC" counsel-M-x "M-x")
      (":" eval-expression "eval expression")
      ("!" zc-term/open "open terminal")
      ("r" ivy-resume "ivy resume")
      ("v" er/expand-region "expand region")
      ("m" zc-hydra/major-mode-hydra "major mode hydra")
      ("u" universal-argument "universal argument"))

     "Quick Switch"
     (("1" winum-select-window-1 "window 1")
      ("2" winum-select-window-2 "window 2")
      ("3" winum-select-window-3 "window 3")
      ("4" winum-select-window-4 "window 4")
      ("5" winum-select-window-5 "window 5"))

     ""
     (("b" zc-main-hydra--buffer/body "+buffer")
      ("e" zc-main-hydra--error/body "+flycheck")
      ("f" zc-main-hydra--file/body "+file")
      ("g" zc-main-hydra--git/body "+git")
      ("h" zc-main-hydra--help/body "+help")
      ("j" zc-main-hydra--jump/body "+jump"))

     ""
     (("o" zc-main-hydra--org/body "+org")
      ("p" zc-main-hydra--project/body "+project")
      ("s" zc-main-hydra--symbol/body "+symbol")
      ("t" zc-main-hydra--toggle/body "+toggle")
      ("w" nw-main-hydra--window/body "+window")))))

(use-package general
  :straight t
  :config
  (general-setq general-override-states
                '(insert emacs hybrid normal visual motion operator replace))
  (general-override-mode +1)
  (general-define-key :keymaps 'override :states '(normal visual motion)
    "SPC" #'nw-main-hydra/body
    ","   #'nw-hydra/major-mode-hydra)
  (general-define-key :keymaps 'override :states '(insert)
    "s-SPC" #'nw-main-hydra/body
    "s-,"   #'nw-hydra/major-mode-hydra))

;; (use-package nw-spacemacs-keys
;;   :preface
;;   (progn
;;     ;; (autoload 'evil-window-next "evil-commands")
;;     ;; (autoload 'evil-window-split "evil-commands")
;;     ;; (autoload 'evil-window-vsplit "evil-commands")

;;     (defun nw-leader-keys/reload-file ()
;;       "Revisit the current file."
;;       (interactive)
;;       (when-let (path (buffer-file-name))
;;         (find-alternate-file path))))

;;   :config
;;   (progn
;;     (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

;;     (spacemacs-keys-set-leader-keys
;;       "u"   #'universal-argument
;;       "SPC" #'execute-extended-command
;;       ;; "<escape>" #'keyboard-escape-quit
;;       ;; "TAB" #'rk/alternate-buffer
;;       ;; "|"   #'rk/toggle-window-split

;;       "!"   #'shell-command

;;       "b d" #'kill-this-buffer
;;       "b b" #'nw-switch-buffer
;;       "b v" #'nw-leader-keys/reload-file

;;       ;; "C" #'compile

;;       ;; "c r" #'comment-or-uncomment-region

;;       ;; "f D" #'rk/delete-current-buffer-and-file
;;       "f F" #'find-file-other-window
;;       ;; "f R" #'rk/rename-file-and-buffer
;;       ;; "f e" #'rk/sudo-edit
;;       "f f" #'find-file
;;       "f s" #'save-buffer
;;       "f S" #'save-some-buffers
;;       "f W" #'write-file
;;       ;; "f v" #'rk-leader-keys/reload-file
;;       ;; "f y" #'rk/copy-buffer-path

;;       ;; "g i" #'rk-goto-init-file
;;       ;; "g m" #'rk-goto-messages
;;       ;; "g p" #'rk-goto-personal-config

;;       "h d c" #'describe-face
;;       "h d k" #'describe-key
;;       "h d m" #'describe-mode
;;       "h d v" #'describe-variable
;;       "h f c" #'find-face-definition
;;       "h f f" #'find-function
;;       "h f l" #'find-library
;;       "h f v" #'find-variable
;;       "h i"   #'info

;;       "k b" #'kill-this-buffer
;;       "k w" #'delete-window

;;       "n d" #'narrow-to-defun
;;       "n f" #'narrow-to-defun
;;       "n r" #'narrow-to-region
;;       "n s" #'org-narrow-to-subtree
;;       "n w" #'widen

;;       "q w" #'delete-window
;;       "q q" #'kill-emacs

;;       "t F" #'toggle-frame-fullscreen

;;       "w =" #'balance-windows
;;       "w w" #'other-window
;;       "w o" #'delete-other-windows
;;       "w d" #'delete-window
;;       ;; "w r" #'evil-window-rotate-downwards
;;       "w -" #'split-window-vertically
;;       "w /" #'split-window-horizontally)))

;; esc always quits
;; via: https://superuser.com/questions/669902/is-it-possible-to-get-esc-to-behave-as-an-actual-escape-key/945245#945245
(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)

;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'nw-base)