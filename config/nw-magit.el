(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evil-transient-state)

(autoload 'evil-define-key "evil-core")

(use-package with-editor
  :commands (with-editor-finish
             with-editor-cancel)
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-minor-mode 'with-editor-mode
      "c" #'with-editor-finish
      "k" #'with-editor-cancel)))

(use-package magit
  :defer t
  :commands (magit-status magit-blame magit-branch-and-checkout magit-dispatch-popup)
  :functions (magit-display-buffer-fullframe-status-v1)
  :preface
  (evil-transient-state-define git-blame
    :title "Git Blame Transient State"
    :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
    :on-enter (unless (bound-and-true-p magit-blame-mode)
                (call-interactively 'magit-blame))
    :on-exit (magit-blame-quit)
    :foreign-keys run
    :bindings
    ("b" magit-blame)
    ("q" nil :exit t))
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "gs" #'magit-status
      "gm" #'magit-dispatch-popup
      "gb" #'git-blame-transient-state/body))
  :config
  (progn
    (require 'evil-magit)
    (evil-define-key 'normal magit-refs-mode-map (kbd ".") #'magit-branch-and-checkout)
    (setq magit-log-section-commit-count 0)
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)))

(use-package git-commit-jira-prefix
  :after git-commit
  :commands git-commit-jira-prefix-init
  :config (git-commit-jira-prefix-init))

(use-package git-timemachine
  :defer t
  :commands
  (git-timemachine
   git-timemachine-show-current-revision
   git-timemachine-show-nth-revision
   git-timemachine-show-previous-revision
   git-timemachine-show-next-revision
   git-timemachine-show-previous-revision
   git-timemachine-kill-revision
   git-timemachine-quit)
  :preface
  (evil-transient-state-define time-machine
    :title "Git Timemachine Transient State"
    :doc "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
    :on-enter (unless (bound-and-true-p git-timemachine-mode)
                (call-interactively 'git-timemachine))
    :on-exit (when (bound-and-true-p git-timemachine-mode)
               (git-timemachine-quit))
    :foreign-keys run
    :bindings
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("N" git-timemachine-show-previous-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil :exit t))
  :init
  (spacemacs-keys-set-leader-keys
    "gt" #'time-machine-transient-state/body))

(use-package diff-hl
  :after magit
  :commands (diff-hl-magit-post-refresh
             global-diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk
             diff-hl-revert-hunk
             diff-hl-diff-goto-hunk)
  :init
  (progn
    (evil-transient-state-define git-hunks
      :title "Git Hunk Transient State"
      :doc "
[_p_/_N_] previous [_n_] next [_g_] goto [_x_] revert [_q_] quit"
      :foreign-keys run
      :bindings
      ("n" diff-hl-next-hunk)
      ("N" diff-hl-previous-hunk)
      ("p" diff-hl-previous-hunk)
      ("g" diff-hl-diff-goto-hunk)
      ("x" diff-hl-revert-hunk)
      ("q" nil :exit t))

    (spacemacs-keys-set-leader-keys "g." 'git-hunks-transient-state/body))
  :config
  (progn
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode)))

(provide 'nw-magit)
