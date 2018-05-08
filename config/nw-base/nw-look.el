(require 'dash)

(show-paren-mode 1)
(global-linum-mode t)

(defvar nw-default-font nil
  "The universal default font.")

(defvar nw-variable-pitch-font nil
  "The font to use in the variable-pitch face.")

(defvar nw-fixed-pitch-font nil
  "The font to use in the fixed-pitch face.")

;; Work around Emacs frame sizing bug when line-spacing
;; is non-zero, which impacts e.g. grizzl, and allow resizing when
;; vertical modes are enabled or user has customized nw-resize-minibuffer
(defvar nw-resize-minibuffer nil
  "Whether the minibuffer should be resizable.")

(defun nw-resize-minibuffer-p ()
  (or (-any? 'featurep '(ivy grizzl ido-vertical-mode))
      nw-resize-minibuffer))

(defun nw-minibuffer-setup-hook ()
  (if (nw-resize-minibuffer-p)
      (set (make-local-variable 'line-spacing) 0)
    (setq resize-mini-windows nil)))

(add-hook 'minibuffer-setup-hook
          'nw-minibuffer-setup-hook)

(add-hook 'ido-minibuffer-setup-hook
          'nw-minibuffer-setup-hook)

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode tool-bar-mode blink-cursor-mode))

(defvar nw-geometry-file
  (expand-file-name ".nw-geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")

(defun nw-load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p nw-geometry-file)
      (with-temp-buffer
        (insert-file-contents nw-geometry-file)
        (read (buffer-string)))
    '(100 40 0 0)))

(defun nw-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file nw-geometry-file
    (print (nw-get-geometry) (current-buffer))))

(defun nw-get-geometry ()
  "Get the current geometry of the active frame."
  (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

(defun nw-set-geometry ()
  "Set the default frame geometry using the values loaded from nw-geometry-file."
  (let ((geom (nw-load-frame-geometry)))
    (let ((f-width (nth 0 geom))
          (f-height (nth 1 geom))
          (f-top (nth 2 geom))
          (f-left (nth 3 geom)))
      (setq default-frame-alist
            (append default-frame-alist
                    `((width . ,f-width)
                      (height . ,f-height)
                      (top . ,f-top)
                      (left . ,f-left)))))))

(defun nw-maybe-set-default-font (default-font var-pitch-font pitch-font)
  "Set up default fonts when they are not set."
  (unless nw-default-font
    (setq nw-default-font default-font))
  (unless nw-variable-pitch-font
    (setq nw-variable-pitch-font var-pitch-font))
  (unless nw-fixed-pitch-font
    (setq nw-fixed-pitch-font pitch-font)))

(defun nw-set-fonts ()
  "Set up default fonts."
  (cond
   ((eq system-type 'darwin)
    (nw-maybe-set-default-font "Monaco-11" "Lucida Grande-11" "Monaco-11"))
   ((eq system-type 'gnu/linux)
    (nw-maybe-set-default-font "DejaVu Sans Mono-10" "Liberation Sans-10" "DejaVu Sans Mono-10"))
   (t
    (nw-maybe-set-default-font (face-font 'default) (face-font 'variable-pitch) (face-font 'fixed-pitch)))))


(defun nw-look-startup-after-init ()
  "Load defaults for the overall look -- to be called after loading the init file so as to pick up custom settings."
  (if window-system
      (progn
        (nw-set-geometry)
        (add-hook 'kill-emacs-hook 'nw-save-frame-geometry)
        (setq-default line-spacing 2)
        (nw-set-fonts)
        (add-to-list 'default-frame-alist `(font . ,nw-default-font))
        (set-face-font 'default nw-default-font)
        (set-face-font 'variable-pitch nw-variable-pitch-font)
        (set-face-font 'fixed-pitch nw-fixed-pitch-font)
        (add-to-list 'default-frame-alist '(internal-border-width . 0))
        (set-fringe-mode '(8 . 0)))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'nw-look-startup-after-init)


(provide 'nw-look)