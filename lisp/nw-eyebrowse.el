(require 'pretty-hydra)
(require 'eyebrowse)

(defun nw-eyebrowse-layout-tag (slot)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (tag (nth 2 window-config))
         (name (cond
                ((not tag)          "") ; show nothing if it's not created yet
                ((= (length tag) 0) "default") ; default name if there is no tag
                (t                  tag)))
         (face (if (= slot (eyebrowse--get 'current-slot))
                   '(underline t)
                 'shadow)))
    (propertize name 'face face)))

(pretty-hydra-define nw-eyebrowse (:foreign-keys warn :quit-key "q")
  ("Actions"  (("TAB" eyebrowse-last-window-config   "last")
               ("n" eyebrowse-next-window-config   "next")
               ("p" eyebrowse-prev-window-config   "previous")
               ("r" eyebrowse-rename-window-config "rename")
               ("x" eyebrowse-close-window-config  "close")
               ("<escape>" nil "quit"))
   "Layouts"  (("1" eyebrowse-switch-to-window-config-1 (nw-eyebrowse-layout-tag 1) :exit t :width 30)
               ("2" eyebrowse-switch-to-window-config-2 (nw-eyebrowse-layout-tag 2) :exit t)
               ("3" eyebrowse-switch-to-window-config-3 (nw-eyebrowse-layout-tag 3) :exit t)
               ("4" eyebrowse-switch-to-window-config-4 (nw-eyebrowse-layout-tag 4) :exit t)
               ("5" eyebrowse-switch-to-window-config-5 (nw-eyebrowse-layout-tag 5) :exit t)
               ("6" eyebrowse-switch-to-window-config-6 (nw-eyebrowse-layout-tag 6) :exit t)
               ("7" eyebrowse-switch-to-window-config-7 (nw-eyebrowse-layout-tag 7) :exit t)
               ("8" eyebrowse-switch-to-window-config-8 (nw-eyebrowse-layout-tag 8) :exit t)
               ("9" eyebrowse-switch-to-window-config-9 (nw-eyebrowse-layout-tag 9) :exit t))))

(provide 'nw-eyebrowse)