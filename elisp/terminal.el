(require 'vterm)
(require 'vterm-toggle)

(setq vterm-toggle-fullscreen-p nil
      vterm-toggle-cd-auto-create-buffer t)

(add-to-list 'display-buffer-alist
             '((lambda (bufname _) (equal bufname vterm-buffer-name))
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . bottom)
               (dedicated . t)
               (reusable-frames . visible)
               (window-height . 0.3)))

(global-set-key (kbd "C-x C-t") 'vterm-toggle)
