;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes/")
(setq catppuccin-flavor 'macchiato)
(load-theme 'rose-pine-dawn  :no-confirm)

;; Delimiters highlighting
(require 'rainbow-delimiters)
(add-hook 'org-mode-hook  #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Frame
(require 'frame)

;; Default frame settings
(setq default-frame-alist '((min-height . 20)  (height . 40)
                            (min-width  . 40)  (width  . 84)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)
(setq x-underline-at-descent-line t)

;; Functions
(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
      FRAME can be a frame name, a terminal name, or a frame.
      If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height)
           ;; for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position
             (flatten-list (list frame center))))))

;; Window
(setq-default window-divider-default-right-width 24
              window-divider-default-places 'right-only		
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil)

(set-face-attribute 'window-divider nil
		    :foreground (face-background 'default)
		    :background (face-background 'default))
(set-face-attribute 'window-divider-first-pixel nil
		    :foreground (face-background 'default)
		    :background (face-background 'default))
(set-face-attribute 'window-divider-last-pixel nil
		    :foreground (face-background 'default)
		    :background (face-background 'default))

(window-divider-mode 1)

;; Dialog
(setq-default show-help-function nil    ; No help text
  	      use-file-dialog nil       ; No file dialog
  	      use-dialog-box nil        ; No dialog box
  	      pop-up-windows nil)       ; No popup windows

(tooltip-mode -1)                       ; No tooltips
(scroll-bar-mode -1)                    ; No scroll bars
(tool-bar-mode -1)                      ; No toolbar

;; Cursor
(setq-default cursor-type '(bar . 8)
	      cursor-in-non-selected-window nil
	      cursor-intangible-mode t
	      x-stretch-cursor nil)
(blink-cursor-mode 1)

;; Line numbers
(require 'display-line-numbers)
(setq display-line-numbers-type 'absolute)
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(global-set-key (kbd "C-x n") 'display-line-numbers-mode)

;; Headerline and modeline
(add-to-list 'load-path "~/.emacs.d/elisp/mono-modeline")
(require 'mono-modeline)

;; (setq mono-modeline-background-color (catppuccin-color 'crust 'macchiato)
;;       mono-modeline-icon-RW-color    (catppuccin-color 'mauve 'macchiato)
;;       mono-modeline-icon-**-color    (catppuccin-color 'red   'macchiato)
;;       mono-modeline-icon-RO-color    (catppuccin-color 'teal  'macchiato)
;;       mono-modeline-name-color       (catppuccin-color 'blue  'macchiato)
;;       mono-modeline-right-color      (catppuccin-color 'surface1  'macchiato))

(mono-modeline-mode)
(mono-modeline-thin-enable)
