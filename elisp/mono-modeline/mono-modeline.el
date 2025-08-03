;; mono-modeline.el --- -*- lexical-binding: t -*-
(require 'nerd-icons)

(defgroup mono-modeline nil
  "A cute minor mode line for the Emacs."
  :group 'convenience)

(defcustom mono-modeline-thin-height 0.1
  "Height for mode-line face.
   Used this variable when the value of `mono-modeline-thin-modeline' is t"
  :type 'float
  :group 'mono-modeline)

(defcustom mono-modeline-background-color "#f2e9e1"
  "Background color for header line."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-icon-RW-color "#575279"
  "Foreground for icon when buffer is read and write status."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-icon-**-color "#b4637a"
  "Foreground for icon when buffer is edited status."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-icon-RO-color "#56949f"
  "Foreground for icon when buffer is read only status."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-name-color "#575279"
  "Foreground for buffer name."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-right-color "#9893a5"
  "Foreground for text from right side of tabline."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-i-color "#9893a5"
  "Foreground for inactive tab line."
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-name-height 1.1
  "Height for the buffer name."
  :type 'float
  :group 'mono-modeline)

(defcustom mono-modeline-right-height 1.1
  "Height for the text on right side tabline."
  :type 'float
  :group 'mono-modeline)

(defcustom mono-modeline-icon-height 1.8
  "Height for the icon buffer."
  :type 'float
  :group 'mono-modeline)

(defcustom mono-modeline-thin-color "#907aa9"
  "Color for active modeline"
  :type 'string
  :group 'mono-modeline)

(defcustom mono-modeline-thin-i-color "#9893a5"
  "Color for active modeline"
  :type 'string
  :group 'mono-modeline)

(defface mono-modeline-icon-RW-face
  '((t :inherit nil
       :box nil))
  "Face for icon when is read and write mode."
  :group 'mono-modeline)

(defface mono-modeline-icon-**-face
  '((t :inherit nil
       :box nil))
  "Face for icon when `buffer-modified-p' is t."
  :group 'mono-modeline)

(defface mono-modeline-icon-RO-face
  '((t :inherit nil
       :box nil))
  "Face for icon when `buffer-read-only' is t."
  :group 'mono-modeline)

(defface mono-modeline-icon-i-face
  '((t :inherit nil
       :box nil))
  "Face for icon when buffer-window is unfocused."
  :group 'mono-modeline)

(defface mono-modeline-name-face
  '((t :inherit nil
       :box nil))
  "Face for buffer name."
  :group 'mono-modeline)

(defface mono-modeline-name-i-face
  '((t :inherit nil
       :box nil))
  "Face for buffer name when buffer-window is unfocused."
  :group 'mono-modeline)

(defface mono-modeline-right-face
  '((t :inherit nil
       :box nil))
  "Face for text from right side of header line."
  :group 'mono-modeline)

(defface mono-modeline-right-i-face
  '((t :inherit nil
       :box nil))
  "Face for text in right side of header line when buffer-window is unfocused."
  :group 'mono-modeline)

(defun mono-modeline-setup-faces ()
  (custom-set-faces
   `(mono-modeline-icon-RW-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-icon-RW-color
	 :height ,mono-modeline-icon-height)))
   `(mono-modeline-icon-**-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-icon-**-color
	 :height ,mono-modeline-icon-height)))
   `(mono-modeline-icon-RO-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-icon-RO-color
	 :height ,mono-modeline-icon-height)))
   `(mono-modeline-icon-i-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-i-color
	 :height ,mono-modeline-icon-height)))
   `(mono-modeline-name-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-name-color
	 :height ,mono-modeline-name-height
	 :weight bold
	 :slant italic)))
   `(mono-modeline-name-i-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-i-color
	 :height ,mono-modeline-name-height
	 :weight regular
	 :slant italic)))
   `(mono-modeline-right-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-right-color
	 :height ,mono-modeline-right-height
	 :weight regular
	 :slant italic)))
   `(mono-modeline-right-i-face
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-i-color
	 :height ,mono-modeline-right-height
	 :weight regular
	 :slant italic)))
   `(header-line
     ((t :inherit nil
	 :height ,mono-modeline-name-height
	 :background ,mono-modeline-background-color)))))

(defun mono-modeline-thin-enable ()
  "Format the mode-line like a thin line."
  (setq-default mode-line-format (list ""))
  (custom-set-faces
   `(mode-line
     ((t :background ,mono-modeline-icon-RW-color
	 :foreground ,mono-modeline-icon-RW-color
	 :height     ,mono-modeline-thin-height
	 :box        nil)))
   `(mode-line-active
     ((t :background ,mono-modeline-icon-RW-color
	 :foreground ,mono-modeline-icon-RW-color
	 :height     ,mono-modeline-thin-height
	 :box        nil)))
   `(mode-line-inactive
     ((t :background ,mono-modeline-background-color
	 :foreground ,mono-modeline-background-color
	 :height     ,mono-modeline-thin-height
	 :box        nil)))))

(defun mono-modeline-get-icon (&optional buffer)
  (let* ((buffer (or buffer (current-buffer)))	 
	 (default (char-to-string #xe632))) ;  emacs icon
    (with-current-buffer buffer
      (format "%s "
	      (if (or buffer-file-name (nerd-icons-auto-mode-match?))
	  (char-to-string (aref (nerd-icons-icon-for-buffer) 0)) default)))))

(defun mono-modeline-get-buffer-name (&optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
	 (limit-char (ceiling (* (window-total-width) 0.70)))
	 (file       (buffer-file-name buffer)))
    (format "%s " (if (and file (< (length file) limit-char))
		      (abbreviate-file-name file)
		    (concat " " (buffer-name buffer))))))

(defun mono-modeline-get-icon-face (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (cond (buffer-read-only           'mono-modeline-icon-RO-face)
	  ((buffer-modified-p buffer) 'mono-modeline-icon-**-face)
	  (t                          'mono-modeline-icon-RW-face))))

(defun mono-modeline-format (buffer &optional icon name right-text)
  (let* ((active (eq mono-modeline--selected-window
		     (get-buffer-window buffer)))
	 (icon   (or icon (mono-modeline-get-icon buffer)))
	 (name   (or name (mono-modeline-get-buffer-name buffer)))
	 (right-text (or right-text (format-mode-line "%l:%c"))) 
	 (reserve-area  (+ (length right-text) 2))
	 (icon-face (if active
			(mono-modeline-get-icon-face buffer)
		      'mono-modeline-icon-i-face))
	 (name-face (if active
			'mono-modeline-name-face
		      'mono-modeline-name-i-face))
	 (right-face (if active
			 'mono-modeline-right-face
		       'mono-modeline-right-i-face)))
    (concat
     (propertize " "  'face name-face 'display `(raise 0.25))
     (propertize icon 'face icon-face)
     (propertize name 'face name-face 'display `(raise 0.15))
     (propertize " "  'face name-face
		 'display `(space :align-to (- right ,reserve-area)))
     (propertize right-text 'face right-face 'display `(raise 0.15)))))

(defvar mono-modeline--selected-window nil
  "Variable for select inactive variants faces.")

(defun mono-modeline-update-selected-window ()
  "Function for update selected-window register."
  (setq mono-modeline--selected-window (selected-window)))

(defun mono-modeline-enable ()
  (mono-modeline-setup-faces)
  (setq-default header-line-format
		'(:eval
		  (cond ((derived-mode-p 'org-agenda-mode)
			  (mono-modeline-format
			   (current-buffer) "󰄵 " (upcase org-agenda-name)
			   (format-time-string "%A %d, %B %Y  ")))
			 (t (mono-modeline-format (current-buffer)))))))

;;;###autoload
(define-minor-mode mono-modeline-mode
  "My own mode for header-line."
  :global t
  :init-value nil
  (if mono-modeline-mode
      (progn
	(mono-modeline-enable)
	(add-hook 'post-command-hook #'mono-modeline-update-selected-window))
    (remove-hook 'post-command-hook #'mono-modeline-update-selected-window)
    (setq-default header-line-format nil)))

(provide 'mono-modeline)
