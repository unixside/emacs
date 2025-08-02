(defgroup chess nil
  "My own chess game in emacs lisp."
  :group 'convenience)

(defcustom cache-file "~/.emacs.d/elisp/chess/.lines"
  "File for save lines."
  :type 'string
  :group 'chess)

(defcustom draw-position-script "~/.emacs.d/elisp/chess/drawPosition.py"
  "Script for generate chess position png image."
  :type 'string
  :group 'chess)

(defcustom wcolor "#faf4ed"
  "Color for white box."
  :type 'string
  :group 'chess)

(defcustom bcolor "#575279"
  "Color for black box."
  :type 'string
  :group 'chess)

(defun save-line (line)
  (with-temp-file cache-file
    (insert line)))

(defun read-lines-cache ()
  (if (file-exists-p cache-file)
      (with-temp-buffer
	(insert-file-contents cache-file)
	(split-string (buffer-string) "\n" t))
    '()))

(defun get-chess-line (&optional line file swap)
  "TODO: docstring."
  (interactive)
  (let* ((insert-line (and line t))
	 (line  (or line (read-string "Insert line: ")))
	 (file  (read-file-name   "Insert file output: "))
	 (swap  (read-char-choice "Swap orientation ? [y/n]: " '(?y ?n)))
	 (cmd   (format "python %s -w '%s' -b '%s' -m '%s' -o %s %s"
			draw-position-script
			wcolor
			bcolor
			line
			file
			(if (char-equal swap ?y) "-s" ""))))
    (shell-command cmd)
    (unless insert-line
      (insert (format "=%s=" line)))
    (insert (format "\n[[%s]]\n" file))
    (org-redisplay-inline-images)))

(defun get-chess-line-region ()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position)
				(line-end-position))))
    (get-chess-line line)))

(provide 'chess)
