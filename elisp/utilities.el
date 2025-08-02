;; Orderless
(require 'orderless)
(setq completion-styles '(substring orderless basic)
      orderless-component-separator 'orderless-escapable-split-on-space
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(require 'consult)
(setq consult-preview-key nil) ; No live preview

(defun my/consult-line ()
  "Consult line with live preview"
  (interactive)
  (let ((consult-preview-key 'any)
        (mini-frame-resize 'grow-only)) ;; !! Important
    (consult-line)))

(defun my/consult-goto-line ()
  "Consult goto line with live preview"  
  (interactive)
  (let ((consult-preview-key 'any))
    (consult-goto-line)))

(bind-key "C-x C-r" #'consult-recent-file)
(bind-key "C-x h"   #'consult-outline)
(bind-key "C-x b"   #'consult-buffer)
(bind-key "C-c h"   #'consult-history)
(bind-key "C-s"     #'my/consult-line)
(bind-key "M-g g"   #'my/consult-goto-line)
(bind-key "M-g M-g" #'my/consult-goto-line)

;; Vertico
(require 'vertico)
(setq vertico-resize nil        ; How to resize the Vertico minibuffer window.
      vertico-count 10          ; Maximal number of candidates to show.
      vertico-count-format nil) ; No prefix with number of entries

      
(vertico-mode)

;; (setq vertico-grid-separator
;;       #("  |  " 2 3 (display (space :width (1))
;;                              face (:background "#ECEFF1")))

;;       vertico-group-format
;;       (concat #(" " 0 1 (face vertico-group-title))
;;               #(" " 0 1 (face vertico-group-separator))
;;               #(" %s " 0 4 (face vertico-group-title))
;;               #(" " 0 1 (face vertico-group-separator
;;                               display (space
;; 				       :align-to (- right (-1 . right-margin)
;; 						    (- +1)))))))

;; (set-face-attribute 'vertico-group-separator nil
;;                     :strike-through t)

;; (setq completion-in-region-function
;;       (lambda (&rest args)
;;         (apply (if vertico-mode
;;                    #'consult-completion-in-region
;;                  #'completion--in-region)
;;                args)))


;; (defun minibuffer-format-candidate (orig cand prefix suffix index _start)
;;   (let ((prefix (if (= vertico--index index)
;;                     "  "
;;                   "   "))) 
;;     (funcall orig cand prefix suffix index _start)))

;; (advice-add #'vertico--format-candidate
;;             :around #'minibuffer-format-candidate)

;;   (defun minibuffer-vertico-setup ()
;;     (setq truncate-lines t)
;;     (setq completion-in-region-function
;;           (if vertico-mode
;;               #'consult-completion-in-region
;;             #'completion--in-region)))

;; (add-hook 'vertico-mode-hook #'minibuffer-vertico-setup)
;; (add-hook 'minibuffer-setup-hook #'minibuffer-vertico-setup)

;; Marginalia
(require 'marginalia)
(setq-default marginalia--ellipsis "…"    ; Nicer ellipsis
              marginalia-align 'right     ; right alignment
              marginalia-align-offset -1) ; one space on the right
(marginalia-mode)

;; Icons
(require 'nerd-icons)

;; Which Key
(require 'which-key)
(which-key-mode t)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; IEdit
(require 'iedit)
(global-set-key (kbd "C-:") 'iedit-mode)

;; Functions
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun lean/copy-line ()
  "Copies the current line wiout moving the column cursor."
  (interactive)
  (let* ((line (buffer-substring-no-properties
		(line-beginning-position)
		(line-end-position)))
	 (new-pos (+ (length line) (point) 1)))
    (end-of-line)
    (insert (format "\n%s" line))
    (goto-char new-pos)))

(bind-key "C-." #'lean/copy-line)

;; Projectile
(require 'projectile)
(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun my-projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
  	 (or (not (projectile-run-command (projectile-compilation-dir)))
  	     prompt)))
    (projectile-run-project prompt)))

;; Accents
(global-set-key (kbd "C-x C-a") 'accent-menu)
