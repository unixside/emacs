;; General
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode))
(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-image-actual-width nil)
(setq-default org-support-shift-select t)
(setq-default org-fontify-quote-and-verse-blocks t)
(setq-default org-hide-emphasis-markers nil)
(setq-default org-cycle-inline-images-display t
              org-image-actual-width 450
              org-image-align 'center)
(setq enable-local-eval t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<) t
                             (,electric-pair-inhibit-predicate c))))))

;; Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; Olivetti and writer-mode
(require 'olivetti)

(add-to-list 'load-path "~/.emacs.d/elisp/writer/")
(require 'writer-mode)

;; Functions
(defun mda/org-open-current-window ()
  "Opens file in current window."                  
  (interactive)                                              
  (let ((org-link-frame-setup (cons (cons 'file 'find-file)
				    org-link-frame-setup)))
    (org-open-at-point)))

(define-key global-map (kbd "C-o") #'mda/org-open-current-window)

;; My program for generate png images from chess positions
(add-to-list 'load-path "~/.emacs.d/elisp/chess/")
(require 'chess)
