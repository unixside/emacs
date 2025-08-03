;; straight.el
(defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
  	(url-retrieve-synchronously
  	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
  	 'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;; packages list
(setq package-list
        '(rainbow-mode
          rainbow-delimiters
          lua-mode
          rust-mode
          yaml-mode
          haskell-mode
          org-bullets
          olivetti
          which-key
          multiple-cursors
          ibuffer
          iedit
          projectile
          vterm
          vterm-toggle
          vertico
	  vertico-posframe
          cape
          orderless
          consult
          consult-recoll
          marginalia
          zencoding-mode
          nerd-icons
          nerd-icons-dired
          dired-open
          diredfl
          peep-dired
          counsel
          ligature
          yuck-mode
          ts-mode
	  accent
	  svg-lib
	  flyspell
	  flyspell-correct
	  tempel
	  tempel-collection
	  company-mode
	  company-box
	  lsp-mode
	  lsp-java
	  lsp-jedi
	  dap-mode
	  tree-sitter
	  tree-sitter-langs
	  catppuccin-theme
	  magit))

(dolist (pkg package-list)
  (straight-use-package pkg))
