(add-to-list 'load-path "~/.emacs.d/elisp/")

(dolist (config '("~/.emacs.d/elisp/general.el"
		  "~/.emacs.d/elisp/pkg.el"
		  "~/.emacs.d/elisp/utilities.el"
		  "~/.emacs.d/elisp/completion.el"
		  "~/.emacs.d/elisp/terminal.el"	   
		  "~/.emacs.d/elisp/org-mode.el"
		  "~/.emacs.d/elisp/agenda.el"
		  "~/.emacs.d/elisp/file-manager.el"		   
		  "~/.emacs.d/elisp/ui.el"
		  "~/.emacs.d/elisp/typography.el"))
  (load config))
