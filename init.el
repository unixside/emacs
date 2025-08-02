(dolist (config '("~/.emacs.d/elisp/general.el"
		  "~/.emacs.d/elisp/pkg.el"
		  "~/.emacs.d/elisp/utilities.el"
		  "~/.emacs.d/elisp/completion.el"
		  "~/.emacs.d/elisp/terminal.el"	   
		  "~/.emacs.d/elisp/org-mode.el"
		  "~/.emacs.d/elisp/agenda.el"
		  "~/.emacs.d/elisp/file-manager.el"		   
		  "~/.emacs.d/elisp/interface.el"
		  "~/.emacs.d/elisp/fonts.el"))
  (load config))
