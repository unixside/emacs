;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set LANG environment variable
(setenv "LANG" "en_US.UTF-8")
					; Default to utf-8 encoding
(set-default-coding-systems 'utf-8)
					; Add utf-8 at the front for automatic detection.
(prefer-coding-system       'utf-8)
					; Coding system of terminal output
(set-terminal-coding-system 'utf-8)
					; Coding system for keyboard input on TERMINAL
(set-keyboard-coding-system 'utf-8)
					; Set up multilingual environment
(set-language-environment "English")   

(setq-default inhibit-startup-screen t      
              inhibit-startup-message t     
              inhibit-startup-echo-area-message t 
              initial-scratch-message "" 
              initial-buffer-choice t
              inhibit-x-resources t
	      use-short-answers t
	      confirm-nonexistent-file-or-buffer nil)

;; Locale time
(setq system-time-locale "C")

(setq-default c-basic-offset 4)
(electric-pair-mode t)
(setq make-backup-files nil) ; stop creating ~ files

;; Scroll 
(setq scroll-step            1
      scroll-conservatively  10000
      pixel-scroll-precision-mode t
      pixel-scroll-precision-large-scroll-height 40.0)
(pixel-scroll-precision-mode 1)

;; Recent files
(require 'recentf)
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100)

(let (message-log-max)
  (recentf-mode 1))

;; Clipboard
(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

;; Text
(setq-default use-short-answers t                     ; Replace yes/no prompts with y/n
              confirm-nonexistent-file-or-buffer nil) ; Ok to visit non existent files
(delete-selection-mode 1)

;; Scroll
(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
              scroll-margin 2                 ; Add a margin when scrolling vertically
              recenter-positions '(5 bottom)) ; Set re-centering positions
