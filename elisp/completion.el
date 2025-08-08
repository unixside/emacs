;; Company
(require 'company)
(require 'company-box)

(setq-default company-minimum-prefix-length 3
              company-idle-delay 0)
(global-company-mode)

;; Emmet
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; Languages modes
(require 'lua-mode)
(require 'yaml-mode)
(require 'haskell-mode)
(require 'rust-mode)
(require 'yuck-mode)
(require 'ts-mode)

;; Rust
(add-hook 'rust-mode-hook '(setq indent-tabs-mode nil))
(add-hook 'rust-mode-hook '(prettify-symbols-mode))

(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "C-c C-o") 'rust-compile)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run-clippy)

;; LSP Emacs like
(require 'lsp)
(require 'lsp-java)
(require 'lsp-jedi)
(require 'dap-mode)
(require 'dap-java)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-symbol-highlighting nil
      lsp-headerline-breadcrumb-enable nil
      lsp-ui-sideline-enable nil
      lsp-eldoc-enable-hover nil
      lsp-ui-sideline-show-diagnostics nil
      lsp-rust-server 'rust-analyzer
      lsp-enable-file-watchers nil)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'lua-mode-hook 'lsp)
(add-hook 'java-mode-hook 'lsp)
(add-hook 'js-mode-hook 'lsp)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

;; Dictionary for writer mode
(require 'ispell)
(require 'flyspell)
(require 'flyspell-correct)
(setq ispell-dictionary "es_AR"
      ispell-program-name "hunspell")

;(add-hook 'text-mode-hook 'flyspell-mode)

(require 'tempel)

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-_" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
