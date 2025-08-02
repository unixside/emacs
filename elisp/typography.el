;; Fonts
(set-face-attribute 'default nil
                    :family "Iosevka NF"
                    :weight 'light
                    :height 130)

(set-face-attribute 'bold nil
                    :family "Iosevka NF"
                    :weight 'regular)

(set-face-attribute 'italic nil
                    :family "Iosevka NF"
                    :weight 'regular
                    :slant 'italic)

(set-fontset-font t 'unicode
                  (font-spec :name "Inconsolata") nil)

(set-fontset-font t '(#xe000 . #xffdd)
                  (font-spec :name "Iosevka NF"
                             :size 14) nil)

;; Font locks
(set-face-attribute 'font-lock-keyword-face nil
		    :family "Iosevka SS15"
		    :weight 'regular
		    :slant 'italic)

(set-face-attribute 'font-lock-type-face nil
		    :family "Iosevka SS07"
		    :weight 'regular
		    :slant 'normal)

(set-face-attribute 'font-lock-constant-face nil
		    :family "Iosevka SS12"
		    :weight 'light
		    :slant 'italic)

(set-face-attribute 'font-lock-function-name-face nil
		    :family "Iosevka SS12"
		    :weight 'regular
		    :slant 'italic)

(set-face-attribute 'font-lock-variable-name-face nil
		    :family "Iosevka SS12"
		    :weight 'regular)

(set-face-attribute 'font-lock-string-face nil
		    ;;:family "Iosevka SS05"
		    :weight 'light)

(set-face-attribute 'font-lock-doc-face nil
		    ;;:family "Iosevka SS05"
		    :weight 'light
		    :slant 'italic)

(set-face-attribute 'font-lock-comment-face nil
		    :family "Iosevka SS05"
		    :weight 'light
		    :slant 'italic)

(set-face-attribute 'font-lock-builtin-face nil
		    :family "Iosevka SS07"
		    :weight 'regular)

(set-face-attribute 'font-lock-preprocessor-face nil
		    :family "Iosevka NF"
		    :weight 'regular)

(set-face-attribute 'font-lock-negation-char-face nil
		    :family "Iosevka NF"
		    :weight 'regular
		    :slant 'italic)


;; Delimiters
(mapc (lambda (face)
	"Set family font and weight to FACE from rainbow delimiters faces list."
	(set-face-attribute face nil
			    :family "Iosevka SS15"
			    :weight 'semibold))
      '(rainbow-delimiters-depth-1-face
	rainbow-delimiters-depth-2-face
	rainbow-delimiters-depth-3-face
	rainbow-delimiters-depth-4-face
	rainbow-delimiters-depth-5-face
	rainbow-delimiters-depth-6-face
	rainbow-delimiters-depth-7-face
	rainbow-delimiters-depth-8-face
	rainbow-delimiters-depth-9-face
	rainbow-delimiters-unmatched-face
	rainbow-delimiters-mismatched-face))

;; Typography
(setq-default fill-column 80
  	      sentence-end-double-space nil
  	      bidi-paragraph-direction 'left-to-right
  	      truncate-string-ellipsis "…")

(setq x-underline-at-descent-line nil
      x-use-underline-position-properties t
      underline-minimum-offset 10)

;; Ligatures
(require 'ligature)
(ligature-set-ligatures 'prog-mode
			'("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                          ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                          "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                          "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                          "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                          "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                          "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                          "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                          ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                          "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                          "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                          "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                          "\\\\" "://"))
(add-hook 'prog-mode-hook #'ligature-mode)
