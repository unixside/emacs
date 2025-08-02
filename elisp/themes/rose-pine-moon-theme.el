(deftheme rose-pine-moon
  "Alternative dark variant of rose pine theme")

(let ((base    "#232136")
      (surface "#2a273f")
      (overlay "#393552")
      (muted   "#6e6a86")
      (subtle  "#908caa")
      (text    "#e0def4")
      (love    "#eb6f92")
      (gold    "#f6c177")
      (rose    "#ea9a97")
      (pine    "#3e8fb0")
      (foam    "#9ccfd8")
      (iris    "#c4a7e7")
      (hl-low  "#2a283e")
      (hl-med  "#44415a")
      (hl-high "#56526e"))

  (custom-theme-set-faces
   'rose-pine-moon
   `(default                          ((t (:foreground ,text
					   :background ,base))))   
   `(hl-line                          ((t (:background ,hl-low))))
   `(fringe                           ((t (:background ,base))))
   `(highlight                        ((t (:background ,hl-low))))
   `(warning                          ((t (:foreground ,love))))
   `(minibuffer-prompt                ((t (:foreground ,foam))))
   `(link                             ((t (:foreground ,foam
						       :underline t))))
   `(link-visited                     ((t (:foreground ,iris
						       :underline t))))

   `(company-echo                     ((t (:background ,overlay))))
   `(company-echo-common              ((t (:background ,surface))))

   `(company-box-annotation           ((t (:foreground ,text
					   :background ,overlay))))
   `(company-box-background           ((t (:background ,overlay))))
   `(company-box-selection            ((t (:background ,hl-med))))
   `(company-preview                  ((t (:background ,overlay))))
   `(company-preview-common           ((t (:background ,overlay))))

   `(company-scrollbar-fg             ((t (:background ,muted))))
   `(company-scrollbar-bg             ((t (:background ,surface))))

   `(company-box-scrollbar            ((t (:foreground ,muted
					   :background ,surface))))
   `(company-tooltip                  ((t (:background ,overlay))))
   `(company-tooltip-common           ((t (:background ,overlay))))
   `(company-tooltip-search           ((t (:background ,overlay))))

   `(region                           ((t (:background ,hl-med))))
   `(show-paren-match                 ((t (:background ,hl-med))))
   `(show-paren-match-expression      ((t (:background ,hl-med))))
   `(show-paren-mismatch              ((t (:background ,hl-med))))
   
   ;; Line numbers
   `(line-number                      ((t (:foreground ,muted
					   :background ,base
					   :weight light :slant italic))))
   `(line-number-current-line         ((t (:foreground ,iris
					   :background ,hl-low
					   :weight bold
					   :slant italic))))          

   ;; Windows dividers
   `(window-divider                   ((t (:foreground ,base
					   :background ,base))))
   `(window-divider-first-pixel       ((t (:foreground ,base
					   :background ,base))))
   `(window-divider-last-pixel        ((t (:foreground ,base
					   :background ,base))))

   ;; Mode line
   `(mode-line                        ((t (:foreground ,text
					   :background ,overlay))))
   `(mode-line-active                 ((t (:foreground ,text
					   :background ,overlay))))
   `(mode-line-inactive               ((t (:foreground ,text
					   :background ,overlay))))     

   ;; Header line
   `(header-line                      ((t (:background ,base :box nil))))
   `(header-line-active               ((t (:background ,base :box nil))))
   `(header-line-inactive             ((t (:background ,base :box nil))))

   ;; Font locks
   `(font-lock-warning-face           ((t (:foreground ,love
						       :weight regular))))
   `(font-lock-function-name-face     ((t (:foreground ,rose
						       :weight regular
						       :slant italic))))
   `(font-lock-function-call-face     ((t (:foreground ,rose
						       :weight semilight
						       :slant normal))))
   `(font-lock-variable-name-face     ((t (:foreground ,text
						       :weight regular
						       :slant normal))))
   `(font-lock-variable-use-face      ((t (:foreground ,text
						       :weight semilight
						       :slant normal))))
   `(font-lock-keyword-face           ((t (:foreground ,pine
						       :weight regular
						       :slant italic))))
   `(font-lock-comment-face           ((t (:foreground ,muted
						       :weight light
						       :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,muted
						       :weight regular
						       :slant italic))))
   `(font-lock-type-face              ((t (:foreground ,foam
						       :weight regular
						       :slant italic))))
   `(font-lock-constant-face          ((t (:foreground ,foam
						       :weight light
						       :slant italic))))
   `(font-lock-builtin-face           ((t (:foreground ,love
						       :weight regular
						       :slant normal))))
   `(font-lock-preprocessor-face      ((t (:foreground ,love
						       :weight light
						       :slant normal))))
   `(font-lock-string-face            ((t (:foreground ,gold
						       :weight light
						       :slant normal))))
   `(font-lock-doc-face               ((t (:foreground ,gold
						       :weight light
						       :slant normal))))
   `(font-lock-doc-markup-face        ((t (:foreground ,foam
						       :weight light
						       :slant normal))))
   `(font-lock-negation-char-face     ((t (:foreground ,love
						       :weight regular
						       :slant italic))))
   `(font-lock-scape-face             ((t (:foreground ,love
						       :weight light
						       :slant normal))))
   `(font-lock-number-face            ((t (:foreground ,rose
						       :weight light
						       :slant normal))))
   `(font-lock-operator-face          ((t (:foreground ,subtle
						       :weight regular
						       :slant normal))))
   `(font-lock-punctuation-face       ((t (:foreground ,subtle
						       :weight regular
						       :slant normal))))
   `(font-lock-delimiter-face         ((t (:foreground ,rose
						       :weight regular
						       :slant normal))))
   `(font-lock-property-name-face     ((t (:foreground ,foam
						       :weight light
						       :slant normal))))
   `(font-lock-property-use-face      ((t (:foreground ,foam
						       :weight light
						       :slant italic))))
   `(font-lock-misc-punctuation-face  ((t (:foreground ,iris
						       :weight regular))))
   
   ;; VTerm mode
   `(vterm-color-black          ((t (:foreground ,muted))))
   `(vterm-color-red            ((t (:foreground ,love))))
   `(vterm-color-green          ((t (:foreground ,pine))))
   `(vterm-color-yellow         ((t (:foreground ,gold))))
   `(vterm-color-blue           ((t (:foreground ,foam))))
   `(vterm-color-magenta        ((t (:foreground ,iris))))
   `(vterm-color-cyan           ((t (:foreground ,rose))))
   `(vterm-color-white          ((t (:foreground ,text))))
   `(vterm-color-bright-black   ((t (:foreground ,muted))))
   `(vterm-color-bright-red     ((t (:foreground ,love))))
   `(vterm-color-bright-green   ((t (:foreground ,pine))))
   `(vterm-color-bright-yellow  ((t (:foreground ,gold))))
   `(vterm-color-bright-blue    ((t (:foreground ,foam))))
   `(vterm-color-bright-magenta ((t (:foreground ,iris))))
   `(vterm-color-bright-cyan    ((t (:foreground ,rose))))
   `(vterm-color-bright-white   ((t (:foreground ,text))))

   ;; Rainbow delimiters mode
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,pine
					   :weight semibold))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,foam
					   :weight semibold))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,gold
					   :weight semibold))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,rose
					   :weight semibold))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,love
					   :weight semibold))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,iris
					   :weight semibold))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,pine
					   :weight semibold))))
   `(rainbow-delimiters-depth-8-face    ((t (:foreground ,foam
					   :weight semibold))))
   `(rainbow-delimiters-depth-9-face    ((t (:foreground ,gold
					   :weight semibold))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,muted
					     :weight semibold))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,love
					     :weight semibold))))

   ;; Dired full color
   `(diredfl-dir-priv    ((t (:background ,base :foreground ,pine :weight bold))))
   `(diredfl-read-priv   ((t (:background ,base :foreground ,gold :weight bold))))
   `(diredfl-write-priv  ((t (:background ,base :foreground ,love :weight bold))))
   `(diredfl-exec-priv   ((t (:background ,base :foreground ,pine :weight regular))))
   `(diredfl-no-priv     ((t (:background ,base :foreground ,muted :weight bold))))
   `(diredfl-date-time   ((t (:background ,base :foreground ,foam :weight regular))))
   `(diredfl-dir-heading ((t (:background ,base :foreground ,text :weight bold :slant italic))))
   `(diredfl-dir-name    ((t (:background ,base :foreground ,pine :weight regular))))
   `(diredfl-file-name   ((t (:background ,base :foreground ,text :weight regular))))
   `(diredfl-file-suffix ((t (:background ,base :foreground ,text :weight regular))))
   `(diredfl-number      ((t (:background ,base :foreground ,foam :weight light))))


   ;; Org Mode Faces
   `(variable-pitch ((t (:weight light :height 140))))
   `(fixed-pitch    ((t (:weight light :height 140))))

   `(org-level-8 ((t (:weight normal :foreground ,foam))))
   `(org-level-7 ((t (:weight normal :foreground ,pine))))
   `(org-level-6 ((t (:weight normal :foreground ,iris))))
   `(org-level-5 ((t (:weight normal :foreground ,love))))
   `(org-level-4 ((t (:weight normal :foreground ,rose))))
   `(org-level-3 ((t (:weight normal :foreground ,gold))))
   `(org-level-2 ((t (:weight normal :foreground ,foam))))
   `(org-level-1 ((t (:weight normal :foreground ,pine))))
   
   `(org-document-title        ((t (:weight bold
					    :height 1.2
					    :underline nil))))
   `(org-block                 ((t (:inherit fixed-pitch
					     :background ,hl-low))))
   `(org-code                  ((t (:inherit (shadow fixed-pitch)))))
   `(org-document-info         ((t (:foreground ,text ))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
   `(org-link                  ((t (:foreground ,iris :underline t))))
   `(org-meta-line             ((t (:inherit (font-lock-comment-face
					      fixed-pitch)))))
   `(org-property-value        ((t (:inherit fixed-pitch))) t)
   `(org-special-keyword       ((t (:inherit (font-lock-comment-face
					      fixed-pitch)))))
   `(org-table                 ((t (:inherit fixed-pitch
					     :foreground ,subtle)))) 
   `(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
   `(org-drawer                ((t (:inherit fixed-pitch
					     :foreground ,muted))))
   `(org-hide                  ((t (:inherit fixed-pitch
					     :foreground ,base))))
   `(org-tag                   ((t (:foreground ,muted
						:weight regular ))))
   `(org-todo                  ((t (:foreground ,love :weight bold))))
   `(org-done                  ((t (:foreground ,muted :weight bold))))
   `(org-date                  ((t (:foreground ,muted
						:weight light
						:underline nil))))

   `(org-agenda-calendar-daterange ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-calendar-event     ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-calendar-sexp      ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-clocking           ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-column-dateline    ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-current-time       ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-date               ((t (:foreground ,foam
						    :weight light
						    :slant normal))))
   `(org-agenda-date-today         ((t (:foreground ,pine
						    :weight bold
						    :slant italic))))
   `(org-agenda-date-weekend       ((t (:foreground ,pine
						    :weight bold
						    :slant italic))))
   `(org-agenda-date-weekend-today ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-diary              ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-dimmed-todo-face   ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-done               ((t (:foreground ,muted
						    :weight light))))
   `(org-agenda-filter-category    ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-filter-effort      ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-filter-regexp      ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-filter-tags        ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-restriction-lock   ((t (:foreground ,text
						    :weight regular))))
   `(org-agenda-structure          ((t (:foreground ,text
						    :weight bold
						    :slant italic)))) 
   `(org-agenda-structure-filter   ((t (:foreground ,love
						    :weight light
						    :slant italic))))
   `(org-agenda-structure-secondar ((t (:foreground ,text
						    :weight regular))))
   `(org-scheduled                 ((t (:foreground ,text
						    :weight light))))
   `(org-scheduled-previously      ((t (:foreground ,iris
						    :weight light))))
   `(org-scheduled-today           ((t (:foreground ,text
						    :weight light))))
   `(org-imminent-deadline         ((t (:foreground ,love
						    :weight light))))
   `(org-upcoming-deadline         ((t (:foreground ,love
						    :weight light))))
   `(org-upcoming-distant-deadline ((t (:foreground ,rose
						    :weight light))))
   `(org-time-grid                 ((t (:foreground ,gold
						    :weight light))))
   `(org-priority                  ((t (:foreground ,pine
						    :weight regular)))))
    )

(provide-theme 'rose-pine-moon)
(provide 'rose-pine-moon-theme)
