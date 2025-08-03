;; Calendar
(setq calendar-date-style 'iso)
(setq calendar-week-start-day 1)

;; Agenda
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(require 'org-agenda)

(global-set-key (kbd "C-c a") 'org-agenda)

;; Files
(setq org-agenda-files '("~/Dropbox/Org/Agenda/Bandeja.org"
                         "~/Dropbox/Org/Agenda/Tareas.org"
                         "~/Dropbox/Org/Agenda/Trabajo.org"
                         "~/Dropbox/Org/Agenda/Habitos.org"
                         "~/Dropbox/Org/Agenda/Notas.org"
                         "~/Dropbox/Org/Agenda/Reuniones.org"))

(setq org-archive-location
      "~/Dropbox/Org/Agenda/Archivo/%s_store.org::datetree/")

;; Keywords
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "PROCESSING(p!)"	  
                  "LOCKED(l!)"   		  
                  "|" "CANCELLED(c!)" "DONE(d!)" "STORE(s!)")))
;; Faces
(setq org-todo-keyword-faces
      (quote (("TODO"       :inherit org-todo)
              ("DONE"       :inherit org-done)
              ("PROCESSING" :inherit font-lock-keyword-face)
              ("LOCKED"     :inherit font-lock-warning)
              ("CANCELED"   :inherit font-lock-comment-face)
              ("STORE"      :inherit font-lock-doc-face))))

(setq org-agenda-inhibit-startup t
      org-agenda-include-diary nil
      org-agenda-show-log t
      org-agenda-show-all-dates t
      org-agenda-time-in-grid t
      org-agenda-show-current-time-in-grid t
      org-agenda-span 7
      org-agenda-start-on-weekday 1
      org-agenda-sticky nil
      org-agenda-window-setup 'current-window
      org-agenda-use-tag-inheritance t
      org-show-habits-only-for-today t
      org-deadline-warning-days 7
      org-log-done 'time
      org-log-into-drawer "LOGBOOK"
      org-tags-column 0
      org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 1))
      org-refile-use-outline-path 'file
      org-agenda-block-separator "\s"
      org-habit-show-all-today t
      org-habit-show-habits nil
      org-extend-today-unitl 3)

(setq org-agenda-custom-commands
      '(
        ("d" "Hoy"
         ;; View curret day in the calendar view
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-format-date "")
                      (org-agenda-overriding-header "")))))
        ("t" "Tareas"
         ((alltodo ""
                   ((org-agenda-overriding-header "")))))
        ))

(setq org-capture-templates
      `(
        ("t" "Tareas"
         entry (file+headline "~/Dropbox/Org/Agenda/Tareas.org"
                              "Tareas")
         "* TODO %?\n Captured: %<%Y-%m-%d %I:%M %p>"
         :empty-lines 1 )
        ("h" "Habitos"
         entry (file+headline "~/Dropbox/Org/Agenda/Habitos.org"
                              "Habitos")
         "* TODO %?\n Captured: %<%Y-%m-%d %I:%M %p>"
         :empty-lines 1)
        ("i" "Reuniones"
         entry (file "~/Dropbox/Org/Agenda.org")
         "* Meeting with %? \n%T" :empty-lines 1)
        ("n" "Notas"
         entry (file+headline "~/Dropbox/Org/Agenda/Notas.org" "Notas")
         "* %?\n Captured: %<%Y-%m-%d %I:%M %p>"
         :empty-lines 1)
        )
      )

(global-set-key (kbd "C-c c") #'org-capture)
