;;; +roam.el -*- lexical-binding: t; -*-



(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory "~/Dropbox/org-roam"
        org-roam-db-location "~/Dropbox/org-roam/org-roam.db"
        org-roam-graph-exclude-matcher "private")
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t)
           (("q" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t))
           ))
(setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n- tags:: \n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
        ("m" "meeting" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n:participants:\n* %<%m-%d-%Y  %H:%M>"
         :unnarrowed t
         :immediate-finish t)
        ("c" "clipboard" plain (function org-roam--capture-get-point)
         "#+BEGIN_SRC\n%^C\n#+END_SRC\n"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#i+SOURCE: \n- tags:: \n\n"
         :unnarrowed t)
        ("P" "person" plain (function org-roam--capture-get-point)
"- tags::
* How We Met
* Company
* Location
* Interests
* Interesting facts"
         :filename "${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)
   ))
  :config
  (require 'org-roam-protocol)
  (setq org-roam-graph-viewer "/usr/bin/open")
  )



(after! (org-roam)
  (winner-mode +1)
  (map! :map winner-mode-map
        "<M-right>" #'winner-redo
        "<M-left>" #'winner-undo))

;; org-agenda
(setq org-agenda-files
      (list "~/Dropbox/org-roam/todos.org"
            "~/Dropbox/org-roam/spinuptodos.org"
            "~/Dropbox/org-roam/eggheadtodos.org"
            "~/Dropbox/gtd/inbox.org"
            "~/Dropbox/gtd/gtd.org"
            "~/Dropbox/gtd/tickler.org"
            ))

;; org-journal
(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "~/Dropbox/org-roam"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(setq org-capture-templates `(
        ("l" "link" entry (file+headline "~/Dropbox/gtd/inbox.org" "Inbox")
        "* TODO [[%^{link}][\"%^{description}\"]]\n")
        ("t" "Todo [inbox]" entry
                (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
                 "* TODO %i%?")
        ("T" "Tickler" entry
        (file+headline "~/Dropbox/gtd/tickler.org" "Tickler")
        "* %i%? \n %U")
))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

;; GTD refiling
(setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/gtd/someday.org" :level . 1)
                           ("~/Dropbox/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-method '+org/org-download-method))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))
;; Setup pretty priorities
(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "❗")
                                   (?B . "⬆")
                                   (?C . ""))))

;; Pomodoro sounds from Daniel
(setq org-pomodoro-play-sounds t
      org-pomodoro-short-break-sound-p t
      org-pomodoro-long-break-sound-p t
      org-pomodoro-short-break-sound (expand-file-name "/System/Library/Sounds/Glass.aiff")
      org-pomodoro-long-break-sound (expand-file-name "/System/Library/Sounds/Glass.aiff")
      org-pomodoro-finished-sound (expand-file-name "/System/Library/Sounds/Glass.aiff"))


(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)
(add-hook 'dired-mode-hook 'org-download-enable)
(use-package! org-roam-server)
(setq org-roam-graph-viewer "/usr/bin/open")
(add-hook! 'org-mode-hook #'turn-on-org-pretty-table-mode)
(add-hook! 'org-mode-hook (lambda () (setq left-margin-width 2 right-margin-width 2)))
(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))
(use-package! writegood-mode
  :hook (text-mode org-mode)
  :diminish
  ;; Some additional weasel words.
  :config
  (--map (push it writegood-weasel-words)
         '("some" "simple" "simply" "easy" "often" "easily" "probably"
           "clearly"               ;; Is the premise undeniably true?
           "experience shows"      ;; Whose? What kind? How does it do so?
           "may have"              ;; It may also have not!
           "it turns out that"))
(setq-hook! org-mode
  org-log-done t
  org-image-actual-width '(700)
  org-clock-into-drawer t
  org-clock-persist t
  org-columns-default-format "%60ITEM(Task) %20TODO %10Effort(Effort){:} %10CLOCKSUM"
  org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                ("STYLE_ALL" . "habit")))
  org-duration-format '((special . h:mm))
  org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  bidi-paragraph-direction t
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t
  ))
(use-package! org-gcal)
(map! "C-c c" #'org-capture)
