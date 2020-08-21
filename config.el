;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevin Cunningham"
      user-mail-address "kevin@kevincunningham.co.uk"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "Iosevka" :size 20)
      doom-big-font (font-spec :family "Iosevka" :size 40)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville")
      doom-theme 'modus-operandi)

(setq org-directory "~/org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited
      ido-mode t
      ideo-enable-flex-matching t
      display-line-numbers-type t
      org-export-with-toc nil
      direnv-always-show-summary nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      company-idle-delay nil
      evil-want-fine-undo t ;; More granular than the single insert blob
      doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(remove-hook!
  'doom-first-buffer-hook #'smartparens-global-mode
  '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(electric-pair-mode +1)

;;
;; Clojure
;;
(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs)

;;
;; Helm
;;
(when (featurep! :completion helm)
  (map! "M-i"   #'helm-imenu)
  (map! :map helm-map
        "<tab>"   #'helm-execute-persistent-action))
(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 40
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-display-header-line t
      helm-split-window-in-side-p nil
      helm-move-to-line-cycle-in-source nil
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-echo-input-in-header-line nil)

;; some default settings from https://tecosaur.github.io/emacs-config/config.html#rudimentary-configuration
(setq-default
 tab-width 2
 )

;; mode-line settings
(display-time-mode 1)
(display-battery-mode 1)


;; Hides LF UTF-8 encoding
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package! deadgrep
  :if (executable-find "rg")
  :init
  (map! "M-s" #'deadgrep))


(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)


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
   ))
  )
      
  :config
  (require 'org-roam-protocol)
  (setq org-roam-graph-viewer "/usr/bin/open")


(after! (org-roam)
  (winner-mode +1)
  (map! :map winner-mode-map
        "<M-right>" #'winner-redo
        "<M-left>" #'winner-undo))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/org-roam"))
(require 'find-lisp)


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

(after! mixed-pitch
  (dolist (f (-filter (lambda (sym)
                        (s-prefix? "company-" (symbol-name sym)))
                      (face-list)))
    (pushnew! mixed-pitch-fixed-pitch-faces f)))

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


(add-hook 'dired-mode-hook 'org-download-enable)
(use-package! org-roam-server)
(setq org-roam-graph-viewer "/usr/bin/open")

;; Adds the projectile serach path
(setq projectile-project-search-path '("~/code" "~/code/egghead" "~/code/spinup" "~/code/personal"))

(require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)
    (require 'cider)

;; Allows autocomplete in ielm - the elisp REPL.
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; To allow org-export to deal with quotes.
(setq org-export-with-smart-quotes nil)

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

;; Settings for keycast
;;

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast-mode-line-update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))


(add-hook! 'org-mode-hook #'turn-on-org-pretty-table-mode)
(add-hook! 'org-mode-hook (lambda () (setq left-margin-width 2 right-margin-width 2)))

;; elcord
(setq elcord-use-major-mode-as-main-icon t)

(setq eros-eval-result-prefix "⟹ ")

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(setq spray-wpm 500
      spray-height 700)

(setq yas-triggers-in-field t)

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))

;;
;; isearch centers search results
;;

(defun private/after-jump ()
  "Centers vertically and flashes the current line."
  (interactive)
  (recenter)
  (+nav-flash/blink-cursor))

(add-hook! 'isearch-mode-end-hook #'private/after-jump)

(defadvice isearch-forward
    (after isearch-forward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-repeat-forward)

(defadvice isearch-backward
    (after isearch-backward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-backward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-repeat-backward)

(map! :leader
  (:prefix "b"
    :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))


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

;; Goto files
(map! :leader
      (:prefix "o" (
      :desc "Open GTD"      "g" (lambda () (interactive) (find-file "~/Dropbox/gtd/gtd.org"))
      :desc "Open inbox"    "i" (lambda () (interactive) (find-file "~/Dropbox/gtd/inbox.org"))
      )))

(use-package! rjsx-mode
  :mode "\\.js\\'")

(defun setup-tide-mode()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  )


;; typescript - needed for some of the js stuff
(use-package! tide
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package! prettier-js
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)))

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
           "it turns out that"))) 

(after! org
  (add-to-list 'org-modules 'org-habit))
;;(map! :leader 
;;      :desc "Open like spacemacs" "SPC" #'counsel-M-x)
