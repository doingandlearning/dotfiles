;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevin Cunningham"
      user-mail-address "kevin@kevincunningham.co.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq doom-font (font-spec :family "Jetbrains Mono" :size 16))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

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
           )))
      


  :config
  (require 'org-roam-protocol)
  (setq org-roam-graph-viewer "/usr/bin/open")

  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("l" "Link" entry (file+headline "~/Dropbox/org-roam/links.org" "Links")
           "* %a %^g\n %?\n %i")
          ))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/org-roam"))
(setq rmh-elfeed-org-files "~/.elfeed/elfeed.org")
(require 'find-lisp)
(setq kevin/org-agenda-directory "~/Dropbox/org-roam/")
(setq org-agenda-files
        (find-lisp-find-files kevin/org-agenda-directory "\.org$"))
(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir "~/Dropbox/org-roam"
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))


(setq org-capture-templates `(
	("p" "Protocol" entry (file+headline "~/Dropbox/org-roam/inbox.org" "Inbox")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")


;;("p" "ref" entry (function org-roam-capture--get-point)
;;           "%?"
;;           :file-name "websites/${url}"
;;           :head "#+TITLE: ${title}
;;#+ROAM_KEY: ${ref}
;;- source :: ${ref}
;;#+BEGIN_QUOTE
;;${body}
;;#+END_QUOTE"
;;           :unnarrowed t)

                              ("L" "Protocol Link" entry (file+headline "~/Dropbox/org-roam/inbox.org" "Inbox")
        "* %? [[%:link][\"%:description\"]]\n")
))
;;
;; (require 'org-gcal)
;; (setq org-gcal-client-id "660340819767-g2c81h7pj3jene7uc3a64bc7m9mblh2q.apps.googleusercontent.com"
;;      org-gcal-client-secret "qPFh0xxkRER91ewMTfS2V6ns"
;;      org-gcal-file-alist '(("kev.cunningham@gmail.com" .  "~/schedule.org")
;;                            ("kev.cunningham@gmail.com" .  "~/task.org")))
;; (require 'org-download)
;; Drag-and-drop toxxss `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


  (setq org-roam-graph-viewer "/usr/bin/open")



;; Transclude lines from one file to another
(defun org-dblock-write:transclusion (params)
  (progn
    (with-temp-buffer
      (insert-file-contents (plist-get params :filename))
      (let ((range-start (or (plist-get params :min) (line-number-at-pos (point-min))))
            (range-end (or (plist-get params :max) (line-number-at-pos (point-max)))))
        (copy-region-as-kill (line-beginning-position range-start)
                             (line-end-position range-end))))
    (yank)))

;; Prettier
(add-hook!
  js2-mode 'prettier-js-mode
  (add-hook 'before-save-hook #'refmt-before-save nil t))

(setq rmh-elfeed-org-files "~/.elfeed/elfeed.org")
