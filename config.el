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
      calendar-latitude 50.819519
      calendar-longitude -0.136420
      calendar-location-name "Brighton"
      doom-theme 'modus-operandi)

(setq org-directory "~/org/"
      org-ellipsis " ▼ "
      org-deadline-warning-days 7
      org-agenda-breadcrumbs-separator " ❱ "
      org-adapt-indentation nil
      search-highlight t
      search-whitespace-regexp ".*?"
      display-line-numbers-type t
      org-export-with-toc nil
      evil-want-fine-undo t ;; More granular than the single insert blob
      doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq-default tab-width 2)
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

(load! "+javascript")
(load! "+searching")
(load! "+clojure")
(load! "+modeline")
(load! "+org-mode")
(load! "+secrets")

(remove-hook!
  'doom-first-buffer-hook #'smartparens-global-mode
  '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(electric-pair-mode +1)

;; ;;
;; ;; Helm
;; ;;
;; (when (featurep! :completion helm)
;;   (map! "M-i"   #'helm-imenu)
;;   (map! :map helm-map
;;         "<tab>"   #'helm-execute-persistent-action))
;; (setq helm-autoresize-max-height 0
;;          helm-autoresize-min-height 40
;;       helm-M-x-fuzzy-match t
;;       helm-buffers-fuzzy-matching t
;;       helm-display-header-line t
;;       helm-split-window-in-side-p nil
;;       helm-move-to-line-cycle-in-source nil
;;       helm-ff-search-library-in-sexp t
;;       helm-scroll-amount 8
;;       helm-echo-input-in-header-line nil)






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



(after! mixed-pitch
  (dolist (f (-filter (lambda (sym)
                        (s-prefix? "company-" (symbol-name sym)))
                      (face-list)))
    (pushnew! mixed-pitch-fixed-pitch-faces f)))




;; Adds the projectile serach path
(setq projectile-project-search-path '("~/code" "~/code/egghead" "~/code/spinup" "~/code/personal"))






;; elcord
(setq elcord-use-major-mode-as-main-icon t)

;; (setq eros-eval-result-prefix "⟹ ")



(setq yas-triggers-in-field t)

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

;; Goto files
(map! :leader
      (:prefix "o" (
      :desc "Open GTD"      "g" (lambda () (interactive) (find-file "~/Dropbox/gtd/gtd.org"))
      :desc "Open inbox"    "i" (lambda () (interactive) (find-file "~/Dropbox/gtd/inbox.org"))

      )))
(use-package! emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)))

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
