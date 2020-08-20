;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
(package! prettier-js)
;(package! vue-mode)
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))
;(package! elfeed)
(package! org-roam-server :recipe (:host github :repo "org-roam/org-roam-server" :files ("*")))
(package! org-noter)
(package! org-pdftools)
(package! org-noter-pdftools)
(package! org-ref)
(package! helm-bibtex)
(package! org-roam-bibtex)
(package! org-pomodoro)
(package! command-log-mode)
(package! org-download)
(package! org-fancy-priorities)
(package! org-journal)
(package! spray)
(package! keycast)
(package! info-colors)
(package! org-msg)
(package! mu4e-alert)
(unpin! org)
(package! org-super-agenda)
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "88380f865a...")
(package! ox-gfm)
(package! org-chef)
(unpin! org-roam)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! xref-js2)
(package! company-tern)
(package! tide)
(package! rjsx-mode)
(package! prettier-js)
(package! dired-narrow)
(package! deadgrep)
(package! easy-kill)
(package! org-clock-convenience)
(package! company-posframe)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! modus-operandi-theme)
(package! outshine)
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! emmet-mode)
(package! write-good
  :recipe (:host github :repo "bnbeckwith/writegood-mode"))
