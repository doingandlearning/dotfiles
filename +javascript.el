;;; +javascript.el -*- lexical-binding: t; -*-

(defun setup-tide-mode ()
  (interactive)
  (setq typescript-indent-level 2)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (auto-complete-mode 1)
  (set-local-company-backends-for-typescript)
  (company-mode 1)
  (hs-minor-mode 1))
(setq company-tooltip-align-annotations t)
(defun set-local-company-backends-for-typescript ()
  (interactive)
  (setq-local company-backends
              '((company-tide :separate company-dabbrev company-keywords))))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; custom keybindings
(add-hook 'typescript-mode-hook '(lambda ()
                                   (local-set-key (kbd "C-<tab>") 'yas-expand)
                                   (local-set-key (kbd "C-c C-r") 'tide-rename-symbol)
                                   (local-set-key (kbd "C-c C-o") 'hs-toggle-hiding)))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook 'setup-tide-mode)
(add-hook 'typescript-mode-hook 'zdx/use-prettier-if-in-node-modules)

;; typescript - needed for some of the js stuff
(use-package! tide
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package! prettier-js
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

(use-package! rjsx-mode
  :mode "\\.js\\'")
