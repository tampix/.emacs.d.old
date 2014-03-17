(require 'server)
(unless (server-running-p)
  (server-start))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'use-package)

(require 'powerline)
(powerline-default-theme)

(load-theme 'grandshell :no-confirm)

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))))

(use-package projectile
  :init (projectile-global-mode t)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ido)))

(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq ido-everywhere t)))

(use-package flx-ido
  :init (flx-ido-mode t)
  :config (setq ido-use-face nil))

(use-package emacs-lisp-mode
  :mode ("Cask" . emacs-lisp-mode))
