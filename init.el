(require 'server)
(unless (server-running-p)
  (server-start))

(mapc
  (lambda (mode)
    (when (fboundp mode)
      (funcall mode -1)))
  '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq inhibit-startup-screen t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'use-package)

(use-package ir-black-theme
  :config (load-theme 'ir-black :no-confirm))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (progn
    (set-face-attribute 'mode-line nil
			:box nil)
    (set-face-attribute 'mode-line-inactive nil
			:box nil)))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda ()
      (c-set-style "bsd"))))
    (add-hook 'java-mode-hook (lambda ()
      (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t))))

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
    (setq ido-everywhere t)
    (setq ido-create-new-buffer t)
    (use-package ido-other-window
      :config
      (progn 
	(defun my-ido-split-keys ()
	  "Add my keybindings to enable window splitting with ido."
	  (mapcar (lambda (map)
		    (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
		    (define-key map (kbd "C-h") 'ido-invoke-in-vertical-split)
		    (define-key map (kbd "C-v") 'ido-invoke-in-horizontal-split)
		    (define-key map (kbd "C-t") 'ido-invoke-in-new-frame))
		  (list ido-buffer-completion-map
			ido-common-completion-map
			ido-file-completion-map
			ido-file-dir-completion-map)))
	(add-hook 'ido-setup-hook 'my-ido-split-keys)))
    (use-package ido-vertical-mode
      :init (ido-vertical-mode t))
    (use-package flx-ido
      :init (flx-ido-mode t)
      :config (setq ido-use-face nil))))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :mode ("Cask" . emacs-lisp-mode))

(use-package ag)

(use-package evil
  :defer t ;; needed for C-u, C-i, etc ...
  :init
  (progn
    (setq evil-default-cursor t)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump t)
    (setq evil-want-C-w-in-emacs-state t)
    (evil-mode t))
  :config
  (progn
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    ;; Vim-ism ;]
    (define-key evil-normal-state-map "Y" (kbd "y$"))
    ;; ESC exit from anywhere
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)))
