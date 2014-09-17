(require 'server)
(unless (server-running-p)
  (server-start))

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq inhibit-startup-screen t)

(toggle-truncate-lines t)

(setq redisplay-dont-pause t
      scroll-step 1
      scroll-conservatively 10000)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(put 'narrow-to-region 'disabled nil)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(require 'use-package)

(use-package sublime-themes
  :config
  (load-theme 'granger :no-confirm))

(use-package smart-mode-line
  :pre-load (setq sml/no-confirm-load-theme t)
  :init (sml/setup)
  :config
  (progn
    (set-face-attribute 'mode-line nil
			:foreground "gray40"
			:background "black"
			:box '(:color "black" :line-width 6))
    (set-face-attribute 'mode-line-inactive nil
			:foreground "gray15"
			:background "black"
			:box '(:color "black" :line-width 6))))

(use-package org
  :config
  (progn
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done 'time
	  org-startup-folded nil)))

(use-package paren
  :init (show-paren-mode t))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook
	      (lambda ()
		(c-set-style "bsd")))
    (add-hook 'c++-mode-hook 'google-set-c-style)
    (add-hook 'java-mode-hook
	      (lambda ()
		(setq c-basic-offset 4
		      tab-width 4
		      indent-tabs-mode t)))))

(use-package nxml-mode
  :config
  (progn
    (fset 'html-mode 'nxml-mode)
    (setq nxml-slash-auto-complete-flag t)

    (defadvice nxml-cleanup (around nxml-cleanup-no-widen activate)
      "Avoid the use of widen in nxml-cleanup which defeats the purpose of indirect
buffers."
      (flet ((widen () (ignore)))
	ad-do-it))

    (defun my-nxml-indent-hook ()
      "Indent with spaces instead of tabs"
      (setq indent-tabs-mode nil))
    (add-hook 'nxml-mode-hook 'my-nxml-indent-hook))
  :mode ("\\.tml$" . nxml-mode))

(use-package abbrev
  :diminish abbrev-mode)

(use-package projectile
  :diminish projectile-mode
  :init (projectile-global-mode t)
  :config
  (progn
    ;; Tapestry settings
    (add-to-list 'projectile-other-file-alist (list ".java" ".tml" "Impl.java"))
    (add-to-list 'projectile-other-file-alist (list ".tml" ".java"))

    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ido)
    (setq projectile-switch-project-action 'projectile-vc)))

(use-package magit
  :diminish magit-auto-revert-mode
  :init
  (progn
    (use-package magit-blame))
  :config
  (progn
    (setq magit-status-buffer-switch-function 'switch-to-buffer))
  :commands magit-status)

(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq ido-everywhere t)
    (setq ido-create-new-buffer 'always)
    (setq ido-default-buffer-method 'selected-window)
    (use-package ido-other-window
      :config
      (progn
	(defun my-ido-split-keys ()
	  "Add my keybindings to enable window splitting with ido."
	  (mapcar
	   (lambda (map)
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
      :config (setq ido-use-face nil))
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode t))
    (use-package ido-hacks
      :init (ido-hacks-mode t))
    (use-package smex
      :init (smex-initialize)
      :bind ("M-x" . smex))))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :diminish eldoc-mode
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package rainbow-delimiters
      :init
      (progn
	(defun turn-on-rainbow-delimiters-mode ()
	  (rainbow-delimiters-mode t))
	(add-hook 'emacs-lisp-mode-hook 'turn-on-rainbow-delimiters-mode)))
    (use-package highlight-parentheses
      :init
      (progn
	(defun turn-on-highlight-parentheses-mode ()
	  (highlight-parentheses-mode t))
	(add-hook 'emacs-lisp-mode-hook 'turn-on-highlight-parentheses-mode))))
  :mode ("Cask" . emacs-lisp-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package ag
  :config
  (progn
    (setq ag-reuse-buffers t)
    (setq ag-reuse-window t)))

(use-package company
  :diminish company-mode
  :init (global-company-mode t)
  :config
  (progn
    (setq company-transformers '(company-sort-by-occurrence))
    (setq company-require-match t)
    ;; key mapping
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "<tab>") 'company-complete)))

(use-package rainbow-mode
  :init
  (progn
    (add-hook 'css-mode-hook 'rainbow-turn-on)
    (add-hook 'html-mode-hook 'rainbow-turn-on)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-turn-on)))

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :init (global-git-gutter+-mode t)
  :config
  (progn
    (git-gutter-fr+-minimal)))

(use-package erc
  :config
  (progn
    (use-package erc-hl-nicks
      :init (erc-hl-nicks-enable))
    (defun bitlbee-identify ()
      "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
      (when (and (string= "localhost" erc-session-server)
		 (string= "&bitlbee" (buffer-name)))
	(erc-message "PRIVMSG" (format "%s identify %s"
				       (erc-default-target)
				       bitlbee-password)))
      (clear-string bitlbee-password))
    (add-hook 'erc-join-hook 'bitlbee-identify)

    (defun bitlbee ()
      "Connect to IM networks using bitlbee."
      (interactive)
      (setq bitlbee-password (read-passwd "Password: "))
      (erc :server "localhost" :port 6667 :nick "tampix"))

    (defun my-erc-bitlbee-query (nick)
      "Query someone from the &bitlbee channel."
      (interactive (list (completing-read
			  "Nick: "
			  (with-current-buffer "&bitlbee" erc-channel-users))))
      (with-current-buffer "&bitlbee" (erc-cmd-QUERY nick)))

    (defun my-erc-switch-buffer ()
      "Switch to an ERC buffer using ido completion."
      (interactive)
      (switch-to-buffer
       (ido-completing-read
	"ERC Buffer:"
	(save-excursion
	  (delq nil (mapcar (lambda (buf)
			      (when (buffer-live-p buf)
				(with-current-buffer buf
				  (and (eq major-mode 'erc-mode)
				       (buffer-name buf)))))
			    (buffer-list)))))))
    ))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package evil
  :pre-load
  (setq evil-want-C-u-scroll t
	evil-want-C-i-jump t
	evil-want-C-w-in-emacs-state t
	evil-search-module 'evil-search)
  :init
  (progn
    (use-package evil-leader
      ; TODO
      )
    (use-package evil-visualstar))
  :config
  (progn
    (setq evil-default-cursor t)
    (evil-mode t)

    (evil-add-hjkl-bindings magit-status-mode-map 'emacs
      ":" 'evil-ex)
    ;; move ESC and SPC from motion state to normal state
    (mapcar
     (lambda (k)
       (define-key evil-normal-state-map k (lookup-key evil-motion-state-map k))
       (define-key evil-motion-state-map k nil))
     (list (kbd "RET") " "))
    ;; ex-mode shortcuts
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "pf " 'projectile-find-file)
    (define-key evil-ex-map "ps " 'projectile-switch-project)
    (define-key evil-ex-map "pa " 'projectile-ag)
    (define-key evil-ex-map "pb " 'projectile-switch-to-buffer)
    (define-key evil-ex-map "po " 'projectile-find-other-file)
    (define-key evil-ex-map "ee " 'my-erc-bitlbee-query)
    (define-key evil-ex-map "eb " 'my-erc-switch-buffer)
    ;; Vim-ism ;]
    (define-key evil-normal-state-map "Y" (kbd "y$"))
    ;; ESC exit from anywhere
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    (evil-define-operator evil-narrow-indirect (beg end type)
      "Indirectly narrow the region from BEG to END."
      (interactive "<R>")
      (evil-normal-state)
      (narrow-to-region-indirect beg end))
    (define-key evil-normal-state-map "," 'evil-narrow-indirect)
    (define-key evil-visual-state-map "," 'evil-narrow-indirect)))
