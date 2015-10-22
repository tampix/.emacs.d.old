(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(line-number-mode t)
(column-number-mode t)
(display-battery-mode t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(setq-default major-mode 'text-mode)

(set-fontset-font "fontset-default" nil
		  (font-spec :size 7 :name "FontAwesome"))

(setq gc-cons-threshold 50000000 ;; Fix perf issues at startup
      jit-lock-stealth-time 3
      echo-keystrokes 0.1
      require-final-newline t
      redisplay-dont-pause t
      ring-bell-function #'ignore
      require-final-newline t
      scroll-step 1
      scroll-conservatively 10000
      inhibit-default-init t
      inhibit-startup-screen t
      confirm-nonexistent-file-or-buffer nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(toggle-truncate-lines t)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(put 'narrow-to-region 'disabled nil)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region,
indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(require 'use-package)

(use-package esup
  :commands esup)

(use-package cl)

(use-package server
  :if (display-graphic-p)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package sublime-themes
  :config
  (load-theme 'granger :no-confirm))

(use-package hl-line
  :init (global-hl-line-mode t)
  :config
  (set-face-background hl-line-face "gray5")
  (set-face-foreground hl-line-face nil))

(use-package command-log-mode
  :commands (global-command-log-mode
	     clm/open-command-log-buffer))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (set-face-attribute 'mode-line nil
		      :foreground "gray40"
		      :background "black"
		      :box '(:color "black" :line-width 6))
  (set-face-attribute 'mode-line-inactive nil
		      :foreground "gray15"
		      :background "black"
		      :box '(:color "black" :line-width 6)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :commands volatile-highlights-mode
  :init (add-hook 'after-init-hook 'volatile-highlights-mode t))

(use-package highlight-numbers
  :diminish highlight-numbers-mode
  :commands highlight-numbers-mode
  :init (add-hook 'after-init-hook 'highlight-numbers-mode t))

(use-package highlight-quoted
  :diminish highlight-quoted-mode
  :commands highlight-quoted-mode
  :init (add-hook 'after-init-hook 'highlight-quoted-mode t))

(use-package yascroll
  :commands global-yascroll-bar-mode
  :init (add-hook 'after-init-hook 'global-yascroll-bar-mode t))

(use-package org
  :commands (org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-log-done 'time
	org-startup-folded nil))

(use-package paren
  :init (show-paren-mode t))

(use-package cc-mode
  :defer t
  :config
  (add-hook 'c-mode-hook
	    (lambda ()
	      (c-set-style "bsd")))
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'java-mode-hook
	    (lambda ()
	      (setq c-basic-offset 4
		    tab-width 4
		    indent-tabs-mode t))))

(use-package nxml-mode
  :config
  (add-hook 'nxml-mode-hook
	    (lambda () (setq nxml-child-indent 8
			     nxml-attribute-indent 8))))

(use-package web-mode
  :mode ("\\.handlebars$" . web-mode))

(use-package abbrev
  :diminish abbrev-mode)

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :commands global-git-gutter+-mode
  :defer t
  :config
  (git-gutter-fr+-minimal))

(use-package recentf
  :commands (recentf-mode)
  :config
  (setq recentf-max-saved-items 100))

(use-package projectile
  :diminish (projectile-mode)
  :commands (projectile-global-mode
	     projectile-find-file
	     projectile-switch-project
	     projectile-switch-to-buffer)
  :config
  (projectile-global-mode t)

  (global-git-gutter+-mode t)
  (recentf-mode t)
  (helm-projectile-on)
  (setq projectile-enable-caching t
	projectile-sort-order 'recentf
	projectile-completion-system 'helm
	projectile-switch-project-action 'projectile-vc))

(use-package magit
  :commands (magit-status-internal
	     magit-status
	     magit-diff
	     magit-log)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-push-always-verify nil)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-completing-read-function 'helm--completing-read-default)

  (advice-add 'magit-visit-item :after 'reposition-window))

(use-package git-timemachine
  :commands git-timemachine
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package help-mode
  :init
  (defun my-help-button-action-advice (orig-fun &rest args)
    "Reuse current window when following links."
    (cl-letf (((symbol-function 'pop-to-buffer)
	       'switch-to-buffer))
      (apply orig-fun args)))
  (advice-add 'help-button-action :around 'my-help-button-action-advice))

(use-package emacs-lisp-mode
  :init
  (use-package eldoc
    :commands eldoc-mode
    :diminish eldoc-mode
    :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  (use-package rainbow-delimiters
    :commands rainbow-delimiters-mode-enable
    :init
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable))
  (use-package highlight-parentheses
    :commands highlight-parentheses-mode
    :init
    (defun turn-on-highlight-parentheses-mode ()
      (highlight-parentheses-mode t))
    (add-hook 'emacs-lisp-mode-hook 'turn-on-highlight-parentheses-mode))

  (defun my-setup-imenu-for-use-package ()
    "Recognize `use-package' in imenu"
    (when (string= buffer-file-name (expand-file-name "init.el" "~/.emacs.d"))
      (add-to-list
       'imenu-generic-expression
       '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2))))
  (add-hook 'emacs-lisp-mode-hook 'my-setup-imenu-for-use-package)
  :mode ("Cask" . emacs-lisp-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :commands yas-global-mode
  :init
  (setq yas-verbosity 1
	yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))

  (add-hook 'after-init-hook 'yas-global-mode t))

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode t)
  :config
  (setq company-transformers '(company-sort-by-occurrence)
	company-require-match t)
  ;; key mapping
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<enter>") 'company-complete))

(use-package rainbow-mode
  :commands rainbow-turn-on
  :init
  (add-hook 'css-mode-hook 'rainbow-turn-on)
  (add-hook 'html-mode-hook 'rainbow-turn-on)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-turn-on))

(use-package eshell
  :commands eshell
  :init
  (defun eshell-mode-hook-func ()
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))

  (add-hook 'eshell-mode-hook 'eshell-mode-hook-func))

(use-package sauron
  :commands sauron-start-hidden
  :config
  (setq sauron-modules '(sauron-erc
			 sauron-notifications))

  (defun my-notify (msg sender timestamp)
    "Notify using libnotify."
    (sauron-fx-notify (format "%s [%s]" sender timestamp) msg 6000))

  (use-package terminal-notifier
    :if (eq system-type 'darwin)
    :init
    (defun my-notify (msg sender timestamp)
      "Notify using terminal-notifier."
      (tn-notify msg (format "%s [%s]" sender timestamp))))

  (defun my-sauron-event-handler (origin prio msg &optional props)
    "Notify incomming priv msg."
    (if (eq origin 'erc)
	(let*
	    ((sender (plist-get props :sender))
	     (timestamp (format-time-string "%X")))
	  (my-notify msg sender timestamp))))

  (add-hook 'sauron-event-added-functions 'my-sauron-event-handler))

(use-package erc
  :commands bitlbee
  :config
  (use-package erc-hl-nicks
    :init (erc-hl-nicks-enable))

  (setq erc-query-display 'buffer
	erc-auto-query 'frame)

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
    (erc :server "localhost" :port 6667 :nick "tampix")
    (sauron-start-hidden))

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

  (setq erc-flood-protect nil))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-mode
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :config
  (setq ace-jump-mode-scope 'window))

(use-package winner
  :init (winner-mode t))

(use-package helm
  :commands helm-mode
  :init
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)

  (setq helm-candidate-number-limit 100
	helm-idle-delay 0.0
	helm-input-idle-delay 0.01
	helm-quick-update t
	helm-M-x-requires-pattern nil
	helm-ff-skip-boring-files t
	helm-bookmark-show-location t
	helm-buffers-fuzzy-matching t)

  (add-hook 'after-init-hook 'helm-mode t)
  :config
  (use-package helm-config)
  (use-package helm-misc)
  (use-package helm-projectile)
  (use-package helm-mode)
  (use-package helm-match-plugin)
  (use-package helm-buffers)
  (use-package helm-files)
  (use-package helm-locate)
  (use-package helm-bookmark)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)
	   ("C-h w" . helm-descbinds))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-C-i-jump t
	evil-want-C-w-in-emacs-state t
	evil-search-module 'evil-search
	evil-default-cursor t)

  (add-hook 'after-init-hook 'evil-mode t)

  (defsubst add-evil-mode-hook (function)
    "Add FUNCTION to `evil-mode-hook'"
    (add-hook 'evil-mode-hook function))

  (use-package evil-visualstar
    :commands global-evil-visualstar-mode
    :init (add-evil-mode-hook 'global-evil-visualstar-mode))
  (use-package evil-jumper
    :commands evil-jumper-mode
    :init (add-evil-mode-hook 'evil-jumper-mode))
  (use-package evil-surround
    :commands global-evil-surround-mode
    :init
    (add-evil-mode-hook 'global-evil-surround-mode)
    :config
    (setq-default surround-pairs-alist
		  '((?\( . ("(" . ")"))
		    (?\[ . ("[" . "]"))
		    (?\{ . ("{" . "}"))
		    (?\) . ("( " . " )"))
		    (?\] . ("[ " . " ]"))
		    (?\} . ("{ " . " }"))
		    (?# . ("#{" . "}"))
		    (?b . ("(" . ")"))
		    (?B . ("{" . "}"))
		    (?> . ("<" . ">"))
		    (?t . evil-surround-read-tag)
		    (?< . evil-surround-read-tag)
		    (?f . evil-surround-function))))
  :config
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    ":" 'evil-ex
    "K" 'magit-discard-item
    "l" 'magit-log-popup
    "h" 'magit-diff-toggle-refine-hunk)

  (defsubst evil-map (mode key cmd)
    (define-key mode key cmd))
  (defsubst evil-nmap (key cmd)
    (evil-map evil-normal-state-map key cmd))
  (defsubst evil-mmap (key cmd)
    (evil-map evil-motion-state-map key cmd))
  (defsubst evil-vmap (key cmd)
    (evil-map evil-visual-state-map key cmd))
  (defsubst evil-imap (key cmd)
    (evil-map evil-insert-state-map key cmd))
  (defsubst evil-cmap (key cmd)
    (evil-map evil-ex-map key cmd))

  ;; move RET and SPC from motion state to normal state
  (mapcar
   (lambda (k)
     (evil-nmap k (lookup-key evil-motion-state-map k))
     (evil-mmap k nil))
   (list (kbd "RET") " "))
  ;; company-yasnippet binding
  (evil-imap (kbd "S-<tab>") 'company-yasnippet)
  ;; normal-mode shortcuts
  (evil-nmap (kbd "+") 'evil-numbers/inc-at-pt)
  (evil-nmap (kbd "-") 'evil-numbers/dec-at-pt)
  (evil-nmap (kbd "\M-n") 'company-select-next)
  (evil-nmap (kbd "\M-p") 'company-select-previous)
  ;; restore previous window layout
  (evil-nmap (kbd "C-w u") 'winner-undo)
  ;; vnew
  (evil-nmap (kbd "C-w N") 'evil-window-vnew)
  ;; ex-mode shortcuts
  (evil-cmap "e " 'helm-find-files)
  (evil-cmap "b " 'helm-buffers-list)
  (evil-cmap "pf " 'helm-projectile-find-file)
  (evil-cmap "ps " 'helm-projectile-switch-project)
  (evil-cmap "pa " 'helm-projectile-ag)
  (evil-cmap "pb " 'helm-projectile-switch-to-buffer)
  (evil-cmap "po " 'helm-projectile-find-other-file)
  (evil-cmap "ee " 'my-erc-bitlbee-query)
  (evil-cmap "eb " 'my-erc-switch-buffer)
  ;; Vim-ism ;]
  (evil-nmap "Y" (kbd "y$"))
  ;; ESC exit from anywhere
  (evil-nmap [escape] 'keyboard-quit)
  (evil-vmap [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)
  (add-hook 'helm-mode-hook
	    (lambda () (define-key helm-map [escape] 'helm-keyboard-quit)))

  ;; Evil ace-jump motions
  (evil-define-motion evil-ace-jump-word-no-prefix-mode (count)
    "ace-jump-word-mode without having to input the head char."
    (let ((ace-jump-word-mode-use-query-char nil))
      (evil-ace-jump-word-mode count)))
  (evil-nmap (kbd "SPC") 'evil-ace-jump-char-mode)
  (evil-nmap (kbd "S-SPC") 'evil-ace-jump-word-mode)
  (evil-nmap (kbd "C-SPC") 'evil-ace-jump-word-no-prefix-mode)

  ;; Evil narrow-indirect motions
  (evil-define-operator evil-narrow-indirect (beg end type)
    "Indirectly narrow the region from BEG to END."
    (interactive "<R>")
    (evil-normal-state)
    (narrow-to-region-indirect beg end))
  (evil-nmap (kbd ", n") 'evil-narrow-indirect)
  (evil-vmap (kbd ", n") 'evil-narrow-indirect))
