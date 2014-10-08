(defun my-show-startup-time ()
  "Show Emacs's startup time in the minibuffer"
  (message "Startup time: %s seconds."
           (emacs-uptime "%s")))
(add-hook 'emacs-startup-hook 'my-show-startup-time 'append)

(require 'server)
(unless (server-running-p)
  (server-start))

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(line-number-mode t)
(column-number-mode t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq-default major-mode 'text-mode)

(setq redisplay-dont-pause t
      scroll-step 1
      scroll-conservatively 10000
      inhibit-startup-screen t
      confirm-nonexistent-file-or-buffer nil
      custom-file "~/.emacs.d/custom.el")
(load custom-file)

(toggle-truncate-lines t)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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

(use-package benchmark-init
  :init (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package sublime-themes
  :config
  (load-theme 'granger :no-confirm))

(use-package smart-mode-line
  :pre-load (setq sml/no-confirm-load-theme t)
  :init (sml/setup)
  :config
  (set-face-attribute 'mode-line nil
		      :foreground "gray40"
		      :background "black"
		      :box '(:color "black" :line-width 6))
  (set-face-attribute 'mode-line-inactive nil
		      :foreground "gray15"
		      :background "black"
		      :box '(:color "black" :line-width 6)))

(use-package highlight-numbers
  :init (highlight-numbers-mode t))

(use-package highlight-quoted
  :init (highlight-quoted-mode t))

(use-package org
  :commands org-mode
  :config
  (define-key global-map "\C-ca" 'org-agenda)
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
  :defer t
  :config
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
  (add-hook 'nxml-mode-hook 'my-nxml-indent-hook)
  :mode ("\\.tml$" . nxml-mode))

(use-package abbrev
  :diminish abbrev-mode)

(use-package projectile
  :diminish (projectile-mode)
  :commands (projectile-global-mode
	     projectile-find-file
	     projectile-switch-project
	     projectile-switch-to-buffer)
  :init (projectile-global-mode t)
  :config
  (use-package git-gutter-fringe+
    :diminish git-gutter+-mode
    :commands global-git-gutter+-mode
    :idle (global-git-gutter+-mode t)
    :config
    (git-gutter-fr+-minimal))
  ;; Tapestry settings
  (add-to-list 'projectile-other-file-alist (list ".java" ".tml" "Impl.java"))
  (add-to-list 'projectile-other-file-alist (list ".tml" ".java"))

  (setq projectile-enable-caching t
	projectile-completion-system 'ido
	projectile-switch-project-action 'projectile-vc))

(use-package magit
  :diminish magit-auto-revert-mode
  :commands (magit-status
	     magit-diff
	     magit-log)
  :init
  (use-package magit-blame
    :commands  magit-blame-mode)
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer))

(use-package ido
  :commands (ido-find-file
	     ido-switch-buffer)
  :init
  (ido-mode t)
  (setq ido-everywhere t
	ido-create-new-buffer 'always
	ido-default-buffer-method 'selected-window)
  (use-package ido-other-window
    :config
    (defun my-ido-split-keys ()
      "Add my keybindings to enable window splitting with ido."
      (mapcar
       (lambda (map)
	 (define-key map (kbd "<C-return>") 'ido-exit-minibuffer)
	 (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
	 (define-key map (kbd "C-h") 'ido-invoke-in-vertical-split)
	 (define-key map (kbd "C-v") 'ido-invoke-in-horizontal-split)
	 (define-key map (kbd "C-t") 'ido-invoke-in-new-frame))
       (list ido-buffer-completion-map
	     ido-common-completion-map
	     ido-file-completion-map
	     ido-file-dir-completion-map)))
    (add-hook 'ido-setup-hook 'my-ido-split-keys))
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
    :bind ("M-x" . smex)))

(use-package emacs-lisp-mode
  :init
  (use-package eldoc
    :commands eldoc-mode
    :diminish eldoc-mode
    :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
  (use-package rainbow-delimiters
    :commands rainbow-delimiters-mode
    :init
    (defun turn-on-rainbow-delimiters-mode ()
      (rainbow-delimiters-mode t))
    (add-hook 'emacs-lisp-mode-hook 'turn-on-rainbow-delimiters-mode))
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

(use-package ag
  :commands (ag
	     ag-files
	     ag-regexp
	     ag-project
	     ag-project-files
	     ag-project-regexp)
  :config
  (setq ag-reuse-buffers t
	ag-reuse-window t))

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :idle (global-company-mode t)
  :config
  (setq company-transformers '(company-sort-by-occurrence)
	company-require-match t)
  ;; key mapping
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "<tab>") 'company-complete))

(use-package rainbow-mode
  :commands rainbow-turn-on
  :init
  (add-hook 'css-mode-hook 'rainbow-turn-on)
  (add-hook 'html-mode-hook 'rainbow-turn-on)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-turn-on))

(use-package sauron
  :commands sauron-start-hidden
  :config
  (setq sauron-modules '(sauron-erc
			 sauron-notifications))

  (defun my-sauron-event-handler (origin prio msg &optional props)
    "Use libnotify for incomming priv msg."
    (if (eq origin 'erc)
	(let*
	    ((sender (plist-get props :sender))
	     (timestamp (format-time-string "%X")))
	  (sauron-fx-notify (format "%s [%s]" sender timestamp) msg 6000))))

  (add-hook 'sauron-event-added-functions 'my-sauron-event-handler))

(use-package erc
  :commands bitlbee
  :config
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

(use-package smartparens
  :diminish smartparens-mode
  :commands smartparens-global-mode
  :idle (smartparens-global-mode t)
  :config
  (add-hook 'erc-mode-hook 'turn-off-smartparens-mode)
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  (sp-local-pair '(emacs-lisp-mode org-mode git-commit-mode) "`" "'")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(use-package evil
  :pre-load
  (setq evil-want-C-u-scroll t
	evil-want-C-i-jump t
	evil-want-C-w-in-emacs-state t
	evil-search-module 'evil-search
	evil-default-cursor t)
  :config
  (evil-mode t)

  (defun my-client-frame-list ()
    "List frames for current emacsclient."
    (--filter (eq (frame-parameter it 'client)
		  (frame-parameter (selected-frame) 'client))
	      (frame-list)))
  (evil-define-command evil-quit (&optional bang)
    "Closes the current window, current frame, Emacs.
If the current frame is the only one that belongs to some client the
client connection is closed."
    :repeat nil
    (interactive "<!>")
    (condition-case nil
	(delete-window)
      (error
       (condition-case nil
	   (let ((proc (frame-parameter (selected-frame) 'client)))
	     (if (and proc (= 1 (length (my-client-frame-list))))
		 (evil-quit-all bang)
	       (delete-frame)))
	 (error
	  (evil-quit-all bang))))))

  (use-package evil-visualstar)
  (use-package evil-jumper)
  (use-package evil-surround
    :commands global-evil-surround-mode
    :idle (global-evil-surround-mode t)
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

  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    ":" 'evil-ex
    "K" 'magit-discard-item
    "l" 'magit-key-mode-popup-logging
    "h" 'magit-diff-toggle-refine-hunk)
  ;; move RET and SPC from motion state to normal state
  (mapcar
   (lambda (k)
     (define-key evil-normal-state-map k (lookup-key evil-motion-state-map k))
     (define-key evil-motion-state-map k nil))
   (list (kbd "RET") " "))
  ;; normal-mode shortcuts
  (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
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

  ;; Evil ace-jump motions
  (evil-define-motion evil-ace-jump-word-no-prefix-mode (count)
    "ace-jump-word-mode without having to imput the head char."
    (let ((ace-jump-word-mode-use-query-char nil))
      (evil-ace-jump-word-mode count)))
  (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "C-SPC") 'evil-ace-jump-word-no-prefix-mode)

  ;; Evil smartparens motions
  (evil-define-motion evil-sp-forward-sexp (count)
    (let*
	((evil-move-cursor-back nil))
      (sp-forward-sexp count)))
  (evil-define-motion evil-sp-backward-sexp (count)
    (let*
	((evil-move-cursor-back nil))
      (sp-backward-sexp count)))
  (evil-define-motion evil-sp-up-sexp (count)
    (let*
	((evil-move-cursor-back nil))
      (sp-up-sexp count)))
  (evil-define-motion evil-sp-backward-up-sexp (count)
    (let*
	((evil-move-cursor-back nil))
      (sp-backward-up-sexp count)))
  (evil-define-motion evil-sp-down-sexp (count)
    (let*
	((evil-move-cursor-back nil))
      (sp-down-sexp count)))
  (define-key evil-normal-state-map (kbd "[ x") 'evil-sp-backward-sexp)
  (define-key evil-normal-state-map (kbd "] x") 'evil-sp-forward-sexp)
  (define-key evil-normal-state-map (kbd "[ X") 'evil-sp-up-sexp)
  (define-key evil-normal-state-map (kbd "] X") 'evil-sp-down-sexp)

  ;; Evil smartparens text objects
  (evil-define-text-object evil-a-sexp (count &optional beg end type)
    (evil-an-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))
  (evil-define-text-object evil-inner-sexp (count &optional beg end type)
    (evil-inner-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))
  (define-key evil-outer-text-objects-map "x" 'evil-a-sexp)
  (define-key evil-inner-text-objects-map "x" 'evil-a-sexp)

  ;; Evil narrow-indirect motions
  (evil-define-operator evil-narrow-indirect (beg end type)
    "Indirectly narrow the region from BEG to END."
    (interactive "<R>")
    (evil-normal-state)
    (narrow-to-region-indirect beg end))
  (define-key evil-normal-state-map (kbd ", n") 'evil-narrow-indirect)
  (define-key evil-visual-state-map (kbd ", n") 'evil-narrow-indirect))
