(deftheme miro "The Miro color theme")

(defvar miro-default-colors-alist
  '(("miro-bg+3"      . "#5e5e5e")  ; configuration-based light black hexadecimal
    ("miro-bg+2"      . "#3d3d3d")  ; configuration-based dark black hexadecimal
    ("miro-bg+1"      . "#272727")  ; cursor line/column hexadecimal
    ("miro-bg+05"     . "#121212")  ; default backgroung
    ("miro-bg"        . "#000000")  ; high-contrast background
    ("miro-bg-05"     . "#000000")  ; FIXME
    ("miro-bg-1"      . "#000000")  ; FIXME
    ("miro-bg-2"      . "#000000")  ; FIXME
    ("miro-fg+1"      . "#c0c0c0")  ; FIXME
    ("miro-fg"        . "#c0c0c0")  ; foreground color hexadecimal
    ("miro-fg-1"      . "#999999")  ; FIXME
    ("miro-red+1"     . "#899ca1")  ; FIXME
    ("miro-red"       . "#cf4f88")  ; light red hexadecimal     (color 9)
    ("miro-red-1"     . "#8a2f58")  ; dark red hexadecimal      (color 1)
    ("miro-red-2"     . "#8a2f58")  ; FIXME
    ("miro-red-3"     . "#8a2f58")  ; FIXME
    ("miro-red-4"     . "#8a2f58")  ; FIXME
    ("miro-yellow"    . "#bf85cc")  ; light yellow hexadecimal  (color 11)
    ("miro-yellow-1"  . "#914e89")  ; dark yellow hexadecimal   (color 3)
    ("miro-yellow-2"  . "#914e89")  ; FIXME
    ("miro-orange"    . "#7f62b3")  ; FIXME
    ("miro-green+4"   . "#53a6a6")  ; FIXME
    ("miro-green+3"   . "#53a6a6")  ; FIXME
    ("miro-green+2"   . "#53a6a6")  ; FIXME
    ("miro-green+1"   . "#53a6a6")  ; FIXME
    ("miro-green"     . "#53a6a6")  ; light green hexadecimal   (color 10)
    ("miro-green-1"   . "#287373")  ; dark green hexadecimal    (color 2)
    ("miro-cyan"      . "#47959e")  ; light cyan hexadecimal    (color 14)
    ("miro-blue+1"    . "#4779b3")  ; light blue hexadecimal    (color 12)
    ("miro-blue"      . "#395573")  ; dark blue hexadecimal     (color 4)
    ("miro-blue-1"    . "#2b7694")  ; dark cyan hexadecimal     (color 6)
    ("miro-blue-2"    . "#2b7694")  ; FIXME
    ("miro-blue-3"    . "#2b7694")  ; FIXME
    ("miro-blue-4"    . "#2b7694")  ; FIXME
    ("miro-blue-5"    . "#2b7694")  ; FIXME
    ("miro-magenta"   . "#7f62b3")  ; light magenta hexadecimal (color 13)
    ("miro-magenta-1" . "#5e468c")  ; dark magentahexadecimal   (color 5)
    ("miro-white"     . "#c0c0c0")  ; light white hexadecimal   (color 15)
    ("miro-white-1"   . "#899ca1")) ; dark white hexadecimal    (color 7)
  "List of Miro colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar miro-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar miro-colors-alist
  (append miro-default-colors-alist miro-override-colors-alist))

(defmacro miro-with-color-variables (&rest body)
  "`let' bind all colors defined in `miro-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   miro-colors-alist))
     ,@body))

;;; Theme Faces
(miro-with-color-variables
  (custom-theme-set-faces
   'miro
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,miro-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,miro-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,miro-fg :background ,miro-bg))))
   `(cursor ((t (:foreground ,miro-fg :background ,miro-fg+1))))
   `(escape-glyph ((t (:foreground ,miro-yellow :bold t))))
   `(fringe ((t (:foreground ,miro-fg :background ,miro-bg+1))))
   `(header-line ((t (:foreground ,miro-yellow
                                  :background ,miro-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,miro-bg-05))))
   `(success ((t (:foreground ,miro-green :weight bold))))
   `(warning ((t (:foreground ,miro-orange :weight bold))))
   `(tooltip ((t (:foreground ,miro-fg :background ,miro-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,miro-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,miro-green))))
   `(compilation-error-face ((t (:foreground ,miro-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,miro-fg))))
   `(compilation-info-face ((t (:foreground ,miro-blue))))
   `(compilation-info ((t (:foreground ,miro-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,miro-green))))
   `(compilation-line-face ((t (:foreground ,miro-yellow))))
   `(compilation-line-number ((t (:foreground ,miro-yellow))))
   `(compilation-message-face ((t (:foreground ,miro-blue))))
   `(compilation-warning-face ((t (:foreground ,miro-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,miro-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,miro-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,miro-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,miro-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,miro-fg))))
   `(grep-error-face ((t (:foreground ,miro-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,miro-blue))))
   `(grep-match-face ((t (:foreground ,miro-orange :weight bold))))
   `(match ((t (:background ,miro-bg-1 :foreground ,miro-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,miro-yellow-2 :weight bold :background ,miro-bg+2))))
   `(isearch-fail ((t (:foreground ,miro-fg :background ,miro-red-4))))
   `(lazy-highlight ((t (:foreground ,miro-yellow-2 :weight bold :background ,miro-bg-05))))

   `(menu ((t (:foreground ,miro-fg :background ,miro-bg))))
   `(minibuffer-prompt ((t (:foreground ,miro-yellow))))
   `(mode-line
     ((,class (:foreground ,miro-green+1
                           :background ,miro-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,miro-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,miro-green-1
                      :background ,miro-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,miro-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,miro-bg+2))))
   `(trailing-whitespace ((t (:background ,miro-red))))
   `(vertical-border ((t (:foreground ,miro-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,miro-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,miro-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,miro-green-1))))
   `(font-lock-constant-face ((t (:foreground ,miro-green+4))))
   `(font-lock-doc-face ((t (:foreground ,miro-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,miro-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,miro-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,miro-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,miro-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,miro-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,miro-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,miro-red))))
   `(font-lock-type-face ((t (:foreground ,miro-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,miro-orange))))
   `(font-lock-warning-face ((t (:foreground ,miro-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,miro-fg))))
   `(newsticker-default-face ((t (:foreground ,miro-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,miro-green+3))))
   `(newsticker-extra-face ((t (:foreground ,miro-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,miro-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,miro-green))))
   `(newsticker-new-item-face ((t (:foreground ,miro-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,miro-red))))
   `(newsticker-old-item-face ((t (:foreground ,miro-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,miro-fg))))
   `(newsticker-treeview-face ((t (:foreground ,miro-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,miro-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,miro-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,miro-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,miro-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,miro-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,miro-bg-1 :foreground ,miro-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,miro-fg-1 :background ,miro-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,miro-green+2 :background ,miro-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,miro-fg-1 :background ,miro-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,miro-green+1))))
   `(android-mode-error-face ((t (:foreground ,miro-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,miro-fg))))
   `(android-mode-verbose-face ((t (:foreground ,miro-green))))
   `(android-mode-warning-face ((t (:foreground ,miro-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,miro-cyan :weight bold))))
   `(anzu-match-1 ((t (:foreground ,miro-bg :background ,miro-green))))
   `(anzu-match-2 ((t (:foreground ,miro-bg :background ,miro-orange))))
   `(anzu-match-3 ((t (:foreground ,miro-bg :background ,miro-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,miro-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,miro-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,miro-yellow))))
   `(font-latex-italic-face ((t (:foreground ,miro-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,miro-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,miro-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,miro-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,miro-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,miro-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,miro-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,miro-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,miro-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,miro-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,miro-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,miro-bg :background ,miro-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,miro-bg :background ,miro-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,miro-bg :background ,miro-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,miro-bg :background ,miro-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,miro-bg :background ,miro-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,miro-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,miro-bg+3 :foreground ,miro-bg-2))))
   `(ac-selection-face ((t (:background ,miro-blue-4 :foreground ,miro-fg))))
   `(popup-tip-face ((t (:background ,miro-yellow-2 :foreground ,miro-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,miro-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,miro-bg-1))))
   `(popup-isearch-match ((t (:background ,miro-bg :foreground ,miro-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,miro-fg-1 :background ,miro-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,miro-green+3 :background ,miro-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,miro-yellow :background ,miro-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,miro-red+1 :background ,miro-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,miro-cyan :background ,miro-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,miro-fg :background ,miro-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,miro-orange :background ,miro-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,miro-orange :background ,miro-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,miro-fg :background ,miro-bg-1))))
   `(company-tooltip-mouse ((t (:background ,miro-bg-1))))
   `(company-tooltip-common ((t (:foreground ,miro-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,miro-green+2))))
   `(company-scrollbar-fg ((t (:background ,miro-bg-1))))
   `(company-scrollbar-bg ((t (:background ,miro-bg+2))))
   `(company-preview ((t (:background ,miro-green+2))))
   `(company-preview-common ((t (:foreground ,miro-green+2 :background ,miro-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,miro-yellow-1 :foreground ,miro-bg))))
   `(bm-fringe-face ((t (:background ,miro-yellow-1 :foreground ,miro-bg))))
   `(bm-fringe-persistent-face ((t (:background ,miro-green-1 :foreground ,miro-bg))))
   `(bm-persistent-face ((t (:background ,miro-green-1 :foreground ,miro-bg))))
;;;;; cider
   `(cider-result-overlay-face ((t (:foreground ,miro-fg-1 :background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,miro-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,miro-green+1))))
   `(cider-deprecated-face ((t (:background ,miro-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,miro-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,miro-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,miro-red-4))))
   `(cider-test-error-face ((t (:background ,miro-magenta))))
   `(cider-test-success-face ((t (:background ,miro-green-1))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,miro-cyan))))
   `(circe-my-message-face ((t (:foreground ,miro-fg))))
   `(circe-fool-face ((t (:foreground ,miro-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,miro-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,miro-fg))))
   `(circe-server-face ((t (:foreground ,miro-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,miro-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,miro-orange :background ,miro-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,miro-fg)))
   `(context-coloring-level-1-face ((t :foreground ,miro-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,miro-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,miro-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,miro-orange)))
   `(context-coloring-level-5-face ((t :foreground ,miro-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,miro-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,miro-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,miro-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,miro-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,miro-blue :foreground ,miro-bg))))
   `(ctbl:face-continue-bar ((t (:background ,miro-bg-05 :foreground ,miro-bg))))
   `(ctbl:face-row-select ((t (:background ,miro-cyan :foreground ,miro-bg))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,miro-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,miro-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,miro-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,miro-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,miro-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,miro-red))))
   `(diff-header ((,class (:background ,miro-bg+2))
                  (t (:background ,miro-fg :foreground ,miro-bg))))
   `(diff-file-header
     ((,class (:background ,miro-bg+2 :foreground ,miro-fg :bold t))
      (t (:background ,miro-fg :foreground ,miro-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,miro-blue :background ,miro-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,miro-red+1 :background ,miro-red-1))))
   `(diff-hl-insert ((,class (:foreground ,miro-green+1 :background ,miro-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,miro-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,miro-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,miro-orange))))
   `(diredp-date-time ((t (:foreground ,miro-magenta))))
   `(diredp-deletion ((t (:foreground ,miro-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,miro-red))))
   `(diredp-dir-heading ((t (:foreground ,miro-blue :background ,miro-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,miro-cyan))))
   `(diredp-exec-priv ((t (:foreground ,miro-red))))
   `(diredp-executable-tag ((t (:foreground ,miro-green+1))))
   `(diredp-file-name ((t (:foreground ,miro-blue))))
   `(diredp-file-suffix ((t (:foreground ,miro-green))))
   `(diredp-flag-mark ((t (:foreground ,miro-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,miro-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,miro-red))))
   `(diredp-link-priv ((t (:foreground ,miro-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,miro-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,miro-orange))))
   `(diredp-no-priv ((t (:foreground ,miro-fg))))
   `(diredp-number ((t (:foreground ,miro-green+1))))
   `(diredp-other-priv ((t (:foreground ,miro-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,miro-red-1))))
   `(diredp-read-priv ((t (:foreground ,miro-green-1))))
   `(diredp-symlink ((t (:foreground ,miro-yellow))))
   `(diredp-write-priv ((t (:foreground ,miro-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,miro-red :weight bold))))
   `(dired-async-message ((t (:foreground ,miro-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,miro-yellow))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,miro-fg :background ,miro-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,miro-fg :background ,miro-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,miro-fg :background ,miro-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,miro-fg :background ,miro-blue-5))))
   `(ediff-even-diff-A ((t (:background ,miro-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,miro-bg+1))))
   `(ediff-even-diff-B ((t (:background ,miro-bg+1))))
   `(ediff-even-diff-C ((t (:background ,miro-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,miro-fg :background ,miro-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,miro-fg :background ,miro-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,miro-fg :background ,miro-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,miro-fg :background ,miro-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,miro-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,miro-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,miro-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,miro-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,miro-fg))))
   `(egg-help-header-1 ((t (:foreground ,miro-yellow))))
   `(egg-help-header-2 ((t (:foreground ,miro-green+3))))
   `(egg-branch ((t (:foreground ,miro-yellow))))
   `(egg-branch-mono ((t (:foreground ,miro-yellow))))
   `(egg-term ((t (:foreground ,miro-yellow))))
   `(egg-diff-add ((t (:foreground ,miro-green+4))))
   `(egg-diff-del ((t (:foreground ,miro-red+1))))
   `(egg-diff-file-header ((t (:foreground ,miro-yellow-2))))
   `(egg-section-title ((t (:foreground ,miro-yellow))))
   `(egg-stash-mono ((t (:foreground ,miro-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,miro-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,miro-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,miro-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,miro-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,miro-green))))
   `(elfeed-search-feed-face ((t (:foreground ,miro-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,miro-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,miro-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,miro-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,miro-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,miro-green+2 :background ,miro-bg))))
   `(w3m-lnum-match ((t (:background ,miro-bg-1
                                     :foreground ,miro-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,miro-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,miro-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,miro-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,miro-yellow))))
   `(erc-keyword-face ((t (:foreground ,miro-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,miro-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,miro-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,miro-green))))
   `(erc-pal-face ((t (:foreground ,miro-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,miro-orange :background ,miro-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,miro-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,miro-green+4 :background ,miro-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,miro-red :background ,miro-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,miro-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,miro-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,miro-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,miro-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,miro-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,miro-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,miro-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,miro-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-red-1) :inherit unspecified))
      (t (:foreground ,miro-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-yellow) :inherit unspecified))
      (t (:foreground ,miro-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-cyan) :inherit unspecified))
      (t (:foreground ,miro-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,miro-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,miro-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,miro-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,miro-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,miro-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,miro-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-orange) :inherit unspecified))
      (t (:foreground ,miro-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-red) :inherit unspecified))
      (t (:foreground ,miro-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,miro-fg))))
   `(ack-file ((t (:foreground ,miro-blue))))
   `(ack-line ((t (:foreground ,miro-yellow))))
   `(ack-match ((t (:foreground ,miro-orange :background ,miro-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,miro-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,miro-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,miro-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,miro-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,miro-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,miro-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,miro-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,miro-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,miro-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,miro-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, miro-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,miro-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,miro-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,miro-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,miro-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,miro-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,miro-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,miro-blue))))
   `(gnus-summary-high-read ((t (:foreground ,miro-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,miro-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,miro-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,miro-blue))))
   `(gnus-summary-low-read ((t (:foreground ,miro-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,miro-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,miro-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,miro-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,miro-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,miro-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,miro-fg))))
   `(gnus-summary-selected ((t (:foreground ,miro-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,miro-blue))))
   `(gnus-cite-10 ((t (:foreground ,miro-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,miro-yellow))))
   `(gnus-cite-2 ((t (:foreground ,miro-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,miro-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,miro-green+2))))
   `(gnus-cite-5 ((t (:foreground ,miro-green+1))))
   `(gnus-cite-6 ((t (:foreground ,miro-green))))
   `(gnus-cite-7 ((t (:foreground ,miro-red))))
   `(gnus-cite-8 ((t (:foreground ,miro-red-1))))
   `(gnus-cite-9 ((t (:foreground ,miro-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,miro-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,miro-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,miro-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,miro-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,miro-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,miro-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,miro-bg+2))))
   `(gnus-signature ((t (:foreground ,miro-yellow))))
   `(gnus-x ((t (:background ,miro-fg :foreground ,miro-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,miro-blue))))
   `(guide-key/key-face ((t (:foreground ,miro-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,miro-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,miro-green
                      :background ,miro-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,miro-yellow
                      :background ,miro-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,miro-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,miro-bg+1))))
   `(helm-visible-mark ((t (:foreground ,miro-bg :background ,miro-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,miro-green+4 :background ,miro-bg-1))))
   `(helm-separator ((t (:foreground ,miro-red :background ,miro-bg))))
   `(helm-time-zone-current ((t (:foreground ,miro-green+2 :background ,miro-bg))))
   `(helm-time-zone-home ((t (:foreground ,miro-red :background ,miro-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,miro-orange :background ,miro-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,miro-magenta :background ,miro-bg))))
   `(helm-bookmark-info ((t (:foreground ,miro-green+2 :background ,miro-bg))))
   `(helm-bookmark-man ((t (:foreground ,miro-yellow :background ,miro-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,miro-magenta :background ,miro-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,miro-red :background ,miro-bg))))
   `(helm-buffer-process ((t (:foreground ,miro-cyan :background ,miro-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,miro-fg :background ,miro-bg))))
   `(helm-buffer-size ((t (:foreground ,miro-fg-1 :background ,miro-bg))))
   `(helm-ff-directory ((t (:foreground ,miro-cyan :background ,miro-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,miro-fg :background ,miro-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,miro-green+2 :background ,miro-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,miro-red :background ,miro-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,miro-yellow :background ,miro-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,miro-bg :background ,miro-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,miro-cyan :background ,miro-bg))))
   `(helm-grep-file ((t (:foreground ,miro-fg :background ,miro-bg))))
   `(helm-grep-finish ((t (:foreground ,miro-green+2 :background ,miro-bg))))
   `(helm-grep-lineno ((t (:foreground ,miro-fg-1 :background ,miro-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,miro-red :background ,miro-bg))))
   `(helm-match ((t (:foreground ,miro-orange :background ,miro-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,miro-cyan :background ,miro-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,miro-fg-1 :background ,miro-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,miro-fg :background ,miro-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,miro-fg :background ,miro-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,miro-yellow :background ,miro-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,miro-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,miro-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,miro-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,miro-red-1 :background ,miro-bg))))
   `(hydra-face-amaranth ((t (:foreground ,miro-red-3 :background ,miro-bg))))
   `(hydra-face-blue ((t (:foreground ,miro-blue :background ,miro-bg))))
   `(hydra-face-pink ((t (:foreground ,miro-magenta :background ,miro-bg))))
   `(hydra-face-teal ((t (:foreground ,miro-cyan :background ,miro-bg))))
;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,miro-green :background ,miro-bg))))
   `(ivy-match-required-face ((t (:foreground ,miro-red :background ,miro-bg))))
   `(ivy-remote ((t (:foreground ,miro-blue :background ,miro-bg))))
   `(ivy-subdir ((t (:foreground ,miro-yellow :background ,miro-bg))))
   `(ivy-current-match ((t (:foreground ,miro-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,miro-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,miro-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,miro-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,miro-green+1))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,miro-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,miro-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,miro-yellow))))
   `(ido-indicator ((t (:foreground ,miro-yellow :background ,miro-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,miro-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,miro-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,miro-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,miro-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,miro-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,miro-orange))))
   `(jabber-roster-user-error ((t (:foreground ,miro-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,miro-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,miro-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,miro-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,miro-green+3))))
   `(jabber-activity-face((t (:foreground ,miro-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,miro-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,miro-orange))))
   `(js2-error ((t (:foreground ,miro-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,miro-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,miro-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,miro-green+3))))
   `(js2-function-param ((t (:foreground, miro-orange))))
   `(js2-external-variable ((t (:foreground ,miro-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,miro-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,miro-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,miro-red-1))))
   `(js2-object-property ((t (:foreground ,miro-blue+1))))
   `(js2-magic-paren ((t (:foreground ,miro-blue-5))))
   `(js2-private-function-call ((t (:foreground ,miro-cyan))))
   `(js2-function-call ((t (:foreground ,miro-cyan))))
   `(js2-private-member ((t (:foreground ,miro-blue-1))))
   `(js2-keywords ((t (:foreground ,miro-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,miro-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,miro-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,miro-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,miro-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,miro-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,miro-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,miro-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,miro-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,miro-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,miro-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,miro-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,miro-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,miro-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,miro-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,miro-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,miro-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,miro-green+2 :background ,miro-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,miro-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,miro-bg :background ,miro-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,miro-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,miro-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,miro-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,miro-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,miro-green+2 :background ,miro-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,miro-blue-1))))
   `(lui-hilight-face ((t (:foreground ,miro-green+2 :background ,miro-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,miro-green+2 :background ,miro-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,miro-red+1 :background ,miro-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,miro-blue+1 :background ,miro-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,miro-magenta :background ,miro-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,miro-yellow :background ,miro-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,miro-bg+05))))
   `(magit-section-heading             ((t (:foreground ,miro-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,miro-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,miro-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,miro-bg+05
                                                        :foreground ,miro-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,miro-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,miro-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,miro-bg+2
                                                        :foreground ,miro-orange))))
   `(magit-diff-lines-heading          ((t (:background ,miro-orange
                                                        :foreground ,miro-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,miro-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,miro-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,miro-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,miro-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,miro-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,miro-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,miro-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,miro-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,miro-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,miro-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,miro-orange))))
   `(magit-log-date      ((t (:foreground ,miro-fg-1))))
   `(magit-log-graph     ((t (:foreground ,miro-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,miro-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,miro-green))))
   `(magit-sequence-part ((t (:foreground ,miro-yellow))))
   `(magit-sequence-head ((t (:foreground ,miro-blue))))
   `(magit-sequence-drop ((t (:foreground ,miro-red))))
   `(magit-sequence-done ((t (:foreground ,miro-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,miro-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,miro-green))))
   `(magit-bisect-skip ((t (:foreground ,miro-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,miro-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,miro-bg-1 :foreground ,miro-blue-2))))
   `(magit-blame-hash    ((t (:background ,miro-bg-1 :foreground ,miro-blue-2))))
   `(magit-blame-name    ((t (:background ,miro-bg-1 :foreground ,miro-orange))))
   `(magit-blame-date    ((t (:background ,miro-bg-1 :foreground ,miro-orange))))
   `(magit-blame-summary ((t (:background ,miro-bg-1 :foreground ,miro-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,miro-bg+3))))
   `(magit-hash           ((t (:foreground ,miro-bg+3))))
   `(magit-tag            ((t (:foreground ,miro-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,miro-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,miro-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,miro-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,miro-blue   :weight bold))))
   `(magit-refname        ((t (:background ,miro-bg+2 :foreground ,miro-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,miro-bg+2 :foreground ,miro-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,miro-bg+2 :foreground ,miro-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,miro-green))))
   `(magit-signature-bad       ((t (:foreground ,miro-red))))
   `(magit-signature-untrusted ((t (:foreground ,miro-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,miro-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,miro-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,miro-green))))
   `(magit-reflog-amend        ((t (:foreground ,miro-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,miro-green))))
   `(magit-reflog-checkout     ((t (:foreground ,miro-blue))))
   `(magit-reflog-reset        ((t (:foreground ,miro-red))))
   `(magit-reflog-rebase       ((t (:foreground ,miro-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,miro-green))))
   `(magit-reflog-remote       ((t (:foreground ,miro-cyan))))
   `(magit-reflog-other        ((t (:foreground ,miro-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,miro-green+1))))
   `(message-header-other ((t (:foreground ,miro-green))))
   `(message-header-to ((t (:foreground ,miro-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,miro-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,miro-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,miro-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,miro-green))))
   `(message-mml ((t (:foreground ,miro-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,miro-orange))))
   `(mew-face-header-from ((t (:foreground ,miro-yellow))))
   `(mew-face-header-date ((t (:foreground ,miro-green))))
   `(mew-face-header-to ((t (:foreground ,miro-red))))
   `(mew-face-header-key ((t (:foreground ,miro-green))))
   `(mew-face-header-private ((t (:foreground ,miro-green))))
   `(mew-face-header-important ((t (:foreground ,miro-blue))))
   `(mew-face-header-marginal ((t (:foreground ,miro-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,miro-red))))
   `(mew-face-header-xmew ((t (:foreground ,miro-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,miro-red))))
   `(mew-face-body-url ((t (:foreground ,miro-orange))))
   `(mew-face-body-comment ((t (:foreground ,miro-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,miro-green))))
   `(mew-face-body-cite2 ((t (:foreground ,miro-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,miro-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,miro-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,miro-red))))
   `(mew-face-mark-review ((t (:foreground ,miro-blue))))
   `(mew-face-mark-escape ((t (:foreground ,miro-green))))
   `(mew-face-mark-delete ((t (:foreground ,miro-red))))
   `(mew-face-mark-unlink ((t (:foreground ,miro-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,miro-green))))
   `(mew-face-mark-unread ((t (:foreground ,miro-red-2))))
   `(mew-face-eof-message ((t (:foreground ,miro-green))))
   `(mew-face-eof-part ((t (:foreground ,miro-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,miro-cyan :background ,miro-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,miro-bg :background ,miro-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,miro-bg :background ,miro-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,miro-blue))))
   `(mingus-pausing-face ((t (:foreground ,miro-magenta))))
   `(mingus-playing-face ((t (:foreground ,miro-cyan))))
   `(mingus-playlist-face ((t (:foreground ,miro-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,miro-yellow))))
   `(mingus-stopped-face ((t (:foreground ,miro-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,miro-yellow))))
   `(nav-face-button-num ((t (:foreground ,miro-cyan))))
   `(nav-face-dir ((t (:foreground ,miro-green))))
   `(nav-face-hdir ((t (:foreground ,miro-red))))
   `(nav-face-file ((t (:foreground ,miro-fg))))
   `(nav-face-hfile ((t (:foreground ,miro-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,miro-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,miro-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,miro-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,miro-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,miro-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,miro-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,miro-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,miro-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,miro-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,miro-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,miro-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,miro-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,miro-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,miro-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,miro-fg :weight bold))))
   `(org-checkbox ((t (:background ,miro-bg+2 :foreground ,miro-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,miro-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,miro-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,miro-green+3))))
   `(org-formula ((t (:foreground ,miro-yellow-2))))
   `(org-headline-done ((t (:foreground ,miro-green+3))))
   `(org-hide ((t (:foreground ,miro-bg-1))))
   `(org-level-1 ((t (:foreground ,miro-orange))))
   `(org-level-2 ((t (:foreground ,miro-green+4))))
   `(org-level-3 ((t (:foreground ,miro-blue-1))))
   `(org-level-4 ((t (:foreground ,miro-yellow-2))))
   `(org-level-5 ((t (:foreground ,miro-cyan))))
   `(org-level-6 ((t (:foreground ,miro-green+2))))
   `(org-level-7 ((t (:foreground ,miro-red-4))))
   `(org-level-8 ((t (:foreground ,miro-blue-4))))
   `(org-link ((t (:foreground ,miro-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,miro-green+4))))
   `(org-scheduled-previously ((t (:foreground ,miro-red))))
   `(org-scheduled-today ((t (:foreground ,miro-blue+1))))
   `(org-sexp-date ((t (:foreground ,miro-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,miro-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,miro-orange))))
   `(org-todo ((t (:bold t :foreground ,miro-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,miro-red :weight bold :underline nil))))
   `(org-column ((t (:background ,miro-bg-1))))
   `(org-column-title ((t (:background ,miro-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,miro-fg :background ,miro-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,miro-bg :background ,miro-red-1))))
   `(org-ellipsis ((t (:foreground ,miro-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,miro-cyan :underline t))))
   `(org-document-title ((t (:foreground ,miro-blue))))
   `(org-document-info ((t (:foreground ,miro-blue))))
   `(org-habit-ready-face ((t :background ,miro-green)))
   `(org-habit-alert-face ((t :background ,miro-yellow-1 :foreground ,miro-bg)))
   `(org-habit-clear-face ((t :background ,miro-blue-3)))
   `(org-habit-overdue-face ((t :background ,miro-red-3)))
   `(org-habit-clear-future-face ((t :background ,miro-blue-4)))
   `(org-habit-ready-future-face ((t :background ,miro-green-1)))
   `(org-habit-alert-future-face ((t :background ,miro-yellow-2 :foreground ,miro-bg)))
   `(org-habit-overdue-future-face ((t :background ,miro-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,miro-orange))))
   `(outline-2 ((t (:foreground ,miro-green+4))))
   `(outline-3 ((t (:foreground ,miro-blue-1))))
   `(outline-4 ((t (:foreground ,miro-yellow-2))))
   `(outline-5 ((t (:foreground ,miro-cyan))))
   `(outline-6 ((t (:foreground ,miro-green+2))))
   `(outline-7 ((t (:foreground ,miro-red-4))))
   `(outline-8 ((t (:foreground ,miro-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,miro-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,miro-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,miro-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,miro-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,miro-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,miro-fg :background ,miro-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,miro-bg :background ,miro-orange))))
   `(proof-error-face ((t (:foreground ,miro-fg :background ,miro-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,miro-bg :background ,miro-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,miro-bg :background ,miro-orange))))
   `(proof-locked-face ((t (:background ,miro-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,miro-bg :background ,miro-orange))))
   `(proof-queue-face ((t (:background ,miro-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,miro-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,miro-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,miro-bg))))
   `(proof-warning-face ((t (:foreground ,miro-bg :background ,miro-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,miro-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,miro-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,miro-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,miro-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,miro-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,miro-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,miro-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,miro-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,miro-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,miro-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,miro-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,miro-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,miro-blue))))
   `(rcirc-other-nick ((t (:foreground ,miro-orange))))
   `(rcirc-bright-nick ((t (:foreground ,miro-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,miro-blue-2))))
   `(rcirc-server ((t (:foreground ,miro-green))))
   `(rcirc-server-prefix ((t (:foreground ,miro-green+1))))
   `(rcirc-timestamp ((t (:foreground ,miro-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,miro-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,miro-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,miro-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,miro-green))))
   `(rpm-spec-doc-face ((t (:foreground ,miro-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,miro-red))))
   `(rpm-spec-macro-face ((t (:foreground ,miro-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,miro-red))))
   `(rpm-spec-package-face ((t (:foreground ,miro-red))))
   `(rpm-spec-section-face ((t (:foreground ,miro-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,miro-blue))))
   `(rpm-spec-var-face ((t (:foreground ,miro-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,miro-orange))))
   `(rst-level-2-face ((t (:foreground ,miro-green+1))))
   `(rst-level-3-face ((t (:foreground ,miro-blue-1))))
   `(rst-level-4-face ((t (:foreground ,miro-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,miro-cyan))))
   `(rst-level-6-face ((t (:foreground ,miro-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,miro-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,miro-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,miro-red+1 :background ,miro-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,miro-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Miro for sml
   `(sml/global ((,class (:foreground ,miro-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,miro-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,miro-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,miro-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,miro-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,miro-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,miro-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,miro-orange))))
   `(sml/git ((,class (:foreground ,miro-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,miro-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,miro-red-2))))
   `(sml/outside-modified ((,class (:foreground ,miro-orange))))
   `(sml/modified ((,class (:foreground ,miro-red))))
   `(sml/vc-edited ((,class (:foreground ,miro-green+2))))
   `(sml/charging ((,class (:foreground ,miro-green+4))))
   `(sml/discharging ((,class (:foreground ,miro-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,miro-red+1 :background ,miro-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,miro-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,miro-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,miro-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-red)))
      (t
       (:underline ,miro-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-orange)))
      (t
       (:underline ,miro-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-yellow)))
      (t
       (:underline ,miro-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,miro-green)))
      (t
       (:underline ,miro-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,miro-green+2))))
   `(speedbar-directory-face ((t (:foreground ,miro-cyan))))
   `(speedbar-file-face ((t (:foreground ,miro-fg))))
   `(speedbar-highlight-face ((t (:foreground ,miro-bg :background ,miro-green+2))))
   `(speedbar-selected-face ((t (:foreground ,miro-red))))
   `(speedbar-separator-face ((t (:foreground ,miro-bg :background ,miro-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,miro-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,miro-fg
                                    :background ,miro-bg))))
   `(tabbar-selected ((t (:foreground ,miro-fg
                                      :background ,miro-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,miro-fg
                                        :background ,miro-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,miro-bg
                                       :background ,miro-bg-1))))
   `(term-color-red ((t (:foreground ,miro-red-2
                                     :background ,miro-red-4))))
   `(term-color-green ((t (:foreground ,miro-green
                                       :background ,miro-green+2))))
   `(term-color-yellow ((t (:foreground ,miro-orange
                                        :background ,miro-yellow))))
   `(term-color-blue ((t (:foreground ,miro-blue-1
                                      :background ,miro-blue-4))))
   `(term-color-magenta ((t (:foreground ,miro-magenta
                                         :background ,miro-red))))
   `(term-color-cyan ((t (:foreground ,miro-cyan
                                      :background ,miro-blue))))
   `(term-color-white ((t (:foreground ,miro-fg
                                       :background ,miro-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,miro-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,miro-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,miro-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,miro-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,miro-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,miro-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,miro-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,miro-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,miro-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,miro-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,miro-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,miro-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,miro-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,miro-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,miro-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,miro-bg+1 :foreground ,miro-bg+1))))
   `(whitespace-hspace ((t (:background ,miro-bg+1 :foreground ,miro-bg+1))))
   `(whitespace-tab ((t (:background ,miro-red-1))))
   `(whitespace-newline ((t (:foreground ,miro-bg+1))))
   `(whitespace-trailing ((t (:background ,miro-red))))
   `(whitespace-line ((t (:background ,miro-bg :foreground ,miro-magenta))))
   `(whitespace-space-before-tab ((t (:background ,miro-orange :foreground ,miro-orange))))
   `(whitespace-indentation ((t (:background ,miro-yellow :foreground ,miro-red))))
   `(whitespace-empty ((t (:background ,miro-yellow))))
   `(whitespace-space-after-tab ((t (:background ,miro-yellow :foreground ,miro-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,miro-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,miro-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,miro-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,miro-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,miro-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,miro-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,miro-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,miro-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,miro-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,miro-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,miro-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,miro-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,miro-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,miro-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,miro-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,miro-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,miro-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,miro-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,miro-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,miro-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,miro-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,miro-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,miro-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,miro-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,miro-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,miro-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,miro-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,miro-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,miro-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,miro-bg :background ,miro-blue+1))))
   `(cscope-separator-face ((t (:foreground ,miro-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,miro-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,miro-bg-1 :foreground ,miro-bg-1))))
   ))

;;; Theme Variables
(miro-with-color-variables
  (custom-theme-set-variables
   'miro
;;;;; ansi-color
   `(ansi-color-names-vector [,miro-bg ,miro-red ,miro-green ,miro-yellow
                                          ,miro-blue ,miro-magenta ,miro-cyan ,miro-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,miro-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,miro-red ,miro-orange ,miro-yellow ,miro-green ,miro-green+4
                    ,miro-cyan ,miro-blue+1 ,miro-magenta))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,miro-red-1)
       ( 40. . ,miro-red)
       ( 60. . ,miro-orange)
       ( 80. . ,miro-yellow-2)
       (100. . ,miro-yellow-1)
       (120. . ,miro-yellow)
       (140. . ,miro-green-1)
       (160. . ,miro-green)
       (180. . ,miro-green+1)
       (200. . ,miro-green+2)
       (220. . ,miro-green+3)
       (240. . ,miro-green+4)
       (260. . ,miro-cyan)
       (280. . ,miro-blue-2)
       (300. . ,miro-blue-1)
       (320. . ,miro-blue)
       (340. . ,miro-blue+1)
       (360. . ,miro-magenta)))
   `(vc-annotate-very-old-color ,miro-magenta)
   `(vc-annotate-background ,miro-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar miro-add-font-lock-keywords nil
  "Whether to add font-lock keywords for miro color names.
In buffers visiting library `miro-theme.el' the miro
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar miro-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after miro activate)
;;   "Maybe also add font-lock keywords for miro colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or miro-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "miro-theme.el")))
;;     (unless miro-colors-font-lock-keywords
;;       (setq miro-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car miro-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc miro-colors-alist))))))
;;     (font-lock-add-keywords nil miro-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after miro activate)
;;   "Also remove font-lock keywords for miro colors."
;;   (font-lock-remove-keywords nil miro-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'miro)
