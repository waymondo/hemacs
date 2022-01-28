;; -*- lexical-binding: t -*-

(load (concat user-emacs-directory "lib.el"))

;;;;; Personal Variables & Key Maps

(defconst indent-sensitive-modes '(coffee-mode slim-mode yaml-mode))
(defconst writing-modes '(org-mode markdown-mode fountain-mode git-commit-mode))
(defconst default-font-size 15)
(define-prefix-command 'hemacs-git-map)
(define-prefix-command 'hemacs-help-map)
(bind-key "s-g" #'hemacs-git-map)
(bind-key "s-h" #'hemacs-help-map)

;;;;; Bootstrap

(use-feature emacs
  :custom
  (history-length 128)
  (history-delete-duplicates t)
  (maximum-scroll-margin 0.5)
  (scroll-margin 50)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position 'always)
  (auto-window-vscroll nil)
  (echo-keystrokes 1e-6)
  (delete-by-moving-to-trash t)
  (ring-bell-function #'ignore)
  (ns-function-modifier 'control)
  (create-lockfiles nil)
  (disabled-command-function nil)
  (ad-redefinition-action 'accept)
  (custom-safe-themes t)
  (custom-file (make-temp-file "emacs-custom"))
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message "")
  (standard-indent 2)
  (enable-recursive-minibuffers t)
  (kill-buffer-query-functions nil)
  (ns-pop-up-frames nil)
  (frame-inhibit-implied-resize t)
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (frame-resize-pixelwise t)
  (read-process-output-max (* 1024 1024))
  (use-short-answers t)
  (x-underline-at-descent-line t)
  (tab-always-indent 'complete)
  :config
  (setq-default indent-tabs-mode nil
                line-spacing 2
                tab-width 2
                c-basic-offset 2
                cursor-type 'bar
                cursor-in-non-selected-windows nil
                bidi-display-reordering 'left-to-right
                fill-column 100
                truncate-lines t)
  (advice-add 'keyboard-quit :around #'keyboard-quit-minibuffer-first)
  (add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
  (add-hook 'minibuffer-exit-hook #'restore-garbage-collection)
  (add-hook 'emacs-startup-hook #'restore-garbage-collection 100)
  (add-hook 'emacs-startup-hook #'restore-default-file-name-handler-alist)
  (add-hook 'window-setup-hook #'restore-redisplay-and-message))

(use-package dash
  :custom
  (dash-enable-fontlock t))

(use-package transform-string-at-point
  :straight
  (:host github :repo "waymondo/transform-string-at-point")
  :custom
  (transform-string-at-point-cursor-after-transform 'next-string)
  :bind
  ("s-;" . transform-string-at-point))

(use-feature tool-bar
  :config
  (tool-bar-mode -1))

(use-feature menu-bar
  :bind
  ("s-w" . kill-this-buffer))

;;;;; Processes, Shells, Compilation

(use-package alert
  :custom
  (alert-default-style 'osx-notifier)
  :init
  (defun alert-after-finish-in-background (buf str)
    (when (or (not (get-buffer-window buf 'visible)) (not (frame-focus-state)))
      (alert str :buffer buf))))

(use-feature comint
  :bind
  (:map comint-mode-map
        ("RET"       . comint-return-dwim)
        ("C-r"       . comint-history-isearch-backward-regexp)
        ("s-k"       . comint-clear-buffer)
        ("M-TAB"     . comint-previous-matching-input-from-input)
        ("<M-S-tab>" . comint-next-matching-input-from-input))
  :custom
  (comint-prompt-read-only t)
  :hook
  (comint-mode . text-smaller-no-truncation)
  :config
  (setq-default comint-input-ignoredups t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (def comint-return-dwim
    (cond
     ((comint-after-pmark-p)
      (comint-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t
      (comint-next-prompt 1))))
  (defun write-input-ring-for-shellish-modes ()
    (when (derived-mode-p 'comint-mode)
      (comint-write-input-ring)))
  (defun write-input-ring-for-all-shellish-modes ()
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (write-input-ring-for-shellish-modes))))
  (add-hook 'kill-buffer-hook #'write-input-ring-for-shellish-modes)
  (add-hook 'kill-emacs-hook #'write-input-ring-for-all-shellish-modes))

(use-feature compile
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :hook
  (compilation-mode . text-smaller-no-truncation)
  :init
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-feature profiler
  :bind
  ("C-x P r"  . profiler-report)
  ("C-x P 1"  . profiler-start)
  ("C-x P 0"  . profiler-stop))

(use-feature warnings
  :custom
  (warning-suppress-types '((comp) (undo discard-info))))

(use-package vterm
  :ensure-system-package cmake
  :hook
  (vterm-mode . text-smaller-no-truncation))

(use-package sh-script
  :mode
  ((rx (and (? ".") (or "bash" "zsh" "zprofile"))) . sh-mode)
  :custom
  (sh-basic-offset 2))

(use-feature executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

;;;;; Files & History

(use-feature image-mode
  :hook
  (image-mode . show-image-dimensions-in-mode-line)
  :custom
  (image-animate-loop t)
  :mode "\\.otf\\'"
  :config
  (after moody
    (defun show-image-dimensions-in-mode-line ()
      (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
             (width (car image-dimensions))
             (height (cdr image-dimensions)))
        (setq moody-mode-line-buffer-identification
              `(:eval (moody-tab (format "%s %dx%d" (propertized-buffer-identification "%b") ,width ,height)
                                 20 'down)))))))

(use-feature files
  :custom
  (require-final-newline t)
  (confirm-kill-processes nil)
  (confirm-kill-emacs nil)
  (enable-local-variables :safe)
  (confirm-nonexistent-file-or-buffer nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (large-file-warning-threshold 50000000)
  :chords
  (";f" . find-file)
  :config
  (defun hemacs-save-hook ()
    (unless (member major-mode '(markdown-mode gfm-mode sql-mode csv-mode))
      (delete-trailing-whitespace))
    (when (region-active-p)
      (deactivate-mark t)))
  (add-hook 'before-save-hook #'hemacs-save-hook)
  (defun find-file-maybe-make-directories ()
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'find-file-maybe-make-directories find-file-not-found-functions))

(use-feature savehist
  :custom
  (savehist-additional-variables
   '(search-ring regexp-search-ring comint-input-ring projector-command-history))
  :init
  (savehist-mode))

(use-feature saveplace
  :init
  (save-place-mode))

(use-feature recentf
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 200)
  :config
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer)
  (recentf-mode))

(use-feature dired
  :custom
  (dired-use-ls-dired nil))

(use-package all-the-icons
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts)))

(use-package dirvish
  :bind
  ("s-\\" . dirvish)
  :hook
  (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-header-style 'normal)
  (dirvish-body-fontsize-increment 0))

(use-package terminal-here
  :bind
  ("C-c o t" . terminal-here))

(use-package reveal-in-osx-finder
  :bind
  ("C-c o f" . reveal-in-osx-finder))

;;;;; Editing

(use-feature newcomment
  :bind
  ("s-." . insert-todo-comment)
  ("s-/" . comment-line)
  :config
  (def insert-todo-comment
    (call-interactively #'comment-dwim)
    (ensure-space :before)
    (insert "TODO:")
    (ensure-space :after)))

(use-feature face-remap
  :hook
  (writing-modes . variable-pitch-mode))

(use-feature simple
  :custom
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (yank-pop-change-selection t)
  (idle-update-delay 2)
  (next-error-recenter t)
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  (what-cursor-show-names t)
  :bind
  ("M-`" . list-processes)
  ("s-P" . execute-extended-command)
  ("s-z" . undo-only)
  ("s-Z" . undo-redo)
  ("<escape>" . keyboard-escape-quit)
  (:map minibuffer-local-map
        ("<escape>"  . abort-recursive-edit)
        ("M-TAB"     . previous-complete-history-element)
        ("<M-S-tab>" . next-complete-history-element))
  :hook
  (writing-modes . auto-fill-mode)
  :config
  (column-number-mode)
  (defun pop-to-mark-command-until-new-point (f &rest args)
    (let ((p (point)))
      (dotimes (_i 10)
        (when (= p (point))
          (apply f args)))))
  (defun maybe-indent-afterwards (&optional _)
    (and (not current-prefix-arg)
         (not (member major-mode indent-sensitive-modes))
         (or (-any? #'derived-mode-p '(prog-mode sgml-mode)))
         (indent-region (region-beginning) (region-end) nil)))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (advice-add 'pop-to-mark-command :around #'pop-to-mark-command-until-new-point)
  (advice-add 'yank :after #'maybe-indent-afterwards)
  (advice-add 'yank-pop :after #'maybe-indent-afterwards)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

(use-package puni
  :hook
  (after-init . puni-global-mode)
  (vterm-mode . puni-disable-puni-mode)
  :bind
  ("C-," . puni-expand-region))

(use-feature subword
  :hook
  (after-init . global-subword-mode))

(use-feature autoinsert
  :init
  (auto-insert-mode))

(use-feature delsel
  :init
  (delete-selection-mode))

(use-feature elec-pair
  :init
  (electric-pair-mode))

(use-feature electric
  :custom
  (electric-quote-string t)
  (electric-quote-context-sensitive t)
  :hook
  (writing-modes . electric-quote-local-mode))

(use-package avy
  :custom
  (avy-style 'de-bruijn)
  :bind
  (:map dired-mode-map ("." . avy-goto-word-or-subword-1))
  :chords
  ("jj" . avy-goto-char-timer)
  ("jk" . avy-goto-word-or-subword-1)
  ("jl" . avy-goto-line))

(use-package ace-link
  :bind
  ("M-g e" . avy-jump-error)
  :config
  (ace-link-setup-default)
  (defun avy-jump-error-next-error-hook ()
    (let ((compilation-buffer (compilation-find-buffer)))
      (quit-window nil (get-buffer-window compilation-buffer))
      (recenter)))
  (def avy-jump-error
    (let ((compilation-buffer (compilation-find-buffer))
          (next-error-hook '(avy-jump-error-next-error-hook)))
      (when compilation-buffer
        (with-current-buffer compilation-buffer
          (when (derived-mode-p 'compilation-mode)
            (pop-to-buffer compilation-buffer)
            (ace-link-compilation)))))))

(use-package smart-newline
  :bind
  ("<s-return>" . eol-then-smart-newline)
  :hook
  (prog-mode . maybe-enable-smart-newline-mode)
  :init
  (defun maybe-enable-smart-newline-mode ()
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode)))
  (def eol-then-smart-newline
    (move-end-of-line nil)
    (smart-newline)))

(use-package shift-number
  :bind
  ("<M-up>"   . shift-number-up)
  ("<M-down>" . shift-number-down))

(use-package multiple-cursors
  :bind
  ("s-d"     . mc/mark-next-like-this)
  ("s-D"     . mc/mark-previous-like-this)
  ("C-c s-d" . mc/mark-all-like-this-dwim)
  :hook
  (before-save . mc/keyboard-quit))

(use-package crux
  :bind
  ("s-," . crux-find-user-init-file)
  ("s-D" . crux-duplicate-current-line-or-region)
  ("s-W" . crux-delete-file-and-buffer)
  ("s-S" . crux-rename-file-and-buffer)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package cycle-quotes
  :bind
  ("C-'" . cycle-quotes))

(use-feature flyspell
  :hook
  (writing-modes . flyspell-mode))

(use-package flyspell-correct
  :after
  flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

(use-package smart-backspace
  :commands (smart-backspace)
  :init
  (bind-key "<backspace>" #'smart-backspace global-map
            (and (member (char-before) (string-to-list " \t"))
                 (not (or (region-active-p) (member major-mode indent-sensitive-modes))))))

(use-package smart-shift
  :bind
  ("s-[" . smart-shift-left)
  ("s-]" . smart-shift-right)
  :config
  (advice-add 'smart-shift-override-local-map :override #'ignore))

(use-package drag-stuff
  :bind
  ("C-s-k" . drag-stuff-down)
  ("s-TAB" . drag-stuff-up)
  :config
  (defun indent-unless-sensitive (_arg)
    (unless (member major-mode indent-sensitive-modes)
      (indent-according-to-mode)))
  (advice-add 'drag-stuff-line-vertically :after #'indent-unless-sensitive)
  (advice-add 'drag-stuff-lines-vertically :after #'indent-unless-sensitive))

;;;;; Completion

(use-feature cursor-sensor
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))

(use-package vertico
  :preface
  (load (concat straight-base-dir "straight/repos/vertico/extensions/vertico-quick.el"))
  :config
  (vertico-mode)
  (def vertico-quick-insert-and-return
    (vertico-quick-insert)
    (vertico-exit))
  :chords
  (:map vertico-map ("jj" . vertico-quick-insert-and-return)))

(use-package corfu
  :hook
  (after-init . corfu-global-mode))

(use-package corfu-doc
  :after corfu
  :straight
  (:host github :repo "galeo/corfu-doc")
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up))
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package kind-icon
  :after corfu
  :commands
  (kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package marginalia
  :bind
  ("C-?" . marginalia-cycle)
  :init
  (marginalia-mode))

(use-package consult
  :commands
  (consult--crm-selected)
  :bind
  ([remap goto-line] . consult-goto-line)
  ([remap yank-pop] . consult-yank-pop)
  ([remap isearch-forward] . consult-line)
  ("C-c C-t" . consult-theme)
  :chords
  (";s" . consult-buffer)
  (":G" . consult-ripgrep)
  (";r" . consult-imenu)
  (":R" . consult-imenu-multi)
  :custom
  (consult-project-root-function #'consult-project-current)
  (consult-preview-key (kbd "M-."))
  :init
  (defun consult-project-current ()
    (when-let (project (project-current))
      (car (project-roots project)))))

(use-package affe
  :custom
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :chords
  (";g" . affe-grep)
  :init
  (defun affe-orderless-regexp-compiler (input _type)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str)))))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C->" . embark-act)
  (:map hemacs-help-map ("b" . embark-bindings))
  :config
  (after which-key
    (defun embark-which-key-indicator ()
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))
    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))
    (defun embark-hide-which-key-indicator (fn &rest args)
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    (advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)))

(use-package embark-consult
  :demand t
  :after
  (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-feature hippie-exp
  :custom
  (hippie-expand-verbose nil)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-matching-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-dabbrev-other-buffers))
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  :bind*
  ("M-'" . hippie-expand-line)
  :hook
  ((emacs-lisp-mode ielm-mode) . hippie-expand-allow-lisp-symbols)
  :init
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((hippie-expand-only-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun try-expand-dabbrev-other-buffers (old)
    (let ((hippie-expand-ignore-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun hippie-expand-case-sensitive (f &rest args)
    (let ((case-fold-search nil))
      (apply f args)))
  (defun inhibit-message-in-minibuffer (f &rest args)
    (let ((inhibit-message (minibufferp)))
      (apply f args)))
  (defun hippie-expand-maybe-kill-to-eol (f &rest args)
    (unless (eolp)
      (kill-line))
    (apply f args))
  (defalias 'hippie-expand-line (make-hippie-expand-function
                                 '(try-expand-line
                                   try-expand-line-all-buffers)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'inhibit-message-in-minibuffer)
  (advice-add 'hippie-expand-line :around #'hippie-expand-maybe-kill-to-eol)
  (defun hippie-expand-allow-lisp-symbols ()
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

(use-package yasnippet
  :custom
  (yas-wrap-around-region t)
  :mode
  ("\\.yasnippet\\'" . snippet-mode)
  :config
  (defun yas-indent-unless-case-sensitive (f &rest args)
    (let ((yas-indent-line (if (member major-mode indent-sensitive-modes) nil 'auto)))
      (apply f args)))
  (delete 'yas-installed-snippets-dir yas-snippet-dirs)
  (advice-add 'yas--indent :around #'yas-indent-unless-case-sensitive)
  (yas-global-mode))

(use-package bash-completion
  :init
  (bash-completion-setup))

;;;;; Navigation & Search

(use-feature window
  :chords
  (";w" . toggle-split-window)
  (":W" . delete-other-windows)
  :custom
  (switch-to-buffer-obey-display-actions t)
  (split-width-threshold (- (window-width) 10))
  :config
  (defun do-not-split-more-than-two-non-side-windows (window &optional horizontal)
    (if (and horizontal (> (length (--filter (not (window-parameter it 'window-side)) (window-list))) 1))
        nil
      t))
  (advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-non-side-windows)
  (def toggle-split-window
    (if (eq last-command 'toggle-split-window)
        (progn
          (jump-to-register :toggle-split-window)
          (setq this-command 'toggle-unsplit-window))
      (window-configuration-to-register :toggle-split-window)
      (switch-to-buffer-other-window nil))))

(use-package rg
  :ensure-system-package rg
  :hook
  (after-init . rg-enable-default-bindings))

(use-package wgrep-ag
  :after rg
  :custom
  (wgrep-auto-save-buffer t)
  :hook
  (rg-mode . wgrep-ag-setup))

(use-package bm
  :bind
  ("s-1" . bm-toggle)
  ("s-2" . bm-next)
  ("s-@" . bm-previous)
  :custom
  (bm-cycle-all-buffers t))

(use-feature imenu
  :custom
  (imenu-auto-rescan t)
  :hook
  (emacs-lisp-mode . hemacs-imenu-elisp-expressions)
  :config
  (defun hemacs-imenu-elisp-expressions ()
    (dolist (pattern '((nil "^[[:space:]]*(def \\(.+\\)$" 1)
                       ("Features" "^(use-feature \\(.+\\)$" 1)
                       ("Sections" "^;;;;; \\(.+\\)$" 1)))
      (add-to-list 'imenu-generic-expression pattern))))

(use-feature project
  :demand t
  :bind-keymap
  ("s-p" . project-prefix-map)
  :bind
  ("C-x m" . project-shell)
  (:map project-prefix-map
        ("m" . magit-status)
        ("t" . project-vterm))
  :chords
  (";t" . project-find-file)
  :custom
  (project-switch-use-entire-map t)
  :init
  (defun project-vterm ()
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer default-project-vterm-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer display-comint-buffer-action)
        (vterm (generate-new-buffer-name default-project-vterm-name))))))

(use-package projector
  :bind
  ("C-x RET"        . projector-run-shell-command-project-root)
  (:map comint-mode-map ("s-R" . projector-rerun-buffer-process))
  (:map project-prefix-map ("RET" . projector-run-shell-command-project-root))
  :custom
  (projector-project-package 'project)
  (projector-command-modes-alist
   '(("^heroku console" . inf-ruby-mode))))

(use-package beginend
  :init
  (beginend-global-mode))

;;;;; External Utilities

(use-package emacs-everywhere)

(use-package restart-emacs
  :bind
  ([remap save-buffers-kill-terminal] . restart-emacs))

;;;;; Major Modes

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode))

(use-feature org
  :bind
  (:map org-mode-map
        ("," . self-with-space)
        ("C-c C-." . org-todo)
        ("C-c t" . insert-date))
  :custom
  (org-support-shift-select t)
  (org-startup-indented t)
  :config
  (def insert-date
    (insert (format-time-string "%m/%d/%Y")))
  (advice-add 'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window))

(use-package org-autolist
  :after org
  :hook
  (org-mode . org-autolist-mode))

(use-feature sgml-mode
  :ensure-system-package
  (html-languageserver . "npm i -g vscode-html-languageserver-bin")
  :bind
  (:map html-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim))
  :chords
  (:map html-mode-map
        ("<>" . sgml-close-tag))
  :hook
  (sgml-mode . sgml-electric-tag-pair-mode)
  :config
  (add-λ 'sgml-mode-hook
    (run-hooks 'prog-mode-hook))
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (def html-newline-dwim
    (move-end-of-line nil)
    (smart-newline)
    (sgml-close-tag)
    (move-beginning-of-line nil))
  (bind-key "'" "’" html-mode-map (eq 0 (car (syntax-ppss)))))

(use-package web-mode
  :mode
  ("\\.erb\\'"        . web-mode)
  ("\\.php\\'"        . web-mode)
  ("\\.hbs\\'"        . web-mode)
  ("\\.handlebars\\'" . web-mode)
  ("\\.tsx\\'"        . web-mode)
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim)
        ("C-c C-." . typescript-mode))
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t))

(use-package lorem-ipsum)

(use-package emmet-mode
  :hook
  (sgml-mode web-mode))

(use-package markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map ("," . self-with-space))
  :ensure-system-package (marked . "npm i -g marked")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "marked")
  (markdown-indent-on-enter nil))

(use-package pandoc-mode
  :after (markdown-mode org-mode)
  :ensure-system-package pandoc
  :hook
  (markdown-mode org-mode)
  (pandoc-mode . pandoc-load-default-settings))

(use-feature css-mode
  :ensure-system-package
  (css-languageserver . "npm i -g vscode-css-languageserver-bin")
  :mode "\\.css\\.erb\\'"
  :bind
  (:map css-mode-map
        ("," . self-with-space)
        ("{" . open-brackets-newline-and-indent))
  :custom
  (css-indent-offset 2))

(use-package less-css-mode
  :ensure-system-package (lessc . "npm i -g less")
  :custom
  (less-css-lessc-options '("--no-color" "-x")))

(use-feature js
  :ensure-system-package
  (eslint_d . "npm install -g eslint_d")
  :bind
  (:map js-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package nodejs-repl
  :ensure-system-package node)

(use-package json-mode
  :mode
  ("\\.bowerrc$"     . json-mode)
  ("\\.jshintrc$"    . json-mode)
  ("\\.json_schema$" . json-mode)
  :custom
  (js-indent-level 2))

(use-package graphql-mode)

(use-package coffee-mode
  :ensure-system-package
  (coffee . "npm i -g coffeescript")
  :mode "\\.coffee\\.*"
  :bind
  (:map coffee-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        ("C-c C-c"    . coffee-compile-region))
  :custom
  (coffee-args-repl '("-i" "--nodejs"))
  :config
  (defun coffee-indent ()
    (if (coffee-line-wants-indent)
        (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
      (coffee-insert-spaces (coffee-previous-indent))))
  (add-λ 'coffee-mode-hook
    (setq-local indent-line-function #'coffee-indent))
  (add-to-list 'coffee-args-compile "--no-header"))

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

(use-package typescript-mode
  :ensure-system-package
  (typescript-language-server . "npm i -g typescript-language-server")
  :bind
  (:map typescript-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space)
        ("C-c C-." . web-mode))
  :custom
  (typescript-indent-level 2))

(use-package ts-comint
  :ensure-system-package (tsun . "npm i -g tsun"))

(use-package ember-mode
  :ensure-system-package (ember . "npm i -g ember-cli"))

(use-package mmm-mode
  :custom
  (mmm-submode-decoration-level 0))

(use-package format-all
  :ensure-system-package
  (prettier . "npm i -g prettier")
  (rufo . "gem install rufo"))

(use-package slim-mode
  :bind
  (:map slim-mode-map
        (","          . self-with-space)
        (":"          . smart-ruby-colon)
        ("<C-return>" . slim-newline-dwim))
  :custom
  (slim-backspace-backdents-nesting nil)
  :config
  (def slim-newline-dwim
    (move-end-of-line nil)
    (newline-and-indent))
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))

(use-feature ruby-mode
  :mode ("Appraisals$" (rx (and (group (= 1 upper) (1+ lower)) (not (any "Proc"))) "file" eos))
  :bind
  (:map ruby-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        (":"          . smart-ruby-colon)
        ("<C-return>" . ruby-newline-dwim))
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :config
  (def smart-ruby-colon
    (if (and (looking-back "[[:word:]]" nil)
             (not (memq (get-text-property (- (point) 1) 'face)
                        '(font-lock-type-face tree-sitter-hl-face:type))))
        (insert ": ")
      (insert ":")))
  (def ruby-newline-dwim
    (let ((add-newline (or (eolp)
                           (looking-at "\|$")
                           (looking-at "\)$"))))
      (move-end-of-line nil)
      (smart-newline)
      (insert "end")
      (move-beginning-of-line nil)
      (if add-newline
          (smart-newline)
        (indent-according-to-mode))))
  (defun hippie-expand-ruby-symbols (f &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply f args)))
      (apply f args)))
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols))

(use-package ruby-tools
  :demand t
  :after ruby-mode)

(use-package rspec-mode
  :after ruby-mode
  :bind
  (:map rspec-compilation-mode-map ("s-R" . rspec-rerun))
  :config
  (after yasnippet
    (rspec-install-snippets)))

(use-package minitest
  :after ruby-mode
  :hook
  (ruby-mode . minitest-mode)
  :custom
  (minitest-keymap-prefix (kbd "C-c ."))
  :bind
  (:map minitest-compilation-mode-map ("s-R" . minitest-rerun))
  :config
  (after yasnippet
    (minitest-install-snippets)))

(use-package inf-ruby
  :config
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter t)
  (add-hook 'after-init-hook #'inf-ruby-switch-setup)
  (add-λ 'inf-ruby-mode-hook
    (turn-on-comint-history ".pry_history"))
  (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
  (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map))

(use-package projectile-rails
  :bind
  (:map projectile-rails-mode-map ("C-c r" . projectile-rails-command-map))
  :init
  (projectile-rails-global-mode))

(use-package ruby-hash-syntax
  :after ruby-mode
  :bind
  (:map ruby-mode-map ("C-c C-:" . ruby-hash-syntax-toggle)))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind
  (:map yaml-mode-map (":" . self-with-space))
  :hook
  (yaml-mode . text-smaller-no-truncation))

(use-feature text-mode
  :bind
  (:map text-mode-map ("," . self-with-space)))

(use-package lua-mode
  :mode
  ("\\.lua$" . lua-mode)
  :custom
  (lua-indent-level 2))

;;;;; Version Control

(use-feature ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-feature vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-feature diff-mode
  :custom
  (diff-font-lock-prettify t))

(use-package magit
  :bind
  ("s-m" . magit-status)
  (:map hemacs-git-map ("l" . magit-clone))
  (:map hemacs-git-map ("f" . magit-diff-buffer-file))
  :custom
  (magit-status-goto-file-position t)
  (magit-log-section-commit-count 0)
  (magit-log-auto-more t)
  (magit-branch-prefer-remote-upstream t)
  (magit-diff-refine-hunk 'all)
  (magit-no-confirm t)
  :hook
  (magit-process-mode . text-smaller-no-truncation)
  :config
  (defun magit-process-alert-after-finish-in-background (f &rest args)
    (let* ((process (nth 0 args))
           (event (nth 1 args))
           (buf (process-get process 'command-buf))
           (buff-name (buffer-name buf)))
      (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
        (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
      (apply f (list process event))))
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background))

(use-package forge
  :after magit
  :custom
  (forge-topic-list-limit '(60 . 0))
  :bind
  (:map hemacs-git-map ("c" . forge-browse-commit)))

(use-package magit-todos
  :after magit
  :custom
  (magit-todos-max-items 256)
  :bind
  (:map hemacs-git-map ("t" . magit-todos-list)))

(use-package tray
  :bind
  ("C-c g" . tray-epa-key-list-dispatch))

(use-package browse-at-remote
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  (browse-at-remote-prefer-symbolic nil)
  :bind
  (:map hemacs-git-map ("o" . browse-at-remote)))

;;;;; Emacs Lisping

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  (:map hemacs-help-map ("." . helpful-at-point)))

(use-package elisp-slime-nav
  :hook
  ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

;;;;; Help & Docs

(use-package define-word
  :bind
  (:map hemacs-help-map ("w" . define-word-at-point)))

(use-package google-this
  :bind
  (:map hemacs-help-map ("g" . google-this)))

(use-package gist
  :bind
  (:map hemacs-git-map ("g" . gist-region-or-buffer-private)))

(use-package git-modes)

(use-package flycheck
  :custom
  (flycheck-display-errors-delay most-positive-fixnum)
  :bind
  ("s-?" . flycheck-display-error-at-point))

(use-package list-environment
  :bind
  (:map hemacs-help-map ("l" . list-environment)))

(use-package memory-usage
  :bind
  (:map hemacs-help-map ("m" . memory-usage)))

;;;;; Posframe

(use-package posframe
  :config
  (defun hemacs-posframe-arghandler (f &rest args)
    (apply f (car args) (plist-put (cdr args) :internal-border-width 12)))
  (advice-add 'posframe-show :around #'hemacs-posframe-arghandler))

(use-package transient-posframe
  :custom
  (transient-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)
  (transient-posframe-min-height 1)
  (transient-posframe-min-width 1)
  :init
  (transient-posframe-mode))

(use-package frog-menu
  :custom
  (frog-menu-avy-padding t))

(use-package frog-jump-buffer
  :chords
  (";a" . frog-jump-buffer)
  :custom
  (frog-jump-buffer-posframe-handler #'posframe-poshandler-frame-center)
  (frog-jump-buffer-default-filters-capital-letters t)
  (frog-jump-buffer-filter-actions '(("X" "[special]" frog-jump-buffer-filter-special-buffers)))
  :config
  (dolist (regexp '("TAGS" "-lsp\\*$" "^\\*lsp-" "^\\*straight-process" "^\\magit-" "^\\*Compile-log"
                    "-debug\\*$" "^\\:" "^\\*helpful" "^\\*Async" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*direnv" "^\\*vc" "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))
  (defun frog-jump-buffer-filter-special-buffers (buffer)
    (with-current-buffer buffer
      (-any? #'derived-mode-p '(comint-mode vterm-mode magit-mode inf-ruby-mode rg-mode compilation-mode)))))

;;;;; Language Server

(use-package lsp-mode
  :hook
  ((typescript-mode sgml-mode web-mode html-mode css-mode less-css-mode scss-mode ruby-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-signature-function #'lsp-signature-posframe)
  (lsp-eldoc-enable-hover nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none)
  :config
  (dolist (pattern '("[/\\\\]storage\\'" "[/\\\\]tmp\\'" "[/\\\\]log\\'" "[/\\\\]\\.log\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories pattern))
  (defun lsp-format-buffer-maybe-call-format-all (f &rest args)
    (condition-case err
        (apply f args)
      (error
       (call-interactively 'format-all-buffer))))
  (advice-add 'lsp-format-buffer :around #'lsp-format-buffer-maybe-call-format-all)
  (push '(".*\\.html\\.erb$" . "html") lsp-language-id-configuration)
  (push '(".*\\.hbs$" . "html") lsp-language-id-configuration)
  :bind*
  ("C-M-\\" . lsp-format-buffer))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay most-positive-fixnum)
  :config
  (lsp-ui-doc-mode)
  :bind
  (:map lsp-command-map ("hd" . lsp-ui-doc-show)))

(use-package lsp-tailwindcss
  :custom
  (lsp-tailwindcss-add-on-mode t)
  (lsp-tailwindcss-emmet-completions t))

;;;;; Appearance

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package fira-code-mode
  :custom
  (fira-code-mode-enable-hex-literal nil)
  :init
  (unless (member "Fira Code Symbol" (font-family-list))
    (fira-code-mode-install-fonts))
  (global-fira-code-mode)
  (set-face-attribute 'default nil :height (* default-font-size 10)))

(use-package showtip
  :after
  flycheck
  :commands
  (flycheck-display-error-messages-tooltip)
  :custom
  (showtip-top-adjust (* default-font-size -1))
  (flycheck-display-errors-function #'flycheck-display-error-messages-tooltip)
  :config
  (let ((inhibit-message t))
    (shell-command
     (concat "defaults write org.gnu.Emacs NSToolTipsFontSize -int "
             (number-to-string default-font-size))))
  (defun flycheck-display-error-messages-tooltip (errors)
    (when errors
      (let ((message (flycheck-help-echo-all-error-messages errors)))
        (showtip message)))))

(use-feature frame
  :custom
  (blink-cursor-blinks 0)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  :init
  (add-to-list 'initial-frame-alist '(undecorated . t))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (window-divider-mode)
  (blink-cursor-mode))

(use-feature scroll-bar
  :hook
  (window-configuration-change . update-scroll-bars)
  (buffer-list-update . update-scroll-bars)
  :config
  (def update-scroll-bars
    (mapc (lambda (win) (set-window-scroll-bars win nil)) (window-list))
    (when (and buffer-file-name (> (car (buffer-line-statistics)) (window-screen-lines)))
      (set-window-scroll-bars (selected-window) nil t))))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :init
  (global-page-break-lines-mode))

(use-feature pulse
  :custom
  (pulse-iterations 20)
  :hook
  (window-configuration-change-hook . hemacs-pulse-line)
  :config
  (defun hemacs-pulse-line (&rest _)
    (interactive)
    (let ((pulse-command-advice-flag
           (not (or (window-minibuffer-p)
                    (seq-find 'derived-mode-p '(magit-status-mode comint-mode vterm-mode))))))
      (pulse-line-hook-function)))
  (dolist (command '(next-window-any-frame
                     scroll-up-command
                     scroll-down-command
                     revert-buffer
                     recenter-top-bottom
                     move-to-window-line-top-bottom
                     symbol-overlay-basic-jump))
    (advice-add command :after #'hemacs-pulse-line)))

(use-package goggles
  :hook
  ((prog-mode text-mode) . goggles-mode))

(use-package symbol-overlay
  :custom
  (symbol-overlay-idle-time 0.2)
  :bind
  ("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-feature tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-show 1)
  (tab-bar-tab-name-function #'tab-bar-tab-project-name-current)
  :bind-keymap
  ("s-t" . tab-prefix-map)
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  :hook
  (after-init . tab-bar-mode)
  :init
  (defun tab-bar-tab-project-name-current ()
    (if-let (project (project--find-in-directory (buffer-file-name (window-buffer (minibuffer-selected-window)))))
        (file-name-nondirectory (directory-file-name (car (project-roots project))))
      (tab-bar-tab-name-truncated)))
  (after embark
    (keymap-set embark-buffer-map "t" #'switch-to-buffer-other-tab)
    (keymap-set embark-file-map "t" #'find-file-other-tab))
  (tab-bar-mode))

(use-package highlight-indentation
  :hook
  (indent-sensitive-modes . highlight-indentation-current-column-mode))

(use-feature hl-line
  :init
  (global-hl-line-mode))

(use-package hl-sentence
  :hook
  (text-mode . hl-sentence-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 5))

(use-package moody
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :init
  (minions-mode))

(use-package hide-mode-line
  :hook
  ((dired-mode magit-mode magit-popup-mode org-capture-mode tab-switcher-mode) . hide-mode-line-mode))

(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen t)
  :config
  (show-paren-mode))

(use-feature fringe
  :config
  (fringe-mode '(16 . 8))
  :custom
  (fringe-indicator-alist
   (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)))

(use-package solaire-mode
  :config
  (advice-remove 'transient--insert-groups #'solaire-mode--enable-if-global)
  :init
  (solaire-global-mode))

(use-package apropospriate-theme
  :custom
  (apropospriate-org-level-resizing nil)
  :init
  (load-theme 'apropospriate-light t t)
  (load-theme 'apropospriate-dark t))

(use-package popper
  :bind
  ("C-`"   . popper-toggle-latest)
  ("C-~"   . popper-cycle)
  :custom
  (popper-window-height 0.37)
  (popper-display-function #'popper-select-popup-at-bottom-no-mode-line)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Backtrace\\*"
     "\\*Warnings\\*"
     "Output\\*$"
     "*Process List*"
     "*Embark Actions*"
     "COMMIT_EDITMSG"
     help-mode
     helpful-mode
     embark-collect-mode
     grep-mode
     rg-mode
     vc-annotate-mode
     rspec-compilation-mode
     minitest-compilation-mode
     inf-ruby-mode
     nodejs-repl-mode
     ts-comint-mode
     compilation-mode))
  :init
  (defun popper-select-popup-at-bottom-no-mode-line (buffer &optional _alist)
    (let ((window (display-buffer-in-side-window
                   buffer
                   `((window-height . ,popper-window-height)
                     (side . bottom)
                     (slot . 1)
                     (window-parameters ,`(mode-line-format . none))))))
      (select-window window)))
  (popper-mode)
  (popper-echo-mode))

;;;;; Bindings & Chords

(use-feature ns-win
  :custom
  (mac-right-option-modifier 'none))

(use-feature ffap
  :commands
  (ffap-file-at-point ffap-url-at-point)
  :bind
  (:map hemacs-help-map ("p" . ffap)))

(use-package key-chord
  :chords
  ("}|" . pad-pipes)
  ("[]" . pad-brackets)
  ("{}" . open-brackets-newline-and-indent)
  ("-=" . insert-arrow)
  ("_+" . insert-fat-arrow)
  ("''" . "’")
  ("^^" . "λ")
  :custom
  (key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1))

(use-package free-keys
  :bind
  (:map hemacs-help-map ("K" . free-keys)))

(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-100)
  (which-key-max-display-columns 4)
  :init
  (which-key-mode)
  (dolist (prefix '("projectile-switch-project" "ember" "magit" "projectile" "rails" "project" "embark"))
    (let ((pattern (concat "^" prefix "-\\(.+\\)")))
      (push `((nil . ,pattern) . (nil . "\\1"))
            which-key-replacement-alist))))
