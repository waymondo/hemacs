;; -*- lexical-binding: t -*-

;;;;; Personal Variables, Macros, & Helpers

(defconst indent-sensitive-modes '(coffee-mode slim-mode haml-mode yaml-ts-mode))
(defconst writing-modes '(org-mode markdown-mode fountain-mode git-commit-mode))
(defconst default-indent-width 2)
(define-prefix-command 'hemacs-git-map)
(define-prefix-command 'hemacs-help-map)
(bind-key "s-g" #'hemacs-git-map)
(bind-key "s-h" #'hemacs-help-map)

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro after (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro use-feature (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :ensure nil
     ,@args))

(defun inhibit-message-in-minibuffer (f &rest args)
  (let ((inhibit-message (minibufferp)))
    (apply f args)))

(defun reset-scroll-margin ()
  (set (make-local-variable 'scroll-margin) 0))

(defun text-smaller-no-truncation ()
  (setq truncate-lines nil)
  (text-scale-set -0.25))

;;;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(use-package no-littering :demand t)
(use-package use-package-chords :demand t)
(use-feature use-package-ensure-system-package :demand t)

;;;;; Bootstrap

(use-feature emacs
  :custom
  (history-length 64)
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
  (enable-recursive-minibuffers t)
  (kill-buffer-query-functions nil)
  (frame-inhibit-implied-resize t)
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (frame-resize-pixelwise t)
  (read-process-output-max (* 1024 1024))
  (use-short-answers t)
  (x-underline-at-descent-line t)
  (cursor-type 'bar)
  :config
  (setq-default line-spacing 2
                tab-width default-indent-width
                cursor-in-non-selected-windows nil
                fill-column 100
                truncate-lines t))

(use-feature cus-edit
  :custom
  (custom-file (make-temp-file "emacs-custom")))

(use-feature advice
  :custom
  (ad-redefinition-action 'accept))

(use-feature novice
  :custom
  (disabled-command-function nil))

(use-feature paragraphs
  :custom
  (sentence-end-double-space nil))

(use-package dash
  :custom
  (dash-enable-fontlock t))

(use-feature keymap
  :init
  (dolist (key-binding '("s-q" "s-t" "s-o" "s-n"))
    (keymap-global-unset key-binding)))

(use-package transform-string-at-point
  :vc
  (:url "https://github.com/waymondo/transform-string-at-point" :rev :newest)
  :custom
  (transform-string-at-point-cursor-after-transform 'next-string)
  :bind
  ("s-;" . transform-string-at-point))

(use-feature menu-bar
  :bind
  ("s-w" . kill-this-buffer))

(use-feature subr
  :config
  (y-or-n-p-use-read-key t))

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
  (comint-mode . reset-scroll-margin)
  :config
  (setq-default comint-input-ignoredups t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setopt comint-input-ring-file-name history-file)
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
  (compilation-mode . reset-scroll-margin)
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

(use-package mistty
  :bind
  ("C-x m" . mistty-in-project)
  (:map project-prefix-map ("t" . mistty-in-project))
  :custom
  (mistty-detect-foreign-overlays nil)
  :hook
  (mistty-mode . reset-scroll-margin))

(use-feature sh-script
  :mode
  ((rx (or ".zshrc" ".zprofile")) . sh-mode)
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
  (view-read-only t)
  (confirm-kill-emacs nil)
  (enable-local-variables :safe)
  (confirm-nonexistent-file-or-buffer nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (large-file-warning-threshold 50000000)
  (find-sibling-rules '(("\\([^/.]+\\)\\..*\\'" "\\1.*")))
  :chords
  (";f" . find-file)
  (":F" . find-sibling-file)
  :bind
  ("s-S" . rename-visited-file)
  ("s-W" . delete-visited-file)
  ("s-," . find-user-init-file)
  ([remap save-buffers-kill-terminal] . restart-emacs)
  :config
  (defun find-user-init-file ()
    (interactive)
    (find-file user-init-file))
  (defun delete-visited-file ()
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (delete-file filename delete-by-moving-to-trash)
        (message "Deleted file %s" filename)
        (kill-buffer))))
  (advice-add 'rename-visited-file :around #'rename-file-maybe-make-directories)
  (defun rename-file-maybe-make-directories (f &rest args)
    (maybe-make-directories (nth 0 args))
    (apply f args))
  (defun maybe-make-directories (&optional target-file-name)
    (let* ((target-file-name (or target-file-name buffer-file-name))
           (dir (file-name-directory target-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'maybe-make-directories find-file-not-found-functions))

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
  :hook
  (after-init . recentf-mode))

(use-feature dired
  :custom
  (dired-use-ls-dired nil)
  (dired-recursive-deletes 'always))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1)
  (all-the-icons-default-adjust 0)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts)))

(use-package all-the-icons-completion
  :after
  (marginalia all-the-icons)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package dirvish
  :preface
  (require 'dirvish-subtree)
  (require 'dirvish-side)
  :bind
  ("s-\\" . dirvish-side)
  (:map dired-mode-map
        ([remap dired-summary] . dirvish-dispatch)
        ("TAB" . dirvish-subtree-toggle))
  :hook
  (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-mode-line-position 'disable)
  (dirvish-attributes '(subtree-state all-the-icons collapse file-size)))

(use-package terminal-here
  :bind
  ("C-c o t" . terminal-here))

(use-package reveal-in-folder
  :bind
  ("C-c o f" . reveal-in-folder))

;;;;; Editing

(use-feature indent
  :custom
  (standard-indent default-indent-width)
  (tab-always-indent 'complete))

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
  (indent-tabs-mode nil)
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
  (before-save . progish-delete-trailing-whitespace)
  :config
  (column-number-mode)
  (defun delete-region-instead-of-kill (f &rest args)
    (cl-letf (((symbol-function 'kill-region) #'delete-region))
      (apply f args)))
  (advice-add 'backward-kill-word :around #'delete-region-instead-of-kill)
  (defun progish-delete-trailing-whitespace ()
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace)))
  (defun keyboard-quit-minibuffer-first (f &rest args)
    (if-let ((minibuffer (active-minibuffer-window)))
        (with-current-buffer (window-buffer minibuffer)
          (minibuffer-keyboard-quit))
      (apply f args)))
  (advice-add 'keyboard-quit :around #'keyboard-quit-minibuffer-first)
  (defun pop-to-mark-command-until-new-point (f &rest args)
    (let ((p (point)))
      (dotimes (_i 10)
        (when (= p (point))
          (apply f args)))))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (advice-add 'pop-to-mark-command :around #'pop-to-mark-command-until-new-point)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

(use-package expreg
  :vc
  (:url "https://github.com/casouri/expreg" :rev :newest)
  :bind
  ("C-," . expreg-expand))

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
  (:map dired-mode-map ("." . avy-goto-line-this-window))
  (:map isearch-mode-map ("M-q" . avy-isearch))
  :chords
  ("jj" . avy-goto-char-timer)
  ("jk" . avy-goto-word-or-subword-1)
  ("jl" . avy-goto-line)
  :config
  (def avy-goto-line-this-window
    (avy-goto-line 4)))

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
  ("C-c s-d" . mc/mark-all-like-this-dwim)
  :hook
  (before-save . mc/keyboard-quit))

(use-feature misc
  :bind
  ("s-D" . duplicate-dwim))

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
  (require 'vertico-quick)
  (require 'vertico-directory)
  :custom
  (vertico-count 20)
  :config
  (vertico-mode)
  (def vertico-quick-insert-and-return
    (vertico-quick-insert)
    (vertico-exit))
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :chords
  (:map vertico-map ("jj" . vertico-quick-insert-and-return)))

(use-package corfu
  :preface
  (require 'corfu-popupinfo)
  (require 'corfu-quick)
  :hook
  (after-init . global-corfu-mode)
  (minibuffer-setup . corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :chords
  (:map corfu-map ("jj" . corfu-quick-complete)))

(use-package kind-icon
  :after corfu
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
  (consult-preview-key "M-."))

(use-package affe
  :custom
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :chords
  (";g" . affe-grep)
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
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
    (setopt embark-indicators
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
  :hook
  (after-init . yas-global-mode))

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions (cons #'tempel-expand completion-at-point-functions)))
  :hook
  ((prog-mode text-mode eglot-managed-mode) . tempel-setup-capf))

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
  :chords
  (";t" . project-find-file)
  :custom
  (project-switch-use-entire-map t)
  (project-mode-line t))

(use-package projector
  :bind
  ("C-x RET" . projector-run-shell-command-project-root)
  (:map comint-mode-map ("s-R" . projector-rerun-buffer-process))
  (:map project-prefix-map ("RET" . projector-run-shell-command-project-root))
  :custom
  (projector-project-package 'project))

(use-package beginend
  :init
  (beginend-global-mode))

;;;;; External Utilities

(use-package gptel)

(use-package emacs-everywhere)

;;;;; Major Modes

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode))

(use-feature org
  :bind
  (:map org-mode-map
        ("," . self-with-space)
        ("C-c C-." . org-todo))
  :custom
  (org-support-shift-select t)
  (org-startup-indented t)
  :config
  (advice-add 'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window))

(use-package org-autolist
  :after org
  :hook
  (org-mode . org-autolist-mode))

(use-feature sgml-mode
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
  (defun run-prog-mode-hooks ()
    (run-hooks 'prog-mode-hook))
  (add-hook 'sgml-mode-hook #'run-prog-mode-hooks)
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
  ("\\.ecr\\'"        . web-mode)
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim)
        ("C-c C-." . tsx-ts-mode))
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
  :bind
  (:map css-base-mode-map
        ("," . self-with-space)
        ("{" . open-brackets-newline-and-indent))
  :custom
  (css-indent-offset 2))

(use-package less-css-mode
  :custom
  (less-css-lessc-options '("--no-color" "-x")))

(use-feature js
  :bind
  (:map js-base-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  :custom
  (js-indent-level 2))

(use-package nodejs-repl)

(use-package graphql-ts-mode
  :config
  (after treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))

(use-package dotenv-mode)

(use-feature typescript-ts-mode
  :demand t
  :bind
  (:map typescript-ts-base-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  (:map tsx-ts-mode-map
        ("C-c C-." . web-mode)))

(use-package ts-comint)

(use-package format-all
  :bind
  ("C-M-\\" . format-all-buffer))

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
    (newline-and-indent)))

(use-feature ruby-ts-mode
  :bind
  (:map ruby-ts-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        (":"          . smart-ruby-colon)
        ("<C-return>" . ruby-newline-dwim))
  :init
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
      (newline)
      (insert "end")
      (move-beginning-of-line nil)
      (if add-newline
          (smart-newline)
        (indent-according-to-mode)))))

(use-package ruby-tools
  :demand t
  :after ruby-mode)

(use-package yard-mode
  :hook
  (ruby-base-mode . yard-mode))

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
  (ruby-base-mode . minitest-mode)
  :custom
  (minitest-keymap-prefix (kbd "C-c ."))
  :bind
  (:map minitest-compilation-mode-map ("s-R" . minitest-rerun))
  :config
  (after yasnippet
    (minitest-install-snippets)))

(use-package inf-ruby
  :hook
  (ruby-base-mode . inf-ruby-minor-mode)
  (compilation-filter . inf-ruby-auto-enter)
  (after-init . inf-ruby-switch-setup))

(use-package rbs-mode)

(use-package projectile-rails
  :bind
  (:map projectile-rails-mode-map ("C-c r" . projectile-rails-command-map))
  :init
  (projectile-rails-global-mode))

(use-package rails-i18n)

(use-package haml-mode)

(use-package coffee-mode)

(use-package sass-mode)

(use-package crystal-mode)

(use-package inf-crystal
  :hook
  (crystal-mode . inf-crystal-minor-mode))

(use-feature dockerfile-ts-mode
  :demand t)

(use-feature yaml-ts-mode
  :demand t
  :bind
  (:map yaml-ts-mode-map (":" . self-with-space))
  :hook
  (yaml-ts-mode . text-smaller-no-truncation))

(use-feature text-mode
  :bind
  (:map text-mode-map ("," . self-with-space)))

(use-feature lua-ts-mode
  :custom
  (lua-ts-indent-offset 2))

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
  (:map project-prefix-map ("m" . magit-project-status))
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
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
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

(use-feature eldoc
  :hook
  (after-init . global-eldoc-mode)
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil))

(use-feature shortdoc
  :bind
  (:map hemacs-help-map ("s" . shortdoc)))

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

(use-package list-environment
  :bind
  (:map hemacs-help-map ("l" . list-environment)))

(use-package memory-usage
  :bind
  (:map hemacs-help-map ("m" . memory-usage)))

(use-package devdocs-browser
  :bind
  (:map hemacs-help-map ("d" . devdocs-browser-open)))

;;;;; Childframes & Posframes

(use-package eldoc-box
  :bind
  ("s-?" . eldoc-box-help-at-point))

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
  :hook
  (after-init . transient-posframe-mode))

(use-package which-key-posframe
  :custom
  (which-key-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)
  :hook
  (after-init . which-key-posframe-mode))

(use-package frog-menu
  :custom
  (frog-menu-avy-padding t))

(use-package frog-jump-buffer
  :chords
  (";a" . frog-jump-buffer)
  :custom
  (frog-jump-buffer-posframe-handler #'posframe-poshandler-frame-center)
  (frog-jump-buffer-default-filters-capital-letters t)
  (frog-jump-buffer-project-package 'project)
  :config
  (dolist
      (regexp
       '("TAGS"
         "^\\magit-"
         "^\\*Compile-log"
         "-debug\\*$"
         "^\\:"
         "^\\*helpful"
         "^\\*Async"
         "errors\\*$"
         "^\\*Backtrace"
         "stderr\\*$"
         "EGLOT"
         "^\\*Flymake"
         "^\\*Warnings"
         "^\\*eldoc"
         "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers)))

(use-package mini-popup
  :vc
  (:url "https://github.com/minad/mini-popup" :rev :newest)
  :config
  (defun set-mini-popup-frame-parameters ()
    (let* ((char-height (frame-char-height))
           (mini-frame-height (+ 3 vertico-count))
           (mini-frame-width 128)
           (mini-frame-border-width (/ char-height 2)))
      (setq mini-popup--frame-parameters
            (map-merge
             'alist
             mini-popup--frame-parameters
             `(
               (height . ,mini-frame-height)
               (width . ,mini-frame-width)
               (user-position . t)
               (left . ,(/ (- (frame-outer-width) (* mini-frame-width (frame-char-width)) char-height) 2))
               (top . ,(/ (- (frame-outer-height) (* mini-frame-height (frame-char-height)) char-height) 2))
               (child-frame-border-width . ,mini-frame-border-width)
               (left-fringe . 0)
               (right-fringe . 0))))))
  (defun maybe-disable-vertico-resize-window (&rest args)
    (unless mini-popup-mode
      (apply args)))
  (advice-add #'vertico--resize-window :around #'maybe-disable-vertico-resize-window)
  (add-hook 'consult--completion-refresh-hook #'mini-popup--setup 99)
  :hook
  (mini-popup-mode . set-mini-popup-frame-parameters)
  (after-init . mini-popup-mode))

;;;;; Language Server & Tree Sitter

(use-feature eglot
  :hook
  ((typescript-ts-base-mode
    js-base-mode
    sgml-mode
    css-base-mode
    ruby-base-mode
    yaml-ts-mode
    toml-ts-mode
    lua-mode
    dockerfile-ts-mode
    markdown-mode
    sh-base-mode) . eglot-ensure)
  :bind
  ("C-M-s-\\" . eglot-format)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :init
  (defun disable-eglot-format-check (f &rest args)
    (cl-letf (((symbol-function 'eglot--server-capable) #'identity))
      (apply f args)))
  (advice-add #'eglot-format :around #'disable-eglot-format-check))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :hook
  (after-init . global-treesit-auto-mode))

;;;;; Appearance

(use-feature custom
  :custom
  (custom-safe-themes t))

(use-feature faces
  :init
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 150))

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  :hook
  (after-init . global-ligature-mode))

(use-package hl-todo
  :hook
  (after-init . global-hl-todo-mode))

(use-feature frame
  :custom
  (blink-cursor-blinks 0)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  :init
  (window-divider-mode)
  (blink-cursor-mode))

(use-feature scroll-bar
  :hook
  (window-configuration-change . update-scroll-bars)
  :config
  (add-hook 'window-size-change-functions #'update-scroll-bars)
  (add-hook 'window-selection-change-functions #'update-scroll-bars)
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
  (window-configuration-change . hemacs-pulse-line)
  :config
  (defun hemacs-pulse-line (&rest _)
    (interactive)
    (let ((pulse-command-advice-flag
           (not (or (window-minibuffer-p)
                    (frame-parent)
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

(use-feature hl-line
  :init
  (global-hl-line-mode))

(use-package indent-bars
  :vc
  (:url "https://github.com/jdtsmith/indent-bars" :rev :newest)
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-highlight-current-depth '(:blend 0.64))
  (indent-bars-treesit-support t)
  :hook
  (yaml-ts-mode . indent-bars-mode))

(use-package hl-sentence
  :hook
  (text-mode . hl-sentence-mode))

(use-package paren-face
  :hook
  (after-init . global-paren-face-mode)
  :custom
  (paren-face-regexp "[][(){}]")
  :config
  (push 'prog-mode paren-face-modes))

(use-package moody
  :init
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification))

(use-package minions
  :init
  (minions-mode))

(use-package hide-mode-line
  :hook
  ((dired-mode help-mode magit-mode magit-popup-mode org-capture-mode tab-switcher-mode) . hide-mode-line-mode))

(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode))

(use-feature fringe
  :config
  (fringe-mode '(16 . 8))
  :custom
  (fringe-indicator-alist
   (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)))

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
     "*format-all-errors*"
     "*eldoc*"
     "COMMIT_EDITMSG"
     flymake-diagnostics-buffer-mode
     help-mode
     helpful-mode
     embark-collect-mode
     grep-mode
     shortdoc-mode
     rg-mode
     vc-annotate-mode
     rspec-compilation-mode
     xref--xref-buffer-mode
     minitest-compilation-mode
     inf-ruby-mode
     magit-process-mode
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
  (ns-pop-up-frames nil)
  (mac-right-option-modifier 'none))

(use-feature ffap
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
  (key-chord-safety-interval-forward 0.05)
  (key-chord-safety-interval-backward 0.05)
  (key-chord-two-keys-delay 0.05)
  :init
  (def insert-arrow
    (ensure-space :before)
    (insert "->")
    (ensure-space :after))
  (def insert-fat-arrow
    (ensure-space :before)
    (insert "=>")
    (ensure-space :after))
  (def pad-pipes
    (ensure-space :before)
    (insert "||")
    (backward-char))
  (def pad-brackets
    (unless (looking-back (rx (or "(" "[")) nil)
      (ensure-space :before))
    (insert "{  }")
    (backward-char 2))
  :config
  (key-chord-mode 1))

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

;;;;; Snippets

(defun ensure-space (direction)
  (let* ((char-fn
          (cond
           ((eq direction :before)
            #'char-before)
           ((eq direction :after)
            #'char-after)))
         (char-result (funcall char-fn)))
    (unless (and (not (eq char-result nil)) (string-match-p " " (char-to-string char-result)))
      (insert " "))
    (when (and (eq char-fn #'char-after) (looking-at " "))
      (forward-char))))

(def self-with-space
  (call-interactively #'self-insert-command)
  (ensure-space :after))

(def pad-equals
  (if (nth 3 (syntax-ppss))
      (call-interactively #'self-insert-command)
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>+~]" nil)
           (ensure-space :before)))
    (self-with-space)))

(def open-brackets-newline-and-indent
  (let ((inhibit-message t)
        (text
         (when (region-active-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (unless (looking-back (rx (or "(" "[")) nil)
      (ensure-space :before))
    (insert (concat "{\n" text "\n}"))
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))
