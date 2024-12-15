;; -*- lexical-binding: t -*-

;;;;; Macros

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

;;;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(use-package no-littering :demand t)
(use-package use-package-chords :demand t)
(use-package system-packages)
(use-feature use-package-ensure-system-package :demand t)

;;;;; Bootstrap

(use-feature emacs
  :custom
  (history-delete-duplicates t)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position 'always)
  (echo-keystrokes 1e-6)
  (delete-by-moving-to-trash t)
  (ring-bell-function #'ignore)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (redisplay-skip-fontification-on-input t)
  (frame-resize-pixelwise t)
  (read-process-output-max (* 1024 1024))
  (use-short-answers t)
  (x-underline-at-descent-line t)
  (cursor-type 'bar)
  (cursor-in-non-selected-windows nil)
  (line-spacing 2)
  (tab-width 2)
  (fill-column 100)
  (ns-right-alternate-modifier 'none))

(use-feature cus-edit
  :custom
  (custom-file (make-temp-file "emacs-custom")))

(use-feature advice
  :custom
  (ad-redefinition-action 'accept))

(use-feature novice
  :custom
  (disabled-command-function nil))

(use-feature keymap
  :init
  (dolist (key-binding '("s-q" "s-t" "s-o" "s-n" "s-w"))
    (keymap-global-unset key-binding)))

;;;;; Processes, Shells, Compilation

(use-feature comint
  :bind
  (:map comint-mode-map
        ("RET"       . comint-return-dwim)
        ("C-r"       . comint-history-isearch-backward-regexp)
        ("M-TAB"     . comint-previous-matching-input-from-input)
        ("<M-S-tab>" . comint-next-matching-input-from-input))
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-show-maximum-output nil)
  (comint-output-filter-functions
   '(ansi-color-process-output
     comint-truncate-buffer
     comint-watch-for-password-prompt))
  :config
  (defun turn-on-comint-history (history-file)
    (setopt comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (defun comint-return-dwim ()
    (interactive)
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
  :init
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package mistty
  :bind
  ("C-x m" . mistty-in-project)
  (:map project-prefix-map ("t" . mistty-in-project))
  :custom
  (mistty-detect-foreign-overlays nil))

(use-feature executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

;;;;; Files & History

(use-feature files
  :custom
  (require-final-newline t)
  (confirm-kill-processes nil)
  (make-backup-files nil)
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (find-sibling-rules '(("\\([^/.]+\\)\\..*\\'" "\\1.*")))
  (revert-buffer-quick-short-answers t)
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

(use-feature ffap
  :hook
  (after-init . ffap-bindings))

(use-feature savehist
  :custom
  (savehist-additional-variables
   '(search-ring regexp-search-ring comint-input-ring projector-command-history))
  :hook
  (after-init . savehist-mode))

(use-feature saveplace
  :custom
  (save-place-limit nil)
  :hook
  (after-init . save-place-mode))

(use-feature recentf
  :custom
  (recentf-max-saved-items nil)
  :hook
  (after-init . recentf-mode))

(use-feature dired
  :custom
  (dired-use-ls-dired nil)
  (dired-recursive-deletes 'always))

(use-package dirvish
  :bind
  (:map dired-mode-map ([remap dired-summary] . dirvish-dispatch))
  :hook
  (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-mode-line-position 'disable))

(use-feature dirvish-subtree
  :after
  (dirvish all-the-icons)
  :bind
  (:map dired-mode-map ("TAB" . dirvish-subtree-toggle))
  :custom
  (dirvish-attributes '(subtree-state all-the-icons collapse file-size)))

(use-feature dirvish-side
  :after dirvish
  :bind
  ("s-\\" . dirvish-side))

;;;;; Editing

(use-package transform-symbol-at-point
  :bind
  ("s-;" . transform-symbol-at-point-map))

(use-feature indent
  :custom
  (standard-indent 2)
  (tab-always-indent 'complete))

(use-feature newcomment
  :bind
  ("s-." . insert-todo-comment)
  ("s-/" . comment-line)
  :config
  (defun insert-todo-comment ()
    (interactive)
    (call-interactively #'comment-dwim)
    (ensure-space :before)
    (insert "TODO:")
    (ensure-space :after)))

(use-feature simple
  :custom
  (indent-tabs-mode nil)
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (line-move-visual nil)
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  (backward-delete-char-untabify-method 'all)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-show-help nil)
  :bind
  ("M-`" . list-processes)
  ("s-P" . execute-extended-command)
  ("s-z" . undo-only)
  ("s-Z" . undo-redo)
  ([remap newline] . reindent-then-newline-and-indent)
  ("<escape>" . keyboard-escape-quit)
  ("<s-return>" . eol-then-newline)
  (:map minibuffer-local-map
        ("<escape>"  . abort-recursive-edit)
        ("M-TAB"     . previous-complete-history-element)
        ("<M-S-tab>" . next-complete-history-element))
  :hook
  ((text-mode markdown-ts-mode) . auto-fill-mode)
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
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (defun eol-then-newline ()
    (interactive)
    (move-end-of-line nil)
    (reindent-then-newline-and-indent))
  (advice-add 'pop-to-mark-command :around #'pop-to-mark-command-until-new-point)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

(use-package expreg
  :bind
  ("C-," . expreg-expand))

(use-feature subword
  :hook
  (after-init . global-subword-mode))

(use-feature autoinsert
  :hook
  (after-init . auto-insert-mode))

(use-feature delsel
  :hook
  (after-init . delete-selection-mode))

(use-feature elec-pair
  :hook
  (after-init . electric-pair-mode))

(use-feature electric
  :custom
  (electric-quote-string t)
  :hook
  (after-init . electric-quote-mode))

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
  (defun avy-goto-line-this-window ()
    (interactive)
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
  (defun avy-jump-error ()
    (interactive)
    (let ((compilation-buffer (compilation-find-buffer))
          (next-error-hook '(avy-jump-error-next-error-hook)))
      (when compilation-buffer
        (with-current-buffer compilation-buffer
          (when (derived-mode-p 'compilation-mode)
            (pop-to-buffer compilation-buffer)
            (ace-link-compilation)))))))

(use-package multiple-cursors
  :bind
  ("s-d"     . mc/mark-next-like-this)
  ("C-c s-d" . mc/mark-all-like-this-dwim)
  :hook
  (before-save . mc/keyboard-quit))

(use-feature misc
  :bind
  ("s-D" . duplicate-dwim))

(use-feature flyspell
  :ensure-system-package ispell
  :custom
  (flyspell-use-meta-tab nil)
  :hook
  (text-mode . flyspell-mode))

;;;;; Completion

(use-feature cursor-sensor
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))

(use-package vertico
  :demand t
  :custom
  (vertico-count 20)
  :hook
  (after-init . vertico-mode))

(use-feature vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-feature vertico-quick
  :after vertico
  :config
  (defun vertico-quick-insert-and-return ()
    (interactive)
    (vertico-quick-insert)
    (vertico-exit))
  :chords
  (:map vertico-map ("jj" . vertico-quick-insert-and-return)))

(use-feature completion-preview
  :hook
  (prog-mode . completion-preview-mode)
  :bind
  (:map
   completion-preview-active-mode-map
   ([remap forward-word] . completion-preview-insert-word)
   ([remap forward-sexp] . completion-preview-insert-sexp)))

(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  :bind
  (:map corfu-map ("RET" . corfu-send)))

(use-feature corfu-popupinfo
  :after corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode))

(use-feature corfu-quick
  :after corfu
  :demand t
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
  :hook
  (after-init . marginalia-mode))

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
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t))))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C->" . embark-act)
  ([remap describe-bindings] . embark-bindings)
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
  (defun hippie-expand-maybe-kill-to-eol (f &rest args)
    (unless (eolp)
      (kill-line))
    (apply f args))
  (defalias 'hippie-expand-line (make-hippie-expand-function
                                 '(try-expand-line
                                   try-expand-line-all-buffers)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
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

(use-feature ns-win
  :custom
  (ns-pop-up-frames nil))

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
  (defun toggle-split-window ()
    (interactive)
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
  (project-mode-line t)
  (project-vc-extra-root-markers '(".tool-versions")))

(use-package projector
  :bind
  ("C-x RET" . projector-run-shell-command-project-root)
  (:map comint-mode-map ("s-R" . projector-rerun-buffer-process))
  (:map project-prefix-map ("RET" . projector-run-shell-command-project-root))
  :custom
  (projector-project-package 'project))

(use-package beginend
  :hook
  (after-init . beginend-global-mode))

;;;;; External Utilities

(use-package gptel)

(use-package emacs-everywhere)

(use-package direnv
  :hook
  (after-init . direnv-mode))

(use-package alert
  :custom
  (alert-default-style 'osx-notifier)
  :init
  (defun alert-after-finish-in-background (buf str)
    (when (or (not (get-buffer-window buf 'visible)) (not (frame-focus-state)))
      (alert str :buffer buf))))

(use-package terminal-here
  :bind
  ("C-c o t" . terminal-here))

(use-package reveal-in-folder
  :bind
  ("C-c o f" . reveal-in-folder))

(use-package list-environment)

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
  (defun html-newline-dwim ()
    (interactive)
    (move-end-of-line nil)
    (reindent-then-newline-and-indent)
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

(use-package markdown-ts-mode
  :mode
  ("\\.md\\'" . markdown-ts-mode)
  :bind
  (:map markdown-ts-mode-map ("," . self-with-space))
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(use-package gh-md
  :after markdown-ts-mode
  :bind
  (:map markdown-ts-mode-map ("C-c b" . gh-md-render-buffer)))

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
  :mode
  ("\\.mjs\\'" . js-ts-mode)
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
  (add-to-list 'treesit-language-source-alist '(graphql "https://github.com/bkegley/tree-sitter-graphql")))

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

(use-package jest-test-mode
  :hook
  (typescript-ts-base-mode js-base-mode))

(use-package format-all
  :bind
  ("C-M-\\" . format-all-buffer))

(use-package slim-mode
  :bind
  (:map slim-mode-map
        (","          . self-with-space)
        (":"          . smart-ruby-colon))
  :custom
  (slim-backspace-backdents-nesting nil))

(use-feature ruby-ts-mode
  :bind
  (:map ruby-ts-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        (":"          . smart-ruby-colon)
        ("<C-return>" . ruby-newline-dwim))
  :init
  (defun smart-ruby-colon ()
    (interactive)
    (if (and (looking-back "[[:word:]]" nil)
             (not (memq (get-text-property (- (point) 1) 'face)
                        '(font-lock-type-face tree-sitter-hl-face:type))))
        (insert ": ")
      (insert ":")))
  (defun ruby-newline-dwim ()
    (interactive)
    (let ((add-newline (or (eolp)
                           (looking-at "\|$")
                           (looking-at "\)$"))))
      (move-end-of-line nil)
      (newline)
      (insert "end")
      (move-beginning-of-line nil)
      (if add-newline
          (reindent-then-newline-and-indent)
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
  :hook
  (after-init . projectile-rails-global-mode))

(use-package rails-i18n)

(use-package haml-mode)

(use-package coffee-mode)

(use-package sass-mode)

(use-feature go-ts-mode
  :demand t)

(use-package crystal-mode)

(use-package inf-crystal
  :hook
  (crystal-mode . inf-crystal-minor-mode))

(use-feature elixir-ts-mode
  :demand t)

(use-package mix
  :hook
  (elixir-ts-mode . mix-minor-mode))

(use-feature dockerfile-ts-mode
  :demand t)

(use-feature yaml-ts-mode
  :demand t
  :bind
  (:map yaml-ts-mode-map (":" . self-with-space)))

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

(use-package git-modes)

(use-package magit
  :bind
  ("s-m" . magit-status)
  ("s-g" . magit-dispatch)
  (:map project-prefix-map ("m" . magit-project-status))
  :custom
  (magit-status-goto-file-position t)
  (magit-log-section-commit-count 0)
  (magit-log-auto-more t)
  (magit-branch-prefer-remote-upstream t)
  (magit-diff-refine-hunk 'all)
  (magit-no-confirm t)
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

(use-package forge)

(use-package magit-todos
  :hook
  (after-init . magit-todos-mode))

(use-package browse-at-remote
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  (browse-at-remote-prefer-symbolic nil)
  :init
  (after magit
    (transient-append-suffix 'magit-dispatch "h"
      '("R" "Browse at remote" browse-at-remote))))

;;;;; Emacs Lisping

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap display-local-help] . helpful-at-point))

(use-package elisp-slime-nav
  :hook
  ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-feature eldoc
  :hook
  (after-init . global-eldoc-mode)
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil))

;;;;; Childframes & Posframes

(use-package eldoc-box
  :bind
  ("s-?" . eldoc-box-help-at-point))

(use-package posframe
  :config
  (defun hemacs-posframe-arghandler (f &rest args)
    (apply f (car args) (plist-put (cdr args) :internal-border-width (frame-char-height))))
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
  (:url "https://github.com/minad/mini-popup")
  :config
  (defun set-mini-popup-frame-parameters ()
    (let* ((border-width (frame-char-height))
           (selected-frame-width (frame-width))
           (mini-frame-height (+ 3 vertico-count))
           (mini-frame-width (if (< selected-frame-width 128) selected-frame-width 128)))
      (setq mini-popup--frame-parameters
            (map-merge
             'alist
             mini-popup--frame-parameters
             `(
               (height . ,mini-frame-height)
               (width . ,mini-frame-width)
               (user-position . t)
               (left . ,(/ (- (frame-outer-width) (* mini-frame-width (frame-char-width)) (* border-width 2)) 2))
               (top . ,(/ (- (frame-outer-height) (* mini-frame-height (frame-char-height)) (* border-width 2)) 2))
               (child-frame-border-width . ,border-width)
               (left-fringe . 0)
               (right-fringe . 0))))))
  (defun maybe-disable-vertico-resize-window (&rest args)
    (unless mini-popup-mode
      (apply args)))
  (advice-add #'vertico--resize-window :around #'maybe-disable-vertico-resize-window)
  (add-hook 'consult--completion-refresh-hook #'mini-popup--setup 99)
  (defun maybe-reset-mini-popup-mode ()
    (when (and mini-popup-mode mini-popup--frame (frame-size-changed-p) (not (frame-parent)))
      (set-mini-popup-frame-parameters)
      (modify-frame-parameters mini-popup--frame mini-popup--frame-parameters)))
  :hook
  (mini-popup-mode . set-mini-popup-frame-parameters)
  (window-configuration-change . maybe-reset-mini-popup-mode)
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
    go-ts-mode
    toml-ts-mode
    lua-mode
    dockerfile-ts-mode
    markdown-ts-mode
    elixir-ts-mode
    sh-base-mode) . eglot-ensure)
  :bind
  ("C-M-s-\\" . eglot-format)
  ("C-c a" . eglot-code-actions)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :init
  (defun disable-eglot-format-check (f &rest args)
    (cl-letf (((symbol-function 'eglot-server-capable-or-lose) #'identity))
      (apply f args)))
  (advice-add #'eglot-format :around #'disable-eglot-format-check)
  :config
  (add-to-list 'eglot-server-programs '(markdown-ts-mode . ("marksman" "server"))))

(use-package eglot-booster
  :vc
  (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :custom
  (eglot-booster-io-only t)
  :init
  (let ((emacs-lsp-booster-path (concat user-emacs-directory "emacs-lsp-booster")))
    (unless (file-exists-p emacs-lsp-booster-path)
      (make-directory emacs-lsp-booster-path))
    (push emacs-lsp-booster-path exec-path)
    (unless (executable-find "emacs-lsp-booster")
      (let ((temporary-zip-file (concat temporary-file-directory "emacs-lsp-booster.zip")))
        (shell-command (format "curl https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-apple-darwin.zip -L -o %s" temporary-zip-file))
        (shell-command (format "unzip %s -d %s" temporary-zip-file emacs-lsp-booster-path))
        (shell-command (format "xattr -r -d com.apple.quarantine %s" (concat emacs-lsp-booster-path "/" "emacs-lsp-booster")))
        (delete-file temporary-zip-file))))
  :hook
  (after-init . eglot-booster-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :hook
  (after-init . global-treesit-auto-mode))

;;;;; Appearance

(use-feature image-mode
  :hook
  (image-mode . show-image-dimensions-in-mode-line)
  :custom
  (image-animate-loop t)
  :config
  (after moody
    (defun show-image-dimensions-in-mode-line ()
      (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
             (width (car image-dimensions))
             (height (cdr image-dimensions)))
        (setq moody-mode-line-buffer-identification
              `(:eval
                (moody-tab
                 (format "%s %dx%d" (propertized-buffer-identification "%b") ,width ,height)
                 20 'down)))))))

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
  (after-init . all-the-icons-completion-mode))

(use-feature custom
  :custom
  (custom-safe-themes t))

(use-feature faces
  :init
  (set-face-attribute 'default nil :height 150)
  (let ((font-family-name "JetBrains Mono"))
    (if (member font-family-name (font-family-list))
        (set-face-attribute 'default nil :family font-family-name)
      (message (concat font-family-name " font not installed, downloading"))
      (browse-url "https://download.jetbrains.com/fonts/JetBrainsMono-2.304.zip"))))

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
  :hook
  (after-init . window-divider-mode)
  (after-init . blink-cursor-mode))

(use-feature pixel-scroll
  :hook
  (after-init . pixel-scroll-precision-mode))

(use-feature scroll-bar
  :hook
  (window-configuration-change . update-scroll-bars)
  :config
  (add-hook 'window-size-change-functions #'update-scroll-bars)
  (add-hook 'window-selection-change-functions #'update-scroll-bars)
  (defun update-scroll-bars (&optional _)
    (mapc (lambda (win) (set-window-scroll-bars win nil)) (window-list))
    (when (and buffer-file-name (> (car (buffer-line-statistics)) (window-screen-lines)))
      (set-window-scroll-bars (selected-window) nil t))))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-prefer-character t)
  :hook
  (prog-mode . indent-bars-mode))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :hook
  (after-init . global-page-break-lines-mode))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-prefer-character t)
  :hook
  (prog-mode . indent-bars-mode))

(use-feature pulse
  :custom
  (pulse-iterations 20)
  :hook
  ((next-error window-configuration-change) . hemacs-pulse-line)
  :config
  (defun hemacs-pulse-line (&rest _)
    (interactive)
    (let ((pulse-command-advice-flag
           (not (or (window-minibuffer-p)
                    (frame-parent)
                    (seq-find 'derived-mode-p '(magit-status-mode comint-mode vterm-mode))))))
      (recenter)
      (pulse-line-hook-function)))
  (dolist (command '(next-window-any-frame
                     scroll-up-command
                     scroll-down-command
                     revert-buffer
                     recenter-top-bottom
                     avy-process
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
  ("M-r" . symbol-overlay-rename)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-feature hl-line
  :hook
  (after-init . global-hl-line-mode))

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
  :hook
  (after-init . moody-replace-mode-line-front-space)
  (after-init . moody-replace-mode-line-buffer-identification))

(use-package minions
  :hook
  (after-init . minions-mode))

(use-package hide-mode-line
  :hook
  ((dired-mode help-mode magit-mode magit-popup-mode org-capture-mode tab-switcher-mode) . hide-mode-line-mode))

(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  :hook
  (after-init . show-paren-mode))

(use-feature fringe
  :config
  (fringe-mode (frame-char-height)))

(use-package apropospriate-theme)

(use-package auto-dark
  :custom
  (auto-dark-dark-theme 'apropospriate-dark)
  (auto-dark-light-theme 'apropospriate-light)
  :hook
  (after-init . auto-dark-mode))

(use-package popper
  :bind
  ("C-`"   . popper-toggle)
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
  :hook
  (after-init . popper-mode)
  (after-init . popper-echo-mode))

;;;;; Bindings & Chords

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
  (defun insert-arrow ()
    (interactive)
    (ensure-space :before)
    (insert "->")
    (ensure-space :after))
  (defun insert-fat-arrow ()
    (interactive)
    (ensure-space :before)
    (insert "=>")
    (ensure-space :after))
  (defun pad-pipes ()
    (interactive)
    (ensure-space :before)
    (insert "||")
    (backward-char))
  (defun pad-brackets ()
    (interactive)
    (unless (looking-back (rx (or "(" "[")) nil)
      (ensure-space :before))
    (insert "{  }")
    (backward-char 2))
  :hook
  (after-init . key-chord-mode))

(use-feature which-key
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-100)
  :hook
  (after-init . which-key-mode))

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

(defun self-with-space ()
  (interactive)
  (call-interactively #'self-insert-command)
  (ensure-space :after))

(defun pad-equals ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (call-interactively #'self-insert-command)
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>+~]" nil)
           (ensure-space :before)))
    (self-with-space)))

(defun open-brackets-newline-and-indent ()
  (interactive)
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
