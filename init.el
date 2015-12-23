;;; hemacs --- an emacs configuration -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

;;;;; Source Variables

(setq load-prefer-newer t
      history-length 256
      history-delete-duplicates t
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      echo-keystrokes 0.02
      ns-use-native-fullscreen nil
      ns-use-srgb-colorspace t
      delete-by-moving-to-trash t
      ring-bell-function #'ignore
      gc-cons-threshold (* 1024 1024 32)
      ns-function-modifier 'hyper
      ns-right-option-modifier 'none
      create-lockfiles nil)

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              truncate-lines t)

;;;;; Personal Variables & Helper Macros

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode slim-mode inf-ruby-mode))
(defvar writing-modes
  '(org-mode markdown-mode fountain-mode git-commit-mode))
(defvar monospace-font "Fira Code")

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro add-λ (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro hook-modes (modes &rest body)
  (declare (indent 1) (debug t))
  `(dolist (mode ,modes)
     (add-λ (intern (format "%s-hook" mode)) ,@body)))

;;;;; Package Management

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(def upgrade-packages
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))

(use-package use-package-chords
  :ensure t)

;;;;; Bootstrap

(defun ensure-space ()
  (unless (looking-back " " nil)
    (insert " ")))

(def insert-arrow
  (ensure-space)
  (insert "-> "))

(def insert-fat-arrow
  (ensure-space)
  (insert "=> "))

(def pad-comma
  (call-interactively #'self-insert-command)
  (ensure-space))

(def pad-equals
  (if (nth 3 (syntax-ppss))
      (insert "=")
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>]" nil)
           (ensure-space)))
    (call-interactively #'self-insert-command)
    (ensure-space)))

(def pad-pipes
  (ensure-space)
  (insert "||")
  (backward-char))

(def open-brackets-newline-and-indent
  (ensure-space)
  (insert "{\n\n}")
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(def pad-brackets
  (unless (looking-back (rx (or "(" "[")) nil)
    (ensure-space))
  (insert "{  }")
  (backward-char 2))

(def apostrophe
  (if (eq 0 (car (syntax-ppss)))
      (insert "’")
    (call-interactively #'self-insert-command)))

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package s
  :ensure t)

(use-package thingatpt
  :config
  (defmacro make-transform-symbol-at-point-defun (func)
    `(function
      (lambda ()
        (interactive)
        (save-excursion
          (let ((symbol (thing-at-point 'symbol t))
                (bounds (bounds-of-thing-at-point 'symbol)))
            (delete-region (car bounds) (cdr bounds))
            (insert (funcall ,func symbol))))))))

(use-package tool-bar
  :defer t
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :config (scroll-bar-mode -1))

(use-package menu-bar
  :bind ("s-w" . kill-this-buffer))

(use-package mwheel
  :defer t
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(use-package novice
  :defer t
  :config (setq disabled-command-function nil))

(use-package advice
  :defer t
  :config (setq ad-redefinition-action 'accept))

(use-package cus-edit
  :defer t
  :init (setq custom-file (make-temp-file "emacs-custom")))

(use-package startup
  :defer t
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        inhibit-startup-echo-area-message ""))

(use-package subr
  :preface (provide 'subr)
  :init (defalias 'yes-or-no-p #'y-or-n-p))

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t)
  (setq-default comint-process-echoes t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (defun process-shellish-output ()
    (setq truncate-lines nil)
    (text-scale-decrease 1))
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-hook 'comint-mode-hook #'process-shellish-output)
  (defun improve-npm-process-output (output)
    (replace-regexp-in-string "\\[[0-9]+[GK]" "" output))
  (add-to-list 'comint-preoutput-filter-functions #'improve-npm-process-output)
  (bind-keys :map comint-mode-map
             ("s-k"       . comint-clear-buffer)
             ("M-TAB"     . comint-previous-matching-input-from-input)
             ("<M-S-tab>" . comint-next-matching-input-from-input))
  (add-λ 'kill-emacs-hook
    (--each (buffer-list)
      (with-current-buffer it (comint-write-input-ring)))))

(use-package compile
  :defer t
  :config
  (setq compilation-disable-input t
        compilation-always-kill t)
  (add-hook 'compilation-mode-hook #'process-shellish-output)
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package warnings
  :config
  (setq warning-suppress-types '((undo discard-info))))

(use-package shell
  :defer t
  :config
  (setq async-shell-command-buffer 'new-buffer
        shell-command-switch "-ic"
        explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package sh-script
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode))
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(use-package executable
  :defer t
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

;;;;; Files & History

(use-package image-mode
  :defer t
  :config
  (defun show-image-dimensions-in-mode-line ()
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))
  (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package files
  :defer t
  :bind (("s-," . find-user-init-file-other-window)
         ("s-K" . delete-file-and-buffer)
         ("s-S" . rename-file-and-buffer))
  :chords (";f" . find-file)
  :config
  (def delete-file-and-buffer
    (let ((filename (buffer-file-name)))
      (when filename
        (system-move-file-to-trash filename))
      (kill-buffer)))
  (def rename-file-and-buffer
    (let* ((filename (buffer-file-name))
           (old-name (if filename
                         (file-name-nondirectory filename)
                       (buffer-name)))
           (new-name (read-file-name "New name: " nil nil nil old-name)))
      (cond
       ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
       (:else
        (rename-file filename new-name :force-overwrite)
        (set-visited-file-name new-name :no-query :along-with-file)))))
  (def find-user-init-file-other-window
    (find-file-other-window user-init-file))
  (defun hemacs-save-hook ()
    (unless (member major-mode '(markdown-mode gfm-mode sql-mode))
      (delete-trailing-whitespace))
    (when (region-active-p)
      (deactivate-mark t)))
  (defun find-file-maybe-make-directories (filename &optional wildcards)
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir :make-parents)))))
  (defun save-buffers-kill-emacs-no-process-query (orig-fun &rest args)
    (cl-letf (((symbol-function 'process-list) #'ignore))
      (apply orig-fun args)))
  (add-hook 'before-save-hook #'hemacs-save-hook)
  (advice-add 'find-file :before #'find-file-maybe-make-directories)
  (advice-add 'save-buffers-kill-emacs :around #'save-buffers-kill-emacs-no-process-query)
  (setq require-final-newline t
        confirm-kill-emacs nil
        confirm-nonexistent-file-or-buffer nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package autorevert
  :config
  (global-auto-revert-mode)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package savehist
  :config
  (savehist-mode)
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring comint-input-ring)
        savehist-autosave-interval 30))

(use-package saveplace
  :init (save-place-mode))

(use-package recentf
  :chords (":S" . recentf-find-file)
  :config
  (def recentf-find-file
    (let ((file (completing-read "Choose recent file: "
                                 (-map 'abbreviate-file-name recentf-list)
                                 nil t)))
      (when file
        (find-file file))))
  (recentf-mode)
  (setq recentf-exclude '(".ido.last")
        recentf-max-saved-items 1000))

(use-package dired
  :defer t
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-use-ls-dired nil
        dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package dired-x
  :after dired)

(use-package ranger
  :ensure t
  :bind ("s-\\" . ranger)
  :init
  (setq ranger-cleanup-on-disable t
        ranger-show-dotfiles t))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package midnight
  :config
  (midnight-mode)
  (add-hook 'midnight-hook #'recentf-save-list)
  (setq midnight-period 10000))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :ensure t
  :bind ("C-c f" . reveal-in-osx-finder))

;;;;; Editing

(use-package newcomment
  :bind ("s-/" . comment-or-uncomment-region)
  :config
  (defun with-region-or-line (beg end &optional _)
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))
  (advice-add 'comment-or-uncomment-region :before #'with-region-or-line))

(use-package simple
  :bind ("s-k" . kill-whole-line)
  :config
  (hook-modes writing-modes
    (auto-fill-mode)
    (visual-line-mode))
  (defun maybe-indent-afterwards (&optional _)
    (and (not current-prefix-arg)
         (not (member major-mode indent-sensitive-modes))
         (or (-any? #'derived-mode-p progish-modes))
         (let ((mark-even-if-inactive transient-mark-mode))
           (indent-region (region-beginning) (region-end) nil))))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (defun kill-line-or-join-line (orig-fun &rest args)
    (if (not (eolp))
        (apply orig-fun args)
      (forward-line)
      (join-line)))
  (defun move-beginning-of-line-or-indentation (orig-fun &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply orig-fun args))))
  (defun backward-delete-subword (orig-fun &rest args)
    (cl-letf (((symbol-function 'kill-region) #'delete-region))
      (apply orig-fun args)))
  (defun with-region-or-point-to-eol (beg end &optional _)
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (point) (line-end-position)))))
  (advice-add 'kill-ring-save :before #'with-region-or-point-to-eol)
  (advice-add 'yank :after #'maybe-indent-afterwards)
  (advice-add 'yank-pop :after #'maybe-indent-afterwards)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'backward-kill-word :around #'backward-delete-subword)
  (advice-add 'kill-whole-line :after #'back-to-indentation)
  (advice-add 'kill-line :around #'kill-line-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (bind-keys
   :map minibuffer-local-map
   ("<escape>"  . abort-recursive-edit)
   ("M-TAB"     . previous-complete-history-element)
   ("<M-S-tab>" . next-complete-history-element)))

(use-package indent
  :defer t
  :init
  (setq standard-indent 2)
  (defun with-region-or-buffer (beg end &optional _)
    (interactive
     (if mark-active
         (list (region-beginning) (region-end))
       (list (point-min) (point-max)))))
  (advice-add 'indent-region :before #'with-region-or-buffer))

(use-package delsel
  :init (delete-selection-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package subword
  :init (global-subword-mode))

(use-package expand-region
  :ensure t
  :commands (er/mark-symbol)
  :bind* ("C-," . er/expand-region))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-;" . ace-jump-mode)
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode))
  :config
  (ace-jump-mode-enable-mark-sync)
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

(use-package ace-jump-zap
  :ensure t
  :chords ("jz" . ace-jump-zap-up-to-char))

(use-package ace-window
  :ensure t
  :bind (([remap next-multiframe-window] . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package smart-newline
  :ensure t
  :bind ("<s-return>" . eol-then-smart-newline)
  :init
  (defun smart-newline-no-reindent-first (orig-fun &rest args)
    (cl-letf (((symbol-function 'reindent-then-newline-and-indent) #'newline-and-indent))
      (apply orig-fun args)))
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))
    (advice-add 'smart-newline :around #'smart-newline-no-reindent-first))
  (def eol-then-smart-newline
    (move-end-of-line nil)
    (smart-newline)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package evil-numbers
  :ensure t
  :bind (("<M-up>"   . evil-numbers/inc-at-pt)
         ("<M-down>" . evil-numbers/dec-at-pt)))

(use-package multiple-cursors
  :ensure t
  :bind (("s-d"     . mc/mark-next-like-this)
         ("s-D"     . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim))
  :config
  (add-hook 'before-save-hook #'mc/keyboard-quit))

(use-package toggle-quotes
  :ensure t
  :bind ("C-'" . toggle-quotes))

(use-package scratch
  :ensure t
  :bind ("s-N" . scratch))

(use-package flyspell
  :config
  (hook-modes writing-modes
    (flyspell-mode)))

(use-package smart-shift
  :ensure t
  :bind (("s-[" . smart-shift-left)
         ("s-]" . smart-shift-right))
  :config
  (advice-add 'smart-shift-override-local-map :override #'ignore))

(use-package move-dup
  :ensure t
  :bind (("<C-s-down>" . md/move-lines-down)
         ("<C-s-up>"   . md/move-lines-up)
         ("s-D"        . md/duplicate-down))
  :config
  (defun indent-unless-sensitive (_arg)
    (unless (member major-mode indent-sensitive-modes)
      (indent-according-to-mode)))
  (advice-add 'md/move-line-or-region :after #'indent-unless-sensitive))

;;;;; Completion

(use-package ido
  :config
  (ido-mode)
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always)
  (bind-keys :map ido-completion-map
             ("M-TAB"     . previous-history-element)
             ("<M-S-tab>" . next-history-element)))

(use-package flx-ido
  :ensure t
  :after ido
  :config
  (flx-ido-mode)
  (setq flx-ido-use-faces nil))

(use-package ido-ubiquitous
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :config
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package ido-exit-target
  :ensure t
  :after ido
  :config
  (bind-key "<s-return>" #'ido-exit-target-other-window ido-common-completion-map))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((hippie-expand-only-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun try-expand-dabbrev-other-buffers (old)
    (let ((hippie-expand-ignore-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun hippie-expand-case-sensitive (orig-fun &rest args)
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  (defun hippie-expand-inhibit-message-in-minibuffer (orig-fun &rest args)
    (let ((inhibit-message (minibufferp)))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'hippie-expand-inhibit-message-in-minibuffer)
  (bind-key "TAB" #'hippie-expand read-expression-map)
  (bind-key "TAB" #'hippie-expand minibuffer-local-map)
  (bind-key* "M-?" (make-hippie-expand-function '(try-expand-line-all-buffers)))
  (setq hippie-expand-verbose nil
        hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                           try-expand-dabbrev
                                           try-expand-dabbrev-matching-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev-other-buffers))
  (hook-modes lispy-modes
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  (setq-default yas/prompt-functions '(yas/ido-prompt))
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))

(use-package company
  :ensure t
  :bind ("s-y" . company-kill-ring)
  :config
  (def company-kill-ring
    (company-begin-with
     (mapcar #'substring-no-properties kill-ring))
    (company-filter-candidates))
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-require-match nil
        company-minimum-prefix-length 2
        company-show-numbers t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands '(comint-previous-matching-input-from-input
                                            comint-next-matching-input-from-input))))

(use-package company-dabbrev
  :after company
  :config
  (setq company-dabbrev-minimum-length 2))

(use-package company-dabbrev-code
  :after company
  :config
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t))

(use-package company-emoji
  :ensure t
  :after company
  :config
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (hook-modes writing-modes
    (setq-local company-backends (append '(company-emoji) company-backends))))

(use-package readline-complete
  :ensure t
  :after company
  :config
  (add-λ 'comint-mode-hook
    (setq-local company-backends (append '(company-readline) company-backends))))

(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode)
  (setq smart-tab-using-hippie-expand t
        smart-tab-completion-functions-alist '()))

;;;;; Navigation & Search

(use-package ffap
  :chords ("fp" . ffap))

(use-package window
  :preface (provide 'window)
  :chords ((";s" . switch-to-buffer)
           (";w" . toggle-split-window)
           (":W" . delete-other-windows)
           (":Q" . delete-side-windows))
  :config
  (def toggle-split-window
    (if (eq last-command 'toggle-split-window)
        (progn
          (jump-to-register :toggle-split-window)
          (setq this-command 'toggle-unsplit-window))
      (window-configuration-to-register :toggle-split-window)
      (switch-to-buffer-other-window nil)))
  (defun delete-side-windows ()
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))
  (setq display-buffer-alist
        `((,(rx bos (or "*Flycheck errors*"
                        "*Backtrace"
                        "*Warnings"
                        "*compilation"
                        "*Help"
                        "*less-css-compilation"
                        "*Packages"
                        "*rspec-compilation"
                        "*SQL"
                        "*ag"))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side            . bottom)
           (reusable-frames . visible)
           (window-height   . 0.33))
          ("." nil (reusable-frames . visible)))))

(use-package wgrep-ag
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package ag
  :ensure t
  :chords ((";g" . ag-project)
           (":G" . ag))
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t))

(use-package anzu
  :ensure t
  :bind (([remap query-replace] . anzu-query-replace)
         ("s-q" . anzu-query-replace))
  :config
  (global-anzu-mode))

(use-package imenu
  :config
  (defun hemacs-imenu-elisp-expressions ()
    (--each '(("packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
              (nil "^(def \\(.+\\)$" 1)
              ("sections" "^;;;;; \\(.+\\)$" 1))
      (add-to-list 'imenu-generic-expression it)))
  (add-hook 'emacs-lisp-mode-hook #'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t))

(use-package imenu-anywhere
  :ensure t
  :chords (";r" . imenu-anywhere))

(use-package ace-jump-buffer
  :ensure t
  :chords ((";a" . ace-jump-buffer)
           (":A" . ace-jump-buffer-other-window)
           (";x" . ace-jump-shellish-buffers))
  :config
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode))))
  (setq ajb-home-row-keys t))

(use-package projectile
  :ensure t
  :bind ("s-p" . projectile-command-map)
  :chords (";t" . projectile-find-file)
  :config
  (setq projectile-enable-caching t)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (defmacro make-projectile-switch-project-defun (func)
    `(function
      (lambda ()
        (interactive)
        (let ((projectile-switch-project-action ,func))
          (projectile-switch-project)))))
  (defun projectile-relevant-known-git-projects ()
    (mapcar
     (lambda (dir)
       (substring dir 0 -1))
     (cl-remove-if-not
      (lambda (project)
        (unless (file-remote-p project)
          (file-directory-p (concat project "/.git/"))))
      (projectile-relevant-known-projects))))
  (projectile-global-mode)
  (projectile-cleanup-known-projects))

(use-package projectile-rails
  :ensure t
  :after (ruby-mode projectile)
  :config
  (add-λ 'ruby-mode-hook
    (setq-local projectile-tags-command "ripper-tags -R -f TAGS"))
  (add-hook 'projectile-mode-hook #'projectile-rails-on))

(use-package projector
  :ensure t
  :after projectile
  :bind* (("C-x RET"        . projector-run-shell-command-project-root)
          ("C-x m"          . projector-switch-to-or-create-project-shell)
          ("C-x <C-return>" . projector-run-default-shell-command))
  :config
  (bind-key "s-R" #'projector-rerun-buffer-process comint-mode-map)
  (setq projector-always-background-regex
        '("^powder restart\\.*"
          "^heroku restart\\.*"
          "^heroku addons:open\\.*"
          "^spring stop"
          "^gulp publish\\.*"
          "^git push\\.*"
          "^pkill\\.*")
        projector-command-modes-alist
        '(("^heroku run console" . inf-ruby-mode))))

(use-package swiper
  :ensure t
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper)))

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("s-P" . smex))
  :config
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

;;;;; External Utilities

(use-package edit-server
  :ensure t
  :config
  (edit-server-start)
  (setq edit-server-new-frame nil
        edit-server-default-major-mode 'gfm-mode))

(use-package crab
  :ensure t
  :defer 2
  :bind (("s-R" . crab-reload)
         ("<S-s-left>" . crab-prev-tab)
         ("<S-s-right>" . crab-next-tab))
  :config
  (crab-server-start))

;;;;; Major Modes

(use-package org
  :defer t
  :config
  (setq org-support-shift-select t
        org-completion-use-ido t
        org-startup-indented t)
  (bind-key "," #'pad-comma org-mode-map)
  (bind-key "'" #'apostrophe org-mode-map))

(use-package org-autolist
  :ensure t
  :after org
  :config (add-hook 'org-mode-hook #'org-autolist-mode))

(use-package org-repo-todo
  :ensure t
  :after org
  :bind (("s-`" . ort/goto-todos)
         ("s-n" . ort/capture-checkitem)))

(use-package sgml-mode
  :mode (("\\.hbs\\'"        . html-mode)
         ("\\.handlebars\\'" . html-mode))
  :chords ("<>" . sgml-close-tag)
  :config
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (add-hook 'sgml-mode #'sgml-electric-tag-pair-mode)
  (def html-newline-dwim
    (move-end-of-line nil)
    (reindent-then-newline-and-indent)
    (sgml-close-tag)
    (move-beginning-of-line nil)
    (open-line 1)
    (indent-according-to-mode))
  (bind-keys :map html-mode-map
             ("," . pad-comma)
             ("'" . apostrophe)
             ("<C-return>" . html-newline-dwim)))

(use-package handlebars-sgml-mode
  :ensure t
  :after sgml-mode
  :config (handlebars-use-mode 'global))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.php\\'" . web-mode)))

(use-package fountain-mode
  :ensure t
  :mode "\\.fountain$")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (bind-key "'" #'apostrophe markdown-mode-map)
  (bind-key "," #'pad-comma markdown-mode-map)
  (setq markdown-command "marked"
        markdown-indent-on-enter nil))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  :init
  :config
  (def smart-css-colon
    (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (cond ((string-match "^\\(?:[^[:blank:]]+\\|[[:blank:]]+[[:word:]]*[#&.@,]+\\)" current-line)
             (insert ":"))
            ((looking-at "\;.*")
             (insert ": "))
            (:else
             (insert ": ;")
             (backward-char)))))
  (defun set-css-imenu-generic-expression ()
    (setq imenu-generic-expression '((nil "^\\([^\s-].*+\\(?:,\n.*\\)*\\)\\s-{$" 1))))
  (add-hook 'css-mode-hook #'set-css-imenu-generic-expression)
  (setq css-indent-offset 2)
  (bind-keys :map css-mode-map
             (":" . smart-css-colon)
             ("," . pad-comma)
             ("{" . open-brackets-newline-and-indent)))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\.erb\\'"
  :init (setq less-css-lessc-options '("--no-color" "-x")))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.es6$"  . js2-mode))
  :interpreter (("node" . js2-mode))
  :config
  (def smart-js-colon
    (insert ":")
    (ensure-space)
    (insert ",")
    (backward-char))
  (def js-insert-console-log
    (insert "console.log()")
    (backward-char))
  (setq js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-basic-offset 2)
  (bind-keys :map js2-mode-map
             ("," . pad-comma)
             ("=" . pad-equals)
             (":" . smart-js-colon))
  (bind-chord "qq" #'js-insert-console-log js2-mode-map)
  (setq-default js2-global-externs
                '("clearTimeout" "setTimeout" "module" "require" "_")))

(use-package json-mode
  :ensure t
  :mode (("\\.bowerrc$"     . js2-mode)
         ("\\.json_schema$" . js2-mode)))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\.*"
  :config
  (def coffee-newline-dwim
    (move-end-of-line nil)
    (ensure-space)
    (insert "=> ")
    (coffee-newline-and-indent))
  (setq coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header")
  (bind-keys :map coffee-mode-map
             (","          . pad-comma)
             ("="          . pad-equals)
             ("<C-return>" . coffee-newline-dwim)
             ("C-c C-c"    . coffee-compile-region)))

(use-package ember-mode
  :ensure t)

(use-package web-beautify
  :ensure t
  :config
  (with-eval-after-load 'js2-mode
    (bind-key "s-b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "s-b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "s-b" #'web-beautify-css css-mode-map)))

(use-package slim-mode
  :ensure t
  :config
  (def slim-newline-dwim
    (move-end-of-line nil)
    (newline-and-indent))
  (setq slim-backspace-backdents-nesting nil)
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= "."))
  (bind-keys :map slim-mode-map
             (","          . pad-comma)
             (":"          . smart-ruby-colon)
             ("<C-return>" . slim-newline-dwim)))

(use-package ruby-mode
  :mode
  (("Appraisals$"   . ruby-mode)
   ("\\.rabl\\'"    . ruby-mode)
   ("\\.builder\\'" . ruby-mode))
  :init
  (def smart-ruby-colon
    (if (looking-back "[[:word:]]" nil)
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
  (bind-keys :map ruby-mode-map
             (","          . pad-comma)
             ("="          . pad-equals)
             (":"          . smart-ruby-colon)
             ("<C-return>" . ruby-newline-dwim))
  (defun hippie-expand-ruby-symbols (orig-fun &rest args)
    (let ((table (make-syntax-table ruby-mode-syntax-table)))
      (modify-syntax-entry ?: "." table)
      (with-syntax-table table (apply orig-fun args))))
  (add-λ 'ruby-mode-hook
    (add-function :around (local 'hippie-expand) #'hippie-expand-ruby-symbols)))

(use-package ruby-tools
  :ensure t
  :after ruby-mode)

(use-package rspec-mode
  :ensure t
  :after ruby-mode)

(use-package inf-ruby
  :ensure t
  :config
  (add-λ 'inf-ruby-mode-hook
    (turn-on-comint-history ".pry_history"))
  (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
  (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map))

(use-package bundler
  :ensure t
  :after projectile-rails
  :config
  (bind-key "G" #'bundle-open projectile-rails-command-map))

(use-package chruby
  :ensure t
  :after projectile-rails
  :init
  (bind-key "V" #'chruby-use-corresponding projectile-rails-command-map)
  (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding)
  (advice-add 'projectile-rails-console :before #'chruby-use-corresponding)
  (advice-add 'projectile-rails-server :before #'chruby-use-corresponding))

(use-package ruby-hash-syntax
  :ensure t
  :after ruby-mode
  :init
  (bind-key "C-c C-:" #'ruby-toggle-hash-syntax ruby-mode-map))

(use-package yaml-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package text-mode
  :preface (provide 'text-mode)
  :init
  (bind-key "," #'pad-comma text-mode-map))

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :ensure t
  :bind ("s-m" . magit-status)
  :after alert
  :config
  (def magit-just-amend
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))
  (bind-key "C-c C-a" #'magit-just-amend magit-mode-map)
  (defun magit-process-alert-after-finish-in-background (orig-fun &rest args)
    (let* ((process (nth 0 args))
           (event (nth 1 args))
           (buf (process-get process 'command-buf))
           (buff-name (buffer-name buf)))
      (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
        (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
      (apply orig-fun (list process event))))
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)
  (add-hook 'magit-process-mode-hook #'process-shellish-output)
  (setq git-commit-summary-max-length git-commit-fill-column
        magit-revert-buffers 2
        magit-completing-read-function 'magit-ido-completing-read
        magit-log-auto-more t
        magit-repository-directories (funcall #'projectile-relevant-known-git-projects)
        magit-no-confirm t))

(use-package magit-gh-pulls
  :ensure t
  :after magit
  :config (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-messenger
  :ensure t
  :config (setq git-messenger:show-detail t))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

;;;;; Help & Docs

(use-package google-this
  :ensure t)

(use-package find-func
  :config
  (def open-package
    (let* ((packages (mapcar 'symbol-name (mapcar 'car package-alist)))
           (package (completing-read "Open package: " packages nil t)))
      (find-library package)))
  (find-function-setup-keys))

(use-package etags
  :init (setq tags-revert-without-query t))

(use-package elisp-slime-nav
  :ensure t
  :config
  (hook-modes lispy-modes
    (elisp-slime-nav-mode)))

(use-package github-browse-file
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package gist
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package dash-at-point
  :load-path "lib/dash-at-point/")

(use-package discover
  :ensure t
  :config (global-discover-mode))

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint)
                flycheck-idle-change-delay 1
                flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Appearance

(use-package ns-win
  :defer t
  :config (setq ns-pop-up-frames nil))

(use-package frame
  :defer t
  :config
  (setq blink-cursor-blinks 0)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
  (when (member monospace-font (font-family-list))
    (set-frame-font (concat monospace-font "-15"))))

(use-package prog-mode
  :defer t
  :init (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config (global-prettify-symbols-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode)
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 4
        beacon-dont-blink-commands '(next-line previous-line forward-line)))

(use-package highlight-symbol
  :ensure t
  :config
  (hook-modes progish-modes
    (highlight-symbol-mode)
    (highlight-symbol-nav-mode))
  (setq highlight-symbol-idle-delay 0
        highlight-symbol-highlight-single-occurrence nil))

(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (hook-modes progish-modes
    (rainbow-delimiters-mode)))

(use-package powerline
  :load-path "lib/powerline/"
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'utf-8))

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package alert
  :ensure t
  :config
  (defun alert-after-finish-in-background (buf str)
    (unless (get-buffer-window buf 'visible)
      (alert str :buffer buf)))
  (setq alert-default-style 'notifier))

(use-package auto-dim-other-buffers
  :ensure t
  :config (auto-dim-other-buffers-mode))

(use-package centered-cursor-mode
  :ensure t
  :config
  (hook-modes progish-modes
    (centered-cursor-mode)))

(use-package fringe
  :defer t
  :config (fringe-mode '(20 . 8)))

(use-package custom
  :defer t
  :config (setq custom-safe-themes t))

(use-package apropospriate-theme
  :ensure t
  :init
  (defun what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))))

(use-package highlight-tail
  :ensure t
  :config
  (setq highlight-tail-steps 16)
  (defun highlight-tail-reload-when-idle (&optional _no-confirm _no-enable)
    (run-with-idle-timer 1 nil #'highlight-tail-reload))
  (highlight-tail-mode))

(use-package cycle-themes
  :ensure t
  :config
  (setq cycle-themes-theme-list '(apropospriate-dark apropospriate-light))
  (add-hook 'cycle-themes-after-cycle-hook #'highlight-tail-reload-when-idle)
  (cycle-themes-mode))

;;;;; Bindings & Chords

(use-package key-chord
  :defer t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "^^" "λ")
  (key-chord-define-global "::" "::")
  (setq key-chord-two-keys-delay 0.05)
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil)))

(use-package free-keys
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(bind-chords
 ("}|" . pad-pipes)
 ("{}" . open-brackets-newline-and-indent)
 ("[]" . pad-brackets)
 ("_+" . insert-fat-arrow)
 ("-=" . insert-arrow))

(bind-keys
 :prefix-map hemacs-help-map
 :prefix "s-h"
 ("k" . describe-personal-keybindings)
 ("K" . free-keys)
 ("f" . what-face)
 ("m" . discover-my-major)
 ("g" . google-this)
 ("d" . dash-at-point)
 ("D" . dash-at-point-with-docset)
 ("i" . insert-local-ip-address)
 ("o" . open-package))

(bind-keys
 :prefix-map switch-project-map
 :prefix "s-o"
 ("t"          . projectile-switch-project)
 ("M"          . projector-switch-project-run-shell-command)
 ("m"          . projector-open-project-shell)
 ("<C-return>" . projector-switch-project-run-default-shell-command))

(bind-key "g" (make-projectile-switch-project-defun #'projectile-vc) switch-project-map)
(bind-key "u" (make-projectile-switch-project-defun #'projectile-run-project) switch-project-map)
(bind-key "`" (make-projectile-switch-project-defun #'ort/goto-todos) switch-project-map)
(bind-key "n" (make-projectile-switch-project-defun #'ort/capture-checkitem) switch-project-map)

(bind-keys
 :prefix-map symbol-at-point-map
 :prefix "s-;"
 ("d" . mc/mark-all-symbols-like-this)
 ("q" . anzu-query-replace-at-cursor))

(bind-key "c" (make-transform-symbol-at-point-defun #'s-lower-camel-case) symbol-at-point-map)
(bind-key "C" (make-transform-symbol-at-point-defun #'s-upper-camel-case) symbol-at-point-map)
(bind-key "_" (make-transform-symbol-at-point-defun #'s-snake-case) symbol-at-point-map)
(bind-key "-" (make-transform-symbol-at-point-defun #'s-dashed-words) symbol-at-point-map)

(bind-keys
 :prefix-map hemacs-git-map
 :prefix "s-g"
 ("o" . github-browse-file)
 ("b" . github-browse-file-blame)
 ("c" . github-browse-commit)
 ("l" . magit-clone)
 ("g" . gist-region-or-buffer-private)
 ("t" . git-timemachine)
 ("p" . git-messenger:popup-message))
