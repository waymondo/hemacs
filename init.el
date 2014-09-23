(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(require 'use-package)
(use-package s)
(use-package noflet)
(use-package dash :config (dash-enable-font-lock))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :height 150 :font "Meslo LG M DZ for Powerline")

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defalias 'yes-or-no-p 'y-or-n-p)

(load (locate-user-emacs-file "defuns.el"))

(setq debug-on-error t
      history-length 100
      history-delete-duplicates t
      scroll-margin 24
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      echo-keystrokes 0.1
      inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message ""
      ns-use-native-fullscreen nil
      ns-use-srgb-colorspace t
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      vc-follow-symlinks t
      gc-cons-threshold 50000000
      byte-compile-warnings '(not obsolete)
      disabled-command-function nil
      create-lockfiles nil
      kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              indicate-empty-lines t
              left-fringe-width 10
              right-fringe-width 5)

(setq split-height-threshold nil
      split-width-threshold 0
      pop-up-windows nil
      display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-pop-up-frame)))

(with-region-or-line comment-or-uncomment-region)
(with-region-or-line upcase-region)
(with-region-or-line capitalize-region)
(with-region-or-line downcase-region)
(with-region-or-line yank-region)
(with-region-or-line kill-region t)
(with-region-or-line kill-ring-save t)
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(define-prefix-command 'hemacs-map)
(define-prefix-command 'hemacs-projectile-map)
(define-prefix-command 'hemacs-crab-map)
(bind-key "C-z" 'hemacs-map)
(bind-key "s-o" 'hemacs-projectile-map)
(bind-key "C-z c" 'hemacs-crab-map)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (exec-path-from-shell-initialize)
    (--each '("HISTFILE" "NODE_PATH" "SSL_CERT_FILE")
      (exec-path-from-shell-copy-env it))))

(use-package ns-win
  :config
  (setq mac-function-modifier 'hyper
        ns-pop-up-frames nil
        mac-right-option-modifier 'none))

(use-package minibuffer
  :init
  (use-package minibuf-eldef
    :init (minibuffer-electric-default-mode)
    :config (setq minibuffer-eldef-shorten-default t))
  :config
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'face-remapping-alist)
         '((default :height 0.9)))))

(use-package prog-mode
  :init (global-prettify-symbols-mode))

(use-package image-mode
  :init (add-hook 'image-mode-hook 'show-image-dimensions-in-mode-line))

(use-package files
  :config
  (progn
    (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate compile)
      (noflet ((process-list ())) ad-do-it))
    (setq require-final-newline t
          confirm-kill-emacs nil
          confirm-nonexistent-file-or-buffer nil
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
    (add-hook 'before-save-hook 'hemacs-save-hook)
    (add-hook 'after-save-hook 'byte-compile-current-buffer)
    (add-hook 'find-file-hook 'sm-try-smerge t)))

(use-package delsel
  :init (delete-selection-mode))

(use-package comint
  :init
  (progn
    (bind-key "s-k" 'clear-shell comint-mode-map)
    (setq comint-process-echoes t)
    (setq-default comint-prompt-read-only t)
    (setq-default comint-input-ignoredups t)
    (setq comint-buffer-maximum-size 5000)
    (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
    (add-to-list 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (add-hook 'comint-mode-hook 'hemacs-shellish-hook)))

(use-package compile
  :init
  (progn
    (setq compilation-disable-input t)
    (setq compilation-message-face nil)
    (setq compilation-always-kill t)
    (add-hook 'compilation-mode-hook 'hemacs-shellish-hook)))

(use-package shell
  :init
  (progn
    (setq async-shell-command-buffer 'new-buffer)
    (setq shell-command-switch (purecopy "-ic"))
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
    (add-λ 'shell-mode-hook
      (turn-on-comint-history (getenv "HISTFILE")))))

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package imenu
  :init
  (use-package imenu-anywhere
    :bind ("s-r" . imenu-anywhere)
    :config (add-hook 'imenu-after-jump-hook #'pulse-line-hook-function))
  :config (setq imenu-auto-rescan t))

(use-package savehist
  :init (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring comint-input-ring)
        savehist-autosave-interval 30))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-exclude '(".ido.last" "COMMIT_EDITMSG")
        initial-buffer-choice (car recentf-list)
        recentf-max-saved-items 500))

(use-package paren
  :init (show-paren-mode))

(use-package elec-pair
  :init (electric-pair-mode)
  :config
  (setq electric-pair-pairs '
        ((?\( . ?\))
         (?\" . ?\")
         (?\{ . ?\})
         (?\[ . ?\]))
        electric-pair-text-pairs '
        ((?\" . ?\")
         (?\` . ?\`))))

(use-package subword
  :bind* (("<M-left>" . subword-left)
          ("<M-right>" . subword-right))
  :init (global-subword-mode)
  :config
  (define-key subword-mode-map [remap backward-kill-word] 'subword-backward-delete))

(use-package pulse
  :config
  (progn
    (setq pulse-command-advice-flag t)
    (setq pulse-delay 0)
    (setq pulse-iterations 8)
    (--each '(next-error-hook focus-in-hook)
      (add-hook it #'pulse-line-hook-function))))

(use-package hippie-exp
  :init
  (progn
    (defadvice hippie-expand (around hippie-expand-case-fold activate compile)
      (let ((case-fold-search nil))
        ad-do-it))
    (defalias 'he-dabbrev-beg 'hemacs-dabbrev-beg)
    (global-set-key [remap dabbrev-expand] #'hippie-expand)
    (bind-key "TAB" 'hippie-expand read-expression-map)
    (bind-key "TAB" 'hippie-expand minibuffer-local-map)
    (bind-key "M-?" (make-hippie-expand-function '(try-expand-line) t))
    (setq hippie-expand-verbose nil)
    (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                             try-expand-dabbrev
                                             try-expand-dabbrev-matching-buffers
                                             try-complete-file-name-partially
                                             try-complete-file-name
                                             try-expand-dabbrev-other-buffers))
    (hook-modes lispy-modes
      (setq-local hippie-expand-try-functions-list
                  (append '(try-complete-lisp-symbol-partially
                            try-complete-lisp-symbol)
                          hippie-expand-try-functions-list)))))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config (setq-default save-place t))

(use-package server
  :if window-system
  :init (unless (server-running-p) (server-start)))

(use-package edit-server
  :init (edit-server-start t))

(use-package zone
  :init
  (progn
    (defadvice zone (before zone-one-buffer activate compile)
      (delete-other-windows))
    (zone-when-idle 248)))

(use-package dired
  :init
  (progn
    (use-package dired-toggle
      :bind ("s-\\" . dired-toggle)
      :config (setq dired-toggle-window-size 48))
    (bind-key "C-z C-k" 'dired-do-delete dired-mode-map)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (setq dired-recursive-deletes 'always
          dired-recursive-copies 'always
          dired-auto-revert-buffer t)))

(use-package org
  :config
  (progn
    (setq org-support-shift-select t)
    (setq org-completion-use-ido t)
    (use-package org-repo-todo
      :bind (("s-n" . ort/capture-todo)
             ("s-`" . ort/goto-todos)))))

(use-package find-func
  :init (find-function-setup-keys))

(use-package eldoc
  :init
  (hook-modes lispy-modes
    (eldoc-mode)))

(use-package elisp-slime-nav
  :init
  (hook-modes lispy-modes
    (elisp-slime-nav-mode)))

(use-package smex
  :bind ("s-P" . smex)
  :init
  (progn
    (smex-initialize)
    (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
    (global-set-key [remap execute-extended-command] #'smex)))

(use-package ag
  :config
  (progn
    (setq ag-reuse-buffers t)
    (setq ag-highlight-search t)))

(use-package alert
  :config (setq alert-default-style 'notifier))

(use-package crab-mode
  :bind (("C-z c r" . crab-reload)
         ("C-z c e" . crab-eval-coffee)
         ("C-z c l" . crab-console-log)
         ("C-z c <left>" . crab-prev-tab)
         ("C-z c <right>" . crab-next-tab))
  :idle (crab-server-start))

(use-package projectile
  :bind (("s-t" . projectile-find-file)
         ("s-b" . projectile-switch-to-buffer)
         ("s-p" . projectile-commander))
  :idle (projectile-global-mode)
  :config (setq projectile-enable-caching t)
  :init
  (use-package projectile-rails
    :init (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package httprepl)

(use-package sgml-mode
  :init
  (progn
    (modify-syntax-entry ?= "." html-mode-syntax-table)
    (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
    (bind-key "<C-return>" 'html-smarter-newline html-mode-map)
    (make-beautify-defun "html")))

(use-package handlebars-mode)

(use-package fountain-mode
  :mode ("\\.fountain$" . fountain-mode)
  :config (add-hook 'fountain-mode-hook 'hemacs-writing-hook))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'hemacs-writing-hook))

(use-package css-mode
  :init
  (use-package less-css-mode
    :mode ("\\.scss$" . less-css-mode))
  :config
  (progn
    (setq css-indent-offset 2)
    (make-beautify-defun "css")))

(use-package js
  :mode ("\\.json$" . js-mode)
  :interpreter ("node" . js-mode)
  :config
  (progn
    (make-beautify-defun "js")
    (setq-default js-indent-level 2)))

(use-package coffee-mode
  :mode ("\\.coffee\\.*" . coffee-mode)
  :ensure coffee-mode
  :config
  (progn
    (setq coffee-args-repl '("-i" "--nodejs"))
    (bind-key "<C-return>" 'coffee-smarter-newline coffee-mode-map)
    (bind-key "C-c C-c" 'coffee-compile-region coffee-mode-map)))

(use-package ruby-mode
  :init
  (progn
    (bind-key "<C-return>" 'ruby-smarter-newline ruby-mode-map)
    (use-package rspec-mode)
    (use-package ruby-end
      :config (setq ruby-end-insert-newline nil))
    (use-package robe
      :init (add-hook 'ruby-mode-hook 'robe-mode))
    (use-package inf-ruby
      :init (add-λ 'inf-ruby-mode-hook
              (turn-on-comint-history "~/.irb_history")))
    (use-package slim-mode
      :config
      (progn
        (setq slim-backspace-backdents-nesting nil)
        (bind-key "C-j" 'electric-indent-just-newline slim-mode-map)
        (add-λ 'slim-mode-hook (modify-syntax-entry ?\= "."))))
    (use-package ruby-hash-syntax
      :init
      (progn
        (bind-key "C-:" 'ruby-toggle-hash-syntax ruby-mode-map)
        (bind-key "C-:" 'ruby-toggle-hash-syntax slim-mode-map)))
    (use-package rhtml-mode))
  :mode (("Procfile$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("\\.env\\.*" . ruby-mode)))

(use-package magit
  :bind ("s-m" . magit-status)
  :config
  (progn
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
    (bind-key "C-c C-p" 'magit-pull-request-for-issue-number magit-mode-map)
    (bind-key "C-z C-k" 'magit-kill-file-on-line magit-mode-map)
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          magit-completing-read-function 'magit-ido-completing-read
          magit-log-auto-more t
          magit-set-upstream-on-push t
          magit-restore-window-configuration t
          magit-save-some-buffers nil
          magit-revert-item-confirm nil
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-commit-ask-to-stage nil)
    (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'magit-process-mode-hook 'hemacs-shellish-hook)))

(use-package github-browse-file
  :bind (("C-x v o" . github-browse-file)
         ("C-x v b" . github-browse-file-blame)))

(use-package github-clone
  :bind ("C-x v c" . github-clone))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config (setq git-messenger:show-detail t))

(use-package swoop
  :bind ("C-s" . swoop)
  :config
  (progn
    (setq swoop-font-size-change: nil
          swoop-window-split-direction: 'split-window-horizontally
          swoop-pre-input-point-at-function: (λ))
    (bind-key "C-o" 'swoop-multi-from-swoop swoop-map)))

(use-package projector
  :bind* (("C-z RET" . projector-run-shell-command-project-root)
          ("C-z m" . projector-switch-to-or-create-project-shell))
  :config
  (setq projector-projects-root "~/code/"
        projector-always-background-regex
        '("^mysql.server\\.*"
          "^powder restart\\.*"
          "^heroku restart\\.*"
          "^spring stop"
          "^git push\\.*"
          "\\.*cordova run\\.*"
          "^redis-server"
          "^pkill\\.*")))

(use-package ido
  :init
  (progn
    (ido-mode)
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode))
    (use-package flx-ido
      :init (flx-ido-mode)
      :config (setq flx-ido-use-faces nil))
    (use-package ido-vertical-mode
      :init (ido-vertical-mode)
      :config (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))
  :config
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 10
        ido-enable-flex-matching t
        ido-enable-dot-prefix t
        ido-max-prospects 10
        ido-create-new-buffer 'always))

(use-package diff-hl
  :init
  (progn
    (global-diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

(use-package hungry-delete
  :init (global-hungry-delete-mode))

(use-package highlight-symbol
  :init
  (hook-modes progish-modes
    (highlight-symbol-mode)
    (highlight-symbol-nav-mode))
  :config (setq highlight-symbol-idle-delay 0))

(use-package volatile-highlights
  :init (volatile-highlights-mode))

(use-package rainbow-mode
  :init
  (--each '(css-mode-hook emacs-lisp-mode-hook)
    (add-hook it 'rainbow-mode)))

(use-package rainbow-delimiters
  :init (global-rainbow-delimiters-mode))

(use-package god-mode
  :bind ("<escape>" . god-local-mode)
  :config
  (progn
    (bind-key "i" 'kill-region-and-god-local-mode god-local-mode-map)
    (bind-key "." 'repeat god-local-mode-map)
    (add-λ 'god-mode-enabled-hook
      (setq cursor-type 'box))
    (add-λ 'god-mode-disabled-hook
      (setq cursor-type 'bar))
    (add-to-list 'god-exempt-major-modes 'git-commit-mode)))

(use-package dash-at-point
  :bind (("C-h d" . dash-at-point)
         ("C-h C-d" . dash-at-point-with-docset))
  :config
  (setq dash-at-point-docsets
        '("coffee" "lisp" "css" "less" "html" "javascript" "iphoneos" "ruby" "elisp"
          "jquery" "rails" "underscore" "backbone" "bootstrap" "markdown" "zepto"
          "angularjs" "psql" "emacs" "fa" "redis" "git" "bash" "moment")))

(use-package bind-key
  :bind ("C-h C-k" . describe-personal-keybindings))

(use-package free-keys
  :bind ("C-h C-f" . free-keys))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package guide-key
  :init (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r" "C-x" "C-c"
          "C-z" "C-c p" "C-x +" "C-c ," "C-h" "M-s" "s-o")
        guide-key/popup-window-position 'bottom))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package anzu
  :bind ("s-q" . anzu-query-replace)
  :init (global-anzu-mode))

(use-package toggle-quotes
  :bind ("C-'" . toggle-quotes))

(use-package ace-jump-mode
  :bind* ("C-;" . ace-jump-word-mode)
  :config
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

(use-package expand-region
  :bind* ("C-," . er/expand-region))

(use-package multiple-cursors
  :bind (("C-z C-." . mc/mark-next-like-this)
         ("C-z C-," . mc/mark-previous-like-this)
         ("C-z C-/" . mc/mark-all-like-this-dwim)))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package highlight-tail
  ;; :idle (highlight-tail-mode)
  :config (setq highlight-tail-timer 0.02))

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (progn
    (use-package misc)
    (use-package ace-jump-buffer)
    (use-package ace-jump-zap)
    (key-chord-define-global ",." "<>\C-b")
    (key-chord-define-global "}|" "||\C-b")
    (key-chord-define-global "<>" 'sgml-close-tag)
    (key-chord-define-global "{}" 'open-brackets-newline-and-indent)
    (key-chord-define-global "[]" 'pad-brackets)
    (key-chord-define-global "_+" 'insert-fat-arrow)
    (key-chord-define-global "-=" 'insert-arrow)
    (key-chord-define-global "^^" (λ (insert "λ")))
    (key-chord-define-global ";a" 'ace-jump-buffer)
    (key-chord-define-global ":A" 'ace-jump-buffer-other-window)
    (key-chord-define-global ";s" 'ido-switch-buffer)
    (key-chord-define-global ":S" 'ido-switch-buffer-other-window)
    (key-chord-define-global ";w" 'toggle-split-window)
    (key-chord-define-global ":W" 'delete-other-windows)
    (key-chord-define-global ";f" 'ido-find-file)
    (key-chord-define-global ":F" 'ido-find-file-other-window)
    (key-chord-define-global ";t" 'projectile-find-file)
    (key-chord-define-global ":T" 'projectile-find-file-other-window)
    (key-chord-define-global ";g" 'projectile-ag)
    (key-chord-define-global ":G" 'ag)
    (key-chord-define-global "jj" 'ace-jump-char-mode)
    (key-chord-define-global "jk" 'ace-jump-word-mode)
    (key-chord-define-global "jl" 'ace-jump-line-mode)
    (key-chord-define-global "jz" 'ace-jump-zap-up-to-char)
    (key-chord-define-global "zz" 'zap-up-to-char)
    (setq key-chord-two-keys-delay 0.07)))

(use-package company
  :init
  (progn
    (add-hook 'after-init-hook #'global-company-mode)
    (use-package readline-complete
      :init (push 'company-readline company-backends)
      :config (add-λ 'rlc-no-readline-hook (company-mode -1))))
  :config
  (setq company-tooltip-flip-when-above t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-occurrence-weight-function 'company-occurrence-prefer-any-closest
        company-dabbrev-downcase nil)
  (bind-key "TAB" 'company-complete shell-mode-map))

(use-package smart-newline
  :init
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode 1))))

(use-package back-button
  :bind (("s-{" . back-button-global-backward)
         ("s-}" . back-button-global-forward))
  :idle (back-button-mode))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (progn
    (setq powerline-default-separator 'utf-8)
    (defpowerline powerline-minor-modes nil)))

(bind-key "TAB" 'tab-dwim)
(bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)
(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)

(bind-key "<M-up>" 'increment-number-at-point)
(bind-key "<M-down>" 'decrement-number-at-point)
(bind-key "<M-S-up>" 'move-line-up)
(bind-key "<M-S-down>" 'move-line-down)

(bind-key "s-[" 'shift-left)
(bind-key "s-]" 'shift-right)
(bind-key "s-:" 'pad-colon)
(bind-key "s-u" 'duplicate-dwim)
(bind-key "s-s" 'save-buffer)
(bind-key "s-/" 'comment-or-uncomment-region)
(bind-key "<s-return>" 'eol-then-newline)

(bind-key "C-a" 'back-to-indentation-or-beginning)
(bind-key "s-," 'find-user-init-file-other-window)
(bind-key "s-N" 'create-scratch-buffer)
(bind-key "s-k" 'kill-whole-line)
(bind-key "s-w" 'kill-this-buffer)
(bind-key "s-W" 'bury-buffer)

(bind-key "C-h C-p" 'describe-thing-in-popup)

(bind-key "C-z C-k" 'delete-file-and-buffer)
(bind-key "C-z C-r" 'rename-file-and-buffer)
(bind-key "C-z `" 'list-processes)
(bind-key "C-z C-\\" 'align-regexp)
(bind-key "C-z C-w" 'what-face)
(bind-key "C-z C-l" 'log-statement)
(bind-key "C-z C-g" 'google-dwim)
(bind-key "C-z C-f" 'ffap)
(bind-key "C-z C--" (λ (replace-region-or-symbol-at-point-with 's-dashed-words)))
(bind-key "C-z C-_" (λ (replace-region-or-symbol-at-point-with 's-snake-case)))
(bind-key "C-z C-c" (λ (replace-region-or-symbol-at-point-with 's-lower-camel-case)))
(bind-key "C-z C-C" (λ (replace-region-or-symbol-at-point-with 's-upper-camel-case)))

(bind-key "s-o g" (λ (projectile-switch-project-command 'projectile-vc)))
(bind-key "s-o m" (λ (projectile-switch-project-command 'projector-run-shell-command-project-root)))
(bind-key "s-o x" (λ (projectile-switch-project-command 'projector-switch-to-or-create-project-shell)))
(bind-key "s-o n" (λ (projectile-switch-project-command 'ort/capture-todo)))
(bind-key "s-o `" (λ (projectile-switch-project-command 'ort/goto-todos)))

(bind-key "M-TAB" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" 'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input inf-ruby-mode-map)
(bind-key "M-TAB" 'previous-history-element ido-completion-map)
(bind-key "<M-S-tab>" 'next-history-element ido-completion-map)

(load-theme 'hemacs :no-confirm)
(toggle-frame-fullscreen)
(setq debug-on-error nil)
