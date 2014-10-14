(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'use-package)
(use-package noflet)
(use-package dash :config (dash-enable-font-lock))
(use-package tool-bar :init (tool-bar-mode -1))
(use-package scroll-bar :init (scroll-bar-mode -1))

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode rhtml-mode slim-mode inf-ruby-mode))

(load (locate-user-emacs-file "defuns.el"))

(setq load-prefer-newer t
      history-length 100
      history-delete-duplicates t
      scroll-margin 24
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      echo-keystrokes 0.1
      ns-use-native-fullscreen nil
      ns-use-srgb-colorspace t
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      vc-follow-symlinks t
      gc-cons-threshold 50000000
      disabled-command-function nil
      create-lockfiles nil
      kill-buffer-query-functions '(hemacs-kill-buffer-query))

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              indicate-empty-lines t
              left-fringe-width 10
              right-fringe-width 5)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message ""
      split-height-threshold nil
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
(with-region-or-line kill-region :point-to-eol)
(with-region-or-line kill-ring-save :point-to-eol)
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(define-prefix-command 'hemacs-map)
(bind-key "C-z" 'hemacs-map)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (--each '("HISTFILE" "NODE_PATH")
    (exec-path-from-shell-copy-env it)))

(use-package ns-win
  :config
  (setq ns-pop-up-frames nil
        mac-function-modifier 'hyper
        mac-right-option-modifier 'none))

(use-package frame
  :config (setq blink-cursor-blinks 0))

(use-package prog-mode
  :init (global-prettify-symbols-mode))

(use-package simple
  :bind (("C-`" . list-processes)
         ("s-k" . kill-whole-line))
  :init
  (defadvice backward-kill-word (around backward-kill-subword activate compile)
    (noflet ((kill-region (beg end) (delete-region beg end)))
      ad-do-it))
  (defadvice kill-whole-line (after kill-whole-lines-back-to-indentation activate compile)
    (back-to-indentation))
  (defadvice move-beginning-of-line
      (around move-beginning-of-line-or-indentation activate compile)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        ad-do-it))))

(use-package align
  :bind ("C-\\" . align-regexp))

(use-package image-mode
  :init (add-hook 'image-mode-hook 'show-image-dimensions-in-mode-line))

(use-package files
  :mode ("Cask" . emacs-lisp-mode)
  :config
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate compile)
    (noflet ((process-list ())) ad-do-it))
  (setq require-final-newline t
        confirm-kill-emacs nil
        confirm-nonexistent-file-or-buffer nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (add-hook 'before-save-hook 'hemacs-save-hook)
  (add-hook 'find-file-hook 'sm-try-smerge t))

(use-package newcomment
  :bind ("s-/" . comment-or-uncomment-region))

(use-package ffap
  :bind ("C-z C-f" . ffap))

(use-package menu-bar
  :bind ("s-w" . kill-this-buffer))

(use-package delsel
  :init (delete-selection-mode))

(use-package comint
  :init
  (bind-key "s-k" 'clear-shell comint-mode-map)
  (setq comint-process-echoes t
        comint-buffer-maximum-size 5000)
  (setq-default comint-prompt-read-only t
                comint-input-ignoredups t)
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (add-hook 'comint-mode-hook 'hemacs-shellish-hook))

(use-package compile
  :init
  (setq compilation-disable-input t
        compilation-message-face nil
        compilation-always-kill t)
  (add-hook 'compilation-mode-hook 'hemacs-shellish-hook))

(use-package shell
  :init
  (setq async-shell-command-buffer 'new-buffer
        shell-command-switch (purecopy "-ic")
        explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package auto-compile
  :init (auto-compile-on-load-mode)
  :config (setq auto-compile-display-buffer nil))

(use-package imenu
  :init
  (add-hook 'emacs-lisp-mode-hook 'setup-imenu-for-use-package)
  (setq imenu-auto-rescan t)
  (use-package imenu-anywhere
    :bind ("s-r" . imenu-anywhere)))

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
        recentf-max-saved-items 500))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-style 'mixed))

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
  :init (global-subword-mode))

(use-package pulse
  :config
  (setq pulse-command-advice-flag t
        pulse-delay 0.01
        pulse-iterations 4)
  (add-λ 'post-command-hook
    (when (member this-command
                  '(scroll-down-command
                    scroll-up-command
                    next-multiframe-window
                    find-tag))
      (pulse-line-hook-function)))
  (--each '(next-error-hook
            focus-in-hook
            find-function-after-hook
            imenu-after-jump-hook)
    (add-hook it #'pulse-line-hook-function)))

(use-package hippie-exp
  :init
  (defadvice hippie-expand (around hippie-expand-case-fold activate compile)
    (let ((case-fold-search nil))
      ad-do-it))
  (defalias 'he-dabbrev-beg 'hemacs-dabbrev-beg)
  (bind-key [remap dabbrev-expand] #'hippie-expand)
  (bind-key "TAB" 'hippie-expand read-expression-map)
  (bind-key "TAB" 'hippie-expand minibuffer-local-map)
  (bind-key "M-?" (make-hippie-expand-function '(try-expand-line) t))
  (setq hippie-expand-verbose nil
        hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                           try-expand-dabbrev
                                           try-expand-dabbrev-matching-buffers
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev-other-buffers))
  (hook-modes lispy-modes
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config (setq-default save-place t))

(use-package edit-server
  :init (edit-server-start)
  :config (setq edit-server-new-frame nil))

(use-package dired
  :init
  (bind-key "C-z C-k" 'dired-do-delete dired-mode-map)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package dired-toggle
  :bind ("s-\\" . dired-toggle)
  :config (setq dired-toggle-window-size 48))

(use-package org
  :config
  (setq org-support-shift-select t
        org-completion-use-ido t))

(use-package org-repo-todo
  :bind (("s-n" . ort/capture-todo)
         ("s-`" . ort/goto-todos)))

(use-package find-func
  :init (find-function-setup-keys))

(use-package paredit
  :init
  (hook-modes lispy-modes
    (enable-paredit-mode)))

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
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (bind-key [remap execute-extended-command] #'smex))

(use-package ag
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t))

(use-package easy-kill
  :init
  (bind-key [remap kill-ring-save] 'easy-kill)
  (bind-key [remap mark-sexp] 'easy-mark))

(use-package alert
  :config (setq alert-default-style 'notifier))

(use-package crab-mode
  :bind (("s-R" . crab-reload)
         ("s-”" . crab-prev-tab)
         ("s-’" . crab-next-tab))
  :idle (crab-server-start))

(use-package projectile
  :bind (("s-t" . projectile-find-file)
         ("s-p" . projectile-commander))
  :idle (projectile-global-mode)
  :config (setq projectile-enable-caching t
                projectile-tags-command "ripper-tags -R -f TAGS")
  :init
  (use-package projectile-rails
    :init (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package sgml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars$" . html-mode))
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (bind-key "<C-return>" 'html-smarter-newline html-mode-map)
  (make-beautify-defun "html"))

(use-package fountain-mode
  :mode ("\\.fountain$" . fountain-mode)
  :config (add-hook 'fountain-mode-hook 'hemacs-writing-hook))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'hemacs-writing-hook))

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (make-beautify-defun "css"))

(use-package less-css-mode
  :mode ("\\.scss$" . less-css-mode))

(use-package js
  :mode ("\\.json$" . js-mode)
  :interpreter ("node" . js-mode)
  :config
  (make-beautify-defun "js")
  (setq-default js-indent-level 2))

(use-package coffee-mode
  :mode ("\\.coffee\\.*" . coffee-mode)
  :ensure coffee-mode
  :config
  (setq coffee-args-repl '("-i" "--nodejs"))
  (bind-key "<C-return>" 'coffee-smarter-newline coffee-mode-map)
  (bind-key "C-c C-c" 'coffee-compile-region coffee-mode-map))

(use-package ruby-mode
  :init
  (bind-key "<C-return>" 'ruby-smarter-newline ruby-mode-map)
  (use-package rspec-mode)
  (use-package inf-ruby
    :init (add-λ 'inf-ruby-mode-hook
            (turn-on-comint-history "~/.irb_history")))
  (use-package slim-mode
    :config
    (setq slim-backspace-backdents-nesting nil)
    (bind-key "C-j" 'electric-indent-just-newline slim-mode-map)
    (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))
  (use-package rhtml-mode)
  (use-package chruby
    :init
    (add-hook 'projectile-switch-project-hook 'chruby-use-corresponding))
  (use-package ruby-hash-syntax
    :init
    (--each ruby-modes
      (bind-key "C-:" 'ruby-toggle-hash-syntax
                (symbol-value (intern (format "%s-map" it))))))
  :mode (("Procfile$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("\\.env\\.*" . ruby-mode)))

(use-package magit
  :bind ("s-m" . magit-status)
  :config
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
  (add-hook 'magit-process-mode-hook 'hemacs-shellish-hook))

(use-package git-commit-mode
  :config (setq git-commit-fill-column 90))

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

(use-package projector
  :bind* (("C-z RET" . projector-run-shell-command-project-root)
          ("C-z m" . projector-switch-to-or-create-project-shell))
  :config
  (setq projector-always-background-regex
        '("^mysql.server\\.*"
          "^powder restart\\.*"
          "^heroku restart\\.*"
          "^spring stop"
          "^gulp publish\\.*"
          "^git push\\.*"
          "\\.*cordova run\\.*"
          "^redis-server"
          "^pkill\\.*")))

(use-package ido
  :init
  (ido-mode)
  (use-package ido-ubiquitous
    :init (ido-ubiquitous-mode))
  (use-package flx-ido
    :init (flx-ido-mode)
    :config (setq flx-ido-use-faces nil))
  (use-package ido-vertical-mode
    :init (ido-vertical-mode))
  :config
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

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
  :init
  (hook-modes progish-modes
    (rainbow-delimiters-mode)))

(use-package god-mode
  :bind ("<escape>" . god-local-mode)
  :config
  (bind-key "i" 'kill-region-and-god-local-mode god-local-mode-map)
  (bind-key "." 'repeat god-local-mode-map)
  (add-λ 'god-mode-enabled-hook
    (setq cursor-type 'box))
  (add-λ 'god-mode-disabled-hook
    (setq cursor-type 'bar))
  (add-to-list 'god-exempt-major-modes 'git-commit-mode))

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

(use-package popup
  :commands popup-tip
  :bind ("C-h C-p" . describe-thing-in-popup))

(use-package guide-key
  :init (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r" "C-x" "C-c"
          "C-z" "C-c p" "C-x +" "C-c ," "C-h" "M-s")
        guide-key/popup-window-position 'bottom))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package anzu
  :bind (("s-q" . anzu-query-replace)
         ("M-q" . anzu-query-replace-at-cursor))
  :init (global-anzu-mode))

(use-package toggle-quotes
  :bind ("C-'" . toggle-quotes))

(use-package expand-region
  :bind* ("C-," . er/expand-region))

(use-package multiple-cursors
  :bind (("C-z C-." . mc/mark-next-like-this)
         ("C-z C-," . mc/mark-previous-like-this)
         ("C-z C-/" . mc/mark-all-like-this-dwim)))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package ace-jump-mode
  :bind* ("C-;" . ace-jump-word-mode)
  :init (use-package ace-jump-zap)
  :config
  (ace-jump-mode-enable-mark-sync)
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

(use-package ace-jump-buffer
  :init
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode))))
  (make-ace-jump-buffer-function "magit"
    (with-current-buffer buffer
      (not (derived-mode-p 'magit-mode)))))

(use-package swoop
  :config
  (setq swoop-font-size-change: nil
        swoop-window-split-direction: 'split-window-horizontally)
  (bind-key "C-o" 'swoop-multi-from-swoop swoop-map)
  (bind-key "C-s" 'swoop-action-goto-line-next swoop-map)
  (bind-key "C-r" 'swoop-action-goto-line-prev swoop-map))

(use-package ace-isearch
  :init (global-ace-isearch-mode)
  :config (setq ace-isearch-funtion-from-isearch 'swoop-from-isearch))

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil))
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
  (key-chord-define-global "jb" 'ace-jump-buffer-with-configuration)
  (key-chord-define-global "jj" 'ace-jump-char-mode)
  (key-chord-define-global "jk" 'ace-jump-word-mode)
  (key-chord-define-global "jl" 'ace-jump-line-mode)
  (key-chord-define-global "jz" 'ace-jump-zap-up-to-char)
  (key-chord-define-global "zz" 'zap-up-to-char)
  (setq key-chord-two-keys-delay 0.07))

(use-package company
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (add-hook 'inf-ruby-mode-hook #'company-mode)
  (use-package readline-complete
    :init (push 'company-readline company-backends)
    :config (add-λ 'rlc-no-readline-hook (company-mode -1)))
  :config
  (setq company-tooltip-flip-when-above t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-minimum-prefix-length 2
        company-occurrence-weight-function 'company-occurrence-prefer-any-closest
        company-dabbrev-downcase nil)
  (bind-key "TAB" 'company-complete shell-mode-map))

(use-package smart-newline
  :init
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))))

(use-package back-button
  :bind (("s-{" . back-button-global-backward)
         ("s-}" . back-button-global-forward))
  :idle (back-button-mode))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (setq powerline-default-separator 'utf-8)
  (defpowerline powerline-minor-modes nil))

(bind-key "TAB" 'tab-dwim)
(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)

(bind-key "<M-up>" 'increment-number-at-point)
(bind-key "<M-down>" 'decrement-number-at-point)
(bind-key "<M-S-up>" 'move-line-up)
(bind-key "<M-S-down>" 'move-line-down)

(bind-key "s-[" 'shift-left)
(bind-key "s-]" 'shift-right)
(bind-key "s-:" 'pad-colon)
(bind-key "s-u" 'duplicate-dwim)
(bind-key "<s-return>" 'eol-then-newline)

(bind-key "s-," 'find-user-init-file-other-window)
(bind-key "s-N" 'create-scratch-buffer)
(bind-key "s-W" 'bury-buffer)
(bind-key "s-g" 'google)

(bind-key "C-z C-k" 'delete-file-and-buffer)
(bind-key "C-z C-r" 'rename-file-and-buffer)
(bind-key "C-z C-l" 'log-statement)
(bind-key "C-z C-w" 'what-face)
(bind-key "C-z C-o" 'open-package)

(bind-key "C-z -" (λ (replace-region-or-symbol-at-point-with 's-dashed-words)))
(bind-key "C-z _" (λ (replace-region-or-symbol-at-point-with 's-snake-case)))
(bind-key "C-z c" (λ (replace-region-or-symbol-at-point-with 's-lower-camel-case)))
(bind-key "C-z C" (λ (replace-region-or-symbol-at-point-with 's-upper-camel-case)))

(define-prefix-command 'hemacs-projectile-map)
(bind-key "s-o" 'hemacs-projectile-map)
(bind-key "s-o m" (λ (projectile-switch-project-command 'projectile-vc)))
(bind-key "s-o f" (λ (projectile-switch-project-command 'projectile-find-file)))
(bind-key "s-o c" (λ (projectile-switch-project-command 'projector-run-shell-command-project-root)))
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

(set-face-attribute 'default nil :height 150 :font "Meslo LG M DZ for Powerline")
(load-theme 'hemacs :no-confirm)
(toggle-frame-fullscreen)
