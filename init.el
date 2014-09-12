(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(require 'use-package)
(use-package s)
(use-package noflet)
(use-package dash :config (dash-enable-font-lock))
(load (locate-user-emacs-file "defuns.el"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message "")
(setq ring-bell-function 'ignore)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-emacs nil)
(setq vc-follow-symlinks t)
(setq gc-cons-threshold 50000000)
(setq byte-compile-warnings '(not obsolete))
(setq disabled-command-function nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(transient-mark-mode t)
(delete-selection-mode t)
(setq echo-keystrokes 0.1)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq-default tab-width 2)
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t)
(setq x-select-enable-clipboard t)
(setq history-length 100)
(setq history-delete-duplicates t)

(with-region-or-line comment-or-uncomment-region)
(with-region-or-line upcase-region)
(with-region-or-line capitalize-region)
(with-region-or-line downcase-region)
(with-region-or-line yank-region)
(with-region-or-line kill-region t)
(with-region-or-line kill-ring-save t)
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(setq ns-use-native-fullscreen nil)
(setq mac-function-modifier 'hyper)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq ns-pop-up-frames nil)
(setq ns-use-srgb-colorspace t)
(setq delete-by-moving-to-trash t)
(setq mac-right-option-modifier 'none)

(setq completion-pcm-complete-word-inserts-delimiters t)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode t)

(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq pop-up-windows nil)
(setq truncate-partial-width-windows 90)
(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-pop-up-frame)))

(setq scroll-margin 24)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(setq blink-cursor-blinks 0)
(setq-default cursor-type 'bar)
(setq-default indicate-empty-lines t)
(setq-default left-fringe-width 10)
(setq-default right-fringe-width 5)
(set-face-attribute 'default nil :height 150 :font "Meslo LG M DZ for Powerline")
(global-prettify-symbols-mode t)
(add-λ 'minibuffer-setup-hook
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 0.9))))

(add-λ 'before-save-hook
  (unless (eq major-mode 'markdown-mode)
    (delete-trailing-whitespace))
  (when (region-active-p)
    (deactivate-mark t)))
(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-hook 'find-file-hook 'sm-try-smerge t)
(add-hook 'image-mode-hook 'show-image-dimensions-in-mode-line)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(define-prefix-command 'hemacs-map)
(define-prefix-command 'hemacs-github-map)
(define-prefix-command 'hemacs-crab-map)
(bind-key "C-z" 'hemacs-map)
(bind-key "C-z g" 'hemacs-github-map)
(bind-key "C-z c" 'hemacs-crab-map)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (exec-path-from-shell-initialize)
    (--each '("HISTFILE" "NODE_PATH" "SSL_CERT_FILE")
      (exec-path-from-shell-copy-env it))))

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
  :init
  (progn
    (global-auto-revert-mode t)
    (setq auto-revert-verbose nil)
    (setq global-auto-revert-non-file-buffers t)))

(use-package imenu
  :config (setq imenu-auto-rescan t)
  :init
  (use-package imenu-anywhere
    :bind ("s-r" . imenu-anywhere)
    :config (add-hook 'imenu-after-jump-hook #'pulse-line-hook-function)))

(use-package savehist
  :init
  (progn
    (savehist-mode t)
    (setq savehist-additional-variables
          '(search-ring regexp-search-ring comint-input-ring))
    (setq savehist-autosave-interval 30)))

(use-package recentf
  :init
  (progn
    (recentf-mode t)
    (setq recentf-exclude '(".ido.last" "COMMIT_EDITMSG"))
    (setq initial-buffer-choice (car recentf-list))
    (setq recentf-max-saved-items 500)))

(use-package paren
  :init (show-paren-mode t))

(use-package elec-pair
  :init
  (progn
    (electric-pair-mode t)
    (setq electric-pair-pairs '
          ((?\( . ?\))
           (?\" . ?\")
           (?\{ . ?\})
           (?\[ . ?\])))
    (setq electric-pair-text-pairs '
          ((?\" . ?\")
           (?\` . ?\`)))))

(use-package subword
  :init
  (progn
    (global-subword-mode 1)
    (bind-key* "<M-left>" 'subword-left)
    (bind-key* "<M-right>" 'subword-right)
    (define-key subword-mode-map [remap backward-kill-word] 'subword-backward-delete)))

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
      (set (make-local-variable 'hippie-expand-try-functions-list)
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

(use-package dired-x
  :init
  (progn
    (bind-key "s-\\" 'dired-jump-other-window)
    (bind-key "C-z C-k" 'dired-do-delete dired-mode-map)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    (setq dired-auto-revert-buffer t)
    (when (and (memq window-system '(mac ns)) (executable-find "gls"))
      (setq insert-directory-program "gls" dired-use-ls-dired t))))

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

(use-package dash-at-point
  :bind (("C-z C-d" . dash-at-point)
         ("C-z C-D" . dash-at-point-with-docset))
  :config
  (setq dash-at-point-docsets
        '("coffee" "lisp" "css" "elisp" "html" "javascript" "iphoneos"
          "ruby" "jquery" "meteor" "phonegap" "rubygems" "rails" "macosx"
          "underscore" "d3" "backbone" "bootstrap" "markdown" "zepto")))

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

(use-package popwin
  :init (popwin-mode 1)
  :config
  (progn
    (setq popwin:popup-window-height 0.3)
    (push '(dired-mode :position left) popwin:special-display-config)
    (push "COMMIT_EDITMSG" popwin:special-display-config)))

(use-package projectile
 :bind (("s-t" . projectile-find-file)
        ("s-b" . projectile-switch-to-buffer)
        ("s-o" . projectile-switch-project-vc)
        ("s-O" . projectile-switch-project-run-command-project-root)
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
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-log-auto-more t)
    (setq magit-set-upstream-on-push t)
    (setq magit-restore-window-configuration t)
    (setq magit-save-some-buffers nil)
    (setq magit-revert-item-confirm nil)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-commit-ask-to-stage nil)
    (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'magit-process-mode-hook 'hemacs-shellish-hook)))

(use-package github-browse-file
  :bind (("C-z g o" . github-browse-file)
         ("C-z g b" . github-browse-file-blame)))

(use-package github-clone
  :bind ("C-z g c" . github-clone))

(use-package git-timemachine
  :bind ("C-z g t" . git-timemachine))

(use-package git-messenger
  :bind ("C-z g p" . git-messenger:popup-message)
  :config (setq git-messenger:show-detail t))

(use-package projector
  :config
  (progn
    (bind-key* "C-z RET" 'projector-run-shell-command-project-root)
    (bind-key* "C-z m" 'projector-switch-to-or-create-project-shell)
    (setq projector-projects-root "~/code/")
    (setq projector-always-background-regex
          '("^mysql.server\\.*"
            "^powder restart"
            "^heroku restart\\.*"
            "^spring stop"
            "^git push\\.*"
            "\\.*cordova run\\.*"
            "^redis-server"
            "^pkill\\.*"))))

(use-package ido
  :init
  (progn
    (ido-mode t)
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode 1))
    (use-package flx-ido
      :init (flx-ido-mode 1))
    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1)
      :config (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))
  :config
  (progn
    (setq ido-cannot-complete-command 'exit-minibuffer)
    (setq ido-use-virtual-buffers t)
    (setq ido-auto-merge-delay-time 10)
    (setq ido-enable-flex-matching t)
    (setq ido-enable-dot-prefix t)
    (setq ido-max-prospects 10)
    (setq ido-create-new-buffer 'always)
    (bind-key "~" 'ido-go-home ido-file-completion-map)))

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
  :init (volatile-highlights-mode t))

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

(use-package bind-key
  :bind ("C-h C-k" . describe-personal-keybindings))

(use-package free-keys
  :bind ("C-h C-f" . free-keys))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package guide-key
  :init (guide-key-mode 1)
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r" "C-x" "C-c"
            "C-z" "C-z g" "C-c p" "C-x +" "C-c ," "C-h" "M-s"))
    (setq guide-key/popup-window-position 'bottom)))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package anzu
  :bind ("s-q" . anzu-query-replace)
  :init (global-anzu-mode 1))

(use-package ace-jump-mode
  :init (bind-key* "C-;" 'ace-jump-word-mode)
  :config (setq ace-jump-mode-case-fold nil
                ace-jump-mode-scope 'visible))

(use-package expand-region
  :init (bind-key* "C-," 'er/expand-region))

(use-package multiple-cursors
  :bind (("C-z C-." . mc/mark-next-like-this)
         ("C-z C-," . mc/mark-previous-like-this)
         ("C-z C-/" . mc/mark-all-like-this-dwim)))

(use-package change-inner
  :bind (("C-z TAB" . change-inner)
         ("C-z C-o" . change-outer)))

(use-package highlight-tail
  ;; :idle (highlight-tail-mode)
  :config (setq highlight-tail-timer 0.02))

(use-package tabbar
  :bind (("s-{" . tabbar-backward)
         ("s-}" . tabbar-forward))
  ;; :init (tabbar-mode)
  :config
  (progn
    (setq tabbar-buffer-groups-function
          (λ (list (if (projectile-project-p) (projectile-project-name) "Emacs"))))
    (defadvice tabbar-line-format (around no-tabbar-buttons activate compile)
      (noflet ((tabbar-line-buttons (tabset) (list tabbar-separator-value))) ad-do-it))
    (defadvice tabbar-buffer-tab-label (after buffer-tab-padding activate compile)
      (setq ad-return-value (concat " " (concat ad-return-value " "))))))

(use-package golden-ratio
  ;; :init (golden-ratio-mode)
  :config
  (progn
    (setq golden-ratio-extra-commands
          (append golden-ratio-extra-commands
                  '(next-multiframe-window)))
    (setq golden-ratio-exclude-modes
          '("magit-key-mode"))
    (setq golden-ratio-inhibit-functions
          '(golden-ratio-inhibit-popwin-config))
    (setq golden-ratio-recenter t)
    (setq golden-ratio-exclude-buffer-names
          '("*buffer-selection*"
            " *guide-key*"
            "CAPTURE-TODO.org"))))

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (progn
    (use-package misc)
    (use-package ace-jump-buffer)
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
  :init (add-hook 'after-init-hook #'global-company-mode)
  :config
  (progn
    (setq company-tooltip-flip-when-above t)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations t)
    (setq company-auto-complete t)
    (setq company-occurrence-weight-function 'company-occurrence-prefer-any-closest)
    (setq company-dabbrev-downcase nil)
    (use-package readline-complete
      :init (push 'company-readline company-backends)
      :config (add-λ 'rlc-no-readline-hook (company-mode -1)))
    (push 'company-robe company-backends)))

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
(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)
(bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)

(bind-key "<M-up>" 'move-line-up)
(bind-key "<M-down>" 'move-line-down)
(bind-key "<s-up>" 'increment-number-at-point)
(bind-key "<s-down>" 'decrement-number-at-point)

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
