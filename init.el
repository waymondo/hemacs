(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)
(use-package s)
(use-package noflet)
(use-package dash :config (dash-enable-font-lock))
(load (locate-user-emacs-file "defuns.el"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message "")
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-emacs nil)
(setq vc-follow-symlinks t)
(setq load-prefer-newer t)
(setq byte-compile-warnings '(not obsolete))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(transient-mark-mode t)
(delete-selection-mode t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq-default tab-width 2)
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t)
(setq x-select-enable-clipboard t)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)
(with-region-or-line comment-or-uncomment-region)
(with-region-or-line capitalize-region)
(with-region-or-line downcase-region)
(with-region-or-line kill-region)
(with-region-or-line eval-region)

(add-λ 'before-save-hook
  (unless (eq major-mode 'markdown-mode)
    (delete-trailing-whitespace)))
(add-λ 'before-save-hook
  (when (region-active-p) (deactivate-mark t)))
(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-hook 'find-file-hook 'sm-try-smerge t)

(setq next-error-recenter t)
(setq async-shell-command-buffer 'new-buffer)

(setq echo-keystrokes 0.1)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq minibuffer-eldef-shorten-default t)
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(minibuffer-electric-default-mode t)
(setq history-length 100)
(setq history-delete-duplicates t)

(setq ns-use-native-fullscreen nil)
(setq mac-function-modifier 'hyper)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq ns-pop-up-frames nil)
(setq ns-use-srgb-colorspace t)
(setq delete-by-moving-to-trash t)
(setq gc-cons-threshold 50000000)

(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq pop-up-windows nil)
(setq scroll-margin 24)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq truncate-partial-width-windows 90)
(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-pop-up-frame)))

(setq-default cursor-type 'bar)
(setq blink-cursor-blinks 0)
(setq linum-format " %3s ")
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(fringe-mode '(24 . 8))
(column-number-mode t)
(set-face-attribute 'default nil :height 190 :font "Inconsolata")
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-modified
                " " mode-line-buffer-identification " "
                mode-line-position (vc-mode vc-mode) " "
                mode-name " " mode-line-misc-info mode-line-end-spaces))

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
    (setq explicit-shell-file-name "bash")
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
    :bind ("s-r" . imenu-anywhere)))

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
           (?\[ . ?\])
           (?\` . ?\`)))
    (setq electric-pair-text-pairs '
          ((?\" . ?\")
           (?\` . ?\`)))
    (dolist (c electric-pair-pairs)
      (let ((char (make-string 1 (car c))))
        (global-set-key (kbd (concat "H-" char))
                        `(lambda ()
                           (interactive)
                           (insert ,char)))))))

(use-package subword
  :init
  (progn
    (global-subword-mode 1)
    (setq subword-forward-regexp "\\W*\\(\\([_[:upper:]]*\\(\\W\\)?\\)[[:lower:][:digit:]]*\\)")
    (setq subword-backward-regexp "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\|_\\w+\\)")
    (bind-key* "<M-left>" 'subword-left)
    (bind-key* "<M-right>" 'subword-right)
    (define-key subword-mode-map [remap backward-kill-word] 'subword-backward-delete)))

(use-package hippie-exp
  :init
  (progn
    (global-set-key [remap dabbrev-expand] #'hippie-expand)
    (bind-key "TAB" 'hippie-expand read-expression-map)
    (bind-key "TAB" 'hippie-expand minibuffer-local-map)
    (bind-key "M-?" (make-hippie-expand-function '(try-expand-line) t))
    (setq hippie-expand-verbose nil)
    (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                             try-expand-dabbrev
                                             try-expand-dabbrev-matching-buffers
                                             try-expand-all-abbrevs
                                             try-complete-file-name-partially
                                             try-complete-file-name))
    (add-λ 'emacs-lisp-mode-hook
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

(use-package dired-x
  :init
  (progn
    (bind-key "C-x C-k" 'dired-do-delete dired-mode-map)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    (setq dired-auto-revert-buffer t)
    (when (and (memq window-system '(mac ns)) (executable-find "gls"))
      (setq insert-directory-program "gls" dired-use-ls-dired t))
    (use-package dired-toggle
      :bind ("s-\\" . dired-toggle))))

(use-package ignoramus
  :init (ignoramus-setup))

(use-package org
  :config
  (progn
    (setq org-support-shift-select t)
    (setq org-completion-use-ido t)))

(use-package eldoc
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-eldoc-mode)))

(use-package smex
  :bind ("s-P" . smex)
  :init
  (progn
    (smex-initialize)
    (global-set-key [remap execute-extended-command] #'smex)))

(use-package dash-at-point
  :bind (("C-c d" . dash-at-point)
         ("C-c D" . dash-at-point-with-docset))
  :config
  (setq dash-at-point-docsets
        '("coffee" "lisp" "css" "elisp" "html" "javascript" "iphoneos"
          "ruby" "jquery" "meteor" "phonegap" "rubygems" "rails" "macosx"
          "underscore" "d3" "backbone" "bootstrap" "markdown" "zepto")))

(use-package github-browse-file
  :bind (("C-c g" . github-browse-file)
         ("C-c G" . github-browse-file-blame)))

(use-package ag
  :config
  (progn
    (setq ag-reuse-buffers t)
    (setq ag-highlight-search t)))

(use-package alert
  :config (setq alert-default-style 'notifier))

(use-package crab-mode
  :bind (("C-M-," . crab-reload)
         ("C-M-;" . crab-eval-coffee))
  :idle (crab-server-start))

(use-package elisp-slime-nav
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package popwin
  :init (popwin-mode 1)
  :config
  (progn
    (setq popwin:popup-window-height 0.3)
    (push "COMMIT_EDITMSG" popwin:special-display-config)))

(use-package projectile
  :bind (("s-t" . projectile-find-file)
         ("s-b" . projectile-switch-to-buffer)
         ("s-o" . projectile-switch-project-vc)
         ("s-p" . projectile-commander))
  :init
  (progn
    (use-package projectile-rails
      :init (add-hook 'projectile-mode-hook 'projectile-rails-on))
    (projectile-global-mode)))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package httprepl)

(use-package handlebars-mode)

(use-package fountain-mode
  :mode ("\\.fountain$" . fountain-mode)
  :config (add-hook 'fountain-mode-hook 'hemacs-writing-hook))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'hemacs-writing-hook))

(use-package css-mode
  :bind ("C-c C-b" . beautify-css)
  :config (setq css-indent-offset 2)
  :init
  (use-package less-css-mode
    :mode ("\\.scss$" . less-css-mode)))

(use-package js
  :mode ("\\.json$" . js-mode)
  :interpreter ("node" . js-mode)
  :config (setq-default js-indent-level 2))

(use-package coffee-mode
  :config
  (progn
    (setq coffee-args-repl '("-i" "--nodejs"))
    (add-to-list 'auto-mode-alist '("\\.coffee\\.*" . coffee-mode))
    (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
    (bind-key "M-r" 'coffee-compile-region coffee-mode-map)))

(use-package ruby-mode
  :init
  (progn
    (use-package rspec-mode)
    (use-package ruby-end)
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
  :config
  (progn
    (setq ruby-use-smie nil)
    (setq ruby-deep-arglist nil)
    (setq ruby-deep-indent-paren nil))
  :mode (("Procfile$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("\\.env\\.*" . ruby-mode)))

(use-package magit
  :bind ("s-m" . magit-status)
  :config
  (progn
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
    (bind-key "C-c C-p" 'magit-pull-request-for-issue-number magit-mode-map)
    (bind-key "C-x C-k" 'magit-kill-file-on-line magit-mode-map)
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
    (setq magit-git-executable "/usr/local/bin/git")
    (setq magit-log-auto-more t)
    (setq magit-set-upstream-on-push t)
    (setq magit-restore-window-configuration t)
    (setq magit-save-some-buffers nil)
    (setq magit-revert-item-confirm nil)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-commit-ask-to-stage nil)
    (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'magit-process-mode-hook 'hemacs-shellish-hook)
    (use-package magit-filenotify
      :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config (setq git-messenger:show-detail t))

(use-package projector
  :config
  (progn
    (bind-key* "C-c RET" 'projector-run-shell-command-project-root)
    (setq projector-projects-root "~/code/")
    (setq projector-always-background-regex
          '("^mysql.server\\.*"
            "^bundle install"
            "^bundle update\\.*"
            "^powder restart"
            "^heroku restart\\.*"
            "^spring stop"
            "^git push\\.*"
            "^rake db:migrate"
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

(use-package highlight-symbol
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-symbol-mode)
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))
  :config (setq highlight-symbol-idle-delay 0))

(use-package volatile-highlights
  :init (volatile-highlights-mode t))

(use-package highlight-tail
  :idle (highlight-tail-mode)
  :config (setq highlight-tail-timer 0.02))

(use-package rainbow-mode
  :init
  (dolist (hook '(css-mode-hook emacs-lisp-mode-hook))
    (add-hook hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package god-mode
  :bind ("s-`" . god-mode-all)
  :config
  (progn
    (bind-key "i" 'kill-region-and-god-local-mode god-local-mode-map)
    (bind-key "." 'repeat god-local-mode-map)
    (add-λ 'god-mode-enabled-hook
      (setq cursor-type 'box))
    (add-λ 'god-mode-disabled-hook
      (setq cursor-type 'bar))
    (dolist (mode '(git-commit-mode))
      (add-to-list 'god-exempt-major-modes mode))))

(use-package bind-key
  :bind ("C-h C-k" . describe-personal-keybindings))

(use-package guide-key
  :init (guide-key-mode 1)
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r" "C-x" "C-c"
            "C-c p" "C-x +" "C-c ," "C-h" "M-s"))
    (setq guide-key/popup-window-position 'bottom)
    (setq guide-key/idle-delay 0.5)))

(use-package free-keys
  :bind ("C-h C-f" . free-keys))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package anzu
  :init (global-anzu-mode 1))

(use-package ace-jump-mode
  :init (bind-key* "C-;" 'ace-jump-word-mode))

(use-package expand-region
  :init (bind-key* "C-," 'er/expand-region))

(use-package multiple-cursors
  :bind (("C-c C-." . mc/mark-next-like-this)
         ("C-c C-," . mc/mark-previous-like-this)
         ("C-c C-/" . mc/mark-all-like-this-dwim)))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package org-repo-todo
  :init
  (progn
    (bind-key* "C-." 'ort/capture-todo)
    (bind-key* "C-/" 'ort/goto-todos)))

(use-package easy-kill
  :init
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark)))

(use-package misc
  :bind ("C-z" . zap-up-to-char))

(use-package ace-jump-buffer)

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (progn

    (key-chord-define-global "<>" 'sgml-close-tag)
    (key-chord-define-global "{}" 'open-brackets-newline-and-indent)
    (key-chord-define-global "[]" 'pad-brackets)
    (key-chord-define-global "_+" 'insert-fat-arrow)
    (key-chord-define-global "-=" 'insert-arrow)
    (key-chord-define-global "}|" 'delete-other-windows)
    (key-chord-define-global "^^" (λ (insert "λ")))

    (key-chord-define-global ";a" 'ace-jump-buffer-in-one-window)
    (key-chord-define-global ":A" 'ace-jump-buffer-other-window)
    (key-chord-define-global ";w" 'toggle-split-window)
    (key-chord-define-global ";s" 'projectile-recentf)
    (key-chord-define-global ":S" 'ido-switch-buffer)
    (key-chord-define-global ";t" 'projectile-find-file)
    (key-chord-define-global ";r" 'imenu-anywhere)
    (key-chord-define-global ";f" 'ido-find-file)
    (key-chord-define-global ";g" 'projectile-ag)
    (key-chord-define-global ":G" 'ag)
    (key-chord-define-global ";x" 'projector-switch-to-shell-buffer)
    (key-chord-define-global ";c" 'projector-switch-to-or-create-project-shell)

    (setq key-chord-two-keys-delay 0.05)
    (setq key-chord-two-keys-delay 0.1)))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (setq company-tooltip-flip-when-above t)
    (setq company-idle-delay 0.4)
    (setq company-minimum-prefix-length 2)
    (setq company-dabbrev-code-other-buffers 'code)
    (setq company-require-match nil)
    (setq company-auto-complete t)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations t)
    (bind-key "C-n" 'company-select-next company-active-map)
    (bind-key "C-p" 'company-select-previous company-active-map)
    (use-package readline-complete
      :init (push 'company-readline company-backends)
      :config (add-λ 'rlc-no-readline-hook
                (company-mode -1)))
    (use-package company-inf-ruby
      :init (push 'company-inf-ruby company-backends))
    (push 'company-robe company-backends)))

(use-package smart-newline
  :idle (smart-newline-mode))

(bind-key "TAB" 'tab-dwim)
(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)
(bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)

(bind-key "<M-up>" 'move-line-up)
(bind-key "<M-down>" 'move-line-down)
(bind-key "<s-up>" 'increment-number-at-point)
(bind-key "<s-down>" 'decrement-number-at-point)

(bind-key "s-:" 'pad-colon)
(bind-key "s-u" 'duplicate-dwim)
(bind-key "s-q" 'query-replace)
(bind-key "s-]" 'shift-right)
(bind-key "s-[" 'shift-left)
(bind-key "s-s" 'save-buffer)
(bind-key "s-/" 'comment-or-uncomment-region)
(bind-key "<s-return>" 'eol-then-newline)

(bind-key "M--" (λ (replace-region-or-symbol-at-point-with 's-dashed-words)))
(bind-key "M-_" (λ (replace-region-or-symbol-at-point-with 's-snake-case)))
(bind-key "M-c" (λ (replace-region-or-symbol-at-point-with 's-lower-camel-case)))
(bind-key "M-C" (λ (replace-region-or-symbol-at-point-with 's-upper-camel-case)))

(bind-key "C-a" 'back-to-indentation-or-beginning)
(bind-key "C-o" 'smart-open-line-above)
(bind-key "s-l" 'goto-line-with-feedback)
(bind-key "s-," 'find-user-init-file-other-window)
(bind-key "s-n" 'create-scratch-buffer)
(bind-key "s-k" 'kill-whole-line)
(bind-key "s-w" 'bury-buffer)

(bind-key "C-x C-k" 'delete-file-and-buffer)
(bind-key "C-c r" 'rename-file-and-buffer)
(bind-key "C-c `" 'list-processes)
(bind-key "C-c C-\\" 'align-regexp)
(bind-key "C-c C-w" 'what-face)
(bind-key "C-c m" 'shell)
(bind-key "C-c l" 'log-statement)
(bind-key "C-c C-o" 'google-dwim)

(bind-key "M-TAB" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" 'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input inf-ruby-mode-map)

(load-theme 'hemacs :no-confirm)

(toggle-frame-fullscreen)
