;;; hemacs --- an emacs configuration -*- lexical-binding: t; -*-

;;;;; Source Variables

(setq load-prefer-newer t
      history-length 256
      history-delete-duplicates t
      scroll-margin 24
      scroll-conservatively 10000
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      echo-keystrokes 0.02
      ns-use-native-fullscreen nil
      ns-use-srgb-colorspace t
      delete-by-moving-to-trash t
      ring-bell-function #'ignore
      gc-cons-threshold 50000000
      ns-function-modifier 'hyper
      ns-right-option-modifier 'none
      create-lockfiles nil)

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              truncate-lines t)

;;;;; Personal Variables

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

;;;;; Bootstrap

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package use-package-chords
  :ensure t)

(use-package hemacs
  :load-path "lib/")

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package s
  :ensure t
  :commands (s-lower-camel-case s-upper-camel-case s-snake-case s-dashed-words)
  :config
  (make-transform-symbol-at-point-defun s-lower-camel-case)
  (make-transform-symbol-at-point-defun s-upper-camel-case)
  (make-transform-symbol-at-point-defun s-snake-case)
  (make-transform-symbol-at-point-defun s-dashed-words))

(use-package tool-bar
  :defer t
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :config (scroll-bar-mode -1))

(use-package menu-bar
  :bind ("s-w" . kill-this-buffer))

(use-package novice
  :defer t
  :config (setq disabled-command-function nil))

(use-package advice
  :defer t
  :config (setq ad-redefinition-action 'accept))

(use-package cus-edit
  :defer t
  :init (setq custom-file (locate-user-emacs-file "custom.el"))
  :config (load custom-file 'no-error 'no-message))

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
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-hook 'comint-mode-hook #'process-output-scrolling)
  (add-to-list 'comint-preoutput-filter-functions #'improve-npm-process-output)
  (bind-keys :map comint-mode-map
             ("s-K"       . comint-clear-buffer)
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
  (add-hook 'compilation-mode-hook #'process-output-scrolling)
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

(use-package projector
  :ensure t
  :bind* (("C-x RET" . projector-run-shell-command-project-root)
          ("C-x m"   . projector-switch-to-or-create-project-shell))
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

;;;;; Files & History

(use-package image-mode
  :defer t
  :config
  (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package files
  :defer t
  :chords (";f" . find-file)
  :config
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
  :config
  (recentf-mode)
  (setq recentf-exclude '(".ido.last")
        recentf-max-saved-items 1000))

(use-package dired
  :defer t
  :init
  (use-package dired-x)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-use-ls-dired nil
        dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

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

;;;;; Editing

(use-package newcomment
  :bind ("s-/" . comment-or-uncomment-region)
  :config
  (advice-add 'comment-or-uncomment-region :before #'with-region-or-line))

(use-package simple
  :bind ("s-k" . kill-whole-line)
  :config
  (hook-modes writing-modes
    (auto-fill-mode)
    (visual-line-mode))
  (advice-add 'yank :after #'maybe-indent-afterwards)
  (advice-add 'yank-pop :after #'maybe-indent-afterwards)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'backward-kill-word :around #'backward-delete-subword)
  (advice-add 'kill-whole-line :after #'back-to-indentation)
  (advice-add 'kill-line :around #'kill-line-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation))

(use-package indent
  :defer t
  :init
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
  :config
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))))

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
         ("C-c s-d" . mc/mark-all-like-this-dwim)))

(use-package toggle-quotes
  :ensure t
  :bind ("C-'" . toggle-quotes))

(use-package flyspell
  :config
  (hook-modes writing-modes
    (flyspell-mode)))

;;;;; Completion

(use-package ido
  :config
  (use-package ido-ubiquitous
    :ensure t
    :config
    (ido-ubiquitous-mode))
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode)
    (setq flx-ido-use-faces nil))
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
  (use-package ido-exit-target
    :ensure t
    :config
    (bind-key "<s-return>" #'ido-exit-target-other-window ido-common-completion-map))
  (ido-mode)
  (bind-keys :map ido-completion-map
             ("M-TAB"     . previous-history-element)
             ("<M-S-tab>" . next-history-element))
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((matching-buffers (--filter
                             (eq major-mode (with-current-buffer it major-mode))
                             (buffer-list))))
      (flet ((buffer-list () matching-buffers))
        (try-expand-dabbrev-all-buffers old))))
  (defun try-expand-dabbrev-other-buffers (old)
    (let ((matching-buffers (--reject
                             (eq major-mode (with-current-buffer it major-mode))
                             (buffer-list))))
      (flet ((buffer-list () matching-buffers))
        (try-expand-dabbrev-all-buffers old))))
  (defun hippie-expand-case-sensitive (orig-fun &rest args)
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  (defun hippie-expand-ruby-symbols (orig-fun &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply orig-fun args)))
      (apply orig-fun args)))
  (defun hippie-expand-inhibit-message-in-minibuffer (orig-fun &rest args)
    (let (inhibit-message (minibufferp))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)
  (advice-add 'hippie-expand :around #'hippie-expand-inhibit-message-in-minibuffer)
  (bind-key "TAB" #'hippie-expand read-expression-map)
  (bind-key "TAB" #'hippie-expand minibuffer-local-map)
  (bind-key* "M-?" (make-hippie-expand-function '(try-expand-line) t))
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

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-require-match nil
        company-minimum-prefix-length 2
        company-idle-delay 0.4
        company-show-numbers t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands '(comint-previous-matching-input-from-input
                                            comint-next-matching-input-from-input)))
  (use-package company-flx
    :ensure t
    :config
    (company-flx-mode))
  (use-package company-dabbrev
    :config
    (setq company-dabbrev-minimum-length 2))
  (use-package company-emoji
    :ensure t
    :config
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    (hook-modes writing-modes
      (setq-local company-backends (append '(company-emoji) company-backends))))
  (use-package company-dabbrev-code
    :config
    (setq company-dabbrev-code-modes t
          company-dabbrev-code-everywhere t))
  (use-package readline-complete
    :ensure t
    :config
    (add-λ 'comint-mode-hook
      (setq-local company-backends (append '(company-readline) company-backends)))))

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
           (":W" . delete-other-windows)))

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
  :bind-keymap ("s-p" . projectile-command-map)
  :bind ("s-t" . projectile-find-file)
  :chords (";t" . projectile-find-file)
  :init
  (setq projectile-enable-caching t
        projectile-tags-command "ripper-tags -R -f TAGS")
  :config
  (make-projectile-switch-project-defun projectile-vc)
  (make-projectile-switch-project-defun projectile-find-file)
  (make-projectile-switch-project-defun projector-run-shell-command-project-root)
  (make-projectile-switch-project-defun projector-switch-to-or-create-project-shell)
  (use-package projectile-rails
    :ensure t
    :config
    (add-hook 'projectile-mode-hook #'projectile-rails-on))
  (projectile-global-mode)
  (projectile-cleanup-known-projects))

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
  (bind-key "," #'pad-comma org-mode-map))

(use-package org-autolist
  :ensure t
  :config (add-hook 'org-mode-hook #'org-autolist-mode))

(use-package org-repo-todo
  :ensure t
  :bind (("s-`" . ort/goto-todos)
         ("s-n" . ort/capture-checkitem))
  :config
  (make-projectile-switch-project-defun ort/capture-todo)
  (make-projectile-switch-project-defun ort/goto-todos))

(use-package sgml-mode
  :mode (("\\.hbs\\'"        . html-mode)
         ("\\.handlebars\\'" . html-mode))
  :chords ("<>" . sgml-close-tag)
  :config
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (add-hook 'sgml-mode #'sgml-electric-tag-pair-mode)
  (use-package handlebars-sgml-mode
    :ensure t
    :config (handlebars-use-mode 'global))
  (bind-keys :map html-mode-map
             ("," . pad-comma)
             ("<C-return>" . html-smarter-newline)))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2))

(use-package fountain-mode
  :ensure t
  :mode "\\.fountain$")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (bind-key "," #'pad-comma markdown-mode-map)
  (setq markdown-command "marked"
        markdown-indent-on-enter nil))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  :init
  (use-package less-css-mode
    :ensure t
    :mode "\\.less\\.erb\\'"
    :init (setq less-css-lessc-options '("--no-color" "-x")))
  :config
  (add-hook 'css-mode-hook #'css-imenu-generic-expression)
  (setq css-indent-offset 2)
  (bind-keys :map css-mode-map
             (":" . smart-css-colon)
             ("," . pad-comma)
             ("{" . open-brackets-newline-and-indent)))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.es6$"  . js2-mode))
  :interpreter (("node" . js2-mode))
  :config
  (setq js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-basic-offset 2)
  (bind-keys :map js2-mode-map
             ("," . pad-comma)
             ("=" . pad-equals)
             (":" . smart-js-colon))
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
  (setq coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header")
  (bind-keys :map coffee-mode-map
             (","          . pad-comma)
             ("="          . pad-equals)
             ("<C-return>" . coffee-smarter-newline)
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
  :config
  (bind-keys :map ruby-mode-map
             (","          . pad-comma)
             ("="          . pad-equals)
             (":"          . smart-ruby-colon)
             ("<C-return>" . ruby-newline-dwim))
  (setenv "RIPPER_TAGS_EMACS" "1")
  (use-package ruby-tools :ensure t)
  (use-package rspec-mode :ensure t)
  (use-package inf-ruby
    :ensure t
    :init
    (add-λ 'inf-ruby-mode-hook
      (turn-on-comint-history ".pry_history"))
    (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
    (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map))
  (use-package bundler
    :ensure t
    :config
    (bind-key "G" #'bundle-open projectile-rails-command-map))
  (use-package chruby
    :ensure t
    :init
    (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding)
    (bind-key "V" #'chruby-use-corresponding projectile-rails-command-map)
    (advice-add 'inf-ruby-console-auto :before #'chruby-use-corresponding))
  (use-package ruby-hash-syntax
    :ensure t
    :init
    (bind-key "C-c C-:" #'ruby-toggle-hash-syntax ruby-mode-map)))

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
  :config
  (use-package magit-gh-pulls
    :ensure t
    :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
  (bind-key "C-c C-a" #'magit-just-amend magit-mode-map)
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)
  (add-hook 'magit-process-mode-hook #'process-output-scrolling)
  (magit-define-popup-action 'magit-dispatch-popup ?x "Reset" 'magit-reset ?!)
  (setq git-commit-summary-max-length git-commit-fill-column
        magit-revert-buffers 2
        magit-completing-read-function 'magit-ido-completing-read
        magit-push-always-verify nil
        magit-log-auto-more t
        magit-repository-directories (funcall #'projectile-relevant-known-git-projects)
        magit-no-confirm t))

(use-package git-messenger
  :ensure t
  :config (setq git-messenger:show-detail t))

(use-package diff-hl
  :ensure t
  :config
  (use-package diff-hl-flydiff
    :init (setq diff-hl-flydiff-delay 3))
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

;;;;; Help & Docs

(use-package find-func
  :config (find-function-setup-keys))

(use-package elisp-slime-nav
  :ensure t
  :config
  (hook-modes lispy-modes
    (elisp-slime-nav-mode)))

(use-package github-browse-file
  :load-path "lib/github-browse-file/")

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
  (setq-default flycheck-disabled-checkers
                '(html-tidy javascript-jshint emacs-lisp-checkdoc)
                flycheck-idle-change-delay 1
                flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Bindings & Chords

(use-package key-chord
  :defer t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.05)
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil)))

(use-package free-keys
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(bind-keys
 ("<M-S-up>"   . move-text-up)
 ("<M-S-down>" . move-text-down)
 ("M-\\"       . align-regexp)
 ("s-K"        . hemacs-delete)
 ("s-["        . shift-left)
 ("s-]"        . shift-right)
 ("s-u"        . duplicate-dwim)
 ("<s-return>" . eol-then-newline)
 ("s-,"        . find-user-init-file-other-window)
 ("s-N"        . create-scratch-buffer)
 ("s-y"        . company-kill-ring)
 ("s-S"        . rename-file-and-buffer)
 ("<f5>"       . toggle-transparency))

(bind-chords
 ("}|" . pad-pipes)
 ("{}" . open-brackets-newline-and-indent)
 ("[]" . pad-brackets)
 ("_+" . insert-fat-arrow)
 ("-=" . insert-arrow)
 ("^^" . insert-λ)
 ("::" . insert-two-colons)
 ("qq" . log-statement)
 (":S" . recentf-find-file)
 (";w" . toggle-split-window))

(bind-keys
 :prefix-map hemacs-help-map
 :prefix "s-h"
 ("k" . describe-personal-keybindings)
 ("K" . free-keys)
 ("f" . what-face)
 ("m" . discover-my-major)
 ("g" . google)
 ("d" . dash-at-point)
 ("D" . dash-at-point-with-docset)
 ("F" . browse-file-directory)
 ("i" . insert-local-ip-address)
 ("o" . open-package))

(bind-keys
 :prefix-map hemacs-projectile-map
 :prefix "s-o"
 ("m" . projectile-switch-project-projectile-vc)
 ("f" . projectile-switch-project-projectile-find-file)
 ("c" . projectile-switch-project-projector-run-shell-command-project-root)
 ("x" . projectile-switch-project-projector-switch-to-or-create-project-shell)
 ("n" . projectile-switch-project-ort/capture-todo)
 ("`" . projectile-switch-project-ort/goto-todos))

(bind-keys
 :prefix-map hemacs-symbol-at-point-map
 :prefix "s-;"
 ("w" . kill-symbol-at-point)
 ("k" . delete-symbol-at-point)
 ("d" . mc/mark-all-like-this)
 ("q" . anzu-query-replace-at-cursor)
 ("c" . s-lower-camel-case-symbol-at-point)
 ("C" . s-upper-camel-case-symbol-at-point)
 ("_" . s-snake-case-symbol-at-point)
 ("-" . s-dashed-words-symbol-at-point))

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

(bind-keys :map minibuffer-local-map
           ("<escape>"  . abort-recursive-edit)
           ("M-TAB"     . previous-complete-history-element)
           ("<M-S-tab>" . next-complete-history-element))

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
        beacon-dont-blink-commands '(next-line previous-line)))

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
  :config (setq alert-default-style 'notifier))

(use-package auto-dim-other-buffers
  :ensure t
  :config (auto-dim-other-buffers-mode))

(use-package fringe
  :defer t
  :config (fringe-mode '(20 . 8)))

(use-package highlight-tail
  :ensure t
  :config
  (setq highlight-tail-steps 8
        highlight-tail-timer 0.05))

(use-package custom
  :defer t
  :init
  (advice-add 'load-theme :after #'refresh-themed-packages-when-idle))

(use-package apropospriate-theme
  :ensure t
  :init
  (load-theme 'apropospriate-dark t))
