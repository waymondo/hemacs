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

(defalias 'yes-or-no-p #'y-or-n-p)

;;;;; Personal Variables

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode slim-mode inf-ruby-mode))
(defvar shellish-modes
  '(comint-mode compilation-mode magit-process-mode))
(defvar writing-modes
  '(org-mode markdown-mode fountain-mode git-commit-mode))

;;;;; Bootstrap

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package bind-key :ensure t)
(use-package s :ensure t)
(use-package noflet :ensure t)

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package novice
  :config (setq disabled-command-function nil))

(use-package advice
  :config (setq ad-redefinition-action 'accept))

(use-package cus-edit
  :init (setq custom-file (locate-user-emacs-file "custom.el"))
  :config (load custom-file 'no-error 'no-message))

(use-package "startup"
  :config
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        inhibit-startup-echo-area-message ""))

;;;;; Load Personal Hemacs Library

(use-package hemacs
  :load-path "lib/"
  :config
  (setq kill-buffer-query-functions '(hemacs-kill-buffer-query))
  (hook-modes writing-modes
    (visual-line-mode)
    (flyspell-mode)
    (key-chord-define-local "::" #'company-only-emoji))
  (hook-modes shellish-modes
    (hemacs-shellish-hook))
  (add-hook 'before-save-hook #'hemacs-save-hook)
  (with-region-or-line comment-or-uncomment-region)
  (with-region-or-line upcase-region)
  (with-region-or-line capitalize-region)
  (with-region-or-line downcase-region)
  (with-region-or-line yank-region)
  (with-region-or-line kill-region :point-to-eol)
  (with-region-or-line kill-ring-save :point-to-eol)
  (with-region-or-buffer indent-region)
  (with-region-or-buffer untabify)
  (make-projectile-switch-project-defun projectile-vc)
  (make-projectile-switch-project-defun projectile-find-file)
  (make-projectile-switch-project-defun projector-run-shell-command-project-root)
  (make-projectile-switch-project-defun projector-switch-to-or-create-project-shell)
  (make-projectile-switch-project-defun ort/capture-todo)
  (make-projectile-switch-project-defun ort/goto-todos)
  (make-transform-symbol-at-point-defun s-lower-camel-case)
  (make-transform-symbol-at-point-defun s-upper-camel-case)
  (make-transform-symbol-at-point-defun s-snake-case)
  (make-transform-symbol-at-point-defun s-dashed-words)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'backward-kill-word :around #'backward-delete-subword)
  (advice-add 'kill-whole-line :after #'back-to-indentation)
  (advice-add 'kill-line :around #'kill-line-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'find-file :before #'find-file-maybe-make-directories)
  (advice-add 'save-buffers-kill-emacs :around #'save-buffers-kill-emacs-no-process-query))

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))

(use-package comint
  :config
  (setq comint-prompt-read-only t)
  (setq-default comint-process-echoes t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-to-list 'comint-preoutput-filter-functions #'improve-npm-process-output)
  (add-λ 'kill-emacs-hook
    (--each (buffer-list)
      (with-current-buffer it (comint-write-input-ring)))))

(use-package compile
  :config
  (setq compilation-disable-input t
        compilation-always-kill t)
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package warnings
  :config
  (setq warning-suppress-types '((undo discard-info))))

(use-package shell
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
  :config
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package projector
  :ensure t
  :bind* (("C-x RET" . projector-run-shell-command-project-root)
          ("C-x m"   . projector-switch-to-or-create-project-shell))
  :config
  (setq projector-always-background-regex
        '("^powder restart\\.*"
          "^heroku restart\\.*"
          "^spring stop"
          "^gulp publish\\.*"
          "^git push\\.*"
          "^redis-server"
          "^pkill\\.*")
        projector-command-modes-alist
        '(("^heroku run console" . inf-ruby-mode))))

;;;;; Files & History

(use-package image-mode
  :config
  (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package elisp-lisp-mode
  :mode ("Cask" . emacs-lisp-mode))

(use-package files
  :config
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
  :config
  (setq-default save-place t))

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-exclude '(".ido.last")
        recentf-max-saved-items 1000))

(use-package dired
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
  (add-hook 'midnight-hook #'recentf-save-list)
  (setq midnight-period 10000))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package saveplace
  :init (save-place-mode))

;;;;; Editing

(use-package delsel
  :init (delete-selection-mode))

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

(use-package expand-region
  :ensure t
  :commands (er/mark-symbol)
  :bind* ("C-," . er/expand-region))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-;" . ace-jump-mode)
  :config
  (ace-jump-mode-enable-mark-sync)
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

(use-package ace-jump-zap :ensure t)

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
  (ido-mode)
  (advice-add 'ido-read-internal :around #'ido-read-internal-determine-window-target)
  (bind-key "<s-return>" #'ido-select-for-other-window ido-common-completion-map)
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)
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
        company-idle-delay 0.25
        company-show-numbers t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands '(comint-previous-matching-input-from-input
                                            comint-next-matching-input-from-input)))
  (bind-key "<tab>" #'company-complete-common-or-cycle company-active-map)
  (use-package company-dabbrev
    :config
    (setq company-dabbrev-minimum-length 2))
  (use-package company-emoji
    :ensure t
    :config
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
  (use-package company-dabbrev-code
    :config
    (setq company-dabbrev-code-modes t
          company-dabbrev-code-everywhere t))
  (use-package readline-complete
    :ensure t
    :config (push #'company-readline company-backends)))

(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode)
  (setq smart-tab-using-hippie-expand t
        smart-tab-completion-functions-alist '()))

;;;;; Navigation & Search

(use-package wgrep-ag
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package ag
  :ensure t
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
  :init
  (add-hook 'emacs-lisp-mode-hook #'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t)
  (use-package imenu-anywhere :ensure t))

(use-package ace-jump-buffer
  :ensure t
  :config
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode))))
  (setq ajb-home-row-keys t))

(use-package projectile
  :ensure t
  :bind-keymap ("s-p" . projectile-command-map)
  :bind ("s-t" . projectile-find-file)
  :init
  (setq projectile-enable-caching t
        projectile-tags-command "ripper-tags -R -f TAGS")
  :config
  (use-package projectile-rails
    :ensure t
    :config
    (add-hook 'projectile-mode-hook #'projectile-rails-on))
  (projectile-global-mode)
  (projectile-cleanup-known-projects))

(use-package swiper
  :ensure t
  :bind (([remap isearch-forward] . swiper))
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

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

(use-package "text-mode"
  :config (bind-key "," #'pad-comma text-mode-map))

(use-package org
  :init
  (setq org-support-shift-select t
        org-completion-use-ido t)
  (use-package org-repo-todo :ensure t))

(use-package sgml-mode
  :mode (("\\.hbs\\'"        . html-mode)
         ("\\.handlebars\\'" . html-mode))
  :config
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (use-package handlebars-sgml-mode
    :ensure t
    :config (handlebars-use-mode 'global))
  (bind-keys :map html-mode-map
             ("," . pad-comma)
             ("<C-return>" html-smarter-newline)))

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
  (setq markdown-command "marked"))

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

(use-package js
  :config
  (setq-default js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"        . js2-mode)
         ("\\.bowerrc$"     . js2-mode)
         ("\\.json_schema$" . js2-mode)
         ("\\.es6$"         . js2-mode))
  :interpreter (("node" . js2-mode))
  :config
  (setq js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil)
  (bind-keys :map js2-mode-map
             (","     . pad-comma)
             ("="     . pad-equals)
             (":"     . smart-js-colon)
             ("C-c l" . js2-log-arguments))
  (setq-default js2-global-externs
                '("clearTimeout" "setTimeout" "module" "require" "angular" "Ember")))

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :ensure t
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
  (eval-after-load 'js2-mode
    '(bind-key "s-b" #'web-beautify-js js2-mode-map))
  (eval-after-load 'sgml-mode
    '(bind-key "s-b" #'web-beautify-html html-mode-map))
  (eval-after-load 'css-mode
    '(bind-key "s-b" #'web-beautify-css css-mode-map)))

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
  (use-package foreman-mode
    :ensure t
    :config
    (bind-key "F" #'foreman projectile-rails-command-map))
  (use-package inf-ruby
    :ensure t
    :init
    (add-λ 'inf-ruby-mode-hook
      (turn-on-comint-history ".pry_history")))
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
    (each-mode-map ruby-modes
      (bind-key "C-c C-:" #'ruby-toggle-hash-syntax mode-map))))

(use-package yaml-mode :ensure t)
(use-package restclient :ensure t)

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :ensure t
  :bind ("s-m" . magit-status)
  :config
  (bind-key "C-c C-a" #'magit-just-amend magit-mode-map)
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)
  (magit-define-popup-action 'magit-dispatch-popup ?x "Reset" 'magit-reset ?!)
  (setq git-commit-summary-max-length 72
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
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
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

(use-package git-timemachine :ensure t)
(use-package gist :ensure t)
(use-package gitattributes-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)

(use-package dash-at-point
  :load-path "lib/dash-at-point/")

(use-package popup
  :ensure t
  :commands popup-tip)

(use-package discover
  :ensure t
  :config (global-discover-mode))

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint emacs-lisp-checkdoc))
                flycheck-html-tidy-executable "tidy5"
                flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Bindings & Chords

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil))
  (key-chord-define-global ",." "<>\C-b")
  (key-chord-define-global "<>" #'sgml-close-tag)
  (key-chord-define-global "}|" #'pad-pipes)
  (key-chord-define-global "{}" #'open-brackets-newline-and-indent)
  (key-chord-define-global "[]" #'pad-brackets)
  (key-chord-define-global "_+" #'insert-fat-arrow)
  (key-chord-define-global "-=" #'insert-arrow)
  (key-chord-define-global "^^" (λ (insert "λ")))
  (key-chord-define-global "::" (λ (insert "::")))
  (key-chord-define-global "qq" #'log-statement)
  (key-chord-define-global "fp" #'ffap)
  (key-chord-define-global ";a" #'ace-jump-buffer)
  (key-chord-define-global ":A" #'ace-jump-buffer-other-window)
  (key-chord-define-global ";x" #'ace-jump-shellish-buffers)
  (key-chord-define-global ";s" #'switch-to-buffer)
  (key-chord-define-global ":S" #'recentf-find-file)
  (key-chord-define-global ";w" #'toggle-split-window)
  (key-chord-define-global ":W" #'delete-other-windows)
  (key-chord-define-global ";f" #'find-file)
  (key-chord-define-global ";t" #'projectile-find-file)
  (key-chord-define-global ";g" #'ag-project)
  (key-chord-define-global ":G" #'ag)
  (key-chord-define-global ";r" #'imenu-anywhere)
  (key-chord-define-global "jj" #'ace-jump-char-mode)
  (key-chord-define-global "jk" #'ace-jump-word-mode)
  (key-chord-define-global "jl" #'ace-jump-line-mode)
  (key-chord-define-global "jz" #'ace-jump-zap-up-to-char)
  (setq key-chord-two-keys-delay 0.05))

(use-package free-keys :ensure t)

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
 ("s-k"        . kill-whole-line)
 ("<s-return>" . eol-then-newline)
 ("s-,"        . find-user-init-file-other-window)
 ("s-`"        . ort/goto-todos)
 ("s-n"        . ort/capture-checkitem)
 ("s-N"        . create-scratch-buffer)
 ("s-w"        . kill-this-buffer)
 ("s-y"        . company-kill-ring)
 ("s-/"        . comment-or-uncomment-region)
 ("s-S"        . rename-file-and-buffer)
 ("<f5>"       . toggle-transparency))

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
 ("o" . open-package)
 ("p" . describe-thing-in-popup))

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
 ("i" . github-browse-new-issue)
 ("r" . github-browse-pull-request)
 ("l" . magit-clone)
 ("g" . gist-region-or-buffer-private)
 ("t" . git-timemachine)
 ("p" . git-messenger:popup-message))

(bind-key "<escape>" #'abort-recursive-edit minibuffer-local-map)
(bind-key "M-TAB" #'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" #'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" #'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" #'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map)
(bind-key "M-TAB" #'previous-history-element ido-completion-map)
(bind-key "<M-S-tab>" #'next-history-element ido-completion-map)

;;;;; Appearance

(use-package ns-win
  :config (setq ns-pop-up-frames nil))

(use-package frame
  :config
  (setq blink-cursor-blinks 0)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
  (when (member "Meslo LG L DZ for Powerline" (font-family-list))
    (set-frame-font "Meslo LG L DZ for Powerline-15")))

(use-package prog-mode
  :config (global-prettify-symbols-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

(use-package pulse
  :config
  (setq pulse-command-advice-flag t
        pulse-delay 0.01
        pulse-iterations 4)
  (funcall-after-commands #'pulse-line-hook-function
    '(scroll-down-command
      scroll-up-command
      next-multiframe-window
      find-tag))
  (--each '(next-error-hook
            find-function-after-hook
            isearch-mode-end-hook
            imenu-after-jump-hook)
    (add-hook it #'pulse-line-hook-function)))

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
  :config (fringe-mode '(20 . 8)))

(use-package highlight-tail
  :ensure t
  :config
  (setq highlight-tail-steps 8
        highlight-tail-timer 0.05))

(use-package custom
  :init
  (advice-add 'load-theme :after #'refresh-themed-packages-when-idle))

(use-package apropospriate-theme
  :ensure t
  :init
  (load-theme 'apropospriate-dark t))
