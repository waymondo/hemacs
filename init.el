;;; hemacs --- an emacs configuration -*- lexical-binding: t; -*-

;;;;; Bootstrap

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package bind-key :ensure t)
(use-package noflet :ensure t)
(use-package s :ensure t)
(use-package dash :ensure t :config (dash-enable-font-lock))
(use-package tool-bar :config (tool-bar-mode -1))
(use-package scroll-bar :config (scroll-bar-mode -1))
(use-package novice :config (setq disabled-command-function nil))
(use-package advice :config (setq ad-redefinition-action 'accept))

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode slim-mode inf-ruby-mode))
(defvar shellish-modes
  '(comint-mode compilation-mode ielm-mode magit-process-mode))
(defvar writing-modes
  '(org-mode markdown-mode fountain-mode))

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

;;;;; Unprovided Internal Packages

(defalias 'yes-or-no-p #'y-or-n-p)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message "")

;;;;; Load Personal Hemacs Library

(use-package hemacs
  :load-path "lib/"
  :config
  (setq kill-buffer-query-functions '(hemacs-kill-buffer-query))
  (hook-modes writing-modes
    (hemacs-writing-hook))
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
  (advice-add 'save-buffers-kill-emacs :around #'save-buffers-kill-emacs-no-process-query)
  (advice-add 'package-install :around #'package-install-never-select))

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
  (add-λ 'kill-emacs-hook
    (--each (buffer-list)
      (with-current-buffer it (comint-write-input-ring)))))

(use-package compile
  :config
  (setq compilation-disable-input t
        compilation-always-kill t)
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

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
        '("^mysql.server\\.*"
          "^powder restart\\.*"
          "^heroku restart\\.*"
          "^spring stop"
          "^gulp publish\\.*"
          "^git push\\.*"
          "\\.*cordova run\\.*"
          "^redis-server"
          "^pkill\\.*")))

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
  :init (use-package dired-x)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-use-ls-dired nil
        dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package midnight
  :config
  (add-hook 'midnight-hook #'recentf-save-list)
  (setq midnight-period 10000))

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

(use-package avy
  :ensure t
  :bind* ("C-;" . avi-goto-word-1)
  :config (setq avi-background t))

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

(use-package lispy
  :disabled t
  :init
  (hook-modes lispy-modes
    (lispy-mode)))

(use-package evil-numbers :ensure t)
(use-package multiple-cursors :ensure t)
(use-package toggle-quotes :ensure t)
(use-package splitjoin :ensure t)

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
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
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
  (use-package company-dabbrev
    :config
    (setq company-dabbrev-minimum-length 2))
  (use-package company-dabbrev-code
    :config
    (setq company-dabbrev-code-modes t
          company-dabbrev-code-everywhere t))
  (use-package readline-complete
    :ensure t
    :config (push 'company-readline company-backends)))

(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode)
  (setq smart-tab-using-hippie-expand t
        smart-tab-completion-functions-alist '())
  (--each shellish-modes
    (push it smart-tab-disabled-major-modes)))

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
  :bind ("s-r" . imenu)
  :init
  (use-package imenu-anywhere
    :ensure t
    :bind ("s-R" . imenu-anywhere))
  (add-hook 'emacs-lisp-mode-hook #'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t))

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
  :config
  (use-package projectile-rails
    :ensure t
    :config (add-hook 'projectile-mode-hook #'projectile-rails-on))
  (setq projectile-enable-caching t
        projectile-tags-command "ripper-tags -R -f TAGS")
  (projectile-global-mode)
  (projectile-cleanup-known-projects))

(use-package swiper
  :ensure t
  :bind (([remap isearch-forward] . swiper)
         ("s-f" . swiper)))

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
  :init
  (setq org-support-shift-select t
        org-completion-use-ido t)
  (use-package org-repo-todo :ensure t))

(use-package sgml-mode
  :config
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (bind-key "," #'pad-comma html-mode-map)
  (bind-key "<C-return>" #'html-smarter-newline html-mode-map)
  (make-beautify-defun "html"))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))
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
    :mode "\\.less\\.erb\\'")
  :config
  (add-hook 'css-mode-hook #'css-imenu-generic-expression)
  (bind-key ":" #'smart-css-colon css-mode-map)
  (bind-key "," #'smart-css-comma css-mode-map)
  (bind-key "{" #'open-brackets-newline-and-indent css-mode-map)
  (setq css-indent-offset 2)
  (make-beautify-defun "css"))

(use-package js
  :mode (("\\.bowerrc$"     . js-mode)
         ("\\.json_schema$" . js-mode)
         ("\\.es6$"         . js-mode))
  :config
  (make-beautify-defun "js")
  (setq-default js-indent-level 2))

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :ensure coffee-mode
  :config
  (setq coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header")
  (bind-key "," #'pad-comma coffee-mode-map)
  (bind-key "=" #'pad-equals coffee-mode-map)
  (bind-key "<C-return>" #'coffee-smarter-newline coffee-mode-map)
  (bind-key "C-c C-c" #'coffee-compile-region coffee-mode-map))

(use-package slim-mode
  :ensure t
  :config
  (setq slim-backspace-backdents-nesting nil)
  (bind-key "," #'pad-comma slim-mode-map)
  (bind-key ":" #'smart-ruby-colon slim-mode-map)
  (bind-key "<C-return>" #'slim-newline-dwim slim-mode-map)
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))

(use-package ruby-mode
  :mode
  (("Appraisals$" . ruby-mode)
   ("\\.rabl\\'" . ruby-mode)
   ("\\.builder\\'" . ruby-mode))
  :config
  (bind-key "," #'pad-comma ruby-mode-map)
  (bind-key "=" #'pad-equals ruby-mode-map)
  (bind-key ":" #'smart-ruby-colon ruby-mode-map)
  (bind-key "<C-return>" #'ruby-newline-dwim ruby-mode-map)
  (setenv "RIPPER_TAGS_EMACS" "1")
  (use-package ruby-tools :ensure t)
  (use-package rspec-mode :ensure t)
  (use-package foreman-mode :ensure t)
  (use-package inf-ruby
    :ensure t
    :init
    (add-λ 'inf-ruby-mode-hook
      (turn-on-comint-history ".pry_history")))
  (use-package bundler
    :ensure t
    :init
    (each-mode-map ruby-modes
      (bind-key "s-b" #'bundle-open mode-map)))
  (use-package chruby
    :ensure t
    :init
    (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding)
    (advice-add 'inf-ruby-console-auto :before #'chruby-use-corresponding))
  (use-package ruby-hash-syntax
    :ensure t
    :init
    (each-mode-map ruby-modes
      (bind-key "C-c C-:" #'ruby-toggle-hash-syntax mode-map))))

(use-package yaml-mode :ensure t)

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :load-path "lib/magit/"
  :bind ("s-m" . magit-status)
  :config
  (bind-key "C-c C-a" #'magit-just-amend magit-mode-map)
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)
  (setq git-commit-summary-max-length 72
        magit-completing-read-function 'magit-ido-completing-read
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
;; (use-package gist :ensure t)
(use-package gitattributes-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)
;; (use-package helm-open-github :ensure t)

(use-package dash-at-point
  :ensure t
  :config
  (setq dash-at-point-docsets
        '("coffee" "lisp" "css" "less" "html" "javascript" "iphoneos" "ruby" "elisp"
          "jquery" "rails" "underscore" "backbone" "bootstrap" "markdown" "zepto"
          "angularjs" "psql" "emacs" "fa" "redis" "git" "bash" "moment")))

(use-package popup
  :ensure t
  :commands popup-tip)

(use-package discover
  :ensure t
  :config
  (global-discover-mode)
  (use-package discover-my-major :ensure t))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-function nil
        flycheck-checkers (--remove (eq it 'emacs-lisp-checkdoc) flycheck-checkers))
  (setq-default flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Bindings & Chords

(use-package free-keys :ensure t)
(use-package helm-descbinds :ensure t)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil))
  (key-chord-define-global "sj" #'splitjoin)
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
  (key-chord-define-global ";s" #'ido-switch-buffer)
  (key-chord-define-global ":S" #'recentf-ido-find-file-other-window)
  (key-chord-define-global ";w" #'toggle-split-window)
  (key-chord-define-global ":W" #'delete-other-windows)
  (key-chord-define-global ";f" #'ido-find-file)
  (key-chord-define-global ":F" #'ido-find-file-other-window)
  (key-chord-define-global ";t" #'projectile-find-file)
  (key-chord-define-global ":T" #'projectile-find-file-other-window)
  (key-chord-define-global ";g" #'ag-project)
  (key-chord-define-global ":G" #'ag)
  (key-chord-define-global ";r" #'imenu-anywhere)
  (key-chord-define-global "jb" #'ace-jump-buffer-with-configuration)
  (key-chord-define-global "jj" #'avi-goto-char)
  (key-chord-define-global "jk" #'avi-goto-word-1)
  (key-chord-define-global "jl" #'avi-goto-line)
  (key-chord-define-global "jz" #'ace-jump-zap-up-to-char)
  (setq key-chord-two-keys-delay 0.05))

(use-package guide-key
  :ensure t
  :config
  (guide-key-mode)
  (setq guide-key/guide-key-sequence
        '("C-x" "C-c" "C-c" "C-h" "s-h" "s-g" "C-." "s-;" "s-p")
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom))

(bind-keys
 ("C-z"        . ace-jump-zap-up-to-char)
 ("C-'"        . toggle-quotes)
 ("C-`"        . list-processes)
 ("<M-up>"     . evil-numbers/inc-at-pt)
 ("<M-down>"   . evil-numbers/dec-at-pt)
 ("<M-S-up>"   . move-text-up)
 ("<M-S-down>" . move-text-down)
 ("M-\\"       . align-regexp)
 ("s-K"        . hemacs-delete)
 ("s-["        . shift-left)
 ("s-]"        . shift-right)
 ("s-u"        . duplicate-dwim)
 ("s-k"        . kill-whole-line)
 ("s-\\"       . dired-jump-other-window)
 ("<s-return>" . eol-then-newline)
 ("s-,"        . find-user-init-file-other-window)
 ("s-`"        . ort/goto-todos)
 ("s-n"        . ort/capture-checkitem)
 ("s-N"        . create-scratch-buffer)
 ("s-w"        . kill-this-buffer)
 ("s-/"        . comment-or-uncomment-region)
 ("s-S"        . rename-file-and-buffer)
 ("s-d"        . mc/mark-next-like-this)
 ("s-D"        . mc/mark-previous-like-this)
 ("C-c s-d"    . mc/mark-all-like-this-dwim)
 ("<f5>"       . toggle-transparency))

(bind-keys
 :prefix-map hemacs-help-map
 :prefix "s-h"
 ("k" . describe-personal-keybindings)
 ("y" . company-kill-ring)
 ("K" . free-keys)
 ("b" . helm-descbinds)
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
 ("C" . helm-open-github-from-commit)
 ("I" . helm-open-github-from-issues)
 ("R" . helm-open-github-from-pull-requests)
 ("g" . gist-region-or-buffer-private)
 ("t" . git-timemachine)
 ("p" . git-messenger:popup-message))

(bind-key "," #'pad-comma text-mode-map)
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
  :init (toggle-frame-fullscreen)
  :config (setq blink-cursor-blinks 0))

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
  (setq highlight-symbol-idle-delay 0))

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

(use-package faces
  :config (set-face-attribute 'default nil :height 150 :font "Meslo LG L DZ for Powerline"))

(use-package writeroom-mode
  :ensure t
  :config
  (global-writeroom-mode)
  (setq writeroom-width 100
        writeroom-global-effects '()
        writeroom-major-modes (append writing-modes '(gfm-mode))))

(use-package fringe
  :config (fringe-mode '(20 . 8)))

(use-package highlight-tail
  :disabled t
  :config
  (setq highlight-tail-steps 8
        highlight-tail-timer 0.02)
  (highlight-tail-mode))

(use-package theme-changer
  :ensure t
  :init
  (use-package solar
    :init
    (setq calendar-location-name "New York, NY"
          calendar-latitude 41.8
          calendar-longitude -73.59))
  :config
  (use-package apropospriate-theme :ensure t)
  (change-theme 'apropospriate-light 'apropospriate-dark))
