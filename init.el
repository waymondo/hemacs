;;; hemacs --- an emacs configuration -*- lexical-binding: t; -*-

;;;;; Bootstrap

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'use-package)
(use-package pallet :init (pallet-mode))
(use-package noflet)
(use-package names-dev)
(use-package s)
(use-package misc)
(use-package dash :config (dash-enable-font-lock))
(use-package tool-bar :init (tool-bar-mode -1))
(use-package scroll-bar :init (scroll-bar-mode -1))
(use-package novice :config (setq disabled-command-function nil))
(use-package auto-compile :init (auto-compile-on-load-mode))
(load (locate-user-emacs-file "hemacs.el"))

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
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      echo-keystrokes 0.1
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
              cursor-in-non-selected-windows nil)

;;;;; Unprovided Internal Packages

(defalias 'yes-or-no-p #'y-or-n-p)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message "")

;;;;; Load Personal Hemacs Library

(use-package hemacs
  :init
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
  (make-transform-symbol-at-point-defun s-dashed-words))

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))

(use-package comint
  :init
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
  :init
  (setq compilation-disable-input t
        compilation-always-kill t)
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package shell
  :init
  (setq async-shell-command-buffer 'new-buffer
        shell-command-switch "-ic"
        explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package sh-script
  :config (setq-default sh-indentation 2
                        sh-basic-offset 2)
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)))

(use-package executable
  :init
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package projector
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
  :init (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package files
  :mode ("Cask" . emacs-lisp-mode)
  :config
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate compile)
    (noflet ((process-list ())) ad-do-it))
  (setq require-final-newline t
        confirm-kill-emacs nil
        confirm-nonexistent-file-or-buffer nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package savehist
  :init (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring comint-input-ring)
        savehist-autosave-interval 30))

(use-package saveplace
  :config (setq-default save-place t))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-exclude '(".ido.last" "COMMIT_EDITMSG")
        recentf-max-saved-items 1000))

(use-package dired
  :init
  (use-package dired-x)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

;;;;; Editing

(use-package simple
  :init
  (defadvice list-processes (after goto-process-list activate compile)
    (pop-to-buffer "*Process List*"))
  (defadvice backward-kill-word (around backward-kill-subword activate compile)
    (noflet ((kill-region (beg end) (delete-region beg end)))
      ad-do-it))
  (defadvice kill-whole-line (after kill-whole-lines-back-to-indentation activate compile)
    (back-to-indentation))
  (defadvice kill-line (around kill-or-join-line activate compile)
    (if (not (eolp))
        ad-do-it
      (forward-line)
      (join-line)))
  (defadvice move-beginning-of-line
      (around move-beginning-of-line-or-indentation activate compile)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        ad-do-it))))

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

(use-package hungry-delete
  :init (global-hungry-delete-mode))

(use-package expand-region
  :bind* ("C-," . er/expand-region)
  :init
  (use-package change-inner
    :bind (("M-i" . change-inner)
           ("M-o" . change-outer))))

(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode)
  :config
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

(use-package smart-newline
  :init
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))))

(use-package paredit
  :init
  (hook-modes lispy-modes
    (enable-paredit-mode)))

;;;;; Completion

(use-package ido
  :init (ido-mode)
  (use-package ido-ubiquitous
    :init (ido-ubiquitous-mode))
  (use-package flx-ido
    :init (flx-ido-mode)
    :config (setq flx-ido-use-faces nil))
  (use-package ido-vertical-mode
    :init (ido-vertical-mode)
    :config
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
  :config
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always))

(use-package hippie-exp
  :init
  (defadvice hippie-expand (around hippie-expand-case-fold activate compile)
    (let ((case-fold-search nil))
      ad-do-it))
  (defalias 'he-dabbrev-beg 'hemacs-dabbrev-beg)
  (bind-key [remap dabbrev-expand] #'hippie-expand)
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
  :init
  (global-company-mode)
  :config
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
    :init (push 'company-readline company-backends)))

;;;;; Navigation & Search

(use-package ag
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t))

(use-package anzu
  :bind (("s-q" . anzu-query-replace)
         ("M-q" . anzu-query-replace-at-cursor))
  :init (global-anzu-mode))

(use-package imenu
  :init
  (use-package imenu-anywhere
    :bind (("s-r" . imenu-anywhere)))
  (add-hook 'emacs-lisp-mode-hook #'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t))

(use-package ace-jump-buffer
  :init
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode)))))

(use-package projectile
  :bind (("s-p" . projectile-commander))
  :idle (projectile-cleanup-known-projects)
  :config
  (setq projectile-enable-caching t
        projectile-tags-command "ripper-tags -R -f TAGS")
  :init
  (projectile-global-mode)
  (use-package projectile-rails
    :init (add-hook 'projectile-mode-hook #'projectile-rails-on)))

(use-package helm-swoop
  :bind ("s-f" . helm-swoop)
  :config
  (bind-key "s-f" #'helm-swoop-from-isearch isearch-mode-map)
  (bind-key "s-f" #'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
  (setq helm-multi-swoop-edit-save t
        helm-swoop-use-line-number-face t
        helm-swoop-pre-input-function (lambda ())))

(use-package smex
  :bind (([remap execute-extended-command] . smex)
         ("s-P" . smex))
  :init
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package iflipb
  :bind (("s-{" . iflipb-previous-buffer)
         ("s-}" . iflipb-next-buffer))
  :config
  (setq iflipb-wrap-around t))

;;;;; External Utilities

(use-package edit-server
  :init (edit-server-start)
  :config (setq edit-server-new-frame nil
                edit-server-default-major-mode 'gfm-mode))

(use-package crab
  :bind (("s-R" . crab-reload)
         ("<S-s-left>" . crab-prev-tab)
         ("<S-s-right>" . crab-next-tab))
  :idle (crab-server-start))

;;;;; Major Modes

(use-package org
  :config
  (setq org-support-shift-select t
        org-completion-use-ido t))

(use-package sgml-mode
  :init
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (bind-key "<C-return>" #'html-smarter-newline html-mode-map)
  (make-beautify-defun "html"))

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2))

(use-package fountain-mode
  :mode "\\.fountain$")

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-command "marked"))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  :config
  (use-package less-css-mode
    :mode "\\.less\\.erb\\'")
  (bind-key "{" #'open-brackets-newline-and-indent css-mode-map)
  (setq css-indent-offset 2)
  (make-beautify-defun "css"))

(use-package js
  :mode (("\\.bowerrc$" . js-mode)
         ("\\.es6$"     . js-mode))
  :config
  (make-beautify-defun "js")
  (setq-default js-indent-level 2))

(use-package coffee-mode
  :mode "\\.coffee\\.*"
  :ensure coffee-mode
  :config
  (setq coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header")
  (bind-key "<C-return>" #'coffee-smarter-newline coffee-mode-map)
  (bind-key "C-c C-c" #'coffee-compile-region coffee-mode-map))

(use-package slim-mode
  :config
  (setq slim-backspace-backdents-nesting nil)
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))

(use-package ruby-mode
  :bind ("C-'" . ruby-toggle-string-quotes)
  :mode
  (("Procfile$" . ruby-mode)
   ("Appraisals$" . ruby-mode)
   ("\\.rabl$" . ruby-mode))
  :init
  (bind-key "<C-return>" #'ruby-smarter-newline ruby-mode-map)
  (setenv "RIPPER_TAGS_EMACS" "1")
  (use-package inf-ruby
    :init
    (add-λ 'inf-ruby-mode-hook
      (turn-on-comint-history ".pry_history")))
  (use-package bundler
    :init
    (each-mode-map ruby-modes
      (bind-key "s-b" #'bundle-open mode-map)))
  (use-package ruby-tools)
  (use-package chruby
    :init
    (add-hook 'projectile-switch-project-hook #'chruby-use-corresponding))
  (use-package ruby-hash-syntax
    :init
    (each-mode-map ruby-modes
      (bind-key "C-c C-:" #'ruby-toggle-hash-syntax mode-map))))

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package vc-hooks
  :config (setq vc-follow-symlinks t))

(use-package magit
  :bind ("s-m" . magit-status)
  :config
  (bind-key "C-c C-a" #'magit-just-amend magit-mode-map)
  (bind-key "C-c C-p" #'magit-pull-request-for-issue-number magit-mode-map)
  (defadvice magit-process-sentinel (around alert-process-message (process event) activate compile)
    (let* ((buf (process-get process 'command-buf))
           (buff-name (buffer-name buf)))
      (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
        (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
      ad-do-it))
  (setq git-commit-summary-max-length 72
        magit-completing-read-function 'magit-ido-completing-read
        magit-log-auto-more t
        magit-repository-directories '("~/code")
        magit-no-confirm t))

(use-package git-messenger
  :config (setq git-messenger:show-detail t))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

;;;;; Help & Docs

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

(use-package dash-at-point
  :config
  (setq dash-at-point-docsets
        '("coffee" "lisp" "css" "less" "html" "javascript" "iphoneos" "ruby" "elisp"
          "jquery" "rails" "underscore" "backbone" "bootstrap" "markdown" "zepto"
          "angularjs" "psql" "emacs" "fa" "redis" "git" "bash" "moment")))

(use-package popup
  :commands popup-tip)

(use-package discover
  :init (global-discover-mode))

(use-package guide-key
  :init (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-c" "C-h" "s-h" "s-g")
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom))

(use-package flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  (setq-default flycheck-disabled-checkers '(html-tidy))
  (setq-default flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Bindings & Chords

(use-package god-mode
  :bind ("<escape>" . god-local-mode)
  :config
  (bind-key "." #'repeat god-local-mode-map)
  (add-λ 'god-local-mode-hook
    (when (region-active-p) (call-interactively 'kill-region)))
  (add-λ 'god-mode-enabled-hook
    (setq cursor-type 'box))
  (add-λ 'god-mode-disabled-hook
    (setq cursor-type 'bar)))

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (add-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil))
  (key-chord-define-global ",." "<>\C-b")
  (key-chord-define-global "}|" "||\C-b")
  (key-chord-define-global "<>" #'sgml-close-tag)
  (key-chord-define-global "{}" #'open-brackets-newline-and-indent)
  (key-chord-define-global "[]" #'pad-brackets)
  (key-chord-define-global "_+" #'insert-fat-arrow)
  (key-chord-define-global "-=" #'insert-arrow)
  (key-chord-define-global "^^" (λ (insert "λ")))
  (key-chord-define-global "qq" #'log-statement)
  (key-chord-define-global "fp" #'ffap)
  (key-chord-define-global ";a" #'ace-jump-buffer)
  (key-chord-define-global ":A" #'ace-jump-buffer-other-window)
  (key-chord-define-global ";s" #'projectile-recentf)
  (key-chord-define-global ":S" #'ido-switch-buffer-other-window)
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
  (key-chord-define-global "jj" #'ace-jump-char-mode)
  (key-chord-define-global "jk" #'ace-jump-word-mode)
  (key-chord-define-global "jl" #'ace-jump-line-mode)
  (key-chord-define-global "jz" #'ace-jump-zap-up-to-char)
  (setq key-chord-two-keys-delay 0.05))

(bind-keys
 ("C-z"        . zap-up-to-char)
 ("C-`"        . list-processes)
 ("TAB"        . tab-dwim)
 ("<M-up>"     . evil-numbers/inc-at-pt)
 ("<M-down>"   . evil-numbers/dec-at-pt)
 ("<M-S-up>"   . move-line-up)
 ("<M-S-down>" . move-line-down)
 ("s-K"        . hemacs-delete)
 ("s-["        . shift-left)
 ("s-]"        . shift-right)
 ("s-:"        . pad-colon)
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
 ("C-x y"      . company-kill-ring)
 ("C-x \\"     . align-regexp)
 ("C-x f"      . ffap)
 ("C-x o"      . browse-file-directory)
 ("s-d"        . mc/mark-next-like-this)
 ("s-D"        . mc/mark-previous-like-this)
 ("C-x s-d"    . mc/mark-all-like-this)
 ("<f5>"       . toggle-transparency))

(bind-keys
 :prefix-map hemacs-help-map
 :prefix "s-h"
 ("k" . describe-personal-keybindings)
 ("f" . free-keys)
 ("F" . what-face)
 ("g" . google)
 ("m" . discover-my-major)
 ("d" . dash-at-point)
 ("D" . dash-at-point-with-docset)
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
 ("c" . s-lower-camel-case-symbol-at-point)
 ("C" . s-upper-camel-case-symbol-at-point)
 ("_" . s-snake-case-symbol-at-point)
 ("-" . s-dashed-words-symbol-at-point))

(bind-keys
 :prefix-map hemacs-git-map
 :prefix "s-g"
 ("o" . github-browse-file)
 ("b" . github-browse-file-blame)
 ("i" . github-browse-new-issue)
 ("g" . gist-region-or-buffer-private)
 ("c" . github-clone)
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
  :init (toggle-frame-fullscreen)
  :config (setq blink-cursor-blinks 0))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

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
            imenu-after-jump-hook)
    (add-hook it #'pulse-line-hook-function)))

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
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :init
  (hook-modes progish-modes
    (rainbow-delimiters-mode)))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (setq powerline-default-separator 'utf-8))

(use-package paren
  :init (show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package alert
  :config (setq alert-default-style 'notifier))

(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

(use-package faces
  :init (set-face-attribute 'default nil :height 150 :font "Meslo LG L DZ for Powerline"))

(use-package fringe
  :init (fringe-mode '(16 . 8)))

(use-package highlight-tail
  :idle (highlight-tail-mode)
  :config (setq highlight-tail-steps 8
                highlight-tail-timer 0.02))

(use-package custom
  :init (load-theme 'hemacs :no-confirm))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
