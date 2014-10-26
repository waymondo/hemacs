;;;;; Bootstrap

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'use-package)
(use-package noflet)
(use-package s)
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
(defvar shellish-modes
  '(comint-mode inf-ruby-mode ielm-mode))

(load (locate-user-emacs-file "defuns.el"))

;;;;; Source Variables

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
      ns-function-modifier 'hyper
      ns-right-option-modifier 'none
      create-lockfiles nil
      kill-buffer-query-functions '(hemacs-kill-buffer-query))

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              indicate-empty-lines t
              left-fringe-width 10
              right-fringe-width 5)

;;;;; Unprovided Internal Packages

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

;;;;; Apply Macros

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

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (--each '("HISTFILE" "NODE_PATH")
    (exec-path-from-shell-copy-env it)))

(use-package comint
  :init
  (setq comint-prompt-read-only t)
  (setq-default comint-process-echoes t)
  (add-to-list 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-mode-hook #'hemacs-shellish-hook))

(use-package compile
  :init
  (setq compilation-disable-input t
        compilation-always-kill t)
  (add-hook 'compilation-mode-hook #'hemacs-shellish-hook)
  (add-hook 'compilation-finish-functions #'alert-after-compilation-finish))

(use-package auto-compile
  :init (auto-compile-on-load-mode)
  :config (setq auto-compile-display-buffer nil))

(use-package shell
  :init
  (setq async-shell-command-buffer 'new-buffer
        shell-command-switch (purecopy "-ic")
        explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

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
  (add-hook 'before-save-hook 'hemacs-save-hook))

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
        recentf-max-saved-items 500))

(use-package dired
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package dired-toggle
  :bind ("s-\\" . dired-toggle)
  :config (setq dired-toggle-window-size 48))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

;;;;; Editing

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
  :bind* ("C-," . er/expand-region))

(use-package ace-jump-mode
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
    :init (flx-ido-mode))
  (use-package ido-vertical-mode
    :init (ido-vertical-mode))
  :config
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-auto-merge-delay-time 2
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always))

(use-package smex
  :bind ("s-P" . smex)
  :init
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (bind-key [remap execute-extended-command] #'smex))

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
  (add-hook 'emacs-lisp-mode-hook 'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t))

(use-package ace-jump-buffer
  :init
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode))))
  (make-ace-jump-buffer-function "magit"
    (with-current-buffer buffer
      (not (derived-mode-p 'magit-mode)))))

(use-package projectile
  :bind (("s-t" . projectile-find-file)
         ("s-p" . projectile-commander))
  :config
  (setq projectile-enable-caching t
        projectile-tags-command "ripper-tags -R -f TAGS")
  :init
  (projectile-global-mode)
  (use-package projectile-rails
    :init (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package swoop
  :config
  (setq swoop-font-size-change: nil
        swoop-window-split-direction: 'split-window-horizontally)
  (bind-key "C-o" 'swoop-from-isearch isearch-mode-map)
  (bind-key "C-o" 'swoop-multi-from-swoop swoop-map)
  (bind-key "C-s" 'swoop-action-goto-line-next swoop-map)
  (bind-key "C-r" 'swoop-action-goto-line-prev swoop-map))

;;;;; External Utilities

(use-package edit-server
  :init (edit-server-start)
  :config (setq edit-server-new-frame nil))

(use-package crab-mode
  :bind (("s-R" . crab-reload)
         ("s-”" . crab-prev-tab)
         ("s-’" . crab-next-tab))
  :idle (crab-server-start))

;;;;; Major Modes

(use-package org
  :config
  (setq org-support-shift-select t
        org-completion-use-ido t))

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

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package git-commit-mode
  :config (setq git-commit-fill-column 90))

(use-package magit
  :bind ("s-m" . magit-status)
  :config
  (bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
  (bind-key "C-c C-p" 'magit-pull-request-for-issue-number magit-mode-map)
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

(use-package git-messenger
  :config (setq git-messenger:show-detail t))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

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

(use-package guide-key
  :init (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r" "C-x" "C-c"
          "C-z" "C-c p" "C-x +" "C-c ," "C-h" "M-s")
        guide-key/popup-window-position 'bottom))

;;;;; Bindings & Chords

(use-package evil
  :init
  (use-package evil-surround
    :init (global-evil-surround-mode))
  (--each '(dired git-commit-mode comint-mode shell-mode org-mode help-mode)
    (evil-set-initial-state it 'emacs))
  (bind-key "Y" "y$" evil-normal-state-map)
  (bind-key "SPC" 'ace-jump-word-mode evil-normal-state-map)
  (bind-key "SPC" 'ace-jump-word-mode evil-visual-state-map)
  (evil-mode)
  :config
  (setq evil-shift-width 2
        evil-move-cursor-back nil
        evil-symbol-word-search t))

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
  (key-chord-define-global "qq" 'log-statement)
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
  (key-chord-define-global "jc" 'ace-jump-char-mode)
  (key-chord-define-global "jk" 'ace-jump-word-mode)
  (key-chord-define-global "jl" 'ace-jump-line-mode)
  (key-chord-define-global "jz" 'ace-jump-zap-up-to-char)
  (key-chord-define-global "zz" 'zap-up-to-char)
  (setq key-chord-two-keys-delay 0.07))

(bind-keys
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
 ("<s-return>" . eol-then-newline)
 ("s-,"        . find-user-init-file-other-window)
 ("s-`"        . ort/goto-todos)
 ("s-n"        . ort/capture-todo)
 ("s-N"        . create-scratch-buffer)
 ("s-w"        . kill-this-buffer)
 ("s-/"        . comment-or-uncomment-region)
 ("s-r"        . imenu-anywhere)
 ("C-\\"       . align-regexp)
 ("C-x C-r"    . rename-file-and-buffer)
 ("C->"        . mc/mark-next-like-this)
 ("C-<"        . mc/mark-previous-like-this)
 ("C-x C-<"    . mc/mark-all-like-this))

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
 :prefix-map hemacs-transform-map
 :prefix "s-;"
 ("c" . s-lower-camel-case-symbol-at-point)
 ("C" . s-upper-camel-case-symbol-at-point)
 ("_" . s-snake-case-symbol-at-point)
 ("-" . s-dashed-words-symbol-at-point))

(bind-keys
 :prefix-map hemacs-git-map
 :prefix "s-g"
 ("o" . github-browse-file)
 ("b" . github-browse-file-blame)
 ("i" . gist-region-or-buffer-private)
 ("c" . github-clone)
 ("t" . git-timemachine)
 ("p" . git-messenger:popup-message))

(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)
(bind-key "M-TAB" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" 'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input inf-ruby-mode-map)
(bind-key "M-TAB" 'previous-history-element ido-completion-map)
(bind-key "<M-S-tab>" 'next-history-element ido-completion-map)

;;;;; Appearance

(use-package ns-win
  :config (setq ns-pop-up-frames nil))

(use-package frame
  :init (toggle-frame-fullscreen)
  :config (setq blink-cursor-blinks 0))

(use-package prog-mode
  :init (global-prettify-symbols-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

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

(use-package powerline
  :init (powerline-default-theme)
  :config
  (setq powerline-default-separator 'utf-8)
  (defpowerline powerline-minor-modes nil))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-style 'mixed))

(use-package alert
  :config (setq alert-default-style 'notifier))

(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

(use-package faces
  :init (set-face-attribute 'default nil :height 150 :font "Meslo LG M DZ for Powerline"))

(use-package custom
  :init (load-theme 'hemacs :no-confirm))
