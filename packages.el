(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (exec-path-from-shell-initialize)
    (--each '("HISTFILE" "NODE_PATH" "SSL_CERT_FILE")
      (exec-path-from-shell-copy-env it))))

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
    (define-key subword-mode-map [remap backward-kill-word] 'subword-backward-delete)))

(use-package hippie-exp
  :init
  (progn
    (global-set-key [remap dabbrev-expand] #'hippie-expand)
    (setq hippie-expand-verbose nil)
    (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                             try-expand-dabbrev
                                             try-expand-dabbrev-matching-buffers
                                             try-expand-dabbrev-other-buffers
                                             try-expand-dabbrev-from-kill
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
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    (setq dired-auto-revert-buffer t)
    (when (and (memq window-system '(mac ns)) (executable-find "gls"))
      (setq insert-directory-program "gls" dired-use-ls-dired t))))

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
  :init (smex-initialize))

(use-package dash-at-point
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
  :init (crab-server-start))

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
  :init
  (progn
    (use-package projectile-rails
      :init (add-hook 'projectile-mode-hook 'projectile-rails-on))
    (projectile-global-mode)))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package sgml-mode)

(use-package handlebars-mode)

(use-package fountain-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
    (add-hook 'fountain-mode-hook 'hemacs-writing-hook)))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'hemacs-writing-hook))

(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package less-css-mode
  :mode ("\\.scss$" . less-css-mode))

(use-package js
  :mode ("\\.json$" . js-mode)
  :interpreter ("node" . js-mode)
  :init
  (setq-default js-indent-level 2))

(use-package coffee-mode
  :config
  (progn
    (setq coffee-args-repl '("-i" "--nodejs"))
    (add-to-list 'auto-mode-alist '("\\.coffee\\.*" . coffee-mode))
    (add-λ 'coffee-mode-hook (modify-syntax-entry ?\@ "_"))))

(use-package slim-mode
  :config
  (progn
    (setq slim-backspace-backdents-nesting nil)
    (add-λ 'slim-mode-hook (modify-syntax-entry ?\= "."))))

(use-package ruby-mode
  :init
  (progn
    (use-package rspec-mode)
    (use-package robe
      :init (add-hook 'ruby-mode-hook 'robe-mode))
    (use-package inf-ruby
      :init (add-λ 'inf-ruby-mode-hook
              (turn-on-comint-history "~/.irb_history")))
    (use-package ruby-hash-syntax)
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
  :init
  (progn
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
      :init (remove-hook 'magit-status-mode-hook 'magit-filenotify-mode))))

(use-package git-timemachine)

(use-package git-messenger
  :config (setq git-messenger:show-detail t))

(use-package projector
  :config
  (progn
    (setq projector-projects-root hemacs-code-dir)
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
    (setq ido-create-new-buffer 'always)))

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
    (add-λ 'prog-mode-hook
      (highlight-symbol-mode)))
  :config (setq highlight-symbol-idle-delay 0))

(use-package color-identifiers-mode
  :init (global-color-identifiers-mode t))

(use-package volatile-highlights
  :init (volatile-highlights-mode t))

(use-package highlight-tail
  :init
  (progn
    (setq highlight-tail-timer 0.02)
    (eval-after-init
     (run-at-time 2 nil 'highlight-tail-mode))))

(use-package rainbow-mode
  :init
  (dolist (hook '(css-mode-hook emacs-lisp-mode-hook))
    (add-hook hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package god-mode
  :init
  (progn
    (god-mode)
    (add-λ 'god-mode-enabled-hook
      (setq cursor-type 'box))
    (add-λ 'god-mode-disabled-hook
      (setq cursor-type 'bar))
    (dolist (mode '(git-commit-mode))
      (add-to-list 'god-exempt-major-modes mode))))

(use-package guide-key
  :init (guide-key-mode 1)
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-x r" "C-x 4" "C-x x" "C-x v" "C-c r"
            "C-c p" "C-x +" "C-c ," "C-h" "M-s"))
    (setq guide-key/popup-window-position 'bottom)
    (setq guide-key/idle-delay 0.5)))

(use-package discover-my-major)

(use-package jump-char
  :config (setq jump-char-lazy-highlight-face nil))

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package anzu
  :init (global-anzu-mode 1))

(use-package ace-jump-char)

(use-package expand-region)

(use-package multiple-cursors)

(use-package change-inner)

(use-package popup-kill-ring)

(use-package org-repo-todo)

(use-package easy-kill
  :init
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark)))

(use-package misc)

(use-package ace-jump-buffer)

(use-package imenu-anywhere)

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
    (key-chord-define-global "xz" 'smex)

    (key-chord-define-global ";a" 'ace-jump-buffer)
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
    (use-package readline-complete
      :init (push 'company-readline company-backends)
      :config (add-λ 'rlc-no-readline-hook
                (company-mode -1)))
    (use-package company-inf-ruby
      :init (push 'company-inf-ruby company-backends))
    (push 'company-robe company-backends)))
