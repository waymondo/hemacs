(bind-key "TAB" 'tab-dwim)

(bind-key "C-`" 'list-processes)

(bind-key "C-M-," 'crab-reload)
(bind-key "C-M-;" 'crab-eval-coffee)

(bind-key "C-x \\" 'align-regexp)
(bind-key "C-x w" 'what-face)

(bind-key "<M-up>" 'move-line-up)
(bind-key "<M-down>" 'move-line-down)

(bind-key "<s-up>" 'increment-number-at-point)
(bind-key "<s-down>" 'decrement-number-at-point)

(bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)

(bind-key "s-:" 'pad-colon)
(bind-key "s-l" 'goto-line-with-feedback)
(bind-key "s-r" 'rename-current-file-or-buffer)
(bind-key "s-u" 'duplicate-dwim)
(bind-key "s-k" 'kill-whole-line)
(bind-key "s-q" 'query-replace)
(bind-key "s-]" 'shift-right)
(bind-key "s-[" 'shift-left)
(bind-key "s-s" 'save-buffer)
(bind-key "s-w" 'kill-this-buffer)
(bind-key "s-_" 'text-scale-decrease)
(bind-key "s-+" 'text-scale-increase)
(bind-key "s-/" 'comment-or-uncomment-region)

(bind-key "C-a" 'back-to-indentation-or-beginning)
(bind-key "C-o" 'smart-open-line-above)
(bind-key "C-l" 'log-statement)

(bind-key "C-c u" 'browse-url-at-point)
(bind-key "C-c o" 'ffap)

(bind-key "C-x m" 'eshell)

(bind-key "<s-return>" 'smart-open-line)
(bind-key "<C-return>" 'newline-dwim)
(bind-key "<C-return>" 'ruby-smart-newline-end-defun ruby-mode-map)

(bind-key "M--" (λ (replace-region-or-symbol-at-point-with 's-dashed-words)))
(bind-key "M-_" (λ (replace-region-or-symbol-at-point-with 's-snake-case)))
(bind-key "M-c" (λ (replace-region-or-symbol-at-point-with 's-lower-camel-case)))
(bind-key "M-C" (λ (replace-region-or-symbol-at-point-with 's-upper-camel-case)))

(bind-key "C-x C-k" 'delete-current-buffer-file)
(bind-key "C-x C-k" 'magit-kill-file-on-line magit-mode-map)
(bind-key "C-x C-k" 'dired-do-delete dired-mode-map)

(bind-key "M-TAB" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" 'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input inf-ruby-mode-map)

(bind-key "s-o" 'projectile-switch-project-vc)

(bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
(bind-key "C-c C-p" 'magit-pull-request-for-issue-number magit-mode-map)

(bind-key "C-c e" 'eval-region-and-maybe-deactivate-mark emacs-lisp-mode-map)

(bind-key "s-k" 'clear-shell comint-mode-map)
