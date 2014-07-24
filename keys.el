(bind-key "<escape>" 'god-mode-all)
(bind-key "i" 'kill-region-and-god-local-mode god-local-mode-map)
(bind-key "." 'repeat god-local-mode-map)

(bind-key "TAB" 'tab-dwim)
(bind-key "TAB" 'hippie-expand read-expression-map)
(bind-key "TAB" 'hippie-expand minibuffer-local-map)
(bind-key "M-/" 'hippie-expand)
(bind-key "M-?" (make-hippie-expand-function '(try-expand-line) t))

(bind-key "C-n" 'company-select-next company-active-map)
(bind-key "C-p" 'company-select-previous company-active-map)

(bind-key "C-z" 'zap-up-to-char)
(bind-key* "C-," 'er/expand-region)
(bind-key* "C-;" 'ace-jump-word-mode)

(bind-key "C-c C-." 'mc/mark-next-like-this)
(bind-key "C-c C-," 'mc/mark-previous-like-this)
(bind-key "C-c C-/" 'mc/mark-all-like-this-dwim)

(bind-key* "C-." 'ort/capture-todo)
(bind-key* "C-/" 'ort/goto-todos)

(bind-key "C-`" 'list-processes)

(bind-key "C-M-," 'crab-reload)
(bind-key "C-M-;" 'crab-eval-coffee)

(bind-key "C-x \\" 'align-regexp)
(bind-key "C-x w" 'what-face)

(bind-key* "<M-left>" 'subword-left)
(bind-key* "<M-right>" 'subword-right)
(bind-key "<M-up>" 'move-line-up)
(bind-key "<M-down>" 'move-line-down)

(bind-key "M-i" 'change-inner)
(bind-key "M-o" 'change-outer)

(bind-key "M-n" 'jump-char-forward)
(bind-key "M-p" 'jump-char-backward)

(bind-key "s-n" 'highlight-symbol-next)
(bind-key "s-p" 'highlight-symbol-prev)

(bind-key "<s-up>" 'increment-number-at-point)
(bind-key "<s-down>" 'decrement-number-at-point)

(bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)

(bind-key "s-:" 'pad-colon)
(bind-key "s-z" 'undo-tree-undo)
(bind-key "s-Z" 'undo-tree-redo)
(bind-key "s-l" 'goto-line-with-feedback)
(bind-key "s-t" 'projectile-find-file)
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

(bind-key "s-P" 'projectile-commander)
(bind-key "s-o" 'projectile-switch-project-vc)
(bind-key "s-m" 'magit-status)
(bind-key* "C-c RET" 'projector-run-shell-command-project-root)

(bind-key "C-a" 'back-to-indentation-or-beginning)
(bind-key "C-o" 'smart-open-line-above)
(bind-key "C-l" 'log-statement)

(bind-key "C-c g" 'github-browse-file)
(bind-key "C-c G" 'github-browse-file-blame)
(bind-key "C-c d" 'dash-at-point)
(bind-key "C-c D" 'dash-at-point-with-docset)
(bind-key "C-c u" 'browse-url-at-point)
(bind-key "C-c o" 'ffap)

(bind-key "C-x v t" 'git-timemachine)
(bind-key "C-x v p" 'git-messenger:popup-message)

(bind-key "<s-return>" 'smart-open-line)
(bind-key "<C-return>" 'newline-dwim)
(bind-key "<C-return>" 'ruby-smart-newline-end-defun ruby-mode-map)

(bind-key "M--" (λ (replace-region-or-symbol-at-point-with 's-dashed-words)))
(bind-key "M-_" (λ (replace-region-or-symbol-at-point-with 's-snake-case)))
(bind-key "M-c" (λ (replace-region-or-symbol-at-point-with 's-lower-camel-case)))
(bind-key "M-C" (λ (replace-region-or-symbol-at-point-with 's-upper-camel-case)))

(bind-key "C-h C-m" 'discover-my-major)

(bind-key "C-x C-k" 'delete-current-buffer-file)
(bind-key "C-x C-k" 'magit-kill-file-on-line magit-mode-map)
(bind-key "C-x C-k" 'dired-do-delete dired-mode-map)

(bind-key "C-:" 'ruby-toggle-hash-syntax ruby-mode-map)
(bind-key "C-:" 'ruby-toggle-hash-syntax slim-mode-map)

(bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
(bind-key "C-j" 'electric-indent-just-newline slim-mode-map)

(bind-key "M-TAB" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<M-S-tab>" 'next-complete-history-element minibuffer-local-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input comint-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input comint-mode-map)
(bind-key "M-TAB" 'comint-previous-matching-input-from-input inf-ruby-mode-map)
(bind-key "<M-S-tab>" 'comint-next-matching-input-from-input inf-ruby-mode-map)

(bind-key "C-c C-a" 'magit-just-amend magit-mode-map)
(bind-key "C-c C-p" 'magit-pull-request-for-issue-number magit-mode-map)

(bind-key "M-r" 'coffee-compile-region coffee-mode-map)

(bind-key "<escape>" 'abort-recursive-edit minibuffer-local-map)

(bind-key "C-c e" 'eval-region-and-maybe-deactivate-mark emacs-lisp-mode-map)

(bind-key "s-k" 'clear-shell comint-mode-map)
