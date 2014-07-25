(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'use-package)
(use-package f)
(use-package s)
(use-package dash :config (dash-enable-font-lock))

(defvar hemacs-code-dir "~/code/")

(--each '("macros" "defuns" "config" "packages" "keys")
  (load (expand-file-name (concat it ".el") user-emacs-directory)))

(load-theme 'hemacs :no-confirm)
