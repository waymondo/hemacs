(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'use-package)

(use-package f)
(use-package s)
(use-package dash
  :config (dash-enable-font-lock))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "HISTFILE")
    (exec-path-from-shell-copy-env "NODE_PATH")
    (exec-path-from-shell-copy-env "SSL_CERT_FILE")))

(defvar hemacs-code-dir "~/code/")

(loop for name in '("defuns" "config" "packages" "keys")
      do (load (expand-file-name (concat name ".el") user-emacs-directory)))

(load-theme 'hemacs :no-confirm)
