;; -*- lexical-binding: t -*-

(setq straight-check-for-modifications 'live-with-find)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package
 '(use-package :type git :host github :repo "waymondo/use-package" :branch "personal"))
(setq straight-use-package-by-default t)

(use-package no-littering)
(use-package use-package-chords)
(use-package use-package-ensure-system-package)

(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)
