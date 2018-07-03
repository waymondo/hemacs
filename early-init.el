;;; hemacs --- an emacs configuration -*- lexical-binding: t -*-

(when (< emacs-major-version 27)
  (error "Emacs should be version 27 or greater"))

(setq straight-check-for-modifications 'live-with-find)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)
(use-package use-package-chords)
(use-package use-package-ensure-system-package)
