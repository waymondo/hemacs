;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq straight-check-for-modifications 'live-with-find)
(setq straight-cache-autoloads t)

(defvar bootstrap-version)
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
(setq straight-vc-git-default-protocol 'ssh)
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)
(setq inhibit-startup-screen t)

(straight-use-package
 '(use-package :type git :host github :repo "waymondo/use-package" :branch "personal"))
(setq straight-use-package-by-default t)

(use-package bind-key)
(use-package no-littering)
(use-package use-package-chords)
(use-package use-package-ensure-system-package)
