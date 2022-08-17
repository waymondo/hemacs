;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq-default file-name-handler-alist nil)

(setq straight-check-for-modifications 'live-with-find)
(setq straight-cache-autoloads t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(setq use-package-always-defer t)
(setq straight-vc-git-default-protocol 'ssh)
(setq package-enable-at-startup nil)
(setq inhibit-startup-screen t)
(setq site-run-file nil)
(setq-default inhibit-redisplay t)
(setq-default inhibit-message t)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering :demand t)
(use-package use-package-chords :demand t)
(use-package use-package-ensure-system-package :demand t)
