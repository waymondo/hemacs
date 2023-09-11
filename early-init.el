;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq-default file-name-handler-alist nil)

(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-default-init t)
(setq site-run-file nil)
(setq-default inhibit-redisplay t)
(setq-default inhibit-message t)

(setq use-package-enable-imenu-support t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(setq package-native-compile t)
(setq package-quickstart t)

(add-to-list 'initial-frame-alist '(undecorated . t))
