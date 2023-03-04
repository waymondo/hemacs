;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq-default file-name-handler-alist nil)

(setq inhibit-startup-screen t)
(setq site-run-file nil)
(setq-default inhibit-redisplay t)
(setq-default inhibit-message t)

(setq use-package-enable-imenu-support t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
