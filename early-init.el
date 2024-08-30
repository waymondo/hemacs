;; -*- lexical-binding: t -*-

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      inhibit-default-init t
      inhibit-message t
      inhibit-redisplay t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message ""
      initial-scratch-message nil
      inhibit-startup-screen t
      load-prefer-newer noninteractive
      package-native-compile t
      site-run-file nil
      use-package-always-defer t
      use-package-always-ensure t
      use-package-enable-imenu-support t
      use-package-vc-prefer-newest t)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)

(defun hemacs-startup ()
  (setq file-name-handler-alist default-file-name-handler-alist
        gc-cons-threshold (* 1024 1024 64)
        inhibit-redisplay nil
        inhibit-message nil)
  (makunbound 'default-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'hemacs-startup 100)
