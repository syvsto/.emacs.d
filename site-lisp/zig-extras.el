;;; zig-extras.el --- Additional features for building and working with Zig in Emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Syver Storm-Furru

;; Author: Syver Storm-Furru <syver.storm.furru@gmail.com>
;; Keywords: programming, language, zig
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 'transient)

(transient-define-prefix zig-extras-build ()
  ["Arguments"
   ("-d" "Link dynamically"  "-dynamic")]
  ["Build"
   [("l" "Library" zig-extras--build-library)
    ("b" "Executable" zig-extras--build-exe)
    ("o" "Object" zig-extras--build-obj)]])

(defun zig-extras-build-arguments nil
  (transient-args 'zig-extras-build))

(defun zig-extras--build-library ()
  (interactive)
  (zig-extras--build-command "build-lib"))

(defun zig-extras--build-exe ()
  (interactive)
  (zig-extras--build-command "build-exe"))

(defun zig-extras--build-obj ()
  (interactive)
  (zig-extras--build-command "build-obj"))

(defun zig-extras--build-command (cmd)
  (let ((args (append (list "zig" cmd) (zig-extras-build-arguments))))
    (compile (s-join " " args))))





(defun zig-extras-menu ()
  (interactive)
  (transient-setup 'zig-extras))

(provide 'zig-extras)
