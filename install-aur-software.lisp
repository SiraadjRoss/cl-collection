#!/usr/bin/sbcl --script

;;;; AUR software installer (conceptual, educational use only!)
(require :sb-posix)

(defpackage :aur-software-installer
  (:use :cl :sb-ext)
  (:export :main))
(in-package :aur-software-installer)

(defvar *software-list* ((name . url)))

(defun run-shell-command (cmd &key (output t) (error-output t))
  "Execute shell command. cmd is string"
  (let ((exit-code (sb-ext:process-exit-code
                    (sb-ext:run-program "/bin/sh"
                                        (list "-c" cmd)
                                        :output output
                                        :error error-output
                                        :wait t))))
    (unless (zerop exit-code)
      (error "Command failed: ~A (exit code ~A)" cmd exit-code))))

(defun run-user-command (cmd)
  (let ((preffix "su - siraadj -c"))
    (run-command (concatenate 'string preffix (format nil " ~s" cmd)))))

