#!/usr/bin/sbcl --script

;;;; quicklisp-installer (conceptual, educational use only!)
;;;; execute as root
(require :sb-posix)

(defpackage :quicklisp-install
  (:use :cl :sb-ext)
  (:export :main))
(in-package :quicklisp-install)

(defvar *quicklisp-url* "https://beta.quicklisp.org/quicklisp.lisp")
(defvar *quicklisp-setup* (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(defvar *slime-helper* (merge-pathnames "quicklisp/slime-helper.lisp" (user-homedir-pathname)))

(defun run-command (cmd &key (output t) (error-output t))
  "Execute shell command. cmd is string"
  (let ((exit-code (sb-ext:process-exit-code
                    (sb-ext:run-program "/bin/sh"
                                        (list "-c" cmd)
                                        :output output
                                        :error error-output
                                        :wait t))))
    (unless (zerop exit-code)
      (error "Command failed: ~A (exit code ~A)" cmd exit-code))))

(defun install-quicklisp ()
  ;; Change current dirrectory
  (sb-posix:chdir (user-homedir-pathname))
  (format t "Downloading the quicklisp.lisp file from ~A...~%" *quicklisp-url*)
;;  (run-command (format nil "curl -O ~A" *quicklisp-url*))
  (format t "Installing quicklisp...~%")
  (load "quicklisp.lisp")
  (funcall (find-symbol (string :install) (find-package "QUICKLISP-QUICKSTART"))))
;;  (delete-file "quicklisp.lisp"))

(defun setup-quicklisp-autoload ()
  (format t "Setup quicklisp autoload...~%")
  ;; Create empty file
  (with-open-file (f (merge-pathnames ".sbclrc" (user-homedir-pathname))
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create))
  (funcall (find-symbol (string :add-to-init-file) (find-package "QL"))))

(defun init-pacman-keyring ()
  (run-command "pacman-key --init")
  (run-command "pacman-key --populate archlinux"))

(defun main ()
  (handler-case
      (progn
	(install-quicklisp)
	(setup-quicklisp-autoload)
	(init-pacman-keyring))
    (error (e)
      (format *error-output* "X Error: ~A~%" e))))
;;; Run if executed as script
(main)
