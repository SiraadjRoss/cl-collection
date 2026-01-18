#!/usr/bin/sbcl --script

;;;; Brave browser installer (conceptual, educational use only!)
(require :sb-posix)

(defpackage :brave-browser-install
  (:use :cl :sb-ext)
  (:import-from :sb-posix :chdir)
  (:export :main))
(in-package :brave-browser-install)


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


(defun main ()
  (handler-case
      (progn
	(format t ">> Starting brave-browser installer (SBCL script)...~%")
	(chdir "/home/siraadj")
;; Exequite comand from the siraadj user	
	(run-command "su - siraadj -с 'git clone https://aur.archlinux.org/brave-bin.git'")
	(chdir "brave-bin")
	(run-command "su - siraadj -c 'makepkg -s'")
	(run-command "pacman -U *.zst")
;;	(run-command "cd brave-bin && makepkg -si")
;;	(run-command "makepkg -si")
	(format t "[ DONE ] brave-browser installed!~%"))
    (error (e)
      (format *error-output* "X Error: ~A~%" e))))


;;; Run if executed as script
(main)
