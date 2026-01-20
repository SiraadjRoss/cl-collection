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

(defun run-user-command (cmd)
  (let ((preffix "su - siraadj -c "))
    (run-command (concatenate 'string preffix (format nil "'~A'" cmd)))))

(defun brave-browser ()
  (format t ">> Starting brave-browser installer (SBCL script)...~%")
;; Exequite comand from the siraadj user	
  (run-command "cd /tmp && git clone https://aur.archlinux.org/brave-bin.git")
  (run-command "cd /tmp/brave-bin && makepkg -si --noconfirm")
;; (run-command "pacman -U --noconfirm /home/siraadj/brave-bin/*.zst")
  (format t "[ DONE ] brave-browser installed!~%"))

(defun chrome-browser ()
(format t ">> Starting chrome-browser installer (SBCL script)...~%")
;; Exequite comand from the siraadj user	
  (run-command "cd /tmp && git clone https://aur.archlinux.org/google-chrome.git")
  (run-command "cd /tmp/google-chrome && makepkg -si --noconfirm")
;; (run-command "pacman -U --noconfirm /home/siraadj/google-chrome/*.zst")
  (format t "[ DONE ] chrome-browser installed!~%"))

	       
(defun main ()
  (handler-case
      (progn
	(brave-browser)
	(chrome-browser))
    (error (e)
      (format *error-output* "X Error: ~A~%" e))))


;;; Run if executed as script
(main)
