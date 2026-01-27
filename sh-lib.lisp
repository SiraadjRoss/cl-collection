(defpackage :sl
  (:use :cl :sb-ext)
  (:export :run-command
	   :chroot-run))
(in-package :sl)

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

(defun chroot-run (cmd)
  (run-command (format nil "arch-chroot /mnt /bin/sh -c ~s" cmd)))

