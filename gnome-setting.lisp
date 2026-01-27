#!/usr/bin/sbcl --script
(load "sh-lib.lisp")

(defpackage :gi
  (:use :cl :sb-ext :sl)
  (:export :run-command
	   :chroot-run))
(in-package :gi)

;; Keyboard setting
;; Добавьте русскую (ru) и, например, английскую (us) раскладки:
(run-command "gsettings set org.gnome.desktop.input-sources sources \"[('xkb', 'us'), ('xkb', 'ru')]\"")
;; Установить сочетание клавиш переключения раскладки на Alt+Shift
(run-command "gsettings set org.gnome.desktop.input-sources xkb-options \"['grp:alt_shift_toggle']\"")
