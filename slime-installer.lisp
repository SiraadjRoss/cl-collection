#!/usr/bin/sbcl --script

;;;; install-slime.lisp
;;;; Установка SLIME/Swank из официального репозитория без Quicklisp
;;;; Совместимо с Arch Linux, SBCL, ASDF, Emacs

(in-package :cl-user)

(defvar *slime-dir* (merge-pathnames ".slime/" (user-homedir-pathname)))
(defvar *sbclrc-path* (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(defvar *emacs-config-snippet*
  ";; Добавьте это в ~/.emacs или ~/.emacs.d/init.el
(add-to-list 'load-path \"~/.slime/\")
(require 'slime)
(setq inferior-lisp-program \"sbcl\")
")

(defun run-shell-command (cmd)
  "Выполняет shell-команду и возвращает T при успехе."
  (zerop (sb-ext:process-exit-code
          (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output t :error t))))

(defun ensure-slime-installed ()
  "Клонирует или обновляет SLIME в ~/.slime/"
  (format t "~&[1/4] Проверка наличия ~~/.slime/...~%")
  (unless (probe-file *slime-dir*)
    (format t "[1/4] Клонирование SLIME из GitHub...~%")
    (unless (run-shell-command (format nil "git clone https://github.com/slime/slime.git ~A" *slime-dir*))
      (error "Не удалось клонировать SLIME")))
  (format t "[2/4] Обновление SLIME до последней версии...~%")
  (unless (run-shell-command (format nil "cd ~A && git pull --quiet" *slime-dir*))
    (warn "Не удалось обновить SLIME")))

(defun ensure-sbclrc-configured ()
  "Добавляет загрузку Swank в ~/.sbclrc"
  (format t "[3/4] Настройка ~~/.sbclrc...~%")
  (let ((config-line "(push #p\"~/.slime/\" asdf:*central-registry*)"))
    (with-open-file (out *sbclrc-path*
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :append)
      (format out "~%~%;;; === Автоматически добавлено install-slime.lisp ===~%")
      (format out "(require 'asdf)~%")
      (format out "~A~%" config-line)
      (format out "(asdf:load-system :uiop)~%")
      (format out ";;; ==============================================~%"))
    (format t "Добавлена строка в ~~/.sbclrc:~%  ~A~%" config-line)))

(defun print-emacs-instructions ()
  "Печатает инструкцию для Emacs"
  (format t "~&[4/4] Готово!~%")
  (format t "~&Теперь добавьте в ваш ~~/.emacs или ~~/.emacs.d/init.el:~%~%")
  (write-string *emacs-config-snippet*)
  (format t "~%~%После этого запустите Emacs и выполните: M-x slime~%"))

(defun main ()
  "Основная логика установки"
  (handler-case
      (progn
        (ensure-slime-installed)
        (ensure-sbclrc-configured)
        (print-emacs-instructions))
    (error (e)
      (format *error-output* "Ошибка: ~A~%" e))))

;; Запуск
(main)
