#!/usr/bin/sbcl --script

;;; setup-ssh-server.lisp
;;; Автоматизация развёртывания SSH-сервера на Arch Linux с помощью Common Lisp (SBCL)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :asdf)
    (require "asdf"))
  (asdf:find-system :uiop) ; чтобы uiop стал доступен
  (use-package :uiop))

;; Вспомогательные функции
(defun run-command (cmd &key (error-output nil))
  "Выполняет команду в оболочке. Возвращает (values output success-p)."
  (let ((output (with-output-to-string (s)
                  (run-program cmd
                               :output s
                               :error-output (if error-output error-output :output)
                               :ignore-error-status t))))
    (values output (= (nth-value 1 (run-program cmd :ignore-error-status t)) 0))))

(defun file-contains-p (file pattern)
  "Проверяет, содержится ли строка PATTERN в файле FILE."
  (when (probe-file file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil nil)
            while line
            thereis (search pattern line :test #'char=)))))

(defun ensure-directory (path owner)
  "Создаёт директорию, если не существует, и устанавливает владельца."
  (unless (probe-file path)
    (run-program `("mkdir" "-p" ,path)))
  (run-program `("chown" ,owner ,path)))

(defun append-to-file-unless-contains (file line owner)
  "Добавляет строку в файл, если она ещё не присутствует."
  (unless (file-contains-p file line)
    (with-open-file (stream file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :append)
      (format stream "~A~%" line))
    (run-program `("chown" ,owner ,file))))

;; Основная логика
(defun main ()
  (let* ((ssh-user (or (getenv "SSH_USER") (string-trim '(#\Space #\Tab #\Newline) (run-program '("whoami") :output :string))))
         (ssh-port (or (getenv "SSH_PORT") "22"))
         (authorized-key (getenv "AUTHORIZED_KEY"))
         (home-dir (format nil "/home/~A" ssh-user))
         (ssh-dir (format nil "~A/.ssh" home-dir))
         (auth-keys-file (format nil "~A/authorized_keys" ssh-dir))
         (sshd-config "/etc/ssh/sshd_config"))

    ;; 1. Установка openssh, если не установлен
    (multiple-value-bind (output installed-p)
        (run-command '("pacman" "-Qq" "openssh"))
      (unless installed-p
        (format t "Установка openssh...~%")
        (run-command '("pacman" "-Sy" "--noconfirm" "openssh"))))

    ;; 2. Настройка ~/.ssh и authorized_keys
    (ensure-directory ssh-dir (format nil "~A:~A" ssh-user ssh-user))
    (run-command `("chmod" "700" ,ssh-dir))

    (when authorized-key
      (let ((key-content
              (if (probe-file authorized-key)
                  (with-open-file (s authorized-key)
                    (read-line s))
                  authorized-key)))
        (append-to-file-unless-contains auth-keys-file key-content (format nil "~A:~A" ssh-user ssh-user))
        (run-command `("chmod" "600" ,auth-keys-file))))

    ;; 3. Резервная копия sshd_config
    (unless (probe-file (format nil "~A.bak" sshd-config))
      (run-command `("cp" ,sshd-config ,(format nil "~A.bak" sshd-config))))

    ;; Функция обновления параметра в конфиге
    (flet ((set-sshd-param (param value)
             (let ((pattern (format nil "^~A\\s" param)))
               (if (run-program `("grep" "-q" ,pattern ,sshd-config) :ignore-error-status t)
                   (run-command `("sed" "-i" ,(format nil "s/^~A\\s.*/~A ~A/" param param value) ,sshd-config))
                   (with-open-file (s sshd-config :direction :output :if-exists :append)
                     (format s "~A ~A~%" param value))))))

      (set-sshd-param "PermitRootLogin" "no")
      (set-sshd-param "PasswordAuthentication" "no")
      (set-sshd-param "PubkeyAuthentication" "yes")
      (set-sshd-param "AuthorizedKeysFile" ".ssh/authorized_keys")
      (set-sshd-param "UseDNS" "no")
      (unless (string= ssh-port "22")
        (set-sshd-param "Port" ssh-port)))

    ;; 4. Запуск службы
    (format t "Включение и запуск sshd...~%")
    (run-command '("systemctl" "enable" "--now" "sshd"))

    ;; 5. Проверка статуса
    (if (run-command '("systemctl" "is-active" "--quiet" "sshd"))
        (format t "✅ SSH-сервер настроен и запущен на порту ~A.~%" ssh-port)
        (progn
          (format t "❌ Ошибка: служба sshd не активна!~%")
          (uiop:quit -1)))))

;; Запуск
(main)
