# Настройка имени и email
git config --global user.name "SiraadjRoss"
git config --global user.email "siraadjibnross@gmail.com"
git config --global --list

# Проверьте, есть ли у вас SSH-ключ
ls -al ~/.ssh/id_*.pub

# Если нет — создайте:
ssh-keygen -t ed25519 -C "ваш@email.com"

# Запустите SSH-агент и добавьте ключ:
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# Выведите публичный ключ в концоль чтоб скопиовать в буфер обмена:
cat ~/.ssh/id_ed25519.pub

# После этого клонируйте репозитории по SSH
git clone git@github.com:SiraadjRoss/cl-collection.git

------------------------------------------------------------------------------------------
# Пример рабочего цикла с GitHub (по SSH)
# Клонирование
# git clone git@github.com:ваш_логин/ваш_репозиторий.git
# cd ваш_репозиторий

# Работа...
# echo "новый код" >> file.lisp
# git add .
# git commit -m "Добавил новую функцию"

# Отправка
# git push origin main
------------------------------------------------------------------------------------------
