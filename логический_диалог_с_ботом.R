library(telegram.bot)

# переменные
chat_state <- 'start' # состояние диалога
username   <- NULL    # имя
userage    <- NULL    # возраст

# инициализация бота
updater <- Updater(token = "***REMOVED***")

# методы
# начало диалога
start <- function(bot, update) {

  # Send query
  bot$sendMessage(update$message$chat_id, 
                  text = "Введи своё имя")
  
  # переключаем состояние диалога в режим ожидания ввода имени
  chat_state <<- 'wait_name'
  
}

# ###############
# проверка состояния диалога
state <- function(bot, update) {
  
  # Send state
  bot$sendMessage(update$message$chat_id, 
                  text = chat_state)
  
}

# ############
# обработчик, который принимает имя
enter_name <- function(bot, update) {
  
  uname <- update$message$text
  
  # Send message with name
  bot$sendMessage(update$message$chat_id, 
                  text = paste0(uname, ", приятно познакомится, я бот!"))
  
  # Записываем имя в глобальную переменную
  username <<- uname
  
  # Справшиваем возраст
  bot$sendMessage(update$message$chat_id, 
                  text = "Сколько тебе лет?")
  
  # Меняем состояние на ожидание ввода имени
  chat_state <<- 'wait_age'
   
}

# ################
# обработчик, который принемает возраст
enter_age <- function(bot, update) {
  
  uage <- as.numeric(update$message$text)
  
  # проверяем было введено число или нет
  if ( is.na(uage) ) {
    
    # если введено не число то переспрашиваем возраст
    bot$sendMessage(update$message$chat_id, 
                    text = "Ты ввёл некорректные данные, введи число")
    
  } else {
    
    # если введено число сообщаем что возраст принят
    bot$sendMessage(update$message$chat_id, 
                    text = "ОК, возраст принят")
    
    # записываем глобальную переменную с возрастом
    userage <<- uage
    
    # сообщаем какие данные были собраны
    bot$sendMessage(update$message$chat_id, 
                    text = paste0("Тебя зовут ", username, " и тебе ", userage, " лет. Будем знакомы"))
    
    # возвращаем диалог в исходное состояние
    chat_state <<- 'start'
  }
    
}
  

# обработчики команд
start_h <- CommandHandler('start', start)
state_h <- CommandHandler('state', state)

# фильтры сообщений
# фильтр сообщение в состоянии ожидания имени
MessageFilters$wait_name <- BaseFilter(function(message) {
  chat_state  == "wait_name"
}
)

# фильтр сообщение в состоянии ожидания возраста
MessageFilters$wait_age <- BaseFilter(function(message) {
  chat_state  == "wait_age"
}
)

# обработчик сообщений
## !MessageFilters$command - означает что команды данные обработчики не обрабатывают, 
## только текстовые сообщения
wait_age_h  <- MessageHandler(enter_age,  MessageFilters$wait_age  & !MessageFilters$command)
wait_name_h <- MessageHandler(enter_name, MessageFilters$wait_name & !MessageFilters$command)

# добавляем все обработчики в диспетчер
updater <- updater +
              start_h +
              wait_age_h +
              wait_name_h +
              state_h

# 
updater$start_polling()
