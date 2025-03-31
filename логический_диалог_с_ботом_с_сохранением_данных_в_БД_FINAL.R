library(telegram.bot)
library(tidyverse)
library(RSQLite)
library(DBI)


# инициализация бота
updater <- Updater(token = "***REMOVED***")

# ###########################################################
# function
get_state <- function(chat_id) {
  
  con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/test_bot_db.db")
  
  chat_state <- dbGetQuery(con, str_interp("SELECT state FROM chat_state WHERE chat_id == ${chat_id}"))$state
  
  return(unlist(chat_state))
  
  dbDisconnect(con)
}

set_state <- function(chat_id, state) {
  
  con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/test_bot_db.db")
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO chat_state (chat_id, state)
                VALUES(${chat_id}, '${state}') 
                ON CONFLICT(chat_id) 
                DO UPDATE SET state='${state}';
            ")
  )

  dbDisconnect(con)
  
}

set_chat_data <- function(chat_id, field, value) {
  
  
  con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/test_bot_db.db")
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO chat_data (chat_id, ${field})
                VALUES(${chat_id}, '${value}') 
                ON CONFLICT(chat_id) 
                DO UPDATE SET ${field}='${value}';
            ")
  )
  
  dbDisconnect(con)
  
}

get_chat_data <- function(chat_id, field) {
  
  
  con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/test_bot_db.db")
  
  # upsert состояние чата
  data <- dbGetQuery(con, 
            str_interp("
            SELECT ${field}
            FROM chat_data
            WHERE chat_id = ${chat_id};
            ")
  )
  
  dbDisconnect(con)
            
  return(data[[field]])

}

# ###########################################################
# методы
# начало диалога
start <- function(bot, update) {
  
  # 
  
  # Send query
  bot$sendMessage(update$message$chat_id, 
                  text = "Введи своё имя")
  
  # переключаем состояние диалога в режим ожидания ввода имени
  set_state(chat_id = update$message$chat_id, state = 'wait_name')
  
}

# ###############
# проверка состояния диалога
state <- function(bot, update) {
  
  chat_state <- get_state(update$message$chat_id)
  
  # Send state
  bot$sendMessage(update$message$chat_id, 
                  text = unlist(chat_state))
  
}

reset <- function(bot, update) {
  
  set_state(chat_id = update$message$chat_id, state = 'start')
  
}

# ############
# обработчик, который принимает имя
enter_name <- function(bot, update) {
  
  uname <- update$message$text
  
  # Send message with name
  bot$sendMessage(update$message$chat_id, 
                  text = paste0(uname, ", приятно познакомится, я бот!"))
  
  # Записываем имя в глобальную переменную
  #username <<- uname
  set_chat_data(update$message$chat_id, 'name', uname) 
  
  # Справшиваем возраст
  bot$sendMessage(update$message$chat_id, 
                  text = "Сколько тебе лет?")
  
  # Меняем состояние на ожидание ввода имени
  set_state(chat_id = update$message$chat_id, state = 'wait_age')
  
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
    #userage <<- uage
    set_chat_data(update$message$chat_id, 'age', uage) 
    
    # сообщаем какие данные были собраны
    username <- get_chat_data(update$message$chat_id, 'name')
    userage  <- get_chat_data(update$message$chat_id, 'age')
    
    bot$sendMessage(update$message$chat_id, 
                    text = paste0("Тебя зовут ", username, " и тебе ", userage, " лет. Будем знакомы"))
    
    # возвращаем диалог в исходное состояние
    set_state(chat_id = update$message$chat_id, state = 'start')
  }
  
}

# ###########################################################
# фильтры сообщений
# фильтр сообщение в состоянии ожидания имени
MessageFilters$wait_name <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_name"
}
)

# фильтр сообщение в состоянии ожидания возраста
MessageFilters$wait_age <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_age"
}
)

# ###########################################################
# обработчики команд
start_h <- CommandHandler('start', start)
state_h <- CommandHandler('state', state)
reset_h <- CommandHandler('reset', reset)

# обработчик сообщений
## !MessageFilters$command - означает что команды данные обработчики не обрабатывают, 
## только текстовые сообщения
wait_age_h  <- MessageHandler(enter_age,  MessageFilters$wait_age  & !MessageFilters$command)
wait_name_h <- MessageHandler(enter_name, MessageFilters$wait_name & !MessageFilters$command)

# ###########################################################
# добавляем все обработчики в диспетчер
updater <- updater +
  start_h +
  wait_age_h +
  wait_name_h +
  state_h +
  reset_h

# ###########################################################
updater$start_polling()
