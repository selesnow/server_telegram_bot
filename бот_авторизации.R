# netpeak bi bot
library(telegram.bot)
library(RSQLite)
library(DBI)
library(taskscheduleR)
library(dplyr)
library(stringr)
library(readr)
library(ppcLoader)
library(configr)


library(httr)

# create bot instance
updater <- Updater(token = "***REMOVED***")

# Read function
source('db_bot_function.R')

# config 
cfg <- read.config('C:\\my_develop_workshop\\telegram_bot\\config.cfg')
# Commands #####################
## start menu command
start <- function(bot, update) {

  message(Sys.time(), " ", " f - start ",  
          update$message$from$username, " - ", 
          update$message$text)
  
  # set state
  set_state(update$message$chat_id, 
            update$message$from$username,
            'start')
  
  RKM <- ReplyKeyboardMarkup(
    keyboard = list(
      list(KeyboardButton("Список задач")),
      list(KeyboardButton("Меню запуска задач")),
      list(KeyboardButton("Тихие задачи")),
      list(KeyboardButton("Полный список задач")),
      list(KeyboardButton("Получить id чата"))
    ),
    resize_keyboard = TRUE,
    one_time_keyboard = FALSE
  )
  
  # Send Custom Keyboard
  bot$sendMessage(update$message$chat_id, 
                  text = "Выберите команду", 
                  reply_markup = RKM)
  
}

## auth
ya_auth  <- function(bot, update, args) {
  
  # проверка есть ли логин
  if ( length(args) == 0 ) {
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    parse_mode = "html",
                    text = "Для авторизации необзодимо указывать логин, пример <code>/ya_auth login</code>. Повторите попытку.")
    
    # set state
    set_state(update$message$chat_id, 
              update$message$from$username,
              'start')
    
  } else {
  
    # считываем логин
    Login <- args[1]
    
    # write login
    set_chat_data(update$message$chat_id, 
                  update$message$from$username,
                  field = 'login',
                  value =  Login)
    
    # inline кнока для перехода к авторизации
    url <- paste0("https://oauth.yandex.ru/authorize?response_type=code&client_id=365a2d0a675c462d90ac145d4f5948cc&redirect_uri=https://selesnow.github.io/ryandexdirect/getToken/get_code.html&force_confirm=1", paste0("&login_hint=", Login))
    
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(paste0("Авторизоваться под ", Login ), url = url)
        )
      )
    )
    
    # отправка клавиатуры
    # Отправляем клавиатуру в чат
    bot$sendMessage(update$message$chat_id, 
                    text = 'Для авторизации нажмите кнопку ниже, предоставьте в браузере все разрешения и введите полученный код авторизации',
                    reply_markup = IKM)
    
    # переключаем состояние бота в ожидание ввода кода
    # отправляем сообщение в телеграм  
    bot$sendMessage(chat_id = update$message$chat_id,
                    parse_mode = "html",
                    text = "Введите код подтверждения авторизации:")
    
    # set state
    set_state(update$message$chat_id, 
              update$message$from$username,
              'wait_code')
  
  }
  
}

ya_get_code <- function(bot, update) {

  if ( nchar(update$message$text) != 7 ) {
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    parse_mode = "html",
                    text = "Вы ввели неверный код, код должен состоять из 7 цыфр, попробуйте ещё раз!")
    
    
  } else {
    
    token_raw <- httr::POST("https://oauth.yandex.ru/token", 
                            body = list(grant_type="authorization_code", 
                                        code = update$message$text, 
                                        client_id = "365a2d0a675c462d90ac145d4f5948cc", 
                                        client_secret = "f2074f4c312449fab9681942edaa5360"), 
                            encode = "form")
    
    # parse token
    token <- content(token_raw)
    token$expire_at <- Sys.time() + as.numeric(token$expires_in, units = "secs")
    class(token) <- "yadir_token"
    
    # check for error
    if (!is.null(token$error_description)) {
      
      bot$sendMessage(chat_id = update$message$chat_id,
                      parse_mode = "HTML",
                      text = paste0("Ошибка - ", token$error_description, ". Повторите попытку с самого начала командой <code>/ya_auth</code>!"))
      
      
      # set state
      set_state(update$message$chat_id, 
                update$message$from$username,
                'start')

    } else {
      
      Login <- get_chat_data(update$message$chat_id, 
                             update$message$from$username,
                             field = 'login')
      
      # save into local file
      save(token, file = paste0("C:/my_develop_workshop/ppc_digest/token_yandex/", Login, ".yadirAuth.RData"))
      
      # set state
      set_state(update$message$chat_id, 
                update$message$from$username,
                'start')
      
      
      bot$sendMessage(chat_id = update$message$chat_id,
                      parse_mode = "html",
                      text = paste0("Вы успешно авторизовались в Яндекс Директ под логином ", Login))
      
      
      
    }
    
  }
  
}

MessageFilters$ya_auth_code <- 
  BaseFilter( function(message) {
    
    get_state(message$chat_id, message$from$username) == 'wait_code'
    
  })

start_handler    <- CommandHandler("start", start)
auth_handler     <- CommandHandler("ya_auth", ya_auth, pass_args = TRUE, username = TRUE)
get_code_handler <- MessageHandler(ya_get_code, MessageFilters$ya_auth_code & ! MessageFilters$command)

updater <- updater + 
           start_handler + 
           auth_handler +
           get_code_handler


updater$start_polling()

