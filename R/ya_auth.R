## ################################### ##
## functions for auth in Yandex Direct ##
## ################################### ##
## run auth
ya_auth  <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  
  # log
  # выт¤гиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'ya_auth',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    entities     = FALSE
  )
  
  # логируем доп инфу по команде
  if ( length(msg$entities) > 0 ) {
    
    for ( ent in msg$entities ) {
      lg$info(
        session_id = get_session_id(),
        message_id = msg$message_id,
        offset     = ent$offset,
        length     = ent$length,
        type       = ent$type,
        url        = ent$url,
        entities = TRUE
      )
      
    }
    
  }
  
  # emoji
  access_denied_smile <- '\U00026D4'
  err_smile           <- '\U000274C'
  ok_smile            <- '\U0002705'
  time_smile          <- '\U00023F0'
  
  # variables
  username  <- update$effective_message()$from$username
  
  # check user
  if ( ! check_previlegies(update$from_user_id(), 'ya_auth')  ) {
    
    bot$sendMessage(update$from_chat_id(),
                    text = str_glue("\n{access_denied_smile} ” вас нет прав на создание токенов дл¤ яндекс ?ирект."),
                    reply_to_message_id = update$effective_message()$message_id,
                    parse_mode = "HTML")
    
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'ya_auth',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = "нет прав",
      entities     = FALSE
    )
    
    # check variables
  } else if ( length(args) == 0 ) {
    
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("{err_smile} ?л¤ авторизации необзодимо указывать логин, пример <code>/ya_auth login</code>. ѕовторите попытку."))
    
    # set state
    set_state(update$from_chat_id(),
              update$from_user_id(),
              'start')
    
  } else {
    
    # считываем логин
    Login <- args[1]
    
    # write login
    set_chat_data(update$from_chat_id(),
                  update$from_user_id(),
                  field = 'login',
                  value =  Login)
    
    # inline кнока дл¤ перехода к авторизации
    url <- paste0("https://oauth.yandex.ru/authorize?response_type=code&client_id=365a2d0a675c462d90ac145d4f5948cc&redirect_uri=https://selesnow.github.io/ryandexdirect/getToken/get_code.html&force_confirm=1", paste0("&login_hint=", Login))
    
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(paste0("јвторизоватьс¤ под ", Login ), url = url)
        )
      )
    )
    
    # отправка клавиатуры
    # ќтправл¤ем клавиатуру в чат
    bot$sendMessage(update$from_chat_id(),
                    text = '?л¤ авторизации нажмите кнопку ниже, предоставьте в браузере все разрешени¤ и введите полученный код авторизации',
                    reply_markup = IKM)
    
    # переключаем состо¤ние бота в ожидание ввода кода
    # отправл¤ем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = "¬ведите код подтверждени¤ авторизации:")
    
    # set state
    set_state(update$from_chat_id(),
              update$from_user_id(),
              'wait_code')
    
  }
  
}

## check auth code
ya_get_code <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  
  # log
  # выт¤гиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'ya_get_code',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    entities     = FALSE
  )
  
  # логируем доп инфу по команде
  if ( length(msg$entities) > 0 ) {
    
    for ( ent in msg$entities ) {
      lg$info(
        session_id = get_session_id(),
        message_id = msg$message_id,
        offset     = ent$offset,
        length     = ent$length,
        type       = ent$type,
        url        = ent$url,
        entities = TRUE
      )
      
    }
    
  }
  
  # emoji
  access_denied_smile <- '\U00026D4'
  err_smile           <- '\U000274C'
  ok_smile            <- '\U0002705'
  time_smile          <- '\U00023F0'
  
  
  if ( nchar(update$effective_message()$text) != 7 ) {
    
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("{err_smile} ¬ы ввели неверный код, код должен состо¤ть из 7 цыфр, попробуйте ещЄ раз!"))
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'ya_get_code',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = "нет прав",
      entities     = FALSE
    )
    
    
  } else {
    
    token_raw <- httr::POST("https://oauth.yandex.ru/token",
                            body = list(grant_type="authorization_code",
                                        code = update$effective_message()$text,
                                        client_id = "365a2d0a675c462d90ac145d4f5948cc",
                                        client_secret = "f2074f4c312449fab9681942edaa5360"),
                            encode = "form")
    
    # parse token
    token <- content(token_raw)
    token$expire_at <- Sys.time() + as.numeric(token$expires_in, units = "secs")
    class(token) <- "yadir_token"
    
    # check for error
    if (!is.null(token$error_description)) {
      
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "HTML",
                      text = paste0(err_smile, " ќшибка - ", token$error_description, ". ѕовторите попытку с самого начала командой <code>/ya_auth</code>!"))
      
      
      # set state
      set_state(update$from_chat_id(),
                update$from_user_id(),
                'start')
      
    } else {
      
      Login <- get_chat_data(update$from_chat_id(),
                             update$from_user_id(),
                             field = 'login')
      
      # save into local file
      save(token, file = paste0("C:/my_develop_workshop/ppc_digest/token_yandex/", Login, ".yadirAuth.RData"))
      
      # set state
      set_state(update$from_chat_id(),
                update$from_user_id(),
                'start')
      
      
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0(ok_smile, " ¬ы успешно авторизовались в яндекс ?ирект под логином ", Login))
      
      
      
    }
    
  }
  
}
