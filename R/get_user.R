## получить список пользователей
get_users_method <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_users_method',
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
  
  tryCatch(
    expr = {
      
      if ( ! check_previlegies(update$from_user_id(), 'users') ) {
        
        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователей!"))
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'get_users_method',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
      } else {
        
        con <- dbConnect(SQLite(), cfg$db_settings$db_path)
        
        data <- dbReadTable(con, 'bot_users')
        
        tg_table <- data %>%
          select(-is_bot) %>%
          df_to_tg
        
        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "Markdown",
                        text = tg_table)
        
      }
      
    },
    error = function(err) {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))
      
    },
    finally = {
      
      dbDisconnect(con)
      
    }
    
  )
  
}
