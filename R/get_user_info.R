## получить информацию о пользователе
get_user_info <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_user_info',
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
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователями!"))
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'get_user_info',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
      } else {
        
        
        data <- db_get_user_info(args[1])
        
        # отправка данных
        if ( ! is.null(data) ) {
          
          msg <- str_glue_data(
            data, 
            "Username: <b>{username}</b>", 
            "User ID: {user_id}",
            "Name: {first_name} {last_name}",
            "Last activity: {last_activity} ({round(as.numeric(difftime(Sys.time(), as.POSIXct(data$last_activity), units = 'days')), 0)}) days ago",
            "Priv: \n{str_c('  - ', data$previlegies$priv, ' (', data$previlegies$priv_add_datetime, ')', collapse = '\n')}", 
            .sep = '\n'
          )
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          text = msg)
          
        } else {
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = 'По указанному username пользователь не найден!')
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'get_user_info',
            chat_id      = update$from_chat_id(),
            user_id      = update$from_user_id(),
            msg_text     = 'По указанному username пользователь не найден!',
            entities     = FALSE
          )
          
          
        }
        
      }
      
    },
    error = function(err) {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))
      
      # log
      lg$warn(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'get_user_info',
        chat_id      = update$from_chat_id(),
        user_id      = update$from_user_id(),
        msg_text     = err$message,
        entities     = FALSE
      )
      
      
    },
    finally = {
      
      #dbDisconnect(con)
      
    }
    
  )
  
  
}
