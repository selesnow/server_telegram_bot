## ################################### ##
## User management                     ##
## ################################### ##
add_priv <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'add_priv',
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
  
  tryCatch(expr =
             
             {
               
               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))
                 
                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'add_priv',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )
                 
               } else {
                 
                 first_name <- args[1]
                 last_name  <- args[2]
                 cmd        <- args[3]
                 
                 # получаем id пользователя по имени и фамилии
                 uid <- get_user_id(first_name, last_name)
                 # добавляем привелегию
                 add_privilege(uid, cmd)
                 
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно добавлена!"))
                 
               }
               
             }
  )
}

add_priv_by_id <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'add_oriv_by_id',
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
  
  tryCatch(expr =
             
             {
               
               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))
                 
                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'add_priv_by_id',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )
                 
               } else {
                 
                 uid <- args[1]
                 cmd <- args[2]
                 
                 # добавляем привелегию
                 add_privilege(uid, cmd)
                 
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно добавлена!"))
                 
               }
               
             }
  )
}

## ################################### ##
remove_priv <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'remove_priv',
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
  
  tryCatch(expr =
             
             {
               
               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))
                 
                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'remove_priv',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )
                 
                 
               } else {
                 
                 first_name <- args[1]
                 last_name  <- args[2]
                 cmd        <- args[3]
                 
                 # получаем id пользователя по имени и фамилии
                 uid <- get_user_id(first_name, last_name)
                 # добавляем привелегию
                 remove_privilege(uid, cmd)
                 
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно удалена!"))
                 
               }
               
             }
  )
}

remove_priv_by_id <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'remove_priv_by_id',
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
  
  tryCatch(expr =
             
             {
               
               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))
                 
                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'remove_priv_by_id',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )
                 
               } else {
                 
                 uid <- args[1]
                 cmd <- args[2]
                 
                 # добавляем привелегию
                 remove_privilege(uid, cmd)
                 
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно удалена!"))
                 
               }
               
             }
  )
}

## получить привелегии пользователя
get_user_priv <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_user_priv',
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
          bot_function = 'get_user_priv',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
      } else {
        
        con <- dbConnect(SQLite(), cfg$db_settings$db_path)
        
        data <- dbGetQuery(con,
                           str_glue('SELECT command, add_datetime
                           FROM users_previlegies
                           WHERE
                           user_id = {args[1]}
                           ORDER BY add_datetime DESC')
        )
        
        # отправка данных
        if ( nrow(data) > 0 ) {
          
          tg_table <- data %>%
            rename(priv = command) %>%
            df_to_tg
          
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = tg_table)
        } else {
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = 'У данного пользователя отсутвуют привелегии')
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'get_user_priv',
            chat_id      = update$from_chat_id(),
            user_id      = update$from_user_id(),
            msg_text     = 'У данного пользователя отсутвуют привелегии',
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
        bot_function = 'get_user_priv',
        chat_id      = update$from_chat_id(),
        user_id      = update$from_user_id(),
        msg_text     = err$message,
        entities     = FALSE
      )
      
      
    },
    finally = {
      
      dbDisconnect(con)
      
    }
    
  )
  
}
