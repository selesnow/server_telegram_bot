## task to disable autofix
autofix_disable <- function(bot, update, args){
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем входящее сообщение
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'autofix_disable',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    entities     = FALSE
  )
  
  # логируем все урлы из сообщения
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
  
  if ( ! check_previlegies(update$from_user_id(), 'autofix_disable')  ) {
    
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для отключения автофикса у задач!"))
    
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'autofix_disable',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )
    
  } else {
    
    con <- dbConnect(SQLite(), cfg$db_settings$db_path)
    
    df <- data.frame(task_name    = args[1],
                     disable_till = paste(args[2], args[3]))
    
    dbWriteTable(con,
                 name = 'autofix_disable_queue',
                 value = df,
                 append = TRUE)
    
    dbDisconnect(con)
    
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("Для задачи {args[1]} автофикс отключен до {args[2]} {args[3]}!"))
  }
}
