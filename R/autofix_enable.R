## enable autofix for a task
autofix_enable <- function(bot, update, args){
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем входящее сообщение
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'autofix_enable',
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
  
  if ( ! check_previlegies(update$from_user_id(), 'autofix_disable') ) {
    
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для включения автофикса у задач!"))
    
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'autofix_enable',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )
    
  } else {
    
    con <- dbConnect(SQLite(), cfg$db_settings$db_path)
    
    queue_tasks <- dbExecute(con, str_glue("DELETE FROM autofix_disable_queue WHERE disable_till > datetime('now', 'localtime') AND task_name = '{args}'"))
    
    dbDisconnect(con)
    
    if ( queue_tasks == 1 ) {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = str_glue("Для задачи {args} автофикс успешно включен обратно!"))
      
    } else {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = str_glue("Не удалось найти задачу {args} в списке отключенных автофиксов или срок отключения уже истек."))
      
    }
    
  }
}
