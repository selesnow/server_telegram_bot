# получить файл из папки ----------------------------------------
get_file <- function(bot, update, args) {
  
  args <- paste(args, collapse = ' ')
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'dir',
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
  
  if ( ! check_previlegies(update$from_user_id(), 'files') ) {
    
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для работы с файлами сервера!"))
    
    
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'dir',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )
    
    
  } else {
    
    if ( file.exists(args) ) {
      
      updater$bot$sendDocument(
        chat_id = update$from_chat_id(),
        document = args
      )
      
    } else {
      
      bot$sendMessage(chat_id = update$from_chat_id(),
                      text = str_glue("Не могу найти файл {args}"),
                      parse_mode = 'markdown')
      
    }
  }
}
