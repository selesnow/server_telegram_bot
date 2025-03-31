## stop polling
stop <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'stop',
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
  
  
  if (check_previlegies(update$from_user_id(), 'stop')) {
    
    future::plan('sequential')
    
    # время падения бота
    bot_stop_time <- Sys.time()
    bot_work_time  <- difftime(bot_stop_time, bot_start_time,units =  'mins')
    
    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "Бот остановлен")
    
    lg$log(level = 'stop',
           session_id = get_session_id(),
           message_id = msg$message_id,
           error_text = NULL,
           stop_reason = "user",
           entities = FALSE,
           session_start_at = bot_start_time,
           session_duratin_min = round(bot_work_time, 2))
    
    
    
    IKM <- InlineKeyboardMarkup(
      inline_keyboard =
        list(
          list(
            InlineKeyboardButton(text = 'Запустить бота', url = str_glue('http://94.130.22.47:8000/run?task=alsey\\netpeak_core\\nc_analytics_team\\als_tg_bot&tkn={Sys.getenv("API_TOKEN")}'))
          )
        )
    )
    
    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "Для запуска бота нажмите кнопку",
                    reply_markup = IKM)
    
    updater$stop_polling()
    
    quit("no")
    #taskcheduler_stop('alsey\\als_tg_bot')
    
  } else {
    
    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "У вас не достаточно прав для остновки бота.")
    
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'stop',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )
    
  }
  
}
