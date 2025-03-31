get_template <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_template',
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
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(InlineKeyboardButton(text = "Поставить задачу на аналитиков", url = 'https://t.netpeak.group/task/1730126'))
    )
  )
  
  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Задачи на отдел аналитики ставятся по шаблону, нажмите на кнопку под этим сообщением.",
                  reply_markup = IKM, parse_mode = 'Markdown')
  
}
