## Get chat id command
chat_id <- function(bot, update){
  
  # write userdata
  add_user(update)
  
  # log
  # выт¤гиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'chat_id',
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
  
  # отправл¤ем сообщение в телеграм
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "Markdown",
                  text = paste0("Chat ID этой группы: ", update$from_chat_id()))
  
}
