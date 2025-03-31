## handler for run menu
answer_cb <- function(bot, update) {
  
  # write userdata
  #effective_user <- list(message = list( from = update$effective_user()))
  
  # write to db
  add_user(update)
  
  # log
  # выт¤гиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'answer_cb',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    cb_data      = update$callback_query$data,
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
  
  data <- update$callback_query$data
  
  taskcheduler_runnow(data)
  
  # Send Custom Keyboard
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "html",
                  text = paste0("«адача <b>", data, "</b> запущена на сервере! "))
  
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id)
}
