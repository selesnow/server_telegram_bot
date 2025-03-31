## Помощь
help <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'help',
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
      list(InlineKeyboardButton(text = "Помощь", url = 'https://doc-naf.netpeak.net/books/внутренний-телеграм-бот-департамента-аналитики'))
    )
  )
  
  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Документаця доступна только с использованием OpenVPN, информация по его установке и настройке доступна по [ссылке](https://wiki.netpeak.net/Windows#12._.D0.A3.D1.81.D1.82.D0.B0.D0.BD.D0.BE.D0.B2.D0.BA.D0.B0_.D0.B8_.D0.BD.D0.B0.D1.81.D1.82.D1.80.D0.BE.D0.B9.D0.BA.D0.B0_Openvpn).",
                  reply_markup = IKM, parse_mode = 'Markdown')
  
}
