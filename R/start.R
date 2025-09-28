## start menu command
start <- function(bot, update) {

  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'start',
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
  
  # set state
  set_state(update$from_chat_id(),
            update$from_user_id(),
            'start')
  
  # Получаем сактуальную ссылку на приложение
  ngrok_info <- request("http://127.0.0.1:4040/api/tunnels") %>%
    req_perform() %>%
    resp_body_json()
  
  # Берём первый HTTPS туннель
  ngrok_url <- ngrok_info$tunnels[[1]]$public_url
  
  RKM <- ReplyKeyboardMarkup(
    keyboard = list(
      list(KeyboardButton("Server Manager", web_app = ngrok_url)),
      list(KeyboardButton("Поставить задачу")),
      list(KeyboardButton("Помощь")),
      list(KeyboardButton("Список задач")),
      list(KeyboardButton("Меню запуска задач")),
      list(KeyboardButton("Тихие задачи")),
      list(KeyboardButton("Полный список задач")),
      list(KeyboardButton("Получить id чата"))
    ),
    resize_keyboard = TRUE,
    one_time_keyboard = FALSE
  )
  
  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Выберите команду",
                  reply_markup = RKM)
  
}
