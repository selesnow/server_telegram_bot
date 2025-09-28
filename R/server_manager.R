server_manager <- function(bot, update) {
  
  # Получаем сактуальную ссылку на приложение
  ngrok_info <- request("http://127.0.0.1:4040/api/tunnels") %>%
    req_perform() %>%
    resp_body_json()
  
  # Берём первый HTTPS туннель
  ngrok_url <- ngrok_info$tunnels[[1]]$public_url
  
  # Создаём inline кнопку для запуска
  appIKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(
        InlineKeyboardButton(text = 'Server Manager', web_app = ngrok_url)
      )
    )
  )
  
  # Send Inline Keyboard
  bot$sendMessage(
    chat_id = update$from_chat_id(), 
    text = "Запуск Server Manager", 
    reply_markup = appIKM
  )
  
}