## run gemini command
gemini <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем входящее сообщение
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'gemini',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    entities     = FALSE
  )

  # логируем сущности если есть
  if ( length(msg$entities) > 0 ) {
    for ( ent in msg$entities ) {
      lg$info(
        session_id = get_session_id(),
        message_id = msg$message_id,
        offset     = ent$offset,
        length     = ent$length,
        type       = ent$type,
        url        = ent$url,
        entities   = TRUE
      )
    }
  }

  # Проверка прав доступа
  if ( ! check_previlegies(update$from_user_id(), 'ai') ) {

    bot$sendMessage(
      chat_id    = update$from_chat_id(),
      parse_mode = "html",
      text       = paste0("\U00026D4 У вас не достаточно прав для использования Gemini!")
    )

    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'gemini',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )

    return()
  }

  # Проверка аргументов
  if ( length(args) == 0 ) {
    bot$sendMessage(
      chat_id    = update$from_chat_id(),
      parse_mode = "html",
      text       = "\U00002139 Укажите запрос после команды <code>/gemini</code>.\nПример: <code>/gemini Проверь статус задач на сервере</code>"
    )
    return()
  }

  # Извлекаем текст запроса, объединяя аргументы через пробел
  user_query <- paste(args, collapse = " ")

  # Отправляем предварительный статус
  msg_wait <- bot$sendMessage(
    chat_id             = update$from_chat_id(),
    text                = "\U000023F3 Запрос передан в Gemini CLI, ожидайте ответ...",
    reply_to_message_id = msg$message_id
  )

  # Запоминаем контекст для асинхронного выполнения
  target_chat_id <- update$from_chat_id()
  token          <- bot_api_token
  reply_msg_id   <- msg$message_id
  wait_msg_id    <- msg_wait$message_id

  # Асинхронное выполнение
  promises::future_promise({

    library(stringr)
    library(telegram.bot)

    tryCatch({
      current_user <- Sys.getenv("USERNAME", "Alsey")
      user_email   <- paste0("gemini_cli_", tolower(current_user), "@netpeak.group")
      git_env      <- str_glue("set GIT_AUTHOR_NAME=Gemini CLI ({current_user}) && set GIT_AUTHOR_EMAIL={user_email} && set GIT_COMMITTER_NAME=Gemini CLI ({current_user}) && set GIT_COMMITTER_EMAIL={user_email}")

      work_dir <- "C:\\scripts"

      # Очищаем кавычки в запросе пользователя
      clean_query <- str_replace_all(user_query, '"', "'")

      # Формируем текст промпта
      cmd_prompt <- str_glue(
        "Ты запущен автономно в неинтерактивном режиме по запросу пользователя из Telegram-бота. ",
        "Запрос пользователя: ",
        "--- ",
        "{clean_query} ",
        "--- ",
        "КРИТИЧЕСКИ ВАЖНО: Для отправки итогового ответа ты ОБЯЗАН использовать навык 'telegram-sender'. ",
        "Вызови скрипт навыка и отправь ответ в Telegram со следующими параметрами: ",
        "--token \"{token}\" --chat-id \"{target_chat_id}\" --delete-message-id \"{wait_msg_id}\". ",
        "Если ответ объемный, содержит код или таблицы, сохрани его во временный markdown-файл и передай флаг --file. ",
        "Ты обязан завершить выполнение отправкой сообщения через этот навык."
      )

      # Экранируем двойные кавычки для безопасной передачи в cmd
      cmd_prompt_escaped <- str_replace_all(cmd_prompt, '"', '\\\\"')

      # Строим команду cmd
      cmd <- str_glue('{git_env} && cd /d "{work_dir}" && gemini --skip-trust --approval-mode=yolo -p "{cmd_prompt_escaped}"')

      cat("\n[GEMINI] Запуск команды для чата", target_chat_id, "\n")
      shell_res <- shell(cmd)
      cat("[GEMINI] Завершено с кодом:", shell_res, "\n")

      # Удаляем сообщение ожидания, если оно ещё не было удалено скриптом навыка
      try({
        bot_client <- Bot(token = token)
        bot_client$deleteMessage(chat_id = target_chat_id, message_id = wait_msg_id)
      }, silent = TRUE)

    }, error = function(e) {

      cat("[GEMINI ERROR]:", e$message, "\n")

      # Удаляем сообщение ожидания при ошибке
      try({
        bot_err <- Bot(token = token)
        bot_err$deleteMessage(chat_id = target_chat_id, message_id = wait_msg_id)
      }, silent = TRUE)

      try({
        bot_err <- Bot(token = token)
        bot_err$sendMessage(
          chat_id             = target_chat_id,
          parse_mode          = "html",
          reply_to_message_id = reply_msg_id,
          text                = str_glue("\U000274C Ошибка при выполнении запроса Gemini: {e$message}")
        )
      })

    })

  })

}
