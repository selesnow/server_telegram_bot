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

  # Извлекаем текст запроса: из оригинального сообщения с сохранением переносов строк или из args
  user_query <- sub("^/gemini(@\\w+)?\\s*", "", msg$text)
  if (is.null(user_query) || is.na(user_query) || nchar(trimws(user_query)) == 0) {
    user_query <- paste(args, collapse = " ")
  }

  # Проверка аргументов
  if ( length(args) == 0 && nchar(trimws(user_query)) == 0 ) {
    bot$sendMessage(
      chat_id    = update$from_chat_id(),
      parse_mode = "html",
      text       = "\U00002139 Укажите запрос после команды <code>/gemini</code>.\nПример: <code>/gemini Проверь статус задач на сервере</code>"
    )
    return()
  }

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

      # Устанавливаем переменные окружения для git
      Sys.setenv(
        GIT_AUTHOR_NAME     = paste0("Gemini CLI (", current_user, ")"),
        GIT_AUTHOR_EMAIL    = user_email,
        GIT_COMMITTER_NAME  = paste0("Gemini CLI (", current_user, ")"),
        GIT_COMMITTER_EMAIL = user_email
      )

      work_dir <- "C:\\scripts"
      old_wd   <- setwd(work_dir)
      on.exit(setwd(old_wd), add = TRUE)

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

      # Аргументы командной строки
      cli_args <- c(
        "--skip-trust",
        "--approval-mode=yolo",
        "-p",
        shQuote(cmd_prompt, type = "cmd")
      )

      cat("\n[GEMINI] Запуск команды для чата", target_chat_id, "\n")
      res_code <- system2(
        command = "gemini.cmd",
        args    = cli_args,
        stdout  = "",
        stderr  = ""
      )
      cat("[GEMINI] Завершено с кодом:", res_code, "\n")

      if (res_code != 0) {
        stop(str_glue("Процесс Gemini CLI завершился с кодом ошибки {res_code}"))
      }

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
