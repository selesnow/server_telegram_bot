## reload ppc data in google big query
data_reload <- function(bot, update, args) {
  
  promises::future_promise(
    
    {
      
      # write userdata
      add_user(update)
      
      # log
      # вытягиваем сообщение
      msg <- update$effective_message()
      
      # логируем основную информацию
      lg$info(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'data_reload',
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
      
      # emoji
      access_denied_smile <- '\U00026D4'
      err_smile           <- '\U000274C'
      ok_smile            <- '\U0002705'
      time_smile          <- '\U00023F0'
      
      tryCatch(expr = {
        
        # variables
        source    <- args[1]
        date_from <- args[2]
        date_to   <- args[3]
        proj_list <- args[4:length(args)]
        username  <- update$effective_message()$from$username
        
        # check user
        if ( ! check_previlegies(update$from_user_id(), 'data_reload') ) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{access_denied_smile} У вас нет прав для запуска перезагрузки данных по проектам."),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'нет прав',
            entities     = FALSE
          )
          
          
          # check variables
        } else if ( is.null(source) | is.null(date_from) | is.null(date_to) | is.null(proj_list) | length(args) < 4) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Вы указали не все параметры запроса, запрос имеет следующий формат <code>/reload source date_from date_to proj_id_1 proj_id_2 ... proj_id_n</code>"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'указаны не все параметры запроса',
            entities     = FALSE
          )
          
        } else if ( ! source %in% c('fb', 'ga', 'ya', 'mt', 'fb_custom', 'ga_custom', 'all') ) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Указан неверный источник {source}: возможные варианты fb, fb_custom, ga, ga_custom, ya, mt"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'указан неверный источник',
            entities     = FALSE
          )
          # check first date
        } else if ( ! str_detect(date_from, '(\\d{4})-(\\d{2})-(\\d{2})') ) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Неверно указана начальная дата {date_from}, формат ввода даты ГГГГ-ММ-ДД"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'неверно указана начальная дата',
            entities     = FALSE
          )
          # check last date
        } else if ( ! str_detect(date_to, '(\\d{4})-(\\d{2})-(\\d{2})') ) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Неверно указана конечная дата {date_from}, формат ввода даты ГГГГ-ММ-ДД"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'неверно указана конечная дата',
            entities     = FALSE
          )
          # check that the first date is earlier than the last date
        } else if ( as.Date(date_from) > as.Date(date_to) ) {
          
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Начальная дата не может быть позже конечно, ты указал начальной {date_from} а конечную {date_to}"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'начальная дата больше конечной',
            entities     = FALSE
          )
          # run procedure
        } else {
          
          # detect function
          fun <- switch (source,
                         'fb'        = 'fb_load',
                         'fb_custom' = 'fb_custom_load',
                         'ga'        = 'ga_load',
                         'ga_custom' = 'ga_custom_load',
                         'ya'        = 'ya_load',
                         'mt'        = 'mt_load',
                         'all'       = 'ppc_load'
          )
          
          # send wait message
          msg_wait <- bot$sendMessage(update$from_chat_id(),
                                      text = str_glue("{time_smile} Перезагрузка данных по указанным проектам запущена, я сообщу когда обработка данных будет завершена!"),
                                      parse_mode = "HTML")
          
          # run fun
          do.call(fun, list(proj_list, date_from, date_to))
          
          # delete waiting message
          bot$deleteMessage(update$from_chat_id(), msg_wait$message_id)
          
          # complete message
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue('{ok_smile} Данные по указанным проектам успешно перезагружены!'),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
        }
      },
      error = function(err) {
        
        # delete waiting message
        if ( exists('msg_wait') ) {
          bot$deleteMessage(update$from_chat_id(), msg_wait$message_id)
        }
        
        # error message
        # complete message
        bot$sendMessage(update$from_chat_id(),
                        text = str_glue("@AlexeySeleznev \n{err_smile} В ходе перезагрузки данных возникла ошибка: {err$message}"),
                        reply_to_message_id = update$effective_message()$message_id,
                        parse_mode = "HTML")
      })
    }, 
    packages = c('ppcLoader')
  )
}
