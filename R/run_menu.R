## run menu inline
run_menu <- function(bot, update) {
  
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
        bot_function = 'run_menu',
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
      
      tryCatch(
        
        expr = {
          
          if ( ! check_previlegies(update$from_user_id(), 'run_menu') ) {
            
            # отправляем сообщение в телеграм
            bot$sendMessage(chat_id = update$from_chat_id(),
                            parse_mode = "html",
                            text = paste0("\U00026D4 У вас не достаточно прав для запуска задач!"))
            
            
            # log
            lg$warn(
              message_id   = msg$message_id,
              session_id   = get_session_id(),
              bot_function = 'run_menu',
              chat_id      = update$from_chat_id(),
              chat_title   = update$effective_chat()$title,
              user_id      = update$from_user_id(),
              msg_text     = 'нет прав',
              entities     = FALSE
            )
            
            
            
          } else {
            
            con <- dbConnect(SQLite(), cfg$db_settings$db_path)
            quiet_tasks <- dbGetQuery(con, "SELECT task_name FROM forget_queue WHERE quiet_till > datetime('now')")
            dbDisconnect(con)
            
            Sys.setlocale("LC_CTYPE", "russian")
            
            # bot typing action
            bot$sendChatAction(update$from_chat_id(),
                               action = "typing"
            )
            
            analysts_team <- dept::dp_get_team()
            analysts <- names(analysts_team)
            analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
            
            # get tasks
            suppressWarnings( {
              task <- taskscheduleR::taskscheduler_ls(fill=TRUE) %>%
                mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
                filter(str_detect(string = tolower(Author), analyst_filter) &
                         ! `Last Result`  %in% c("0", "267011") ) %>%
                filter(`Scheduled Task State` == "Enabled") %>%
                filter(Status != "Running") %>%
                select(TaskName) %>%
                unique() %>%
                unlist(use.names = FALSE)
            })
            
            # with out quiet tasks
            task <- task[ ! task %in% quiet_tasks$task_name ]
            
            if ( length(task) == 0 ) {
              
              # отправляем сообщение в телеграм если все задачи выполненны успешно
              bot$sendMessage(chat_id = update$from_chat_id(),
                              parse_mode = "html",
                              text = "Все ровненько, задачи выполненны без ошибок.")
              
            } else {
              
              names(task) <- NULL
              # создаём меню в случае если есть проблемные таски
              task_number <- length(task)
              iters <- ceiling( task_number / 2 )
              parts <- split(1:task_number, 1:iters)
              
              # create keyboard
              keys <- list()
              
              # check task name for more than 64 characters
              longname_tasks <- task[nchar(task) > 64]
              
              if ( length(longname_tasks) > 0 ) {
                
                longname_tasks <- paste0(longname_tasks, collapse = '\n')
                
                # Уведомление о задачах которые нельзя запустить кнопкой
                bot$sendMessage(chat_id = update$from_chat_id(),
                                parse_mode = "html",
                                text = paste0("<b>Внимание!!!</b> Следующие задачи имеют в название более 64 символов, их нельзя запустить кнопкой, используйте команду <code>/run имя_задачи</code> \n\n", longname_tasks))
                
              }
              
              for ( p in parts ) {
                
                keys_r <- lapply(task[p], function(x) InlineKeyboardButton(x, callback_data = str_sub(x, 1, 64)))
                
                keys <- append(keys, list(keys_r))
                
              }
              
              IKM <- InlineKeyboardMarkup(
                inline_keyboard = keys
              )
              
              # Send Custom Keyboard
              bot$sendMessage(update$from_chat_id(),
                              text = "Меню запуска задач",
                              reply_markup = IKM)
            }
            
          }
        },
        error = function(err) {
          
          uname <- update$effective_message()$from$username
          
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          reply_to_message_id = update$effective_message()$message_id,
                          text = str_glue("\U000274C @AlexeySeleznev Ошибка при вызове метода <code>run_menu</code>: {err$message}"))
          
        }
      )
    }
  )
}
