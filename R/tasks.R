tasks <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'task',
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
      if ( ! check_previlegies(update$from_user_id(), 'task') ) {
        
        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для просмотра задач!"))
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'tasks',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
        
      } else {
        
        con <- dbConnect(SQLite(), cfg$db_settings$db_path)
        quiet_tasks <- dbGetQuery(con, "SELECT task_name FROM forget_queue WHERE quiet_till > datetime('now', 'localtime')")
        dbDisconnect(con)
        
        Sys.setlocale("LC_CTYPE", "russian")
        
        # bot typing action
        bot$sendChatAction(update$from_chat_id(),
                           action = "typing"
        )
        
        analysts_team <-  dept::dp_get_team()
        analysts <- names(analysts_team)
        analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
        
        # read task list
        suppressWarnings( {
          task <- taskscheduleR::taskscheduler_ls(fill=TRUE) %>%
            mutate(`Run As User` = str_remove_all(`Run As User`, 'ANALYTICS\\\\')) %>% 
            mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
            filter(str_detect(string = tolower(`Run As User`), analyst_filter) &
                     ! `Last Result`  %in% c("0", "267011", "2") ) %>%
            filter(`Scheduled Task State` == "Enabled") %>%
            filter(Status != "Running") %>%
            unique()
        })
        # with out quiet tasks
        task <- filter(task, ! TaskName %in% quiet_tasks$task_name )
        
        # telegram
        if ( nrow(task) > 0 ) {
          
          # создаём текст для отправки в телеграм
          txt <- "<i>Задачи которые завершились аварийно!:</i> \n"
          
          for( user in unique( task$`Run As User` ) ) {
            
            ut <-
              task %>%
              filter(`Run As User` == user) %>%
              mutate(TaskName = paste0('<code>', TaskName, '</code>'," ( ", `Last Result`, " ) |", as.character(`Last Run Time`))) %>%
              select(TaskName) %>%
              unlist() %>%
              unique() %>%
              str_c(., collapse = "\n")
            
            txt <- paste0(txt, "\n \n",paste0(str_glue("<b>{user}:</b>")),"\n \n", ut)
            
          }
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          text = txt)
        } else {
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          text = "Все ровненько, задачи выполненны без ошибок.")
          
        }
      }
    },
    error = function(err) {
      
      uname <- update$effective_message()$from$username
      
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      reply_to_message_id = update$effective_message()$message_id,
                      text = str_glue("\U000274C @AlexeySeleznev Ошибка при вызове метода <code>tasks</code>: {err$message}"))
      
    }
  )
  
}
