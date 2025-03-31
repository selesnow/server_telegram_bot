## get tasks lists
running_tasks <- function(bot, update){
  
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
              bot_function = 'running_tasks',
              chat_id      = update$from_chat_id(),
              chat_title   = update$effective_chat()$title,
              user_id      = update$from_user_id(),
              msg_text     = 'нет прав',
              entities     = FALSE
            )
            
            
          } else {
            
            Sys.setlocale("LC_CTYPE", "russian")
            
            # bot typing action
            bot$sendChatAction(update$from_chat_id(),
                               action = "typing"
            )
            
            analysts_team <- dept::dp_get_team()
            analysts <- names(analysts_team)
            analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
            
            # read task list
            suppressWarnings( {
              task <- taskscheduleR::taskscheduler_ls(fill=TRUE) %>%
                #mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
                filter(str_detect(string = tolower(`Run As User`), analyst_filter)) %>%
                filter(`Scheduled Task State` == "Enabled") %>%
                filter(Status == "Running") %>%
                unique()
            })
            # with out quiet tasks
            # task <- filter(task, ! TaskName %in% quiet_tasks$task_name )
            
            # telegram
            if ( nrow(task) > 0 ) {
              
              # создаём текст для отправки в телеграм
              txt <- str_glue("<i>В данный момент выполняется {nrow(task)} задач:</i> \n")
              
              for( user in unique( task$`Run As User` ) ) {
                
                ut <-
                  task %>%
                  filter(`Run As User` == user) %>%
                  mutate(
                    last_run_time = as.character(`Last Run Time`),
                    during        = round(as.numeric(difftime(Sys.time(), mdy_hms(`Last Run Time`, tz = 'Europe/Helsinki'), units = 'mins')), 1)
                  ) %>% 
                  arrange(during) %>% 
                  mutate(TaskName = str_glue("{last_run_time}| <code>{TaskName}</code> ({during} мин.)")) %>%
                  #mutate(TaskName = paste0(as.character(`Last Run Time`), '| <code>', TaskName, '</code>', " | ", round(as.numeric(difftime(Sys.time(), mdy_hms(`Last Run Time`, tz = 'Europe/Helsinki'), units = 'mins')), 1), ' мин.')) 
                  pull(TaskName) %>%
                  #unlist() %>%
                  unique() %>%
                  str_c(., collapse = "\n")
                
                txt <- paste0(txt, "\n \n",paste0(str_glue("<b>{user} {nrow(filter(task, `Run As User` == user))} задач:</b>")),"\n \n", ut)
                
              }
              
              # отправляем сообщение в телеграм
              bot$sendMessage(chat_id = update$from_chat_id(),
                              parse_mode = "html",
                              text = txt)
            } else {
              
              # отправляем сообщение в телеграм
              bot$sendMessage(chat_id = update$from_chat_id(),
                              parse_mode = "html",
                              text = "В данный момент задачи не выполняются")
              
            }
          }
        },
        error = function(err) {
          
          uname <- update$effective_message()$from$username
          
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          reply_to_message_id = update$effective_message()$message_id,
                          text = str_glue("\U000274C @AlexeySeleznev Ошибка при вызове метода <code>running_tasks</code>: {err$message}"))
          
        }
      )
      
    }
  )
  
}
