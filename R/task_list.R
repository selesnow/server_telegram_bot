## get tasks lists
task_list <- function(bot, update) {
  
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
        bot_function = 'task_list',
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
      
      Sys.setlocale("LC_CTYPE", "russian")
      
      if ( ! check_previlegies(update$from_user_id(), 'task') ) {
        
        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для просмотра задач!"))
        
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'task_list',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
        
        
      } else {
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
            mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
            filter(str_detect(string = tolower(Author), analyst_filter) ) %>%
            filter(`Scheduled Task State` == "Enabled") %>%
            unique()
        })
        
        
        # telegram
        if ( nrow(task) > 0 ) {
          
          # создаём текст для отправки в телеграм
          txt <- "<i>Общий список заданий на сервере:</i> \n"
          
          for( user in unique( task$`Run As User` ) ) {
            
            # Отдельно по каждому пользователю получаем список задач
            ut <-
              task %>%
              filter(`Run As User` == user)
            
            # разбиваем задачи на части что бы влезли в сообщение
            task_number <- nrow(ut)
            iters  <- ceiling( task_number / 25 )
            parts  <- split(ut, 1:iters)
            n_part <- 1
            
            # отправляем отдельно каждую часть
            for ( p in parts ) {
              
              part_txt <- str_interp("( tasks ${n_part} - ${nrow(p) + n_part - 1} )")
              
              temp <- p %>%
                select(TaskName, Comment) %>%
                unique() %>%
                mutate(Comment = if_else( Comment == "N/A", "Без описания", Comment),
                       TaskName = paste0("<ins>", TaskName, "</ins>", " - <i>", Comment, "</i>" )) %>%
                select(TaskName) %>%
                unlist() %>%
                str_c(., collapse = "\n")
              
              # отправляем сообщение
              bot$sendMessage(chat_id = update$from_chat_id(),
                              parse_mode = "html",
                              text = paste0(txt, "\n \n",paste0(str_glue("<b>{user} {part_txt}:</b>")),"\n", temp))
              
              # переключаем часть
              n_part <- n_part + nrow(p)
              
            }
            
            n_part <- 1
          }
        } else {
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          text = "На сервере нет настроенных заданий.")
          
        }
      }
    }
  )
}
