## get tasks info
task_info <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  #browser()
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'task_info',
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
      bot_function = 'task_info',
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
    
    # read task list
    suppressWarnings( {
      task <- taskscheduler_ls(fill=TRUE) %>%
        mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
        filter( TaskName == args ) %>%
        unique()
    })
    
    # telegram
    if ( nrow(task) > 0 ) {
      
      task_status <- ifelse(task$`Last Result` == 0, "Успешно", paste0("<u>Ошибка</u> ", task$`Last Result`))
      task_status <- ifelse(task$`Last Result` == "267011", "Задача ещё ни разу не запускалась", task_status)
      task_status <- ifelse(task$`Last Result` == "267009", "Задача в данный момент выполняется", task_status)
      
      txt <- str_glue("<i>Информация о задаче {args}:</i> \n
                          Название: <code>{task$TaskName}</code>
                          Автор: {task$`Run As User`}
                          Статус: {task$`Scheduled Task State`}
                          Описание: {task$Comment}
                          Скрипт запуска: {task$`Task To Run`}
                          Работая директория: {task$`Start In`}
                          Последний запуск: {as.character(task$`Last Run Time`)}
                          Результат последнего запуска: {task_status}
                          Следующий запуск: {as.character(task$`Next Run Time`)}
                          Тип рассписания: {task$`Schedule Type`}
                          Дни запуска: {task$Days}
                          Месяца запуска: {task$Months}
                          Повторять: {task$`Repeat: Every`}
                          
                          Следующим сообщением по возможности пришлю или команду для запроса лога, или содержание bat файла.")
      
      # отправляем сообщение
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = txt)
      
      # запросить лог последнего выполнения
      file_ext <- tools::file_ext(unique(trimws(task$`Task To Run`)))
      
      if (tolower(file_ext) == 'r') {
        
        r_file    <- strsplit(unique(task$`Task To Run`), split = ' ')[[1]] %>% 
          .[length(.)]
        rout_path <- str_glue('`/get_file {task$`Start In`}\\{r_file}out`')
        
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "markdown",
                        text = rout_path)
      }
      
      if (tolower(file_ext) == 'bat') {
        
        bat_file  <- readLines(unique(task$`Task To Run`)) %>% 
          str_c(., collapse = '\n')
        
        bat_file  <- str_glue('```\n{bat_file}\n```')
        
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "markdown",
                        text = bat_file)
      }
      
    } else {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Задача ", args, " не найдена на сервере!"))
      
    }
  }
}
