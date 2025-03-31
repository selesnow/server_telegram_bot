## get quite tasks
quite_tasks <- function(bot, update){
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'quite_tasks',
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
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  queue_tasks <- dbGetQuery(con, "SELECT * FROM forget_queue WHERE quiet_till > datetime('now', 'localtime')") %>%
    mutate(info = str_glue("{quiet_till}\t{task_name}"))
  
  dbDisconnect(con)
  
  n_tasks <- nrow(queue_tasks)
  
  txt <- str_glue("<i>Количество отключенных от мониторинга задач: {n_tasks}</i> \n")
  
  txt <- paste0(txt, "\n", str_c(queue_tasks$info, collapse = "\n"))
  
  # отправляем сообщение в телеграм
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "html",
                  text = txt)
}
