##run task command
run <- function(bot, update, args){
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'run',
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
  
  
  tryCatch(expr =
             
             {
               
               library(taskscheduleR)
               
               if ( ! check_previlegies(update$from_user_id(), 'run') ) {
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для запуска задач!"))
                 
                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'run',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )
                 
                 
               } else if (length(args) > 1) {
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("Одновременно можно запустить не более 1 задачи!"))
               } else {
                 
                 taskcheduler_runnow(args)
                 
                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("Задача <b>", args, "</b> запущена на сервере! "))
                 
               }
             },
           error = function(err) {
             uname <- update$effective_message()$from$username
             
             bot$sendMessage(chat_id = update$from_chat_id(),
                             parse_mode = "html",
                             reply_to_message_id = update$effective_message()$message_id,
                             text = str_glue("\U000274C @AlexeySeleznev Ошибка при вызове метода <code>run</code>: {err$message}"))
             
           }
  )
  
}
