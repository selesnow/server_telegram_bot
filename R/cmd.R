# запустить команду на сервере --------------------------------------------
cmd <- function(bot, update, args) {
  
  promises::future_promise(
    {
      # write userdata
      add_user(update)
      
      # log
      # выт§гиваем сообщение
      msg <- update$effective_message()
      
      # логируем основную информацию
      lg$info(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'cmd',
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
      
      if ( ! check_previlegies(update$from_user_id(), 'cmd') ) {
        
        # отправл§ем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 Ф вас не достаточно прав дл§ запуска команд на сервере!"))
        
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'cmd',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
        
      } else {
        
        args <- paste(args, collapse= " ")
        cmd_out <- try(system(args, intern = T, wait = T))
        
        if (inherits(cmd_out, 'try-error')) {
          cmd_out <- attr(cmd_out, 'condition') %>% as.character()
        } else {
          cmd_out <- paste(cmd_out, collapse= "\n")
        }
        
        bot$sendMessage(chat_id = update$from_chat_id(),
                        text = str_glue("*Цезультат*\n\n{cmd_out}"),
                        reply_to_message_id = update$effective_message()$message_id,
                        parse_mode = 'markdown')
        
      }
    }
  )
}
