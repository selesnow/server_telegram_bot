library(lgr)
library(lgrExtra)
library(RSQLite)

# add logers level
add_log_levels(c(sessions = 75, stop = 80))

# 
# dbilo <- LayoutDbi$new(
#   col_types = c(level     = "integer", 
#                 timestamp = "timestamp", 
#                 logger    = "varchar(256)",
#                 caller    = "varchar(256)", 
#                 msg       = "varchar(2048)", 
#                 chat_id   = "bigint",
#                 user_id   = "bigint",
#                 )
# )

# create logers
lg <- get_logger_glue("bot_logger")

# add loggers
## for sessions
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "session", 
    lgrExtra::AppenderDbi$new(
      #layout      = dbilo,
      conn        = DBI::dbConnect(RSQLite::SQLite(), cfg$db_settings$db_path),
      table       = "log_session_start",
      buffer_size = 0
    )
  )

## for errors
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "errors", 
    lgrExtra::AppenderDbi$new(
      #layout      = dbilo,
      conn        = DBI::dbConnect(RSQLite::SQLite(), cfg$db_settings$db_path),
      table       = "log_errors",
      buffer_size = 0
    )
  )

## for all events
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "work", 
    lgrExtra::AppenderDbi$new(
      #layout      = dbilo,
      conn        = DBI::dbConnect(RSQLite::SQLite(), cfg$db_settings$db_path),
      table       = "log_work",
      buffer_size = 0
    )
  )

## for entities
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "entities", 
    lgrExtra::AppenderDbi$new(
      #layout      = dbilo,
      conn        = DBI::dbConnect(RSQLite::SQLite(), cfg$db_settings$db_path),
      table       = "log_entities",
      buffer_size = 0
    )
  )

# set threshold
lg$appenders$session$set_threshold('sessions')
lg$appenders$errors$set_threshold('error')
lg$appenders$work$set_threshold('all')

# set filters
## error
lg$appenders$errors$set_filters( list( function(event) { ! event$level == 75 && ! event$entities }) )

## works
#lg$appenders$work$set_filters( list( function(event) { ! event$level %in% c(75, 80) && ! event$entities }) )
lg$appenders$work$add_filter(filter = function(event) { ! event$level %in% c(75, 80)}, name = 'f_sessions')
lg$appenders$work$add_filter(filter = function(event) { ! event$entities }, name = 'f_entities')

## entities
lg$appenders$entities$set_filters( list( function(event) { event$entities }) )
# 
# # actions
# ## start session
# lg$log(level = 'sessions',
#        entities = FALSE)
# 
# ## write error
# lg$error('Any Error', 
#          session_id = 8, 
#          action     = 'Тестирую уровень error',  
#          message_id = 1, 
#          error_text = 'Error text2',
#          bot_function = 'my_bot_function',
#          stop_reason = "error",
#          chat_id = 1323,
#          user_id = 4343,
#          msg_text = "Message TG TExt",
#          entities = FALSE)
# 
# # write action
# lg$info(
#        session_id = 15, 
#        action     = 'Тестирую уровень info',  
#        message_id = 3, 
#        error_text = NULL,
#        bot_function = 'bot_function2',
#        chat_id = 4553,
#        user_id = 53453,
#        msg_text = "Текст сообщения",
#        entities = FALSE
#        )
# 
# 
# 
# 
# 
# update <- upds[[4]]
# # write entities
# # запись вложений
# msg <- update$effective_message()
#   
# for ( ent in msg$entities ) {
#     lg$info(
#       session_id = 15,
#       message_id = msg$message_id,
#       offset     = ent$offset,
#       length     = ent$length,
#       type       = ent$type,
#       url        = ent$url,
#       entities = TRUE
#     )
#     
# }
#   
# 
# 
# 
# # #############################################
# # #############################################
# # #############################################
# # #############################################
# message_id	"offset"	"length"	"type"	url
# 
# # write stop
# lg$log(level = 'stop', 
#        session_id = 10,
#        message_id = 4,
#        error_text = NULL,
#        stop_reason = "user")
# 
# lg$info(
#   message_id   = msg$message_id,
#   session_id   = get_session_id(),
#   bot_function = ,
#   chat_id      = ,
#   user_id      = ,
#   msg_text     = ,
#   entities     = FALSE
# )
# 
# 
# 
# lg$error(
#   message_id   = msg$message_id,
#   session_id   = get_session_id(),
#   error_text   = ,
#   timestamp    = ,
#   stop_reason  = ,
#   level        = ,
#   bot_function = ,
#   chat_id      = ,
#   user_id      = ,
#   msg_text     = ,
#   entities     = FALSE
# )
# 
# 
# fields <- c('message_id', "offset", "length", "type", 'url', 'session_id', 'message_id', 'error_text', "timestamp", 'stop_reason', "level", 'session_id', "timestamp", 'session_id', 'message_id', 'bot_function', 'chat_id', 'user_id', 'msg_text', "timestamp", "level")
# cat(paste0(unique(fields), sep = '\n'))
# log_entities
# 
# 
# 
