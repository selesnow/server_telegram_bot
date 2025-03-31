# ###########################################################
# Function for work bot with database

# get current chat state
get_state <- function(chat_id, username) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  chat_state <- dbGetQuery(con, str_interp("SELECT state FROM chat_state WHERE chat_id = ${chat_id} and username = '${username}'"))$state
  
  if ( length(chat_state) == 0 ) return(FALSE)
  
  return(unlist(chat_state))
  
  dbDisconnect(con)
}

# set current chat state
set_state <- function(chat_id, username, state) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO chat_state (chat_id, username, state)
                VALUES(${chat_id}, '${username}', '${state}') 
                ON CONFLICT(chat_id, username) 
                DO UPDATE SET state='${state}';
            ")
  )
  
  dbDisconnect(con)
  
}

# write chat data
set_chat_data <- function(chat_id, username, field, value) {
  
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO chat_data (chat_id, username, ${field})
                VALUES(${chat_id}, '${username}', '${value}') 
                ON CONFLICT(chat_id, username) 
                DO UPDATE SET ${field}='${value}';
            ")
  )
  
  dbDisconnect(con)
  
}

# read chat data
get_chat_data <- function(chat_id, username, field) {
  
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  data <- dbGetQuery(con, 
                     str_interp("
            SELECT ${field}
            FROM chat_data
            WHERE chat_id = ${chat_id} AND username = '${username}';
            ")
  )
  
  dbDisconnect(con)
  
  return(data[[field]])
  
}

# add user
add_user <- function(update = update) {
  
  # get variables
  msg        <- update$effective_message()
  user_id    <- msg$from$id
  is_bot     <- msg$from$is_bot
  first_name <- msg$from$first_name
  last_name  <- msg$from$last_name
  username   <- msg$from$username
 
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO bot_users (user_id, is_bot, first_name, last_name, last_activity, username)
                VALUES(${user_id}, 
                       ${is_bot}, 
                      '${first_name}',
                      '${last_name}',
                      '${Sys.time()}',
                      '${username}') 
                ON CONFLICT(user_id) 
                DO UPDATE SET 
                is_bot        =  ${is_bot},
                first_name    = '${first_name}',
                last_name     = '${last_name}',
                last_activity = '${Sys.time()}',
                username      = '${username}';
            ")
  )
  
  dbDisconnect(con)
  
}

# get_user_info
db_get_user_info <- function(username) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  user_info <- 
    dbGetQuery(con, 
               str_interp("
            SELECT *
            FROM bot_users bu 
            LEFT JOIN users_previlegies up 
            ON bu.user_id = up.user_id 
            WHERE LOWER(bu.username) = '${tolower(username)}'
            ")   )
  
  dbDisconnect(con)
  
  if ( length(user_info$user_id) < 1 ) {
    
    return(NULL)
    
  } else {
    
    user_info <- list(
      username = unique(user_info$username),
      user_id = unique(user_info$user_id),
      first_name = unique(user_info$first_name),
      last_name = unique(user_info$last_name),
      last_activity = unique(user_info$last_activity),
      previlegies = list(
        priv = user_info$command,
        priv_add_datetime = user_info$add_datetime
      )
    )
    
    return(user_info)
    
  }
  
}

# get_user_id
get_user_id <- function(first_name, last_name) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  all_ids <- 
    dbGetQuery(con, 
          str_interp("
            SELECT user_id 
            FROM bot_users 
            WHERE 
            first_name = '${first_name}'
            AND last_name = '${last_name}';
            ")   )
  
  dbDisconnect(con)
  
  if ( length(all_ids$user_id) < 1 ) {
    
    return(NULL)
    
  } else {
  
    ids <- str_c(all_ids$user_id, collapse = ', ')
    
    return(ids)
  
  }
  
}

# add privilegies
add_privilege <- function(user_id, command) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            INSERT INTO users_previlegies (user_id, command, add_datetime)
                VALUES(${user_id}, 
                      '${command}',
                      '${Sys.time()}') 
                ON CONFLICT(user_id, command) 
                DO UPDATE SET 
                command = '${command}',
                add_datetime = '${Sys.time()}';
            ")
  )
  
  dbDisconnect(con)
  
}

# remove privilegies
remove_privilege <- function(user_id, command) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  # upsert состояние чата
  dbExecute(con, 
            str_interp("
            DELETE from users_previlegies 
            WHERE 
            user_id = ${user_id}
            AND command = '${command}';
            ")
  )
  
  dbDisconnect(con)
  
}

# check user previlegies
check_previlegies <- function(user_id, command) {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  previlegies <- dbGetQuery(con, 
          str_interp("
            SELECT command 
            FROM users_previlegies 
            WHERE 
            user_id = '${user_id}'
            AND command = '${command}';
            "))
  
  dbDisconnect(con)
  
  if ( nrow(previlegies) > 0 ) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

# get session id
get_session_id <- function() {
  
  con <- dbConnect(SQLite(), cfg$db_settings$db_path)
  
  id <- 
    dbGetQuery(con, 
               "select max(session_id) as session_id from log_session_start")
  
  dbDisconnect(con)
  return(id$session_id)
  
}

# create table
df_to_tg <- function( table, align = NULL, indents = 3, parse_mode = 'Markdown' ) {
  
  # если выравнивание не задано то выравниваем по левому краю
  if ( is.null(align) ) {
    
    col_num <- length(table)
    align   <- str_c( rep('l', col_num), collapse = '' )
    
  }
  
  # проверяем правильно ли заданно выравнивание
  if ( length(table) != nchar(align) ) {
    
    align <- NULL
    
  }
  
  # новое выравнивание столбцов
  side <- sapply(1:nchar(align),
                 function(x) {
                   letter <- substr(align, x, x)
                   switch (letter,
                           'l' = 'right',
                           'r' = 'left',
                           'c' = 'both',
                           'left'
                   )
                 })
  
  # сохраняем имена
  t_names      <- names(table)
  
  # вычисляем ширину столбцов
  names_length <- sapply(t_names, nchar)
  value_length <- sapply(table, function(x) max(nchar(as.character(x))))
  max_length   <- ifelse(value_length > names_length, value_length, names_length)
  
  # подгоняем размер имён столбцов под их ширину + указанное в indents к-во пробелов
  t_names <- mapply(str_pad,
                    string = t_names,
                    width  = max_length + indents,
                    side   = side)
  
  # объединяем названия столбцов
  str_names <- str_c(t_names, collapse = '')
  
  # аргументы для фукнции str_pad
  rules <- list(string = table, width = max_length + indents, side = side)
  
  # поочереди переводим каждый столбец к нужному виду
  t_str <-   pmap_df( rules, str_pad )%>%
    unite("data", everything(), remove = TRUE, sep = '') %>%
    unlist(data) %>%
    str_c(collapse = '\n')
  
  # если таблица занимает более 4096 символов обрезаем её
  if ( nchar(t_str) >= 4021 ) {
    
    warning('Таблица составляет более 4096 символов!')
    t_str <- substr(t_str, 1, 4021)
    
  }
  
  # символы выделения блока кода согласно выбранной разметке
  code_block <- switch(parse_mode,
                       'Markdown' = c('```', '```'),
                       'HTML' = c('`', '`'))
  
  # переводим в code
  res <- str_c(code_block[1], str_names, t_str, code_block[2], sep = '\n')
  
  return(res)
}
