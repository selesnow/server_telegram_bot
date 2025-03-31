# netpeak bi bot
library(telegram.bot)
library(RSQLite)
library(DBI)
library(taskscheduleR)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ppcLoader)
library(configr)
library(httr)
library(purrr)
library(lubridate)
library(future)
library(promises)

Sys.setlocale("LC_CTYPE", "russian")

# Включаем параллельный план вычислений
future::plan('multisession')

# create bot instance
updater <- Updater(token = bot_token("Netpeak BI Bot"))

# очищаем старые обновления
updater$bot$clean_updates()

# имя бота
botname <- updater$bot$getMe()$username

# config
cfg <- read.config('C:\\my_develop_workshop\\telegram_bot\\config.cfg')

# Read function
source('C:\\my_develop_workshop\\telegram_bot\\db_bot_function.R')
source('C:\\my_develop_workshop\\telegram_bot\\logger.R')

# сообщаем о том, что бот вклчючен
bot_start_time <- Sys.time()

## start session
lg$log(level = 'sessions',
       entities = FALSE)

updater$bot$sendMessage(chat_id = cfg$info_chat$chat_id,
                        text = str_glue('*Бот запущен* в {bot_start_time}, id сессии {get_session_id()}'),
                        parse_mode = 'Markdown')

# Commands #####################
## start menu command
start <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'start',
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

  # set state
  set_state(update$from_chat_id(),
            update$from_user_id(),
            'start')

  RKM <- ReplyKeyboardMarkup(
    keyboard = list(
      list(KeyboardButton("Помощь")),
      list(KeyboardButton("Список задач")),
      list(KeyboardButton("Меню запуска задач")),
      list(KeyboardButton("Тихие задачи")),
      list(KeyboardButton("Полный список задач")),
      list(KeyboardButton("Получить id чата"))
    ),
    resize_keyboard = TRUE,
    one_time_keyboard = FALSE
  )

  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Выберите команду",
                  reply_markup = RKM)

}

## Помощь
get_template <- function(bot, update) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_template',
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
  
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(InlineKeyboardButton(text = "Поставить задачу на аналитиков", url = 'https://t.netpeak.group/task/1730126'))
    )
  )
  
  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Задачи на отдел аналитики ставятся по шаблону, нажмите на кнопку под этим сообщением.",
                  reply_markup = IKM, parse_mode = 'Markdown')
  
}

# #########################################
## Помощь
help <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'help',
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

  IKM <- InlineKeyboardMarkup(
    inline_keyboard = list(
      list(InlineKeyboardButton(text = "Помощь", url = 'https://doc-naf.netpeak.net/books/внутренний-телеграм-бот-департамента-аналитики'))
    )
  )

  # Send Custom Keyboard
  bot$sendMessage(update$from_chat_id(),
                  text = "Документаця доступна только с использованием OpenVPN, информация по его установке и настройке доступна по [ссылке](https://wiki.netpeak.net/Windows#12._.D0.A3.D1.81.D1.82.D0.B0.D0.BD.D0.BE.D0.B2.D0.BA.D0.B0_.D0.B8_.D0.BD.D0.B0.D1.81.D1.82.D1.80.D0.BE.D0.B9.D0.BA.D0.B0_Openvpn).",
                  reply_markup = IKM, parse_mode = 'Markdown')

}

# #########################################
## get tasks lists
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
    
            con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")
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
              filter(str_detect(string = tolower(Author), analyst_filter) &
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

# #########################################
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

# #########################################
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

# #########################################
## Get chat id command
chat_id <- function(bot, update){

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'chat_id',
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

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "Markdown",
                    text = paste0("Chat ID этой группы: ", update$from_chat_id()))

}

## run menu
run_menu_old <- function(bot, update){

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'run_menu_old',
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

  if ( ! ! check_previlegies(update$from_user_id(), 'run_menu') ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для запуска задач!"))


    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'run_menu_old',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )


  } else {

    con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")
    quiet_tasks <- dbGetQuery(con, "SELECT task_name FROM forget_queue WHERE quiet_till > datetime('now')")
    dbDisconnect(con)

    Sys.setlocale("LC_CTYPE", "russian")

    # bot typing action
    bot$sendChatAction(update$from_chat_id(),
                       action = "typing"
    )
    
    analysts_team <-  dept::dp_get_team()
    analysts <- names(analysts_team)
    analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()

    # get tasks
    suppressWarnings( {
    task <- taskscheduleR::taskscheduler_ls(fill=TRUE) %>%
      mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
      filter(str_detect(string = tolower(Author), analyst_filter) &
               `Last Result` != "0" &
               between(as.Date(`Last Run Time`), Sys.Date() - 4 , Sys.Date()) ) %>%
      filter(`Scheduled Task State` == "Enabled") %>%
      filter(Status != "Running") %>%
      select(TaskName) %>%
      unlist()
    })

    # with out quiet tasks
    task <- task[ ! task %in% quiet_tasks$task_name ]

    if ( length(task) == 0 ) {

      # отправляем сообщение в телеграм если все задачи выполненны успешно
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = "Все ровненько, задачи выполненны без ошибок.")

    } else {

      task <- str_c("/run ", task)
      # создаём меню в случае если есть проблемные таски
      task_number <- length(task)
      iters <- floor( task_number / 3 )
      parts <- split(1:task_number, 1:iters)

      keys <- list(list(KeyboardButton("/start")))

      for ( p in parts ) {

        keys_r <- lapply(task[p], KeyboardButton)

        keys <- append(keys, list(keys_r))

      }

      RKM_runner <- ReplyKeyboardMarkup(
        keyboard = keys,
        resize_keyboard = TRUE,
        one_time_keyboard = FALSE
        )

      # Send Custom Keyboard
      bot$sendMessage(update$from_chat_id(),
                      text = "Меню запуска задач",
                      reply_markup = RKM_runner)
    }
 }
}

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
    
              con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")
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

## handler for run menu
answer_cb <- function(bot, update) {

  # write userdata
  #effective_user <- list(message = list( from = update$effective_user()))

  # write to db
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'answer_cb',
    chat_id      = update$from_chat_id(),
    chat_title   = update$effective_chat()$title,
    user_id      = update$from_user_id(),
    msg_text     = msg$text,
    cb_data      = update$callback_query$data,
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

  data <- update$callback_query$data

  taskcheduler_runnow(data)

  # Send Custom Keyboard
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "html",
                  text = paste0("Задача <b>", data, "</b> запущена на сервере! "))

  bot$answerCallbackQuery(callback_query_id = update$callback_query$id)
}

## task to quite
forget <- function(bot, update, args){

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'forget',
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

  if ( ! check_previlegies(update$from_user_id(), 'quite_tasks')  ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для отключения задач от мониторинга!"))


    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'forget',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )



  } else {

    con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")

    df <- data.frame(task_name  = args[1],
                     quiet_till = paste(args[2], args[3]))

    dbWriteTable(con,
                 name = 'forget_queue',
                 value = df,
                 append = TRUE)

    dbDisconnect(con)

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("Задача {args} добавлена в список тихих!"))
  }
}

## get quite tasks
unforget <- function(bot, update, args){

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'unforget',
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


  if ( ! check_previlegies(update$from_user_id(), 'quite_tasks')) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для управления мониторингом задач!"))


    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'unforget',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )


  } else {

    con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")

    queue_tasks <- dbExecute(con, str_glue("DELETE FROM forget_queue WHERE quiet_till > datetime('now', 'localtime') AND task_name = '{args}'"))

    dbDisconnect(con)

    if ( queue_tasks == 1 ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("Задача {args} удалена из списка тихих!"))

    } else {

      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = "Неудалось удалить задачу {args} из списка тихих!")

    }

 }
}

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

  con <- dbConnect(SQLite(), "C:/my_develop_workshop/telegram_bot/bot_db.db")

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

## finde task by name
task_search <- function(bot, update, args){

      
      # write userdata
      add_user(update)
    
      # log
      # вытягиваем сообщение
      msg <- update$effective_message()
    
      # логируем основную информацию
      lg$info(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'task_search',
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
          bot_function = 'task_search',
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
    
          # pattern
          #pat <- str_c(args, collapse = "|")
          # pat <- str_glue('({str_c(args, collapse = "|")})') %>% rep(., length(args)) %>%  str_c(collapse = '.*')
          # advance search
          # мы берём поисковый запрос, и ищем все ключевые слова в любом порядке, 
          # при этом каждое ключевое слово может встретится в результатах только один раз
          pat <- c()
          for ( x in 1:length(args) ) {
            
            if (x == 1) {
              gr <- ''
            } else {
              parts <- str_c('\\', 1:(x-1), collapse = '|')
              gr <- str_glue('(?!{parts})')
            }
            
            pat[x] <- str_glue('{gr}({str_c(args, collapse = "|")})') 
            
          }
          pat <- str_c(pat, collapse = '.*') %>% str_to_lower()
          
          analysts_team <- dept::dp_get_team()
          analysts <- names(analysts_team)
          analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
    
          # read task list
          suppressWarnings( {
          task <- taskscheduleR::taskscheduler_ls(fill=TRUE) %>%
            mutate(`Last Run Time` = parse_datetime(`Last Run Time`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
            filter(str_detect(string = tolower(Author), analyst_filter) ) %>%
            #filter(`Scheduled Task State` == "Enabled") %>%
            filter(str_detect(str_to_lower(.$TaskName), pat, )|str_detect(str_to_lower(.$`Task To Run`), pat)|str_detect(str_to_lower(.$Comment), pat)) %>%
            unique()
          })
    
          # telegram
          if ( nrow(task) > 0 ) {
    
            # создаём текст для отправки в телеграм
            txt <- "<i>Найденные задачи:</i> \n"
    
            for( user in unique( task$`Run As User` ) ) {
    
              # Отдельно по каждому пользователю получаем список задач
              ut <-
                task %>%
                filter(`Run As User` == user)
    
              # разбиваем задачи на части что бы влезли в сообщение
              task_number <- nrow(ut)
              iters  <- ceiling( task_number / 50 )
              parts  <- split(ut, 1:iters)
              n_part <- 1
    
              # отправляем отдельно каждую часть
              for ( p in parts ) {
    
                part_txt <- str_interp("( tasks ${n_part} - ${nrow(p) + n_part - 1} )")
    
                temp <- p %>%
                  select(TaskName, Comment) %>%
                  unique() %>%
                  mutate(Comment = if_else( Comment == "N/A", "Без описания", Comment),
                         TaskName = paste0("<code>",TaskName, "</code>", " - <i>", Comment, "</i>" )) %>%
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
                            text = str_glue("На сервере нет задач согласно заданному фильтру - '{pat}'."))
    
          }
      }

}

## reload ppc data in google big query
data_reload <- function(bot, update, args) {
  
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
        bot_function = 'data_reload',
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
    
      # emoji
      access_denied_smile <- '\U00026D4'
      err_smile           <- '\U000274C'
      ok_smile            <- '\U0002705'
      time_smile          <- '\U00023F0'
    
      tryCatch(expr = {
    
        # variables
        source    <- args[1]
        date_from <- args[2]
        date_to   <- args[3]
        proj_list <- args[4:length(args)]
        username  <- update$effective_message()$from$username
    
        # check user
        if ( ! check_previlegies(update$from_user_id(), 'data_reload') ) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{access_denied_smile} У вас нет прав для запуска перезагрузки данных по проектам."),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
    
    
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'нет прав',
            entities     = FALSE
          )
    
    
          # check variables
        } else if ( is.null(source) | is.null(date_from) | is.null(date_to) | is.null(proj_list) | length(args) < 4) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Вы указали не все параметры запроса, запрос имеет следующий формат <code>/reload source date_from date_to proj_id_1 proj_id_2 ... proj_id_n</code>"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
    
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'указаны не все параметры запроса',
            entities     = FALSE
          )
    
        } else if ( ! source %in% c('fb', 'ga', 'ya', 'mt', 'fb_custom', 'ga_custom', 'all') ) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Указан неверный источник {source}: возможные варианты fb, fb_custom, ga, ga_custom, ya, mt"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
    
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'указан неверный источник',
            entities     = FALSE
          )
          # check first date
        } else if ( ! str_detect(date_from, '(\\d{4})-(\\d{2})-(\\d{2})') ) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Неверно указана начальная дата {date_from}, формат ввода даты ГГГГ-ММ-ДД"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
    
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'неверно указана начальная дата',
            entities     = FALSE
          )
          # check last date
        } else if ( ! str_detect(date_to, '(\\d{4})-(\\d{2})-(\\d{2})') ) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Неверно указана конечная дата {date_from}, формат ввода даты ГГГГ-ММ-ДД"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'неверно указана конечная дата',
            entities     = FALSE
          )
          # check that the first date is earlier than the last date
        } else if ( as.Date(date_from) > as.Date(date_to) ) {
    
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue("\n{err_smile} Начальная дата не может быть позже конечно, ты указал начальной {date_from} а конечную {date_to}"),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
    
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'data_reload',
            chat_id      = update$from_chat_id(),
            chat_title   = update$effective_chat()$title,
            user_id      = update$from_user_id(),
            msg_text     = 'начальная дата больше конечной',
            entities     = FALSE
          )
          # run procedure
        } else {
    
          # detect function
          fun <- switch (source,
                         'fb'        = 'fb_load',
                         'fb_custom' = 'fb_custom_load',
                         'ga'        = 'ga_load',
                         'ga_custom' = 'ga_custom_load',
                         'ya'        = 'ya_load',
                         'mt'        = 'mt_load',
                         'all'       = 'ppc_load'
          )
    
          # send wait message
          msg_wait <- bot$sendMessage(update$from_chat_id(),
                                      text = str_glue("{time_smile} Перезагрузка данных по указанным проектам запущена, я сообщу когда обработка данных будет завершена!"),
                                      parse_mode = "HTML")
    
          # run fun
          do.call(fun, list(proj_list, date_from, date_to))
    
          # delete waiting message
          bot$deleteMessage(update$from_chat_id(), msg_wait$message_id)
    
          # complete message
          bot$sendMessage(update$from_chat_id(),
                          text = str_glue('{ok_smile} Данные по указанным проектам успешно перезагружены!'),
                          reply_to_message_id = update$effective_message()$message_id,
                          parse_mode = "HTML")
        }
      },
      error = function(err) {
    
        # delete waiting message
        if ( exists('msg_wait') ) {
          bot$deleteMessage(update$from_chat_id(), msg_wait$message_id)
        }
    
        # error message
        # complete message
        bot$sendMessage(update$from_chat_id(),
                        text = str_glue("@AlexeySeleznev \n{err_smile} В ходе перезагрузки данных возникла ошибка: {err$message}"),
                        reply_to_message_id = update$effective_message()$message_id,
                        parse_mode = "HTML")
      })
    }, 
    packages = c('ppcLoader')
  )
}

## для проверки бота
test <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'test',
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


  # отправляем сообщение в телеграм
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "html",
                  reply_to_message_id = update$effective_message()$message_id,
                  text = "Всё ок, я на связи!")

}


## для спасибо
thanks <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'thanks',
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


  # отправляем сообщение в телеграм
  bot$sendMessage(chat_id = update$from_chat_id(),
                  parse_mode = "html",
                  reply_to_message_id = update$effective_message()$message_id,
                  text = paste0("Рад быть полезен, ", update$effective_message()$from$first_name, "!"))

}

## stop polling
stop <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'stop',
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


  if (check_previlegies(update$from_user_id(), 'stop')) {
    
    future::plan('sequential')

    # время падения бота
    bot_stop_time <- Sys.time()
    bot_work_time  <- difftime(bot_stop_time, bot_start_time,units =  'mins')

    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "Бот остановлен")

    lg$log(level = 'stop',
           session_id = get_session_id(),
           message_id = msg$message_id,
           error_text = NULL,
           stop_reason = "user",
           entities = FALSE,
           session_start_at = bot_start_time,
           session_duratin_min = round(bot_work_time, 2))



    IKM <- InlineKeyboardMarkup(
      inline_keyboard =
        list(
          list(
            InlineKeyboardButton(text = 'Запустить бота', url = str_glue('http://95.216.4.19:8000/run?task=alsey\\als_tg_bot&tkn={Sys.getenv("API_TOKEN")}'))
          )
        )
    )

    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "Для запуска бота нажмите кнопку",
                    reply_markup = IKM)

    updater$stop_polling()

    quit("no")
    #taskcheduler_stop('alsey\\als_tg_bot')

  } else {

    # Send Custom Keyboard
    bot$sendMessage(update$from_chat_id(),
                    text = "У вас не достаточно прав для остновки бота.")

    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'stop',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )

  }

}

## update db
update_db <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'update_db',
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

  if ( ! check_previlegies(update$from_user_id(), 'update_db') ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    reply_to_message_id = update$effective_message()$message_id,
                    text = "У вас нет прав на обновление базы данных!")

    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'update_db',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )

  } else {

    taskcheduler_runnow('alsey\\als_db_updater')

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "Markdown",
                    reply_to_message_id = update$effective_message()$message_id,
                    text = "Процесс обновления баз данных запущен, но он может занимать до 1 часа. Проследить за прогрессом выполнения можно по [ссылке](https://ci.netpeak.net/view/pup/job/pup.inweb.slave.Alsey/).")

  }

}

## ################################### ##
## User management                     ##
## ################################### ##
add_priv <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'add_priv',
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

               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {

                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))

                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'add_priv',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )

               } else {

                 first_name <- args[1]
                 last_name  <- args[2]
                 cmd        <- args[3]

                 # получаем id пользователя по имени и фамилии
                 uid <- get_user_id(first_name, last_name)
                 # добавляем привелегию
                 add_privilege(uid, cmd)

                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно добавлена!"))

               }

             }
  )
}

add_priv_by_id <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'add_oriv_by_id',
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

               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {

                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))

                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'add_priv_by_id',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )

               } else {

                 uid <- args[1]
                 cmd <- args[2]

                 # добавляем привелегию
                 add_privilege(uid, cmd)

                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно добавлена!"))

               }

             }
  )
}

## ################################### ##
remove_priv <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'remove_priv',
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

               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {

                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))

                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'remove_priv',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )


               } else {

                 first_name <- args[1]
                 last_name  <- args[2]
                 cmd        <- args[3]

                 # получаем id пользователя по имени и фамилии
                 uid <- get_user_id(first_name, last_name)
                 # добавляем привелегию
                 remove_privilege(uid, cmd)

                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно удалена!"))

               }

             }
  )
}

remove_priv_by_id <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'remove_priv_by_id',
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

               if ( ! check_previlegies(update$from_user_id(), 'priv') ) {

                 # отправляем сообщение в телеграм
                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U00026D4 У вас не достаточно прав для управлением привилегиями пользователей!"))

                 # log
                 lg$warn(
                   message_id   = msg$message_id,
                   session_id   = get_session_id(),
                   bot_function = 'remove_priv_by_id',
                   chat_id      = update$from_chat_id(),
                   chat_title   = update$effective_chat()$title,
                   user_id      = update$from_user_id(),
                   msg_text     = 'нет прав',
                   entities     = FALSE
                 )

               } else {

                 uid <- args[1]
                 cmd <- args[2]

                 # добавляем привелегию
                 remove_privilege(uid, cmd)

                 bot$sendMessage(chat_id = update$from_chat_id(),
                                 parse_mode = "html",
                                 text = paste0("\U0002705 Привелегия успешно удалена!"))

               }

             }
  )
}

## получить список пользователей
get_users_method <- function(bot, update) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_users_method',
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

      if ( ! check_previlegies(update$from_user_id(), 'users') ) {

        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователей!"))

        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'get_users_method',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )

      } else {

        con <- dbConnect(SQLite(), cfg$db_settings$db_path)

        data <- dbReadTable(con, 'bot_users')

        tg_table <- data %>%
                    select(-is_bot) %>%
                    df_to_tg

        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "Markdown",
                        text = tg_table)

      }

    },
    error = function(err) {

      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))

    },
    finally = {

      dbDisconnect(con)

    }

    )

}

## find user
## получить список пользователей
find_users_method <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'find_users_method',
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

      if ( ! check_previlegies(update$from_user_id(), 'users') ) {

        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователей!"))

        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'find_users_method',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )

      } else {

        con <- dbConnect(SQLite(), cfg$db_settings$db_path)

        data <- dbGetQuery(con,
                str_glue('SELECT *
                           FROM bot_users
                           WHERE
                           first_name LIKE "%{args[1]}%"
                           OR last_name LIKE "%{args[1]}%"')
        )

        # отправка данных
        if ( nrow(data) > 0 ) {

          tg_table <- data %>%
            select(-is_bot) %>%
            df_to_tg

          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = tg_table)
        } else {

          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = 'По вашему запросу не найденно пользователей')

        }

      }

    },
    error = function(err) {

      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))

    },
    finally = {

      dbDisconnect(con)

    }

  )

}

## получить информацию о пользователе
get_user_info <- function(bot, update, args) {
  
  # write userdata
  add_user(update)
  
  # log
  # вытягиваем сообщение
  msg <- update$effective_message()
  
  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_user_info',
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
      
      if ( ! check_previlegies(update$from_user_id(), 'users') ) {
        
        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователями!"))
        
        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'get_user_info',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )
        
      } else {
        
        
        data <- db_get_user_info(args[1])
        
        # отправка данных
        if ( ! is.null(data) ) {
          
          msg <- str_glue_data(
            data, 
            "Username: <b>{username}</b>", 
            "User ID: {user_id}",
            "Name: {first_name} {last_name}",
            "Last activity: {last_activity} ({round(as.numeric(difftime(Sys.time(), as.POSIXct(data$last_activity), units = 'days')), 0)}) days ago",
            "Priv: \n{str_c('  - ', data$previlegies$priv, ' (', data$previlegies$priv_add_datetime, ')', collapse = '\n')}", 
            .sep = '\n'
            )
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "html",
                          text = msg)
        } else {
          
          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = 'По указанному username пользователь не найден!')
          
          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'get_user_info',
            chat_id      = update$from_chat_id(),
            user_id      = update$from_user_id(),
            msg_text     = 'По указанному username пользователь не найден!',
            entities     = FALSE
          )
          
          
        }
        
      }
      
    },
    error = function(err) {
      
      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))
      
      # log
      lg$warn(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'get_user_info',
        chat_id      = update$from_chat_id(),
        user_id      = update$from_user_id(),
        msg_text     = err$message,
        entities     = FALSE
      )
      
      
    },
    finally = {
      
      dbDisconnect(con)
      
    }
    
  )
  
  
}

## получить привелегии пользователя
get_user_priv <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'get_user_priv',
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

      if ( ! check_previlegies(update$from_user_id(), 'users') ) {

        # отправляем сообщение в телеграм
        bot$sendMessage(chat_id = update$from_chat_id(),
                        parse_mode = "html",
                        text = paste0("\U00026D4 У вас не достаточно прав для управлениея пользователями!"))

        # log
        lg$warn(
          message_id   = msg$message_id,
          session_id   = get_session_id(),
          bot_function = 'get_user_priv',
          chat_id      = update$from_chat_id(),
          chat_title   = update$effective_chat()$title,
          user_id      = update$from_user_id(),
          msg_text     = 'нет прав',
          entities     = FALSE
        )

      } else {

        con <- dbConnect(SQLite(), cfg$db_settings$db_path)

        data <- dbGetQuery(con,
                 str_glue('SELECT command, add_datetime
                           FROM users_previlegies
                           WHERE
                           user_id = {args[1]}
                           ORDER BY add_datetime DESC')
        )

        # отправка данных
        if ( nrow(data) > 0 ) {

          tg_table <- data %>%
                      rename(priv = command) %>%
                      df_to_tg


          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = tg_table)
        } else {

          # отправляем сообщение в телеграм
          bot$sendMessage(chat_id = update$from_chat_id(),
                          parse_mode = "Markdown",
                          text = 'У данного пользователя отсутвуют привелегии')

          # log
          lg$warn(
            message_id   = msg$message_id,
            session_id   = get_session_id(),
            bot_function = 'get_user_priv',
            chat_id      = update$from_chat_id(),
            user_id      = update$from_user_id(),
            msg_text     = 'У данного пользователя отсутвуют привелегии',
            entities     = FALSE
          )


        }

      }

    },
    error = function(err) {

      # отправляем сообщение в телеграм
      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0("Ошибка: ", err$message))

      # log
      lg$warn(
        message_id   = msg$message_id,
        session_id   = get_session_id(),
        bot_function = 'get_user_priv',
        chat_id      = update$from_chat_id(),
        user_id      = update$from_user_id(),
        msg_text     = err$message,
        entities     = FALSE
      )


    },
    finally = {

      dbDisconnect(con)

    }

  )

}

## ################################### ##
## functions for auth in Yandex Direct ##
## ################################### ##
## run auth
ya_auth  <- function(bot, update, args) {

  # write userdata
  add_user(update)


  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'ya_auth',
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

  # emoji
  access_denied_smile <- '\U00026D4'
  err_smile           <- '\U000274C'
  ok_smile            <- '\U0002705'
  time_smile          <- '\U00023F0'

  # variables
  username  <- update$effective_message()$from$username

  # check user
  if ( ! check_previlegies(update$from_user_id(), 'ya_auth')  ) {

    bot$sendMessage(update$from_chat_id(),
                    text = str_glue("\n{access_denied_smile} У вас нет прав на создание токенов для Яндекс Директ."),
                    reply_to_message_id = update$effective_message()$message_id,
                    parse_mode = "HTML")

    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'ya_auth',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = "нет прав",
      entities     = FALSE
    )

    # check variables
  } else if ( length(args) == 0 ) {

    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("{err_smile} Для авторизации необзодимо указывать логин, пример <code>/ya_auth login</code>. Повторите попытку."))

    # set state
    set_state(update$from_chat_id(),
              update$from_user_id(),
              'start')

  } else {

    # считываем логин
    Login <- args[1]

    # write login
    set_chat_data(update$from_chat_id(),
                  update$from_user_id(),
                  field = 'login',
                  value =  Login)

    # inline кнока для перехода к авторизации
    url <- paste0("https://oauth.yandex.ru/authorize?response_type=code&client_id=365a2d0a675c462d90ac145d4f5948cc&redirect_uri=https://selesnow.github.io/ryandexdirect/getToken/get_code.html&force_confirm=1", paste0("&login_hint=", Login))

    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton(paste0("Авторизоваться под ", Login ), url = url)
        )
      )
    )

    # отправка клавиатуры
    # Отправляем клавиатуру в чат
    bot$sendMessage(update$from_chat_id(),
                    text = 'Для авторизации нажмите кнопку ниже, предоставьте в браузере все разрешения и введите полученный код авторизации',
                    reply_markup = IKM)

    # переключаем состояние бота в ожидание ввода кода
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = "Введите код подтверждения авторизации:")

    # set state
    set_state(update$from_chat_id(),
              update$from_user_id(),
              'wait_code')

  }

}

## check auth code
ya_get_code <- function(bot, update) {

  # write userdata
  add_user(update)


  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'ya_get_code',
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

  # emoji
  access_denied_smile <- '\U00026D4'
  err_smile           <- '\U000274C'
  ok_smile            <- '\U0002705'
  time_smile          <- '\U00023F0'


  if ( nchar(update$effective_message()$text) != 7 ) {

    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = str_glue("{err_smile} Вы ввели неверный код, код должен состоять из 7 цыфр, попробуйте ещё раз!"))
    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'ya_get_code',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = "нет прав",
      entities     = FALSE
    )


  } else {

    token_raw <- httr::POST("https://oauth.yandex.ru/token",
                            body = list(grant_type="authorization_code",
                                        code = update$effective_message()$text,
                                        client_id = "365a2d0a675c462d90ac145d4f5948cc",
                                        client_secret = "f2074f4c312449fab9681942edaa5360"),
                            encode = "form")

    # parse token
    token <- content(token_raw)
    token$expire_at <- Sys.time() + as.numeric(token$expires_in, units = "secs")
    class(token) <- "yadir_token"

    # check for error
    if (!is.null(token$error_description)) {

      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "HTML",
                      text = paste0(err_smile, " Ошибка - ", token$error_description, ". Повторите попытку с самого начала командой <code>/ya_auth</code>!"))


      # set state
      set_state(update$from_chat_id(),
                update$from_user_id(),
                'start')

    } else {

      Login <- get_chat_data(update$from_chat_id(),
                             update$from_user_id(),
                             field = 'login')

      # save into local file
      save(token, file = paste0("C:/my_develop_workshop/ppc_digest/token_yandex/", Login, ".yadirAuth.RData"))

      # set state
      set_state(update$from_chat_id(),
                update$from_user_id(),
                'start')


      bot$sendMessage(chat_id = update$from_chat_id(),
                      parse_mode = "html",
                      text = paste0(ok_smile, " Вы успешно авторизовались в Яндекс Директ под логином ", Login))



    }

  }

}


# запросить список файлов из папки ----------------------------------------

dir <- function(bot, update, args) {

  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'dir',
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

  if ( ! check_previlegies(update$from_user_id(), 'files') ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для работы с файлами сервера!"))


    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'dir',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )


  } else {

    files <- base::dir(args) %>%
      str_c(collapse = "\n")

    files <- str_glue("Содержание папки {args}\n\n<code>{files}\n</code>")

    bot$sendMessage(chat_id = update$from_chat_id(),
                    text = files,
                    parse_mode = 'html')
  }
}

# получить файл из папки ----------------------------------------
get_file <- function(bot, update, args) {


  # write userdata
  add_user(update)

  # log
  # вытягиваем сообщение
  msg <- update$effective_message()

  # логируем основную информацию
  lg$info(
    message_id   = msg$message_id,
    session_id   = get_session_id(),
    bot_function = 'dir',
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

  if ( ! check_previlegies(update$from_user_id(), 'files') ) {

    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для работы с файлами сервера!"))


    # log
    lg$warn(
      message_id   = msg$message_id,
      session_id   = get_session_id(),
      bot_function = 'dir',
      chat_id      = update$from_chat_id(),
      chat_title   = update$effective_chat()$title,
      user_id      = update$from_user_id(),
      msg_text     = 'нет прав',
      entities     = FALSE
    )


  } else {

    if ( file.exists(args) ) {

      updater$bot$sendDocument(
        chat_id = update$from_chat_id(),
        document = args
      )

    } else {

      bot$sendMessage(chat_id = update$from_chat_id(),
                      text = str_glue("Не могу найти файл {args}"),
                      parse_mode = 'markdown')

    }
  }
}


# запустить команду на сервере --------------------------------------------
cmd <- function(bot, update, args) {
  
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
    
    # отправляем сообщение в телеграм
    bot$sendMessage(chat_id = update$from_chat_id(),
                    parse_mode = "html",
                    text = paste0("\U00026D4 У вас не достаточно прав для запуска команд на сервере!"))
    
    
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
                    text = str_glue("*Результат*\n\n{cmd_out}"),
                    reply_to_message_id = update$effective_message()$message_id,
                    parse_mode = 'markdown')
   
  }
    }
  )
}

# ####################################
# Filters  ###########################
MessageFilters$tasks <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Список задач"
  } else {
    FALSE
  }
}
)

MessageFilters$run_menu <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Меню запуска задач"
  } else {
    FALSE
  }
}
)

MessageFilters$quite_tasks <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Тихие задачи"
  } else {
    FALSE
  }
}
)

MessageFilters$task_list <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Полный список задач"
  } else {
    FALSE
  }
}
)

MessageFilters$chat_id <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Получить id чата"
  } else {
    FALSE
  }
}
)

MessageFilters$help <- BaseFilter(function(message) {

  if ( !is.null(message$text) ) {
    message$text == "Помощь"
  } else {
    FALSE
  }
}
)

MessageFilters$template <- BaseFilter(function(message) {
  
  if ( !is.null(message$text) ) {
    grepl('поставить задачу|шаблон задачи|задачу на отдел аналитики', message$text, ignore.case = T)
  } else {
    FALSE
  }
}
)

MessageFilters$ya_auth_code <-
  BaseFilter( function(message) {

    if ( !is.null(message$chat_id) ) {
      get_state(message$chat_id, message$from$id) == 'wait_code'
    } else {
      FALSE
    }

})

MessageFilters$test <-
  BaseFilter( function(message) {

    if ( !is.null(message$text) ) {
      tolower(message$text) == 'бот тест'
    } else {
      FALSE
    }
})

MessageFilters$thanks <-
  BaseFilter( function(message) {

    if ( !is.null(message$text) ) {
      tolower(message$text) == 'спасибо бот'
    } else {
      FALSE
    }
  })

MessageFilters$in_chat <- BaseFilter(function(message) {
  any( !is.null(message$new_chat_participant), !is.null(message$new_chat_member), !is.null(message$new_chat_members) )
}
)

MessageFilters$out_chat <- BaseFilter(function(message) {
  any( !is.null(message$left_chat_participant), !is.null(message$left_chat_member) )
}
)
# Handlers #####################
## command hadlers
start_handler       <- CommandHandler("start", start, username = botname)
tasks_handler       <- CommandHandler("tasks", tasks, username = botname)
running_tasks_h     <- CommandHandler('running_tasks', running_tasks, username = botname)
chat_id_handler     <- CommandHandler("chat_id", chat_id, username = botname)
run_handler         <- CommandHandler("run", run, pass_args = TRUE, username = botname)
run_menu_handler    <- CommandHandler("run_menu", run_menu, username = botname)
forget_handler      <- CommandHandler('forget', forget, pass_args = TRUE, username = botname)
unforget_handler    <- CommandHandler('unforget', unforget, pass_args = TRUE, username = botname)
quite_tasks_handler <- CommandHandler('quite_tasks', quite_tasks, username = botname)
task_list_handler   <- CommandHandler('task_list', task_list, username = botname)
run_menu_old_h      <- CommandHandler('run_menu_old', run_menu_old, username = botname)
task_info_handler   <- CommandHandler('task_info', task_info, pass_args = TRUE, username = botname)
task_search_handler <- CommandHandler('task_search', task_search, pass_args = TRUE, username = botname)
h_reloader          <- CommandHandler('reload', data_reload, pass_args = TRUE, username = botname)
auth_handler        <- CommandHandler("ya_auth", ya_auth, pass_args = TRUE, username = botname)
help_handler        <- CommandHandler("help", help, username = TRUE)
stop_habler         <- CommandHandler('stop', stop, username = botname)
update_db_handler   <- CommandHandler('update_db', update_db, username = botname)
remove_priv_handler <- CommandHandler('remove_priv', remove_priv, pass_args = TRUE, username = botname)
add_priv_handler    <- CommandHandler('add_priv', add_priv, pass_args = TRUE, username = botname)
add_priv_by_id_h    <- CommandHandler('add_priv_by_id', add_priv_by_id, pass_args = TRUE, username = botname)
remove_priv_by_id_h <- CommandHandler('remove_priv_by_id', remove_priv_by_id, pass_args = TRUE, username = botname)
get_users_handler   <- CommandHandler('get_users', get_users_method, username = botname)
find_users_handler  <- CommandHandler('find_user', find_users_method, pass_args = TRUE, username = botname)
get_user_priv_h     <- CommandHandler('get_user_priv', get_user_priv, pass_args = TRUE, username = botname)
get_code_handler    <- MessageHandler(ya_get_code, MessageFilters$ya_auth_code & ! MessageFilters$command )
h_dir               <- CommandHandler("dir", dir, pass_args = TRUE)
h_get_file          <- CommandHandler("get_file", get_file, pass_args = TRUE)
р_get_template      <- CommandHandler("get_template", get_template, username = botname)
h_cmd               <- CommandHandler('cmd', cmd, username = botname)
h_get_user_info     <- CommandHandler('get_user_info', get_user_info, pass_args = TRUE, username = botname)
inline_handler      <- CallbackQueryHandler(answer_cb)

## messange handers
m_tasks_handler       <- MessageHandler(tasks, MessageFilters$tasks )
m_run_menu_handler    <- MessageHandler(run_menu, MessageFilters$run_menu )
m_quite_tasks_handler <- MessageHandler(quite_tasks, MessageFilters$quite_tasks )
m_task_list_handler   <- MessageHandler(task_list, MessageFilters$task_list )
m_chat_id_handler     <- MessageHandler(chat_id, MessageFilters$chat_id )
m_test_handler        <- MessageHandler(test, MessageFilters$test )
m_help                <- MessageHandler(help, MessageFilters$help )
m_thanks              <- MessageHandler(thanks, MessageFilters$thanks)
m_template            <- MessageHandler(get_template, MessageFilters$template)

# Add hadlesra to updater ######
updater <-
  updater +
     start_handler +
     chat_id_handler +
     tasks_handler +
     running_tasks_h +
     m_tasks_handler +
     run_handler +
     run_menu_handler +
     m_run_menu_handler +
     forget_handler +
     unforget_handler +
     quite_tasks_handler +
     m_quite_tasks_handler +
     task_list_handler +
     m_task_list_handler +
     run_menu_old_h +
     inline_handler +
     m_chat_id_handler +
     task_info_handler +
     task_search_handler +
     h_reloader +
     auth_handler +
     get_code_handler +
     m_test_handler +
     help_handler +
     m_help +
     stop_habler +
     update_db_handler +
     remove_priv_handler +
     add_priv_handler +
     add_priv_by_id_h +
     remove_priv_by_id_h +
     get_users_handler +
     find_users_handler +
     get_user_priv_h +
     m_thanks +
     h_dir +
     h_get_file +
     р_get_template +
     m_template +
     h_cmd +
     h_get_user_info

##################
# start bot
tryCatch(
  expr = updater$start_polling(),
  error = function(err) {
    
    # выключаем многопоточный режим
    future::plan('sequential')

    # время падения бота
    bot_crash_time <- Sys.time()
    bot_work_time  <- difftime(bot_crash_time, bot_start_time,units =  'mins')

    # loging
    update <- updater$bot$get_updates(offset = -1)
    update_name <- paste0('upd_', get_session_id(), '___', str_replace_all(Sys.time(), '\\:|\\ |-', replacement = '_'), '.rds')

    # save last update to rds file
    saveRDS(update, file = paste0('C:\\my_develop_workshop\\telegram_bot\\error_updater\\', update_name))

    # get message data
    msg    <- update[[1]]$effective_message()

    ## write error
    lg$error('Critical Error',
             session_id   = get_session_id(),
             message_id   = msg$message_id,
             error_text   = err$message,
             stop_reason  = "error",
             chat_id      = update[[1]]$from_chat_id(),
             user_id      = update[[1]]$from_user_id(),
             msg_text     = msg$text,
             session_duratin_min = round(bot_work_time, 2),
             last_update_file = update_name,
             session_start_at = bot_start_time,
             entities     = FALSE)

    # бот для оповещения
    bot <- Bot(token = bot_token("Netpeak BI Bot"))
    # чат для оповещения
    chat_id <- cfg$info_chat$chat_id

    message("ID инфо чата: ", chat_id)

    # сообщение
    msg <- str_glue("*Бот упал*: _{err$message}_ {bot_crash_time}, общее время работы бота составило {round(bot_work_time, 2)} (мин.).")

    bot$sendMessage(chat_id = chat_id,
                    text = msg,
                    parse_mode = 'Markdown')

    # не надо останавливать polling
    #updater$stop_polling()
    # очищаем всё чтов бота прилетело
    updater$bot$clean_updates()

    # чат для оповещения
    chat_id <- cfg$info_chat$chat_id

    # информация о том, что бот будет перезапущен
    bot$sendMessage(chat_id = chat_id,
                    text = str_glue('*Перезапускаю бота* в {Sys.time()}'),
                    parse_mode = 'Markdown')

    # перезапускаем задачу
    # taskscheduler_runnow('alsey\als_tg_bot')

    # очищаем рабочее окружение
    rm(list = ls())

    # закрываем соединение с базой логирования
    unlink(cfg$db_settings$db_path)

    # перезапускаем скрипт бота
    source('C:\\my_develop_workshop\\telegram_bot\\netpeak_tg_bot.R')

  },
  finally = {

    # останавливаем пулинг
    updater$stop_polling()

    # закрываем соединение с базой логирования
    try(unlink(cfg$db_settings$db_path))

  }
)

