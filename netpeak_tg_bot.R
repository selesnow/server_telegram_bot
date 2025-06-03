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

# config
cfg <- read.config(here::here('config.cfg'))

# check git branch
# if master start production bot
# if dev start test bot
if (system("git rev-parse --abbrev-ref HEAD", intern = TRUE) == 'master') {
  alert_chat_id <- cfg$info_chat$chat_id
  bot_api_token <- cfg$bot_settings$bot_token
} else {
  alert_chat_id <- cfg$info_chat$test_chat_id
  bot_api_token <- cfg$bot_settings$test_bot_token
}

# create bot instance
updater   <- Updater(token = bot_api_token)

# очищаем старые обновления
updater$bot$clean_updates()

# имя бота
botname <- updater$bot$getMe()$username

# Read function
source(here::here('db_bot_function.R'))
source(here::here('logger.R'))

# сообщаем о том, что бот вклчючен
bot_start_time <- Sys.time()

## start session
lg$log(level = 'sessions',
       entities = FALSE)

# Commands #####################
bot_methods <- dir(here::here("R"))
for (method in bot_methods) {
  print(here::here('R', method))
  source(here::here('R', method))
}

updater$bot$sendMessage(
  chat_id = alert_chat_id,
  text = str_glue('*Бот запущен* в {bot_start_time}\n\nid сессии: {get_session_id()}\nсервер: {get_ip()}'),
  parse_mode = 'Markdown'
)

# ####################################
# Filters  ###########################
print(here::here('filters.R'))
source(here::here('filters.R'))

# Handlers #####################
## command hadlers
source(here::here('handlers.R'))

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
     h_get_template +
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
    saveRDS(update, file = paste0('error_updater\\', update_name))

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
    bot <- Bot(token = bot_api_token)
    
    # чат для оповещения
    #chat_id <- cfg$info_chat$chat_id

    message("ID инфо чата: ", chat_id)

    # сообщение
    msg <- str_glue("*Бот упал*: _{err$message}_ {bot_crash_time}, общее время работы бота составило {round(bot_work_time, 2)} (мин.).")

    bot$sendMessage(chat_id = alert_chat_id,
                    text = msg,
                    parse_mode = 'Markdown')

    # не надо останавливать polling
    #updater$stop_polling()
    # очищаем всё чтов бота прилетело
    updater$bot$clean_updates()

    # чат для оповещения
    #chat_id <- cfg$info_chat$chat_id

    # информация о том, что бот будет перезапущен
    bot$sendMessage(chat_id = chat_id,
                    text = str_glue('*Перезапускаю бота* в {Sys.time()}'),
                    parse_mode = 'Markdown')

    # перезапускаем задачу
    # taskscheduler_runnow('alsey\als_tg_bot')

    # очищаем рабочее окружение
    rm(list = ls())

    # перезапускаем скрипт бота
    source('netpeak_tg_bot.R')

  },
  finally = {

    # останавливаем пулинг
    updater$stop_polling()

  }
)

