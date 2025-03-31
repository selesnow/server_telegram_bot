
# ################
library(telegram.bot)
updater <- Updater(token = "***REMOVED***")


#updater <- Updater(token = "12345:ABCDE")

inline <- function(bot, update) {
  #browser()
text <- "Yes or no?"
IKM <- InlineKeyboardMarkup(
  inline_keyboard = list(
    list(
      InlineKeyboardButton("??", callback_data = 'yes'),
      InlineKeyboardButton("No ??", callback_data = 'no')
    )
  )
)

# Send Inline Keyboard
bot$sendMessage(update$message$chat_id, text, reply_markup = IKM)
}

inline_h <- CommandHandler('inline', inline)
updater <- updater + inline_h

answer_cb <- function(bot, update) {
 
  data <- update$callback_query$data
  #browser()
  # Send Custom Keyboard
  bot$sendMessage(chat_id = update$from_chat_id(),#update$message$chat_id, 
                  text = data)
  
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id) 
}


query_handler <- CallbackQueryHandler(answer_cb)
updater <- updater + query_handler

updater$start_polling()

