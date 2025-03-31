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
