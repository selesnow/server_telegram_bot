# Установка необходимых пакетов
if (!requireNamespace("httr2", quietly = TRUE)) {
  install.packages("httr2", repos = "https://cran.r-project.org", quiet = TRUE)
}

# Получаем сообщение коммита через API GitLab
library(httr2)

gitlab_token <- Sys.getenv("GITLAB_PAT")
project_id <- Sys.getenv("CI_PROJECT_ID")
commit_sha <- Sys.getenv("CI_COMMIT_SHA")

gitlab_url <- paste0("https://git.netpeak.net/api/v4/projects/", project_id, "/repository/commits/", commit_sha)

commit_raw <- tryCatch({
  response <- request(gitlab_url) |>
    req_headers("PRIVATE-TOKEN" = gitlab_token) |>
    req_perform()
  
  if (resp_status(response) == 200) {
    json <- resp_body_json(response)
    json$message
  } else {
    "Не удалось получить коммит через API"
  }
}, error = function(e) {
  "Ошибка при запросе API GitLab"
})
str('str 29')
# Подготовка данных
status <- Sys.getenv("CI_JOB_STATUS")
author <- Sys.getenv("GITLAB_USER_NAME")
branch <- Sys.getenv("CI_COMMIT_REF_NAME")
pipeline_id <- Sys.getenv("CI_PIPELINE_ID")
pipeline_url <- Sys.getenv("CI_PIPELINE_URL")

escape_markdown <- function(text) {
  # Список символов для экранирования
  special_chars <- c("_", "*")
  
  # Разделяем текст на части по бектикам
  parts <- unlist(strsplit(text, "`", fixed = TRUE))
  
  # Обрабатываем части: четные индексы - вне бектиков, нечетные - внутри
  for (i in seq_along(parts)) {
    # Если это часть вне бектиков (четный индекс при нумерации с 1)
    if (i %% 2 == 1) {
      # Экранируем специальные символы
      for (char in special_chars) {
        parts[i] <- gsub(paste0(char), paste0("\\", char), parts[i], fixed = TRUE)
      }
    }
    # Части внутри бектиков оставляем без изменений
  }
  
  # Собираем текст обратно, добавляя бектики между частями
  result <- parts[1]
  if (length(parts) > 1) {
    for (i in 2:length(parts)) {
      if (i %% 2 == 0) {
        # Добавляем бектик перед и после части внутри бектиков
        result <- paste0(result, "`", parts[i], "`")
      } else {
        # Просто добавляем часть вне бектиков
        result <- paste0(result, parts[i])
      }
    }
  }
  
  return(result)
}

# Создание сообщения
message <- paste0(
  "🚀 *Обновление Telegram бота отдела аналитики!*\n\n",
  "_Что было доработано:_\n\n",
  escape_markdown(commit_raw), "\n\n",
  "● Автор: ", author, "\n",
  "● Ветка: ", branch, "\n",
  "● *Pipeline *Pipeline:* [#", pipeline_id, "](", pipeline_url, ")\n\n",
  "#netpeak\\_bi\\_bot\n"
)

# Отправка сообщения в Telegram
token <- Sys.getenv("TG_BOT_TOKEN")
chat_id <- Sys.getenv("TG_CHAT_ID")
url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")

if (!grepl('--test', x = commit_raw, ignore.case = T)) {
  str('str 90')
  print(message)
  cat(message)
  response <- request(url) |>
    req_body_form(
      chat_id = chat_id,
      parse_mode = "Markdown",
      text = message
    ) |>
    req_perform()
  
  # Проверка ответа
  if (resp_status(response) != 200) {
    cat("Ошибка при отправке сообщения: ", resp_body_string(response), "\n")
    quit(status = 1)
  } else {
    cat("Сообщение успешно отправлено!\n")
  }
  
}
