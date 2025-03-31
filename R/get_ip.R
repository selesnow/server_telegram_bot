get_ip <- function() {
  response <- GET("https://api64.ipify.org?format=text")
  content(response, "text", encoding = "UTF-8")
}