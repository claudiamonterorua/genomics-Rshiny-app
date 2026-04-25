library(mongolite)

con <- mongo(
  collection = "logs",
  db = "genomics_db",
  url = "YOUR_MONGODB_URL"
)

files <- list.files("json_logs", full.names = TRUE)

json_strings <- sapply(files, function(f) {
  paste(readLines(f, warn = FALSE), collapse = "\n")
})

con$insert(json_strings)

con$count()