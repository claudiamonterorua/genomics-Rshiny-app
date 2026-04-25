library(mongolite)

con <- mongo(
  collection = "logs",
  db = "genomics_db",
  url = "mongodb+srv://claudiamonterorua:Cmr15acc@genomics-cluster.hfue3hm.mongodb.net/?appName=genomics-cluster"
)

files <- list.files("json_logs", full.names = TRUE)

json_strings <- sapply(files, function(f) {
  paste(readLines(f, warn = FALSE), collapse = "\n")
})

con$insert(json_strings)

con$count()