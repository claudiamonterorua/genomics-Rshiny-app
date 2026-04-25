library(jsonlite)

dir.create("json_logs", showWarnings = FALSE)

for (i in 1:100) {
  
  start <- Sys.time()
  duration <- sample(30:300, 1)
  end <- start + duration
  
  json_logs_data <- list(
    "@context" = "http://www.w3.org/ns/prov#",
    "@id" = paste0("urn:uuid:", i),
    "@type" = "Activity",
    
    label = paste("Process sample_", i),
    
    startTime = format(start, "%Y-%m-%dT%H:%M:%SZ"),
    endTime = format(end, "%Y-%m-%dT%H:%M:%SZ"),
    
    executionNode = sample(c("cresselia", "pikachu", "snorlax"), 1),
    
    sourceDirectory = "/data/input/",
    destinationDirectory = paste0("/data/output/sample_", i),
    
    wasAssociatedWith = list(
      list(
        "@type" = "SoftwareAgent",
        label = "seqfu",
        version = "1.22.3"
      ),
      list(
        "@type" = "SoftwareAgent",
        label = "sha256sum",
        version = "8.32"
      ),
      list(
        "@type" = "SoftwareAgent",
        label = "Nextflow pipeline",
        repository = "local",
        commitId = "N/A",
        revision = "N/A"
      ),
      list(
        "@id" = "urn:person:salle_alumni",
        "@type" = "Person",
        label = "User: salle_alumni",
        actedOnBehalfOf = list(
          "@id" = "https://ror.org/01y990p52",
          "@type" = "Organization",
          label = "La Salle"
        )
      )
    ),
    
    generated = list(
      list(
        "@type" = "Entity",
        label = "SHA256",
        description = "Checksum validation",
        value = sample(c("OK", "FAIL"), 1)
      ),
      list(
        "@type" = "Entity",
        label = "Seqfu",
        description = "FASTQ integrity",
        value = sample(c("OK", "FAIL"), 1)
      ),
      list(
        "@type" = "Entity",
        label = "FASTQ Files",
        totalSizeBytes = sample(1000000000:5000000000, 1),
        category = "Genet",
        fileCount = sample(1:5, 1)
      )
    )
  )
  
  write_json(json_logs_data,
             paste0("json_logs/log_", i, ".json"),
             pretty = TRUE,
             auto_unbox = TRUE)
}