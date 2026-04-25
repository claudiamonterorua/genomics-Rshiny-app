library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)
library(mongolite)
library(jsonlite)

con <- mongo(
  collection = "logs",
  db = "genomics_db",
  url = "mongodb+srv://claudiamonterorua:Cmr15acc@genomics-cluster.hfue3hm.mongodb.net/?appName=genomics-cluster"
)

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#E83E8C"
  ),
  
  div(style = "padding:25px;",
      
      h2("Genomic Provenance Monitor"),
      
      sidebarLayout(
        
        sidebarPanel(
          h4("Filters"),
          
          selectInput("node", "Execution Node:",
                      choices = c("All"),
                      selected = "All")
        ),
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("System Health",
                     plotOutput("health_plot")
            ),
            
            tabPanel("Efficiency",
                     plotOutput("time_plot")
            ),
            
            tabPanel("Throughput",
                     plotOutput("size_plot")
            ),
            
            tabPanel("Details",
                     plotOutput("process_plot"),
                     plotOutput("fail_plot"),
                     plotOutput("scatter_plot"),
                     plotOutput("files_plot")
            ),
            
            tabPanel("Data",
                     DTOutput("table"),
                     verbatimTextOutput("json_view")
            )
          )
        )
      )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    df <- con$find()

    df$generated <- lapply(df$generated, function(x) {
      if (is.data.frame(x)) split(x, seq(nrow(x)))
      else x
    })

    df %>%
      mutate(
        duration = as.numeric(
          difftime(
            as.POSIXct(endTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            as.POSIXct(startTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            units = "secs"
          )
        ),
        
        SHA256 = sapply(generated, function(x) {
          if (!is.list(x)) return(NA)
          vals <- sapply(x, function(e) {
            if (is.list(e) && !is.null(e$label) && e$label == "SHA256") e$value else NA
          })
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA else vals[1]
        }),

        Seqfu = sapply(generated, function(x) {
          if (!is.list(x)) return(NA)
          vals <- sapply(x, function(e) {
            if (is.list(e) && !is.null(e$label) && e$label == "Seqfu") e$value else NA
          })
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA else vals[1]
        }),

        size = as.numeric(sapply(generated, function(x) {
          if (!is.list(x)) return(NA)
          vals <- sapply(x, function(e) {
            if (is.list(e) && !is.null(e$label) && e$label == "FASTQ Files") e$totalSizeBytes else NA
          })
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA else vals[1]
        })) / 1e9,

        fileCount = as.numeric(sapply(generated, function(x) {
          if (!is.list(x)) return(NA)
          vals <- sapply(x, function(e) {
            if (is.list(e) && !is.null(e$label) && e$label == "FASTQ Files") e$fileCount else NA
          })
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA else vals[1]
        })),

        category = sapply(generated, function(x) {
          if (!is.list(x)) return(NA)
          vals <- sapply(x, function(e) {
            if (is.list(e) && !is.null(e$label) && e$label == "FASTQ Files") e$category else NA
          })
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA else vals[1]
        })
      )
  })
  
  observe({
    nodes <- unique(data()$executionNode)
    updateSelectInput(session, "node",
                      choices = c("All", nodes))
  })
  
  filtered <- reactive({
    if (input$node == "All") return(data())
    data() %>% filter(executionNode == input$node)
  })
  
  # SYSTEM HEALTH
  output$health_plot <- renderPlot({
    df <- filtered()
    
    health <- data.frame(
      Status = c("OK", "PARTIAL SUCCESS", "FAIL"),
      Count = c(
        sum(df$SHA256 == "OK" & df$Seqfu == "OK", na.rm = TRUE),
        sum((df$SHA256 == "OK" & df$Seqfu == "FAIL") |
              (df$SHA256 == "FAIL" & df$Seqfu == "OK"), na.rm = TRUE),
        sum(df$SHA256 == "FAIL" & df$Seqfu == "FAIL", na.rm = TRUE)
      )
    )
    
    ggplot(health, aes(x = Status, y = Count, fill = Status)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "OK" = "#00BA38",
        "PARTIAL SUCCESS" = "#619CFF",
        "FAIL" = "#F8766D"
      )) +
      theme_minimal()
  })
  
  # EFFICIENCY
  output$time_plot <- renderPlot({
    df <- filtered()
    
    ggplot(df, aes(x = executionNode, y = duration)) +
      geom_boxplot(fill = "#EC407A") +
      theme_minimal() +
      labs(y = "Time (seconds)")
  })
  
  # THROUGHPUT
  output$size_plot <- renderPlot({
    df <- filtered()
    
    ggplot(df %>% mutate(size = ifelse(is.na(size), 0, size)),
           aes(x = executionNode, y = size)) +
      geom_bar(stat = "summary", fun = "sum", fill = "#F48FB1") +
      theme_minimal() +
      labs(y = "GB processed")
  })
  
  # PROCESSES
  output$process_plot <- renderPlot({
    df <- filtered()
    
    ggplot(df, aes(x = executionNode)) +
      geom_bar(fill = "#FFCC80") +
      theme_minimal() +
      labs(y = "Volume of processes")
  })
  
  # PROPORTION
  output$fail_plot <- renderPlot({
    df <- filtered()
    
    df$Status <- ifelse(df$SHA256 == "OK" & df$Seqfu == "OK", "OK",
                        ifelse(df$SHA256 == "FAIL" & df$Seqfu == "FAIL", "FAIL", "PARTIAL SUCCESS"))
    
    ggplot(df, aes(x = executionNode, fill = Status)) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = c(
        "OK" = "#00BA38",
        "PARTIAL SUCCESS" = "#619CFF",
        "FAIL" = "#F8766D"
      )) +
      theme_minimal() +
      labs(y = "Proportion")
  })
  
  # SIZE vs TIME
  output$scatter_plot <- renderPlot({
    df <- filtered()
    
    ggplot(df, aes(x = size, y = duration, color = executionNode)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("#F8BBD0","#EC407A","#AD1457")) +
      theme_minimal() +
      labs(x = "GB processed", y = "Time (seconds)")
  })
  
  # FILES
  output$files_plot <- renderPlot({
    df <- filtered()
    
    ggplot(df, aes(x = executionNode, y = fileCount)) +
      geom_boxplot(fill = "#D1C4E9") +
      theme_minimal() +
      labs(y = "Number of files")
  })
  
  output$table <- renderDT({
    datatable(filtered(),
              options = list(pageLength = 5),
              selection = "single")
  })
  
  output$json_view <- renderPrint({
    req(input$table_rows_selected)
    row <- filtered()[input$table_rows_selected, ]
    
    cat(toJSON(row, pretty = TRUE, auto_unbox = TRUE))
  })
}

shinyApp(ui, server)