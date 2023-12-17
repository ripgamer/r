if (!require(shiny)) install.packages('shiny')
if (!require(readxl)) install.packages('readxl')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyr)) install.packages('tidyr')
if (!require(cowplot)) install.packages('cowplot')


library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

# Define UI
ui <- fluidPage(
  titlePanel("Student Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File"),
      selectizeInput("subject", "Select Subject(s):", choices = NULL, multiple = TRUE),
      selectInput("class", "Select Class:", choices = NULL, multiple = TRUE)
    ),
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$file, {
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    student_data <- read_excel(inFile$datapath)
    
    updateSelectInput(session, "class", choices = unique(student_data$class), selected = unique(student_data$class))
    updateSelectizeInput(session, "subject", choices = colnames(student_data)[grep("^[A-Z]+$", colnames(student_data))], selected = NULL)
  })
  
  output$comparisonPlot <- renderPlot({
    req(input$file)
    
    student_data <- read_excel(input$file$datapath)
    
    if (length(input$class) > 0 && length(input$subject) > 0) {
      # Scatter Plot for selected subjects by Class
      filtered_data <- student_data %>%
        filter(class %in% input$class)
      
      # Create a color palette for subjects
      subject_colors <- rainbow(length(input$subject))
      
      scatter_plot <- ggplot(filtered_data, aes_string(x = "class", color = "class")) +
        labs(title = paste("Comparative Scatter Plot of", paste(input$subject, collapse = ", "), "by Class"),
             x = "Class", y = "Marks") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      for (i in seq_along(input$subject)) {
        subj <- input$subject[i]
        color <- subject_colors[i]
        scatter_plot <- scatter_plot +
          geom_point(data = filtered_data, aes_string(y = subj), color = color, position = position_jitter(width = 0.2, height = 0), size = 3) +
          scale_color_manual(name = "Subject", values = setNames(color, subj))
      }
      
      scatter_plot
    } else {
      ggplot() + 
        theme_void() +
        labs(title = "Select Class(es) and Subject(s) for Comparison")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
