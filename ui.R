# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("This is a Shiny APP for matching addresses"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Input data panel", 
                 
                 # Input: Selector for choosing dataset ----
                 fileInput("file1", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy
                 
                 selectInput("dataset","Data:", choices = "Files uploaded"), #Kathy
                 selectInput("ID","ID Field:", choices = NULL),
                 selectInput("variable","Variable:", choices = NULL),
                 
                 tags$hr(style="border-color: Orange;"),
                 
                 # ----
                 fileInput("file2", "Please upload data file (xlsx or csv)", accept = c(".xlsx", ".csv"), multiple=F), # Kathy
                 
                 selectInput("dataset2","Data:", choices = "Files uploaded"), #Kathy
                 selectInput("ID2","ID Field:", choices = NULL),
                 selectInput("variable2","Variable:", choices = NULL),
                 
                 actionButton("GoButton","Let's GO!"),
                 downloadButton("downloadData", "Download")
                 
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel("Output result panel", 
              verbatimTextOutput("View", placeholder=TRUE), 
              tableOutput("Your.output")
    )
    
  )
)