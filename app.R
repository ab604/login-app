library(shiny)
library(shinymanager)
library(keyring)
library(shinyAce)
library(mailR)

# # Read credentials
credentials <- readRDS("data/credentials.Rds")

# # Create SQL db
 shinymanager::create_db(
 credentials_data=credentials,
 sqlite_path="data/protectedUsers.sqlite",
 passphrase=Sys.getenv("KEYRING_KEY")
 )

# Wrap your UI with secure_app
ui <- secure_app(fluidPage(
    
    # Heading
    tags$h2("My secure application"),
    # Text output for login
    verbatimTextOutput("auth_output"),
    
    textInput("to", "To:", value="Enter email address"),
    textInput("subject", "Subject:", value="Weclome to doxyICU"),
    aceEditor("message", value="Hello World"),
    #shinyFilesButton("file", "File select", "Please select a file", multiple = TRUE),
    actionButton("send", "Send mail"),
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))

# Server
server <- function(input, output) {
    # Login
    user <- secure_server(
        check_credentials <- check_credentials(db="data/protectedUsers.sqlite", 
                                               passphrase=Sys.getenv("KEYRING_KEY")))
    # Get login details
    output$auth_output <- renderPrint({reactiveValuesToList(user)})
    
    # Email
    observe({
        if(is.null(input$send) || input$send==0) return(NULL)
        subject <- isolate(input$subject)
        msg <- isolate(input$message)
        sender <- "doxyicu@gmail.com"
        #recipients <- {reactiveValuesToList(user$emailAddress)}
        recipients <- isolate(input$to)
        #path <- parseFilePaths(volumes, input$file)
        #fileName <-   shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('pdf', 'html'))
        send.mail(from = sender,
                  to = recipients,
                  subject = subject,
                  body = msg,
                  smtp = list(host.name = "smtp.gmail.com", port = 465,
                              user.name=sender, passwd=Sys.getenv("DOXY_GMAIL"), ssl=TRUE),
                  authenticate = TRUE,
                  send = TRUE)
        
    })
    
    # Histogram
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
