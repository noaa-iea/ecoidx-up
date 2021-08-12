librarian::shelf(
    DT, shiny, juba/shinyglide)

ui <- fluidPage(
    titlePanel("Upload File"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # tags$hr(),
            # checkboxInput("header", "Header", TRUE),
            # radioButtons("sep", "Separator",
            #              choices = c(Comma = ",",
            #                          Semicolon = ";",
            #                          Tab = "\t"),
            #              selected = ","),
            # radioButtons("quote", "Quote",
            #              choices = c(None = "",
            #                          "Double Quote" = '"',
            #                          "Single Quote" = "'"),
            #              selected = '"'),
            # tags$hr(),
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),
        
        mainPanel(
            dataTableOutput("contents")
        )
    )
)

server <- function(input, output) {
    
    output$contents <- renderDataTable({
        req(input$file1)
        df <- read_csv(input$file1$datapath)
                       # ,
                       # header = input$header,
                       # sep = input$sep,
                       # quote = input$quote)
        
        # if(input$disp == "head") {
        #     return(head(df))
        # }
        # else {
            # return(df)
        # }
        df
    })
    
}
# Run the app ----
shinyApp(ui, server)