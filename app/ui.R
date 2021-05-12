library(shiny)

shinyUI(fluidPage(
  titlePanel("CCIEA Metadata"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "sel_pi", "PI", unique(providers$pi))),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "dataset_vars", 
          dataTableOutput("tbl_dataset_vars")))))
))
