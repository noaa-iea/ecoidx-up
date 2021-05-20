# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.cloud.google.com/apis/credentials/oauthclient/596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com?authuser=2&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com")

shinyUI(fluidPage(
  titlePanel("CCIEA Metadata"),
  sidebarLayout(
    sidebarPanel(
      googleSignInUI("gar"),
      selectInput(
        "sel_pi", "PI", unique(providers$pi))),
    mainPanel(
      with(
        tags, 
        dl(dt("Name"), dd(textOutput("g_name")),
           dt("Email"), dd(textOutput("g_email")),
           dt("Image"), dd(uiOutput("g_image")) )),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Datasets", 
          dataTableOutput("tbl_datasets")),
        tabPanel(
          "Variables", 
          dataTableOutput("tbl_dataset_vars")),
        tabPanel(
          "Plot", 
          #plotlyOutput("fig_ts"))
          plotOutput("fig_ts"))
        )))
))
