navbarPage(
  id = "tabs",
  title = "CCIEA Uploader",
  tabPanel(
    "1. Login",
    googleSignInUI("gar"),
    with(
      tags, 
      dl(dt("Name"), dd(textOutput("g_name")),
         dt("Email"), dd(textOutput("g_email")),
         dt("Image"), dd(uiOutput("g_image")) ))),
  tabPanel(
    "2. Upload",
    helpText("Upload a comma-seperated value (*.csv) file from your machine"),
    fileInput(
      "file1", "Choose CSV File",
      multiple = F,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
    dataTableOutput("tbl_up")),
    
  tabPanel(
    "3. Match",
      h2("Choose dataset"),
      helpText("Choose a California Current IEA dataset you mean to update with this upload."),
      selectInput(
        "sel_dataset", "Dataset",
        chs_datasets,
        selected = "cciea_EI_FBC_2020"),
      dataTableOutput("tbl_ed"),
    
      h2("Match columns"),
      helpText("Please match one or more columns in the original dataset with those available in the uploaded dataset."),
      uiOutput("ui_match_cols"),
      actionButton("btn_matched", "Confirm matches")),
    
  tabPanel(
    "4. Plot",
      h2("4. Confirm with plot"),
      helpText("Before submitting this update, please confirm your data visually."),
      plotlyOutput("plot"),
      dataTableOutput("tbl_match")),
  
  tabPanel(
    "5. Metadata"),

  tabPanel(
    "6. Submit",
      actionButton("btn_submit", "Submit")) )
