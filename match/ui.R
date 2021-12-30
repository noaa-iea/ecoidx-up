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
    #dataTableOutput("tbl_ed"),
    
    h2("Match columns to CCIEA standardized format"),
    fluidRow(
      class = "panel panel-heading",
      div(
        class = "panel-heading",
        h3("Drag columns from Uploaded to Target")
      ),
      fluidRow(
        class = "panel-body",
        column(
          width = 2,
          tags$div(
            class = "panel panel-default",
            tags$div(class = "panel-heading", "Uploaded"),
            tags$div(
              class = "panel-body",
              id = "sort1",
              colnames_to_tags(flds_std)))),
        column(
          width = 2,
          #"todo: target columns"),
          colnames_to_target(flds_std)),
        column(
          width = 8,
          verbatimTextOutput("variables"),
          #plotOutput("plot")
          h3("Uploaded"),
          dataTableOutput("tbl_up"),
          h3("Target"),
          dataTableOutput("tbl_ed")))),
    sortable_js(
      "sort1",
      options = sortable_options(
        multiDrag = T,
        group = "name",
        # group = list(
        #   name = "sortGroup1",
        #   put = TRUE
        # ),
        sort = FALSE,
        onSort = sortable_js_capture_input("sort_vars"))),
    colnames_to_target_sortables(flds_std),
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
