# global ----
librarian::shelf(
  dplyr, DT, ggplot2, glue, 
  googleAuthR, googlesheets4,
  here, plotly, purrr, 
  readr, shiny, shinyglide, stringr, tidyr)
options(readr.show_col_types = FALSE)

# options(error = browser()) # stop on error
# options(warn = 2)          # turn warnings into errors
options(warn = 0); .Options$error <- NULL # default

#* variables ----
#dir_ecoidx   <- "/Users/bbest/github/noaa-iea/ecoidx"
dir_ecoidx   <- "/share/github/ecoidx"
datasets_csv <- file.path(dir_ecoidx, "data-raw/_cciea_datasets.csv")
lut_cmp_csv  <- here("data/lut_components.csv")

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.cloud.google.com/apis/credentials/oauthclient/596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com?authuser=2&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com")

gsheets_sa_json <- switch(
  Sys.info()[["effective_user"]],
  bbest = "/Users/bbest/My Drive (ben@ecoquants.com)/private/iea-uploader-6ad8e5a412dc_google-service-account.json")
gsheet <- "https://docs.google.com/spreadsheets/d/1F8H2UFcajLVqq_MIPS0YAUt3ZnSSJP7cZ1hxCsMLW4g/edit"

# ensure secret JSON file exists
stopifnot(file.exists(gsheets_sa_json))

# authenticate to GoogleSheets using Google Service Account's secret JSON
gs4_auth(path = gsheets_sa_json)

#sheet_names(gsheet)
g <- read_sheet(gsheet, "Indicator Metadata")

#* load dataset choices ----
d_cmp <- read_csv(lut_cmp_csv)

d_datasets <- read_csv(datasets_csv) %>% 
  mutate(
    cmp = str_replace(dataset_id, "cciea_([A-Z]+).*", "\\1")) %>% 
  left_join(
    d_cmp, by = "cmp") %>% 
  arrange(component, title)
# d_datasets %>% filter(dataset_id == "cciea_EI_HCI") %>% pull(title) # "Habitat Compression Index"
# d_datasets %>% filter(dataset_id == "cciea_EI_FBN") %>% pull(title) # "Habitat Compression Index"
# EI:  "Forage Biomass, California Current North"

vec_datasets <- with(d_datasets, setNames(dataset_id, title))

d <- d_datasets %>% 
  select(component, dataset_id, title) %>% 
  group_by(component) %>% 
  nest()
chs_datasets <- list()
for (component in d$component){ # component = d$component[1]
  chs_datasets[component] <- list(with(
    d %>% 
      filter(component == !!component) %>% 
      pull(data) %>% .[[1]],
    setNames(dataset_id, title)))
}

# ui ----
ui <- fluidPage(
  titlePanel("CCIEA Upload"),
  googleSignInUI("gar"),
  with(
    tags, 
    dl(dt("Name"), dd(textOutput("g_name")),
       dt("Email"), dd(textOutput("g_email")),
       dt("Image"), dd(uiOutput("g_image")) )),
  glide(
    #height = "350px",
    screen(
      #* 1. Upload csv ----
      h2("1. Upload file"),
      helpText("Upload a comma-seperated value (*.csv) file from your machine"),
      fileInput(
        "file1", "Choose CSV File",
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")),
      dataTableOutput("tbl_up")),
    
    screen(
      #* 2. Select dataset ----
      h2("2. Select dataset"),
      helpText("Choose a California Current IEA dataset you mean to update with this upload."),
      selectInput(
        "sel_dataset", "Dataset",
        chs_datasets,
        selected = "cciea_EI_FBC_2020"),
      dataTableOutput("tbl_ed")),
    
    screen(
      #* 3. Match columns ----
      h2("3. Match columns"),
      helpText("Please match one or more columns in the original dataset with those available in the uploaded dataset."),
      uiOutput("ui_match_cols"),
      actionButton("btn_matched", "Confirm matches")),
    
    screen(
      #* 4. Confirm with plot ----
      h2("4. Confirm with plot"),
      helpText("Before submitting this update, please confirm your data visually."),
      plotlyOutput("plot"),
      dataTableOutput("tbl_match"),
      actionButton("btn_submit", "Submit")),

    screen(
      #* 5. Enter Metadata ----
      h2("5. Enter Metadata"),
      textAreaInput(
        "txt_src",
        "Source Data"),
      textAreaInput(
        "txt_calcs",
        "Additional Calculations"),
      actionButton(
        "btn_savemeta", "Save Metadata", icon=icon("fas fa-save")))
))

# server ----
server <- function(input, output, session) {

  #* login ----
  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name  = renderText({ input[["gar-g_name"]] })
  output$g_email = renderText({ input[["gar-g_email"]] })
  output$g_image = renderUI({ img(src=input[["gar-g_image"]]) })
  
  #* values ----
  values <- reactiveValues(
    cols_matched = F)
  
  #* get_d_up() ----
  get_d_up <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  #* get_d_ed() ----
  get_d_ed <- reactive({
    req(input$sel_dataset)
    # input <- list(sel_dataset = "cciea_EI_FBC_2020")
    ed_csv <- glue("{dir_ecoidx}/data-raw/{input$sel_dataset}_raw.csv")
    ed_yml <- glue("{dir_ecoidx}/data-raw/{input$sel_dataset}_meta.yml")
    
    d_ed <- readr::read_csv(ed_csv, na = c("", "NA", "NaN")) # %>% 
      #mutate(mean_cpue_dbl = as.double(mean_cpue))
    m_ed <- yaml::read_yaml(ed_yml)
    
    attr(d_ed, "meta") <- m_ed
    d_ed
  })
  
  #* get_d_match() ----
  get_d_match <- reactive({
    
    d_ed <- get_d_ed()
    m_ed <- attr(d_ed, "meta")
    
    # browser()
    d_ed <- d_ed %>% 
      mutate(
        source = "current",
        time = as.Date(time))
    
    cols_match <- tibble(
      sel_ed = str_subset(names(input), "^sel_match_")) %>% 
      mutate(
        col_up = map_chr(sel_ed, ~ input[[.x]]),
        col_ed = str_replace(sel_ed, "sel_match_", "")) %>% 
      select(-sel_ed)
    
    # TODO: confirm all columns exist and are of proper data type 
    
    d_up <- get_d_up()
    # if(!"time" %in% names(d_up))
    #   browser()
    # stopifnot("time" %in% names(d_up))
    
    d_up <- d_up() %>% 
      select(all_of(cols_match$col_up)) %>% 
      rename_with(~ filter(cols_match, col_up==.x) %>% pull(col_ed)) %>% 
      mutate(
        source = "upload",
        # TODO: dynamically handle time
        #time = as.Date(glue("{time}-01-01"), "%Y-%m-%d"))
        time = as.Date(glue("{year}-01-01"), "%Y-%m-%d"))
    
    d_match <- bind_rows(
      d_ed,
      d_up)
    
    attr(d_match, "meta") <- m_ed
    d_match
  })
  
  #* tbl_up ----
  output$tbl_up <- renderDataTable({
    get_d_up()
  })
  
  #* tbl_ed ----
  output$tbl_ed <- renderDataTable({
    get_d_match()
  })
  
  #* tbl_match ----
  output$tbl_match <- renderDataTable({
    get_d_match() 
  })
  
  #* get_match_autoselect() ----
  get_match_autoselect <- function(col_ed, d_up, d_ed, m_ed){
    # d_ed <- read_csv(glue("{dir_ecoidx}/data-raw/cciea_EI_FBN_raw.csv"))
    # d_up <- read_csv(glue("/Users/bbest/github/noaa-iea/meta-app/data/upload/cciea_EI_FBN/NCC.ForageJellies.2021.csv"))
    
    # col_ed = "mean_cpue"
    m_ed <- attr(d_ed, "meta")
    col_ed_m <- m_ed$columns %>% 
      keep(~ col_ed %in% .x) %>% 
      names()

    if (col_ed_m == "time"){
      col_time <- case_when(
        "time"       %in% names(d_up) ~ "time",
        "year-month" %in% names(d_up) ~ "year-month",
        "year"       %in% names(d_up) ~ "year",
        "month"      %in% names(d_up) ~ "month")
      return(col_time)
    }
    
    if (col_ed_m == "metrics"){
      # TODO: handle multiple metrics
      
      col_metric <- case_when(
        "index" %in% names(d_up) ~ "index")
      return(col_metric)
    }
     
    if (col_ed_m == "indices"){
      # TODO: handle multiple indices
      
      cols_idx_all = c(
        "metric",    # cciea_EI_RREAS_diversity_list
        "latitude",  # cciea_OC_BEUTI, cciea_OC_CUTI, cciea_OC_SL1
        "longitude", # cciea_OC_SL1
        "blob_id",   # cciea_OC_MHW_EV
        "station",   # cciea_OC_SL1, newportCTD
        "depth", "project", # newportCTD
        # read_csv("data-raw/_extra_erddap_indexes.csv") %>%
        #   distinct(index) %>% pull(index) %>% sort() %>% paste(collapse = '", "') %>% cat()
        "common_name", "county", "diet_species_cohort", "location", "population", "region", "scientific_name", "site", "species", "species_cohort", "species_group", "taxa", "timeseries", "use_type", "vessel_category")
      
      cols_idx <- intersect(names(d_up), cols_idx_all) 
      if (length(cols_idx) > 0)
        return(cols_idx[1])
    }
    
    if (col_ed_m == "errors"){
      i <- which(tolower(names(d_up)) == tolower(col_ed))
      if (length(i) == 1)
        return(names(d_up)[i])
    }
    
    NA
    }

  #* ui_match_cols ----
  output$ui_match_cols <- renderUI({
    req(input$file1, input$sel_dataset)
    
    ctls <- list()
    
    d_up <- get_d_up()
    d_ed <- get_d_ed()
    cols_up <- c("", names(d_up))
    
    for (col_ed in names(d_ed)){
      # if(col_ed == "time") browser() # DEBUG
      
      ctls <- append(
        ctls, 
        list(
          selectInput(
            paste0("sel_match_", col_ed),
            col_ed,
            cols_up,
            selected = get_match_autoselect(col_ed, d_up, d_ed))))
    }
    tagList(ctls)
    
  })
  
  #* btn_matched ----
  observeEvent(input$btn_matched, {
    values$cols_matched = T
  })
  
  #* observe id, update metadata txt_* ----
  observeEvent(input$sel_dataset, {
    req(input$sel_dataset)
    
    id <- input$sel_dataset
    cid <- glue("cciea_{id}")
    
    d <- filter(g, `ERDDAP Dataset ID` == id) %>% slice(1)
    
    updateTextAreaInput(
      session,
      "txt_src",
       value = d$`Source Data`)
    
    updateTextAreaInput(
      session,
      "txt_calcs",
       value = d$`Additional Calculations`)
  })
  
  #* observe btn_savemeta, update gsheet ----
  observeEvent(
    input$btn_savemeta,{
    
      i <- which(g$`ERDDAP Dataset ID` == input$sel_dataset)[1] + 1
      
      range_write(ss = gsheet, data = tibble(
          `Source Data` = input$txt_src), 
        sheet = "Indicator Metadata", col_names = F,
        range = glue("Z{i}"))
                  
      range_write(ss = gsheet, data = tibble(
          `Additional Calculations` = input$txt_calcs), 
        sheet = "Indicator Metadata", col_names = F,
        range = glue("AA{i}"))
      
    })
  
  #* plot ----
  # TODO: select variable
  # TODO: augment with variability
  output$plot <- renderPlotly({
    #n_matches <- sum(nchar(map_chr(str_subset(names(input), "^sel_match_"), function(x) input[[x]])) > 1)
    # TODO: reactive condition on whether time, index and at least 1 value matched
    req(input$file1, input$sel_dataset, values$cols_matched)

    d_P <- get_d_match()
    m_ed <- attr(d_P, "meta")
    z <- m_ed$columns
    
    # TODO: filter by indexes
    d_p <- d_P %>% 
      filter(species_group == d_P$species_group[1]) %>% 
      select(all_of(c(z$time, z$metrics[1], "source"))) %>% 
      arrange(time, source)
    
    # browser()

    p <- ggplot(
      d_p,
      aes_string(
        x = z$time, y = z$metrics[1])) +
      geom_line(
        data = subset(d_p, source == "current"),
        color = "blue", size = 0.5) +
      geom_line(
        data = subset(d_p, source == "upload"), 
        color = "red", size = 0.2)
      # theme(
      #   legend.position = c(1, 1),
      #   #legend.direction="horizontal", 
      #   legend.justification = c(1, 1))
    # p
      
    ggplotly(p)
  })
}
shinyApp(ui, server)