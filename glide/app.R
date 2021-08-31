# global ----
librarian::shelf(
  dplyr, DT, ggplot2, glue, here, plotly, purrr, readr, shiny, shinyglide, stringr, tidyr)
options(readr.show_col_types = FALSE)

#* variables ----
dir_ecoidx   <- "/Users/bbest/github/noaa-iea/ecoidx"
datasets_csv <- file.path(dir_ecoidx, "data-raw/_cciea_datasets.csv")
lut_cmp_csv  <- here("data/lut_components.csv")

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
        selected = "cciea_EI_HCI"),
      dataTableOutput("tbl_ed")),
    
    screen(
      #* 3. Match columns ----
      h2("3. Match columns"),
      helpText("Please match one or more columns in the original dataset with those available in the uploaded dataset."),
      uiOutput("ui_match_cols")),
    
    screen(
      #* 4. Confirm with plot ----
      h2("4. Confirm with plot"),
      helpText("Before submitting this update, please confirm your data visually."),
      plotlyOutput("plot"),
      actionButton("btn_submit", "Submit"))
    ))

# server ----
server <- function(input, output) {
  
  #* get_d_up() ----
  get_d_up <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  #* tbl_up ----
  output$tbl_up <- renderDataTable({
    get_d_up()
  })
  
  #* get_d_ed() ----
  get_d_ed <- reactive({
    req(input$sel_dataset)
    ed_csv <- glue("{dir_ecoidx}/data-raw/{input$sel_dataset}_raw.csv")
    #ed_yml <- glue("{dir_ecoidx}/data-raw/{input$sel_dataset}_meta.yml")
    read_csv(ed_csv)
  })
  
  #* tbl_ed ----
  output$tbl_ed <- renderDataTable({
    get_d_ed()
  })
  
  #* get_match_autoselect() ----
  get_match_autoselect <- function(col_ed, d_up, d_ed){
    # d_ed <- read_csv(glue("{dir_ecoidx}/data-raw/cciea_EI_FBN_raw.csv"))
    # d_up <- read_csv(glue("/Users/bbest/github/noaa-iea/meta-app/data/upload/cciea_EI_FBN/NCC.ForageJellies.2021.csv"))
    
    if (col_ed == "time"){
      sel <- case_when(
        "time"       %in% names(d_ed) ~ "time",
        "year-month" %in% names(d_ed) ~ "year-month",
        "year"       %in% names(d_ed) ~ "year",
        "month"      %in% names(d_ed) ~ "month",
        TRUE ~ NULL)
      return(sel)
    }
    
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
    
    # cols_idx = intersect(names(d), cols_idx_all)
    intersect(names(d), cols_idx_all) 
    }

  #* ui_match_cols ----
  output$ui_match_cols <- renderUI({
    req(input$file1, input$sel_dataset)
    
    ctls <- list()
    
    d_up <- get_d_up()
    d_ed <- get_d_ed()
    cols_up <- c("", names(d_up))
    
    for (col_ed in names(d_ed)){
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
  
  #* plot ----
  # TODO: select variable
  # TODO: augment with variability
  output$plot <- renderPlotly({
    n_matches <- sum(nchar(map_chr(str_subset(names(input), "^sel_match_"), function(x) input[[x]])) > 1)
    # TODO: reactive condition on whether time, index and at least 1 value matched
    req(input$file1, input$sel_dataset, n_matches > 1)
    
    #browser()

    d_ed2 <- get_d_ed() %>% 
    #d_ed2 <- read_csv(glue("{dir_ecoidx}/data-raw/cciea_EI_HCI_raw.csv")) %>% 
      select(time, hci_regn1) %>% 
      mutate(
        source = "current",
        time = as.Date(time))
    
    d_up2 <- get_d_up() %>% 
    #d_up2 <- read_csv(glue("/Users/bbest/github/noaa-iea/meta-app/data/upload/cciea_EI_HCI_new/ei_hci_rgn1_M.csv")) %>% 
      select(time, hci_regn1 = data) %>% 
      mutate(
        source = "upload",
        time = as.Date(glue("{time}-01"), "%Y-%m-%d"))
    
    d_p <- bind_rows(
      d_ed2,
      d_up2)

    p <- ggplot(d_p, aes(x=time, y=hci_regn1, group=source, color=source, size=source)) + 
      geom_line() + 
      #geom_point() +
      scale_color_manual(values = c(
        "current" = "blue", "upload" = "red")) +
      scale_size_manual(values = c(
        "current" = 0.5, "upload" = 0.2)) # + 
      # theme(
      #   legend.position = c(1, 1),
      #   #legend.direction="horizontal", 
      #   legend.justification = c(1, 1))
    # p
      
    ggplotly(p)
  })
}
shinyApp(ui, server)