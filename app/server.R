shinyServer(function(input, output, session) {

  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name  = renderText({ input[["gar-g_name"]] })
  output$g_email = renderText({ input[["gar-g_email"]] })
  output$g_image = renderUI({ img(src=input[["gar-g_image"]]) })
  
  output$tbl_datasets <- renderDataTable({
    # TODO: PI or Contact to dataset
    datasets #%>% 
      #filter(pi == input$sel_pi)
    }, options = list(scrollX = TRUE), selection = 'single')
  
  # get_dataset_id() ----
  get_dataset_id <- reactive({
    req(input$tbl_datasets_rows_selected)
    
    datasets %>% 
      slice(input$tbl_datasets_rows_selected) %>% 
      pull(dataset_id)
  })
  
  get_var_id <- reactive({
    req(input$tbl_dataset_vars_rows_selected)
    
    dataset_vars %>% 
      slice(input$tbl_dataset_vars_rows_selected) %>% 
      pull(var_id)
  })
  
  output$tbl_dataset_vars <- renderDataTable({
    dataset_vars %>% 
      filter(dataset_id == get_dataset_id())
    }, options = list(scrollX = TRUE), selection = 'single', editable = T)
  
  #output$fig_ts <- renderPlotly({
  output$fig_ts <- renderPlot({

    dataset_id <- get_dataset_id()
    var_id     <- get_var_id()
    
    req(dataset_id, var_id)
    
    # browser()
    # dataset_id = "cciea_AC"; var_id = "consumption_fish"
    
    d <- get(dataset_id) %>% 
      tibble() %>% 
      select(time, index = all_of(var_id)) %>% 
      mutate(
        year  = as.integer(format(as.Date(time),"%Y")),
        # year  = as.Date(time),
        index = as.double(index)) %>% 
      select(-time) %>% 
      filter(
        !is.na(year), !is.na(index),
        !is.nan(year), !is.nan(index)) %>%
      select(year, index)
    # 
    #browser()
    # View(d)
    
    # devtools::load_all("~/github/noaa-iea/ecoidx")
    fig_static <- ecoidx::plot_ts(d) +
       ggtitle(glue("{dataset_id}: {var_id}"))
    # p_interactive <- ecoidx::plot_ts(d, add_icons = F) %>% 
    #   plotly::ggplotly()
    # p_interactive
    fig_static
  })
  
  
})
