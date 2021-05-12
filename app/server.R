shinyServer(function(input, output) {

  output$tbl_dataset_vars <- renderDataTable({
    dataset_vars %>% 
      filter(pi == input$sel_pi)
    }, options = list(scrollX = TRUE))
  
})
