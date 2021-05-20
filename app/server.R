shinyServer(function(input, output, session) {

  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name  = renderText({ input[["gar-g_name"]] })
  output$g_email = renderText({ input[["gar-g_email"]] })
  output$g_image = renderUI({ img(src=input[["gar-g_image"]]) })
  
  
  output$tbl_dataset_vars <- renderDataTable({
    dataset_vars %>% 
      filter(pi == input$sel_pi)
    }, options = list(scrollX = TRUE))
  
})
