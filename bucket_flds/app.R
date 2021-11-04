## Example shiny app to create a plot from sortable inputs
librarian::shelf(
  glue, htmlwidgets, magrittr, readr, shiny, sortable)

options(readr.show_col_types = FALSE)
d_source <- readr::read_csv(here::here("data/upload/cciea_EI_FBC/CCC.Forage.2020.csv"))
#d_target <- readr::read_csv("/share/github/ecoidx/data-raw/cciea_EI_FBC_2020_raw.csv")
d_target <- readr::read_csv(here::here("data/upload/cciea_EI_COP/IEA_NoSoCopeAnom_SppRichness.2021.csv"))

# paste(colnames(d_target), collapse=", ")
# time, species_group, mean_cpue, Seup, Selo

colnames_to_tags <- function(df){
  lapply(
    colnames(df),
    function(co) { # co = colnames(df)[1]
      tag(
        "p",
        list(
          class = class(df[[co]]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co))) })}

colnames_to_target <- function(df){
  lapply(
    colnames(df),
    function(fld) {
      tags$div(
        class = "panel panel-default",
        tags$div(
          class = "panel-heading",
          tags$span(class = "glyphicon glyphicon-stats"),
          fld),
        tags$div(
          class = "panel-body",
          id = glue::glue("sort_target_{fld}"))) }) }

colnames_to_target_sortables <- function(df){
  lapply(
    colnames(df),
    function(fld) {
      sortable_js(
        glue("sort_target_{fld}"),
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE),
          onSort = sortable_js_capture_input(glue("sort_target_{fld}")))) }) }

ui <- fluidPage(
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
            colnames_to_tags(d_source)))),
      column(
        width = 2,
        #"todo: target columns"),
        colnames_to_target(d_target)),
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
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars"))),
  colnames_to_target_sortables(d_target)
)

server <- function(input, output) {
  output$variables <- renderPrint({

    in_vars <- names(input) %>% str_subset("^sort_target_.*")
    in_vals <- purrr::map_chr(in_vars, function(x) input[[x]])
    return(glue("{in_vars}: {in_vals}"))

  })

  #* tbl_up ----
  output$tbl_up <- renderDataTable({
    d_source
  }, options = list(
    pageLength = 3,
    lengthMenu = c(3, 5, 10, 15, 20)))
  
  #* tbl_ed ----
  output$tbl_ed <- renderDataTable({
    d_target
  }, options = list(
    pageLength = 3,
    lengthMenu = c(3, 5, 10, 15, 20)))
}
shinyApp(ui, server)