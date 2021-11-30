# server: calculate statistics and generate plot
server <- function(input, output, session) {

  output$graph_target <- shiny::renderPlot({
    if(input$target != "<no target>" & input$var != input$target)  {
      data %>% explore(!!sym(input$var), target = !!sym(input$target), auto_scale = input$auto_scale, split = input$split)
    }
  }) # renderPlot graph_target
  
  output$graph_explain <- shiny::renderPlot({
    if(input$target != "<no target>") {
      if (ncol(data) > 20) {
        # show waiting-window
        shiny::showModal(modalDialog("Growing tree ... (this may take a while)", footer = NULL))
        # grow decision tree
        data %>% explain_tree(target = !!sym(input$target), size=0.9)
        # ready
        shiny::removeModal()
      } else {
        # grow decision tree
        data %>% explain_tree(target = !!sym(input$target), size=0.9)
      } # if ncol
    } # if input$target
  }) # renderPlot graph_explain
  
  output$graph <- shiny::renderPlot({
    data %>% explore(!!sym(input$var), auto_scale = input$auto_scale)
  }) # renderPlot graph
  
  output$text <- shiny::renderPrint({
    data %>% describe(!!input$var, out = "text", margin = 4)
  }) # renderText
  
  output$describe_tbl <- shiny::renderPrint({
    data %>% describe_tbl(out = "text")
  }) # renderText
  
  output$describe_all <- DT::renderDT({
    DT::datatable(data = data %>% describe(out = "text"),
                  rownames = FALSE,
                  selection = 'none',
                  options = list(pageLength = 15))
  }) # renderDataTable
  
  
  output$view <- DT::renderDT({
    DT::datatable(data = data,
                  rownames = FALSE,
                  selection = 'none',
                  options = list(pageLength = 15, scrollX = TRUE))
  }) # renderDataTable
  
  output$data_variables <- DT::renderDT({
    DT::datatable(data = data_variables,
                  rownames = FALSE,
                  selection = 'none',
                  options = list(pageLength = 15, scrollX = TRUE))
  }) # renderDataTable
  
} # server