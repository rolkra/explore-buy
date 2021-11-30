# define ui
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h3("explore"),
      shiny::hr(),
      shiny::selectInput(inputId = "target",
                         label = "target",
                         choices = c("<no target>",guesstarget),
                         selected = "target_ind"),
      shiny::selectInput(inputId = "var",
                         label = "variable",
                         choices = names(data),
                         selected = "disp"),
      shiny::checkboxInput(inputId = "auto_scale", label="auto scale", value=TRUE),
      shiny::checkboxInput(inputId = "split", label="split by target", value=TRUE)      
      , width = 3),  #sidebarPanel
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("variable",
                        shiny::conditionalPanel(condition = "input.target != '<no target>'",
                                                shiny::plotOutput("graph_target")),
                        shiny::plotOutput("graph", height = 300),
                        shiny::verbatimTextOutput("text")
        ),
        #textOutput("text")
        shiny::tabPanel("explain",
                        shiny::plotOutput("graph_explain")),
        shiny::tabPanel("overview", shiny::br(),
                        shiny::verbatimTextOutput("describe_tbl"),
                        DT::DTOutput("describe_all")),
        shiny::tabPanel("data", shiny::br(),
                         DT::DTOutput("view")),
        shiny::tabPanel("help", shiny::br(),
                        p(data_title),
                        p(data_description),
                        DT::DTOutput("data_variables")
                       )   # defined in global.R
        
      ) # tabsetPanel
      , width = 9) # mainPanel
  ) # sidebarLayout
) # fluidPage