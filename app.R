#
# Feature weights shiny UI
#
library(DT)
library(ggdark)
library(plotly)
library(shinythemes)
library(tidyverse)
library(yaml)

options(stringsAsFactors = FALSE)
options(digits = 3)

source('helper.R')

# load configuration
config <- yaml::read_yaml('config.yml')

server <- function(input, output) {
  # reactive config / table
  data_config <- reactive({
    config[[input$feature_type]][[input$data_source]]
  })
  weights_table <- reactive({
    req(rv$score_field)
    load_table(data_config(), rv$score_field)
  })

  # reactive values
  rv <- reactiveValues(score_field = NULL)

  # event handlers

  # reset score field when data source is changed
  observe({
    rv$score_field <- input$score_field
  })
  observeEvent(input$data_source, {
    rv$score_field <- NULL
  })

  # text/html output
  output$weights_name <- renderUI(h3(input$data_source))
  output$about_html <- renderUI(HTML(paste0(readLines('html/about.html'), collapse = '\n')))

  # ui output
  output$select_score <- renderUI({
    selectInput("score_field",  "Score:",
                choices = data_config()$scores,
                selected = data_config()$scores[[1]])
  })

  # table output
  output$weights_table <- renderDataTable({
    feature_weights <- weights_table()

    if (input$feature_type == 'Genes') {
      feature_weights <- feature_weights %>%
        inner_join(get_gene_external_links(feature_weights$symbol))
    }
    DT::datatable(feature_weights, style = 'bootstrap', escape = FALSE)
  })

  # plot output
  output$weights_dist <- renderPlotly({
    scores <- weights_table() %>%
      pull(input$score_field)

    dat <- data.frame(score = scores)

    ggplot(dat, aes(x = score)) +
      geom_density(alpha = 0.85, fill = '#9dff00') +
      ggtitle(sprintf("Distribution of %s", input$score_field)) +
      dark_theme_gray() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.grid.major = element_line(color = "#555555", size = 0.2),
        panel.grid.minor = element_line(color = "#555555", size = 0.2)
      )
  })

}

ui <- fluidPage(
  titlePanel(sprintf("Feature Weights (%s)", config$version), 
             windowTitle = sprintf("Feature Weights (%s)", config$version)),
  tags$head(includeCSS("resources/styles.css")),
  theme = shinytheme("darkly"),
  sidebarLayout(
    #
    # sidebar
    #
    sidebarPanel(
      p(sprintf("Last Update: %s", config$last_update)),
      tabsetPanel(type = "tabs",
          tabPanel(
            "Settings",
            br(),
            selectInput("feature_type", "Feature:", choices = c('Genes'), selected = 'Genes'),
            selectInput("data_source",  "Source:",  choices = names(config$Genes), selected =  names(config$Genes)[1]),
            uiOutput("select_score")
          ),
          tabPanel("About", uiOutput("about_html")))
    ),
    #
    # main panel
    #
    mainPanel(
        uiOutput("weights_name"),
        plotlyOutput("weights_dist"),
        br(),
        dataTableOutput('weights_table')
    )
  )
)

shinyApp(ui = ui, server = server)
