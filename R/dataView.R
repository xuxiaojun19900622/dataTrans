#' Data Summary
#' @name dataView
#' @description View and plot data frame
#' @return none
#' @import shiny
#' @import miniUI
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#' @import plotly

dataView <- function() {
  datalist <- tryCatch({
    ls(envir = .GlobalEnv)[unlist(lapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv) , is.data.frame))]},
    error = function(e) stop("There are no data frames in the global environment!"))

  ## Gadget UI
  ui <- miniUI::miniPage(
    theme = "spacelab",
    gadgetTitleBar(title = "Data Summary",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniTabstripPanel(
      # Page 1
      miniTabPanel("Manage Data",
        icon = icon("table"),
        sidebarLayout(
          sidebarPanel(
            tags$h4("Select data frame"),
            selectizeInput(inputId = "obj_name", label = NULL, choices = datalist),
            checkboxInput(inputId = "showTips",label = "Show Tips",value = F),
            conditionalPanel("input.showTips",
                               list(
                                 tags$b("Tips:"),
                                 tags$li("Click on columns to toggle selection status. At least one column should be selected."),
                                 # tags$li("Click and drag the table header to reorder columns."),
                                 tags$li("Searching and filtering are case-insensitive and using regular expressions."),
                                 br(),
                                 tags$a(href = "", "Click here for more details.")
                               ),
            )
          ),
          mainPanel(h3("Data Table"),
                    tags$br(),
                    DT::DTOutput("table_1", width = "95%", height = "auto"),
                    tags$br(),
                    fluidRow(
                      column(4, offset = 0,
                            actionButton(inputId = "select_all", label = "Select All",class = "btn-success"),
                            actionButton(inputId = "reset", label = "Reset")
                      ),
                      column(2, offset = 4,
                            actionButton(inputId = "view",label = "View and update",class = "btn-primary")
                      ),
                    )
          ),
        )
      ),

      # Page 2
      miniTabPanel("Summary",
        icon = icon("area-chart"),
        miniContentPanel(
          fluidRow(
            column(3,
                   h4(icon("bar-chart-o"), "Plot"),
                   selectInput(inputId = "var", label = "Choose a variable", choices = NULL),
            ),
            column(8,offset = 1,
                   plotlyOutput("plot"),
            )
          ),
          h4(icon("table"), "Plot and Summary"),
          verbatimTextOutput("summary")
        )
      )
    )
  )

  server <- function(input, output, session) {

    ## Page 1

    data <- reactiveVal()
    observeEvent(input$obj_name, {
      if (!nzchar(input$obj_name))
        showNotification("No dataset available.", duration = 10, type = "error")
      req(input$obj_name)
      data(get(input$obj_name, envir = .GlobalEnv))
    })

    options(DT.TOJSON_ARGS = list(na = 'string'))
    output$table_1 <- DT::renderDT(datatable(data(),
                                                    class = 'display compact nowrap',
                                                    filter = list(position = 'top', plain = TRUE,clear = FALSE),
                                                    extensions = c('Scroller','FixedColumns','Buttons'), # ColReorder
                                                    selection = list(mode = 'multiple', target = 'column'),
                                                    options = list(
                                                      dom  = 'Bfti',
                                                      buttons = list('copy','csv'),
                                                      scroller = TRUE, scrollX = TRUE, scrollY = 330, autoWidth= F,
                                                      searchHighlight = T, fixedColumns = T,
                                                      # colReorder = T, stateSave = T,
                                                      search = list(regex = TRUE, caseInsensitive = T),
                                                      columnDefs=list(list(className="dt-left",targets="_all"))
                                                    )),
                                          server = T,
                                          )

    ## button select_all and reset
    proxy <- dataTableProxy('table_1')
    observe({replaceData(proxy, data(), resetPaging = T, clearSelection = "all")})

    observeEvent(input$reset, {
      data(get(input$obj_name, envir = .GlobalEnv)) # reload data
      proxy %>% reloadData(resetPaging = T, clearSelection = "all") %>% clearSearch()
    })

    observeEvent(input$select_all, {
      proxy %>% selectColumns(1:ncol(data()))
    })

    observeEvent(input$view, {
      data(get(input$obj_name, envir = .GlobalEnv)) # reload data
      s1 = input$table_1_columns_selected # indices of the selected columns
      s2 = input$table_1_rows_all # indices of rows on all pages (after the table is filtered by the search strings)
      if (is.null(s1))
        showNotification("At least one column should be selected.", duration=5, type = "error")
      # s3 = input$table_1_search # the global search string
      # s4 = input$table_1_search_columns # the vector of column search strings
      req(s1, s2)
      new_data = subset(data()[s2,], select = names(data())[s1])
      data(new_data) # update the value
    })


    # Page 2
    options(htmlwidgets.TOJSON_ARGS = NULL)
    observe({
      x <- input$obj_name
      updateSelectInput(session,"var",label = "Choose a variable to plot", choices = names(data()))
    })

    output$plot <- renderPlotly({
      new_data <- data.frame(table(subset(data(), select = input$var)))
      plot_ly(data = new_data, x = ~Var1, y = ~Freq,type = "bar",color = I("lightblue")) %>%
        layout(yaxis = list(title = 'Count', showline = T,showgrid = T),
               xaxis = list(title = input$var, showline = T, showgrid = F))
    })

    output$summary <- renderPrint({
      summary(data())
      # summary(CreateTableOne(data = data(),includeNA = T))
    })

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer(dialogName="RWE Lab Addins", width = 1000, height = 700)
  runGadget(ui, server, viewer = viewer)

}

# dataView()
