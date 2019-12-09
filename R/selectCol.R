#' Select Variables
#' @name selectCol
#' @description Select and reorder columns of data frames
#' @return none
#' @import shiny
#' @import miniUI
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#' @import dplyr
#' @import shinyjqui


selectCol <- function() {

  datalist <- tryCatch({
    ls(envir = .GlobalEnv)[unlist(lapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv) , is.data.frame))]},
    error = function(e) stop("There are no data frames in the global environment!"))

  ## Gadget UI
  ui <- miniUI::miniPage(theme = "spacelab",
                 gadgetTitleBar(title = "Select Variables",
                                left = miniTitleBarCancelButton(),
                                right = miniTitleBarButton("done", "Done", primary = TRUE)),
                 ## Page 1
                 sidebarLayout(
                   sidebarPanel(style = "overflow-y:scroll; max-height: 650px; position:relative;",
                                tags$h4("Select data frame"),
                                selectizeInput(inputId = "obj_name", label = NULL, choices = datalist),
                                hr(),
                                h4("Manage data columns"),
                                tags$p("Tips: Click checkbox to select the column, the choices are also sortable by drag and drop.",style="color:gray"),
                                # textInput(inputId = "search",label = NULL, placeholder = "Search..."),
                                shinyjqui::sortableCheckboxGroupInput(inputId = 'var_choose', label = "Select and reorder", choices = NULL,selected = NULL),
                                actionButton(inputId = "var_reset",label = "Reset All"),
                                actionButton(inputId = "update",label = "Update",class = "btn-primary"),

                   ),
                   mainPanel(style = "overflow-y:scroll; max-height: 650px; position:relative;",
                             h3("Data Table"),br(),
                             DT::DTOutput("table_1", width = "95%", height = "auto"),
                             h3(icon("code"),"Code"),
                             htmlOutput("code"),
                   ),
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

    observe({
      choices_list <- names(data())
      updateCheckboxGroupInput(session, "var_choose", choices = choices_list, selected = choices_list)
      updateSelectInput(session, inputId = "var_name", choices = choices_list)
      updateTextInput(session, inputId = "var_rename", placeholder = input$var_name)
    })

    options(DT.TOJSON_ARGS = list(na = 'string'))
    output$table_1 <- DT::renderDT(datatable(data(),
                                                 class = 'display compact nowrap',
                                                 filter = list(position = 'top', plain = TRUE,clear = FALSE),
                                                 extensions = c('Scroller','FixedColumns','Buttons'), # ColReorder
                                                 selection = "none",
                                                 options = list(
                                                   dom  = 'Bfti',
                                                   buttons = list('copy','csv'),
                                                   scroller = TRUE, scrollX = TRUE, scrollY = 280,
                                                   searchHighlight = T, fixedColumns = T,
                                                   search = list(regex = TRUE, caseInsensitive = T),
                                                   columnDefs=list(list(className="dt-left",targets="_all"))
                                                 )))

    proxy_1 <- dataTableProxy('table_1')
    observe({replaceData(proxy_1, data(), resetPaging = T, clearSelection = "all")})


    observeEvent(input$var_select_all,{
      updateCheckboxGroupInput(session, "var_choose", selected = names(data()))
    })

    observeEvent(input$update,{
      if (length(input$var_choose) >= 1) {
        if (length(input$var_choose_order) == 0) {
          new_data = data() %>% dplyr::select(input$var_choose)
        } else{
          new_data = data() %>% dplyr::select(intersect(input$var_choose_order,input$var_choose))
        }
        data(new_data)
      } else {
        showNotification("At least one column should be selected.", type = "error")
      }
    })

    observeEvent(input$var_reset,{
      data(get(input$obj_name, envir = .GlobalEnv))
      updateCheckboxGroupInput(session, "var_choose", choices = names(data()), selected = names(data()))
    })

    output$code <- renderText({
      header <- HTML(sprintf("<p class='header'style='color:green;'>## Select and reorder columns from <tt>%s</tt>.</p>",
                              req(input$obj_name)))
      out <- HTML(sprintf("%s <- dplyr::select(%s, ", req(input$obj_name), req(input$obj_name)))
      col_names <- paste0(names(data()),collapse = ", ")
      out <- paste0(out, col_names,")")
      codeout <- paste0(header,"<pre class='r'><code class='r' id='code'>",out,"</code></pre>")
      codeout

    })


    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- dialogViewer(dialogName="RWE Lab Addins", width = 1000, height = 700)
  runGadget(ui, server, viewer = viewer)

}

# selectCol()

