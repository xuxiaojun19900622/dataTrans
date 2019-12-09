#' Factor Recorder
#' @name asFactor
#' @description Wrangling categorical data as factor
#' @return none
#' @import shiny
#' @import miniUI
#' @import shinyjqui
#' @import plotly
#' @import tableone

asFactor <- function() {

  datalist <- tryCatch({
    ls(envir = .GlobalEnv)[unlist(lapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv) , is.data.frame))]},
    error = function(e) stop("There are no data frames in the global environment!"))

  ui <- miniUI::miniPage(
    theme = "spacelab",
    gadgetTitleBar(title = "Encode Factor",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniTabstripPanel(
      # Page 1
      miniTabPanel(
        "Recoding Settings",
        icon = icon("sliders"),
        sidebarLayout(
          sidebarPanel(
            tags$h4("Select data frame"),
            selectizeInput(inputId = "obj_name", label = NULL, choices = datalist),
            selectInput(inputId = "var_name",label="Select variable to recode",choices= NULL),
            checkboxInput(inputId = "rename", label = "Rename variable", value = FALSE, width = NULL),
            conditionalPanel("input.rename",
                             textInput(inputId="var_rename",label="New variable name",value = ""),
            ),
          ),
          mainPanel(
            h3(icon("sliders"), "Modifying Factor Order and Levels"),
            wellPanel(id = "tPanel",
                      style = "overflow-y:scroll; height:530px; width:630px",
                      sortableTableOutput("table_1")),
          ),
        ),
      ),
      miniTabPanel(
        "Code and Summary",
        icon = icon("area-chart"),
        miniContentPanel(
          h4(icon("code"),"Code"),
          htmlOutput("code"),
          splitLayout(
            div(h4(icon("table"), "Summary"),
                # tags$p("Tips: Old variable as rows, new variable as columns.",style="color:gray"),
                verbatimTextOutput("summary")
                ),
            div(h4(icon("bar-chart-o"), "Plot"),
                plotlyOutput("plot",width = "90%",height = "350px"),
            )
          )

        ),
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
      updateSelectInput(session, inputId = "var_name", choices = names(data()))
    })

    rvar <- reactiveVal()
    observeEvent(input$var_name,{
      updateTextInput(session, inputId = "var_rename", value = paste0(input$var_name,"_rec"))
    })

    levs <- reactiveVal()
    output$table_1 <- renderUI({
      req(input$var_name)
      var_temp <- data()[names(data()) == input$var_name]
      levs(sort(unique(var_temp[[input$var_name]]), na.last = NA) )# missing values in the data are removed
      ## Generate fields
      out <- "<table><tbody>"
      for (l in levs()) {
        out <- paste0(out, '<tr>')
        out <- paste0(out,'<td style="vertical-align:baseline; text-align:left;"><span class="glyphicon glyphicon-move"></span> &ensp;</td>',
                      '<td style="vertical-align:baseline; text-align:left;">',htmltools::htmlEscape(l),' &ensp;</td>',

                      '<td style="vertical-align:baseline; text-align:left;"><span class="glyphicon glyphicon-arrow-right"></span> &ensp;</td>')
        label <- l
        l <- gsub(":", "_", l)
        id <- paste0("lev_", l)
        # if (id == "lev_NA") {label <- "NA"} ## If the level is NA
        ## If the level is an empty string
        if (id == "lev_") {label <- ""}
        out <- paste0(out,'<td style="vertical-align:baseline; text-align:left;">',textInput(inputId = id,label = NULL,value = label),'</td>')
        out <- paste0(out,'</tr>')
      }
      out <- paste0(out, "</tbody></table>")
      HTML(out)
    })

    new_levels <- reactiveVal()
    new_labels <- reactiveVal()
    output$code <- renderText({
      # render levels ans labels
      levels <- levs()[input$table_1_row_index]
      new_levels(levels)
      labels <- NULL
      for (l in levs()) labels <- c(labels, input[[paste0("lev_", l)]])
      labels <- labels[input$table_1_row_index]
      new_labels(labels)
      # generate code
      header <- HTML(sprintf("<p class='header' style='color:green;'>## Encode variable <strong>%s$%s</strong> as a factor.</p>",
                             req(input$obj_name), req(input$var_name)))
      # levels <- paste0('"', new_levels,'"',collapse = ", ")
      out <- HTML(sprintf("%s$%s <- <strong>factor</strong>(%s$%s,
                          levels = c(<span style='color:blue;'>%s</span>),
                          labels = c(<span style='color:blue;'>%s</span>))",
                          req(input$obj_name), req(input$var_name), req(input$obj_name), req(input$var_name),
                          paste0('"', new_levels(),'"',collapse = ", "), paste0('"', new_labels(),'"',collapse = ", ")
                          )
      )
      codeout <- paste0(header,"<pre class='r'><code class='r' id='code'>",out,"</code></pre>")
      codeout
    })

    rvar <- reactiveVal()
    output$plot <- renderPlotly({
      # encode factor
      data_1 <- subset(data(), select = input$var_name)
      var_name <- input$var_name
      if (input$rename) {
        names(data_1)[1] <- input$var_rename
        var_name <- input$var_rename
      }
      data_1[[var_name]] <- factor(data_1[[var_name]], levels = new_levels(), labels = new_labels())
      rvar(data_1)
      plot_ly(data = data.frame(table(data_1)), x = ~data_1, y = ~Freq,type = "bar",color = I("lightblue")) %>%
        layout(yaxis = list(title = 'Count', showline = T,showgrid = T),
               xaxis = list(title = var_name, showline = T, showgrid = F))

    })

    output$summary <- renderPrint({
      # summary(rvar())
      print(CreateTableOne(data = rvar(), includeNA = T))
    })

    observeEvent(input$done, {
      stopApp()
    })
  }

  viewer <- dialogViewer(dialogName="RWE Lab Addins", width = 1000, height = 700)
  runGadget(ui, server, viewer = viewer)

}

# asFactor()
