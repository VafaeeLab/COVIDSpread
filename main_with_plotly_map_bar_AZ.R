source("generateModel.R")
source("update_data_models.R")
source("setRegressors.R")
source("transformation.R")

# if(!require("qdapTools")) install.packages("qdapTools", repos = "http://cran.us.r-project.org")
# if(!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# # if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
# if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
# # if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
# # if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
# if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
# if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
# # if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
# if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

library(sparkline)
library(ggplot2)
library(ggiraph)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinythemes)

library(leaflet)
library(geojsonio)
library(DT)
library(shinycssloaders)
library(shinycustomloader)
library(shinyjs)
library(dashboardthemes)
library(matrixStats)
library(dplyr)
library(forecast)
library(data.table)
library(survival)
library(shinyBS)

library(coronavirus)
library(tidyr)

options(htmlwidgets.TOJSON_ARGS = NULL)

# Read ECDC or JHU data and models ----------------------------------------------------
# update_data_model() # should not be read from here but from a scheduler or manually on daily basis
ecdc <-
  read.csv("Data/jhu.csv",
           row.names = 1,
           fileEncoding = "UTF-8") #ECDC or JHU data, here JHU is used
allModels <- readRDS("Data/allModels.rds")

countries <-
  unique(as.character(colnames(ecdc))) %>% gsub("_", " ", ., fixed = T)


# Load 10 days prediction data -------
all.pred.dat <-
  fread("Data/all_prediction_next_days.csv", encoding = "UTF-8") %>% as.data.frame()
deCummuliateData <- function(aVec) {
  retVec = c()
  for (i in length(aVec):2) {
    retVec = c(retVec, aVec[i] - aVec[i - 1])
  }
  return(rev(retVec))
}

all.pred.dat[, 1] <-
  gsub("_", " ", all.pred.dat[, 1], fixed = T) # replace "_" with <space> in country names
colnames(all.pred.dat) <-
  c(
    "Countries",
    "day1",
    "day2",
    "day3",
    "day4",
    "day5",
    "day6",
    "day7",
    "day8",
    "day9",
    "day10"
  )
# print(all.pred.dat[which(all.pred.dat$Countries == "Bangladesh"),])
# Read map data and project basemap ----------------
jhu_worldcountry = geojson_read("Data/countries.geo.json", what = "sp")
plot_map <-
  jhu_worldcountry[jhu_worldcountry$name %in% as.character(all.pred.dat[, 1]),]

# print(setdiff(as.character(all.pred.dat[,1]), jhu_worldcountry$name)) # see for which country there's no json data

basemap <-
  leaflet(jhu_worldcountry, options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addTiles() %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Next 1 day", "Next 5 day", "Next 10 day"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  hideGroup(c("Next 1 day", "Next 5 day", "Next 10 day"))  %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  
  # addProviderTiles("https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png") %>%
  
  fitBounds(0, -25, 90, 65) # alternative coordinates for closer zoom


# for adding chart column in DT ------
js <-
  "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
colDefs2 <- list(
  list(targets = c(3), render = JS(js)),
  list(targets = c(1:3), className = 'dt-center'),
  list(targets = c(1), visible = F),
  # Unlike R, these columns follows 0-based indexing
  list(targets = c(3), orderable = F)
)

bar_string <- "type: 'bar',
              barColor: 'orange',
              height: 40,
              width: 80,
              negBarColor: 'purple',
              highlightColor: 'red',
              zeroAxis: true,
              barWidth: 8"
cb_bar <-
  JS(
    paste0(
      "function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ",
      bar_string,
      " }); }"
    ),
    collapse = ""
  )


# UI -------
ui <- fluidPage(
  title = "COVID-19 TS",
  theme = shinytheme('darkly'),
  
  navbarPage(
    tags$style(
      HTML(
        "
        .navbar { background-color: #ffe600;}
        .navbar-default .navbar-nav > li > a {background-color: #ffe600;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {background-color: #ffe600;}
        .navbar-default .navbar-nav > li > a:hover {background-color: #ffe600;}
    "
      )
    ),
    
    
    tabPanel(
      # option-1
      # title = HTML("<div style=\" display:table; background-color: #ffe600;\"><img src=\"UNSW-logo2.png\",  style=\"height: 53.5px; width:124px; border-radius: 5px; \"></img></div>"),
      
      # option-2
      title = HTML(
        "<div style=\" text-align: center;  margin: 0px auto; padding: 5px;\">
                                    <div style=\" float: left;\">
                                      <img src=\"UNSW-logo2.png\",
                                        style=\"height: 53.5px;
                                              width:124px;
                                              border-radius: 5px; \">
                                      </img>
                                    </div>

                                    <div style=\" float: right;\">
                                      <h3 style=\" color: black; margin-top: 10px; padding-left: 50px;\">
                                        <b>Time-Series Modelling of COVID-19 Spread and Interventions, Prediction of Future Spread</b>
                                      </h3>
                                    </div>

                                    <div style =\" clear:both;\"></div>

                                 </div>
                                 "
      ),
      
      # option-3: still trying to justify
      # title = HTML("<div style=\" text-align: justify; width:100%;\">
      #                 <div style=\" float: right; width:50%;\">
      #                   <h3 style=\" color: black;\">
      #                     <b>Time-Series Modelling of COVID-19 Spread and Interventions, Prediction of Future Spread</b>
      #                   </h3>
      #                 </div>
      #
      #                 <div style=\"width:50%;\">
      #                   <img src=\"UNSW-logo2.png\",
      #                     style=\"height: 53.5px;
      #                           width:124px;
      #                           border-radius: 5px; \">
      #                   </img>
      #                 </div>
      #
      #                 <div style =\" clear:both;\"></div>
      #
      #              </div>
      #              "),
      # tags$style(type="text/css", "body {padding-top: 70px;}"),
      # tags$style(type="text/css", "body {font-family: Lato;}"),
      
      tags$style(
        HTML(
          "
                                      #first {
                                      border-radius: 5px; border: 1px solid #5c5c5c; padding-left: 15px; padding-right: 15px;
                                      }
                                      #sld {
                                          border-radius: 5px; border: 0px solid #ffe600; height: 500px; margin-top: 60px;
                                      }
                                      #spread {
                                          text-align: justify;
                                      }
                                    "
        )
      ),
      tags$br(),
      fluidRow(
        column(
          width = 3,
          style = 'border: 1px solid #ffe600; border-radius: 5px; height: 925px;',
          
          fluidRow(
            tags$script(
              "$(document).on('click', '#contents button', function () {
                                            Shiny.onInputChange('select_button',this.id);
                                            });"
            ),
            tags$br(),
            column(width = 12,
                   DT::dataTableOutput("contents") %>% withLoader(loader="loader4"))
          )
        ),
        # column(width = 1),
        column(
          width = 5,
          div(
            style = "padding-left: 15px",
            fluidRow(
              id = "plot_panel",
              
              column(
                width = 12,
                style = 'border: 1px solid #ffe600; border-radius: 5px;',
                fluidRow(tags$br(),
                         column(
                           width = 12,
                           plotlyOutput('lineplot', width = "100%") %>% withLoader(loader="loader4")
                         )),
                fluidRow(
                  # column(width = 3,
                  #                        airDatepickerInput(
                  #                          inputId = "intrv_past",
                  #                          label = "Add interventions:",
                  #                          placeholder = "Click to pick dates",
                  #                          multiple = T, clearButton = T, range = F, separator = ", ",
                  #                          todayButton = T, autoClose = T, update_on = 'change'
                  #                        ),
                  #                        bsTooltip("intrv_past", "Click for selecting Intervention Dates",
                  #                                  "bottom", options = list(container = "body")
                  #                                  )
                  #
                  #
                  # ),
                  
                  column(
                    width = 3,
                    numericInput(
                      "kstepforecast",
                      label = "Select forecasting days",
                      value = 10,
                      min = 2,
                      max = 100
                    ),
                    bsTooltip(
                      "kstepforecast",
                      "Select forecasting days",
                      "bottom",
                      options = list(container = "body")
                    )
                    
                    
                  ),
                  
                  # control panel -----------------
                  column(
                    width = 6,
                    absolutePanel(
                      fixed = F,
                      draggable = F,
                      prettyToggle(
                        inputId = "toggle1",
                        label_on = "Now, Predict again",
                        label_off = "Prediction period",
                        icon_on = icon("repeat", lib = "glyphicon"),
                        icon_off = icon("calendar", lib = "glyphicon"),
                        status_on = "primary",
                        status_off = "default",
                        plain = TRUE,
                        outline = TRUE,
                        bigger = TRUE,
                        inline = TRUE
                      )
                      
                    ),
                    fluidRow(
                      column(
                        width = 5,
                        actionButton("show_pred", "Predict") ,
                        bsTooltip(
                          "show_pred",
                          "Click for Predicting next days spread",
                          "bottom",
                          options = list(container = "body")
                        )
                      ),
                      column(
                        width = 7,
                        selectInput(
                          inputId = "sel_trans",
                          label = "Pick Transformation",
                          choices = c(
                            "Automated" = 1,
                            "Without Transformation" = 2,
                            "Ratio Transformation" = 3,
                            "Power Transformation" = 4,
                            "Logarithmic Transformation" = 5,
                            "Box-Cox Transformation" = 6
                          ),
                          selected = 1,
                          selectize = F,
                          multiple = F
                        )
                        
                      )
                    ),
                    
                    
                    
                    
                    conditionalPanel(
                      condition = "input.toggle1",
                      # tags$br(),
                      # tags$br(),
                      
                      absolutePanel(
                        fixed = F,
                        draggable = F,
                        class = "panel panel-info",
                        
                        sliderInput(
                          "DatesMerge",
                          "Dates:",
                          min = Sys.Date() -
                            10,
                          max = Sys.Date() +
                            10,
                          value = c(Sys.Date() -
                                      5, Sys.Date() + 5),
                          timeFormat =
                            "%Y-%m-%d",
                        ),
                        style = "opacity: 0.85; z-index: 10;" ## z-index modification
                      )
                    )
                    
                  ),
                  
                  #style="color: #fff; background-color: #000000; border-color: #000000")),
                  column(
                    width = 3,
                    
                    absolutePanel(
                      fixed = F,
                      draggable = F,
                      downloadBttn(
                        outputId = "download_prediction",
                        style = "float",
                        color = "warning",
                        size = 'xs',
                        label = 'Download',
                        block = T
                      )
                      
                    ),
                    
                    actionButton(inputId = "model_param", "Parameters"),
                    bsTooltip(
                      "model_param",
                      "Click for showing model parameters",
                      "bottom",
                      options = list(container = "body")
                    )
                    
                  ),
                  tags$br()
                  # -----------------
                ),
                tags$style(
                  type = 'text/css',
                  "#kstepforecast { background-color: #ffe600; color: #000000; }"
                ),
                tags$style(
                  type = 'text/css',
                  "#intrv_past { background-color: #ffe600; color: #000000; }"
                ),
                tags$style(
                  type = 'text/css',
                  "#show_pred { background-color: #ffe600; color: #000000; width:100%; margin-top: 25px;}"
                ),
                tags$style(
                  type = 'text/css',
                  "#sel_trans { background-color: #ffe600; color: #000000; width:100%;}"
                ),
                tags$style(
                  type = 'text/css',
                  "#model_param { background-color: #ffe600; color: #000000; width:100%; margin-top: 25px;}"
                )
                
              )
              
            ),
            tags$br(),
            
            # leaflet map -----------------
            div(style = "margin-top: -5px;",
                fluidRow(
                  id = "map_panel",
                  # tags$script(src="http://unpkg.com/leaflet@1.3.1/dist/leaflet.js"),
                  column(
                    width = 12,
                    style = 'border: 1px solid #ffe600; border-radius: 5px;',
                    # tags$script("leaflet-providers.js"),
                    
                    leafletOutput('mymap', width = '100%')
                  )
                  
                ))
            # -----------------
          )
        ),
        column(width = 4,
               div(
                 style = "padding-left: 15px;",
                 fluidRow(
                   id = "another_plot_panel",
                   column(
                     width = 12,
                     style = 'border: 1px solid #ffe600; border-radius: 5px;  height: 925px;',
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     plotlyOutput('anotherPlot', width = "100%", height = "800px") %>% withLoader(loader="loader4"),
                     tags$br(),
                     tags$br()
                   )
                 )
               ))
      )
    )
  ),
  
  tags$br(),
  useSweetAlert("borderless"),
  fixedPanel(
    actionBttn(
      inputId = "video_btn",
      label = "Video Help",
      icon = icon("play", lib = "font-awesome"),
      style = "float",
      color = "warning",
      size = 's'
    ),
    right = 20,
    bottom = 50
  ),
  fixedPanel(
    actionBttn(
      inputId = "FAQ_btn",
      label = "FAQ",
      icon = icon("question-circle", lib = "font-awesome"),
      style = "float",
      color = "warning",
      size = 's'
    ),
    right = 20,
    bottom = 10
  ),
  # bsModal(id="faqModal", "FAQ", trigger = "FAQ_btn", size = 'large',
  #         htmlOutput("faq_html"), class = "modal-dialog modal-dialog-scrollable", role="document"),
  
  
  tags$footer(
    HTML(
      "
                    <!-- Footer -->
                           <footer class='page-footer' >
                           <!-- Copyright -->

                           <div style='color: #000000; background-color: #ffe600;
                                padding-left: 20px; ' class='footer-copyright text-left'>
                                 </br><b>Developed by: </b>
                                 Siroos Shahriari,  Taha Hossein Rashidi, AKM Azad and Fatemeh Vafaee </br>
                                 <b>Correspondance to: </b>
                                 <a href='mailto:rashidi@unsw.edu.au' style='color:red'> <i>rashidi@unsw.edu.au</i> </a>, and
                                <a href='mailto:f.vafaee@unsw.edu.au' style='color:red'> <i>f.vafaee@unsw.edu.au</i> </a> </br></br>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"
    )
  )
)

# Server ------
server <- function(input, output, session) {
  # Reactive variables ------------
  interventions.past <- reactiveValues(Value = NULL)
  country <- reactiveValues(Value = 'Australia')
  output.df <- reactiveValues(Value = NULL)
  output.Pred.df <- reactiveValues(Value = NULL)
  model.params <- reactiveValues(Value = NULL)
  isFail <- reactiveVal(F)
  
  
  allStartDay <-
    tail(rownames(ecdc), 1) %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                           "%Y-%m-%d")
  allEndDay <-
    head(rownames(ecdc), 1) %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                           "%Y-%m-%d")
  
  updateAirDateInput(
    session,
    inputId = "intrv_past",
    options = list (
      minDate = as.character(allStartDay),
      maxDate = as.character(allEndDay + 9)
    )
  )
  updateSliderInput(
    session,
    inputId = "DatesMerge",
    min =  allStartDay,
    max =  allEndDay,
    value =  c(allStartDay, allEndDay)
  )
  
  
  # fixing country display names ----
  displyCountries <- countries
  
  if (length(which(displyCountries == "United Arab Emirates")) != 0) {
    displyCountries[which(displyCountries == "United Arab Emirates")] <-
      "UAE"
  }
  if (length(which(displyCountries == "United States of America")) != 0) {
    displyCountries[which(displyCountries == "United States of America")] <-
      "USA"
  }
  if (length(which(displyCountries == "United Kingdom")) != 0) {
    displyCountries[which(displyCountries == "United Kingdom")] <- "UK"
  }
  if (length(which(displyCountries == "Democratic Republic of the Congo")) != 0) {
    displyCountries[which(displyCountries == "Democratic Republic of the Congo")] <-
      "Demc. Rep. of Congo"
  }
  
  # Generating datatable for left-panel ----------
  re_df <- data.frame(
    Country = displyCountries,
    # display countries are "_" free and accronymed
    id = colnames(ecdc),
    Action = paste0(
      '
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary view"
             style=\" background-color: #ffe600; color: #000000; border-radius: 15px;\"
             id=',
      colnames(ecdc),
      '>Select</button>
             </div>
             '
    ),
    # but button ID have country names with both "_" and full
    stringsAsFactors = FALSE
  )
  newDat <- ecdc[11:1, ] %>% apply(2, FUN = deCummuliateData) %>% t()
  newDat <- data.frame(
    Country = colnames(ecdc),
    total = apply(newDat, 1, paste, collapse = ",")
    ,
    stringsAsFactors = F
  )
  re_df <-
    dplyr::inner_join(re_df, newDat, by = c("id" = "Country")) %>% as.data.frame()
  
  # render datatable of country list - with barplot (last 10days) --------
  callback <- c(
    "$('#DataTables_Table_0_filter input').css('background-color', 'yellow');",
    "$('#DataTables_Table_0_filter label').css('color', 'white');",
    "$('#DataTables_Table_0_info').css('color','white')"
  )
  
  output$contents <- DT::renderDataTable({
    dt <-
      DT::datatable(
        re_df,
        selection = 'none',
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Scroller',
        colnames = c("Country", "ID", "Action", "Daily New Cases"),
        callback = JS(callback),
        options = list(
          deferRender = TRUE,
          scrollY = 800,
          scroller = TRUE,
          autoWidth = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#222222', 'color': 'white'});",
            "}"
          ),
          columnDefs = colDefs2,
          fnDrawCallback = cb_bar
        )
      ) %>%
      formatStyle(c(1, 2, 3, 4), backgroundColor = '#222222', color = "white")
    dt$dependencies <-
      append(dt$dependencies,
             htmlwidgets:::getDependency("sparkline"))
    dt
  })
  
  # Search country as you type
  observeEvent(input$page, {
    dataTableProxy("contents") %>% selectPage(input$page)
  })
  
  
  # on selecting a country ------
  observeEvent(input$select_button, {
    # clearing reusable reactive values -----
    output.Pred.df$Value <- NULL
    updateAirDateInput(session, inputId = "intrv_past", clear = T)
    # updateSliderInput(session, inputId = "DatesMerge", min = NULL, max = NULL, value = c(Sys.Date()-5,Sys.Date() + 5))
    model.params$Value <- NULL
    country$Value <<- input$select_button
    
    # model operationing ----------
    output <- allModels[[which(colnames(ecdc) == country$Value)]]
    
    coef <- output$model$coef
    se <- sqrt(diag(output$model$var.coef))
    model.coef = paste0(sapply(
      names(coef),
      FUN =
        function(x)
          paste(
            trimws(toupper(x)),
            ": ",
            round(coef[x], 3),
            " (",
            round(se[x], 2),
            ")" ,
            sep = ""
          )
    ), collapse = ", ")
    
    model.params$Value <- paste0(
      "Transformation: <i>" ,
      output$name,
      "</i></br>",
      "Model: <i>",
      output$model,
      "</i></br>",
      "Coefficients:</br>",
      "<i>",
      model.coef ,
      "</i></br>",
      "RMSE: <i>",
      round(as.numeric(output$RMSE), 2) ,
      "</i>"
    )
    
    
    print(as.character(output$model))
    
    output.df$Value <-
      output$data %>% as.data.frame() %>% cbind(rownames(.), .)
    colnames(output.df$Value) <- c("date", "count")
    output.df$Value$date <-
      output.df$Value$date %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                          "%Y-%m-%d")
    output.df$Value$count <- as.numeric(output.df$Value$count)
    output.df$country = country$Value
    
    allStartDay <-
      head(output.df$Value$date, 1) %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                                   "%Y-%m-%d")
    allEndDay <-
      tail(output.df$Value$date, 1) %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                                   "%Y-%m-%d")
    
    updateAirDateInput(
      session,
      inputId = "intrv_past",
      options = list (
        minDate = as.character(allStartDay),
        maxDate = as.character(allEndDay + 9)
      )
    )
    updateSliderInput(
      session,
      inputId = "DatesMerge",
      min =  allStartDay,
      max =  allEndDay,
      value =  c(allStartDay, allEndDay)
    )
    # ----
    
  })
  
  observe({
    isolate({
      output <- allModels[[which(colnames(ecdc) == country$Value)]]
      
      coef <- output$model$coef
      se <- sqrt(diag(output$model$var.coef))
      model.coef = paste0(sapply(
        names(coef),
        FUN =
          function(x)
            paste(
              trimws(toupper(x)),
              ": ",
              round(coef[x], 3),
              " (",
              round(se[x], 2),
              ")" ,
              sep = ""
            )
      ), collapse = ", ")
      
      model.params$Value <- paste0(
        "Transformation: <i>" ,
        output$name,
        "</i></br>",
        "Model: <i>",
        output$model,
        "</i></br>",
        "Coefficients:</br>",
        "<i>",
        model.coef ,
        "</i></br>",
        "RMSE: <i>",
        round(as.numeric(output$RMSE), 2) ,
        "</i>"
      )
      
      print(paste0("within onload ", as.character(output$model)))
      
      output.df$Value <<-
        output$data %>% as.data.frame() %>% cbind(rownames(.), .)
      colnames(output.df$Value) <<- c("date", "count")
      output.df$Value$date <<-
        output.df$Value$date %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                            "%Y-%m-%d")
      output.df$Value$count <<- as.numeric(output.df$Value$count)
      output.df$country <<- country$Value
    })
    
    ## here: disable dates in datepicker that are outside (output.df$Value$date + 10 days ahead)
    
  })
  
  # plot left plot ------
  output$lineplot <- renderPlotly({
    if (!is.null(output.df$Value)) {
      currCountry = unique(as.character(output.df$country))
      figTitle = paste0("<b>Total confirmed cases in: ",
                        gsub("_", " ", currCountry, fixed = T),
                        "</b>")
      
      fig <- plot_ly() %>%
        plotly::config(
          scrollZoom = T,
          toImageButtonOptions = list(
            format = "png",
            filename = "myplot",
            width = 600,
            height = 700,
            setBackground = 'rgba(0,0,0,0)'
            #           setBackground = tags$script("$function(gd){ gd._fullLayout.paper_bgcolor = 'rgba(0,0,0,0)';
            # gd._fullLayout.plot_bgcolor = 'rgba(0,0,0,0)';};")
            
          )
        )
      fig <-
        fig %>% add_markers(
          x = output.df$Value$date,
          y = output.df$Value$count,
          alpha = 0.9,
          color = I("red"),
          name = "observed"
        )
      
      if (!is.null(output.Pred.df$Value)) {
        plot.Pred.df <- output.Pred.df$Value
        figTitle = paste0(
          "<b>Total confirmed with predicted cases in: ",
          gsub("_", " ", currCountry, fixed = T),
          "</b>"
        )
        
        fig <-
          fig %>% add_ribbons(
            x = as.Date(plot.Pred.df$date, format = "%Y-%m-%d"),
            ymin = plot.Pred.df$lowBand_80,
            ymax = plot.Pred.df$upBand_80,
            color = I("gray80"),
            name = "80% confidence"
          )
        if (ncol(plot.Pred.df) > 5) {
          fig <-
            fig %>% add_ribbons(
              x = as.Date(plot.Pred.df$date, format = "%Y-%m-%d"),
              ymin = plot.Pred.df$lowBand_95,
              ymax = plot.Pred.df$upBand_95,
              color = I("gray95"),
              name = "95% confidence"
            )
        }
        fig <-
          fig %>% add_lines(
            x = as.Date(plot.Pred.df$date, format = "%Y-%m-%d"),
            y = ceiling(plot.Pred.df$count),
            color = I("blue"),
            name = "prediction"
          )
        
        if ((input$DatesMerge[1] > head(output.df$Value$date, 1)) &
            !isFail()) {
          fig <- fig %>%
            add_segments(
              x = as.Date(input$DatesMerge[1], format = "%Y-%m-%d"),
              y = 0,
              xend = as.Date(input$DatesMerge[1], format = "%Y-%m-%d"),
              yend = ceiling(output.df$Value$count[which(output.df$Value$date == input$DatesMerge[1])]),
              color = I("yellow"),
              showlegend = FALSE,
              inherit = T
            )
        }
        if ((input$DatesMerge[2] < tail(output.df$Value$date, 1)) &
            !isFail()) {
          fig <- fig %>%
            add_segments(
              x = as.Date(input$DatesMerge[2], format = "%Y-%m-%d"),
              y = 0,
              xend = as.Date(input$DatesMerge[2], format = "%Y-%m-%d"),
              yend = ceiling(output.df$Value$count[which(output.df$Value$date == input$DatesMerge[2])]),
              color = I("yellow"),
              showlegend = FALSE,
              inherit = T
            )
        }
        
      }
      
      # formatting the plot -----------------
      f <-
        list(family = "Lato, monospace",
             size = 18,
             color = "FFFFFF")
      x <-
        list(
          title = "",
          showline = T,
          zeroline = T,
          showgrid = F,
          titlefont = f,
          type = 'date',
          tickformat = "%d %B<br>%Y",
          tickfont = list(
            family = "Lato, monospace",
            size = 12,
            color = "FFFFFF"
          )
        )
      y <-
        list(
          title = "Cummulative data",
          zeroline = T,
          showgrid = F,
          #type="log",
          showline = T,
          titlefont = f,
          tickfont = list(
            family = "Lato, monospace",
            size = 12,
            color = "FFFFFF"
          )
        )
      l <- list(
        x = 0.1,
        y = 0.9,
        font = list(
          family = "Lato",
          size = 12,
          color = "#FFFFFF"
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "#5c5c5c",
        borderwidth = 1
      )
      fig <-
        fig %>% layout(
          title = list(
            text = figTitle,
            x = 0.1,
            y = 0.95,
            font = list(
              size = 18,
              color = '#ffe600',
              family = 'Lato, monospace'
            )
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = x,
          yaxis = y,
          legend = l
        )
      
      fig
    }
  })
  
  output$download_prediction <- downloadHandler(
    filename = function() {
      paste('prediction_', country$Value, "_", Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(output.Pred.df$Value, con, fileEncoding = "UTF-8")
    }
  )
  
  
  
  # plot right ploit ----------
  output$anotherPlot <- renderPlotly({
    # thresholds
    th_cases = 1000
    N = 10
    
    # here put plot data
    tdf <- ecdc[1, -1] %>% t() # today's data without the world
    topCountry <-
      tdf[order(tdf, decreasing = T), ][1:N] %>% names()    # top N = 10 countries
    top.df <- ecdc[, match(topCountry, colnames(ecdc))]
    
    # colnames(top.df) <- colnames(top.df) %>% gsub("_"," ",.,fixed = T)
    # colnames(top.df)[which(colnames(top.df) == "United States of America")] <- "USA"
    # colnames(top.df)[which(colnames(top.df) == "United Kingdom")] <- "UK"
    # colnames(top.df)[which(colnames(top.df) == "United Arab Emirates")] <- "UAE"
    
    figTitle = "<b>Top 10 countries with total cases and 10 day Forcasting</b>"
    cp = c(
      "#8c2e63",
      "#f75ef0",
      "#6676a1",
      "#b8b2f0",
      "#9c6ae4",
      "#4f28af",
      "#99def9",
      "#239eb3",
      "#115d52",
      "#75eab6"
    )
    fig <- plot_ly(colors = cp)
    
    # plot things ----
    daysSince_K_cases <- NULL
    for (i in 1:N) {
      temp = top.df[which(top.df[, i] >= th_cases), i]
      temp = temp - tail(temp, 1)
      nDays = 1
      while (nDays < length(temp)) {
        nDays = nDays + 1
        if (temp[nDays] == 0)
          break
      }
      # customizing only for china
      if (colnames(top.df)[i] == 'China')
        nDays = nDays - 5
      daysSince_K_cases <- c(daysSince_K_cases, nDays)
    }
    
    
    for (i in 1:N) {
      aCountry <- data.frame(date <- seq(daysSince_K_cases[i], 0,-1),
                             count <- top.df[1:(daysSince_K_cases[i] + 1), i],
                             stringsAsFactors = F)
      colnames(aCountry) <- c("date", "count")
      # marker options
      currCountry = colnames(top.df)[i]
      
      # prediction data ----------
      # beware of country renaming
      output <- allModels[[which(colnames(ecdc) == currCountry)]]
      pred.val <- output$meanp %>% fortify() %>% dplyr::select("y")
      pred.val <- pred.val[, 1] %>% as.numeric()
      startDay = aCountry$date[1] + 1
      endDay = startDay + 9
      future10Days = seq(startDay, endDay, by = 1)
      pred.df <- data.frame(date = future10Days,
                            count = pred.val,
                            stringsAsFactors = F)
      # ------------
      
      currCountry <- currCountry %>% gsub("_", " ", ., fixed = T)
      currCountry <-
        if (currCountry == "United States of America")
          "USA"
      else
        currCountry
      currCountry <-
        if (currCountry == "United Kingdom")
          "UK"
      else
        currCountry
      currCountry <-
        if (currCountry == "United Arab Emirates")
          "UAE"
      else
        currCountry
      currCountry <-
        if (currCountry == "Democratic Republic of the Congo")
          "Demc. Rep. of Congo"
      else
        currCountry
      
      marker.Ops <- list(
        xref = 'paper',
        yref = 'paper',
        x = aCountry$date[1],
        y = aCountry$count[1],
        xanchor = 'left',
        yanchor = 'bottom',
        text = currCountry,
        
        font = list(
          family = 'Lato',
          color = cp[i],
          size = 10
        ),
        showarrow = F
      )
      fig <- fig %>%
        add_trace(
          x = aCountry$date,
          y = aCountry$count,
          type = "scatter",
          mode = "lines",
          hoverinfo = 'y',
          line = list(width = 1, color = cp[i]),
          name = currCountry
        )  %>%
        add_trace(
          x = aCountry$date[1],
          y = aCountry$count[1],
          type = 'scatter',
          mode = 'markers',
          name = currCountry,
          marker = list(size = 4, color = cp[i])
        ) %>%
        add_markers(
          x = pred.df$date,
          y = ceiling(pred.df$count),
          type = 'scatter',
          mode = 'markers+text',
          marker = list(size = 2, color = cp[i]),
          name = currCountry
        ) %>%
        
        layout(showlegend = F) %>%
        
        add_text(
          x = if (currCountry == "China")
            tail(pred.df$date, 1) - 5
          else
            tail(pred.df$date, 1) + 1 ,
          y = if (currCountry == "China")
            tail(pred.df$count, 1) - 10
          else
            tail(pred.df$count, 1),
          text = currCountry,
          textposition = if (currCountry == "China")
            'top'
          else
            'right',
          color = cp[i]
        )
    }
    
    
    # formatting the plot -----------------
    f <-
      list(family = "Lato, monospace",
           size = 18,
           color = "FFFFFF")
    x <-
      list(
        title = paste0("Days since ", th_cases, " cases"),
        showline = T,
        zeroline = T,
        showgrid = F,
        titlefont = f,
        autotick = T,
        tickfont = list(
          family = "Lato, monospace",
          size = 12,
          color = "FFFFFF"
        )
      )
    y <-
      list(
        title = "Cummulative data",
        zeroline = T,
        showgrid = F,
        type = 'log',
        showline = T,
        titlefont = f,
        tickfont = list(
          family = "Lato, monospace",
          size = 12,
          color = "FFFFFF"
        )
      )
    
    fig <-
      fig %>% layout(
        title = list(
          text = figTitle,
          x = 0.1,
          y = 1,
          xanchor = 'left',
          xref = "paper",
          yref = "paper",
          font = list(
            size = 18,
            color = '#ffe600',
            family = 'Lato, monospace'
          )
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        xaxis = x,
        yaxis = y
      )
    
    
    fig
    
  })
  
  observe({
    interventions.past$Value <<-
      as.character(input$intrv_past) #%>% gsub("-","_",.)
    
  })
  
  # prediction --------------------
  predictionOps <- function() {
    today = output.df$Value[nrow(output.df$Value), 1] %>% as.Date(format = "%Y-%m-%d") # the lastday in the data
    firstDay = output.df$Value[1, 1] %>% as.Date(format = "%Y-%m-%d")
    day.config = if (today == input$DatesMerge[2] &&
                     firstDay == input$DatesMerge[1])
      0
    else
      1
    # day.config = if(input$DatesMerge[1] > head(output.df$Value$date,1) |
    #                 input$DatesMerge[2] < tail(output.df$Value$date,1)) 1 else 0
    print(paste("Today", today, sep = ""))
    # print(paste("today ", today, "\\nfirst day ", firstDay, "\\nchange? ",day.config))
    
    if (is.null(interventions.past$Value)) {
      nIntervention.Past = c()
      nIntervention.Future = c()
      
    } else{
      tmp = interventions.past$Value %>% as.Date(format = "%Y-%m-%d")
      past_dates <- which(tmp <= input$DatesMerge[2])
      future_dates <- which(tmp > input$DatesMerge[2])
      
      nIntervention.Future = if (length(future_dates) == 0)
        c()
      else
        interventions.past$Value[future_dates] %>% gsub("-", "_", .)
      nIntervention.Past = if (length(past_dates) == 0)
        c()
      else
        interventions.past$Value[past_dates] %>% gsub("-", "_", .)
    }
    
    reg.config = if (length(nIntervention.Past) > 0)
      1
    else
      0
    
    isFail(F) # some flag for later use
    
    if (reg.config == 1 ||
        day.config == 1 || as.numeric(input$sel_trans) > 0) {
      output <-
        try(generateModel(
          country$Value,
          ecdc,
          r = reg.config,
          ms = as.numeric(input$sel_trans),
          interventions.past = nIntervention.Past,
          interventions.future = nIntervention.Future,
          day.range = input$DatesMerge,
          kstepRMSE = input$kstepforecast,
          kstepforecast = input$kstepforecast
        ),
        silent = TRUE)
      
      
      if (class(output) != "try-error") {
        coef <- output$model$coef
        se <- sqrt(diag(output$model$var.coef))
        model.coef = paste0(sapply(
          names(coef),
          FUN =
            function(x)
              paste(
                trimws(toupper(x)),
                ": ",
                round(coef[x], 3),
                " (",
                round(se[x], 2),
                ")" ,
                sep = ""
              )
        ), collapse = ", ")
        
        model.params$Value <- paste0(
          "Transformation: <i>" ,
          output$name,
          "</i></br>",
          "Model: <i>",
          output$model,
          "</i></br>",
          "Coefficients:</br>",
          "<i>",
          model.coef ,
          "</i></br>",
          "RMSE: <i>",
          round(as.numeric(output$RMSE), 2) ,
          "</i>"
        )
        
      }
      
      
      if (class(output) == "try-error") {
        print(paste0("error: "))
        print(paste0(output))
        message("error")
        message(output)
        
        output <- allModels[[which(colnames(ecdc) == country$Value)]]
        coef <- output$model$coef
        se <- sqrt(diag(output$model$var.coef))
        model.coef = paste0(sapply(
          names(coef),
          FUN =
            function(x)
              paste(
                trimws(toupper(x)),
                ": ",
                round(coef[x], 3),
                " (",
                round(se[x], 2),
                ")" ,
                sep = ""
              )
        ), collapse = ", ")
        
        model.params$Value <- paste0(
          "Transformation: <i>" ,
          output$name,
          "</i></br>",
          "Model: <i>",
          output$model,
          "</i></br>",
          "Coefficients:</br>",
          "<i>",
          model.coef ,
          "</i></br>",
          "RMSE: <i>",
          round(as.numeric(output$RMSE), 2) ,
          "</i>"
        )
        # restore the transformation dropdown
        updateSelectInput(session = session,
                          inputId = "sel_trans",
                          selected = 1)
        
        
        isFail(T)
        
        
        
        if (reg.config == 1)
          showNotification(
            "Error adding intervetions: Plot showing data without intervention",
            type = "error",
            duration = 5,
            closeButton = T
          )
        else if (day.config == 1) {
          showNotification(
            "Error adding range prediction: Plot showing data without the date range",
            type = "error",
            duration = 5,
            closeButton = T
          )
        }
      }
    }
    else{
      # when no intervention, no prediction, i.e. reg.config = 0 && day.config == 0
      output <- allModels[[which(colnames(ecdc) == country$Value)]]
      #print("line 737")
      #print(output$model$coef)
      #model.coef = paste0(round(output$model$coef, 2), collapse = ", ")
      coef <- output$model$coef
      se <- sqrt(diag(output$model$var.coef))
      model.coef = paste0(sapply(
        names(coef),
        FUN =
          function(x)
            paste(
              trimws(toupper(x)),
              ": ",
              round(coef[x], 3),
              " (",
              round(se[x], 2),
              ")" ,
              sep = ""
            )
      ), collapse = ", ")
      
      model.params$Value <- paste0(
        "Transformation: <i>" ,
        output$name,
        "</i></br>",
        "Model: <i>",
        output$model,
        "</i></br>",
        "Coefficients:</br>",
        "<i>",
        model.coef ,
        "</i></br>",
        "RMSE: <i>",
        round(as.numeric(output$RMSE), 2) ,
        "</i>"
      )
      
      # restore the transformation dropdown
      updateSelectInput(session = session,
                        inputId = "sel_trans",
                        selected = 1)
    }
    
    print(paste0("within pred-event", as.character(output$model)))
    
    # trying [END] ----------------------------------------
    
    
    pred.val <- output$meanp %>% fortify() %>% dplyr::select("y")
    pred.val <- pred.val[, 1]
    
    if (day.config == 1 & !isFail())
      startDay = input$DatesMerge[2] %>% as.Date(format = "%Y-%m-%d") + 1 # the lastday in the data
    else
      startDay = output.df$Value[nrow(output.df$Value), 1] %>% as.Date(format =
                                                                         "%Y-%m-%d") + 1 # the lastday in the data
    # input$kstepforecast contains the number of days user wants the forecasting
    endDay = startDay + (input$kstepforecast - 1)
    
    # print(paste0("reg.config: ", reg.config))
    # print(paste0("day.config: ", day.config))
    
    future10Days = as.character(seq(startDay, endDay, by = "day"))
    
    
    if (is.null(dim(output$meanu))) {
      pred.up_80 <-
        output$meanu %>% fortify() %>% dplyr::select("y") # meanu[,1]: 80%, meanu[,2]: 95%
      pred.up_80 <- pred.up_80[, 1]
      pred.low_80 <-
        output$meanl %>% fortify() %>% dplyr::select("y")
      pred.low_80 <- pred.low_80[, 1]
      
      
      output.Pred.df$Value = data.frame(
        date = future10Days,
        count = pred.val,
        upBand_80 = pred.up_80,
        lowBand_80 = pred.low_80,
        stringsAsFactors = F
      )
    }
    else{
      pred.up_80 <-
        output$meanu[, 1] %>% fortify() %>% dplyr::select("y") # meanu[,1]: 80%, meanu[,2]: 95%
      pred.up_80 <- pred.up_80[, 1]
      pred.low_80 <-
        output$meanl[, 1] %>% fortify() %>% dplyr::select("y")
      pred.low_80 <- pred.low_80[, 1]
      pred.up_95 <-
        output$meanu[, 2] %>% fortify() %>% dplyr::select("y") # meanu[,1]: 80%, meanu[,2]: 95%
      pred.up_95 <- pred.up_95[, 1]
      pred.low_95 <-
        output$meanl[, 2] %>% fortify() %>% dplyr::select("y")
      pred.low_95 <- pred.low_95[, 1]
      
      
      output.Pred.df$Value = data.frame(
        date = future10Days,
        count = pred.val,
        upBand_80 = pred.up_80,
        lowBand_80 = pred.low_80,
        upBand_95 = pred.up_95,
        lowBand_95 = pred.low_95,
        stringsAsFactors = F
      )
    }
    output.Pred.df$Value$date <-
      output.Pred.df$Value$date %>% gsub("_", "-", ., fixed = T) %>% as.Date(format =
                                                                               "%Y-%m-%d")
    output.Pred.df$Value$count <-
      as.numeric(output.Pred.df$Value$count)
    print("line 886")
  }
  
  observeEvent(input$show_pred, {
    predictionOps()
  })
  
  
  
  output$mymap <- renderLeaflet({
    all.pred.dat = all.pred.dat[-which(
      all.pred.dat$Countries %in% setdiff(all.pred.dat$Countries, jhu_worldcountry$name)
    ), ]
    countryNames = as.character(all.pred.dat$Countries)
    
    # pal <- colorNumeric("YlOrRd", base::range(all.pred.dat[,-1]))
    # bins = c(0,0.05,0.1,0.5,1,2,5,7,10,15)
    # pal <- colorBin("YlOrRd", domain = base::range(all.pred.dat[,-1]), bins = bins)
    tmp <- as.data.frame(apply(all.pred.dat[, -1], 2, as.numeric))
    bins = c(1, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18, Inf)
    pal <-
      colorBin("YlOrRd",
               domain = c(0,base::max(tmp[,-1])),
               bins = bins, right = T)
    bins = c(1000, 1e4,5e4,1e5,5e5,1e6,5e6,Inf)
    pal <- colorBin("YlOrRd", domain = base::range(tmp*1e5), bins = bins)
    basemap%>%
      clearShapes() %>%
      addPolygons(data = jhu_worldcountry[match(countryNames,jhu_worldcountry$name )  ,], # next 1 day
                  stroke = T, weight = 0.5, color = "#a8b7b9",
                  opacity = 0.5,
                  group = "Next 1 day",
                  smoothFactor = 0.1,
                  fillOpacity = 0.4,
                  fillColor = ~pal(tmp$day1*1e5),
                  # fillColor = ~colorBin("YlOrRd", domain = base::range(as.numeric(all.pred.dat$day1)), bins = bins),
                  
                  highlightOptions = highlightOptions(color = "yellow", weight = 1,
                                                      bringToFront = TRUE),
                  
                  label = sprintf(
                    "<strong>%s</strong><br/>%g cases</sup>",
                    all.pred.dat$Countries, round(tmp$day1*1e5,2)
                  ) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "color" = "red",
                      "font-family" = "Lato",
                      "font-style" = "italic",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "font-size" = "12px",
                      "border-color" = "rgba(0,0,0,0.5)",
                      "font-weight" = "normal",  padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      
      addPolygons(data = jhu_worldcountry[match(countryNames,jhu_worldcountry$name ) ,], # next 5 day
                  stroke = T, weight = 0.5, color = "#a8b7b9",
                  opacity = 0.5,
                  group = "Next 5 day",
                  smoothFactor = 0.1,
                  fillOpacity = 0.4,
                  fillColor = ~pal(tmp$day5*1e5),
                  # fillColor = ~colorBin("YlOrRd", domain = base::range(as.numeric(all.pred.dat$day5)), bins = bins),
                  
                  highlightOptions = highlightOptions(color = "yellow", weight = 1,
                                                      bringToFront = TRUE),
                  
                  label = sprintf(
                    "<strong>%s</strong><br/>%g cases</sup>",
                    all.pred.dat$Countries, round(tmp$day5*1e5,2)
                  ) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "color" = "red",
                      "font-family" = "Lato",
                      "font-style" = "italic",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "font-size" = "12px",
                      "border-color" = "rgba(0,0,0,0.5)",
                      "font-weight" = "normal",  padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      
      addPolygons(data = jhu_worldcountry[match(countryNames,jhu_worldcountry$name ) ,], # next 10 day
                  stroke = T, weight = 0.5, color = "#a8b7b9",
                  opacity = 0.5,
                  group = "Next 10 day",
                  smoothFactor = 0.1,
                  fillOpacity = 0.4,
                  fillColor = ~pal(tmp$day10*1e5),
                  # fillColor = ~colorBin("YlOrRd", domain = base::range(as.numeric(all.pred.dat$day10)), bins = bins),
                  highlightOptions = highlightOptions(color = "yellow", weight = 1,
                                                      bringToFront = TRUE),
                  label = sprintf(
                    "<strong>%s</strong><br/>%g cases</sup>",
                    all.pred.dat$Countries, round(tmp$day10*1e5,2)
                  ) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      "color" = "red",
                      "font-family" = "Lato",
                      "font-style" = "italic",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "font-size" = "12px",
                      "border-color" = "rgba(0,0,0,0.5)",
                      "font-weight" = "normal",  padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      
      addLegend(pal = pal,  position = "bottomleft",
                values = base::range(tmp[,-1]*1e5), 
                opacity = 1.0, na.label = "No Data",
                # labFormat = labelFormat(
                #   # transform = function(x) if (x >= 1e4) format(x, format = 'e', digits = 2) else x
                #   transform = format(scientific = T)
                # ),
                title = "<small>Predicted cases</small>"
      ) %>%
      
      
      addEasyButton(
        easyButton(
          icon = "fa-globe",
          title = "Zoom to Level 1",
          position = "topright",
          onClick = JS("function(btn, map){ map.setZoom(1); }")
        )
      ) %>%
      addEasyButton(
        easyButton(
          icon = "fa-crosshairs",
          title = "Locate Me",
          position = "topright",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")
        )
      )
    
  })
  
  observeEvent(input$model_param, {
    #print(country$Value)
    #print(model.params$Value)
    #
    showModal(
      modalDialog(
        title =  country$Value %>% gsub("_", " ", ., fixed = T),
        easyClose = T,
        fade = T,
        footer = modalButton("Close"),
        HTML(model.params$Value),
        size = "s"
      )
    )
    
    # addPopover(session, id = "model_param", "Model Parameters", placement = "bottom", content = model.params$Value , trigger = 'click', options = NULL)
  })
  
  observeEvent(input$FAQ_btn, {
    showModal(
      modalDialog(
        title =  "Frequently Asked Questions",
        easyClose = T,
        fade = T,
        footer = modalButton("Close"),
        
        HTML(
          paste0(
            "<div style='font-family: Lato; font-size: 14px;'>",
            includeMarkdown("Data/FAQ.Rmd"),
            "</div>"
          )
        ),
        # HTML(includeMarkdown("Data/FAQ.Rmd")),
        
        size = "l"
      )
    )
    
    
  })
  
  observeEvent(input$video_btn, {
    showModal(
      modalDialog(
        title =  "How to work with this dashboard?",
        easyClose = T,
        fade = T,
        footer = modalButton("Close"),
        
        # HTML('<div style = "align: center;"><iframe width="600" height="355" src="https://www.youtube.com/embed/m9frLiLVUHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div>'),
        HTML(
          '<div style = "align: center;">
                                    <iframe width="560" height="315"
                                            src="https://www.youtube.com/embed/RqvntYZqwTI"
                                                  frameborder="0"
                                                  allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
                                                  allowfullscreen></iframe></div>'
        ),
        
        # HTML(includeMarkdown("Data/FAQ.Rmd")),
        
        size = "m"
      )
    )
    
    
  })
  
  
}

shinyApp(ui, server)

# # deployment issue:
# library(BiocManager)
# options(repos = BiocManager::repositories())
