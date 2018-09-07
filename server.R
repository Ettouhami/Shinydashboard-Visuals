### Libraries----------------------------------------------------------------------------------
library(shinydashboard)
library(xtable)
library(maps)
library(mapproj)
library(highcharter)
library(dplyr)
library(viridisLite)
library(treemap)
library(xts)
library(quantmod)
library(DT)
#library(dygraphs)
library(leaflet)
library(rgdal)
library(plotly)
library(rio)
library(anytime)
library(readxl)
library(shinyjs)


### Plotly Daily Spot Prices-------------------------------------------------------------------
data <- read_excel("Crude&Gold Prices.xlsx", sheet = 3)
#data <- read_excel("C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/Crude&Gold Prices.xlsx", sheet = 3) 

data <- data[-1,]

data <- data[,11:16]

data$Indexed <- as.Date(as.numeric(data$Indexed), origin = "1899-12-30")

l <- list(
  font= list(
    family = "sans-serif", 
    size = 12, 
    color = "#000"), 
  bgcolor = "#E2E2E2", 
  bordercolor = "#FFFFFF", 
  borderwidth = 2)


annotations <- list (
  list(x = "2017-11-20", y = 0.5, xref = "x", yref ="paper", showarrow = FALSE, 
       text = "Keystone pipeline leak", textangle = 270, font = list(size = 12))
)

shapes <- list(
  list(xref = "x", x0 ="2017-11-16", x1 = "2017-11-16", text = "none", 
       yref = "paper", y0 = 0, y1 =1, line = list(color = "rgb(192, 192, 192)", dash = "dot")
  )
)

### Leaflet Net Export-------------------------------------------------------------------------
#netexports_data <- read.csv("C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/NetExports.csv")
netexports_data <- read.csv("NetExports.csv")

### Getting the vector of string values for pop-up label on map
x <- netexports_data$TRADE_BALANCE
x <- round(x/1000000, digits=2)
x <- paste(x)
netexports_data <- cbind(x, netexports_data)

world_map1 <- readOGR(dsn=".", "TM_WORLD_BORDERS-0.3")
#world_map1 <- readOGR(dsn="C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard", "TM_WORLD_BORDERS-0.3")
drops <- c("FIPS","ISO3", "UN", "AREA", "POP2005", "REGION", "SUBREGION", "LON", "LAT")
xxx <- world_map1[,!(names(world_map1) %in% drops)]
newobj <- merge(xxx, netexports_data, by.x="ISO2", by.y="COUNTRY_OF_EXPORT_ISO_CODE")
drops <- c("DATA_YEAR","VALUE_M", "VALUE_X", "INDEX_STR", "NAME")
xxx <- newobj[,!(names(newobj) %in% drops)]
# head(xxx@data, 3)

pal <- colorBin(c("#FF3041", "#FEA89B","#F9FFA5", "#D3FDB1", "#3FFF90"),
                bins = c(0,1,2,3,4,5),
                domain = xxx$INDEX,
                na.color = "#ffffff"
)

colors_to_fill <-  pal(xxx$INDEX)



### Daily Spot Prices--------------------------------------------------------------------------
dat <- read.csv("DailyPrices.csv")
#dat <- read.csv("C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/DailyPrices.csv")
price_date <- dat$DATE
date_format <- as.Date(dat[,1], format = "%m/%d/%Y")
xts_wti <- xts(dat[,4], order.by=date_format) # convert to xts
xts_wcs <- xts(dat[,3], order.by=date_format)
xts_exchange <- xts(dat[,2], order.by=date_format)
colnames(xts_wti) <- paste0("WTI") # name the columns
colnames(xts_wcs) <- paste0("WCS")
colnames(xts_exchange) <- paste0("Exchange")

### Net Exports Map----------------------------------------------------------------------------

#netexports_data <- read.csv("NetExports.csv")
# thm1 <- 
#   hc_theme(
#     colors = c("#ff0f33", "#434348", "#1a6ecc"),
#     chart = list(
#       backgroundColor = "transparent",
#       style = list(fontFamily = "Source Sans Pro")
#     ),
#     xAxis = list(
#       gridLineWidth = 1
#     ),
#     yAxis = list(
#       offset = 0,
#       tickLength =  0,
#       gridLineWidth = 0,
#       minorGridLineWidth = 0,
#       labels = list(style = list(fontSize = "8px"), format = "h", useHTML = TRUE)  
#     )
#     
#   )
data("worldgeojson")



### Highchart Map Example----------------------------------------------------------------------
thm <-
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )

data("USArrests", package = "datasets")
data("usgeojson")

USArrests <- USArrests %>%
  mutate(state = rownames(.))

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()

b <- 4
colstops1 <- data.frame(
  q = 0:b/b,
  c = substring(viridis(n + 1), 0, 8)) %>%
  list_parse2()

### Aircraft Exports---------------------------------------------------------------------------
#aircraft <- read.csv("C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/Aircraft.csv")
aircraft <- read.csv("Aircraft.csv")
aircraft1 <- read.csv("Aircraft1.csv")
#aircraft1 <- read.csv("C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/Aircraft1.csv")
x2014 <- as.list(aircraft[2])
x2015 <- as.list(aircraft[3]) # for highcharts
x2016 <- as.list(aircraft[4])
x2017 <- as.list(aircraft[5])
x2018 <- as.list(aircraft[6])

getMonth <- 'function(d){
var monthNames=["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"];
return monthNames[d.getMonth()];
}'
getMonthDay <- 'function(d){
var monthNames=["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"];
date = new Date(d);
return monthNames[d.getMonth()];
}'

date_format_aircraft_2014 <- as.Date(aircraft1[,1], format = "%m/%d/%Y")
# date_format_aircraft_2015 <- as.Date(aircraft1[,3], format = "%m/%d/%Y")
# date_format_aircraft_2016 <- as.Date(aircraft1[,5], format = "%m/%d/%Y")
# date_format_aircraft_2017 <- as.Date(aircraft1[,7], format = "%m/%d/%Y")
# date_format_aircraft_2018 <- as.Date(aircraft1[,9], format = "%m/%d/%Y")

xts_2014 <- xts(aircraft1[,2], order.by=date_format_aircraft_2014) # convert to xts
xts_2015 <- xts(aircraft1[,4], order.by=date_format_aircraft_2014)
xts_2016 <- xts(aircraft1[,6], order.by=date_format_aircraft_2014)
xts_2017 <- xts(aircraft1[,8], order.by=date_format_aircraft_2014)
xts_2018 <- xts(aircraft1[,10], order.by = date_format_aircraft_2014)
shaded_region <- xts(c(2000000,2000000,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), order.by=date_format_aircraft_2014)

colnames(xts_2014) <- paste0("2014") 
colnames(xts_2015) <- paste0("2015")
colnames(xts_2016) <- paste0("2016") # name the columns
colnames(xts_2017) <- paste0("2017")
colnames(xts_2018) <- paste0("2018")
colnames(shaded_region) <- paste0("Current")

### Economic Dashboard-------------------------------------------------------------------------
has_data <- function(x) { sum(!is.na(x)) > 0}

economic_dashboard <- read.csv("EconomicDashboard.csv", header = FALSE)
TradeAggregates <- read.csv("TradeAggregates.csv", header = FALSE)
ExchangeRate <- read.csv("ExchangeRate.csv", header = FALSE)
BankForecast <- read.csv("BankForecast.csv", header = FALSE)

economic_dashboard <- economic_dashboard %>% select_if(has_data)
TradeAggregates <- TradeAggregates %>% select_if(has_data)
ExchangeRate <- ExchangeRate %>% select_if(has_data)
BankForecast <- BankForecast %>% select_if(has_data)

colnames(economic_dashboard) <- c("","January","February","m/m % change","Last month's % change")
colnames(TradeAggregates) <- c("","m/m %  change","y/y % change")
colnames(ExchangeRate) <- c("","January","February")
colnames(BankForecast) <- c("Bank","Forecast")

### Aluminum and Steel Pie Charts--------------------------------------------------------------
data_al<- data.frame(Countries=c("United States","Mexico", "Japan", "South Korea", "Other"), Amount = c(9195024939,406845589,321450007,292357519,160963760))
data_pie<- data.frame(Countries=c("United States","Mexico", "Other"), Amount = c(7199186371,498670441,239157223))

### Aluminum and Steel Area Charts-------------------------------------------------------------
alum_steel <- read.csv("AluminumSteel.csv")
year_range <- c(alum_steel$X)
a_to_world <- c(alum_steel$Aluminum.Exports.to.the.world)
a_to_us <- c(alum_steel$Aluminum.Exports.to.the.United.States)
s_to_world <- c(alum_steel$Steel.Exports.to.the.world)
s_to_us <- c(alum_steel$Steel.Exports.to.the.United.States)

### Aluminum Stream Graph----------------------------------------------------------------------
str_graph <- read.csv("Stream.csv")
stream_year <- c(str_graph$X)
stream_w <- c(str_graph$World)
stream_us <- c(str_graph$US)
stream_ca <- c(str_graph$Canada)
stream_ch <- c(str_graph$China)
stream_no <- c(str_graph$Norway)
stream_in <- c(str_graph$India)
stream_ru <- c(str_graph$Russia)
stream_o <- c(str_graph$Other)

### Plotly Aircraft Exports-------------------------------------------------------------------
#aircraft_p <- import('Aircraft.csv')
# aircraft_p <- import('C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/Aircraft.csv')
# 
# l <- list(
#   font= list(
#     family = "sans-serif", 
#     size = 12, 
#     color = "#000"), 
#   bgcolor = "#E2E2E2", 
#   bordercolor = "#FFFFFF", 
#   borderwidth = 2)
# 
# 
# 
# colnames(aircraft_p) <- aircraft_p[1,]
# 
# aircraft_p <- aircraft_p[-1,]
# colnames(aircraft_p)[1] <- c('Month')
# 
# aircraft_p$Month <- as.factor(aircraft_p$Month)

##############################################################################################################
library(digest) # digest() Create hash function digests for R objects

formName <- "2014-fall-basic-info"
resultsDir <- file.path("data", formName)
dir.create(resultsDir, recursive = TRUE, showWarnings = FALSE)

# names of the fields on the form we want to save
fieldNames <- c("firstName",
                "lastName",
                "question",
                "divnum",
                "email",
                "back",
                "osType"
)


# names of users that have admin power and can view all submitted responses
adminUsers <- c("staff", "admin")
##############################################################################################################


### Server-------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ### Button-----------------------------------------------------------------------------------
  onclick("ab1", runjs("window.open('https://news.google.com/?hl=en-CA&gl=CA&ceid=CA:en')"))
  onclick("ab2", tryCatch(file.choose(), error = function(e) ""))
  
  
  ### Plotly Aircraft Exports------------------------------------------------------------------
  output$plotlyaircraft <- renderPlotly({
    aircraft_p <- import('Aircraft_plotly.csv')
    #aircraft_p <- import('C:/Users/Mohammad/Documents/semester 8/Visualizations/DailyDashboard/Aircraft_plotly.csv')
    
    # l <- list(
    #   font= list(
    #     family = "sans-serif", 
    #     size = 12, 
    #     color = "#000"), 
    #   bgcolor = "#E2E2E2", 
    #   bordercolor = "#FFFFFF", 
    #   borderwidth = 2)
    l <- list(
      font= list(
        family = "sans-serif",
        size = 12,
        color = "#000"),
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 2,
      orientation = 'h',
      x= 0.3)
    
    colnames(aircraft_p) <- aircraft_p[1,]

    aircraft_p <- aircraft_p[-1,]
    colnames(aircraft_p)[1] <- c('Month')
    aircraft_p$Month <- as.factor(aircraft_p$Month)
    
    pp <- plot_ly(aircraft_p, x= ~Month, y = ~ `2014`  ,name = '2014',  type = 'scatter', mode= "lines+marker", line = list( width = 2, color = "#008000"), width = 1090, marker= list( symbol = 28, color = "#008000"))%>%
      
      add_trace( y = ~ `2015`, name = '2015', line = list( width = 2, color = "#F7A31C"), marker= list( symbol = 2, color = "#F7A31C"))%>%
      
      add_trace(y = ~ `2016`, name = "2016", line = list( width = 2, color = '#890282'), marker= list( symbol = 29,  color = '#890282')) %>%
      
      add_trace(y = ~ `2017`, name = '2017', line= list(width = 2, color = "#FFFB57"), marker= list( symbol = 19, color = "#FFFB57"))%>%
      
      add_trace(y = ~ `2018`, name = '2018', line= list(width =2, color = "#060BED"), marker= list( symbol = 20,  color = "#060BED")) %>%
      
      layout(hovermode = "compare" , legend = l, xaxis =
               list(categoryorder = 'trace', title = "", showgrid = FALSE), yaxis = list(title = "", side= "left"))
    
    pp
  })
  
  
  ### Plotly daily price ----------------------------------------------------------------------
  output$plotlyprice <- renderPlotly({
    pp <- plot_ly(data, x= ~Indexed, y = ~ WCS ,name = 'WCS',  type = 'scatter', mode= "lines", line = list( width = 2, color = "#9CE589" ))%>%
      
      add_trace( y = ~ WTI, name = 'WTI', line = list( width = 2, color = "#7FB1DC"))%>%
      
      add_trace(y = ~ `Exchange rate CAD/USD`, name = "CND/USD", line = list( width = 2, color = "414141")) %>%
      
      layout(hovermode = "compare" , annotations = annotations, shapes = shapes, legend = l, width = 1090, xaxis =
               list(title = "", rangeselector = list( buttons = list( 
                 list( count = 1, label = "1 Mo", step = "month", stepmode = "backward"), 
                 list( count = 3, label = "3 Mo", step = "month", stepmode = "backward"), 
                 list( count = 1, label = "1 Yr", step = "year", stepmode = "backward"), 
                 list( count = 1, label = "YTD", step = "year", stepmode = "todate"), 
                 list(step = "all"))), rangeslider = list(type = "date")), yaxis = list(title = ""))
    
    pp
  })
  
  
  ##############################################################################################################
  ##########################################
  ##### Admin panel#####
  
  # if logged in user is admin, show a table aggregating all the data
  # isAdmin <- reactive({
  #   !is.null(session$user) && session$user %in% adminUsers
  # })
  infoTable <- reactive({
    #if (!isAdmin()) return(NULL)
    
    ### This code chunk reads all submitted responses and will have to change
    ### based on where we store persistent data
    infoFiles <- list.files(resultsDir)
    allInfo <- lapply(infoFiles, function(x) {
      read.csv(file.path(resultsDir, x))
    })
    ### End of reading data
    
    #allInfo <- data.frame(rbind_all(allInfo)) # dplyr version
    #allInfo <- data.frame(rbindlist(allInfo)) # data.table version
    allInfo <- data.frame(do.call(rbind, allInfo))
    if (nrow(allInfo) == 0) {
      allInfo <- data.frame(matrix(nrow = 1, ncol = length(fieldNames),
                                   dimnames = list(list(), fieldNames)))
    }
    return(allInfo)
  })
  output$adminPanel <- renderUI({
    #if (!isAdmin()) return(NULL)
    
    div(id = "adminPanelInner",
        h3("This table is only visible to admins",
           style = "display: inline-block;"),
        a("Show/Hide",
          href = "javascript:toggleVisibility('adminTableSection');",
          class = "left-space"),
        div(id = "adminTableSection",
            dataTableOutput("adminTable"),
            downloadButton("downloadSummary", "Download results")
        )
    )
  })
  output$downloadSummary <- downloadHandler(
    filename = function() { 
      paste0(formName, "_", getFormattedTimestamp(), '.csv')  
    },
    content = function(file) {
      write.csv(infoTable(), file, row.names = FALSE)
    }
  )
  output$adminTable <- renderDataTable({
    infoTable()
  })
  
  ##### End admin panel #####
  ##########################################
  
  # only enable the Submit button when the mandatory fields are validated
  observe({
    if (input$firstName == '' || input$question == '' ||
        input$divnum == '' ||
        !validateStudentNum(input$divnum)) {
      session$sendCustomMessage(type = "disableBtn", list(id = "submitBtn"))
    } else {
      session$sendCustomMessage(type = "enableBtn", list(id = "submitBtn"))
    }
  })
  
  # the name to show in the Thank you confirmation page
  output$thanksName <- renderText({
    paste0("Thank you ", input$firstName, "!")
  })
  
  # we need to have a quasi-variable flag to indicate when the form was submitted
  output$formSubmitted <- reactive({
    FALSE
  })
  outputOptions(output, 'formSubmitted', suspendWhenHidden = FALSE)
  
  # show an error beside the student number when the regex (4 digits) fails
  output$studentNumErr <- renderUI({
    if (input$divnum != '') {
      if(validateStudentNum(input$divnum)) return(NULL)
      span("Division number must be 4 digits", class = "left-space error")
    }
  })
  
  # show a link to test the GitHub name
  output$gitTest <- renderUI({
    if (input$back == '') return(NULL)
    a("Click here to test GitHub name", target = "_blank",
      href = paste0("https://github.com/", input$back),
      class = "left-space")
  })
  
  # show a link to test the Twitter name
  output$twitterTest <- renderUI({
    if (input$lastName == '') return(NULL)
    a("Click here to test Twitter name", target = "_blank",
      href = paste0("https://twitter.com/", input$lastName),
      class = "left-space")
  })  
  
  # submit the form  
  observe({
    #if (input$submitConfirmDlg < 1) return(NULL)
    if (input$submitBtn < 1) return(NULL)
    
    # read the info into a dataframe
    isolate(
      infoList <- t(sapply(fieldNames, function(x) x = input[[x]]))
    )
    
    # generate a file name based on timestamp, user name, and form contents
    isolate(
      fileName <- paste0(
        paste(
          getFormattedTimestamp(),
          input$question,
          input$firstName,
          digest(infoList, algo = "md5"),
          sep = "_"
        ),
        ".csv"
      )
    )
    
    # write out the results
    ### This code chunk writes a response and will have to change
    ### based on where we store persistent data
    write.csv(x = infoList, file = file.path(resultsDir, fileName),
              row.names = FALSE)
    ### End of writing data
    
    # indicate the the form was submitted to show a thank you page so that the
    # user knows they're done
    output$formSubmitted <- reactive({ TRUE })
  })
  
  ##############################################################################################################
  
  output$value <- renderPrint({ input$text })
  
  ### Leaflet Net Export Map-------------------------------------------------------------------
  output$netexportsmap_leaflet <- renderLeaflet({
    withProgress(message = 'Making plot', value = 0, {
      n <- 10
      # pal <- colorBin(c("#FF3041", "#FEA89B","#F9FFA5", "#D3FDB1", "#3FFF90"),
      #                 bins = c(0,1,2,3,4,5),
      #                 domain = xxx$INDEX,
      #                 na.color = "#ffffff"
      # )
      #bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
      bins <- c(0, 1, 2, 3, 4, 5)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Trade Balance: $%s million",
        #"<strong>%s</strong><br/>Trade Balance: $%.0f million",
        xxx$COUNTRY, xxx$x
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet(xxx, options = leafletOptions(minZoom = 2))
      m <- m %>% setView(-5, 30, zoom = 2)
      m <- m %>%   setMaxBounds( lng1 = -150
                                 , lat1 = 75
                                 , lng2 = 165
                                 , lat2 = -53 )
      m <- m %>% addTiles()
      incProgress(1/5, detail = paste("Adding Tiles", 5))
      m <- m %>% addPolygons(
        fillColor = colors_to_fill,
        #fillColor = ~pal(INDEX),
        #fillColor = c("#FF3041",  "#FEA89B", "#F9FFA5", "#D3FDB1", "#3FFF90"),
        #fillColor = c("#3FFF90", "#D3FDB1", "#F9FFA5", "#FEA89B", "#FF3041"),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 0.2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
      incProgress(1/4, detail = paste("Drawing Borders", 4))
      #m <- m %>% leaflet::addLegend('topright', pal = pal, values = ~INDEX, opacity = 0.7, title = NULL,
      m <- m %>% leaflet::addLegend(opacity = 0.7, title = NULL, values = ~INDEX,
                                    colors =c("#FF3041",  "#FEA89B", "#F9FFA5", "#D3FDB1", "#3FFF90"),
                                    labels= c("< -$1 billion", "-$1 billion to -$1 million","-$1 million to +$1 million","+$1 million to +$1 billion","> +$1 Billion"),
                                    position = "bottomleft")
      incProgress(1/3, detail = paste("Adding Legend", 3))
      
      Sys.sleep(5)
      incProgress(1/2, detail = paste("Adding Color", 2))
      Sys.sleep(5)
      incProgress(1/1, detail = paste("Finishing Up...", 1))
      Sys.sleep(10)
    })

    m
    })
  
  
  ### Aircraft Exports-------------------------------------------------------------------------
  # output$aircraft_exports <- renderDygraph({
  #   aircraft_series <- cbind(xts_2014, xts_2015, xts_2016, xts_2017, xts_2018, shaded_region)
  #   # dygraph(aircraft_series, main = "Aircraft Exports, Unadjusted, 2014-2018, thousand of dollars")  %>%
  #   #   dyOptions(fillGraph = FALSE, drawGrid = FALSE) %>%
  #   #   dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  #   #   #dyEvent("2017-02-06", "KeyStone Accident", labelLoc = "bottom") %>%
  #   #   dyRangeSelector()
    
  #   dygraph(aircraft_series, main = "Aircraft Exports, Unadjusted, 2014-2018, thousand of dollars")  %>%
  #     dySeries("X2014", drawPoints = TRUE, color = "green") %>%
  #     dySeries("X2015", drawPoints = TRUE, color = "orange") %>%
  #     dySeries("X2016", drawPoints = TRUE, color = "purple") %>%
  #     dySeries("X2017", drawPoints = TRUE, color = "yellow") %>%
  #     dySeries("X2018", drawPoints = TRUE, color = "blue", fillGraph = FALSE) %>%
  #     dySeries("Current", drawPoints = TRUE, color = "blue", fillGraph = TRUE, strokeWidth = 0, pointSize = 0) %>%
  #     dyHighlight(highlightCircleSize = 5,
  #                 highlightSeriesBackgroundAlpha = 0.2,
  #                 hideOnMouseOut = FALSE,
  #                 highlightSeriesOpts = list(strokeWidth = 3)) %>%
  #     dyAxis("x", valueFormatter = JS(getMonthDay), axisLabelFormatter = JS(getMonth)) %>%
  #     dyOptions(fillGraph = FALSE, drawGrid = FALSE, maxNumberWidth = 20) %>%
  #     #dyShading(from="1", to="", axis = "y") %>%
  #     dyRangeSelector()
  #   
  # })
  
  # ### Daily spot price-------------------------------------------------------------------------
  # output$dailyprices1 <- renderDygraph({
  #   price_series <- cbind(xts_exchange, xts_wti, xts_wcs)
  #   dygraph(price_series, main = "Daily Spot prices, WTI, WCS and exchange rate CAD/USD")  %>%
  #     dyOptions(fillGraph = FALSE, drawGrid = FALSE) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  #     dyEvent("2017-02-06", "KeyStone Accident", labelLoc = "bottom") %>%
  #     dyRangeSelector()
  # })
  
  ### Daily spot price-------------------------------------------------------------------------
  output$dailyprices <- renderHighchart({
    #usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
    #eurkpw <- getSymbols("EUR/KPW", src = "oanda", auto.assign = FALSE)
    
    series_prices = list(
      list(
        name = 'Exchange rate',
        data = xts_exchange,
        color = "#CE4B46"
      ),
      list(
        name = 'WTI',
        data = xts_wti,
        color = '#CAD2B5'
      ),
      list(
        name = 'WCS',
        data = xts_wcs,
        color = '#AFC0D2'
      )
    )
    
    data_flags <- data_frame(
      date = sample(time(xts_wti), size = 1),
      title = sprintf("Keystone Accident", seq_along(date)),
      text = sprintf("An interesting event that took place!", seq_along(date), date)
    )
    
    highchart(type = "stock") %>% 
      #hc_title(text = "Daily Spot prices, WTI, WCS and exchange rate CAD/USD") %>% 
      hc_subtitle(text = "Data extracted from online sources") %>% 
      #hc_add_series_list(series_prices) %>%
      hc_add_series(xts_wti, id = "xts_wti") %>%
      hc_add_series(xts_wcs) %>%
      hc_add_series(xts_exchange) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                 ${point.y:,.0f}<br/>",
                 shared = TRUE) %>% 
      hc_xAxis(categories = price_date,
               tickmarkPlacement = "on",
               title = list(enabled = FALSE)) %>%
      
      hc_exporting(enabled = TRUE) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data_flags, hcaes(x = date),
                    type = "flags", onSeries = "xts_wti"
      )
  })
  
  ### Net Exports Map--------------------------------------------------------------------------
  series_legend = list(
    list(
      name = '> +$1 Billion'
    ),
    list(
      name = '+$1 million to +$1 billion'
    ),
    list(
      name = '+$1 million to -$1 million'
    ),
    list(
      name = '-$1 million to -$1 billion'
    ),
    list(
      name = '< -$1 billion'
    )
  )
  
  # output$netexportsmap <- renderHighchart({
  #   highchart() %>%
  #     hc_chart(backgroundColor = "#8DB4D5") %>% 
  #     hc_tooltip(pointFormat = "<strong>{point.COUNTRY}</strong></span>:<br/>
  #                (Net Export: {point.VALUE_M:,.0f})<br/>
  #                (Trade Balance: {point.TRADE_BALANCE:,.0f})<br/>",
  #                shared = TRUE) %>% 
  #     hc_add_series_map(worldgeojson, netexports_data, name = "<i>($ millions)<i/>", value = "INDEX", joinBy = c("name", "COUNTRY")) %>%
  #     hc_colorAxis(dataClasses = color_classes(colors=c("#FF3041", "#FEA89B","#F9FFA5", "#D3FDB1", "#3FFF90"), c(seq(0, 5, by = 1)))) %>% 
  #     #hc_colorAxis(dataClasses = color_classes(colors=c("#FF3041", "#FEA89B","#F9FFA5", "#D3FDB1", "#3FFF90"), c(c(1000000000,1000000,-1000000,-1000000000)))) %>% 
  #     hc_legend(layout = "vertical", align = "right", reversed = TRUE,
  #               floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
  #     hc_mapNavigation(enabled = TRUE) %>%
  #     hc_add_theme(thm)
  # })
  
  output$netexportsmapdata <- renderDataTable(
    netexports_data, options = list(paging = FALSE)
    
  )
  output$netexportsmapdata_leaflet <- renderDataTable(
    netexports_data, options = list(paging = FALSE)
    
  )
  
  # output$netexportsmap1 <- renderHighchart({
  #   highchart() %>%
  #     hc_chart(backgroundColor = "#8DB4D5") %>% 
  #     hc_tooltip(pointFormat = "<strong>{point.COUNTRY}</strong></span>:<br/>
  #                (Net Export: {point.VALUE_M:,.0f})<br/>
  #                (Trade Balance: {point.TRADE_BALANCE:,.0f})<br/>",
  #                shared = TRUE) %>% 
  #     hc_add_series_map(worldgeojson, netexports_data, name = "<i>($ millions)<i/>", value = "INDEX", joinBy = c("name", "COUNTRY")) %>%
  #     #hc_colorAxis(stops = colstops1) %>%
  #     #hc_colorAxis(dataClasses = color_classes(colors = c("#FF3041", "#FEA89B","#F9FFA5", "#D3FDB1", "#3FFF90"))) %>%
  #     hc_colorAxis(minColor = "#FF3041", maxColor = "#3FFF90") %>%
  #     hc_legend(layout = "vertical", align = "right", reversed = TRUE,
  #               floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
  #     hc_mapNavigation(enabled = TRUE)
  #     #hc_add_theme(thm1)
  # })
  
  # ### USARRESTS example------------------------------------------------------------------------
  # output$usarrests <- renderHighchart({
  #   highchart() %>%
  #     hc_add_series_map(usgeojson, USArrests, name = "Sales",
  #                       value = "Murder", joinBy = c("woename", "state"),
  #                       dataLabels = list(enabled = TRUE,
  #                                         format = '{point.properties.postalcode}')) %>%
  #     hc_colorAxis(stops = colstops) %>%
  #     hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  #     hc_mapNavigation(enabled = TRUE) %>%
  #     hc_add_theme(thm)
  # })
  
  ### Tables-----------------------------------------------------------------------------------
  
  output$tbl1 <- renderTable({head(economic_dashboard, n=3)},
                             striped = TRUE, bordered = TRUE,
                             spacing = 's', na="", hover = TRUE, align = "c",
                             caption="($ millions, SA) ", caption.placement = getOption("xtable.caption.placement", "bottom"),
                             caption.width = getOption("xtable.caption.width", NULL))
  
  output$tbl2 <- renderTable({head(TradeAggregates, n=10)},
                             striped = TRUE, bordered = TRUE,
                             spacing = 's', na="", hover = TRUE, align = "c", width = "100px")
  
  output$tbl3 <- renderTable({head(ExchangeRate, n=10)},
                             striped = TRUE, bordered = TRUE,
                             spacing = 's', na="", hover = TRUE, align = "c", width = "100px")
  
  output$tbl4 <- renderTable({head(BankForecast, n=10)},
                             striped = TRUE, bordered = TRUE,
                             spacing = 's', na="", hover = TRUE, align = "c")
  
  
  ### Pie and Area charts----------------------------------------------------------------------
  output$piechart1 <- renderHighchart({
    highchart() %>%
      hc_title(text = "Exports of Aluminum, 2017") %>%
      hc_add_series_labels_values(data_al$Countries, data_al$Amount, name="Pie", colorByPoint= TRUE, type="pie")
  })
  
  output$piechart2 <- renderHighchart({
    highchart() %>%
      hc_title(text = "Exports of Steel, 2017") %>%
      hc_add_series_labels_values(data_pie$Countries, data_pie$Amount, name="Pie", colorByPoint= TRUE, type="pie")
  })
  
  output$areachart1 <- renderHighchart({
    series_one = list(
      list(
        name = 'Exports to the world',
        data = a_to_world,
        color = "#72a6ff"
      ),
      list(
        name = 'Exports to the United States',
        data = a_to_us,
        color = '#ed8910'
      )
    )
    
    highchart() %>% 
      hc_chart(type = "area") %>% 
      hc_title(text = "Exports of aluminum, 2008-2017") %>% 
      hc_subtitle(text = "Source: aluminum.org") %>% 
      hc_xAxis(categories = year_range,
               tickmarkPlacement = "on",
               title = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Percent")) %>% 
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                 ({point.y:,.0f} million)<br/>",
                 shared = TRUE) %>% 
      hc_plotOptions(area = list(
        stacking = NULL,
        lineColor = "#ffffff",
        lineWidth = 1,
        marker = list(
          lineWidth = 1,
          lineColor = "#ffffff"
        ))
      ) %>% 
      hc_add_series_list(series_one)
  })
  
  output$areachart2 <- renderHighchart({
    series_two = list(
      list(
        name = 'Exports to the world',
        data = s_to_world,
        color = "#72a6ff"
      ),
      list(
        name = 'Exports to the United States',
        data = s_to_us,
        color = '#ed8910'
      )
    )
    
    highchart() %>% 
      hc_chart(type = "area") %>% 
      hc_title(text = "Exports of steel, 2008-2017") %>% 
      hc_subtitle(text = "Source: steel.org") %>% 
      hc_xAxis(categories = year_range,
               tickmarkPlacement = "on",
               title = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Percent")) %>% 
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                 ({point.y:,.0f} million)<br/>",
                 shared = TRUE) %>% 
      hc_plotOptions(area = list(
        stacking = NULL,
        lineColor = "#ffffff",
        lineWidth = 1,
        marker = list(
          lineWidth = 1,
          lineColor = "#ffffff"
        ))
      ) %>% 
      hc_add_series_list(series_two)
  })
  
  ### Aircraft Exports-------------------------------------------------------------------------
  output$hcontainer <- renderHighchart({
    
    series = list(
      list(
        name = '2014',
        color = 'green',
        data = x2014[[1]]
      ),
      list(
        name = '2015',
        color = 'orange',
        data = x2015[[1]]
      ),
      list(
        name = '2016',
        color = 'purple',
        data = x2016[[1]]
      ),
      list(
        name = '2017',
        color = 'yellow',
        data = x2017[[1]]
      ),
      list(
        name = '2018',
        color = 'blue',
        data = x2018[[1]]
      )
    )
    
    highchart() %>%
      #hc_title(text = "Aircraft Exports, Unadjusted, 2014-2018, thousand of dollars") %>%
      hc_add_series_list(series) %>%
      hc_xAxis(categories=list("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
               opposite = TRUE,
               plotBands = list(
                 list(from = 0, to = 1, color = "#bfd7ff",
                      label = list(text = "2018")))
      ) %>%
      hc_yAxis(opposite = TRUE,
               showFirstLabel = FALSE
               
      )
  })
  
  
  # hc %>% 
  #   hc_xAxis(title = list(text = "Month in x Axis"),
  #            opposite = TRUE,
  #            plotLines = list(
  #              list(label = list(text = "This is a plotLine"),
  #                   color = "#FF0000",
  #                   width = 2,
  #                   value = 5.5))) %>% 
  #   hc_yAxis(title = list(text = "Temperature in y Axis"),
  #            opposite = TRUE,
  #            minorTickInterval = "auto",
  #            minorGridLineDashStyle = "LongDashDotDot",
  #            showFirstLabel = FALSE,
  #            showLastLabel = FALSE,
  #            plotBands = list(
  #              list(from = 25, to = JS("Infinity"), color = "rgba(100, 0, 0, 0.1)",
  #                   label = list(text = "This is a plotBand"))))
  
  
  
  
  
  
  ### Stream Graph-----------------------------------------------------------------------------
  output$streamgraph <- renderHighchart({
    
    df <- ggplot2movies::movies %>%
      select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
      tidyr::gather(genre, value, -year) %>%
      group_by(year, genre) %>%
      summarise(n = sum(value)) %>% 
      ungroup()
    df
    
    df1 <- 
      
      hchart(df, "streamgraph", hcaes(year, n, group = genre)) %>% 
      hc_yAxis(visible = FALSE)
    
    # series_str = list(
    #   list(
    #     name = 'World',
    #     color = 'green',
    #     data = stream_w
    #   ),
    #   list(
    #     name = 'US',
    #     color = 'red',
    #     data = stream_us
    #   ),
    #   list(
    #     name = 'China',
    #     color = 'purple',
    #     data = stream_ch
    #   ),
    #   list(
    #     name = 'Canada',
    #     color = 'yellow',
    #     data = stream_ca
    #   ),
    #   list(
    #     name = 'India',
    #     color = 'blue',
    #     data = stream_in
    #   ),
    #   list(
    #     name = 'Norway',
    #     color = 'orange',
    #     data = stream_no
    #   ),
    #   list(
    #     name = 'Russia',
    #     color = 'grey',
    #     data = stream_ru
    #   ),
    #   list(
    #     name = 'Other',
    #     color = 'pink',
    #     data = stream_o
    #   )
    # )
    # 
    # highchart() %>% 
    #   hc_chart(type = "streamgraph") %>% 
    #   hc_title(text = "Aluminum Production") %>% 
    #   hc_subtitle(text = "Source:") %>%
    #   hc_xAxis(categories = stream_year) %>%
    #   #          tickmarkPlacement = "on",
    #   #          title = list(enabled = FALSE)) %>%
    #   # hc_yAxis(title = list(text = "Percent")) %>% 
    #   # hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
    #   #            ({point.y:,.0f} million)<br/>",
    #   #            shared = TRUE) %>% 
    #   hc_plotOptions(area = list(
    #     stacking = "percent",
    #     lineColor = "#ffffff",
    #     lineWidth = 1,
    #     marker = list(
    #       lineWidth = 1,
    #       lineColor = "#ffffff"
    #     ))
    #   ) %>% 
    #   hc_add_series_list(series_str)
  })
  
  #### Pie and area charts (inside tabset panels)----------------------------------------------
  output$piechart3 <- renderHighchart({
    highchart() %>%
      hc_title(text = "Exports of Aluminum, 2017") %>%
      hc_add_series_labels_values(data_al$Countries, data_al$Amount, name="Pie", colorByPoint= TRUE, type="pie")
  })
  
  output$piechart4 <- renderHighchart({
    highchart() %>%
      hc_title(text = "Exports of Steel, 2017") %>%
      hc_add_series_labels_values(data_pie$Countries, data_pie$Amount, name="Pie", colorByPoint= TRUE,type="pie")%>%
      
      hc_exporting(enabled = TRUE)
  })
  
  output$areachart3 <- renderHighchart({
    series_one = list(
      list(
        name = 'Exports to the world',
        data = a_to_world,
        color = "#72a6ff"
      ),
      list(
        name = 'Exports to the United States',
        data = a_to_us,
        color = '#ed8910'
      )
    )
    
    highchart() %>% 
      hc_chart(type = "area") %>% 
      hc_title(text = "Exports of Aluminum, 2008-2017") %>% 
      hc_subtitle(text = "Source: aluminum.org") %>% 
      hc_xAxis(categories = year_range,
               tickmarkPlacement = "on",
               title = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Percent")) %>% 
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                 ({point.y:,.0f} million)<br/>",
                 shared = TRUE) %>% 
      hc_plotOptions(area = list(
        stacking = NULL,
        lineColor = "#ffffff",
        lineWidth = 1,
        marker = list(
          lineWidth = 1,
          lineColor = "#ffffff"
        ))
      ) %>% 
      hc_add_series_list(series_one)
  })
  
  output$areachart4 <- renderHighchart({
    series_two = list(
      list(
        name = 'Exports to the world',
        data = s_to_world,
        color = "#72a6ff"
      ),
      list(
        name = 'Exports to the United States',
        data = s_to_us,
        color = '#ed8910'
      )
    )
    
    highchart() %>% 
      hc_chart(type = "area") %>% 
      hc_title(text = "Exports of Steel, 2008-2017") %>% 
      hc_subtitle(text = "Source: steel.org") %>% 
      hc_xAxis(categories = year_range,
               tickmarkPlacement = "on",
               title = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Percent")) %>% 
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
                 ({point.y:,.0f} million)<br/>",
                 shared = TRUE) %>% 
      hc_plotOptions(area = list(
        stacking = NULL,
        lineColor = "#ffffff",
        lineWidth = 1,
        marker = list(
          lineWidth = 1,
          lineColor = "#ffffff"
        ))
      ) %>% 
      hc_add_series_list(series_two)%>%
      
      hc_exporting(enabled = TRUE)
  })
  
}