### Libraries----------------------------------------------------------------------------------
library(shinydashboard)
library(maps)
library(mapproj)
library(highcharter)
library(dplyr)
library(viridisLite)
library(forecast)
library(treemap)
library(streamgraph)
library(babynames)
library(DT)
library(dygraphs)
library(leaflet)
library(plotly)
library(shinyjs)

### UI ----------------------------------------------------------------------------------------

ui <- dashboardPage(
  #skin = "black",
  skin = "blue",
  dashboardHeader(title = paste("IATD",format(Sys.time(), "%b %d"), sep = " "),
                  #tags$li("August", class="dropdown"),
                  tags$li(a(href = 'https://www150.statcan.gc.ca/n1/dai-quo/index-eng.htm?HPA=1',
                            icon("power-off"),
                            title = "Back to The Daily"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.statcan.gc.ca/eng/start',
                            img(src = 'logo.png',
                                title = "StatCan Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(id='sidebar',
      #menuItem("Main", tabName = "main", icon = icon("navicon")),
      menuItem("Main", tabName = "main2", icon = icon("navicon")),
      menuItem("Economic Dashboard", tabName = "economic_dashboard", icon = icon("dashboard")),
      menuItem("Daily Prices", tabName = "daily_price", icon = icon("money")),
      #menuItem("Canadian Products", tabName = "canadian_products", icon = icon("bicycle")),
      menuItem("Aircraft Exports", tabName = "aircraft_exports", icon = icon("line-chart")),
      menuItem("Trade Balance", tabName = "trade_balance", icon = icon("bar-chart")),
      #menuItem("Net Exports", tabName = "net_exports", icon = icon("map-pin")),
      menuItem("Aluminum and Steel", tabName = "aluminum_steel", icon = icon("area-chart"),
               menuItem("Visuals", tabName = "aluminum_steel", icon = icon("pie-chart")), menuItem("News", tabName = "alum_news", icon = icon("comments"))),
      menuItem("Daily Prices (F)", tabName = "daily_price_1", icon = icon("money")),
      menuItem("Aircraft Exports (F)", tabName = "aircraft_exports_1", icon = icon("line-chart")),
      menuItem("Net Exports", tabName = "net_exports_1", icon = icon("map-pin")),
      menuItem("Questions", tabName = "data", icon = icon("question"), badgeLabel = "new"),
      
      br(), br(), br(), br(),
      conditionalPanel("input.sidebar == 'net_exports_1'",
                       sliderInput("slider2", label = div(style="text-align:centre","Year"), min = 2014, 
                                   max = 2019, value = c(2017, 2018))
      )
      
    )
    
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    
    tabItems(
      # 1st tab content------------------------------------------------------------------------
      tabItem(tabName = "main",
              
              tags$style(HTML("
                              .box.box-warning {
                              border-top-color: #66ccff;
                              }
                              .content {background-color: white;}
                              ")),
              
              fluidRow(
                box(width = 7, height = 250,solidHeader = TRUE),
                box(solidHeader = TRUE, align="center", width = 5, height = 250, br(), h1("Overview of the International Trade Program Monthly Production Operations"))
              ),
              fluidRow(
                #class="center-block"
                box(solidHeader = TRUE, img(src="leaf.png", width="400px", height="290px", align="right"), width = 7, height = 300),
                box(align="center", solidHeader = TRUE, width = 5, height = 300, br(), h3("International Accounts and Trade Division"),br(),br(),br(),br(),br(), p(paste(format(Sys.time(), "%b %d %Y"))))
              )
              ),
      
      # 3rd tab content------------------------------------------------------------------------
      tabItem(tabName = "daily_price", #highchartOutput("streamgraph", height = "590px")
              h2("Daily Spot prices, WTI, WCS and exchange rate CAD/USD"),
              fluidRow(
                column(width = 12, highchartOutput("dailyprices", height = "590px"))
              )
      ),
      
      # 4th tab content
      tabItem(tabName = "canadian_products",
              fluidRow(
                box(
                  title="Energy Products lead increase in imports", solidHeader = TRUE, width = 12, "Following a 4.3% decline in January, total imports were up 1.9% in February to $48.6 billion, with increases observed in 8 of 11 sections. Higher imports of energy products and motor vehicles and parts were partially offset by lower imports of gold. Year-over-year, imports increased 3.5%.")
              ),
              fluidRow(
                box(
                  title="Energy Products", width = 4, solidHeader = TRUE, status = 'primary',
                  "box content"
                )
              )
      ),
      
      # 5th tab content------------------------------------------------------------------------
      tabItem(tabName = "aircraft_exports",
              h2("Aircraft Exports, Unadjusted, 2014-2018, thousand of dollars"), br(),
              tags$style(HTML("
                              .content-wrapper {background-color: white;}
                              ")),
              fluidRow(
                column(width = 12, highchartOutput("hcontainer", height = "590px"))
              )
              ),
      
      
      
      
      # 6th tab content------------------------------------------------------------------------
      tabItem(tabName = "trade_balance",
              h2("Canadian exports of Aluminum and Steel"), br(),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         tabPanel(br(),br(),br(),title="Aluminum",
                                  fluidRow(
                                    column(width=12, splitLayout(
                                      highchartOutput("piechart3"),
                                      highchartOutput("areachart3")
                                    ))
                                  )
                         ),
                         tabPanel(br(),br(),br(),title="Steel",
                                  fluidRow(
                                    column(width=12, splitLayout(
                                      highchartOutput("piechart4"),
                                      highchartOutput("areachart4")
                                    ))
                                  )
                         )
                       )
                )
              )
      ),
      
      # 7th tab content------------------------------------------------------------------------
      tabItem(tabName = "net_exports",
              h2("Net exports for all countries, 2017 (customs, unadjusted, year-to-date)"),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         tabPanel(title="Map",
                                  fluidRow(
                                    column(width=12,
                                      highchartOutput("netexportsmap", height = "570px")
                                      
                                    )
                                  )
                         ),
                         tabPanel(title="Data",
                                  fluidRow(
                                    column(width=12,
                                           dataTableOutput("netexportsmapdata"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                    )
                                  )
                         )
                       )
                )
              )
      ),
      
      # 8th tab content------------------------------------------------------------------------
      tabItem(tabName = "aluminum_steel",
              h1("Canadian exports of Aluminum and Steel"), br(),
              fluidRow(
                column(width = 12,
                       box(highchartOutput("piechart1", height = "250px")),
                       box(highchartOutput("areachart1", height = "250px")))
              ),
              fluidRow(
                column(width = 12,
                       box(highchartOutput("piechart2", height = "250px")),
                       box(highchartOutput("areachart2", height = "250px")))
              )
      ),
      # 9th tab content------------------------------------------------------------------------
      tabItem(tabName = "net_exports_1",
              
              tags$style(HTML("
                              h3 {margin-top: 0;}
                              ")),
              
              h3("Net exports for all countries, 2017 (customs, unadjusted, year-to-date)"),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         tabPanel(title="Map",
                                  fluidRow(
                                    column(width=12,
                                           leafletOutput("netexportsmap_leaflet", height = "550px")
                                           
                                    )
                                  )
                         ),
                         tabPanel(title="Data",
                                  fluidRow(
                                    column(width=12,
                                           dataTableOutput("netexportsmapdata_leaflet"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                    )
                                  )
                         )
                       )
                )
              )

      ),
      
      # 10th tab content------------------------------------------------------------------------
      tabItem(tabName = "data",
              source("helpers.R"),
              fluidPage(
                
                # add external JS and CSS
                singleton(
                  tags$head(includeScript(file.path('www', 'message-handler.js')),
                            includeCSS(file.path('www', 'style.css'))
                  )
                ),
                
                title = "Questions and Feedback",
                h2("Questions and Feedback"),
                
                # admin panel will only be shown to users with sufficient privileges
                uiOutput("adminPanel"),
                
                conditionalPanel(
                  # only show this form before the form is submitted
                  condition = "!output.formSubmitted",
                  
                  # form instructions
                  p("In order to improve our presentation, it would help us
                    tremendously if you could provide us with some questions and comments."),
                  p("You don't have to submit this form, but we would REALLY appreciate it if you did."),
                  p("The fields marked with * are mandatory (if you choose to participate at all),
                    and the rest are optional but highly recommended."),
                  strong("Help us help you :)"),
                  shiny::hr(),
                  
                  # form fields
                  textInput(inputId = "firstName", label = "First name *"),
                  textInput(inputId = "lastName", label = "Last name *"),
                  textInput(inputId = "question", label = "Question or Comment *"),
                  textInput(inputId = "divnum",
                            label = "Division #"),
                  uiOutput(outputId = "studentNumErr"),
                  selectInput(inputId = "back", label = "Background",
                              choices = c("", "Economics", "Statistics", "Computer Science", "Health",
                                          "Other"),
                              selected = ""),
                  textInput(inputId = "email", label = "Preferred email"),
                  selectInput(inputId = "osType", label = "Operating system",
                              choices = c("", "Windows 7", "Windows 8", "Mac", "Linux",
                                          "Other"),
                              selected = ""),
                  # textInput(inputId = "gitName", label = "GitHub username"),
                  # uiOutput("gitTest"),

                  
                  # 
                  # uiOutput("twitterTest"),
                  br(),
                  actionButton(inputId = "submitBtn", label = "Submit")
                  
                  # the following lines use a confirmation dialog before submitting
                  #modalTriggerButton("submitBtn", "#submitConfirmDlg", "Submit"),
                  #modalDialog(id="submitConfirmDlg", body = "Are you sure you want to submit?",
                  #            footer=list(
                  #  modalTriggerButton("submitConfirmDlg", "#submitConfirmDlg", "Submit"),
                  #  tags$button(type = "button", class = "btn btn-primary", 'data-dismiss' = "modal", "Cancel")
                  #))
                  ),
                
                conditionalPanel(
                  # thank you screen after form is submitted
                  condition = "output.formSubmitted",
                  
                  h3(textOutput("thanksName"))
                ),
                
                # author info
                shiny::hr(),
                em(
                  span("Created by "),
                  a("Sandra Mutimukeye", href = "http://deanattali.com"),
                  span(", Aug 2018"),
                  br(), br()
                )
              )),
              
              ### stream graph testing
              # dat_str %>%
              #   streamgraph("asset_class", "volume_billions", "year", interpolate="cardinal") %>%
              #   sg_axis_x(1, "year", "%Y") %>%
              #   sg_fill_brewer("PuOr")
      
      
      # 11th tab content------------------------------------------------------------------------
      tabItem(tabName = "aircraft_exports_1",
              h2("Aircraft Exports, Unadjusted, 2014-2018, thousand of dollars"),
              fluidRow(
                #column(width = 12, dygraphOutput("aircraft_exports", height = "590px"))
                box(width = 12, plotlyOutput("plotlyaircraft", height = "590px"))
              )
              ),
      # 12th tab content------------------------------------------------------------------------
      tabItem(tabName = "daily_price_1",
              tags$style(HTML("
                              h2 {margin-top: 0;}
                              ")),
              h2("Daily Spot Prices Jan/2017 - Apr/2018"),
              fluidRow(
                #column(width = 12, dygraphOutput("dailyprices1", height = "590px"))
                box(width = 12, plotlyOutput("plotlyprice", height = "590px"))
              )
              ),
      

      
      # 2nd tab content------------------------------------------------------------------------
      tabItem(tabName = "main2",

              fluidRow(
                box(width = 12, height = 650, solidHeader = TRUE, img(src="template.png", width="1000px", height="650px", align="center"))
              )


      ),
      # 2nd tab content------------------------------------------------------------------------
      tabItem(tabName = "economic_dashboard",
              h2(strong("Economic Dashboard")),
              fluidRow(
                column(width = 12,
                       box(width = 12, includeHTML("www/table1.html"))
                )
              ),
              fluidRow(
                column(width = 7,
                       box(height = 390, width = 12, solidHeader=FALSE, includeHTML("www/table2.html"))
                       ),
                column(width = 5,
                  box(width = 12, solidHeader=FALSE, includeHTML("www/table3.html")),
                  box(width = 12, solidHeader=FALSE, includeHTML("www/table4.html"))
                )
              )
      ),
      # 2nd tab content------------------------------------------------------------------------
      tabItem(tabName = "alum_news",
              h2(strong("In the News")),
              div(style="text-align:justify","This text does not make sense. In the fall of 2015 annual increase, it gave way to the market share of 23%.
                  Finally, we showed you this slide last month. Those are our exports of Aluminum and Steel. Why we show that again?
                  First because after the POTUS proclamation, we were able to identify exactly what the US are tariffing. Last month we did not know, so
                  we presented some data that we thought was going to be tariffed. So this month we have the revised figures.
                  Second, you probably read in the news that the US, if no NAFTA dealis done by end of April, will apply tarrifs on Aluminum and steel.
                  Finally, we showed you this slide last month. Those are our exports of Aluminum and Steel. Why we show that again?
                  Second, you probably read in the news that the US, if no NAFTA dealis done by end of April, will apply tarrifs on Aluminum and steel.
                  So here you have it. Aluminum exports were a little over $10 billion in 2017, with 89% going to the US. And iron and steel exports were close to $8 billion in 2017, with",
                  br(), br(),
                  "Quandl Data shows that aluminum is tricky to deal with. Last Wednesday the chief steel officer met with the production to discuss drops in rate.
                  Quandl Data shows that aluminum is tricky to deal with. Last Wednesday the chief steel officer met with the production to discuss drops in rate.
                  Quandl Data shows that aluminum is tricky to deal with. Last Wednesday the chief steel officer met with the production to discuss drops in rate.
                  "),
              br(), img(h4(strong("The Decline of Steel production in Canada")),div(style="text-align:justify", "Finally, we showed you this slide last month. Those are our exports of Aluminum and Steel. Why we show that again?

                                                                     First because after the POTUS proclamation, we were able to identify exactly what the US are tariffing. Last month we did not know, so we presented some data that we thought was going to be tariffed. So this month we have the revised figures.
                                                                     
                                                                     Second, you probably read in the news that will be explained using biometric laws."), src="Picture1.png", width="300px", height="150px", align="right"),
              
              br(),br(),br(),br(),br(),br(), box(align="center", status = 'primary', solidHeader = TRUE, title = "Steel", "Some description",br(),br(), useShinyjs(), shiny::actionButton(inputId="ab1", label = "Learn More", icon = icon("th"))),
              box(align="center", status = 'warning', solidHeader = TRUE, title = "Aluminum", "Some description",br(),br(), useShinyjs(), shiny::actionButton(inputId="ab2", label = "Learn More", icon = icon("th")))
              

      )
      
      )
    )
    )