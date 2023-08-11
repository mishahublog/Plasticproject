
library(shiny)
library(shinyjs)
library(leaflet)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyauthr)
library(shinymanager)
library(shinyWidgets)
library(tidyverse)
library(base64enc)
library(writexl)
#library(gargle)
#library(bs4Dash)
library(waiter)
library(rdrop2)
library(DBI)
library(RSQLite)
library(firebase)
library(dplyr)
library(plotly)
library(reshape2)
library(r2social)

#source("refreshable _token_drop.R")

#token <- drop_auth_RT()
#saveRDS(token, file = "token.rds")

#DOWNLOAD database===========
#drop_download("photodata.sqlite",dtoken = token,overwrite = TRUE)
con1<- dbConnect(SQLite(), "photodata.sqlite")
#dropbox shared links
#shared_links<- drop_list_shared_links(verbose = FALSE,dtoken = token)

#loading_screen <- tagList(
#  h3("Please bear a moment, we are organising your assets for next login", style = "color:gray;")
#)

##UI starts=========
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Litter-Log",
                                 userOutput("user")),

  
  # Title
  dashboardSidebar(
    
    #tags$style(HTML(".sidebar-menu li a { font-size: 100px; color: red}")),
    sidebarMenu( 
      menuItem("Login", tabName = "UserLogin", icon = icon("sign-in")),
      menuItem("map", tabName = "map", icon = icon("map"),badgeLabel = "Desktop only",badgeColor = "red")
      
      
      
    )),
  
  dashboardBody(
    useWaiter(),
    waiterOnBusy(),
    
    
    style = "height: 90vh; overflow-y: auto;", 
    
    #geotag============
    tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
    ##===========
    
    tabItems(
      tabItem(tabName = "UserLogin",
              useFirebase(),
              firebaseUIContainer(),
              reqSignin(        
                
                #=======
                box(
                  title = "Box with boxPad containing inputs",
                  status = "warning",
                  width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      boxPad(
                        color = "gray",
                        
                        fileInput("imageFile", "Choose Image File",capture = "camera"),
                        leafletOutput("mobilemap")),
                      
                      
                    )
                  ),
                  column(width = 6
                         
                  )
                )
                
                ,
                
                #==============        
                box(
                  title = "Box with boxPad containing inputs",
                  status = "warning",
                  width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      boxPad(
                        color = "gray",
                        sliderInput(
                          "obs2", 
                          "Number of observations:",
                          min = 0, max = 1000, value = 500
                        ),
                        div(
                          style = "display: flex; flex-direction: column; align-items: center;",  
                       column(width = 2,offset = 2,
                        knobInput(
                          inputId = "myKnob",
                          skin = "tron",
                          readOnly = TRUE,
                          label = "Display previous:",width = "90%",height = "90%",
                          value = 50,
                          min = -100,
                          displayPrevious = TRUE,
                          fgColor = "#428BCA",
                          inputColor = "#428BCA"
                        )),
                        column(width = 2,offset = 2,
                        knobInput(
                          inputId = "myKnob",
                          skin = "tron",
                          readOnly = TRUE,
                          label = "Display previous:",width = "90%",height = "90%",
                          value = 50,
                          min = -100,
                          displayPrevious = TRUE,
                          fgColor = "#428BCA",
                          inputColor = "#428BCA"
                        ))
                        )
                        
                        
                        
                      )
                    ),
                    column(
                      width = 6,
                      plotOutput("distPlot2", height = "200px"),
                      tableOutput("data")
                    )
                  )
                )   
              )
              
              
              
              
              # leafletOutput("usermap")# Adding Map
              
              
              
      ),
      
      tabItem(tabName = "map",
              #fill maps=====
              fluidRow(
                fillPage(
               leafletOutput("desktopmap",height = 670))),
              # Adding Map
                valueBoxOutput("trashnos"),
                valueBoxOutput("scout"),
                valueBoxOutput("satellite")
                
      
      
      
    ))))




server <- function(input, output,session) { 
  
  #login & authentications ===========
  f <- FirebaseUI$ 
    new()$
    set_providers(
      email = TRUE,
      google = TRUE
    )$
    launch()
  
  
  #
  #=====================================
  #camera============================    
  observeEvent(input$imageFile, {
    f$req_sign_in()
    
    inFile <- input$imageFile
    if (!is.null(inFile)) {
      imageContent <- base64encode(inFile$datapath)
      output$renderedImage <- renderUI({
        tags$img(src = paste0("data:image/png;base64,", imageContent), width = "300px", height = "300px")
      })
    } 
    showModal(
      modalDialog(
        div(
          style = "display: flex; flex-direction: column; align-items: center;",
        uiOutput("renderedImage"),
        
        checkboxGroupButtons(
          inputId = "plastic",
          label = "Label",
          choices = c("Plastic Bottles", "Soft Plastics", "Hard plastics")
        ),
       
        conditionalPanel(
          condition = "input.plastic.includes('Plastic Bottles')",
          sliderInput("slider2", label = h3("Plastic Bottles"), min = 1, 
                      max = 1000, value = 50)
        ),
        
        conditionalPanel(
          condition = "input.plastic.includes('Soft Plastics')",
          sliderInput("slider3", label = h3("Soft Plastics"), min = 1, 
                      max = 1000, value = 50)
                  ),
        
        conditionalPanel(
          condition = "input.plastic.includes('Hard plastics')",
          sliderInput("slider4", label = h3("Hard plastics"), min = 1, 
                      max = 1000, value = 50)
        ),
        
        selectInput("select", label = h3("Garbage dump site"), 
                    choices = list("Tennis Court" = 1, 
                                   "Carroms" = 2, 
                                   "Football Court" = 3), 
                    selected = 1),
        
      
        
        
        
        #submitt button
        actionButton(inputId = "submit",label = "submit"),
        easyClose = TRUE,
        footer = NULL
      ))) 
    
    
    
    
    observeEvent(input$submit,{
      f$req_sign_in()
      con1<- dbConnect(SQLite(), "photodata.sqlite")
      
      
      #Get data
      timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 18)
      user <- f$get_signed_in() #get user data
      library(dplyr)#add inputs to database
      add_sessionid_to_db <- function(name,date,size,bottle,softplastic,hardplastic,plasticcover,longitude,latitude,photo,conn = con1) {
        tibble(name=name,
               date= date,
               size=size, 
               bottle=bottle,
               softplastic = softplastic,
               hardplastic=hardplastic,
               longitude= longitude,
               latitude= latitude,
               photo=photo) %>%
          dbWriteTable(con1, "userdata", ., append = TRUE)}
      add_sessionid_to_db(name = user$response$displayName,
                          date= timeid,
                          size= input$select,
                          bottle=input$slider2,
                          softplastic=input$slider3,
                          hardplastic = input$slider4,
                          longitude= input$long,
                          latitude= input$lat,photo=paste0("data:image/png;base64,", imageContent),conn = con1
      )
      
     # drop_upload("photodata.sqlite",dtoken = token,mode = "overwrite")
      
      
      showModal(modalDialog(
        "Hmm, this is really great"
      ))
      
    })
  })
  #=======================================
  #mape================
  output$desktopmap<- renderLeaflet({
    
    con1<- dbConnect(SQLite(), "photodata.sqlite")
    
    # Retrieve data from the database
    data <- data.frame(tbl(con1,"userdata")) 
    #dbGetQuery(conn = con1, "SELECT * FROM userdata")
    
    # Add markers to the map
    
    #photoContent <- base64decode(encodedPhoto)
    
    library(leaflet)
    library(leaflet)
    leaflet(data = data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0(
          "<img src='",photo,"' width='50px' height='50px'>"
        ), clusterOptions = markerClusterOptions(),
        clusterId = "quakesCluster"
      ) %>%
      addEasyButton(easyButton(
        states = list(
          easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))
  
    
    
    
    
  })
  # user_map==========================
  
  
  output$mobilemap<- renderLeaflet({
    user <- f$get_signed_in() #get user data
    con1<- dbConnect(SQLite(), "photodata.sqlite")
    
    # Retrieve data from the database
    data <- data.frame(tbl(con1,"userdata") )
    #create userdata
    map_points<- data[grep(pattern = user$response$displayName,data$name,fixed = TRUE),]
    data_user<- reactive({map_points})
    #data_user<-  data %>% filter(name==user$response$displayName)
    # Add markers to the map
    
    #photoContent <- base64decode(encodedPhoto)
    
    library(leaflet)
    leaflet(data = data_user()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0(
          "<div style='text-align: center;'>",
          "<img src='",photo,"' width='100px' height='100px'>",
          "<br><button onclick='Shiny.onInputChange(\"delete_marker\", \"",data$date, "\")'>Delete Image</button>",
          "</div>"
          
          
        ), clusterOptions = markerClusterOptions(),
        clusterId = "quakesCluster"
      ) %>%
      addEasyButton(easyButton(
        states = list(
          easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))
    
    
    
    
    
    
  })
  #===================================
  
  #status boxes
  output$trashnos <- renderValueBox({
    valueBox(
      24, "Trash found", 
      icon = tags$i(tags$img(src= "icons8-garbage-66.png", height='80', width='80')),
      color = "yellow"
    )
  })
  output$scout<- renderValueBox({
    valueBox(
      30, "Scouts joined", 
      icon = tags$i(tags$img(src= "www/icons8-attire-51.png", height='80', width='80')),
      color = "teal"
    )
  })
  
  output$satellite<- renderValueBox({
    valueBox(
      45, "Satelliet callibration samples", 
      icon = tags$i(tags$img(src= "www/icons8-satellite-signal-50.png", height='80', width='80')),
      color = "teal"
    )
  })
  

  #loginnto =======================
  
  output$user <- renderUser({
    f$req_sign_in()
    user <- f$get_signed_in()#add user
    dashboardUser(
      name = user$response$providerData[[1]]$displayName, 
      image = user$response$providerData[[1]]$photoURL, 
      title = "Litterlog",
      subtitle = "volunteer", 
      footer = p("The footer", class = "text-center"),
      fluidRow(
       
        dashboardUserItem(
          width = 6,
          actionButton("signout", "Sign out", icon("sign-out"), 
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
    )
  })
  #signout================
  observeEvent(input$signout, {
    f$sign_out()
  })
  # 
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)