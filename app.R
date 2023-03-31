
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

source("refreshable _token_drop.R")

token <- drop_auth_RT()
saveRDS(token, file = "token.rds")

#DOWNLOAD database===========
drop_download("photodata.sqlite",dtoken = token,overwrite = TRUE)
con1<- dbConnect(SQLite(), "photodata.sqlite")

##UI starts=========
ui <- dashboardPage(
  dashboardHeader(title = "Litter-Log",
                  userOutput("user")),
  # Title
  dashboardSidebar(
    
    #tags$style(HTML(".sidebar-menu li a { font-size: 100px; color: red}")),
    sidebarMenu( 
      menuItem("Login", tabName = "UserLogin", icon = icon("sign-in")),
      menuItem("map", tabName = "map", icon = icon("map"),badgeLabel = "Desktop only",badgeColor = "red"),
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout"))
      
      
    )),
  
  dashboardBody(
    
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
              uiOutput("infobox"),
              leafletOutput("mymap",height = 700) # Adding Map
              
              
              
              
              
      ),
      
      tabItem(tabName = "map",
              #fill maps=====
              fluidRow(
                fillPage(
                  leafletOutput("map",height = 700) # Adding Map
                )))
      
      
    ),
    
    
    #flotting buttons====
    fab_button(
      actionButton("register", "Show registration"),
      actionButton("camera","camera",icon = icon("camera")),
      
      
      actionButton(
        inputId = "info",
        label = "Information",
        icon = icon("info")
      ),
      position = c("bottom-right"),
      animation = c("zoomin"),
      toggle = c("hover"),
      inputId = "test",
      label = "test"
    )
  )
)

server <- function(input, output) { 
  
  f <- FirebaseUI$
    new("session")$
    set_providers(
      email = TRUE,
      google = TRUE
    )$
    launch()
  #get userdetails

  #map for users without login==================
  output$map <- renderLeaflet({ # map positions 
    leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% addTiles()
  })
  
  observe({
    #No permission check
    if (!isTRUE(f$req_sign_in())){
      observeEvent(input$camera,{
        showModal(modalDialog(
          "You need to login"
        ))
      })
    } else 
      
      # Add camera ====
    observeEvent(input$camera,{
      # add model dialogue
      f$req_sign_in()
      
      showModal(modalDialog(
        
        column(width = 8,align="center",
               title = "Title",
               #condition for display Ui image 
               conditionalPanel(
                 condition = "is.null(input[['upload']]==TRUE )",
                 uiOutput("userpro")
                 
               ),
               conditionalPanel(
                 condition = "!is.null(input[['upload']]==TRUE )",
                 uiOutput("image")
                 
               ),
               
               #camera
               fileInput("upload", "Upload image", accept = "image",capture = "camera",buttonLabel = "Camera"),
               
               #fileInput(inputId = "camera",accept = "image/png",
               #         label = "Turn on your camera",buttonLabel = "camera",placeholder = "No photos taken"),
               #Add size
               sliderInput("slider2", label = h3("Slider Range"), min = 1, 
                           max = 100, value = 50),
               #add 
               #Add shape
               selectInput("select", label = h3("Select shape"), 
                           choices = list("Square" = 1, "rectangle" = 2, "round" = 3), 
                           selected = 1),
               
               #submitt button
               actionButton(inputId = "submit",label = "submit"),
               easyClose = TRUE,
               footer = NULL
        )
        
      ))
      
    })
  })
  
  #Find userb and update data
  observeEvent(input$submit,{
    f$req_sign_in()
    #Get data
    timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 13)
    user <- f$get_signed_in() #get user data
    library(dplyr)#add inputs to database
    add_sessionid_to_db <- function(name,date,size,select,longitude,latitude,conn = con1) {
      tibble(name=name,date= date,size=size, select=select,longitude= longitude,latitude= latitude) %>%
        dbWriteTable(con1, "userdata", ., append = TRUE)}
    add_sessionid_to_db(name = user$response$displayName,
                        date= timeid,
                        size= input$slider2,
                        select=input$select,
                        longitude= input$long,
                        latitude= input$lat,conn = con1
    )
    drop_upload("photodata.sqlite",dtoken = token,mode = "overwrite")
    
    inFile <- input$upload
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path(paste( user$response$displayName,timeid,".png",sep = "")))
   
    
    drop_upload(paste(user$response$displayName,timeid,".png",
                      sep = ""),dtoken = token,mode = "overwrite")
    showModal(modalDialog(
      "Hmm, this is really great"
    ))
    
  })
  
  #show photo
  base64 <- reactive({
    inFile <- input[["upload"]] 
    if(!is.null(inFile)){
      dataURI(file = inFile$datapath)
    } 
  })
  
  b64 <- reactive({ dataURI(file = "www/camera_icon.png", mime = "image/png") })
  
  
  #render image
  output[["image"]] <- renderUI({
    if(!is.null(base64())){
      tags$div(
        tags$img(src= base64(), width="100%"),
        style = "width: 400px;"
      )
    } else {
      
      tags$img(src = b64(),width="60%")
      

    }
  })
  
}

shinyApp(ui, server)