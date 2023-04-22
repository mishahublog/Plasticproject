
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

source("refreshable _token_drop.R")

token <- drop_auth_RT()
saveRDS(token, file = "token.rds")

#DOWNLOAD database===========
drop_download("photodata.sqlite",dtoken = token,overwrite = TRUE)
con1<- dbConnect(SQLite(), "photodata.sqlite")
#dropbox shared links
shared_links<- drop_list_shared_links(verbose = FALSE,dtoken = token)

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
      reqSignin(actionButton("signout", "Sign out"))
      
      
      
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
              reqSignin(leafletOutput("usermap",height = 700)),# Adding Map
              fluidRow(
                    valueBoxOutput("trashnos"),
                       valueBoxOutput("scout")
             
             ),
              
              
              
              
              
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
#login & authentications ===========
  f <- FirebaseUI$ 
    new()$
    set_providers(
      email = TRUE,
      google = TRUE
    )$
    launch()
  
  user <- observe({f$get_signed_in()})#get user data
  
  #get userdetails
  #add if data not found
  if (dbExistsTable(con1,paste(user$response$displayName,"coordphotos"))==FALSE){
    
        dbCreateTable(conn = con1,name = paste(user$response$displayName,"coordphotos"), 
                             map_user_points_na[-(1:10),])}
#Data collection===================================
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
               sliderInput("slider1", label = h3("Size of dumping spot"), min = 1, 
                           max = 100, value = 50),
               #add 
               #Add size
               sliderInput("slider2", label = h3("% of plastic bottiles"), min = 1, 
                           max = 100, value = 50),
               sliderInput("slider3", label = h3("% of soft plastics"), min = 1, 
                           max = 100, value = 50),
               sliderInput("slider4", label = h3("% of Hard plastics"), min = 1, 
                           max = 100, value = 50),
               sliderInput("slider5", label = h3("% of plastic covers"), min = 1, 
                           max = 100, value = 50),
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
    con1<- dbConnect(SQLite(), "photodata.sqlite")
    
    #Get data
    timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 13)
    user <- f$get_signed_in() #get user data
    library(dplyr)#add inputs to database
    add_sessionid_to_db <- function(name,date,size,bottle,softplastic,hardplastic,plasticcover,longitude,latitude,conn = con1) {
      tibble(name=name,date= date,size=size, bottle=bottle,softplastic = softplastic,hardplastic=hardplastic,plasticcover=plasticcover,
             longitude= longitude,latitude= latitude) %>%
        dbWriteTable(con1, "userdata", ., append = TRUE)}
    add_sessionid_to_db(name = user$response$displayName,
                        date= timeid,
                        size= input$slider1,
                        bottle=input$slider2,
                        softplastic=input$slider3,
                        hardplastic = input$slider4,
                        plasticcover = input$slider5,
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
    #Make photos available in search
    drop_share(paste(user$response$displayName,timeid,".png",
                     sep = ""),dtoken = token)
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
# Data displaying ======================
  #user level ===================
  
  no_photo <- reactive({ dataURI(file = "www/No_photo_found.png", mime = "image/png") })
#create userlevel photos
  output$usermap<- renderLeaflet({
    #user <- f$get_signed_in() #get user data
    con1<- dbConnect(SQLite(), "photodata.sqlite")
    
    library(dplyr)
    userdata<- data.frame(tbl(con1,"userdata"))
    
    map_points<- userdata[grep(pattern = user$response$displayName,userdata$name),]
    map_points$id<- paste(map_points$name,map_points$date,sep="")
    
    #find shared links
    sharelinks<- drop_list_shared_links(verbose = FALSE,dtoken = token)
    
    #coord_user_data<- function(sharelinks,user){
    
    #find links  
    listlinks<- try(compact(lapply(1:length(sharelinks$links),function(i)
      ifelse((!is.na(as.character(str_match(string = sharelinks$links[[i]]$name,
                                            pattern = paste(map_points$name,map_points$date,sep=""))))),
             yes = linklist<- sharelinks$links[[i]]$url,no =NA))))
    #Remove NAs from list
    listnos<- try(compact(lapply(1:length(listlinks), function(x)
      which(str_detect(string = sharelinks$links[[x]]$name,
                       pattern = paste(map_points$name,map_points$date,sep="")))
    )))
    
    #extract urls only
    listurls_only<- try(lapply(1:length(listnos),function(x)unique(listlinks[[x]][!is.na(listlinks[[x]])])))
    #make it open for display
    listurls_open<- try(unlist(lapply(1:length(listurls_only),function(i)
      paste(str_remove(listurls_only[[i]],pattern = "dl=0"),"raw=1",sep = "")))  )
    
    
    listurl_names<- try(unlist(lapply(1:length(listurls_open),function(x)
      str_sub(str_replace_all(basename(listurls_open[[x]]),pattern = "%20",replacement = " "),
              start = 1,end = str_length(str_sub(str_replace_all(basename(listurls_open[[x]]),pattern = "%20",replacement = " ")))-10
      ))))
    
    idurllist<- try(data.frame(listurls_open,id=listurl_names))
    map_points_matched<- try(merge(idurllist,map_points,by="id"))
    
    map_user_points_na<- reactive({ map_points_matched[complete.cases(map_points_matched),] })
    
    dbWriteTable(con1, paste(user$response$displayName,"coordphotos"),map_user_points_na(), append = TRUE)
    
    if (length(listlinks)==0){
      
      leaflet() %>% addTiles() 
      
    } else if (length(listlinks)!=0) {
      
      leaflet() %>%
        addTiles %>%
        addCircleMarkers(data= map_user_points_na(), lng =~longitude, lat = ~latitude,
                         popup = paste0("<img src = ",listurls_open," width = 80vw>"))
    }
    
  })
   
   observeEvent(input$signout, {
     f$sign_out()
   })
  # 
  output$user <- renderUser({
    f$req_sign_in()
    user <- f$get_signed_in()#add user
    
     dashboardUser(
       name = user$response$providerData[[1]]$displayName,
       image = user$response$providerData[[1]]$photoURL, 
       title = "shinydashboardPlus",
       subtitle = "Author", 
       footer = p("The footer", class = "text-center"),
       fluidRow(
         dashboardUserItem(
           width = 6,
           socialButton(
             href = "https://dropbox.com",
             icon = icon("dropbox")
           )
         ),
         dashboardUserItem(
           width = 6,
           socialButton(
             href = "https://github.com",
             icon = icon("github")
           )
         )
       )
     )
   })

  output$trashnos <- renderValueBox({
    f$req_sign_in()
    valueBox(
      length(listnos), "Trash found", 
      icon = tags$i(tags$img(src= "icons8-garbage-66.png", height='80', width='80')),
      color = "yellow"
    )
  })
  output$scout<- renderValueBox({
    f$req_sign_in()
    valueBox(
      length(listnos), "Scouts joined", 
      icon = tags$i(tags$img(src= "icons8-attire-51.png", height='80', width='80')),
      color = "teal"
    )
  })
 
}

shinyApp(ui, server)