



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


library(rdrop2)

source("refreshable _token_drop.R")

token <- drop_auth_RT()
saveRDS(token, file = "token.rds")

drop_userbase<- drop_download("vol_data.xlsx",dtoken = token,overwrite = TRUE)
drop_photodata<- drop_download("photodata.xlsx",dtoken = token,overwrite = TRUE)

library(readxl)
userbase<- data.frame(read_excel("vol_data.xlsx"))
photodata<- data.frame(read_excel("photodata.xlsx"))

#App starts====
ui <- dashboardPage(freshTheme = "mytheme.css",skin = "purple",
                   
  
 
 dashboardHeader(title = "Litter-Log"), # Title
  dashboardSidebar(
   
                   #tags$style(HTML(".sidebar-menu li a { font-size: 100px; color: red}")),
                   sidebarMenu(
                     menuItem("Login", tabName = "UserLogin", icon = icon("sign-in")),
                     menuItem("map", tabName = "map", icon = icon("map"),badgeLabel = "Desktop only",badgeColor = "red")
                   )),
  
  dashboardBody(
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

    tabItems(
      tabItem(tabName = "UserLogin",
              div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
              shinyauthr::loginUI(id = "login"),
              uiOutput("infobox")
            

              
              ),

    tabItem(tabName = "map",
    #fill maps=====
    fluidRow(
     fillPage(
        leafletOutput("mymap",height = 700) # Adding Map
      )))),
    
    
      
      # add login panel UI function
      
    
     
      
     
      
      
    #),
    
    
   
    
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
#server=======
server <- function(input, output) {
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger============
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = userbase, 
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  # call the logout module with reactive trigger to hide/show====
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  #map for users without login==================
  output$mymap <- renderLeaflet({ # map positions 
    leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% addTiles()
  })
  #input register============
  observeEvent(input$register,{
    showModal(modalDialog(
      dashboardBody(
        # Insert Name
        textInputIcon(
          inputId = "user",
          label = "Name"),
        # Insert password
        passwordInput(
          inputId = "password",
          label = "password"),
        
        # Joining data
        dateInput(
          inputId = "dob",
          label ="Birth",
          min = "1960-01-01",
          max = Sys.Date(), format = "yyyy/mm/dd", 
          language = "en"),
        #Age
        textInputIcon(
          inputId = "age",
          label = "Age"),
        
        # Phone
        textInputIcon(
          inputId = "phone",
          label = "Phone"),
        
        # E-mail
        textInputIcon(
          inputId = "email", 
          label = "E-mail"),
        
        # Send
        actionButton("send", "Send")
        
      )
    )
    
    
    
    )
    
    
  })
  
  #registartion for new users=====
  observeEvent(input$send, {
    require(tidyverse)
    #gather info of all the relevant inputs
    client_df <- c('user','password','dob', 'age', 'phone', 'email') %>% 
      map_dfc(~ tibble("{.x}" := input[[.x]]))
    # print(client_df) #this will print a df with one row in the console
   userbase<-  rbind(userbase, data = client_df)
   write_xlsx(userbase,"vol_data.xlsx")
   drop_upload("vol_data.xlsx",dtoken = token,mode = "overwrite")
    
  })
  # User response ===========
  observeEvent(input$send,{
    showModal(
      modalDialog(
        p("its done")
      )
    )
    
  })
  
  #Need permission===
  observe({
    #No permission check
    if (!isTRUE(credentials()$user_auth)){
      observeEvent(input$camera,{
        showModal(modalDialog(
          "You need to login"
        ))
      })
    } else 
      
      # Add camera ====
    observeEvent(input$camera,{
      # add model dialogue
      req(credentials()$user_auth)
      
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
               fileInput("upload", "Upload image", accept = "image/png",capture = "camera",buttonLabel = "Camera"),
               
               #fileInput(inputId = "camera",accept = "image/png",
               #         label = "Turn on your camera",buttonLabel = "camera",placeholder = "No photos taken"),
               #Add size
               sliderInput("slider2", label = h3("Slider Range"), min = 1, 
                           max = 1000, value = c(40, 60)),
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
    #Get data
    timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 13)
  library(dplyr)
    photodata<- bind_rows(photodata,tibble(name=paste(credentials()$info$user,timeid,sep = ""),
                                       size=input$slider2,
                                       select=input$select,
                                       longitude= input$long,latitude= input$lat))
    write_xlsx(photodata,"photodata.xlsx")
    drop_upload("photodata.xlsx",dtoken = token,mode = "overwrite")
   
    
    
  })
  #photo file
  library(stringr)
  timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 15)
  
  observeEvent(input$upload, {
    inFile <- input$upload
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path(paste(credentials()$info$user,timeid,".png",sep = "")) )
  })
  
  observeEvent(input$submit,{
    drop_upload(paste(credentials()$info$user,timeid,".png",
                      sep = ""),dtoken = token,mode = "overwrite")
  })
  
  
  #After photo taken
  observeEvent(input$submit,{
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
  
  output[["image"]] <- renderUI({
    if(!is.null(base64())){
      tags$div(
        tags$img(src= base64(), width="100%"),
        style = "width: 400px;"
      )
    }
  })
  
  
output$userpro<- renderUI({
  req(credentials()$user_auth)
  userBox(
    title = userDescription(
      title = "Shoot at sight",
      subtitle = "Take your camera",
      type = 1,backgroundImage = "https://images.pexels.com/photos/9037602/pexels-photo-9037602.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2",
      image = "https://cdn-icons-png.flaticon.com/512/5708/5708327.png",
    ))
  })
  
output$infobox<- renderUI({
    req(credentials()$user_auth)
    fluidRow(
      # A static infoBox
      infoBox("Photos Taken", 10 * 2, icon = icon("credit-card"),width = 3),
      # Dynamic infoBoxes
      infoBox("Plastics Detected",width=3,),
      infoBox("Locations covered",width = 3),
    )
    
  })
  
  

  
  
  
  
  
  
  
  #create photo data and login ids
  
  
  
  
}



shinyApp(ui, server)


