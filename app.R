library(shiny)
library(shinyjs)
library(leaflet)
library(shinyauthr)
library(shinymanager)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)
library(googledrive)
library(base64enc)
library(writexl)
#library(gargle)
library(bs4Dash)


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
ui <- dashboardPage(
 
  dashboardHeader(title = "Litter-Log"), # Title
  dashboardSidebar(skin = "dark",
                   #      tags$style(HTML(".sidebar-menu li a { font-size: 100px; color: red}")),
                   sidebarMenu(
                     uiOutput("userpro")
                     
                   )),
  
  dashboardBody(tags$head(
    tags$style(
      "body {overflow-y: hidden;}"
    )
  ),
  
    #conditional panel for login=====
    conditionalPanel(
      condition = ("input.login == 1"),
      tags$style(".login-ui input {background-color: grey  !important; color: white !important; border-color: black !important; }"),
      div(class = "login-ui",
          shinyauthr::loginUI(id = "login",
                                          title = "Please log in", 
                                                  user_title = "User",
                                                  pass_title = "Password", 
                                                  login_title = "Log in",
                                                  error_message = "Incorrect user or password")),
      
      div(class = "pull-right", logoutUI(id = "logout", 
                                         label = "Log out", 
                                         icon = NULL, 
                                         class = "btn-danger",
                                         style = "color: black; 
                                         background-color: darkgoldenrod; border-color: black !important"))
      
      
    ),
    
    
    #fill maps=====
    fillPage(
      leafletOutput("mymap",height = 800) # Adding Map
    ),
    
    #flotting buttons====
    fab_button(
      actionButton("register", "Show registration"),
      actionButton("camera","camera",icon = icon("camera")),
      actionButton(
        inputId = "login",
        label = "Login",
        icon = icon("sign-in")
      ),
      actionButton(
        inputId = "logout",
        label = "Logout",
        icon = icon("sign-out")
      ),
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
               #camera
               uiOutput("image"),
               fileInput("upload", "Upload image", accept = "image/png",capture = "camera"),
               
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
  user_data <- reactive({
    #Get data
    timeid<- str_sub(str_remove_all(Sys.time(),pattern = ":"),start = 1,end = 13)
    photo_meta<- tibble(name=paste(credentials()$info$user,timeid,sep = ""),
                        size=input$slider2,
                        select=input$select,
                        longitude="",Latitude="",photolink="")
    photodata<- rbind(photodata,photo_meta)
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
    file.copy(inFile$datapath, file.path("c:/temp", paste(credentials()$info$user,timeid,".png",sep = "")) )
  })
  
  observeEvent(input$submit, {
    drop_upload(paste(credentials()$info$user,timeid,".png",
                      sep = ""),dtoken = token,mode = "overwrite")
    
    
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
      title = "Alexander Pierce",
      subtitle = "Founder & CEO",
      type = 1,
      image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
    ))
})
  
  
  
  
  
  
  
  #create photo data and login ids
  
  
  
  
}



shinyApp(ui, server)


