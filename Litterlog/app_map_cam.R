library(shinydashboard)
library(shiny)
library(leaflet)
library(shinymanager)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)


#Setting up the databases
# userdata
#userdata<- gs4_create(name = "vol_data") #create a data
#Photo data





#App starts
ui <- dashboardPage(
  dashboardHeader(title = "Litter-Log"), # Title
  dashboardSidebar(),
  dashboardBody(

    # Boxes need to be put in a row (or column)
    fillPage(
      leafletOutput("mymap",height = 700) # Adding Map
      ),
    #fab buttons
    fab_button(
      actionButton("show", "Show modal dialog"),
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
#server 
server <- function(input, output) {
 
  output$mymap <- renderLeaflet({ # map positions 
    leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% addTiles()
     })
  #modal 
  observeEvent(input$show,{

    showModal(modalDialog(
      
      dashboardBody(
        
        
        
        # Insery name
        textInputIcon(
          inputId = "name",
          label = "Name"),
        
        # Birth
        dateInput(
          inputId = "dob",
          label ="Birth",
          min = "1960-01-01",
          max = Sys.Date(), format = "yyyy/mm/dd", 
          language = "en"),
        
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
  
  #registartion for new users
  observeEvent(input$send, {
    require(tidyverse)
    #gather info of all the relevant inputs
    client_df <- c('name', 'dob', 'age', 'phone', 'email') %>% 
      map_dfc(~ tibble("{.x}" := input[[.x]]))
    # print(client_df) #this will print a df with one row in the console
   userbase<-   gs4_get(userdata)
    sheet_append(userbase, data = client_df)
  })
  
  #create data from user data for camera input
  users<- data.frame(read_sheet(ss = userdata,range = "A:A"))#collect usernames
  df = data.frame(names=users$Name,photo="",Notes="",Longitude="",Latitude="") 
  
  library(googlesheets4)
  photodata<- gs4_create(name = "metaphoto",sheets = "photo")
  sheet_write(ss = photodata,data = tibble::tibble(df),sheet = "photo")
  # Add camera 
  observeEvent(input$camera,{
    # add model dialogue
    showModal(modalDialog(
      column(width = 8,align="center",
             title = "Title",
      tags$img(src="www/bisi_image.jpeg"),
      fileInput(inputId = "camera",accept = "jpeg",label = "Turn on your camera",capture = "camera",buttonLabel = "camera",placeholder = "No photos taken"),
      sliderInput("slider2", label = h3("Slider Range"), min = 1, 
                  max = 1000, value = c(40, 60)),
      selectInput("select", label = h3("Select shape"), 
                  choices = list("Square" = 1, "rectangle" = 2, "round" = 3), 
                  selected = 1),
      actionButton(inputId = "submit",label = "submit"),
       easyClose = TRUE,
      footer = NULL
      )
      
    ))
    
  })
  
  
  
}



shinyApp(ui, server)


  