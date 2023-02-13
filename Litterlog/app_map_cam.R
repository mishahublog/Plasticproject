library(shinydashboard)
library(shiny)
library(leaflet)
library(shinymanager)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)


#Setting up the databases
# userdata
userdata<- gs4_create(name = "vol_data") #create a data
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
  
  
  observeEvent(input$send, {
    require(tidyverse)
    #gather info of all the relevant inputs
    client_df <- c('name', 'dob', 'age', 'phone', 'email') %>% 
      map_dfc(~ tibble("{.x}" := input[[.x]]))
    # print(client_df) #this will print a df with one row in the console
   userbase<-   gs4_get(userdata)
    sheet_append(userbase, data = client_df)
  })
  
  observeEvent(input$camera,{
    
    showModal(modalDialog(
      
      
      
    ))
    
  })
  
  
  
}



shinyApp(ui, server)


  