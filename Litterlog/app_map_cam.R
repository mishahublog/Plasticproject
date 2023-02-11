library(shinydashboard)
library(shiny)
library(leaflet)
library(shinymanager)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Litter-Log"),
  dashboardSidebar(),
  dashboardBody(

    # Boxes need to be put in a row (or column)
    fillPage(
      leafletOutput("mymap",height = 700)
      ),
    
    fab_button(
      actionButton("show", "Show modal dialog"),
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

server <- function(input, output) {
 
  output$mymap <- renderLeaflet({
    leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% addTiles()
     })
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
}

shinyApp(ui, server)


  