###practice

library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(curl)
library(httr)
library(jsonlite)
library(data.table)
library(DT)
library(shinyBS)
library(shinyjs)

# # This function will create the buttons for the datatable, they will be unique
# shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
# for (i in seq_len(len)) {
#   inputs[i] <- as.character(FUN(paste0(id, i), ...))}
# inputs
# }


#---------------------------------------------------------------------------------------------------------------------#

#Variables

url = "http://chapi.tauri.hu/apiIndex.php?apikey="
api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"







#UI

ui<- dashboardPage(
  dashboardHeader(title = "WoW", titleWidth = 400),
  dashboardSidebar(
    width = 400,
    sidebarMenu(id="tabs",
                sidebarMenu(
                  menuItem("Raids", tabName = "Raids", icon = icon("sliders-h")),
                  menuItem("AH", tabName = "AH", icon = icon("chart-line"))
                )
    ),
    fluidRow(
      box(width = 12,
          selectInput(inputId = "servername",choices = c("[EN] Evermoon","[HU] Tauri WoW Server","[HU] Warriors of Darkness"),multiple = FALSE,label = "Server Name"),
          textInput(inputId = "name", label = "Character Name", placeholder = "Character Name"),
          actionButton("button", "Go"))
    )
    
  ),
  dashboardBody(
    column(width = 6,
           tableOutput("table")),
    fluidRow(
      
      column(width = 1,
             uiOutput('raidlist')),uiOutput("popup")
      
    )
  )
  
)










#---------------------------------------------------------------------------------------------------------------------#


server <- function(input,output,session){
  data1 <- list()
  raids <- list()
  raiddata <- list()
  raidlist <- list()
  logID <- list()
  IDS <- c()
  observeEvent(input$button, {
    
    data1 <- list()
    par <- data.frame( r = input$servername , n = input$name)
    data1 <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret,"character-sheet",par = par))$response
    output$table <- renderTable({data.frame("gear" = data1$characterItems$originalname, "itemlvl" = data1$characterItems$ilevel)
    })
    data1 <<- data1
    
    
    raids <- list()
    par2 <- data.frame(r = input$servername, cn = input$name, from = 0, limit = 0)
    raids <- fromJSON(GetTauri(url,api_key,secret,"raid-player",par2))$response
    raids <<- raids
    
    logID <- head(data.frame(raids$logs$log_id), 20)
    logID <<- logID
    raiddata <- list()
    raidlist <- list()
    for(i in 1:20){
      par3 <- data.frame(r = input$servername, id = logID[i,])
      raiddata <- fromJSON(GetTauri(url,api_key,secret,"raid-log",par3))$response
      ##make action button for each encounter
      
      raidlist <- c(raidlist, 
                    list(actionButton(paste0('ID',logID[i,]), raiddata$encounter_data$encounter_name)))
      
      
      
      
      
      
      
      
    }
    browser()
    output$raidlist <- renderUI({raidlist})
    
  })
  
    observeEvent({
      browser()
      reactiveValuesToList(input[[logID[,1]]])
    },{
      browser()
      par3 <- data.frame(r = input$servername, id = logID[i,])
      raiddata <- fromJSON(GetTauri(url,api_key,secret,"raid-log",par3))$response
      popupdata <- data.frame("name" = raiddata$members$name[order(raiddata$members$dmg_done, decreasing = TRUE)],
                              "dmg" = sort(raiddata$members$dmg_done, decreasing = TRUE))

      toggleModal(session, "modalExample", "open")

      output$popup <- renderUI({bsModal("modalExample",size = "large", renderTable(popupdata))})
    })
  
  
  
  
  
  
  # observeEvent(get(inputs), {
  #   # for(i in 1:length(names(input) %like% "button")) {
  #   #   ID <- gsub("ID", "", "ID1337")}
  #   
  #   browser()
  #   par3 <- data.frame(r = input$servername, id = logID[i,])
  #   browser()
  #   raiddata <- fromJSON(GetTauri(url,api_key,secret,"raid-log",par3))$response
  #   popupdata <- data.frame("name" = raiddata$members$name[order(raiddata$members$dmg_done, decreasing = TRUE)], 
  #                           "dmg" = sort(raiddata$members$dmg_done, decreasing = TRUE))
  #   
  #   toggleModal(session, "modalExample", "open")
  #   
  #   output$popup <- renderUI({bsModal("modalExample",trigger = paste("button",i),size = "large", renderTable(popupdata))}) 
  #   
  # })
  
  
  
  output$tableraid <- renderTable({data.frame(raids$logs$log_id)})
  
  
  
  
  
  
  
  
  
  
  
  
}




shinyApp(ui,server)  