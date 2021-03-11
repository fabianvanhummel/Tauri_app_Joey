##App where you can enter your ingame name and see on which boss you would use your warforged seals##


#source functions

lapply(paste0("fun/",list.files("fun")),source)

initializeApp() #load packages
#---------------------------------------------------------------------------------------------------------------------------------------------#

#UI

ui <- dashboardPage(
  dashboardHeader(title = "WoW coinapp", titleWidth = 400),
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      menuItem("Search", text = "Search", icon = icon("search"))
    ),
    fluidRow(
      box(width = 12,
          div(id = "serverlabel", 
              selectInput(inputId = "servername",choices = c("[EN] Evermoon","[HU] Tauri WoW Server","[HU] Warriors of Darkness"),multiple = FALSE,label = "Server Name")),
          tags$style("#serverlabel {color: black}"),
          div(id = "charlabel", 
              textInput(inputId = "name", label = "Character Name", placeholder = "Character Name")),
          tags$style("#charlabel {color: black}"),
          div(id = "stat", 
              selectInput(inputId = "stat",choices = c("Haste","Mastery","Critical Strike"),multiple = FALSE, label = "Main Stat")),
          tags$style("#stat {color: black}"),
          div(id = "WarforgedSeals",
              sliderInput("slider", label = "Warforged Seals", min = 0, max = 10,step = 1, value = 0, ticks = F)),
          tags$style("#WarforgedSeals {color: black}"),
          
          actionButton("naam", "Go"),
          tags$script('$(document).on("keydown", function(e){
          Shiny.onInputChange("lastkeypresscode", e.keyCode);
                    });
                    '),
      )
    ) 
  ), 
  
  dashboardBody(
    fluidRow(
      column(width = 2,
             tableOutput("geartable")),
      column(width = 2, offset = 4,
             tableOutput("stattable"),
             fluidRow(div(id = "Roll",
                          actionButton("roll", "Roll", icon = icon("dice")))
             )
      )
    )
    
  ),
  skin = "purple"
)
#-----------------------------------------------------------------------------------------------------------------------------------#

##Server##


server <- function(input,output,session){
  
  DepList <- loadDependencies()
  
  url <- DepList$url
  api_key <- DepList$api_key
  secret <- DepList$secret
  heroiclist <- DepList$her       #items per boss
  SOOlist <- DepList$SOO          #itemlist filtered by unnecessary items
  Trinkets <- DepList$Trinkets    #list of trinkets
  
  ##
  curr.naam  <- 0  # Corresponds to the last known GO value (integer)
  
  lastEvent <- reactive({
    
    #take action if go button or enter is pressed
    takeAction(input,curr.naam) 
    
  })
  
  observeEvent(lastEvent(),{
    #charactersheet data
    data <- fetchData(input,url,api_key,secret)
  
    if(lastEvent()==1){
      
      #outputs information about the character's gear,stats and itemlevel
      characterInfo(input,output,data)
      
    }  
  })
  
  
  observeEvent(input$roll,{
    
    #take action when roll button is pressed
    data <- fetchData(input,url,api_key,secret) #charactersheet data
   
    #character needs to be level 90, if not return this image
    if(data$level < 90){
      output$backtogrind <- (renderImage({list(src = "grindkek.gif", heigth = "100%", width = "100%", align = "center", alt = "B2TG")},
                                         deleteFile = FALSE))
      
      showModal(modalDialog(
        imageOutput("backtogrind"), easyClose = TRUE)
      )
      return()
    } 
   
    #find item priority
    IPList <- ItemPrio(input,data,cls_spec,SOOlist,url,api_key,secret,Trinkets)
    
    Item_prio_base <- IPList$IP
    Trinket_BIS <- IPList$TB
    DF_rec <- IPList$DFR
    Itv <- IPList$ITV
    ##show items when the slider is not zero
    if(input$slider!= 0){
      
      #always prioritize trinkets
      if((input$slider-length(Trinket_BIS)) > 0){
        Item_prio <- Item_prio_base[1:(input$slider-length(Trinket_BIS)),]
      }
      else{
        Item_prio <- Item_prio_base
      }
      
      #choose the recommended gear if it has higher value
      Item_prio_temp <- Item_prio[,2]>Item_prio[,4] 
      Item_names <- row.names(DF_rec[DF_rec[,2] %in% Item_prio$value.x[which(Item_prio_temp)],])   
      
      #if you are missing trinkets, put them on top
      if(length(Trinket_BIS!=0)){
        Item_names <- append(Trinket_BIS,Item_names)
      }
      
      #apply logic to which items are better
      Boss_items <- lapply(heroiclist,FUN = function(x){x %in% Item_names})  
      #use this logical list to find the names of the bosses that drop *Item_names* items
      Boss_name <<- names(Boss_items)[unlist(lapply(Boss_items,FUN = function(x){any(x)}))] 
      
      #If the slider is bigger than the amount of bosses, keep adding items from the list until 
      #it matches the slider or there are no more items
      while(length(Boss_name) < input$slider){
        #add items from the base
        Prio_temp <- Item_prio_base[!(Item_prio_base$value.x %in% Item_prio$value.x),]  
        
        #save to Item_prio to keep loop going
        Item_prio_base <- Prio_temp
        Item_prio <- Prio_temp[1:(input$slider-length(Boss_name)),]               
        #check if difference is still positive
        P_temp <- Item_prio[,2]>Item_prio[,4]
        
        #make new lists
        Item_names <- append(Item_names,(row.names(Itv[Itv[,2] %in% Item_prio$value.x[which(P_temp)],])))
        Boss_items <- lapply(heroiclist,FUN = function(x){x %in% Item_names})   
        Boss_name <<- names(Boss_items)[unlist(lapply(Boss_items,FUN = function(x){any(x)}))]  
        
        #break loop if there are no more items
        if(nrow(Prio_temp)==0){
          break
        }
      }
      
      #for later use in showing items when you click the boss button
      Boss_items_2 <<- data.frame("item" = unlist(heroiclist)[unlist(heroiclist) %in% Item_names])
      
      #preassign 
      Coin_list <- list()
      Coin <- list()
      #find the items
      for(i in seq_len(length(names(heroiclist)))){
        
        Coin_list[[i]] <- heroiclist[[i]][unlist(Boss_items[i])]
        
      }
      
      names(Coin_list) <- names(heroiclist) 
      Coin_list <- Coin_list[lengths(Coin_list) != 0]
      Coinlist_backup <<- Coin_list
      
      #make actionbuttons per boss
      for(i in seq_len(length(names(Coin_list)))){
        
        Coin <- c(Coin,
                  
                  list(actionButton(paste0("button",i),names(Coin_list)[i])))
      }
      
      Coin_backup <<- Coin
      
      #assign id for each button
      Couple <<- data.frame("ID" = c(1:14), "button" = c(1:14)) 
      
      output$coin <- renderUI({Coin})
      
      showModal(
        ui = shiny::modalDialog(size = "l", {
          Coin
        }, title = "Coin", easyClose = TRUE), session = session)
      
      #show if there are no more upgrades
      if(length(Item_names)==0){
        output$grinder <- renderImage({list(src = "grinder.jpg", heigth = "100%", width = "100%", alt = "FAG")},
                                      deleteFile = FALSE)
        
        showModal(modalDialog(
          imageOutput("grinder"), easyClose = TRUE
        ))
      }
    }
    
    else{
      output$clown <- renderImage({list(src = "Coinslmao.jpg", alt = "Get coins")},
                                  deleteFile = FALSE)
      output$cointxt <- renderText("Get some coins")
      
      showModal(modalDialog(
        fluidPage(column(1, align="center",offset = 3,
                         imageOutput("clown"), 
                         textOutput("cointxt"))),easyClose = TRUE
      ))
    }
  })
  
  #If the boss buttons are clicked show items
  observeEvent(c(
    input$button1,
    input$button2,
    input$button3,
    input$button4,
    input$button5,
    input$button6,
    input$button7,
    input$button8,
    input$button9,
    input$button10,
    input$button11,
    input$button12,
    input$button13,
    input$button14 ),
    {
      values <- reactiveValuesToList(input)
      values <- values[names(values) %like% "button"]
      if(any(unlist(values) == 1)) {
        
        button_choice <- names(values)[unlist(values) == 1]
        ID <- Couple[Couple$button == gsub("button", "", button_choice),1]
        
        items <- Boss_items_2[rownames(Boss_items_2) %like% Boss_name[ID],]
        
        output$items <- DT::renderDataTable({
          DT::datatable(as.data.frame(items), escape = FALSE)
        })
        
        showModal( 
          fluidPage( 
            column(width =1,modalDialog(
              
              title = Boss_name[ID],
              dataTableOutput("items"), easyClose = FALSE,
              footer = actionButton("dismiss_modal",label = "Dismiss")
            ))
          )
        )
      }
      
    })
  
  observeEvent(input$dismiss_modal,{
    
    #If dismissed show the bosses again
    
    output$coin <- renderUI({Coin_backup})
    
    showModal(
      ui = shiny::modalDialog(size = "l", {
        Coin_backup
      }, title = "Coin", easyClose = TRUE), session = session)
  })
}

##run##

shinyApp(ui,server)
