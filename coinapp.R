##App where you can enter your ingame name and see where you'd best spend your warforged seals##

for(i in list.files("functions/")) {
  source(paste0("functions/", i))
}

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

#---------------------------------------------------------------------------------------------------------------------------------------------#

#Variables
bossnamelist <- readRDS("heroiclist.Rds")
item_list <- readRDS("~/R/Robjects/Tauri_app_Joey/item_list.Rds")
Filter1 <- item_list[!names(item_list) %like% "Prideful"]
Filter2 <- Filter1[!names(Filter1) %like% "Echoes of War"]
Filter3 <- Filter2[!names(Filter2) %like% "5.4"] # filter dogshit
Filter4 <- Filter3[!lapply(Filter3, "[[", 27) == 608] #filter leg cloaks
SOOlist <- Filter4[!lapply(Filter4, "[[", 44) %like% "trinket"]      ##44 = icon
SOOlist <- SOOlist[!lapply(SOOlist, "[[", 45) %in% "Weapon"]        ##45 = itemclassname

Trinkets <- Filter4[lapply(Filter4,"[[", 44) %like% "trinket"]
Weapons <- Filter4[lapply(Filter4, "[[", 45) %in% "Weapon"]

url = "http://chapi.tauri.hu/apiIndex.php?apikey="
api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"

#UI

ui <- dashboardPage(
  dashboardHeader(title = "BlessRNG", titleWidth = 400),
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      menuItem("Search", tabName = "Search", icon = icon("search"))
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
             tableOutput("table")),
      column(width = 2, offset = 4,
             tableOutput("stattable"),
             fluidRow(div(id = "Roll",
                          actionButton("roll", "Roll", icon = icon("dice")))
             )
             
      )
      
    )
    
    # fluidRow(
    #   column(width = 1,
    #          div(id = "Roll",
    #              actionButton("roll", "Roll", icon = icon("dice"))
    #          )
    #   )
    
    
    
    
    
    #tags$style("#Roll {align:center;}"))),
  ),
  skin = "purple"
)


#-----------------------------------------------------------------------------------------------------------------------------------#

##Server##

server <- function(input,output,session){
  
  
  
  #curr.val <- "hier komt je shit" # Corresponds to the current displayed input$myinput
  
  curr.naam  <- 0  # Corresponds to the last known GO value (integer)
  
  lastEvent <- reactive({
    # Is reactive to the following events
    input$naam
    input$lastkeypresscode
    
    if(!is.null(input$lastkeypresscode)) {
      
      # Decide which action should be taken
      if(input$naam > curr.naam) {
        # The user pushed the GO actionButton, so take action
        action <- 1
        curr.naam <<- input$naam
      } else if(input$lastkeypresscode == 13) {
        # The user pressed the Enter key, so take action
        action <- 1
      } else {
        # The user did anything else, so do nothing
        action <- 0
        
      }
      return(action)
    } else {
      return(0)
    }
  })
  
  observeEvent(lastEvent(),{
    
    
    
    if(lastEvent()==1){
      
      data1 <- list()
      par1 <- data.frame( r = input$servername , n = input$name)
      data1 <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret,"character-sheet",par = par1))$response
      output$table <- renderTable({
        data.frame("gear" = data1$characterItems$originalname, "itemlvl" = data1$characterItems$ilevel)
      })
      
      gamestats <- data.frame("Stat" = c("Strength","Agility","Intellect","Spirit","Stamina","Hit","Haste","Mastery","Critical Strike"), "Value" = 
                                
                                c(data1$characterStat$effective_strength,
                                  data1$characterStat$effective_agility,
                                  data1$characterStat$effective_intellect,
                                  data1$characterStat$effective_spirit,
                                  data1$characterStat$effective_stamina,
                                  data1$characterStat$ranged_hit,
                                  data1$characterStat$spell_haste_rating,
                                  data1$characterStat$mastery_rating,
                                  data1$characterStat$spell_crit_rating) 
      )
      
      
      
      output$stattable <- renderTable({
        gamestats
      })
      
    }
    
  })
  
  
  # 1 - Warrior
  # 2 - Paladin
  # 3 - Hunter
  # 4 - Rogue
  # 5 - Priest
  # 6 - Death Knight
  # 7 - Shaman
  # 8 - Mage
  # 9 - Warlock
  # 10 - Monk
  # 11 - Druid 
  
  observeEvent(input$roll,{
    
    data2 <- list()
    
    par2 <- data.frame( r = input$servername , n = input$name)
    
    data2 <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret,"character-sheet",par = par2))$response
    
    
    if(data2$activeSpec == 0){
      cls_spec <- data.frame("class" = data2$class, "spec" = data2$treeName_0)
    }
    else{
      cls_spec <- data.frame("class" = data2$class, "spec" = data2$treeName_1)
    }
    
    ###sorteer armor voor spec
    ##dps plate##
    if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Retribution","Arms","Fury","Frost","Unholy")){
      typelabel <- 1
    }
    
    ##tank plate##
    else if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Protection","Blood")){
      typelabel <- 2
    }
    
    ##int plate##
    else if(cls_spec[1] == 2 && cls_spec[2] %in% "Holy"){
      typelabel <-3
    }
    
    ##agi mail##
    else if(cls_spec[1] %in% c(3,7) && cls_spec[2] %in% c("Survival", "Beast Mastery", "Marksmanship","Enhancement")){
      typelabel <- 4
    }
    
    ##int mail##
    else if(cls_spec[1] == 7 && cls_spec[2] %in% c("Restoration", "Elemental")){
      typelabel <- 5
    }
    
    ##cloth+spirit##
    else if(cls_spec[1] == 5){
      typelabel <- 6
    }
    
    ##cloth-spirit##
    else if(cls_spec[1] %in% c(8,9)){
      typelabel <- 7
    }
    
    ##agi leather##
    else if(cls_spec[1] %in% c(4,10,11) && cls_spec[2] %in% c("Combat","Assassination","Subtlety","Feral","Guardian","Brewmaster","Windwalker")){
      typelabel <- 8
    } 
    
    ##int leather##
    else if(cls_spec[1] %in% c(10,11) && cls_spec[2] %in% c("Balance","Restoration","Mistweaver")){
      typelabel <- 9
    }
    
    
    ##sorteer voor class armor##
    if(data2$class %in% c(5,8,9)){
      type <- 1
    }
    else if(data2$class %in% c(4,10,11)){
      type <- 2
    }
    else if(data2$class %in% c(3,7)){
      type <- 3
    }
    else if(data2$class %in% c(1,2,6)){
      type <- 4
    }  
    
    ##type##
    # 1 = cloth
    # 2 = leather
    # 3 = mail
    # 4 = plate
    
    getarmortype <- lapply(SOOlist, "[", c(16,65)) ##16 = armortype 65 = itemstats##
    ARMOR <- getarmortype[!lapply(getarmortype,"[[",1) %in% c(1,2,3,4)[!(c(1,2,3,4) %in% type)]] ##pogchamp code
    
    ##typelabel##
    # 1 = dps plate
    # 2 = tank plate
    # 3 = int plate
    # 4 = agi mail
    # 5 = int mail
    # 6 = cloth spirit
    # 7 = clotg - spirit
    # 8 = agi leather
    # 9 = int leather
    
    ##dps plate##
    if(typelabel == 1){
      
      
      armorset <- data.frame("dpsplate" = c(names(Trinkets[c(4,7,14,16,17)]), names(ARMOR[grepl("^(?=.*Strength)(?!.*Dodge)(?!.*Parry)", ARMOR, ignore.case = TRUE, perl = TRUE )])))
      
    }
    
    ##tank plate##
    else if(typelabel == 2){
      
      armorset <- data.frame("tankplate" = c(names(Trinkets[c(4,14,15,16,17)]), names(ARMOR[grepl("Strength", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##int plate##
    else if(typelabel == 3){
      
      armorset <- data.frame("intplate" = c(names(Trinkets[c(3,8,13,18)]), names(ARMOR[grepl("Intellect", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##agi mail
    else if(typelabel == 4){
      
      armorset <- data.frame("agimail" = c(names(Trinkets[c(1,10,20)]), names(ARMOR[grepl("Agility", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##int mail
    else if(typelabel == 5){
      
      armorset <-data.frame("intmail" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(ARMOR[grepl("Intellect", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##cloth + spirit
    else if(typelabel == 6){
      
      armorset <- data.frame("spiritcloth" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(ARMOR[grepl("Intellect", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##cloth - spirit
    else if(typelabel == 7){
      
      armorset <- data.frame("cloth" =c(names(Trinkets[c(2,9,19)]),names(ARMOR[grepl("^(?=.*Intellect)(?!.*Spirit)", ARMOR, ignore.case = TRUE, perl = TRUE)])))
      
    }
    
    ##agi leather##
    else if(typelabel == 8){
      
      armorset <- data.frame("agileather" = c(names(Trinkets[c(1,10,20)]),names(ARMOR[grepl("Agility", ARMOR, ignore.case = TRUE)])))
      
    }
    
    ##int leather##
    else if(typelabel == 9){
      
      armorset <- data.frame("intleather" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(ARMOR[grepl("Intellect", ARMOR, ignore.case = TRUE)])))
      
    }
    
    
    # output$modaltable <- DT::renderDataTable({
    #   DT::datatable(
    #     colnames =  "Item",
    #     as.data.frame(c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(ARMOR[grepl("Intellect", ARMOR, ignore.case = TRUE)]))
    #                   ,escape = FALSE))
    # })
    
    # showModal(modalDialog(
    #   title = "Coin",
    #   dataTableOutput("modaltable"), easyClose = TRUE
    # ))    
    
    
    
    #zoek entries voor gear
    entries <- data2$characterItems$entry  #(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6,7 nvt, 8=wrist, 9=gloves, 10=belt, 11=legs, 12=feet, 13=ring1, 14=ring2, 15=trinket1, 16=trinket2, 17=mainhand, 18=offhand)
    entries <- entries[entries !=0] #(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6=wrist, 7=gloves, 8=belt, 9=legs, 10=feet, 11=ring1, 12=ring2, 13=trinket1, 14=trinket2, 15=mainhand, 16=offhand)
    
    
    
    
    gearset <- list()
    
    for(j in 1:length(entries)){
      
      par3 <- data.frame(r=input$servername, e = entries[j])
      gearset[[j]] <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret, url2 = "item-tooltip", par = par3))$response
      
    }
    
    input$stat
    
    statslist <- list(lapply(gearset,"[[",65)) #iteminfo van equipped items
    names(statslist[[1]]) <- c("head",
                               "neck",
                               "shoulder",
                               "cape",
                               "chest",
                               "wrist",
                               "gloves",
                               "belt",
                               "legs",
                               "feet",
                               "ring1",
                               "ring2",
                               "trinket1",
                               "trinket2",
                               "mainhand",
                               "offhand")
    
    substat <- c("Haste","Mastery","Critical Strike")
    label <- match(input$stat,substat)
    statlabel <-c("Haste","Mastery","Critical") #anders gaat shit mis met crit
    
    
    gearcheck <- lapply(statslist[[1]],FUN = function(x){(x %like% statlabel[label])})
    gearcheck2 <- (lapply(gearcheck,"[",3)) #check equipped items voor items zonder gekozen stat
    gearcheck3 <- names(gearcheck2[gearcheck2 %in% "FALSE"]) #slots van deze items
    
    statitems <- Filter4[names(Filter4) %in% armorset[,1]] #alle properties van bijbehorende gear
    statitems2 <- lapply(statitems, "[[",65) #sort op itemstats
    statitems3 <- lapply(statitems2, "[", 3) #sort op stat description
    statitems4 <- lapply(statitems3, FUN = function(x){(x %like% statlabel[label])}) #check of item gekozen stat heeft
    statcheck <- names(statitems[statitems4 %in% TRUE]) #alle mogelijke items met gekozen stat
    
    gearnames <- lapply(gearset,"[[",80) #vind de namen van equipped items
    uniqueitems <- statcheck[!(statcheck %in% gearnames)] #alle items met prefered stat die je niet hebt
    
    matchname <- lapply(bossnamelist, FUN = function(x){x %in% uniqueitems}) #match de items die je niet hebt met de droplist zodat je ziet welke boss het dropt
    matchname2 <- names(matchname[matchname %like% TRUE]) #namen van de bosses die het droppen
    
    
    getstats1 <- Filter4[names(Filter4) %in% uniqueitems] 
    getstats2 <- lapply(getstats1, "[[",65) #sort op itemstats
    
    
    kekw <- lapply(getstats2, '[[',3)
    kekw2 <- lapply(kekw, FUN = function(x){(x %like% statlabel[label])})
    kekw3 <- lapply(kekw2, which)
    
    val <- lapply(getstats2, '[[',2)          
    test <- t(as.data.frame(val))[1:length(kekw3),unlist(kekw3)]
    diag(test)
    
    browser()
    
    
    
    
    
    
    
    
    
    
    #head #1
    helmstats <- gearset[[1]]$ItemStat
    
    #necklace #2
    neckstats <- gearset[[2]]$ItemStat
    
    #shoulder #3
    shoulderstats <- gearset[[3]]$ItemStat
    
    #cape #4
    capestats <- gearset[[4]]$ItemStat
    
    #chest #5
    cheststats <- gearset[[5]]$ItemStat
    
    #wrists #6
    wristsstats <- gearset[[6]]$ItemStat
    
    #gloves #7
    glovesstats <- gearset[[7]]$ItemStat
    
    #belt #8
    beltstats <- gearset[[8]]$ItemStat
    
    #legs #9
    legstats <- gearset[[9]]$ItemStat
    
    #feet #10
    feetstats <- gearset[[10]]$ItemStat
    
    #ring1 #11
    ring1stats <- gearset[[11]]$ItemStat
    
    #ring2 #12
    ring2stats <- gearset[[12]]$ItemStat
    
    #trinket1 #13
    #trinket2 #14
    
    
  })
}



##run##

shinyApp(ui,server)



#plans#
#filter items op class
#filter op stat
#check hoeveelheid coins > vergelijk items met minste gewenste stat filter op #coins > laat zien





#match item met heroiclist

# lapply(heroiclist, FUN = function(y) {x %in% y})  y = list x = gezochte item

# which(unlist(lapply(heroiclist, FUN = function(y) {unlist(x %in% y)}))) ##return bossname voor items met TRUE

# lapply(heroiclist, FUN = function(y) {SOOlist %in% y})
