##App where you can enter your ingame name and see where you'd best spend your warforged seals##

###############################TO DO##################################

#source functions

lapply(paste0("fun/",list.files("fun")),source)

initializeApp()
#---------------------------------------------------------------------------------------------------------------------------------------------#

#Variables



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
    
  ),
  skin = "purple"
)


#-----------------------------------------------------------------------------------------------------------------------------------#

##Server##

server <- function(input,output,session){
  
  heroiclist <- readRDS("heroiclist.Rds")
  bossnamelist <- readRDS("heroiclist.Rds")
  item_list <- readRDS("~/R/Robjects/Tauri_app_Joey/item_list.Rds")
  Filter <- item_list[!names(item_list) %like% "Prideful"]
  Filter <- Filter[!names(Filter) %like% "Echoes of War"]
  Filter <- Filter[!names(Filter) %like% "5.4"] # filter dogshit
  Filter <- Filter[!lapply(Filter, "[[", 27) == 608] #filter leg cloaks
  Trinkets <- Filter[lapply(Filter,"[[", 44) %like% "trinket"]
  SOOlist <- Filter[!lapply(Filter, "[[", 44) %like% "trinket"]      ##44 = icon
  
  
  
  
  
  url = "http://chapi.tauri.hu/apiIndex.php?apikey="
  api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
  secret <- "8a80fb7c81fb718a7a80339d689582642974ce12" 
  
  
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
      
      gamestats <- data.frame("Stat" = c("Strength","Agility","Intellect","Spirit","Stamina","Hit","Haste","Mastery","Critical Strike","Itemlevel"), "Value" = 
                                
                                c(data1$characterStat$effective_strength,
                                  data1$characterStat$effective_agility,
                                  data1$characterStat$effective_intellect,
                                  data1$characterStat$effective_spirit,
                                  data1$characterStat$effective_stamina,
                                  data1$characterStat$ranged_hit,
                                  data1$characterStat$spell_haste_rating,
                                  data1$characterStat$mastery_rating,
                                  data1$characterStat$spell_crit_rating,
                                  data1$avgitemlevel) 
      )
      
      
      
      output$stattable <- renderTable({
        gamestats
      })
      
    }
    
  })
  
  #classtag volgens tauri api
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
    
    
    
    if(data2$level < 90){
      output$noob <- (renderImage({list(src = "grindkek.gif", heigth = "100%", width = "100%", align = "center", alt = "B2TG")},
                                 deleteFile = FALSE))
      
      showModal(modalDialog(
        imageOutput("noob"), easyClose = TRUE)
      )
      return()
    } 
  
      
    if(data2$activeSpec == 0){                        
      cls_spec <- data.frame("class" = data2$class, "spec" = data2$treeName_0)           #check active spec bij class
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
    else if(cls_spec[1] %in% c(5,8,9)){
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
    
    
    #zoek entries voor gear
    entries <- data2$characterItems$entry  #(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6,7 nvt, 8=wrist, 9=gloves, 10=belt, 11=legs, 12=feet, 13=ring1, 14=ring2, 15=trinket1, 16=trinket2, 17=mainhand, 18=offhand)
    entries <- entries[entries !=0] #(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6=wrist, 7=gloves, 8=belt, 9=legs, 10=feet, 11=ring1, 12=ring2, 13=trinket1, 14=trinket2, 15=mainhand, 16=offhand)
    
    
    
    
    gearset <- list()
    
    for(j in 1:length(entries)){
      
      par3 <- data.frame(r=input$servername, e = entries[j])
      gearset[[j]] <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret, url2 = "item-tooltip", par = par3))$response
      
    }
    
    
    names(gearset) <- lapply(gearset, "[[",80)
    
    statslist <- (lapply(gearset,"[[",65)) #iteminfo van equipped items
    
    substat <- c("Haste","Mastery","Critical Strike")
    label <- match(input$stat,substat)
    statlabel <-c("Haste","Mastery","Critical") #anders gaat shit mis met crit
    
    
    gearcheck <- lapply(statslist,FUN = function(x){(x %like% statlabel[label])})
    gearcheck2 <- (lapply(gearcheck,"[",3)) #check equipped items voor items zonder gekozen stat
    gearcheck3 <- names(gearcheck2[gearcheck2 %in% "FALSE"]) #slots van deze items
    
    statitems <- SOOlist[names(SOOlist) %in% armorset[,1]] #alle properties van bijbehorende gear
    statitems2 <- lapply(statitems, "[[",65) #sort op itemstats
    statitems3 <- lapply(statitems2, "[", 3) #sort op stat description
    statitems4 <- lapply(statitems3, FUN = function(x){(x %like% statlabel[label])}) #check of item gekozen stat heeft
    statcheck <- names(statitems[statitems4 %in% TRUE]) #alle mogelijke items met gekozen stat
    
    gearnames <- lapply(gearset,"[[",80) #vind de namen van equipped items
    uniqueitems <- statcheck[!(statcheck %in% gearnames)] #alle items met preferred stat die je niet hebt
    
    matchname <- lapply(bossnamelist, FUN = function(x){x %in% uniqueitems}) #match de items die je niet hebt met de droplist zodat je ziet welke boss het dropt
    matchname2 <- names(matchname[matchname %like% TRUE]) #namen van de bosses die het droppen
    
    getstats1 <- SOOlist[names(SOOlist) %in% uniqueitems] 
    getstats2 <- lapply(getstats1, "[[",65) #sort op itemstats
    
    statspos <- lapply(getstats2, '[[',3) #possible item stats
    statspos <- lapply(statspos, function(x){x[which(lengths(x)!=4)][1:4]}) #haal spell/attackpower weg zodat de dimensies in een dataframe passen
    statspos2 <- lapply(statspos, FUN = function(x){(x %like% statlabel[label])})
    statspos3 <- lapply(statspos2, which) #plek waar gekozen stat zit
    
    
    
    val <- lapply(getstats2, '[[',2)  #item statvalues
    val <- lapply(val, function(x){x[which(lengths(x)!=4)][1:4]}) #haal spell/attackpower weg zodat dimensies gelijk blijven voor dataframe
    test <- t(as.data.frame(val))[1:length(statspos3),unlist(statspos3)] #match plek met value
    val2 <- diag(test) # gekozen stat values
    
    itemlabel0 <- lapply(getstats1,"[[",18) #inv type
    
    labelval <- cbind(t(as.data.table(itemlabel0)),val2) #itemtype en value bij elkaar
    
    #hetzelfde voor equipped
    
    getstatseqp2 <- statslist[lengths(statslist)!=0]
    getstatseqp2 <- getstatseqp2[which(as.numeric(unlist(lapply(getstatseqp2,nrow))) > 1)] #geen trinkets
    
    statseqp <- lapply(getstatseqp2, '[[',3)
    statseqp1 <- lapply(statseqp, FUN = function(x){(x %like% statlabel[label])})
    statseqp2 <- lapply(statseqp1, which) #plek waar gekozen stat zit
    
    statseqp3 <- lapply(statseqp, FUN = function(x){(!(x %like% statlabel[label]))})
    statseqp4 <- getstatseqp2[unlist(lapply(statseqp3,all))] #items zonder gekozen stat
    
    valeqp <- lapply(getstatseqp2, '[[',2)  #item statvalues
    
    
    val3 <- vector()
    bindval <- NULL
    temp <- valeqp[names(valeqp) %in% names(statseqp2[lengths(statseqp2)!=0])]
    for(i in 1:length(temp)){
      statseqp2 <- statseqp2[lengths(statseqp2) !=0]
      bindval <- temp[[i]][statseqp2[[i]]]                  #val3 voor items met stat
      val3 <- append(val3,bindval)
      
    }
    
    itemlabel <- lapply(gearset,"[[",18)
    itemlabel2 <- itemlabel[names(itemlabel) %in% names(temp) & itemlabel!=12] #return type
    itemlabel3 <- itemlabel[!(names(itemlabel) %in% names(temp)) & itemlabel!=12] # zonder stat
    
    val4 <- integer(length = length(itemlabel3))
    
    labelval2 <- cbind(c(t(as.data.table(itemlabel2)),t(as.data.table(itemlabel3))),c(val3,val4)) #type + value
    
    #vergelijk type & stats 
    
    #labelval
    DFlv <- as.data.frame(labelval)
    DFlv <- cbind(DFlv,rownames(DFlv))
    names(DFlv) <- c("type","value","item")
    
    #labelval2
    DFlv2 <- as.data.frame(labelval2)
    names(DFlv2) <- c("type", "value")
    #merge per type (itemsoort) 
    matchval <- merge(DFlv,DFlv2, by.x ="type",by.y="type",all.x=FALSE)  #y equipped 
    sort_stat <- matchval[,2] - matchval[,4]   #x=2 y=4
    
    matchval <- cbind(matchval,"Diff"= sort_stat)
    
    prio <- arrange(as.data.frame(matchval), desc(Diff)) #sorteer op stat verschil
    prio <- prio[!duplicated(prio$type),] #Hou alleen 1 van elke type over met het hoogste verschil
    prio <- prio[prio$type!=16,] #filter capes want die zijn dogshit
    
    
    ##trinkets##
    
    #Fury,Arms EEOG/TTT  Prot Vial/TTT
    #Ret EEOG/TTT Prot vial/TTT Holy PPP/DSOD
    #Surv,MM AOC/Haromm BM AOC/TED
    #Combat,Sub AOC/Haromm Assa AOC/TED
    #Shadow PBOI/BBOY Holy PPP/DSOD Disc PBOI/PPP
    #Frost,Unholy EEOG/TTT Blood TTT/SBT
    #Ele PBOI/KTT Enh AOC/Haromm Resto PPP/Thok
    #Arc,Frost PBOI/KTT Fire PBOI/BBOY
    #Demo,Affl PBOI/BBOY Destro PBOI/KTT
    #BM,WW Haromm/TED  MW PPP/Thok
    #Guardian Vial/Haromm Feral RoRo/Haromm Balance PBOI/KTT Resto PPP/Thok
    
    #Groups
    #EEOG/TTT <- fury/arms/frost/unholy/ret
    #PBOI/BBOY <- affl/fire/shadow/demo
    #PBOI/KTT <- ele/arc/frost/destro/balance
    #AOC/haromm <- surv/mm/combat/sub/enh
    #AOC/TED <- BM/assa
    #PPP/DSOD <- holy/holy
    #PPP/PBOI <- disc
    #PPP/Thok <- restp/resto/mw
    #vial/TTT <- prot/prot
    #haromm/TED <- BM/WW
    
    trinkets <- Trinkets[names(gearset)]
    trinkets <- trinkets[!is.na(names(trinkets))]
    spec <- cls_spec[2]
    
    if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Retribution","Arms","Fury","Frost","Unholy")){
      
      trinketBIS <- Trinkets[c(7,14)]
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) #kijk welke je niet hebt
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Protection","Blood")){
      
      
      if((spec=="Protection")==TRUE){
        trinketBIS <- Trinkets[c(7,14)]
      }
      else if((spec=="Blood")==TRUE){
        trinketBIS <- Trinkets[c(14,17)]
      }
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) #kijk welke je niet hebt
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% c(5,7,8,9,11) && cls_spec[2] %in% c("Affliction","Demonology","Fire","Shadow")){
      
      trinketBIS <- Trinkets[c(2,19)]
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    
    else if(cls_spec[1] %in% c(5,7,8,9,11) && cls_spec[2] %in% c("Destruction","Balance","Elemental","Arcane","Frost")){
      
      trinketBIS <- Trinkets[c(2,9)]
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% c(3,4,7) && cls_spec[2] %in% c("Survival","Marksmanship","Combat","Subtlety","Enhancement","Assassination","Beast Mastery")){
      
      if((spec=="Assassination" | spec=="Beast Mastery")==TRUE){
        trinketBIS <- Trinkets[c(1,20)]
      }
      else if(spec %in% c("Survival","Marksmanship","Combat","Subtlety","Enhancement")){ 
        trinketBIS <- Trinkets[c(1,10)]
      }
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% c(2,5,7,10,11) && cls_spec[2] %in% c("Holy","Discipline","Restoration","Mistweaver")){
      
      if((spec=="Holy")==TRUE){
        trinketBIS <- Trinkets[c(8,18)]
      }
      else if((spec=="Discipline")==TRUE){
        trinketBIS <- Trinkets[c(2,8)]
      }
      else if((spec %in% c("Restoration","Mistweaver"))==TRUE){
        trinketBIS <- Trinkets[c(8,13)]
      }
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% 10 && cls_spec[2] %in% c("Brewmaster","Windwalker")){
      trinketBIS <- Trinkets[c(10,20)]
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    else if(cls_spec[1] %in% 11 && cls_spec[2] %in% c("Guardian","Feral")){
      if((spec=="Feral")==TRUE){
        trinketBIS <- append(Trinkets[10], "Rune of Reorigination")
      }
      else if((spec=="Guardian")==TRUE){
        trinketBIS <- Trinkets[10,15]
      }
      TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
      trinketBIS <- names(trinketBIS[TRmiss])
    }
    
    labelval <- as.data.frame(labelval)
    
    if(input$slider!= 0){
      
      if((input$slider-length(trinketBIS)) > 0){
        prio3 <- prio[1:(input$slider-length(trinketBIS)),]
      }
      else{
        prio3 <- NULL
      }
      
      prio2 <- prio3[,2]>prio3[,4] #Als value van recommended gear groter is kies die
      itemnames <- row.names(labelval[labelval[,2] %in% prio3$value.x[which(prio2)],]) #kekw vindt namen  
      
      
      if(length(trinketBIS!=0)){
        itemnames <- append(trinketBIS,itemnames)
      }
      
      
      coinlist <- list()
      coin <- list()
      
      
      BN <- lapply(heroiclist,FUN = function(x){x %in% itemnames})   #welke items uit SOO zijn beter?
      roll <<- names(BN)[unlist(lapply(BN,FUN = function(x){any(x)}))] #welke boss dropt dit
      
      while(length(roll) < input$slider){
        
        priotemp <- prio[!(prio$value.x %in% prio3$value.x),]           #deze loop kijkt of aantal bosses gelijk is aan de seals
        prio <- priotemp                                                #zo niet vult ie aan totdat het zo is 
        prio3 <- priotemp[1:(input$slider-length(roll)),]               
        
        
        ptemp <- prio3[,2]>prio3[,4]
        
        itemnames <- append(itemnames,(row.names(labelval[labelval[,2] %in% prio3$value.x[which(ptemp)],])))
        
        BN <- lapply(heroiclist,FUN = function(x){x %in% itemnames})   
        
        roll <<- names(BN)[unlist(lapply(BN,FUN = function(x){any(x)}))] # <<- want nodig in andere observer 
        
        if(nrow(priotemp)==0){
          break
        }
        
        
      }
      
      
      #andere manier (beter)
      BN2 <<- data.frame("item" = unlist(heroiclist)[unlist(heroiclist) %in% itemnames])
      
      
      for(i in 1:length(names(heroiclist))){
        coinlist[[i]] <- heroiclist[[i]][unlist(BN[i])] #vindt deze items
        
      }
      
      names(coinlist) <- names(heroiclist) 
      coinlist <- coinlist[lengths(coinlist) != 0]
      
      
      
      
      
      for(i in 1:length(names(coinlist))){
        
        coin <- c(coin,
                  
                  list(actionButton(paste0("button",i),names(coinlist)[i])))
      }
      
      coinlist_backup <<- coin
      koppeltabel <<- data.frame("ID" = c(1:14), "button" = c(1:14)) #geef id aan buttons voor ez acces
      
      output$coin <- renderUI({coin})
      
      showModal(
        ui = shiny::modalDialog(size = "l", {
          coin
        }, title = "Coin"), session = session)
      
      if(length(itemnames)==0){
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
    
    browser()
    
  })
  
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
        
        gekozen_button <- names(values)[unlist(values) == 1]
        ID <- koppeltabel[koppeltabel$button == gsub("button", "", gekozen_button),1]
        
        items <- BN2[rownames(BN2) %like% roll[ID],]
        
        
        
        
        output$modaltable <- DT::renderDataTable({
          DT::datatable(as.data.frame(items), escape = FALSE)
        })
        
        showModal( 
          fluidPage( 
            column(width =1,modalDialog(
              
              title = roll[ID],
              dataTableOutput("modaltable"), easyClose = FALSE
            )
            
            ),
            footer = actionButton("dismiss_modal",label = "Dismiss")
          )  )
        
        
      }
    })
  
  
  
}



##run##

shinyApp(ui,server)




#match item met heroiclist

# lapply(heroiclist, FUN = function(y) {x %in% y})  y = list x = gezochte item

# which(unlist(lapply(heroiclist, FUN = function(y) {unlist(x %in% y)}))) ##return bossname voor items met TRUE

# lapply(heroiclist, FUN = function(y) {SOOlist %in% y})
