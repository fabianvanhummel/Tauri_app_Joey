## In dit script maken we een body voor een app die raid data van de Tauri API afhaalt

## rogue is ez loot

url = "http://chapi.tauri.hu/apiIndex.php?apikey="
api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"
url_2 <- "character-sheet"
params <- data.frame(r = '[EN] Evermoon')

for(i in list.files("functions/")) {
  source(paste0("functions/", i))
}


library(RCurl)
library(RJSONIO)
library(curl)
library(httr)
library(jsonlite)
library(data.table)

##char <- (GetTauri(url = url,secret = secret,apikey = api_key,url2 = url_2,par = params))
##DT<-(fromJSON(char)) maak list

##get ilvls 
names <- c("Zhaolong","Juin","Gidan","Ryujinsama","Shruikán","Lesca","Zekkzo","Acsel")
ilvls <- GetItemlvl(url,api_key,secret,url_2,names = names)


######### Guild raids

for(i in 647034:647035){
  par1 <- data.frame(r = '[EN] Evermoon', gn = 'Nightdawn',from = 0, limit = 50000)
  playraid <- fromJSON(GetTauri(url,api_key,secret,"raid-guild",par1))$response
}

df_encounters <- data.frame(playraid$logs[!(names(playraid$logs) %in% c("mapentry", "encounter_data", "guilddata"))])
df_encounters <- cbind(df_encounters, playraid$logs$mapentry)
df_encounters <- cbind(df_encounters, playraid$logs$encounter_data)
df_encounters <- cbind(df_encounters, playraid$logs$guilddata)

######### Meer raid info met alleen ID als input
par <- data.frame(r = '[EN] Evermoon', id = 652341)
playraid <- GetTauri(url = url,apikey = api_key,secret = secret,url2 = 'raid-log',par = par)

playraid <- fromJSON(playraid)

playraid <- data.frame(playraid[!(names(playraid) %in% c("mapentry", "encounter_data", "guilddata", "members", "items"))])

### Timestamp naar huidige datum / tijd
as.POSIXct(1593548982, origin="1970-01-01")

##raidlogs
par2 <- data.frame(r= '[EN] Evermoon', from = 552341, limit = 100000)
Logs <- GetTauri(url,api_key,secret,"raid-last",par2)




## Items
# goede_items <- list()
# a = 1
# for(i in (110000:120000)) {
#   par1 <- data.frame(r = '[EN] Evermoon', e = i, expansion = 6)
#   playraid <- fromJSON(GetTauri(url,api_key,secret,"item-tooltip",par1))$response
#   if(length(playraid) > 0) {
#     a = 1
#     if(playraid$itemLevel > 527) { 
#       print(i)
#       goede_items[[playraid$name]] <- playraid
#     }
#   } else {
#     a = a + 1
#   }
#   
#   if(i %% 100 == 0) {
#     print(i)
#   }
#   
#   if(a > 10) {
#     print("Meer dan 10 NULL's achter elkaar")
#   }
#   
# }
# 
# View(playraid$logs)





raidID <- fromJSON(Logs)$response
subset <- raidID$logs[raidID$logs$mapentry$name %in% "Siege of Orgrimmar",]
logid <- subset$log_id

itemlist <- list()


for (i in 1:length(logid)){
  
  par <- data.frame(r= '[EN] Evermoon', id = logid[i])
  rlog <- fromJSON(GetTauri(url,api_key,secret,"raid-log",par))
  itemlist[[as.character(logid[i])]] <- list(rlog$response$encounter_data$encounter_name,rlog$response$items$itemid)
  
}

bosses <- c("Immerseus","Fallen Protectors","Norushen","Sha of Pride","Galakras","Iron Juggernaut","Kor'kron Dark Shaman","General Nazgrim","Malkorok","Spoils of Pandaria","Thok the Bloodthirsty","Siegecrafter Blackfuse","Paragons of the Klaxxi","Garrosh Hellscream")
droplist <- vector("list", length(bosses))
names(droplist) <- bosses

for(i in 1:length(itemlist)){
  boss <- itemlist[[i]][[1]]              ##assign boss uit itemlist
  drops <- itemlist[[i]][[2]]             ##assign drops uit itemlist
  droplist[[boss]] <- c(droplist[[boss]], drops)  ##maak list op met onder boss de drops
  
}
droplisth <- vector("list", length(bosses)) ##heroic
droplistn <- vector("list", length(bosses))  ##normal
names(droplisth) <- bosses
names(droplistn) <- bosses

for (j in 1:length(bosses)) {
  a=1
  b=1
  for(i in 1:lengths(droplist,use.names = FALSE)[j]){                        ##kijk voor alle elementen in droplist[boss]
    par <- data.frame(r= '[EN] Evermoon', e = droplist[[j]][i])
    result <- fromJSON(GetTauri(url,api_key,secret,"item-tooltip",par))
    print(i)
    if(result$response$itemLevel >= 566){
      
      droplisth[[j]][a] <- result$response$item_name
      a <- a+1
    } 
    else if(result$response$itemLevel <= 559){
      
      droplistn[[j]][b] <- result$response$item_name
      b <- b+1
    }
    
  }
  
  
}   


##hou alleen unique

droplisthuni <- lapply(droplisth, unique)
droplistnuni <- lapply(droplistn, unique)
##sorteer alfabetisch
droplisthuni <- lapply(droplisthuni, sort)
droplistnuni <- lapply(droplistnuni, sort)
## GTFO met pets/echos
droplisthuni <- droplisthuni[!(droplisthuni %in% c("Echoes of War","Kovok","Droplet of Y'Shaarj"))]
droplistnuni <- droplistnuni[!(droplistnuni %in% c("Echoes of War","Kovok","Droplet of Y'Shaarj"))]



##saveRDS(droplisthuni,file = "heroiclist.Rds")





##zoek character/get info

names <- "Drimwaz"

par <- data.frame(r = "[EN] Evermoon", n = names)
char <- fromJSON(GetTauri(url = url, apikey = api_key, secret = secret, url2 = "character-sheet",par = par))$response
##check equipped items en check of er een beter item is

ilvl <- char$characterItems$ilevel
item <- char$characterItems$name

eqp <- matrix(c(ilvl,item),ncol = 2) ##zet ilvl en itemname naast elkaar

advice <- vector("numeric",nrow(eqp))
for(i in 1:nrow(eqp)){
  
  if(ilvl[i] >= 588){
    advice[i] <- "BIS"
  }
  else if(ilvl[i]==0){
    "NA"
  }
  else if(ilvl[i]==572 || ilvl[i]==576 || ilvl[i]==580 || ilvl[i]==584){
    advice[i] <- "Upgrade"
  }
  else if(ilvl[i]==566 || ilvl[i]==570 || ilvl[i]==574 || ilvl[i]==578 || ilvl[i]==582){
    advice[i] <- "Upgrade or get heroic warforged"
  }
  else if(ilvl[i]==559 || ilvl[i]==563 || ilvl[i]==567 || ilvl[i]==571 || ilvl[i]==575){
    advice[i] <- "Upgrade or get heroic"
  }
  else if(ilvl[i]==553 || ilvl[i]==557 || ilvl[i]==561 || ilvl[i]==565 || ilvl[i]==569){
    advice[i] <- "Upgrade or get warforged or heroic"
  }
  else if(ilvl[i]==540 || ilvl[i]==544 || ilvl[i]==548 || ilvl[i]==552 || ilvl[i]==556){
    advice[i] <- "Get normal version"
  }
  else if(ilvl[i]>1 && ilvl[i]<540){
    advice[i] <- "do flex"
  }
  
}

overview <- cbind(eqp,"advice" = advice)


################idee##############

##input = name,realm,spec
##ouput = waar kun je het beste rolls gebruiken
 










