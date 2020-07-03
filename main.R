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
names <- c("Shruikán","Zhaolong","Juin","Gidan","Ryujinsama")
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
for(i in (5000:10000)) {
  Logs <- RaidLog(url = url, apikey = api_key,secret = secret,url2 = "raid-last", from = 22473,limit = 2)
  
}


## Items
goede_items <- list()
a = 1
for(i in (110000:120000)) {
  par1 <- data.frame(r = '[EN] Evermoon', e = i, expansion = 6)
  playraid <- fromJSON(GetTauri(url,api_key,secret,"item-tooltip",par1))$response
  if(length(playraid) > 0) {
    a = 1
    if(playraid$itemLevel > 527) { 
      print(i)
      goede_items[[playraid$name]] <- playraid
    }
  } else {
    a = a + 1
  }
  
  if(i %% 100 == 0) {
    print(i)
  }
  
  if(a > 10) {
    print("Meer dan 10 NULL's achter elkaar")
  }
  
}

View(playraid$logs)





