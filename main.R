## In dit script maken we een body voor een app die raid data van de Tauri API afhaalt

## rogue is ez loot

url = "http://chapi.tauri.hu/apiIndex.php?apikey="
api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"
url_2 <- "character-sheet"
name <- "Lesca"
params <- data.frame(r = '[EN] Evermoon')



library(RCurl)
library(RJSONIO)
library(curl)
library(httr)
library(jsonlite)

##char <- (GetTauri(url = url,secret = secret,apikey = api_key,url2 = url_2,par = params))
##DT<-(fromJSON(char)) maak list

##get ilvls 
names <- c("Shruikán","Zhaolong","Juin","Gidan","Ryujinsama")
ilvls <- GetItemlvl(url,api_key,secret,url_2,names = names)

##raidlogs
#Logs <- RaidLog(url = url, apikey = api_key,secret = secret,url2 = "raid-last", from = 22473,limit = 2)
for(i in 22473:30000){
par1 <- data.frame(r = '[EN] Evermoon',from = i)
playraid <- fromJSON(GetTauri(url,api_key,secret,"raid-last",par1))
}