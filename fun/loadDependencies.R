loadDependencies <- function(){
  
  heroiclist <- readRDS("heroiclist.Rds")
  item_list <- readRDS("~/R/Robjects/Tauri_app_Joey/item_list.Rds")
  Filter <- item_list[!names(item_list) %like% "Prideful"]
  Filter <- Filter[!names(Filter) %like% "Echoes of War"]
  Filter <- Filter[!names(Filter) %like% "5.4"] # filter dogshit
  Filter <- Filter[!lapply(Filter, "[[", 27) == 608] #filter leg cloaks
  SOOlist <- Filter[!lapply(Filter, "[[", 44) %like% "trinket"]      ##44 = icon
  Trinkets <- Filter[lapply(Filter,"[[", 44) %like% "trinket"]
 
  
  url = "http://chapi.tauri.hu/apiIndex.php?apikey="
  api_key <- "aa7798b3e5032a0c89ce3a4d0ba6dd95"
  secret <- "8a80fb7c81fb718a7a80339d689582642974ce12"
  
  heroiclist
  
  item_list
  
  
  return(list(url=url,api_key=api_key,secret=secret,her = heroiclist, itemlist=item_list,SOO = SOOlist,Trinkets =Trinkets))
}