GetItemlvl <- function(url,apikey,secret,url2,names,...){
  data <- list()
  ilvl <- vector("double",length = length(names))
  comment <- vector("character",length = length(names))
  dt <- data.frame()
  for(i in 1:length(names)){
    par <- data.frame(r = '[EN] Evermoon', n = names[i] )
    data[[names[i]]] <- fromJSON(GetTauri(url = url,apikey = apikey, secret = secret, url2 = url2, par = par))
    ilvl[i] <- data[[names[i]]]$response$avgitemlevel
    if(ilvl[i]>570){
      comment[i] <- "bigdick"
    } else {
      comment[i] <- "meergrinden"
    }
    
  }
  dt <- data.frame("Name" = names, "ilvl" = ilvl, "judgement" = comment)
  dt <- dt[order(dt$ilvl,decreasing = TRUE),] 
  
  avg_ilvl <- mean(ilvl)
  
  dt <- list("Info" = list(dt),"avgilvl" = avg_ilvl)
  
  # if(i == 1) {
  # dt[i] <- data.frame("Name" = names[i], "ilvl" = ilvl[i], "judgement" = comment[i])
  #} else {
  #  dt <- rbind(dt, data.frame("Name" = names[i], "ilvl" = ilvl[i], "judgement" = comment[i]))
  # }
  sort
  print(dt)
}




