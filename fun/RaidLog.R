RaidLog <- function(url,apikey,secret,url2,fromid,limit,...){
  
  par <- data.frame(r = '[EN] Evermoon', from = fromid, limit = limit)
  data <- GetTauri(url = url,apikey = apikey,secret = secret,url2 = url2,par = par)

  data <- fromJSON(data)


}