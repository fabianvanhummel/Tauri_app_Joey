characterInfo <- function(input,output,data){
  
  
  geartable <- data.frame("gear" = data$characterItems$originalname, "itemlvl" = data$characterItems$ilevel)
  geartable <- geartable[geartable$itemlvl !=0,]
  
  output$geartable <- renderTable({
    geartable
  })
  
  #characters gear, stats and average itemlevel
  gamestats <- data.frame("Stat" = c("Strength","Agility","Intellect","Spirit","Stamina","Hit","Haste","Mastery","Critical Strike","Itemlevel"), "Value" = 
                            
                            c(data$characterStat$effective_strength,
                              data$characterStat$effective_agility,
                              data$characterStat$effective_intellect,
                              data$characterStat$effective_spirit,
                              data$characterStat$effective_stamina,
                              data$characterStat$ranged_hit,
                              data$characterStat$spell_haste_rating,
                              data$characterStat$mastery_rating,
                              data$characterStat$spell_crit_rating,
                              data$avgitemlevel) 
  )
  
  
  
  output$stattable <- renderTable({
    gamestats
  })
  
  return(output)
}