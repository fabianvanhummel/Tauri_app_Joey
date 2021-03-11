loadPlayerStats <- function(input,output,data){
  
 
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
  return(output)
}
