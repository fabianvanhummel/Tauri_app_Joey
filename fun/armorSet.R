armorSet <- function(Armor_type_current,Armor_label,Trinkets){
  
  ##dps plate##
  if(Armor_label == 1){
    
    armorset <- data.frame("dpsplate" = c(names(Trinkets[c(4,7,14,16,17)]), names(Armor_type_current[grepl("^(?=.*Strength)(?!.*Dodge)(?!.*Parry)", Armor_type_current, ignore.case = TRUE, perl = TRUE )])))
  }
  
  ##tank plate##
  else if(Armor_label == 2){
    
    armorset <- data.frame("tankplate" = c(names(Trinkets[c(4,14,15,16,17)]), names(Armor_type_current[grepl("Strength", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##int plate##
  else if(Armor_label == 3){
    
    armorset <- data.frame("intplate" = c(names(Trinkets[c(3,8,13,18)]), names(Armor_type_current[grepl("Intellect", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##agi mail
  else if(Armor_label == 4){
    
    armorset <- data.frame("agimail" = c(names(Trinkets[c(1,10,20)]), names(Armor_type_current[grepl("Agility", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##int mail
  else if(Armor_label == 5){
    
    armorset <-data.frame("intmail" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(Armor_type_current[grepl("Intellect", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##cloth + spirit
  else if(Armor_label == 6){
    
    armorset <- data.frame("spiritcloth" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(Armor_type_current[grepl("Intellect", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##cloth - spirit
  else if(Armor_label == 7){
    
    armorset <- data.frame("cloth" =c(names(Trinkets[c(2,9,19)]),names(Armor_type_current[grepl("^(?=.*Intellect)(?!.*Spirit)", Armor_type_current, ignore.case = TRUE, perl = TRUE)])))
  }
  
  ##agi leather##
  else if(Armor_label == 8){
    
    armorset <- data.frame("agileather" = c(names(Trinkets[c(1,10,20)]),names(Armor_type_current[grepl("Agility", Armor_type_current, ignore.case = TRUE)])))
  }
  
  ##int leather##
  else if(Armor_label == 9){
    
    armorset <- data.frame("intleather" = c(names(Trinkets[c(2,3,8,9,13,18,19)]),names(Armor_type_current[grepl("Intellect", Armor_type_current, ignore.case = TRUE)])))
  }
  
  return(armorset)
}