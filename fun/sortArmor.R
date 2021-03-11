sortArmor <- function(data,cls_spec){
 
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
  
  ##sorteer voor class armor##
  
  ##type##
  # 1 = cloth
  # 2 = leather
  # 3 = mail
  # 4 = plate
  
  if(data$class %in% c(5,8,9)){
    type <- 1  
  }
  else if(data$class %in% c(4,10,11)){
    type <- 2
  }
  else if(data$class %in% c(3,7)){
    type <- 3
  }
  else if(data$class %in% c(1,2,6)){
    type <- 4
  } 
  
  return(list(label = typelabel,type=type))
  
}
