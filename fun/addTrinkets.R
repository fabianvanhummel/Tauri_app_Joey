addTrinkets <- function(Trinkets,cls_spec,Gear_set){
  
  ##trinkets##
  
  #Fury,Arms EEOG/TTT  Prot Vial/TTT
  #Ret EEOG/TTT Prot vial/TTT Holy PPP/DSOD
  #Surv,MM AOC/Haromm BM AOC/TED
  #Combat,Sub AOC/Haromm Assa AOC/TED
  #Shadow PBOI/BBOY Holy PPP/DSOD Disc PBOI/PPP
  #Frost,Unholy EEOG/TTT Blood TTT/SBT
  #Ele PBOI/KTT Enh AOC/Haromm Resto PPP/Thok
  #Arc,Frost PBOI/KTT Fire PBOI/BBOY
  #Demo,Affl PBOI/BBOY Destro PBOI/KTT
  #BM,WW Haromm/TED  MW PPP/Thok
  #Guardian Vial/Haromm Feral RoRo/Haromm Balance PBOI/KTT Resto PPP/Thok
  
  #Groups
  #EEOG/TTT <- fury/arms/frost/unholy/ret
  #PBOI/BBOY <- affl/fire/shadow/demo
  #PBOI/KTT <- ele/arc/frost/destro/balance
  #AOC/haromm <- surv/mm/combat/sub/enh
  #AOC/TED <- BM/assa
  #PPP/DSOD <- holy/holy
  #PPP/PBOI <- disc
  #PPP/Thok <- restp/resto/mw
  #vial/TTT <- prot/prot
  #haromm/TED <- BM/WW
  
  trinkets <- Trinkets[names(Gear_set)]
  trinkets <- trinkets[!is.na(names(trinkets))]
  spec <- cls_spec[2]
  
  if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Retribution","Arms","Fury","Frost","Unholy")){
    
    trinketBIS <- Trinkets[c(7,14)]
    #check which are missing
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% c(1,2,6) && cls_spec[2] %in% c("Protection","Blood")){
    
    
    if((spec=="Protection")==TRUE){
      trinketBIS <- Trinkets[c(14,15)]
    }
    else if((spec=="Blood")==TRUE){
      trinketBIS <- Trinkets[c(14,17)]
    }
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% c(5,7,8,9,11) && cls_spec[2] %in% c("Affliction","Demonology","Fire","Shadow")){
    
    trinketBIS <- Trinkets[c(2,19)]
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  
  else if(cls_spec[1] %in% c(5,7,8,9,11) && cls_spec[2] %in% c("Destruction","Balance","Elemental","Arcane","Frost")){
    
    trinketBIS <- Trinkets[c(2,9)]
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% c(3,4,7) && cls_spec[2] %in% c("Survival","Marksmanship","Combat","Subtlety","Enhancement","Assassination","Beast Mastery")){
    
    if((spec=="Assassination" | spec=="Beast Mastery")==TRUE){
      trinketBIS <- Trinkets[c(1,20)]
    }
    else if(spec %in% c("Survival","Marksmanship","Combat","Subtlety","Enhancement")){ 
      trinketBIS <- Trinkets[c(1,10)]
    }
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% c(2,5,7,10,11) && cls_spec[2] %in% c("Holy","Discipline","Restoration","Mistweaver")){
    
    if((spec=="Holy")==TRUE){
      trinketBIS <- Trinkets[c(8,18)]
    }
    else if((spec=="Discipline")==TRUE){
      trinketBIS <- Trinkets[c(2,8)]
    }
    else if((spec %in% c("Restoration","Mistweaver"))==TRUE){
      trinketBIS <- Trinkets[c(8,13)]
    }
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% 10 && cls_spec[2] %in% c("Brewmaster","Windwalker")){
    trinketBIS <- Trinkets[c(10,20)]
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets))))
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  else if(cls_spec[1] %in% 11 && cls_spec[2] %in% c("Guardian","Feral")){
    if((spec=="Feral")==TRUE){
      trinketBIS <- append(Trinkets[10], "Rune of Reorigination")
    }
    else if((spec=="Guardian")==TRUE){
      trinketBIS <- Trinkets[10,15]
    }
    TRmiss <- which(is.na(match(names(trinketBIS),names(trinkets)))) 
    trinketBIS <- names(trinketBIS[TRmiss])
  }
  
  return(BIS = trinketBIS)
}