itemTypeValue <- function(Gear_set,Gear_stats,Statlabel,Stat_label,SOOlist,Armor_set){
  
  label <- Statlabel
  statlabel <- Stat_label
  
  #Properties of all the gear
  Stat_items <- SOOlist[names(SOOlist) %in% Armor_set[,1]] 
  #get the stats on the item
  Stat_items <- lapply(Stat_items, "[[",65) 
  #get stat description
  Stat_items <- lapply(Stat_items, "[", 3) 
  #check if item has the chosen stat
  Stat_items_test <- lapply(Stat_items, FUN = function(x){(x %like% statlabel[label])}) 
  #shows all items with chosen stat
  Stat_check <- names(Stat_items[Stat_items_test %in% TRUE]) 
  
  #names of equipped items
  Gear_names <- lapply(Gear_set,"[[",80) 
  #items with chosen stat that you dont have
  Uniq_items <- Stat_check[!(Stat_check %in% Gear_names)] 
  
  Get_items <- SOOlist[names(SOOlist) %in% Uniq_items] 
  #get the stats on these items
  Get_stats <- lapply(Get_items, "[[",65) 
  
  #get description so it is possible to filter spell/attackpower to keep the dimensions equal
  Stat_pos <- lapply(Get_stats, '[[',3) 
  Stat_pos <- lapply(Stat_pos, function(x){x[which(lengths(x)!=4)][1:4]}) 
  Stat_pos <- lapply(Stat_pos, FUN = function(x){(x %like% statlabel[label])})
  #position of the chosen stat
  Stat_pos <- lapply(Stat_pos, which) 
  
  #values of the stats on the item
  Stat_val <- lapply(Get_stats, '[[',2)  
  #remove spell/attackpower again
  Stat_val <- lapply(Stat_val, function(x){x[which(lengths(x)!=4)][1:4]}) 
  #match position with value
  Match_pos <- t(as.data.frame(Stat_val))[1:length(Stat_pos),unlist(Stat_pos)]
  #chosen stat values
  Stat_val <- diag(Match_pos) 
  
  #inventory type (what kind of item)
  Inv_type <- lapply(Get_items,"[[",18) 
  
  #combine type and value
  Item_type_value <- cbind(t(as.data.table(Inv_type)),Stat_val) 
  
  ##same thing for equipped items
  
  Get_stats_eq <- Gear_stats[lengths(Gear_stats)!=0]
  #filter trinkets
  Get_stats_eq <- Get_stats_eq[which(as.numeric(unlist(lapply(Get_stats_eq,nrow))) > 1)] 
  
  Stat_items_eq <- lapply(Get_stats_eq, '[[',3)
  Stat_pos_eq <- lapply(Stat_items_eq, FUN = function(x){(x %like% statlabel[label])})
  Stat_pos_eq <- lapply(Stat_pos_eq, which) 
  
  Stat_pos_eq_wo <- lapply(Stat_items_eq, FUN = function(x){(!(x %like% statlabel[label]))})
  #items without chosen stat
  Stat_pos_eq_wo <- Stat_pos_eq[unlist(lapply(Stat_pos_eq_wo,all))]
  
  #values
  Stat_val_eq <- lapply(Get_stats_eq, '[[',2) 
  
  Stat_val_eq_w <- vector()
  Bind_val <- NULL
  #items with chosen stat
  Temp_stats <- Stat_val_eq[names(Stat_val_eq) %in% names(Stat_pos_eq[lengths(Stat_pos_eq)!=0])]
  
  for(i in 1:length(Temp_stats)){
    Stat_pos_eq <- Stat_pos_eq[lengths(Stat_pos_eq) !=0]
    #find the chosen stat value
    Bind_val <- Temp_stats[[i]][Stat_pos_eq[[i]]] 
    #combine into 1 array
    Stat_val_eq_w <- append(Stat_val_eq_w,Bind_val)
    
  }
  
  #return type
  Inv_type_eq <- lapply(Gear_set,"[[",18)
  
  #types with chosen stat
  Inv_type_eq_w <- Inv_type_eq[names(Inv_type_eq) %in% names(Temp_stats) & Inv_type_eq!=12] 
  #types without chosen stat
  Inv_type_eq_wo <- Inv_type_eq[!(names(Inv_type_eq) %in% names(Temp_stats)) & Inv_type_eq!=12] 
  #assign zeros to values of items without chosen stat
  Stat_val_eq_wo <- integer(length = length(Inv_type_eq_wo))
  
  #combine into one
  Item_type_value_eq <- cbind("type" = c(t(as.data.table(Inv_type_eq_w)),t(as.data.table(Inv_type_eq_wo))),"value" = c(Stat_val_eq_w,Stat_val_eq_wo)) #type + value
  
  return(list(ITV = Item_type_value, ITVQ = Item_type_value_eq))
  
  
}

