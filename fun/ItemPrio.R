 
ItemPrio <- function(input,data,cls_spec,SOOlist,url,api_key,secret,Trinkets){


#check for class and spec
#classtags according to tauri 
# 1 - Warrior
# 2 - Paladin
# 3 - Hunter
# 4 - Rogue
# 5 - Priest
# 6 - Death Knight
# 7 - Shaman
# 8 - Mage
# 9 - Warlock
# 10 - Monk
# 11 - Druid 

#fetch active spec and combine class
if(data$activeSpec == 0){                        
  cls_spec <- data.frame("class" = data$class, "spec" = data$treeName_0)           
}
else{
  cls_spec <- data.frame("class" = data$class, "spec" = data$treeName_1)
}

#begin sorting the armor

ArmorList <- sortArmor(data,cls_spec)
#What kind of armor is appropriate for current class
Armor_label <- ArmorList$label 
Armor_class <- ArmorList$type  
##16 = armortype 65 = itemstats##
Armor_type <- lapply(SOOlist, "[", c(16,65)) 
##what is the current armortype
Armor_type_current <- Armor_type[!lapply(Armor_type,"[[",1) %in% c(1,2,3,4)[!(c(1,2,3,4) %in% Armor_class)]] 
##armorset for the current spec
Armor_set <- armorSet(Armor_type_current,Armor_label,Trinkets)

##Search entries for gear
#(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6,7 NA, 8=wrist, 9=gloves, 10=belt, 11=legs, 12=feet, 13=ring1, 14=ring2, 15=trinket1, 16=trinket2, 17=mainhand, 18=offhand)
entries <- data$characterItems$entry  
#(1=head, 2=neck, 3=shoulder, 4=cape, 5=chest, 6=wrist, 7=gloves, 8=belt, 9=legs, 10=feet, 11=ring1, 12=ring2, 13=trinket1, 14=trinket2, 15=mainhand, 16=offhand)
entries <- entries[entries !=0] 

#Get information of the equipped gear
Gear_set <- list()

for(j in seq_len(length(entries))){
  
  par <- data.frame(r=input$servername, e = entries[j])
  Gear_set[[j]] <- fromJSON(GetTauri(url = url,apikey = api_key,secret = secret, url2 = "item-tooltip", par = par))$response
  
}
#fetch names
names(Gear_set) <- lapply(Gear_set, "[[",80) 
#item info of equipped items
Gear_stats <- (lapply(Gear_set,"[[",65)) 

##Define chosen stat
Stat_sub <- c("Haste","Mastery","Critical Strike")
Statlabel <- match(input$stat,Stat_sub)
#otherwise applying a %like% will not work for crit
Stat_label <-c("Haste","Mastery","Critical") 

##Combine Itemtype and value of recommended and equipped items in respective dataframes

ItemValTypeList <- itemTypeValue(Gear_set,Gear_stats,Statlabel,Stat_label,SOOlist,Armor_set)

#recommended
Itv  <- ItemValTypeList$ITV
#equipped
Itvq <- ItemValTypeList$ITVQ

##Compare type with stats

#recommended
DF_rec <- as.data.frame(Itv)
#keep names for reference
DF_rec <- cbind(DF_rec,rownames(DF_rec)) 
names(DF_rec) <- c("type","value","item")

#equipped
DF_eq <- as.data.frame(Itvq)
names(DF_eq) <- c("type", "value")

#merge per type  
Match_values <- merge(DF_rec,DF_eq, by.x ="type",by.y="type",all.x=FALSE)  #y equipped 
#calculate difference between values
Sort_stat <- Match_values[,2] - Match_values[,4]   #x=2 y=4
#add the difference column
Match_values <- cbind(Match_values,"Diff"= Sort_stat)

#Items with the highest difference in stats have priority
Item_prio <- arrange(as.data.frame(Match_values), desc(Diff)) 
#keep highest of each type
Item_prio <- Item_prio[!duplicated(Item_prio$type),]
#filter capes
Item_prio_base <- Item_prio[Item_prio$type!=16,] 

#function to assign the best trinkets if you don't have them already

Trinket_BIS <- addTrinkets(Trinkets,cls_spec,Gear_set)

return(list(IP=Item_prio_base,TB=Trinket_BIS,DFR=DF_rec,ITV=Itv))
}

