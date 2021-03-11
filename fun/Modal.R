Modal <- function(output,roll,BN2){
  gekozen_button <- names(values)[unlist(values) == 1]
  ID <- koppeltabel[koppeltabel$button == gsub("button", "", gekozen_button),1]
  
  items <- BN2[rownames(BN2) %like% roll[ID],]
  
  
  
  
  output$modaltable <- DT::renderDataTable({
    DT::datatable(as.data.frame(items), escape = FALSE)
  })
  
  showModal( 
    fluidPage( 
      column(width =1,modalDialog(
        
        title = roll[ID],
        dataTableOutput("modaltable"), easyClose = FALSE
      )
      
      ),
      footer = actionButton("dismiss_modal",label = "Dismiss")
    )  )
  return(output)
}