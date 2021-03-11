takeAction <- function(input,curr.naam){
  
  if(!is.null(input$lastkeypresscode)) {
    
    # Decide which action should be taken
    if(input$naam > curr.naam) {
      # The user pushed the GO actionButton, so take action
      action <- 1
      curr.naam <<- input$naam
    } else if(input$lastkeypresscode == 13) {
      # The user pressed the Enter key, so take action
      action <- 1
    } else {
      # The user did anything else, so do nothing
      action <- 0
      
    }
    return(action)
  } else {
    return(0)
  }
}