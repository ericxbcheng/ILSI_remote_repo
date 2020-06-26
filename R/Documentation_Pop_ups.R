#Documentatation pop ups. 

observeEvent(input$Doc_Bins, {
    showModal(modalDialog(
      title = "Documentatation for Bins",
      includeHTML("R/Docs.html"),
      easyClose = TRUE
      ,size = "l"
    ))
  })