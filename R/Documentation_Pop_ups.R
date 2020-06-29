#Documentatation pop ups. 

observeEvent(input$Doc_Bins, {
    showModal(modalDialog(
      title = "Documentatation: Sizes of Storage equipment",
      includeHTML("R/Documentation Documents/html sizes of storage.html"),
      easyClose = TRUE
      ,size = "l"
    ))
  })


observeEvent(input$Doc_Levels, {
  showModal(modalDialog(
    title = "Documentatation: Permitted mycotoxin levels",
    includeHTML("R/Documentation Documents/Mycotoxin Levels.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Distributions, {
  showModal(modalDialog(
    title = "Documentatation: Effect of Distributions",
    includeHTML("R/Documentation Documents/Distributions.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Clusters, {
  showModal(modalDialog(
    title = "Documentatation: Effect of clustering",
    includeHTML("R/Documentation Documents/Clusters.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Sampling, {
  showModal(modalDialog(
    title = "Documentatation: Effect of Sampling Strategies",
    includeHTML("R/Documentation Documents/Sampling.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Probes, {
  showModal(modalDialog(
    title = "Documentatation: Probe Sizes",
    includeHTML("R/Documentation Documents/Probes.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})