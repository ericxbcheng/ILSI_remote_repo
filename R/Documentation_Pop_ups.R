#Documentatation pop ups. 

# 3D ----------------------------------------------------------------------


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

observeEvent(input$Doc_Homogeneity, {
  showModal(modalDialog(
    title = "Documentatation: Homogenity",
    includeHTML("R/Documentation Documents/Homogeneity.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Tuning, {
  showModal(modalDialog(
    title = "Documentatation: Tuning Parameters",
    includeHTML("R/Documentation Documents/Tuning.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})



# 2D ----------------------------------------------------------------------

observeEvent(input$Doc_Field_Man, {
  showModal(modalDialog(
    title = "Documentatation: Field Size",
    includeHTML("R/Documentation Documents/FieldArea.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Geometry_Man, {
  showModal(modalDialog(
    title = "Documentatation: Geometry of Contamination Event",
    includeHTML("R/Documentation Documents/Geometry.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Radius_Man, {
  showModal(modalDialog(
    title = "Documentatation: Radius of Contamination Area",
    includeHTML("R/Documentation Documents/Radius.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Contlevels_Man, {
  showModal(modalDialog(
    title = "Documentatation: Mean and standard deviation of contamination level (log CFU/g)",
    includeHTML("R/Documentation Documents/ContLevels.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Backlevels_Man, {
  showModal(modalDialog(
    title = "Documentatation: Background Levels",
    includeHTML("R/Documentation Documents/Backlevels.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Decay_Man, {
  showModal(modalDialog(
    title = "Documentatation: Decay Function",
    includeHTML("R/Documentation Documents/Decay.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_LimitCont_Man, {
  showModal(modalDialog(
    title = "Documentatation: Limit Contribution",
    includeHTML("R/Documentation Documents/LimitCont.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_Samplepoints_Man, {
  showModal(modalDialog(
    title = "Documentatation: Number of Sample Points",
    includeHTML("R/Documentation Documents/SamplePoints.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})

observeEvent(input$Doc_SamplingStrat_Man, {
  showModal(modalDialog(
    title = "Documentatation: Sampling Strategies",
    includeHTML("R/Documentation Documents/Sampling.html"),
    easyClose = TRUE
    ,size = "l"
  ))
})


