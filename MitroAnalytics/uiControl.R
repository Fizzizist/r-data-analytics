source("uiPlotControl.R")
source("uiFilterControl.R")
source("uiAuthControl.R")
source("io.R")
source("stats.R")

loadUI <- function(input, output, session){
  uiReactValues <- reactiveValues(
    loadedStatHist = FALSE,
    loadedIntPlot = FALSE,
    loadedPlotlyPlot = FALSE,
    loadIntRDS = FALSE,
    authenticated = FALSE
  )
  
  session$userData$username <- NULL
  session$userData$sampElem <- NULL
  session$userData$elemNames <- NULL
  session$userData$elemSelected <- NULL
  
  displayLoginView(output, session)
  observeUserLogin(input, output, session, uiReactValues$authenticated)
  observeUserLogout(input, output, session, uiReactValues$authenticated)
  
  observeEvent(input$btnIntSave,
   {
     sampElem <- session$userData$sampElem
     saveUserDataset(sampElem, session$userData$username)
     drawLeftInteractivePlot(output, sampElem, 'Zn')
     drawLeftInteractiveDataTable(output, sampElem)
     drawRightInteractivePlot(input, output, sampElem, 'Zn')
   }
  )
  
  observeEvent(input$btnIntLoad,
   {
     sampElem <- getSampElem(session$userData$username)
     drawLeftInteractivePlot(output, sampElem, 'Zn')
     drawLeftInteractiveDataTable(output, sampElem)
     drawRightInteractivePlot(input, output, sampElem, 'Zn')
   }
  )
  
  observeEvent(input$btnIntReset,
   {
     session$userData$sampElem <- getPTValues()
     renderIntPlotElemFilter(output, names(session$userData$sampElem))
     observeIntPlotSelectElemEvent(input, output, session, session$userData$sampElem)
   }
  )
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statHistogram") return()
      if(uiReactValues$loadedStatHist) return()
      uiReactValues$loadedStatHist <- TRUE
      print("inside hist")
      
      burns <- getBurnList()
      renderHistBurnFilter(output, burns[["burn_id"]])
      observeHistSelectBurnEvent(input, output, session)
    }
  )
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statIntPlot") return()
      if(uiReactValues$loadedIntPlot) return()
      uiReactValues$loadedIntPlot <- TRUE
      
      if(!exists("samp.elem")) {
        samp.elem <- getPTValues()
      }
      renderIntPlotElemFilter(output, names(samp.elem))
      observeIntPlotSelectElemEvent(input, output, session, samp.elem)
    }
  )

observeEvent(input$sidebarMenu, 
      {
        if(input$sidebarMenu != "statPlotly") return()
        if(uiReactValues$loadedPlotlyPlot) return()
        uiReactValues$loadedPlotlyPlot <- TRUE
        
        if(!exists("samp.elem")) {
          samp.elem <- getPTValues()
        }
        renderPoltlyPlotElemFilter(output, names(samp.elem))
        observePlotlyPlotSelectElemEvent(input, output, session, samp.elem)
      }
    )
}