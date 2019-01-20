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
  
  observeEvent(input$sidebarMenu, 
    {
      if(input$sidebarMenu != "statHistogram") return()
      if(uiReactValues$loadedStatHist) return()
      uiReactValues$loadedStatHist <- TRUE
      print("inside hist")
      
      burns <- getBurnList()
      renderSelectInput(output, 'selectBurn', 'Select burn:', burns[["burn_id"]])
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
      session$userData$elemNames <- names(samp.elem)
      renderSelectInput(output, 'selectIntElement', "Choose an element:", session$userData$elemNames, 'Zn')
      observeIntPlotSelectElemEvent(input, output, session, samp.elem)
      observeIntPlotBtnEvent(input, output, session)
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
        session$userData$elemNames <- names(samp.elem)
        renderSelectInput(output, 'selectPlotlyPlotElement', "Choose an element:", session$userData$elemNames, 'Zn')
        observePlotlyPlotSelectElemEvent(input, output, session, samp.elem)
      }
    )
}