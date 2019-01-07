library(dplyr)
library(ggplot2)

drawHistogram <- function(output, df){
  output$barGraph <- renderPlot({
    ggplot(data = df, aes(factor(Element), Soln.Conc, fill=Solution.Label)) +
      geom_bar(stat = "identity", position = 'dodge') +
      scale_fill_brewer(palette = "Set1") +
      xlab("Element") +
      ylab("Concentration") +
      labs(fill = "Sample(s)") +
      coord_flip()
  })
}

# A drop box for the elements.

    column(3,
           selectInput("elem", h3("Choose an element:"), 
                       choices = c("Al","As","Ba","Ca","Cd",
                          "Cu","Co","Cr","Cu","Fe",
                          "K","Mg","Mn","Mo","Ni",
                          "Pb","Se","Sr","Y","Zn"),
                       selected = "Zn")),

# Function to construct a dot-plot based on an element symbol.
# Not sure if my renderPlot syntax is correct, feel free modify as needed.

drawDotplot <- function(i='Al'){
  
  if(!exists("samp.elem")) {
    samp.elem <- getPTValues()
  }else{ break }

  if(dim(samp.elem[[i]]) %in% c(0,NULL)) {
    output$dotPlot <- renderText({
      paste('No statistically significant data points for ',i,'!\n')
      })    
  }else{
    output$dotPlot <- renderPlot({
      ggplot(samp.elem[[i]], aes(x=i, y=solid_conc)) +
        stat_summary() +
        geom_dotplot(aes(colour=factor(element_id)),binaxis='y',stackdir='center', dotsize = 0.25) +
        labs(x="Element",y="Solid Concentration (ppm)",colour="Emission Wavelength")
    })
  }
}