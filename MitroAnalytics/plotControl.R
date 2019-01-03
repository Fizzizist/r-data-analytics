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