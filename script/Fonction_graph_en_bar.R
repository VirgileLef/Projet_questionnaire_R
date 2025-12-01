graph_en_bar <- function(data,i) {
    var <- data
    #tableau de fréquence
    tableau_d_effectif <- table(var)
    
    #graphique en barplot avec ggplot2
    plot_data <- as.data.frame(tableau_d_effectif)
    colnames(plot_data) <- c("Categorie", "Effectif")
    
    graph <- ggplot(plot_data, aes(x = Categorie, y = Effectif)) +
      geom_bar(stat = "identity", fill = "blue") +
      ggtitle(paste("Distribution de", i))
    
    #sauvegarde du graphique dans le dossier output
    return(graph)

}



















