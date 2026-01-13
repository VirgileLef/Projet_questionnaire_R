graph_en_bar <- function(data, i) {
  # On crée le tableau de fréquence
  tableau_freq <- table(data)
  
  # On transforme en data.frame pour ggplot
  plot_data <- as.data.frame(tableau_freq)
  colnames(plot_data) <- c("Categorie", "Effectif")
  
  # On force la catégorie à être du texte
  plot_data$Categorie <- as.character(plot_data$Categorie)
  
  graph <- ggplot(plot_data, aes(x = Categorie, y = Effectif)) +
    geom_bar(stat = "identity", fill = "blue") +
    # On incline les étiquettes si ce sont des dates pour qu'elles soient lisibles
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Distribution de", i)) +
    labs(x = i, y = "Nombre de réponses")
  
  return(graph)
}















