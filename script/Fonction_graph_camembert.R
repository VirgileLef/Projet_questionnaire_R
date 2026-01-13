# Fonction_graph_camembert.R
graph_camembert <- function(data, i) {
  plot_data <- as.data.frame(table(data))
  colnames(plot_data) <- c("Categorie", "Effectif")
  
  # Calcul des positions pour les labels
  plot_data <- plot_data %>%
    arrange(desc(Categorie)) %>%
    mutate(
      Proportion = Effectif / sum(Effectif),
      pourcentage_label = paste0(round(Proportion * 100), "%"),
      y_pos = cumsum(Proportion) - 0.5 * Proportion
    )
  
  # Création du graphique
  graph <- ggplot(plot_data, aes(x = "", y = Proportion, fill = Categorie)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    geom_text(aes(y = y_pos, label = pourcentage_label), color = "black", size = 5) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_viridis_d(direction = 1) + 
    ggtitle(paste("Répartition :", i)) +
    labs(fill = "Réponses")
  
  return(graph)
}