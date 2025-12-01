graph_camembert <- function(data, i) { # data est un vecteur de données pour le camembert et i est le nom de variable
  # Calcule les effectifs
  tableau_d_effectif <- table(data) # Utilisation du vecteur data
  
  # Prépare les données pour ggplot2
  plot_data <- as.data.frame(tableau_d_effectif)
  colnames(plot_data) <- c("Categorie", "Effectif")
  
  # Calcule des proportions
  plot_data <- plot_data %>%
    mutate(
      Proportion = Effectif / sum(Effectif),
      pourcentage_label = paste0(round(Proportion * 100), "%"),
      y_pos = cumsum(Proportion) - 0.5 * Proportion
    )
  
  # Création du graphique en camembert
  graph <- ggplot(plot_data, aes(x = "", y = Proportion, fill = Categorie)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    geom_text(aes(y = y_pos, label = pourcentage_label), color = "black", size = 4) +
    coord_polar("y", start = 0) +
    theme_void() +
    # Utilisation de 'i' pour le titre
    ggtitle(paste("Distribution de", i)) +
    # Utilisation de 'i' pour la légende
    labs(fill = i)
  
  # Sauvegarde du graphique dans le dossier 'output/'
  # Utilisation de 'i' pour le nom du fichier
  return(graph)
}


