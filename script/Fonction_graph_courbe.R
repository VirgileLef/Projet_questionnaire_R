graph_courbe <- function(data, i) {
  df_temp <- as.data.frame(table(data))
  colnames(df_temp) <- c("Valeur", "Effectif")
  
  # Si c'est une date, on s'assure que R la traite de façon chronologique
  if (inherits(data, "Date")) {
    df_temp$Valeur <- as.Date(as.character(df_temp$Valeur))
  } else if (is.numeric(data)) {
    # Si c'est du numérique, on convertit pour l'ordre des chiffres
    df_temp$Valeur <- as.numeric(as.character(df_temp$Valeur))
  } else {
    # Si c'est du texte, on garde l'ordre alphabétique ou celui du facteur
    df_temp$Valeur <- factor(df_temp$Valeur, levels = unique(df_temp$Valeur))
  }
  
  # Création du graphique
  graph <- ggplot(df_temp, aes(x = Valeur, y = Effectif, group = 1)) +
    geom_line(color = "#2c3e50", linewidth = 1) + 
    geom_point(color = "#e74c3c", linewidth = 2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Évolution / Répartition de :", i)) +
    labs(x = i, y = "Nombre d'occurrences")
  
  if (inherits(data, "Date")) {
    graph <- graph + scale_x_date(date_labels = "%d/%m/%Y")
  }
  
  return(graph)
}