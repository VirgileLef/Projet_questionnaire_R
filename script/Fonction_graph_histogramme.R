graph_histogramme <- function(data, i) {
  # Création d'un dataframe
  df <- data.frame(Valeur = data)
  
  graph <- ggplot(df, aes(x = Valeur)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
    theme_minimal() +
    ggtitle(paste("Distribution de :", i)) +
    labs(x = i, y = "Nombre de personnes")
  
  return(graph)
}