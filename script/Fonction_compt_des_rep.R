compt_des_rep <- function(data) {
  
  # Création d'une copie du dataframe pour travailler dessus
  data_integer <- data
  
  # Boucle sur toutes les colonnes du dataframe
  for (col_name in names(data_integer)) {
    
    # Vérification du type de variable
    # Tester si la variable est de type character
    if (is.character(data_integer[[col_name]]) || is.factor(data_integer[[col_name]])) {
      
      modalities_sorted <- sort(unique(data_integer[[col_name]]))
      
      # Création d'un facteur ordonné
      # On force l'ordre sur un tri alphabétique
      factor_ordered <- factor(
        data_integer[[col_name]],
        levels = modalities_sorted,
        ordered = TRUE
      )
      
      #  Conversion du facteur ordonné en entier (integer)
      # La conversion en numérique d'un facteur ordonné attribue 
      # le score 1 au premier niveau, 2 au second, et ainsi de suite.
      data_integer[[col_name]] <- as.numeric(factor_ordered)
      
    }
  }
  
  # Retourner les data transformé
  return(data_integer)
}
































