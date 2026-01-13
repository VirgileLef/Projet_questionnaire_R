tableau_d_effectif <- function(data, nom_variable) {
  #tableau de fréquence
  tableau_d_effectif <- table(data)
  tableau_d_effectif <- as.data.frame(tableau_d_effectif)
  return(tableau_d_effectif)
}
tableau_d_effectif <- function(data, nom_variable) {
  #tableau de fréquence
  tableau_d_effectif <- table(data)
  
  #Conversion en data.frame
  tableau_d_effectif <- as.data.frame(tableau_d_effectif)
  
  # Renommage des colonnes
  # Par défaut c'est c("data", "Freq")
  colnames(tableau_d_effectif) <- c("data", "Effectifs")
  
  return(tableau_d_effectif)
}