tableau_d_effectif <- function(data, nom_variable) {
  #tableau de fréquence
  tableau_d_effectif <- table(data)
  tableau_d_effectif <- as.data.frame(tableau_d_effectif)
  return(tableau_d_effectif)
}