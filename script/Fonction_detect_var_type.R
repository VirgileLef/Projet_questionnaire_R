#script : Integer ou string----
#commentaire : Ce script permet de retourner si les modalités sont integer ou string
#Author : LEFEBVRE Virgile
#Créé le : 15/10/2025
#dernière modification : 19/10/2025
#texte defaut encoding : UTF-8
#R version R : 4.5.1 (2025-06-13 ucrt)

# dans cette autre fichier créer une boucle for pour compter toute les réponses de chacune des variables
# faire une double boucle puis on verra ce qu'on fait

detect_type_var <- function(data) {
  # Création d'un vecteur vide pour y stocker les types des variables
  var_type <- c()
  
  # Parcourir les colonnes
  for (i in 1:ncol(data)) {
    variable <- data[[i]]
    
    if (is.character(variable)) {
      var_type <- c(var_type, "string")
    } else if (is.integer(variable)) {
      var_type <- c(var_type, "integer")
    }  else {
      var_type <- c(var_type, "other")
    }
  }
  
  # Ajouter les noms de colonnes au vecteur de types
  names(var_type) <- colnames(data)
  
  # Retourner le vecteur des types
  return(var_type)
}









