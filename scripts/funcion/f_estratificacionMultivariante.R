####################### Funciones automatizadas ########################

# deff para cada variable
deff_estratificado <-
  function(datos,
           txt_variable_evaluacion,
           txt_estrato) {
    datos[[txt_estrato]] <- as.factor(datos[[txt_estrato]])
    modelo <-
      lm(datos[[txt_variable_evaluacion]]  ~ datos[[txt_estrato]])
    # Y~ auscencia, X: Estrato (factor), datos, la base censarl
    resumen <- summary(modelo)
    deff <- 1 - resumen$r.squared
    return(deff)
  }


# Función Jarque y K means
Estratif_kmeans <- function(Matriz_Estratificacion,
                            num_estratos,
                            variable_evaluacion_orden_estrato,
                            nombre_estrato_Construido,
                            semilla = 12345,
                            inter.max = 100,
                            metodo = c("Jarque", "Kmeans")) {
  X <- Matriz_Estratificacion
  H <- num_estratos
  
  if (metodo == "Jarque") {
    x1 <- scale(X, center = F, scale = apply(X, 2, sd))
    
    set.seed(semilla)
    kmedias <- kmeans(
      x = x1,
      centers = H,
      algorithm  = "MacQueen",
      iter.max = inter.max
    )
  }
  
  if (metodo == "Kmeans") {
    set.seed(semilla)
    kmedias <- kmeans(
      x = X,
      centers = H,
      algorithm  = "MacQueen",
      iter.max = inter.max
    )
  }
  
  vct_estratoJarque <- kmedias$cluster
  
  # Con no hacinado establecer el orden del estrato
  vctr_orden_estrato <-
    sort(tapply(X[[variable_evaluacion_orden_estrato]], vct_estratoJarque, FUN = mean))
  valores_origen <- names(vctr_orden_estrato)
  valores_destino <- 1:H
  estrato <-
    plyr::mapvalues(x = vct_estratoJarque, valores_origen, valores_destino)
  return(estrato)
}
