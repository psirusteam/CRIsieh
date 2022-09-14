
# deff para cada variable
deff_estratificado <-
  function(datos,
           txt_variable_evaluacion,
           txt_estrato) {
    datos[[txt_estrato]] <- as.factor( datos[[txt_estrato]])
    modelo <-
      lm(datos[[txt_variable_evaluacion]]  ~ datos[[txt_estrato]])
    resumen <- summary(modelo)
    deff <- 1 - resumen$r.squared
    return(deff)
  }
