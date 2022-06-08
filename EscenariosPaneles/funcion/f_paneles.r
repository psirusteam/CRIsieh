
paneles_rotativos_basicos <- function(A, B, C){
  
  
  length_period <- 2 * (A + B) # El de a corresponde al primer panel b corresponde a los siguientes casos y c las repeticiones
  number_panels <- 1 + (B / A) # el número de paneles que se cuadra en medio de los períodos inacivos y agreglarle uno
  
  if(B == 0) {
    length_period <- A + B
    number_panels <- 1
  } 
  
  
  ciclo_basico <- rep(1:number_panels, rep(A, number_panels))
  
  
  ciclo_basico_rep <- rep(ciclo_basico, 2)
  
  if(B == 0) {
    ciclo_basico_rep <- ciclo_basico
  } 
  
  
  
  num_columnas <- A
  
  lista <- vector(mode = "list", length = num_columnas)
  lista[[1]] <- ciclo_basico_rep
  for(i in 1:(A-1)){
    lista[[i+1]] <- c(dplyr::lead(ciclo_basico_rep, i)[-(length_period:(length_period - (i-1)) )], 
                      rep(number_panels + 1, i))
  }
  
  df_escenario <- as.data.frame(do.call(cbind, lista))
  # names(df_escenario) <- LETTERS[1:ncol(df_escenario)]
  # 
  # for(i in 1:nrow(df_escenario)){
  #   for(j in 1:ncol(df_escenario)){
  #     df_escenario[i,j] <- paste0(colnames(df_escenario)[j],  df_escenario[i,j] )   
  #   }
  # }
  
  
  df_escenario
}

# paneles_rotativos_basicos(4,8,4) # Ok
# paneles_rotativos_basicos(2,2,2) # OK
# paneles_rotativos_basicos(5,0,0)  # OK
# paneles_rotativos_basicos(4,0,0)  # OK
# paneles_rotativos_basicos(2,0,0)  # OK
# paneles_rotativos(6,0,0)
# paneles_rotativos(4,4,4)


# A = 4; B = 8; C = 4; periods = 48; letra_inicial = "A"
panelesRotativos <- function(A, B, C, periods, 
                             letra_inicial = "A"){
  
  panel_basico <-  paneles_rotativos_basicos(A, B, C)     
  num_periodos_basico <- nrow(panel_basico)
  
  
  if(periods < num_periodos_basico){
    panel_resultado <- panel_basico[1:periods,]
  }
  
  
  if(periods == num_periodos_basico){
    panel_resultado <- paneles_rotativos_basicos(A, B, C)
  }
  
  
  if( (periods >  num_periodos_basico) & (periods %% num_periodos_basico) == 0 ){
    repeticiones <- periods / num_periodos_basico
    
    panel_basico <- paneles_rotativos_basicos(A,B,C)
    panel_resultado <- vector(mode = "list", length = repeticiones)
    panel_resultado[[1]] <- panel_basico
    
    for(i in 1:(repeticiones - 1)){
      valorSumar <- max(panel_resultado[[i]][,1])
      panel_resultado[[i+1]] <- panel_basico + valorSumar
    }
    panel_resultado <- bind_rows(panel_resultado)
    
  }
  
  
  
  
  if( (periods >  num_periodos_basico) & (periods %% num_periodos_basico) > 0 ){
    repeticiones <- ceiling(periods / num_periodos_basico)
    
    panel_basico <- paneles_rotativos_basicos(A,B,C)
    panel_resultado <- vector(mode = "list", length = repeticiones)
    panel_resultado[[1]] <- panel_basico
    
    for(i in 1:(repeticiones - 1)){
      valorSumar <- max(panel_resultado[[i]][,1])
      panel_resultado[[i+1]] <- panel_basico + valorSumar
    }
    panel_resultado <- bind_rows(panel_resultado)
    panel_resultado <- panel_resultado[1:periods,]
  }
  
  
  posicion_letra_inicial <- which(LETTERS == letra_inicial)
  letras <- LETTERS[-(1:(posicion_letra_inicial-1))]
  if(letra_inicial == "A") letras <- LETTERS
  
  names(panel_resultado) <- letras[1:ncol(panel_resultado)]
  
  for(i in 1:nrow(panel_resultado)){
    for(j in 1:ncol(panel_resultado)){
      panel_resultado[i,j] <- paste0(colnames(panel_resultado)[j], 
                                     panel_resultado[i,j] )
    }
  }
  
  
  
  panel_resultado
}

#panelesRotativos(4, 8, 3, 72, "A")

# panel1 <- panelesRotativos(5, 0, 0, 40)
# panel2 <- panelesRotativos(5, 0, 0, 40, "F")
# panel3 <- panelesRotativos(5, 0, 0, 40, "K")
