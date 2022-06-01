# Correr hasta la línea 130
# i = 7
# DF = datos_UPMS %>% filter(estrato_Recod == vctr_estratos_recod[i])
# nombre_estrato = "estrato_Recod"
# nombre_UPM = "upm_virtual"
# panel = paneles
# NumUPM_estrato =  2 * df_tamPobMueEstratos$tamano_estratos_UPM[df_tamPobMueEstratos$estrato_Recod == vctr_estratos_recod[i]] # 2 * tamano_estratos_UPM[i], # Multiplicar por dos debido a las UPMs virtuales
# TamMueUPM_estrato = df_tamPobMueEstratos$tam_mue_estrato_transv[df_tamPobMueEstratos$estrato_Recod == vctr_estratos_recod[i]]

##################### Función  con los siguientes argumentos ###############################

f_escenariosPanel <- function(DF, nombre_estrato, nombre_UPM, 
                              panel, NumUPM_estrato, TamMueUPM_estrato,
                              semilla = 12345){
  
  # DF: dataframe con las siguientes columnas: Estrato, UPM
  # paneles: dataframe con esquema rotativo (las columnas son las letras con las que se construye el esquema)
  # NumUPM_estrato: número de UPMs en el estrato (poblacional)
  # TamMueUPM_estrato: número de UPMs en la muestra en cada estrato 

  # Salidas
  # 1. DF_EstratoPaneles: Data frame de todas las UPMs, 
  # con cada una de las UPM asignada a un panel,
  # número de UPM en la población y en la muestra asignada a un PANEL

  # 2. DF_TamPobPaneles: dataframe con número total de UPMs en cada panel (población)
  # 3. DF_TamMuebPaneles: dataframe con número de  UPMs en cada panel (muestra)
  # 4. InfoConformacionPaneles: vector detalle de conformación de paneles
  # 5. InfoMuePaneles: vector con detalles de selección de la muestra de UPMs en cada panel
  # 6. chequeo_NumUPM_estrato: vector con corrobación de asignación correcta de UPM a los paneles de manera que se respete el número de UPMs por estrato 
  # 7. chequeo_NumUPM_estrato: vector con corrobación de selección  correcta de UPM en los paneles de manera que se respete el tamaño de muestra de UPMs en cada estrato 
  
  DF$Estrato <- DF[[nombre_estrato]]
  DF$UPM <- DF[[nombre_UPM]]
  
  cod_paneles <- sort(as.matrix(panel) %>% as.vector() %>% unique())
  num_paneles_totales <- panel %>% as.matrix() %>% as.vector() %>% unique() %>% length()
  
  num_paneles_periodo <- ncol(panel)
  
  # Función para calcular el número de UPMs en cada panel
  Info_ConformacionUPM <- function(NumUPM_estrato, num_paneles_totales){
    numUPM_paneles_normales <- floor((NumUPM_estrato / num_paneles_totales))
    num_grupos_grandes <- NumUPM_estrato - (numUPM_paneles_normales * num_paneles_totales)
    prue <-  num_grupos_grandes * (numUPM_paneles_normales + 1) +
      (num_paneles_totales - num_grupos_grandes) * numUPM_paneles_normales
    resultado <- c(num_grupos_grandes, num_paneles_totales - num_grupos_grandes, num_paneles_totales,
                   numUPM_paneles_normales + 1, numUPM_paneles_normales)
    names(resultado) <- c("Num_paneles_grandes", "Num_paneles_normales", "Num_paneles",
                          "Num_UPMs_paneles_grandes", "Num_UPMs_paneles_normales")
    resultado
  }
  
  # Función para calcular el tamaño de muestra de cada panel
  Info_tamMuePaneles <- function(TamMueUPM_estrat, Num_paneles_per){
    numUPM_paneles_normales <- floor((TamMueUPM_estrato / Num_paneles_per))
    num_grupos_grandes <- TamMueUPM_estrato - (numUPM_paneles_normales * Num_paneles_per)
    prue <-  num_grupos_grandes * (numUPM_paneles_normales + 1) +
      (Num_paneles_per - num_grupos_grandes) * numUPM_paneles_normales
    resultado <- c(num_grupos_grandes, Num_paneles_per - num_grupos_grandes,
                   Num_paneles_per,
                   numUPM_paneles_normales + 1, numUPM_paneles_normales)
    names(resultado) <- c("Num_paneles_grandes", "Num_paneles_normales", "Num_paneles",
                          "Num_UPMs_paneles_grandes", "Num_UPMs_paneles_normales")
    resultado
  }
  
  ################## Asignar cada uno de los paneles a la tabla de UPMs aleatoriamente ##################
  set.seed(semilla)
  DF$aleatorio <- runif(n = nrow(DF))
  DF <- arrange(DF, aleatorio)
  info <- Info_ConformacionUPM(NumUPM_estrato, num_paneles_totales)
  info <- c(info, nrow(DF))
  names(info)[6] <- "TamUPM_estrato"
  
  # Si hay algunos paneles grandes y otros de tamaño normal
  if(info["Num_paneles_grandes"] != 0){  
    
    # Asignación paneles grandes (con un elemento más)
    upm1 <- rep(cod_paneles[1:info["Num_paneles_grandes"]],
                rep(info["Num_UPMs_paneles_grandes"], info["Num_paneles_grandes"]))
    
    # Asignación paneles normales
    upm2 <- rep(cod_paneles[(info["Num_paneles_grandes"] + 1):(info["Num_paneles_grandes"] + info["Num_paneles_normales"])],
                rep(info["Num_UPMs_paneles_normales"], info["Num_paneles_normales"]))
  }   else { # Si todos son paneles normales
    upm1 <- NULL
    upm2 <- rep(cod_paneles[1:info["Num_paneles_normales"]],
                rep(info["Num_UPMs_paneles_normales"], 
                    info["Num_paneles_normales"]))
  }
  
  DF$Paneles <- c(upm1, upm2)
  
  # Consulta de tamaño poblacional de UPMs en cada panel
  consulta_tamanoPob <-  DF %>% group_by(Paneles) %>% summarise(NpobUPMEnPanel = n())
  DF <- DF %>% group_by(Paneles ) %>% mutate(NpobUPMEnPanel = n())
  
  
  #################### Asignación de tamaño de muestra de paneles ##############
  info_mue <- Info_tamMuePaneles(TamMueUPM_estrato, num_paneles_periodo)
  info_mue <- c(info_mue, TamMueUPM_estrato)
  names(info_mue)[6] <- "TamMueUPM_estrato" 
  
  # Si algunos de los 15 paneles en un trimestre son de tamaño normal y algunos de tamaño grande
  if(info_mue["Num_paneles_grandes"] != 0){
    
    # Elegir tres columnas que corresponden a los paneles grandes 
    indice_selPanelesGrandes <- sample(ncol(paneles), info_mue["Num_paneles_grandes"] )  
    upm1_mue <- paneles[, indice_selPanelesGrandes]  # 1:info_mue["Num_paneles_grandes"]
    upm1_mue <- upm1_mue %>% as.matrix() %>% as.vector() %>% unique()
    
    # Elegir las columnas que corresponden a los paneles medianos
    # (info_mue["Num_paneles_grandes"] + 1):(info_mue["Num_paneles_grandes"] + info_mue["Num_paneles_normales"])
    indices_selPanelesMedios <-  1:(info_mue["Num_paneles_grandes"] + info_mue["Num_paneles_normales"])
    indices_selPanelesMedios <- setdiff(indices_selPanelesMedios, indice_selPanelesGrandes)
    upm2_mue <- paneles[, indices_selPanelesMedios] %>% as.matrix() %>% as.vector() %>% unique()
    
    rep_upm1_mue <- rep(upm1_mue, rep(info_mue["Num_UPMs_paneles_grandes"] %>% as.numeric(), length(upm1_mue)))
    rep_upm2_mue <- rep(upm2_mue, rep(info_mue["Num_UPMs_paneles_normales"] %>% as.numeric(), length(upm2_mue)))
    
    rep_upm_mue <- c(rep_upm1_mue, rep_upm2_mue)
    df_tamMuePaneles <- table(rep_upm_mue)
    df_tamMuePaneles <- df_tamMuePaneles %>% as.data.frame()
    names(df_tamMuePaneles) <- c("Paneles", "nUPMEnPanel")
  } else  {
    # En el caso de que todos los paneles (15) en un trimestre sean de tamaño normal
    upm2_mue <- paneles %>% as.matrix() %>% as.vector() %>% unique()
    
    rep_upm1_mue <- NULL
    rep_upm2_mue <- rep(upm2_mue, rep(info_mue["Num_UPMs_paneles_normales"] %>% as.numeric(), length(upm2_mue)))
    
    rep_upm_mue <- c(rep_upm1_mue, rep_upm2_mue)
    df_tamMuePaneles <- table(rep_upm_mue)
    df_tamMuePaneles <- df_tamMuePaneles %>% as.data.frame()
    names(df_tamMuePaneles) <- c("Paneles", "nUPMEnPanel")
  }
  
  # Integración a tamaño de muestra
  DF <- left_join(DF, df_tamMuePaneles, by = "Paneles")
  
  ######################## Selección aleatorio de las UPM al interior de cada panel ###########################
  set.seed(semilla)
  DF$alea_sel <- runif(n = nrow(DF))
  DF <- DF %>% arrange(Estrato, Paneles, alea_sel)
  
  # Secuencia
  DF <- DF %>% group_by(Estrato, Paneles) %>% mutate(secuencia = 1:n())
  DF$SeleccionUPM <- as.numeric(DF$nUPMEnPanel >= DF$secuencia)
  
  # Agregarle el estrato
  consulta_tamanoPob$Estrato <- unique(DF$Estrato)
  df_tamMuePaneles$Estrato <- unique(DF$Estrato)
  
  consulta_tamanoPob <- consulta_tamanoPob[c("Estrato", "Paneles", "NpobUPMEnPanel")]
  df_tamMuePaneles <- df_tamMuePaneles[c("Estrato", "Paneles", "nUPMEnPanel")]
  
  
  info <- c(unique(DF$Estrato), info)
  names(info)[1] <- "Estrato"
  
  info_mue <- c(unique(DF$Estrato), info_mue)
  names(info_mue)[1] <- "Estrato"
  
  
  ################## Módulo de chequeo ###########################
  # Revisión número de estratos
  if(NumUPM_estrato == as.numeric(info[names(info) == "TamUPM_estrato"]) & 
    NumUPM_estrato == nrow(DF)  & 
    NumUPM_estrato == 
    DF %>% filter(Paneles %in% as.character(unique(paneles %>% as.matrix() %>% as.character()))) %>% 
    group_by(Paneles) %>%
    summarise(NpobUPMEnPanel = max(NpobUPMEnPanel)) %>% pull(NpobUPMEnPanel) %>% sum()
    ) {
    chequeo_NumUPM_estrato <- "OK"
  } else {
    chequeo_NumUPM_estrato <- "Error_NumUPM_estrato"
  }
  
  
  
  vctr_revisionTamMueUPM_estrato <- nrow(paneles) 
  for(k in 1:nrow(paneles)){
  vctr_revisionTamMueUPM_estrato[k] <- DF %>% filter(Paneles %in% as.character(paneles[k,])) %>% group_by(Paneles) %>%
    summarise(nUPMEnPanel = max(nUPMEnPanel)) %>% pull(nUPMEnPanel) %>% sum() == TamMueUPM_estrato
  }
  
  if(sum(vctr_revisionTamMueUPM_estrato) == nrow(paneles)){
    chequeo_TamMueUPM_estrato <- "OK"
  } else {
    chequeo_TamMueUPM_estrato <- "Error_TamMueUPM_estrato"
  }
  
  
  salidas <- list(DF,
                  consulta_tamanoPob, df_tamMuePaneles,
                  info, info_mue, chequeo_NumUPM_estrato, chequeo_TamMueUPM_estrato)
  names(salidas) <- c("DF_EstratoPaneles",
                      "DF_TamPobPaneles", "DF_TamMuebPaneles",
                      "InfoConformacionPaneles", "InfoMuePaneles",
                      "chequeo_NumUPM_estrato", "chequeo_TamMueUPM_estrato")
  salidas
}
