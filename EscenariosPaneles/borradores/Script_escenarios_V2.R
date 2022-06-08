library(extraDistr)
library(haven)
library(labelled)
#library(plyr)
library(dplyr)


ruta <- "F:/Documents/CEPAL 2022/Costa Rica/EscenariosPaneles"
setwd(ruta)


############# Lectura de funciones ###################
source("funcion/f_paneles.r")
source("funcion/f_escenariosPaneles.r")


marco <- read_spss("MMV_2011 usuarios_Cepal.sav")


###################### Exclusiones ##########################
# Disponer del doble de las 12 viviendas que se realizan en cada UPM

marco <- marco %>% group_by(Num_UPM11) %>% mutate(num_viv_ocup_UpmCompleta = sum(Viv_ocupadas))
marco <- filter(marco, num_viv_ocup_UpmCompleta >= 24)


####################### Esquema panel ############################
paneles <- cbind(panelesRotativos(5, 0, 0, 40), 
                 panelesRotativos(5, 0, 0, 40, letra_inicial = "F"),
                 panelesRotativos(5, 0, 0, 40, letra_inicial = "K"))

num_letras <- paneles %>% as.matrix() %>% as.vector() %>% unique() %>% length()
num_paneles_trimestre <- ncol(paneles)

########################## Arreglo marco de UPMS (Nacional) ##############################

###################### Consulta Tamaño de estratos  ###################
consulta_temp <- marco %>% group_by(estrato_rsns) %>% summarise(cuenta = n()) %>%
            arrange(cuenta)
consulta_temp$Estrato <- consulta_temp$estrato_rsns %>% labelled::to_factor()
rm(consulta_temp)

########################## Recodificaciones ###############################
# Central Resto Alto unirlo con Central Resto Medio
# (código 122 y 123)

marco$estrato_Recod <- ifelse(marco$estrato_rsns %in% c(122, 123), "122-123",
                              marco$estrato_rsns)

estrato_codigo <- c("122-123", "321", "322", "211", "421", "212", "411", "522", "312", 
                    "521", "121", "611", "511", "114", "311", "422", "512", "221", 
                    "412", "222",  "612", "113", "124", "111", "112")

estrato_etiqueta <- c("Central Resto Alto - Medio", "Pac. Central Sur Urbano", "Pac. Central Sur Rural", 
                      "Chorotega Norte Urbano", "Brunca Sur Urbano", "Chorotega Norte Rural", 
                      "Brunca Norte Urbano", "H. Atlántica Sur Rural", "Pac. Central Norte Rural", 
                      "H. Atlántica Sur Urbano", "Central Resto Bajo", "H. Norte Urbano", 
                      "H. Atlántica Norte Urbano", "Central GAM Rural", "Pac. Central Norte Urbano", 
                      "Brunca Sur Rural", "H. Atlántica Norte Rural", "Chorotega Sur Urbano", 
                      "Brunca Norte Rural", "Chorotega Sur Rural", 
                      "H. Norte Rural", "Central GAM Alto", "Central Resto Rural", 
                      "Central GAM Bajo", "Central GAM Medio")


marco$estrato_Recod_etiq <- plyr::mapvalues(marco$estrato_Recod, 
                                            from = estrato_codigo, to = estrato_etiqueta)



consulta_numUpmEstrato <- marco %>% group_by(estrato_Recod) %>% summarise(cuenta = n()) %>%
  arrange(cuenta)

consulta_numUpmEstrato$estrato_Recod_etiq <- plyr::mapvalues(consulta_numUpmEstrato$estrato_Recod, 
                                                             from = estrato_codigo, to = estrato_etiqueta)



############################## Tamaño de muestra de UPMs Universo ###################
# Ver hoja de cálculo 5-Tamaños de muestra_EncuBase_20-04-2022, hoja: TM_Desocupados (12 viviendas)
tam_mue_nacional_transv <- 1868

############### Repartir tamaño de muestra de forma proporcional ################
# Más adalente asignción Kish por ahora proporcional
tam_mue_estrato_transv <- round(tam_mue_nacional_transv *
                                consulta_numUpmEstrato$cuenta / sum(consulta_numUpmEstrato$cuenta))

names(tam_mue_estrato_transv) <- consulta_numUpmEstrato$estrato_Recod_etiq
# n_estrato





########################## Creaciones de UPMs virtuales ########################
# Replicar cada UPM un número especifico de veces
num_particiones_upm <- 2
datos_UPMS <- data.frame(estrato_Recod = marco$estrato_Recod,   estrato_Recod_etiq = marco$estrato_Recod_etiq,
                         Num_UPM11 = marco$Num_UPM11, num_viv_ocup_UpmCompleta = marco$num_viv_ocup_UpmCompleta)

datos_UPMS <- arrange(datos_UPMS, Num_UPM11)

datos_UPMS <- datos_UPMS[rep(row.names(datos_UPMS), each = num_particiones_upm), ]
datos_UPMS <- datos_UPMS %>% group_by(Num_UPM11) %>%  mutate(sectemp = 1:n())
datos_UPMS$upm_virtual <- paste0(datos_UPMS$Num_UPM11,"_", 1:num_particiones_upm)

############################## Cálculos de tamaño por estrato ############################## 
# ################## Consultar el tamaño de muestra y de población de cada estrato
tamano_estratos_UPM <-  consulta_numUpmEstrato$cuenta
names(tamano_estratos_UPM) <- consulta_numUpmEstrato$estrato_Recod_etiq

# # Integrar tamaño de población y de muestra de los estratos de la UPM
df_tamPobMueEstratos <- inner_join(as.data.frame(tamano_estratos_UPM) %>% tibble::rownames_to_column(),
                                   as.data.frame(tam_mue_estrato_transv) %>% tibble::rownames_to_column())
names(df_tamPobMueEstratos)[names(df_tamPobMueEstratos) == "rowname"] <- "estrato_Recod_etiq"


df_tamPobMueEstratos <- df_tamPobMueEstratos %>%
  left_join(consulta_numUpmEstrato %>% select(estrato_Recod, estrato_Recod_etiq),
            by = "estrato_Recod_etiq") %>% select(estrato_Recod, estrato_Recod_etiq,
                                                  tamano_estratos_UPM, tam_mue_estrato_transv)
# Tamaño de muestra por panel
# tam_muestra_panel <- tam_mue_estrato_transv / num_paneles_trimestre
# datos_UPMS %>% group_by(estrato_Recod, estrato_Recod_etiq) %>% 


# Salidas: df_tamPobMueEstratos y datos_UPMS
########################################### FIN conformación UPMs Nacional #####################################################



################################## Cálculos por estrato ##############################
################ Recorrer en cada uno de los estratos ########################### 

# Datos estratos paneles
vctr_estratos_recod <- sort(unique(datos_UPMS$estrato_Recod))

lst_estratoPaneles <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_tampobpaneles <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_tammuebpaneles <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_infoconformacionpaneles <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_infomuepaneles <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_ChequeoTamPobUPM <- vector(mode = "list", length = length(vctr_estratos_recod))
lst_ChequeoTamMueUPM <- vector(mode = "list", length = length(vctr_estratos_recod))


for(i in 1:length(vctr_estratos_recod)){
   
  temp <- (f_escenariosPanel(DF = datos_UPMS %>% 
           filter(estrato_Recod == vctr_estratos_recod[i]),
           nombre_estrato = "estrato_Recod",
           nombre_UPM = "upm_virtual",
           panel = paneles,
           NumUPM_estrato =  2 * df_tamPobMueEstratos$tamano_estratos_UPM[df_tamPobMueEstratos$estrato_Recod == vctr_estratos_recod[i]], # 2 * tamano_estratos_UPM[i], # Multiplicar por dos debido a las UPMs virtuales
          TamMueUPM_estrato = df_tamPobMueEstratos$tam_mue_estrato_transv[df_tamPobMueEstratos$estrato_Recod == vctr_estratos_recod[i]]))
  
  #df_tamPobMueEstratos$tamano_estratos_UPM
  lst_estratoPaneles[[i]] <- temp$DF_EstratoPaneles
  lst_tampobpaneles[[i]] <- temp$DF_TamPobPaneles
  lst_tammuebpaneles[[i]] <- temp$DF_TamMuebPaneles
  lst_infoconformacionpaneles[[i]] <- temp$InfoConformacionPaneles
  lst_infomuepaneles[[i]] <- temp$InfoMuePaneles
  lst_ChequeoTamPobUPM[[i]] <- temp$chequeo_NumUPM_estrato
  lst_ChequeoTamMueUPM[[i]]<- temp$chequeo_TamMueUPM_estrato
  print(paste("Periodo", i))
  }


# Revisión chequeo
unlist(lst_ChequeoTamPobUPM) %>% table()
unlist(lst_ChequeoTamMueUPM) %>% table()


df_estratoPaneles <- bind_rows(lst_estratoPaneles)
df_tampobpaneles <- bind_rows(lst_tampobpaneles)
df_tammuebpaneles <- bind_rows(lst_tammuebpaneles)
df_infoconformacionpaneles <- bind_rows(lst_infoconformacionpaneles)
df_infomuepaneles <- bind_rows(lst_infomuepaneles)

df_estratoPaneles$Estrato_Panel <- paste0(df_estratoPaneles$Estrato, "_",
                                    df_estratoPaneles$Paneles)

df_estratoPaneles <- df_estratoPaneles[c("Estrato_Panel", "Estrato", "UPM", "aleatorio", 
  "Paneles", "NpobUPMEnPanel", 
  "nUPMEnPanel", "alea_sel", "secuencia", "SeleccionUPM")]

