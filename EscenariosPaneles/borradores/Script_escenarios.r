library(extraDistr)
library(dplyr)
ruta <- "F:/Documents/CEPAL 2022/Costa Rica/EscenariosPaneles"
setwd(ruta)
source("funcion/f_paneles.r")


# Algunos ejemplos prvios
panelesRotativos(2,2,2, 24)

############### Esquema panel ##########################
paneles <- cbind(panelesRotativos(5, 0, 0, 40), 
          panelesRotativos(5, 0, 0, 40, letra_inicial = "F"),
          panelesRotativos(5, 0, 0, 40, letra_inicial = "K"))

num_letras <- paneles %>% as.matrix() %>% as.vector() %>% unique() %>% length()

############# Tamaño poblacional de panel #############
# 5000000 / (150 * 3.128657 * 30)
# 5000000 / 3.128657 # Viviendas
# 5000000 / (3.128657 * 150) # 150 viviendas por UPM
# 10,654 UPMs (10.461 en la la realidad)

5000000 / (3.128657 * 150 * 30) # Número de UPMs en cada uno de los 30 estratos
num_upmEstrato <- 355
num_upm_panel <- num_upmEstrato / num_letras  
# 2,69 UPM por panel

################## Tamaño de muestra de UPMs Universo ##################
tam_mue_nacional_transv <- 3000
num_estratos <- 30
tam_mue_estrato_transv <- tam_mue_nacional_transv / num_estratos

# Tamaño de muestra por panel
tam_muestra_panel <- tam_mue_estrato_transv / 15  # 5 paneles
# Tamaño muestral 6.66 UPMs en cada panel 


######################### Simulación de UPMs ######################
# Dado el bajo número de UPM se devidirán las UPM en 5 
# El tamaño de cada UPM es de 150, se dividirá en 5 a UPMs de tamaño 30

df_UPMs <- data.frame(UPM = c(paste0("UPM",1:num_upmEstrato,"_1"),
                              paste0("UPM",1:num_upmEstrato,"_2"), 
                              paste0("UPM",1:num_upmEstrato,"_3"),
                              paste0("UPM",1:num_upmEstrato,"_4"),
                             paste0("UPM",1:num_upmEstrato,"_5"))  
                      )
df_UPMs$Estrato <- 1

num_upmEstrato <- nrow(df_UPMs) # 5 * 355 = 1775
num_letras <- paneles %>% as.matrix() %>% as.vector() %>% unique() %>% length()

num_upm_panel <- num_upmEstrato / num_letras  

################## Tamaño de muestra de UPMs Universo ##################
tam_mue_nacional_transv <- 3000
num_estratos <- 30
tam_mue_estrato_transv <- tam_mue_nacional_transv / num_estratos

# Tamaño de muestra por panel
tam_muestra_panel <- tam_mue_estrato_transv / 15  # 5 paneles
# Tamaño muestral 6.66 UPMs en cada panel 



# DF: Data frame con dos columnas: estrato  y UPMs
# panel: data.frame con los escenarios rotativos
# NumUPM_estrato: Número de UPMs totales (poblacional) en el estrato
# TamMueUPM_estrato: Número de UPMs a seleccionar en el estrato

# DF <- df_UPMs;  panel <- paneles
# NumUPM_estrato <- nrow(df_UPMs);
# TamMueUPM_estrato <- tam_mue_estrato_transv; semilla = 12345

f_escenariosPanel <- function(DF, panel, NumUPM_estrato, TamMueUPM_estrato,
                              semilla = 12345){
cod_paneles <- sort(as.matrix(panel) %>% as.vector() %>% unique())
num_paneles_totales <- panel %>% as.matrix() %>% as.vector() %>% unique() %>% length()

num_paneles_periodo <- ncol(panel)

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


# num_paneles_totales
# TamMueUPM_estrat = TamMueUPM_estrato; Num_paneles_per = num_paneles_periodo

#TamMueUPM_estrat = NumUPM_estrato; Num_paneles_per = num_paneles_totales

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

if(info["Num_paneles_grandes"] != 0){
  # Asignación paneles grandes (con un elemento más)
  upm1 <- rep(cod_paneles[1:info["Num_paneles_grandes"]],
  rep(info["Num_UPMs_paneles_grandes"], info["Num_paneles_grandes"]))

  # Asignación paneles normales
  upm2 <- rep(cod_paneles[(info["Num_paneles_grandes"] + 1):(info["Num_paneles_grandes"] + info["Num_paneles_normales"])],
  rep(info["Num_UPMs_paneles_normales"], info["Num_paneles_normales"]))
} else {
  upm1 <- NULL
  upm2 <- rep(cod_paneles[1:info["Num_paneles_normales"]],
          rep(info["Num_UPMs_paneles_normales"], 
              info["Num_paneles_normales"]))
}

DF$Paneles <- c(upm1, upm2)

# Tamaños poblacionales
consulta_tamanoPob <-  DF %>% group_by(Paneles ) %>% summarise(NpobUPMEnPanel = n())
DF <- DF %>% group_by(Paneles ) %>% mutate(NpobUPMEnPanel = n())

#################### Asignación de tamaño de muestra de paneles ##############
info_mue <- Info_tamMuePaneles(TamMueUPM_estrato, num_paneles_periodo)

if(info_mue["Num_paneles_grandes"] != 0){
  
upm1_mue <- paneles[,1:info_mue["Num_paneles_grandes"]] %>%
  as.matrix() %>% as.vector() %>% unique()

upm2_mue <- paneles[,(info_mue["Num_paneles_grandes"] + 1):(info_mue["Num_paneles_grandes"] + info_mue["Num_paneles_normales"])] %>% as.matrix() %>% as.vector() %>% unique()

rep_upm1_mue <- rep(upm1_mue, rep(info_mue["Num_UPMs_paneles_grandes"] %>% as.numeric(), length(upm1_mue)))
rep_upm2_mue <- rep(upm2_mue, rep(info_mue["Num_UPMs_paneles_normales"] %>% as.numeric(), length(upm2_mue)))

rep_upm_mue <- c(rep_upm1_mue, rep_upm2_mue)
df_tamMuePaneles <- table(rep_upm_mue)
df_tamMuePaneles <- df_tamMuePaneles %>% as.data.frame()
names(df_tamMuePaneles) <- c("Paneles", "nUPMEnPanel")
}
else
  {
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

# Selección aleatorio
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

  
salidas <- list(DF,
consulta_tamanoPob, df_tamMuePaneles,
info, info_mue)
names(salidas) <- c("DF_EstratoPaneles",
"DF_TamPobPaneles", "DF_TamMuebPaneles",
"InfoConformacionPaneles", "InfoMuePaneles")
salidas
}

escenarios <- f_escenariosPanel(DF = df_UPMs, panel = paneles,
                                NumUPM_estrato = num_upmEstrato, 
                                TamMueUPM_estrato = tam_mue_estrato_transv)



################################## Simulación de 30 estratos ##############################
# Número de UPMs (población) en cada estrato
set.seed(12345)
tamano_estratos_UPM <- rdunif(n = 30, min = 350, max = 360) # 1775 al replicarlo 5 veces

# Tamaño de muestra de UPMs en cada estrato
set.seed(12345)
n_estrato <- rdunif(n = 30, min = 80, max = 120)  # 100

lst_estratosUPMs <- vector(mode = "list", length = length(tamano_estratos_UPM))

for(i in 1:length(tamano_estratos_UPM)){
  lst_estratosUPMs[[i]] <- data.frame(Estrato = i,
                            UPM = c(paste0("UPM", 1:tamano_estratos_UPM[i], "_1"),
                                    paste0("UPM", 1:tamano_estratos_UPM[i], "_2"),
                                    paste0("UPM", 1:tamano_estratos_UPM[i], "_3"),
                                    paste0("UPM", 1:tamano_estratos_UPM[i], "_4"),
                                    paste0("UPM", 1:tamano_estratos_UPM[i], "_5")
                                    ))
}

# Colocar _1,.._5
df_EstratosUPMs <- bind_rows(lst_estratosUPMs)
paneles <- cbind(panelesRotativos(5, 0, 0, 40), 
                 panelesRotativos(5, 0, 0, 40, letra_inicial = "F"),
                 panelesRotativos(5, 0, 0, 40, letra_inicial = "K"))

################ Recorrer en cada uno de los estratos ########################### 

# Datos estratos paneles
vctr_estratos <- sort(unique(df_EstratosUPMs$Estrato))


lst_estratoPaneles <- vector(mode = "list", length = length(vctr_estratos))
lst_tampobpaneles <- vector(mode = "list", length = length(vctr_estratos))
lst_tammuebpaneles <- vector(mode = "list", length = length(vctr_estratos))
lst_infoconformacionpaneles <- vector(mode = "list", length = length(vctr_estratos))
lst_infomuepaneles <- vector(mode = "list", length = length(vctr_estratos))



# escenarios <- f_escenariosPanel(DF = df_UPMs, panel = paneles,
#                                 NumUPM_estrato = num_upmEstrato, 
#                                 TamMueUPM_estrato = tam_mue_estrato_transv)

#i = 1

for(i in 1:length(vctr_estratos)){
   
  temp <- (f_escenariosPanel(DF = df_EstratosUPMs %>% 
           filter(Estrato == vctr_estratos[i]), 
           panel = paneles,
           NumUPM_estrato = 5 * tamano_estratos_UPM[i],
           TamMueUPM_estrato = n_estrato[i]))
  
  lst_estratoPaneles[[i]] <- temp$DF_EstratoPaneles
  lst_tampobpaneles[[i]] <- temp$DF_TamPobPaneles
  lst_tammuebpaneles[[i]] <- temp$DF_TamMuebPaneles
  lst_infoconformacionpaneles[[i]] <- temp$InfoConformacionPaneles
  lst_infomuepaneles[[i]] <- temp$InfoMuePaneles
  }


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

# Resumen

#Tamaño de los estratos
5000000 / (3.128657 * 150 * 30) # Número de UPMs en cada uno de los 30 estratos
# Cada estrato de tamaño 355 (se aleatoriza entre 350 y 360 y se multiplica por 5)
# Los paneles son 132 por lo que los paneles quedaría con muy pocas UPMS y no se alcanza tamaño de muestra

# Tamaño de muestra 3000 UPMs (por trimestre)

# tamaño de muestra por estrato alrededor de 100
3000 / 30

# Las 100 encuestas se reparten en 15 paneles (5 por meses X 3 meses)
100 / 15
# Tamaño de muestra de 6 o 7


a <- df_estratoPaneles %>% filter(Estrato == 1)

# NumUPM_estrato
5 * tamano_estratos_UPM[1] # 357 * 5 = 1785 
# El tamaño del estrato si multiplica por 5, ya que se replican las UPMS

n_estrato[1] # 109, El tamaño de muestra a seleccionar en cada estrato
num_paneles_periodo # 15 paneles en cada período, sobre estos se calcula la muestra


1785 / 132 # 13 o 14 el número de paneles
df_infoconformacionpaneles[1,]

109 / 15 # Tamaño de muestra de 7 u 8

  df_infomuepaneles[1,]
