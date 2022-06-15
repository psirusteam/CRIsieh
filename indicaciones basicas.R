
setwd("F:/Documents/CEPAL 2022/Costa Rica/CRIsieh/data")
dir()
encuesta <- haven::read_dta("CRI_2020N1.dta")
pobreza        

labelled::val_labels(encuesta$pobreza)
# Pobreza extrema  Pobreza no extrema Fuera de la pobreza 
#1                   2                   3 

# Pobreza unir 1 y 2

labelled::val_labels(encuesta$condact3)
# 
# labelled::val_labels(encuesta$condact3)
# NA    Ocupado Desocupado   Inactivo      NS/NR 
# -1          1          2          3          9 

# -1 menor a 15 años


length(unique((encuesta$`_upm`)))

length(unique((encuesta$`_estrato`)))

# encuesta$region

# encuesta$area_ee
# 
# Estrato cominar región area_ee


setwd("cri20n1")
dir()
encuesta_grande <- haven::read_dta( "cri20n1.dta" )
encuesta_grande$diseno_muestra # Estrato

table(encuesta_grande$diseno_muestra, encuesta_grande$region)

encuesta_grande$region_area <- paste0(encuesta_grande$region, "_", encuesta_grande$areageo)
table(encuesta_grande$diseno_muestra, encuesta_grande$region)
table(encuesta_grande$diseno_muestra, encuesta_grande$region_area)

intersect(names(encuesta), names(encuesta_grande))

dim(encuesta)
dim(encuesta_grande)

encuesta_grande$factorex %>% sum()
encuesta_grande$condact3 %>% labelled::val_labels()
encuesta_grande$pobreza %>% labelled::val_labels()
encuesta_grande$ipm_pobreza
encuesta_grande$ipm_e1 # carencias

# Hitos

# Tablas de muestreo pobreza monetaria, ipm y desocupación
# Tablas de muestreo escoger un tamaño de muestra que satisfaga los tres indicadores (nI, y num encuestas por UPM)
# Costo (elaborar)
# Cuadrar en el panel rotativo
