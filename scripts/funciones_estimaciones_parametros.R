library(rlang)
library(tidyverse)

f_indicadores_str_dominio <- function(str_dominio) {
  # Número promedio de personas por UPMs
  df_promPersUPM <- df_encuesta %>% group_by(!!sym(str_dominio), upm) %>%
    summarise(nII = n()) %>% ungroup() %>% group_by(!!sym(str_dominio)) %>%
    summarise(n_dominio = mean(nII)) %>% 
               arrange(!!sym(str_dominio)) 
  # df_encuesta %>% filter(areageo == 1) %>% group_by(upm) %>% 
  #   summarise(nII = n())
  # 
  df_estima_indicadores <- diseno_pobreza %>% group_by(!!sym(str_dominio)) %>%
    summarize(pobreza = survey_mean(pobreza_monet, na.rm = T, deff = T),
              pobrezaExtr = survey_mean(pobreza_monet_extrema, na.rm = T, deff = T),
              NBI = survey_mean(NBI, na.rm = T, deff = T),
              tasa_desocupacion  = survey_ratio(desocupados, activos, na.rm = T,
                                                deff = T),
              tasa_ocupacion = survey_ratio(ocupados, poblacion_EdadTrabajar2,
                                            na.rm = T,
                                            deff = T),
              tasa_participacion = survey_ratio(activos, poblacion_EdadTrabajar2,
                                                na.rm = T, deff = T),
              N = survey_total(unos),
              N_activos = survey_total(activos, na.rm = T, deff = F)
    ) %>% arrange(!!sym(str_dominio)) %>% left_join(df_promPersUPM) %>% 
         arrange(!!sym(str_dominio))  

  names(df_estima_indicadores)[names(df_estima_indicadores) == "pobreza"] <- "pobreza_prop"
  names(df_estima_indicadores)[names(df_estima_indicadores) == "pobrezaExtr"] <- "pobrezaExtr_prop"
  names(df_estima_indicadores)[names(df_estima_indicadores) == "NBI"] <- "NBI_prop"
  names(df_estima_indicadores)[names(df_estima_indicadores) == "tasa_desocupacion"] <- "tasa_desocupacion_prop"
  names(df_estima_indicadores)[names(df_estima_indicadores) == "tasa_ocupacion"] <- "tasa_ocupacion_prop"
  names(df_estima_indicadores)[names(df_estima_indicadores) == "tasa_participacion"] <- "tasa_participacion_prop"

  conf <- 0.95
  df_estima_indicadores <- df_estima_indicadores %>%
    mutate(pobreza_prop = round(pobreza_prop, 2),
           pobreza_se = round(pobreza_se, 4),
           pobreza_me = round(qnorm(1-(1-conf)/2) * pobreza_se, 4),
           pobreza_mer = round(pobreza_me / pobreza_prop, 4),
           pobreza_cve = round(100 * pobreza_se / pobreza_prop, 1),
           pobreza_LI = pobreza_prop - pobreza_me,
           pobreza_LS = pobreza_prop + pobreza_me,
           pobreza_deff = round(pobreza_deff, 1),
           pobreza_rho = (pobreza_deff - 1) / (n_dominio - 1),
           
           pobrezaExtr_prop = round(pobrezaExtr_prop, 2),
           pobrezaExtr_se = round(pobrezaExtr_se, 4),
           pobrezaExtr_me = round(qnorm(1-(1-conf)/2) * pobrezaExtr_se,4),
           pobrezaExtr_mer = round(pobrezaExtr_me / pobrezaExtr_prop, 4),
           pobrezaExtr_cve = round(100 * pobrezaExtr_se / pobrezaExtr_prop, 1),
           pobrezaExtr_LI = pobrezaExtr_prop - pobrezaExtr_me,
           pobrezaExtr_LS = pobrezaExtr_prop + pobrezaExtr_me,
           pobrezaExtr_deff = round(pobrezaExtr_deff, 1),
           pobrezaExtr_rho = (pobrezaExtr_deff - 1) / (n_dominio - 1),
           
           NBI_prop = round(NBI_prop, 2),
           NBI_se = round(NBI_se, 4),
           NBI_me = round(qnorm(1-(1-conf)/2) * NBI_se,4),
           NBI_mer = round(NBI_me / NBI_prop, 4),
           NBI_cve = round(100 * NBI_se / NBI_prop,1),
           NBI_LI = NBI_prop - NBI_me,
           NBI_LS = NBI_prop + NBI_me,
           NBI_deff = round(NBI_deff, 1),
           NBI_rho = (NBI_deff - 1) / (n_dominio - 1),
           
           tasa_desocupacion_prop = round(tasa_desocupacion_prop, 4),
           tasa_desocupacion_se = round(tasa_desocupacion_se, 4),
           tasa_desocupacion_me = round(qnorm(1-(1-conf)/2) * tasa_desocupacion_se,4),
           tasa_desocupacion_mer = round(tasa_desocupacion_me / tasa_desocupacion_prop, 4),
           tasa_desocupacion_cve = round(100 * tasa_desocupacion_se / tasa_desocupacion_prop, 2),
           tasa_desocupacion_LI = tasa_desocupacion_prop - tasa_desocupacion_me,
           tasa_desocupacion_LS = tasa_desocupacion_prop + tasa_desocupacion_me,
           tasa_desocupacion_deff = round(tasa_desocupacion_deff, 1),
           tasa_desocupacion_rho = (tasa_desocupacion_deff - 1) / (n_dominio - 1),
           
           tasa_ocupacion_prop = round(tasa_ocupacion_prop, 4),
           tasa_ocupacion_se = round(tasa_ocupacion_se, 4),
           tasa_ocupacion_me = round(qnorm(1-(1-conf)/2) * tasa_ocupacion_se,4),
           tasa_ocupacion_mer = round(tasa_ocupacion_me / tasa_ocupacion_prop, 4),
           tasa_ocupacion_cve = round(100 * tasa_ocupacion_se / tasa_ocupacion_prop, 2),
           tasa_ocupacion_LI = tasa_ocupacion_prop - tasa_ocupacion_me,
           tasa_ocupacion_LS = tasa_ocupacion_prop + tasa_ocupacion_me,
           tasa_ocupacion_deff = round(tasa_ocupacion_deff, 1),
           tasa_ocupacion_rho = (tasa_ocupacion_deff - 1) / (n_dominio - 1),
           
           tasa_participacion_prop = round(tasa_participacion_prop, 4),
           tasa_participacion_se = round(tasa_participacion_se, 4),
           tasa_participacion_me = round(qnorm(1-(1-conf)/2) * tasa_participacion_se,4),
           tasa_participacion_mer = round(tasa_participacion_me / tasa_participacion_prop, 4),
           tasa_participacion_cve = round(100 * tasa_participacion_se / tasa_participacion_prop, 2),
           tasa_participacion_LI = tasa_participacion_prop - tasa_participacion_me,
           tasa_participacion_LS = tasa_participacion_prop + tasa_participacion_me,
           tasa_participacion_deff = round(tasa_participacion_deff, 1),
           tasa_participacion_rho = (tasa_participacion_deff - 1) / (n_dominio - 1)
           
    )  %>%
    select(!!sym(str_dominio),
           pobreza_prop, pobreza_se, pobreza_me, pobreza_mer,
           pobreza_cve, pobreza_LI, pobreza_LS, pobreza_deff, pobreza_rho,
           pobrezaExtr_prop, pobrezaExtr_se, pobrezaExtr_me, pobrezaExtr_mer,
           pobrezaExtr_cve, pobrezaExtr_LI, pobrezaExtr_LS, pobrezaExtr_deff, pobrezaExtr_rho,
           NBI_prop, NBI_se, NBI_me, NBI_mer,
           NBI_cve, NBI_LI, NBI_LS, NBI_deff, NBI_rho,
           tasa_desocupacion_prop, tasa_desocupacion_se, tasa_desocupacion_me, tasa_desocupacion_mer,
           tasa_desocupacion_cve, tasa_desocupacion_LI, tasa_desocupacion_LS, tasa_desocupacion_deff, tasa_desocupacion_rho,
           tasa_ocupacion_prop, tasa_ocupacion_se, tasa_ocupacion_me, tasa_ocupacion_mer,
           tasa_ocupacion_cve, tasa_ocupacion_LI, tasa_ocupacion_LS, tasa_ocupacion_deff, tasa_ocupacion_rho,
           tasa_participacion_prop, tasa_participacion_se, tasa_participacion_me, tasa_participacion_mer,
           tasa_participacion_cve, tasa_participacion_LI, tasa_participacion_LS, tasa_participacion_deff, tasa_participacion_rho,
           N_activos, N, n_dominio)
  df_estima_indicadores
}









# En caso de que la proporción sea calculado en un universo
#restringido (por ejemplo, ocupados, se agregan las estadísticas globales
#             en r se especifica la fracción de muestra del dominio)


f_paramGenerales_dominio <- function(str_dominio){
  N <- df_encuesta %>% 
    summarise(N = sum(factorex)) %>%
    pull(N)
  # Para región esta un numero, ojo.
  b <- df_num_perXhog <- df_encuesta %>% group_by(id_hogar) %>%
    summarise(cuenta = n()) %>%
    summarise(num_persXhog = mean(cuenta)) %>% pull(num_persXhog)
  
  df_N_dominio <- df_encuesta %>% group_by(!!sym(str_dominio)) %>% 
    summarise(N_pobreza = sum(factorex)) %>% arrange(!!sym(str_dominio))
  #r_pobreza <-  df_N_dominio$N_pobreza /  N
  r_pobreza <-  1 # ojo
  
  df_N_dominio_activos <- df_encuesta %>% filter(activos == 1) %>% 
    group_by(!!sym(str_dominio)) %>% 
    summarise(N_activos = sum(factorex)) %>% arrange(!!sym(str_dominio))
  #r_activos <-  df_N_dominio_activos$N_activos /  N# Ojo
  r_activos <- 0.4574218
  
  df_N_dominio_edad_trabajar <- df_encuesta %>% filter(poblacion_EdadTrabajar2 == 1) %>% 
    group_by(!!sym(str_dominio)) %>% 
    summarise(N_edad_trabajar = sum(factorex)) %>% arrange(!!sym(str_dominio))
  #r_edad_trabajar <-  df_N_dominio_edad_trabajar$N_edad_trabajar /  N
  r_edad_trabajar <- 0.7969917
  
  lst_ParamGenerales_dominio <- list(N = N, r_pobreza = r_pobreza,
                                     r_activos = r_activos,
                                     r_edad_trabajar = r_edad_trabajar,
                                     b = b)
  
  lst_ParamGenerales_dominio
}

# f_paramGenerales_dominio("region")
# f_paramGenerales_dominio("region")
# f_paramGenerales_dominio("areageo2")


################################# Cálculos nacionales ###################################

#Calcular el margen de error
me_calculo <- function (N, n, b, r, rho, P, conf, m) 
{
  bar.n <- m * r * b
  Deff <- 1 + (bar.n - 1) * rho
  me <- e4p(N, n, P, DEFF = Deff, conf = 0.95)
  as.data.frame(me)
}

##################### Estimación por dominios ######


# r_indicador: r_edad_trabajar, r_pobreza, r_activos
f_calculo_margen_error <- function(n_urbano, n_rural, str_dominio,
                                   confiabilidad = 0.95, m = 6,
                                   str_indicador, r_indicador){
consulta_N_urbano <- df_censo %>% filter(areageo2 == 1) %>% group_by(!!sym(str_dominio)) %>%
  summarise(Nh = n())

consulta_N_rural <- df_censo %>% filter(areageo2 == 2) %>% group_by(!!sym(str_dominio)) %>%
  summarise(Nh = n())


nh_urbano <- n_urbano * (consulta_N_urbano$Nh / sum(consulta_N_urbano)) 
nh_rural <- n_rural * (consulta_N_rural$Nh / sum(consulta_N_rural)) 

nh <- nh_urbano + nh_rural
lst_me_region <- vector(mode = "list", length = length(unique(df_encuesta[[str_dominio]])))

for(i in 1:length(unique(df_encuesta[[str_dominio]]))){
  
lst_me_region[[i]] <- me_calculo(N = f_paramGenerales_dominio(str_dominio)$N,
            n = nh[i], 
            b = f_paramGenerales_dominio(str_dominio)$b, 
            r = f_paramGenerales_dominio(str_dominio)[[r_indicador]],  #[i],# r_pobreza
            rho = f_indicadores_str_dominio(str_dominio)[[paste0(str_indicador,"_rho")]][i],
            P = f_indicadores_str_dominio(str_dominio)[[paste0(str_indicador,"_prop")]][i],
            conf = confiabilidad, m)
}

df_me <- bind_rows(lst_me_region)
df_me
}

# 
# test <- f_calculo_margen_error(n_urbano = 8000, n_rural = 4000,
#                                str_dominio = "region",
#                                str_indicador = "pobreza" ,
#                                r_indicador = "r_pobreza")
# 


# # Cálculo por dominio región
# n_urbano <- 8000
# n_rural <- 4000
# 
# 
# consulta_N_urbano <- df_censo %>% filter(ID_ZONA == 1) %>% group_by(region) %>%
#   summarise(Nh = n())
# 
# consulta_N_rural <- df_censo %>% filter(ID_ZONA == 2) %>% group_by(region) %>%
#   summarise(Nh = n())
# 
# 
# nh_urbano <- n_urbano * (consulta_N_urbano$Nh / sum(consulta_N_urbano)) 
# nh_rural <- n_rural * (consulta_N_rural$Nh / sum(consulta_N_rural)) 
# 
# nh <- nh_urbano + nh_rural
# 
# lst_me_region <- vector(mode = "list", length = length(consulta_N_region$region))
# 
# for(i in 1:length(consulta_N_region$region)){
#   
#   lst_me_region[[i]] <- me_calculo(N = f_paramGenerales_dominio("region")$N,
#                                    n = nh[i], 
#                                    b = f_paramGenerales_dominio("region")$b, 
#                                    r = f_paramGenerales_dominio("region")$r_pobreza[i],
#                                    rho = f_indicadores_str_dominio("region")$pobreza_rho[i],
#                                    P = f_indicadores_str_dominio("region")$pobreza_prop[i],
#                                    conf = 0.95, m = 6)
# }
# 
# df_me <- bind_rows(lst_me_region)


















############## Prueba funciones ##########################################

# df_estima_indicadores_region <- f_indicadores_str_dominio("region")
# df_estima_indicadores_edadR <- f_indicadores_str_dominio("edadR")
# df_estima_indicadores_sexo <- f_indicadores_str_dominio("sexo")
# #df_estima_indicadores_niveduc_ee <- f_indicadores_str_dominio("niveduc_ee") # categorías poco frecuentes
# 

# aa <- f_paramGenerales_dominio(df_estima_indicadores_region, "region")



# 
# vctr_regiones <- sort(unique(a$region))
# 
# M_region <- df_censo %>% group_by(region, UPM2) %>%
#                      summarise(num_personas = n()) %>% 
#                      group_by(region) %>% 
#                      summarise(num_upms = n()) %>% collect() %>%
#                      arrange(region) %>% pull(num_upms)
# 
# N_region <- df_encuesta %>% group_by(region) %>%
#   summarise(N = sum(factorex)) %>% arrange(N) %>%
#   pull(N) 
# 
# 
# df_num_perXhog <- df_encuesta %>% group_by(region, id_hogar) %>%
#                   summarise(cuenta = n()) %>% group_by(region) %>% 
#                   summarise(num_persXhog = mean(cuenta)) %>%
#                   arrange(region)
# 
# b_region <- df_num_perXhog %>% arrange(region) %>% pull(num_persXhog)
# 
# 
# nII_hog_region <- vector(mode = "numeric", length = length(vctr_regiones))
# nII_pers_region <- vector(mode = "numeric", length = length(vctr_regiones))
# rho_Urbana_region <- vector(mode = "numeric", length = length(vctr_regiones))
# 
# for(i in 1:length(vctr_regiones)){
#   nII_hog_region[vctr_regiones[i]] <- df_encuesta %>% 
#     filter(region == vctr_regiones[i]) %>% 
#     group_by(upm) %>% 
#     summarise(nII = n_distinct(id_hogar)) %>%
#     pull(nII) %>% mean()
#   
#   nII_pers_region[vctr_regiones[i]] <- df_encuesta %>% 
#                                   filter(region == vctr_regiones[i]) %>% 
#                                   group_by(upm) %>% 
#                                   summarise(nII = n()) %>% pull(nII) %>% mean()
#   
#   rho_Urbana_region[vctr_regiones[i]] <- (a$pobreza_deff[vctr_regiones[i]] - 1) / (nII_pers_region[vctr_regiones[i]] - 1)
# }
# 
# 
# df_ParamGenerales_region <- data.frame(M_region, N_region, nII_hog_region, nII_pers_region, rho_Urbana_region)
# 





# 
# f_paramGenerales_dominio <- function(df_estima_indicadores_dominio,
#                                      str_dominio, str_indicadorDeff){
#   vctr_dominio <- sort(unique(df_estima_indicadores_dominio[[str_dominio]]))
#   
#   # M_dominio <- df_censo %>% group_by(!!sym(str_dominio), UPM2) %>%
#   #                      summarise(num_personas = n()) %>%
#   #                      group_by(!!sym(str_dominio)) %>%
#   #                      summarise(num_upms = n()) %>% collect() %>%
#   #                      arrange(!!sym(str_dominio)) %>% pull(num_upms)
#   
#   N_dominio <- df_encuesta %>% group_by(!!sym(str_dominio)) %>%
#     summarise(N = sum(factorex)) %>% arrange(N) %>%
#     pull(N)
#   
#   
#   df_num_perXhog <- df_encuesta %>% group_by(!!sym(str_dominio), id_hogar) %>%
#     summarise(cuenta = n()) %>% group_by(!!sym(str_dominio)) %>%
#     summarise(num_persXhog = mean(cuenta)) %>%
#     arrange(!!sym(str_dominio))
#   
#   b_dominio <- df_num_perXhog %>% arrange(!!sym(str_dominio)) %>% pull(num_persXhog)
#   
#   
#   nII_hog_dominio <- vector(mode = "numeric", length = length(vctr_dominio))
#   nII_pers_dominio <- vector(mode = "numeric", length = length(vctr_dominio))
#   rho_Urbana_dominio <- vector(mode = "numeric", length = length(vctr_dominio))
#   
#   for(i in 1:length(vctr_dominio)){
#     nII_hog_dominio[vctr_dominio[i]] <- df_encuesta %>%
#       filter(!!sym(str_dominio) == vctr_dominio[i]) %>%
#       group_by(upm) %>%
#       summarise(nII = n_distinct(id_hogar)) %>%
#       pull(nII) %>% mean()
#     
#     nII_pers_dominio[vctr_dominio[i]] <- df_encuesta %>%
#       filter(!!sym(str_dominio) == vctr_dominio[i]) %>%
#       group_by(upm) %>%
#       summarise(nII = n()) %>% pull(nII) %>% mean()
#     # pobreza_deff
#     rho_Urbana_dominio[vctr_dominio[i]] <- (df_estima_indicadores_dominio[[str_indicadorDeff]][vctr_dominio[i]] - 1) / (nII_pers_dominio[vctr_dominio[i]] - 1)
#   }
#   
#   
#   df_ParamGenerales_dominio <- data.frame(M_dominio, N_dominio, 
#                                           nII_hog_dominio, nII_pers_dominio, 
#                                           rho_Urbana_dominio)
#   
#   df_ParamGenerales_dominio
# }