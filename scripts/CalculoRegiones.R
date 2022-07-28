
################################# C?lculos nacionales ###################################

#Calcular el margen de error
me_calculo <- function (N, n, r, b, rho, P, conf, m) 
{
    bar.n <- m * r * b
    Deff <- 1 + (bar.n - 1) * rho
    me <- e4p(N, n, P, DEFF = Deff, conf = 0.95)
    # Mi <- n / (r * b)
    # M <- n / bar.n
    # 
  # result <- data.frame(HouseholdsPerPSU = round(m), PersonsPerPSU = round(bar.n), 
  #                      DEFF = round(Deff, 2), PSUinSample = round(M), HouseholdsInSample = round(Mi), 
  #                      PersonsInSample = round(n))
  as.data.frame(me)
}


########################### Margen de error panel ################################

MedidasErrorPobrezaAnual <- me_calculo(N = df_ParamGenerales$N[indice_Urbana],
           n = 2 * tablaMuestreo_pobreza_Urbana$PersonsInSample[2], 
           r = df_ParamGenerales$r_pobreza[indice_Urbana],
           b = df_ParamGenerales$b[indice_Urbana], 
           rho = df_rho$`rho pobreza.`[df_rho$Area == "Urbana"], 
           P = df_ValoresFijados$pobreza_prop[indice_Urbana], 
           conf = 0.95,
           m = tablaMuestreo_pobreza_Urbana$HouseholdsPerPSU[2]) 
  

MedidasErrorAnual_Pobreza_Urbana <- me_calculo(N = df_ParamGenerales$N[indice_Urbana],
                                       n = 2 * tablaMuestreo_pobreza_Urbana$PersonsInSample[2], 
                                       r = df_ParamGenerales$r_pobreza[indice_Urbana],
                                       b = df_ParamGenerales$b[indice_Urbana], 
                                       rho = df_rho$`rho pobreza.`[df_rho$Area == "Urbana"], 
                                       P = df_ValoresFijados$pobreza_prop[indice_Urbana], 
                                       conf = 0.95,
                                       m = tablaMuestreo_pobreza_Urbana$HouseholdsPerPSU[2]) 

MedidasErrorAnual_Pobreza_Rural <- me_calculo(N = df_ParamGenerales$N[indice_Rural],
                                               n = 2 * tablaMuestreo_pobreza_Rural$PersonsInSample[2], 
                                               r = df_ParamGenerales$r_pobreza[indice_Rural],
                                               b = df_ParamGenerales$b[indice_Rural], 
                                               rho = df_rho$`rho pobreza.`[df_rho$Area == "Rural"], 
                                               P = df_ValoresFijados$pobreza_prop[indice_Rural], 
                                               conf = 0.95,
                                               m = tablaMuestreo_pobreza_Rural$HouseholdsPerPSU[2]) 


MedidasErrorAnual_PobrezaExtr_Urbana <- me_calculo(N = df_ParamGenerales$N[indice_Urbana],
                                               n = 2 * tablaMuestreo_pobrezaExtr_Urbana$PersonsInSample[2], 
                                               r = df_ParamGenerales$r_pobrezaExtr[indice_Urbana],
                                               b = df_ParamGenerales$b[indice_Urbana], 
                                               rho = df_rho$`rho pobreza extrema`[df_rho$Area == "Urbana"], 
                                               P = df_ValoresFijados$pobrezaExtr_prop[indice_Urbana], 
                                               conf = 0.95,
                                               m = tablaMuestreo_pobrezaExtr_Urbana$HouseholdsPerPSU[2]) 

MedidasErrorAnual_PobrezaExtr_Rural <- me_calculo(N = df_ParamGenerales$N[indice_Rural],
                                                   n = 2 * tablaMuestreo_pobrezaExtr_Rural$PersonsInSample[2], 
                                                   r = df_ParamGenerales$r_pobrezaExtr[indice_Rural],
                                                   b = df_ParamGenerales$b[indice_Rural], 
                                                   rho = df_rho$`rho pobreza extrema`[df_rho$Area == "Rural"], 
                                                   P = df_ValoresFijados$pobrezaExtr_prop[indice_Rural], 
                                                   conf = 0.95,
                                                   m = tablaMuestreo_pobrezaExtr_Rural$HouseholdsPerPSU[2]) 



MedidasErrorAnual_NBI_Urbana <- me_calculo(N = df_ParamGenerales$N[indice_Urbana],
                                                   n = 2 * tablaMuestreo_NBI_Urbana$PersonsInSample[2], 
                                                   r = df_ParamGenerales$r_NBI[indice_Urbana],
                                                   b = df_ParamGenerales$b[indice_Urbana], 
                                                   rho = df_rho$`rho NBI`[df_rho$Area == "Urbana"], 
                                                   P = df_ValoresFijados$NBI_prop[indice_Urbana], 
                                                   conf = 0.95,
                                                   m = tablaMuestreo_NBI_Urbana$HouseholdsPerPSU[2]) 

MedidasErrorAnual_NBI_Rural <- me_calculo(N = df_ParamGenerales$N[indice_Rural],
                                                  n = 2 * tablaMuestreo_NBI_Rural$PersonsInSample[2], 
                                                  r = df_ParamGenerales$r_NBI[indice_Rural],
                                                  b = df_ParamGenerales$b[indice_Rural], 
                                                  rho = df_rho$`rho NBI`[df_rho$Area == "Rural"], 
                                                  P = df_ValoresFijados$NBI_prop[indice_Rural], 
                                                  conf = 0.95,
                                                  m = tablaMuestreo_NBI_Rural$HouseholdsPerPSU[2]) 


attributes(df_encuesta$region) <- NULL


# Par?metros por regi?n
df_num_perXhog_region <- df_encuesta %>% group_by(region, id_hogar) %>% 
                         summarise(cuenta = n()) %>% group_by(region) %>% 
                         summarise(num_persXhog = mean(cuenta))


M_region <- df_censo %>% group_by(ID_REGION  , UPM2) %>% summarise(num_personas = n()) %>% 
            group_by(ID_REGION) %>% summarise(num_upms = n()) %>%
            arrange(ID_REGION) %>% 
            collect()

N_region = df_encuesta %>% group_by(region) %>% 
           summarise(N_region = sum(factorex)) %>%
           arrange(region)


# N?mero de personas por hogar seg?n el censo
b_region <- df_encuesta %>% group_by(region, id_hogar) %>% 
            summarise(cuenta = n()) %>% 
            group_by(region) %>% 
            summarise(num_persXhog = mean(cuenta)) %>%
            arrange(region)

nII_hog_region <- df_encuesta %>% group_by(region, upm) %>%
                  summarise(nII = n_distinct(id_hogar)) %>%
                  ungroup() %>% group_by(region) %>%
                  summarise(nII_hog = mean(nII))  %>%
                  arrange(region)

nII_pers_region <- df_encuesta %>% group_by(region, upm) %>%
                   summarise(nII = n()) %>%
                   ungroup() %>% group_by(region) %>%
                   summarise(nII_hog = mean(nII)) %>%
                   arrange(region)


lst_ParamGenerales <- list(region = 1:6, M_region = M_region,
                           N_region = N_region,
                           b_region = b_region,
                           nII_hog_region = nII_hog_region,
                           nII_pers_region = nII_pers_region)


################################# Ciclo para calcular las medidas de error por regi?n #########################

df_estima_indicadores_region <- diseno_pobreza %>% group_by(region) %>%
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
  )

names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "pobreza"] <- "pobreza_prop"
names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "pobrezaExtr"] <- "pobrezaExtr_prop"
names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "NBI"] <- "NBI_prop"
names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "tasa_desocupacion"] <- "tasa_desocupacion_prop"
names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "tasa_ocupacion"] <- "tasa_ocupacion_prop"
names(df_estima_indicadores_region)[names(df_estima_indicadores_region) == "tasa_participacion"] <- "tasa_participacion_prop"

attributes(df_estima_indicadores_region$region) <- NULL
conf <- 0.95
df_estima_indicadores_region <- df_estima_indicadores_region %>%
  mutate(region = region,
         pobreza_prop = round(pobreza_prop, 2), 
         pobreza_se = round(pobreza_se, 4),
         pobreza_me = round(qnorm(1-(1-conf)/2) * pobreza_se, 4),
         pobreza_mer = round(pobreza_me / pobreza_prop, 4),
         pobreza_cve = round(100 * pobreza_se / pobreza_prop, 1),
         pobreza_LI = pobreza_prop - pobreza_me,
         pobreza_LS = pobreza_prop + pobreza_me,
         pobreza_deff = round(pobreza_deff, 1),
         
         pobrezaExtr_prop = round(pobrezaExtr_prop, 2), 
         pobrezaExtr_se = round(pobrezaExtr_se, 4),
         pobrezaExtr_me = round(qnorm(1-(1-conf)/2) * pobrezaExtr_se,4),
         pobrezaExtr_mer = round(pobrezaExtr_me / pobrezaExtr_prop, 4),
         pobrezaExtr_cve = round(100 * pobrezaExtr_se / pobrezaExtr_prop, 1),
         pobrezaExtr_LI = pobrezaExtr_prop - pobrezaExtr_me,
         pobrezaExtr_LS = pobrezaExtr_prop + pobrezaExtr_me,
         pobrezaExtr_deff = round(pobrezaExtr_deff, 1),
         
         NBI_prop = round(NBI_prop, 2),
         NBI_se = round(NBI_se, 4),
         NBI_me = round(qnorm(1-(1-conf)/2) * NBI_se,4),
         NBI_mer = round(NBI_me / NBI_prop, 4),
         NBI_cve = round(100 * NBI_se / NBI_prop,1),
         NBI_LI = NBI_prop - NBI_me,
         NBI_LS = NBI_prop + NBI_me,
         NBI_deff = round(NBI_deff, 1),
         
         tasa_desocupacion_prop = round(tasa_desocupacion_prop, 4),
         tasa_desocupacion_se = round(tasa_desocupacion_se, 4),
         tasa_desocupacion_me = round(qnorm(1-(1-conf)/2) * tasa_desocupacion_se,4),
         tasa_desocupacion_mer = round(tasa_desocupacion_me / tasa_desocupacion_prop, 4),
         tasa_desocupacion_cve = round(100 * tasa_desocupacion_se / tasa_desocupacion_prop, 2),
         tasa_desocupacion_LI = tasa_desocupacion_prop - tasa_desocupacion_me,
         tasa_desocupacion_LS = tasa_desocupacion_prop + tasa_desocupacion_me,
         tasa_desocupacion_deff = round(tasa_desocupacion_deff, 1),
         
         tasa_ocupacion_prop = round(tasa_ocupacion_prop, 4),
         tasa_ocupacion_se = round(tasa_ocupacion_se, 4),
         tasa_ocupacion_me = round(qnorm(1-(1-conf)/2) * tasa_ocupacion_se,4),
         tasa_ocupacion_mer = round(tasa_ocupacion_me / tasa_ocupacion_prop, 4),
         tasa_ocupacion_cve = round(100 * tasa_ocupacion_se / tasa_ocupacion_prop, 2),
         tasa_ocupacion_LI = tasa_ocupacion_prop - tasa_ocupacion_me,
         tasa_ocupacion_LS = tasa_ocupacion_prop + tasa_ocupacion_me,
         tasa_ocupacion_deff = round(tasa_ocupacion_deff, 1),
         
         tasa_participacion_prop = round(tasa_participacion_prop, 4),
         tasa_participacion_se = round(tasa_participacion_se, 4),
         tasa_participacion_me = round(qnorm(1-(1-conf)/2) * tasa_participacion_se,4),
         tasa_participacion_mer = round(tasa_participacion_me / tasa_participacion_prop, 4),
         tasa_participacion_cve = round(100 * tasa_participacion_se / tasa_participacion_prop, 2),
         tasa_participacion_LI = tasa_participacion_prop - tasa_participacion_me,
         tasa_participacion_LS = tasa_participacion_prop + tasa_participacion_me,
         tasa_participacion_deff = round(tasa_participacion_deff, 1)
  ) %>%
  select(region,
         pobreza_prop, pobreza_se, pobreza_me, pobreza_mer,
         pobreza_cve, pobreza_LI, pobreza_LS, pobreza_deff,
         pobrezaExtr_prop, pobrezaExtr_se, pobrezaExtr_me, pobrezaExtr_mer,
         pobrezaExtr_cve, pobrezaExtr_LI, pobrezaExtr_LS, pobrezaExtr_deff,
         NBI_prop, NBI_se, NBI_me, NBI_mer,
         NBI_cve, NBI_LI, NBI_LS, NBI_deff,
         tasa_desocupacion_prop, tasa_desocupacion_se, tasa_desocupacion_me, tasa_desocupacion_mer,
         tasa_desocupacion_cve, tasa_desocupacion_LI, tasa_desocupacion_LS, tasa_desocupacion_deff,
         tasa_ocupacion_prop, tasa_ocupacion_se, tasa_ocupacion_me, tasa_ocupacion_mer,
         tasa_ocupacion_cve, tasa_ocupacion_LI, tasa_ocupacion_LS, tasa_ocupacion_deff,
         tasa_participacion_prop, tasa_participacion_se, tasa_participacion_me, tasa_participacion_mer,
         tasa_participacion_cve, tasa_participacion_LI, tasa_participacion_LS, tasa_participacion_deff,
         N_activos, N 
  )




# Rho calcularlo m?s preciso
nII_pers_region <- lst_ParamGenerales$nII_pers_region$nII_hog # Cambiar el nombre a nII_per

deff_pobreza_region <- df_estima_indicadores_region$pobreza_deff

# deff =  1 + (n-1) * rho, despejando:  rho = (deff - 1) / (n - 1)
rho_pobreza_region <- (deff_pobreza_region - 1) / (nII_pers_region - 1)


consulta_N_region <- df_censo %>% group_by(ID_REGION) %>% summarise(N_region = n()) %>% collect()
n_region <- round(2 * tablaMuestreo_pobreza_Urbana$PersonsInSample[2] * 
            (consulta_N_region$N_region / sum(consulta_N_region$N_region)))

lst_MedidasErrorPobrezaAnual_region <- vector(mode = "list", length = length(df_encuesta$region)) 
lst_MedidasErrorPobrezaAnual_region[[i]] <- me_calculo(N = lst_ParamGenerales$N_region[i],
                                       n = n_region[i], 
                                       r = 1,
                                       b = lst_ParamGenerales$b_region$num_persXhog[i], 
                                       rho = df_rho$`rho pobreza.`[df_rho$Area == "Urbana"], # Cambiar
                                       P = df_ValoresFijados$pobreza_prop[indice_Urbana], # Cambiar
                                       conf = 0.95,
                                       m = tablaMuestreo_pobreza_Urbana$HouseholdsPerPSU[2]) 

