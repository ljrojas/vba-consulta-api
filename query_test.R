# query

# librerías ---------------------------------------------------------------

# Funciones ---------------------------------------------------------------
to_df <- function(list_res){
  df = do.call(what = rbind, args = list_res)%>%
    as.data.frame()
  return(df)
}

#* Return valores de Puntaje Sectorial
#* @param x Coordenada x
#* @param y Coordenada y
#* @post /puntaje_sectorial
query_p_sectorial <- function(x, y, url, cod_uso){
  body <- list(x = x, y = y, cod_uso = cod_uso)
  r = httr::POST(url, body = body, encode = "json", httr::verbose())
  df <- to_df(httr::content(r))
  return(df)
}

#* Return valores de evaluación Histórica
#* @param cod_uso Código de Uso
#* @param via_aprobacion Vía de Aprobación
#* @param categoria Categoría de Inversión
#* @param div_adm División Administrativa
#* @post /eval_hist
query_eval_hist <- function(div_adm, cod_uso, via_aprobacion, categoria, url){
  body <- list(cod_uso = cod_uso, div_adm = div_adm,
               via_aprobacion = via_aprobacion, categoria = categoria)
  r = httr::POST(url, body = body, encode = "json", httr::verbose())
  df <- to_df(httr::content(r)) 
  return(df)
}

#* Return valores de Prioridad Social Comunal
#* @param comuna_nom Nombre Comuna
#* @param col_extras Valores extras agregar
#* @post /prioridad_social
query_prio_social <- function(comuna_nom, col_extras, url){
  body <- list(comuna_nom = comuna_nom, col_extras = col_extras)
  r = httr::POST(url, body = body, encode = "json", httr::verbose())
  df <- to_df(httr::content(r))
  return(df)
}

# Data Query --------------------------------------------------------------


### PUNTAJE SECTORIAL
x <- "-70.655065"
y <- "-33.534731"
CASO_USO <- 104
# cod_uso <- 206
# CASO_USO <- 124 #206
# cod_uso_indice

data <- query_p_sectorial(x = x, y = y, cod_uso = CASO_USO,  url =  "http://127.0.0.1:8000/puntaje_sectorial")
# data
t_data <- as.data.frame(t(data))
t_data


### EVALUACIÖN HISTÓRICA
VIA_APROBACION  <- "CIRC. 33"
CATEGORIA <- "INVERSIÓN"
DIV_ADM <- "ESTACION CENTRAL" #COMUNAS RURALES
CASO_USO <- 122
hist_data <- query_eval_hist(div_adm = DIV_ADM, cod_uso = CASO_USO, 
                             via_aprobacion = VIA_APROBACION, categoria = CATEGORIA, 
                             url = "http://127.0.0.1:8000/eval_hist")
hist_data
# t_data <- as.data.frame(t(hist_data))


### PRIORIDAD SOCIAL COMUNAL
COMUNA <- "ESTACIÓN CENTRAL"
COL_EXTRAS <- FALSE
prio_social_data <- query_prio_social(comuna_nom = COMUNA, col_extras = COL_EXTRAS,
                                      url = "http://127.0.0.1:8000/prioridad_social")
prio_social_data

# t_data <- as.data.frame(t(prio_social_data))

# Evaluación global -------------------------------------------------------

# 
df <- data.frame(x = runif(n = 10, max = -70.66, min = -70.90),
                 y = runif(n = 10, max = -33.40, min = -33.50))

variables <- c("COMUNA", "NOM_COMUNA", "PERSONAS", "IBT", "DIM_SEG")
variables <- names(data)

resultados <- data.frame()
for(i in 1:nrow(df)){
  data <- query_p_sectorial(x = df[i, 1], y = df[i, 2], 
                            cod_uso = CASO_USO, url =  "http://127.0.0.1:8000/puntaje_sectorial")

  if(nrow(data)==0){
    data <- as.data.frame(matrix(rep(NA,length(variables)), nrow = 1))
    names(data) <- names(variables)
  }
  data$x = df[i, 1]
  data$y = df[i, 2]
  print(paste0("iterador ",i, " de ", nrow(df), " Comuna de ", data[,"NOM_COMUNA"]))
  resultados <- rbind(resultados, data)
}
resultados




# Pendientes --------------------------------------------------------------
# PUNTAJE SECTORIAL
# - agregar código de USO (tabla joseto)
#- selección de variables en la query

# EVALUACIÖN HISTÓRICA
# - div_adm ("PROVINCIA MELIPILLA", "MELIPILLA, CHACABUCO Y SAN JOSE DE MAIPO")
# - alerta comuna no existe

# PRIORIDAD SOCIAL COMUNAL
# - Comuna no existe

# GLOBAL
# normalizar nombres de comunas








# Código Residual ---------------------------------------------------------
# - if comuna no existe


# Apply the function
# my_df_2 <- df %>%
#   mutate(new_col = map_dbl(old_col, ~query_info_cit(.x, y = 3)))
# 
# df <- split(df, seq(nrow(df)))
#  
# resultado <- df%>%
#   purrr::map(query_info_cit, x = x, y = y,url =  "http://127.0.0.1:8000/info_cit")
# 
# resultado
# 
# data <- query_info_cit(x =df[1,1], y = df[1,2] , url =  "http://127.0.0.1:8000/info_cit")


# referenicas
# https://www.statworx.com/at/blog/how-to-create-rest-apis-with-r-plumber/


# Residuos ----------------------------------------------------------------

# curl --data "a=4&b=3" "http://localhost:8000/sum"
# curl --data "x=-70.680&y=-33.462" "http://127.0.0.1:8000/info_cit"
# query <- paste0("http://127.0.0.1:8000/info_cit?x=", x, "&y=",y)

# url <- "http://127.0.0.1:8000/info_cit"
# body <- list(x = x, y = y)
# 
# # Query -------------------------------------------------------------------
# r = POST(url, body = body, encode = "json", verbose())
# 
# # Resultados --------------------------------------------------------------
# 
# df <- to_df(content(r))
# df

# Parámetros --------------------------------------------------------------

# 
# eval_hist(COD = cod_uso, COMUNA_CALC = COMUNA_CALC, V_AP = via_aprobacion, CAT = categoria, 
#           BASE_C_COD = hist_c_cod, BASE_S_COD = hist_S_cod)

# test(x = x, y = y, cod_uso = cod_uso, via_aprobacion = via_aprobacion, categoria = categoria, base = base)

