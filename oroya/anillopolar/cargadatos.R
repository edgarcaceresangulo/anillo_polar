#------------------------------------------------------------------------------#
#------------------------- ESTACIÓN OROYA - ANILLO POLAR ----------------------#
#------------------------------------------------------------------------------#

#------------------ Carga de datos

#### Carga manual ####

# 1 hora
bd.1h <- read.csv("../../datos/CA-CC-01 HISTORICO 1h.csv", stringsAsFactors = F, sep = ";") %>% 
  dplyr::mutate(
    date = as.POSIXct(strptime(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))) %>% 
  dplyr::select("date","ws","wd","SO2_1h_ppb","SO2_1h_ugm3") %>% 
  dplyr::as_tibble()

head(bd.1h); tail(bd.1h)

fmax <- format(as.POSIXct(max(bd.1h$date, na.rm = T) + lubridate::hours(1), format = "%Y-%m-%d %H:%M:%S UTC"), format = "%Y-%m-%d %H:%M:%S")

#### Fin de la carga manual ####

##### Carga automática ####

# conexión a Producción

driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
  "ojdbc7.jar")

conexion <- DBI::dbConnect(driver,"jdbc:oracle:thin:@orclnod-cluster-scan:1534:nexoefa1",
  "SHINY",
  "j46wMmcvz0Qp")

result <- DBI::dbSendQuery(conexion, statement = paste(
  "SELECT FECHA_DATA_LOGGER_TIMESTAMP,FECHA_DATA_LOGER,HORA_DATA_LOGER,WS,WD,SO2_CONC",
  "FROM VIGAMB.VIGAMB_TRAMA_AIRE",
  "WHERE ID_ESTACION = 2",
  "AND FECHA_DATA_LOGGER_TIMESTAMP BETWEEN to_date(to_char('", fmax, "'),'YYYY-MM-DD HH24:MI:SS') AND SYSDATE",
  "ORDER BY FECHA_DATA_LOGGER_TIMESTAMP",
  sep = " "))

tb <- DBI::dbFetch(result)

#### Fin de la carga automática ####

#------------------ Transformación de datos 

#### De 5 minuto a 1 hora #### 

# 5 minuto
bd.5min.tem <- tb %>% 
  dplyr::mutate(
    date = as.POSIXct(strptime(paste(FECHA_DATA_LOGER, HORA_DATA_LOGER, sep = " "), format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))) %>%
  dplyr::rename(
    # nuevo = antiguo
    SO2_5min_ppb = SO2_CONC,
    ws = WS,
    wd = WD
  ) %>% 
  dplyr::select("date","SO2_5min_ppb","ws","wd") %>% 
  dplyr::arrange(., date) %>%
  tidyr::complete(
    date = base::seq(
      lubridate::ymd_hms(paste(as.Date(min(date, na.rm = T)), lubridate::hour(min(date, na.rm = T)),":00:00")),
      lubridate::ymd_hms(paste(as.Date(max(date, na.rm = T)), lubridate::hour(max(date, na.rm = T)),":59:00")),
      by = "5 min"))

L1 <- 0
cond <- which( bd.5min.tem$SO2_5min_ppb <= L1 )

if(length(cond) != 0){ bd.5min.tem$SO2_5min_ppb[cond] <- NA }

# 1 hora
bd.1h.tem <- bd.5min.tem %>%
  openair::timeAverage(., avg.time = "1 hour", data.thresh = 75, statistic = "mean") %>%
  dplyr::mutate_if(is.numeric, list( ~round(., 2))) %>%
  dplyr::rename(.,
    # nuevo = antiguo
    SO2_1h_ppb = SO2_5min_ppb,
  ) %>%
  dplyr::mutate(
    SO2_1h_ugm3 = round(SO2_1h_ppb*2.62, 1)
    ) %>%
  dplyr::select("date","SO2_1h_ppb","SO2_1h_ugm3","ws","wd") %>%
  dplyr::mutate_if(is.numeric, list( ~round(., 2)))

# Unión horas
bd.1h <- dplyr::bind_rows(bd.1h, bd.1h.tem) %>% 
  corrector_hora_actual(.)

head(bd.1h); tail(bd.1h)

#### Fin de 1 min a 1 hora ####

#------------------ Cortes y cotas

# Corte: fecha y hora de inicio del monitoreo automático
corte <- bd.1h.tem %>% 
  dplyr::select("date") %>%
  dplyr::summarise(date.min = min(as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S", tz = "UTC")), na.rm = T)) %>% 
  magrittr::extract2(1)

# Cotas: solo la fecha final del monitoreo automático
cota <- bd.1h %>% 
  tidyr::drop_na(SO2_1h_ugm3,wd) %>%
  dplyr::select("date") %>%
  dplyr::summarise(date.max = max(base::as.Date(date, format = "%Y-%m-%d"),  na.rm = T)) %>% 
  magrittr::extract2(1)

rm(list = c("tb","bd.5min.tem","bd.1h.tem"))

# Busqueda de duplicados
bd.1h[base::duplicated(bd.1h),]

DBI::dbClearResult(result)
DBI::dbDisconnect(conexion)
