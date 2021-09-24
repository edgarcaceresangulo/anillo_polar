corrector_hora_actual <- function(bd){
  f1 <- max(bd$date, na.rm = T) 
  f2 <- strptime(paste(Sys.Date(), paste0(lubridate::hour(Sys.time()),":00:00"), sep = " "), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if(f1 == f2){
    return(slice(bd,1:(nrow(bd)-1)))
  }else{
    return(bd)
  }
}