anillopolar <- function(bd, sw2, titulo){
  
  main <- 
    titulo %>% 
    dplyr::filter(type == sw2) %>% 
    dplyr::pull(titulo)
  
  bd <- bd %>% dplyr::rename(SO2 = SO2_1h_ugm3)

  polarAnnulus(mydata = bd, poll = "SO2", period = sw2, main = main)
  
}

