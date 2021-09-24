rangofecha <- function(bd, f1, f2, sw, v){
  bd <- openair::selectByDate(bd, start = f1, end = f2, hour = v)
  return(bd)
}

# rangofecha(bd.1h, f1 = "2021-09-10", f2 = "2021-09-22", 0:23)