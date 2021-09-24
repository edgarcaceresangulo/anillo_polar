tabladedatos <- function(bd, corte){

  bd <- bd %>%
    mutate(Flag = recode_factor((date < corte)*1, 
      "1" = "Validado", 
      "0" = "Crudo",
      .ordered = T))
  
  pintura <- data.frame(Flag = c("Validado","Crudo"),Colores = c("#144AA7","#666666"))
  selecpin <- as.character(merge(x = data.frame(Flag = levels(bd$Flag)), y = pintura, by = "Flag", all.x = T, sort = FALSE)$Colores)
  
  mat.etiq <- matrix(c(
    "1","date", "Fecha", 
    "2","ws","Vel. viento (ms\u207B\u00B9)",
    "3","wd","Dir. viento (\u00B0)",
    "4","SO2_1h_ppb", "SO\u2082 horario (ppb)", 
    "5","SO2_1h_ugm3", "SO\u2082 horario (\u03BCg/m\u00B3)",
    "6","Flag","Estado del dato"
  ), 
    nr = 6, 
    nc = 3, 
    byrow = T)
  colnames(mat.etiq) <- c("id","variable","nombre")
  
  library(DT)
  DT::datatable(bd,
    filter = "top",
    extensions = "Responsive",
    options = list(
      #dom = "t",
      pageLength = 10,
      columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all")),
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
      autoWidth = TRUE,
      searchHighlight = TRUE), 
    class = "cell-border stripe",
    colnames = mat.etiq[,"nombre"]) %>%
    formatDate("date", 
      method = "toLocaleString", 
      params = list(
        "se",
        list(timeZone = "UTC", hour12 = FALSE) # Formato tiempo
      )
    ) %>% 
    formatStyle("Flag", 
      color = styleEqual(
        levels = unique(bd$Flag), 
        values = selecpin
      )
    )

}