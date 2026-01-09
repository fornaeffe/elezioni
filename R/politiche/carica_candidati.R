

carica_candidati <- function(scenario, dati_politiche, parametri_input) {
  
  carica_candidati_ramo <- function(ramo) {
    dati_ramo <- dati_politiche[[ramo]]
    
    candidati_uni <- data.table::as.data.table(
      readxl::read_xlsx(
        scenario, 
        paste0(ramo, "_candidati_uni")
      )
    )
    candidati_pluri <- data.table::as.data.table(
      readxl::read_xlsx(
        scenario, 
        paste0(ramo, "_candidati_pluri")
      )
    )
  }
  
  camera <- carica_candidati_ramo("camera")
  senato <- carica_candidati_ramo("senato")
}