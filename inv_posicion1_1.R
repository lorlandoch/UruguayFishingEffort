inv_posicion <- function(cuad, subcuad = NULL) {
  # Ensure character for safe substring extraction
  cuad <- as.character(cuad)
  
  # Validate format: must be 3 digits (NNL)
  if (any(nchar(cuad) != 3))
    stop("El código 'cuad' debe tener 3 dígitos, ej. '354'.")
  
  # Extract base degree components
  lat_deg_abs  <- as.numeric(substr(cuad, 1, 2))  # first two digits = degrees of lat (absolute)
  long_units   <- as.numeric(substr(cuad, 3, 3))  # last digit = units of longitude (54°W → 4)
  
  # Base cuadrante center (always negative in southern/western hemisphere)
  latR  <- - (lat_deg_abs + 0.5)
  longR <- - (50 + long_units + 0.5)
  
  # If no subcuad, return cuadrante centers
  if (is.null(subcuad) || all(is.na(subcuad))) {
    return(data.frame(latR, longR))
  }
  
  subcuad <- tolower(as.character(subcuad))
  
  # Apply 0.25° shifts according to quadrant:
  latR <- dplyr::case_when(
    subcuad == "a" ~ latR + 0.25, # NW
    subcuad == "b" ~ latR + 0.25, # NE
    subcuad == "c" ~ latR - 0.25, # SW
    subcuad == "d" ~ latR - 0.25, # SE
    TRUE ~ latR
  )
  
  longR <- dplyr::case_when(
    subcuad == "a" ~ longR - 0.25, # NW
    subcuad == "b" ~ longR + 0.25, # NE
    subcuad == "c" ~ longR - 0.25, # SW
    subcuad == "d" ~ longR + 0.25, # SE
    TRUE ~ longR
  )
  
  data.frame(latR, longR)
}
