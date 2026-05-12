posicion <- function(lat, long, hex = FALSE, cuad = TRUE, subcuad = FALSE) {
  # Validate lengths
  if (length(lat) != length(long)) stop("Latitud y longitud deben tener la misma longitud.")
  
  # Work with character for substr if needed
  lat_chr  <- as.character(lat)
  long_chr <- as.character(long)
  
  # Convert from DINARA-like (e.g. 3526 -> 35°26') to decimal degrees if requested
  if (hex) {
    deg_lat  <- as.numeric(substr(lat_chr, 1, 2))
    min_lat  <- as.numeric(substr(lat_chr, 3, 4))
    deg_long <- as.numeric(substr(long_chr, 1, 2))
    min_long <- as.numeric(substr(long_chr, 3, 4))
    
    min_lat[is.na(min_lat)]   <- 0
    min_long[is.na(min_long)] <- 0
    
    outlat  <- - (deg_lat + min_lat / 60)
    outlong <- - (deg_long + min_long / 60)
  } else {
    outlat  <- as.numeric(lat)
    outlong <- as.numeric(long)
  }
  
  # If user doesn't want cuad, return decimals only
  if (!cuad) return(data.frame(lat = outlat, long = outlong))
  
  # --- Compute cuad as 3-digit code:
  # two-digit absolute degrees of latitude (truncated toward zero) + units digit of absolute degrees of longitude
  lat_deg_abs  <- abs(trunc(outlat))    # use trunc, not floor
  long_deg_abs <- abs(trunc(outlong))   # use trunc, not floor
  
  long_units <- long_deg_abs %% 10
  
  cuad_code <- sprintf("%02d%1d", lat_deg_abs, long_units)
  
  if (!subcuad) {
    return(data.frame(lat = outlat, long = outlong, cuad = cuad_code, stringsAsFactors = FALSE))
  } else {
    # Minutes from decimal part: fractional degrees * 60
    min_lat_dec  <- abs(outlat  - trunc(outlat))
    min_long_dec <- abs(outlong - trunc(outlong))
    
    subcuad_code <- dplyr::case_when(
      min_lat_dec < 0.5 & min_long_dec >= 0.5 ~ "a",
      min_lat_dec < 0.5 & min_long_dec < 0.5 ~ "b",
      min_lat_dec >=  0.5 & min_long_dec >=  0.5 ~ "c",
      min_lat_dec >=  0.5 & min_long_dec <  0.5 ~ "d",
      TRUE ~ NA_character_
    )
    
    return(data.frame(lat = outlat, long = outlong,
                      cuad = cuad_code, subcuad = subcuad_code,
                      stringsAsFactors = FALSE))
  }
}
