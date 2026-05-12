#' geo_check
#'
#' Chequea rápidamente si los lances de pesca caen sobre tierra o dentro de un puerto.
#'
#' Por defecto, `"tierra"` usa un mapa de Sudamérica.
#' También puede usarse `"mvd"` (puerto de Montevideo), `"lpl"` (puerto de La Paloma),
#' o `"ports"` (ambos puertos combinados).
#'
#' Si `dist = TRUE`, devuelve la distancia en metros al polígono (0 si está dentro).
#'
#' @param lat Vector numérico con latitudes.
#' @param long Vector numérico con longitudes.
#' @param poly String: `"tierra"`, `"mvd"`, `"lpl"`, o `"ports"`.
#' @param dist Lógico. Si TRUE, devuelve distancia en metros al polígono (por defecto FALSE).
#'
#' @return Un vector: binario si `dist = FALSE`, o distancias en metros si `dist = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' long <- c(-55, -56.204417, -54.143567)
#' lat  <- c(-37, -34.897430, -34.652272)
#' geo_check(lat = lat, long = long, poly = "ports")
#' geo_check(lat = lat, long = long, poly = "ports", dist = TRUE)
#' }

geo_check <- function(lat, long, poly = "tierra", dist = FALSE) {
  # --- Validations ---
  if (length(lat) != length(long))
    stop("lat y long deben tener la misma cantidad de datos")
  
  if (!poly %in% c("tierra", "mvd", "lpl", "ports"))
    stop('El argumento "poly" debe ser uno de: "tierra", "mvd", "lpl", "ports".')
  
  # --- Define built-in polygons ---
  # Montevideo 
  mvd_coords <- data.frame(
    long = c(-56.25301181640987, -56.21447383728389, -56.19533359375362,
             -56.19679271545772, -56.21490299072627, -56.229773887813515,
             -56.247283348262734, -56.25301181640987),
    lat  = c(-34.89309793803404, -34.90865489016588, -34.90006725380263,
             -34.8817627376096,  -34.8702147147422,  -34.869742214570486,
             -34.87340397947751, -34.89309793803404)
  )
  mvd_poly <- sf::st_sfc(sf::st_polygon(list(as.matrix(mvd_coords))), crs = 4326)
  
  # La Paloma
  lpl_coords <- data.frame(
    long = c(-54.14856, -54.14513, -54.14403, -54.14257, -54.14204,
             -54.14165, -54.14068, -54.14129, -54.14077, -54.14457,
             -54.14869, -54.14856),
    lat  = c(-34.65129, -34.65260, -34.65404, -34.65452, -34.65450,
             -34.65450, -34.65339, -34.65297, -34.65231, -34.64831,
             -34.65080, -34.65129)
  )
  lpl_poly <- sf::st_sfc(sf::st_polygon(list(as.matrix(lpl_coords))), crs = 4326)
  
  # Elegir poly
  if (poly == "tierra") {
    sa <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
            "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Falkland Islands")
    SA <- giscoR::gisco_get_countries(country = sa)
    equis <- sf::st_union(SA$geometry)
  } else if (poly == "mvd") {
    equis <- sf::st_sf(mvd_poly)
  } else if (poly == "lpl") {
    equis <- sf::st_sf(lpl_poly)
  } else if (poly == "ports") {
    equis <- sf::st_union(sf::st_sf(c(mvd_poly, lpl_poly)))
  }
  
  # --- Convert points to sf ---
  lances <- sf::st_as_sf(data.frame(long = long, lat = lat),
                         coords = c("long", "lat"), crs = 4326)
  
  # --- Check intersection ---
  inside <- as.numeric(sf::st_intersects(lances, equis, sparse = FALSE))
  inside[is.na(inside)] <- 0
  
  # --- Distance calculation (if requested) ---
  if (dist) {
    # Automatically choose UTM zone based on mean location
    mean_long <- mean(long, na.rm = TRUE)
    mean_lat  <- mean(lat, na.rm = TRUE)
    utm_zone  <- floor((mean_long + 180) / 6) + 1
    epsg_code <- ifelse(mean_lat >= 0, 32600, 32700) + utm_zone
    
    # Project to local UTM zone for accurate meters
    lances_utm <- suppressMessages(sf::st_transform(lances, epsg_code))
    equis_utm  <- suppressMessages(sf::st_transform(equis, epsg_code))
    
    # Compute distance to polygon (meters)
    distances <- as.numeric(sf::st_distance(lances_utm, equis_utm))
    distances[inside == 1] <- 0
    return(distances)
  } else {
    return(inside)
  }
}