expandir_eusilc <- function(
    .datos,
    #.tipo = c("cross", "long"),
    .base = c("H", "P"),
    ...
    #.individuos = NULL
) {
  # Chequeos ---------------------------------
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble")
  }
  #.tipo <- rlang::arg_match(.tipo)
  .base <- rlang::arg_match(.base)

  variables <- names(.datos)
  tipos_var <- map_chr(.datos, class)

  if (.base == "P") {
    if (any(!(c("PB010", "PB020") %in% variables))) {
      rlang::abort("Falta `PB010` (año) o `PB020` (país).")
    }
    if (!(tipos_var["PB010"] %in% c("numeric", "integer"))) {
      rlang::abort("`PB010` (año) debe ser numérica.")
    }

    año <- unique(.datos$PB010)
    pais <- unique(.datos$PB020)
    chequear_pais_año(pais, año)
    chequear_variables(variables, tipos_var, necesarias_p[[as.character(año)]])

    .datos <- construir_variables_p(.datos, .pais)
  } else {
    #TODO
    rlang::warn("Todavía no preparé nada para las bases H")
  }
  #if (.base == "H" & is.null(.individuos)) {
  #  mensaje <- paste0("Para expandir la base tipo `H` se debe proveer `.individuos`.\n",
  #                    "`.individuos` debe ser una base tipo `P` expandida con `expandir_eusilc()`.")
  #  rlang::abort(mensaje)
  #}

  # ------------------------------------------
  return(.datos)
}

chequear_variables <- function(.variables, .tipos, .necesarias) {
  faltan <- .necesarias[!(.necesarias %in% .variables)]
  if (length(faltan) != 0) {
    mensaje <- paste0("Faltan las siguientes variables en .datos: `",
                      paste0(faltan, collapse = "`, `"), "`")
    rlang::abort(mensaje)
  }

  no_numericas <- .necesarias[!(.tipos[.necesarias] %in% c("numeric", "integer"))]
  no_numericas <- no_numericas[no_numericas != "PB020" & no_numericas != "HB020"]
  if (length(no_numericas) != 0) {
    mensaje <- paste0("Las siguientes variables requeridas deberían ser numéricas y no lo son: `",
                      paste0(no_numericas, collapse = "`, `"), "`")
    rlang::abort(mensaje)
  }
}

chequear_pais_año <- function(.pais, .año) {
  if (length(.pais) > 1) {
    rlang::abort("Por ahora sólo se admiten bases de un único país.")
  }
  if (length(.año) > 1) {
    rlang::abort("Por ahora sólo se admiten bases de un único año.")
  }
  if (!(.pais %in% paises_admitidos)) {
    mensaje <- paste0("La base debe ser de uno de los siguientes países: `",
                      paste0(paises_admitidos, collapse = "`, `"),
                      ", no `", .pais, "`.")
    rlang::abort(mensaje)
  }
  if (.año < 2021) {
    rlang::abort("Por ahora sólo se admiten bases posteriores a 2021.")
  }
}

construir_variables_p <- function(.datos, .pais, ...) {
  .datos <- .datos |>
    mutate(
      # Bloque I
      pi01 = PB010,
      pi02 = PB020,
      pi03 = "base D",
      pi04 = PX030,
      pi05 = PB030,
      pi06 = PB040,
      # Bloque D
      pd01 = "base R",
      pd02 = PB150,
      pd03 = case_when(
        PE041 == 0   ~ 1,
        PE041 == 100 ~ 2,
        PE041 == 200 ~ 3,
        PE041 == 300 ~ 4,
        PE041 == 340 ~ 4,
        PE041 == 344 ~ 4,
        PE041 == 350 ~ 4,
        PE041 == 353 ~ 4,
        PE041 == 354 ~ 4,
        PE041 == 450 ~ 5,
        PE041 == 500 ~ 6,
        .default = NA
      ),
      pd04 = "base R",
      pd05 = "base R",
      # Bloque L
      PSH  = "a definir",
      pl02 = case_when(
        PL032 == 1 ~ 1,
        PL032 == 2 ~ 2,
        PL032 %in% 3:8 ~ 3,
        .default = NA
      ),
      pl03a = PL051A,
      pl03b = PL051A %/% 10,
      pl04 = PL040A,
      pl05 = case_when(
        PL111A == "b - e" ~ 1,
        PL111A == "f" ~ 2,
        PL111A == "g" ~ 3,
        PL111A == "i" ~ 3,
        PL111A == "h" ~ 4,
        PL111A == "j" ~ 4,
        PL111A == "k" ~ 5,
        PL111A == "l - n" ~ 5,
        PL111A == "o" ~ 6,
        PL111A == "p" ~ 7,
        PL111A == "q" ~ 7,
        PL111A == "r - u" ~ 9,
        PL111A == "a" ~ 9,
        .default = NA
      ),
      pl06a = "Modulo LMH",
      pl06b = "Modulo LMH",
      pl07 = "Modulo LMH",
      pl08a = case_when(
        PL051A %in% 11:13 | PL051A %/% 10 == 2 ~ 1,
        PL051A == 14 | PL051A %/% 10 == 3 ~ 2,
        PL051A %/% 10 %in% 4:8 ~ 3,
        PL051A %/% 10 == 9 ~ 4,
        .default = NA
      ),
      pl09a = "Modulo LMH",
      pl09b = "Modulo LMH",
      pl10 = case_when(
        PL051A %in% c(1, 11:26) ~ 1,
        PL040A == 1 ~ 2,
        PL040A == 2 ~ 5,
        PL150 == 1 ~ 6,
        PL051A %in% c(2, 31:35) ~ 3,
        PL051A %in% c(3, 41:44) ~ 4,
        PL051A %in% c(51:54) ~ 5,
        PL051A %in% c(61, 62, 92) ~ 10,
        PL051A %in% c(71:83) ~ 8,
        PL051A %in% c(91, 93:96) ~ 9,
        .default = NA
      ),
      .keep = "none"
    )
  # ------------------------------------------
  return(.datos)
}
