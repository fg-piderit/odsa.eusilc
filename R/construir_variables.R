# Funciones para construir las variables de las bases finales
#
# construir variables_p ------------------------------------------------------
construir_variables_p <- function(.datos, .pais, ...) {
  .datos <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      pi01 = PB010,
      pi02 = PB020,
      pi03 = "base D",
      pi04 = PX030,
      pi05 = PB030,
      pi06 = PB040,
      # Bloque D -----------------------
      pd01 = "base R",
      pd02 = PB150,
      pd03 = dplyr::case_when(
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
      # Bloque L -----------------------
      PSH  = "a definir",
      pl02 = dplyr::case_when(
        PL032 == 1 ~ 1,
        PL032 == 2 ~ 2,
        PL032 %in% 3:8 ~ 3,
        .default = NA
      ),
      pl03a = PL051A,
      pl03b = PL051A %/% 10,
      pl04 = PL040A,
      pl05 = dplyr::case_when(
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
      pl08a = dplyr::case_when(
        PL051A %in% 11:13 | PL051A %/% 10 == 2 ~ 1,
        PL051A == 14 | PL051A %/% 10 == 3 ~ 2,
        PL051A %/% 10 %in% 4:8 ~ 3,
        PL051A %/% 10 == 9 ~ 4,
        .default = NA
      ),
      pl09a = "Modulo LMH",
      pl09b = "Modulo LMH",
      .pl10 = dplyr::case_when(
        PL051A %in% c(1, 11:26) ~ 1,
        PL051A %in% c(2, 31:35) ~ 3,
        PL051A %in% c(3, 41:44) ~ 4,
        PL051A %in%   51:54 ~ 5,
        PL051A %in% c(61, 62, 92) ~ 10,
        PL051A %in%   71:83 ~ 8,
        PL051A %in% c(91, 93:96) ~ 9,
        .default = NA
      ),
      pl10 = dplyr::replace_when(
        x = .pl10,
        .pl10 == 8 & PL040A != 1 ~ 8,
        .pl10 > 1 & PL040A == 1 ~ 2,
        .pl10 > 1 & PL040A == 2 ~ 6,
        .pl10 > 1 & is.na(PL040A) ~ NA,
        .pl10 > 2 & PL150 == 1 ~ 7,
        .pl10 > 2 & is.na(PL150) ~ NA
      ),
      informalidad = "a definir",
      informalidad4 = "a definir",
      # Bloque Y -----------------------
      .py01 = "Depende de modulo LMH",
      .py02 = "Depende de modulo LMH",
      .py03 = "Depende de modulo LMH",
      .py04 = PY010N,
      .py05 = PY050N,
      .py06 = PY100N,
      .py07 = PY080N,
      .py08 = PY090N,
      .py09 = PY110N + PY120N + PY130N + PY140N,
      .py10 = .py06 + .py07,
      .py11 = .py04 + .py05,
      .py12 = .py08 + .py09 + .py10,
      .py13 = .py11 + .py12,
      .haa = (PL073 + PL074) * PL060 * 4.2,
      .han = (PL075 + PL076) * PL060 * 4.2,
      py01h = "Depende de modulo LMH",
      py02h = "Depende de modulo LMH",
      py03h = "Depende de modulo LMH",
      py04h = .py04 / .haa,
      py05h = .py05 / .han,
      dplyr::across(.py04:.py13, \(y) y / 12, .names = "{.col}m"),
      pyxxp = "py01 a py13 (h y m) / PPA correspondiente",
      .keep = "none"
    )
  # ------------------------------------------
  return(.datos)
}

# construir variables_h ------------------------------------------------------
