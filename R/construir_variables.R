# Funciones para construir las variables de las bases finales
#
# construir variables_p ------------------------------------------------------
construir_variables_p <- function(
    .datos,
    .pais,
    .bloques,
    ...
) {
  datos <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      pi01 = PB010,
      pi02 = PB020,
      pi04 = PX030,
      pi05 = PB030,
      pi06 = PB040,
      # Bloque D -----------------------
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
      # Bloque L -----------------------
      pl01 = "a definir",
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
      pl08a = dplyr::case_when(
        PL051A %in% 11:13 | PL051A %/% 10 == 2 ~ 1,
        PL051A == 14 | PL051A %/% 10 == 3 ~ 2,
        PL051A %/% 10 %in% 4:8 ~ 3,
        PL051A %/% 10 == 9 ~ 4,
        .default = NA
      ),
      pl08b = dplyr::case_when(
        PL051A == 2 | (PL051A >= 20 & PL051A <= 35) ~ 1,
        !is.na(PL051A) ~ 2,
        .default = NA
      ),
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
      pl11a = "a definir",
      pl11b = "a definir",
      # Bloque Y -----------------------
      py04 = PY010N,
      py05 = PY050N,
      py06 = PY100N,
      py07 = PY080N,
      py08 = PY090N,
      py09 = PY110N + PY120N + PY130N + PY140N,
      py10 = py06 + py07,
      py11 = py04 + py05,
      py12 = py08 + py09 + py10,
      py13 = py11 + py12,
      .haa = (PL073 + PL074) * PL060 * 4.2,
      .han = (PL075 + PL076) * PL060 * 4.2,
      py04h = py04 / .haa,
      py05h = py05 / .han,
      dplyr::across(py04:py13, \(y) y / 12, .names = "{.col}m"),
      pyxxq = "py01 a py13 (h y m) / PPA correspondiente",
      .keep = "all"
    )

  if (.bloques["R"]) {
    datos <- datos |>
      dplyr::mutate(
        pd01a = RB082,
        pd01b = RB081,
        pd04 = dplyr::if_else(RB280 == pi02, 1, 2),
        pd05 = dplyr::if_else(RB290 == pi02, 1, 2),
        .keep = "all"
      )
  }

  if (.bloques["LMH"]) {
    datos <- datos |>
      dplyr::mutate(
        # Bloque L -----------------------
        pl06a = dplyr::case_when(
          PL130 <= 5 ~ 1,
          PL130 > 5 & PL130 <= 9 ~ 2,
          PL130 > 9 & PL130 <= 11 ~ 3,
          PL130 > 11 & PL130 <= 13 ~ 4,
          .default = NA
        ),
        pl06b = dplyr::case_when(
          PL130 <= 5 ~ 1,
          PL130 > 5 & PL130 <= 11 ~ 2,
          PL130 > 11 & PL130 <= 13 ~ 3,
          .default = NA
        ),
        pl07 = dplyr::case_when(
          PL230 == 1 ~ 1,
          PL230 == 2 ~ 2,
          .default = NA
        ),
        pl09a = dplyr::case_when(
          PL040A == 1 & pl07 == 2 & pl06b > 1 ~ 1,
          PL040A == 2 & pl07 == 2 & pl08b == 1 ~ 2,
          pl02 == 1 & pl07 == 1 ~ 3,
          PL040A == 3 & pl07 == 2 & pl06b == 3 ~ 4,
          PL040A == 3 & pl07 == 2 & pl06b == 2 ~ 5,
          PL040A == 1 & pl07 == 2 & pl06b == 1 ~ 6,
          PL040A == 2 & pl07 == 2 & pl08b == 2 ~ 7,
          PL040A == 3 & pl07 == 2 & pl06b == 1 ~ 8,
          pl02 == 1 & pl05 == 8 ~ 9,
          .default = NA
        ),
        pl09b = dplyr::case_when(
          pl09a == 3 ~ 1,
          pl09a %in% c(1, 2, 4, 5, 9) ~ 2,
          pl09a %in% c(7, 8) ~ 3,
          .default = NA
        ),
        # Bloque Y -----------------------
        py01 = dplyr::if_else(py11 != 0 & pl09b == 1, py11, 0),
        py02 = dplyr::if_else(py11 != 0 & pl09b == 2, py11, 0),
        py03 = dplyr::if_else(py11 != 0 & pl09b == 3, py11, 0),
        across(py01:py03, \(y) y / .haa, .names = "{.col}h"),
        .keep = "all"
      )
  }


  # ------------------------------------------
  return(datos)
}

# agregar_p ------------------------------------------------------------------
agregar_personas <- function(.personas) {
  if (attr(.personas, "bloques")["LMH"]) {
    personas <- .personas |>
      dplyr::summarise(
        # Bloque Y -----------------------
        across(c(py01:py03, py04:py13), sum, .names = "{.col}p"),
        # Bloque P -----------------------
        across(c(py01:py03, py04:py13), \(y) sum(y != 0), .names = "x{.col}"),
        .by = c(pi01, pi02, pi04)
      )
  } else {
    personas <- .personas |>
      dplyr::summarise(
        # Bloque Y -----------------------
        dplyr::across(py04:py13, sum, .names = "{.col}p"),
        # Bloque P -----------------------
        dplyr::across(py04:py13, \(y) sum(y != 0), .names = "x{.col}"),
        .by = c(pi01, pi02, pi04)
      )
  }

  personas <- personas |>
    dplyr::rename_with(.cols = dplyr::starts_with("py"), .fn = \(n) sub("py", "hy", n)) |>
    dplyr::rename_with(.cols = dplyr::starts_with("xpy"), .fn = \(n) sub("xpy", "hp", n))

  return(personas)
}

# construir_variables_h ------------------------------------------------------
construir_variables_h <- function(
    .datos,
    .pais,
    .ind,
    ...
) {
  hogares <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      hi01 = HB010,
      hi02 = HB020,
      hi04 = HB030,
      # Bloque D -----------------------
      hd01 = HX040,
      thogara = "A definir",
      thogarb = "A definir",
      # Bloque L -----------------------
      # Bloque Y -----------------------
      hy14 = HY040N + HY090N,
      hy15 = HY050N + HY060N + HY070N,
      hy16 = HY080N + HY110N,
      hy17 = hy14 + hy16,
      hy18 = hy08p + hy09p + hy15,
      hy19 = hy10p + hy18,
      hy20 = hy12p + hy14 + hy15 + hy16,
      hy21 = hy13p + hy14 + hy15 + hy16,
      dplyr::across(
        .cols = c(hy04p:hy13p, hy14:hy21),
        .fns = \(y) y / 12, .names = "{.col}m"
      ),
      dplyr::across(
        .cols = c(hy04p:hy13p, hy14:hy17, hy18:hy21),
        .fns = \(y) y / hd01, .names = "{.col}c"
      ),
      hyxxq = "hy01p a hy21 / PPA correspondiente",
      .keep = "all"
    )

  # ------------------------------------------
  return(hogares)
}
