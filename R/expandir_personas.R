expandir_personas <- function(
    .datos,
    ...,
    .D = NULL,
    .R = NULL,
    .mantener = FALSE
) {
  # Chequeos args ----------------------

  # Calcular vbles ---------------------
  bloques <- c(
    D = !is.null(.D), R = !is.null(.R), LMH = all(c("PL130", "PL230") %in% names(.datos))
  )

  if (bloques["D"]) {
    D <- .D |>
      dplyr::select(DB010, DB020, DB030, DB040)
    .datos <- .datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
      ) |>
      dplyr::rename(pi03 = DB040)
  }

  if (bloques["R"]) {
    R <- .R |>
      dplyr::select(RB010, RB020, RB030, RB081, RB082, RB280, RB290)
    .datos <- .datos |>
      dplyr::left_join(
        R, by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
      )

  }

  datos <- construir_variables_p(.datos, .pais, bloques)

  if (!.mantener) datos <- datos |> dplyr::select(-dplyr::all_of(names(.datos)))

  attr(datos, "bloques") <- bloques
  attr(datos, "base") <- "P"

  return(datos)
}
