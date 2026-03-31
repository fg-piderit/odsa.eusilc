expandir_hogares <- function(
    .datos,
    .P,
    ...,
    .D = NULL,
    .mantener = FALSE
) {
  # Chequeos args ----------------------

  # Calcular vbles ---------------------
  bloques <- c(
    D = !is.null(.D), attr(.P, "bloques")["LMH"]
  )

  if (bloques["D"]) {
    D <- .D |>
      select(DB010, DB020, DB030, DB040, DB090)
    .datos <- .datos |>
      dplyr::left_join(
        D, by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 = DB030)
      )
  }

  P <- agregar_p(.P)
  datos <- .datos |>
    dplyr::left_join(
      P, by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
    )

  datos <- construir_variables_h(datos, .pais, bloques, P, .mantener = .mantener)

  return(datos)
}
