#' Title
#'
#' @param .datos Conjunto de datos P a imputar.
#' @param .variables Grupo de variables a imputar.
#' @param ... ...
#' @param .N_imps Cantidad de conjuntos imputados a crear.
#'
#' @returns Conjunto de datos P con variables imputadas.
#' @export
imputar_personas <- function(
  .datos,
  .variables,
  ...,
  .N_imps = 10
) {
  # Chequeos args ------------------------------------------------------------

  # Construccion flags -------------------------------------------------------
  .datos <- dplyr::mutate(
    .datos,
    .f_maa = dplyr::case_when(
      PY010N != 0 & (is.na(PL073 + PL074) | PL073 + PL074 == 0) ~ -1,
      PY010N != 0 & !is.na(PL073 + PL074) & PL073 + PL074 != 0 ~ 1,
      .default = 0
    ),
    .f_man = dplyr::case_when(
      PY050N != 0 & (is.na(PL075 + PL076) | PL075 + PL076 == 0) ~ -1,
      PY050N != 0 & !is.na(PL075 + PL076) & PL075 + PL076 != 0 ~ 1,
      .default = 0
    ),
    .f_PL060 = PL060_F,
    .f_PL040A = PL040A_F,
    .f_PL040B = dplyr::case_when(
      PY010N != 0 & PL040B_F == -1 ~ -1,
      PY010N != 0 & PL040B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL051A = PL051A_F,
    .f_PL051B = dplyr::case_when(
      PY010N != 0 & PL051B_F == -1 ~ -1,
      PY010N != 0 & PL051B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL111A = PL111A_F,
    .f_PL111B = dplyr::case_when(
      PY010N != 0 & PL111B_F == -1 ~ -1,
      PY010N != 0 & PL111B_F == 1 ~ 1,
      .default = 0
    )
  )

  # Construccion vbles -------------------------------------------------------
  .datos <- dplyr::mutate(
    .datos,
    .maa = dplyr::case_when(
      .f_maa == -1 ~ NA_integer_,
      .default = PL073 + PL074
    ),
    .man = dplyr::case_when(
      .f_man == -1 ~ NA_integer_,
      .default = PL075 + PL076
    )
  )

  # Imputacion ---------------------------------------------------------------
  # Meses ------------------------------------
  # Random Forests para lidiar con la distribución atípica de los meses
  # Imputación múltiple para mantener variabilidad
  # TODO: Selección de hiperparámetros
  datos_imp_maa <- .datos |>
    dplyr::filter(.f_maa %in% c(-1, 1)) |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, PB140, PB150, PE041, .maa, .f_maa)

  imp_meses <- purrr::map(1:.N_imps, \(.i) {
      imp <- missRanger::missRanger(
        data = datos_imp_maa,
        formula = .maa ~ PY010N + PB140 + PB150 + PE041,
        num.trees = 100,
        pmm.k = 10
      )
      imp <- dplyr::mutate(imp, imp = .i)
      dplyr::filter(imp, .f_maa == -1)
    }) |>
    purrr::list_rbind() |>
    dplyr::slice_sample(n = 1, by = PB030)

  # Datos finales ------------------------------------------------------------
  .datos <- .datos |>
    dplyr::left_join(
      imp_meses,
      by = dplyr::join_by(PB010, PB020, PB030),
      suffix = c("", "_imp")
    ) |>
    dplyr::mutate(
      .maa = dplyr::case_when(
        .f_maa == -1 ~ .maa_imp,
        .default = .maa
      )
    )

  .datos
}
