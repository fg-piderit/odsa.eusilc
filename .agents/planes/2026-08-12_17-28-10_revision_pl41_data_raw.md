# Prompt

> Apliqué algunos cambios para incorporar una nueva variable, pl41. Además incorporé la construcción de los datos internos y públicos en la carpeta data-raw. Los cambios abarcan los dos últimos commits. Me gustaría que revises los cambios y me digas si encuentras errores o potenciales conflictos con otros elementos del paquete.

# Plan final ejecutado

1. Delimitar los cambios de los commits `68e6a4b` y `3ed73e3` y revisar el estado del repositorio.
2. Rastrear `pl41`, sus variables auxiliares (`toc` y `pomj`) y las tablas de etiquetas en el código, los datos generados y la documentación.
3. Revisar la migración de los objetos internos `etq`/`etiquetas_` y `tabla_ppa`/`tabla_ppa_` en todos los flujos de personas y hogares.
4. Examinar la reproducibilidad de los scripts de `data-raw` y la consistencia entre los archivos XLSX, `R/sysdata.rda` y los objetos públicos de `data`.
5. Contrastar la codificación de `PL141` con la definición oficial y ejecutar verificaciones no destructivas: carga del paquete, prueba aislada de `calc_calidad()`, suite de tests y `devtools::check()`.
6. Informar los hallazgos ordenados por severidad, sin modificar el código revisado.

# Verificaciones realizadas

- `devtools::test()`: 46 tests aprobados; la suite actual no recorre rutas exitosas de cálculo/expansión.
- `devtools::check(error_on = "never", cran = FALSE)`: 0 errores, 0 warnings y 1 note; la nota identifica, entre otros, `case_when`, `etq` y `tabla_ppa` como símbolos no visibles.
- Inspección de datos: `R/sysdata.rda` contiene `etiquetas_` y `tabla_ppa_`, pero no `etq` ni `tabla_ppa`.
- Contraste oficial: la recodificación de permanencia desde `PL141` coincide con los códigos 11/12 (temporal) y 21/22 (permanente).
