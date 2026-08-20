# Chequeo de insumos completamente perdidos

## Solicitud original

> Me gustaría que agregues una función llamada `chequear_insumos_perdidos` en el archivo `R/chequear.R` que chequee si alguno de un número arbitrario de vectores está completamente perdido y devuelva `TRUE` si es así. Usa `any` y `vapply` como mencionaste antes. Luego, me gustaría que agregues ese chequeo en todas las funciones `calc_*` en `R/calcular_personas.R` que no se apliquen dentro de los controles de flujo asociados con las variables `PL130` y `PL230`, de forma tal que esas funciones devuelvan `NA_integer_` en caso de que falte un insumo.

## Ajuste posterior

> Ahora que lo pienso, quizás conviene agregar esos chequeos en todas las funciones de `calc_*` ya que no quiero que esos indicadores sean calculados si faltan insumos, pero el control de flujos sólo chequea que estén `PL130` o `PL230`. Por ejemplo, en `calc_y_sector`, no quiero que los indicadores queden en 0 si en realidad están completamente perdidos.

Se acordó mantener `chequear_insumos_perdidos()` como función interna y usar `NA_real_` para el retorno anticipado de `calc_y_sector()`.

## Plan final ejecutado

### Resumen

Agregar una función interna `chequear_insumos_perdidos(...)` y utilizarla al inicio de todas las funciones `calc_*` de `R/calcular_personas.R`. Cada indicador dejará de calcularse cuando cualquiera de sus vectores insumo esté completamente perdido, independientemente de los controles de presencia de `PL130` y `PL230`.

### Cambios

- En `R/chequear.R`, implementar `chequear_insumos_perdidos(...)` mediante `list(...)`, `vapply()` y `any()`.
- Considerar completamente perdido sólo un vector no vacío cuyos elementos sean todos `NA`. Sin argumentos o con vectores vacíos, devolver `FALSE`.
- Mantener la función interna, sin exportarla.
- Agregar el chequeo al inicio de:
  - `calc_heterogeneidad()`: comprobar sus seis vectores; excluir `.nivel`.
  - `calc_egp()`: comprobar sus tres vectores.
  - `calc_informalidad()`: comprobar sus tres vectores y reemplazar el chequeo anterior limitado a `.PY030G`; excluir `.nivel`.
  - `calc_calidad()`: comprobar sus tres vectores.
  - `calc_y_sector()`: comprobar `.py10` y `.pl31`; excluir `.sector`.
  - `calc_variante_c()`: comprobar sus tres vectores.
- Retornar anticipadamente `NA_integer_` en los indicadores categóricos y `NA_real_` en `calc_y_sector()`, preservando el tipo monetario de `py13`, `py14` y `py15`.
- Mantener sin cambios los controles de flujo de `PL130` y `PL230`: gestionan columnas ausentes, mientras que las funciones `calc_*` gestionan columnas presentes pero completamente perdidas.

### Verificación

- Comprobar que `chequear_insumos_perdidos()` devuelve `TRUE` cuando al menos un vector está completamente perdido y `FALSE` ante pérdidas parciales, ausencia de argumentos o vectores vacíos.
- Verificar que cada `calc_*` retorna anticipadamente cuando alguno de sus vectores insumo está completamente perdido.
- Confirmar que `.nivel` y `.sector` no se consideran insumos sujetos al chequeo.
- Confirmar que las funciones conservan su comportamiento habitual cuando ningún insumo está completamente perdido.
- No agregar archivos de tests; realizar comprobaciones puntuales cargando el paquete.

### Resultado

El plan fue implementado y las comprobaciones puntuales finalizaron correctamente.
