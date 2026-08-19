# Estandarización extensible por país

## Problema

Las funciones `estandarizar_*()` deben resolver diferencias comunes a todos
los países y excepciones específicas de algunos países y períodos. A medida
que se incorporen nuevas excepciones, estas funciones pueden acumular muchas
responsabilidades y condiciones.

## Sugerencia

Mantener las funciones públicas `estandarizar_personas()` y
`estandarizar_hogares()` como únicos puntos de entrada y organizar su
implementación interna como una secuencia explícita de etapas:

1. Validar los conjuntos de entrada.
2. Normalizar las diferencias comunes entre períodos y fuentes.
3. Aplicar los ajustes específicos del país.
4. Construir las variables comunes finales.
5. Asignar los atributos de la base estandarizada.

La función interna principal debería actuar como orquestadora, por ejemplo:

```r
estandarizar_personas_ <- function(.P, .R, .D, .anio, .pais) {
  .P |>
    normalizar_periodo_personas(.R, .anio) |>
    incorporar_datos_hogar(.D) |>
    aplicar_ajustes_pais_personas(.pais, .anio) |>
    construir_variables_estandar_personas()
}
```

Los nombres y argumentos concretos deberán adaptarse a las necesidades de
cada etapa.

## Registro de ajustes por país

Para evitar una sucesión creciente de condiciones `if`, se puede mantener un
registro interno que relacione cada país con las transformaciones que le
corresponden:

```r
ajustes_personas_por_pais <- list(
  IT = list(
    ajustar_py120n_italia
  ),
  ES = list(
    ajustar_variable_x_espania,
    ajustar_variable_y_espania
  )
)
```

Un aplicador genérico ejecutaría las transformaciones en el orden declarado:

```r
aplicar_ajustes_pais_personas <- function(.P, .pais, .anio) {
  ajustes <- ajustes_personas_por_pais[[.pais]]

  if (is.null(ajustes)) {
    return(.P)
  }

  for (ajustar in ajustes) {
    .P <- ajustar(.P, .anio)
  }

  .P
}
```

## Granularidad de las funciones

Conviene nombrar las funciones según la transformación que realizan, no sólo
según el país. Algunos ejemplos serían:

- `ajustar_py120n_italia()`
- `recodificar_actividad_espania()`
- `completar_region_alemania()`

De esta manera, el nombre documenta la regla aplicada y una transformación
puede generalizarse fácilmente si luego se descubre que afecta a varios
países. Si un país acumula muchas reglas relacionadas, una función como
`ajustar_personas_italia()` puede utilizarse como orquestadora de ajustes más
pequeños, sin concentrar en ella toda la lógica.

## País y período

Es probable que varias excepciones dependan tanto del país como del año. El
año puede permanecer como argumento de cada ajuste:

```r
ajustar_variable_x_espania <- function(.P, .anio) {
  if (.anio < 2018) {
    # Transformación.
  }

  .P
}
```

No parece necesario crear inicialmente un registro para cada combinación de
país y año. Si en el futuro aparecen muchas reglas simples y repetitivas,
podría evaluarse representarlas mediante una tabla de configuración.

## Contrato y orden de ejecución

El orden de las etapas debe formar parte del diseño. En particular, conviene
establecer que:

- los ajustes nacionales reciben variables cuyos nombres ya fueron
  estandarizados;
- pueden corregir o completar esas variables sin alterar el esquema esperado;
- las variables derivadas comunes se construyen después de esos ajustes.

Esto evita que cada función nacional tenga que conocer las diferencias de
nombres y estructuras entre períodos.

## Alcance recomendado

Por el momento, un registro explícito de funciones resulta más legible que un
sistema de métodos S3 o un framework general de reglas. Además, las funciones
pequeñas permiten probar las excepciones con datos sintéticos, sin depender de
los conjuntos reales de EU-SILC.
