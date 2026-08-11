# Metadata y trazabilidad de los objetos EU-SILC

## Motivación

Los atributos son apropiados para describir propiedades del conjunto completo,
pero resultan frágiles cuando se utilizan como atributos informales y dispersos:

```r
attr(.P, "base")
attr(.P, "estandar")
attr(.P, "imputada")
attr(.P, "flags imp.")
attr(.P, "vble. PL230")
```

La fragilidad tiene cuatro causas principales:

1. Algunas transformaciones pueden eliminar atributos personalizados. Su
   preservación depende de la función y del paquete que reconstruye el objeto.
2. Un atributo ausente es ambiguo: puede representar `FALSE`, un objeto antiguo,
   una transformación que eliminó metadata o un objeto ajeno al paquete.
3. No existe un esquema central que determine qué atributos debe tener cada
   objeto ni qué combinaciones son válidas.
4. Los atributos actuales describen principalmente el estado final, pero no
   registran las operaciones que condujeron hasta él.

Por ejemplo, en `calcular_hogares()` se reemplaza `.P` por el resultado de
`agregar_personas(.P)` y posteriormente se consulta el atributo
`"vble. PL230"`. No es evidente que ese atributo sobreviva a la agregación.

## Alternativa recomendada

Mantener el resultado como un `tibble`, pero asignarle una clase S3 propia y un
único atributo de metadata estructurada:

```r
metadata <- list(
  version_esquema = 1L,
  tipo = "personas",
  anio = 2022,
  pais = "ES",
  estado = list(
    estandarizada = TRUE,
    imputada = FALSE,
    calculada = TRUE,
    etiquetada = TRUE,
    originales_conservadas = FALSE
  ),
  insumos = list(
    conjunto_d = TRUE,
    conjunto_r = TRUE,
    variable_pl130 = TRUE,
    variable_pl230 = FALSE
  ),
  historial = list(
    list(
      operacion = "estandarizar_personas",
      parametros = list(flags = FALSE)
    ),
    list(
      operacion = "calcular_personas",
      parametros = list(expandir = FALSE)
    ),
    list(
      operacion = "etiquetar_eusilc",
      parametros = list()
    )
  )
)
```

Un constructor podría crear el objeto de la siguiente manera:

```r
crear_resultado_personas <- function(datos, metadata) {
  attr(datos, "odsa_eusilc") <- metadata
  class(datos) <- c("odsa_eusilc_personas", class(datos))
  datos
}
```

Las funciones siguientes validarían la clase y la versión del esquema:

```r
validar_resultado_personas <- function(datos) {
  if (!inherits(datos, "odsa_eusilc_personas")) {
    cli::cli_abort(
      "El objeto debe haber sido procesado como una base de personas.",
      class = "no_resultado_personas"
    )
  }

  metadata <- attr(datos, "odsa_eusilc")

  if (is.null(metadata) || metadata$version_esquema != 1L) {
    cli::cli_abort(
      "La metadata del objeto no está disponible o es incompatible.",
      class = "metadata_incompatible"
    )
  }

  invisible(datos)
}
```

El acceso y la actualización de la metadata deberían centralizarse:

```r
obtener_metadata <- function(datos) {
  attr(datos, "odsa_eusilc")
}

esta_estandarizada <- function(datos) {
  obtener_metadata(datos)$estado$estandarizada
}

registrar_operacion <- function(datos, operacion, parametros = list()) {
  metadata <- obtener_metadata(datos)

  metadata$historial <- append(
    metadata$historial,
    list(list(
      operacion = operacion,
      parametros = parametros
    ))
  )

  attr(datos, "odsa_eusilc") <- metadata
  datos
}
```

## Estado e historial

Las funciones deberían decidir qué hacer mediante el estado actual:

```r
metadata$estado$estandarizada
metadata$estado$imputada
metadata$insumos$variable_pl230
```

El historial debería reservarse para auditoría y diagnóstico:

```r
metadata$historial
```

No conviene reconstruir el estado recorriendo el historial completo. Mantener
ambos conceptos separados hace que las validaciones sean simples y conserva la
trazabilidad requerida.

## Preservación durante transformaciones

La clase S3 permitiría implementar métodos de reconstrucción —por ejemplo,
`dplyr_reconstruct.odsa_eusilc_personas()`— para restaurar deliberadamente la
clase y la metadata después de operaciones de `dplyr`. Así su conservación no
dependería del comportamiento incidental de cada verbo.

## Alternativa más robusta, pero más invasiva

La metadata también podría ser un componente ordinario de una lista:

```r
resultado <- structure(
  list(
    datos = personas,
    metadata = metadata
  ),
  class = "resultado_eusilc"
)
```

Esto protege la metadata de las transformaciones aplicadas a `resultado$datos`,
pero cambia la experiencia de uso y requeriría métodos adicionales para que el
objeto se comporte como un `tibble`.

## Recomendación

1. Crear las clases `odsa_eusilc_personas` y `odsa_eusilc_hogares`.
2. Reemplazar los atributos dispersos por un único atributo `odsa_eusilc`.
3. Definir y versionar el esquema de metadata.
4. Separar `estado`, `insumos` e `historial`.
5. Centralizar el acceso y la actualización mediante funciones auxiliares.
6. Implementar la reconstrucción de los objetos después de operaciones de
   `dplyr`.
7. Interpretar la metadata ausente como estado desconocido, no como `FALSE`.

Esta solución conserva la ergonomía actual —el resultado continúa siendo un
`tibble`— y convierte los atributos en un protocolo explícito, trazable y
verificable.
