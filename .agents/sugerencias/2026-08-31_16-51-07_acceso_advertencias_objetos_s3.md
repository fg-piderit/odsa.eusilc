# Acceso a las advertencias de objetos EU-SILC

## Objetivo

Proporcionar una función pública que devuelva las advertencias asociadas a una
base EU-SILC armonizada, manteniendo una interfaz estable tanto con la
representación actual mediante atributos como con una futura clase S3.

## Recomendación principal

Definir desde el comienzo `obtener_advertencias()` como un genérico S3. La API
para el usuario no cambiaría cuando se introduzcan las clases propias del
paquete.

En la implementación actual, el método para `data.frame` podría leer el
atributo existente:

```r
obtener_advertencias <- function(.datos, ...) {
  UseMethod("obtener_advertencias")
}

obtener_advertencias.data.frame <- function(.datos, ...) {
  advertencias <- attr(.datos, "advertencias", exact = TRUE)

  if (is.null(advertencias)) {
    rlang::abort(
      "El objeto no contiene información sobre advertencias.",
      class = "advertencias_no_disponibles"
    )
  }

  advertencias
}

obtener_advertencias.default <- function(.datos, ...) {
  rlang::abort(
    "No se pueden obtener advertencias de este tipo de objeto.",
    class = "objeto_no_compatible"
  )
}
```

La función interna que actualmente se llama `obtener_advertencias()` debería
recibir otro nombre, por ejemplo `consultar_advertencias()` o
`buscar_advertencias()`. Como no está exportada, el cambio no afectaría la API
pública.

## Ausencia de advertencias y ausencia de metadata

Estos casos no deberían confundirse:

- Un `tibble` de cero filas, pero con el esquema esperado, significa que se
  realizó la revisión y no hay advertencias documentadas.
- Un atributo ausente significa que la información es desconocida: el objeto
  podría no provenir del paquete o una transformación podría haber eliminado
  su metadata.

Por lo tanto, el accesor debería producir un error cuando la metadata no está
disponible, en lugar de devolver `NULL` o una tabla vacía.

## Implementación con una clase S3

Conviene que las clases de personas y hogares hereden de una clase común:

```r
class(datos) <- c(
  "odsa_eusilc_personas",
  "odsa_eusilc",
  class(datos)
)
```

```r
class(datos) <- c(
  "odsa_eusilc_hogares",
  "odsa_eusilc",
  class(datos)
)
```

Las advertencias no describen un estado booleano, un insumo ni una operación
del historial. Deberían ubicarse directamente en la metadata estructurada:

```r
metadata <- list(
  version_esquema = 1L,
  tipo = "personas",
  anio = 2022,
  pais = "ES",
  advertencias = advertencias,
  estado = list(...),
  insumos = list(...),
  historial = list(...)
)
```

Un único método para la clase padre sería suficiente mientras personas y
hogares compartan el mismo contrato de retorno:

```r
obtener_advertencias.odsa_eusilc <- function(.datos, ...) {
  metadata <- attr(.datos, "odsa_eusilc", exact = TRUE)

  if (is.null(metadata) || is.null(metadata$advertencias)) {
    rlang::abort(
      "La metadata del objeto no contiene información sobre advertencias.",
      class = "advertencias_no_disponibles"
    )
  }

  metadata$advertencias
}
```

Durante una transición podrían coexistir el método
`obtener_advertencias.data.frame()` para objetos antiguos y el método
`obtener_advertencias.odsa_eusilc()` para los nuevos. El primero podría
deprecarse cuando ya no sea necesaria la compatibilidad.

## Qué conviene guardar

Se consideraron tres alternativas:

1. Guardar en el objeto el `tibble` completo de advertencias.
2. Guardar solamente los identificadores de las advertencias.
3. Guardar país, año y tipo de base, y volver a consultar la tabla interna cada
   vez que se use el accesor.

Se recomienda la primera alternativa. Conserva una fotografía de las
advertencias conocidas en el momento de armonizar la base y favorece la
reproducibilidad si la tabla interna cambia en una versión posterior del
paquete. Dado que las advertencias asociadas a una base deberían ser pocas, el
costo de almacenar el `tibble` completo no parece relevante.

Guardar identificadores reduciría la duplicación, pero haría que las
descripciones dependieran de la versión instalada. Recalcular completamente
las advertencias a partir del contexto sería todavía menos reproducible.

La función interna `obtener_contexto_advertencias()` podría utilizarse como
mecanismo de migración o recuperación para objetos antiguos. No debería ser el
comportamiento principal, porque inferir la metadata a partir de nombres y
valores de columnas es menos fiable que leerla de un objeto creado mediante un
protocolo explícito.

## Advertencias documentadas y diagnósticos de procesamiento

El atributo actual contiene advertencias documentadas por país, año y
variable. Durante el procesamiento también se comunican situaciones como la
ausencia de los conjuntos D o R, pero esos mensajes no quedan almacenados.

Inicialmente, `obtener_advertencias()` debería conservar el significado y el
tipo de retorno actuales: devolver únicamente el `tibble` de advertencias
documentadas.

Si en el futuro se registran también los diagnósticos producidos durante la
armonización, convendría distinguirlos explícitamente:

```r
metadata$advertencias <- list(
  documentadas = advertencias_documentadas,
  procesamiento = advertencias_procesamiento
)
```

No obstante, para evitar cambiar el contrato de `obtener_advertencias()`, sería
preferible ofrecer otro accesor, como `obtener_diagnosticos()`, para los
eventos del procesamiento.

## Separación entre consulta y comunicación

La consulta de las advertencias y su presentación en la consola deberían ser
responsabilidades distintas:

- Una función interna consulta y devuelve el `tibble` sin efectos secundarios.
- Otra función informa el resultado mediante `cli` durante la armonización.
- El accesor público se limita a devolver las advertencias almacenadas y no
  imprime mensajes.

Esta separación facilita la reutilización, los tests y el uso programático del
resultado.

## Síntesis

1. Crear `obtener_advertencias()` como genérico S3 público.
2. Mantener inicialmente un método para `data.frame` que lea el atributo
   `"advertencias"`.
3. Introducir luego un método para la clase padre `odsa_eusilc`.
4. Guardar el `tibble` completo dentro de la metadata estructurada.
5. Tratar la metadata ausente como información desconocida, no como ausencia
   de advertencias.
6. Mantener separadas las advertencias documentadas y los diagnósticos del
   procesamiento.
7. Separar la consulta de datos de los mensajes mostrados con `cli`.
