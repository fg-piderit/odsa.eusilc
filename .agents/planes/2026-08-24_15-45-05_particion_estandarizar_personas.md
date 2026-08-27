# Partición de `estandarizar_personas_()`

## Solicitud original

> En esta rama vamos a trabajar sobre las funciones de estandarización de los
> conjuntos de datos. En
> `.agents/sugerencias/2026-08-18_14-43-20_estandarizacion_por_pais.md` dejaste
> algunas recomendaciones la última vez que te consulté. Me gustaría que
> abordemos algunas de ellas, pero no todas. La primera que quisiera abordar es
> la particion de las función interna `expandir_personas_` en varias
> sub-funciones. Me gustaría que haya una para la estandarización por año que es
> común a todos los países, una para el traspaso de variables del conjunto D al
> P y otra para lo mismo entre los conjuntos R y P, una para los ajustes por
> país (por ahora los haremos todos en un mismo lugar) y una para crear las
> funciones auxiliares. La primera puede requerir algunos ajustes específicos,
> ya que la estandarización por año involucra también al conjunto R. Cualquier
> problema que encuentres o ambigüedad que haya quedado, por favor coméntamelo.

Se confirmó que la función que debía particionarse era
`estandarizar_personas_()` y que la última etapa debía crear las variables
auxiliares `maa`, `man`, `toc` y `pomj`.

## Ajuste posterior

> Está muy bien, pero preferiría que los nombres de las funciones sean los
> siguientes: `estandarizar_anio_personas()`, `agregar_r_personas()`,
> `agregar_d_personas()`, `estandarizar_paises_personas()` y
> `calc_auxiliares_personas()`. Si notas alguna incongruencia o problema,
> dímelo.

Se acordó usar `calcular_auxiliares_personas()` en lugar de
`calc_auxiliares_personas()` para respetar la convención de comenzar los
nombres de las funciones con un verbo en infinitivo.

## Plan final ejecutado

### Resumen

Refactorizar `estandarizar_personas_()` como orquestadora de cinco etapas
internas, sin modificar resultados, mensajes ni interfaces públicas.

### Cambios

- Mantener la firma de `estandarizar_personas_()` y sus llamadas desde
  `estandarizar_personas()` y `expandir_personas()`.
- Crear en `R/estandarizar_personas.R`:
  - `estandarizar_anio_personas(.P, .anio)`: armonizar nombres, categorías y
    flags según el período.
  - `agregar_r_personas(.P, .R)`: unir R o crear valores sustitutos o ausentes.
    La orquestadora sólo llama a esta función para 2021 o años posteriores.
  - `agregar_d_personas(.P, .D)`: incorporar `DB040` o crearla como ausente.
  - `estandarizar_paises_personas(.P, .anio, .pais)`: reunir el ajuste italiano
    de `PY120N` y el aviso para Alemania anterior a 2020.
  - `calcular_auxiliares_personas(.P, .anio)`: construir `maa`, `man`, `toc` y
    `pomj`.
- Ejecutar las etapas en este orden: año, R a P, D a P, avisos de
  disponibilidad, ajustes por país y variables auxiliares.
- Mantener en la orquestadora los avisos sobre `PL130` y `PL230`.
- Conservar los mensajes de `cli`, el comportamiento ante D o R ausentes y el
  orden original de las columnas.
- No introducir todavía un registro de ajustes por país.

### Verificación

- Cargar el paquete para detectar errores de sintaxis o dependencias.
- Ejecutar los tests existentes.
- Comparar la salida con la versión anterior para períodos anteriores y
  posteriores a 2021, con y sin R, con y sin D, y para los ajustes de Italia y
  Alemania.
- No agregar nuevos tests en este refactor.

### Resultado

El paquete cargó correctamente y los 46 tests existentes finalizaron sin
fallos, advertencias ni omisiones. Las salidas de seis escenarios sintéticos
fueron idénticas a las de la versión anterior, incluido el orden de columnas.
