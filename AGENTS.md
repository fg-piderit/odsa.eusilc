# Contenidos del repositorio

Este repositorio contiene el código de un paquete de R cuyo propósito es
transformar los conjuntos de datos de la _European Union Statistics on Income
and Living Conditions_. Los conjuntos de datos NO se incluyen ni se deben
incluir en el paquete; se espera que los usuarios accedan a los datos a través
de los medios apropiados.

El repositorio tiene los siguientes subdirectorios:

- \data: Archivos .rda con datos auxiliares que pueden ser de utilidad para el usuario.
- \man: Documentación de los objetos contenidos en el paquete.
- \R: Funciones del paquete y datos auxiliares internos.
- \tests: Tests para las funciones del paquete.

# Comandos

```
# Para correr código
Rscript -e "devtools::load_all(); codigo"

# Para correr todos los tests
Rscript -e "devtools::test()"

# Para documentar el paquete
Rscript -e "devtools::document()"

# Para chequear el paquete
Rscript -e "devtools::check()"

# Para crear nuevos archivos .r en \R 
Rscript -e "usethis::use_r('{nombre_del_archivo}')"

# Para crear tests asociados a un archivo
Rscript -e "usethis::use_test('{nombre_del_archivo}')"

# Para incluir datos en el paquete
Rscript -e "usethis::use_data({datos})

# Si los datos son internos únicamente
Rscript -e "usethis::use_data({datos}, internal = TRUE)"

# Para incluir un paquete en las dependencias
Rscript -e "usethis::use_package('{paquete}')
```

# Código

Cuando generes nuevo código, me gustaría que:

- Priorices la legibilidad por sobre la completitud; genera código que cumpla
  su función, sin hacer chequeos exhaustivos ni generar inmediatamente los
  tests necesarios.
- Si generas output a la línea de comandos, prioriza las funciones del paquete
  `cli`.

En cuanto a los chequeos, me gustaría que:

- Los generes cuando te los pida.
- Priorices las funciones del paquete `rlang`.
- Los ubiques en los puntos de entrada de los objetos, de forma tal que las
  funciones internas puedan dar por supuesto que son adecuados.
- Los ubiques al inicio de las funciones.

En cuanto a los tests, recuerda que NO disponemos de los datos de la encuesta
en este paquete, por lo cual no podemos crear tests que dependan de esos datos.
Sólo podemos testear funcionalidades que NO dependan de esos datos.

En cuanto al estilo del código, me gustaría que:

- Uses el español para nombrar los objetos.
- Uses snake_case para nombrar los objetos.
- Uses verbos en infinitivo al inicio de los nombres de funciones (por ejemplo,
  `calcular_indicador()`).
- Uses el pipe de R base `|>`, no el del paquete `magrittr`, `%>%`.

# Control de cambios

- No crees nuevas branches ni cambies de branch sin consultarme.
- No hagas commits.

# Planificación

En cuanto a la planificación, me gustaría que:

- Me pidas todas las aclaraciones que consideres necesarias.
- Me hagas notar posibles problemas o inconsistencias en lo que te pido.
- Sólo para planes extensos que desarrollemos en Plan Mode, guardes el prompt
  que te di y el plan final ejecutado en un archivo markdown en la carpeta
  `\.agents\planes`. El nombre del archivo debe tener un timestamp con formato
  "YYYY-MM-DD_hh-mm-ss" seguido de una breve descripción.
- Si te pido que guardes una sugerencia, hazlo en `\.agents\sugerencias` con el
  nombre `YYYY-MM-DD_hh-mm-ss_descripcion_breve.md`.
