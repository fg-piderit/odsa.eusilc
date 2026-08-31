# Reformulación de advertencias de estandarización

## Solicitud original

> Me gustaría reformular el manejo de las advertencias en las funciones de
> estandarización. Ahora mismo estas funciones cumplen dos roles: modificar el
> conjunto de datos para que quede estandarizado e informar al usuario de los
> cambios que se están realizando. Me parece más sensato que estas dos
> responsabilidades estén un poco más separadas. Mi idea es crear una tabla que
> indique, para cada año y cada país (de ser necesario), todas las variables que
> tienen advertencias. Esta tabla quedaría pública para que los usuarios la
> puedan inspeccionar, pero también la usarían las funciones internas para
> gestionar las advertencias. Ahora mismo me gustaría que formulemos un plan
> para realizar estos cambios. En primer lugar, deberíamos definir la estructura
> de la tabla y la forma en la que las funciones internas la usarían. También
> deberíamos considerar las dificultades y posibles problemas de seguir esta
> ruta. En segundo lugar, deberíamos obtener la información que se va a incluir
> en la tabla, es decir, las advertencias para cada variable en cada año y país.
> En principio, sólo nos interesan las variables que son usadas por el paquete.
> En tercer lugar, deberíamos incorporar esta tabla en el código. Por ahora me
> gustaría que me ayudes a formular un plan preciso por etapas que quede
> registrado en la carpeta de planes y que pueda recuperar para ir avanzando en
> cada etapa.

## Aclaraciones posteriores

- La primera cobertura comprende ES, IT, DE, PL y PT entre 2014 y 2025.
- Se registran pérdidas, problemas UDB y comparabilidad imperfecta, no
  transformaciones transparentes.
- Las ausencias normales de PL130 y PL230 por el módulo rotativo no forman
  parte de la tabla de advertencias. Su calendario se mantiene por ahora en
  código.
- Las correcciones del predicado italiano y de la disponibilidad efectiva de
  PL130/PL230 quedan para un plan separado.
- No se crea una tabla pública de disponibilidad modular en este trabajo.

## Plan final ejecutado

### 1. Tablas públicas

- [x] Crear `advertencias_estandarizacion`, con una fila por advertencia, año,
  país y variable original.
- [x] Crear `cobertura_advertencias`, con una fila por país y año revisado.
- [x] Usar títulos generales de documentos como fuente, sin distribuir los
  documentos de `.misc`.

### 2. Inventario

- [x] Inventariar las variables P/H/D/R que afectan resultados del paquete.
- [x] Registrar las dependencias de cada variable advertida.
- [x] Migrar las advertencias vigentes y contrastarlas con la documentación.
- [x] Excluir renombres transparentes, insumos opcionales ausentes y ausencias
  normales de módulos rotativos.

### 3. Relevamiento

- [x] Revisar las operaciones 2014–2020.
- [x] Revisar las operaciones 2021–2025.
- [x] Usar las últimas versiones transversales de diferencias UDB, planillas de
  problemas/modificaciones y guías metodológicas.
- [x] Completar la cobertura para ES, IT, DE, PL y PT.

### 4. Incorporación

- [x] Generar y documentar los objetos públicos desde `data-raw`.
- [x] Crear helpers internos de consulta, cobertura y presentación con `cli`.
- [x] Hacer silenciosas las funciones internas de transformación.
- [x] Emitir un único resumen desde los cuatro puntos de entrada.
- [x] Agregar `consultar_advertencias(.datos)`.
- [x] Mantener el calendario de PL130/PL230 encapsulado en código interno.

### 5. Verificación

- [x] Validar claves, tipos, categorías y las 60 combinaciones de cobertura.
- [x] Verificar consultas con bases P/H crudas, estandarizadas y expandidas.
- [x] Comparar escenarios sintéticos para asegurar que las transformaciones no
  cambien.
- [x] Ejecutar documentación, carga del paquete y tests existentes.

## Estructura acordada

`advertencias_estandarizacion`: `id_advertencia`, `anio`, `pais`, `base`,
`conjunto_origen`, `variable`, `tipo`, `advertencia`, `consecuencia`,
`accion_paquete` y `fuente`.

`cobertura_advertencias`: `anio`, `pais`, `estado` y `fuentes`, donde sólo
`revisado` permite interpretar la ausencia de filas como ausencia de hallazgos.

## Resultado

- Se generaron 706 filas de advertencias y 60 filas de cobertura.
- Se incorporaron hallazgos adicionales de las planillas UDB para los
  identificadores de 2014 y las rupturas de ingresos italianas de 2015.
- La documentación y los 46 tests existentes finalizaron correctamente.
- `devtools::check()` finalizó con 0 errores, 0 warnings y 1 note preexistente
  sobre símbolos de evaluación no estándar.
