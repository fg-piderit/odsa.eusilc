# Guardar sugerencia sobre metadata de objetos

## Prompt

> Bien, podríamos implementar esta sugerencia en un futuro. Por ahora, me
> gustaría que guardes la sugerencia en la carpeta que te indique anteriormente
> con el mismo esquema de nomenclatura. Además, me gustaría que agregues una
> línea al AGENTS.md en la que se te explicite que si te pido que guardes una
> sugerencia, lo hagas en esa carpeta y con ese formato y nombre.

## Plan final ejecutado

1. Obtener un timestamp con el formato definido en `AGENTS.md`.
2. Guardar la sugerencia sobre metadata y clases S3 en
   `.agents/sugerencias`, usando el timestamp y una descripción breve.
3. Agregar a `AGENTS.md` la instrucción para guardar futuras sugerencias en esa
   carpeta con el mismo esquema de nombres.
4. Verificar los archivos creados y el cambio realizado.
