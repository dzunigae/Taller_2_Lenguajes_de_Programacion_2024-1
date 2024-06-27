# Reglas generales
  
NOTA: Estas reglas están más pensadas para el analizador sintáctico (y semántico)  
  
Las variables tienen scope dependiendo de la estructura dentro de la cual fueron declaradas.
El tamaño de los arreglos es inmutable una vez definidos.  
Alfa, beta, tau y delta sólo pueden ser declaradas dentro de un modelo.  
Los modelos y alfabetos pueden cambiar sus valores internamente más no como asignaciones a nuevos objetos.  
Una variable no puede ser nombrada empezando con un número.  
No hay posibilidad de declarar booleanos, aunque se puede imprimir el valor de expresiones que los
devuelvan.  

# Datos primitivos soportados
Entero (Incluye - para negativos, no soporta + para positivos, soporta números con notación científica)  
Real (Incluye - para negativos, no soporta + para positivos, soporta números con notación científica)  
string (Símbolos entre "", las rachas y cadenas es la misma notación, No se puede declarar en sí como string, sino como racha o cadena)  
Char (Símbolos cualquieras, entre '')  
Booleano (Aunque no se pueden declarar explicitamente)  

# Estructuras de datos soportadas
Arreglos (Son sumamente flexibles, pueden tener cualquier tipo de dato dentro de ellos, incluyendo datos de tipos diferentes, las matrices en sí, se pueden definir como arreglos de arreglos)  

# Operación de asignación
**=** Asignación  

# Token Colon
**:** Asignación dentro de modelo

# Operaciones aritméticas
**+:** Suma  
**-:** Resta  
***:** Multiplicación  
**\*\*:** Potenciación  
**/:** División  
**div:** División entera  
**mod:** Operador módulo  

# Operaciones lógicas
**y:** "y" lógico  
**o:** "o" lógico  
**no:** negación lógica  

# operaciones relaciones
**==:** Operador igual que  
**<:** Operador menor que  
**>:** Operador mayor que  
**<=:** Operador menor igual que  
**>=:** Operador mayor igual que  
**!=:** Operador distinto que  

# Delimitadores
**()**  
**{}**  
**[]**  
**;**  
**,**  

# Llamado de atributos o métodos
**.**

# Comentarios
**##:** Comentario de línea.  
**# #:** Comentario de bloque.  

# Vocabulario

## Estructuras propias de los lenguajes imperativos
**mostrar:** Equivalente a print.  
**si:** Equivalente al if.  
**sino:** Equivalente al else.  
**caso:** Equivalente a switch. 
**mientras:** Equivalente a While.  
**repita:** Equivalente a Do.  
**para:** Equivalente a for.  
**hasta:** Indicador de cual será el final del ciclo "para".  
**funcion:** Forma de declarar una función.  
**retornar:** Valor de retorno de una función.  
**nada:** Palabra especial para poner en el retorno que hace que la función no retorne nada.  
**tipo:** Función que devuelve el tipo de dato de una variable.  
**tamaño:** Función que devuelve el tamaño de un arreglo.

## Declaraciones de datos (normales)
**entero:** Definición de una variable con un entero.  
**real:** Definición de una variable con un real.  
**arreglo:** Definición de una variable que será un arreglo.  
**cadena:** Definición de una cadena, la cual sería un string.  
**simbolo:** Definición de un char.  

## Declaraciones de datos (teoría de rachas)
**racha:** Definición de una racha, la cual sería un string.  
**alfabeto:** Definición de un alfabeto, el cual sería un arreglo.  
**alfa:** Definición del parámetro efecto del modelo (Es un real)  
**tau:** Definición del parámetro efectos de los tratamientos (Es un arreglo)  
**beta:** Definición del parámetro efectos de los bloques (Es un arreglo)  
**delta:** Definición de la matriz de los datos del modelo (Es un arreglo multidimensional)  
**modelo:** Definición de lo que sería un modelo.  

## Operaciones sobre los datos de la teoría de rachas
**add:** Para añadir un nuevo símbolo al alfabeto.  

## Estructuras de inicio y finalización del bloque principal
**inicio_main:** Inicio de la secuencia principal del programa.  
**fin_main:** Final de la secuencia principal del programa.  

## Funciones especiales
**ordenar_mayor:** Función especial que ordena un arreglo de mayor a menor.  
**ordenar_menor:** Función especial que ordena un arreglo de menor a mayor.  

## Funciones propias de la teoría de rachas
**multicotomizacion:** Función especial de multicotomizacion.  
**datos_modelo:** Función especial que da el total de datos del modelo de dos vías de clasificación.  
**datos_bloque:** Función especial que da el total de datos del bloque.  
**datos_tratamiento:** Función especial que da el total de datos del tratamiento.
**conjunto_datos:** Función especial que da el conjunto de todos los datos del modelo de dos vías de clasificación.  
**conjunto_datos_bloque:** Función especial que da el conjunto de todos los datos del bloque.  
**conjunto_datos_tratamiento:** Función especial que da el conjunto de todos los datos del tratamiento.  
**numero_rachas_hasta_dato:** Función especial que da el número de rachas que se forman hasta la ubicación del dato especificado.  
**rachas_celda:** Función especial que da el número de rachas en la celda especificada.  
**promedio_rachas_celda:** Función especial que da el promedio de rachas en la celda especificada.  
**rachas_bloque:** Función especial que da la suma del número de rachas en el bloque especificado.  
**rachas_tratamiento:** Función especial que da la suma del número de rachas en el tratamiento especificado.  
**promedio_rachas_bloque:** Función especial que da el promedio de rachas en el bloque especificado.  
**promedio_rachas_tratamiento:** Función especial que da el promedio de rachas en el tratamiento especificado.  
**rachas_modelo:** Función especial que da la suma del número de rachas de todo el modelo.  
**promedio_rachas_modelo:** Función especial que da el promedio del número de rachas de todo el modelo.  