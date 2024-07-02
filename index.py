import ply.lex as lex
import ply.yacc as yacc
import re

# Identificadores y palabras reservadas
vocabulario = {
    'mostrar': 'MOSTRAR', 'si': 'SI', 'sino': 'SINO', 'caso': 'CASO',
    'mientras': 'MIENTRAS', 'repita': 'REPITA', 'para': 'PARA', 'hasta': 'HASTA',
    'funcion': 'FUNCION', 'retornar': 'RETORNAR', 'nada': 'NADA', 'tipo': 'TIPO',
    'tamaño': 'TAMANO', 'entero': 'ENTERO', 'real': 'REAL', 'arreglo': 'ARREGLO', 'simbolo': 'SIMBOLO',
    'cadena': 'CADENA', 'racha': 'RACHA', 'alfabeto': 'ALFABETO', 'alfa': 'ALFA', 'tau': 'TAU', 
    'beta': 'BETA', 'delta': 'DELTA', 'modelo': 'MODELO', 'add': 'ADD', 'inicio_main': 'INICIO', 
    'fin_main': 'FIN', 'ordenar_mayor': 'O_MAYOR', 'ordenar_menor': 'O_MENOR', 
    'multicotomizacion': 'MULTICOTOMIZACION', 'datos_modelo': 'DATOS_MODELO', 
    'datos_bloque': 'DATOS_BLOQUE', 'datos_tratamiento': 'DATOS_TRATAMIENTO', 
    'conjunto_datos': 'CONJUNTO_DATOS', 'conjunto_datos_bloque': 'CONJUNTO_DATOS_BLOQUE', 
    'conjunto_datos_tratamiento': 'CONJUNTO_DATOS_TRATAMIENTO', 
    'numero_rachas_hasta_dato': 'NUMERO_RACHAS_HASTA_DATO', 'rachas_celda': 'RACHAS_CELDA',
    'promedio_rachas_celda': 'PROMEDIO_RACHAS_CELDA', 'rachas_bloque': 'RACHAS_BLOQUE',
    'rachas_tratamiento': 'RACHAS_TRATAMIENTO', 'promedio_rachas_bloque': 'PROMEDIO_RACHAS_BLOQUE',
    'promedio_rachas_tratamiento': 'PROMEDIO_RACHAS_TRATAMIENTO', 'rachas_modelo': 'RACHAS_MODELO',
    'promedio_rachas_modelo': 'PROMEDIO_RACHAS_MODELO', 'y': 'Y', 'o': 'O', 'div': 'DIV',
    'mod': 'MOD', 'no': 'NO'
}

# Definición de tokens
tokens = (
    'ID', 'INTEGER', 'FLOAT', 'STRING', 'CHARACTER',
    'MAS', 'RESTA', 'MULTIPLICACION', 'POTENCIACION', 'DIVISION',
    'IGUAL_QUE', 'MENOR_QUE', 'MAYOR_QUE', 'MENOR_IGUAL_QUE', 
    'MAYOR_IGUAL_QUE', 'DISTINTO_QUE', 'ASSIGNMENT_OPERATOR', 
    'COLON', 'LLAVE_ABRIENDO', 'LLAVE_CERRANDO',
    'CORCHETE_ABRIENDO', 'CORCHETE_CERRANDO', 'COMA', 'PUNTO_COMA',
    'PAR_ABRIENDO', 'PAR_CERRANDO','CALL'
)+tuple(vocabulario.values())

# Tokens simples usando expresiones regulares
t_MAS = r'\+'
t_RESTA = r'-'
t_MULTIPLICACION = r'\*'
t_POTENCIACION = r'\*\*'
t_DIVISION = r'/'
t_MENOR_IGUAL_QUE = r'<='
t_MAYOR_IGUAL_QUE = r'>='
t_DISTINTO_QUE = r'!='
t_IGUAL_QUE = r'=='
t_MENOR_QUE = r'<'
t_MAYOR_QUE = r'>'
t_ASSIGNMENT_OPERATOR = r'='
t_COLON = r':'
t_LLAVE_ABRIENDO = r'\{'
t_LLAVE_CERRANDO = r'\}'
t_CORCHETE_ABRIENDO = r'\['
t_CORCHETE_CERRANDO = r'\]'
t_COMA = r','
t_PUNTO_COMA = r';'
t_PAR_ABRIENDO = r'\('
t_PAR_CERRANDO = r'\)'
t_CALL = r'\.'

# Identificadores y palabras clave
def t_ID(t):
    r'[a-zA-ZñÑ_][a-zA-Z0-9ñÑ_]*'
    t.type = vocabulario.get(t.value, 'ID')
    return t

# Real con notación científica
def t_FLOAT(t):
    r'-?\d+\.\d+([eE][-+]?\d+)?'
    t.value = float(t.value)  # Convertir a float para mantener la precisión
    return t

# Entero con notación científica
def t_INTEGER(t):
    r'-?\d+([eE][-+]?\d+)?'
    t.value = int(t.value) if 'e' not in t.value and 'E' not in t.value else float(t.value)
    return t

# Cadena
def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]
    return t

# Caracter
def t_CHARACTER(t):
    r"'[^']'"
    t.value = t.value[1]
    return t

# Comentarios
def t_COMMENT(t):
    r'\#\#.*'
    pass

def t_COMMENT_BLOCK(t):
    r'\#[\s\S]*?\#'
    pass

# Ignorar espacios y tabulaciones
t_ignore = ' \t\n'

# Manejo de errores
def t_error(t):
    print("Carácter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

###############################################################
###############################################################
###############################################################

# REGLAS DE PRODUCCIÓN

#Simbolo de entrada

# <start> ::= <zona_de_asignaciones> <zona_de_funciones> <zona_principal>
def p_start(p):
    '''
    start : zona_de_asignaciones zona_de_funciones zona_principal
    '''
    p[0] = (p[1], p[2], p[3])

# Definir una regla para manejar errores de sintaxis
def p_error(p):
    print("Error de sintaxis en '%s'" % p.value if p else "EOF")

# Vacío

def p_empty(p):
    'empty :'
    p[0] = None

# Operadores

#<operador_aritmetico> ::= POTENCIACION 
#                        | MAS 
#                        | RESTA 
#                        | MULTIPLICACION 
#                        | DIVISION
def p_operador_aritmetico(p):
    '''
    operador_aritmetico : POTENCIACION
                        | MAS
                        | RESTA
                        | MULTIPLICACION
                        | DIVISION
    '''
    p[0] = p[1]

#<operador_relacional> ::= MENOR_IGUAL_QUE 
#                        | MAYOR_IGUAL_QUE 
#                        | DISTINTO_QUE 
#                        | IGUAL_QUE 
#                        | MENOR_QUE
#                        | MAYOR_QUE
def p_operador_relacional(p):
    '''
    operador_relacional : MENOR_IGUAL_QUE
                        | MAYOR_IGUAL_QUE
                        | DISTINTO_QUE
                        | IGUAL_QUE
                        | MENOR_QUE
                        | MAYOR_QUE
    '''
    p[0] = p[1]

#<y_o> ::= Y 
#        | O 
def p_y_o(p):
    '''
    y_o : Y
        | O
    '''
    p[0] = p[1]

# Comodines compuestos

#<palabras_clave_modelo> ::= ALFA 
#                          | TAU 
#                          | BETA 
#                          | DELTA
def p_palabras_clave_modelo(p):
    '''
    palabras_clave_modelo : ALFA
                          | TAU
                          | BETA
                          | DELTA
    '''
    p[0] = p[1]

# Datos compuestos

#<numero> ::= INTEGER 
#           | FLOAT
def p_numero(p):
    '''
    numero : INTEGER
           | FLOAT
    '''
    p[0] = p[1]

# Factores compuestos

# <identificador_opcional_id_compuesto> ::= CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO <identificador_opcional_id_compuesto>
#                                         | empty
def p_identificador_opcional_id_compuesto(p):
    '''
    identificador_opcional_id_compuesto : CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO identificador_opcional_id_compuesto
                                        | empty
    '''
    if len(p) == 5:
        p[0] = (2,4)
    else:
        p[0] = None

# <id_compuesto> ::= ID <identificador_opcional_id_compuesto>
def p_id_compuesto(p):
    '''
    id_compuesto : ID identificador_opcional_id_compuesto
    '''
    p[0] = (p[1], p[2])

# <variables_del_modelo> ::= <id_compuesto> CALL <palabras_clave_modelo>
def p_variables_del_modelo(p):
    '''
    variables_del_modelo : id_compuesto CALL palabras_clave_modelo
    '''
    p[0] = (p[1], p[2], p[3])

# Datos especiales

#<funcion_del_lenguaje> ::= DIV 
#                         | MOD 
#                         | O_MAYOR 
#                         | O_MENOR 
#                         | TIPO 
#                         | TAMANO 
#                         | MULTICOTOMIZACION 
#                         | DATOS_MODELO 
#                         | DATOS_BLOQUE 
#                         | DATOS_TRATAMIENTO 
#                         | CONJUNTO_DATOS 
#                         | CONJUNTO_DATOS_BLOQUE 
#                         | CONJUNTO_DATOS_TRATAMIENTO 
#                         | NUMERO_RACHAS_HASTA_DATO 
#                         | RACHAS_CELDA 
#                         | PROMEDIO_RACHAS_CELDA 
#                         | RACHAS_BLOQUE 
#                         | RACHAS_TRATAMIENTO
#                         | PROMEDIO_RACHAS_BLOQUE 
#                         | PROMEDIO_RACHAS_TRATAMIENTO 
#                         | RACHAS_MODELO 
#                         | PROMEDIO_RACHAS_MODELO
def p_funcion_del_lenguaje(p):
    '''
    funcion_del_lenguaje : DIV
                         | MOD
                         | O_MAYOR
                         | O_MENOR
                         | TIPO
                         | TAMANO
                         | MULTICOTOMIZACION
                         | DATOS_MODELO
                         | DATOS_BLOQUE
                         | DATOS_TRATAMIENTO
                         | CONJUNTO_DATOS
                         | CONJUNTO_DATOS_BLOQUE
                         | CONJUNTO_DATOS_TRATAMIENTO
                         | NUMERO_RACHAS_HASTA_DATO
                         | RACHAS_CELDA
                         | PROMEDIO_RACHAS_CELDA
                         | RACHAS_BLOQUE
                         | RACHAS_TRATAMIENTO
                         | PROMEDIO_RACHAS_BLOQUE
                         | PROMEDIO_RACHAS_TRATAMIENTO
                         | RACHAS_MODELO
                         | PROMEDIO_RACHAS_MODELO
    '''
    p[0] = p[1]

# EXPRESIONES

# Comodines expresiones

# <call_alfa> ::= CALL ALFA 
#               | empty
def p_call_alfa(p):
    '''
    call_alfa : CALL ALFA
              | empty
    '''
    if len(p) == 3:  
        p[0] = (p[1], p[2])
    else: 
        p[0] = None 

#<menos_opcional> ::= RESTA <menos_opcional>
#                   | empty
def p_menos_opcional(p):
    '''
    menos_opcional : RESTA menos_opcional
                   | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

#<no_opcional> ::= NO <no_opcional>
#                | empty
def p_no_opcional(p):
    '''
    no_opcional : NO no_opcional
                | empty
    '''
    if len(p) == 3: 
        p[0] = (p[1], p[2])
    else:
        p[0] = None

# Expresiones en sí

# <parametro_dato> ::= <id_compuesto> <call_alfa> 
#                    | STRING 
#                    | CHARACTER 
#                    | <numero> 
#                    | <invocacion_de_funcion>
def p_parametro_dato(p):
    '''
    parametro_dato : id_compuesto call_alfa
                   | STRING
                   | CHARACTER
                   | numero
                   | invocacion_de_funcion
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

# <termino_aritmetico> ::= <parametro_dato> 
#                        | PAR_ABRIENDO <expresion_aritmetica> PAR_CERRANDO
def p_termino_aritmetico(p):
    '''
    termino_aritmetico : parametro_dato
                       | PAR_ABRIENDO expresion_aritmetica PAR_CERRANDO
    '''
    if len(p) == 2: 
        p[0] = p[1]
    else: 
        p[0] = p[2]

#<expresion_aritmetica_prima> ::= <operador_aritmetico> <menos_opcional> <termino_aritmetico> <expresion_aritmetica_prima>
#                               | empty
def p_expresion_aritmetica_prima(p):
    '''
    expresion_aritmetica_prima : operador_aritmetico menos_opcional termino_aritmetico expresion_aritmetica_prima
                               | empty
    '''
    if len(p) == 5:
        p[0] = (p[1], p[2], p[3], p[4])
    else:
        p[0] = None

#<expresion_aritmetica> ::= <menos_opcional> <termino_aritmetico> <expresion_aritmetica_prima>
def p_expresion_aritmetica(p):
    '''
    expresion_aritmetica : menos_opcional termino_aritmetico expresion_aritmetica_prima
    '''
    p[0] = (p[1], p[2], p[3])

# <expresion_relacional> ::= <expresion_aritmetica> <operador_relacional> <expresion_aritmetica>
def p_expresion_relacional(p):
    '''
    expresion_relacional : expresion_aritmetica operador_relacional expresion_aritmetica
    '''
    p[0] = (p[1], p[2], p[3])

#<expresion_relacional_compuesta_prima> ::= <expresion_relacional> 
#                                         | PAR_ABRIENDO <no_opcional> <expresion_relacional_compuesta> PAR_CERRANDO
def p_expresion_relacional_compuesta_prima(p):
    '''
    expresion_relacional_compuesta_prima : expresion_relacional 
                                         | PAR_ABRIENDO no_opcional expresion_relacional_compuesta PAR_CERRANDO
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[2], p[3])

#<expresion_relacional_compuesta> ::= <no_opcional> <expresion_relacional_compuesta_prima>
def p_expresion_relacional_compuesta(p):
    '''
    expresion_relacional_compuesta : no_opcional expresion_relacional_compuesta_prima
    '''
    p[0] = (p[1], p[2])

#<termino_logico> ::= <expresion_relacional_compuesta> 
#                   | PAR_ABRIENDO <expresion_logica> PAR_CERRANDO
def p_termino_logico(p):
    '''
    termino_logico : expresion_relacional_compuesta
                   | PAR_ABRIENDO expresion_logica PAR_CERRANDO
    '''
    if len(p) == 2: 
        p[0] = p[1]
    else:
        p[0] = p[2]

#<expresion_logica_prima> ::= <y_o> <no_opcional> <termino_logico> <expresion_logica_prima>
#                           | empty
def p_expresion_logica_prima(p):
    '''
    expresion_logica_prima : y_o no_opcional termino_logico expresion_logica_prima
                           | empty
    '''
    if len(p) == 5:
        p[0] = (p[1], p[2], p[3], p[4])
    else:
        p[0] = None

#<expresion_logica> ::= <no_opcional> <termino_logico> <expresion_logica_prima>    
def p_expresion_logica(p):
    '''
    expresion_logica : no_opcional termino_logico expresion_logica_prima    
    '''
    p[0] = (p[1], p[2], p[3])

#<conjunto_expresiones> ::= <expresion_logica> 
#                         | <expresion_aritmetica>
def p_conjunto_expresiones(p):
    '''
    conjunto_expresiones : expresion_logica
                         | expresion_aritmetica
    '''
    p[0] = p[1]

# DECLARACION DE VARIABLES

#<datos_comunes_y_rachas> ::= ENTERO 
#                           | REAL 
#                           | CADENA 
#                           | SIMBOLO 
#                           | RACHA
def p_datos_comunes_y_rachas(p):
    '''
    datos_comunes_y_rachas : ENTERO
                           | REAL
                           | CADENA
                           | SIMBOLO
                           | RACHA
    '''
    p[0] = p[1]

# Comodines de las variables

#<contenido_lista_explicita> ::= <expresion_aritmetica> 
#                              | <lista_explicita>
def p_contenido_lista_explicita(p):
    '''
    contenido_lista_explicita : expresion_aritmetica
                              | lista_explicita
    '''
    p[0] = p[1]

#<contenido_lista_explicita_plus> ::= <contenido_lista_explicita> COMA <contenido_lista_explicita_plus>
#                                   | empty
def p_contenido_lista_explicita_plus(p):
    '''
    contenido_lista_explicita_plus : contenido_lista_explicita COMA contenido_lista_explicita_plus
                                   | empty
    '''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = None

#<lista_explicita> ::= CORCHETE_ABRIENDO <contenido_lista_explicita_plus> <contenido_lista_explicita> CORCHETE_CERRANDO
def p_lista_explicita(p):
    '''
    lista_explicita : CORCHETE_ABRIENDO contenido_lista_explicita_plus contenido_lista_explicita CORCHETE_CERRANDO
    '''
    p[0] = (p[2], p[3])

#<interior_delta_plus> ::= CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON <lista_explicita> COMA <interior_delta_plus>
#                        | empty
def p_interior_delta_plus(p):
    '''
    interior_delta_plus : CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON lista_explicita COMA interior_delta_plus
                        | empty
    '''
    if len(p) == 11:
        p[0] = (p[2], p[5], p[8], p[10])
    else:
        p[0] = None

#<interior_delta> ::= <interior_delta_plus> CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON <lista_explicita>
def p_interior_delta(p):
    '''
    interior_delta : interior_delta_plus CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON lista_explicita
    '''
    p[0] = (p[1], p[3], p[6], p[9])

#<declaracion_datos_comunes_y_rachas> ::= <datos_comunes_y_rachas> ID ASSIGNMENT_OPERATOR <expresion_aritmetica> PUNTO_COMA
def p_declaracion_datos_comunes_y_rachas(p):
    '''
    declaracion_datos_comunes_y_rachas : datos_comunes_y_rachas ID ASSIGNMENT_OPERATOR expresion_aritmetica PUNTO_COMA
    '''
    p[0] = (p[1], p[2], p[3], p[4])

# Arreglos

#<arreglo_implicito> ::= ARREGLO ID CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO ASSIGNMENT_OPERATOR CORCHETE_ABRIENDO CORCHETE_CERRANDO PUNTO_COMA
def p_arreglo_implicito(p):
    '''
    arreglo_implicito : ARREGLO ID CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO ASSIGNMENT_OPERATOR CORCHETE_ABRIENDO CORCHETE_CERRANDO PUNTO_COMA
    '''
    p[0] = (p[1], p[2], p[4], p[6])

# <arreglo_explicito> ::= ARREGLO ID ASSIGNMENT_OPERATOR <lista_explicita> PUNTO_COMA
def p_arreglo_explicito(p):
    '''
    arreglo_explicito : ARREGLO ID ASSIGNMENT_OPERATOR lista_explicita PUNTO_COMA
    '''
    p[0] = (p[1], p[2], p[3], p[4])

# Teoría de rachas

#<definicion_explicita_del_alfabeto> ::= <expresion_aritmetica> COMA <definicion_explicita_del_alfabeto>
#                                      | empty
def p_definicion_explicita_del_alfabeto(p):
    '''
    definicion_explicita_del_alfabeto : expresion_aritmetica COMA definicion_explicita_del_alfabeto
                                      | empty
    '''
    if len(p) == 4: 
        p[0] = (p[1], p[3])
    else:
        p[0] = None

# <alfabeto_explicito> ::= ALFABETO ID LLAVE_ABRIENDO <definicion_explicita_del_alfabeto> <expresion_aritmetica> LLAVE_CERRANDO PUNTO_COMA
def p_alfabeto_explicito(p):
    '''
    alfabeto_explicito : ALFABETO ID LLAVE_ABRIENDO definicion_explicita_del_alfabeto expresion_aritmetica LLAVE_CERRANDO PUNTO_COMA
    '''
    p[0] = (p[1], p[2], p[4], p[5]) 

# <alfabeto_implicito> ::= ALFABETO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA
def p_alfabeto_implicito(p):
    '''
    alfabeto_implicito : ALFABETO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA
    '''
    p[0] = (p[1], p[2])

# <modelo_explicito> ::= MODELO ID LLAVE_ABRIENDO ALFA COLON <expresion_aritmetica> COMA BETA COLON <lista_explicita> COMA TAU COLON <lista_explicita> COMA DELTA CORCHETE_ABRIENDO <interior_delta> CORCHETE_CERRANDO LLAVE_CERRANDO PUNTO_COMA
def p_modelo_explicito(p):
    '''
    modelo_explicito : MODELO ID LLAVE_ABRIENDO ALFA COLON expresion_aritmetica COMA BETA COLON lista_explicita COMA TAU COLON lista_explicita COMA DELTA CORCHETE_ABRIENDO interior_delta CORCHETE_CERRANDO LLAVE_CERRANDO PUNTO_COMA                 
    '''
    p[0] = (p[1], p[2], p[4], p[6], p[8], p[10], p[12], p[14], p[16], p[18])

# <modelo_implicito> ::= MODELO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA
#                        | ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA
def p_modelo_implicito(p):
    '''
    modelo_implicito : MODELO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA
                     | ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA                       
    '''
    if len(p) == 30:
        p[0] = (p[1], p[2], p[6], p[7], p[8], p[10], p[13], p[14], p[15], p[17], p[20], p[21], p[22], p[24], p[27])
    else:
        p[0] = (p[1], p[2], p[3], p[5], p[7], p[8], p[10], p[12], p[13], p[15], p[18])

# SENTENCIAS

# Comodines para las sentencias

# <entero_o_vacío> ::= INTEGER 
#                    | empty
def p_entero_o_vacio(p):
    '''
    entero_o_vacio : INTEGER 
                   | empty
    '''
    p[0] = p[1] if p[1] != 'empty' else None 

#<lista_de_llamados> ::= CORCHETE_ABRIENDO <entero_o_vacio> CORCHETE_CERRANDO <lista_de_llamados>
#                      | empty
def p_lista_de_llamados(p):
    '''
    lista_de_llamados : CORCHETE_ABRIENDO entero_o_vacio CORCHETE_CERRANDO lista_de_llamados
                      | empty
    '''
    if len(p) == 5:
        p[0] = (p[2], p[4])
    else:
        p[0] = None

#<condicional_anidado_prima> ::= LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO
#                              | <condicional>
def p_condicional_anidado_prima(p):
    '''
    condicional_anidado_prima : LLAVE_ABRIENDO rutina LLAVE_CERRANDO
                               | condicional
    '''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]

#<condicional_anidado> ::= SINO <condicional_anidado_prima>
#                        | empty
def p_condicional_anidado(p):
    '''
    condicional_anidado : SINO condicional_anidado_prima
                        | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

#<interior_switch> ::= <id_compuesto> <operador_relacional> <expresion_aritmetica> COLON <rutina> PUNTO_COMA <interior_switch>
#                    | empty
def p_interior_switch(p):
    '''
    interior_switch : id_compuesto operador_relacional expresion_aritmetica COLON rutina PUNTO_COMA interior_switch
                    | empty
    '''
    if len(p) == 8:
        p[0] = (p[1], p[2], p[3], p[5], p[7])
    else:
        p[0] = None

# <variables_del_modelo_extendidas> ::= <variables_del_modelo> <lista_de_llamados>
def p_variables_del_modelo_extendidas(p):
    '''
    variables_del_modelo_extendidas : variables_del_modelo lista_de_llamados
    '''
    p[0] = (p[1], p[2])

#<valores_de_retorno> ::= <conjunto_expresiones> 
#                        | <lista_explicita>
#                        | <variables_del_modelo_extendidas> 
#                        | NADA
def p_valores_de_retorno(p):
    '''
    valores_de_retorno : conjunto_expresiones 
                       | lista_explicita 
                       | variables_del_modelo_extendidas 
                       | NADA
    '''
    p[0] = p[1]

#<posibles_variables_a_reasignar> ::= <id_compuesto> 
#                                   | <variables_del_modelo_extendidas>
def p_posibles_variables_a_reasignar(p):
    '''
    posibles_variables_a_reasignar : id_compuesto 
                                   | variables_del_modelo_extendidas
    '''
    p[0] = p[1]

#<posibles_asignaciones> ::= <expresion_aritmetica> 
#                          | <lista_explicita> 
#                          | <variables_del_modelo_extendidas>
def p_posibles_asignaciones(p):
    '''
    posibles_asignaciones : expresion_aritmetica 
                          | lista_explicita 
                          | variables_del_modelo_extendidas
    '''
    p[0] = p[1]

# Sentencias en sí

# <imprimir_en_consola> ::= MOSTRAR PAR_ABRIENDO <conjunto_expresiones> PAR_CERRANDO PUNTO_COMA
def p_imprimir_en_consola(p):
    '''
    imprimir_en_consola : MOSTRAR PAR_ABRIENDO conjunto_expresiones PAR_CERRANDO PUNTO_COMA
    '''
    p[0] = (p[1], p[3])

# <condicional> ::= SI PAR_ABRIENDO <expresion_logica> PAR_CERRANDO LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO <condicional_anidado>
def p_condicional(p):
    '''
    condicional : SI PAR_ABRIENDO expresion_logica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO condicional_anidado
    '''
    p[0] = (p[1], p[3], p[6], p[8])

# <switch> ::= CASO PAR_ABRIENDO <id_compuesto> PAR_CERRANDO LLAVE_ABRIENDO <interior_switch> LLAVE_CERRANDO
def p_switch(p):
    '''
    switch : CASO PAR_ABRIENDO id_compuesto PAR_CERRANDO LLAVE_ABRIENDO interior_switch LLAVE_CERRANDO
    '''
    p[0] = (p[1], p[3], p[6])

# <bucle_repita_mientras> ::= REPITA LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO MIENTRAS PAR_ABRIENDO <expresion_logica> PAR_CERRANDO
def p_bucle_repita_mientras(p):
    '''
    bucle_repita_mientras : REPITA LLAVE_ABRIENDO rutina LLAVE_CERRANDO MIENTRAS PAR_ABRIENDO expresion_logica PAR_CERRANDO
    '''
    p[0] = (p[1], p[3], p[5], p[7])

# <bucle_mientras> ::= mientras PAR_ABRIENDO <expresion_logica> PAR_CERRANDO LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO
def p_bucle_mientras(p):
    '''
    bucle_mientras : MIENTRAS PAR_ABRIENDO expresion_logica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO
    '''
    p[0] = (p[1], p[3], p[6])

# <bucle_para> ::= PARA PAR_ABRIENDO ENTERO ID ASSIGNMENT_OPERATOR <expresion_aritmetica> HASTA ID IGUAL_QUE <expresion_aritmetica> PAR_CERRANDO LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO
def p_bucle_para(p):
    '''
    bucle_para : PARA PAR_ABRIENDO ENTERO ID ASSIGNMENT_OPERATOR expresion_aritmetica HASTA ID IGUAL_QUE expresion_aritmetica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO
    '''
    p[0] = (p[1], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[13])

# <retornar_valor> ::= RETORNAR <valores_de_retorno>
def p_retornar_valor(p):
    '''
    retornar_valor : RETORNAR valores_de_retorno
    '''
    p[0] = (p[1], p[2])

#<declaracion> ::= <declaracion_datos_comunes_y_rachas> 
#                | <arreglo_implicito> 
#                | <arreglo_explicito>
#                | <alfabeto_explicito> 
#                | <alfabeto_implicito> 
#                | <modelo_explicito>
#                | <modelo_implicito>
def p_declaracion(p):
    '''
    declaracion : declaracion_datos_comunes_y_rachas 
                | arreglo_implicito 
                | arreglo_explicito
                | alfabeto_explicito 
                | alfabeto_implicito 
                | modelo_explicito
                | modelo_implicito
    '''
    p[0] = p[1]

# <asignacion> ::= <posibles_variables_a_reasignar> ASSIGNMENT_OPERATOR <posibles_asignaciones>
def p_asignacion(p):
    '''
    asignacion : posibles_variables_a_reasignar ASSIGNMENT_OPERATOR posibles_asignaciones
    '''
    p[0] = (p[1], p[2], p[3])

# <añadir_a_alfabeto> ::= <id_compuesto> CALL ADD PAR_ABRIENDO CHARACTER PAR_CERRANDO PUNTO_COMA
def p_anadir_a_alfabeto(p):
    '''
    anadir_a_alfabeto : id_compuesto CALL ADD PAR_ABRIENDO CHARACTER PAR_CERRANDO PUNTO_COMA
    '''
    p[0] = (p[1], p[2], p[3], p[5])

# <sentencias> ::= <imprimir_en_consola> 
#                | <condicional> 
#                | <switch> 
#                | <bucle_repita_mientras>
#                | <bucle_mientras> 
#                | <bucle_para> 
#                | <retornar_valor> 
#                | <declaracion> 
#                | <asignacion> 
#                | <anadir_a_alfabeto>
def p_sentencias(p):
    '''
    sentencias : imprimir_en_consola 
               | condicional 
               | switch 
               | bucle_repita_mientras
               | bucle_mientras 
               | bucle_para 
               | retornar_valor 
               | declaracion 
               | asignacion 
               | anadir_a_alfabeto
    '''
    p[0] = p[1]

# FUNCIONES

# Comodines para las funciones

#<expresiones_separadas_por_comas> ::= <expresion_aritmetica> COMA <expresiones_separadas_por_comas>
#                                    | empty
def p_expresiones_separadas_por_comas(p):
    '''
    expresiones_separadas_por_comas : expresion_aritmetica COMA expresiones_separadas_por_comas
                                        | empty
    '''
    if len(p) == 4:
        p[0] = (p[1], p[3])
    else:
        p[0] = None

#<declaracion_de_datos_separados_por_comas> ::= <datos_que_se_pueden_declarar> ID COMA <declaracion_de_datos_separados_por_comas>
#                                             | empty
def p_declaracion_de_datos_separados_por_comas(p):
    '''
    declaracion_de_datos_separados_por_comas : datos_que_se_pueden_declarar ID COMA declaracion_de_datos_separados_por_comas
                                             | empty
    '''
    if len(p) == 5:
        p[0] = (p[1], p[2], p[4])
    else:
        p[0] = None

#<mas_de_una_declaracion> ::= <declaracion_de_datos_separados_por_comas> <datos_que_se_pueden_declarar> ID 
#                           | empty
def p_mas_de_una_declaracion(p):
    '''
    mas_de_una_declaracion : declaracion_de_datos_separados_por_comas datos_que_se_pueden_declarar ID 
                           | empty
    '''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = None

# <parametro_funcion> ::= PAR_ABRIENDO <expresiones_separadas_por_comas> <expresion_aritmetica> PAR_CERRANDO
def p_parametro_funcion(p):
    '''
    parametro_funcion : PAR_ABRIENDO expresiones_separadas_por_comas expresion_aritmetica PAR_CERRANDO
    '''
    p[0] = (p[2], p[3])

#<funcion> ::= <funcion_del_lenguaje> 
#            | ID
def p_funcion(p):
    '''
    funcion : funcion_del_lenguaje 
            | ID
    '''
    p[0] = p[1]

#<datos_que_se_pueden_declarar> ::= <datos_comunes_y_rachas> 
#                                 | ARREGLO 
#                                 | ALFABETO 
#                                 | MODELO
def p_datos_que_se_pueden_declarar(p):
    '''
    datos_que_se_pueden_declarar : datos_comunes_y_rachas 
                                 | ARREGLO 
                                 | ALFABETO 
                                 | MODELO
    '''
    p[0] = p[1]

#<datos_que_se_pueden_declarar_y_nada> ::= <datos_que_se_pueden_declarar> 
#                                        | NADA
def p_datos_que_se_pueden_declarar_y_nada(p):
    '''
    datos_que_se_pueden_declarar_y_nada : datos_que_se_pueden_declarar 
                                        | NADA
    '''
    p[0] = p[1]

# Definir funciones

# <definicion_de_funcion> ::= FUNCION <datos_que_se_pueden_declarar_y_nada> ID PAR_ABRIENDO <mas_de_una_declaracion> PAR_CERRANDO LLAVE_ABRIENDO <rutina> LLAVE_CERRANDO
def p_definicion_de_funcion(p):
    '''
    definicion_de_funcion : FUNCION datos_que_se_pueden_declarar_y_nada ID PAR_ABRIENDO mas_de_una_declaracion PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO                     
    '''
    p[0] = (p[1], p[2], p[3], p[5], p[8])

# Invocación

# <invocacion_de_funcion> ::= <funcion> <parametro_funcion>
def p_invocacion_de_funcion(p):
    '''
    invocacion_de_funcion : funcion parametro_funcion
    '''
    p[0] = (p[1], p[2])

# RUTINAS

#<rutina> ::= <sentencias> <rutina>
#           | empty
def p_rutina(p):
    '''
    rutina : sentencias rutina 
           | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

# ESTRUCTURA DEL PROGRAMA

#<zona_de_asignaciones> ::= <declaracion> <zona_de_asignaciones>
#                         | empty
def p_zona_de_asignaciones(p):
    '''
    zona_de_asignaciones : declaracion zona_de_asignaciones
                         | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

#<zona_de_funciones> ::= <definicion_de_funcion> <zona_de_funciones>
#                      | empty
def p_zona_de_funciones(p):
    '''
    zona_de_funciones : definicion_de_funcion zona_de_funciones
                      | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

# <zona_principal> ::= INICIO <rutina> FIN
def p_zona_principal(p):
    '''
    zona_principal : INICIO rutina FIN
    '''
    p[0] = (p[1], p[2], p[3])

########################################################################################
########################################################################################
########################################################################################

# Construir el lexer
lexer = lex.lex()

# Construir el parser
parser = yacc.yacc()

resultado_gramatica = []

# Función de prueba
def prueba_sintactica(data):
    global resultado_gramatica
    resultado_gramatica.clear()

    for item in data.splitlines():
        if item:
            gram = parser.parse(item)
            if gram:
                resultado_gramatica.append(str(gram))
            else:
                print("Error de sintaxis en la línea: ", item)
        else:
            print("Data vacía")

    return resultado_gramatica

if __name__ == '__main__':
    while True:
        try:
            s = input('Ingrese dato >>> ')
        except EOFError:
            break
        if not s: continue

        try:
            gram = parser.parse(s)
            print("Resultado: ", gram)
            prueba_sintactica(s)
            print("Resultados de prueba sintáctica: ", resultado_gramatica)
        except Exception as e:
            print(f"Error: {e}")