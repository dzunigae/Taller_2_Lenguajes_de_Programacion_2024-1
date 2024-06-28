import ply.lex as lex
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
    'ID', 'KEYWORD', 'INTEGER', 'FLOAT', 'STRING', 'CHARACTER',
    'MAS', 'RESTA', 'MULTIPLICACION', 'POTENCIACION', 'DIVISION',
    'IGUAL_QUE', 'MENOR_QUE', 'MAYOR_QUE', 'MENOR_IGUAL_QUE', 
    'MAYOR_IGUAL_QUE', 'DISTINTO_QUE', 'ASSIGNMENT_OPERATOR', 
    'COLON', 'DELIMITER', 'LLAVE_ABRIENDO', 'LLAVE_CERRANDO',
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

# Asociatividad y precedencia de los operadores
precedence = (
    ('left', 'MAS', 'RESTA'),
    ('left', 'MULTIPLICACION', 'DIVISION'),
    ('right', 'POTENCIACION'),
)

# Reglas de producción

# Operadores

def p_operador_aritmetico(p):
    '''
    operador_aritmetico : POTENCIACION
                        | MAS
                        | RESTA
                        | MULTIPLICACION
                        | DIVISION
    '''
    p[0] = p[1]

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

def p_y_o(p):
    '''
    y_o : Y
        | O
    '''
    p[0] = p[1]

# Comodines compuestos

def p_palabras_clave_modelo(p):
    '''
    palabras_clave_modelo : ALFA
                          | TAU
                          | BETA
                          | DELTA
    '''
    p[0] = p[1]

# Datos compuestos

def p_numero(p):
    '''
    numero : INTEGER
           | FLOAT
    '''
    p[0] = p[1]

# Factores compuestos

def p_identificador_opcional_id_compuesto(p):
    '''
    identificador_opcional_id_compuesto : CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO
                                        | identificador_opcional_id_compuesto CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO
                                        | empty
    '''
    if len(p) == 4:
        p[0] = (p[2],)  # Retorna el INTEGER como una tupla
    elif len(p) == 5:
        p[0] = p[1] + (p[3],)  # Concatena el resultado de identificador_opcional_id_compuesto con el INTEGER
    else:
        p[0] = None  # Retorna None si es empty

# Construir el lexer
lexer = lex.lex()

# Ejemplo de uso del lexer
if __name__ == "__main__":
    data = '''
arreglo g = conjunto_datos_tratamiento(modelo_x,2);
    '''

    lexer.input(data)

    for tok in lexer:
        print(tok)