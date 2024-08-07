grammar MiLenguaje;

start : zona_de_asignaciones zona_de_funciones? zona_principal;

zona_principal : INICIO rutina FIN;

zona_de_funciones : FUNCION datos_que_se_pueden_declarar_y_nada ID PAR_ABRIENDO (declaracion_de_datos_separados_por_comas datos_que_se_pueden_declarar ID)? PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO zona_de_funciones?;

zona_de_asignaciones : (declaracion zona_de_asignaciones)?;

rutina : (sentencias rutina)?;

datos_que_se_pueden_declarar_y_nada : datos_que_se_pueden_declarar | NADA;

datos_que_se_pueden_declarar : datos_comunes_y_rachas | ARREGLO | ALFABETO | MODELO;

declaracion_de_datos_separados_por_comas : (datos_que_se_pueden_declarar ID COMA declaracion_de_datos_separados_por_comas)?;

expresiones_separadas_por_comas : (expresion_aritmetica COMA expresiones_separadas_por_comas)?;

sentencias : imprimir_en_consola | condicional | switch | bucle_repita_mientras | bucle_mientras | bucle_para | retornar_valor | declaracion | asignacion | anadir_a_alfabeto;

anadir_a_alfabeto : id_compuesto CALL ADD PAR_ABRIENDO CHARACTER PAR_CERRANDO PUNTO_COMA;

asignacion : posibles_variables_a_reasignar ASSIGNMENT_OPERATOR posibles_asignaciones PUNTO_COMA;

declaracion : declaracion_datos_comunes_y_rachas | arreglo_implicito | arreglo_explicito | alfabeto_explicito | alfabeto_implicito | modelo_explicito | modelo_implicito;

retornar_valor : RETORNAR valores_de_retorno PUNTO_COMA;

bucle_para : PARA PAR_ABRIENDO ENTERO ID ASSIGNMENT_OPERATOR expresion_aritmetica HASTA ID IGUAL_QUE expresion_aritmetica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO;

bucle_mientras : MIENTRAS PAR_ABRIENDO expresion_logica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO;

bucle_repita_mientras : REPITA LLAVE_ABRIENDO rutina LLAVE_CERRANDO MIENTRAS PAR_ABRIENDO expresion_logica PAR_CERRANDO;

switch : CASO PAR_ABRIENDO id_compuesto PAR_CERRANDO LLAVE_ABRIENDO interior_switch LLAVE_CERRANDO;

condicional : SI PAR_ABRIENDO expresion_logica PAR_CERRANDO LLAVE_ABRIENDO rutina LLAVE_CERRANDO condicional_anidado;

imprimir_en_consola : MOSTRAR PAR_ABRIENDO conjunto_expresiones PAR_CERRANDO PUNTO_COMA;

posibles_asignaciones : expresion_aritmetica | lista_explicita | variables_del_modelo_extendidas;

posibles_variables_a_reasignar : id_compuesto | variables_del_modelo_extendidas;

valores_de_retorno : conjunto_expresiones | lista_explicita | variables_del_modelo_extendidas | NADA;

variables_del_modelo_extendidas : variables_del_modelo lista_de_llamados;

interior_switch : (id_compuesto operador_relacional expresion_aritmetica COLON rutina interior_switch)?;

condicional_anidado : (SINO condicional_anidado_prima)?;

condicional_anidado_prima : LLAVE_ABRIENDO rutina LLAVE_CERRANDO | condicional;

lista_de_llamados : (CORCHETE_ABRIENDO INTEGER? CORCHETE_CERRANDO lista_de_llamados)?;

modelo_implicito : MODELO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA ID CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA
                    | ID CALL BETA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL TAU CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CALL DELTA CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO PUNTO_COMA;

modelo_explicito : MODELO ID LLAVE_ABRIENDO ALFA COLON expresion_aritmetica COMA BETA COLON lista_explicita COMA TAU COLON lista_explicita COMA DELTA CORCHETE_ABRIENDO interior_delta CORCHETE_CERRANDO LLAVE_CERRANDO PUNTO_COMA;

alfabeto_implicito : ALFABETO ID LLAVE_ABRIENDO LLAVE_CERRANDO PUNTO_COMA;

alfabeto_explicito : ALFABETO ID LLAVE_ABRIENDO definicion_explicita_del_alfabeto expresion_aritmetica LLAVE_CERRANDO PUNTO_COMA;

definicion_explicita_del_alfabeto : (expresion_aritmetica COMA definicion_explicita_del_alfabeto)?;

arreglo_explicito : ARREGLO ID ASSIGNMENT_OPERATOR lista_explicita PUNTO_COMA;

arreglo_implicito : ARREGLO ID CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO ASSIGNMENT_OPERATOR CORCHETE_ABRIENDO CORCHETE_CERRANDO PUNTO_COMA;

declaracion_datos_comunes_y_rachas : datos_comunes_y_rachas ID ASSIGNMENT_OPERATOR expresion_aritmetica PUNTO_COMA;

interior_delta : interior_delta_plus CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON lista_explicita;

interior_delta_plus : (CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO CORCHETE_ABRIENDO INTEGER CORCHETE_CERRANDO COLON lista_explicita COMA interior_delta_plus)?;

lista_explicita : CORCHETE_ABRIENDO contenido_lista_explicita_plus contenido_lista_explicita CORCHETE_CERRANDO;

contenido_lista_explicita_plus : (contenido_lista_explicita COMA contenido_lista_explicita_plus)?;

contenido_lista_explicita : expresion_aritmetica | lista_explicita;

datos_comunes_y_rachas : ENTERO | REAL | CADENA | SIMBOLO | RACHA | ARREGLO | ALFABETO;

conjunto_expresiones : expresion_logica | expresion_aritmetica;

expresion_logica : no_opcional termino_logico expresion_logica_prima;

expresion_logica_prima : (y_o no_opcional termino_logico expresion_logica_prima)?;

termino_logico : expresion_relacional_compuesta | PAR_ABRIENDO expresion_logica PAR_CERRANDO;

expresion_relacional_compuesta : no_opcional expresion_relacional_compuesta_prima;

expresion_relacional_compuesta_prima : expresion_relacional | PAR_ABRIENDO no_opcional expresion_relacional_compuesta PAR_CERRANDO;

expresion_relacional : expresion_aritmetica operador_relacional expresion_aritmetica;

expresion_aritmetica : menos_opcional termino_aritmetico expresion_aritmetica_prima;

expresion_aritmetica_prima : (operador_aritmetico menos_opcional termino_aritmetico expresion_aritmetica_prima)?;

termino_aritmetico : parametro_dato | PAR_ABRIENDO expresion_aritmetica PAR_CERRANDO;

parametro_dato : id_compuesto (CALL ALFA)? | STRING | CHARACTER | INTEGER | FLOAT | funcion PAR_ABRIENDO expresiones_separadas_por_comas expresion_aritmetica PAR_CERRANDO;

no_opcional : (NO no_opcional)?;

menos_opcional : (RESTA menos_opcional)?;

funcion : DIV | MOD | O_MAYOR | O_MENOR | TIPO | TAMANO | MULTICOTOMIZACION | DATOS_MODELO | DATOS_BLOQUE | DATOS_TRATAMIENTO
        | CONJUNTO_DATOS | CONJUNTO_DATOS_BLOQUE | CONJUNTO_DATOS_TRATAMIENTO | NUMERO_RACHAS_HASTA_DATO | RACHAS_CELDA
        | PROMEDIO_RACHAS_CELDA | RACHAS_BLOQUE | RACHAS_TRATAMIENTO | PROMEDIO_RACHAS_BLOQUE | PROMEDIO_RACHAS_TRATAMIENTO
        | RACHAS_MODELO | PROMEDIO_RACHAS_MODELO | ID;

variables_del_modelo : id_compuesto CALL palabras_clave_modelo;

id_compuesto : ID identificador_opcional_id_compuesto;

identificador_opcional_id_compuesto : (CORCHETE_ABRIENDO integer_id CORCHETE_CERRANDO identificador_opcional_id_compuesto)?;

palabras_clave_modelo : ALFA | TAU | BETA | DELTA;

y_o : Y | O;

integer_id : INTEGER | ID;

operador_relacional : MENOR_IGUAL_QUE | MAYOR_IGUAL_QUE | DISTINTO_QUE | IGUAL_QUE | MENOR_QUE | MAYOR_QUE;

operador_aritmetico : POTENCIACION | MAS | RESTA | MULTIPLICACION | DIVISION;

MOSTRAR: 'mostrar';
SI: 'si';
SINO: 'sino';
CASO: 'caso';
MIENTRAS: 'mientras';
REPITA: 'repita';
PARA: 'para';
HASTA: 'hasta';
FUNCION: 'funcion';
RETORNAR: 'retornar';
NADA: 'nada';
TIPO: 'tipo';
TAMANO: 'tamano';
ENTERO: 'entero';
REAL: 'real';
ARREGLO: 'arreglo';
SIMBOLO: 'simbolo';
CADENA: 'cadena';
RACHA: 'racha';
ALFABETO: 'alfabeto';
ALFA: 'alfa';
TAU: 'tau';
BETA: 'beta';
DELTA: 'delta';
MODELO: 'modelo';
ADD: 'add';
INICIO: 'inicio_main';
FIN: 'fin_main';
O_MAYOR: 'ordenar_mayor';
O_MENOR: 'ordenar_menor';
MULTICOTOMIZACION: 'multicotomizacion';
DATOS_MODELO: 'datos_modelo';
DATOS_BLOQUE: 'datos_bloque';
DATOS_TRATAMIENTO: 'datos_tratamiento';
CONJUNTO_DATOS: 'conjunto_datos';
CONJUNTO_DATOS_BLOQUE: 'conjunto_datos_bloque';
CONJUNTO_DATOS_TRATAMIENTO: 'conjunto_datos_tratamiento';
NUMERO_RACHAS_HASTA_DATO: 'numero_rachas_hasta_dato';
RACHAS_CELDA: 'rachas_celda';
PROMEDIO_RACHAS_CELDA: 'promedio_rachas_celda';
RACHAS_BLOQUE: 'rachas_bloque';
RACHAS_TRATAMIENTO: 'rachas_tratamiento';
PROMEDIO_RACHAS_BLOQUE: 'promedio_rachas_bloque';
PROMEDIO_RACHAS_TRATAMIENTO: 'promedio_rachas_tratamiento';
RACHAS_MODELO: 'rachas_modelo';
PROMEDIO_RACHAS_MODELO: 'promedio_rachas_modelo';
Y: 'y';
O: 'o';
DIV: 'div';
MOD: 'mod';
NO: 'no';

INTEGER : '-'? [0-9]+ ([eE] [-+]? [0-9]+)? ;
FLOAT   : '-'? [0-9]+ '.' [0-9]+ ([eE] [-+]? [0-9]+)? ;
STRING      : '"' ~["]* '"' ;
CHARACTER : '\'' . '\'' ;
MAS         : '+' ;
RESTA       : '-' ;
MULTIPLICACION : '*' ;
POTENCIACION : '**' ;
DIVISION    : '/' ;
MENOR_IGUAL_QUE : '<=' ;
MAYOR_IGUAL_QUE : '>=' ;
DISTINTO_QUE : '!=' ;
IGUAL_QUE   : '==' ;
MENOR_QUE   : '<' ;
MAYOR_QUE   : '>' ;
ASSIGNMENT_OPERATOR : '=' ;
COLON       : ':' ;
LLAVE_ABRIENDO : '{' ;
LLAVE_CERRANDO : '}' ;
CORCHETE_ABRIENDO : '[' ;
CORCHETE_CERRANDO : ']' ;
COMA        : ',' ;
PUNTO_COMA  : ';' ;
PAR_ABRIENDO : '(' ;
PAR_CERRANDO : ')' ;
CALL        : '.' ;
ID : [a-zA-Z_][a-zA-Z0-9_]*;

COMMENT : '##' .*? '\n' -> skip ;
COMMENT_BLOCK : '#' .*? '#' -> skip ;
WS : [ \t\n\r]+ -> skip ;

// Manejo de errores léxicos
ANY_OTHER : . {
    System.err.println("Error léxico: carácter ilegal '" + getText() + "' en la línea " + getLine() + ", columna " + getCharPositionInLine());
} ;