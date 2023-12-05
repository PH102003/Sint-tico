import ply.yacc as yacc
from lexico import tokens

precedencia = (
    ('left', 'OU'),
    ('left', 'E'),
    ('nonassoc', 'IGUAL', 'DIF', 'MENOR', 'MENORIG', 'MAIOR', 'MAIORIG'),
    ('left', 'MAIS', 'MENOS'),
    ('left', 'MULT', 'DIV', 'MOD'),
    ('right', 'MENOS_UNARIO', 'NAO'),
    ('left', 'INCREMENTO', 'DECREMENTO'),
)

t_TERM = r'termo'
t_LOGICO = r'logico'
t_RELACIONAL = r'relacional'
t_PARENTESE_ESQ = r'\('
t_PARENTESE_DIR = r'\)'
t_ARGUMENTOS = r'argumentos'
t_STRUCT = r'STRUCT'
t_CHAVE_ESQ = r'{'
t_CHAVE_DIR = r'}'
t_T = r't'
t_BLOCO = r'bloco'
# ignora espaços em branco e tabulações
t_ignore = ' \t'
# expressões regulares para tokens simples
t_MAIS = r'\+'
t_MENOS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_OU = r'\|\|'
t_E = r'&&'
t_NAO = r'!'
t_IGUALIGUAL = r'=='
t_IGUAL = r'='
t_DIF = r'!='
t_MENOR = r'<'
t_MENORIG = r'<='
t_MAIOR = r'>'
t_MAIORIG = r'>='
t_INCREMENTO = r'\+\+'
t_DECREMENTO = r'--'
t_PARENTESE_ESQ = r'\('
t_PARENTESE_DIR = r'\)'
t_CHAVE_ESQ = r'{'
t_CHAVE_DIR = r'}'
t_COLCHETE_ESQ = r'\['
t_COLCHETE_DIR = r'\]'
t_PONTOS = r':'
t_PONTO_VIRGULA = r';'
t_VIRGULA = r','

# Regras de token com expressões regulares e ações
def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_TEXTO(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1]
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

# Manipulador de erros
def t_error(t):
    print(f"Caracter inesperado: '{t.value[0]}' na linha {t.lineno}, coluna {t.lexpos + 1}")
    t.lexer.skip(1)

# Regras de produção
def p_programa(p):
    # as (''') são usadas para criar strings de várias linhas. São usadas em comentários longos, 
    # documentação de funções (docstrings) 
    # ou simplesmente para criar strings que se estendem por várias linhas de código.
    '''programa : lista_declaracoes'''
    p[0] = p[1]

# Corrija a definição de lista_declaracoes
def p_lista_declaracoes(p):
    '''
    lista_declaracoes : declaracao
                    | lista_declaracoes declaracao
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_declaracao(p):
    '''declaracao : declaracao_variavel
                  | declaracao_funcao
                  | declaracao_estrutura
                  | comentario'''
    p[0] = p[1]

def p_termo(p):
    '''termo : expressao'''
    p[0] = p[1]

def p_logico(p):
    '''logico : expressao'''
    p[0] = p[1]

def p_relacional(p):
    '''relacional : expressao'''
    p[0] = p[1]    

def p_expressao(p):
    '''
    expressao : termo
              | expressao OPERADORES termo
              | expressao logico termo
              | expressao relacional termo
              | SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL
              | ID SIMBOLO_ESPECIAL argumentos SIMBOLO_ESPECIAL
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4 and p[2] == '(':
        # Chamada de função
        p[0] = {'chamada_funcao': {'IDentificador': p[1], 'argumentos': p[3]}}
    else:
        # Operadores binários
        p[0] = {'expressao_binaria': {'operador': p[2], 'operando_esquerdo': p[1], 'operando_direito': p[3]}}

def p_declaracao_variavel(p):
    '''
    declaracao_variavel : PALAVRAS_RESERVADAS ID SIMBOLO_ESPECIAL
                       | PALAVRAS_RESERVADAS ID OPERADORES ID SIMBOLO_ESPECIAL
    '''
    if len(p) == 4:
        p[0] = {'tipo': p[1], 'IDentificador': p[2]}
    elif len(p) == 6:
        p[0] = {'tipo': p[1], 'IDentificador': p[2], 'expressao': p[4]}

def p_declaracao_estrutura(p):
    '''
    declaracao_estrutura : STRUCT ID SIMBOLO_ESPECIAL declaracao_variavel_lista SIMBOLO_ESPECIAL SIMBOLO_ESPECIAL
    '''
    p[0] = {'struct': {'IDentificador': p[2], 'declaracoes_variaveis': p[4]}}

def p_declaracao_variavel_lista(p):
    '''
    declaracao_variavel_lista : declaracao_variavel
                             | declaracao_variavel_lista declaracao_variavel
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_declaracao_funcao(p):
    '''
    declaracao_funcao : PALAVRA_RESERVADA ID SIMBOLO_ESPECIAL parametros SIMBOLO_ESPECIAL bloco
    '''
    p[0] = {'tipo': p[1], 'IDentificador': p[2], 'parametros': p[4], 'bloco': p[6]}

def p_parametros(prod):
    '''
    parametros : parametro
               | parametros SIMBOLO_ESPECIAL parametro
    '''
    if len(prod) == 2:
        prod[0] = [prod[1]]
    elif len(prod) == 4:
        prod[0] = prod[1] + [prod[3]]
        

def p_parametro(p):
    '''
    parametro : PALAVRA_RESERVADA ID
              | PALAVRA_RESERVADA ID SIMBOLO_ESPECIAL SIMBOLO_ESPECIAL
    '''
    if len(p) == 3:
        p[0] = {'tipo': p[1], 'IDentificador': p[2]}
    elif len(p) == 5:
        p[0] = {'tipo': p[1], 'IDentificador': p[2], 'array': True}
    elif len(p) == 4:
        p[0] = {'tipo': p[1], 'IDentificador': p[3], 'variadico': True}
 #variadico é a capacIDade de uma função ou método aceitar um número variável de argumentos

def p_comentario(prod):
    '''
    comentario : COMENTARIO_LINHA
               | COMENTARIO_BLOCO
    '''

def p_comentario_linha(p):
    'COMENTARIO_LINHA : TEXTO'
    p[0] = {'comentario_linha': p[2]}

def p_comentario_bloco(p):
    'COMENTARIO_BLOCO : TEXTO '
    p[0] = {'comentario_bloco': p[2]}

def p_estrutura_controle(prod):
    '''
    estrutura_controle : estrutura_controle_if
                      | estrutura_controle_while
                      | estrutura_controle_for
                      | estrutura_controle_switch
                      | estrutura_controle_break
                      | estrutura_controle_continue
                      | estrutura_controle_return
    '''
    prod[0] = prod[1]

def p_estrutura_controle_if(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL bloco
                      | PALAVRA_RESERVADA SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL bloco PALAVRA_RESERVADA bloco
    '''
    if len(p) == 6:
        p[0] = {'if': {'expressao': p[3], 'bloco_true': p[5]}}
    elif len(p) == 8:
        p[0] = {'if_else': {'expressao': p[3], 'bloco_true': p[5], 'bloco_false': p[7]}}

def p_estrutura_controle_while(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL bloco
    '''
    p[0] = {'while': {'expressao': p[3], 'bloco': p[5]}}

def p_estrutura_controle_for(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL bloco
    '''
    p[0] = {'for': {'expressao_inicial': p[3], 'expressao_condicional': p[5], 'expressao_incremento': p[7], 'bloco': p[9]}}

def p_estrutura_controle_switch(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL case_lista
    '''
    p[0] = {'switch': {'expressao': p[3], 'case_lista': p[5]}}

def p_case_lista(p):
    '''
    case_lista : case_decl
               | case_lista case_decl
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 3:
        p[0] = p[1] + [p[2]]

def p_case_decl(p):
    '''
    case_decl : PALAVRA_RESERVADA ID DOIS_PONTOS bloco
              | PALAVRA_RESERVADA DOIS_PONTOS bloco
    '''
    if p[1] == 'case':
        p[0] = {'case': {'expressao': p[2], 'bloco': p[4]}}
    elif p[1] == 'default':
        p[0] = {'default': {'bloco': p[3]}}

def p_estrutura_controle_break(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL
    '''
    p[0] = {'break': True}

def p_estrutura_controle_continue(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA SIMBOLO_ESPECIAL
    '''
    p[0] = {'continue': True}

def p_estrutura_controle_return(p):
    '''
    estrutura_controle : PALAVRA_RESERVADA ID SIMBOLO_ESPECIAL
                      | PALAVRA_RESERVADA SIMBOLO_ESPECIAL
    '''
    if len(p) == 3:
        p[0] = {'return': {'expressao': p[2]}}
    elif len(p) == 2:
        p[0] = {'return': {'expressao': None}}
    
def p_primaria(p):
    '''
    primaria : ID
             | NUM_INT
             | NUM_DEC
             | TEXTO
             | SIMBOLO_ESPECIAL ID SIMBOLO_ESPECIAL
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = p[2]

def p_expressao_unaria(p):
    '''
    expressao_unaria : expressao_postfix
                    | MENOS_UNARIO expressao_unaria
                    | OPERADORES expressao_unaria
                    | OPERADORES expressao_unaria
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = (p[1], p[2])

def p_expressao_aritmetica(p):
    '''
    expressao_aritmetica : expressao_multiplicativa
                        | expressao_aritmetica OPERADORES expressao_multiplicativa
                        | expressao_aritmetica OPERADORES expressao_multiplicativa
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])

def p_expressao_multiplicativa(p):
    '''
    expressao_multiplicativa : expressao_unaria
                           | expressao_multiplicativa OPERADORES expressao_unaria
                           | expressao_multiplicativa OPERADORES expressao_unaria
                           | expressao_multiplicativa OPERADORES expressao_unaria
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])

def p_expressao_logica(p):
    '''
    expressao_logica : expressao_relacional
                    | OPERADORES expressao_logica
                    | expressao_logica OPERADORES expressao_logica
                    | expressao_logica OPERADORES expressao_logica
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = (p[1], p[2])
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])

def p_expressao_relacional(p):
    '''
    expressao_relacional : expressao_aritmetica
                        | expressao_relacional OPERADORES expressao_relacional
                        | expressao_relacional OPERADORES expressao_relacional
                        | expressao_relacional OPERADORES expressao_relacional
                        | expressao_relacional OPERADORES expressao_relacional
                        | expressao_relacional OPERADORES expressao_relacional
                        | expressao_relacional OPERADORES expressao_relacional
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])





# Tratamento de erros de sintaxe
def p_error(p):
    if p:
        print(f"Erro de sintaxe na entrada: '{p.value}' na linha {p.lineno}, coluna {p.lexpos + 1}")
    else:
        print("Erro de sintaxe")

# Construção do analisador sintático
yacc.yacc()

# Exemplo para uso
dados = """
int x;
float y = 3.14;
struct Ponto {
    float z;
    if(y < 3){
    return 0;
    }
};
// Comentário de exemplo
"""
resultado = yacc.parse(dados)
print(resultado)
