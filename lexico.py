import ply.lex as lex

# Definir tokens
tokens = [
    'NUM_INT',
    'NUM_DEC',
    'ID',
    'TEXTO',
    'PALAVRAS_RESERVADAS',
    'COMENTARIOS',
    'OPERADORES',
    'SIMBOLO_ESPECIAL'
]

# Expressões regulares para tokens
# Expressão regular para reconhecer palavras reservadas

t_SIMBOLO_ESPECIAL = r'[(){}\[\];.]'
t_ID = r'[a-zA-Z][a-zA-Z0-9_]*'
t_OPERADORES = r'&&|\|\||<=|>=|==|!=|[+\-*/=%&|!><]'  # Modificação aqui para colocar operadores compostos primeiro

# Expressão regular para reconhecer palavras reservadas
def t_PALAVRAS_RESERVADAS(t):
    r'(int|else|if|scanf|while|float|double|boolean|char|void|println|main|return|for|struct|case|default|break|continue)\b'
    return t

# Expressão regular para reconhecer números decimais
def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Expressão regular para reconhecer números decimais
def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t




# Expressão regular para reconhecer constantes de texto
t_TEXTO = r'"([^"\\]*\\.)*[^"\\]*"'

# Corrija a expressão regular para COMENTARIOS em t_COMENTARIOS
def t_COMENTARIOS(t):
    r'\/\/[^\n]*'
    pass  # Ignorar comentários de linha


# Ignorar caracteres como espaços e pular linha
t_ignore = ' \n'

# Tratamento de erro no token
def t_error(t):
    print("Carácter não é válido: '%s'" % t.value[0])
    t.lexer.skip(1)

# Construir analisador léxico
lexer = lex.lex()

# Exemplo
data = 'int x = 3.4; if(d <= 2 && x >= 5 || y == 10 && z != 15)'
lexer.input(data)

# Obter os tokens reconhecidos
while True:
    token = lexer.token()
    if not token:
        break
    print(token)
