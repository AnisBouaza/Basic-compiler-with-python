import ply
import ply.lex as lex
import ply.yacc as yacc
import sys
import re

succes = True

tokens = [
    "OPAR",
    "CPAR",
    "OCURL",
    "CCURL",
    "OBRAC",
    "CBRAC",
    "COMMA",
    "DPOINTS",

    "EQUAL",

    "INC",
    "DEC",

    "SUM",
    "SUB",
    "MUL",
    "DIV",
    "MOD",

    "GRTR",
    "LSSR",
    "GEQUAL",
    "LEQUAL",
    "EEQUAL",
    "NEQUAL",

    "numeric_value",
    "integer_value",
    "character_value",

    "IDF",
    "NEWLINE",
    "IF",
    "ELSE",
    "FOR",
    "IN",
    "OR",
    "AND",
    "WHILE",
    "NUMERIC",
    "INTEGER",
    "CHARACTER",
    "LOGICAL",
    "TRUE",
    "FALSE",
    "comment"
]

reserved = {
    "IF": "IF",
    "ELSE": "ELSE",
    "FOR": "FOR",
    "IN": "IN",
    "OR": "OR",
    "AND": "AND",
    "WHILE": "WHILE",
    "NUMERIC": "NUMERIC",
    "INTEGER": "INTEGER",
    "CHARACTER": "CHARACTER",
    "LOGICAL": "LOGICAL",
    "TRUE": "TRUE",
    "FALSE": "FALSE"

}


t_ignore  = " \t\r"

t_OPAR = r"\("
t_CPAR = r"\)"
t_OCURL = r"\{"
t_CCURL = r"\}"
t_OBRAC = r"\["
t_CBRAC = r"\]"
t_COMMA=r"\,"
t_DPOINTS=r"\:"
t_SUM = r"\+"
t_SUB = r"\-"
t_MUL = r"\*"
t_DIV = r"\/"
t_MOD = r"\%"
t_EQUAL = r"\="
t_LSSR = r"\<"
t_GRTR = r"\>"

def t_comment(t):
    r"\#.*"
    return t

def t_TRUE(t):
    r"TRUE"
    t.value = "TRUE"
    return t

def t_FALSE(t):
    r"FALSE"
    t.value = "FALSE"
    return t

def t_IDF(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    t.type = reserved.get(t.value, "IDF")
    if t.type == "IDF":
        if t.value[0].islower() or len(t.value) > 10:
            print("Invalid identifier line:{} col:{}".format(t.lineno, t.lexpos))
            t.lexer.skip(1)
            return
    return t

def t_INC(t):
    r"\+="
    t.value = "+="
    return t

def t_DEC(t):
    r"-="
    t.value = "-="
    return t

def t_NEQUAL(t):
    r"!="
    t.value = "!="
    return t

def t_EEQUAL(t):
    r"=="
    t.value = "=="
    return t

def t_GEQUAL(t):
    r">="
    t.value = ">="
    return t

def t_LEQUAL(t):
    r"<="
    t.value = "<="
    return t

def t_character_value(t):
    r"\'.\'"
    t.value = t.value[1]
    return t

def t_numeric_value(t):
    r"(\d+\.\d+|\(\-\d+\.\d+\))"
    t.value = t.value.replace("(", "")
    t.value = t.value.replace(")", "")
    t.value = float(t.value)
    return t

def t_integer_value(t):
    r"(\d+|\(\-\d+\))"
    t.value = t.value.replace("(", "")
    t.value = t.value.replace(")", "")
    t.value = int(t.value)
    if t.value < -32768 or t.value > 32767:
       print("Integer out of range line:{} col:{}".format(t.lineno, t.lexpos))
       t.lexer.skip(1)
       return
    return t

def t_NEWLINE(t):
    r"\n+"
    t.lexer.lineno += len(t.value)
    t.value = ""
    return t

def t_error(t):
    print("Erreur lexicale line:{} col:{}".format(t.lineno, t.lexpos))
    t.lexer.skip(1)

source_text = None

if len(sys.argv) > 1:
    try:
        source_file = open(sys.argv[1], "r")
        source_text = source_file.read()
    except FileNotFoundError as e:
        print("Invalid source file")
        exit(0)

lexer = lex.lex()
lexer.input(source_text)

quads = []
numQuad = 0
pileAdresses = []
cpt = 0

precedence = (
    ('left','OR'),
     ('left','AND'),
     ('left', 'SUM', 'SUB'),
     ('left', 'MUL', 'DIV','MOD')
 )

class Symb(object):
    def __init__(self, name, type=None,category=None,size=1,initialized=False):
        self.name = name
        self.type = type
        self.category = category
        self.size=size
        self.initialized = initialized

class TableSymb(object):
    def __init__(self):
        self._symbols = {}

    def insert(self, symbol):
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        symbol = self._symbols.get(name)
        return symbol

table_symbols = TableSymb()

def p_starter(p):
    '''
    STARTER : INSTRUCTION_BLOCK
            | NEWLINE INSTRUCTION_BLOCK
    '''

def p_bloc_inst(p):
    '''
    INSTRUCTION_BLOCK : INSTRUCTION_BLOCK INST NEWLINE
              | INST NEWLINE
              | IF_CLAUSE
              | WHILE_INSTRUCTION
              | FOR_INSTRUCTION
              | INSTRUCTION_BLOCK IF_CLAUSE
              | INSTRUCTION_BLOCK WHILE_INSTRUCTION
              | INSTRUCTION_BLOCK FOR_INSTRUCTION
    '''

def p_inst(p):
    '''
    INST : DECLARATION
         | AFFECT
         | AFFECT_COND
         | INCR
         | DECR
         | TAB_AFFECT
         | comment
    '''
def p_incr(p):
    '''
    INCR : IDF INC integer_value
    '''
    global succes
    if(table_symbols.lookup(p[1])):
        if(table_symbols.lookup(p[1]).type == "Character"):
            print("Syntax Error : Incompatible types. Line "+str(p.lineno(1))+" column "+str(find_column(p.lexpos(1)))+"\n")
            succes = False
        else:
            quads.append(["+",p[1],p[3],p[1]])
    else:
        succes = False
        print("Syntax Error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))

def p_decr(p):
    '''
    DECR : IDF DEC integer_value
    '''
    global succes
    if(table_symbols.lookup(p[1])):
        if(table_symbols.lookup(p[1]).type == "Character"):
            succes = False
            print("Syntax Error : Incompatible types. Line "+str(p.lineno(1))+" column "+str(find_column(p.lexpos(1)))+"\n")
        else :
            quads.append(["-",p[1],p[3],p[1]])
    else:
        succes = False
        print("Syntax Error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))

def p_affect(p):
    '''
    AFFECT : IDF EQUAL EXPR
    '''
    global numQuad
    quads.append([p[2],p[3],None,p[1]])
    p[0] = p[1]
    if(not table_symbols.lookup(p[1])):
        symbol = Symb(p[1],"Dynamic","Variable")
        table_symbols.insert(symbol)
        table_symbols.lookup(p[1]).initialized = True
    else:
        table_symbols.lookup(p[1]).initialized = True

def p_affect_char(p):
    '''
    AFFECT : IDF EQUAL character_value
    '''
    global numQuad
    quads.append([p[2],p[3],None,p[1]])
    p[0] = p[1]
    if(not table_symbols.lookup(p[1])):
        symbol = Symb(p[1],"Character","Variable")
        table_symbols.insert(symbol)
        table_symbols.lookup(p[1]).initialized = True
    else:
        table_symbols.lookup(p[1]).initialized = True

def p_declaration(p):
    '''
    DECLARATION : TYPE AFFECT
                | DECLARATION_MULTIPLE
    '''
    if(len(p)>2):
        if(not table_symbols.lookup(p[2]) or table_symbols.lookup(p[2]).type == "Dynamic" ):
            symbol = Symb(p[2],p[1],"Variable")
            symbol.initialized = True
            table_symbols.insert(symbol)

def p_declaration_multiple(p):
    '''
    DECLARATION_MULTIPLE : TYPE IDF
    '''
    p[0] = p[1]
    if(not table_symbols.lookup(p[2]) or table_symbols.lookup(p[2]).type == "Dynamic"):
        symbol = Symb(p[2],p[0],"Variable")
        table_symbols.insert(symbol)

def p_declaration_multiple_multiple(p):
    '''
    DECLARATION_MULTIPLE : DECLARATION_MULTIPLE COMMA IDF
    '''
    p[0] = p[1]
    if(not table_symbols.lookup(p[3]) or not table_symbols.lookup(p[3]).type == "Dynamic"):
        symbol = Symb(p[3],p[0],"Variable")
        table_symbols.insert(symbol)

def p_declaration_tab(p):
    '''
    DECLARATION : ARRAY_DECLARATION
    '''
    p[0] = p[1]

def p_tab_declaration(p):
    '''
    ARRAY_DECLARATION : TYPE IDF OBRAC integer_value CBRAC
    '''
    p[0] = p[2]
    if(not table_symbols.lookup(p[2])):
        symbol = Symb(p[2],p[1],"Variable",p[4])
        table_symbols.insert(symbol)

def p_type(p):
    '''
    TYPE : INTEGER
         | NUMERIC
         | CHARACTER
         | LOGICAL
    '''
    p[0] = p[1]

def p_EXPR(p):
    '''
    EXPR : ARITHM_EXPRESSION
         | COMPARISON_EXPRESSION
         | LOGICAL_EXPRESSION
    '''
    p[0] = p[1]

def p_ARITHM_EXPRESSION(p):
    '''
    ARITHM_EXPRESSION : ARITHM_EXPRESSION SUM ARITHM_EXPRESSION
                      | ARITHM_EXPRESSION SUB ARITHM_EXPRESSION
                      | ARITHM_EXPRESSION MUL ARITHM_EXPRESSION
                      | ARITHM_EXPRESSION DIV ARITHM_EXPRESSION
                      | ARITHM_EXPRESSION MOD ARITHM_EXPRESSION
                      | VALUE
    '''
    global cpt
    global numQuad
    if(len(p)>2):
        cpt+=1
        p[0] = "tmp_" + str(cpt)
        quads.append([p[2],p[1],p[3],p[0]])
    else:
        p[0] = p[1]


def p_affect_tab(p):
    '''
    TAB_AFFECT : TABCASE EQUAL EXPR
    '''
    p[0] = p[1]
    quads.append([p[2],p[3],None,p[1]])

def p_TABCASE(p):
    '''
    TABCASE : IDF OBRAC ARITHM_EXPRESSION CBRAC
    '''
    p[0] = p[1] + "[" + str(p[3]) + "]"

def p_ARITHM_EXPRESSION_IDF(p):
    '''
    ARITHM_EXPRESSION : IDF
    '''
    global succes
    if(table_symbols.lookup(p[1])):
        if(table_symbols.lookup(p[1]).initialized):
            p[0]=p[1]
        else :
            succes = False
            print("Syntax error : Variable " + str(p[1]) +" hasn't been assigned at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))
    else:
        succes = False
        print("Syntax error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))

def p_ARITHM_EXPRESSION_PAR(p):
    '''
    ARITHM_EXPRESSION : OPAR ARITHM_EXPRESSION CPAR
    '''
    p[0] = p[2]

def p_COMPARISON_EXPRESSION(p):
    '''
    COMPARISON_EXPRESSION : ARITHM_EXPRESSION COMPARISON_OPERATOR ARITHM_EXPRESSION
    '''
    global numQuad
    global cpt
    cpt += 1
    p[0] = "tmp_" + str(cpt)
    quads.append([p[2],p[1],p[3],p[0]])
def p_COMPARISON_EXPRESSION_par(p):
    '''
    COMPARISON_EXPRESSION : OPAR COMPARISON_EXPRESSION CPAR
    '''
    p[0] = p[2]

def p_LOGICAL_EXPRESSION(p):
    '''
    LOGICAL_EXPRESSION : EXPR AND EXPR
    | EXPR OR EXPR
    '''
    global cpt
    if(len(p)>2):
        cpt+=1
        p[0] = "tmp_" + str(cpt)
        quads.append([p[2],p[1],p[3],p[0]])
    else:
        p[0]=p[1]

def p_LOGICAL_EXPRESSION2(p):
    '''
    LOGICAL_EXPRESSION : LOGICAL_VALUE
    '''
    p[0] = p[1]
def p_LOGICAL_EXPRESSION_par(p):
    '''
    LOGICAL_EXPRESSION : OPAR LOGICAL_EXPRESSION CPAR
    '''
    p[0] = p[2]

def p_COMPARISON_OPERATOR(p):
    '''
    COMPARISON_OPERATOR : GRTR
                  | LSSR
                  | GEQUAL
                  | LEQUAL
                  | EEQUAL
                  | NEQUAL
    '''
    p[0] = p[1]
def p_logical_value(p):
    '''
    LOGICAL_VALUE : TRUE
                  | FALSE
    '''
    p[0] = p[1]

def p_value_integer(p):
    '''
    VALUE : integer_value
    '''
    p[0] = p[1]


def p_value_float(p):
    '''
    VALUE : numeric_value
    '''
    p[0] = p[1]

def p_AFFECT_COND(p):
    '''
    AFFECT_COND : AFF_COND_EXP EXPR CPAR
    '''
    quads.append(['=', p[2], None, p[1]])
    adresseBR = pileAdresses.pop()
    quads[adresseBR[0]][1] = len(quads)
def p_AFF_COND_EXP(p):
    '''
    AFF_COND_EXP : AFF_COND_DEB EXPR COMMA
    '''
    adresseBZ = pileAdresses.pop()
    quads.append(['=', p[2], None, p[1]])
    pileAdresses.append([len(quads), 'AFFCONDbr'])
    quads.append(['BR', None, None, None])
    quads[adresseBZ[0]][1] = len(quads)

    p[0] = p[1]

def p_AFF_COND_DEB(p):
    '''
    AFF_COND_DEB : IDF EQUAL OPAR EXPR COMMA
    '''
    pileAdresses.append([len(quads), 'AFFCONDtest'])
    quads.append(['BZ', None, p[4], None])
    p[0] = p[1]


def p_IF_CLAUSE(p):
    '''
    IF_CLAUSE : IF_INSTRUCTION NEWLINE
              | IF_ELSE_BLOC INSTRUCTION_BLOCK CCURL NEWLINE

    '''
    while(True):
        adresse = pileAdresses.pop()
        quads[adresse[0]][1] = len(quads)
        if adresse[1] is not None:
            break
def p_IF_INSTRUCTION(p):
    '''
    IF_INSTRUCTION : IF_CONDITION NEWLINE OCURL NEWLINE INSTRUCTION_BLOCK CCURL
            | ELSE_IF_CONDITION OCURL NEWLINE INSTRUCTION_BLOCK CCURL
    '''

def p_IF_CONDITION(p):
    '''
    IF_CONDITION : IF OPAR EXPR CPAR

    '''
    pileAdresses.append([len(quads), "startIF"])
    quads.append(['BZ', None,  p[3], None])

def p_IF_ELSE_BLOCK(p):
    '''
    IF_ELSE_BLOC : IF_INSTRUCTION NEWLINE ELSE NEWLINE OCURL NEWLINE
    '''
    adresse = pileAdresses.pop()
    if adresse[1] == "startIF":
        hint = "firstElse"
    else: hint = None

    pileAdresses.append([len(quads), hint])
    quads.append(['BR', None, None, None])
    quads[adresse[0]][1] = len(quads)

def p_ELSE_IF(p):
    '''
    ELSE_IF : IF_INSTRUCTION NEWLINE ELSE IF
    '''
    adresse = pileAdresses.pop()

    if adresse[1] == "startIF":
        hint = "firstElse"
    else: hint = None

    pileAdresses.append([len(quads), hint])
    quads.append(['BR', None, None, None])
    quads[adresse[0]][1] = len(quads)

def p_ELSE_IF_CONDITION(p):
    '''
    ELSE_IF_CONDITION : ELSE_IF OPAR EXPR CPAR NEWLINE
    '''
    pileAdresses.append([len(quads), None])
    quads.append(['BZ', None,  p[3], None])

def p_WHILE_INSTRUCTION(p):
    '''
    WHILE_INSTRUCTION : WHILE_COND OCURL NEWLINE INSTRUCTION_BLOCK CCURL NEWLINE
    '''
    adresseTest = pileAdresses.pop()
    adresseDebutCond = pileAdresses.pop()
    quads.append(['BR', adresseDebutCond[0],  None, None])
    quads[adresseTest[0]][1] = len(quads)
def p_while_cond(p):
    '''
    WHILE_COND : WHILE_START OPAR EXPR CPAR NEWLINE
    '''
    pileAdresses.append([len(quads), "WHILEcond"])
    quads.append(['BZ', None,  p[3], None])

def p_while_start(p):
    '''
    WHILE_START : WHILE
    '''
    pileAdresses.append([len(quads), "WHILEstart"])

def p_for_block(p):
    '''
     FOR_INSTRUCTION : FOR_CONDITION OCURL NEWLINE INSTRUCTION_BLOCK CCURL NEWLINE
    '''
    adresseBGE = pileAdresses.pop()
    adresseTest = pileAdresses.pop()
    quads.append(['+', p[1], 1, p[1]])
    quads.append(['BR', adresseTest[0], None, None])
    quads[adresseBGE[0]][1] = len(quads)
def p_for_condition(p):
    '''
    FOR_CONDITION : FOR_START for_integer CPAR NEWLINE
    '''
    pileAdresses.append([len(quads), "FORbge"])
    quads.append(['BGE', None, p[1], p[2]])
    p[0] = p[1]

def p_for_start(p):
    '''
    FOR_START : FOR OPAR IDF IN for_integer DPOINTS
    '''
    quads.append(['=', p[5], None, p[3]])
    pileAdresses.append([len(quads), "FORtest"])
    p[0] = p[3]

def p_for_integer(p):
    '''
    for_integer : IDF
    | integer_value
    '''
    p[0] = p[1]

def p_error(p):
    global succes
    succes = False
    if p:
        print("Syntax error at token", p.type)
        parser.errok()
    else:
        print("Syntax error at EOF")

def find_column(lexpos):
    global source_text
    line_start = source_text.rfind('\n', 0, lexpos) + 1
    return (lexpos - line_start) + 1

parser = yacc.yacc()
source_text += "\n"
parser.parse(source_text)


def is_var(thing):
    if not isinstance(thing, str):
        return thing
    if thing == "TRUE":
        return "1"
    if thing == "FALSE":
        return "0"
    if "tmp_" in thing:
        return "r"+(thing.replace("tmp_", ""))
    if thing[-1] == "]":
        m = re.search(r"\[(.*)\]", thing)
        t = m.group(1)
        m = re.search(r"(.*)\[.*\]", thing)
        thing = m.group(1)+"+"+t
    return "["+thing+"]"

def var_type(size):
    if size == 1:
        return "DB"
    else:
        return ".space {}".format(size)

def jump_type(comp):
    op = ["!=", "==", ">=", "<=", ">", "<"]
    ret = ["jnz", "jz", "jge", "jle", "jg", "jl"]
    return ret[op.index(comp)]

def quad_to_asm_jump(j):
    if j == "BZ":
        return "jz"
    elif j == "BGE":
        return "jge"

def quad_to_asm_op(o):
    op = ["+", "-", "*", "/"]
    ret = ["add", "sub", "mul", "div"]
    return ret[op.index(o)]

def asm(quads):
    output2 = []
    output2.append("section .data")
    for name, value in table_symbols._symbols.items():
        output2.append(name+" "+var_type(value.size))
    output2.append("section .start")

    output = []
    labels = []
    i = 0
    label_counter = 1
    for quad in quads:
        if quad[0] == "=":
            if not is_var(quad[1]) and type(quad[1]) is str:
                output.append([i, "mov {}, '{}'".format(is_var(quad[3]), quad[1])])
            else:
                output.append([i, "mov {}, {}".format(is_var(quad[3]), is_var(quad[1]))])
        elif quad[0] in ["+", "-", "/", "*"]:
            if quad[1] == quad[3]:
                output.append([i, "mov rax, {}".format(is_var(quad[3]))])
                if quad[2] == 1:
                    if quad[0] == "-":
                        output.append([i, "dec rax"])
                    elif quad[0] == "+":
                        output.append([i, "inc rax"])
                else:
                    output.append([i, "{} rax, {}".format(quad_to_asm_op(quad[0]), is_var(quad[2]))])
                output.append([i, "mov {}, rax".format(is_var(quad[3]))])
            else:
                output.append([i, "mov {}, {}".format(is_var(quad[3]), is_var(quad[1]))])
                output.append([i, "{} {}, {}".format(quad_to_asm_op(quad[0]), is_var(quad[3]), is_var(quad[2]))])
        elif quad[0] in ["!=", "==", ">=", "<=", "<", ">"]:
            output.append([i, "mov {}, {}".format(is_var(quad[3]), is_var(quad[1]))])
            output.append([i, "sub {}, {}".format(is_var(quad[3]), is_var(quad[2]))])
            output.append([i, "cmp {}, 0".format(is_var(quad[3]))])
            output.append([i, "{} .label_{}".format(jump_type(quad[0]), label_counter)])
            output.append([i, "mov {}, 0".format(is_var(quad[3]))])
            output.append([i, "jmp .label_{}".format(label_counter+1)])
            output.append([i, ".label_{}".format(label_counter)])
            output.append([i, "mov {}, 1".format(is_var(quad[3]))])
            output.append([i, ".label_{}".format(label_counter+1)])
            label_counter += 2
        elif quad[0] in ["AND", "OR"]:
            output.append([i, "mov {}, {}".format(is_var(quad[3]), is_var(quad[1]))])
            output.append([i, "{} {}, {}".format(quad[0].lower(), is_var(quad[3]), is_var(quad[2]))])
        elif quad[0] == "BZ":
            output.append([i, "cmp {}, 0".format(is_var(quad[2]))])
            output.append([i, "{} .label_{}".format(quad_to_asm_jump(quad[0]), label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        elif quad[0] == "BGE":
            output.append([i, "cmp {}, {}".format(is_var(quad[2]), is_var(quad[3]))])
            output.append([i, "{} .label_{}".format(quad_to_asm_jump(quad[0]), label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        elif quad[0] == "BR":
            output.append([i, "jmp .label_{}".format(label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        i += 1
    output.append([output[-1][0]+1, "END"])

    for inst in output:
        to_delete = []
        for k, label in enumerate(labels):
            if label[0] == inst[0]:
                output2.append(".label_{}".format(label[1]))
                to_delete.append(k)
        labels = [j for i, j in enumerate(labels) if i not in to_delete]

        if inst[1] != "END":
            output2.append(inst[1])

    return output2

quads.append(["END", "", "", ""])

if not succes:
    exit()

print("Table de symboles :")
print("===================")
for name, value in table_symbols._symbols.items():
    print("name: "+name+" size:"+str(value.size))

print()
print()

print("Quadruplés :")
print("=============")
for quad in quads:
    print(quad)

print()
print()
print("Code machine :")
print("==============")
for a in asm(quads):
    print(a)