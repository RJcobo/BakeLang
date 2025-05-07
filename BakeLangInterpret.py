import re, sys

# Lexer
class Token:
    def __init__(self, type_, value):
        self.type, self.value = type_, value
    def __repr__(self):
        return f"Token({self.type}, {self.value!r})"

def tokenize(source):
    specs = [
        ('NUM',  r'\d+'),
        ('STR',  r'"([^\\"]*)"'),
        ('NAME', r'[A-Za-z_]\w*'),
        ('OPS',  r'==|%|\+'),
        ('SYM',  r'[;:{}\(\),]'),
        ('SKIP', r'[ \t\n]+'),
    ]
    tok_regex = '|'.join(f'(?P<{n}>{p})' for n,p in specs)
    matcher = re.compile(tok_regex).match
    pos, tokens = 0, []
    while pos < len(source):
        m = matcher(source, pos)
        if not m:
            raise SyntaxError(f"Unexpected character: {source[pos]!r}")
        kind = m.lastgroup
        val  = m.group(kind)
        if kind == 'NUM':
            tokens.append(Token('NUMBER', int(val)))
        elif kind == 'STR':
            tokens.append(Token('STRING', val[1:-1]))
        elif kind == 'NAME':
            tokens.append(Token('ID', val))
        elif kind == 'OPS':
            tokens.append(Token('OP', val))
        elif kind == 'SYM':
            tokens.append(Token(val, val))  # type = the symbol itself
        pos = m.end()
    tokens.append(Token('EOF', None))
    return tokens

# AST Nodes
class Program:
    def __init__(self, recipes, main_stmts):
        self.recipes, self.main = recipes, main_stmts
    def eval(self):
        env = {'__recipes__': {}}
        for r in self.recipes: r.eval(env)
        for s in self.main:      s.eval(env)

class IngredientDecl:
    def __init__(self, name, _type): self.name, self.type_ = name, _type
    def eval(self, env): env[self.name] = None

class MixStmt:
    def __init__(self, expr, target): self.expr, self.target = expr, target
    def eval(self, env): env[self.target] = self.expr.eval(env)

class ServeStmt:
    def __init__(self, expr): self.expr = expr
    def eval(self, env): print(self.expr.eval(env))

class AskStmt:
    def __init__(self, prompt, target):
        self.prompt, self.target = prompt, target

    def eval(self, env):
        p = self.prompt.eval(env)
        raw = input(str(p) + " ")
        # try to turn to int if it looks like one:
        try:
            val = int(raw)
        except ValueError:
            val = raw
        env[self.target] = val

class IfStmt:
    def __init__(self, cond, then_s, else_s=None):
        self.cond, self.then_s, self.else_s = cond, then_s, else_s
    def eval(self, env):
        if self.cond.eval(env):
            for s in self.then_s: s.eval(env)
        elif self.else_s:
            for s in self.else_s: s.eval(env)

class RepeatStmt:
    def __init__(self, start, end, body): self.start, self.end, self.body = start, end, body
    def eval(self, env):
        lo, hi = self.start.eval(env), self.end.eval(env)
        for i in range(lo, hi+1):
            env['i'] = i
            for s in self.body: s.eval(env)

class RecipeDef:
    def __init__(self, name, params, body): self.name, self.params, self.body = name, params, body
    def eval(self, env):
        env['__recipes__'][self.name] = (self.params, self.body)

class MakeStmt:
    def __init__(self, name, args): self.name, self.args = name, args
    def eval(self, env):
        recipes = env['__recipes__']
        if self.name not in recipes:
            raise NameError(f"Recipe '{self.name}' not found")
        params, body = recipes[self.name]
        if len(params)!=len(self.args):
            raise TypeError(f"{self.name} expects {len(params)} args, got {len(self.args)}")
        local = {'__recipes__': recipes}
        for pname, arg in zip(params, self.args):
            local[pname] = arg.eval(env)
        for s in body: s.eval(local)

class Number:
    def __init__(self, v): self.v = v
    def eval(self, env): return self.v

class String:
    def __init__(self, v): self.v = v
    def eval(self, env): return self.v

class Var:
    def __init__(self, n): self.n = n
    def eval(self, env):
        if self.n not in env or env[self.n] is None:
            raise NameError(f"'{self.n}' undefined/uninitialized")
        return env[self.n]

class BinOp:
    def __init__(self, left, op, right):
        self.left, self.op, self.right = left, op, right

    def eval(self, env):
        a = self.left.eval(env)
        b = self.right.eval(env)

        if self.op == '+':
            # if either side is a string, coerce both to str
            if isinstance(a, str) or isinstance(b, str):
                return str(a) + str(b)
            return a + b

        if self.op == '%':
            return a % b

        if self.op == '==':
            return int(a == b)

        raise SyntaxError(f"Unknown operator {self.op}")


# Parser
class Parser:
    def __init__(self, tokens): self.tokens, self.pos = tokens, 0
    def cur(self):        return self.tokens[self.pos]
    def eat(self, ttype, tval=None):
        tok = self.cur()
        if tok.type==ttype and (tval is None or tok.value==tval):
            self.pos += 1; return tok
        raise SyntaxError(f"Expected {ttype} {tval}, got {tok.type} {tok.value!r}")

    def parse(self):
        recipes, main = [], []
        while self.cur().type!='EOF':
            if self.cur().type=='ID' and self.cur().value.lower()=='recipe':
                recipes.append(self.parse_recipe())
            else:
                main.append(self.parse_stmt())
        return Program(recipes, main)

    def parse_recipe(self):
        self.eat('ID')               # Recipe
        name = self.eat('ID').value
        self.eat('(')
        params=[]
        if self.cur().type=='ID':
            params.append(self.eat('ID').value)
            while self.cur().value==',':
                self.eat(','); params.append(self.eat('ID').value)
        self.eat(')')
        body = self.parse_block()
        return RecipeDef(name, params, body)

    def parse_stmt(self):
        tok = self.cur()
        if tok.type=='ID':
            kw = tok.value.lower()
            if kw in ('mix','fold'): return self.parse_mix()
            if kw=='serve':          return self.parse_serve()
            if kw=='ask':            return self.parse_ask()
            if kw=='if':             return self.parse_if()
            if kw=='repeat':         return self.parse_repeat()
            if kw=='make':           return self.parse_make()
            if kw=='ingredient':     return self.parse_decl()
        raise SyntaxError(f"Unknown statement {tok.value!r}")

    def parse_decl(self):
        self.eat('ID')              # Ingredient
        name = self.eat('ID').value
        self.eat(':'); _t=self.eat('ID').value
        self.eat(';')
        return IngredientDecl(name, _t)

    def parse_mix(self):
        self.eat('ID')              # Mix/Fold
        expr = self.parse_expr()
        self.eat('ID')              # into
        targ = self.eat('ID').value
        self.eat(';')
        return MixStmt(expr, targ)

    def parse_serve(self):
        self.eat('ID')              # Serve
        expr = self.parse_expr()
        self.eat(';')
        return ServeStmt(expr)

    def parse_ask(self):
        self.eat('ID')              # Ask
        prompt = self.parse_expr()
        self.eat('ID')              # into
        name = self.eat('ID').value
        self.eat(';')
        return AskStmt(prompt, name)

    def parse_if(self):
        self.eat('ID')              # If
        self.eat('(')
        cond = self.parse_expr()
        self.eat(')')
        self.eat('ID')              # then
        then_b = self.parse_block()
        else_b = None
        if self.cur().type=='ID' and self.cur().value.lower()=='else':
            self.eat('ID'); else_b=self.parse_block()
        return IfStmt(cond, then_b, else_b)

    def parse_repeat(self):
        self.eat('ID')              # Repeat
        start=self.parse_expr()
        self.eat('ID')              # to
        end  =self.parse_expr()
        body =self.parse_block()
        return RepeatStmt(start, end, body)

    def parse_make(self):
        self.eat('ID')              # Make
        name=self.eat('ID').value
        self.eat('(')
        args=[]
        if self.cur().type in ('NUMBER','STRING','ID','('):
            args.append(self.parse_expr())
            while self.cur().value==',':
                self.eat(','); args.append(self.parse_expr())
        self.eat(')')
        self.eat(';')
        return MakeStmt(name, args)

    def parse_block(self):
        self.eat('{')
        stmts=[]
        while self.cur().value != '}':
            stmts.append(self.parse_stmt())
        self.eat('}')
        return stmts

    # expr -> equality -> add -> mod -> primary
    def parse_expr(self): return self.parse_eq()
    def parse_eq(self):
        node=self.parse_add()
        while self.cur().type=='OP' and self.cur().value=='==':
            op=self.eat('OP').value
            node=BinOp(node, op, self.parse_add())
        return node
    def parse_add(self):
        node=self.parse_mod()
        while self.cur().type=='OP' and self.cur().value=='+':
            op=self.eat('OP').value
            node=BinOp(node, op, self.parse_mod())
        return node
    def parse_mod(self):
        node=self.parse_primary()
        while self.cur().type=='OP' and self.cur().value=='%':
            op=self.eat('OP').value
            node=BinOp(node, op, self.parse_primary())
        return node
    def parse_primary(self):
        t=self.cur()
        if t.type=='NUMBER':
            self.eat('NUMBER'); return Number(t.value)
        if t.type=='STRING':
            self.eat('STRING'); return String(t.value)
        if t.type=='ID':
            self.eat('ID'); return Var(t.value)
        if t.value=='(':
            self.eat('(')
            node=self.parse_expr()
            self.eat(')')
            return node
        raise SyntaxError(f"Unexpected token {t}")

# Runner & CLI
def run_file(path):
    code = open(path).read()
    ast  = Parser(tokenize(code)).parse()
    ast.eval()

def main():
    if len(sys.argv)!=2 or not sys.argv[1].endswith('.bl'):
        print("Usage: python BakeLangInterpret.py <recipe>.bl")
        sys.exit(1)
    run_file(sys.argv[1])

if __name__=='__main__':
    main()
