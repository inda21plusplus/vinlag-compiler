use std::mem;

use crate::{jit, Token};

// box to not trigger infinate size error
pub enum Expr {
    String(i64), // index to the const string
    Int(i64),
    Identifier(String),
    Assign(String, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Break,
    /** where the last expr is returned*/
    Field(Vec<Expr>),

    /** condition, Field*/
    If(Box<Expr>, Box<Expr>),

    /** condition, then_body, else body*/
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /** condition, code*/
    WhileLoop(Box<Expr>, Box<Expr>),

    /** id, args*/
    Call(String, Vec<Expr>),
}

impl Expr {
    // used to print the AST before it gets compiled
    #[allow(dead_code)]
    fn to_string(self: &Expr) -> String {
        match self {
            Expr::Int(int) => int.to_string(),
            Expr::String(id) => {
                let mut text = String::from('&');
                text.push_str(&id.to_string());
                text
            }
            Expr::Call(id, args) => {
                let mut text = String::from(id);
                text.push('(');
                for arg in args {
                    text.push_str(&arg.to_string());
                    text.push(',')
                }
                text.push(')');
                text
            }
            Expr::Assign(id, expr) => {
                let mut text = String::from('%');
                text.push_str(&id);
                text.push_str(" = ");
                text.push_str(&*expr.to_string());
                text
            }
            Expr::Identifier(id) => {
                let mut text = String::from('%');
                text.push_str(&id);
                text
            }
            Expr::Add(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" + ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Sub(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" - ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Mul(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" * ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Div(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" / ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Eq(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" == ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Ne(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" != ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Lt(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" < ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Le(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" <= ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }
            Expr::Ge(lhs, rhs) => {
                let mut r = String::from("(");
                r.push_str(&*lhs.to_string());
                r.push_str(" >= ");
                r.push_str(&*rhs.to_string());
                r.push(')');
                r
            }

            Expr::If(cond, then_body) => {
                let mut r = String::from("if");
                r.push_str(&*cond.to_string());
                r.push_str(&*then_body.to_string());
                r
            }
            Expr::IfElse(cond, then_body, else_body) => {
                let mut r = String::from("if");
                r.push_str(&*cond.to_string());
                r.push_str(&*then_body.to_string());
                r.push_str("else\n");
                r.push_str(&*else_body.to_string());
                r
            }
            Expr::Field(exprs) => {
                let mut r = String::from("\n{\n");
                for expr in exprs {
                    r.push_str(&*expr.to_string().replace("\n", "\n\t"));
                    r.push_str("\n");
                }
                r.push_str("}");
                r.replace("\n\n", "\n")
            }
            Expr::WhileLoop(cond, body) => {
                let mut r = String::from("while ");
                r.push_str(&cond.to_string());
                r.push_str(&body.to_string());
                r
            }
            Expr::Break => {
                let r = String::from("break ");
                r
            }
            _ => String::new(),
        }
    }
}

pub struct ParserFunction {
    pub name: String,
    //pub return_name: String,
    pub arguments: Vec<String>,
    pub stmts: Expr,
}

struct TokenIter {
    consts_strings: Vec<String>,
    index: usize,
    data: Vec<Token>,
}

impl TokenIter {
    fn new(data: Vec<Token>) -> Self {
        return TokenIter {
            index: 0usize,
            data,
            consts_strings: Vec::new(),
        };
    }

    fn itt(self: &mut TokenIter) {
        self.index += 1;
    }

    /**reads current, and then steps 1 */
    fn next(self: &mut TokenIter) -> &Token {
        if let Some(token) = self.data.get(self.index) {
            self.index += 1;
            token
        } else {
            &Token::EOF
        }
    }

    fn get_stacktrace(self: &mut TokenIter, err: String) -> String {
        let mut err_string = String::new();
        err_string.push_str("Expected ");
        err_string.push_str(&err);
        err_string.push_str(" at\n");

        if self.index == 0 {
            err_string.push_str(&self.data[self.index].to_string());
            return err_string;
        }

        for i in (i32::max(0, self.index as i32 - 5) as usize)..self.index {
            err_string.push_str(&self.data[i].to_string());
            err_string.push(' ');
        }
        err_string.push('[');
        err_string.push_str(&self.data[self.index].to_string());
        err_string.push(']');

        err_string
    }

    fn eat(self: &mut TokenIter, token: &Token, err: Option<String>) -> Result<(), String> {
        if self.next() == token {
            Ok(())
        } else {
            Err(self.get_stacktrace(err.unwrap_or(token.to_string())))
        }
    }

    fn current(self: &TokenIter) -> &Token {
        &self.data[self.index]
    }

    fn peek(self: &mut TokenIter) -> &Token {
        &self.data[self.index + 1]
    }

    /*fn print_current(self: &TokenIter) {
        println!("CURRENT: {}", self.current().to_string());
    }*/

    fn accept_all(self: &mut TokenIter) -> Result<Vec<(bool, ParserFunction)>, String> {
        let mut functions: Vec<(bool, ParserFunction)> = Vec::new();
        loop {
            if self.current() == &Token::EOF {
                break Ok(functions);
            } else {
                functions.push(self.accept_head_function()?);
            }
        }
    }

    fn accept_head_function(self: &mut TokenIter) -> Result<(bool, ParserFunction), String> {
        match self.next() {
            &Token::ENTRY => {
                return Ok((true, self.accept_function()?));
            }
            &Token::DEF => {
                return Ok((false, self.accept_function()?));
            }
            _ => Err(self.get_stacktrace("Function Definition".to_string())),
        }
    }

    fn next_identifier(self: &mut TokenIter, err: Option<String>) -> Result<String, String> {
        match self.next() {
            Token::IDENTIFIER(data) => Ok(data.to_string()),
            _ => Err(self.get_stacktrace(err.unwrap_or("Identifier".to_string()))),
        }
    }

    fn accept_identifier(
        self: &mut TokenIter,
        id: String,
        self_contained: bool,
    ) -> Result<Expr, String> {
        match self.peek() {
            Token::EQUALS => {
                if self_contained {
                    self.itt();
                    return Ok(Expr::Identifier(id));
                }
                self.itt();
                self.eat(&Token::EQUALS, None)?;
                Ok(Expr::Assign(id.to_string(), Box::new(self.accept_expr()?)))
            }
            Token::LPAREN => {
                self.itt();
                self.eat(&Token::LPAREN, None)?;

                let mut arguments: Vec<Expr> = Vec::new();
                loop {
                    match self.current() {
                        &Token::RPAREN => {
                            self.eat(&Token::RPAREN, None)?;
                            break;
                        }
                        _ => {
                            arguments.push(self.accept_expr()?);
                        }
                    }
                }

                Ok(Expr::Call(TokenIter::parse_call(id), arguments))
            }
            _ => {
                self.itt();
                Ok(Expr::Identifier(id))
            }
        }
    }

    fn factor(self: &mut TokenIter, self_contained: bool) -> Result<Expr, String> {
        match self.current() {
            Token::PLUS => {
                // +++ => +
                self.itt();
                Ok(self.factor(self_contained)?)
            }
            Token::MINUS => {
                // --x => -(0-x)
                self.itt();
                Ok(Expr::Sub(
                    Box::new(Expr::Int(0)),
                    Box::new(self.factor(self_contained)?),
                ))
            }

            Token::LPAREN => {
                self.eat(&Token::LPAREN, None)?;
                let node = self.expr(self_contained)?;
                self.eat(&Token::RPAREN, None)?;
                Ok(node)
            }
            Token::INT(int_ptr) => {
                let int = *int_ptr;
                self.itt();
                Ok(Expr::Int(int))
            }
            Token::LCURLY => Ok(self.accept_expr()?),
            Token::IDENTIFIER(string_ptr) => {
                let id = string_ptr.to_string();
                self.accept_identifier(id, self_contained)
            }
            Token::IF => {
                Ok(self.accept_expr()?)
            }
            _ => Err(self.get_stacktrace("factor".to_string())),
        }
    }

    fn term(self: &mut TokenIter, self_contained: bool) -> Result<Expr, String> {
        let mut node = self.factor(self_contained)?;
        loop {
            let token = self.current();
            if token == &Token::TIMES {
                self.next();
                node = Expr::Mul(Box::new(node), Box::new(self.factor(self_contained)?));
            } else if token == &Token::DIVIDE {
                self.next();
                node = Expr::Div(Box::new(node), Box::new(self.factor(self_contained)?));
            } else if token == &Token::MODULUS {
                //a % b = a - (b * int(a/b))
                self.next();
                let mod_a = "_mod_a".to_string();
                let mod_b = "_mod_b".to_string();
                node = Expr::Field(vec![
                    Expr::Assign(mod_a.clone(), Box::new(node)),
                    Expr::Assign(mod_b.clone(), Box::new(self.factor(self_contained)?)),
                    Expr::Sub(
                        Box::new(Expr::Identifier(mod_a.clone())),
                        Box::new(Expr::Mul(
                            Box::new(Expr::Identifier(mod_b.clone())),
                            Box::new(Expr::Div(
                                Box::new(Expr::Identifier(mod_a)),
                                Box::new(Expr::Identifier(mod_b)),
                            )),
                        )),
                    ),
                ]);
            } else {
                break Ok(node);
            }
        }
    }

    fn expr(self: &mut TokenIter, self_contained: bool) -> Result<Expr, String> {
        let mut node = self.term(self_contained)?;

        loop {
            let token = self.current();
            if token == &Token::PLUS {
                self.next();
                node = Expr::Add(Box::new(node), Box::new(self.term(self_contained)?));
            } else if token == &Token::MINUS {
                self.next();
                node = Expr::Sub(Box::new(node), Box::new(self.term(self_contained)?));
            } else {
                break Ok(node);
            }
        }
    }

    fn accept_math(self: &mut TokenIter) -> Result<Expr, String> {
        self.expr(false)
    }

    fn accept_cond(self: &mut TokenIter) -> Result<Expr, String> {
        let lhs = self.expr(true)?;
        let cond = self.next().clone();

        let next_is_eq = self.current() == &Token::EQUALS;
        if next_is_eq {
            self.eat(&Token::EQUALS, None)?;
        }

        let rhs = self.expr(true)?;

        if next_is_eq {
            match cond {
                Token::EQUALS => Ok(Expr::Eq(Box::new(lhs), Box::new(rhs))),
                Token::GREATER => Ok(Expr::Ge(Box::new(lhs), Box::new(rhs))),
                Token::LESS => Ok(Expr::Le(Box::new(lhs), Box::new(rhs))),
                Token::NOT => Ok(Expr::Ne(Box::new(lhs), Box::new(rhs))),
                _ => Err(self.get_stacktrace("condition".to_string())),
            }
        } else {
            match cond {
                Token::EQUALS => Ok(Expr::Eq(Box::new(lhs), Box::new(rhs))),
                Token::GREATER => Ok(Expr::Gt(Box::new(lhs), Box::new(rhs))),
                Token::LESS => Ok(Expr::Lt(Box::new(lhs), Box::new(rhs))),
                Token::NOT => Ok(Expr::Ne(Box::new(lhs), Box::new(rhs))),
                _ => Err(self.get_stacktrace("condition".to_string())),
            }
        }
    }

    fn accept_expr(self: &mut TokenIter) -> Result<Expr, String> {
        match &*self.current() {
            Token::LCURLY => self.accept_field(),
            Token::LPAREN => self.accept_math(),
            Token::FOR => {
                self.eat(&Token::FOR, None)?;
                let indexer = self.next_identifier(Some("for indexer".to_string()))?;
                let mut to_indexer = String::from("_");
                to_indexer.push_str(&indexer);

                let from = self.accept_expr()?;
                let to = self.accept_expr()?;
                let mut field = self.accept_field_lines()?;
                field.push(Expr::Assign(
                    indexer.clone(),
                    Box::new(Expr::Add(
                        Box::new(Expr::Identifier(indexer.clone())),
                        Box::new(Expr::Int(1)),
                    )),
                ));

                Ok(Expr::Field(vec![
                    Expr::Assign(indexer.clone(), Box::new(from)),
                    Expr::Assign(to_indexer.clone(), Box::new(to)),
                    Expr::WhileLoop(
                        Box::new(Expr::Lt(
                            Box::new(Expr::Identifier(indexer.clone())),
                            Box::new(Expr::Identifier(to_indexer.clone())),
                        )),
                        Box::new(Expr::Field(field)),
                    ),
                ]))
            }
            Token::BREAK => {
                self.eat(&Token::BREAK, None)?;

                Ok(Expr::Break)
            }

            Token::IF => {
                self.eat(&Token::IF, None)?;
                let cond = self.accept_cond()?;
                let then_body = self.accept_field()?;
                if self.current() == &Token::ELSE {
                    self.eat(&Token::ELSE, None)?;
                    let else_body = self.accept_expr()?;

                    Ok(Expr::IfElse(
                        Box::new(cond),
                        Box::new(then_body),
                        Box::new(else_body),
                    ))
                } else {
                    Ok(Expr::If(Box::new(cond), Box::new(then_body)))
                }
            }
            Token::IFBREAK => {
                self.eat(&Token::IFBREAK, None)?;
                let cond = self.accept_cond()?;
                let then_body = Expr::Break;
                Ok(Expr::If(Box::new(cond), Box::new(then_body)))
            }
            Token::INT(int_ptr) => {
                let int = *int_ptr;
                match self.peek() {
                    Token::TIMES => self.accept_math(),
                    Token::MINUS => self.accept_math(),
                    Token::PLUS => self.accept_math(),
                    Token::DIVIDE => self.accept_math(),
                    _ => {
                        self.itt();
                        Ok(Expr::Int(int)) //TODO
                    }
                }
            }
            Token::IDENTIFIER(_) => self.accept_math(),
            Token::STRING(str_ptr) => {
                let string_data = str_ptr.to_string();
                self.consts_strings.push(string_data.replace("\\n", "\n"));
                self.itt();
                Ok(Expr::String(self.consts_strings.len() as i64 - 1))
            }
            _ => Err(self.get_stacktrace("accept_expr".to_string())),
        }
    }

    fn accept_field_lines(self: &mut TokenIter) -> Result<Vec<Expr>, String> {
        self.eat(&Token::LCURLY, None)?;

        let mut lines: Vec<Expr> = Vec::new();
        loop {
            match self.current() {
                &Token::RCURLY => {
                    self.eat(&Token::RCURLY, None)?;
                    break;
                }
                _ => {
                    lines.push(self.accept_expr()?);
                }
            }
        }

        if lines.len() == 0 {
            lines.push(Expr::Int(0))
        }
        Ok(lines)
    }

    fn accept_field(self: &mut TokenIter) -> Result<Expr, String> {
        Ok(Expr::Field(self.accept_field_lines()?))
    }

    fn parse_call(id: String) -> String {
        match id.as_ref() {
            "yell" => String::from("printf"),
            _ => {
                let mut name = String::from("__");
                name.push_str(&id);
                name
            }
        }
    }

    fn accept_function(self: &mut TokenIter) -> Result<ParserFunction, String> {
        let name = self.next_identifier(Some("Function Name".to_string()))?;
        self.eat(&Token::LPAREN, None)?;

        let mut arguments: Vec<String> = Vec::new();

        loop {
            if self.current() == &Token::RPAREN {
                self.itt();
                break;
            } else {
                arguments.push(self.next_identifier(None)?)
            }
        }


        let stmts = self.accept_field()?;

        Ok(ParserFunction {
            name: TokenIter::parse_call(name),
            arguments,
            stmts,
        })
    }
}

pub(crate) fn get_string_id(id: i64) -> String {
    let mut string_name = String::from("__str");
    string_name.push_str(&id.to_string());
    string_name
}

pub fn get_functions(tokens: Vec<Token>) {
    let mut iter = TokenIter::new(tokens);
    // println!("=====================");
    let result = iter.accept_all();

    if let Err(err_string) = result {
        println!("{}", err_string);
    } else if let Ok(ok_expr) = result {
        //println!("=========");
        //for (is_main, func) in &ok_expr {
        //    println!("{} {}", is_main, func.name);
        //    println!("{}", func.stmts.to_string());
        //}
        //println!("=========");

        let mut jit = jit::JIT::default();

        for index in 0..iter.consts_strings.len() {
            let _data_err = jit.create_data(
                &get_string_id(index as i64),
                iter.consts_strings[index].as_bytes().to_vec(),
            );
        }

        //println!("Running code...");

        unsafe {
            let result = run_code(&mut jit, ok_expr, ());
           // println!("\nDONE!");
            if let Ok(exit_code) = result {
                println!("Exit code {}", exit_code);
            } else if let Err(err) = result {
                println!("ERROR {}", &err)
            }
        }

        //println!("RES: {}", ok_expr.to_string());
    }
}

unsafe fn run_code<I>(
    jit: &mut jit::JIT,
    code: Vec<(bool, ParserFunction)>,
    input: I,
) -> Result<isize, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(code)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = mem::transmute::<_, fn(I) -> isize>(code_ptr);
    // And now we can call it!

    //use std::time::Instant;
    //let now = Instant::now();
    let exitcode = code_fn(input);
    //let elapsed = now.elapsed();
    // println!("Elapsed: {:.2?}", elapsed);

    Ok(exitcode)
}
