pub mod enums;
pub mod errors;
#[cfg(test)]
mod tests;
use std::fs::File;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::path::PathBuf;
use std::rc::Rc;
use std::slice::Iter;
use std::vec;

use rpds::HashTrieMap;
use rpds::List;

use crate::lexer::enums::*;
use crate::lexer::*;
use crate::parser::enums::*;
use crate::parser::errors::*;

const OUTSIDE_INDENTATION: i32 = -1;

macro_rules! new_binary_operation {
    ($op:expr, $expr_1:expr, $expr_2:expr) => {
        Expression::BinaryOperation($op, Rc::new($expr_1), Rc::new($expr_2))
    };
}

macro_rules! new_unary_function {
    ($parser:expr, $exp:expr, $pure:expr) => {{
        let param = $parser.new_ident("x");
        Expression::Value(Value::Function {
            params: vec![param],
            scope: new_scope_with_single_expr($exp(Rc::new(Expression::Ident(param))), $pure),
        })
    }};
}

macro_rules! new_print_function {
    ($parser:expr, $newline:expr) => {{
        let param = $parser.new_ident("x");
        Expression::Value(Value::Function {
            params: vec![param],
            scope: new_scope_with_single_expr(
                Expression::PrintCall {
                    expr: Rc::new(Expression::Ident(param)),
                    newline: $newline,
                },
                false,
            ),
        })
    }};
}

macro_rules! new_binary_function {
    ($parser:expr, $exp:expr, $pure:expr) => {{
        let param_1 = $parser.new_ident("x");
        let param_2 = $parser.new_ident("y");
        Expression::Value(Value::Function {
            params: vec![param_1, param_2],
            scope: new_scope_with_single_expr(
                $exp(
                    Rc::new(Expression::Ident(param_1)),
                    Rc::new(Expression::Ident(param_2)),
                ),
                $pure,
            ),
        })
    }};
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionLine {
    Expression(Rc<Expression>),
    Assignment(u32, Rc<Expression>),
}

pub struct Parser {
    next_lines: Peekable<LinesIterator>,
    filename: String,
    file_path: PathBuf,
    next_ident: u32,
    pub names: Vec<String>,
}

impl Parser {
    pub fn new(lines: LinesIterator, file_path: PathBuf) -> Parser {
        Parser {
            next_lines: lines.peekable(),
            filename: file_path.to_string_lossy().to_string(),
            file_path,
            next_ident: 0,
            names: vec![],
        }
    }

    // entry point for the parser
    pub fn parse_outside_scope(&mut self) -> Result<Scope, ParserError> {
        let (scope, _) = self.parse_scope(
            OUTSIDE_INDENTATION,
            false,
            List::new().push_front(self.file_path.clone()),
            HashTrieMap::new(),
        )?;
        Ok(scope)
    }

    // returns new unique identifier
    fn new_ident(&mut self, name: &str) -> u32 {
        self.names.push(name.to_owned());
        let ret = self.next_ident;
        self.next_ident += 1;
        ret
    }

    // checks whether the path is already imported
    // if it is not imported, adds it to the list
    fn check_imported_else_add(
        &mut self,
        path: PathBuf,
        imported: List<PathBuf>,
        line: &Line,
        token: usize,
    ) -> Result<(bool, List<PathBuf>), ParserError> {
        let canonical_path = path.canonicalize().map_err(|err| {
            self.produce_error(
                ParserErrorKind::CannotImportFile {
                    import_filename: path.to_string_lossy().to_string(),
                    error_message: err.to_string(),
                },
                Some(line),
                token,
            )
        })?;

        if imported.iter().all(|x| *x != canonical_path) {
            Ok((false, imported.push_front(canonical_path)))
        } else {
            Ok((true, imported))
        }
    }

    // returns error with the specified error kind
    fn produce_error(
        &self,
        kind: ParserErrorKind,
        line: Option<&Line>,
        token_pos: usize,
    ) -> ParserError {
        ParserError {
            kind,
            filename: self.filename.clone(),
            line: line.cloned(),
            token_pos,
        }
    }

    fn assert_next_token(
        &self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        token: Token,
        error_kind: ParserErrorKind,
        line: &Line,
    ) -> Result<(), ParserError> {
        match tokens.next() {
            Some((_, actual_token)) if *actual_token == token => (),
            Some((token_pos, _)) => {
                return Err(self.produce_error(error_kind, Some(line), token_pos))
            }
            None => return Err(self.produce_error(error_kind, Some(line), line.tokens.len())),
        };
        Ok(())
    }

    fn assert_no_more_tokens(
        &self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
    ) -> Result<(), ParserError> {
        match tokens.next() {
            None => (),
            Some((token_pos, token)) => {
                return Err(self.produce_error(
                    ParserErrorKind::UnexpectedToken {
                        token: token.clone(),
                    },
                    Some(line),
                    token_pos,
                ))
            }
        };
        Ok(())
    }

    fn parse_scope(
        &mut self,
        outside_indentation: i32,
        pure: bool,
        imported: List<PathBuf>,
        mut ident_map: HashTrieMap<String, u32>,
    ) -> Result<(Scope, HashTrieMap<String, u32>), ParserError> {
        let mut lines: Vec<FunctionLine> = vec![];
        let mut already_parsed_expr = false;

        let line = self
            .next_lines
            .next()
            .ok_or(self.produce_error(ParserErrorKind::UnexpectedEOF, None, 0))?
            .map_err(|_| self.produce_error(ParserErrorKind::LexerError, None, 0))?;

        let scope_indentation = line.indentation;
        if scope_indentation as i32 <= outside_indentation {
            return Err(self.produce_error(
                ParserErrorKind::IndentationError {
                    msg: String::from("Scope has to be indented"),
                    expected: outside_indentation + 1,
                    actual: outside_indentation,
                },
                Some(&line),
                0,
            ));
        }

        // parse the first line
        ident_map = self.parse_line(
            &line,
            &mut lines,
            pure,
            &mut already_parsed_expr,
            imported.clone(),
            ident_map,
        )?;

        // read next lines
        while let Some(line_result) = self.next_lines.peek() {
            let next_line = line_result.as_ref().map_err(|_| ParserError {
                kind: ParserErrorKind::PeekError,
                filename: self.filename.clone(),
                line: None,
                token_pos: 0,
            })?;
            let next_line_indentation = next_line.indentation;

            if next_line_indentation < scope_indentation
                && next_line_indentation as i32 <= outside_indentation
            {
                // end of scope
                break;
            } else if outside_indentation > 0 && next_line_indentation < scope_indentation {
                // scope has not ended yet but has different indentation
                let next_line = next_line.clone();
                return Err(self.produce_error(
                    ParserErrorKind::IndentationError {
                        msg: String::from("The indentation of a scope should be the same"),
                        expected: scope_indentation as i32,
                        actual: next_line_indentation as i32,
                    },
                    Some(&next_line),
                    0,
                ));
            } else {
                // scope has not ended and has valid indentation
                let line = self.next_lines.next().unwrap().map_err(|_| ParserError {
                    kind: ParserErrorKind::LexerError,
                    filename: self.filename.clone(),
                    line: None,
                    token_pos: 0,
                })?;

                // parse line
                ident_map = self.parse_line(
                    &line,
                    &mut lines,
                    pure,
                    &mut already_parsed_expr,
                    imported.clone(),
                    ident_map,
                )?;
            }
        }

        if pure {
            let mut expressions = vec![];
            let mut assignments = vec![];

            for function_line in lines {
                match function_line {
                    FunctionLine::Assignment(name, expr) => {
                        assignments.push((name, expr));
                    }
                    FunctionLine::Expression(expr) => {
                        expressions.push(expr);
                    }
                }
            }

            if expressions.len() == 0 {
                Err(self.produce_error(ParserErrorKind::ExpressionExpected, Some(&line), 0))
            } else {
                Ok((
                    Scope::Pure {
                        assignments,
                        expression: expressions.pop().unwrap(),
                    },
                    ident_map,
                ))
            }
        } else {
            Ok((Scope::Impure { lines }, ident_map))
        }
    }

    // mutates the lines vector, adding the assignments and expressions of the import scope
    fn add_import(
        &mut self,
        imported: List<PathBuf>,
        lines: &mut Vec<FunctionLine>,
        line: &Line,
        import_filename: &str,
        once: bool,
        as_name: Option<&str>,
        mut ident_map: HashTrieMap<String, u32>,
    ) -> Result<HashTrieMap<String, u32>, ParserError> {
        let import_path = match PathBuf::from(self.filename.clone()).parent() {
            Some(parent) => parent.join(import_filename),
            None => PathBuf::from(import_filename),
        };
        let import_path_str = import_path.to_string_lossy().to_string();
        let (already_imported, new_imported) =
            self.check_imported_else_add(import_path.clone(), imported, line, 0)?;

        if !(once && already_imported) {
            let file = File::open(import_path.clone()).map_err(|err| {
                self.produce_error(
                    ParserErrorKind::CannotImportFile {
                        import_filename: import_path_str.clone(),
                        error_message: err.to_string(),
                    },
                    Some(line),
                    0,
                )
            })?;

            let mut parser = Parser {
                next_lines: crate::lexer::lines(file).peekable(),
                filename: import_path.to_string_lossy().to_string(),
                file_path: import_path,
                next_ident: self.next_ident,
                names: vec![],
            };
            let (scope, map) = parser.parse_scope(-1, false, new_imported, HashTrieMap::new())?;
            ident_map = map.into_iter().fold(ident_map, |acc, (key, value)| {
                match as_name {
                    // append "<as_name>." to the imported names
                    Some(name) => {
                        let mut name = name.to_owned();
                        name.push('.');
                        name.push_str(key);
                        acc.insert(name, *value)
                    },
                    None => acc.insert(key.to_string(), *value)
                }
            });
            self.next_ident = parser.next_ident;
            self.names.append(&mut parser.names);

            match scope {
                Scope::Impure {
                    lines: mut import_lines,
                } => lines.append(&mut import_lines),
                Scope::Pure { .. } => panic!("Unexpected Pure scope in import"),
            };
            return Ok(ident_map);
        }
        Ok(ident_map)
    }

    // parses the line and mutates the lines vector
    fn parse_line(
        &mut self,
        line: &Line,
        lines: &mut Vec<FunctionLine>,
        pure: bool,
        already_parsed_expr: &mut bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
    ) -> Result<HashTrieMap<String, u32>, ParserError> {
        match line.tokens.as_slice() {
            [Token::Import, Token::Str(string), Token::As, Token::Name(as_str)] => {
                self.add_import(imported, lines, line, string, false,Some(&as_str), ident_map)
            }
            [Token::Import, Token::Str(_), Token::As, ..] => {
                Err(self.produce_error(ParserErrorKind::AsNameExpected, Some(line), 3))
            }
            [Token::Import, Token::Once, Token::Str(string), Token::As, Token::Name(as_str)] => {
                self.add_import(imported, lines, line, string, true, Some(&as_str), ident_map)
            }
            [Token::Import, Token::Once, Token::Str(_), Token::As, ..] => {
                Err(self.produce_error(ParserErrorKind::AsNameExpected, Some(line), 4))
            }
            [Token::Import, Token::Str(string)] => {
                self.add_import(imported, lines, line, string, false, None, ident_map)
            }
            [Token::Import, Token::Once, Token::Str(string)] => {
                self.add_import(imported, lines, line, string, true, None, ident_map)
            }
            [Token::Import, Token::Once, ..] => {
                Err(self.produce_error(ParserErrorKind::FilenameStringExpected, Some(line), 2))
            }
            [Token::Import, ..] => {
                Err(self.produce_error(ParserErrorKind::FilenameStringExpected, Some(line), 1))
            }
            [Token::Name(string), Token::Assignment, ..] => {
                let mut iter = line.tokens.iter().enumerate().peekable();
                iter.next().unwrap();
                iter.next().unwrap();
                // if name shadowing then replace the name with the old identifier
                // search the position in the names vector, starting from the end, because usually
                // names that have been introduced not long ago are shadowed
                let ident = if let Some(ident) = ident_map.get(string) {
                    *ident
                } else {
                    self.new_ident(string)
                };

                let new_map = ident_map.insert(string.to_string(), ident);
                let expression =
                    self.parse_expression(&mut iter, line, pure, imported, new_map.clone())?;
                self.assert_no_more_tokens(&mut iter, line)?;
                if pure && *already_parsed_expr {
                    return Err(self.produce_error(
                        ParserErrorKind::ReturnExpressionError {
                            msg: String::from(
                                "Cannot have assignments after expression in a single pure scope!",
                            ),
                        },
                        Some(line),
                        2,
                    ));
                }
                lines.push(FunctionLine::Assignment(ident, Rc::new(expression)));
                Ok(new_map)
            }
            [_, Token::Assignment, ..] => Err(self.produce_error(
                ParserErrorKind::AssignmentError {
                    msg: String::from(
                        "The token on the left side of the assignment is not a name!",
                    ),
                },
                Some(line),
                0,
            )),
            [exp @ ..] => {
                let mut iter = exp.iter().enumerate().peekable();
                let expression =
                    self.parse_expression(&mut iter, line, pure, imported, ident_map.clone())?;
                self.assert_no_more_tokens(&mut iter, line)?;
                if pure && *already_parsed_expr {
                    return Err(self.produce_error(
                        ParserErrorKind::ReturnExpressionError {
                            msg: String::from("A pure scope can only have 1 expression!"),
                        },
                        Some(line),
                        0,
                    ));
                }
                *already_parsed_expr = true;
                lines.push(FunctionLine::Expression(Rc::new(expression)));
                Ok(ident_map)
            }
        }
    }

    // parses single expression without operators
    // the expression can be on multiple lines
    fn parse_single_expression(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        mut ident_map: HashTrieMap<String, u32>,
    ) -> Result<Expression, ParserError> {
        let (token_pos, token) = tokens.next().ok_or(self.produce_error(
            ParserErrorKind::ExpressionExpected,
            Some(line),
            line.tokens.len(),
        ))?;
        let expr = match token {
            Token::Name(string) => {
                if let Some(ident) = ident_map.get(string) {
                    if let Some((_, Token::LeftBracket)) = tokens.peek() {
                        tokens.next().unwrap();
                        let expr_vec = self.parse_args_list(
                            tokens,
                            line,
                            pure,
                            imported.clone(),
                            ident_map.clone(),
                        )?;
                        Expression::FunctionCall {
                            expr: Rc::new(Expression::Ident(*ident)),
                            args: expr_vec,
                        }
                    } else {
                        Expression::Ident(*ident)
                    }
                } else {
                    return Err(self.produce_error(
                        ParserErrorKind::UnknownNameError {
                            name: string.clone(),
                        },
                        Some(line),
                        token_pos,
                    ));
                }
            }
            Token::Number(string) => parse_number(string, line, token_pos, &self.filename)?,
            Token::Str(string) => Expression::Value(parse_string(string)),
            Token::Char(string) => {
                if string.chars().count() != 1 {
                    return Err(self.produce_error(
                        ParserErrorKind::CharParseError,
                        Some(line),
                        token_pos,
                    ));
                } else {
                    Expression::Value(Value::Char(string.chars().next().unwrap()))
                }
            }
            Token::True => Expression::Value(Value::Boolean(true)),
            Token::False => Expression::Value(Value::Boolean(false)),
            Token::Operation(Op::Minus) => {
                let expr = self.parse_single_expression(
                    tokens,
                    line,
                    pure,
                    imported.clone(),
                    ident_map.clone(),
                )?;
                Expression::UnaryOperation(UnaryOp::Minus, Rc::new(expr))
            }
            Token::Operation(Op::Negation) => {
                let expr = self.parse_single_expression(
                    tokens,
                    line,
                    pure,
                    imported.clone(),
                    ident_map.clone(),
                )?;
                Expression::UnaryOperation(UnaryOp::Negation, Rc::new(expr))
            }
            Token::Nil => Expression::Value(Value::Nil),
            Token::Cons => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    let (expr_1, expr_2) = self.parse_built_in_binary_function_call(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    Expression::Cons(Rc::new(expr_1), Rc::new(expr_2))
                } else {
                    new_binary_function!(self, Expression::Cons, true)
                }
            }
            Token::Left => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    Expression::Left(Rc::new(self.parse_built_in_unary_function_call(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?))
                } else {
                    new_unary_function!(self, Expression::Left, true)
                }
            }
            Token::Right => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    Expression::Right(Rc::new(self.parse_built_in_unary_function_call(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?))
                } else {
                    new_unary_function!(self, Expression::Right, true)
                }
            }
            Token::Empty => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    Expression::Empty(Rc::new(self.parse_built_in_unary_function_call(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?))
                } else {
                    new_unary_function!(self, Expression::Empty, true)
                }
            }
            Token::Print => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    Expression::PrintCall {
                        expr: Rc::new(self.parse_built_in_unary_function_call(
                            tokens,
                            line,
                            pure,
                            imported.clone(),
                            ident_map.clone(),
                        )?),
                        newline: false,
                    }
                } else {
                    new_print_function!(self, false)
                }
            }
            Token::Println => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    Expression::PrintCall {
                        expr: Rc::new(self.parse_built_in_unary_function_call(
                            tokens,
                            line,
                            pure,
                            imported.clone(),
                            ident_map.clone(),
                        )?),
                        newline: true,
                    }
                } else {
                    new_print_function!(self, true)
                }
            }
            Token::Read => {
                if let Some((_, Token::LeftBracket)) = tokens.peek() {
                    self.assert_next_token(
                        tokens,
                        Token::LeftBracket,
                        ParserErrorKind::LeftBracketExpected,
                        line,
                    )?;
                    self.assert_next_token(
                        tokens,
                        Token::RightBracket,
                        ParserErrorKind::ClosingBracketExpected,
                        line,
                    )?;
                    self.assert_no_more_tokens(tokens, line)?;
                    Expression::ReadCall
                } else {
                    let ident = self.new_ident("x");
                    Expression::Value(Value::Function {
                        params: vec![],
                        scope: Box::new(Scope::Impure {
                            lines: vec![
                                FunctionLine::Assignment(ident, Rc::new(Expression::ReadCall)),
                                FunctionLine::Expression(Rc::new(Expression::Ident(ident))),
                            ],
                        }),
                    })
                }
            }
            Token::Impure => {
                let mut params = Vec::new();
                if let Some((_, Token::LeftBoxBracket)) = tokens.peek() {
                    tokens.next().unwrap();
                    (params, ident_map) = self.parse_name_list(tokens, line, ident_map)?;
                }
                self.assert_next_token(tokens, Token::Arrow, ParserErrorKind::ArrowExpected, line)?;

                let scope;
                // check if the return expression is on the same line
                if let Some(_) = tokens.peek() {
                    let expr = self.parse_expression(
                        tokens,
                        line,
                        false,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    scope = new_scope_with_single_expr(expr, false);
                } else {
                    scope = {
                        let (scope, _) = self.parse_scope(
                            line.indentation.into(),
                            false,
                            imported.clone(),
                            ident_map.clone(),
                        )?;
                        Box::new(scope)
                    };
                }

                Expression::Value(Value::Function { params, scope })
            }
            Token::Arrow => {
                let scope;
                // check if the return expression is on the same line
                if let Some(_) = tokens.peek() {
                    let expr = self.parse_expression(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    scope = new_scope_with_single_expr(expr, true);
                } else {
                    scope = {
                        let (scope, _) = self.parse_scope(
                            line.indentation.into(),
                            true,
                            imported.clone(),
                            ident_map.clone(),
                        )?;
                        Box::new(scope)
                    };
                }
                Expression::Value(Value::Function {
                    params: vec![],
                    scope,
                })
            }
            Token::LeftBoxBracket => {
                let (params, ident_map) = self.parse_name_list(tokens, line, ident_map.clone())?;
                self.assert_next_token(tokens, Token::Arrow, ParserErrorKind::ArrowExpected, line)?;
                let scope;
                // check if the return expression is on the same line
                if let Some(_) = tokens.peek() {
                    let expr =
                        self.parse_expression(tokens, line, pure, imported.clone(), ident_map)?;
                    scope = new_scope_with_single_expr(expr, true);
                } else {
                    scope = {
                        let (scope, _) = self.parse_scope(
                            line.indentation.into(),
                            true,
                            imported.clone(),
                            ident_map,
                        )?;
                        Box::new(scope)
                    };
                }

                Expression::Value(Value::Function { params, scope })
            }

            Token::If => {
                let condition = Rc::new(self.parse_expression(
                    tokens,
                    line,
                    pure,
                    imported.clone(),
                    ident_map.clone(),
                )?);
                self.assert_next_token(tokens, Token::Then, ParserErrorKind::ThenExpected, line)?;
                let then_scope;
                let else_scope;
                // check for 'then expression' on the same line
                if let Some(_) = tokens.peek() {
                    let expr = self.parse_expression(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    then_scope = new_scope_with_single_expr(expr, pure);
                    // if yes, check for 'else expression' on the same line and return
                    if let Some(_) = tokens.peek() {
                        self.assert_next_token(
                            tokens,
                            Token::Else,
                            ParserErrorKind::ElseExpected,
                            line,
                        )?;
                        let expr =
                            self.parse_expression(tokens, line, pure, imported, ident_map)?;
                        else_scope = new_scope_with_single_expr(expr, pure);

                        return Ok(Expression::If {
                            condition,
                            then_scope,
                            else_scope,
                        });
                    }
                } else {
                    // if 'then expression' is not on the same line, parse scope
                    then_scope = {
                        let (scope, _) = self.parse_scope(
                            line.indentation.into(),
                            pure,
                            imported.clone(),
                            ident_map.clone(),
                        )?;
                        Box::new(scope)
                    }
                }
                // take next line and scan it for else
                let next_line = self
                    .next_lines
                    .next()
                    .ok_or(self.produce_error(ParserErrorKind::UnexpectedEOF, None, 0))?
                    .map_err(|_| self.produce_error(ParserErrorKind::LexerError, None, 0))?;
                if next_line.indentation != line.indentation {
                    return Err(self.produce_error(
                        ParserErrorKind::IndentationError {
                            msg: String::from("Indentation of else should be the same as if"),
                            expected: line.indentation as i32,
                            actual: next_line.indentation as i32,
                        },
                        Some(&next_line),
                        0,
                    ));
                }
                let mut next_line_tokens = next_line
                    .tokens
                    .as_slice()
                    .into_iter()
                    .enumerate()
                    .peekable();
                self.assert_next_token(
                    &mut next_line_tokens,
                    Token::Else,
                    ParserErrorKind::ElseExpected,
                    &next_line,
                )?;

                // check if 'else expression' is on the same line
                let else_scope;
                if let Some(_) = next_line_tokens.peek() {
                    let expr = self.parse_expression(
                        &mut next_line_tokens,
                        &next_line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    else_scope = new_scope_with_single_expr(expr, pure);
                } else {
                    // if not on the same line then parse scope
                    else_scope = {
                        let (scope, _) = self.parse_scope(
                            line.indentation.into(),
                            pure,
                            imported.clone(),
                            ident_map.clone(),
                        )?;
                        Box::new(scope)
                    }
                }

                Expression::If {
                    condition,
                    then_scope,
                    else_scope,
                }
            }
            Token::LeftBracket => {
                let expr =
                    self.parse_expression(tokens, line, pure, imported.clone(), ident_map.clone())?;
                self.assert_next_token(
                    tokens,
                    Token::RightBracket,
                    ParserErrorKind::ClosingBracketExpected,
                    line,
                )?;
                expr
            }
            token => {
                return Err(self.produce_error(
                    ParserErrorKind::UnexpectedToken {
                        token: token.clone(),
                    },
                    Some(line),
                    token_pos,
                ))
            }
        };

        // check if the expression is called as a function
        match tokens.peek() {
            Some((_, Token::LeftBracket)) => {
                tokens.next().unwrap();
                let expr_vec = self.parse_args_list(tokens, line, pure, imported, ident_map)?;
                Ok(Expression::FunctionCall {
                    expr: Rc::new(expr),
                    args: expr_vec,
                })
            }
            _ => Ok(expr),
        }
    }

    // Parses expressions with operator precedence
    // pseudo code from wikipedia: https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_expression_with_precedence(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
        mut left: Expression,
        min_precedence: u8,
    ) -> Result<Expression, ParserError> {
        let mut lookahead = tokens.peek();
        loop {
            match lookahead {
                Some((_, Token::Operation(op))) if op.precedence() >= min_precedence => {
                    tokens.next().unwrap();
                    let mut right = self.parse_single_expression(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    lookahead = tokens.peek();

                    let mut op_token_pos: usize = 0;
                    loop {
                        match lookahead {
                            Some((token_pos, Token::Operation(op_2)))
                                if op_2.precedence() > op.precedence() =>
                            {
                                op_token_pos = *token_pos;
                                right = self.parse_expression_with_precedence(
                                    tokens,
                                    line,
                                    pure,
                                    imported.clone(),
                                    ident_map.clone(),
                                    right,
                                    op.precedence() + 1,
                                )?;
                                lookahead = tokens.peek();
                            }
                            _ => break,
                        }
                    }
                    left = parse_binary_operation(
                        op,
                        left,
                        right,
                        line,
                        op_token_pos,
                        &self.filename,
                    )?;
                }
                _ => break,
            }
        }

        return Ok(left);
    }

    // parses expression with operators
    fn parse_expression(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
    ) -> Result<Expression, ParserError> {
        let expr =
            self.parse_single_expression(tokens, line, pure, imported.clone(), ident_map.clone())?;
        self.parse_expression_with_precedence(tokens, line, pure, imported, ident_map, expr, 0)
    }

    // parses "name(arg)" function call
    fn parse_built_in_unary_function_call(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
    ) -> Result<Expression, ParserError> {
        self.assert_next_token(
            tokens,
            Token::LeftBracket,
            ParserErrorKind::LeftBracketExpected,
            line,
        )?;
        let expr = self.parse_expression(tokens, line, pure, imported, ident_map)?;
        self.assert_next_token(
            tokens,
            Token::RightBracket,
            ParserErrorKind::ClosingBracketExpected,
            line,
        )?;
        Ok(expr)
    }

    // parses "name(arg, arg)" function call
    fn parse_built_in_binary_function_call(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
    ) -> Result<(Expression, Expression), ParserError> {
        self.assert_next_token(
            tokens,
            Token::LeftBracket,
            ParserErrorKind::LeftBracketExpected,
            line,
        )?;
        let expr_1 =
            self.parse_expression(tokens, line, pure, imported.clone(), ident_map.clone())?;
        self.assert_next_token(tokens, Token::Comma, ParserErrorKind::CommaExpected, line)?;
        let expr_2 = self.parse_expression(tokens, line, pure, imported, ident_map)?;
        self.assert_next_token(
            tokens,
            Token::RightBracket,
            ParserErrorKind::ClosingBracketExpected,
            line,
        )?;
        Ok((expr_1, expr_2))
    }

    // parses arbitrary list of args, ending with right bracket
    fn parse_args_list(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        pure: bool,
        imported: List<PathBuf>,
        ident_map: HashTrieMap<String, u32>,
    ) -> Result<Vec<Rc<Expression>>, ParserError> {
        let mut vec = vec![];

        loop {
            let (_, token) = tokens.peek().ok_or(self.produce_error(
                ParserErrorKind::UnexpectedEndOfLine,
                Some(line),
                line.tokens.len(),
            ))?;

            match token {
                Token::RightBracket => {
                    tokens.next().unwrap();
                    return Ok(vec);
                }
                _ => {
                    let expr = self.parse_expression(
                        tokens,
                        line,
                        pure,
                        imported.clone(),
                        ident_map.clone(),
                    )?;
                    vec.push(Rc::new(expr));
                    match tokens.peek() {
                        Some((_, Token::Comma)) => {
                            tokens.next().unwrap();
                        }
                        Some((_, Token::RightBracket)) => (),
                        Some((token_pos, _)) => {
                            return Err(self.produce_error(
                                ParserErrorKind::CommaExpected,
                                Some(line),
                                *token_pos,
                            ))
                        }
                        _ => {
                            return Err(self.produce_error(
                                ParserErrorKind::UnexpectedEndOfLine,
                                Some(line),
                                line.tokens.len(),
                            ))
                        }
                    };
                }
            };
        }
    }
    // parses arbitrary number of parameters, ending in right box bracket
    fn parse_name_list(
        &mut self,
        tokens: &mut Peekable<Enumerate<Iter<Token>>>,
        line: &Line,
        mut ident_map: HashTrieMap<String, u32>,
    ) -> Result<(Vec<u32>, HashTrieMap<String, u32>), ParserError> {
        let mut vec = vec![];
        loop {
            let (token_pos, token) = tokens.next().ok_or(self.produce_error(
                ParserErrorKind::UnexpectedEndOfLine,
                Some(line),
                line.tokens.len(),
            ))?;
            match token {
                Token::RightBoxBracket => return Ok((vec, ident_map)),
                Token::Name(string) => {
                    let ident = self.new_ident(string);
                    ident_map = ident_map.insert(string.to_string(), ident);
                    vec.push(ident);
                    match tokens.peek() {
                        Some((_, Token::Comma)) => {
                            tokens.next().unwrap();
                        }
                        Some((_, Token::RightBoxBracket)) => (),
                        _ => {
                            return Err(self.produce_error(
                                ParserErrorKind::CommaExpected,
                                Some(line),
                                token_pos + 1,
                            ))
                        }
                    };
                }
                _ => {
                    return Err(self.produce_error(
                        ParserErrorKind::NameExpected,
                        Some(line),
                        token_pos,
                    ))
                }
            };
        }
    }
}

fn new_scope_with_single_expr(expr: Expression, pure: bool) -> Box<Scope> {
    if pure {
        Box::new(Scope::Pure {
            assignments: vec![],
            expression: Rc::new(expr),
        })
    } else {
        Box::new(Scope::Impure {
            lines: vec![FunctionLine::Expression(Rc::new(expr))],
        })
    }
}

fn parse_binary_operation(
    op: &Op,
    expr_1: Expression,
    expr_2: Expression,
    line: &Line,
    op_token_pos: usize,
    filename: &str,
) -> Result<Expression, ParserError> {
    Ok(match op {
        Op::And => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::And), expr_1, expr_2),
        Op::Or => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::Or), expr_1, expr_2),
        Op::Xor => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::Xor), expr_1, expr_2),
        Op::Plus => new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Plus), expr_1, expr_2),
        Op::Minus => new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Minus), expr_1, expr_2),
        Op::Division => {
            new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Division), expr_1, expr_2)
        }
        Op::IntDivision => {
            new_binary_operation!(
                BinaryOp::Arithmetic(ArithBinOp::IntDivision),
                expr_1,
                expr_2
            )
        }
        Op::Multiplication => new_binary_operation!(
            BinaryOp::Arithmetic(ArithBinOp::Multiplication),
            expr_1,
            expr_2
        ),
        Op::Exponentiation => new_binary_operation!(
            BinaryOp::Arithmetic(ArithBinOp::Exponentiation),
            expr_1,
            expr_2
        ),
        Op::Modulo => {
            new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Modulo), expr_1, expr_2)
        }
        Op::Eq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Eq), expr_1, expr_2),
        Op::NEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::NEq), expr_1, expr_2),
        Op::Lt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Lt), expr_1, expr_2),
        Op::Gt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Gt), expr_1, expr_2),
        Op::LEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::LEq), expr_1, expr_2),
        Op::GEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::GEq), expr_1, expr_2),
        Op::Negation => {
            return Err(ParserError {
                kind: ParserErrorKind::OpError,
                line: Some(line.to_owned()),
                token_pos: op_token_pos,
                filename: filename.to_string(),
            })
        }
    })
}

// represents string as list of characters
pub fn parse_string(string: &str) -> Value {
    string.chars().rev().fold(Value::Nil, |acc, x| {
        Value::Tuple(Box::new(Value::Char(x)), Box::new(acc))
    })
}

fn parse_number(
    string: &str,
    line: &Line,
    token_pos: usize,
    filename: &str,
) -> Result<Expression, ParserError> {
    if let Some(integer) = string.parse::<i32>().ok() {
        Ok(Expression::Value(Value::Number(Number::Integer(integer))))
    } else if let Some(float) = string.parse::<f64>().ok() {
        Ok(Expression::Value(Value::Number(Number::Float(float))))
    } else {
        Err(ParserError {
            kind: ParserErrorKind::NumberParseError,
            filename: filename.to_string(),
            line: Some(line.to_owned()),
            token_pos,
        })
    }
}
