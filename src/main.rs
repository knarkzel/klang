#![feature(let_chains)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, digit1, multispace0, multispace1},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::{collections::HashMap, fmt::Display};

// Helpers
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

// Ast
#[derive(Debug, Clone)]
enum Type {
    Void,
    Integer,
    Array(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Integer => write!(f, "int"),
            Type::Array(item) => write!(f, "[]{item}"),
        }
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Assignment,
    PlusEquals,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Assignment => write!(f, "="),
            Operator::PlusEquals => write!(f, "+="),
        }
    }
}

#[derive(Debug, Clone)]
enum Ast {
    // Low level
    Type(Type),
    Number(i64),
    Name(String),
    String(String),
    Operator(Operator),

    // High level
    Return(Box<Ast>),
    List(Vec<Ast>),
    Statement {
        name: Box<Ast>,
        operator: Box<Ast>,
        value: Box<Ast>,
    },
    Call {
        name: Box<Ast>,
        args: Vec<Ast>,
    },
    Function {
        name: Box<Ast>,
        args: Vec<(Ast, Ast)>,
        returns: Box<Ast>,
        body: Vec<Ast>,
    },
    For {
        name: Box<Ast>,
        variable: Box<Ast>,
        body: Vec<Ast>,
    },
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Type(item) => write!(f, "{item}"),
            Ast::Number(number) => write!(f, "{number}"),
            Ast::Name(name) => write!(f, "{name}"),
            Ast::String(string) => write!(f, "\"{string}\""),
            Ast::Operator(item) => write!(f, "{item}"),
            Ast::Return(item) => write!(f, "return {item}"),
            Ast::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    write!(f, "{item},")?;
                    if i < items.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            },
            Ast::Statement { name, operator, value } => write!(f, "{name} {operator} {value}"),
            Ast::Call { name, args } => {
                write!(f, "{name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{arg},")?;
                    if i < args.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            },
            Ast::Function { name, args, returns, body } => todo!(),
            Ast::For { name, variable, body } => todo!(),
        }
    }
}

// Low level parsers
fn parse_number(input: &str) -> IResult<&str, Ast> {
    map(digit1, |number: &str| {
        Ast::Number(number.parse::<i64>().expect("Invalid number"))
    })(input)
}

fn parse_name(input: &str) -> IResult<&str, Ast> {
    map(alpha1, |name: &str| Ast::Name(name.to_string()))(input)
}

fn parse_string(input: &str) -> IResult<&str, Ast> {
    let parser = delimited(tag("\""), take_until("\""), tag("\""));
    map(parser, |string: &str| Ast::String(string.to_string()))(input)
}

fn parse_int(input: &str) -> IResult<&str, Type> {
    map(tag("int"), |_| Type::Integer)(input)
}

fn parse_array(input: &str) -> IResult<&str, Type> {
    map(preceded(tag("[]"), alt((parse_int, parse_array))), |kind| {
        Type::Array(Box::new(kind))
    })(input)
}

fn parse_type(input: &str) -> IResult<&str, Ast> {
    let parser = alt((parse_int, parse_array));
    map(parser, |kind| Ast::Type(kind))(input)
}

fn parse_operator(input: &str) -> IResult<&str, Ast> {
    let parser = alt((
        map(tag("="), |_| Operator::Assignment),
        map(tag("+="), |_| Operator::PlusEquals),
    ));
    map(parser, |operator| Ast::Operator(operator))(input)
}

fn parse_body(input: &str) -> IResult<&str, Vec<Ast>> {
    delimited(tag("{"), parse_ast, tag("}"))(input)
}

// High level parsers
fn parse_value(input: &str) -> IResult<&str, Ast> {
    alt((
        parse_call,
        parse_list,
        parse_string,
        parse_name,
        parse_number,
    ))(input)
}

fn parse_list(input: &str) -> IResult<&str, Ast> {
    let parser = delimited(
        tag("["),
        separated_list0(ws(tag(",")), parse_value),
        tag("]"),
    );
    map(parser, |items| Ast::List(items))(input)
}

fn parse_return(input: &str) -> IResult<&str, Ast> {
    let parser = preceded(tuple((tag("return"), multispace1)), parse_value);
    map(parser, |name| Ast::Return(Box::new(name)))(input)
}

fn parse_statement(input: &str) -> IResult<&str, Ast> {
    let parser = tuple((parse_name, ws(parse_operator), parse_value));
    map(parser, |it| Ast::Statement {
        name: Box::new(it.0),
        operator: Box::new(it.1),
        value: Box::new(it.2),
    })(input)
}

fn parse_call(input: &str) -> IResult<&str, Ast> {
    let parse_args = separated_list0(ws(tag(",")), parse_value);
    let parser = tuple((
        terminated(parse_name, tag("(")),
        terminated(parse_args, tag(")")),
    ));
    map(parser, |it| Ast::Call {
        name: Box::new(it.0),
        args: it.1,
    })(input)
}

fn parse_function(input: &str) -> IResult<&str, Ast> {
    let parse_args = separated_list0(ws(tag(",")), tuple((ws(parse_name), parse_type)));
    let parser = tuple((
        delimited(tag("fn"), ws(parse_name), tag("(")),
        ws(terminated(parse_args, tag(")"))),
        opt(ws(parse_type)),
        parse_body,
    ));
    map(parser, |it| Ast::Function {
        name: Box::new(it.0),
        args: it.1,
        returns: Box::new(it.2.unwrap_or(Ast::Type(Type::Void))),
        body: it.3,
    })(input)
}

fn parse_for(input: &str) -> IResult<&str, Ast> {
    let parser = tuple((
        preceded(tag("for"), ws(parse_name)),
        preceded(tag("in"), ws(parse_name)),
        parse_body,
    ));
    map(parser, |it| Ast::For {
        name: Box::new(it.0),
        variable: Box::new(it.1),
        body: it.2,
    })(input)
}

fn parse_ast(input: &str) -> IResult<&str, Vec<Ast>> {
    many0(ws(alt((
        parse_for,
        parse_function,
        parse_return,
        parse_statement,
        parse_call,
    ))))(input)
}

// Interpreter
use anyhow::{bail, Error};
use fehler::throws;

#[throws]
fn void() -> Ast {
    Ast::Type(Type::Void)
}

#[derive(Debug)]
struct Interpreter {
    context: HashMap<String, Ast>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            context: HashMap::new(),
        }
    }

    #[throws]
    fn eval(&mut self, ast: Ast) -> Ast {
        match ast {
            Ast::Type(_)
            | Ast::Number(_)
            | Ast::String(_)
            | Ast::Operator(_)
            | Ast::List(_)
            | Ast::Return(_) => ast,
            Ast::Name(name) => {
                if let Some(ast) = self.context.get(&name) {
                    ast.clone()
                } else {
                    bail!("Invalid identifier: {name}")
                }
            }
            Ast::Statement {
                name,
                operator,
                value,
            } => {
                let value = self.eval(*value)?;
                match (*name, *operator) {
                    (Ast::Name(name), Ast::Operator(Operator::Assignment)) => {
                        self.context.insert(name, value);
                    },
                    (Ast::Name(name), Ast::Operator(Operator::PlusEquals)) => {
                        if let Some(Ast::Number(lhs)) = self.context.get_mut(&name) && let Ast::Number(rhs) = value {
                            *lhs += rhs;
                        }
                    }
                    _ => bail!("Failed to match `statement`"),
                }
                void()?
            }
            Ast::Call { name, args } => {
                match *name {
                    Ast::Name(name) if name.as_str() == "print" => {
                        for arg in args {
                            let value = self.eval(arg)?;
                            println!("{value}");
                        }
                    }
                    _ => bail!("Failed to match `call`"),
                }
                void()?
            }
            Ast::Function {
                name,
                args,
                returns,
                body,
            } => todo!(),
            Ast::For {
                name,
                variable,
                body,
            } => todo!(),
        }
    }
}

#[throws]
fn main() {
    let input = include_str!("../input.k");
    let (_, ast) = parse_ast(input)?;
    let mut interpreter = Interpreter::new();
    for item in ast {
        interpreter.eval(item)?;
    }
}
