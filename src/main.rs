use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, digit1, multispace0, multispace1},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

// Helpers
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

// Ast
#[derive(Debug)]
enum Type {
    Void,
    Integer,
    Array(Box<Type>),
}

#[derive(Debug)]
enum Operator {
    Assignment,
    PlusEquals,
}

#[derive(Debug)]
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
    map(preceded(tag("[]"), parse_int), |kind| {
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
    alt((parse_call, parse_list, parse_string, parse_name, parse_number))(input)
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

fn main() {
    let input = include_str!("../input.k");
    let ast = parse_ast(input);
    println!("{ast:#?}");
}
