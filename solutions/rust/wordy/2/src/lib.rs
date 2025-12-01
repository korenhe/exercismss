use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0},
    combinator::{map, opt},
    sequence::{preceded},
    IResult, Parser,
};

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Minus,
    Multiply,
    Divide,
    Exp,
}

#[derive(Debug)]
enum Token {
    Number(i32),
    Oper(Operator),
}

fn parse_int(input: &str) -> IResult<&str, i32> {
    // Parse an optional minus sign
    let (input, sign) = opt(char('-')).parse(input)?;

    // Parse one or more digits
    let (input, digits) = digit1(input)?;

    // Convert the digits to an integer
    let mut num: i32 = digits.parse().unwrap(); // In a real application, handle Result properly

    // Apply the sign if present
    if sign.is_some() {
        num = -num;
    }

    Ok((input, num))
}

// parse ordinal like 1st, 2nd, 3rd, 4th
fn parse_ordinal(input: &str) -> IResult<&str, i32> {
    let (rest, n) = parse_int(input)?;
    let suffixes = ["st", "nd", "rd", "th"];
    let mut rest_trimmed = rest;
    for suf in &suffixes {
        if let Some(remain) = rest.strip_prefix(suf) {
            rest_trimmed = remain;
            break;
        }
    }
    Ok((rest_trimmed, n))
}

// parse a number token
fn number(input: &str) -> IResult<&str, Vec<Token>> {
    map(alt((parse_ordinal, parse_int)), |n| vec![Token::Number(n)]).parse(input)
}

fn parse_exp(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, _) = tag("raised")(input)?;

    // now match "to the"
    let (input, _) = preceded(multispace0, tag("to the")).parse(input)?;

    // parse the exponent number
    let (input, mut n_tok) = preceded(multispace0, number).parse(input)?;

    // requires "power"
    let (input, _) = preceded(multispace0, tag("power")).parse(input)?;

    // Return *only one* operator token here.
    let mut tokes = vec![Token::Oper(Operator::Exp)];
    tokes.append(&mut n_tok);

    Ok((input, tokes))
}

// parse operators
fn operator(input: &str) -> IResult<&str, Vec<Token>> {
    let add = map(tag("plus"), |_| vec![Token::Oper(Operator::Add)]);
    let sub = map(tag("minus"), |_| vec![Token::Oper(Operator::Minus)]);
    let mul = map(tag("multiplied by"), |_| vec![Token::Oper(Operator::Multiply)]);
    let div = map(tag("divided by"), |_| vec![Token::Oper(Operator::Divide)]);

    alt((add, sub, mul, div, parse_exp)).parse(input)
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    let mut all_tokens = Vec::new();
    let mut rest = input;

    while !rest.is_empty() {
        // Skip leading whitespace
        let (i, _) = multispace0(rest)?;
        rest = i;

        // Try number or operator
        match alt((number, operator)).parse(rest) {
            Ok((i, mut toks)) => {
                all_tokens.append(&mut toks);
                rest = i;
            }
            Err(_) => {
                // Unknown token found
                return Err(nom::Err::Error(nom::error::Error::new(
                    rest,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }
    }

    Ok((rest, all_tokens))
}

pub fn answer(command: &str) -> Option<i32> {
    let mut command = command.trim_start_matches("What is");
    command = command.trim_end_matches("?");

    let (_, toks) = tokens(command).ok()?;

    let mut value: Option<i32> = None;
    let mut lastop: Option<Operator> = None;

    for t in toks {
        match t {
            Token::Number(n) => {
                match (lastop.clone(), value) {
                    (Some(op), Some(v)) => {
                        value = Some(match op {
                            Operator::Add => v + n,
                            Operator::Minus => v - n,
                            Operator::Multiply => v * n,
                            Operator::Divide => v / n,
                            Operator::Exp => v.pow(n.try_into().unwrap()),
                        });
                    }
                    (None, None) => value = Some(n),
                    _ => return None,
                }
                lastop = None;
            }
            Token::Oper(op) => {
                if lastop.is_some() { return None; }
                lastop = Some(op);
            }
        }
    }

    if lastop.is_none() { value } else { None }

}
