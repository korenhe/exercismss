#[derive (Debug, Clone)]
enum Operator {
    Add,
    Minus,
    Multiple,
    Divide,
    Exp,
    Unknown,
}

#[derive (Debug)]
enum Op {
    Number(i32),
    Oper(Operator),
}

fn parse_ordinal(s: &str) -> Option<i32> {
    let s = s
        .strip_suffix("st")
        .or_else(|| s.strip_suffix("nd"))
        .or_else(|| s.strip_suffix("rd"))
        .or_else(|| s.strip_suffix("th"))
        .unwrap_or(s);

    s.parse::<i32>().ok()
}

pub fn answer(command: &str) -> Option<i32> {
    println!("ops={:?}", command.split(&['?', ' ']).filter(|x| !x.is_empty()).collect::<Vec<_>>());
    let ops = command
        .split(&['?', ' '])
        .filter(|x| !x.is_empty())
        .filter_map(|word| {
            // Try parse number
            if let Some(n) = parse_ordinal(word) {
                return Some(Op::Number(n));
            }

            match word.to_lowercase().as_str() {
                "plus" => Some(Op::Oper(Operator::Add)),
                "minus" => Some(Op::Oper(Operator::Minus)),
                "multiplied" => Some(Op::Oper(Operator::Multiple)),
                "divided" => Some(Op::Oper(Operator::Divide)),
                "raised" => Some(Op::Oper(Operator::Exp)),
                "what" => None,
                "is" => None,
                "by" => None,
                "to" => None,
                "the" => None,
                "power" => None,
                _ => Some(Op::Oper(Operator::Unknown))
            }
        })
        .collect::<Vec<Op>>();

    println!("ops={:?}", ops);
    let mut value: Option<i32> = None;
    let mut lastop: Option<Operator> = None;
    for op in ops {
        match op {
            Op::Number(n) => {
                match (lastop.clone(), value) {
                    (Some(ll),Some(vv)) => {
                        match ll {
                            Operator::Add => value = Some(vv + n),
                            Operator::Minus => value = Some(vv - n),
                            Operator::Multiple => value = Some(vv * n),
                            Operator::Divide => value = Some(vv / n),
                            Operator::Exp => value = Some(vv.pow(n.try_into().unwrap())),
                            Operator::Unknown => (),
                        }
                    }
                    (None, None) => value = Some(n),
                    _ => value= None,
                };
                lastop = None;
            },
            Op::Oper(o) => {
                if lastop.is_some() {
                    return None;
                }
                if value.is_some() {
                    lastop = Some(o.clone());
                }
            },
        }
    }

    if lastop.is_none() {
        value
    } else {
        None
    }
}
