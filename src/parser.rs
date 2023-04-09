use crate::problem::*;
use std::collections::HashMap;

pub fn parse_lp_file(lp: &str) -> Problem {
    let tokens = tokenize_lp_file(lp);

    let mut q = tokens.iter().peekable();
    let mut variable_map: HashMap<String, usize> = HashMap::new();
    let mut is_maximize = None;
    let mut objective_name = None;
    let mut objective = None;
    let mut constraints = None;

    // NOTE: bounds, general, binaryは未実装
    while let Some(tok) = q.next() {
        match tok {
            Token::Keyword { val } => match *val {
                Keyword::Maximize | Keyword::Minimize => {
                    is_maximize = Some(*val == Keyword::Maximize);
                    let (obj_name, obj) = parse_objective(&mut q, &mut variable_map);
                    objective_name = Some(obj_name);
                    objective = Some(obj);
                }
                Keyword::SubjectTo => {
                    constraints = Some(parse_constraints(&mut q, &mut variable_map));
                }
                Keyword::End => break,
                _ => panic!("unexpected keyword {}", val.to_str()),
            },
            _ => panic!("unexpected token: {:?}", tok),
        }
    }

    let mut variables = vec![];
    let mut var_names: Vec<(&String, &usize)> = variable_map.iter().collect();
    var_names.sort_by(|a, b| a.1.cmp(b.1));
    for (var_name, _) in var_names {
        variables.push(Variable {
            name: var_name.to_owned(),
            var_type: VariableType::Real,
            is_free: false,
        });
    }

    Problem {
        is_maximize: is_maximize.expect("could not parse problem objective"),
        objective_name: objective_name.expect("could not parse objective name"),
        variables,
        objective: objective.expect("could not parse objective"),
        constraints: constraints.expect("could not parse constraints"),
    }
}

fn tokenize_lp_file(lp: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut q = lp.split_whitespace().peekable();

    while let Some(s) = q.next() {
        // キーワードの検知
        if let Some(keyword) = Keyword::matched_keyword(s) {
            tokens.push(Token::Keyword { val: keyword });
            continue;
        }

        // 名前の検知
        if s.ends_with(":") {
            tokens.push(Token::Name {
                val: s[0..s.len() - 1].to_owned(),
            });
            continue;
        }
        // waiting for if-let chain...
        if q.peek().is_some() && q.peek().unwrap() == &":" {
            tokens.push(Token::Name { val: s.to_owned() });
            q.next();
            continue;
        }

        // NOTE: 1xのように、スペースで区切られていない場合は対応していない
        let starts_with_digit = s.chars().next().unwrap().is_digit(10);
        if starts_with_digit {
            // 定数をパースする
            tokens.push(Token::Const {
                val: s.parse().unwrap(),
            });
        } else {
            // 変数をパースする
            tokens.push(Token::Variable { name: s.to_owned() });
        }
    }

    tokens
}

fn parse_constraints(
    q: &mut std::iter::Peekable<core::slice::Iter<Token>>,
    variable_map: &mut HashMap<String, usize>,
) -> Vec<Constraint> {
    let mut constraints = vec![];
    while let Some(tok) = q.peek() {
        match tok {
            Token::Keyword { val: _ } => {
                return constraints;
            }
            _ => {
                let c = parse_constraint(q, variable_map, &format!("c{}", constraints.len()));
                constraints.push(c);
            }
        }
    }
    constraints
}

fn parse_name(q: &mut std::iter::Peekable<core::slice::Iter<Token>>, default_name: &str) -> String {
    match q.peek() {
        Some(Token::Name { val }) => {
            q.next();
            val.to_owned()
        }
        _ => default_name.to_owned(),
    }
}

fn parse_constant(q: &mut std::iter::Peekable<core::slice::Iter<Token>>) -> Option<f64> {
    if let Some(Token::Const { val }) = q.peek() {
        q.next();
        Some(*val)
    } else {
        None
    }
}

fn parse_variable(
    q: &mut std::iter::Peekable<core::slice::Iter<Token>>,
    variable_map: &mut HashMap<String, usize>,
) -> usize {
    let Some(Token::Variable { name: var_name }) = q.next() else {
        panic!("expected variable name");
    };
    return if variable_map.contains_key(var_name) {
        variable_map[var_name]
    } else {
        let idx = variable_map.len();
        variable_map.insert(var_name.clone(), idx);
        idx
    };
}

fn parse_expr(
    q: &mut std::iter::Peekable<core::slice::Iter<Token>>,
    variable_map: &mut HashMap<String, usize>,
) -> Vec<Term> {
    let mut expr = vec![];
    while let Some(tok) = q.peek() {
        match tok {
            Token::Keyword { val } => {
                match val {
                    Keyword::Minus | Keyword::Plus => {
                        let sign = if *val == Keyword::Minus { -1. } else { 1. };
                        q.next();
                        let coef = parse_constant(q).unwrap_or(1.) * sign;
                        let var_idx = parse_variable(q, variable_map);
                        expr.push(Term { var_idx, coef });
                    }
                    _ => {
                        return expr;
                    }
                };
            }
            Token::Variable { name: _ } => {
                // 符号なし、係数なしの項は式の最初しか認めない
                if expr.len() != 0 {
                    panic!("unexpected token: {:?}", tok);
                }
                let var_idx = parse_variable(q, variable_map);
                expr.push(Term { var_idx, coef: 1. })
            }
            Token::Const { val: coef } => {
                // 符号なしの項は式の最初しか認めない
                if expr.len() != 0 {
                    panic!("unexpected token: {:?}", tok);
                }
                q.next();
                let var_idx = parse_variable(q, variable_map);
                expr.push(Term {
                    var_idx,
                    coef: *coef,
                })
            }
            _ => {
                panic!("unexpected token: {:?}", tok);
            }
        }
    }
    expr
}

fn parse_objective(
    q: &mut std::iter::Peekable<core::slice::Iter<Token>>,
    variable_map: &mut HashMap<String, usize>,
) -> (String, Vec<Term>) {
    let obj_name = parse_name(q, "obj");
    let expr = parse_expr(q, variable_map);
    (obj_name, expr)
}

fn parse_constraint(
    q: &mut std::iter::Peekable<core::slice::Iter<Token>>,
    variable_map: &mut HashMap<String, usize>,
    default_name: &str,
) -> Constraint {
    let c_name = parse_name(q, default_name);
    let expr = parse_expr(q, variable_map);
    let cmp = match q.next().unwrap() {
        Token::Keyword { val } => match val {
            Keyword::GreaterEqual | Keyword::Equal | Keyword::LessEqual => {
                Compare::from_keyword(val)
            }
            _ => {
                panic!("unexpected keyword: {:?}", val);
            }
        },
        _ => {
            panic!("unexpected token");
        }
    };
    let rhs = parse_constant(q).expect("expected constant");

    Constraint {
        name: c_name,
        expr,
        cmp,
        rhs,
    }
}

#[derive(Debug)]
enum Token {
    // NOTE: TokenとKeywordを一緒にしたら楽じゃない?
    Const { val: f64 },
    Variable { name: String },
    Name { val: String },
    Keyword { val: Keyword },
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Keyword {
    Maximize,
    Minimize,
    SubjectTo,
    Bounds,
    Free,
    General,
    Binary,
    End,
    LessEqual,
    Equal,
    GreaterEqual,
    Plus,
    Minus,
}

impl Keyword {
    fn to_str(&self) -> &str {
        match self {
            Keyword::Maximize => "maximize",
            Keyword::Minimize => "minimize",
            Keyword::SubjectTo => "st",
            Keyword::Bounds => "bounds",
            Keyword::Free => "free",
            Keyword::General => "general",
            Keyword::Binary => "binary",
            Keyword::End => "end",
            Keyword::LessEqual => "<=",
            Keyword::Equal => "=",
            Keyword::GreaterEqual => ">=",
            Keyword::Plus => "+",
            Keyword::Minus => "-",
        }
    }

    fn is_same(&self, s: &str) -> bool {
        match self {
            _ => s == self.to_str(),
        }
    }

    fn matched_keyword(s: &str) -> Option<Keyword> {
        for keyword in Keyword::all() {
            if keyword.is_same(s) {
                return Some(keyword);
            }
        }
        None
    }

    fn all() -> impl Iterator<Item = Keyword> {
        [
            Keyword::Maximize,
            Keyword::Minimize,
            Keyword::SubjectTo,
            Keyword::Bounds,
            Keyword::Free,
            Keyword::General,
            Keyword::Binary,
            Keyword::End,
            Keyword::LessEqual,
            Keyword::Equal,
            Keyword::GreaterEqual,
            Keyword::Plus,
            Keyword::Minus,
        ]
        .iter()
        .copied()
    }
}

impl Compare {
    fn from_keyword(keyword: &Keyword) -> Compare {
        match keyword {
            Keyword::LessEqual => Compare::LessEqual,
            Keyword::Equal => Compare::Equal,
            Keyword::GreaterEqual => Compare::GreaterEqual,
            _ => panic!("unexpected keyword: {:?}", keyword),
        }
    }
}
