use std::collections::HashMap;

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

#[derive(Clone, Copy, Debug)]
struct Term {
    var_idx: usize,
    coef: f64,
}

impl Term {
    fn to_str(&self, is_first: bool, variable_names: &Vec<Variable>) -> String {
        let sign = if self.coef < 0. {
            "- "
        } else if !is_first {
            "+ "
        } else {
            ""
        };
        format!(
            "{}{} {} ",
            sign,
            self.coef.abs(),
            variable_names[self.var_idx].name
        )
    }
}

#[derive(Clone, Debug)]
struct Constraint {
    name: String,
    expr: Vec<Term>,
    cmp: Keyword,
    rhs: f64,
}

impl Constraint {
    fn to_str(&self, variables: &Vec<Variable>) -> String {
        let mut ret = String::new();
        for (i, term) in self.expr.iter().enumerate() {
            ret += &term.to_str(i == 0, variables);
        }
        ret += self.cmp.to_str();
        ret += " ";
        ret += &self.rhs.to_string();
        ret
    }
}

#[derive(Clone)]
enum VariableType {
    Real,
    General,
    Binary,
}

#[derive(Clone)]
struct Variable {
    name: String,
    var_type: VariableType,
    is_free: bool,
}

#[derive(Clone)]
struct Problem {
    is_maximize: bool,
    variables: Vec<Variable>,
    objective: Vec<Term>,
    constraints: Vec<Constraint>,
}

impl Problem {
    fn normalized(&self) -> Problem {
        let objective = if self.is_maximize {
            self.objective.clone()
        } else {
            self.objective
                .iter()
                .map(|x| Term {
                    var_idx: x.var_idx,
                    coef: -x.coef,
                })
                .collect()
        };

        let mut variables = vec![];
        for var in self.variables.iter() {
            if !var.is_free {
                variables.push(var.clone());
                continue;
            }
            variables.push(Variable {
                name: var.name.to_string() + "+",
                var_type: var.var_type.clone(),
                is_free: false,
            });
            variables.push(Variable {
                name: var.name.to_string() + "-",
                var_type: var.var_type.clone(),
                is_free: false,
            });
        }

        let mut constraints = vec![];
        for c in self.constraints.iter() {
            if c.cmp != Keyword::Equal {
                constraints.push(c.clone());
                continue;
            }
            constraints.push(Constraint {
                name: c.name.to_owned() + "_1",
                expr: c.expr.clone(),
                cmp: Keyword::LessEqual,
                rhs: c.rhs,
            });
            constraints.push(Constraint {
                name: c.name.to_owned() + "_2",
                expr: c.expr.clone(),
                cmp: Keyword::GreaterEqual,
                rhs: c.rhs,
            });
        }
        for c in &mut constraints {
            if c.cmp != Keyword::GreaterEqual {
                continue;
            }
            c.expr.iter_mut().for_each(|t| t.coef *= -1.);
            c.cmp = Keyword::LessEqual;
            c.rhs *= -1.;
        }

        Problem {
            is_maximize: true,
            variables,
            objective,
            constraints,
        }
    }

    fn output(&self) {
        if self.is_maximize {
            println!("{}", Keyword::Maximize.to_str());
        } else {
            println!("{}", Keyword::Minimize.to_str());
        }

        print!("obj: ");
        for (i, term) in self.objective.iter().enumerate() {
            print!("{}", term.to_str(i == 0, &self.variables));
        }
        println!();

        println!("{}", Keyword::SubjectTo.to_str());

        for constraint in self.constraints.iter() {
            println!(
                "{}: {}",
                constraint.name,
                constraint.to_str(&self.variables)
            );
        }

        println!("{}", Keyword::End.to_str());
    }
}

trait Solver {
    fn solve(&self, problem: &Problem) -> f64;
}

struct SimplexModule {
    n: usize,
    m: usize,
    a: Vec<Vec<f64>>,
    b: Vec<f64>,
    c: Vec<f64>,
    x: Vec<f64>,
    obj: f64,
}

impl SimplexModule {
    fn from(problem: &Problem) -> SimplexModule {
        // 前提
        // - 標準形に変換済みの問題を渡す
        // - x = 0 が実行可能解である
        let n = problem.variables.len() + problem.constraints.len();
        let m = problem.constraints.len();

        let mut a = vec![vec![0.; n]; m];
        let mut b = vec![0.; m];
        let mut c = vec![0.; n];
        let mut x = vec![0.; n];
        let obj = 0.;

        for (i, c) in problem.constraints.iter().enumerate() {
            for t in c.expr.iter() {
                a[i][t.var_idx] = t.coef;
            }

            let slack_var_idx = n - m + i;

            a[i][slack_var_idx] = 1.;
            b[i] = c.rhs;
            x[slack_var_idx] = c.rhs;
        }

        for t in problem.objective.iter() {
            c[t.var_idx] = -t.coef;
        }

        SimplexModule {
            n,
            m,
            a,
            b,
            c,
            x,
            obj,
        }
    }

    fn round(&mut self) {
        for i in 0..self.m {
            for j in 0..self.n {
                self.a[i][j] = round_to_zero(self.a[i][j]);
            }
            self.b[i] = round_to_zero(self.b[i]);
        }
        for i in 0..self.n {
            self.c[i] = round_to_zero(self.c[i]);
            self.x[i] = round_to_zero(self.x[i]);
        }
        self.obj = round_to_zero(self.obj);
    }

    fn dump(&self) {
        let w = 7 * (self.n + 1);
        println!("{}", "=".repeat(w));
        for i in 0..self.m {
            for j in 0..self.n {
                print!("{:6.2} ", self.a[i][j]);
            }
            print!("|{:6.2}", self.b[i]);
            println!();
        }
        println!("{}", "-".repeat(w));
        for i in 0..self.n {
            print!("{:6.2} ", self.c[i]);
        }
        print!("|{:6.2}", self.obj);
        println!();
        println!("{}", "=".repeat(w));
    }
}

const EPS: f64 = 1e-6;

fn round_to_zero(v: f64) -> f64 {
    if v.abs() < EPS {
        0.
    } else {
        v
    }
}

struct SimplexLpSolver;

impl SimplexLpSolver {
    fn solve_simplex(&self, module: &mut SimplexModule) {
        module.dump();

        // 単体法
        loop {
            // 誤差対策: 0に近い値を0にする
            module.round();

            // 被約費用を計算し、変更する非基底変数を選択する
            // Blandの最小添字規則 (= 最大係数規則 + 最小添字規則)
            let mut pivot_var_idx = 0;
            for i in 0..module.n {
                if module.c[i] < module.c[pivot_var_idx] {
                    pivot_var_idx = i;
                }
            }

            // cで係数が負の項がない、最適解が求まっている
            if module.c[pivot_var_idx] >= 0. {
                break;
            }

            let mut theta = f64::MAX;
            let mut pivot_c_idx = 0;
            for i in 0..module.m {
                let theta_i = module.b[i] / module.a[i][pivot_var_idx];
                if theta_i < theta {
                    theta = theta_i;
                    pivot_c_idx = i;
                }
            }

            if theta <= 0. {
                // 解なし
                break;
            }

            // 単体表を更新する
            let div = module.a[pivot_c_idx][pivot_var_idx];
            for i in 0..module.n {
                module.a[pivot_c_idx][i] /= div;
            }
            module.b[pivot_c_idx] /= div;

            // 制約式の更新
            for i in 0..module.m {
                if i == pivot_c_idx {
                    continue;
                }
                let mul = module.a[i][pivot_var_idx];
                for j in 0..module.n {
                    module.a[i][j] -= mul * module.a[pivot_c_idx][j];
                }
                module.b[i] -= mul * module.b[pivot_c_idx];
            }

            // 目的関数の更新
            let mul = module.c[pivot_var_idx];
            for i in 0..module.n {
                module.c[i] -= mul * module.a[pivot_c_idx][i];
            }
            module.obj -= mul * module.b[pivot_c_idx];

            module.dump();
        }

        module.dump();
    }
}

impl Solver for SimplexLpSolver {
    fn solve(&self, problem: &Problem) -> f64 {
        // 標準形に変換する
        let problem = problem.normalized();

        // 単体表を作成する
        let mut module = SimplexModule::from(&problem);

        // 単体法を用いて解を見つける
        self.solve_simplex(&mut module);

        // TODO: 解を返す
        module.obj
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
) -> Vec<Term> {
    let obj_name = parse_name(q, "obj");
    let expr = parse_expr(q, variable_map);
    expr
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
            Keyword::GreaterEqual | Keyword::Equal | Keyword::LessEqual => val,
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
        cmp: *cmp,
        rhs,
    }
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

fn parse_lp_file(tokens: &Vec<Token>) -> Problem {
    let mut q = tokens.iter().peekable();
    let mut variable_map: HashMap<String, usize> = HashMap::new();
    let mut is_maximize = None;
    let mut objective = None;
    let mut constraints = None;

    // NOTE: bounds, general, binaryは未実装
    while let Some(tok) = q.next() {
        match tok {
            Token::Keyword { val } => match *val {
                Keyword::Maximize | Keyword::Minimize => {
                    is_maximize = Some(*val == Keyword::Maximize);
                    objective = Some(parse_objective(&mut q, &mut variable_map));
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
    for (var_name, var_idx) in variable_map {
        variables.push(Variable {
            name: var_name,
            var_type: VariableType::Real,
            is_free: false,
        });
    }

    Problem {
        is_maximize: is_maximize.expect("could not parse problem objective"),
        variables,
        objective: objective.expect("could not parse objective"),
        constraints: constraints.expect("could not parse constraints"),
    }
}

fn main() {
    let tokens = tokenize_lp_file(
        "maximize
obj: 5 x + 4 y
st
c1: 1.5 x + 3 y <= 13.5
c2: 3 x + 1 y <= 10
end
",
    );
    let problem = parse_lp_file(&tokens);
    problem.output();
}

#[test]
fn test_simple_lp_problem() {
    let tokens = tokenize_lp_file(
        "maximize
obj: 5 x + 4 y
st
c1: 1.5 x + 3 y <= 13.5
c2: 3 x + 1 y <= 10
end
",
    );
    let problem = parse_lp_file(&tokens);
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    assert!(f64::abs(obj - 24.6) < EPS);
}
