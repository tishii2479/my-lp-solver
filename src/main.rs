enum Compare {
    Less,
    LessEqual,
    Equal,
    Greater,
    GreaterEqual,
}

impl Compare {
    fn to_str(&self) -> &str {
        match self {
            Compare::Less => "<",
            Compare::LessEqual => "<=",
            Compare::Equal => "=",
            Compare::Greater => ">",
            Compare::GreaterEqual => ">=",
        }
    }
}

struct Term {
    var_idx: usize,
    coef: f64,
}

impl Term {
    fn to_str(&self, is_first: bool, variable_names: &Vec<String>) -> String {
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
            variable_names[self.var_idx]
        )
    }
}

struct Constraint {
    expr: Vec<Term>,
    cmp: Compare,
    rhs: f64,
}

impl Constraint {
    fn to_str(&self, variable_names: &Vec<String>) -> String {
        let mut ret = String::new();
        for (i, term) in self.expr.iter().enumerate() {
            ret += &term.to_str(i == 0, variable_names);
        }
        ret += self.cmp.to_str();
        ret += " ";
        ret += &self.rhs.to_string();
        ret
    }
}

struct Problem {
    is_maximize: bool,
    variable_names: Vec<String>,
    objective: Vec<Term>,
    constraints: Vec<Constraint>,
}

impl Problem {
    fn output(&self) {
        if self.is_maximize {
            println!("maximize");
        } else {
            println!("minimize");
        }

        for (i, term) in self.objective.iter().enumerate() {
            print!("{}", term.to_str(i == 0, &self.variable_names));
        }
        println!();

        println!("subject to");

        for (i, constraint) in self.constraints.iter().enumerate() {
            println!("c{}: {}", i + 1, constraint.to_str(&self.variable_names));
        }
    }
}

fn main() {
    let problem = Problem {
        is_maximize: true,
        variable_names: vec!["x".to_owned(), "y".to_owned()],
        objective: vec![
            Term {
                var_idx: 0,
                coef: 3.,
            },
            Term {
                var_idx: 1,
                coef: 4.,
            },
        ],
        constraints: vec![
            Constraint {
                expr: vec![Term {
                    var_idx: 0,
                    coef: 1.,
                }],
                cmp: Compare::GreaterEqual,
                rhs: 0.,
            },
            Constraint {
                expr: vec![Term {
                    var_idx: 1,
                    coef: 1.,
                }],
                cmp: Compare::GreaterEqual,
                rhs: 0.,
            },
            Constraint {
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: 5.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 1.,
                    },
                ],
                cmp: Compare::LessEqual,
                rhs: 10.,
            },
            Constraint {
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: -3.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 4.,
                    },
                ],
                cmp: Compare::LessEqual,
                rhs: 9.,
            },
        ],
    };

    problem.output();
}
