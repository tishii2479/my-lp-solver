#[derive(Clone, Copy, PartialEq)]
enum Compare {
    LessEqual,
    Equal,
    GreaterEqual,
}

impl Compare {
    fn to_str(&self) -> &str {
        match self {
            Compare::LessEqual => "<=",
            Compare::Equal => "=",
            Compare::GreaterEqual => ">=",
        }
    }

    fn inv(&self) -> Compare {
        match self {
            Compare::Equal => Compare::Equal,
            Compare::GreaterEqual => Compare::LessEqual,
            Compare::LessEqual => Compare::GreaterEqual,
        }
    }
}

#[derive(Clone, Copy)]
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

#[derive(Clone)]
struct Constraint {
    expr: Vec<Term>,
    cmp: Compare,
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
            if c.cmp != Compare::Equal {
                constraints.push(c.clone());
                continue;
            }
            constraints.push(Constraint {
                expr: c.expr.clone(),
                cmp: Compare::LessEqual,
                rhs: c.rhs,
            });
            constraints.push(Constraint {
                expr: c.expr.clone(),
                cmp: Compare::GreaterEqual,
                rhs: c.rhs,
            });
        }
        for c in &mut constraints {
            if c.cmp != Compare::GreaterEqual {
                continue;
            }
            c.expr.iter_mut().for_each(|t| t.coef *= -1.);
            c.cmp = Compare::LessEqual;
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
            println!("maximize");
        } else {
            println!("minimize");
        }

        for (i, term) in self.objective.iter().enumerate() {
            print!("{}", term.to_str(i == 0, &self.variables));
        }
        println!();

        println!("subject to");

        for (i, constraint) in self.constraints.iter().enumerate() {
            println!("c{}: {}", i + 1, constraint.to_str(&self.variables));
        }

        println!("end");
    }
}

trait Solver {
    fn solve(&self, problem: &Problem);
}

struct SimplexModule {
    table: Vec<Vec<f64>>,
}

impl SimplexModule {
    fn from(problem: &Problem) {}
}

struct SimplexLpSolver;

impl Solver for SimplexLpSolver {
    fn solve(&self, problem: &Problem) {
        // 標準形に変換する
        // TODO:

        // 単体表を作成する

        // 単体法
        // 実行可能解を求める

        // 被約費用を計算する

        // 変更する非基底変数を選択する

        // a_kを計算する

        // 単体表を更新する
    }
}

fn main() {
    let problem = Problem {
        is_maximize: false,
        variables: vec![
            Variable {
                name: "x".to_owned(),
                var_type: VariableType::Real,
                is_free: false,
            },
            Variable {
                name: "y".to_owned(),
                var_type: VariableType::Real,
                is_free: false,
            },
        ],
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
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: 5.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 2.,
                    },
                ],
                cmp: Compare::LessEqual,
                rhs: 10.,
            },
            Constraint {
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: 3.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 6.,
                    },
                ],
                cmp: Compare::GreaterEqual,
                rhs: 5.,
            },
            Constraint {
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: 2.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 4.,
                    },
                ],
                cmp: Compare::Equal,
                rhs: 5.,
            },
        ],
    };

    println!("before normalized");
    problem.output();
    let problem = problem.normalized();
    println!("after normalized");
    problem.output();
}
