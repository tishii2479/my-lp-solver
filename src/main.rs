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

    fn dump(&self) {
        let w = 7 * (self.n + 1);
        println!("{}", "=".repeat(w));
        for i in 0..self.m {
            for j in 0..self.n {
                print!("{:6.2} ", self.a[i][j]);
            }
            print!("{:6.2} ", self.b[i]);
            println!();
        }
        println!("{}", "-".repeat(w));
        for i in 0..self.n {
            print!("{:6.2} ", self.c[i]);
        }
        print!("{:6.2} ", self.obj);
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
            for i in 0..module.m {
                for j in 0..module.n {
                    module.a[i][j] = round_to_zero(module.a[i][j]);
                }
                module.b[i] = round_to_zero(module.b[i]);
            }
            for i in 0..module.n {
                module.c[i] = round_to_zero(module.c[i]);
                module.x[i] = round_to_zero(module.x[i]);
            }
            module.obj = round_to_zero(module.obj);

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

            dbg!(pivot_c_idx, pivot_var_idx);

            // 単体表を更新する
            let div = module.a[pivot_c_idx][pivot_var_idx];
            for i in 0..module.n {
                module.a[pivot_c_idx][i] /= div;
            }
            module.b[pivot_c_idx] /= div;

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
    fn solve(&self, problem: &Problem) {
        // 標準形に変換する
        let problem = problem.normalized();

        // 単体表を作成する
        let mut module = SimplexModule::from(&problem);

        self.solve_simplex(&mut module);
    }
}

fn main() {
    let problem = Problem {
        is_maximize: true,
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
                coef: 5.,
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
                        coef: 1.5,
                    },
                    Term {
                        var_idx: 1,
                        coef: 3.,
                    },
                ],
                cmp: Compare::LessEqual,
                rhs: 13.5,
            },
            Constraint {
                expr: vec![
                    Term {
                        var_idx: 0,
                        coef: 3.,
                    },
                    Term {
                        var_idx: 1,
                        coef: 1.,
                    },
                ],
                cmp: Compare::LessEqual,
                rhs: 10.,
            },
            // Constraint {
            //     expr: vec![
            //         Term {
            //             var_idx: 0,
            //             coef: 3.,
            //         },
            //         Term {
            //             var_idx: 1,
            //             coef: 6.,
            //         },
            //     ],
            //     cmp: Compare::GreaterEqual,
            //     rhs: 5.,
            // },
            // Constraint {
            //     expr: vec![
            //         Term {
            //             var_idx: 0,
            //             coef: 2.,
            //         },
            //         Term {
            //             var_idx: 1,
            //             coef: 4.,
            //         },
            //     ],
            //     cmp: Compare::Equal,
            //     rhs: 5.,
            // },
        ],
    };

    println!("before normalized:");
    problem.output();

    println!("after normalized:");
    let problem = problem.normalized();
    problem.output();

    let solver = SimplexLpSolver;
    solver.solve(&problem);
}
