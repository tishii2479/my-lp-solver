mod parser;
mod problem;

trait Solver {
    fn solve(&self, problem: &problem::Problem) -> f64;
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
    fn from(problem: &problem::Problem) -> SimplexModule {
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
    fn solve(&self, problem: &problem::Problem) -> f64 {
        // 標準形に変換する
        let problem = problem.standardized();

        // 単体表を作成する
        let mut module = SimplexModule::from(&problem);

        // 単体法を用いて解を見つける
        self.solve_simplex(&mut module);

        // TODO: 解を返す
        module.obj
    }
}

fn main() {
    let problem = parser::parse_lp_file(
        "maximize
obj: 3 x1 + 2 x2
st
c1: 2 x1 + x2 <= 6
c2: x1 + x2 <= 3
end
",
    );
    problem.output();
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    eprintln!("objective value: {:.2}", obj);
}

#[test]
fn test_cycling_lp_problem() {
    let problem = parser::parse_lp_file(
        "maximize
obj: 3 x1 + 2 x2
st
c1: 2 x1 + x2 <= 6
c2: x1 + x2 <= 3
end
",
    );
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    assert!(f64::abs(obj - 9.) < EPS);
}

#[test]
fn test_simple_lp_problem() {
    let problem = parser::parse_lp_file(
        "maximize
obj: 5 x + 4 y
st
c1: 1.5 x + 3 y <= 13.5
c2: 3 x + 1 y <= 10
end
",
    );
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    assert!(f64::abs(obj - 24.6) < EPS);
}
