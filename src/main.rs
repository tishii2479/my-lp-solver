mod parser;
mod problem;

trait Solver {
    fn solve(&self, problem: &problem::Problem) -> f64;
}

struct SimplexTable {
    n: usize,
    m: usize,
    a: Vec<Vec<f64>>,
    b: Vec<f64>,
    c: Vec<f64>,
    base_var: Vec<usize>,
    obj: f64,
}

impl SimplexTable {
    fn create_normal_table(problem: &problem::Problem) -> SimplexTable {
        // 前提
        // - 標準形に変換済みの問題を渡す
        // - 原点が実行可能解である
        let n = problem.variables.len() + problem.constraints.len();
        let m = problem.constraints.len();

        let mut a = vec![vec![0.; n]; m];
        let mut b = vec![0.; m];
        let mut c = vec![0.; n];
        let mut base_var = vec![0; m];
        let obj = 0.;

        for (i, c) in problem.constraints.iter().enumerate() {
            assert!(c.rhs >= 0., "constraint has negative rhs: {:?}", c);
            for t in c.expr.iter() {
                a[i][t.var_idx] = t.coef;
            }

            let slack_var_idx = n - m + i;

            a[i][slack_var_idx] = 1.;
            b[i] = c.rhs;
            base_var[i] = slack_var_idx;
        }

        // 目的関数の作成
        for t in problem.objective.iter() {
            c[t.var_idx] = -t.coef;
        }

        SimplexTable {
            n,
            m,
            a,
            b,
            c,
            base_var,
            obj,
        }
    }

    fn create_auxiliary_table(problem: &problem::Problem) -> SimplexTable {
        // 前提
        // - 標準形に変換済みの問題を渡す
        let n = problem.variables.len() + problem.constraints.len();
        let m = problem.constraints.len();

        let mut a = vec![vec![0.; n + 1]; m];
        let mut b = vec![0.; m];
        let mut c = vec![0.; n + 1];
        let mut base_var = vec![0; m];
        let obj = 0.;

        let artificial_var_idx = n;
        let mut artificial_c_idx = 0;

        for (i, c) in problem.constraints.iter().enumerate() {
            for t in c.expr.iter() {
                a[i][t.var_idx] = t.coef;
            }

            let slack_var_idx = n - m + i;

            a[i][slack_var_idx] = 1.;
            a[i][artificial_var_idx] = -1.;
            b[i] = c.rhs;
            base_var[i] = slack_var_idx;

            if b[i] < b[artificial_c_idx] {
                artificial_c_idx = i;
            }
        }

        // 目的関数の作成
        c[n] = 1.;

        let mut table = SimplexTable {
            n: n + 1,
            m,
            a,
            b,
            c,
            base_var,
            obj,
        };
        table.pivot(artificial_c_idx, n);

        table
    }

    fn calc_pivot_var_idx(&self) -> usize {
        let mut idx = 0;
        for i in 0..self.n {
            if self.c[i] < self.c[idx] {
                idx = i;
            }
        }
        idx
    }

    fn calc_pivot_c_idx(&self, pivot_var_idx: usize) -> (usize, f64) {
        let mut theta = f64::MAX;
        let mut idx = 0;
        for i in 0..self.m {
            if self.a[i][pivot_var_idx] <= 0. {
                continue;
            }
            let theta_i = self.b[i] / self.a[i][pivot_var_idx];
            if theta_i < theta {
                theta = theta_i;
                idx = i;
            }
        }
        assert_ne!(theta, f64::MAX, "could not find pivot");
        (idx, theta)
    }

    fn pivot(&mut self, pivot_c_idx: usize, pivot_var_idx: usize) {
        self.base_var[pivot_c_idx] = pivot_var_idx;

        let div = self.a[pivot_c_idx][pivot_var_idx];
        for i in 0..self.n {
            self.a[pivot_c_idx][i] /= div;
        }
        self.b[pivot_c_idx] /= div;

        // 制約式の更新
        for i in 0..self.m {
            if i == pivot_c_idx {
                continue;
            }
            let mul = self.a[i][pivot_var_idx];
            for j in 0..self.n {
                self.a[i][j] -= mul * self.a[pivot_c_idx][j];
            }
            self.b[i] -= mul * self.b[pivot_c_idx];
        }

        // 目的関数の更新
        let mul = self.c[pivot_var_idx];
        for i in 0..self.n {
            self.c[i] -= mul * self.a[pivot_c_idx][i];
        }
        self.obj -= mul * self.b[pivot_c_idx];
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
        }
        self.obj = round_to_zero(self.obj);
    }

    fn remove_aritifial_var(&mut self, problem: &problem::Problem) {
        // 人為変数の列の削除
        for i in 0..self.m {
            self.a[i].pop();
        }
        self.c.pop();
        self.n -= 1;

        // 目的変数の再設定
        // 元の目的関数から、基底変数を削除する
        self.c = vec![0.; self.n];
        for t in problem.objective.iter() {
            self.c[t.var_idx] = -t.coef;
        }
        for (c_idx, base_var_idx) in self.base_var.iter().enumerate() {
            // NOTE: 無くても良い
            if round_to_zero(self.c[*base_var_idx]) == 0. {
                continue;
            }
            let mul = self.c[*base_var_idx] / self.a[c_idx][*base_var_idx];
            for i in 0..self.n {
                self.c[i] -= mul * self.a[c_idx][i];
            }
            self.obj -= mul * self.b[c_idx];
        }
    }

    fn get_ans(&self, problem: &problem::Problem) -> Vec<f64> {
        let mut ans = vec![0.; problem.variables.len()];
        for (c_idx, base_var_idx) in self.base_var.iter().enumerate() {
            if *base_var_idx < problem.variables.len() {
                ans[*base_var_idx] = self.b[c_idx];
            }
        }
        ans
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
    fn solve_simplex(&self, table: &mut SimplexTable) {
        table.dump();

        // 単体法
        loop {
            // 誤差対策: 0に近い値を0にする
            table.round();

            // 被約費用を計算し、変更する非基底変数を選択する
            // 最大係数規則 + Blandの最小添字規則
            let pivot_var_idx = table.calc_pivot_var_idx();

            // cで係数が負の項がない(=最適解が求まっている)
            if table.c[pivot_var_idx] >= 0. {
                break;
            }

            // pivot_var_idxの上限(theta)を求める
            let (pivot_c_idx, theta) = table.calc_pivot_c_idx(pivot_var_idx);
            if theta <= 0. {
                // 解なし
                break;
            }

            // 単体表を更新する
            // ピボット操作
            table.pivot(pivot_c_idx, pivot_var_idx);

            table.dump();
        }

        table.dump();
    }
}

impl Solver for SimplexLpSolver {
    fn solve(&self, problem: &problem::Problem) -> f64 {
        // 標準形に変換する
        let problem = problem.standardized();

        // 単体表を作成する
        // constraint.rhs < 0.が負の制約があれば原点は実行可能解ではないため、
        // 補助問題を作成し、実行可能解を求める必要がある
        let origin_is_feasible = problem.constraints.iter().all(|c| c.rhs >= 0.);
        let mut table = if origin_is_feasible {
            SimplexTable::create_normal_table(&problem)
        } else {
            let mut table = SimplexTable::create_auxiliary_table(&problem);
            self.solve_simplex(&mut table);
            if round_to_zero(table.obj) != 0. {
                panic!("is not feasible");
            }
            table.remove_aritifial_var(&problem);
            table
        };

        // 単体法を用いて解を見つける
        self.solve_simplex(&mut table);
        let ans = table.get_ans(&problem);
        dbg!(ans);

        // TODO: 解を返す
        table.obj
    }
}

fn main() {
    let problem = parser::parse_lp_file(
        "maximize
obj: x1 + 2 x2
st
c1: x1 + x2 <= 6
c2: x1 + 3 x2 <= 12
c3: - 3 x1 - 2 x2 <= - 6
end
",
    );
    problem.output();
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    eprintln!("objective value: {:.2}", obj);
}

#[test]
fn test_auxiliary_lp_problem() {
    let problem = parser::parse_lp_file(
        "maximize
obj: x1 + 2 x2
st
c1: x1 + x2 <= 6
c2: x1 + 3 x2 <= 12
c3: - 3 x1 - 2 x2 <= - 6
end
",
    );
    let solver = SimplexLpSolver;
    let obj = solver.solve(&problem);

    assert!(f64::abs(obj - 9.) < EPS);
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
