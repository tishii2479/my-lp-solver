use std::collections::HashMap;

mod parser;
mod problem;

trait Solver {
    fn solve(&self, problem: &problem::Problem) -> Result<Solution, String>;
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
    /// 単体表を作成する
    /// requires:
    /// - 標準形に変換済みの問題を渡す
    fn from(problem: &problem::Problem) -> SimplexTable {
        // 前提
        // - 標準形に変換済みの問題を渡す
        let n = problem.variables.len() + problem.constraints.len();
        let m = problem.constraints.len();

        let mut a = vec![vec![0.; n]; m];
        let mut b = vec![0.; m];
        let mut c = vec![0.; n];
        let mut base_var = vec![0; m];
        let obj = 0.;

        for (i, c) in problem.constraints.iter().enumerate() {
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

    /// 補助問題を解くために、人為変数を追加した単体表に変換する
    fn to_auxiliary_table(&mut self) {
        // b[i](< 0)が最も小さい行に対してピボット操作を行い、実行可能な単体表を得る
        let mut artificial_c_idx = 0;
        // 人為変数の列を足す
        for i in 0..self.m {
            self.a[i].push(-1.);
            if self.b[i] < self.b[artificial_c_idx] {
                artificial_c_idx = i;
            }
        }

        assert!(self.b[artificial_c_idx] < 0.);

        // 目的変数の設定
        self.c = vec![0.; self.n];
        self.c.push(1.);

        self.n += 1;
        self.pivot(artificial_c_idx, self.n - 1);
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

    fn calc_pivot_c_idx(&self, pivot_var_idx: usize) -> Option<(usize, f64)> {
        let mut theta = f64::MAX;
        let mut idx = None;
        for i in 0..self.m {
            if self.a[i][pivot_var_idx] <= 0. {
                continue;
            }
            let theta_i = self.b[i] / self.a[i][pivot_var_idx];
            if theta_i < theta {
                theta = theta_i;
                idx = Some(i);
            }
        }
        if let Some(idx) = idx {
            Some((idx, theta))
        } else {
            None
        }
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

    fn round_to_zero(&mut self) {
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
        // 人為変数が基底変数になっている場合、非基底変数に変更する
        if let Some(artifial_base_c_idx) = self.base_var.iter().position(|&x| x == self.n - 1) {
            let mut new_base_var_idx = None;
            for i in 0..self.n {
                if round_to_zero(self.a[artifial_base_c_idx][i]) != 0.
                    && !self.base_var.contains(&i)
                {
                    new_base_var_idx = Some(i);
                    break;
                }
            }
            self.pivot(
                artifial_base_c_idx,
                new_base_var_idx.expect("was not able to find new candidate of base variable"),
            );
        }

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

        // 人為変数の列の削除
        for i in 0..self.m {
            self.a[i].pop();
        }
        self.c.pop();
        self.n -= 1;
    }

    fn get_variable_values(&self, problem: &problem::Problem) -> HashMap<String, f64> {
        let mut variable_values = HashMap::new();
        for var in problem.variables.iter() {
            variable_values.insert(var.name.clone(), 0.);
        }
        for (c_idx, base_var_idx) in self.base_var.iter().enumerate() {
            if *base_var_idx < problem.variables.len() {
                variable_values
                    .insert(problem.variables[*base_var_idx].name.clone(), self.b[c_idx]);
            }
        }
        variable_values
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
        dbg!(&self.base_var);
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
            table.round_to_zero();

            // 被約費用を計算し、変更する非基底変数を選択する
            // 最大係数規則 + Blandの最小添字規則
            let pivot_var_idx = table.calc_pivot_var_idx();

            // cで係数が負の項がない(=最適解が求まっている)
            if table.c[pivot_var_idx] >= 0. {
                println!("found");
                break;
            }

            // pivot_var_idxの上限(theta)を求める
            let Some((pivot_c_idx, theta)) = table.calc_pivot_c_idx(pivot_var_idx) else {
                println!("unbounded");
                break;
            };
            if theta <= 0. {
                // 解なし
                println!("not able to improve any more");
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

#[derive(Debug)]
struct Solution {
    objective_value: f64,
    variable_values: HashMap<String, f64>,
}

impl Solver for SimplexLpSolver {
    fn solve(&self, raw_problem: &problem::Problem) -> Result<Solution, String> {
        // 標準形に変換する
        let problem = raw_problem.standardized();

        // 単体表を作成する
        // constraint.rhs < 0.が負の制約があれば原点は実行可能解ではないため、
        // 補助問題を作成し、実行可能解を求める必要がある
        let origin_is_feasible = problem.constraints.iter().all(|c| c.rhs >= 0.);
        let mut table = SimplexTable::from(&problem);
        if !origin_is_feasible {
            table.to_auxiliary_table();
            self.solve_simplex(&mut table);
            if round_to_zero(table.obj) != 0. {
                return Err("is not feasible".to_owned());
            }
            table.remove_aritifial_var(&problem);
        }

        // 単体法を用いて解を見つける
        self.solve_simplex(&mut table);
        let variable_values = table.get_variable_values(&problem);

        // TODO: 解を返す
        let objective_value = if raw_problem.is_maximize {
            table.obj
        } else {
            -table.obj
        };

        Ok(Solution {
            objective_value,
            variable_values,
        })
    }
}

fn main() {
    let problem = parser::parse_lp_file(
        "maximize
obj: x1 + x2
st
c1: x1 + x2 <= 2
c2: x1 + x2 >= 2
end
",
    );
    problem.output();
    let solver = SimplexLpSolver;
    let solution = solver.solve(&problem).unwrap();
    dbg!(solution);
}

#[test]
fn test_unfeasible_lp_problem() {
    let problem = parser::parse_lp_file(
        "maximize
obj: x1 + x2
st
c1: x1 + x2 <= 2
c2: x1 + x2 >= 3
end
",
    );
    problem.output();
    let solver = SimplexLpSolver;
    let err = solver.solve(&problem).err();
    assert_eq!(err, Some("is not feasible".to_owned()));
}

#[test]
fn test_equal_auxiliary_lp_problem() {
    let problem = parser::parse_lp_file(
        "maximize
obj: x1 + 3 x2 + 5 x3
st
c1: - x1 + x2 + x3 <= 2
c2: 2 x1 + x2 - x3 = 8
c3: x1 + 2 x2 - x3 >= 1
end
",
    );
    problem.output();
    let solver = SimplexLpSolver;
    let solution = solver.solve(&problem).unwrap();
    assert!(f64::abs(solution.objective_value - 56.) < EPS);
}

#[test]
fn test_minimize_auxiliary_lp_problem() {
    let problem = parser::parse_lp_file(
        "minimize
obj: 12 x1 + 6 x2 + 10 x3
st
c1: x1 + x2 + 2 x3 >= 10
c2: 3 x1 + x2 + x3 >= 20
end
",
    );
    let solver = SimplexLpSolver;
    let solution = solver.solve(&problem).unwrap();
    assert!(f64::abs(solution.objective_value - 90.) < EPS);
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
    let solution = solver.solve(&problem).unwrap();
    assert!(f64::abs(solution.objective_value - 9.) < EPS);
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
    let solution = solver.solve(&problem).unwrap();
    assert!(f64::abs(solution.objective_value - 9.) < EPS);
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
    let solution = solver.solve(&problem).unwrap();
    assert!(f64::abs(solution.objective_value - 24.6) < EPS);
}
