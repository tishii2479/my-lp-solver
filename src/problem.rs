#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Compare {
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
}

#[derive(Clone, Copy, Debug)]
pub struct Term {
    pub var_idx: usize,
    pub coef: f64,
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
pub struct Constraint {
    pub name: String,
    pub expr: Vec<Term>,
    pub cmp: Compare,
    pub rhs: f64,
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
pub enum VariableType {
    Real,
    General,
    Binary,
}

#[derive(Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: VariableType,
    pub is_free: bool,
}

#[derive(Clone)]
pub struct Problem {
    pub is_maximize: bool,
    pub objective_name: String,
    pub variables: Vec<Variable>,
    pub objective: Vec<Term>,
    pub constraints: Vec<Constraint>,
}

impl Problem {
    pub fn standardized(&self) -> Problem {
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
                name: var.name.to_string() + "_plus",
                var_type: var.var_type.clone(),
                is_free: false,
            });
            variables.push(Variable {
                name: var.name.to_string() + "_minus",
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
                name: c.name.to_owned() + "_1",
                expr: c.expr.clone(),
                cmp: Compare::LessEqual,
                rhs: c.rhs,
            });
            constraints.push(Constraint {
                name: c.name.to_owned() + "_2",
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
            objective_name: self.objective_name.to_owned(),
            variables,
            objective,
            constraints,
        }
    }

    pub fn output(&self) {
        if self.is_maximize {
            println!("maximize");
        } else {
            println!("minimize");
        }

        print!("obj: ");
        for (i, term) in self.objective.iter().enumerate() {
            print!("{}", term.to_str(i == 0, &self.variables));
        }
        println!();

        println!("st");

        for constraint in self.constraints.iter() {
            println!(
                "{}: {}",
                constraint.name,
                constraint.to_str(&self.variables)
            );
        }

        println!("end");
    }
}
