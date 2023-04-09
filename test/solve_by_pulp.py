import pulp


def main() -> None:
    problem = pulp.LpProblem("auxiliary problem", sense=pulp.LpMaximize)
    x = {
        "x1": pulp.LpVariable("x1", cat="Continuous", lowBound=0),
        "x2": pulp.LpVariable("x2", cat="Continuous", lowBound=0),
    }

    problem += x["x1"] + 2 * x["x2"]
    problem += x["x1"] + x["x2"] <= 6
    problem += x["x1"] + 3 * x["x2"] <= 12
    problem += -3 * x["x1"] - 2 * x["x2"] <= -6

    print(problem)
    status = problem.solve()
    print(pulp.LpStatus[status], problem.objective.value())
    print(x["x1"].value(), x["x2"].value())


if __name__ == "__main__":
    main()
