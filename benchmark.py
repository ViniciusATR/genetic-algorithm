import sys
from subprocess import run

seeds = [42, 21, 10, 1000, 90, 12, 23, 14, 999, 69]

iterations = [100, 1000]

pso_bias = [1, 2]

ga_selections = ["elitist", "roulette", "tournament"]

ga_muta_prob = [0.01, 0.1, 0.5]

ga_cutoff = [1, 2, 3, 5]

optai_beta = [100.0, 0.05, 0.1, 0.5]

optai_thresh = [0.001, 0.000001]

optai_ra = [0.05, 0.1]


def run_pso_bench():
    best_pso = 9999.9999
    best_pso_params = None
    for seed in seeds:
        for it in iterations:
            for lbias in pso_bias:
                for gbias in pso_bias:
                    res = run(
                        [
                            "stack",
                            "run",
                            "run-pso-exe",
                            "2",
                            f"{seed}",
                            f"{it}",
                            f"{gbias}",
                            f"{lbias}",
                        ],
                        capture_output=True,
                        encoding="utf-8",
                    )
                    if float(res.stdout) < best_pso:
                        best_pso = float(res.stdout)
                        best_pso_params = {
                            "seed": seed,
                            "iterations": it,
                            "gbias": gbias,
                            "lbias": lbias,
                        }
                    print(
                        f"Result with {seed=}, {it=}, {gbias=}, {lbias=} is {res.stdout}"
                    )
    print(f"Best PSO result = {best_pso} with {best_pso_params}")


def run_ga_bench():
    best_ga = 9999.9999
    best_ga_params = None
    for seed in seeds:
        for it in iterations:
            for select in ga_selections:
                for mut_rate in ga_muta_prob:
                    for cutoff in ga_cutoff:
                        res = run(
                            [
                                "stack",
                                "run",
                                "run-ga-exe",
                                f"{select}",
                                f"{seed}",
                                "2",
                                f"{it}",
                                f"{cutoff}",
                                f"{mut_rate}",
                            ],
                            capture_output=True,
                            encoding="utf-8",
                        )
                        if float(res.stdout) < best_ga:
                            best_ga = float(res.stdout)
                            best_ga_params = {
                                "seed": seed,
                                "selection": select,
                                "iterations": it,
                                "cutoff": cutoff,
                                "mut_rate": mut_rate,
                            }
                        print(
                            f"Result with {seed=}, {it=}, {select=}, {cutoff=}, {mut_rate=} is {res.stdout}"
                        )

    print(f"Best GA result = {best_ga} with {best_ga_params}")


def run_optai_bench():
    best_ai = 9999.9999
    best_ai_params = None
    for seed in seeds:
        for it in iterations:
            for rand_add in optai_ra:
                for fit_thresh in optai_thresh:
                    for beta in optai_beta:
                        res = run(
                            [
                                "stack",
                                "run",
                                "run-ainet-exe",
                                "2",
                                f"{seed}",
                                f"{it}",
                                f"{rand_add}",
                                f"{beta}",
                                f"{fit_thresh}",
                            ],
                            capture_output=True,
                            encoding="utf-8",
                        )
                        if float(res.stdout) < best_ai:
                            best_ai = float(res.stdout)
                            best_ai_params = {
                                "seed": seed,
                                "rand_add": rand_add,
                                "iterations": it,
                                "beta": beta,
                                "fit_thresh": fit_thresh,
                            }
                        print(
                            f"Result with {seed=}, {it=}, {rand_add=}, {fit_thresh=}, {beta=} is {res.stdout}"
                        )

    print(f"Best GA result = {best_ai} with {best_ai_params}")


if sys.argv[1] == "pso":
    run_pso_bench()
elif sys.argv[1] == "ga":
    run_ga_bench()
elif sys.argv[1] == "optai":
    run_optai_bench()
