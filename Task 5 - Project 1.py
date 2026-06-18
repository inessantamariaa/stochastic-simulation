import numpy as np
rng = np.random.default_rng(1)

#probability matrix
P = np.array([
    [0.9915, 0.005, 0.0025, 0.0,   0.001],
    [0.0,    0.986, 0.005,  0.004, 0.005],
    [0.0,    0.0,   0.992,  0.003, 0.005],
    [0.0,    0.0,   0.0,    0.991, 0.009],
    [0.0,    0.0,   0.0,    0.0,   1.0]
])
cum_prob = np.cumsum(P, axis=1)

# mean life time (4*4)
Q = P[:4, :4]
mean_lifetime = np.linalg.solve(np.eye(4) - Q, np.ones(4))[0]
print("Theoretical mean lifetime =", round(mean_lifetime, 4), "months")
def lifetime_simulation(N):
    current_state = np.zeros(N, dtype=int)
    death_time = np.zeros(N, dtype=int)
    still_alive = np.ones(N, dtype=bool)
    t = 0
    while np.any(still_alive):
        t += 1
        alive_index = np.where(still_alive)[0]
        u = rng.random(len(alive_index))

        probs = cum_prob[current_state[alive_index]]
        new_state = np.argmax(u[:, None] < probs, axis=1)

        current_state[alive_index] = new_state

        dead_index = alive_index[new_state == 4]
        death_time[dead_index] = t
        still_alive[dead_index] = False
    return death_time


rep = 100
women = 200
x_values = []
z_values = []
for _ in range(rep):
    times = lifetime_simulation(women)

    x_values.append(np.mean(times <= 350))
    z_values.append(np.mean(times))

x_values = np.array(x_values)
z_values = np.array(z_values)

# crude Monte Carlo estimate
theta_crude = np.mean(x_values)
var_crude = np.var(x_values, ddof=1)

# control variate estimate
cov_xz = np.cov(x_values, z_values, ddof=1)[0, 1]
var_z = np.var(z_values, ddof=1)

c = -cov_xz / var_z
y_values = x_values + c * (z_values - mean_lifetime)

theta_cv = np.mean(y_values)
var_cv = np.var(y_values, ddof=1)
var_reduction = 100 * (var_crude - var_cv) / var_crude

print(f"Crude MC: theta = {theta_crude:.4f}, Var = {var_crude:.6e}")
print(f"Control variate: theta = {theta_cv:.4f}, Var = {var_cv:.6e}")
print(f"c = {c:.4f}")
print(f"Variance reduction = {var_reduction:.2f}%")