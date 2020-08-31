params_good <- list(alpha = .01, ## pr(asym)
              omega_c = .25, ## pr(Q|community contact)
              omega_h = .95, ## pr(Q|household contact)
              omega_q = .9, ## pr(Q|quarantined individual)
              rho_s = 0.95, ## pr(detected by surveillance|sym)
              rho_a = 0.05, ## pr(detected by surveillance|asym)
              R = 2.5,
              kappa = .5, ## rel transmiss from asym to sym
              eta = .5, ## pr(household contact)
              nu = 4, ## rel risk household:casual transmission
              t_ds = 2,
              t_da = 3,
              t_qcs = 2,
              t_qca = 3,
              t_qhs = 1,
              t_qha = 1,
              t_q = 1)
params_bad <- list(alpha = .01, ## pr(asym)
              omega_c = .25, ## pr(Q|community contact)
              omega_h = .95, ## pr(Q|household contact)
              t_q = 1)
params_bad2 <- list(alpha = .01, ## pr(asym)
              omega_c = .25, ## pr(Q|community contact)
              omega_h = .95, ## pr(Q|household contact)
              omega_q = -2, ## pr(Q|quarantined individual)
              rho_s = 0.95, ## pr(detected by surveillance|sym)
              rho_a = 0.05, ## pr(detected by surveillance|asym)
              R = 2.5,
              kappa = .5, ## rel transmiss from asym to sym
              eta = .5, ## pr(household contact)
              nu = 4, ## rel risk household:casual transmission
              t_ds = 2,
              t_da = 3,
              t_qcs = 2,
              t_qca = 3,
              t_qhs = 1,
              t_qha = 1,
              t_q = 1)
params_bad3 <- list(alpha = .01, ## pr(asym)
              omega_c = .25, ## pr(Q|community contact)
              omega_h = .95, ## pr(Q|household contact)
              omega_q = .5, ## pr(Q|quarantined individual)
              rho_s = 0.95, ## pr(detected by surveillance|sym)
              rho_a = 0.05, ## pr(detected by surveillance|asym)
              R = 2.5,
              kappa = .5, ## rel transmiss from asym to sym
              eta = .5, ## pr(household contact)
              nu = 4, ## rel risk household:casual transmission
              t_ds = 2,
              t_da = 3,
              t_qcs = 2,
              t_qca = 3,
              t_qhs = 1,
              t_qha = 1,
              t_q = -1)
params_vartype_good <- list(
    alpha = "proportion",
    omega_c = "proportion",
    omega_h = "proportion",
    omega_q = "proportion",
    rho_s = "proportion",
    rho_a = "proportion",
    R = "R_overdispersed",
    kappa = "proportion",
    eta = "proportion",
    nu = "fixed",
    t_ds = "poisson",
    t_da = "poisson",
    t_qcs = "poisson",
    t_qca = "poisson",
    t_qhs = "poisson",
    t_qha = "poisson",
    t_q = "poisson")
params_vartype_bad <- list(
    alpha = "proportion",
    t_qha = "poisson",
    t_q = "poisson")
params_vartype_bad2 <- list(
    alpha = "proportion",
    omega_c = "proportion",
    omega_h = "proportion",
    omega_q = "proportion",
    rho_s = "proportion",
    rho_a = "proportion",
    R = "R_overdispersed",
    kappa = "proportion",
    eta = "proportion",
    nu = "fixed",
    t_ds = "uniform",
    t_da = "poisson",
    t_qcs = "poisson",
    t_qca = "poisson",
    t_qhs = "poisson",
    t_qha = "poisson",
    t_q = "poisson")
params_vartype_bad3 <- list(
    alpha = "proportion",
    omega_c = "proportion",
    omega_h = "proportion",
    omega_q = "proportion",
    rho_s = "proportion",
    rho_a = "proportion",
    R = "R_overdispersed",
    kappa = "proportion",
    eta = "proportion",
    nu = "fixed",
    t_ds = "poisson",
    t_da = "poisson",
    t_qcs = "poisson",
    t_qca = "poisson",
    t_qhs = "poisson",
    t_qha = "gamma",
    t_q = "fixed")

# test_that("Stop if input_list arguments are not specified correctly", {
#   expect_error(get_stochastic_input(
#     2, params_bad, params_vartype_bad
#     ), "specified the correct number")
# })

# test_that("Stop if input arguments are not specified correctly", {
#   expect_warning(get_stochastic_input(
#     2, params_good, params_vartype_bad
#     ), "not provided the same number")
# })

test_that("Warning if invalid distribution specified", {
  expect_warning(get_stochastic_input(
    2, params_good, params_vartype_bad3
    ), "Assuming a fixed value")
})

test_that("Stop if input proportion is mis-specified", {
  expect_error(get_stochastic_input(
    2, params_bad2, params_vartype_good
    ), "supposed to be a proportion")
})

test_that("Stop if input non-negative is mis-specified", {
  expect_error(get_stochastic_input(
    2, params_bad3, params_vartype_bad3
    ), "supposed to be non-negative")
})
