test_that("DQC equilibrium with only household contacts", {
  p <- get_dqc_equilibrium(eta = 1)
  expect_equal(p[["Qcds"]] + p[["Qcda"]], 0)
})

test_that("DQC equilibrium with only symptomatic cases", {
  p <- get_dqc_equilibrium(alpha = 0)
  expect_equal(p[["Pa"]] + p[["Qcda"]] + p[["Qhda"]] + p[["Ca"]], 0)
})

test_that("DQC equilibrium with no detection", {
  p <- get_dqc_equilibrium(rho_a = 0, rho_s = 0)
  expect_equal(sum(
    p[["Ps"]], p[["Pa"]], p[["Qcds"]], p[["Qhds"]],
    p[["Qcda"]], p[["Qhda"]]
  ), 0)
})

test_that("DQC equilibrium with no detection, 50% asymptomatic ", {
  p <- get_dqc_equilibrium(rho_a = 0, rho_s = 0, alpha = 0.5)
  expect_equal(sum(
    p[["Ps"]], p[["Pa"]], p[["Qcds"]], p[["Qhds"]],
    p[["Qcda"]], p[["Qhda"]]
  ), 0)
  expect_equal(p[["Cs"]], 0.5)
  expect_equal(p[["Ca"]], 0.5)
})

test_that("DQC equilibrium with 100% household traced, 0% community", {
  p <- get_dqc_equilibrium(omega_h = 1, omega_c = 0, omega_q = 0)
  expect_equal(sum(p[["Qcds"]], p[["Qcda"]], p[["Qq"]]), 0)
})

test_that("DQC equilibrium with no effective contact tracing, 100% household traced and 0% household contacts", {
  p <- get_dqc_equilibrium(omega_h = 1, omega_c = 0, omega_q = 0, eta = 0)
  expect_equal(sum(
    p[["Qcds"]], p[["Qhds"]], p[["Qcda"]], p[["Qhda"]], p[["Qq"]]
    ), 0)
})

test_that("DQC equilibrium with complete detection", {
  p <- get_dqc_equilibrium(rho_s = 1, rho_a = 1)
  expect_equal(sum(
    p[["Ca"]], p[["Cs"]]
    ), 0)
})

test_that("DQC equilibrium with complete detection and no isolation", {
  p <- get_dqc_equilibrium(rho_s = 1, rho_a = 1, omega_c = 0, omega_h = 0, omega_q = 0)
  expect_equal(sum(
    p[["Ps"]], p[["Pa"]]
    ), 1)
})

test_that("DQC equilibrium with complete detection and contact tracing", {
  p <- get_dqc_equilibrium(rho_s = 1, rho_a = 1, omega_c = 1, omega_h = 1, omega_q = 1)
  expect_equal(sum(
    p[["Qcds"]], p[["Qhds"]], p[["Qcda"]], p[["Qhda"]], p[["Qq"]]
    ), 1)
})

