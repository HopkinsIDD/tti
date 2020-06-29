test_that("get_r_effective works", {
  pqc <- c(Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
           Cs = 0.8, Ca = 0.2)
  inf <- get_infect_mat()
  expect_equal(sum(pqc %*% inf), get_r_effective(pqc))
})

test_that("get_r_effective works, change alpha", {
  param <- 0.5
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(alpha = param)
  r <- get_r_effective(pqc, alpha = param)
  expect_equal(sum(pqc %*% inf), r)
})

test_that("get_r_effective works, change kappa", {
  param <- 0.9
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(kappa = param)
  r <- get_r_effective(pqc, kappa = param)
  expect_equal(sum(pqc %*% inf), r)
})

test_that("get_r_effective works, change eta", {
  param <- 0.9
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(eta = param)
  r <- get_r_effective(pqc, eta = param)
  expect_equal(sum(pqc %*% inf), r)
})

test_that("get_r_effective works, change nu", {
  param <- 0.9
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.15, Qhps = 0.05, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(nu = param)
  r <- get_r_effective(pqc, nu = param)
  expect_equal(sum(pqc %*% inf), r)
})

test_that("get_r_effective works, change t_ps", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_ps = param)
  r <- get_r_effective(pqc, t_ps = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_pa", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_pa = param)
  r <- get_r_effective(pqc, t_pa = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qcs", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_qcs = param)
  r <- get_r_effective(pqc, t_qcs = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qca", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_qca = param)
  r <- get_r_effective(pqc, t_qca = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qhs", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_qhs = param)
  r <- get_r_effective(pqc, t_qhs = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qha", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_qha = param)
  r <- get_r_effective(pqc, t_qha = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_q", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_q = param)
  r <- get_r_effective(pqc, t_q = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change incubation", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(t_incubation = param)
  r <- get_r_effective(pqc, t_incubation = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change shape of gamma", {
  param <- 4
  pqc <- c(Ps = 0.2, Pa = 0.1, Qcps = 0.1, Qhps = 0.1, Qcpa = 0.1, Qhpa = 0.1,
           Qq = 0.15, Cs = 0.1, Ca = 0.05)
  inf <- get_infect_mat(shape = param)
  r <- get_r_effective(pqc, shape = param)
  r2 <- get_r_effective(pqc)
  expect_equal(sum(pqc %*% inf), r)
  expect_true(r < r2)
})

test_that("get_r_effective works, change initial R", {
  R <- 5
  pqc <- c(Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
           Cs = 0.8, Ca = 0.2)
  inf <- get_infect_mat(R = R)
  expect_equal(sum(pqc %*% inf), get_r_effective(pqc, R = R))
  expect_equal(R, get_r_effective(pqc, R = R))
})

