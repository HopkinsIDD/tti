test_that("get_r_effective works", {
  dqc <- c(
    Ds = 0, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0.8, Ca = 0.2
  )
  inf <- get_infect_mat()
  expect_equal(sum(dqc %*% inf), get_r_effective(dqc))
})

test_that("get_r_effective works, change alpha", {
  param <- 0.5
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(alpha = param)
  r <- get_r_effective(dqc, alpha = param)
  expect_equal(sum(dqc %*% inf), r)
})

test_that("get_r_effective works, change kappa", {
  param <- 0.9
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(kappa = param)
  r <- get_r_effective(dqc, kappa = param)
  expect_equal(sum(dqc %*% inf), r)
})

test_that("get_r_effective works, change eta", {
  param <- 0.9
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(eta = param)
  r <- get_r_effective(dqc, eta = param)
  expect_equal(sum(dqc %*% inf), r)
})

test_that("get_r_effective works, change nu", {
  param <- 10
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.15, Qhds = 0.05, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(nu = param)
  r <- get_r_effective(dqc, nu = param)
  expect_equal(sum(dqc %*% inf), r)
})

test_that("get_r_effective works, change t_ds", {
  param <- 4
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_ds = param)
  r <- get_r_effective(dqc, t_ds = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_da", {
  param <- 4
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_da = param)
  r <- get_r_effective(dqc, t_da = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qcs", {
  param <- 6
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_qcs = param)
  r <- get_r_effective(dqc, t_qcs = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qca", {
  param <- 6
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_qca = param)
  r <- get_r_effective(dqc, t_qca = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qhs", {
  param <- 6
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_qhs = param)
  r <- get_r_effective(dqc, t_qhs = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_qha", {
  param <- 6
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_qha = param)
  r <- get_r_effective(dqc, t_qha = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change t_q", {
  param <- 6
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_q = param)
  r <- get_r_effective(dqc, t_q = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change incubation", {
  param <- 2
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(t_incubation = param)
  r <- get_r_effective(dqc, t_incubation = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r > r2)
})

test_that("get_r_effective works, change shape of gamma", {
  param <- 4
  dqc <- c(
    Ds = 0.2, Da = 0.1, Qcds = 0.1, Qhds = 0.1, Qcda = 0.1, Qhda = 0.1,
    Qq = 0.15, Cs = 0.1, Ca = 0.05
  )
  inf <- get_infect_mat(shape = param)
  r <- get_r_effective(dqc, shape = param)
  r2 <- get_r_effective(dqc)
  expect_equal(sum(dqc %*% inf), r)
  expect_true(r < r2)
})

test_that("get_r_effective works, change initial R", {
  R <- 5
  dqc <- c(
    Ds = 0, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0.8, Ca = 0.2
  )
  inf <- get_infect_mat(R = R)
  expect_equal(sum(dqc %*% inf), get_r_effective(dqc, R = R))
  expect_equal(R, get_r_effective(dqc, R = R))
})
