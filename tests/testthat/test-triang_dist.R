# Cuantiles extremos
expect_equal(qtriang(0, min = 1, max = 3, mode = 2), 1)
expect_equal(qtriang(1, min = 1, max = 3, mode = 2), 3)

# Caso asimétrico: moda desplazada
# Si a=0, b=10, c=2, el cuantil en la moda es (c-a)/(b-a) = 0.2
expect_equal(qtriang(0.2, min = 0, max = 10, mode = 2), 2)
)

test_that("rtriang: random generation", {
  set.seed(123)
  n_samples <- 100
  samples <- rtriang(n_samples, min = 1, max = 5, mode = 3)

  # Debe devolver la longitud correcta [cite: 40]
  expect_length(samples, n_samples)

  # Todos los valores deben estar dentro de [a, b] [cite: 12]
  expect_true(all(samples >= 1 & samples <= 5))

  # Prueba con el argumento n como vector (regla de R)
  expect_length(rtriang(c(1, 2, 3), 1, 5, 3), 3)
})

test_that("Constraints & Error Handling", {
  # min > max debe fallar [cite: 50]
  expect_error(dtriang(x = 2, min = 10, max = 5, mode = 7))

  # mode fuera de [min, max] debe fallar [cite: 50]
  expect_error(ptriang(q = 2, min = 1, max = 5, mode = 6))
  expect_error(ptriang(q = 2, min = 1, max = 5, mode = 0))

  # p fuera de [0, 1] en qtriang debe fallar [cite: 50]
  expect_error(qtriang(p = -0.1, min = 1, max = 3, mode = 2))
  expect_error(qtriang(p = 1.1, min = 1, max = 3, mode = 2))
})

test_that("Vectorization and Recycling", {
  # R debe reciclar parámetros de distinta longitud [cite: 26]
  x_vec <- c(1.5, 2.5)
  res <- dtriang(x_vec, min = 1, max = 3, mode = 2)
  expect_length(res, 2)
  expect_equal(res[1], res[2]) # Triángulo simétrico
})
