library(brms)

sticker_sales_formula <- function() {
  bf(
    num_sold ~ 1 + year_centered + weekday * is_holiday +
      s(day_of_year, bs = "cc") +
      (1 + year_centered | country) +
      (1 | store) + (1 | product) +
      (1 | country:store) + (1 | country:product) + (1 | store:product) +
      (1 | country:store:product)
  )
}

sticker_sales_family <- function() {
  Gamma(link = "log")
}

sticker_sales_priors <- function() {
  c(
    set_prior("normal(6.4, 1)", class = "Intercept"),
    set_prior("normal(0, 0.5)", class = "b"),
    set_prior("normal(0, 0.2)", class = "b", coef = "year_centered"),
    set_prior("normal(0, 0.5)", class = "sd", group = "country", coef = "Intercept"),
    set_prior("normal(0, 0.1)", class = "sd", group = "country", coef = "year_centered"),
    set_prior("normal(0, 0.5)", class = "sd", group = "store"),
    set_prior("normal(0, 0.5)", class = "sd", group = "product"),
    set_prior("normal(0, 0.3)", class = "sd", group = "country:store"),
    set_prior("normal(0, 0.3)", class = "sd", group = "country:product"),
    set_prior("normal(0, 0.3)", class = "sd", group = "store:product"),
    set_prior("normal(0, 0.2)", class = "sd", group = "country:store:product"),
    set_prior("lkj(2)", class = "cor", group = "country"),
    set_prior("gamma(2, 0.1)", class = "shape")
  )
}
