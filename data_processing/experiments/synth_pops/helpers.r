get_model_params <- function(model_list, item) {
  unlist(lapply(model_list, function(x) {
    x[[item]]
  }))
}

sum_of_square_error <- function(y, y_hat) {
  sum(abs(y_hat - y) * abs(y_hat - y))
}
