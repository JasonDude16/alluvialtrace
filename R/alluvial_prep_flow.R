#' Title
#'
#' @param data
#' @param steps
#' @param weights
#' @param y_fctr_order
#' @param keep_vars
#' @param curve
#' @param res
#' @param model_fun
#' @param force
#'
#' @return
#' @export
#'
#' @examples
alluvial_prep_flow <- function(data, steps, weights, y_fctr_order = NULL, keep_vars = FALSE, curve = alluvial_curve(),
                               res = 1L, model_fun = alluvial_model, force = FALSE) {

  .alluvial_prep(
    data = data,
    type = "flow",
    id = NULL,
    steps = steps,
    weights = weights,
    y_fctr_order = y_fctr_order,
    values = NULL,
    keep_vars = keep_vars,
    curve = curve,
    res = res,
    model_fun = model_fun,
    force = force,
    add_flows = FALSE
  )

}
