#' Title
#'
#' @param data
#' @param steps
#' @param weights
#' @param fctr_order
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
alluvial_prep_flow <- function(data, steps, weights, fctr_order = steps, keep_vars = FALSE, curve = alluvial_curve(),
                               res = 1L, model_fun = alluvial_model, force = FALSE) {

  .alluvial_prep(
    data = data,
    type = "flow",
    id = NULL,
    steps = steps,
    weights = weights,
    fctr_order = fctr_order,
    is.long = FALSE,
    values = NULL,
    keep_vars = keep_vars,
    curve = curve,
    res = res,
    model_fun = model_fun,
    force = force,
    compute_flows = FALSE
  )

}
