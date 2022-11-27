#' Title
#'
#' @param data
#' @param id
#' @param steps
#' @param y_fctr_order
#' @param values
#' @param keep_vars
#' @param add_flows
#' @param curve
#' @param res
#' @param model_fun
#' @param force
#'
#' @return
#' @export
#'
#' @examples
alluvial_prep_trace <- function(data, id, steps, y_fctr_order = NULL, values = NULL, keep_vars = FALSE, add_flows = FALSE,
                                curve = alluvial_curve(), res = 1L, model_fun = alluvial_model, force = FALSE) {

  .alluvial_prep_base(
    data = data,
    type = "trace",
    id = id,
    steps = steps,
    weights = NULL,
    y_fctr_order = y_fctr_order,
    values = values,
    keep_vars = keep_vars,
    curve = curve,
    res = res,
    model_fun = model_fun,
    force = force,
    add_flows = add_flows
  )

}
