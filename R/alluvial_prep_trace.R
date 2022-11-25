#' Title
#'
#' @param data
#' @param id
#' @param steps
#' @param fctr_order
#' @param is.long
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
alluvial_prep_trace <- function(data, id, steps, fctr_order = steps, is.long = FALSE, values = NULL, keep_vars = FALSE,
                                add_flows = FALSE, curve = alluvial_curve(), res = 1L, model_fun = alluvial_model,
                                force = FALSE) {

  .alluvial_prep(
    data = data,
    type = "trace",
    id = id,
    steps = steps,
    weights = NULL,
    fctr_order = fctr_order,
    is.long = is.long,
    values = values,
    keep_vars = keep_vars,
    curve = curve,
    res = res,
    model_fun = model_fun,
    force = force,
    compute_flows = add_flows
  )

}
