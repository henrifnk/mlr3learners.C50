#' @title Classification C50 Learner
#'
#' @name mlr_learners_classif.C50
#'
#' @description
#' A [mlr3::LearnerClassif] for a classification random from package \CRANpkg{C50}.
#' Calls [C50::C5.0()].
#'
#' @references
#' Quinlan R (1993).
#' C4.5: Programs for Machine Learning
#' \url{http://www.rulequest.com/see5-unix.html}
#'
#' @export
LearnerClassifC5.0 = R6Class("LearnerClassifC50",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.C50") {
      ps = ParamSet$new( # parameter set using the paradox package
        params = list(
          ParamInt$new(id = "trials", default = 1L, lower = 1L, tags = c("train", "predict")),
          ParamLgl$new(id = "rules", default = FALSE, tags = "train"),
          ParamLgl$new(id = "subset", default = FALSE, tags = "train"),
          ParamInt$new(id = "bands", lower = 2L, upper = 1000L, tags = "train"),
          ParamLgl$new(id = "winnow", default = FALSE, tags = "train"),
          ParamLgl$new(id = "noGlobalPruning", default = FALSE, tags = "train"),
          ParamDbl$new(id = "CF", lower = 0, upper = 1, default = 0.25, tags = "train"),
          ParamInt$new(id = "minCases", lower = 0L, upper = Inf, default = 2L, tags = "train"),
          ParamLgl$new(id = "fuzzyThreshold", default = FALSE, tags = "train"),
          ParamDbl$new(id = "sample", lower = 0, upper = .999, default = 0, tags = "train"),
          ParamInt$new(id = "seed", lower = -Inf, upper = Inf, tags = "train"),
          ParamLgl$new(id = "earlyStopping", default = TRUE, tags = "train")
        )
      )
      
      super$initialize(
        # see the mlr3book for a description: https://mlr3book.mlr-org.com/extending-mlr3.html
        id = id,
        packages = "C50",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties =c("twoclass", "multiclass", "missings", "weights")
     
        )
      }
    ),


  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      # Get formula, data, classwt, cutoff for the randomForest
      f = task$formula() #the formula is available in the task
      data = task$data() #the data is avail
      # use the mlr3misc::invoke function (it's similar to do.call())
      invoke(C50::C5.0.formula, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      self$predict_type = "response"
      response = NULL
      prob = NULL
      pars = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names) # get newdata
      
      if(self$predict_type == "response") {
        response = invoke(predict, self$model, newdata = newdata,
                          type = "class", .args = pars)
      } else {
        prob = invoke(predict, self$model, newdata = newdata,
                          type = "prob", .args = pars)
      }

      # Return a prediction object with PredictionClassif$new()
      PredictionClassif$new(task = task, response = response, prob = prob)
    }
    )
  )
