#' Summary statistics
#'
#' @param data, a dataframe
#'
#' @return A data.frame/tibble
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}

#' Plotting metabolite statistics
#'
#' @param data a datframe / tibble
#'
#' @return a ggplot2 plot
plot_distributions <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = value)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free")
}



#' Make columns snakecase in a dataframe
#'
#' @param data a ataframe with string of columns
#' @param cols columns to convert into snake case
#'
#' @return a dataframe / tibble
column_values_to_snake_case <- function(data, cols) {
  data %>%
    mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}

#' Pivot table wider
#'
#' @param data a dataframe
#'
#' @return a pivoted dataframe
metabolites_to_wider <- function(data) {
  data %>% tidyr::pivot_wider(
    names_from = metabolite,
    values_from = value,
    values_fn = mean, names_prefix = "metabolite_"
  )
}

#' A transformation recipe to pre-process the data.
#'
#' @param data The lipidomics dataset
#' @param metabolite_variable The column of the metabolite variable
#'
#' @return recipe with specifications
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }},
      age,
      gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}


#' Create a workflow object of the model and transformations.
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}

#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted.
#'
#' @return A data frame.
tidy_model_output <- function(workflow_fitted_model){
    workflow_fitted_model %>%
        workflows::extract_fit_parsnip() %>%
        broom::tidy(exponentiate = TRUE)
}
