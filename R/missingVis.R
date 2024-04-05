#' Title
#'
#' @param dataset The dataset you want to check
#'
#' @return A barplot
#' @export
#'
#'
#' @examples
#' x <- mtcars
#' missingVis(x)
#' 
missingVis <- function(dataset) {
  
  missing_rate <- tibble::as_tibble(cbind(skimr::skim(dataset)[2],1-skimr::skim(dataset)[4]))
  
  missing_rate |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = reorder(skim_variable,-complete_rate), y = complete_rate*100)
    ) +
    ggplot2::geom_bar(stat = "identity",fill = "steelblue") +
    ggplot2::coord_flip() + # when the features of interest are large
    ggplot2::theme_linedraw() +
    ggplot2::geom_text(ggplot2::aes(label = round(complete_rate*100,2)), hjust = 1.2, vjust = 0.5, color = "white", size = 3.5, fontface = "bold") +
    ggplot2::labs(x = "Features", y = "Missing data percentages (%)", 
         title = "EDA: Missing data percentages by features.") 

}