#' Nice-looking missing value rates barplots. 
#'
#' @param dataset The dataset of interest, with the type of dataframe preferred. 
#'
#' @return A barplot presenting bars of missing values percentages for each variable
#' 
#' @importFrom stats reorder
#' 
#' @export
#' @examples
#' x <- mtcars
#' missingVis(x)
#' 
missingVis <- function(dataset) {
  stopifnot("The input your provided should be a dataframe" =  is.data.frame(dataset))
  missing_rate <- tibble::as_tibble(cbind(skimr::skim(dataset)[2],1-skimr::skim(dataset)[4]))
  missing_rate |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = reorder(skim_variable,-complete_rate), y = complete_rate*100)
    ) +
    ggplot2::geom_bar(stat = "identity",fill = "steelblue") +
    ggplot2::coord_flip() + 
    ggplot2::theme_linedraw() +
    ggplot2::geom_text(ggplot2::aes(label = round(complete_rate*100,2)), hjust = 1.2, vjust = 0.5, color = "white", size = 3.5, fontface = "bold") +
    ggplot2::labs(x = "Features", y = "Missing data percentages (%)", 
         title = "EDA: Missing data percentages by features.") 
}