#' Quick Economic Analysis Visualizations
#'
#' @description
#' Generate pre-configured visualizations for common macroeconomic analysis workflows.
#' This function provides ready-to-use templates for various economic indicators.
#'
#' @param ISO Character vector of ISO3 country codes or country categories
#' @param years Numeric vector of length 2 specifying the year range c(start_year, end_year)
#' @param graph Character string specifying the type of analysis. Options include:
#'   "bop" (balance of payments), "reer" (real effective exchange rate),
#'   "credit", "trade", "fiscal", "investment", "integration", "labour",
#'   "bubble", "impact", "production", "monetary"
#' @param option Integer specifying the visualization variant (default = 1)
#' @param theme List of theme options for customization
#' @param print Logical, whether to display the plot (default = TRUE)
#' @param filename Character string for saving the plot (optional)
#'
#' @return A plot object (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Balance of payments analysis
#' ml_quick("USA", c(2010, 2023), graph = "bop")
#' 
#' # Multiple countries with custom theme
#' ml_quick(c("USA", "CHN", "DEU"), c(2010, 2023), 
#'          graph = "trade",
#'          theme = list(normalization = "GDP"))
#' }
ml_quick <- function(ISO, 
                     years, 
                     graph = "bop",
                     option = 1,
                     theme = list(),
                     print = TRUE,
                     filename = NULL) {
    
    # Define default theme settings
    default_theme <- list(
        na2zero = FALSE,
        match_tot = FALSE,
        with_title = TRUE,
        normalization = "GDP",
        sea_adj = FALSE,
        key_dates = NULL,
        dim = NULL,
        explanation = TRUE
    )
    
    # Merge user-provided theme with defaults
    theme <- modifyList(default_theme, theme)

    # Input validation
    .validate_inputs(ISO, years, theme, graph)


    # Define supported graph types and their corresponding functions
    graph_functions <- list(
        bop = .quick_bop,
        reer = .quick_reer,
        credit = .quick_credit,
        trade = .quick_trade,
        fiscal = .quick_fiscal,
        investment = .quick_investment,
        integration = .quick_integration,
        labour = .quick_labour,
        bubble = .quick_bubble,
        impact = .quick_impact,
        production = .quick_production,
        monetary = .quick_monetary
    )
    
    # Check if requested graph type exists
    if (!graph %in% names(graph_functions)) {
        stop(sprintf("Unsupported graph type: %s. Supported types are: %s", 
                    graph, paste(names(graph_functions), collapse = ", ")))
    }
    
    # Call the appropriate plotting function
    output <- graph_functions[[graph]](
        ISO = ISO,
        years = years,
        theme = theme,
        option = option,
        print = print,
        filename = filename
    )

    invisible(output)
}




# Helper function for input validation
.validate_inputs <- function(ISO, years, theme, graph) {
    if (!is.character(ISO)) {
        stop("ISO must be a character vector")
    }
    
    if (!is.numeric(years)) {
        stop("years must be numeric")
    }
    
    if (length(years) != 2) {
        stop("years must be a vector of length 2")
    }
    
    if (years[1] >= years[2]) {
        stop("First year must be less than second year")
    }
    
    if (!theme$normalization %in% c("GDP", "Inflation", "None")) {
        stop("normalization must be one of: 'GDP', 'Inflation', 'None'")
    }
    
    valid_graphs <- c("bop", "reer", "credit", "trade", "fiscal",
                      "investment", "labour", "bubble", "impact",
                      "production", "monetary", "integration", "others")
    if (!graph %in% valid_graphs) {
        stop(sprintf("Invalid graph type. Must be one of: %s", 
                    paste(valid_graphs, collapse = ", ")))
    }
}


# TODO 
# 1. complement + put all 'credit' into 'bubble' ?
# 2. replace .create_bop_plot by .quick_helper()

