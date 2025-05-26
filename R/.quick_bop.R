.quick_bop <- function(ISO, years,
                         option = 1,
                         theme = list(),
                         print = TRUE,
                         filename = NULL) {
  if (is.null(theme$normalization)) {
    theme$normalization <- "GDP"
  }
  if (is.null(theme$key_dates)) {
    theme$key_dates <- NULL
  }
  if (is.null(theme$sea_adj)) {
    theme$sea_adj <- TRUE
  }
  if (is.null(theme$base_size)) {
    theme$base_size <- 12
  }

  if (option == 1) {
    if (is.null(theme$dim)) {
      theme$dim <- c(12, 12)
    }
    output <- .quick_bop_1(ISO, years, theme, print, filename)
  } else if (option == 2) {
    if (is.null(theme$dim)) {
      theme$dim <- c(18, 12)
    }
    output <- .quick_bop_2(ISO, years, theme, print, filename)
  }

  return(output)
}



#' Create Balance of Payments Visualization
#'
#' @description
#' Creates a three-panel visualization showing Balance of Payments components:
#' Financial Account, Current Account, and Reserves/Financing.
#'
#' @param ISO character; ISO3 country code(s) or country category
#' @param years numeric vector; c(start_year, end_year)
#' @param seasonal_adjustment logical; apply seasonal adjustment (default = TRUE)
#' @param base_size numeric; base font size for plots (default = 12)
#' @param key_dates matrix; event markers with dates (col1: event name, col2: date)
#' @param normalization character; "GDP" or "Inflation" for value scaling
#' @param print logical; display plot (default = TRUE)
#' @param filename character; output file path without extension (default = NULL)
#'
#' @return A combined plot object (invisibly)
#'
#' @examples
#' # Single country example
#' key_dates <- rbind(
#'   c("GFC", "2009-09-15"),
#'   c("CVD", "2020-01-01")
#' )
#' in_plot_bop("USA", c(2000, 2023), key_dates = key_dates)
#'
#' # Multiple countries
#' in_plot_bop(c("USA", "JPN"), c(2000, 2023), normalization = "GDP")
.quick_bop_1 <- function(ISO,
                           years,
                           theme = list(),
                           print = TRUE,
                           filename = NULL) {
  # Parameter validation
  if (!is.character(ISO)) {
    stop("ISO must be a character vector")
  }
  if (!is.numeric(years) || length(years) != 2) {
    stop("years must be a numeric vector of length 2")
  }
  if (!theme$normalization %in% c("GDP", "Inflation")) {
    stop("normalization must be either 'GDP' or 'Inflation'")
  }


  # Set defaults
  theme$area <- TRUE
  theme$bg <- "white"
  theme$x_axis_angle <- 30

  # Determine aggregation based on number of countries
  aggregate_iso <- if (length(isomapper::im_get_category(ISO)) > 1) "Sum" else NULL

  # Fixed subtitles
  subtitle1 <- "   A positive (negative) Financial Account indicates money leaving (entering) the country through financial investments, while a positive (negative) Current
  Account indicates money entering (leaving) through trade and income. Foreign earnings from trade are typically invested abroad, while foreign expenses
  are financed by selling assets to foreigners. "

  subtitle2 <- "Exclude Exceptional Financing. Primary Income includes wages, interest, and dividends. Secondary Income covers transfers like remittances and foreign aid.
  Current Account + Financial Account + Exceptional Financing + Change in Foreign Exchange Reserves ≈ 0 (when Capital Account and Errors are negligible)."

  # Normalization

  # Note: we cannot use "100*FA_C/GDP_C" directly because we want the function to work for one country as well as mutliple
  # In case of multiple countries, we new to sum the gdp and BOP data separately.

  # Helper function for formula suffix
  suffix <- function(vec) {
    add <- if (theme$normalization == "GDP") "_C" else "_R_2023"
    gsub("(\\b\\w+\\b)", paste0("\\1", add), vec, perl = TRUE)
  }

  if (theme$normalization == "GDP") {
    # Get normalization data if needed
    data_gdp <- macrodata::md_data(
      ISO = ISO,
      formula = "GDP_C",
      variable = "GDP",
      years = years,
      adjust_seasonal = theme$sea_adj,
      reference = TRUE,
      aggregate_iso = aggregate_iso,
      na.rm = FALSE,
      clean = FALSE
    )
    unit <- "% of GDP"

    normalize <- function(df) {
      df$Value <- 100 * df$Value / data_gdp$Value
      return(df)
    }
  } else {
    unit <- "Billion 2023 USD"
    normalize <- function(df) {
      df$Value <- df$Value / 1e9
      return(df)
    }
  }


  common <- list(
    ISO = ISO,
    years = years,
    seasonal_adjustment = theme$sea_adj,
    aggregate_iso = aggregate_iso,
    unit = unit,
    key_dates = theme$key_dates,
    theme = theme,
    bg = theme$bg,
    normalize = normalize,
    area = theme$area,
    na2zero = FALSE
  )

  # Financial Account

  formula <- suffix(c("FA", "DER", "oOI-iOI", "oPOR-iPOR", "oFDI-iFDI"))
  variable <- c("Financial Account", "Net Derivatives", "Net Other Investments", "Net Portfolio Investments", "Net FDI")
  title <- c("Financial Account")
  plot_fa <- .create_bop_plot(formula, variable, title, common)

  # Current Account

  formula <- suffix(c("CU", "EXg-IMg", "EXs-IMs", "PIi-PIo", "SIi-SIo"))
  variable <- c("Current Account", "Goods Trade", "Services Trade", "Primary Income", "Secondary Income")
  title <- c("Current Account")
  plot_cu <- .create_bop_plot(formula, variable, title, common, subtitle = subtitle1)

  # Foreign Reserve and Exceptional Financing

  formula <- suffix(c("EO", "CA", "FER", "imfCL+ExF"))
  variable <- c("Errors and Omissions", "Net Capital Account", "Change in Foreign Exchange Reserves", "BOP Support")
  title <- c("Reserves and Support")
  common$area <- FALSE
  common$na2zero <- TRUE
  common$seasonal_adjustment <- FALSE
  plot_fer <- .create_bop_plot(formula, variable, title, common, subtitle = subtitle2)


  # Combine plots
  plot_list <- list(plot_fa, plot_cu, plot_fer)
  if (theme$normalization == "GDP") {
    reference <- paste0("References: IMF's Balance of Payments and International Investment Position; World Bank Indicators")
  } else {
    reference <- paste0("Reference: IMF's Balance of Payments and International Investment Position")
  }

  combined_plot <- pplot::pp_plot_combine(plot_list,
    print = print,
    filename = filename,
    dim = theme$dim,
    reference = reference,
    fontsize_base = theme$base_size,
    ncol = 1
  )

  invisible(combined_plot)
}



.quick_bop_2 <- function(ISO,
                           years,
                           theme = list(),
                           print = TRUE,
                           filename = NULL) {
  # Parameter validation
  if (!is.character(ISO)) {
    stop("ISO must be a character vector")
  }
  if (!is.numeric(years) || length(years) != 2) {
    stop("years must be a numeric vector of length 2")
  }
  if (!theme$normalization %in% c("GDP", "Inflation")) {
    stop("normalization must be either 'GDP' or 'Inflation'")
  }

  # Set defaults
  theme$area <- TRUE
  theme$bg <- "white"

  # Determine aggregation based on number of countries
  aggregate_iso <- if (length(isomapper::im_get_category(ISO)) > 1) "Sum" else NULL



  # Normalization

  # Note: we cannot use "100*FA_C/GDP_C" directly because we want the function to work for one country as well as mutliple
  # In case of multiple countries, we new to sum the gdp and BOP data separately.

  # Helper function for formula suffix
  suffix <- function(vec) {
    add <- if (theme$normalization == "GDP") "_C" else "_R_2023"
    gsub("(\\b\\w+\\b)", paste0("\\1", add), vec, perl = TRUE)
  }

  if (theme$normalization == "GDP") {
    # Get normalization data if needed
    data_gdp <- macrodata::md_data(
      ISO = ISO,
      formula = "GDP_C",
      variable = "GDP",
      years = years,
      adjust_seasonal = theme$sea_adj,
      reference = TRUE,
      aggregate_iso = aggregate_iso,
      na.rm = FALSE,
      clean = FALSE
    )
    unit <- "% of GDP"

    normalize <- function(df) {
      df$Value <- 100 * df$Value / data_gdp$Value
      return(df)
    }
  } else {
    unit <- "Billion 2023 USD"
    normalize <- function(df) {
      df$Value <- df$Value / 1e9
      return(df)
    }
  }


  common <- list(
    ISO = ISO,
    years = years,
    seasonal_adjustment = theme$sea_adj,
    aggregate_iso = aggregate_iso,
    unit = unit,
    key_dates = theme$key_dates,
    theme = theme,
    bg = theme$bg,
    normalize = normalize,
    area = theme$area,
    na2zero = FALSE
  )

  # Financial Account
  formula <- suffix(c("oOID-iOID", "oOID", "-iOID"))
  variable <- c("Net", "Outflows", "Inflows")
  title <- c("Other Debt Invest.")
  plot_iod <- .create_bop_plot(formula, variable, title, common)


  formula <- suffix(c("oOIE-iOIE", "oOIE", "-iOIE"))
  variable <- c("Net", "Outflows", "Inflows")
  title <- c("Other Equity (not securities)")
  plot_ioe <- .create_bop_plot(formula, variable, title, common)

  formula <- suffix(c("oFDI-iFDI", "oFDI", "-iFDI"))
  variable <- c("Net", "Outflows", "Inflows")
  title <- c("FDI")
  plot_fdi <- .create_bop_plot(formula, variable, title, common)

  formula <- suffix(c("oPORD-iPORD", "oPORD", "-iPORD"))
  variable <- c("Net", "Outflows", "Inflows")
  title <- c("Portfolio Debt (mostly Bonds)")
  plot_pord <- .create_bop_plot(formula, variable, title, common)

  formula <- suffix(c("oPORE-iPORE", "oPORE", "-iPORE"))
  variable <- c("Net", "Outflows", "Inflows")
  title <- c("Portfolio Equity (Stocks/Shares)")
  plot_pore <- .create_bop_plot(formula, variable, title, common)

  # Current Account

  formula <- suffix(c("EXg-IMg", "EXg", "-IMg"))
  variable <- c("Net", "Exports", "Imports")
  title <- c("Goods Trade")
  plot_go <- .create_bop_plot(formula, variable, title, common)


  formula <- suffix(c("EXs-IMs", "EXs", "-IMs"))
  variable <- c("Net", "Exports", "Imports")
  title <- c("Services Trade")
  plot_se <- .create_bop_plot(formula, variable, title, common)

  # formula <- suffix(c("TB", "EXs-IMs", "GBT"))
  # variable <- c("Total Trade Balance", "Services Balance", "Goods Balance")
  # title <- c("Trade Components")
  # plot_trade <- .create_bop_plot(formula, variable, title, common)

  # Foreign Reserve and BOP supports
  formula <- suffix(c("imfCL", "ExF", "iOID"))
  variable <- c("Net IMF Credit+Loans", "Exc. Finan.", "Other Debt Instr. Incurred")
  title <- c("IMF")
  common$area <- FALSE
  common$seasonal_adjustment <- FALSE
  plot_imf <- .create_bop_plot(formula, variable, title, common)

  formula <- suffix(c("FER", "imfRE"))
  variable <- c("Reserve Changes", "IMF Reserve Changes")
  title <- c("Foreign Reserves")
  plot_reserves <- .create_bop_plot(formula, variable, title, common)

  # Combine plots
  all_plots <- list(plot_iod, plot_ioe, plot_fdi, plot_pord, plot_pore, plot_imf, plot_go, plot_se, plot_reserves)
  plot_list <- if (!is.null(theme$only)) {
    all_plots[theme$only]
  } else {
    all_plots
  }
  # reference <- paste0("References: ", unique(data_fer$Reference))

  reference <- "   A positive (negative) Financial Account indicates money leaving (entering) the country through financial investments, while a positive (negative) Current Account indicates money
   entering (leaving) through trade and income. Foreign earnings from trade are typically invested abroad, while foreign expenses are financed by selling assets to foreigners. "


  combined_plot <- pplot::pp_plot_combine(plot_list,
    print = print,
    filename = filename,
    dim = theme$dim,
    reference = reference,
    fontsize_base = theme$base_size
  )

  invisible(combined_plot)

}




.create_bop_plot <- function(formula, variable, title, common, subtitle = NULL) {
  # Get and normalize data
  data <- macrodata::md_data(
    ISO = common$ISO,
    formula = formula,
    variable = variable,
    years = common$years,
    adjust_seasonal = common$seasonal_adjustment,
    reference = TRUE,
    aggregate_iso = common$aggregate_iso,
    na.rm = FALSE,
    na2zero = common$na2zero,
    clean = FALSE
  )

  data <- common$normalize(data)

  # Create plot
  plot <- pplot::pp_plot_series(
    data = data,
    y_axis = paste0(title, "\n", common$unit),
    filename = NULL,
    print = FALSE,
    size = 2,
    key_dates = common$key_dates,
    reference = FALSE,
    legend = TRUE,
    by_indicator = TRUE,
    subtitle = subtitle,
    area = common$area,
    bg = common$bg,
    theme = common$theme
  )

  return(plot)
}
