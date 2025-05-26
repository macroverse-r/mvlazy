
in_quick_bubble <- function(ISO,
                           years,
                           option = 1,
                           theme = list(),
                           print = TRUE,
                           filename = NULL
                           ) {

    title <- NULL
    if (is.null(theme$with_title)) { theme$with_title <- FALSE }
    if (is.null(theme$dim)) { theme$dim <- c(14, 10) }
    if (is.null(theme$base_size)) { theme$base_size <- 14 }

    if (option == 1) {
        # Assets: Financial markets and housing
        # FPE_IX is very interesting and quite available
        # ---
        # FPE_IX                     | Financial Market Prices, Equities, Index
        # HOUSE_PRICE_R_2010         | Residential property prices, Real, Index, 2010 = 100
        # TODO: check if FPE_IX is based on current or constant currency
        formula <- c("HOUSE_PRICE_R_2010", "FPE_IX*GDP_R_2015/GDP_C")
        variable <- c("Residential Property Prices", "Financial Market Prices")

        if (theme$with_title) {
            title <- paste0("Financial Markets and Housing Prices - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c(paste0("Index (",years[1]," = 100)"), paste0("Index (",years[1]," = 100)"))
        theme$area <- FALSE
        theme$right_axis <- variable[2]
        if (is.null(theme$align_zeros)) {
            theme$align_zeros <- FALSE
        }

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL, idx = TRUE)


    } else if (option == 2) {
        # Market Capitalization
        # CM_MKT_LCAP_GD_ZS          | Market capitalization of listed domestic companies (% of GDP)
        # CM_MKT_TRAD_GD_ZS          | Stocks traded, total value (% of GDP)
        # CM_MKT_TRNR                | Stocks traded, turnover ratio of domestic shares (%)
        formula <- c("CM_MKT_TRNR", "CM_MKT_TRAD_GD_ZS", "CM_MKT_LCAP_GD_ZS")
        variable <- c("Stocks traded, turnover ratio of domestic shares (%)",
                      "Stocks traded, total value (% of GDP)",
                      "Market capitalization of listed domestic companies (% of GDP)")

        if (theme$with_title) {
            title <- paste0("Market Capitalization and Stock trade - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- NULL
        theme$area <- FALSE
        theme$right_axis <- TRUE

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)

    }

    invisible(output)

}

