

in_quick_reer <- function(ISO,
                           years,
                           option = 1,
                           theme = list(),
                           print = TRUE,
                           filename = NULL
                           ) {



    if (theme$explanation) {
        comment <- paste0("Currency movements (NEER) and relative prices (Pdomestic/Pforeign) determine competitiveness (REER).
REER: Decrease = improved competitiveness, NEER: Decrease = currency depreciation, Relative price: Decrease = lower domestic relative prices
log values to % changes from ",years[1],": 1.5 = + 348% / 1 = +171.8% / 0.5 = +64.9% / 0.1 = +10.5% / -0.1 = -9.5% / -0.5 = -39.3% / -1 = -63.2% / -2 = -86%")
    } else {
        comment <- NULL
    }

    if (is.null(theme$y_axis)) { theme$y_axis <- paste0("ln(index,", years[1], "=1)") }

    if (option %in% c(1, 2)) {
        if (is.null(theme$dim)) { theme$dim <- c(14, 9) }
        if (is.null(theme$base_size)) {theme$base_size <- 14 }

        variable <- c("REER", "NEER", "Relative prices")

        if (option %in% c("Bruegel", 1)) {
            formula <- c("REER_Y", "NEER_Y", "REER_Y/NEER_Y")
        } else if (option %in% c("IFS", 2)) {
            formula <- c("EREER_IX", "ENEER_IX", "EREER_IX/ENEER_IX")
        }

        title <- paste0("Real Effective Exchange Rate - ", wp_from_iso(ISO))
        output <- in_quick_helper(ISO = ISO, 
                                       years = years, 
                                       formula = formula, 
                                       variable = variable, 
                                       theme = theme, 
                                       title = title, 
                                       print = print, 
                                       filename = filename, 
                                       idx = "log", 
                                       comment = comment) 


    } else if (option == 3) {

        if (is.null(theme$dim)) { theme$dim <- c(18, 12) }
        if (is.null(theme$base_size)) { theme$base_size <- 12 }
        if (is.null(theme$ref_width_adj_scale)) { theme$ref_width_adj_scale <- 0.5 }
        

        title <- "REER decomposition - Bruegel"
        variable <- c("REER", "NEER", "Relative prices")
        formula <- c("REER_Y", "NEER_Y", "REER_Y/NEER_Y")
        out_bruegel <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = FALSE, filename = NULL, 
                                       idx = "log", comment = NULL) 

        title <- "REER decomposition - IFS"
        variable <- c("REER", "NEER", "Relative prices")
        formula <- c("EREER_IX", "ENEER_IX", "EREER_IX/ENEER_IX")
        out_ifs <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = FALSE, filename = NULL, 
                                       idx = "log", comment = NULL) 


        title <- "Comparison Real Exchange Rate"
        variable <- c("REER by IFS", "REER by Bruegel (CPI)", "REER by WB")
        formula <- c("EREER_IX", "REER_Y", "PX_REX_REER")
        theme$area <- FALSE

        out_reer <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = FALSE, filename = NULL, 
                                       idx = "log", comment = NULL) 


        title <- "Comparison Nominal Exchange Rate"
        variable <- c("NEER by IFS", "NEER by Bruegel", "FER (USD) by BIS", "FER (USD) by IFS", "FER (USD) by WB")
        formula <- c("ENEER_IX", "NEER_Y", "EXCH_RATE_USD", "ENDE_XDC_USD_RATE", "PA_NUS_FCRF")
        post_processing <- function(data) {
            to_inverse <- c(3, 4, 5)
            for (i in to_inverse) {
                data[data$Variable == variable[i], ]$Value <- 1/data[data$Variable == variable[i], ]$Value
            }
            return(data)
        }

        out_neer <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = FALSE, filename = NULL, 
                                       idx = "log", comment = NULL, post_processing = post_processing)



        title <- "Relative Price"
        # could also include: "PA_NUS_PPPC_RF",
        formula <- c("EREER_IX/ENEER_IX", "REER_Y/NEER_Y", "PX_REX_REER/(1/PA_NUS_FCRF)", "GDP_DEFL_Y", "PRC_IDX_WLD_Y")
        variable <- c("REER/NEER by IFS", "REER/NEER by Bruegel", "REER/FER by WB", "GDP_DEFL_Y", "PRC_IDX_WLD_Y")

        post_processing <- function(data) {
            data[data$Variable == "GDP_DEFL_Y",]$Value <- cumprod(1 + data[data$Variable == "GDP_DEFL_Y",]$Value/100)
            data[data$Variable == "GDP_DEFL_Y",]$Value <- data[data$Variable == "GDP_DEFL_Y",]$Value/data[data$Variable == "PRC_IDX_WLD_Y",]$Value
            data <- data[data$Variable != "PRC_IDX_WLD_Y", ]
            data[data$Variable == "GDP_DEFL_Y",]$Variable <- "GDP defl. idx wrt WLD by WB"
            return(data)
        }

        out_price <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = FALSE, filename = NULL, 
                                       idx = "log", comment = NULL, post_processing = post_processing)

        plot_list <- list(out_bruegel$plot, out_ifs$plot, out_reer$plot, out_neer$plot, out_price$plot)
        plot <- wp_plot_combine(plot_list, 
                                dim = theme$dim, ncol = 2, reference = comment)

        data <- list(bruegel = out_bruegel$data, ifs = out_ifs$data)
        output <- list(plot = plot, data = data)
    }


    return(output)

}



in_plot_reer_complement <- function(ISO,
                     years, 
                     seasonal_adjustment = FALSE,
                     theme = list(base_size = 12),
                     key_dates = NULL,
                     dim = c(18, 10),
                     print = TRUE,
                     filename = NULL, 
                     option = "REER") {



    if (option == "REER") {
        ##########################################################################################################################
        # Real Exchange Rates
        title <- "Comparison Real Exchange Rate"
        formula <<- c("EREER_IX", "REER_Y", "PX_REX_REER")
        variable <<- c("IFS:EREER_IX", "Bruegel:NEER_Y", "WB(O):PX_REX_REER")

        data <- wp_data(ISO = ISO,
                        formula = formula,
                        variable = variable,
                        years = years,
                        adjust_seasonal = seasonal_adjustment,
                        reference = TRUE,
                        clean = FALSE
        )

    } else if (option == "NEER") {
        ##########################################################################################################################
        # Nominal exchange rates
        title <- "Comparison Nominal Exchange Rate"
        formula <- c("ENEER_IX", "NEER_Y", "EXCH_RATE_USD", "ENDE_XDC_USD_RATE", "PA_NUS_FCRF")
        variable <- c("IFS:ENEER_IX", "Bruegel:NEER_Y", "BIS(O):EXCH_RATE_USD", "IFS(O):ENDE_XDC_USD_RATE", "WB(O):PA_NUS_FCRF")
        to_reverse <- c(3, 4, 5)


        data <- wp_data(ISO = ISO,
                        formula = formula,
                        variable = variable,
                        years = years,
                        adjust_seasonal = seasonal_adjustment,
                        reference = TRUE,
                        clean = FALSE
        )

        for (i in to_reverse) {
            data[data$Variable == variable[i], ]$Value <- 1/data[data$Variable == variable[i], ]$Value
        }
        dd <<- data

    } else if (option == "Price") {
        ##########################################################################################################################
        # Price level

        title <- "Relative Price"
        formula <- c("EREER_IX/ENEER_IX", "REER_Y/NEER_Y", "1/(PX_REX_REER/PA_NUS_FCRF)", "PA_NUS_PPPC_RF", "GDP_DEFL_Y", "PRC_IDX_WLD_Y")

        variable <- c("IFS:EREER_IX/", "Bruegel:REER_Y/", "WB:PX_REX_REER/", "PA_NUS_PPPC_RF", "GDP_DEFL_Y", "PRC_IDX_WLD_Y")

        data <- wp_data(ISO = ISO,
                        formula = formula,
                        variable = variable,
                        years = years,
                        adjust_seasonal = seasonal_adjustment,
                        reference = TRUE,
                        clean = FALSE
        )


        data[data$Variable == "GDP_DEFL_Y",]$Value <- cumprod(1 + data[data$Variable == "GDP_DEFL_Y",]$Value/100)

        data[data$Variable == "GDP_DEFL_Y",]$Value <- data[data$Variable == "GDP_DEFL_Y",]$Value/data[data$Variable == "PRC_IDX_WLD_Y",]$Value

        data <- data[data$Variable != "PRC_IDX_WLD_Y", ]
    }


    data <- in_log_transformation_idx(data)

    plot <- wp_plot_series(data = data, 
               filename = NULL,
               print = print,
               size = 1,
               key_dates = key_dates,
               reference = TRUE,
               legend = TRUE,
               theme = theme,
               by_indicator = FALSE,
               title = title,
               area = FALSE
    )

    invisible(plot)

}
