
in_quick_monetary <- function(ISO,
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
        # Interest rates (lot of variability in availability)
        # CB_RATE                    | Central bank policy rates
        # FIACF_PA                   | Financial, Interest Rates, Average Cost of Funds, Percent per Annum
        # FIGB_PA                    | Financial, Interest Rates, Government Securities, Government Bonds, Percent per annum
        # FILR_PA                    | Financial, Interest Rates, Lending Rate, Percent per annum
        # FITB_PA                    | Financial, Interest Rates, Government Securities, Treasury Bills, Percent per annum
        # FPOLM_PA                   | Financial, Interest Rates, Monetary Policy-Related Interest Rate, Percent per annum
        # Complement
        # FR_INR_DPST                | Deposit interest rate (%)
        # FR_INR_LEND                | Lending interest rate (%) # not included bc yearly and less data than FILR_PA which is quarterly
        formula <- c("FPOLM_PA", "CB_RATE",  "FIACF_PA", "FIGB_PA", "FILR_PA", "FITB_PA")
        variable <- c("Policy rate", "Policy rate (2)", "Average Cost of Funds", "Government Bonds", "Lending", "Treasury Bills")

        # add yearly WB data only if asked
        if (isTRUE(theme$add_deposit)) {
            formula <- c(formula, "FR_INR_DPST")
            variable <- c(variable, "Deposit")
        }

        # Variable
        if (theme$with_title) {
            title <- paste0("Interest rates - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "percent per annum"
        if (isTRUE(theme$log)) { theme$y_axis <- paste0("log(",theme$y_axis, ")")}
        theme$area <- FALSE
        theme$clean <- TRUE
        if (is.null(theme$log)) { theme$log <- FALSE }

        post_processing <- function (data) {
            data <- merge_double_var(data)
            if (theme$log) { data$Value <- log(data$Value) }
            return(data)
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing) 


    } else if (option == 2) {
        # CPI_INFL_RATE              | Consumer price index Year-on-year changes (%)
        # formula <- c("CPI_INFL_RATE", "CPI_INFL_RATE/GDP_DEFL_WLD_Y")

        # CPI_INFL_RATE              | Consumer price index Year-on-year changes (%)
        # GDP_DEFL_Y                 | Inflation, GDP deflator (annual %)
        # FP_CPI_TOTL_ZG             | Inflation, consumer prices (annual %)
        # INFL_RR                    | Annual Inflation Rate (numerical)
        # CPI_INFL_RATE  is better (quarterly/BIS) than FP_CPI_TOTL_ZG (yearly/WB) / INFL_RR is the best for yearly data
        formula <- c("CPI_INFL_RATE", "GDP_DEFL_Y")
        variable <- c("Consumer price index (CPI)", "GDP deflator")
        # TODO add an option to divide / minus by the world inflation
        # formula <- c("CPI_INFL_RATE/GDP_DEFL_WLD_Y", "GDP_DEFL_Y/GDP_DEFL_WLD_Y", "FP_CPI_TOTL_ZG/GDP_DEFL_WLD_Y")

        theme$y_axis <- "percent per annum"

        if (isTRUE(theme$broad_money)) {
            formula <- c(formula, "FM_LBL_BMNY_GD_ZS")
            variable <- c(variable, "Broad Money")
            theme$y_axis <- c("percent per annum", "% of GDP")
            theme$right_axis <- variable[3]
        }

        if (isTRUE(theme$log)) { theme$y_axis <- paste0("log(",theme$y_axis, ")")}
        theme$area <- FALSE
        theme$clean <- TRUE
        if (is.null(theme$log)) { theme$log <- FALSE }

        post_processing <- function (data) {
            if (theme$log) { data$Value <- log(data$Value) }
            return(data)
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing) 

    } else if (option == 3) {
        # Monetary sector credit and broad money
        # FM_AST_PRVT_GD_ZS | Monetary Sector credit to private sector (% GDP)
        # Long definition: "Domestic credit to private sector refers to financial resources provided to the private sector, such as through loans, purchases of nonequity securities, and trade credits and other accounts receivable, that establish a claim for repayment. For some countries these claims include credit to public enterprises."
        #
        # FM_AST_NFRG_CN    | Net foreign assets (current LCU)
        # Long definition: "Net foreign assets are the sum of foreign assets held by monetary authorities and deposit money banks, less their foreign liabilities."
        #
        # FM_AST_DOMS_CN    | Net domestic credit (current LCU)
        # Long definition: "Net domestic credit is the sum of net claims on the central government and claims on other sectors of the domestic economy"
        #
        # FM_LBL_BMNY_GD_ZS          | Broad money (% of GDP)
        # Broad money (IFS line 35L..ZK) is the sum of currency outside banks; demand deposits other than those of the central government; the time, savings, and foreign currency deposits of resident sectors other than the central government; bank and traveler’s checks; and other securities such as certificates of deposit and commercial paper.

        formula <- c("FM_LBL_BMNY_GD_ZS",
                     "FM_AST_PRVT_GD_ZS",
                     "100*FM_AST_DOMS_CN/GDP_LCU_Y - FM_AST_PRVT_GD_ZS",
                     "100*FM_AST_NFRG_CN/GDP_LCU_Y")
        variable <- c("Broad money supply (BM)",
                      "Domestic banking credit to private sector (BCP)", 
                      "Domestic banking credit to government (BCG)",
                      "Banking net foreign assets (NFA)")

        comment <- "Explanation: The banking system (central bank + commercial banks) creates money when extending credit through external and domestic channels. NFA represents the net position against the rest of the world, while domestic credit (BCP + BCG) reflects lending to private and public sectors. The banking system's balance sheet is: assets (FA + domestic credit) = liabilities (BM, govt deposits, bank capital, other non-monetary liabilities). Changes in NFA often affect BM unless sterilized through domestic credit adjustments. Conversely, rapid domestic credit growth without NFA growth may signal external pressure. Note that, for some countries, BCP include credit to SEOs.

During currency crises, rapid NFA declines often coincide with BCG expansion as governments defend exchange rates. Banking crises follow a pattern: BCP contracts as private credit freezes, followed by BCG expansion from bailouts. During this phase, BM might contract (deposit runs) or expand (liquidity support), while NFA drops from capital flight. Sovereign debt crises often begin with rising or high BCG levels. As crises unfold, NFA declines from capital outflows and reserve losses. BM might contract (capital controls & monetary tightening). The resolution phase often shows shifts between BCG and BCP (debt restructuring & banking reforms)."


        if (theme$with_title) {
            title <- paste0("Banking System Credit Structure and Money Supply - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)

    } else if (option == 4) {
        # Reserves and net foreign assets
        # quarterly
        formula <- c("100*FM_AST_NFRG_CN/GDP_LCU_Y", "100*RAFA_MV_USD/GDP_C", "100*FER_C/GDP_C")
        variable <- c("Net foreign assets (NFA)", "Total foreign reserves (RES)", "Changes in foreign reserves (ΔRES)")

        # Yearly
        if (isTRUE(theme$yearly_data)) {
            formula <- c("100*FM_AST_NFRG_CN/GDP_LCU_Y", "100*FI_RES_TOTL_CD/GDP_C_Y", "100*BN_RES_INCL_CD/GDP_C_Y")
        }

        comment <- "NFA represents the entire banking system's (central bank + commercial banks) net position against the rest of the world. RES shows only the central bank's gross foreign assets. RES is a component of NFA, as NFA also includes commercial banks' foreign positions and accounts for all foreign liabilities. During currency crises, both NFA and RES typically decline sharply, but NFA might fall more due to additional pressure on commercial banks' foreign positions. Strong positive ΔRES often indicates export surpluses or capital inflows, while negative ΔRES might signal external payment difficulties or defensive intervention."


        if (theme$with_title) {
            title <- paste0("External Position: Net Foreign Assets and Official Reserves - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)

    } else if (option == 5) {
        # Reserves -> interesting to show the large rise in the 90s
        # FI_RES_TOTL_MO             | Total reserves in months of imports
        # FI_RES_TOTL_DT_ZS          | Total reserves (% of total external debt)
        # FI_RES_TOTL_CD             | Total reserves (includes gold, current US$)
        # RAFA_MV_USD       | Total Reserves, US Dollars (Gold at Market Price)
        # "FM_LBL_BMNY_GD_ZS/FM_LBL_BMNY_IR_ZS" == "100*FI_RES_TOTL_CD/GDP_C_Y"
        # but more data for "FI_RES_TOTL_CD/GDP_C_Y"
        formula <- c("100*RAFA_MV_USD/GDP_C_Y", "FI_RES_TOTL_DT_ZS", "FI_RES_TOTL_MO")
        variable <- c("% of GDP", "% of total external debt", "in months of imports")


        # Yearly WB data (rather than quarterly BIS)
        if (isTRUE(theme$yearly_data)) {
            formula[1] <- "100*FI_RES_TOTL_CD/GDP_C_Y"
        }

        if (theme$with_title) {
            title <- paste0("Official Reserves - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- NULL
        theme$area <- FALSE
        theme$right_axis <- TRUE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 6) {
        # Nominal exchange rates
        # in most cases, BIS is just better
        # BIS and IFS are quarterly data, WB is yearly
        if (is.null(theme$fer) || theme$fer == "BIS") {
            formula <- "1/EXCH_RATE_USD"
        } else if (theme$fer == "IFS") {
            formula <- "1/ENDE_XDC_USD_RATE"
        } else if (theme$fer == "WB") {
            formula <- "1/PA_NUS_FCRF"
        }

        variable <- c("Foreign Exchange Rate", "NEER (IMF)", "NEER (Bruegel)")
        formula <- c(formula, "ENEER_IX", "NEER_Y")

        if (theme$with_title) {
            title <- paste0("Nominal Exchange Rate - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- paste0("Index (",years[1]," = 100)")
        theme$area <- FALSE
        theme$clean <- TRUE
        if(is.null(theme$idx)) { theme$idx <- TRUE }
        # if(is.null(theme$right_axis)) { theme$right_axis <- variable[c(1, 2)]}

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       idx = theme$idx, comment = NULL)

    }

    invisible(output)

}



merge_double_var <- function(data, key_variable = c("Policy rate"), min_consecutive = 6) {


    # Test if the two versions of the key_variable are in the dataframe
    if ( sum(c(key_variable, paste0(key_variable, " (2)")) %in% unique(data$Variable)) == 2) {
        # Split data into non-policy rate and policy rate data
        data_1 <- data[!grepl("Policy rate", data$Variable),]
        data_2 <- data[grepl("Policy rate", data$Variable),]

        # Get unique combinations of ISO and Date from original data, excluding ILO
        order_template <- as.data.frame(unique(data_2[!grepl("2", data_2$Variable), c("ISO", "Date", "Variable", "Reference")]))

        # Process the data as before
        results_list <- lapply(key_variable, function(var) {
                                   check_consecutive <- function(x) max(rle(!is.na(x))$lengths[rle(!is.na(x))$values]) >= min_consecutive
                                   valid_regular <- tapply(data_2[data_2$Variable == var, "Value"], data_2[data_2$Variable == var, "ISO"], check_consecutive)
                                   valid_ilo <- tapply(data_2[data_2$Variable == paste0(var, " (2)"), "Value"], 
                                                       data_2[data_2$Variable == paste0(var, " (2)"), "ISO"], check_consecutive)
                                   wide_data <- reshape(data_2[data_2$Variable %in% c(var, paste0(var, " (2)")), ],
                                                        idvar = c("ISO", "Date"), timevar = "Variable", direction = "wide")
                                   vals <- sapply(1:nrow(wide_data), function(i) {
                                                      use_reg <- !is.null(valid_regular[wide_data$ISO[i]]) && valid_regular[wide_data$ISO[i]]
                                                      use_ilo <- !is.null(valid_ilo[wide_data$ISO[i]]) && valid_ilo[wide_data$ISO[i]]
                                                      if(use_reg && use_ilo) mean(unlist(wide_data[i, paste0("Value.", c(var, paste0(var, " (2)")))]), na.rm=TRUE)
                                                      else if(use_reg) wide_data[i, paste0("Value.", var)]
                                                      else if(use_ilo) wide_data[i, paste0("Value.", paste0(var, " (2)"))]
                                                      else NA
                                                        })
                                   data.frame(ISO = wide_data$ISO, Variable = var, Date = wide_data$Date, 
                                              Value = replace(vals, is.nan(vals), NA))
                                       })

        final_result <- as.data.frame(do.call(rbind, results_list))

        # Merge with order template to maintain original order
        merged_result <- merge.data.frame(order_template, final_result, by = c("ISO", "Date", "Variable"), sort = FALSE)

        # Combine with non-policy rate data
        return(rbind(data_1, merged_result))

    } else {
        return(data)
    }


}



