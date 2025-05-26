
in_quick_fiscal <- function(ISO,
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
        # GC_REV_XGRT_GD_ZS          | Revenue, excluding grants (% of GDP)
        # GC_TAX_TOTL_GD_ZS          | Tax revenue (% of GDP)
        # GC_XPN_TOTL_GD_ZS          | Expense (% of GDP)
        formula <- c("GC_XPN_TOTL_GD_ZS", "GC_REV_XGRT_GD_ZS-GC_TAX_TOTL_GD_ZS", "GC_TAX_TOTL_GD_ZS")
        variable <- c("Expense", "Non-tax revenue", "Tax revenue")

        theme$y_axis <- c("% of GDP")

        comment <- "Non-tax revenue includes profits from state-owned enterprises (SOEs), royalties from natural resources, user charges and fees for services (e.g., passport fees and university tuition), fines, sale of government bonds and securities, and investment earnings (e.g., foreign exchange reserves or sovereign wealth funds) - not grants (on this figure). Resource royalties and SOE profits typically being the largest contributors."
        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)



    } else if (option == 2) {
        # Main decomposition of expenses
        # GC_XPN_COMP_CN    | Compensation of employees (current LCU)
        # GC_XPN_COMP_ZS    | Compensation of employees (% of expense)
        # GC_XPN_GSRV_ZS    | Goods and services expense (% of expense)
        # GC_XPN_INTP_CN    | Interest payments (current LCU)
        # GC_XPN_INTP_RV_ZS | Interest payments (% of revenue)
        # GC_XPN_INTP_ZS    | Interest payments (% of expense)
        # GC_XPN_OTHR_ZS    | Other expense (% of expense)
        # GC_XPN_TOTL_GD_ZS | Expense (% of GDP)
        # GC_XPN_TRFT_ZS    | Subsidies and other transfers (% of expense)
        if (is.null(theme$normalization) || theme$normalization == "None") {
            theme$normalization <- "None" 
            theme$y_axis <- "% of expense"
            if (is.null(theme$incl_others) || theme$incl_others) { 
                print("incl_others")
                theme$incl_others <- TRUE
                formula <- c("GC_XPN_COMP_ZS+GC_XPN_GSRV_ZS+GC_XPN_INTP_ZS+GC_XPN_OTHR_ZS+GC_XPN_TRFT_ZS", 
                             "GC_XPN_TRFT_ZS",  "GC_XPN_OTHR_ZS",
                             "GC_XPN_COMP_ZS", "GC_XPN_GSRV_ZS", "GC_XPN_INTP_ZS")
                variable <- c("Total", "Subsidies and other transfers", "Other expenses", "Compensation of employees", 
                              "Goods and services expense", "Interest Payments")

            } else if (!theme$incl_others) {
                print(" not incl_others")
                formula <- c("GC_XPN_COMP_ZS+GC_XPN_GSRV_ZS+GC_XPN_INTP_ZS", 
                             "GC_XPN_COMP_ZS", "GC_XPN_GSRV_ZS", "GC_XPN_INTP_ZS")
                variable <- c("Total", "Compensation of employees", 
                              "Goods and services expense", "Interest Payments")
            }
        } else if (theme$normalization == "GDP") {
            theme$y_axis <- "% of GDP"
            if (is.null(theme$incl_others) || theme$incl_others) { 
                theme$incl_others <- TRUE
                formula <- c("GC_XPN_TOTL_GD_ZS", 
                             "(GC_XPN_TRFT_ZS*GC_XPN_TOTL_GD_ZS/100)", 
                             "(GC_XPN_OTHR_ZS*GC_XPN_TOTL_GD_ZS/100)",
                             "(GC_XPN_COMP_CN*100/GDP_LCU_Y)", 
                             "(GC_XPN_GSRV_ZS*GC_XPN_TOTL_GD_ZS/100)", 
                             "(GC_XPN_INTP_CN*100/GDP_LCU_Y)")
                variable <- c("Total", "Subsidies and other transfers", "Other expenses", "Compensation of employees", 
                              "Goods and services expense", "Interest Payments")
            } else if (!theme$incl_others) {
                formula <- c("(GC_XPN_COMP_CN*100/GDP_LCU_Y)", "(GC_XPN_GSRV_ZS*GC_XPN_TOTL_GD_ZS/100)", "(GC_XPN_INTP_CN*100/GDP_LCU_Y)")
                variable <- c("Total", "Compensation of employees", 
                              "Goods and services expense", "Interest Payments")
                formula <- c(paste0(formula, collapse = " + "), formula)
            }
        }

        if (theme$with_title) {
            title <- paste0("Government Expense - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL) 


    } else if (option == 3) {
        # GC_XPN_INTP_RV_ZS          | Interest payments (% of revenue)
        # GC_XPN_INTP_ZS             | Interest payments (% of expense)
        # Two best exports measure of yearly data
        # formula <- c("NE_EXP_GNFS_CD/1e9", "BX_GSR_GNFS_CD/1e9")

        # Interest payments with different denominator
        formula <- c("100*GC_XPN_INTP_CN/GDP_LCU_Y", "GC_XPN_INTP_ZS", "GC_XPN_INTP_RV_ZS", 
                 "100*(100*GC_XPN_INTP_CN/GDP_LCU_Y)/NE_EXP_GNFS_ZS")
        variable <- c("% of GDP", "% of gvt expense", "% of revenue", "% of exports")
        if (theme$with_title) {
            title <- paste0("Interest Payments - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("%", "%")
        theme$right_axis <- "% of GDP"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL) 

    } else if (option == 4) {
        # Decomposition expenditure 2
        # MS_MIL_XPND_GD_ZS          | Military expenditure (% of GDP)
        # GC_XPN_TOTL_GD_ZS          | Expense (% of GDP)
        # GC_XPN_INTP_ZS             | Interest payments (% of expense)
        # GB_XPD_RSDV_GD_ZS          | Research and development expenditure (% of GDP)
        # SE_XPD_TOTL_GD_ZS          | Government expenditure on education, total (% of GDP)
        # SH_XPD_CHEX_GD_ZS          | Current health expenditure (% of GDP)
        # SH_XPD_GHED_GD_ZS          | Domestic general government health expenditure (% of GDP)

        formula <- c("MS_MIL_XPND_GD_ZS", "GC_XPN_INTP_ZS*GC_XPN_TOTL_GD_ZS/100", "SE_XPD_TOTL_GD_ZS", "SH_XPD_GHED_GD_ZS", "GB_XPD_RSDV_GD_ZS")
        variable <- c("Military Exp.", "Interest Payments", "Government Education Exp.", "Government health Exp.", "R&D Exp.")
        if (theme$with_title) {
            title <- paste0("Main government expense - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = "Note: Unlike other indicators, R&D expenditure includes both public and private spending.")



    } else if (option == 5) {
        # Gross National Expenditure (GNE) = Final consumption expenditure (FCE) + Gross Capital Formation
        # /!\ interesting to see the use of resource during the booms
        # GDP = Final Consumption + Gross Capital Formation + (Exports - Imports)
        # NE_CON_TOTL_ZS             | Final consumption expenditure (% of GDP)
        # NE_DAB_TOTL_ZS             | Gross national expenditure (% of GDP)
        # NE_GDI_TOTL_ZS             | Gross capital formation (% of GDP)
        if (is.null(theme$net_export)) {
            net_export <- "(NE_EXP_GNFS_ZS-NE_IMP_GNFS_ZS)"
        } else if (theme$net_export == "BN") {
            net_export <- "(100*BN_GSR_GNFS_CD/GDP_C_Y)"
        } else if (theme$net_export == "diff") {
            net_export <- "100-NE_DAB_TOTL_ZS"

        }
        formula <- c("NE_DAB_TOTL_ZS", net_export, "NE_CON_PRVT_ZS", "NE_CON_GOVT_ZS", "NE_GDI_TOTL_ZS")
        variable <- c("Gross National Expenditure", "Exports-Imports", "Final Households Consumption", "Final Government Consumption", "Gross Capital Formation")

        if (theme$with_title) {
            title <- paste0("Gross National Expenditure - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"
        comment <- "Final Total Consumption (C = Household Consumption + Government Consumption) plus Gross Capital Formation (I) equals Gross National Expenditure (GNE). The difference between GDP and GNE is Net Exports (NX), giving us GDP = C + I + NX = GNE + NX. When NX is negative, domestic spending exceeds domestic production. This gap may be financed through net income from foreign assets or, if Current Account < 0, through foreign borrowing or asset sales."

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)


    } else if (option == 6) {
        # GC_REV_GOTR_ZS             | Grants and other revenue (% of revenue)
        # GC_TAX_GSRV_RV_ZS          | Taxes on goods and services (% of revenue)
        # GC_TAX_YPKG_RV_ZS          | Taxes on income, profits and capital gains (% of revenue)
        # GC_TAX_INTT_RV_ZS          | Taxes on international trade (% of revenue)
        # GC_TAX_OTHR_RV_ZS          | Other taxes (% of revenue)
        # GC_REV_SOCL_ZS    | Social contributions (% of revenue)
        formula <- c("GC_TAX_OTHR_RV_ZS+GC_TAX_INTT_RV_ZS+GC_TAX_YPKG_RV_ZS+GC_TAX_GSRV_RV_ZS+GC_REV_GOTR_ZS+GC_REV_SOCL_ZS", 
                    "GC_REV_GOTR_ZS + GC_TAX_OTHR_RV_ZS", "GC_TAX_GSRV_RV_ZS", "GC_TAX_INTT_RV_ZS", 
                    "GC_REV_SOCL_ZS", "GC_TAX_YPKG_RV_ZS")
        variable <- c("Total", 
                    "Other revenues (incl. grants and other taxes)", "Consumption (notably VAT)", "International trade", 
                    "Social contributions", "Income, profits and capital gains")

        if (theme$with_title) {
            title <- paste0("Main Decomposition Sources of Revenues - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of revenue"
        if (is.null(theme$rescale) || theme$rescale) {
            post_processing <- in_rescale_to_100
        } else {
            post_processing <- NULL
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing)

    } else if (option == 7) {
        # Decomposition Tax Revenue
        # End of ISI /!\
        # GC_TAX_YPKG_ZS             | Taxes on income, profits and capital gains (% of total taxes)
        # GC_TAX_EXPT_ZS             | Taxes on exports (% of tax revenue)
        # GC_TAX_IMPT_ZS             | Customs and other import duties (% of tax revenue)
        formula <- c("GC_TAX_IMPT_ZS+GC_TAX_EXPT_ZS+GC_TAX_YPKG_ZS", "GC_TAX_IMPT_ZS", "GC_TAX_EXPT_ZS", "GC_TAX_YPKG_ZS")
        variable <- c("Sum", "Income, profits, and capital gains", "Exports", "Imports and custom fees")

        if (theme$with_title) {
            title <- paste0("Partial Decomposition Tax Revenue - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of tax revenue"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 8) {
        # Decomposition International Trade taxes
        formula <- c("GC_TAX_INTT_RV_ZS*GC_REV_XGRT_GD_ZS/GC_TAX_TOTL_GD_ZS", "GC_TAX_IMPT_ZS", "GC_TAX_EXPT_ZS")
        variable <- c("Total international trade", "Exports", "Imports and custom fees")

        if (theme$with_title) {
            title <- paste0("Decomposition Taxes on International Trade - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of tax revenue"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 9) {
        # Tax on conso with different denominator / TVA++
        # GC_TAX_GSRV_VA_ZS          | Taxes on goods and services (% value added of industry and services) [no subsidies]
        # = broader than VAT, but VAT is typically the largest component in most countries
        # = VAT (or general sales tax) + Excise duties (e.g., on alcohol, tobacco) + Selective service taxes (e.g., hotel taxes, insurance taxes) +Property usage taxes + Mining extraction taxes + Profits from government monopolies (e.g., state-owned liquor stores)

        formula <- c("GC_TAX_GSRV_VA_ZS", "100*GC_TAX_GSRV_CN/GDP_LCU_Y", "GC_TAX_GSRV_RV_ZS")
        variable <- c("% V.A. of industry and services", "% of GDP", "% of revenue")

        if (theme$with_title) {
            title <- paste0("Taxes on Consumption - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- NULL
        theme$area <- FALSE
        theme$right_axis <- variable[3]

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 10) {
        # Revenue = Expense + Net govt lending/borrowing + investment in non-financial assets
        # GC_NLD_TOTL_GD_ZS          | Net lending (+) / net borrowing (-) (% of GDP)
        # GC_NFN_TOTL_GD_ZS          | Net investment in nonfinancial assets (% of GDP)
        # GC_REV_XGRT_GD_ZS          | Revenue, excluding grants (% of GDP)
        # GC_XPN_TOTL_GD_ZS          | Expense (% of GDP)

        # Net lending (+) / net borrowing (–) equals government revenue minus expense, minus net investment in nonfinancial assets. It is also equal to the net result of transactions in financial assets and liabilities. Net lending/net borrowing is a summary measure indicating the extent to which government is either putting financial resources at the disposal of other sectors in the economy or abroad, or utilizing the financial resources generated by other sectors in the economy or from abroad.

        formula <- c("GC_REV_XGRT_GD_ZS", "GC_NLD_TOTL_GD_ZS", "GC_NFN_TOTL_GD_ZS", "GC_XPN_TOTL_GD_ZS")
        variable <- c("Revenue", "Net lending (+) / net borrowing (–)", "Net investment in nonfinancial assets", "Expense")

        if (theme$with_title) {
            title <- paste0("Decomposition of Government Revenue: Expense, Investment, and Fiscal Balance - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }

        if (theme$normalization == "GDP") {
            theme$y_axis <- "% of GDP"
        } else if (theme$normalization == "None") {
            formula <- paste0(formula, "*GDP_C_Y/(100*1e9)")
            theme$y_axis <- "Billion current USD"
        } else if (theme$normalization == "Inflation") {
            formula <- paste0(formula, "*GDP_R_2015_Y/(100*1e9)")
            theme$y_axis <- "Billion 2015 USD"
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    }

    invisible(output)

}


# not used
# NY_TAX_NIND_CD             | Taxes less subsidies on products (current US$)
# = (net indirect taxes) = the sum of product taxes less subsidies
# 100*NY_TAX_NIND_CD/GDP_C_Y = the size of net indirect taxation in the economy = total of indirect taxes minus indirect subsidies.
# It captures all indirect taxes - taxes on transactions and products, rather than direct taxes on income or wealth
# It's a net measure, so if a country heavily subsidizes certain products (like fuel or food), this would reduce the ratio even if their gross indirect taxes are high
# It's used in national accounts calculations, specifically in converting GDP from basic prices to market prices



in_rescale_to_100 <- function(data) {
    # Convert to numeric, removing 'X' from year
    data$Date <- as.numeric(gsub("X", "", data$Date))

    # Initialize the output dataframe
    output <- data

    # Get unique years
    years <- unique(data$Date)

    # Get the categories (excluding "Total")
    categories <- unique(data$Variable[data$Variable != "Total"])

    # For each year
    for(year in years) {
        # Get the data for this year
        year_data <- data[data$Date == year,]

        # Skip if there are NAs
        if(any(is.na(year_data$Value))) {
            next
        }

        # Get the values for categories (excluding Total)
        category_values <- year_data$Value[year_data$Variable %in% categories]

        # Calculate the sum
        sum_values <- sum(category_values)

        # If sum is not zero, rescale the values
        if(sum_values != 0) {
            # Calculate scaling factor
            scale_factor <- 100/sum_values

            # Update values in output dataframe
            output$Value[output$Date == year & output$Variable %in% categories] <- 
                category_values * scale_factor

            # Set Total to 100
            output$Value[output$Date == year & output$Variable == "Total"] <- 100
        }
    }

    # Convert year back to original format
    output$Date <- paste0("X", output$Date)

    return(output)
}

