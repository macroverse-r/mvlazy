
#####################################
# Debt and credit
# See source("dev/tests/finCy-s-creditBIS.r") for graph in areas

# WB: good complement to BIS data for some countries (depend on data avail.)
# FS_AST_CGOV_GD_ZS          | Claims on central government, etc. (% GDP)
# FS_AST_DOMS_GD_ZS          | Domestic credit provided by financial sector (% of GDP)
# FS_AST_PRVT_GD_ZS          | Domestic credit to private sector (% of GDP)
formula <- c("FS_AST_CGOV_GD_ZS", "FS_AST_DOMS_GD_ZS", "FS_AST_PRVT_GD_ZS",
             "CRED_ALL_NOFINPRIV_ZS", "CRED_ALL_NOFIN_ZS")

# Sometimes has more data than BIS
# FD_AST_PRVT_GD_ZS          | Domestic credit to private sector by banks (% of GDP)
# CRED_BANK_NOFINPRIV_ZS     | Credit from banks to private non-fin sector (%GDP)
formula <- c("FD_AST_PRVT_GD_ZS", "CRED_BANK_NOFINPRIV_ZS")

in_quick_credit <- function(ISO,
                            years,
                            option = 1,
                            theme = list(),
                            print = TRUE,
                            filename = NULL
                            ) {


    if (is.null(theme$dim)) { theme$dim <- c(10, 10) }
    if (is.null(theme$base_size)) { theme$base_size <- 14 }

    if (option == 1) {
        # Credit general decomposition - BIS
        # See source("dev/tests/finCy-s-creditBIS.r") for graph in areas
        # CRED_BANK_NOFINPRIV_ZS     | Credit from banks to private non-fin sector (%GDP)
        # CRED_ALL_GVT_ZS   | Credit from all sectors to central govt (%GDP)
        # CRED_ALL_HH_ZS    | Credit from all sectors to households (%GDP)
        # CRED_ALL_NOFIN_ZS | Credit from all sectors to non-financial sector (%GDP)
        # CRED_ALL_NOFINFIRM_ZS | Credit from all sectors to non-fin corporations (%GDP)
        # CRED_ALL_NOFINPRIV_ZS | Credit from all sectors to private non-fin sector (%GDP)
        if (!isFALSE(theme$with_hh)) {
            formula <- c("CRED_ALL_NOFIN_CD", "CRED_ALL_NOFINFIRM_CD", "CRED_ALL_HH_CD", "(CRED_ALL_NOFIN_CD-CRED_ALL_NOFINPRIV_CD)")
            variable <- c("Credit to non-financial sector", "Non-financial corporations", "Households", "General Government")
        } else {
            formula <- c("CRED_ALL_NOFIN_CD", "CRED_ALL_NOFINPRIV_CD", "(CRED_ALL_NOFIN_CD-CRED_ALL_NOFINPRIV_CD)")
            variable <- c("Credit to non-financial sector", "Private Non-fin. Sector", "General Government")
        }

        if (theme$match_tot) { formula[1] <- paste0(formula[2:(length(formula)-1)], collapse = "+") }

        title <- NULL
        if (is.null(theme$with_title)) { theme$with_title <- FALSE }
        if (theme$with_title) {
            title <- paste("Credit/Debt Securities by borrowers - ", paste(wp_from_iso(ISO), collapse = ", ") )
        }

        if (theme$normalization == "GDP") {
            theme$y_axis <- "% of GDP"
        } else if (theme$normalization == "None") {
            theme$y_axis <- "Billion USD"
        } else if (theme$normalization == "Inflation") {
            theme$y_axis <- "Billion 2023 USD"
        }


        post_processing <- function(data) {
            environment(in_usd2normalized) <- environment()
            return(in_usd2normalized(data = data, gdp = "100*CRED_ALL_NOFIN_CD/CRED_ALL_NOFIN_ZS"))
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                  formula = formula, variable = variable, 
                                  theme = theme, title = title, print = print, filename = NULL, 
                                  comment = NULL, post_processing = post_processing)

    } else if (option == 2) {
        # Credit decomposition - bank vs non-bank - BIS
        formula <- c("CRED_ALL_NOFINPRIV_CD", "(CRED_ALL_NOFINPRIV_CD-CRED_BANK_NOFINPRIV_CD)", "CRED_BANK_NOFINPRIV_CD")
        variable <- c("Total private non-financial sector", "Non-bank credit", "Bank credit")


        if (theme$match_tot) { formula[1] <- paste0(formula[2:(length(formula)-1)], collapse = "+") }

        title <- NULL
        if (is.null(theme$with_title)) { theme$with_title <- FALSE }
        if (theme$with_title) {
            title <- paste("Credit to Private Non-Financial Sector - bank vs non-bank credit - ", paste(wp_from_iso(ISO), collapse = ", ") )
        }

        if (theme$normalization == "GDP") {
            theme$y_axis <- "% of GDP"
        } else if (theme$normalization == "None") {
            theme$y_axis <- "Billion USD"
        } else if (theme$normalization == "Inflation") {
            theme$y_axis <- "Billion 2023 USD"
        }


        if (isTRUE(theme$use_ZS)) {
            if (length(ISO) > 1) {print("Do not work for more than 1 country")}
            formula <- c("CRED_ALL_NOFINPRIV_ZS", "(CRED_ALL_NOFINPRIV_ZS-CRED_BANK_NOFINPRIV_ZS)", "CRED_BANK_NOFINPRIV_ZS")
            theme$y_axis <- "% of GDP"
            theme$normalization <- "None"
        }


        post_processing <- function(data) {
            environment(in_usd2normalized) <- environment()
            return(in_usd2normalized(data = data, gdp = "100*CRED_ALL_NOFINPRIV_CD/CRED_ALL_NOFINPRIV_ZS"))
        }
        comment <- "Main non-bank creditors include: Insurance companies, Pension, Investment, and mutual funds, Finance non-bank companies, Private equity firms, Leasing companies, Securitization vehicles, and Bond markets (corporate bonds). These institutions provide credit through various instruments like bonds, commercial paper, and direct lending."

        output <- in_quick_helper(ISO = ISO, years = years, 
                                  formula = formula, variable = variable, 
                                  theme = theme, title = title, print = print, filename = NULL, 
                                  comment = comment, post_processing = post_processing)





    } else if (option == 3) {
        # External debt stocks
        # DT_DOD_DECT_CD    | External debt stocks, total (DOD, current US$)
        # DT_DOD_DECT_GN_ZS | External debt stocks (% of GNI)
        # DT_DOD_DLXF_CD    | External debt stocks, long-term (DOD, current US$)
        # DT_DOD_DPNG_CD    | External debt stocks, private nonguaranteed (PNG) (DOD, current US$)
        # DT_DOD_DPPG_CD    | External debt stocks, public and publicly guaranteed (PPG) (DOD, current US$)
        # DT_DOD_DSTC_CD    | External debt stocks, short-term (DOD, current US$)
        # See source("dev/tests/finCy-s-debtStock.r") for debt
        formula <- c("DT_DOD_DECT_CD", "DT_DOD_DPPG_CD", "DT_DOD_DPNG_CD", "DT_DOD_DSTC_CD", "DT_DOD_DIMF_CD")
        variable <- c("Total", "Long-Term PPG", "Long-Term Private", "Short-Term (< 1y)", "IMF Credit")

        if (theme$match_tot) { formula[1] <- paste0("(", paste0(formula[2:(length(formula)-1)], collapse = "+"), ")") }

        title <- NULL
        if (is.null(theme$with_title)) { theme$with_title <- FALSE }
        if (theme$with_title) {
            title <- paste("External debt stocks - ", paste(wp_from_iso(ISO), collapse = ", ") )
        }

        if (theme$normalization == "GDP") {
            theme$y_axis <- "% of GDP"
            formula <- paste0(formula, "*100/GDP_C_Y")
        } else if (theme$normalization == "None") {
            theme$y_axis <- "Billion USD"
        } else if (theme$normalization == "Inflation") {
            theme$y_axis <- "Billion 2023 USD"
            formula <- paste0(formula, "/PRC_IDX_WLD")
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                  formula = formula, variable = variable, 
                                  theme = theme, title = title, print = print, filename = NULL, 
                                  comment = NULL)


    }


        # Short term external debt stocks
        # DT_DOD_DSTC_IR_ZS | Short-term debt (% of total reserves)
        # DT_DOD_DSTC_XP_ZS | Short-term debt (% of exports of goods, services and primary income)
        # DT_DOD_DSTC_ZS    | Short-term debt (% of total external debt)




    return(output)

}



in_usd2normalized <- function(data, gdp) {

    if (theme$normalization == "GDP") {
        if (is.null(theme$na.rm)) { theme$na.rm <- FALSE }
        if (is.null(theme$clean)) { theme$clean <- FALSE }
        GDP <- wp_data(ISO = ISO, formula = gdp, variable = "GDP",
                       years = years, adjust_seasonal = theme$sea_adj,
                       reference = FALSE, aggregate_iso = "Sum",
                       na.rm = theme$na.rm, clean = theme$clean)
        for (var in variable) {
            data[data$Variable == var, ]$Value <- 100*data[data$Variable == var, ]$Value/GDP$Value
        }
    } else if (theme$normalization == "Inflation") {
        PRC_IDX_WLD <- wp_data("USA", years = years, formula = "PRC_IDX_WLD")
        for (var in variable) {
            data[data$Variable == var, ]$Value <- data[data$Variable == var, ]$Value/PRC_IDX_WLD$Value
        }

    }
    return(data)
}



# TODO
# look into these variables and their relations


# formula <- c("FM_LBL_BMNY_GD_ZS",
#              "100*FM_AST_NFRG_CN/GDP_LCU_Y", 
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y")
# variable <- c("Broad Money", "Net foreign assets", "Net domestic credit")
#
#
# formula <- c("FM_AST_PRVT_GD_ZS",
#              "100*FM_AST_NFRG_CN/GDP_LCU_Y", 
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y",
#              "FM_LBL_BMNY_GD_ZS",
#              "100*FI_RES_TOTL_CD/GDP_C_Y"
# )
# variable <- c("Private NDC", 
#               "Net foreign assets (NFA)", 
#               "Net domestic credit (NDC)",
#               "Broad money",
#               "Reserves"
# )

# formula <- c("FM_LBL_BMNY_GD_ZS",
#              "FM_AST_PRVT_GD_ZS",
#              "100*FM_AST_NFRG_CN/GDP_LCU_Y", 
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y - FM_AST_PRVT_GD_ZS"
# )
# variable <- c("Broad money",
#               "Net domestic credit (NDC) to private sector", 
#               "Net foreign assets (NFA)", 
#               "Net domestic credit (NDC) to public sector"
# )
#
# formula <- c("CRED_ALL_NOFINPRIV_ZS", 
#              "FM_AST_PRVT_GD_ZS", 
#              "CRED_ALL_GVT_ZS",
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y - FM_AST_PRVT_GD_ZS",
#              "CRED_ALL_NOFIN_ZS",
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y"
# )
# variable <- c("Private BIS", "Private WB", 
#               "Public BIS", "Public WB", 
#               "Tot BIS", "Tot WB")
#
#
# formula <- c("CRED_ALL_NOFIN_ZS",
#              "100*FM_AST_DOMS_CN/GDP_LCU_Y")
#
#
# formula <- c("CRED_ALL_NOFINPRIV_ZS", 
#              "FM_AST_PRVT_GD_ZS", "CRED_BANK_NOFINPRIV_ZS")

