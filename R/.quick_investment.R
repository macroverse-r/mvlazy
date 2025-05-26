
in_quick_investment <- function(ISO,
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
        # GFCF and Gross Savings
        # Key relations:
        # GCF = GDS + Net foreign borrowing

        # Domestic currency
        # NGDP_SA_XDC                | Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency
        # NI_SA_XDC                  | Gross Capital Formation, Nominal, Seasonally Adjusted, Domestic Currency
        # NFI_SA_XDC                 | Gross Fixed Capital Formation, Nominal, Seasonally Adjusted, Domestic Currency

        # USD
        # NE_GDI_FPRV_ZS    | Gross fixed capital formation, private sector (% of GDP)
        # NE_GDI_FTOT_CD    | Gross fixed capital formation (current US$)
        # NE_GDI_FTOT_ZS    | Gross fixed capital formation (% of GDP)
        # NE_GDI_STKB_CD    | Changes in inventories (current US$)
        # NE_GDI_TOTL_CD    | Gross capital formation (current US$)
        # NE_GDI_TOTL_ZS    | Gross capital formation (% of GDP)
        # NY_GDS_TOTL_ZS    | Gross domestic savings (% of GDP)

        # Note: 
        # more data for "NE_GDI_FTOT_ZS" than "NFI_SA_XDC" and values are almost the same
        # more data for "NE_GDI_TOTL_ZS" than "NI_SA_XDC" and values are almost the same
        # --> always use "NE_GDI_FTOT_ZS" and "NE_GDI_TOTL_ZS" for GFCF and GCF

        CU <- "BN_CAB_XOKA_GD_ZS"
        GCF <- "NE_GDI_TOTL_ZS"
        GFCF <- "NE_GDI_FTOT_ZS"
        GDS <- "NY_GDS_TOTL_ZS"
        inventories <- paste0(GFCF, "-", GCF)
        variable <- c("Gross Fixed Capital Formation (GFCF)", "(-) Changes in inventories", "Gross Domestic Savings (GDS)", "Net Foreign Borrowing (NFB)")

        # why keep GCF and CU in the baseline, see comment below for discussion
        # but more data for the case theme$keep_gds <- TRUE
        if (is.null(theme$keep_gds)) { theme$keep_gds <- FALSE }

        if (!theme$keep_gds) {
            # deduce Gross Domestic Savings from GDS = GCF + Current Account
            deduced_GDS <- paste(GCF, "+", CU)
            NFB <- paste0("-", CU)
            formula <- c(GFCF, inventories, deduced_GDS, NFB)
        } else if (theme$keep_gds) {
            # deduce net foreign borrowing (NFB) = - Current Account from NFB = GCF - GDS
            deduced_NFB <- paste0(GCF, "-", GDS)
            formula <- c(GFCF, inventories, GDS, deduced_NFB)
        }

        comment <- "A country's total investment (GCF) consists of long-term investments like buildings and machinery (GFCF) plus changes in business inventories. To finance these investments, countries rely on gross domestic savings (GDS, which equals GDP minus final consumption expenditure) or, when these are insufficient, they must borrow from abroad (Net Foreign Borrowing = NFB). If NFB is positive (investment > savings), the country is investing more than it saves domestically, indicating it needs to borrow from abroad or attract foreign investment to finance the gap. This results in a current account deficit (NFB = - Current Account). Overall, investment in the economy follows two key identities:
GCF = GFCF + ΔInventories and GCF = GDS + Net Foreign Borrowing where changes in inventories can be either positive or negative.
"

        if (theme$with_title) {
            title <- paste0("Gross Fixed Capital Formation and Gross Savings - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)


    } else if (option == 2) {
        # Private GFCF /!\ interesting to see the evolution of the private sector
        # NE_GDI_FPRV_ZS             | Gross fixed capital formation, private sector (% of GDP)
        # NE_GDI_FTOT_ZS             | Gross fixed capital formation (% of GDP)
        formula <- c("NE_GDI_FTOT_ZS", "NE_GDI_FPRV_ZS", "NE_GDI_FTOT_ZS-NE_GDI_FPRV_ZS")
        variable <- c("Total", "Private GCF", "Public GCF")


        if (theme$with_title) {
            title <- paste0("Private vs Public Investment - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL) 


    # } else if (option == 4) {
    #     # Private GFCF /!\ interesting to see the evolution of the private sector
    #     # NE_GDI_FPRV_ZS             | Gross fixed capital formation, private sector (% of GDP)
    #     # NE_GDI_FTOT_ZS             | Gross fixed capital formation (% of GDP)
    #     formula <- c("NE_GDI_FTOT_ZS", "NE_GDI_FPRV_ZS", "NE_GDI_FTOT_ZS-NE_GDI_FPRV_ZS")
    #     variable <- c("Total", "Private GCF", "Public GCF")
    #
    #
    #     if (theme$with_title) {
    #         title <- paste0("Private vs Public Investment - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
    #     }
    #     theme$y_axis <- "% of GDP"
    #
    #     output <- in_quick_helper(ISO = ISO, years = years, 
    #                                    formula = formula, variable = variable, 
    #                                    theme = theme, title = title, print = print, filename = NULL, 
    #                                    comment = NULL) 
    #



    } else if (option == 3) {
        # Gross National Expenditure (GNE) = Final consumption expenditure (FCE) + Gross Capital Formation
        # /!\ interesting to see the use of resource during the booms
        # GDP = Final Consumption + Gross Capital Formation + (Exports - Imports)
        # NE_CON_TOTL_ZS             | Final consumption expenditure (% of GDP)
        # NE_DAB_TOTL_ZS             | Gross national expenditure (% of GDP)
        # NE_GDI_TOTL_ZS             | Gross capital formation (% of GDP)
        if (is.null(theme$net_export)) {
            if (theme$normalization == "GDP") {
                net_export <- "(NE_EXP_GNFS_ZS-NE_IMP_GNFS_ZS)"
            } else if (theme$normalization == "None") {
                net_export <- "(NE_EXP_GNFS_CD-NE_IMP_GNFS_CD)/1e9"
            } else if (theme$normalization == "Inflation") {
                net_export <- "(NE_DAB_TOTL_KD/NE_DAB_TOTL_CD)*(NE_EXP_GNFS_CD-NE_IMP_GNFS_CD)/1e9"
            }

        } else if (theme$net_export == "BN") {
            if (theme$normalization == "GDP") {
                net_export <- "(100*BN_GSR_GNFS_CD/GDP_C_Y)"
            } else if (theme$normalization == "None") {
                net_export <- "BN_GSR_GNFS_CD/1e9"
            } else if (theme$normalization == "Inflation") {
                net_export <- "BN_GSR_GNFS_CD*1e-9*NE_DAB_TOTL_KD/NE_DAB_TOTL_CD"
            }

        } else if (theme$net_export == "diff") {
            if (theme$normalization == "GDP") {
                net_export <- "100-NE_DAB_TOTL_ZS"
            } else if (theme$normalization == "None") {
                net_export <- "(GDP_C_Y-NE_DAB_TOTL_CD)/1e9"
            } else if (theme$normalization == "Inflation") {
                net_export <- "(GDP_R_2015_Y-NE_DAB_TOTL_KD)/1e9"
            }
        }

        formula <- c("NE_DAB_TOTL_", "NE_CON_PRVT_", "NE_CON_GOVT_", "NE_GDI_TOTL_")
        variable <- c("Gross National Expenditure","Final Households Consumption", "Final Government Consumption", "Gross Capital Formation")

        if (theme$normalization == "GDP") {
            formula <- paste0(formula, "ZS")
            theme$y_axis <- "% of GDP"
        } else if (theme$normalization == "None") {
            formula <- paste0(formula, "CD*1e-9")
            theme$y_axis <- "Billion current USD"
        } else if (theme$normalization == "Inflation") {
            formula <- paste0(formula, "CD*1e-9*NE_DAB_TOTL_KD/NE_DAB_TOTL_CD")
            theme$y_axis <- "Billion 2015 USD"
        }

        formula <- c(formula[1], net_export, formula[2:4])
        variable <- c("Gross National Expenditure", "Net Exports (Exports-Imports)", "Final Households Consumption", "Final Government Consumption", "Gross Capital Formation")

        if (theme$with_title) {
            title <- paste0("Gross National Expenditure - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        comment <- "Final Total Consumption (C = Household Consumption + Government Consumption) plus Gross Capital Formation (I) equals Gross National Expenditure (GNE). The difference between GDP and GNE is Net Exports (NX), giving us GDP = C + I + NX = GNE + NX. When NX is negative, domestic spending exceeds domestic production. This gap may be financed through net income from foreign assets or, if Current Account < 0, through foreign borrowing or asset sales."

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)

    } else if (option == 4) {
        # Net Savings = Gross Savings - Depreciation (Consumption of Fixed Capital)
        # NY_ADJ_NNAT_GN_ZS = NY_ADJ_ICTR_GN_ZS - NY_ADJ_DKAP_GN_ZS
        # NY_ADJ_ICTR_GN_ZS          | Adjusted savings: gross savings (% of GNI)
        # NY_ADJ_NNAT_GN_ZS          | Adjusted savings: net national savings (% of GNI)
        # NY_ADJ_DKAP_GN_ZS          | Adjusted savings: consumption of fixed capital (% of GNI)
        formula <- c("NY_ADJ_NNAT_GN_ZS", "NY_ADJ_ICTR_GN_ZS", "-NY_ADJ_DKAP_GN_ZS")
        variable <- c("Net National Savings", "Gross Savings", "Consumption of fixed capital")

        if (theme$with_title) {
            title <- paste0("Net National Savings - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GNI"

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)


    } else if (option == 5) {

        # Adjusted Net Savings
        # Gross Savings = GCF + Current Account Balance
        # where GCF = GFCF + Inventory Changes
        # Adjusted Net Savings = Net Savings + Education Expenditure - Resource Depletion - Pollution
        # ANS = GFCF + (Inventory Changes + Current Account Balance) - Depreciation + Education Investment - Natural Capital Depletion


        # Adjust net savings = net savings + educ expenses - natural depletino - CO2 damage
        # NY_ADJ_SVNX_GN_ZS = NY_ADJ_NNAT_GN_ZS-NY_ADJ_DRES_GN_ZS+NY_ADJ_AEDU_GN_ZS-NY_ADJ_DCO2_GN_ZS
        # NY_ADJ_AEDU_GN_ZS          | Adjusted savings: education expenditure (% of GNI)
        # NY_ADJ_DRES_GN_ZS          | Adjusted savings: natural resources depletion (% of GNI)
        # NY_ADJ_NNAT_GN_ZS          | Adjusted savings: net national savings (% of GNI)
        # NY_ADJ_SVNX_GN_ZS          | Adjusted net savings, excluding particulate emission damage (% of GNI)
        # NY_ADJ_DCO2_GN_ZS          | Adjusted savings: carbon dioxide damage (% of GNI)

        # Total everything included and decomposed
        # NY_ADJ_SVNG_GN_ZS          | Adjusted net savings, including particulate emission damage (% of GNI)
        # NY_ADJ_DPEM_GN_ZS          | Adjusted savings: particulate emission damage (% of GNI)
        formula <- c("NY_ADJ_SVNG_GN_ZS", "-NY_ADJ_DKAP_GN_ZS", "-NY_ADJ_DPEM_GN_ZS-NY_ADJ_DCO2_GN_ZS", "-NY_ADJ_DRES_GN_ZS", "NY_ADJ_ICTR_GN_ZS", "NY_ADJ_AEDU_GN_ZS")
        variable <- c("Adjusted Net Savings", "Consumption of fixed capital", "Part. and CO2 emission damage", "Natural resources depletion", "Gross Savings", "Education expenditure")

        theme$no_emission_dmg <- if (is.null(theme$no_emission_dmg)) FALSE else theme$no_emission_dmg
        if (theme$no_emission_dmg) {
            # Without CO2 emssions to increase timespan
            formula <- c("-NY_ADJ_DKAP_GN_ZS-NY_ADJ_DRES_GN_ZS+NY_ADJ_ICTR_GN_ZS+NY_ADJ_AEDU_GN_ZS", "-NY_ADJ_DKAP_GN_ZS", "-NY_ADJ_DRES_GN_ZS", "NY_ADJ_ICTR_GN_ZS", "NY_ADJ_AEDU_GN_ZS")
            variable <- c("Adjusted Net Savings", "Consumption of fixed capital", "Natural resources depletion", "Gross Savings", "Education expenditure")
        }


        if (theme$with_title) {
            title <- paste0("Net National Savings - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GNI"

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)

    } else if (option == 6) {
        # NY_ADJ_DRES_GN_ZS = NY_ADJ_DFOR_GN_ZS+NY_ADJ_DMIN_GN_ZS+NY_ADJ_DNGY_GN_ZS
        # NY_ADJ_DRES_GN_ZS          | Adjusted savings: natural resources depletion (% of GNI)
        # NY_ADJ_DFOR_GN_ZS          | Adjusted savings: net forest depletion (% of GNI)
        # NY_ADJ_DMIN_GN_ZS          | Adjusted savings: mineral depletion (% of GNI)
        # NY_ADJ_DNGY_GN_ZS          | Adjusted savings: energy depletion (% of GNI)
        formula <- c("NY_ADJ_DRES_GN_ZS", "NY_ADJ_DFOR_GN_ZS", "NY_ADJ_DMIN_GN_ZS", "NY_ADJ_DNGY_GN_ZS")
        variable <- c("Natural resources depletion (total)", "Net forest depletion", "Mineral depletion", "Energy depletion")

        if (theme$with_title) {
            title <- paste0("Natural resources depletion - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GNI"

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)
    }



    invisible(output)
}



# #####################################
# Option==1, why keep CU and GCF to deduce GDS?
# 
# When working with these three measures, your decision on which variable to deduce should be based on data reliability. Here's my analysis:
#
# Current Account (CA) data tends to be the most reliable because:
# It involves actual international transactions
# It's carefully tracked by central banks
# It's subject to double-entry bookkeeping
# It's cross-checked with other countries' records
# International institutions (IMF) closely monitor it
#
#
# Gross Capital Formation (GCF/Investment) data is generally the second most reliable because:
# It involves concrete transactions
# It's based on measurable physical investments
# It's tracked through both expenditure and production approaches
# It has clear accounting standards
#
#
# Gross Domestic Savings is often the least reliable because:
# It's typically calculated as a residual in national accounts
# It incorporates measurement errors from GDP calculations
# It can include informal sector activities
# It's affected by statistical discrepancies in national accounts
#
# Therefore:
# Keep CA and GCF as your base measures
# Calculate Savings as: NY_GDS_TOTL_ZS = NE_GDI_TOTL_ZS + Current Account





# #####################################
# # Final consumption (households and gvt) - TO ADD
#
# formula <- c("NE_DAB_TOTL_ZS", "NE_CON_TOTL_ZS", "NE_GDI_TOTL_ZS")
# variable <- c("Gross National Expenditure", "Final Consumption", "Gross Capital Formation")
#
# # NE_CON_GOVT_ZS             | General government final consumption expenditure (% of GDP)
# # NE_CON_PRVT_ZS             | Households and NPISHs final consumption expenditure (% of GDP)
# # NE_CON_TOTL_ZS             | Final consumption expenditure (% of GDP)
# # NE_RSB_GNFS_ZS             | External balance on goods and services (% of GDP)
#
# formula <- c("NE_CON_TOTL_ZS", "NE_CON_GOVT_ZS", "NE_CON_PRVT_ZS")
# variable <- c("Final Consumption", "Government Consumption", "Households Consumption")
# # not normalize by GDP, to see evolution of the amount
# # formula <- paste0(formula, "*GDP_R_2015_Y/1e12")

