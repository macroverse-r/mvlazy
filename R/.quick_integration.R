
#  [Quinn] Data from Quinn and Toyoda (2008). ''Does Capital Account Liberalization Lead to Economic Growth?'' Review of Financial Studies. 
# CAP_QUINN                  | de jure indicator of capital account openness - score between 0 and 100 - Higher values indicate fewer government-imposed restrictions
# CUR_QUINN                  | de jure indicator of current account openness - score between 0 and 100 - Higher values indicate fewer government-imposed restrictions
#
#  [KOF] Data from Gygli, Haelg, Potrafke & Sturm (2019), 'The KOF Globalisation Index - Revisited', Review of International Organizations 
# KOF_TRD_DF                 | Trade Globalisation Index - (Trade in goods, services, and trade partner diversity) - De facto -  values between 0 and 100 - higher values indicate higher level of 
# integration
# KOF_TRD_DJ                 | Trade Globalisation Index - (Trade regulations, taxes, tariffs, agreements) - De jure -  values between 0 and 100 - higher values indicate higher level of integrati
# on
# KOF_FIN_DF                 | Financial Globalisation Index - (FDI, Portfolio, inter. debt, reserves, int. income payments) - De facto -  values between 0 and 100 - higher values indicate higher
#  level of integration
# KOF_FIN_DJ                 | Financial Globalisation Index - (Invest. restrictions, CA openness, Invest. Agreements) - De jure -  values between 0 and 100 - higher values indicate higher level 
# of integration


# Socio-cultural 
# interesting to see that Russia "KOF_CUL_DF" rise massively after 1998 while for other ex-Eastern bloc it is in 1990-1991
# interesting to see the Eastern Europe increased their economic and political integration in the 1970s-1980s
# Otherwise, useful to remember some political "shocks"

# KOF_CUL_DF                 | Cultural Globalisation Index - (trade cultural goods, in personal services, inter. trademarks, McDonalds. IKEAs) - De facto -  values between 0 and 100 - higher val
# ues indicate higher level of integration
# KOF_CUL_DJ                 | Cultural Globalisation Index - (Gender parity, human capital, civil liberties) - De jure -  values between 0 and 100 - higher values indicate higher level of integr
# ation
# KOF_IPE_DF                 | Interpersonal Globalisation Index - (inter. voice traffic, transfers, tourism, students) - De facto -  values between 0 and 100 - higher values indicate higher leve
# l of integration
# KOF_IPE_DJ                 | Interpersonal Globalisation Index - (telephone subscriptions, freedom to visit, inter. airports, migration) - De jure -  values between 0 and 100 - higher values in
# dicate higher level of integration
# KOF_POL_DF                 | Political Globalisation Index - (Embassies, UN peace mission, Inter. NGOs) - De facto -  values between 0 and 100 - higher values indicate higher level of integrati
# on
# KOF_POL_DJ                 | Political Globalisation Index - (Inter. organisations, inter treaties, treaty partner diversity) - De jure -  values between 0 and 100 - higher values indicate high
# er level of integration
# KOF_SOC_DF                 | Social Globalisation Index - 33.3 x (Interpersonal, Informational, Cultural) - De facto -  values between 0 and 100 - higher values indicate higher level of integra
# tion
# KOF_SOC_DJ                 | Social Globalisation Index - 33.3 x (Interpersonal, Informational, Cultural) - De jure -  values between 0 and 100 - higher values indicate higher level of integrat
# ion










in_quick_integration <- function(ISO,
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
        # De facto - Finance
        formula <- c("KOF_FIN_DF", "100*RAFA_MV_USD/GDP_C", "DT_DOD_DECT_GN_ZS")
        variable <- c("KOF Index (De facto)", "Reserves", "External debt Stock")

        # Variable
        if (theme$with_title) {
            title <- paste0("Integration - De facto - Finance - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("% of GDP", "Index")
        theme$area <- FALSE
        theme$right_axis <- variable[1]

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 2) {
        # De facto - Trade
        formula <- c("KOF_TRD_DF", "NE_TRD_GNFS_ZS", "100*(EXs_C+EXg_C+IMs_C+IMg_C)/GDP_C")
        variable <- c("KOF Index (De facto)", "Trade (World Bank)", "Trade  (IMF)")

        # Variable
        if (theme$with_title) {
            title <- paste0("Integration - De facto - Trade - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("% of GDP", "Index")
        theme$area <- FALSE
        theme$right_axis <- variable[1]

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)



    } else if (option == 3) {
        # De jure - Finance
        formula <- c("KOF_FIN_DJ", "CAP_QUINN")
        variable <- c("KOF Index (De jure)", "Capital Account Openness Index")

        # Variable
        if (theme$with_title) {
            title <- paste0("Integration - De jure - Finace - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)



    } else if (option == 4) {
        # De jure - Trade
        formula <- c("KOF_TRD_DJ", "CUR_QUINN")
        variable <- c("KOF Index (De jure)", "Current Account Openness Index")

        # Variable
        if (theme$with_title) {
            title <- paste0("Integration - De jure - Trade - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)


    } else if (option == 5) {
        # Political and cultural integration
        formula <- c("KOF_POL_DJ", "KOF_POL_DF", "KOF_CUL_DJ", "KOF_CUL_DF")
        variable <- c("Political (De jure)", "Political (De facto)", "Cultural (De jure)", "Cultural (De facto)")

        # Variable
        if (theme$with_title) {
            title <- paste0("Political and cultural integration - KOF index - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)



    } else if (option == 6) {
        formula <- c("KOF_SOC_DJ", "KOF_SOC_DF", "KOF_IPE_DJ", "KOF_IPE_DF")
        variable <- c("Social (De jure)", "Social (De facto)", "Interpersonal (De jure)", "Interpersonal (De facto)")

        # Variable
        if (theme$with_title) {
            title <- paste0("Social and Interpersonal integration - KOF index - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)







    }

    invisible(output)
}


