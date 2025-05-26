
in_quick_impact <- function(ISO,
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
        # GDP per capita
        # NE_CON_PRVT_PC_KD          | Households and NPISHs Final consumption expenditure per capita (constant 2015 US$)
        # NE_CON_PRVT_PP_KD          | Households and NPISHs Final consumption expenditure, PPP (constant 2021 international $)
        # NY_GDP_PCAP_CD             | GDP per capita (current US$)
        # NY_GDP_PCAP_KD             | GDP per capita (constant 2015 US$)
        # NY_GDP_PCAP_PP_KD          | GDP per capita, PPP (constant 2021 international $)
        # GDP_PPP_PC_MP              | Real GDP (PPP) per capita in 2011$ (Madison)
        # Note: "NE_CON_PRVT_PC_KD" == "NE_CON_PRVT_PP_KD/POP_Y" when using an index

        if (is.null(theme$no_global_deflator)) {
            theme$no_global_deflator <- TRUE
        }
        if (!theme$no_global_deflator) {
            formula <- c("NY_GDP_PCAP_PP_KD", "GDP_PPP_PC_MP", 
                         "NY_GDP_PCAP_CD/PRC_IDX_WLD_Y", "NY_GDP_PCAP_KD", 
                         "NE_CON_PRVT_PC_KD")
            variable <- c("GDP per capita (PPP)",
                          "GDP per capita (PPP, Madison)",
                          "GDP per capita (Global deflator)",
                          "GDP per capita (Local deflator)", 
                          "Consumption per capita")

            comment <- "The PPP-adjusted GDP per capita measures (both standard and Madison) account for price level differences across countries, making living standards internationally comparable. The globally-deflated GDP per capita specifically tracks purchasing power for internationally traded goods, while the locally-deflated measure better reflects domestic purchasing power relative to local goods and services. The consumption measures focus on household welfare by excluding investment and government spending, providing a more direct measure of material living standards. All measures are adjusted for inflation."
        } else if (theme$no_global_deflator) {
            formula <- c("NY_GDP_PCAP_PP_KD", "GDP_PPP_PC_MP", 
                         "NY_GDP_PCAP_KD", 
                         "NE_CON_PRVT_PC_KD")
            variable <- c("GDP per capita (PPP, World Bank)",
                          "GDP per capita (PPP, Madison)",
                          "GDP per capita (deflator)", 
                          "Consumption per capita")

            comment <- "The PPP-adjusted GDP per capita measures (both standard and Madison) account for price level differences across countries, making living standards internationally comparable. The deflated GDP per capita reflects domestic purchasing power relative to local goods and services. The consumption measures focus on household welfare by excluding investment and government spending, providing a more direct measure of material living standards. All measures are adjusted for inflation."
        }


        # SL_GDP_PCAP_EM_KD          | GDP per person employed (constant 2021 PPP $)
        if (is.null(theme$per_employed) || !theme$per_employed) {
            formula <- c(formula, "SL_GDP_PCAP_EM_KD")
            variable <- c(variable, "GDP per person employed (PPP, World Bank)")
        }


        if (theme$with_title) {
            title <- paste0("GDP and Household consumption per capita - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- paste0("Index (",years[1]," = 100)")
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment, idx = TRUE)


    } else if (option == 2) {
        # Eco. Activity Idx 
        # Interesting but not always available for every countries
        # AIP_SA_IX is only useful as sometimes (e.g., MEX) there is errors in AIP_IX

        # AIP_IX                     | Economic Activity, Industrial Production, Index (base year:2010=100)
        # AIPCO_IX                   | Economic Activity, Industrial Production, Construction, Index (base year:2010=100)
        # AIPEE_IX                   | Economic Activity, Industrial Production, Energy, Electricity Production, Index (base year:2010=100)
        # AIPMA_IX                   | Economic Activity, Industrial Production, Manufacturing, Index (base year:2010=100)
        # AIPMI_IX                   | Economic Activity, Industrial Production, Mining, Index (base year:2010=100)
        formula <- if (isTRUE(theme$sa)) "AIP_SA_IX" else "AIP_IX"
        formula <- c(formula, "AIPEE_IX", "AIPCO_IX", "AIPMA_IX", "AIPMI_IX")
        variable <- c("Total", "Energy and Electricity", "Construction", "Manufacturing", "Mining")

        # Variable
        if (theme$with_title) {
            title <- paste0("Economic Activity, Industrial Production - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- paste0("Index (",years[1]," = 100)")
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, idx=TRUE)

    } else if (option == 3) {
        # SH_XPD_OOPC_PC_CD          | Out-of-pocket expenditure per capita (current US$)
        # SH_XPD_PVTD_PC_CD          | Domestic private health expenditure per capita (current US$)
        formula <- c("SH_XPD_OOPC_PC_CD*GDP_R_2015_Y/GDP_C_Y", "SH_XPD_PVTD_PC_CD*GDP_R_2015_Y/GDP_C_Y")
        variable <- c("Out-of-pocket expenditure", "Domestic private health expenditure")

        # Formula
        # Variable
        if (theme$with_title) {
            title <- paste0("Healthcare Spending per Capita - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "Constant 2023 USD"
        # theme$y_axis <- paste0("Index (",years[1]," = 100)")
        comment <- "This graph shows how private healthcare is funded per person: through direct payments by individuals (like paying at a doctor's office or pharmacy) and through other private sources like insurance. The total private spending includes both these direct payments and the additional private funding."
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)



    } else if (option == 4) {
        # GNI/GDP meaning:
        # When GNI/GDP ≠ 1:
        #
        # If > 1: The country earns more than it produces domestically
        # Example: A country with many citizens working abroad sending remittances home
        # Or a country with significant foreign investments earning profits
        # Philippines often has GNI/GDP > 1 due to overseas worker remittances
        #
        # If < 1: The country produces more than its residents earn
        # Example: Countries with many foreign-owned companies that repatriate profits
        # Or countries with significant foreign worker populations sending money home
        # Ireland has a notably low GNI/GDP due to large multinational presence
        #
        # GNI/GDP often drops after financial crises mainly because of how international debt dynamics work:
        # When a country faces a financial crisis, typically:
        # Its currency devalues / Interest rates spike / The country often has significant foreign-denominated debt
        # This leads to:
        # Higher debt service payments to foreigners (increasing outflows)
        # These payments are part of GNI (reducing it) but not GDP
        # The payments become even larger in domestic currency due to devaluation
        #
        # Main Mechanism:
        # During crisis → Higher perceived risk
        # Foreign investors demand higher risk premiums
        # This means higher interest payments on foreign debt
        # These interest payments are subtracted from GNI (but not from GDP)
        # Result: GNI falls relative to GDP
        #
        # TODO: falls in GNI/GDP match quite well with financial crises!
        formula <- c("GDP_C_Y/GDP_C_Y", "NY_GNP_MKTP_CD/GDP_C_Y")
        variable <- c("1", "GNI/GDP")


        if (theme$with_title) {
            title <- paste0("GNI-GDP ratio - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$area <- FALSE
        comment <- "If > 1: The country earns more than it produces domestically. Example: A country with many citizens working abroad sending remittances home Or a country with significant foreign investments earning profits. Philippines often has GNI/GDP > 1 due to overseas worker remittances

If < 1: The country produces more than its residents earn. Example: Countries with many foreign-owned companies that repatriate profits Or countries with significant foreign worker populations sending money home. Ireland has a notably low GNI/GDP due to large multinational presence.

GNI/GDP often drops after financial crises mainly because of how international debt dynamics work: When a country faces a financial crisis, typically: Its currency devalues / Interest rates spike / The country often has significant foreign-denominated debt. This leads to: Higher debt service payments to foreigners (increasing outflows). These payments are part of GNI (reducing it) but not GDP. The payments become even larger in domestic currency due to devaluation.

Main Mechanism: During crisis → Higher perceived risk. Foreign investors demand higher risk premiums. This means higher interest payments on foreign debt. These interest payments are subtracted from GNI (but not from GDP). Result: GNI falls relative to GDP.
"

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = comment)


    } else if (option == 5) {
        # Energy
        # Energy / Electricity use
        # EG_USE_ELEC_KH_PC          | Electric power consumption (kWh per capita)
        # EG_USE_PCAP_KG_OE          | Energy use (kg of oil equivalent per capita)
        formula <- c("EG_USE_ELEC_KH_PC", "EG_USE_PCAP_KG_OE")
        variable <- c("Electric power consumption", "Energy use")


        if (theme$with_title) {
            title <- paste0("Energy Consumption per Capita: Electricity and Total Energy Use - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("kWh per capita", "kg of oil equivalent per capita")
        theme$area <- FALSE
        theme$right_axis <- formula[2]

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)


    } else if (option == 6) {
        # Suicide (by 2000), interesting for GRC
        # SH_STA_SUIC_FE_P5          | Suicide mortality rate, female (per 100,000 female population)
        # SH_STA_SUIC_MA_P5          | Suicide mortality rate, male (per 100,000 male population)
        # SH_STA_SUIC_P5             | Suicide mortality rate (per 100,000 population)
        formula <- c("SH_STA_SUIC_FE_P5", "SH_STA_SUIC_P5", "SH_STA_SUIC_MA_P5")
        variable <- c("Suicide mortality rate, female", "Suicide mortality rate", "Suicide mortality rate, male")

        # rise of homicide after financial crises
        # VC_IHR_PSRC_FE_P5          | Intentional homicides, female (per 100,000 female)
        # VC_IHR_PSRC_MA_P5          | Intentional homicides, male (per 100,000 male)
        # VC_IHR_PSRC_P5             | Intentional homicides (per 100,000 people)
        formula <- c(formula, "VC_IHR_PSRC_P5", "VC_IHR_PSRC_FE_P5", "VC_IHR_PSRC_MA_P5")
        variable <- c(variable, "Homicides", "Homicides, female", "Homicides, male")


        if (theme$with_title) {
            title <- paste0("Suicides and homicides - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("Rate per 100,000 people (Gender-Specific)", "Rate per 100,000 people (Gender-Specific)")
        theme$area <- FALSE
        theme$right_axis <- formula[1:3]

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)


    } else if (option == 7) {
        # Migration
        # SM_POP_NETM                | Net migration = immigrants - emigrants
        # Net migration is the net total of migrants during the period, that is, the number of immigrants minus the number of emigrants, including both citizens and noncitizens.
        # SM_POP_REFG                | Refugee population by country or territory of asylum
        # SM_POP_REFG_OR             | Refugee population by country or territory of origin
        formula <- c("1e6*SM_POP_NETM/POP_Y", "1e6*SM_POP_REFG_OR/POP_Y")
        variable <- c("Net migration [immigrants - emigrants]", "Refugee population by country of origin")


        if (theme$with_title) {
            title <- paste0("Migration - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("per million population", "per million population")
        theme$area <- FALSE
        theme$right_axis <- formula[2]

        output <- in_quick_helper(ISO = ISO, years = years,
                                       formula = formula, variable = variable,
                                       theme = theme, title = title, print = print, filename = NULL,
                                       comment = NULL)
    }

    invisible(output)
}




# Note: 
# NY_GDP_PCAP_KD is likely calculated using national GDP deflators specific to each country
# Using a world GDP deflator (PRC_IDX) would give you different results because:
#
# Price levels and inflation rates vary significantly across countries
# Exchange rate fluctuations affect current dollar values but not constant dollar values in local currency
#
# So NY_GDP_PCAP_CD/PRC_IDX would give you:
#
# GDP per capita in 2015 dollars using global price levels
# But this would differ from NY_GDP_PCAP_KD which uses country-specific price adjustments
#
# NY_GDP_PCAP_CD/PRC_IDX:
#
# Takes current US$ values and deflates them using a global price index
# This effectively measures purchasing power against global goods
# Since iPhones and other international goods are priced in global markets, this makes sense
# The global deflator (PRC_IDX) better reflects price changes in international markets
#
#
# NY_GDP_PCAP_KD:
#
# Uses country-specific deflators
# Better reflects domestic purchasing power changes
# But might not accurately track ability to buy international goods
# Local inflation could diverge significantly from global prices of traded goods
#
#
# For your specific interest:
#
# You want to track how many iPhones (or similar international goods) a person could buy over time
# These goods are priced in international markets
# Their prices change according to global market conditions
# Therefore, NY_GDP_PCAP_CD/PRC_IDX would be the better measure for your purpose.

