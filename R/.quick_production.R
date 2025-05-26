
in_quick_production <- function(ISO,
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
        # Service, industry vs Agriculture
        if (theme$normalization %in% c("None", "Inflation")) {
            # NV_AGR_TOTL_CD             | Agriculture, forestry, and fishing, value added (current US$)
            # NV_IND_MANF_CD             | Manufacturing, value added (current US$)
            # NV_IND_TOTL_CD             | Industry (including construction), value added (current US$)
            # NV_SRV_TOTL_CD    | Services, value added (current US$)
            if (isTRUE(theme$no_manufacturing)) {
                formula <- c("NV_SRV_TOTL_CD", "NV_AGR_TOTL_CD", "NV_IND_TOTL_CD")
            } else {
                formula <- c("NV_SRV_TOTL_CD", "NV_AGR_TOTL_CD", "(NV_IND_TOTL_CD-NV_IND_MANF_CD)", "NV_IND_MANF_CD")
            }
            total <- "(NV_SRV_TOTL_CD+NV_AGR_TOTL_CD+NV_IND_TOTL_CD)"

            formula <- paste0(formula, "*1e-9")
            total <- paste0(total, "*1e-9")
            theme$y_axis <- "Billion USD"

            if (theme$normalization == "Inflation") {
                formula <- paste0(formula, "/PRC_IDX_WLD")
                total <- paste0(total, "/PRC_IDX_WLD")
                theme$y_axis <- "Billion 2023 USD"
            }

        } else if (theme$normalization == "GDP") {
            # NV_IND_TOTL_ZS             | Industry (including construction), value added (% of GDP)
            # NV_IND_MANF_ZS             | Manufacturing, value added (% of GDP)
            # NV_AGR_TOTL_ZS             | Agriculture, forestry, and fishing, value added (% of GDP)
            # NV_SRV_TOTL_ZS             | Services, value added (% of GDP)
            if (isTRUE(theme$no_manufacturing)) {
                formula <- c("NV_SRV_TOTL_ZS", "NV_AGR_TOTL_ZS", "NV_IND_TOTL_ZS")
            } else {
                formula <- c("NV_SRV_TOTL_ZS", "NV_AGR_TOTL_ZS", "(NV_IND_TOTL_ZS-NV_IND_MANF_ZS)", "NV_IND_MANF_ZS")
            }
            total <- paste0("(", paste0(formula, collapse = "+"), ")")

            print(theme$match_tot)
            if (is.null(theme$match_tot) || isTRUE(theme$match_tot)) {
                formula <- paste0(formula, "*100/", total)
                total <- paste0(total, "*100/", total)
            }
            theme$y_axis <- "% of GDP"
        }

        if (isTRUE(theme$no_manufacturing)) {
            variable <- c("Service", "Agriculture", "Industry")
        } else {
            variable <- c("Service", "Agriculture", "Non-Manufacturing Industry", "Manufacturing")
        }


        if (!isFALSE(theme$area)) {
            formula <- c(total, formula)
            variable <- c("Total", variable)
        }



        if (theme$with_title) {
            title <- paste0("Value Added in Service, Industry and Agriculture - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL) 


    } else if (option == 2) {
        # Very good to compare trajectories of LatAm vs Asia
        # NV_AGR_EMPL_KD             | Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)
        # NV_IND_EMPL_KD             | Industry (including construction), value added per worker (constant 2015 US$)
        # NV_SRV_EMPL_KD             | Services, value added per worker (constant 2015 US$)
        formula <- c("NV_SRV_EMPL_KD", "NV_AGR_EMPL_KD", "NV_IND_EMPL_KD")
        variable <- c("Service", "Agriculture", "Industry")
        if (theme$with_title) {
            title <- paste0("Value Added per Worker in Service, Industry and Agriculture - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "constant 2015 US$"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL) 

    } else if (option == 3) {
        # Decomposition of the manufacturing sector
        # /!\ interesting
        # NV_MNF_CHEM_ZS_UN          | Chemicals (% of value added in manufacturing)
        # NV_MNF_FBTO_ZS_UN          | Food, beverages and tobacco (% of value added in manufacturing)
        # NV_MNF_MTRN_ZS_UN          | Machinery and transport equipment (% of value added in manufacturing)
        # NV_MNF_OTHR_ZS_UN          | Other manufacturing (% of value added in manufacturing)
        # NV_MNF_TECH_ZS_UN          | Medium and high-tech manufacturing value added (% manufacturing value added)
        # NV_MNF_TXTL_ZS_UN          | Textiles and clothing (% of value added in manufacturing)
        formula <- c("NV_MNF_CHEM_ZS_UN", "NV_MNF_FBTO_ZS_UN", "NV_MNF_OTHR_ZS_UN",
                     "NV_MNF_MTRN_ZS_UN", "NV_MNF_TXTL_ZS_UN")

        variable <- c("Chemicals", "Food, drinks and tobacco", "Other Manufacturing",
                      "Machinery and Transport Equipment", "Textiles and clothing")

        if (!isFALSE(theme$with_tech)) {
            formula <- c("NV_MNF_TECH_ZS_UN", formula)
            variable <- c("Medium and High-tech", variable)
        } else {
            formula <- c("NV_MNF_CHEM_ZS_UN+NV_MNF_FBTO_ZS_UN+NV_MNF_MTRN_ZS_UN+NV_MNF_OTHR_ZS_UN+NV_MNF_TXTL_ZS_UN", formula)
            variable <- c("Total", variable)
        }
        comment <- "Other manufacturing typically includes the manufacturing of furniture, wood, printing/publishing and paper products, rubber and plastic products, metal and non-metallic mineral products (glass, cement) as well as toys, jewelry and musical instruments."


        if (theme$with_title) {
            title <- paste0("Decomposition of the manufacturing sector - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of value added in manufacturing"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment) 

    } else if (option == 4) {
        # Natural Resources rents 
        # NY_GDP_TOTL_RT_ZS          | Total natural resources rents (% of GDP)
        # NY_GDP_MINR_RT_ZS          | Mineral rents (% of GDP)
        # NY_GDP_FRST_RT_ZS          | Forest rents (% of GDP) (Timber)  
        # NY_GDP_NGAS_RT_ZS          | Natural gas rents (% of GDP)
        # NY_GDP_PETR_RT_ZS          | Oil rents (% of GDP)
        # NY_GDP_COAL_RT_ZS          | Coal rents (% of GDP)
        formula <- c("NY_GDP_TOTL_RT_ZS", "NY_GDP_MINR_RT_ZS", "NY_GDP_FRST_RT_ZS", "NY_GDP_NGAS_RT_ZS", "NY_GDP_PETR_RT_ZS", "NY_GDP_COAL_RT_ZS")
        variable <- c("Total", "Mineral", "Forest", "Natural gas", "Oil", "Coal")
        if (theme$with_title) {
            title <- paste0("Decomposition of Natural Resources Rents - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of GDP"
        comment <- "Note: The estimates of natural resources rents are calculated as the difference between the price of a commodity and the average cost of producing it."

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)

    } else if (option == 5) {
        # Electricity production composition
        # EG_ELC_HYRO_ZS             | Electricity production from hydroelectric sources (% of total)
        # EG_ELC_NUCL_ZS             | Electricity production from nuclear sources (% of total)
        # EG_ELC_RNWX_ZS             | Electricity production from renewable sources, excluding hydroelectric (% of total)
        # ---
        # EG_ELC_FOSL_ZS             | Electricity production from oil, gas and coal sources (% of total)
        # EG_ELC_NGAS_ZS             | Electricity production from natural gas sources (% of total)
        # EG_ELC_PETR_ZS             | Electricity production from oil sources (% of total)
        # EG_ELC_COAL_ZS             | Electricity production from coal sources (% of total)
        formula <- c("EG_ELC_FOSL_ZS",                       
                     "EG_ELC_NUCL_ZS", "EG_ELC_RNWX_ZS", "EG_ELC_HYRO_ZS",
                     "EG_ELC_NGAS_ZS", "EG_ELC_PETR_ZS", "EG_ELC_COAL_ZS")

        variable <- c("Fossil fuels", "Nuclear", "Renewable (excl. hydro)", "Hydroelectricity", "Natural gas", "Oil", "Coal")
        if (theme$with_title) {
            title <- paste0("Electricity production composition - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of total"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 6) {
        # Energy net Imports
        # EG_IMP_CONS_ZS             | Energy imports, net (% of energy use)
        # EG_USE_PCAP_KG_OE          | Energy use (kg of oil equivalent per capita)
        # Very interesting! /!\ + need two-axes
        # Net energy imports are estimated as energy use less production, both measured in oil equivalents. A negative value indicates that the country is a net exporter. Energy use refers to use of primary energy before transformation to other end-use fuels, which is equal to indigenous production plus imports and stock changes, minus exports and fuels supplied to ships and aircraft engaged in international transport.
        formula <- c("EG_IMP_CONS_ZS", "EG_IMP_CONS_ZS*EG_USE_PCAP_KG_OE/1000")

        variable <- c("% of energy use", "kg of oil equivalent per 1000 people")
        if (theme$with_title) {
            title <- paste0("Net Energy Imports - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("% of energy use", "kg of oil equivalent per 1000 people")
        theme$right_axis <- variable[2]
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)


    } else if (option == 7) {
        # FORT_500                   | Number of firms in Fortune Global 500, annual ranking of the largest 500 corporations worldwide as measured by total revenue
        formula <- c("100*GDP_C/GDP_WLD_C", "FORT_500")

        # TODO add World GDP PPP in DATA_Y
        variable <- c("GDP in current USD", "Fortune Global 500")
        if (theme$with_title) {
            title <- paste0("Market size and Fortune Global 500 - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- c("% of World GDP", "Number of firms in the ranking")
        theme$right_axis <- variable[2]
        theme$area <- FALSE
        comment <- "The Fortune Global 500 is an annual ranking of the largest 500 corporations worldwide as measured by total revenue."

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = comment)


    }

    invisible(output)

}














