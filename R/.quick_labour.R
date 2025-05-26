
in_quick_labour <- function(ISO,
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
        # Unemployment

        # SL_UEM_TOTL_FE_NE_ZS       | Unemployment, female (% of female labor force) (national estimate)
        # SL_UEM_TOTL_FE_ZS          | Unemployment, female (% of female labor force) (modeled ILO estimate)
        # SL_UEM_TOTL_MA_NE_ZS       | Unemployment, male (% of male labor force) (national estimate)
        # SL_UEM_TOTL_MA_ZS          | Unemployment, male (% of male labor force) (modeled ILO estimate)
        # SL_UEM_TOTL_NE_ZS          | Unemployment, total (% of total labor force) (national estimate)
        # SL_UEM_TOTL_ZS             | Unemployment, total (% of total labor force) (modeled ILO estimate)
        formula <- c("SL_UEM_TOTL_FE_NE_ZS", "SL_UEM_TOTL_NE_ZS", "SL_UEM_TOTL_MA_NE_ZS", 
                     "SL_UEM_TOTL_FE_ZS",    "SL_UEM_TOTL_ZS",    "SL_UEM_TOTL_MA_ZS")
        variable <- c("female", "total", "male", "female (ILO)", "total (ILO)", "male (ILO)")


        if (theme$with_title) {
            title <- paste0("Unemployment - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of labor force (Gender-Specific)"
        theme$area <- FALSE
        post_processing <- merge_ILO_NE_var

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing)


    } else if (option == 2) {
        # Unemployment Youth
        # SL_UEM_1524_FE_NE_ZS       | Unemployment, youth female (% of female labor force ages 15-24) (national estimate)
        # SL_UEM_1524_FE_ZS          | Unemployment, youth female (% of female labor force ages 15-24) (modeled ILO estimate)
        # SL_UEM_1524_MA_NE_ZS       | Unemployment, youth male (% of male labor force ages 15-24) (national estimate)
        # SL_UEM_1524_MA_ZS          | Unemployment, youth male (% of male labor force ages 15-24) (modeled ILO estimate)
        # SL_UEM_1524_NE_ZS          | Unemployment, youth total (% of total labor force ages 15-24) (national estimate)
        # SL_UEM_1524_ZS             | Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)
        formula <- c("SL_UEM_1524_FE_NE_ZS", "SL_UEM_1524_NE_ZS", "SL_UEM_1524_MA_NE_ZS", 
                     "SL_UEM_1524_FE_ZS",    "SL_UEM_1524_ZS",    "SL_UEM_1524_MA_ZS")
        variable <- c("female", "total", "male", "female (ILO)", "total (ILO)", "male (ILO)")
        if (theme$with_title) {
            title <- paste0("Unemployment, Youth - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of labor force ages 15-24 (Gender-Specific)"
        theme$area <- FALSE
        post_processing <- merge_ILO_NE_var

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing)

    } else if (option == 3) {
        # Unemployment total and Youth
        formula <- c("SL_UEM_1524_NE_ZS", "SL_UEM_1524_ZS",
                     "SL_UEM_TOTL_NE_ZS", "SL_UEM_TOTL_ZS")
        variable <- c("total", "total (ILO)", "youth", "youth (ILO)")


        if (theme$with_title) {
            title <- paste0("Unemployment (Total vs Youth)- ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of labor force (Age-Specific)"
        theme$area <- FALSE
        post_processing <- function (data) {
            return(merge_ILO_NE_var(data, key_variable = c("total", "youth")))
        }

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing)

    } else if (option == 4) {
        # Employment rate
        # SL_EMP_TOTL_SP_FE_NE_ZS    | Employment to population ratio, 15+, female (%) (national estimate)
        # SL_EMP_TOTL_SP_FE_ZS       | Employment to population ratio, 15+, female (%) (modeled ILO estimate)
        # SL_EMP_TOTL_SP_MA_NE_ZS    | Employment to population ratio, 15+, male (%) (national estimate)
        # SL_EMP_TOTL_SP_MA_ZS       | Employment to population ratio, 15+, male (%) (modeled ILO estimate)
        # SL_EMP_TOTL_SP_NE_ZS       | Employment to population ratio, 15+, total (%) (national estimate)
        # SL_EMP_TOTL_SP_ZS          | Employment to population ratio, 15+, total (%) (modeled ILO estimate)
        formula <- c("SL_EMP_TOTL_SP_ZS", "SL_EMP_TOTL_SP_NE_ZS",
                     "SL_EMP_TOTL_SP_FE_ZS", "SL_EMP_TOTL_SP_FE_NE_ZS",
                     "SL_EMP_TOTL_SP_MA_ZS", "SL_EMP_TOTL_SP_MA_NE_ZS")
        variable <- c("total (ILO)", "total", 
                      "female (ILO)", "female",
                      "male (ILO)", "male")


        if (theme$with_title) {
            title <- paste0("Employment - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "%  Working age population (age 15+)"
        theme$area <- FALSE
        post_processing <- merge_ILO_NE_var

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, post_processing = post_processing)


    } else if (option == 5) {
        # Employment - type of jobs 
        # SL_EMP_MPYR_ZS             | Employers, total (% of total employment) (modeled ILO estimate)
        # SL_EMP_SELF_ZS             | Self-employed, total (% of total employment) (modeled ILO estimate)
        # SL_EMP_VULN_ZS             | Vulnerable employment, total (% of total employment) (modeled ILO estimate)
        # SL_EMP_WORK_ZS             | Wage and salaried workers, total (% of total employment) (modeled ILO estimate)
        # SL_FAM_WORK_ZS             | Contributing family workers, total (% of total employment) (modeled ILO estimate)
        # The hierarchy would look like this:
        #     SL_EMP_WORK_ZS (wage/salaried)
        #     SL_EMP_SELF_ZS (self-employed)
        #         SL_EMP_MPYR_ZS (employers)
        #         SL_EMP_VULN_ZS (vulnerable employment)
        #             SL_FAM_WORK_ZS (contributing family workers)
        #             Own-account workers (implied but not directly measured in your indicators)
        formula <- c("SL_EMP_WORK_ZS+SL_EMP_SELF_ZS",
                     "SL_FAM_WORK_ZS", "SL_EMP_WORK_ZS", "SL_EMP_VULN_ZS-SL_FAM_WORK_ZS", "SL_EMP_MPYR_ZS")
        variable <- c("Total", "Contributing family workers", "wage/salaried", "Own-account workers", "Employers")

        if (theme$with_title) {
            title <- paste0("Employment by types of jobs - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of total employment"

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)


    } else if (option == 6) {
        # Employment - type of jobs (non-area) 
        formula <- c("SL_FAM_WORK_ZS", "SL_EMP_WORK_ZS", "SL_EMP_VULN_ZS-SL_FAM_WORK_ZS", "SL_EMP_MPYR_ZS")
        variable <- c("Contributing family workers", "Wage/salaried", "Own-account workers", "Employers")


        if (theme$with_title) {
            title <- paste0("Unemployment by types of jobs - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of total employment"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 7) {
        # Employment by sector (industry, services, agriculture)
        # SL_IND_EMPL_FE_ZS          | Employment in industry, female (% of female employment) (modeled ILO estimate)
        # SL_IND_EMPL_MA_ZS          | Employment in industry, male (% of male employment) (modeled ILO estimate)
        # SL_IND_EMPL_ZS             | Employment in industry (% of total employment) (modeled ILO estimate)
        # SL_SRV_EMPL_FE_ZS          | Employment in services, female (% of female employment) (modeled ILO estimate)
        # SL_SRV_EMPL_MA_ZS          | Employment in services, male (% of male employment) (modeled ILO estimate)
        # SL_SRV_EMPL_ZS             | Employment in services (% of total employment) (modeled ILO estimate)
        # SL_AGR_EMPL_FE_ZS          | Employment in agriculture, female (% of female employment) (modeled ILO estimate)
        # SL_AGR_EMPL_MA_ZS          | Employment in agriculture, male (% of male employment) (modeled ILO estimate)
        # SL_AGR_EMPL_ZS             | Employment in agriculture (% of total employment) (modeled ILO estimate)
        formula <- c("SL_IND_EMPL_ZS", "SL_SRV_EMPL_ZS", "SL_AGR_EMPL_ZS")
        variable <- c("Industry", "Services", "Agriculture")

        if (theme$with_title) {
            title <- paste0("Employment Distribution by Economic Sector - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- "% of total employment"
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL)

    } else if (option == 8) {
        # LWR_IX is not available for many countries but is interesting
        # LWR_IX                     | Labor Markets, Wage Rates, Index (base year:2010=100)
        formula <- c("LWR_IX*GDP_R_2015/GDP_C", "LWR_IX")
        variable <- c("Real (GDP deflator)", "Current")

        if (theme$with_title) {
            title <- paste0("Wages rates - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
        }
        theme$y_axis <- paste0("Index (", years[1], " = 100)")
        theme$area <- FALSE

        output <- in_quick_helper(ISO = ISO, years = years, 
                                       formula = formula, variable = variable, 
                                       theme = theme, title = title, print = print, filename = NULL, 
                                       comment = NULL, idx = TRUE)

    }

    invisible(output)

}





merge_ILO_NE_var <- function(data, key_variable = c("female", "male", "total"), min_consecutive = 6) {
  # Get unique combinations of ISO and Date from original data, excluding ILO
  order_template <- unique(data[!grepl("ILO", data$Variable), c("ISO", "Date", "Variable")])
  
  # Process the data as before
  results_list <- lapply(key_variable, function(var) {
    check_consecutive <- function(x) max(rle(!is.na(x))$lengths[rle(!is.na(x))$values]) >= min_consecutive
    valid_regular <- tapply(data[data$Variable == var, "Value"], data[data$Variable == var, "ISO"], check_consecutive)
    valid_ilo <- tapply(data[data$Variable == paste0(var, " (ILO)"), "Value"], 
                       data[data$Variable == paste0(var, " (ILO)"), "ISO"], check_consecutive)
    wide_data <- reshape(data[data$Variable %in% c(var, paste0(var, " (ILO)")), ],
                        idvar = c("ISO", "Date"), timevar = "Variable", direction = "wide")
    vals <- sapply(1:nrow(wide_data), function(i) {
      use_reg <- !is.null(valid_regular[wide_data$ISO[i]]) && valid_regular[wide_data$ISO[i]]
      use_ilo <- !is.null(valid_ilo[wide_data$ISO[i]]) && valid_ilo[wide_data$ISO[i]]
      if(use_reg && use_ilo) mean(unlist(wide_data[i, paste0("Value.", c(var, paste0(var, " (ILO)")))]), na.rm=TRUE)
      else if(use_reg) wide_data[i, paste0("Value.", var)]
      else if(use_ilo) wide_data[i, paste0("Value.", paste0(var, " (ILO)"))]
      else NA
    })
    data.frame(ISO = wide_data$ISO, Variable = var, Date = wide_data$Date, 
               Value = replace(vals, is.nan(vals), NA))
  })
  
  final_result <- do.call(rbind, results_list)
  
  # Merge with order template to maintain original order
  merge(order_template, final_result, by = c("ISO", "Date", "Variable"), sort = FALSE)
}



# Note:
# LUR_PT                     | Labor Markets, Unemployment Rate, Percent
# is not a good quality indicator, not enough data (but in quarterly)



