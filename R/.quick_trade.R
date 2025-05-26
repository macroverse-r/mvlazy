in_quick_trade <- function(ISO,
                           years,
                           option = 1,
                           theme = list(),
                           print = TRUE,
                           filename = NULL) {
  title <- NULL
  if (is.null(theme$with_title)) {
    theme$with_title <- FALSE
  }
  if (option %in% 1:4) {
    if (is.null(theme$dim)) {
      theme$dim <- c(16, 10)
    }
    if (is.null(theme$base_size)) {
      theme$base_size <- 14
    }
  } else if (option == 5) {
    if (is.null(theme$dim)) {
      theme$dim <- c(16, 10)
    }
    if (is.null(theme$base_size)) {
      theme$base_size <- 14
    }
  } else {
    stop(paste0("ERROR: option '", option, "' is not a valid argument in 'in_quick_trade'"))
  }

  if (option == 1) {
    # GENERAL TRADE DECOMPOSITION BY CATEGORY
    formula_exs <- c("100", "(TX_VAL_INSF_ZS_WT+TX_VAL_OTHR_ZS_WT)", "(TX_VAL_TRAN_ZS_WT+TX_VAL_TRVL_ZS_WT)")
    formula_exs <- paste0(formula_exs, "*TX_VAL_SERV_CD_WT/GDP_C_Y")
    formula_ims <- c("100", "(TM_VAL_INSF_ZS_WT+TM_VAL_OTHR_ZS_WT)", "(TM_VAL_TRAN_ZS_WT+TM_VAL_TRVL_ZS_WT)")
    formula_ims <- paste0(formula_ims, "*TM_VAL_SERV_CD_WT/GDP_C_Y")
    variable_s <- c("Total Service", "Computer, financial and other serv.", "Transport/Travel/Tourism services")

    formula_exg <- c("(TX_VAL_AGRI_ZS_UN+TX_VAL_FOOD_ZS_UN)", "TX_VAL_FUEL_ZS_UN", "TX_VAL_MANF_ZS_UN", "TX_VAL_MMTL_ZS_UN")
    formula_exg <- paste0(formula_exg, "*TX_VAL_MRCH_CD_WT/GDP_C_Y")
    formula_img <- c("(TM_VAL_AGRI_ZS_UN+TM_VAL_FOOD_ZS_UN)", "TM_VAL_FUEL_ZS_UN", "TM_VAL_MANF_ZS_UN", "TM_VAL_MMTL_ZS_UN")
    formula_img <- paste0(formula_img, "*TM_VAL_MRCH_CD_WT/GDP_C_Y")
    variable_g <- c("Agriculture and Food", "Fuel", "Manufactures exports", "Ores and Metals")

    formula_ex <- c(formula_exs[1], formula_exg, formula_exs[2:3])
    formula_im <- c(formula_ims[1], formula_img, formula_ims[2:3])
    variable <- c(variable_s[1], variable_g, variable_s[2:3])

    if (theme$with_title) {
      title <- paste0("Composition of International Trade by Category - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
    }
    theme$y_axis <- "% of GDP"

    output <- in_quick_ex_im_area_helper(
      ISO = ISO, years = years,
      formula_ex = formula_ex, formula_im = formula_im,
      variable = variable, theme = theme, title = title,
      print = print, filename = filename
    )
  } else if (option == 2) {
    if (is.null(theme$include_ict)) {
      theme$include_ict <- FALSE
    }
    # DECOMPOSITION GOODS EX/IMPORTS BY CATEGORY (% of merchandise exports)
    # TX_VAL_AGRI_ZS_UN          | Agricultural raw materials exports (% of merchandise exports)
    # TX_VAL_FOOD_ZS_UN          | Food exports (% of merchandise exports)
    # TX_VAL_FUEL_ZS_UN          | Fuel exports (% of merchandise exports)
    # TX_VAL_MANF_ZS_UN          | Manufactures exports (% of merchandise exports)
    # TX_VAL_MMTL_ZS_UN          | Ores and metals exports (% of merchandise exports)
    # TX_VAL_ICTG_ZS_UN          | ICT goods exports (% of total goods exports)
    # TM_VAL_AGRI_ZS_UN          | Agricultural raw materials imports (% of merchandise imports)
    # TM_VAL_FOOD_ZS_UN          | Food imports (% of merchandise imports)
    # TM_VAL_FUEL_ZS_UN          | Fuel imports (% of merchandise imports)
    # TM_VAL_MANF_ZS_UN          | Manufactures imports (% of merchandise imports)
    # TM_VAL_MMTL_ZS_UN          | Ores and metals imports (% of merchandise imports)
    # TM_VAL_ICTG_ZS_UN          | ICT goods imports (% total goods imports)
    if (theme$include_ict) {
      formula_ex <- c("100", "TX_VAL_ICTG_ZS_UN", "(TX_VAL_MANF_ZS_UN-TX_VAL_ICTG_ZS_UN)", "TX_VAL_MMTL_ZS_UN", "TX_VAL_FOOD_ZS_UN", "TX_VAL_AGRI_ZS_UN", "TX_VAL_FUEL_ZS_UN")
      formula_im <- c("100", "TM_VAL_ICTG_ZS_UN", "(TM_VAL_MANF_ZS_UN-TM_VAL_ICTG_ZS_UN)", "TM_VAL_MMTL_ZS_UN", "TM_VAL_FOOD_ZS_UN", "TM_VAL_AGRI_ZS_UN", "TM_VAL_FUEL_ZS_UN")
      variable <- c("Total", "ICT goods", "Manufactures exports", "Ores and Metals", "Agriculture raw materials", "Food", "Fuel")
    } else {
      formula_ex <- c("100", "TX_VAL_MANF_ZS_UN", "TX_VAL_MMTL_ZS_UN", "TX_VAL_FOOD_ZS_UN", "TX_VAL_AGRI_ZS_UN", "TX_VAL_FUEL_ZS_UN")
      formula_im <- c("100", "TM_VAL_MANF_ZS_UN", "TM_VAL_MMTL_ZS_UN", "TM_VAL_FOOD_ZS_UN", "TM_VAL_AGRI_ZS_UN", "TM_VAL_FUEL_ZS_UN")
      variable <- c("Total", "Manufactures exports", "Ores and Metals", "Food", "Agriculture raw materials", "Fuel")
    }

    formula_ex <- paste0(formula_ex, "*TX_VAL_MRCH_CD_WT/GDP_C_Y")
    formula_im <- paste0(formula_im, "*TM_VAL_MRCH_CD_WT/GDP_C_Y")

    if (theme$with_title) {
      title <- paste0("Composition of Goods Trade by Category - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
    }
    theme$y_axis <- "% of GDP"

    output <- in_quick_ex_im_area_helper(
      ISO = ISO, years = years,
      formula_ex = formula_ex, formula_im = formula_im,
      variable = variable, theme = theme, title = title,
      print = print, filename = filename
    )
  } else if (option == 3) {
    # DECOMPOSITION SERVICES EX/IMPORTS BY CATEGORY (% of service ex/imports)
    # TM_VAL_INSF_ZS_WT          | Insurance and financial services (% of commercial service imports)
    # TM_VAL_OTHR_ZS_WT          | Computer, communications and other services (% of commercial service imports)
    # TM_VAL_TRAN_ZS_WT          | Transport services (% of commercial service imports)
    # TM_VAL_TRVL_ZS_WT          | Travel services (% of commercial service imports)
    # TX_VAL_INSF_ZS_WT          | Insurance and financial services (% of commercial service exports)
    # TX_VAL_OTHR_ZS_WT          | Computer, communications and other services (% of commercial service exports)
    # TX_VAL_TRAN_ZS_WT          | Transport services (% of commercial service exports)
    # TX_VAL_TRVL_ZS_WT          | Travel services (% of commercial service exports)

    formula_ex <- c("100", "TX_VAL_INSF_ZS_WT", "TX_VAL_OTHR_ZS_WT", "TX_VAL_TRAN_ZS_WT", "TX_VAL_TRVL_ZS_WT")
    formula_ex <- paste0(formula_ex, "*TX_VAL_SERV_CD_WT/GDP_C_Y")
    formula_im <- c("100", "TM_VAL_INSF_ZS_WT", "TM_VAL_OTHR_ZS_WT", "TM_VAL_TRAN_ZS_WT", "TM_VAL_TRVL_ZS_WT")
    formula_im <- paste0(formula_im, "*TM_VAL_SERV_CD_WT/GDP_C_Y")
    variable <- c(
      "Total", "Insurance and financial services", "Computer, communications and other services",
      "Transport (freight/shipping, flight, equipment)", "Travel/Tourism (accommodation, food, local transports)"
    )

    if (theme$with_title) {
      title <- paste0("Composition of Services Trade by Category - ", wp_from_iso(ISO), " (", years[1], "-", years[2], ")")
    }
    theme$y_axis <- "% of GDP"

    output <- in_quick_ex_im_area_helper(
      ISO = ISO, years = years,
      formula_ex = formula_ex, formula_im = formula_im,
      variable = variable, theme = theme, title = title,
      print = print, filename = filename
    )
  } else if (option == 4) {
    # exports/Imports = Value x Quantity

    # TM_QTY_MRCH_XD_WD          | Import volume index (2015 = 100)
    # TM_VAL_MRCH_XD_WD          | Import value index (2015 = 100)
    # TM_UVI_MRCH_XD_WD          | Import unit value index (2015 = 100)
    formula_im <- c("TM_VAL_MRCH_XD_WD/PRC_IDX_WLD", "TM_UVI_MRCH_XD_WD/PRC_IDX_WLD", "TM_QTY_MRCH_XD_WD")
    # TX_QTY_MRCH_XD_WD          | Export volume index (2015 = 100)
    # TX_VAL_MRCH_XD_WD          | Export value index (2015 = 100)
    # TX_UVI_MRCH_XD_WD          | Export unit value index (2015 = 100)
    formula_ex <- c("TX_VAL_MRCH_XD_WD/PRC_IDX_WLD", "TX_UVI_MRCH_XD_WD/PRC_IDX_WLD", "TX_QTY_MRCH_XD_WD")
    variable <- c("Value Index (total)", "Unit value index", "Volume Index")

    if (theme$with_title) {
      title <- "Decomposition of Trade Values into Price and Volume Components"
    }
    theme$y_axis <- paste0("Values in ln(index, ", years[1], " = 1)")

    comment <- paste0("Values in ln(index, ", years[1], " = 1). Volume index x Unit value index = Value index = current value of exports (F.O.B.) / imports (C.I.F.) expressed as a % of the value for the base period.
log values to % changes from ", years[1], ": 1.0 = +171.8% / 0.5 = +64.9% / 0.1 = +10.5% / -0.1 = -9.5% / -0.5 = -39.3% / -1.0 = -63.2%")


    output <- in_quick_ex_im_area_helper(
      ISO = ISO, years = years,
      formula_ex = formula_ex, formula_im = formula_im,
      variable = variable, theme = theme, title = title,
      print = print, filename = filename, idx = "log", comment = comment
    )
  } else if (option == 5) {
    # Terms of trade and balance of trade

    # Terms of Trade
    # PXP_IX/PMP_IX = IFS (Q) / TT_PRI_MRCH_XD_WD = WB (Y)
    formula_tot <- c("TT_PRI_MRCH_XD_WD", "100*PXP_IX/PMP_IX")
    variable_tot <- c("Terms of Trade - World Bank", "Terms of Trade - IMF")
    # Trade balance
    # NE_RSB_GNFS_ZS             | External balance on goods and services (% of GDP)
    # TB_C
    formula_bot <- c("NE_RSB_GNFS_ZS", "100*TB_C/GDP_C")
    variable_bot <- c("Trade Balance - World Bank", "Trade Balance - IMF")

    data_tot <- wp_data(
      ISO = ISO, years = years,
      formula = formula_tot, variable = variable_tot,
      adjust_seasonal = TRUE,
      clean = FALSE, na2zero = theme$na2zero, reference = TRUE
    )

    data_tot <- in_as_index(data_tot, base_value = 100, match_index = TRUE)

    data_bot <- wp_data(
      ISO = ISO, years = years,
      formula = formula_bot, variable = variable_bot,
      adjust_seasonal = TRUE,
      clean = FALSE, na2zero = theme$na2zero, reference = TRUE
    )

    formula <- c(formula_bot, formula_tot)
    variable <- c(variable_bot, variable_tot)
    data <- rbind(data_bot, data_tot)

    # remove not available data
    for (var in variable) {
      data_var <- data[data$Variable == var, ]
      if (sum(!is.na(data_var$Value)) == 0) {
        data <- data[data$Variable != var, ]
        variable_tot <- variable_tot[!variable_tot %in% var]
      }
    }

    if (is.null(theme$align_zeros)) {
      theme$align_zeros <- FALSE
    }
    if (theme$with_title) {
      title <- "Trade Balance and Terms of Trade"
    }

    plot <- wp_plot_series(data,
      theme = theme,
      right_axis = variable_tot,
      y_axis = c("% of GDP", paste0("Index (", years[1], " = 100)")),
      title = title, reference = TRUE,
      key_dates = theme$key_dates,
      print = TRUE, area = FALSE, size = 1
    )

    output <- list(plot = plot, data = data)
  }

  invisible(output)
}
