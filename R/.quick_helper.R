#' @keywords internal
.quick_helper <- function(ISO, years, formula, variable, 
                                       theme, title, print, filename, 
                                       idx = FALSE, comment = NULL, 
                                       post_processing = NULL) {


    if (!theme$with_title) { title <- NULL }
    if (is.null(theme$area)) { theme$area <- TRUE }
    if (is.null(theme$clean)) { theme$clean <- FALSE }
    if (is.null(theme$aggregate_iso)) { theme$aggregate_iso <- "Sum" }
    if (is.null(theme$na.rm)) { theme$na.rm <- FALSE }
    if (is.null(theme$remove_na_var)) { theme$remove_na_var <- FALSE }
    if (theme$match_tot) {
        formula[1] <- paste0(formula[2:length(formula)], collapse = "+")
    }

    data <- macrodata::md_data(ISO = ISO, years = years,
                       formula = formula, variable = variable,
                       adjust_seasonal = theme$sea_adj,
                       aggregate_iso = theme$aggregate_iso, na.rm = theme$na.rm,
                       clean = theme$clean, na2zero = theme$na2zero, reference = TRUE)

    # Filter rows with unique Variable values that have at least one non-NA Value
    if (!is.null(theme$right_axis) || theme$remove_na_var) {
        data <- data[data$Variable %in% unique(data$Variable[!is.na(data$Value)]), ]
    }

    if (nrow(data) == 0) {
        output <- list(plot = NULL, data = data)
        mvcommon::mv_print_debug("Only NA values for all ISO codes and variables. Return empty dataframe.", TRUE, TRUE, type = "warning")
    } else {
        if (!is.null(post_processing)) { data <- post_processing(data) }
        reference <- paste0(unique(data$Reference), collapse = "; ")

        if (idx == TRUE) {
            data <- .as_index(data, log = FALSE, base_value = 100, match_index = TRUE)
        } else if (idx == "log") {
            data <- .as_index(data, log = TRUE, base_value = 1, match_index = TRUE)
        }

        if (!is.null(comment)) { reference <- paste0(reference, " \n\n", comment) }
        data$Reference <- reference

        plot <- pplot::pp_plot_series(data,
                               title = title, reference = TRUE, legend = TRUE,
                               key_dates = theme$key_dates, theme = theme,
                               print = print, area = theme$area, size = 4, 
                               y_axis = theme$y_axis, right_axis = theme$right_axis)

        output <- list(plot = plot, data = data)
    }
    return(output)
}



.quick_ex_im_area_helper <- function(ISO, years, formula_ex, formula_im, variable, 
                                       theme, title, print, filename, idx = FALSE, comment = NULL) {


    if (!theme$with_title) { title <- NULL }
    if (theme$match_tot) {
        formula_ex[1] <- paste0(formula_ex[2:length(formula_ex)], collapse = "+")
        formula_im[1] <- paste0(formula_im[2:length(formula_im)], collapse = "+")
    }

    data_ex <- macrodata::md_data(ISO = ISO, years = years,
                       formula = formula_ex, variable = variable,
                       adjust_seasonal = theme$sea_adj,
                       clean = FALSE, na2zero = theme$na2zero, reference = TRUE)
    data_im <- macrodata::md_data(ISO = ISO, years = years,
                       formula = formula_im, variable = variable,
                       adjust_seasonal = theme$sea_adj,
                       clean = FALSE, na2zero = theme$na2zero, reference = TRUE)

    reference <- paste0("Reference: ", unique(data_ex$Reference))

    if (idx == TRUE) {
        data_ex <- .as_index(data_ex, log = FALSE, base_value = 100, match_index = TRUE)
        data_im <- .as_index(data_im, log = FALSE, base_value = 100, match_index = TRUE)
    } else if (idx == "log") {
        data_ex <- .as_index(data_ex, log = TRUE, base_value = 1, match_index = TRUE)
        data_im <- .as_index(data_im, log = TRUE, base_value = 1, match_index = TRUE)
    }

    if (!is.null(comment)) { reference <- paste0(reference, "\n\n", comment) }


    y_axis_im <- if (!is.null(theme$only) && theme$only == "im") { theme$y_axis } else { NULL }

    plot_ex <- pplot::pp_plot_series(data_ex,
                              title = "Exports", reference = FALSE,
                              key_dates = theme$key_dates, theme = theme,
                              print = FALSE, area = TRUE, size = 4, y_axis = theme$y_axis)
    plot_ex + ggplot2::theme(legend.position = "none")
    plot_im <- pplot::pp_plot_series(data_im,
                              title = "Imports", reference = FALSE,
                              key_dates = theme$key_dates, theme = theme,
                              print = FALSE, area = TRUE, size = 4, y_axis = y_axis_im)
    plot_im + ggplot2::theme(legend.position = "none")

    leg <- plot_ex[[2]]
    plot_ex[[2]] <- NULL
    plot_im[[2]] <- NULL
    plot_ex <- plot_ex + patchwork::plot_layout(heights = c(1, 0))
    plot_im <- plot_im + patchwork::plot_layout(heights = c(1, 0))


    if (is.null(theme$only)) {
        plot_list <- list(plot_ex, plot_im)
        ncol <- 2
    } else if (theme$only == "ex") {
        plot_list <- list(plot_ex)
        ncol <- 1
    } else if (theme$only == "im") {
        plot_list <- list(plot_im)
        ncol <- 1
    }
    plot <- pplot::pp_plot_combine(plot_list, 
                            legend = leg, title = title, reference = reference,
                            print = print, filename = filename, dim = theme$dim, ncol = ncol)

    data <- list(ex = data_ex, im = data_im)
    output <- list(plot = plot, data = data)
    return(output)
}









.as_index <- function(data, log = FALSE, base_value = 1, match_index = TRUE) {

    if (nrow(data) == 0) { return(data) }
    # Order the data
    init_order <- unique(data$Variable)
    data <- data[order(data$ISO, data$Variable, data$Date), ]

    # Create normalized values manually by groups
    # First, initialize the normalized column
    # print(head(data))
    data$Value_norm <- NA

    # Get unique combinations of ISO and Variable
    isos <- unique(data[, "ISO"])
    vars <- unique(data[, "Variable"])

    # Loop through each combination and normalize
    for (iso in isos) {
        # Fix the first value to base_value
        first_value_idx <- rep(NA, length(vars))
        names(first_value_idx) <- vars
        for (var in vars) {

            # Get indices for this combination
            indices <- which(data$ISO == iso & data$Variable == var)

            # Get values for this combination
            values <- data$Value[indices]

            # Normalize only if there are non-NA values
            if(!all(is.na(values))) {
                # First non-na value
                first_value_idx[var] <- min(which(!is.na(values)))
                first_val <- values[first_value_idx[var]]  # Get first value
                data$Value_norm[indices] <- base_value*values/first_val
            } else {
                first_value_idx[var] <- NA
            }
        }

        if (sum(first_value_idx != 1, na.rm = TRUE) > 0) {
            mvcommon::mv_print_debug(paste0("ISO: ", iso, " does not have values for the first date for all variable. Indices for the first values are:"), 
                           verbose = TRUE, debug = FALSE, type = "warning")
            print(first_value_idx)
        }

        which_freq <- function (data, var) {
            date <- data$Date[which(data$ISO == iso & data$Variable == var)][1]
            freq <- if (nchar(date) == 6) "Q" else "Y"
            return(freq)
        }
        which_first_year <- function(var) {
            date <- data$Date[which(data$ISO == iso & data$Variable == var)][1]
            year <- sub("^X?(\\d{4}).*$", "\\1", date)
            return(as.numeric(year))
        }

        # Match values to the variable with the first available values
        if (match_index && sum(!is.na(first_value_idx)) > 0) {
            base_var <- names(which.min(first_value_idx))
            # print(base_var)
            base_var_values <- data$Value_norm[which(data$ISO == iso & data$Variable == base_var)]
            base_var_freq <- which_freq(data, base_var)
            base_var_year <- which_first_year(base_var)
            for (var in vars[vars != base_var]) {

                # Get indices for this combination
                indices <- which(data$ISO == iso & data$Variable == var)
                # Get values for this combination
                values <- data$Value_norm[indices]
                freq <- which_freq(data, var)
                year_gap <- which_first_year(var) - which_first_year(base_var)

                # print("==========================================================")
                # print(paste0("var = ", var))
                # print(paste0("freq = ", freq, " | base_var_freq = ", base_var_freq))
                # print("values:")
                # print(values)
                # print(paste0("which_first_year(var):", which_first_year(var)))
                # print(!all(is.na(values)))
                # print(!all(!is.na(values)))
                # print(year_gap)
                # print(year_gap>0)
                # print((!all(!is.na(values)) || year_gap>0))

                if (year_gap < 0){
                    mvcommon::mv_print_debug("Warning: year_gap is negative but this case has not been implemented yet", FALSE, TRUE, type="warning")
                }
                if (!all(is.na(values)) && (!all(!is.na(values)) || year_gap>0) ) {
                    # First non-na value
                    first_idx <- min(which(!is.na(values)))

                    if (freq == "Y" && base_var_freq == "Y") {
                        first_idx_base_var <- first_idx + year_gap
                    } else if (freq == "Q" && base_var_freq == "Q") {
                        first_idx_base_var <- first_idx + year_gap*4
                    } else if (freq == "Y" && base_var_freq == "Q") {
                        first_idx_base_var <- round((first_idx+year_gap-1)*4 + 2)
                    } else if (freq == "Q" && base_var_freq == "Y") {
                        first_idx_base_var <- round((first_idx-1)/4 + year_gap + 1)
                    }

                    # first value = value of the base variable for the same index
                    base_value <- base_var_values[first_idx_base_var] 
                    # print(paste0("base_value = '", base_value, "'  at index = '", first_idx_base_var,"'"))
                    first_val <- values[first_idx]  # Get first value
                    data$Value_norm[indices] <- base_value*values/first_val
                }

            }
        }
    }

    if (log) {
        data$Value <- log(data$Value_norm)
    } else {
        data$Value <- data$Value_norm
    }
    data$Value_norm <- NULL

    # Using match
    data <- data[order(match(data$Variable, 
                             init_order )), ]

}

