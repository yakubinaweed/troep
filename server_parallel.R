# server_parallel.R
# This module contains the logic for the new "Parallel RefineR" tab.
# It handles data upload, parallel analysis, and rendering of results.

# =========================================================================
# UTILITY FUNCTIONS FOR PARALLEL ANALYSIS
# =========================================================================

# A helper function to generate the plot and return it
generate_refiner_plot_single <- function(model, title, xlab, ylab, ref_low, ref_high) {
  req(model)
  plot(model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
       title = title, xlab = xlab, ylab = ylab)
  
  # Get plot coordinates for annotations
  usr <- par("usr")
  y_max <- usr[4]
  y_label_pos <- y_max * 0.95

  # Add manual lower limit line and text
  if (!is.na(ref_low) && is.numeric(ref_low)) {
    abline(v = ref_low, col = "red", lty = 2, lwd = 2)
    text(x = ref_low, y = y_label_pos,
         labels = round(ref_low, 2),
         col = "red", cex = 1.1, pos = 4)
  }

  # Add manual upper limit line and text
  if (!is.na(ref_high) && is.numeric(ref_high)) {
    abline(v = ref_high, col = "blue", lty = 2, lwd = 2)
    text(x = ref_high, y = y_label_pos,
         labels = round(ref_high, 2),
         col = "blue", cex = 1.1, pos = 2)
  }
}

# Worker function to run refineR on a single subpopulation, with enhanced error capture
run_refiner_task <- function(Data, NBootstrap, model, age_range, value_col, age_col) {
  tryCatch({
    # Filter the data for this specific age range
    filtered_data <- Data %>%
      filter(!!rlang::sym(age_col) >= age_range[1] & !!rlang::sym(age_col) <= age_range[2])
    
    if (nrow(filtered_data) < 50) {
      return(list(error = paste("Insufficient data for age range", paste(age_range, collapse="-"), "(n < 50).")))
    }

    # Ensure the data passed to refineR is a numeric vector
    values_for_analysis <- filtered_data[[value_col]]
    if (!is.vector(values_for_analysis) || !is.numeric(values_for_analysis)) {
        return(list(error = paste("Column", value_col, "is not a numeric vector after filtering.")))
    }
    
    refiner_model <- refineR::findRI(Data = values_for_analysis, NBootstrap = NBootstrap, model = model)
    
    return(list(model = refiner_model, age_range = age_range))
  }, error = function(e) {
    # Use conditionMessage(e) to get the error message as a string
    return(list(error = paste("RefineR analysis failed for age range", paste(age_range, collapse="-"), ":", conditionMessage(e))))
  })
}

# =========================================================================
# MAIN SERVER LOGIC
# =========================================================================

parallelServer <- function(input, output, session, parallel_data_rv, parallel_results_rv, message_rv, analysis_running_rv) {
  
  # Helper function to guess column names
  guess_column <- function(cols_available, common_names) {
    for (name in common_names) {
      match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
      if (length(match_idx) > 0) {
        return(cols_available[match_idx[1]])
      }
    }
    return("")
  }

  # Observer for file upload
  observeEvent(input$parallel_data_file, {
    req(input$parallel_data_file)
    tryCatch({
      data <- readxl::read_excel(input$parallel_data_file$datapath)
      parallel_data_rv(data)
      message_rv(list(type = "success", text = "Parallel analysis data uploaded successfully."))

      col_names <- colnames(data)
      all_col_choices_with_none <- c("None" = "", col_names)

      updateSelectInput(session, "parallel_col_value", choices = all_col_choices_with_none, selected = guess_column(col_names, c("HB_value", "Value", "Result", "Measurement", "Waarde")))
      updateSelectInput(session, "parallel_col_age", choices = all_col_choices_with_none, selected = guess_column(col_names, c("leeftijd", "age", "AgeInYears", "Years")))
      updateSelectInput(session, "parallel_col_gender", choices = all_col_choices_with_none, selected = guess_column(col_names, c("geslacht", "gender", "sex", "Gender", "Sex")))
      
    }, error = function(e) {
      message_rv(list(type = "error", text = paste("Error reading parallel file:", e$message)))
      parallel_data_rv(NULL)
    })
  })
  
  # Observer for the Reset button
  observeEvent(input$parallel_reset_btn, {
    parallel_data_rv(NULL)
    parallel_results_rv(NULL)
    shinyjs::reset("parallel_data_file")
    updateSelectInput(session, "parallel_col_value", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "parallel_col_age", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "parallel_col_gender", choices = c("None" = ""), selected = "")
    message_rv(list(text = "Parallel analysis inputs reset.", type = "info"))
  })

  # Observer for the Analyze button
  observeEvent(input$parallel_analyze_btn, {
    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return()
    }
    
    req(parallel_data_rv(), input$parallel_age_ranges_str, input$parallel_col_value, input$parallel_col_age)
    
    data_raw <- parallel_data_rv()
    
    # Custom checks
    if (input$parallel_col_value == "" || input$parallel_col_age == "") {
      message_rv(list(text = "Please select the value and age columns.", type = "error"))
      return(NULL)
    }

    # CRITICAL FIX: Check if selected columns are numeric
    if (!is.numeric(data_raw[[input$parallel_col_value]]) || !is.numeric(data_raw[[input$parallel_col_age]])) {
      message_rv(list(text = "Error: Value and Age columns must contain numeric data. Please check your file.", type = "error"))
      return(NULL)
    }
    
    if (input$parallel_col_value == input$parallel_col_age) {
      message_rv(list(text = "Error: The same column cannot be selected for both Values and Age.", type = "error"))
      return(NULL)
    }

    age_ranges_str <- input$parallel_age_ranges_str
    if (nchar(trimws(age_ranges_str)) == 0) {
      message_rv(list(text = "Please enter at least one age range.", type = "error"))
      return()
    }

    # Parse age ranges from the string
    age_ranges <- strsplit(age_ranges_str, ",")[[1]]
    parsed_ranges <- lapply(age_ranges, function(range) {
      parts <- as.numeric(strsplit(trimws(range), "-")[[1]])
      if (length(parts) == 2 && all(!is.na(parts))) {
        return(parts)
      } else {
        message_rv(list(text = paste("Invalid age range format:", range), type = "error"))
        return(NULL)
      }
    }) %>% purrr::compact()

    if (length(parsed_ranges) == 0) {
      message_rv(list(text = "No valid age ranges were found. Please check your input.", type = "error"))
      return()
    }

    # Disable buttons and start analysis
    analysis_running_rv(TRUE)
    shinyjs::disable("parallel_analyze_btn")
    shinyjs::runjs("$('#parallel_analyze_btn').text('Analyzing...');")
    session$sendCustomMessage('analysisStatus', TRUE)
    message_rv(list(text = "Starting parallel analysis...", type = "info"))
    parallel_results_rv(NULL) # Clear previous results

    tryCatch({
      
      # Prepare tasks based on user inputs
      gender_to_analyze <- input$parallel_gender_choice
      n_cores <- as.integer(input$parallel_n_cores)
      n_bootstrap <- as.integer(input$parallel_nbootstrap)
      model_choice <- input$parallel_model_choice
      
      # Filter data by gender first, and importantly, REMOVE ROWS WITH NA VALUES
      if (input$parallel_col_gender != "" && gender_to_analyze != "Both") {
        gender_data <- data_raw %>%
          mutate(Gender_Standardized = case_when(
            grepl("male|m|man", !!rlang::sym(input$parallel_col_gender), ignore.case = TRUE) ~ "Male",
            grepl("female|f|vrouw", !!rlang::sym(input$parallel_col_gender), ignore.case = TRUE) ~ "Female",
            TRUE ~ "Other"
          )) %>%
          filter(Gender_Standardized == gender_to_analyze) %>%
          na.omit() 
      } else {
        # Use a dummy column if no gender column or if 'Both' is selected
        gender_data <- data_raw %>%
          mutate(Gender_Standardized = "Combined") %>%
          na.omit() 
      }

      if (nrow(gender_data) == 0) {
        stop("No data found for the selected gender after filtering.")
      }
      
      # Create a list of tasks for parallel processing
      tasks <- list()
      for (range in parsed_ranges) {
        tasks[[paste0(range[1], "-", range[2])]] <- list(
          Data = gender_data,
          NBootstrap = n_bootstrap,
          model = model_choice,
          age_range = range,
          value_col = input$parallel_col_value,
          age_col = input$parallel_col_age
        )
      }

      message("Starting parallel analysis with", n_cores, "cores and", length(tasks), "tasks.")
      
      # Setup parallel cluster and load necessary libraries on all worker nodes
      cl <- parallel::makeCluster(n_cores)
      
      # CRITICAL FIX: Explicitly export all necessary objects to the cluster
      cluster_export_vars <- c(
        "run_refiner_task",
        "gender_data",
        "n_bootstrap",
        "model_choice",
        "input$parallel_col_value",
        "input$parallel_col_age"
      )
      
      # CRITICAL FIX: Force evaluation of inputs before exporting to the cluster
      col_value_input <- input$parallel_col_value
      col_age_input <- input$parallel_col_age

      parallel::clusterEvalQ(cl, {
        library(refineR)
        library(tidyverse)
        library(rlang)
      })
      
      parallel::clusterExport(cl, varlist = c("run_refiner_task", "gender_data", "n_bootstrap", "model_choice", "col_value_input", "col_age_input"), envir = environment())
      
      # Run the analysis in parallel
      results <- parallel::parLapply(cl, tasks, function(task) {
        run_refiner_task(
          Data = task$Data,
          NBootstrap = task$NBootstrap,
          model = task$model,
          age_range = task$age_range,
          value_col = task$value_col,
          age_col = task$age_col
        )
      })
      parallel::stopCluster(cl)

      message("Parallel analysis complete.")
      
      # Process results
      final_results <- list()
      for (name in names(results)) {
        res <- results[[name]]
        if (!is.null(res$error)) { # Check for the specific error key
          final_results[[name]] <- res
        } else {
          final_results[[name]] <- res$model
        }
      }

      if (all(sapply(final_results, function(x) !is.null(x$error)))) {
        stop("All refineR analyses failed. Please check your data and parameters.")
      }

      parallel_results_rv(final_results)
      message_rv(list(text = "Parallel analysis complete! Results are ready.", type = "success"))

    }, error = function(e) {
      message_rv(list(text = paste("Parallel Analysis Error:", e$message), type = "danger"))
      parallel_results_rv(NULL)
    }, finally = {
      analysis_running_rv(FALSE)
      shinyjs::enable("parallel_analyze_btn")
      shinyjs::runjs("$('#parallel_analyze_btn').text('Analyze');")
      session$sendCustomMessage('analysisStatus', FALSE)
    })
  })

  # Renders the summary table
  output$parallel_summary_table <- renderUI({
    results <- parallel_results_rv()
    req(results)
    
    # Generate the table
    summary_data <- purrr::map_dfr(names(results), ~{
      model_result <- results[[.x]]
      if (inherits(model_result, "refineR")) {
        summ <- summary(model_result)
        tibble(
          Subpopulation = .x,
          `N` = summ$N,
          `Lower Limit` = round(summ$`Reference interval lower limit`, 2),
          `Upper Limit` = round(summ$`Reference interval upper limit`, 2),
          `Model` = summ$model,
          `Mean` = round(summ$mean, 2)
        )
      } else {
        tibble(
          Subpopulation = .x,
          `N` = "N/A",
          `Lower Limit` = "N/A",
          `Upper Limit` = "N/A",
          `Model` = model_result$error,
          `Mean` = "N/A"
        )
      }
    })
    
    fluidRow(
      column(12,
        div(class = "output-box",
            h4(class = "gmm-title", "Summary of Parallel RefineR Results"),
            renderTable({summary_data})
        )
      )
    )
  })

  # Renders the plots
  output$parallel_plots_ui <- renderUI({
    results <- parallel_results_rv()
    req(results)

    # Use purrr to create a list of plot outputs
    plot_list <- purrr::map(names(results), ~{
      plotname <- paste0("plot_", .x)
      model_result <- results[[.x]]
      
      # Generate the plot only if the model is valid
      if (inherits(model_result, "refineR")) {
        plot_output <- renderPlot({
          plot_title <- paste0("RefineR Analysis for Age ", .x)
          xlab_text <- sprintf("%s [%s]", input$parallel_col_value, input$parallel_unit_input)
          
          generate_refiner_plot_single(
            model = model_result,
            title = plot_title,
            xlab = xlab_text,
            ylab = "Relative Frequency",
            ref_low = input$parallel_ref_low,
            ref_high = input$parallel_ref_high
          )
        })
      } else {
        # Display an error message instead of a plot
        plot_output <- renderPlot({
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = paste0("Plot unavailable: ", model_result$error),
                              size = 6, color = "red") +
            ggplot2::theme_void()
        })
      }
      
      # Wrap the plot in a div for layout
      div(
        class = "col-md-6",
        div(class = "output-box",
          h4(class = "gmm-title", paste0("Age Range: ", .x)),
          plot_output
        )
      )
    })
    
    fluidRow(plot_list)
  })
}