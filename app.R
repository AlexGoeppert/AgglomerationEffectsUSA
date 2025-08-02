# ============================================================================
# Shiny App for MSA and County Level Analysis (V29 – with First-Stage F-Stat)
#   • Added First-Stage F-Statistic reporting for IV regressions
#   • Enhanced professional table formatting
#   • Improved instrument strength diagnostics
# ============================================================================

# --- 1. Load Required Packages ------------------------------------------------
library(shiny)
library(fixest)
library(dplyr)
library(haven)
library(glue)
library(stringr)
library(shinycssloaders) # For withSpinner
library(DT) # For professional table display

# --- 2. Load Data on Startup --------------------------------------------------
tryCatch({
  msa_data    <- haven::read_dta("MSA_analysis_data.dta")
  county_data <- haven::read_dta("master_county_build.dta")
}, error = function(e) {
  stop("Error: Make sure 'MSA_analysis_data.dta' and 'master_county_build.dta' are in the same directory as app.R")
})

# --- 3. Territory Filtering Function ------------------------------------------
get_territory_exclusions <- function(data, fe_column_name, id_column) {
  territory_patterns <- c("territory", "territoy", "territ", "organised", "organized", "district", "columbia", "\\bdc\\b")
  
  if (!fe_column_name %in% names(data)) {
    return(c()) # Return empty vector if column doesn't exist
  }
  
  territory_ids <- data %>%
    filter(
      if_any(all_of(fe_column_name), ~ str_detect(tolower(as.character(.)), 
                                                  paste(territory_patterns, collapse = "|")))
    ) %>%
    pull(!!sym(id_column)) %>%
    unique()
  
  return(territory_ids)
}

# --- 4. Standardized help‑text strings  --------------------------------------
# Master settings
help_analysis_level      <- "Pick the geography to study: entire metro areas (MSAs) or individual counties."
help_year_modern         <- "Year in which today's productivity and employment data are measured."
help_analysis_type       <- paste(
  "Estimation method:",
  " • OLS – simple correlation.",
  " • IV – corrects endogeneity with a historical density instrument.",
  " • First Stage – shows only the instrument → density regression.",
  sep = "\n")

# Fixed effects & scope
help_use_fe              <- "State fixed effects remove any time‑invariant factors shared by locations in the same state."
help_fe_type             <- "Choose the state borders for those fixed effects: modern borders or borders in the instrument year."
help_sample_scope        <- paste(
  "Which modern places stay in the sample?",
  " • States Only – inside states admitted by the chosen year.",
  " • States & Territories – also inside organised U.S. territories.",
  sep = "\n")

# Standardized settings (used for both MSA and County)
help_sectors             <- "Select the productivity measure used to calculate productivity and density."
help_sample_year         <- "Historical year that defines which modern geographic units lie inside the U.S. border."
help_iv_year             <- "Historical year that supplies the population‑density instrument."
help_controls            <- "Add optional historical controls (waterways, early railroads, etc.)."
help_schooling_adj       <- paste(
  "Adjust productivity for schooling:",
  " • None – no adjustment.",
  " • Same Return – nationwide return to schooling.",
  " • Specific Return – return varies by geographic unit.",
  " • Run Both – estimate both adjustments.",
  sep = "\n")
help_apply_college_adj   <- "Subtract coef × modern college‑educated share from productivity."
help_college_coeff       <- "Coefficient used for the college‑share adjustment."
help_mining_filter_active<- "Exclude geographic units whose mining share exceeds the threshold below."
help_mining_threshold    <- "Maximum allowed mining share of total output."
help_se_spec             <- paste(
  "Standard‑error option:",
  " • Cluster on Instrument – clusters by shared historical instrument.",
  " • Cluster on State – clusters by modern state.",
  " • Spatial (Conley) – distance‑based correction.",
  " • Robust – heteroskedasticity‑robust.",
  sep = "\n")
help_spatial_cutoff      <- "Distance in km beyond which spatial correlation is set to zero."

# MSA‑specific settings
help_exclude_multistate_msa  <- "Drop MSAs that cross more than one state."
help_msa_instrument_type     <- paste(
  "How to link historical counties with today's MSA:",
  " • Intersect – any overlap counts.",
  " • Overlap – only if ≥ chosen % of the county lies inside the MSA.",
  sep = "\n")
help_msa_overlap_pct         <- "Minimum % of a historical county's area that must fall inside the MSA (for the Overlap instrument)."

# County‑specific settings
help_county_msa_restriction  <- "Keep only counties that belong to a modern MSA."
help_county_instrument_type  <- paste(
  "Instrument construction:",
  " • Max Density – takes the highest historical density.",
  " • Weighted Density – weighted average of relevant historical counties.",
  "Use 'Overlap' or 'Intersect' versions to decide how counties qualify.",
  sep = "\n")
help_county_overlap_threshold <- "Minimum % overlap for a historical county that must fall inside the modern county (for Overlap instruments)."

# --- Helper for labelled inputs ----------------------------------------------
labeledInput <- function(inputId, labelText, inputUI, helpId, helpText) {
  div(
    class = "labeled-input-container",
    tags$label(labelText, `for` = inputId, class = "control-label"),
    div(class = "input-button-row", inputUI, actionButton(helpId, NULL, icon = icon("question-circle"), class = "help-btn")),
    conditionalPanel(
      condition = glue("input.{helpId} % 2 == 1"),
      div(class = "help-text", helpText)
    )
  )
}

# ============================================================================
# 5. User Interface (UI) -------------------------------------------------------
# ============================================================================
ui <- fluidPage(
  titlePanel("Regression Interface: MSA and County Level Analysis"),
  
  # Enhanced CSS for professional table styling
  tags$head(tags$style(HTML("
    .labeled-input-container{margin-bottom:15px}
    .input-button-row{display:flex;align-items:center;gap:8px}
    .input-button-row .shiny-input-container{flex-grow:1;width:auto!important;margin-bottom:0}
    .labeled-input-container .shiny-options-group{padding-top:5px}
    .help-btn{background:none;border:none;color:#007BFF;font-size:20px;padding:0 5px;cursor:pointer;align-self:center;line-height:1;transition:color .2s ease,transform .2s ease}
    .help-btn:hover{color:#0056b3;transform:scale(1.1)}
    .help-btn:focus{outline:none;box-shadow:none}
    .help-text{background-color:#f8f9fa;border-left:4px solid #007BFF;padding:10px 15px;margin-top:8px;font-size:13px;color:#495057;border-radius:0 4px 4px 0}
    #instrument_map_render img{max-width:100%;height:auto;border:1px solid #ddd;border-radius:4px}
    
    /* LaTeX-style professional table styling */
    .regression-table {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 16px;
      margin: 20px auto;
      border-collapse: collapse;
      width: 100%;
      max-width: 900px;
      background-color: white;
    }
    .regression-table th {
      color: #000;
      font-weight: normal;
      text-align: center;
      padding: 10px 14px;
      border-top: 2px solid #000;
      border-bottom: 1px solid #000;
      border-left: none;
      border-right: none;
      background-color: white;
    }
    .regression-table td {
      padding: 5px 14px;
      text-align: center;
      border: none;
      background-color: white;
    }
    .regression-table .row-label {
      text-align: left;
      font-weight: normal;
      background-color: white;
      padding-left: 10px;
    }
    .regression-table .coefficient {
      font-weight: normal;
      color: #000;
      font-size: 16px;

    }
    .regression-table .se {
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .stats {
      background-color: white;
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .top-border {
      border-top: 1px solid #000;
    }
    .regression-table .bottom-border {
      border-bottom: 2px solid #000;
    }
    .table-notes {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 12px;
      font-style: italic;
      margin-top: 5px;
      text-align: left;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details {
      background-color: #F0F0F0;
      padding: 15px;
      border-radius: 5px;
      margin-top: 20px;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      font-size: 14px;
      color: #495057;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details h5 {
      margin-top: 0;
      color: #343a40;
      font-weight: 600;
    }
  "))),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # ── 1. Master Configuration ───────────────────────────
                 h4("1. Master Configuration"),
                 labeledInput("analysis_level", "Select Analysis Level:",
                              selectInput("analysis_level", NULL, c("MSA", "County")),
                              "help_analysis_level", help_analysis_level),
                 labeledInput("year_modern", "Select Modern Year:",
                              selectInput("year_modern", NULL, seq(2022, 2001, -1), 2010),
                              "help_year_modern", help_year_modern),
                 div(style="margin-top:20px;", actionButton("run_analysis", "Run Analysis", icon=icon("play-circle"), class="btn-primary btn-lg", width="100%")),
                 hr(),
                 
                 # Display options
                 h4("Display Options"),
                 checkboxInput("show_map", "Display Instrument Map", value = TRUE),
                 conditionalPanel("input.show_map == true", sliderInput("map_size", "Map Size:", min = 25, max = 200, value = 75, post = "%")),
                 hr(),
                 
                 # ── 2. Analysis Settings ───────────────────────────────
                 h4("2. Analysis Settings"),
                 labeledInput("analysis_type", "Analysis Method:",
                              selectInput("analysis_type", NULL, c("IV", "OLS", "FirstStage"), "IV"),
                              "help_analysis_type", help_analysis_type),
                 hr(),
                 
                 # ── 3. Fixed Effects & Sample Scope ───────────────────
                 h4("3. Fixed Effects & Sample Scope"),
                 div(class="input-button-row", style="margin-bottom:10px;",
                     checkboxInput("use_fe", "Use State Fixed Effects", value = TRUE),
                     actionButton("help_use_fe", NULL, icon = icon("question-circle"), class = "help-btn")),
                 conditionalPanel("input.help_use_fe % 2 == 1", div(class="help-text", help_use_fe)),
                 conditionalPanel("input.use_fe == true",
                                  labeledInput("fe_type", "Fixed Effects Type:",
                                               selectInput("fe_type", NULL, c("Historical"="historical", "Modern"="modern"), "historical"),
                                               "help_fe_type", help_fe_type)),
                 labeledInput("sample_scope", "Historical Sample Scope:",
                              selectInput("sample_scope", NULL, c("States Only"="states_only", "States & Territories"="states_territories"), "states_only"),
                              "help_sample_scope", help_sample_scope),
                 hr(),
                 
                 # ── MSA‑specific panels ────────────────────────────────
                 conditionalPanel(
                   condition = "input.analysis_level == 'MSA'",
                   h4("4. MSA Specific Settings"),
                   labeledInput("msa_sectors", "Sector:",
                                selectInput("msa_sectors", NULL, c("All"=1, "Private"=2, "Manufacturing"=3, "Private non-farm"=4, "Private non-farm/mining"=5), 1),
                                "help_msa_sectors", help_sectors),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("exclude_multistate_msa", "Exclude multi‑state MSAs", value = TRUE),
                       actionButton("help_exclude_multistate_msa", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_exclude_multistate_msa % 2 == 1", div(class="help-text", help_exclude_multistate_msa)),
                   labeledInput("msa_sample_year", "Sample Definition Year:",
                                selectInput("msa_sample_year", NULL, seq(1790, 1860, 10), 1790),
                                "help_msa_sample_year", help_sample_year),
                   labeledInput("msa_iv_year", "Instrument Year:",
                                selectInput("msa_iv_year", NULL, seq(1790, 1860, 10), 1840),
                                "help_msa_iv_year", help_iv_year),
                   labeledInput("msa_instrument_type", "Instrument Type:",
                                selectInput("msa_instrument_type", NULL, c("overlap", "intersect")),
                                "help_msa_instrument_type", help_msa_instrument_type),
                   labeledInput("msa_overlap_pct", "Overlap % (if applicable):",
                                selectInput("msa_overlap_pct", NULL, c(5,10,20,30,40,50,60,70,80,90), 5),
                                "help_msa_overlap_pct", help_msa_overlap_pct),
                   labeledInput("msa_controls", "Control Variables:",
                                checkboxGroupInput("msa_controls", NULL, c("Water Access 1820"="water_1820", "Railroads 1840"="railroads_1840", "Railroads 1850"="railroads_1850", "Railroads 1861"="railroads_1861"), c("water_1820", "railroads_1840")),
                                "help_msa_controls", help_controls),
                   labeledInput("msa_schooling_adj", "Schooling Adjustment:",
                                selectInput("msa_schooling_adj", NULL, c("None"=0, "Same Return"=1, "Specific Return"=2, "Run Both"=3), 3),
                                "help_msa_schooling_adj", help_schooling_adj),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("msa_apply_college_adj", "Apply College Share Adjustment", value = TRUE),
                       actionButton("help_msa_apply_college_adj", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_msa_apply_college_adj % 2 == 1", div(class="help-text", help_apply_college_adj)),
                   labeledInput("msa_college_coeff", "College Share Coefficient:",
                                numericInput("msa_college_coeff", NULL, 0.75, 0, 2, 0.05),
                                "help_msa_college_coeff", help_college_coeff),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("msa_mining_filter_active", "Filter by Mining Share", value = FALSE),
                       actionButton("help_msa_mining_filter_active", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_msa_mining_filter_active % 2 == 1", div(class="help-text", help_mining_filter_active)),
                   labeledInput("msa_mining_threshold", "Max Mining Share:",
                                numericInput("msa_mining_threshold", NULL, 0.01, 0, 1, 0.01),
                                "help_msa_mining_threshold", help_mining_threshold),
                   labeledInput("msa_se_spec", "Standard Errors:",
                                selectInput("msa_se_spec", NULL, c("Cluster on Instrument"="cluster_instrument", "Cluster on State"="cluster_state", "Spatial (Conley)"="spatial", "Robust"="robust"), "cluster_instrument"),
                                "help_msa_se_spec", help_se_spec),
                   conditionalPanel("input.msa_se_spec == 'spatial'",
                                    labeledInput("msa_spatial_cutoff", "Spatial Cutoff (km):",
                                                 numericInput("msa_spatial_cutoff", NULL, 100),
                                                 "help_msa_spatial_cutoff", help_spatial_cutoff))
                 ),
                 
                 # ── County‑specific panels ─────────────────────────────
                 conditionalPanel(
                   condition = "input.analysis_level == 'County'",
                   h4("4. County Specific Settings"),
                   labeledInput("county_sectors", "Sector:",
                                selectInput("county_sectors", NULL, c("All"=1, "Private"=2, "Manufacturing"=3, "Private non-farm"=4, "Private non-farm/mining"=5), 1),
                                "help_county_sectors", help_sectors),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_msa_restriction", "Restrict to counties in an MSA", value = FALSE),
                       actionButton("help_county_msa_restriction", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_msa_restriction % 2 == 1", div(class="help-text", help_county_msa_restriction)),
                   labeledInput("county_sample_year", "Sample Definition Year:",
                                selectInput("county_sample_year", NULL, seq(1790, 1840, 10), 1790),
                                "help_county_sample_year", help_sample_year),
                   labeledInput("county_iv_year", "Instrument Year:",
                                selectInput("county_iv_year", NULL, seq(1790, 1840, 10), 1840),
                                "help_county_iv_year", help_iv_year),
                   labeledInput("county_instrument_type", "Instrument Type:",
                                selectInput("county_instrument_type", NULL, c("Max Density Overlap"="max_density_overlap", "Weighted Density Overlap"="weighted_density_overlap", "Max Density Intersect"="max_density_intersect", "Weighted Density Intersect"="weighted_density_intersect"), "max_density_overlap"),
                                "help_county_instrument_type", help_county_instrument_type),
                   labeledInput("county_overlap_threshold", "Overlap Threshold %:",
                                selectInput("county_overlap_threshold", NULL, c(5,10,20,30,40,50,60,70,80,90), 5),
                                "help_county_overlap_threshold", help_county_overlap_threshold),
                   labeledInput("county_controls", "Control Variables:",
                                checkboxGroupInput("county_controls", NULL, c("Water Access 1820"="water_1820", "Railroads 1840"="railroads_1840", "Railroads 1850"="railroads_1850", "Railroads 1861"="railroads_1861")),
                                "help_county_controls", help_controls),
                   labeledInput("county_schooling_adj", "Schooling Adjustment:",
                                selectInput("county_schooling_adj", NULL, c("None"=0, "Same Return"=1, "Specific Return"=2, "Run Both"=3), 0),
                                "help_county_schooling_adj", help_schooling_adj),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_apply_college_adj", "Apply College Share Adjustment", value = FALSE),
                       actionButton("help_county_apply_college_adj", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_apply_college_adj % 2 == 1", div(class="help-text", help_apply_college_adj)),
                   labeledInput("county_college_coeff", "College Share Coefficient:",
                                numericInput("county_college_coeff", NULL, 0.75, 0, 2, 0.05),
                                "help_county_college_coeff", help_college_coeff),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_mining_filter_active", "Filter by Mining Share", value = FALSE),
                       actionButton("help_county_mining_filter_active", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_mining_filter_active % 2 == 1", div(class="help-text", help_mining_filter_active)),
                   labeledInput("county_mining_threshold", "Max Mining Share:",
                                numericInput("county_mining_threshold", NULL, 0.01, 0, 1, 0.01),
                                "help_county_mining_threshold", help_mining_threshold),
                   labeledInput("county_se_spec", "Standard Errors:",
                                selectInput("county_se_spec", NULL, c("Cluster on Instrument"="cluster_instrument", "Cluster on State"="cluster_state", "Spatial (Conley)"="spatial", "Robust"="robust"), "cluster_instrument"),
                                "help_county_se_spec", help_se_spec),
                   conditionalPanel("input.county_se_spec == 'spatial'",
                                    labeledInput("county_spatial_cutoff", "Spatial Cutoff (km):",
                                                 numericInput("county_spatial_cutoff", NULL, 100),
                                                 "help_county_spatial_cutoff", help_spatial_cutoff))
                 )
    ), # sidebarPanel
    
    # Main panel with professional output
    mainPanel(
      h3(textOutput("results_header")),
      hr(),
      withSpinner(htmlOutput("results_table"), type = 6, color = "black"),
      htmlOutput("analysis_details"),
      conditionalPanel(condition = "input.show_map == true", hr(), h3(textOutput("map_header")), uiOutput("map_ui"))
    )
  )
)

# ============================================================================
# 6. Server Logic with Professional Table Formatting and F-Statistic
# ============================================================================
server <- function(input, output, session) {
  
  analysis_output <- reactiveVal(NULL)
  map_output <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    
    # --- 1. Run Regression Analysis ---
    reg_results <- tryCatch({
      withProgress(message = 'Running Analysis...', value = 0, {
        
        isolate({
          analysis_level <- input$analysis_level
          year_modern <- as.numeric(input$year_modern)
          analysis_type <- input$analysis_type
          use_fe <- input$use_fe
          fe_type <- input$fe_type
          sample_scope <- input$sample_scope
        })
        
        incProgress(0.1, detail = "Configuring analysis...")
        
        suffix <- if (sample_scope == "states_only") "_s" else ""
        
        if (analysis_level == "MSA") {
          isolate({
            sectors <- as.numeric(input$msa_sectors)
            sample_year <- as.numeric(input$msa_sample_year)
            iv_year <- as.numeric(input$msa_iv_year)
            instrument_type <- input$msa_instrument_type
            overlap_pct <- input$msa_overlap_pct
            controls_vec <- input$msa_controls
            schooling_adj <- as.numeric(input$msa_schooling_adj)
            apply_college_adj <- input$msa_apply_college_adj
            college_coeff <- input$msa_college_coeff
            se_spec <- input$msa_se_spec
            mining_filter_active <- input$msa_mining_filter_active
            mining_threshold <- input$msa_mining_threshold
            exclude_multistate <- input$exclude_multistate_msa
            spatial_cutoff <- input$msa_spatial_cutoff
          })
          
          df <- msa_data %>%
            filter(year == year_modern) %>%
            rename(lat = lat_DD, lon = lon_DD, state_id = modern_state_fe,
                   avg_schooling = msa_schooling_09, college_share = college_share_09)
          
          if (exclude_multistate) {
            if (!"msaname" %in% names(df)) stop("Error: 'msaname' column required for multi-state MSA filter not found.")
            df <- df %>%
              filter(!(str_detect(msaname, ",") & str_detect(str_extract(msaname, ",.*"), "-")))
          }
          
          if (instrument_type == "overlap") {
            sample_col <- glue("MSAol_{sample_year}_{overlap_pct}{suffix}")
            instr_col  <- glue("MSAol_{iv_year}_{overlap_pct}{suffix}")
          } else {
            sample_col <- glue("MSAiv_{sample_year}{suffix}")
            instr_col  <- glue("MSAiv_{iv_year}{suffix}")
          }
          
          # Apply territory filtering for states-only analysis
          if (sample_scope == "states_only") {
            fe_var_name_full <- glue("hist_state_fe_{iv_year}")  # without _s suffix
            
            # Try to find MSA identifier column
            msa_id_col <- NULL
            potential_id_cols <- c("msaid", "msa_code", "msaname", "msa_id", "cbsacode")
            for (col in potential_id_cols) {
              if (col %in% names(df)) {
                msa_id_col <- col
                break
              }
            }
            
            if (!is.null(msa_id_col) && fe_var_name_full %in% names(msa_data)) {
              territory_msa_ids <- get_territory_exclusions(
                data = msa_data %>% filter(year == year_modern),
                fe_column_name = fe_var_name_full,
                id_column = msa_id_col
              )
              
              if (length(territory_msa_ids) > 0) {
                df <- df %>%
                  filter(!.data[[msa_id_col]] %in% territory_msa_ids)
              }
            }
          }
          
          # Remove MSAs without fixed effects when FEs are activated
          if (use_fe) {
            if (fe_type == "modern") {
              # Filter out MSAs without modern state fixed effects
              df <- df %>%
                filter(!is.na(state_id) & state_id != "" & state_id != 0)
              
              # Also remove District of Columbia (not a state)
              df <- df %>%
                filter(!str_detect(tolower(as.character(state_id)), "district|columbia|\\bdc\\b"))
              
            } else {
              # Filter out MSAs without historical state fixed effects
              fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
              if (fe_var_name %in% names(df)) {
                df <- df %>%
                  filter(!is.na(.data[[fe_var_name]]) & .data[[fe_var_name]] != "" & .data[[fe_var_name]] != 0)
                
                # Also remove District of Columbia (not a state)
                df <- df %>%
                  filter(!str_detect(tolower(as.character(.data[[fe_var_name]])), "district|columbia|\\bdc\\b"))
              }
            }
          }
          
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)), LHS = ln_output_worker),
            "2" = df %>% mutate(RHS = log(employment_private / (msaarea / 4046.8)), LHS = ln_output_worker_private),
            "3" = df %>% mutate(RHS = log(emply_indryid_500 / (msaarea / 4046.8)), LHS = log(gmp_indstryid_12 / emply_indryid_500)),
            "4" = df %>% mutate(RHS = log(employment_private_nonfarm / (msaarea / 4046.8)), LHS = log((gmp_private_industry - gmp_agriculture) / employment_private_nonfarm)),
            "5" = df %>% mutate(employment_private_no_am = employment_private_nonfarm - employment_mining,
                                RHS = log(employment_private_no_am / (msaarea / 4046.8)),
                                LHS = log((gmp_private_industry - gmp_agriculture - gmp_mining) / employment_private_no_am)) %>% select(-employment_private_no_am),
            stop("Invalid MSA sector specified.")
          )
          
          if (mining_filter_active) {
            df <- df %>%
              mutate(temp_mining_share = gmp_mining / gmp) %>%
              filter(temp_mining_share <= mining_threshold | is.na(temp_mining_share)) %>%
              select(-temp_mining_share)
          }
          
        } else { # County Logic
          isolate({
            sectors <- as.numeric(input$county_sectors)
            sample_year <- as.numeric(input$county_sample_year)
            iv_year <- as.numeric(input$county_iv_year)
            instrument_type <- input$county_instrument_type
            overlap_threshold <- input$county_overlap_threshold
            controls_vec <- input$county_controls
            schooling_adj <- as.numeric(input$county_schooling_adj)
            apply_college_adj <- input$county_apply_college_adj
            college_coeff <- input$county_college_coeff
            se_spec <- input$county_se_spec
            mining_filter_active <- input$county_mining_filter_active
            mining_threshold <- input$county_mining_threshold
            county_msa_restriction <- input$county_msa_restriction
            spatial_cutoff <- input$county_spatial_cutoff
          })
          
          df <- county_data %>%
            filter(year == year_modern) %>%
            rename(state_id = modern_state_fe)
          
          df <- df %>%
            group_by(geofips) %>%
            mutate(avg_schooling = mean(avg_schooling_10_14, na.rm = TRUE)) %>%
            ungroup()
          
          if (county_msa_restriction) {
            if (!"county_partof_MSA" %in% names(df)) stop("Error: 'county_partof_MSA' column required for MSA restriction not found.")
            df <- df %>% filter(county_partof_MSA == 1)
          }
          
          # Apply territory filtering for states-only analysis
          if (sample_scope == "states_only") {
            fe_var_name_full <- glue("hist_state_fe_{iv_year}")  # without _s suffix
            
            # Try to find county identifier column
            county_id_col <- NULL
            potential_id_cols <- c("geofips", "fips", "county_fips", "countycode", "county_id")
            for (col in potential_id_cols) {
              if (col %in% names(df)) {
                county_id_col <- col
                break
              }
            }
            
            if (!is.null(county_id_col) && fe_var_name_full %in% names(county_data)) {
              territory_county_ids <- get_territory_exclusions(
                data = county_data %>% filter(year == year_modern),
                fe_column_name = fe_var_name_full,
                id_column = county_id_col
              )
              
              if (length(territory_county_ids) > 0) {
                df <- df %>%
                  filter(!.data[[county_id_col]] %in% territory_county_ids)
              }
            }
          }
          
          # Remove counties without fixed effects when FEs are activated
          if (use_fe) {
            if (fe_type == "modern") {
              # Filter out counties without modern state fixed effects
              df <- df %>%
                filter(!is.na(state_id) & state_id != "" & state_id != 0)
              
              # Also remove District of Columbia (not a state)
              df <- df %>%
                filter(!str_detect(tolower(as.character(state_id)), "district|columbia|\\bdc\\b"))
              
            } else {
              # Filter out counties without historical state fixed effects
              fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
              if (fe_var_name %in% names(df)) {
                df <- df %>%
                  filter(!is.na(.data[[fe_var_name]]) & .data[[fe_var_name]] != "" & .data[[fe_var_name]] != 0)
                
                # Also remove District of Columbia (not a state)
                df <- df %>%
                  filter(!str_detect(tolower(as.character(.data[[fe_var_name]])), "district|columbia|\\bdc\\b"))
              }
            }
          }
          
          if (instrument_type == "weighted_density_intersect") {
            sample_col <- glue("iv_intersect_{sample_year}{suffix}")
            instr_col  <- glue("iv_intersect_{iv_year}{suffix}")
          } else if (instrument_type == "weighted_density_overlap") {
            sample_col <- glue("iv_overlap_{overlap_threshold}_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_{overlap_threshold}_{iv_year}{suffix}")
          } else if (instrument_type == "max_density_overlap") {
            sample_col <- glue("iv_overlap_max_{overlap_threshold}_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
          } else if (instrument_type == "max_density_intersect") {
            sample_col <- glue("iv_overlap_max_any_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_max_any_{iv_year}{suffix}")
          } else {
            stop("Error: Invalid instrument_type for County level specified.")
          }
          
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(RHS = log(employment_total / area_acre), LHS = log(gcp_total / employment_total)),
            "2" = df %>% mutate(temp_emp = employment_private_nonfarm + emply_c_indryid_70,
                                RHS = log(temp_emp / area_acre),
                                LHS = log(gcp_private_industry / temp_emp)) %>% select(-temp_emp),
            "3" = df %>% rename(gcp_manufact = gcp_indstryid_12) %>%
              mutate(RHS = log(emply_c_indryid_500 / area_acre), LHS = log(gcp_manufact / emply_c_indryid_500)),
            "4" = df %>% mutate(RHS = log(employment_private_nonfarm / area_acre), LHS = log((gcp_private_industry - gcp_agriculture) / employment_private_nonfarm)),
            "5" = df %>% mutate(temp_emp = employment_private_nonfarm - employment_mining,
                                temp_gcp = gcp_private_industry - gcp_agriculture - gcp_mining,
                                RHS = log(temp_emp / area_acre),
                                LHS = log(temp_gcp / temp_emp)) %>% select(-temp_emp, -temp_gcp),
            stop("Invalid County sector specified.")
          )
          
          if (mining_filter_active) {
            df <- df %>%
              mutate(temp_mining_share = gcp_mining / gcp_total) %>%
              filter(temp_mining_share <= mining_threshold | is.na(temp_mining_share)) %>%
              select(-temp_mining_share)
          }
        }
        
        incProgress(0.3, detail = "Finalizing data preparation...")
        
        req_cols <- c(sample_col, instr_col, controls_vec)
        missing_cols <- req_cols[!req_cols %in% names(df)]
        if (length(missing_cols) > 0) {
          stop(glue("Error: Required column(s) not found: {paste(missing_cols, collapse=', ')}. Check selections or data file."))
        }
        
        df <- df %>% mutate(across(all_of(c(sample_col, instr_col)), as.numeric))
        df <- df %>%
          mutate(.sample_iv = .data[[sample_col]], instrument = .data[[instr_col]]) %>%
          filter(!is.na(.sample_iv) & !is.na(instrument)) %>%
          select(-.sample_iv)
        df$clusterID <- as.integer(as.factor(df$instrument))
        if (apply_college_adj) {
          if (!"college_share" %in% names(df)) stop("Error: 'college_share' column not found.")
          df <- df %>% mutate(LHS = LHS - college_coeff * college_share)
        }
        
        # Check for human capital adjustment columns before using them
        if (!"simple_gamma" %in% names(df) || !"Minc_return_calc" %in% names(df)) {
          stop("Error: 'simple_gamma' and/or 'Minc_return_calc' columns not found in the data. These are required for schooling adjustments.")
        }
        
        mean_schooling <- mean(df$avg_schooling, na.rm = TRUE)
        df <- df %>%
          mutate(
            LHS_adj1 = LHS - simple_gamma * (avg_schooling - mean_schooling),
            LHS_adj2 = LHS - Minc_return_calc * (avg_schooling - mean_schooling)
          )
        df <- df %>% filter(if_all(c(RHS, LHS, instrument), ~is.finite(.)))
        if(nrow(df) == 0){
          stop("After all filtering, 0 observations remain. Check selections for missing data in key variables.")
        }
        
        incProgress(0.5, detail = "Building regression models...")
        
        dep_vars_map <- list(
          "0" = setNames("LHS", "No Schooling Adj."),
          "1" = setNames("LHS_adj1", "Same Return Adj."),
          "2" = setNames("LHS_adj2", "Specific Return Adj."),
          "3" = setNames(c("LHS", "LHS_adj1", "LHS_adj2"), c("No Adj.", "Same Return", "Specific Return"))
        )
        if (analysis_level == "MSA" && schooling_adj == 3) {
          dep_vars_map <- list("3" = setNames(c("LHS_adj1", "LHS_adj2"), c("Same Return Adj.", "Specific Return Adj.")))
        }
        dep_vars <- dep_vars_map[[as.character(schooling_adj)]]
        controls_part <- if (length(controls_vec) > 0) paste("+", paste(controls_vec, collapse = " + ")) else ""
        fe_part <- "0"
        if (use_fe) {
          if (fe_type == "modern") {
            fe_part <- "state_id"
          } else {
            fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
            if (!fe_var_name %in% names(df)) {
              stop(glue("Error: Historical FE variable '{fe_var_name}' not found."))
            }
            df[[fe_var_name]] <- as.factor(df[[fe_var_name]])
            fe_part <- fe_var_name
          }
        }
        vcov_arg <- switch(se_spec,
                           "robust" = "hetero",
                           "cluster_instrument" = ~clusterID,
                           "cluster_state" = ~state_id,
                           "spatial" = vcov_conley(lat = "lat", lon = "lon", cutoff = spatial_cutoff),
                           stop("Invalid 'se_spec' defined.")
        )
        
        incProgress(0.7, detail = "Estimating models...")
        models <- list()
        first_stage_models <- list()  # Store first-stage models for F-stat extraction
        
        for (i in seq_along(dep_vars)) {
          dep_var_name <- dep_vars[i]
          model_name <- names(dep_vars)[i]
          
          formula_str <- switch(analysis_type,
                                "OLS" = glue("{dep_var_name} ~ RHS {controls_part} | {fe_part}"),
                                "IV"  = glue("{dep_var_name} ~ 1 {controls_part} | {fe_part} | RHS ~ instrument"),
                                "FirstStage" = glue("RHS ~ instrument {controls_part} | {fe_part}"),
                                stop("Invalid 'analysis_type' specified.")
          )
          
          model <- feols(as.formula(formula_str), data = df, vcov = vcov_arg)
          models[[model_name]] <- model
          
          # For IV regressions, also run first-stage manually to get F-stat
          if (analysis_type == "IV") {
            fs_formula_str <- glue("RHS ~ instrument {controls_part} | {fe_part}")
            fs_model <- feols(as.formula(fs_formula_str), data = df, vcov = vcov_arg)
            first_stage_models[[model_name]] <- fs_model
          }
        }
        
        incProgress(0.9, detail = "Formatting results...")
        
        # Return both models and metadata for professional formatting
        list(
          models = models,
          first_stage_models = first_stage_models,
          analysis_type = analysis_type,
          se_spec = se_spec,
          fe_type = if(use_fe) fe_type else "No",
          sample_scope = sample_scope,
          controls = if(length(controls_vec) > 0) paste(controls_vec, collapse = ", ") else "None",
          n_obs = nrow(df),
          cluster_data = list(
            clusterID = if("clusterID" %in% names(df)) df$clusterID else NULL,
            state_id = if("state_id" %in% names(df)) df$state_id else NULL
          ),
          data_for_fs = df,  # Pass data for manual F-stat calculation if needed
          controls_vec = controls_vec,
          fe_part = fe_part,
          vcov_arg = vcov_arg
        )
      })
    }, error = function(e) {
      list(error = paste("An error occurred during analysis:\n\n", e$message))
    })
    
    analysis_output(reg_results)
    
    # --- 2. Generate Map Path ---
    isolate({
      level <- input$analysis_level
      scope <- input$sample_scope
      scope_folder <- if (scope == "states_territories") "states_and_territories" else "states_only"
      
      map_path <- NULL
      map_title <- "Map not available for the selected instrument type or settings."
      
      if (level == "MSA") {
        type <- input$msa_instrument_type
        year <- input$msa_iv_year
        pct <- input$msa_overlap_pct
        base_folder <- paste("www", "MSA Maps", scope_folder, sep="/")
        
        if (type == "intersect") {
          filename <- glue("intersect_{year}.png")
          map_path <- paste(base_folder, "intersect", filename, sep="/")
          map_title <- glue("Instrument Map: MSA Intersect ({year})")
        } else if (type == "overlap") {
          filename <- glue("overlap_{pct}pct_{year}.png")
          subfolder <- glue("{pct}pct")
          map_path <- paste(base_folder, "overlap", subfolder, filename, sep="/")
          map_title <- glue("Instrument Map: MSA Overlap {pct}% ({year})")
        }
        
      } else { # County Logic
        type <- input$county_instrument_type
        year <- input$county_iv_year
        pct <- input$county_overlap_threshold
        base_folder <- paste("www", "County Maps", scope_folder, sep="/")
        
        if (type == "max_density_intersect") {
          filename <- glue("max_intersect_{year}.png")
          map_path <- paste(base_folder, "max_intersect", filename, sep="/")
          map_title <- glue("Instrument Map: County Max Density Intersect ({year})")
        } else if (type == "max_density_overlap") {
          filename <- glue("max_overlap_{pct}pct_{year}.png")
          subfolder <- glue("{pct}pct")
          map_path <- paste(base_folder, "max_overlap", subfolder, filename, sep="/")
          map_title <- glue("Instrument Map: County Max Density Overlap {pct}% ({year})")
        }
      }
      
      map_output(list(src = map_path, title = map_title))
    })
  })
  
  # --- 3. Professional Table Formatting Functions ---
  format_coefficient <- function(coef, se, stars = "") {
    coef_str <- sprintf("%.4f", coef)
    se_str <- sprintf("(%.4f)", se)
    if (stars != "") coef_str <- paste0(coef_str, stars)
    return(list(coef = coef_str, se = se_str))
  }
  
  get_significance_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**")
    if (p_value < 0.1) return("*")
    return("")
  }
  
  create_professional_table <- function(models, analysis_type, se_spec = NULL, cluster_data = NULL, 
                                        first_stage_models = NULL, data_for_fs = NULL, 
                                        controls_vec = NULL, fe_part = NULL, vcov_arg = NULL) {
    if (length(models) == 0) return("")
    
    # Get coefficient information for all variables
    all_var_data <- list()
    model_names <- names(models)
    
    # Collect all unique variable names across models
    all_vars <- c()
    for (i in seq_along(models)) {
      model <- models[[i]]
      coef_names <- names(coef(model))
      all_vars <- union(all_vars, coef_names)
    }
    
    # Define main variable name based on analysis type
    if (analysis_type %in% c("IV", "OLS")) {
      main_var <- ifelse(analysis_type == "IV", "fit_RHS", "RHS")
    } else {
      main_var <- "instrument"
    }
    
    # Order variables: main variable first, then controls
    ordered_vars <- c(main_var, setdiff(all_vars, main_var))
    
    # Function to get nice variable names
    get_var_label <- function(var_name) {
      switch(var_name,
             "fit_RHS" = "log(Employment Density) [Instrumented]",
             "RHS" = "log(Employment Density)",
             "instrument" = "Historical Density Instrument",
             "water_18201" = "Water Access 1820",
             "railroads_18401" = "Railroads 1840", 
             "railroads_18501" = "Railroads 1850",
             "railroads_18611" = "Railroads 1861",
             var_name)  # Default to original name if no mapping
    }
    
    # Extract coefficients for each variable and model
    for (var_name in ordered_vars) {
      if (var_name %in% all_vars) {
        var_data <- list()
        
        for (i in seq_along(models)) {
          model_name <- model_names[i]
          model <- models[[i]]
          
          coefs <- coef(model)
          ses <- se(model) 
          pvals <- pvalue(model)
          
          if (var_name %in% names(coefs)) {
            coef_val <- coefs[var_name]
            se_val <- ses[var_name]
            p_val <- pvals[var_name]
            stars <- get_significance_stars(p_val)
            
            var_data[[model_name]] <- format_coefficient(coef_val, se_val, stars)
          } else {
            var_data[[model_name]] <- list(coef = "—", se = "")
          }
        }
        
        all_var_data[[var_name]] <- var_data
      }
    }
    
    # Build LaTeX-style HTML table
    html_parts <- c('<table class="regression-table">')
    
    # Header row
    header_cols <- paste(sapply(model_names, function(x) paste0('<th>', x, '</th>')), collapse = "")
    html_parts <- c(html_parts, paste0('<tr><th class="row-label"></th>', header_cols, '</tr>'))
    
    # Variable rows
    for (var_name in names(all_var_data)) {
      var_label <- get_var_label(var_name)
      var_data <- all_var_data[[var_name]]
      
      # Coefficient row
      coef_cells <- paste(sapply(model_names, function(mn) {
        if (mn %in% names(var_data)) {
          paste0('<td class="coefficient">', var_data[[mn]]$coef, '</td>')
        } else {
          '<td class="coefficient">—</td>'
        }
      }), collapse = "")
      html_parts <- c(html_parts, paste0('<tr><td class="row-label">', var_label, '</td>', coef_cells, '</tr>'))
      
      # Standard error row
      se_cells <- paste(sapply(model_names, function(mn) {
        if (mn %in% names(var_data)) {
          paste0('<td class="se">', var_data[[mn]]$se, '</td>')
        } else {
          '<td class="se"></td>'
        }
      }), collapse = "")
      html_parts <- c(html_parts, paste0('<tr><td class="row-label"></td>', se_cells, '</tr>'))
    }
    
    # Empty row for spacing
    empty_cells <- paste(rep('<td></td>', length(model_names)), collapse = "")
    html_parts <- c(html_parts, paste0('<tr><td class="row-label"></td>', empty_cells, '</tr>'))
    
    # Statistics rows with top border
    n_cells <- paste(sapply(models, function(m) paste0('<td class="stats">', format(nobs(m), big.mark = ","), '</td>')), collapse = "")
    html_parts <- c(html_parts, paste0('<tr class="top-border"><td class="row-label stats">Observations</td>', n_cells, '</tr>'))
    
    # Add First-Stage F-Statistic for IV regressions
    if (analysis_type == "IV") {
      fstat_cells <- paste(sapply(model_names, function(mn) {
        tryCatch({
          # Method 1: Try to get from fitstat
          if (!is.null(first_stage_models) && mn %in% names(first_stage_models)) {
            fs_model <- first_stage_models[[mn]]
            fstat_info <- fitstat(fs_model, type = "ivf")
            if (!is.null(fstat_info) && "stat" %in% names(fstat_info)) {
              fstat_value <- sprintf("%.2f", fstat_info$stat)
              return(paste0('<td class="stats">', fstat_value, '</td>'))
            }
          }
          
          # Method 2: Manual calculation using first-stage model
          if (!is.null(first_stage_models) && mn %in% names(first_stage_models)) {
            fs_model <- first_stage_models[[mn]]
            coef_instr <- coef(fs_model)["instrument"]
            se_instr <- se(fs_model)["instrument"]
            if (!is.na(coef_instr) && !is.na(se_instr) && se_instr != 0) {
              fstat_value <- (coef_instr / se_instr)^2
              fstat_value <- sprintf("%.2f", fstat_value)
              return(paste0('<td class="stats">', fstat_value, '</td>'))
            }
          }
          
          # Method 3: Extract from IV model summary if available
          iv_model <- models[[mn]]
          model_summary <- summary(iv_model)
          if ("iv_first_stage" %in% names(model_summary) && "fstat" %in% names(model_summary$iv_first_stage)) {
            fstat_value <- sprintf("%.2f", model_summary$iv_first_stage$fstat)
            return(paste0('<td class="stats">', fstat_value, '</td>'))
          }
          
          # Method 4: Try using fixest's built-in first stage diagnostics
          if (exists("fitstat")) {
            iv_fstat <- fitstat(iv_model, type = "ivf")
            if (!is.null(iv_fstat) && "stat" %in% names(iv_fstat)) {
              fstat_value <- sprintf("%.2f", iv_fstat$stat)
              return(paste0('<td class="stats">', fstat_value, '</td>'))
            }
          }
          
          return(paste0('<td class="stats">—</td>'))
          
        }, error = function(e) {
          return(paste0('<td class="stats">—</td>'))
        })
      }), collapse = "")
      html_parts <- c(html_parts, paste0('<tr><td class="row-label stats">First-Stage F-Stat</td>', fstat_cells, '</tr>'))
    }
    
    # Add cluster information if relevant
    if (!is.null(se_spec) && !is.null(cluster_data)) {
      if (se_spec == "cluster_instrument" && !is.null(cluster_data$clusterID)) {
        n_clusters <- length(unique(cluster_data$clusterID))
        cluster_cells <- paste(rep(paste0('<td class="stats">', n_clusters, '</td>'), length(models)), collapse = "")
        html_parts <- c(html_parts, paste0('<tr><td class="row-label stats">Clusters (Instrument)</td>', cluster_cells, '</tr>'))
      } else if (se_spec == "cluster_state" && !is.null(cluster_data$state_id)) {
        n_clusters <- length(unique(cluster_data$state_id))
        cluster_cells <- paste(rep(paste0('<td class="stats">', n_clusters, '</td>'), length(models)), collapse = "")
        html_parts <- c(html_parts, paste0('<tr><td class="row-label stats">Clusters (State)</td>', cluster_cells, '</tr>'))
      }
    }
    
    # Close table with bottom border
    html_parts[length(html_parts)] <- gsub('<tr><td', '<tr class="bottom-border"><td', html_parts[length(html_parts)])
    html_parts <- c(html_parts, '</table>')
    
    # Add significance notes in LaTeX style
    note_text <- 'Notes: Standard errors in parentheses. *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1'
    if (analysis_type == "IV") {
      note_text <- paste0(note_text, '. First-Stage F-Stat tests instrument strength (F &gt; 10 indicates strong instrument).')
    }
    html_parts <- c(html_parts, paste0('<div class="table-notes">', note_text, '</div>'))
    
    return(paste(html_parts, collapse = "\n"))
  }
  
  # --- 4. Render Professional Outputs ---
  output$results_header <- renderText({
    req(analysis_output())
    if ("error" %in% names(analysis_output())) {
      "Analysis Error"
    } else {
      glue("{input$analysis_level} Level Results: {input$analysis_type}")
    }
  })
  
  output$results_table <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      div(class = "help-text", style = "color: #dc3545; border-left-color: #dc3545;",
          analysis_output()$error)
    } else {
      details <- analysis_output()
      HTML(create_professional_table(
        models = details$models, 
        analysis_type = details$analysis_type,
        se_spec = details$se_spec,
        cluster_data = details$cluster_data,
        first_stage_models = details$first_stage_models,
        data_for_fs = details$data_for_fs,
        controls_vec = details$controls_vec,
        fe_part = details$fe_part,
        vcov_arg = details$vcov_arg
      ))
    }
  })
  
  output$analysis_details <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      return(NULL)
    }
    
    details <- analysis_output()
    
    # Add clustering information to SE description
    se_description <- details$se_spec
    if (details$se_spec == "cluster_instrument" && !is.null(details$cluster_data$clusterID)) {
      n_clusters <- length(unique(details$cluster_data$clusterID))
      se_description <- paste0(se_description, " (", n_clusters, " clusters)")
    } else if (details$se_spec == "cluster_state" && !is.null(details$cluster_data$state_id)) {
      n_clusters <- length(unique(details$cluster_data$state_id))
      se_description <- paste0(se_description, " (", n_clusters, " clusters)")
    }
    
    HTML(paste0(
      '<div class="analysis-details">',
      '<h5>Analysis Details</h5>',
      '<strong>Standard Errors:</strong> ', se_description, '<br>',
      '<strong>State Fixed Effects:</strong> ', details$fe_type, '<br>',
      '<strong>Sample Scope:</strong> ', details$sample_scope, '<br>',
      '<strong>Controls:</strong> ', details$controls, '<br>',
      '<strong>Total Observations:</strong> ', format(details$n_obs, big.mark = ","),
      '</div>'
    ))
  })
  
  output$map_header <- renderText({
    req(map_output())
    map_output()$title
  })
  
  output$map_ui <- renderUI({
    info <- map_output()
    req(info)
    
    if (!is.null(info$src) && file.exists(info$src)) {
      imageOutput("instrument_map_render", width = paste0(input$map_size, "%"), height = "auto")
    } else {
      div(class = "help-text",
          "Map image not found. Please check your selections and verify your file structure in the 'www' folder.")
    }
  })
  
  output$instrument_map_render <- renderImage({
    info <- map_output()
    req(info$src, file.exists(info$src))
    
    list(
      src = info$src,
      contentType = "image/png",
      alt = "Instrument Map"
    )
  }, deleteFile = FALSE)
}

# --- 7. Run the Application ---
shinyApp(ui = ui, server = server)
