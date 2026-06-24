# =============================================================================
# dashboard/app.R
# India MET Reserving Project — Shiny Dashboard
# Author: Aditya Prakash
# =============================================================================

library(shiny)
library(shinydashboard)
library(ChainLadder)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(scales)
library(DT)
library(plotly)

load("../data/processed/synthetic_triangles.RData")
load("../data/processed/cl_results.RData")
load("../data/processed/bf_results.RData")

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

compute_bf_ibnr <- function(cl_triangle, premium, elr) {
  mack_temp <- MackChainLadder(cl_triangle)
  f         <- mack_temp$f
  n_dev     <- ncol(cl_triangle)

  cdf <- numeric(n_dev)
  cdf[n_dev] <- 1.0
  for (j in (n_dev - 1):1) {
    cdf[j] <- f[j] * cdf[j + 1]
  }

  latest_obs <- apply(cl_triangle, 1, function(row) {
    obs <- row[!is.na(row)]; tail(obs, 1)
  })
  latest_dev_idx <- apply(cl_triangle, 1, function(row) max(which(!is.na(row))))

  apriori_ult    <- premium * elr
  pct_unreported <- 1 - (1 / cdf[latest_dev_idx])
  ibnr_bf        <- apriori_ult * pct_unreported
  ultimate_bf    <- latest_obs + ibnr_bf

  tibble(
    AccidentYear = rownames(cl_triangle),
    LatestObs    = round(latest_obs, 1),
    Ultimate_BF  = round(ultimate_bf, 1),
    IBNR_BF      = round(ibnr_bf, 1),
    PctEmerged   = round((1 / cdf[latest_dev_idx]) * 100, 1)
  )
}

compute_blend <- function(ibnr_bf, ibnr_cl, pct_emerged) {
  bf_weight <- pmax(0, pmin(1, 1 - pct_emerged / 100))
  round(bf_weight * ibnr_bf + (1 - bf_weight) * ibnr_cl, 1)
}

get_triangle <- function(line) {
  switch(line,
    "Motor"       = list(tri = motor_cl_tri,  prem = motor_premium,  def_elr = 0.77),
    "Engineering" = list(tri = eng_cl_tri,    prem = eng_premium,    def_elr = 0.70),
    "Treaty"      = list(tri = treaty_cl_tri, prem = treaty_premium, def_elr = 0.73)
  )
}

get_cl_results <- function(line) {
  switch(line,
    "Motor"       = results_motor,
    "Engineering" = results_eng,
    "Treaty"      = results_treaty
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "India MET Reserving"),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Triangle",  tabName = "triangle",  icon = icon("table")),
      menuItem("About",     tabName = "about",     icon = icon("info-circle"))
    ),
    hr(),
    selectInput(
      inputId  = "line",
      label    = "Line of Business",
      choices  = c("Motor", "Engineering", "Treaty"),
      selected = "Engineering"
    ),
    radioButtons(
      inputId  = "method",
      label    = "Reserving Method",
      choices  = c(
        "Chain Ladder (CL)"         = "CL",
        "Bornhuetter-Ferguson (BF)" = "BF",
        "Credibility Blend"         = "Blend"
      ),
      selected = "BF"
    ),
    hr(),
    conditionalPanel(
      condition = "input.method != 'CL'",
      sliderInput(
        inputId = "elr",
        label   = "A Priori ELR (%)",
        min = 50, max = 110, value = 70, step = 1,
        post = "%"
      ),
      helpText("Default: 70% Engineering, 77% Motor, 73% Treaty.")
    ),
    hr(),
    div(
      style = "padding: 10px; font-size: 11px; color: #aaa;",
      "Synthetic triangles calibrated to IRDAI AR 2022-23 and GIC Re AR 2022-23.",
      br(), br(),
      tags$a(
        href = "https://github.com/Adiprakash01/india-met-reserving",
        target = "_blank", style = "color: #4da6ff;",
        "GitHub Repository"
      )
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f5f7fa; }
      .box { border-radius: 6px; }
    "))),

    tabItems(

      # DASHBOARD TAB
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_ibnr_box", width = 4),
          valueBoxOutput("method_box",     width = 4),
          valueBoxOutput("elr_box",        width = 4)
        ),
        fluidRow(
          box(
            title = "IBNR by Accident Year", width = 8,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("ibnr_plot", height = "380px")
          ),
          box(
            title = "Reserve Summary", width = 4,
            status = "primary", solidHeader = TRUE,
            DTOutput("summary_table")
          )
        ),
        fluidRow(
          box(
            title = "BF vs Chain Ladder vs Blend Comparison", width = 12,
            status = "warning", solidHeader = TRUE,
            plotlyOutput("comparison_plot", height = "320px")
          )
        )
      ),

      # TRIANGLE TAB
      tabItem(tabName = "triangle",
        fluidRow(
          box(
            title = "Loss Development Triangle (Cumulative Incurred, INR Crores)",
            width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("triangle_table")
          )
        ),
        fluidRow(
          box(
            title = "Development Pattern", width = 12,
            status = "info", solidHeader = TRUE,
            plotlyOutput("dev_pattern_plot", height = "300px")
          )
        )
      ),

      # ABOUT TAB
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About This Dashboard", width = 12,
            status = "primary", solidHeader = TRUE,
            h4("India MET Loss Reserving Dashboard"),
            p("Interactive loss reserve estimation for Indian Motor, Engineering, and Treaty reinsurance lines."),
            tags$ul(
              tags$li(strong("Chain Ladder (Mack, 1993):"), " Development pattern extrapolation with standard errors."),
              tags$li(strong("Bornhuetter-Ferguson:"), " Credibility blend of a priori ELR and actual emergence. Preferred for CAT-contaminated and immature accident years."),
              tags$li(strong("Credibility Blend:"), " Weighted combination — more BF weight for immature AYs, more CL weight for mature AYs.")
            ),
            h4("Key Indian Market Findings"),
            tags$ul(
              tags$li(strong("Engineering CAT Contamination:"), " Kerala floods (2018) and Cyclone Fani (2019) inflated link ratios. Select Engineering + CL vs BF to see the divergence."),
              tags$li(strong("Motor MACT Judicial Lag:"), " Only 45% of Motor TP losses emerge by Year 1 vs ~65% in European markets due to MACT tribunal delays."),
              tags$li(strong("ELR Sensitivity:"), " Move the ELR slider to see how sensitive BF reserves are to the a priori assumption.")
            ),
            h4("Data"),
            p("All triangles are synthetic, calibrated to IRDAI Annual Report 2022-23 and GIC Re Annual Report 2022-23."),
            h4("Author"),
            p("Aditya Prakash | B.Com (Economics & Analytics), NM College Mumbai | IFoA (CB2 cleared)"),
            tags$a(
              href = "https://github.com/Adiprakash01/india-met-reserving",
              target = "_blank",
              "github.com/Adiprakash01/india-met-reserving"
            )
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Update ELR slider default when line changes
  observeEvent(input$line, {
    default_elr <- switch(input$line,
      "Motor"       = 77,
      "Engineering" = 70,
      "Treaty"      = 73
    )
    updateSliderInput(session, "elr", value = default_elr)
  })

  # Core reactive: compute all results
  results <- reactive({
    line_data <- get_triangle(input$line)
    cl_res    <- get_cl_results(input$line)

    bf_res <- compute_bf_ibnr(
      cl_triangle = line_data$tri,
      premium     = line_data$prem,
      elr         = input$elr / 100
    )

    ibnr_cl    <- cl_res$IBNR_Cr
    ibnr_bf    <- bf_res$IBNR_BF
    ibnr_blend <- compute_blend(ibnr_bf, ibnr_cl, bf_res$PctEmerged)

    ibnr_sel <- switch(input$method,
      "CL"    = ibnr_cl,
      "BF"    = ibnr_bf,
      "Blend" = ibnr_blend
    )

    list(
      ay           = cl_res$AccidentYear,
      premium      = cl_res$Premium_Cr,
      latest_obs   = cl_res$LatestObs_Cr,
      ibnr_cl      = ibnr_cl,
      ibnr_bf      = ibnr_bf,
      ibnr_blend   = ibnr_blend,
      ibnr_sel     = ibnr_sel,
      pct_emerged  = bf_res$PctEmerged,
      method_label = switch(input$method,
        "CL"    = "Chain Ladder",
        "BF"    = "Bornhuetter-Ferguson",
        "Blend" = "Credibility Blend"
      )
    )
  })

  # Value boxes
  output$total_ibnr_box <- renderValueBox({
    r <- results()
    valueBox(
      value    = paste0("\u20b9", format(round(sum(r$ibnr_sel, na.rm = TRUE)), big.mark = ",")),
      subtitle = paste0("Total IBNR — ", input$line, " (INR Cr)"),
      icon     = icon("rupee-sign"),
      color    = "blue"
    )
  })

  output$method_box <- renderValueBox({
    r <- results()
    valueBox(
      value    = r$method_label,
      subtitle = "Selected Reserving Method",
      icon     = icon("calculator"),
      color    = "green"
    )
  })

  output$elr_box <- renderValueBox({
    valueBox(
      value    = paste0(input$elr, "%"),
      subtitle = "A Priori ELR (BF/Blend)",
      icon     = icon("percent"),
      color    = "yellow"
    )
  })

  # IBNR bar chart
  output$ibnr_plot <- renderPlotly({
    r <- results()
    df <- tibble(
      AccidentYear = r$ay,
      IBNR         = r$ibnr_sel,
      Positive     = r$ibnr_sel >= 0
    )
    p <- ggplot(df, aes(x = AccidentYear, y = IBNR, fill = Positive,
                        text = paste0(AccidentYear, "<br>IBNR: \u20b9", round(IBNR, 1), " Cr"))) +
      geom_col(alpha = 0.85) +
      scale_fill_manual(values = c("TRUE" = "#1a5276", "FALSE" = "#e74c3c"), guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(x = "Accident Year", y = "IBNR (INR Crores)") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  # Summary table
  output$summary_table <- renderDT({
    r <- results()
    df <- data.frame(
      AY          = r$ay,
      LatestObs   = r$latest_obs,
      IBNR        = round(r$ibnr_sel, 1),
      PctEmerged  = r$pct_emerged,
      stringsAsFactors = FALSE
    )
    datatable(df,
      colnames = c("AY", "Latest Obs", "IBNR", "% Emerged"),
      options  = list(pageLength = 8, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })

  # BF vs CL comparison chart
  output$comparison_plot <- renderPlotly({
    r <- results()
    df <- tibble(
      AccidentYear = rep(r$ay, 3),
      IBNR   = c(r$ibnr_cl, r$ibnr_bf, r$ibnr_blend),
      Method = rep(c("Chain Ladder", "BF", "Blend"), each = length(r$ay))
    )
    p <- ggplot(df, aes(x = AccidentYear, y = IBNR, fill = Method,
                        text = paste0(Method, "<br>", AccidentYear,
                                      "<br>\u20b9", round(IBNR, 1), " Cr"))) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c(
        "Chain Ladder" = "#e67e22",
        "BF"           = "#1a5276",
        "Blend"        = "#1e8449"
      )) +
      scale_y_continuous(labels = comma) +
      labs(x = "Accident Year", y = "IBNR (INR Crores)", fill = "Method") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top")
    ggplotly(p, tooltip = "text")
  })

  # Triangle table
  output$triangle_table <- renderDT({
    line_data <- get_triangle(input$line)
    # Use raw matrix, not cl_triangle object
    raw <- switch(input$line,
                  "Motor"       = motor_triangle,
                  "Engineering" = eng_triangle,
                  "Treaty"      = treaty_triangle
    )
    tri_df <- as.data.frame(round(raw, 1))
    tri_df[is.na(tri_df)] <- "—"
    datatable(tri_df,
              options  = list(pageLength = 10, dom = 't', ordering = FALSE),
              rownames = TRUE
    )
  })
  
  # Development pattern plot
  output$dev_pattern_plot <- renderPlotly({
    raw <- switch(input$line,
                  "Motor"       = motor_triangle,
                  "Engineering" = eng_triangle,
                  "Treaty"      = treaty_triangle
    )
    tri_df <- as.data.frame(raw)
    tri_df$AY <- rownames(tri_df)
    
    long_df <- tri_df %>%
      pivot_longer(cols = -AY, names_to = "DevPeriod", values_to = "Losses") %>%
      filter(!is.na(Losses)) %>%
      mutate(DevNum = as.numeric(gsub("[^0-9]", "", DevPeriod)))
    
    p <- ggplot(long_df, aes(x = DevNum, y = Losses, colour = AY, group = AY,
                             text = paste0(AY, " | Dev ", DevNum,
                                           "<br>\u20b9", round(Losses, 1), " Cr"))) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.8) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = 1:8) +
      labs(x = "Development Period", y = "Cumulative Losses (INR Cr)", colour = "AY") +
      theme_minimal(base_size = 11)
    
    ggplotly(p, tooltip = "text")
  })
}

# -----------------------------------------------------------------------------
# RUN
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
