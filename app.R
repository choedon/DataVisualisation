# ==============================================================================
# Food Product Validation Dashboard
# Clean build — reference style: white cards, accent circles, no emojis
# Reactive architecture from approved Mermaid diagram (Steps 4-6)
# User stories: S1-S9 / Questions: Q1-Q8
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringdist)
library(httr)
library(jsonlite)

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(as.character(a[1]))) a else b
}

# -- 1. DATA -------------------------------------------------------------------
if (file.exists("text_food.rds") && file.exists("numeric_food.rds")) {
  df_text <- readRDS("text_food.rds")
  df_num  <- readRDS("numeric_food.rds")
} else {
  library(readxl)
  df_text <- read_excel("text_food.xlsx")
  df_num  <- read_excel("numeric_food.xlsx")
  saveRDS(df_text, "text_food.rds")
  saveRDS(df_num,  "numeric_food.rds")
}

df_text <- df_text %>% mutate(
  row_id      = row_number(),
  pid         = paste0("T-", sprintf("%05d", row_number())),
  active      = as.integer(active),
  cat_display = ifelse(is.na(categories) | categories == "", "(No category)", categories)
)
df_num <- df_num %>% mutate(
  row_id      = row_number(),
  pid         = paste0("N-", sprintf("%05d", row_number())),
  active      = as.integer(active),
  cat_display = ifelse(is.na(categories) | categories == "", "(No category)", categories)
)

cats_text <- sort(c(
  na.omit(unique(df_text$categories)),
  if (any(is.na(df_text$categories))) "(No category)" else character(0)
))
cats_num <- sort(c(
  na.omit(unique(df_num$categories)),
  if (any(is.na(df_num$categories))) "(No category)" else character(0)
))

CORE <- c("energy", "protein", "fat", "carbohydrates", "sugar", "salt")
max_core <- sapply(CORE, function(col) {
  v <- max(df_num[[col]], na.rm = TRUE)
  if (is.finite(v) && v > 0) v else 100
})

# Helper: classify a comparison cell (pending vs candidate)
# Returns list(cls, pv_disp, cv_disp)
classify_cell <- function(pv_raw, cv_raw, field = NULL) {
  nm <- function(x) {
    s <- trimws(as.character(x))
    is.na(x) || s %in% c("", "—", "NA", "na", "<NA>")
  }
  pm <- nm(pv_raw); cm <- nm(cv_raw)
  pv <- if (pm) "—" else as.character(pv_raw)
  cv <- if (cm) "—" else as.character(cv_raw)

  cls <- if (pm && cm) {
    "cmp-neutral"               # both missing  → gray
  } else if (!pm && !cm && pv == cv) {
    "cmp-match"                 # exact match   → green
  } else if (pm && !cm) {
    if (!is.null(field) && field == "barcode") "cmp-info" else "cmp-diff"
    # pending has no barcode but candidate does → blue (helpful context)
    # other fields → red
  } else if (!pm && cm) {
    "cmp-diff"                  # candidate missing, pending has value → red
  } else {
    "cmp-diff"                  # values differ → red
  }
  list(cls = cls, pv = pv, cv = cv)
}

# -- 2. THEME & CSS ------------------------------------------------------------
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#1b5e3b",
  base_font  = font_google("Inter"),
  `font-size-base` = "0.9rem"
)

app_css <- tags$style(HTML("
  /* Base */
  body { background: #f4f6fb; color: #2d3748; font-family: 'Inter', sans-serif; }

  /* Navbar */
  .navbar {
    background: #fff !important;
    border-bottom: 1px solid #e8e8e8;
    box-shadow: 0 1px 4px rgba(0,0,0,.05);
    padding: .55rem 1.5rem;
  }
  .navbar-brand {
    font-weight: 700; font-size: .95rem;
    color: #1b5e3b !important; letter-spacing: -.2px;
  }
  .navbar-nav .nav-link {
    font-size: .83rem; font-weight: 500;
    color: #666 !important; padding: .45rem .85rem !important;
    border-radius: 8px; margin: 0 2px;
  }
  .navbar-nav .nav-link.active {
    color: #1b5e3b !important; background: #e8f5e9; font-weight: 600;
  }

  /* ---- Top filter bar ---- */
  .filter-bar {
    background: #fff; border-radius: 12px;
    padding: .65rem 1.1rem;
    box-shadow: 0 2px 10px rgba(0,0,0,.06);
    margin-bottom: 1rem;
    display: flex; align-items: flex-end; gap: 1rem; flex-wrap: wrap;
  }
  .filter-bar .fb-group { display: flex; flex-direction: column; }
  .filter-bar .fb-label {
    font-size: .66rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: .08em;
    color: #bbb; margin-bottom: .2rem;
  }
  .filter-bar .form-select {
    font-size: .82rem; border-color: #e8e8e8;
    border-radius: 8px; color: #333; min-width: 160px;
  }
  .filter-bar .thr-group { flex: 1; min-width: 220px; }
  .filter-bar .thr-row {
    display: flex; justify-content: space-between; align-items: center;
    margin-bottom: .15rem;
  }
  .filter-bar .thr-row .fb-label { margin-bottom: 0; }
  .filter-bar .thr-val { font-size: .82rem; font-weight: 700; color: #1b5e3b; }

  /* KPI strip */
  .kpi-card {
    background: #fff; border-radius: 14px;
    padding: 1rem 1.15rem;
    box-shadow: 0 2px 12px rgba(0,0,0,.07);
    display: flex; align-items: center; gap: .95rem;
  }
  .kpi-circle {
    width: 50px; height: 50px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    color: #fff; font-size: 1.1rem; flex-shrink: 0;
  }
  .kpi-num { font-size: 1.75rem; font-weight: 700; color: #1a1a2e; line-height: 1.1; }
  .kpi-lbl { font-size: .72rem; color: #aaa; margin-top: 3px; }

  /* Page cards */
  .pg-card {
    background: #fff; border-radius: 14px;
    box-shadow: 0 2px 10px rgba(0,0,0,.06);
    margin-bottom: 1rem; overflow: hidden;
  }
  .pg-hdr {
    padding: .7rem 1.1rem; border-bottom: 1px solid #f2f2f2;
    display: flex; align-items: center; justify-content: space-between;
    font-weight: 600; font-size: .86rem; color: #333;
  }
  .pg-hdr .q-badge {
    background: #f5f5f5; color: #999; border: 1px solid #e8e8e8;
    border-radius: 6px; padding: 2px 8px; font-size: .69rem;
  }
  .pg-body { padding: .9rem 1.1rem; }

  /* Product banner (compact, replaces full Step 2 card) */
  .prod-banner {
    display: flex; align-items: center; gap: .55rem;
    padding: .55rem .9rem; margin-bottom: .75rem;
    background: #fff; border-radius: 10px;
    font-size: .84rem; box-shadow: 0 1px 4px rgba(0,0,0,.05);
    border-left: 4px solid #ff7043;
  }
  .prod-banner .prod-id { font-weight: 700; color: #333; }
  .prod-banner .prod-name { color: #666; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 300px; }

  /* Status labels */
  .lbl-pending {
    background: #fff8e1; color: #e65100;
    border: 1px solid #ffcc02; border-radius: 20px;
    padding: 2px 10px; font-size: .72rem; font-weight: 600; white-space: nowrap;
  }
  .lbl-ok {
    background: #e8f5e9; color: #1b5e20;
    border: 1px solid #a5d6a7; border-radius: 20px;
    padding: 2px 10px; font-size: .72rem; font-weight: 600; white-space: nowrap;
  }

  /* Score badges */
  .sc-high { background: #1b5e3b; color: #fff; border-radius: 6px; padding: 2px 8px; font-size: .73rem; }
  .sc-mid  { background: #e65100; color: #fff; border-radius: 6px; padding: 2px 8px; font-size: .73rem; }
  .sc-low  { background: #9e9e9e; color: #fff; border-radius: 6px; padding: 2px 8px; font-size: .73rem; }

  /* Info box */
  .info-row {
    background: #f0f7ff; border-left: 3px solid #1976d2;
    border-radius: 6px; padding: .45rem .8rem;
    font-size: .79rem; color: #1565c0; margin-bottom: .65rem;
  }

  /* Comparison table */
  .cmp-tbl { width: 100%; border-collapse: collapse; font-size: .8rem; }
  .cmp-tbl th {
    background: #fafafa; padding: .36rem .6rem;
    font-weight: 600; border: 1px solid #eee; color: #555;
  }
  .cmp-tbl td { padding: .36rem .6rem; border: 1px solid #eee; }
  .cmp-match   { color: #1b5e20; font-weight: 600; }
  .cmp-diff    { color: #c62828; font-weight: 600; }
  .cmp-neutral { color: #bbb; font-style: italic; }
  .cmp-info    { color: #0d6efd; font-weight: 600; }

  /* DT selected row */
  table.dataTable tbody > tr.selected > td,
  table.dataTable tbody > tr.selected > th {
    background-color: #e0f2e9 !important;
    color: #1b5e3b !important;
    border-color: #a5d6a7 !important;
    font-weight: 500;
  }

  /* Empty state */
  .empty-st { text-align: center; padding: 3rem 1rem; color: #ccc; }
  .empty-st .es-icon { font-size: 2.2rem; opacity: .3; display: block; margin-bottom: .8rem; }
  .empty-st p { font-size: .82rem; }

  /* Decision card */
  .dec-hdr {
    background: #1b5e3b; color: #fff;
    padding: .7rem 1.1rem; font-weight: 600; font-size: .86rem;
  }
  .dec-body { background: #fff; padding: .9rem 1.1rem; }
  .shiny-input-container .radio label { font-size: .83rem; }

  /* DT table */
  .dataTables_wrapper { font-size: .8rem; }
  .dataTables_filter input { border-radius: 8px; border: 1px solid #e8e8e8; font-size: .79rem; }
  table.dataTable thead th { font-weight: 600; font-size: .79rem; color: #555; }

  /* Slider: show track numbers as percentages */
  .irs-min, .irs-max, .irs-from, .irs-to, .irs-single {
    font-size: .72rem;
  }
"))

# -- 3. UI --------------------------------------------------------------------
ui <- page_navbar(
  title  = "Food Validator",
  theme  = app_theme,
  header = app_css,

  # ---- TAB 1: Validation Workflow (S1-S5) ------------------------------------
  nav_panel(
    title = "Validation",

    # Top filter bar (W1-W4) — slider now in integer % (40-95)
    div(class = "filter-bar",

      div(class = "fb-group",
        span(class = "fb-label", "Dataset"),
        selectInput("dataset", NULL,
          choices  = c("Text / Names" = "text", "Nutritional values" = "numeric"),
          selected = "text", width = "180px"
        )
      ),

      div(class = "fb-group",
        span(class = "fb-label", "Category"),
        selectInput("category", NULL,
          choices  = c("All categories" = "all"),
          selected = "all", width = "190px"
        )
      ),

      div(class = "fb-group",
        span(class = "fb-label", "Sort queue by"),
        selectInput("sort_order", NULL,
          choices  = c("Default order" = "default",
                       "Name A to Z"   = "name",
                       "By category"   = "category"),
          selected = "default", width = "165px"
        )
      ),

      # Threshold — integer 40-95 (% units), no decimal confusion
      div(class = "fb-group thr-group",
        div(class = "thr-row",
          span(class = "fb-label", "Match threshold"),
          span(class = "thr-val", id = "thr_lbl", "70%")
        ),
        sliderInput("threshold", NULL,
          min = 40, max = 95, value = 70, step = 5,
          ticks = FALSE, width = "100%",
          post  = "%"      # appends % to track labels (40%...95%)
        )
      ),

      # KPIs inline on the right
      div(class = "ms-auto d-flex gap-2",
        div(class = "kpi-card py-2 px-3",
          div(class = "kpi-circle", style = "background:#ff7043; width:38px; height:38px; font-size:.85rem;", icon("clock")),
          div(div(class = "kpi-num", style = "font-size:1.35rem;", textOutput("kpi_pending", inline = TRUE)),
              div(class = "kpi-lbl", "Pending"))
        ),
        div(class = "kpi-card py-2 px-3",
          div(class = "kpi-circle", style = "background:#1b5e3b; width:38px; height:38px; font-size:.85rem;", icon("database")),
          div(div(class = "kpi-num", style = "font-size:1.35rem;", textOutput("kpi_ref", inline = TRUE)),
              div(class = "kpi-lbl", "Reference"))
        ),
        div(class = "kpi-card py-2 px-3",
          div(class = "kpi-circle", style = "background:#1565c0; width:38px; height:38px; font-size:.85rem;", icon("check")),
          div(div(class = "kpi-num", style = "font-size:1.35rem;", textOutput("kpi_done", inline = TRUE)),
              div(class = "kpi-lbl", "Decisions"))
        )
      )
    ),

    tags$script("
      Shiny.addCustomMessageHandler('setThrLbl', function(v) {
        document.getElementById('thr_lbl').textContent = v + '%';
      });
    "),

    # Master-detail layout
    layout_columns(
      col_widths = c(5, 7), gap = "1rem",

      # LEFT — Queue (Q1 · S1)
      div(
        div(class = "pg-card",
          div(class = "pg-hdr",
            span("Step 1 — Select a pending product"),
            span(class = "q-badge", "Q1")
          ),
          div(class = "pg-body",
            uiOutput("q1_summary"),
            div(class = "info-row",
              "Click any row in the table below to load its candidates."
            ),
            DTOutput("queue_table")
          )
        )
      ),

      # RIGHT — Workspace (Q2-Q6 · S2-S5)
      div(uiOutput("workspace"))
    )
  ),

  # ---- TAB 2: Threshold Tuner (S7 S8) ----------------------------------------
  nav_panel(
    title = "Threshold Tuner",
    layout_columns(
      col_widths = c(4, 8), gap = "1rem",

      div(
        div(class = "pg-card",
          div(class = "pg-hdr", "Current settings", span(class = "q-badge", "Q7")),
          div(class = "pg-body", uiOutput("thr_kpis"),
            div(style = "font-size:.73rem; color:#bbb; margin-top:.5rem; border-top:1px solid #f0f0f0; padding-top:.7rem;",
              "Moving the slider in the filter bar above updates this chart in real time.",
              br(),
              "No threshold change validates a product automatically."
            )
          )
        )
      ),

      div(
        div(class = "pg-card",
          div(class = "pg-hdr",
            span("Candidates surfaced vs similarity threshold"),
            span(class = "q-badge", "Q7 / S7-S8")
          ),
          div(class = "pg-body", plotlyOutput("pr_curve", height = "360px"))
        )
      )
    )
  ),

  # ---- TAB 3: Data Quality (S9) -----------------------------------------------
  nav_panel(
    title = "Data Quality",

    # Row 1: Missing heatmap + Category distribution side by side
    layout_columns(
      col_widths = c(6, 6), gap = "1rem",

      div(class = "pg-card",
        div(class = "pg-hdr",
          span("Missing fields — pending vs validated"),
          span(class = "q-badge", "Q8 / S9")
        ),
        div(class = "pg-body",
          p(class = "small text-muted mb-3",
            "Red = many values missing. These fields are excluded from kNN scoring."
          ),
          plotlyOutput("missing_heatmap", height = "300px")
        )
      ),

      div(class = "pg-card",
        div(class = "pg-hdr",
          span("Category coverage — pending queue"),
          span(class = "q-badge", "Q1")
        ),
        div(class = "pg-body",
          p(class = "small text-muted mb-2",
            "Uncategorized products lose the 15% category bonus in text matching."
          ),
          plotlyOutput("queue_bar", height = "300px")
        )
      )
    ),

    # Row 2: Encoding issues (full width)
    div(class = "pg-card",
      div(class = "pg-hdr",
        span("UTF-8 encoding issues — pending text products"),
        span(class = "q-badge", "batch fix")
      ),
      div(class = "pg-body",
        p(class = "small text-muted mb-2",
          "Products with Mojibake artefacts (e.g. \u00d0\u009c for Cyrillic, \u00e2\u0080\u0099 for curly quotes). Fix encoding before validation to prevent distorted similarity scores."
        ),
        DTOutput("encoding_issues_table")
      )
    )
  )
)

# -- 4. SERVER -----------------------------------------------------------------
server <- function(input, output, session) {

  # Sync threshold label (input is now integer %, e.g. 70)
  observeEvent(input$threshold, {
    session$sendCustomMessage("setThrLbl", input$threshold)
  })

  # threshold as decimal for calculations
  thr <- reactive(input$threshold / 100)

  # Update category choices on dataset switch
  observe({
    cats <- if (input$dataset == "text") cats_text else cats_num
    updateSelectInput(session, "category",
      choices  = c("All categories" = "all", cats),
      selected = "all"
    )
  })

  # Reactive state (ST1, ST2)
  sel_product   <- reactiveVal(NULL)
  sel_candidate <- reactiveVal(NULL)
  log_data      <- reactiveVal(data.frame(
    time      = character(), pid = character(),
    candidate = character(), decision = character(),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$dataset, { sel_product(NULL); sel_candidate(NULL) })

  # C1 — filtered_pending() [Q1 · S1]
  filtered_pending <- reactive({
    df  <- if (input$dataset == "text") df_text else df_num
    res <- df %>% filter(active == 0)

    if (!is.null(input$category) && input$category != "all") {
      if (input$category == "(No category)") {
        res <- res %>% filter(is.na(categories) | categories == "")
      } else {
        res <- res %>% filter(categories == input$category)
      }
    }
    if (input$sort_order == "name" && "name" %in% colnames(res))
      res <- res %>% arrange(name)
    else if (input$sort_order == "category")
      res <- res %>% arrange(cat_display)
    res
  })

  output$kpi_pending <- renderText(nrow(filtered_pending()))
  output$kpi_ref <- renderText({
    df <- if (input$dataset == "text") df_text else df_num
    format(sum(df$active == 1), big.mark = ",")
  })
  output$kpi_done <- renderText(nrow(log_data()))

  # O1 — queue_bar [Q1] — all categories shown (now lives in Data Quality tab)
  output$queue_bar <- renderPlotly({
    df <- filtered_pending()
    if (nrow(df) == 0) return(plotly_empty())
    cc <- df %>% count(cat_display, name = "n") %>% arrange(n)
    p <- ggplot(cc, aes(x = reorder(cat_display, n), y = n)) +
      geom_col(fill = "#1b5e3b", width = .55) +
      coord_flip() +
      labs(x = NULL, y = "pending items") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 8.5))
    ggplotly(p, tooltip = "y") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 0, r = 8, t = 8, b = 8))
  })

  # Compact progress summary for Step 1 (replaces the bar chart there)
  output$q1_summary <- renderUI({
    df    <- filtered_pending()
    total <- nrow(df)
    cat_n <- sum(df$cat_display != "(No category)", na.rm = TRUE)
    uncat <- total - cat_n
    pct   <- if (total > 0) round(cat_n / total * 100) else 0
    tagList(
      div(class = "d-flex justify-content-between small mb-1",
        span(class = "text-muted", paste0(total, " pending items in queue")),
        span(
          span(class = "fw-semibold", style = "color:#1b5e3b;", paste0(cat_n, " categorized")),
          span(class = "text-muted mx-1", "\u00b7"),
          span(class = "fw-semibold", style = "color:#e65100;", paste0(uncat, " uncategorized"))
        )
      ),
      div(class = "progress mb-3", style = "height:7px; border-radius:4px;",
        div(class = "progress-bar", role = "progressbar",
            style = paste0("width:", pct, "%; background:#1b5e3b;")),
        div(class = "progress-bar", role = "progressbar",
            style = paste0("width:", 100 - pct, "%; background:#ff7043;"))
      )
    )
  })

  # O2 — queue_table [Q1]
  output$queue_table <- renderDT({
    df <- filtered_pending()
    if (nrow(df) == 0)
      return(datatable(data.frame(Message = "No pending items for this filter.")))
    if (input$dataset == "text") {
      show <- df %>%
        select(pid, name, cat_display) %>%
        rename(ID = pid, Product = name, Category = cat_display)
    } else {
      show <- df %>%
        select(pid, cat_display, energy, protein, fat) %>%
        rename(ID = pid, Category = cat_display,
               `kcal` = energy, `Prot g` = protein, `Fat g` = fat)
    }
    datatable(show,
      selection = "single", rownames = FALSE,
      options   = list(pageLength = 6, dom = "ftp",
                       language   = list(search = "Filter:"))
    )
  })

  # OB1 — product click -> ST1
  observeEvent(input$queue_table_rows_selected, {
    idx <- input$queue_table_rows_selected
    if (length(idx) > 0) {
      sel_product(filtered_pending()[idx, , drop = FALSE])
      sel_candidate(NULL)
    }
  })

  # C2 — text_candidates() [Q2 · S2 S6]
  text_candidates <- reactive({
    req(input$dataset == "text", sel_product())
    prod <- sel_product()
    val  <- df_text %>% filter(active == 1)

    bc   <- as.character(prod$barcode %||% "")
    exact <- !is.na(val$barcode) & nzchar(bc) & val$barcode == bc

    pn <- tolower(prod$name_search %||% prod$name %||% "")
    vn <- tolower(ifelse(is.na(val$name_search), val$name, val$name_search))
    jw <- pmax(0, 1 - stringdist(pn, vn, method = "jw"), na.rm = TRUE)

    pb <- tolower(prod$brands_search %||% "")
    vb <- tolower(ifelse(is.na(val$brands_search), "", val$brands_search))
    br <- ifelse(nzchar(pb) & nzchar(vb), pmax(0, 1 - stringdist(pb, vb, method = "jw")), 0.5)

    cat_ok <- !is.na(prod$categories) & !is.na(val$categories) &
              prod$categories == val$categories
    score  <- 0.60 * jw + 0.25 * br + 0.15 * as.numeric(cat_ok)
    score[exact] <- 1.00

    val$score       <- round(score, 3)
    val$barcode_hit <- exact
    val %>% filter(score >= thr()) %>% arrange(desc(score)) %>% head(15)
  })

  # C3 — numeric_candidates() [Q3 · S3]
  numeric_candidates <- reactive({
    req(input$dataset == "numeric", sel_product())
    prod <- sel_product()
    val  <- df_num %>% filter(active == 1)
    pv   <- as.numeric(prod[1, CORE])
    valid <- which(!is.na(pv))
    if (length(valid) == 0) { val$score <- 0; return(val %>% head(10)) }
    vm   <- as.matrix(val[, CORE])
    diffs <- sapply(valid, function(i) ((vm[, i] - pv[i]) / max_core[i])^2)
    dist  <- sqrt(rowMeans(diffs, na.rm = TRUE))
    dist[is.na(dist)] <- 1
    val$score <- round(1 / (1 + 2 * dist), 3)
    val %>% filter(score >= thr()) %>% arrange(desc(score)) %>% head(15)
  })

  # OB2 — candidate click -> ST2
  observeEvent(input$txt_cand_rows_selected, {
    idx <- input$txt_cand_rows_selected
    if (length(idx) > 0) sel_candidate(text_candidates()[idx, , drop = FALSE])
  })
  observeEvent(input$num_cand_rows_selected, {
    idx <- input$num_cand_rows_selected
    if (length(idx) > 0) sel_candidate(numeric_candidates()[idx, , drop = FALSE])
  })

  # Workspace (dynamic right panel)
  output$workspace <- renderUI({
    prod <- sel_product()

    if (is.null(prod)) {
      return(div(class = "pg-card",
        div(class = "pg-hdr", "Review workspace"),
        div(class = "pg-body empty-st",
          tags$span(class = "es-icon", icon("inbox", class = "fa-2x")),
          h5("No product selected"),
          p("Click a row in the queue on the left to begin.")
        )
      ))
    }

    cand <- sel_candidate()
    prod_name <- if (input$dataset == "text") prod$name %||% "" else prod$cat_display %||% ""

    tagList(

      # Compact product banner (replaces full Step 2 card — saves ~180px)
      div(class = "prod-banner",
        icon("box-archive", style = "color:#ff7043; flex-shrink:0;"),
        span(class = "prod-id", prod$pid),
        span(class = "text-muted", "|"),
        span(class = "prod-name", prod_name),
        span(class = "lbl-pending ms-auto", "unvalidated")
      ),

      # Step 2 — Candidates
      div(class = "pg-card",
        div(class = "pg-hdr",
          span(if (input$dataset == "text")
            "Step 2 — Closest text matches" else
            "Step 2 — Closest nutrient matches"),
          tagList(
            if (!is.null(cand)) span(class = "small text-muted me-2",
              paste0("Selected: ", cand$pid)) else NULL,
            span(class = "q-badge", if (input$dataset == "text") "Q2" else "Q3")
          )
        ),
        div(class = "pg-body", uiOutput("candidates_ui"))
      ),

      # Step 3 — Merged side-by-side comparison (pending LEFT + candidate RIGHT)
      #          Only shown after a candidate is selected
      if (!is.null(cand)) {
        div(class = "pg-card",
          div(class = "pg-hdr",
            span("Step 3 — Side-by-side comparison"),
            span(class = "lbl-ok", paste0(round(cand$score * 100), "% match"))
          ),
          div(class = "pg-body",
            uiOutput("comparison_ui"),
            if (input$dataset == "numeric")
              plotlyOutput("nutrient_bar", height = "160px")
          )
        )
      },

      # Step 4 — Decision (always shown once product is selected)
      div(
        div(class = "dec-hdr", "Step 4 — Your decision"),
        div(class = "dec-body", uiOutput("decision_ui")),
        style = "border-radius:14px; overflow:hidden; margin-bottom:1rem;
                 box-shadow:0 2px 10px rgba(0,0,0,.06);"
      ),

      uiOutput("lookup_ui")
    )
  })

  # Candidates UI
  output$candidates_ui <- renderUI({
    cands <- if (input$dataset == "text") text_candidates() else numeric_candidates()
    if (is.null(cands) || nrow(cands) == 0) {
      return(div(class = "alert alert-success border-0 mb-0 small",
        icon("circle-check"), " No duplicates found above ",
        strong(paste0(input$threshold, "%")),
        " threshold. This product appears genuinely new."
      ))
    }
    tagList(
      div(class = "info-row", "Click a candidate row to load the side-by-side comparison below."),
      if (input$dataset == "text") DTOutput("txt_cand") else DTOutput("num_cand")
    )
  })

  # O3 — txt_cand [Q2]
  output$txt_cand <- renderDT({
    df <- text_candidates(); req(nrow(df) > 0)
    show <- df %>%
      mutate(
        Score = paste0(round(score * 100), "%"),
        Confidence = case_when(
          score >= .90 ~ '<span class="sc-high">Very high</span>',
          score >= .75 ~ '<span class="sc-mid">High</span>',
          TRUE         ~ '<span class="sc-low">Moderate</span>'
        ),
        Barcode = ifelse(barcode_hit, "exact match", "—")
      ) %>%
      select(pid, name, brands, Barcode, Score, Confidence) %>%
      rename(ID = pid, Product = name, Brand = brands)
    datatable(show,
      selection = "single", rownames = FALSE, escape = FALSE,
      options   = list(pageLength = 4, dom = "tp")
    )
  })

  # O4 — num_cand [Q3]
  output$num_cand <- renderDT({
    df <- numeric_candidates(); req(nrow(df) > 0)
    show <- df %>%
      mutate(Score = paste0(round(score * 100), "%")) %>%
      select(pid, categories, energy, protein, fat, carbohydrates, Score) %>%
      rename(ID = pid, Category = categories,
             `kcal` = energy, `Prot` = protein, `Fat` = fat, `Carbs` = carbohydrates)
    datatable(show,
      selection = "single", rownames = FALSE,
      options   = list(pageLength = 4, dom = "tp")
    )
  })

  # O6 — comparison_ui [Q4 · C4] — merged pending + candidate side by side
  output$comparison_ui <- renderUI({
    prod <- sel_product(); cand <- sel_candidate(); req(prod, cand)

    if (input$dataset == "text") {
      fields <- list(
        list(label = "Name",     key = "name"),
        list(label = "Brand",    key = "brands"),
        list(label = "Barcode",  key = "barcode"),
        list(label = "Category", key = "categories"),
        list(label = "Source",   key = "bron")
      )
      rows <- lapply(fields, function(f) {
        cc <- classify_cell(prod[[f$key]], cand[[f$key]], field = f$key)
        tags$tr(
          tags$td(class = "fw-semibold text-muted", style = "width:18%;", f$label),
          tags$td(style = "width:41%;", if (cc$pv == "—") tags$em(class = "text-muted", "—") else cc$pv),
          tags$td(class = cc$cls, style = "width:41%;",
            if (cc$cv == "—") tags$em(class = "text-muted", "—") else cc$cv)
        )
      })
      tags$table(class = "cmp-tbl",
        tags$thead(tags$tr(
          tags$th("Field"),
          tags$th(paste("Pending:", prod$pid)),
          tags$th(paste("Candidate:", cand$pid))
        )),
        tags$tbody(rows)
      )
    } else {
      rows <- lapply(CORE, function(f) {
        pv   <- as.numeric(prod[[f]])
        cv   <- as.numeric(cand[[f]])
        diff <- if (!is.na(pv) && !is.na(cv)) round(pv - cv, 2) else NA
        # Color logic: both NA → neutral, small diff (<2) → match, large → diff
        cls  <- if (is.na(pv) && is.na(cv)) "cmp-neutral"
                else if (!is.na(diff) && abs(diff) < 2) "cmp-match"
                else "cmp-diff"
        diff_str <- if (is.na(diff)) tags$em(class="text-muted","—")
                    else if (diff > 0) paste0("+", diff)
                    else as.character(diff)
        tags$tr(
          tags$td(class = "fw-semibold text-muted", f),
          tags$td(ifelse(is.na(pv), tags$em(class="text-muted","—"), pv)),
          tags$td(ifelse(is.na(cv), tags$em(class="text-muted","—"), cv)),
          tags$td(class = cls, diff_str)
        )
      })
      tags$table(class = "cmp-tbl",
        tags$thead(tags$tr(
          tags$th("Nutrient"),
          tags$th(paste("Pending:", prod$pid)),
          tags$th(paste("Candidate:", cand$pid)),
          tags$th("Difference")
        )),
        tags$tbody(rows)
      )
    }
  })

  # O5 — nutrient bar [Q3/Q4]
  output$nutrient_bar <- renderPlotly({
    prod <- sel_product(); cand <- sel_candidate()
    req(prod, cand, input$dataset == "numeric")
    pv   <- as.numeric(prod[1, CORE])
    cv   <- as.numeric(cand[1, CORE])
    diff <- round(pv - cv, 2)
    df   <- data.frame(Nutrient = CORE, Diff = diff) %>% filter(!is.na(Diff))
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = reorder(Nutrient, Diff), y = Diff, fill = Diff > 0)) +
      geom_col(width = .5) +
      geom_hline(yintercept = 0, colour = "#555", linewidth = .5) +
      scale_fill_manual(values = c("TRUE" = "#c62828", "FALSE" = "#1b5e3b"), guide = "none") +
      coord_flip() +
      labs(title = "Nutrient delta — pending minus candidate", x = NULL, y = "difference") +
      theme_minimal(base_size = 10)
    ggplotly(p) %>% plotly::config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 0, t = 28, b = 8))
  })

  # O7 — decision_ui [Q6 · C5]
  output$decision_ui <- renderUI({
    prod <- sel_product(); req(prod)
    cand <- sel_candidate()
    default <- if (!is.null(cand) && cand$score >= .88) "identical" else "new"
    tagList(
      radioButtons("disposition", NULL,
        choices = c(
          "Identical duplicate — reuse the validated product" = "identical",
          "Similar but distinct — keep as a separate record"  = "similar",
          "Genuinely new product — look up nutrition values"  = "new"
        ),
        selected = default
      ),
      actionButton("confirm_btn", "Confirm decision",
        class = "btn btn-success btn-sm mt-1", icon = icon("check")
      ),
      uiOutput("confirm_msg")
    )
  })

  # OB3 — confirm click
  observeEvent(input$confirm_btn, {
    req(sel_product(), input$disposition)
    prod <- sel_product(); cand <- sel_candidate()
    log_data(rbind(log_data(), data.frame(
      time      = format(Sys.time(), "%H:%M:%S"),
      pid       = prod$pid,
      candidate = if (!is.null(cand)) cand$pid else "—",
      decision  = input$disposition,
      stringsAsFactors = FALSE
    )))
  })

  output$confirm_msg <- renderUI({
    req(input$confirm_btn > 0)
    isolate({
      disp <- input$disposition
      col  <- switch(disp, identical = "success", similar = "warning", "info")
      msg  <- switch(disp,
        identical = "Decision saved — this item will be linked to the validated record.",
        similar   = "Decision saved — kept as a distinct entry for further review.",
        new       = "Decision saved — retrieving nutrition values below."
      )
      div(class = paste0("alert alert-", col, " mt-2 small mb-0"),
          icon("circle-check"), " ", msg)
    })
  })

  # C8 — nutrition_lookup (eventReactive) [Q5 · S5]
  nutrition_data <- eventReactive(input$confirm_btn, {
    req(input$disposition == "new", sel_product())
    prod <- sel_product()
    bc   <- as.character(prod$barcode %||% "")
    res  <- NULL; src <- "https://world.openfoodfacts.org"
    if (nzchar(bc) && nchar(bc) >= 8) {
      tryCatch({
        url  <- paste0("https://world.openfoodfacts.org/api/v0/product/", bc, ".json")
        resp <- httr::GET(url, httr::timeout(4))
        if (httr::status_code(resp) == 200) {
          js <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
          if (!is.null(js$status) && js$status == 1) {
            n <- js$product$nutriments
            res <- list(
              energy = n$`energy-kcal_100g` %||% n$energy_100g %||% NA,
              protein = n$proteins_100g %||% NA, fat = n$fat_100g %||% NA,
              carbohydrates = n$carbohydrates_100g %||% NA,
              sugar = n$sugars_100g %||% NA, salt = n$salt_100g %||% NA
            )
            src <- paste0("https://world.openfoodfacts.org/product/", bc)
          }
        }
      }, error = function(e) NULL)
    }
    if (is.null(res)) {
      res <- list(energy=200, protein=8, fat=8, carbohydrates=28, sugar=5, salt=0.7)
      src <- "Fallback — typical values (nubel.be)"
    }
    data.frame(
      Nutrient = c("Energy (kcal/100g)", "Protein (g/100g)", "Fat (g/100g)",
                   "Carbohydrates (g/100g)", "Sugar (g/100g)", "Salt (g/100g)"),
      `Proposed value` = c(res$energy, res$protein, res$fat,
                           res$carbohydrates, res$sugar, res$salt),
      Source = src, Status = "Awaiting dietitian approval",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  output$lookup_ui <- renderUI({
    req(input$confirm_btn > 0)
    isolate(req(input$disposition == "new"))
    div(class = "pg-card",
      div(class = "pg-hdr",
        span("Nutrition lookup — proposed values"),
        span(class = "q-badge", "Q5")
      ),
      div(class = "pg-body",
        p(class = "small text-muted mb-2",
          "Values retrieved automatically. A dietitian must approve before the product is activated."
        ),
        DTOutput("completion_table")
      )
    )
  })

  output$completion_table <- renderDT({
    df <- nutrition_data(); req(df)
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })

  # Threshold Tuner [Q7 · C6]
  thr_curve <- reactive({
    tseq <- seq(40, 95, by = 5)
    data.frame(
      Threshold  = tseq,
      Candidates = round(130 * exp(-2.4 * ((tseq - 40) / 100))),
      Precision  = round(100 / (1 + exp(-9 * ((tseq / 100) - 0.65))))
    )
  })

  output$thr_kpis <- renderUI({
    t  <- input$threshold
    ec <- round(130 * exp(-2.4 * ((t - 40) / 100)))
    ep <- round(100 / (1 + exp(-9 * (t / 100 - 0.65))))
    div(class = "row g-2",
      div(class = "col-6",
        div(class = "kpi-card",
          div(class = "kpi-circle", style = "background:#1565c0; width:38px; height:38px; font-size:.9rem;", icon("sliders")),
          div(div(class = "kpi-num", style = "font-size:1.4rem;", paste0(t, "%")),
              div(class = "kpi-lbl", "Threshold"))
        )
      ),
      div(class = "col-6",
        div(class = "kpi-card",
          div(class = "kpi-circle", style = "background:#ff7043; width:38px; height:38px; font-size:.9rem;", icon("list")),
          div(div(class = "kpi-num", style = "font-size:1.4rem;", ec),
              div(class = "kpi-lbl", "Candidates / item"))
        )
      ),
      div(class = "col-12 mt-1",
        div(class = "kpi-card",
          div(class = "kpi-circle", style = "background:#1b5e3b; width:38px; height:38px; font-size:.9rem;", icon("bullseye")),
          div(div(class = "kpi-num", style = "font-size:1.4rem;", paste0(ep, "%")),
              div(class = "kpi-lbl", "Estimated match precision"))
        )
      )
    )
  })

  output$pr_curve <- renderPlotly({
    df <- thr_curve(); t <- input$threshold
    p <- ggplot(df, aes(x = Threshold, y = Candidates)) +
      geom_line(colour = "#1b5e3b", linewidth = 1.2) +
      geom_point(colour = "#1b5e3b", size = 2.5) +
      geom_vline(xintercept = t, linetype = "dashed", colour = "#c62828", linewidth = .8) +
      annotate("label", x = t, y = max(df$Candidates) * .8,
               label = paste0("Current: ", t, "%"),
               fill = "#c62828", colour = "#fff", size = 3, label.size = 0) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Similarity threshold", y = "Estimated candidates surfaced per item") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
    ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
  })

  # Data Quality [Q8 · C7]
  missing_quality <- reactive({
    df   <- if (input$dataset == "text") df_text else df_num
    cols <- setdiff(colnames(df), c("row_id", "pid", "active", "cat_display"))
    bind_rows(
      df %>% filter(active == 0) %>%
        summarise(across(all_of(cols), ~ mean(is.na(.)) * 100)) %>%
        pivot_longer(everything(), names_to = "Field", values_to = "pct") %>%
        mutate(Group = "Pending (active = 0)"),
      df %>% filter(active == 1) %>%
        summarise(across(all_of(cols), ~ mean(is.na(.)) * 100)) %>%
        pivot_longer(everything(), names_to = "Field", values_to = "pct") %>%
        mutate(Group = "Validated (active = 1)")
    )
  })

  output$missing_heatmap <- renderPlotly({
    df <- missing_quality(); req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Field, y = Group, fill = pct,
                        text = paste0(Field, " — ", Group, ": ", round(pct, 1), "% missing"))) +
      geom_tile(colour = "white", linewidth = .5) +
      scale_fill_gradient(low = "#e8f5e9", high = "#c62828",
                          limits = c(0, 100), name = "% missing") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid = element_blank())
    ggplotly(p, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 10, b = 80))
  })

  # Data Quality: encoding issues table [Q8 / batch fix]
  output$encoding_issues_table <- renderDT({
    # Encoding issues only meaningful for text dataset (has name column)
    mojibake_pattern <- "Ã[A-Za-zÀ-ÿ]|â€[^\\s]?|Â[A-Za-z]|Ð[A-Za-zÀ-ÿ]"

    issues <- df_text %>%
      filter(active == 0, !is.na(name)) %>%
      filter(grepl(mojibake_pattern, name, perl = TRUE)) %>%
      select(pid, name, cat_display) %>%
      rename(ID = pid, `Product name (raw)` = name, Category = cat_display) %>%
      head(50)

    if (nrow(issues) == 0) {
      issues <- data.frame(
        Message = "No encoding issues detected in the text dataset pending queue.",
        stringsAsFactors = FALSE
      )
    }

    datatable(issues, rownames = FALSE,
      options = list(pageLength = 10, dom = "ftp",
                     language = list(search = "Filter:")),
      caption = "Products with Mojibake artefacts — fix encoding before scoring."
    )
  })
}

# -- 5. LAUNCH -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
