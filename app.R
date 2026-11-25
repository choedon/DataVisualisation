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
library(cicerone)

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

# -- Similarity scoring (shared by candidate search and threshold evaluation) --
# Text: 60% name JW + 25% brand JW + 15% same category. Barcode override is
# applied by the caller so the evaluation below can score names on their own.
W_NAME <- 0.60; W_BRAND <- 0.25; W_CAT <- 0.15

# Raw component similarities (0-1) — also drives the Step 3 score breakdown
score_text_parts <- function(prod, val) {
  pn <- tolower(prod$name_search %||% prod$name %||% "")
  vn <- tolower(ifelse(is.na(val$name_search), val$name, val$name_search))
  jw <- pmax(0, 1 - stringdist(pn, vn, method = "jw"), na.rm = TRUE)
  pb <- tolower(prod$brands_search %||% "")
  vb <- tolower(ifelse(is.na(val$brands_search), "", val$brands_search))
  brand_known <- nzchar(pb) & nzchar(vb)
  br <- ifelse(brand_known, pmax(0, 1 - stringdist(pb, vb, method = "jw")), 0.5)
  cat_ok <- !is.na(prod$categories) & !is.na(val$categories) &
            prod$categories == val$categories
  data.frame(name = jw, brand = br, brand_known = brand_known, cat = as.numeric(cat_ok))
}

score_text <- function(prod, val) {
  p <- score_text_parts(prod, val)
  round(W_NAME * p$name + W_BRAND * p$brand + W_CAT * p$cat, 3)
}

# Numeric: 1 / (1 + 2 * normalised RMS distance over the core nutrients)
score_num <- function(prod, val) {
  pv    <- as.numeric(prod[1, CORE])
  valid <- which(!is.na(pv))
  if (length(valid) == 0) return(rep(0, nrow(val)))
  vm    <- as.matrix(val[, CORE])
  diffs <- matrix(sapply(valid, function(i) ((vm[, i] - pv[i]) / max_core[i])^2),
                  nrow = nrow(val))
  dist  <- sqrt(rowMeans(diffs, na.rm = TRUE))
  dist[is.na(dist)] <- 1
  round(1 / (1 + 2 * dist), 3)
}

# -- Threshold evaluation (computed once at startup) ---------------------------
# For every pending item, score it against all validated items and count how
# many clear each threshold. Ground truth: a pending text product whose barcode
# matches a validated product is a known duplicate. Names are scored WITHOUT the
# barcode override, so we measure whether text similarity alone would find it.
THR_SEQ <- seq(40, 95, by = 5)

eval_thresholds <- function(df, kind) {
  pend <- df %>% filter(active == 0)
  val  <- df %>% filter(active == 1)
  scorer <- if (kind == "text") score_text else score_num
  S <- vapply(seq_len(nrow(pend)), function(i) scorer(pend[i, ], val),
              numeric(nrow(val)))                      # rows = validated, cols = pending

  lab <- integer(0)
  if (kind == "text") {
    lab <- which(!is.na(pend$barcode) & pend$barcode %in% na.omit(val$barcode))
  }
  truth <- lapply(lab, function(j) !is.na(val$barcode) & val$barcode == pend$barcode[j])

  bind_rows(lapply(THR_SEQ, function(t) {
    above <- colSums(S >= t / 100)
    out <- data.frame(
      Threshold = t,
      cand_mean = mean(above), cand_median = median(above),
      n_labelled = length(lab), TP = NA_integer_, FN = NA_integer_, FP = NA_integer_,
      precision = NA_real_, fn_ids = ""
    )
    if (length(lab) > 0) {
      hit <- vapply(seq_along(lab), function(k) any(S[truth[[k]], lab[k]] >= t / 100), logical(1))
      tpc <- vapply(seq_along(lab), function(k) sum(S[truth[[k]],  lab[k]] >= t / 100), numeric(1))
      fp  <- vapply(seq_along(lab), function(k) sum(S[!truth[[k]], lab[k]] >= t / 100), numeric(1))
      out$TP <- sum(hit); out$FN <- sum(!hit); out$FP <- sum(fp)
      # Precision = share of surfaced candidates (for labelled items) that are the true match
      out$precision <- if (sum(tpc) + sum(fp) > 0) sum(tpc) / (sum(tpc) + sum(fp)) * 100 else NA_real_
      out$fn_ids <- paste(pend$pid[lab][!hit], collapse = ", ")
    }
    out
  }))
}

# Knee of the candidates curve (Kneedle): on log10(candidates + 1), normalise
# both axes to [0, 1] and take the point furthest from the straight chord
# between the first and last thresholds — where the drop-off bends hardest.
find_knee <- function(d) {
  y  <- log10(d$cand_mean + 1)
  xn <- (d$Threshold - min(d$Threshold)) / diff(range(d$Threshold))
  yn <- if (diff(range(y)) > 0) (y - min(y)) / diff(range(y)) else rep(0, length(y))
  chord <- yn[1] + (yn[length(yn)] - yn[1]) * xn
  d$Threshold[which.max(abs(yn - chord))]
}

thr_eval <- list(text    = eval_thresholds(df_text, "text"),
                 numeric = eval_thresholds(df_num,  "numeric"))

# Sample IDs for hover text: "T-00001, T-00002, … (+12 more)"
sample_ids <- function(ids, k = 5) {
  if (length(ids) == 0) return("none")
  s <- paste(head(ids, k), collapse = ", ")
  if (length(ids) > k) paste0(s, ", … (+", length(ids) - k, " more)") else s
}

# -- Nutrition (per 100 g / 100 ml) -------------------------------------------
# key = column in numeric_food, off = Open Food Facts nutriment field.
# A difference is "meaningful" when it exceeds BOTH the absolute tolerance and
# 10% of the larger value — so 0.2 g vs 0.3 g salt isn't flagged, 2 g vs 9 g is.
NUTR <- data.frame(
  key   = c("energy", "protein", "carbohydrates", "sugar", "fat", "saturated_fatty_acid", "salt"),
  off   = c("energy-kcal_100g", "proteins_100g", "carbohydrates_100g", "sugars_100g",
            "fat_100g", "saturated-fat_100g", "salt_100g"),
  label = c("Energy", "Protein", "Carbs", "Sugars", "Fat", "Saturated fat", "Salt"),
  unit  = c("kcal", "g", "g", "g", "g", "g", "g"),
  tol   = c(15, 1, 1, 1, 1, 0.5, 0.15),
  stringsAsFactors = FALSE
)
nutr_differs <- function(a, b, tol) {
  !is.na(a) & !is.na(b) & abs(a - b) > pmax(tol, 0.10 * pmax(abs(a), abs(b)))
}

# -- Open Food Facts (on demand, cached per session process) -------------------
OFF_UA    <- httr::user_agent("FoodValidator/1.0 (Shiny dashboard)")
off_cache <- new.env()

off_parse <- function(p) {
  n <- p$nutriments %||% list()
  num <- function(f) { v <- suppressWarnings(as.numeric(n[[f]])); if (length(v) == 1) v else NA_real_ }
  list(found = TRUE,
       code  = as.character(p$code %||% ""),
       name  = as.character(p$product_name %||% ""),
       brand = as.character(p$brands %||% ""),
       nutr  = setNames(vapply(NUTR$off, num, numeric(1)), NUTR$key))
}

off_by_barcode <- function(bc) {
  bc <- trimws(strsplit(as.character(bc %||% ""), "[;, ]")[[1]][1] %||% "")
  if (is.na(bc) || !grepl("^[0-9]{8,14}$", bc)) return(list(found = FALSE, reason = "no usable barcode"))
  if (!is.null(off_cache[[bc]])) return(off_cache[[bc]])
  res <- tryCatch({
    r <- httr::GET(paste0("https://world.openfoodfacts.org/api/v2/product/", bc, ".json"),
                   OFF_UA, httr::timeout(5))
    js <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (identical(as.integer(js$status), 1L)) off_parse(js$product)
    else list(found = FALSE, reason = paste0("barcode ", bc, " not on Open Food Facts"))
  }, error = function(e) list(found = FALSE, reason = "Open Food Facts unreachable"))
  res$method <- paste("barcode", bc)
  if (!identical(res$reason, "Open Food Facts unreachable")) off_cache[[bc]] <- res
  res
}

off_by_name <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(trimws(name))) return(list(found = FALSE, reason = "no name to search"))
  res <- tryCatch({
    r <- httr::GET("https://world.openfoodfacts.org/cgi/search.pl", OFF_UA, httr::timeout(8),
                   query = list(search_terms = name, search_simple = 1, action = "process",
                                json = 1, page_size = 1))
    js <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (length(js$products) > 0) off_parse(js$products[[1]])
    else list(found = FALSE, reason = "no Open Food Facts product matches this name")
  }, error = function(e) list(found = FALSE, reason = "Open Food Facts unreachable"))
  res$method <- "name search (best hit — verify it is the same product)"
  res
}

# -- Dietitian-critical keywords ----------------------------------------------
# Product form, dietary claims and flavours that make two same-brand products
# nutritionally different. Patterns are matched case-insensitively on names.
KEYWORDS <- list(
  form = c(
    pudding = "pudding|pouding", powder = "poeder|powder|poudre|\\bmix\\b",
    drink = "\\bdrink|drank|boisson|\\bshake|smoothie", bar = "\\breep|\\bbar\\b|\\bbarre",
    sauce = "\\bsaus|\\bsauce", soup = "\\bsoep|\\bsoupe|\\bsoup",
    yoghurt = "yoghurt|yogurt|yaourt|\\bskyr|kwark", syrup = "siroop|sirop|syrup",
    juice = "\\bsap\\b|\\bjus\\b|juice", prepared = "\\bbereid|prepared|préparé",
    concentrate = "concentra"),
  claim = c(
    `gluten-free` = "glutenvrij|gluten[- ]?free|sans gluten|no gluten|zonder gluten",
    `sugar-free`  = "suikervrij|sugar[- ]?free|sans sucre|zonder suiker|no sugar|zero sugar|0% suiker",
    light = "\\blight\\b|\\blite\\b|allégé|\\bminder\\b",
    `lactose-free` = "lactosevrij|lactose[- ]?free|sans lactose",
    `plant-based` = "\\bvegan|plant[- ]?based|plantaardig|végétal",
    organic = "\\bbio\\b|biologisch|organic",
    protein = "protein|proteïne|proteine|\\beiwit",
    `low-fat` = "vetarm|low[- ]?fat|0% vet|\\bmager",
    `low-salt` = "zoutarm|minder zout|low[- ]?salt",
    sweeteners = "zoetstof|polyol|sweetener",
    wholegrain = "volkoren|whole ?grain|complet"),
  flavour = c(
    chocolate = "choco|cacao|cocoa", vanilla = "vanill", strawberry = "aardbei|strawberr|fraise",
    raspberry = "framboo?s|rasp?berr|framboise",   # also catches the common "rasberry" typo hazelnut = "hazelno|hazelnut|noisette",
    lemon = "citroen|lemon|citron", orange = "sinaasappel|\\borange|naranja",
    banana = "banaan|banan", cherry = "\\bkers|cherr|cerise", peach = "perzik|peach|pêche",
    mango = "mango|mangue", coconut = "kokos|coconut|coco\\b", caramel = "karamel|caramel",
    coffee = "koffie|coffee|\\bcafé|mocca|mocha|cappuccino|latte",
    honey = "honing|honey|\\bmiel", apple = "\\bappel|\\bapple|\\bpomme",
    mint = "\\bmunt|\\bmint", cinnamon = "kaneel|cinnamon|cannelle",
    natural = "\\bnatuur|\\bnature\\b|\\bnaturel|\\bplain", paprika = "paprika",
    cheese = "\\bkaas|cheese|fromage", pistachio = "pistach")
)
KW_FLAT <- unlist(lapply(names(KEYWORDS), function(g)
  setNames(KEYWORDS[[g]], paste0(g, ":", names(KEYWORDS[[g]])))))

find_keywords <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(character(0))
  names(KW_FLAT)[vapply(KW_FLAT, function(p) grepl(p, txt, ignore.case = TRUE, perl = TRUE), logical(1))]
}

# Escape a name, then bold every keyword hit
highlight_keywords <- function(txt) {
  if (is.null(txt) || is.na(txt)) return("")
  out <- htmltools::htmlEscape(txt)
  pat <- paste0("(", paste(KW_FLAT, collapse = "|"), ")")
  gsub(paste0("(?i)", pat), "<b class='kw-hit'>\\1</b>", out, perl = TRUE)
}

# -- UTF-8 repair (mojibake: UTF-8 bytes that were decoded as Windows-1252) ----
# Detects Latin (Ã©, Ã¨…), punctuation (â€œ, â€™) and Cyrillic (ÐœÐ°, Ñ€…) artefacts.
MOJI_RX <- paste0(
  "[ÂÃÐÑ][\\x{80}-\\x{BF}\\x{152}\\x{153}\\x{160}\\x{161}\\x{178}\\x{17D}\\x{17E}",
  "\\x{192}\\x{2C6}\\x{2DC}\\x{2013}\\x{2014}\\x{2018}-\\x{201E}\\x{2020}-\\x{2022}",
  "\\x{2026}\\x{2030}\\x{2039}\\x{203A}\\x{20AC}\\x{2122}]|[âá]€")
CP1252_REV <- c(`8364`=0x80,`8218`=0x82,`402`=0x83,`8222`=0x84,`8230`=0x85,`8224`=0x86,
  `8225`=0x87,`710`=0x88,`8240`=0x89,`352`=0x8A,`8249`=0x8B,`338`=0x8C,`381`=0x8E,
  `8216`=0x91,`8217`=0x92,`8220`=0x93,`8221`=0x94,`8226`=0x95,`8211`=0x96,`8212`=0x97,
  `732`=0x98,`8482`=0x99,`353`=0x9A,`8250`=0x9B,`339`=0x9C,`382`=0x9E,`376`=0x9F)

has_mojibake <- function(x) !is.na(x) & grepl(MOJI_RX, x, perl = TRUE)

# Re-encode to the original bytes and decode as UTF-8; NA if not cleanly reversible
fix_mojibake <- function(s) {
  vapply(s, function(x) {
    if (is.na(x)) return(NA_character_)
    cp <- utf8ToInt(x)
    b  <- ifelse(cp < 256, cp, CP1252_REV[as.character(cp)])
    if (anyNA(b)) return(NA_character_)
    out <- rawToChar(as.raw(b)); Encoding(out) <- "UTF-8"
    if (validUTF8(out)) out else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# All repairable cells in name/brands, as a long before/after table
mojibake_repairs <- function(df) {
  empty <- data.frame(row = integer(), pid = character(), active = numeric(), field = character(),
                      before = character(), after = character(), stringsAsFactors = FALSE)
  bind_rows(empty, lapply(c("name", "brands"), function(col) {
    hit <- which(has_mojibake(df[[col]]))
    if (!length(hit)) return(NULL)
    data.frame(row = hit, pid = df$pid[hit], active = df$active[hit], field = col,
               before = df[[col]][hit], after = fix_mojibake(df[[col]][hit]),
               stringsAsFactors = FALSE)
  })) %>% filter(!is.na(after))
}

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
    "cmp-neutral"               # both missing  → slate grey (—)
  } else if (!pm && !cm && pv == cv) {
    "cmp-match"                 # exact match   → brand green
  } else if (pm && !cm) {
    if (!is.null(field) && field == "barcode") "cmp-info" else "cmp-diff"
    # pending has no barcode but candidate does → neutral slate (context, not an error)
    # other fields → terracotta
  } else if (!pm && cm) {
    "cmp-diff"                  # candidate missing, pending has value → terracotta
  } else {
    "cmp-diff"                  # values differ → terracotta
  }
  list(cls = cls, pv = pv, cv = cv)
}

# -- 2. THEME & CSS ------------------------------------------------------------
# Three-role palette: neutral greyscale for structure, ONE brand/action colour,
# ONE alert/focus accent. Every colour in the app should come from here.
PAL <- list(
  bg          = "#F8FAFC",  # page background
  card        = "#FFFFFF",  # cards / surfaces
  border      = "#E2E8F0",  # borders, neutral badge fill
  text        = "#0F172A",  # main text
  muted       = "#64748B",  # secondary text
  slate       = "#475569",  # neutral badge text / icons
  missing     = "#94A3B8",  # missing values (—)
  bar_muted   = "#CBD5E1",  # de-emphasised data marks
  grid        = "#F1F5F9",  # minimal gridlines
  primary     = "#1B4332",  # brand / action
  brand_soft  = "#ECFDF5",  # soft brand tint
  accent      = "#D95D39",  # alert / focus
  accent_soft = "#FBEAE4"   # soft accent tint
)

# Shared plotly styling: palette text, minimal grid, neutral hover card
pal_plotly <- function(p) {
  p %>% layout(
    font          = list(family = "Inter, sans-serif", color = PAL$text, size = 11),
    paper_bgcolor = PAL$card, plot_bgcolor = PAL$card,
    hoverlabel    = list(align = "left", bgcolor = PAL$card, bordercolor = PAL$border,
                         font = list(color = PAL$text, size = 11))
  ) %>% plotly::config(displayModeBar = FALSE)
}
hint <- function(x) paste0("<span style='color:", PAL$muted, "'>", x, "</span>")

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  bg         = PAL$card,
  fg         = PAL$text,
  primary    = PAL$primary,
  secondary  = PAL$muted,
  success    = PAL$primary,   # flatly's default success is mint-teal
  info       = PAL$slate,     # flatly's default info is bright blue
  warning    = PAL$accent,
  danger     = PAL$accent,
  base_font  = font_google("Inter"),
  `font-size-base` = "0.9rem"
)

app_css <- tags$style(HTML(paste0("
  :root {
    --bg: ", PAL$bg, "; --card: ", PAL$card, "; --border: ", PAL$border, ";
    --text: ", PAL$text, "; --muted: ", PAL$muted, "; --slate: ", PAL$slate, ";
    --missing: ", PAL$missing, "; --bar-muted: ", PAL$bar_muted, "; --grid: ", PAL$grid, ";
    --primary: ", PAL$primary, "; --brand-soft: ", PAL$brand_soft, ";
    --accent: ", PAL$accent, "; --accent-soft: ", PAL$accent_soft, ";
  }
", "
  /* Base */
  body { background: var(--bg); color: var(--text); font-family: 'Inter', sans-serif; }
  .text-muted { color: var(--muted) !important; }

  /* Navigation — classic left sidebar (>= 992px); falls back to a top bar on narrow screens */
  .navbar {
    background: var(--primary) !important; border: none; box-shadow: none;
    padding: .55rem 1.25rem;
  }
  .navbar-brand { color: #FFFFFF !important; font-family: 'Fraunces', Georgia, serif; font-weight: 600; font-size: 1.08rem; letter-spacing: -.2px;
                  display: flex; align-items: center; gap: .6rem; }
  .brand-mark { width: 32px; height: 32px; border-radius: 9px; display: inline-flex; align-items: center;
                justify-content: center; background: rgba(255,255,255,.12); font-size: .9rem; }
  .brand-sub { display: block; font-family: 'Inter', sans-serif; font-size: .68rem; font-weight: 500; color: rgba(255,255,255,.6);
               letter-spacing: .02em; margin-top: 1px; }
  .navbar-nav .nav-link {
    font-size: .84rem; font-weight: 500; color: rgba(255,255,255,.72) !important;
    padding: .55rem .85rem !important; border-radius: 8px; margin: 1px 0;
    display: flex; align-items: center; gap: .7rem; border: none !important;
    transition: background .15s, color .15s;
  }
  .navbar-nav .nav-link i { width: 16px; text-align: center; opacity: .85; }
  .navbar-nav .nav-link:hover { color: #FFFFFF !important; background: rgba(255,255,255,.08); }
  .navbar-nav .nav-link.active {
    color: var(--primary) !important; background: #FFFFFF; font-weight: 600;
  }
  .navbar-nav .nav-link.active i { opacity: 1; }
  .nav-underline .nav-link.active, .nav-underline .nav-link:hover { border-bottom: none !important; }
  .nav-section { display: none; }
  .navbar-toggle { background: transparent !important; border: 1px solid rgba(255,255,255,.4) !important;
                   border-radius: 8px; padding: 8px 9px; }
  .navbar-toggle .icon-bar { display: block; width: 20px !important; height: 2px !important; border-radius: 1px;
                             background: #FFFFFF !important; margin: 4px 0; }

  @media (min-width: 992px) {
    /* Collapsed to a 68px icon rail; expands over the page on hover / keyboard focus */
    body { padding-left: 68px; }
    .navbar {
      position: fixed; top: 0; left: 0; bottom: 0; width: 68px; z-index: 1030;
      padding: 1.25rem .9rem 1rem; align-items: stretch; overflow: hidden;
      transition: width .22s ease, box-shadow .22s ease;
    }
    .navbar:hover, .navbar:has(:focus-visible) { width: 236px; box-shadow: 6px 0 24px rgba(15, 23, 42, .18); }
    .navbar .nav-link, .navbar .btn-tour, .navbar-brand { white-space: nowrap; }
    .brand-text, .nav-section { transition: opacity .15s ease; }
    .navbar:not(:hover):not(:has(:focus-visible)) .brand-text,
    .navbar:not(:hover):not(:has(:focus-visible)) .nav-section { opacity: 0; }
    .navbar:not(:hover):not(:has(:focus-visible)) .nav-link { color: transparent !important; }
    .navbar:not(:hover):not(:has(:focus-visible)) .nav-link i { color: rgba(255,255,255,.8); }
    .navbar:not(:hover):not(:has(:focus-visible)) .nav-link.active i { color: var(--primary); }
    .navbar:not(:hover):not(:has(:focus-visible)) .btn-tour { justify-content: flex-start; padding-left: 11px; color: transparent !important; }
    .navbar:not(:hover):not(:has(:focus-visible)) .btn-tour i { color: #FFFFFF; }
    .navbar > .container-fluid { flex-direction: column; align-items: stretch; height: 100%; padding: 0; flex-wrap: nowrap; }
    .navbar-header { padding: 0 .1rem 1.1rem; margin-bottom: .9rem; border-bottom: 1px solid rgba(255,255,255,.12); }
    .navbar-brand { margin: 0; padding: 0; white-space: normal; line-height: 1.15; }
    .navbar-collapse { display: flex !important; flex: 1; align-items: stretch; }
    .navbar-nav { flex-direction: column !important; width: 100%; height: 100%; gap: 0; }
    .nav-section { display: block; font-size: .66rem; font-weight: 600; letter-spacing: .09em;
                   text-transform: uppercase; color: rgba(255,255,255,.45); padding: 0 .85rem .45rem; }
    .navbar-nav .bslib-nav-spacer { flex: 1; }
    .navbar-collapse { border-top: none !important; box-shadow: none !important; }
    /* narrower content area: slimmer KPI tiles so the filter bar stays on one row */
    .filter-bar .kpi-card { padding: .4rem .7rem !important; gap: .55rem; }
    .filter-bar .kpi-circle.sm { width: 32px; height: 32px; font-size: .78rem; }
    .filter-bar .kpi-num.sm { font-size: 1.15rem; }
    .filter-bar .kpi-lbl { font-size: .68rem; margin-top: 1px; }
    .navbar-nav li:has(#tour_btn) { padding-top: .9rem; border-top: 1px solid rgba(255,255,255,.12); }
    /* inner content keeps its expanded width and is clipped by the rail */
    .navbar-header, .navbar-collapse { width: 207px; min-width: 207px; }
    .navbar-nav .nav-item { width: 100%; }
    .navbar .nav-link, .navbar .btn-tour { width: 100%; transition: width .22s ease, color .15s, background .15s; }
    .navbar:not(:hover):not(:has(:focus-visible)) .nav-link,
    .navbar:not(:hover):not(:has(:focus-visible)) .btn-tour { width: 40px; overflow: hidden; }
    .navbar-nav .btn-tour { width: 100%; justify-content: center; }
  }

  /* ---- Top filter bar ---- */
  .filter-bar {
    background: var(--card); border-radius: 12px;
    border: 1px solid var(--border);
    padding: .65rem 1.1rem;
    margin-bottom: 1rem;
    display: flex; align-items: flex-end; gap: 1rem; flex-wrap: wrap;
  }
  .filter-bar .fb-group { display: flex; flex-direction: column; }
  .filter-bar .fb-label {
    font-size: .66rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: .08em;
    color: var(--muted); margin-bottom: .2rem;
  }
  .filter-bar .form-select {
    font-size: .82rem; border-color: var(--border);
    border-radius: 8px; color: var(--text); min-width: 140px;
  }
  .filter-bar .thr-group { flex: 1; min-width: 180px; padding-top: 6px; }  /* keeps label clear of the slider bubble */
  .filter-bar .thr-row {
    display: flex; justify-content: space-between; align-items: center;
    margin-bottom: .15rem;
  }
  .filter-bar .thr-row .fb-label { margin-bottom: 0; }
  .filter-bar .thr-val { font-size: .82rem; font-weight: 700; color: var(--primary); }

  /* KPI strip */
  .kpi-card {
    background: var(--card); border-radius: 14px;
    border: 1px solid var(--border);
    padding: 1rem 1.15rem;
    display: flex; align-items: center; gap: .95rem;
  }
  .kpi-circle {
    width: 50px; height: 50px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    font-size: 1.1rem; flex-shrink: 0;
  }
  .kpi-circle.sm  { width: 38px; height: 38px; font-size: .88rem; }
  .kpi-alert      { background: var(--accent);     color: #FFFFFF; }       /* the one thing to act on */
  .kpi-brand-soft { background: var(--brand-soft); color: var(--primary); }
  .kpi-neutral    { background: var(--border);     color: var(--slate); }
  .kpi-num { font-size: 1.75rem; font-weight: 700; color: var(--text); line-height: 1.1; }
  .kpi-num.sm { font-size: 1.38rem; }
  .kpi-lbl { font-size: .72rem; color: var(--muted); margin-top: 3px; }

  /* Page cards */
  .pg-card {
    background: var(--card); border-radius: 14px;
    border: 1px solid var(--border);
    margin-bottom: 1rem; overflow: hidden;
  }
  .pg-hdr {
    padding: .7rem 1.1rem; border-bottom: 1px solid var(--border);
    display: flex; align-items: center; justify-content: space-between;
    font-weight: 600; font-size: .86rem; color: var(--text);
  }
  .pg-hdr .q-badge {
    background: var(--bg); color: var(--muted); border: 1px solid var(--border);
    border-radius: 6px; padding: 2px 8px; font-size: .69rem;
  }
  .pg-body { padding: .9rem 1.1rem; }
  .card-note {
    font-size: .73rem; color: var(--muted); margin-top: .5rem;
    border-top: 1px solid var(--border); padding-top: .7rem;
  }

  /* Validation: progressive disclosure (full-width queue -> 5/7 split) */
  .val-grid { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 0fr);
              column-gap: 0; transition: all 0.25s ease; }
  .val-grid.split { grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); column-gap: 1rem; }
  .val-reveal { opacity: 0; transition: all 0.25s ease; }
  .val-grid.split .val-reveal { opacity: 1; }
  .val-grid:not(.split) .val-reveal { display: none; }   /* expanded queue: no Step 2 underneath */
  .val-left, .val-right { min-width: 0; transition: all 0.25s ease; }
  .val-right { opacity: 0; visibility: hidden; transform: translateX(16px); overflow: hidden; }
  .val-grid.split .val-right { opacity: 1; visibility: visible; transform: none; overflow: visible; }
  @media (max-width: 991px) { .val-grid.split { grid-template-columns: minmax(0, 1fr); row-gap: 1rem; } }
  .btn-back { font-size: .74rem; color: var(--slate) !important; background: transparent !important;
              border: 1px solid var(--border) !important; border-radius: 999px; padding: .1rem .6rem;
              margin-left: .4rem; white-space: nowrap; }
  .btn-back:hover { color: var(--text) !important; border-color: var(--slate) !important; background: var(--grid) !important; }

  /* Product banner (compact, replaces full Step 2 card) */
  .prod-banner {
    display: flex; align-items: center; gap: .55rem;
    padding: .55rem .9rem; margin-bottom: .75rem;
    background: var(--card); border-radius: 10px;
    font-size: .84rem; border: 1px solid var(--border);
    border-left: 4px solid var(--accent);
  }
  .prod-banner .prod-icon { color: var(--accent); flex-shrink: 0; }
  .prod-banner .prod-id { font-weight: 700; color: var(--text); }
  .prod-banner .prod-name { color: var(--muted); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 300px; }

  /* Status labels */
  .lbl-pending {                       /* subtle neutral outline */
    background: transparent; color: var(--slate);
    border: 1px solid var(--bar-muted); border-radius: 20px;
    padding: 2px 10px; font-size: .72rem; font-weight: 600; white-space: nowrap;
  }
  .lbl-ok {
    background: var(--brand-soft); color: var(--primary);
    border: 1px solid var(--primary); border-radius: 20px;
    padding: 2px 10px; font-size: .72rem; font-weight: 600; white-space: nowrap;
  }

  /* Score badges */
  .sc-high { background: var(--primary);    color: #FFFFFF;        border-radius: 6px; padding: 2px 8px; font-size: .73rem; }
  .sc-mid  { background: var(--brand-soft); color: var(--primary); border-radius: 6px; padding: 2px 8px; font-size: .73rem; }
  .sc-low  { background: var(--border);     color: var(--slate);   border-radius: 6px; padding: 2px 8px; font-size: .73rem; }

  /* Info box */
  .info-row {
    background: var(--bg); border-left: 3px solid var(--primary);
    border-radius: 6px; padding: .45rem .8rem;
    font-size: .79rem; color: var(--slate); margin-bottom: .65rem;
  }

  /* Comparison table (Step 3) */
  .cmp-tbl { width: 100%; border-collapse: collapse; font-size: .8rem; }
  .cmp-tbl th {
    background: var(--bg); padding: .36rem .6rem;
    font-weight: 600; border: 1px solid var(--border); color: var(--slate);
  }
  .cmp-tbl td { padding: .28rem .6rem; border: 1px solid var(--border); color: var(--text); }
  .cmp-match   { color: var(--primary) !important; font-weight: 600; }
  .cmp-diff    { color: var(--accent)  !important; font-weight: 600; }
  .cmp-neutral { color: var(--missing) !important; }
  .cmp-info    { color: var(--slate)   !important; font-weight: 600; }

  /* Empty state */
  .empty-st { text-align: center; padding: 3rem 1rem; color: var(--missing); }
  .empty-st .es-icon { font-size: 2.2rem; opacity: .5; display: block; margin-bottom: .8rem; }
  .empty-st p { font-size: .82rem; }

  /* Decision card */
  .dec-card { border-radius: 14px; overflow: hidden; margin-bottom: 1rem; border: 1px solid var(--border); }
  .dec-hdr {
    background: var(--primary); color: #FFFFFF;
    padding: .7rem 1.1rem; font-weight: 600; font-size: .86rem;
  }
  .dec-body { background: var(--card); padding: .9rem 1.1rem; }
  .shiny-input-container .radio label { font-size: .83rem; }
  .form-check-input:checked { background-color: var(--primary); border-color: var(--primary); }
  .form-check-input:focus { box-shadow: 0 0 0 .2rem rgba(27,67,50,.2); border-color: var(--primary); }

  /* Primary action */
  #confirm_decision, .btn-primary {
    background: var(--primary) !important; border-color: var(--primary) !important; color: #FFFFFF !important;
  }
  #confirm_decision:hover, .btn-primary:hover { filter: brightness(1.15); }

  /* Status alerts: soft tints, not solid fills */
  .alert-success { background: var(--brand-soft) !important; color: var(--primary) !important; border: 1px solid var(--primary) !important; }
  .alert-warning, .alert-danger { background: var(--accent-soft) !important; color: var(--accent) !important; border: 1px solid var(--accent) !important; }
  .alert-info, .alert-secondary { background: var(--bg) !important; color: var(--slate) !important; border: 1px solid var(--border) !important; }

  /* Progress bar (Step 1 categorized vs uncategorized) */
  .progress { background: var(--border); }
  .pb-brand  { background: var(--primary); }
  .pb-accent { background: var(--accent); }
  .txt-brand  { color: var(--primary); }
  .txt-accent { color: var(--accent); }

  /* DT table */
  .dataTables_wrapper, .dt-container { font-size: .8rem; color: var(--text); }
  .dataTables_filter input, .dt-search input {
    border-radius: 8px; border: 1px solid var(--border); font-size: .79rem;
  }
  table.dataTable thead th { font-weight: 600; font-size: .79rem; color: var(--slate); border-bottom-color: var(--border) !important; }
  table.dataTable tbody td { border-color: var(--border) !important; }

  /* No zebra striping, no sorted-column shading: every unselected row is plain
     white with a hairline bottom border, and a faint tint on hover. DataTables 2
     and Bootstrap 5 paint stripes/hover with inset box-shadows (tr:nth-child(odd),
     .odd, .sorting_1, --bs-table-bg-type), so neutralise background and shadow. */
  table.dataTable > tbody > tr:not(.selected),
  table.dataTable > tbody > tr:not(.selected) > * {
    background-color: var(--card) !important;
    box-shadow: none !important;
    --bs-table-bg-type: transparent; --bs-table-bg-state: transparent; --bs-table-accent-bg: transparent;
  }
  table.dataTable > tbody > tr > * {
    border-top: 0 !important;
    border-bottom: 1px solid var(--border) !important;
  }
  table.dataTable > tbody > tr:not(.selected):hover > * {
    background-color: var(--grid) !important;   /* #F1F5F9 */
    box-shadow: none !important;
  }
  /* Clickable tables: Step 1 queue, Step 2 candidates, Data Quality encoding table */
  #queue_table tbody td:first-child { white-space: nowrap; }   /* keep IDs on one line: shorter rows */
  #queue_table tbody td, #txt_cand tbody td, #num_cand tbody td { padding: 6px 8px !important; }
  #queue_table tbody tr, #txt_cand tbody tr, #num_cand tbody tr,
  #encoding_issues_table tbody tr { cursor: pointer; }

  /* One selection style everywhere (Step 1, Step 2, Data Quality, Step 4 cards):
     #ECFDF5 tint + 2px #1B4332 outline. DataTables 2 paints rows with inset
     box-shadows, so the outline is drawn the same way: top/bottom on every
     cell, plus left/right on the first/last cell. */
  table.dataTable tbody tr.selected > * {
    background-color: var(--brand-soft) !important;
    box-shadow: inset 0 2px 0 var(--primary), inset 0 -2px 0 var(--primary),
                inset 0 0 0 9999px var(--brand-soft) !important;
    color: var(--text) !important; border-color: var(--border) !important; font-weight: 500;
  }
  table.dataTable tbody tr.selected > *:first-child {
    box-shadow: inset 2px 0 0 var(--primary), inset 0 2px 0 var(--primary), inset 0 -2px 0 var(--primary),
                inset 0 0 0 9999px var(--brand-soft) !important;
  }
  table.dataTable tbody tr.selected > *:last-child {
    box-shadow: inset -2px 0 0 var(--primary), inset 0 2px 0 var(--primary), inset 0 -2px 0 var(--primary),
                inset 0 0 0 9999px var(--brand-soft) !important;
  }
  table.dataTable tbody tr.selected a { color: var(--primary) !important; }

  .queue-toggle { font-size: .74rem; color: var(--slate) !important; text-decoration: none;
                  border: 1px solid var(--border); border-radius: 999px; padding: .05rem .55rem; margin-right: .4rem; }
  .queue-toggle:hover { color: var(--primary) !important; border-color: var(--primary); }


  /* ---- Dashboard ---- */
  @keyframes dashUp { from { opacity: 0; transform: translateY(14px); } to { opacity: 1; transform: none; } }
  .dash-anim { animation: dashUp .55s cubic-bezier(.2,.7,.2,1) both; animation-delay: var(--d, 0s); }
  .dash-hero { display: flex; align-items: flex-end; justify-content: space-between; gap: 1rem; flex-wrap: wrap;
               margin: .25rem 0 1.1rem; }
  .dash-title { font-family: 'Fraunces', Georgia, serif; font-size: 2.3rem; font-weight: 600; line-height: 1.05;
                color: var(--primary); margin: 0 0 .35rem; letter-spacing: -.1px; word-spacing: .08em;
                font-variation-settings: 'opsz' 144, 'SOFT' 50; }
  .dash-sub { font-size: .86rem; color: var(--muted); margin: 0; }
  .dash-actions { display: flex; gap: .5rem; flex-wrap: wrap; }
  .btn-dash-primary { background: var(--primary) !important; color: #FFFFFF !important; border: none !important;
                      border-radius: 10px; font-weight: 600; font-size: .84rem; padding: .5rem 1rem;
                      display: inline-flex; flex-direction: row-reverse; gap: .5rem; align-items: center; }
  .btn-dash-primary:hover { filter: brightness(1.15); }
  .btn-dash-primary .fa-arrow-right { transition: transform .2s; }
  .btn-dash-primary:hover .fa-arrow-right { transform: translateX(3px); }

  .dash-kpis { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 1rem; margin-bottom: 1rem; }
  @media (max-width: 1100px) { .dash-kpis { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
  .dash-kpi { background: var(--card); border: 1px solid var(--border); border-radius: 14px; padding: 1rem 1.1rem;
              display: flex; gap: .9rem; align-items: center; transition: transform .2s, box-shadow .2s; }
  .dash-kpi:hover { transform: translateY(-2px); box-shadow: 0 8px 22px rgba(15, 23, 42, .07); }
  .dash-kpi-icon { width: 46px; height: 46px; border-radius: 12px; display: flex; align-items: center;
                   justify-content: center; font-size: 1rem; flex-shrink: 0; }
  /* KPI tone: a thin coloured top rule instead of icon tiles */
  .dash-kpi, .kpi-card { position: relative; overflow: hidden; }
  .tone-alert::before, .tone-neutral::before, .tone-brand::before {
    content: ''; position: absolute; left: 0; right: 0; top: 0; height: 3px; }
  .tone-alert::before   { background: var(--accent); }
  .tone-neutral::before { background: var(--missing); }
  .tone-brand::before   { background: var(--primary); }
  .dash-kpi-num { font-size: 1.7rem; font-weight: 700; line-height: 1.05; color: var(--text); font-variant-numeric: tabular-nums; }
  .dash-kpi-lbl { font-size: .76rem; font-weight: 600; color: var(--text); margin-top: 2px; }
  .dash-kpi-sub { font-size: .7rem; color: var(--muted); }

  /* Progress rings */
  @keyframes ringIn { from { stroke-dashoffset: var(--len); } }
  .ring-row { display: flex; justify-content: space-around; gap: .5rem; margin: .2rem 0 .6rem; }
  .ring { text-align: center; }
  .ring svg { display: block; margin: 0 auto; }
  .ring .arc { animation: ringIn 1.3s cubic-bezier(.2,.7,.2,1) .35s both; transition: stroke-dashoffset .8s ease; }
  .ring-pct { font-size: 1.25rem; font-weight: 700; fill: var(--text); }
  .ring-cap { font-size: .64rem; fill: var(--muted); }
  .ring-lbl { font-size: .78rem; font-weight: 600; color: var(--text); margin-top: .25rem; }
  .ring-sub { font-size: .7rem; color: var(--muted); }
  .next-up { display: flex; align-items: center; justify-content: space-between; gap: .75rem;
             border: 1px solid var(--border); border-radius: 10px; padding: .55rem .75rem; background: var(--grid); }
  .next-up .lbl { font-size: .66rem; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; color: var(--muted); }
  .next-up .val { font-size: .84rem; font-weight: 600; color: var(--text); }

  /* Recent decisions list */
  .recent-list { list-style: none; margin: 0; padding: 0; }
  .recent-list li { display: flex; align-items: center; gap: .65rem; padding: .45rem 0; border-bottom: 1px solid var(--border);
                    font-size: .8rem; animation: dashUp .4s ease both; }
  .recent-list li:last-child { border-bottom: none; }
  .recent-list .dot { width: 9px; height: 9px; border-radius: 50%; flex-shrink: 0; }
  .recent-list .t { color: var(--muted); font-size: .72rem; margin-left: auto; font-variant-numeric: tabular-nums; }
  .dash-empty { display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;
                height: 100%; min-height: 180px; color: var(--muted); font-size: .82rem; gap: .35rem; }
  .dash-empty i { font-size: 1.4rem; color: var(--bar-muted); }

  /* Every card header (all tabs) shares Step 4's filled green header */
  .pg-hdr { background: var(--primary); color: #FFFFFF; border-bottom: none; }
  .pg-hdr .text-muted { color: rgba(255,255,255,.75) !important; }
  .pg-hdr .q-badge { background: rgba(255,255,255,.12); color: #FFFFFF; border-color: rgba(255,255,255,.35); }
  .pg-hdr .lbl-ok { background: #FFFFFF; }
  .pg-hdr .btn-outline-primary { color: #FFFFFF !important; border-color: rgba(255,255,255,.5) !important; background: transparent !important; }
  .pg-hdr .btn-outline-primary:hover { background: #FFFFFF !important; color: var(--primary) !important; border-color: #FFFFFF !important; }
  .step-card .queue-toggle { color: #FFFFFF !important; border-color: rgba(255,255,255,.5); }
  .step-card .queue-toggle:hover { background: #FFFFFF; color: var(--primary) !important; border-color: #FFFFFF; }

  /* DT pagination — white/grey by default, brand only on the active page.
     Covers DataTables 1 (bootstrap5 + default) and DataTables 2 markup. */
  .dataTables_paginate .page-link,
  .dataTables_paginate .paginate_button,
  .dt-paging .page-link,
  .dt-paging .dt-paging-button {
    background: var(--card) !important; color: var(--slate) !important;
    border: 1px solid var(--border) !important; box-shadow: none !important;
  }
  .dataTables_paginate .page-item.active .page-link,
  .dataTables_paginate .paginate_button.current,
  .dt-paging .page-item.active .page-link,
  .dt-paging .dt-paging-button.current {
    background: var(--primary) !important; color: #FFFFFF !important;
    border-color: var(--primary) !important;
  }
  .dataTables_paginate .page-item.disabled .page-link,
  .dataTables_paginate .paginate_button.disabled,
  .dt-paging .page-item.disabled .page-link,
  .dt-paging .dt-paging-button.disabled { color: var(--missing) !important; }

  /* Range slider (ionRangeSlider, shiny skin) */
  .irs--shiny .irs-bar { background: var(--primary); border-color: var(--primary); }
  .irs--shiny .irs-line { background: var(--border); border-color: var(--border); }
  .irs--shiny .irs-handle { border-color: var(--primary); background: var(--card); box-shadow: none; }
  .irs--shiny .irs-handle:hover, .irs--shiny .irs-handle.state_hover { background: var(--brand-soft); }
  .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--primary); }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background: var(--bg); color: var(--muted); }
  .irs-min, .irs-max, .irs-from, .irs-to, .irs-single { font-size: .72rem; }

  /* Step 3: keyword flags + nutrition block */
  .kw-hit { font-weight: 700; color: inherit; }
  .kw-pill {
    display: inline-block; border-radius: 999px; padding: 1px 8px; margin: 1px 3px 1px 0;
    font-size: .7rem; font-weight: 600; white-space: nowrap;
  }
  .kw-same { background: var(--brand-soft); color: var(--primary); border: 1px solid var(--brand-soft); }
  .kw-diff { background: var(--accent-soft); color: var(--accent); border: 1px solid var(--accent); }
  .cmp-tbl tr.cmp-section td {
    background: var(--bg); color: var(--slate); font-size: .7rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: .06em; padding-top: .45rem;
  }
  .cmp-src { font-size: .7rem; color: var(--muted); font-weight: 400; text-transform: none; letter-spacing: 0; }
  .score-strip .pg-body { padding: .55rem 1.1rem .35rem; }

  /* Step 3 nutrition: compact 2-column grid instead of 7 stacked rows */
  .nut-block { padding: .45rem .6rem .5rem !important; background: var(--bg); }
  .nut-head { display: flex; justify-content: space-between; gap: .5rem; flex-wrap: wrap;
              font-size: .7rem; color: var(--muted); margin-bottom: .3rem; }
  .nut-head b { color: var(--slate); text-transform: uppercase; letter-spacing: .06em; }
  .nut-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: 1.2rem; row-gap: 2px; }
  .nut-item { display: grid; grid-template-columns: 1fr auto auto; gap: .5rem; align-items: baseline;
              font-size: .78rem; padding: 2px 0; border-bottom: 1px solid var(--border); }
  .nut-item .nut-lbl { color: var(--slate); }
  .nut-item .nut-lbl small { color: var(--muted); }
  .nut-item .nut-p { color: var(--text); min-width: 3.2em; text-align: right; }
  .nut-item .nut-c { min-width: 3.6em; text-align: right; }
  .nut-item.has-d { grid-template-columns: 1fr auto auto 3.8em; }
  .nut-item .nut-d { text-align: right; font-size: .68rem; }
  @media (max-width: 575px) { .nut-grid { grid-template-columns: 1fr; } }

  /* Step 4 manual nutrition entry (Open Food Facts fallback) */
  .man-grid { display: grid; grid-template-columns: repeat(6, 1fr); gap: .35rem .5rem; margin-top: .45rem; }
  @media (max-width: 1199px) { .man-grid { grid-template-columns: repeat(3, 1fr); } }
  .off-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: .15rem .8rem; font-size: .78rem; }
  @media (max-width: 575px) { .off-grid { grid-template-columns: repeat(2, 1fr); } }
  .man-grid .form-group, .man-grid .shiny-input-container { margin-bottom: 0 !important; width: 100% !important; }
  .man-grid label { font-size: .7rem; color: var(--slate); margin-bottom: 1px; white-space: nowrap; }
  .man-grid input { font-size: .8rem; padding: .2rem .45rem; height: auto; }
  @media (max-width: 575px) { .man-grid { grid-template-columns: repeat(2, 1fr); } }
  .score-strip .ss-lbl { font-size: .72rem; color: var(--muted); }

  /* Step 4: shortcuts + Open Food Facts preview */
  .kbd-hint { font-size: .72rem; color: var(--muted); margin-left: .6rem; }
  kbd {
    background: var(--bg); color: var(--slate); border: 1px solid var(--border);
    border-bottom-width: 2px; border-radius: 4px; padding: 0 5px; font-size: .68rem;
    font-family: inherit; box-shadow: none;
  }
  .new-panel { border: 1px dashed var(--border); border-radius: 10px; padding: .7rem .8rem; margin: .4rem 0 .7rem; }
  .txt-primary { color: var(--primary); font-weight: 600; }
  /* Compact drawer under the decision cards */
  .new-panel { padding: .55rem .7rem !important; margin: .3rem 0 .55rem !important; }
  .new-panel .control-label, .new-panel label { font-size: .74rem; color: var(--slate); margin-bottom: 2px; }
  .new-panel .form-group, .new-panel .shiny-input-container { margin-bottom: .35rem !important; }
  .new-panel .form-control, .new-panel .selectize-input { font-size: .82rem; min-height: 0; padding-top: .3rem; padding-bottom: .3rem; }
  .new-panel .btn.mb-3 { margin-bottom: .35rem !important; }

  /* Step 4 decision cards (the radio group itself) */
  .dcard-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: .6rem;
                width: 100% !important; margin-bottom: .35rem !important; }
  @media (max-width: 767px) { .dcard-grid { grid-template-columns: 1fr; } }
  .dcard { position: relative; display: flex; flex-direction: column; gap: .15rem; cursor: pointer;
           background: var(--card); border: 2px solid var(--border); border-radius: 12px;
           padding: .7rem .8rem .65rem; margin: 0; transition: background .12s, border-color .12s; }
  .dcard:hover { border-color: var(--missing); background: var(--grid); }
  .dcard:has(.dcard-input:checked) { background: var(--brand-soft); border-color: var(--primary); }
  .dcard:has(.dcard-input:focus-visible) { outline: 2px solid var(--primary); outline-offset: 2px; }
  .dcard-input { position: absolute; opacity: 0; pointer-events: none; width: 1px; height: 1px; }
  .dcard-title { font-weight: 700; font-size: .86rem; color: var(--text); }
  .dcard-sub { font-size: .72rem; color: var(--muted); line-height: 1.3; }
  .dcard-top { display: flex; align-items: center; min-height: 24px; }
  .dcard-sug { font-size: .62rem; font-weight: 700; letter-spacing: .03em;
               text-transform: uppercase; color: var(--primary); background: var(--brand-soft);
               border: 1px solid var(--primary); border-radius: 999px; padding: 0 .45rem; line-height: 1.5; }
  .dcard-why { font-size: .72rem; color: var(--muted); margin-bottom: .2rem; }
  .confirm-row { display: flex; align-items: center; flex-wrap: wrap; gap: .4rem; margin-top: .2rem; }
  .btn-confirm { font-weight: 600; padding: .5rem 1.4rem; font-size: .9rem; border-radius: 10px;
                 box-shadow: 0 2px 6px rgba(27, 67, 50, .25); }
  .off-card { border: 1px solid var(--border); border-radius: 10px; padding: .6rem .8rem; margin-top: .6rem; font-size: .8rem; }
  .off-card .off-title { font-weight: 600; color: var(--text); }
  .btn-outline-primary { color: var(--primary) !important; border-color: var(--primary) !important; background: var(--card) !important; }
  .btn-outline-primary:hover { background: var(--brand-soft) !important; }

  /* Navbar tour button */
  .btn-tour {
    font-size: .78rem; font-weight: 600; color: var(--slate) !important;
    background: var(--card) !important; border: 1px solid var(--border) !important; border-radius: 999px;
    padding: .25rem .8rem;
  }
  .btn-tour:hover { color: var(--primary) !important; border-color: var(--primary) !important; }
  .navbar .btn-tour { display: inline-flex; align-items: center; gap: .45rem; color: #FFFFFF !important;
                      background: transparent !important; border-color: rgba(255,255,255,.35) !important; padding: .4rem .9rem; }
  .navbar .btn-tour:hover { background: #FFFFFF !important; color: var(--primary) !important; border-color: #FFFFFF !important; }
  .driver-popover-title, .driver-popover-description { color: var(--text) !important; font-family: 'Inter', sans-serif !important; }
  .driver-popover-footer button, #driver-popover-item .driver-popover-footer button {
    border-radius: 6px !important; text-shadow: none !important; font-family: 'Inter', sans-serif !important;
    background: var(--card) !important; color: var(--slate) !important; border: 1px solid var(--border) !important;
  }
  #driver-popover-item .driver-popover-footer .driver-next-btn,
  .driver-popover-next-btn { background: var(--primary) !important; color: #FFFFFF !important; border-color: var(--primary) !important; }

  /* Data Quality charts are clickable: drill into the Validation queue */
  #missing_heatmap .nsewdrag, #queue_bar .nsewdrag { cursor: pointer !important; }
  .filter-chip {
    display: inline-flex; align-items: center; gap: .4rem;
    background: var(--accent-soft); color: var(--accent); border: 1px solid var(--accent);
    border-radius: 999px; padding: .15rem .65rem; font-size: .76rem; font-weight: 600;
  }
  .filter-chip a { color: var(--accent); text-decoration: none; font-weight: 700; }
")))

# -- 3. UI --------------------------------------------------------------------
ui <- page_navbar(
  id     = "main_nav",
  selected = "Dashboard",
  title  = span(class = "brand-mark", icon("leaf")),
  window_title = "Food Validator",
  # Not fillable: in a fillable page each tab is a flex column that squeezes rows
  # to the window height, so fixed-height plots spilled onto the cards below.
  fillable = FALSE,
  theme  = app_theme,
  header = tagList(
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght,SOFT@9..144,500..700,50&display=swap"),
    app_css,
    use_cicerone(),
    tags$script(HTML("
      // Step 4 keyboard shortcuts: 1/2/3 choose a decision, Enter confirms.
      // Ignored while typing in a field, inside a modal, or off the Validation tab.
      document.addEventListener('keydown', function (e) {
        if (e.altKey || e.ctrlKey || e.metaKey) return;
        var t = e.target;
        if (t.closest && t.closest('input:not([type=radio]), textarea, select, button, a, [contenteditable], .modal')) return;
        if (document.querySelector('.modal.show, .driver-popover')) return;
        var tab = document.querySelector('#main_nav .nav-link.active');
        if (!tab || tab.getAttribute('data-value') !== 'Validation') return;
        var radios = document.querySelectorAll('#disposition input[type=radio]');
        if (!radios.length) return;
        if (['1', '2', '3'].indexOf(e.key) >= 0) {
          var r = radios[+e.key - 1];
          if (r) { r.click(); r.focus(); e.preventDefault(); }
        } else if (e.key === 'Enter') {
          var b = document.getElementById('confirm_decision');
          if (b) { b.click(); e.preventDefault(); }
        }
      });
      // Progressive disclosure: full-width queue <-> split workspace.
      window.valSplit = { on: false, extra: [] };
      function applyQueueMode() {
        var el = $('#queue_table table.dataTable');
        if (!el.length || !$.fn.dataTable.isDataTable(el)) return;
        var t = el.DataTable(), s = window.valSplit, n = t.columns().count();
        s.extra.forEach(function (i) { if (i < n) t.column(i).visible(!s.on, false); });
        var len = s.on ? 8 : 10;
        if (t.page.len() !== len) t.page.len(len);
        // Keep the selected row on screen. DT renders server-side, so rows only
        // exist for the current page: absolute position = page start + local index.
        var sel = t.row('.selected');
        if (sel.any()) {
          var local = t.rows({ page: 'current' }).nodes().toArray().indexOf(sel.node());
          if (local >= 0) t.page(Math.floor((t.page.info().start + local) / len));
        }
        t.columns.adjust().draw(false);
      }
      Shiny.addCustomMessageHandler('valSplit', function (m) {
        window.valSplit = { on: !!m.on, extra: m.extra || [] };
        var g = document.getElementById('val_grid');
        if (g) g.classList.toggle('split', !!m.on);
        applyQueueMode();
        // after the 0.25s transition: let plotly / DT re-measure their widths
        setTimeout(function () { window.dispatchEvent(new Event('resize')); applyQueueMode(); }, 300);
      });
      $(document).on('init.dt', function (e, settings) {
        if ($(settings.nTable).closest('#queue_table').length) setTimeout(applyQueueMode, 0);
      });

      // Dashboard: KPI numbers count up from their current value
      Shiny.addCustomMessageHandler('countUp', function (items) {
        items.forEach(function (m) {
          var el = document.getElementById(m.id); if (!el) return;
          var from = +(el.getAttribute('data-v') || 0), to = +m.value, t0 = null, dur = 1100;
          el.setAttribute('data-v', to);
          function step(ts) {
            if (!t0) t0 = ts;
            var k = Math.min(1, (ts - t0) / dur), e = 1 - Math.pow(1 - k, 3);
            el.textContent = Math.round(from + (to - from) * e).toLocaleString('en-US');
            if (k < 1) requestAnimationFrame(step);
          }
          requestAnimationFrame(step);
        });
      });
      // Dashboard: plotly entrance — bars grow from 0, lines draw in (first render only)
      window.dashAnimate = function (gd, delay) {
        if (gd.__dashAnimated) return; gd.__dashAnimated = true;
        var bars = [], keys = [], finals = [];
        gd.data.forEach(function (t, i) {
          if (t.type === 'bar') {
            var k = t.orientation === 'h' ? 'x' : 'y';
            bars.push(i); keys.push(k); finals.push(Array.from(t[k]));
          }
        });
        bars.forEach(function (i, j) {
          var u = {}; u[keys[j]] = [finals[j].map(function () { return 0; })];
          Plotly.restyle(gd, u, [i]);
        });
        var lines = gd.querySelectorAll('.scatterlayer path.js-line');
        lines.forEach(function (p) {
          var L = p.getTotalLength ? p.getTotalLength() : 0;
          p.style.strokeDasharray = L + ' ' + L; p.style.strokeDashoffset = L;
        });
        var pts = gd.querySelectorAll('.scatterlayer .points path, .annotation');
        pts.forEach(function (p) { p.style.opacity = 0; });
        setTimeout(function () {
          if (bars.length) Plotly.animate(gd, {
            data: bars.map(function (i, j) { var d = {}; d[keys[j]] = finals[j]; return d; }), traces: bars
          }, { transition: { duration: 900, easing: 'cubic-out' }, frame: { duration: 900, redraw: false } });
          lines.forEach(function (p) {
            p.style.transition = 'stroke-dashoffset 1.4s cubic-bezier(.2,.7,.2,1)'; p.style.strokeDashoffset = 0;
          });
          setTimeout(function () {
            pts.forEach(function (p) { p.style.transition = 'opacity .5s'; p.style.opacity = 1; });
            lines.forEach(function (p) { p.style.strokeDasharray = ''; });
          }, 1100);
        }, delay || 0);
      };

      // Guided tour: wait for the workspace to render before starting
      Shiny.addCustomMessageHandler('tourDelay', function (ms) {
        setTimeout(function () { Shiny.setInputValue('tour_ready', Date.now()); }, ms);
      });
    "))
  ),

  # ---- TAB 1: Validation Workflow (S1-S5) ------------------------------------
  nav_item(span(class = "nav-section", "Workspace")),

  # ---- TAB 0: Dashboard (landing page) ----------------------------------------
  nav_panel(
    title = "Dashboard", icon = icon("gauge-high"),

    div(class = "dash-hero dash-anim",
      div(
        h1(class = "dash-title", "Food Validator"),
        p(class = "dash-sub", "Where the pending queue stands, how the matcher is tuned, and what still needs attention.")
      ),
      div(class = "dash-actions",
        actionButton("dash_go_validate", "Start validating", icon = icon("arrow-right"), class = "btn-dash-primary")
      )
    ),

    # KPI tiles (numbers count up)
    div(class = "dash-kpis",
      div(class = "dash-kpi tone-alert dash-anim", style = "--d:.05s",
        div(div(class = "dash-kpi-num", span(id = "dk_pending", "0")),
            div(class = "dash-kpi-lbl", "Pending review"),
            div(class = "dash-kpi-sub", textOutput("dk_pending_sub", inline = TRUE)))
      ),
      div(class = "dash-kpi tone-neutral dash-anim", style = "--d:.12s",
        div(div(class = "dash-kpi-num", span(id = "dk_ref", "0")),
            div(class = "dash-kpi-lbl", "Validated reference"),
            div(class = "dash-kpi-sub", textOutput("dk_ref_sub", inline = TRUE)))
      ),
      div(class = "dash-kpi tone-brand dash-anim", style = "--d:.19s",
        div(div(class = "dash-kpi-num", span(id = "dk_done", "0")),
            div(class = "dash-kpi-lbl", "Decisions this session"),
            div(class = "dash-kpi-sub", textOutput("dk_done_sub", inline = TRUE)))
      ),
      div(class = "dash-kpi tone-alert dash-anim", style = "--d:.26s",
        div(div(class = "dash-kpi-num", span(id = "dk_issues", "0")),
            div(class = "dash-kpi-lbl", "Data issues in queue"),
            div(class = "dash-kpi-sub", textOutput("dk_issues_sub", inline = TRUE)))
      )
    ),

    layout_columns(col_widths = c(7, 5), gap = "1rem",
      div(class = "pg-card dash-anim", style = "--d:.3s",
        div(class = "pg-hdr", span("Pending queue by category"), span(class = "q-badge", "Top 10")),
        div(class = "pg-body", plotlyOutput("dash_cat", height = "262px"))
      ),
      div(class = "pg-card dash-anim", style = "--d:.36s",
        div(class = "pg-hdr", span("Review progress"), span(class = "q-badge", "This session")),
        div(class = "pg-body", uiOutput("dash_progress"))
      )
    ),

    layout_columns(col_widths = c(6, 6), gap = "1rem",
      div(class = "pg-card dash-anim", style = "--d:.42s",
        div(class = "pg-hdr", span("Threshold trade-off"), span(class = "q-badge", textOutput("dash_thr_badge", inline = TRUE))),
        div(class = "pg-body", plotlyOutput("dash_thr", height = "260px"))
      ),
      div(class = "pg-card dash-anim", style = "--d:.48s",
        div(class = "pg-hdr", span("Completeness of pending records"), span(class = "q-badge", "% filled")),
        div(class = "pg-body", plotlyOutput("dash_complete", height = "260px"))
      )
    ),

    div(class = "pg-card dash-anim", style = "--d:.54s",
      div(class = "pg-hdr", span("Recent decisions"), span(class = "q-badge", "Live")),
      div(class = "pg-body",
        layout_columns(col_widths = c(5, 7), gap = "1.25rem",
          plotlyOutput("dash_mix", height = "200px"),
          uiOutput("dash_recent")
        )
      )
    )
  ),
  nav_panel(
    title = "Validation", icon = icon("list-check"),

    # Top filter bar (W1-W4) — slider now in integer % (40-95)
    div(class = "filter-bar", id = "filter_bar",

      div(class = "fb-group",
        span(class = "fb-label", "Dataset"),
        selectInput("dataset", NULL,
          choices  = c("Text / Names" = "text", "Nutritional values" = "numeric"),
          selected = "text", width = "160px"
        )
      ),

      div(class = "fb-group",
        span(class = "fb-label", "Category"),
        selectInput("category", NULL,
          choices  = c("All categories" = "all"),
          selected = "all", width = "170px"
        )
      ),

      div(class = "fb-group",
        span(class = "fb-label", "Sort queue by"),
        selectInput("sort_order", NULL,
          choices  = c("Default order" = "default",
                       "Name A to Z"   = "name",
                       "By category"   = "category"),
          selected = "default", width = "150px"
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
        div(class = "kpi-card tone-alert py-2 px-3",
          div(div(class = "kpi-num sm", textOutput("kpi_pending", inline = TRUE)),
              div(class = "kpi-lbl", "Pending"))
        ),
        div(class = "kpi-card tone-neutral py-2 px-3",
          div(div(class = "kpi-num sm", textOutput("kpi_ref", inline = TRUE)),
              div(class = "kpi-lbl", "Reference"))
        ),
        div(class = "kpi-card tone-brand py-2 px-3",
          div(div(class = "kpi-num sm", textOutput("kpi_done", inline = TRUE)),
              div(class = "kpi-lbl", "Decisions"))
        )
      )
    ),

    tags$script("
      Shiny.addCustomMessageHandler('setThrLbl', function(v) {
        document.getElementById('thr_lbl').textContent = v + '%';
      });
    "),

    # Master-detail layout with progressive disclosure: the queue is full width
    # until a product is picked, then animates to 5/12 and the workspace (7/12)
    # fades in. The .split class is toggled from the server (valSplit message).
    div(id = "val_grid", class = "val-grid",

      # LEFT — Queue (Q1 · S1)
      div(class = "val-left",
        div(class = "pg-card step-card",
          div(class = "pg-hdr",
            span("Step 1 — Select a pending product"),
            span(uiOutput("queue_toggle", inline = TRUE), span(class = "q-badge", "Q1"))
          ),
          div(class = "pg-body",
            uiOutput("q1_summary"),
            DTOutput("queue_table")
          )
        ),
        # Step 2 + score bar, under the queue once a product is selected
        div(class = "val-reveal", uiOutput("workspace_left"))
      ),

      # RIGHT — banner, Step 3, Step 4 (hidden until a product is selected)
      div(class = "val-right", uiOutput("workspace"))
    )
  ),

  # ---- TAB 2: Threshold Tuner (S7 S8) ----------------------------------------
  nav_panel(
    title = "Threshold Tuner", icon = icon("sliders"),
    layout_columns(
      col_widths = c(4, 8), gap = "1rem",

      div(
        div(class = "pg-card",
          div(class = "pg-hdr", "Current settings", span(class = "q-badge", "Q7")),
          div(class = "pg-body", id = "tuner_settings",
            div(class = "thr-row d-flex justify-content-between",
              span(class = "fb-label text-uppercase small fw-bold text-muted", "Match threshold"),
              span(class = "small text-muted", "synced with the Validation tab")
            ),
            sliderInput("threshold_tuner", NULL, min = 40, max = 95, value = 70, step = 5,
                        ticks = FALSE, width = "100%", post = "%"),
            uiOutput("thr_kpis"),
            div(class = "card-note",
              "Drag the slider above (or the one on the Validation tab) to update the chart and KPIs live.",
              br(),
              "No threshold change validates a product automatically."
            )
          )
        )
      ),

      div(
        div(class = "pg-card", id = "curve_card",
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
    title = "Data Quality", icon = icon("chart-column"),

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
            "Darker terracotta = more values missing. These fields are excluded from kNN scoring.",
            strong("Click a cell to open the pending items missing that field.")
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
            "Uncategorized products lose the 15% category bonus in text matching.",
            strong("Click a bar to open that category in the queue.")
          ),
          plotlyOutput("queue_bar", height = "300px")
        )
      )
    ),

    # Row 2: Encoding issues (full width), cleanly below the charts
    div(class = "pg-card", style = "margin-top: 20px;",
      div(class = "pg-hdr",
        span("UTF-8 encoding issues — pending text products"),
        actionButton("batch_fix_btn", "Batch fix\u2026", icon = icon("wand-magic-sparkles"),
                     class = "btn btn-sm btn-outline-primary py-0")
      ),
      div(class = "pg-body",
        p(class = "small text-muted mb-2",
          "Names stored with broken encoding, e.g. ", tags$code("Ã©"), " for é, ",
          tags$code("â€œ"), " for \u201c, or ", tags$code("ÐœÐ°Ð¹"), " for Cyrillic \u041c\u0430\u0439. ",
          "They distort similarity scores \u2014 use ", strong("Batch fix"), " to preview and apply repairs."
        ),
        DTOutput("encoding_issues_table")
      )
    )
  ),

  # ---- Navbar right: guided tour ----------------------------------------------
  nav_spacer(),
  nav_item(actionButton("tour_btn", "Guided tour", icon = icon("circle-question"), class = "btn-tour"))
)

# -- 4. SERVER -----------------------------------------------------------------
server <- function(input, output, session) {

  # Text data lives in a reactiveVal so UTF-8 repairs (Data Quality -> Batch fix)
  # flow through to the queue, candidate scoring and charts without a restart.
  text_data <- reactiveVal(df_text)
  thr_eval_r <- reactiveVal(thr_eval)

  # Sync threshold label (input is now integer %, e.g. 70)
  observeEvent(input$threshold, {
    session$sendCustomMessage("setThrLbl", input$threshold)
    if (!identical(input$threshold_tuner, input$threshold))
      updateSliderInput(session, "threshold_tuner", value = input$threshold)
  })
  # Threshold Tuner has its own slider; keep both in lock-step
  observeEvent(input$threshold_tuner, {
    if (!identical(input$threshold_tuner, input$threshold))
      updateSliderInput(session, "threshold", value = input$threshold_tuner)
  }, ignoreInit = TRUE)

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
    category  = character(), note = character(), off_code = character(), nutrition = character(),
    stringsAsFactors = FALSE
  ))

  # Drill-down filter set from the Missing-fields heatmap (field name or NULL)
  missing_filter <- reactiveVal(NULL)

  observeEvent(input$dataset, { sel_product(NULL); sel_candidate(NULL); missing_filter(NULL) },
               ignoreInit = TRUE)   # don't wipe the start-up pre-selection
  observeEvent(input$clear_missing_filter, missing_filter(NULL))

  # Data Quality -> Validation drill-down: category bar click
  observeEvent(event_data("plotly_click", source = "cat_bar"), {
    cat_clicked <- event_data("plotly_click", source = "cat_bar")$customdata[[1]]
    req(cat_clicked)
    missing_filter(NULL)
    updateSelectInput(session, "category", selected = cat_clicked)
    sel_product(NULL); sel_candidate(NULL)
    nav_select("main_nav", "Validation")
    showNotification(paste0("Queue filtered to category: ", cat_clicked), type = "message", duration = 4)
  })

  # Data Quality -> Validation drill-down: heatmap cell click
  observeEvent(event_data("plotly_click", source = "miss_heat"), {
    field <- event_data("plotly_click", source = "miss_heat")$x[[1]]
    req(field)
    df <- if (input$dataset == "text") text_data() else df_num
    n  <- sum(df$active == 0 & is.na(df[[field]]))
    if (n == 0) {
      showNotification(paste0("No pending items are missing '", field, "'."), type = "warning", duration = 4)
      return()
    }
    missing_filter(field)
    updateSelectInput(session, "category", selected = "all")  # match the heatmap's whole-queue counts
    sel_product(NULL); sel_candidate(NULL)
    nav_select("main_nav", "Validation")
    showNotification(paste0("Queue filtered to ", n, " pending items missing '", field, "'."),
                     type = "message", duration = 4)
  })

  # C1 — filtered_pending() [Q1 · S1]
  filtered_pending <- reactive({
    df  <- if (input$dataset == "text") text_data() else df_num
    res <- df %>% filter(active == 0)

    mf <- missing_filter()
    if (!is.null(mf) && mf %in% colnames(res)) res <- res[is.na(res[[mf]]), ]

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
    df <- if (input$dataset == "text") text_data() else df_num
    format(sum(df$active == 1), big.mark = ",")
  })
  output$kpi_done <- renderText(nrow(log_data()))

  # O1 — queue_bar [Q1] — all categories shown (now lives in Data Quality tab)
  output$queue_bar <- renderPlotly({
    df <- filtered_pending()
    if (nrow(df) == 0) return(plotly_empty())
    total <- nrow(df)
    cc <- df %>%
      group_by(cat_display) %>%
      summarise(n = n(), ids = sample_ids(pid), .groups = "drop") %>%
      arrange(n) %>%
      mutate(
        cat_display = factor(cat_display, levels = cat_display),
        hover = paste0(
          "<b>", cat_display, "</b><br>",
          n, " pending (", round(n / total * 100, 1), "% of queue)<br>",
          hint(paste0("e.g. ", ids)), "<br><i>Click to open in queue</i>"
        )
      )
    # Grey for every category; terracotta only for the one that needs action
    is_uncat <- cc$cat_display == "(No category)"
    plot_ly(cc, x = ~n, y = ~cat_display, type = "bar", orientation = "h",
            source = "cat_bar", customdata = ~as.character(cat_display),
            marker = list(color = ifelse(is_uncat, PAL$accent, PAL$bar_muted)),
            text = ~n, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = ifelse(is_uncat, PAL$accent, PAL$slate), size = 11),
            hovertext = ~hover, hovertemplate = "%{hovertext}<extra></extra>") %>%
      event_register("plotly_click") %>%
      layout(xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE, range = c(0, max(cc$n) * 1.15)),
             yaxis = list(title = "", showgrid = FALSE,
                          tickfont = list(size = 11, color = PAL$slate)),
             margin = list(l = 0, r = 24, t = 8, b = 8)) %>%
      pal_plotly()
  })

  # Compact progress summary for Step 1 (replaces the bar chart there)
  output$q1_summary <- renderUI({
    df    <- filtered_pending()
    total <- nrow(df)
    cat_n <- sum(df$cat_display != "(No category)", na.rm = TRUE)
    uncat <- total - cat_n
    pct   <- if (total > 0) round(cat_n / total * 100) else 0
    mf    <- missing_filter()
    tagList(
      if (!is.null(mf)) div(class = "mb-2",
        span(class = "filter-chip",
          icon("filter"), paste0("Missing: ", mf),
          actionLink("clear_missing_filter", "✕", title = "Clear filter")
        )
      ),
      div(class = "d-flex justify-content-between small mb-1",
        span(class = "text-muted", paste0(total, " pending items in queue")),
        span(
          span(class = "fw-semibold txt-brand", paste0(cat_n, " categorized")),
          span(class = "text-muted mx-1", "\u00b7"),
          span(class = "fw-semibold txt-accent", paste0(uncat, " uncategorized"))
        )
      ),
      div(class = "progress mb-3", style = "height:7px; border-radius:4px;",
        div(class = "progress-bar pb-brand", role = "progressbar",
            style = paste0("width:", pct, "%;")),
        div(class = "progress-bar pb-accent", role = "progressbar",
            style = paste0("width:", 100 - pct, "%;"))
      )
    )
  })

  # Start with the first pending item selected so the workspace is usable at once
  # (the queue table reads this via isolate() and renders with that row selected).
  isolate(tryCatch({
    fp <- filtered_pending()
    if (nrow(fp)) sel_product(fp[1, , drop = FALSE])
  }, error = function(e) message("preselect failed: ", conditionMessage(e))))

  # O2 — queue_table [Q1]
  output$queue_table <- renderDT({
    df <- filtered_pending()
    if (nrow(df) == 0)
      return(datatable(data.frame(Message = "No pending items for this filter.")))
    # Full-width mode shows extra columns (Brand, Barcode / more nutrients);
    # the split view hides them client-side (see valSplit in the page header).
    if (input$dataset == "text") {
      show <- df %>%
        select(pid, name, brands, barcode, cat_display) %>%
        rename(ID = pid, Product = name, Brand = brands, Barcode = barcode, Category = cat_display)
    } else {
      show <- df %>%
        select(pid, cat_display, energy, protein, fat, carbohydrates, sugar, salt) %>%
        rename(ID = pid, Category = cat_display, `kcal` = energy, `Prot g` = protein,
               `Fat g` = fat, `Carbs g` = carbohydrates, `Sugars g` = sugar, `Salt g` = salt)
    }
    cur <- match(isolate(sel_product())$pid %||% NA, df$pid)   # no selection on first load
    datatable(show,
      selection = list(mode = "single", selected = if (is.na(cur)) NULL else cur),
      rownames = FALSE,
      options   = list(pageLength = if (is.na(cur)) 10 else 8, dom = "ftp",
                       language   = list(search = "Filter:"))
    )
  })

  # OB1 — product click -> ST1
  observeEvent(input$queue_table_rows_selected, {
    idx <- input$queue_table_rows_selected
    if (length(idx) > 0) {
      row <- filtered_pending()[idx, , drop = FALSE]
      # Programmatic selections (auto-advance, tour) echo back here — only reset
      # the candidate when the product actually changes.
      if (!identical(row$pid, sel_product()$pid)) {
        sel_product(row)
        sel_candidate(NULL)
      }
      queue_expanded(FALSE)   # picking a row from the expanded queue returns to review
    }
  })

  # C2 — text_candidates() [Q2 · S2 S6]
  # Candidate search as plain functions (also used by the guided tour)
  text_cands_for <- function(prod, t = thr()) {
    val  <- text_data() %>% filter(active == 1)

    bc   <- as.character(prod$barcode %||% "")
    exact <- !is.na(val$barcode) & nzchar(bc) & val$barcode == bc

    score <- score_text(prod, val)
    score[exact] <- 1.00

    val$score       <- score
    val$barcode_hit <- exact
    val %>% filter(score >= t) %>% arrange(desc(score)) %>% head(15)
  }
  num_cands_for <- function(prod, t = thr()) {
    val  <- df_num %>% filter(active == 1)
    if (all(is.na(as.numeric(prod[1, CORE])))) { val$score <- 0; return(val %>% head(10)) }
    val$score <- score_num(prod, val)
    val %>% filter(score >= t) %>% arrange(desc(score)) %>% head(15)
  }
  cands_for <- function(prod) if (input$dataset == "text") text_cands_for(prod) else num_cands_for(prod)

  text_candidates <- reactive({
    # The product must belong to this dataset (outputs can run once with the old
    # product while a dataset switch is clearing it)
    req(input$dataset == "text", sel_product(), "name" %in% names(sel_product()))
    text_cands_for(sel_product())
  })

  # C3 — numeric_candidates() [Q3 · S3]
  numeric_candidates <- reactive({
    req(input$dataset == "numeric", sel_product(), all(CORE %in% names(sel_product())))
    num_cands_for(sel_product())
  })

  # OB2 — candidate click -> ST2
  pick_candidate <- function(cands, idx) {
    if (length(idx) == 0) return()
    row <- cands[idx, , drop = FALSE]
    if (!identical(row$pid, sel_candidate()$pid)) sel_candidate(row)
  }
  observeEvent(input$txt_cand_rows_selected, pick_candidate(text_candidates(), input$txt_cand_rows_selected))
  observeEvent(input$num_cand_rows_selected, pick_candidate(numeric_candidates(), input$num_cand_rows_selected))

  # The workspace re-renders when a candidate is picked, which rebuilds the
  # Step 2 table — restore its selection (and page) so the chosen row stays marked.
  cand_selection <- function(df, page_len = 4) {
    i <- match(sel_candidate()$pid %||% NA, df$pid)
    if (is.na(i)) list(sel = list(mode = "single"), start = 0)
    else list(sel = list(mode = "single", selected = i), start = (ceiling(i / page_len) - 1) * page_len)
  }

  # Workspace (dynamic right panel)
  output$workspace <- renderUI({
    prod <- sel_product()

    # Nothing to show (the column is collapsed) until a product is picked
    if (is.null(prod)) return(NULL)

    cand <- sel_candidate()
    prod_name <- if (input$dataset == "text") prod$name %||% "" else prod$cat_display %||% ""

    tagList(

      # Compact product banner with the way back to the full queue
      div(class = "prod-banner",
        icon("box-archive", class = "prod-icon"),
        span(class = "prod-id", prod$pid),
        span(class = "text-muted", "|"),
        span(class = "prod-name", prod_name),
        span(class = "lbl-pending ms-auto", "unvalidated"),
        actionButton("back_to_queue", HTML("&times; Back to full queue"), class = "btn-back",
                     title = "Clear the selection and show the full queue")
      ),

      # Step 3 — Merged side-by-side comparison (pending LEFT + candidate RIGHT)
      #          Only shown after a candidate is selected
      if (!is.null(cand)) {
        div(class = "pg-card step-card", id = "step3_card",
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
      } else div(class = "pg-card", div(class = "pg-body small text-muted",
        "Pick a candidate in Step 2 to compare it side by side.")),

      # Step 4 — Decision
      div(class = "dec-card", id = "step4_card",
        div(class = "dec-hdr", "Step 4 — Your decision"),
        div(class = "dec-body", uiOutput("decision_ui"))
      )
    )
  })

  # Left column under the queue: Step 2 candidates + score breakdown
  output$workspace_left <- renderUI({
    prod <- sel_product()
    if (is.null(prod)) return(NULL)
    cand <- sel_candidate()

    tagList(
      # Step 2 — Candidates
      div(class = "pg-card step-card", id = "step2_card", style = "margin-top: 1rem;",
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

      # Score breakdown strip (slim) — how the candidate's score is built
      if (!is.null(cand)) {
        div(class = "pg-card score-strip", id = "score_strip",
          div(class = "pg-body",
            div(class = "d-flex justify-content-between",
              span(class = "ss-lbl fw-semibold",
                   if (input$dataset == "text") "Why this score? — hover a segment"
                   else "What separates them? — share of nutrient distance"),
              span(class = "ss-lbl", paste0("Total ", round(cand$score * 100), "%"))
            ),
            plotlyOutput("score_breakdown", height = "46px")
          )
        )
      }
    )
  })

  # Split / full-width queue. extra = 0-based queue columns shown only at full width.
  observe({
    on    <- !is.null(sel_product()) && !queue_expanded()
    extra <- if (input$dataset == "text") c(2, 3) else c(5, 6, 7)
    session$sendCustomMessage("valSplit", list(on = on, extra = as.list(extra)))
  })
  observeEvent(input$back_to_queue, {
    sel_product(NULL); sel_candidate(NULL)
    selectRows(queue_proxy, NULL)
  })

  # "Expand queue" — full-width browsing without losing the current product
  queue_expanded <- reactiveVal(FALSE)
  observeEvent(sel_product(), if (is.null(sel_product())) queue_expanded(FALSE), ignoreNULL = FALSE)
  observeEvent(input$toggle_queue, queue_expanded(!queue_expanded()))
  output$queue_toggle <- renderUI({
    req(sel_product())
    actionLink("toggle_queue", class = "queue-toggle",
      if (queue_expanded()) "Back to review" else "Expand queue")
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
      # Hint only until a candidate is picked (the highlight says the rest)
      if (is.null(sel_candidate()))
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
        # Same source as Step 3: the candidate's own barcode field
        Barcode = ifelse(is.na(barcode) | !nzchar(barcode), "—",
                         paste0(htmltools::htmlEscape(gsub(";", " ", barcode)),
                                ifelse(barcode_hit, ' <span class="kw-pill kw-same">match</span>', ""))),
        name   = htmltools::htmlEscape(name),
        brands = ifelse(is.na(brands), "—", htmltools::htmlEscape(brands))
      ) %>%
      select(pid, name, brands, Barcode, Score, Confidence) %>%
      rename(ID = pid, Product = name, Brand = brands)
    cs <- cand_selection(df)
    datatable(show,
      selection = cs$sel, rownames = FALSE, escape = FALSE,
      options   = list(pageLength = 4, dom = "tp", displayStart = cs$start)
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
    cs <- cand_selection(df)
    datatable(show,
      selection = cs$sel, rownames = FALSE,
      options   = list(pageLength = 4, dom = "tp", displayStart = cs$start)
    )
  })

  # Nutrition for one record: local columns (numeric dataset) or Open Food
  # Facts by barcode (text dataset has no nutrient columns). Cached per barcode.
  record_nutrition <- function(r) {
    if (input$dataset == "numeric") {
      v <- setNames(vapply(NUTR$key, function(k) suppressWarnings(as.numeric(r[[k]] %||% NA)), numeric(1)), NUTR$key)
      return(list(nutr = v, src = "dataset"))
    }
    o <- off_by_barcode(r$barcode)
    if (isTRUE(o$found)) list(nutr = o$nutr, src = paste0("Open Food Facts · ", o$code))
    else list(nutr = setNames(rep(NA_real_, nrow(NUTR)), NUTR$key), src = o$reason)
  }

  cmp_nutrition <- reactive({
    prod <- sel_product(); cand <- sel_candidate(); req(prod, cand)
    list(p = record_nutrition(prod), c = record_nutrition(cand))
  })

  # Compact nutrition block: 7 nutrients in a 2-column grid (pending | candidate)
  # so Step 4 stays on screen. Shared by both datasets.
  nutrition_grid <- function(nut, prod, cand, with_diff = FALSE) {
    fmt <- function(v) if (is.na(v)) tags$em(class = "cmp-neutral", "—") else format(round(v, 1), nsmall = 0)
    pn <- nut$p$nutr; cn <- nut$c$nutr
    items <- lapply(seq_len(nrow(NUTR)), function(i) {
      k <- NUTR$key[i]; pv <- pn[[k]]; cv <- cn[[k]]
      cls <- if (is.na(pv) || is.na(cv)) "cmp-neutral"
             else if (nutr_differs(pv, cv, NUTR$tol[i])) "cmp-diff" else "cmp-match"
      div(class = if (with_diff) "nut-item has-d" else "nut-item",
        span(class = "nut-lbl", NUTR$label[i], tags$small(paste0(" ", NUTR$unit[i]))),
        span(class = "nut-p", fmt(pv)),
        span(class = paste("nut-c", cls), fmt(cv)),
        if (with_diff) span(class = paste("nut-d", cls),
          if (is.na(pv) || is.na(cv)) "" else sprintf("Δ %+.1f", pv - cv))
      )
    })
    div(
      div(class = "nut-head",
        span(tags$b("Nutrition / 100 g"), "  ·  ", prod$pid, " | ", cand$pid),
        if (input$dataset == "text")
          span(paste0("pending: ", nut$p$src, "  ·  candidate: ", nut$c$src))
      ),
      div(class = "nut-grid", items)
    )
  }

  # O6 — comparison_ui [Q4 · C4] — merged pending + candidate side by side
  output$comparison_ui <- renderUI({
    prod <- sel_product(); cand <- sel_candidate(); req(prod, cand)
    nut  <- cmp_nutrition()

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
        show <- function(v) if (v == "—") tags$em(class = "cmp-neutral", "—")
                            else if (f$key == "name") HTML(highlight_keywords(v)) else v
        tags$tr(
          tags$td(class = "fw-semibold text-muted", style = "width:18%;", f$label),
          tags$td(style = "width:41%;", show(cc$pv)),
          tags$td(class = cc$cls, style = "width:41%;", show(cc$cv))
        )
      })

      # Formulation flags: form / claim / flavour keywords. A flag on only one
      # side is terracotta — the classic "same brand, different product" trap.
      kp <- find_keywords(prod$name); kc <- find_keywords(cand$name)
      pills <- function(mine, other) {
        if (!length(mine)) return(tags$em(class = "cmp-neutral", "none"))
        lapply(mine, function(k) span(
          class = paste("kw-pill", if (k %in% other) "kw-same" else "kw-diff"),
          title = if (k %in% other) "On both products" else "Only on this product — check before merging",
          sub("^[a-z]+:", "", k)))
      }
      flag_row <- tags$tr(
        tags$td(class = "fw-semibold text-muted", "Flags"),
        tags$td(pills(kp, kc)), tags$td(pills(kc, kp))
      )
      rows <- append(rows, list(flag_row), after = 1)   # right under Name

      nut_row <- tags$tr(tags$td(colspan = 3, class = "nut-block", nutrition_grid(nut, prod, cand)))
      tags$table(class = "cmp-tbl",
        tags$thead(tags$tr(
          tags$th("Field"),
          tags$th(paste("Pending:", prod$pid)),
          tags$th(paste("Candidate:", cand$pid))
        )),
        tags$tbody(rows, nut_row)
      )
    } else {
      div(class = "nut-block", style = "border:1px solid var(--border); border-radius:8px;",
          nutrition_grid(nut, prod, cand, with_diff = TRUE))
    }
  })

  # Score breakdown strip — segmented bar of what makes up the candidate score
  output$score_breakdown <- renderPlotly({
    prod <- sel_product(); cand <- sel_candidate(); req(prod, cand)
    pct <- function(x) paste0(round(x * 100), "%")

    if (input$dataset == "text") {
      p <- score_text_parts(prod, cand)
      if (isTRUE(cand$barcode_hit)) {
        seg <- data.frame(
          lab = c("Barcode match"), val = 1, col = PAL$primary,
          tip = paste0("<b>Barcode match → score set to 100%</b><br>",
                       "Identical barcode overrides text similarity.<br>",
                       hint(paste0("Text-only score would be ", pct(score_text(prod, cand)), "."))))
      } else {
        parts <- c(W_NAME * p$name, W_BRAND * p$brand, W_CAT * p$cat)
        seg <- data.frame(
          lab = c("Name", "Brand", "Category", "Not matched"),
          val = c(parts, max(0, 1 - sum(parts))),
          col = c(PAL$primary, "rgba(27,67,50,.62)", "rgba(27,67,50,.32)", PAL$border),
          tip = c(
            paste0("<b>Name similarity: ", pct(W_NAME * p$name), " of 60%</b><br>",
                   "Jaro-Winkler similarity of the names = ", round(p$name, 2), "<br>",
                   hint("Character-level; rewards shared prefixes, not shared meaning.")),
            paste0("<b>Brand: ", pct(W_BRAND * p$brand), " of 25%</b><br>",
                   if (p$brand_known) paste0("Brand similarity = ", round(p$brand, 2))
                   else "Brand missing on one side → neutral 0.5 (half credit)"),
            paste0("<b>Category bonus: ", pct(W_CAT * p$cat), " of 15%</b><br>",
                   if (p$cat == 1) "Same category" else
                   if (is.na(prod$categories)) "Pending product has no category → no bonus" else "Different category"),
            paste0("<b>Not matched: ", pct(max(0, 1 - sum(parts))), "</b><br>",
                   hint("Barcode match would override to 100% — none here."))
          ), stringsAsFactors = FALSE)
      }
    } else {
      pv <- as.numeric(prod[1, CORE]); cv <- as.numeric(cand[1, CORE])
      d2 <- ((pv - cv) / max_core)^2
      ok <- !is.na(d2)
      if (!any(ok) || sum(d2[ok]) == 0) {
        seg <- data.frame(lab = "Identical", val = 1, col = PAL$primary,
                          tip = "<b>No nutrient differences</b>", stringsAsFactors = FALSE)
      } else {
        share <- d2[ok] / sum(d2[ok])
        seg <- data.frame(
          lab = CORE[ok], val = share,
          col = ifelse(share >= .30, PAL$accent, PAL$bar_muted),
          tip = paste0("<b>", CORE[ok], ": ", pct(share), " of the distance</b><br>",
                       "pending ", pv[ok], " vs candidate ", cv[ok]),
          stringsAsFactors = FALSE)
      }
    }

    segment_labels        <- ifelse(seg$val >= .09, seg$lab, "")   # too-thin segments stay unlabeled
    detailed_explanations <- seg$tip
    plt <- plot_ly(orientation = "h")
    for (i in seq_len(nrow(seg))) {
      plt <- plt %>% add_bars(
        x = seg$val[i], y = "s", name = seg$lab[i],
        marker = list(color = seg$col[i], line = list(color = PAL$card, width = 1.5)),
        text = segment_labels[i], textposition = "inside", insidetextanchor = "middle",
        textfont = list(size = 10, color = if (seg$col[i] %in% c(PAL$border, PAL$bar_muted)) PAL$slate else PAL$card),
        hovertext = detailed_explanations[i], hoverinfo = "text")
    }
    plt %>% layout(barmode = "stack", showlegend = FALSE, bargap = 0,
                   xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
                   yaxis = list(visible = FALSE, fixedrange = TRUE),
                   margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
      pal_plotly()
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
      geom_hline(yintercept = 0, colour = PAL$slate, linewidth = .5) +
      scale_fill_manual(values = c("TRUE" = PAL$accent, "FALSE" = PAL$primary), guide = "none") +
      coord_flip() +
      labs(title = "Nutrient delta — pending minus candidate", x = NULL, y = "difference") +
      theme_minimal(base_size = 10) +
      theme(text = element_text(colour = PAL$text), axis.text = element_text(colour = PAL$muted),
            panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = PAL$grid))
    ggplotly(p) %>% plotly::config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 0, t = 28, b = 8))
  })

  # O7 — decision_ui [Q6 · C5]
  # Suggested decision for the current pending product / candidate.
  #   exact barcode match -> identical; best score < 65% -> new; otherwise similar.
  suggest_decision <- function(prod, cand) {
    if (input$dataset == "text" && !is.null(cand) &&
        isTRUE(!is.na(prod$barcode) && !is.na(cand$barcode) && prod$barcode == cand$barcode))
      return(list(val = "identical", why = "exact barcode match"))
    best <- if (!is.null(cand)) cand$score else {
      cs <- tryCatch(cands_for(prod), error = function(e) NULL)
      if (is.null(cs) || !nrow(cs)) NA else max(cs$score, na.rm = TRUE)
    }
    if (is.na(best) || best < .65)
      return(list(val = "new", why = if (is.na(best)) "no candidate above the threshold"
                                    else sprintf("best match only %d%%", round(100 * best))))
    list(val = "similar", why = sprintf("%d%% match, no barcode match", round(100 * best)))
  }

  DECISIONS <- list(
    list(val = "identical", title = "Identical duplicate",
         sub = "Merge into the validated record — no new row"),
    list(val = "similar",   title = "Similar but distinct",
         sub = "Keep as a separate record, linked as a variant"),
    list(val = "new",       title = "Genuinely new product",
         sub = "Create a new record awaiting nutrition approval")
  )

  # O7 — decision_ui: three clickable cards. They ARE the radio group: each card
  # is a <label> around a hidden radio named "disposition", so Shiny's radio
  # binding, keyboard 1/2/3 and clicks all drive the same input$disposition.
  output$decision_ui <- renderUI({
    prod <- sel_product(); req(prod)
    cand <- sel_candidate()
    sug  <- suggest_decision(prod, cand)
    card <- function(d) tags$label(class = "dcard",
      title = if (d$val == sug$val) paste("Suggested:", sug$why),
      tags$input(type = "radio", name = "disposition", value = d$val, class = "dcard-input",
                 checked = if (d$val == sug$val) NA),
      # Same-height top row on every card (pill or empty), so icons and titles
      # line up whichever card carries the Suggested pill.
      span(class = "dcard-top", if (d$val == sug$val) span(class = "dcard-sug", "Suggested")),
      span(class = "dcard-title", d$title),
      span(class = "dcard-sub", d$sub)
    )
    tagList(
      div(id = "disposition", role = "radiogroup", `aria-label` = "Decision",
          class = "form-group shiny-input-radiogroup shiny-input-container dcard-grid",
          lapply(DECISIONS, card)),
      div(class = "dcard-why",
          paste0("Suggested because: ", sug$why, ". Nothing is saved until you confirm.")),
      uiOutput("decision_panel"),
      div(class = "confirm-row",
        actionButton("confirm_decision", "Confirm decision",
          class = "btn btn-primary btn-confirm", icon = icon("check"))
      )
    )
  })

  cat_memo <- new.env()
  observeEvent(input$new_category, {
    cat_memo$pid <- sel_product()$pid; cat_memo$cat <- input$new_category
  }, ignoreInit = TRUE)

  # Context panel under the cards — depends on the chosen card
  output$decision_panel <- renderUI({
    prod <- sel_product(); req(prod, input$disposition)
    cand <- sel_candidate()
    disp <- input$disposition
    cats <- setdiff(if (input$dataset == "text") cats_text else cats_num, "(No category)")
    # Keep a category the user already picked for THIS product when switching
    # cards; a new product starts from its own category.
    keep <- if (identical(cat_memo$pid, prod$pid)) cat_memo$cat else ""
    cur  <- if (nzchar(keep)) keep else prod$categories %||% ""
    cat_select <- selectInput("new_category", "Category",
      choices = c("— choose —" = "", cats),
      selected = if (cur %in% cats) cur else "", width = "100%")
    no_cat_warn <- if (is.na(prod$categories %||% NA)) div(class = "small txt-accent mt-1",
      "This product has no category — assign one before saving.")

    if (disp == "identical") {
      if (is.null(cand)) return(div(class = "new-panel small txt-accent",
        "Pick a candidate in Step 2 — that is the validated record this product merges into."))
      nut <- tryCatch(cmp_nutrition(), error = function(e) NULL)
      v   <- nut$c$nutr
      short <- c(energy = "kcal", protein = "P", carbohydrates = "C", sugar = "sugars", fat = "F", salt = "salt")
      nut_txt <- if (is.null(v) || all(is.na(v))) "no nutrition values on file"
        else paste(vapply(names(short), function(k) {
          x <- v[[k]]; if (is.na(x)) paste(short[[k]], "—")
          else if (k == "energy") paste(round(x), "kcal") else paste(short[[k]], format(round(x, 1)))
        }, character(1)), collapse = " · ")
      return(div(class = "new-panel small",
        "Merges ", strong(prod$pid), " into validated ", strong(cand$pid),
        " and inherits its nutrition: ", span(class = "txt-primary", nut_txt),
        if (!is.null(nut$c$src) && input$dataset == "text") span(class = "text-muted", paste0(" (", nut$c$src, ")"))))
    }
    if (disp == "similar") {
      return(div(class = "new-panel",
        div(class = "row g-2",
          div(class = "col-sm-5", cat_select),
          div(class = "col-sm-7", textInput("variant_note", "Variant / difference note (optional)",
            placeholder = "e.g. gluten-free version, 500 g pack, raspberry flavour", width = "100%"))),
        no_cat_warn,
        if (!is.null(cand)) div(class = "small text-muted", "Linked to ", strong(cand$pid), " as a variant.")))
    }
    div(class = "new-panel",
      div(class = "row g-2 align-items-end",
        div(class = "col-sm-6", cat_select),
        div(class = "col-sm-6",
          actionButton("off_fetch", "Fetch Open Food Facts preview",
            icon = icon("magnifying-glass"), class = "btn btn-sm btn-outline-primary w-100 mb-3"))),
      no_cat_warn,
      uiOutput("off_preview"))
  })

  # Open Food Facts preview — only runs when the button is pressed
  off_preview_data <- eventReactive(input$off_fetch, {
    prod <- sel_product(); req(prod)
    if (input$dataset == "numeric")
      return(list(found = FALSE, reason = "Numeric records have no barcode or name to look up."))
    withProgress(message = "Querying Open Food Facts…", value = .5, {
      o <- off_by_barcode(prod$barcode)
      if (!isTRUE(o$found)) o <- off_by_name(prod$name)
      o
    })
  })
  # A new product invalidates the preview
  off_for <- reactiveVal(NULL)
  observeEvent(input$off_fetch, off_for(sel_product()$pid))

  output$off_preview <- renderUI({
    req(input$off_fetch > 0, identical(off_for(), sel_product()$pid))
    o <- off_preview_data()
    if (!isTRUE(o$found)) {
      return(manual_nutrition_ui(o))
    }
    fmt <- function(v) if (is.na(v)) "—" else format(round(v, 1))
    div(class = "off-card",
      div(class = "off-title", o$name %||% "(unnamed)",
          if (nzchar(o$brand %||% "")) span(class = "text-muted fw-normal", paste0(" · ", o$brand))),
      div(class = "small text-muted mb-1",
        "Matched by ", o$method, " · ",
        tags$a(href = paste0("https://world.openfoodfacts.org/product/", o$code),
               target = "_blank", "open on Open Food Facts")),
      div(class = "off-grid", lapply(seq_len(nrow(NUTR)), function(i) div(
        span(class = "text-muted", NUTR$label[i]), " ",
        strong(fmt(o$nutr[[NUTR$key[i]]])), tags$small(class = "text-muted", paste0(" ", NUTR$unit[i]))
      ))),
      div(class = "small text-muted mt-1", "Preview only — saved with your decision as ",
          strong("awaiting dietitian approval"), ".")
    )
  })

  # Open Food Facts fallback. When the live lookup fails or is empty, use local
  # values where they exist (numeric records carry their own nutrients; text
  # records have none), then show compact inputs so the dietitian can type or
  # correct values per 100 g before confirming.
  MAN <- c("energy", "protein", "carbohydrates", "sugar", "fat", "salt")
  local_reference <- function(prod) {
    if (input$dataset == "numeric") {
      v <- record_nutrition(prod)$nutr
      if (any(!is.na(v[MAN]))) return(list(nutr = v, src = paste("this record", prod$pid)))
    }
    NULL
  }
  manual_nutrition_ui <- function(o) {
    prod <- sel_product(); req(prod)
    ref  <- local_reference(prod)
    reason <- trimws(sub("[. ]+$", "", o$reason %||% "Not found"))
    reason <- paste0(toupper(substr(reason, 1, 1)), substring(reason, 2))
    msg <- if (!is.null(ref)) paste0(reason, ". Pre-filled from ", ref$src, " — check and correct below.")
           else paste0(reason, ". No local reference either — enter values per 100 g below (optional).")
    val <- function(k) { v <- ref$nutr[[k]] %||% NA; if (is.na(v)) NA else round(v, 1) }
    lbl <- setNames(paste0(NUTR$label, " (", NUTR$unit, ")"), NUTR$key)
    div(class = "off-card",
      div(class = "small", msg),
      div(class = "man-grid",
        lapply(MAN, function(k) numericInput(paste0("man_", k), lbl[[k]], value = val(k),
                                             min = 0, step = if (k == "energy") 1 else 0.1)))
    )
  }
  manual_nutrition_str <- function(off) {
    if (isTRUE(off$found)) return("")
    v <- vapply(MAN, function(k) suppressWarnings(as.numeric(input[[paste0("man_", k)]] %||% NA)), numeric(1))
    if (all(is.na(v))) return("")
    paste(paste0(NUTR$label[match(MAN, NUTR$key)], " ", ifelse(is.na(v), "—", format(v))), collapse = "; ")
  }

  queue_proxy <- dataTableProxy("queue_table")

  # OB3 — confirm (button or Enter) → log, then advance to next undecided item
  observeEvent(input$confirm_decision, {
    req(sel_product(), input$disposition)
    prod <- sel_product(); cand <- sel_candidate()
    disp <- input$disposition
    if (disp == "identical" && is.null(cand)) {
      showNotification("Pick the validated record in Step 2 before merging.", type = "warning")
      return()
    }
    if (disp %in% c("similar", "new") && is.na(prod$categories %||% NA) && !nzchar(input$new_category %||% "")) {
      showNotification("Assign a category before saving this product.", type = "warning")
      return()
    }
    off <- if (disp == "new" && isTRUE(input$off_fetch > 0) && identical(off_for(), prod$pid))
             tryCatch(off_preview_data(), error = function(e) NULL) else NULL
    log_data(rbind(log_data(), data.frame(
      time      = format(Sys.time(), "%H:%M:%S"),
      pid       = prod$pid,
      candidate = if (!is.null(cand)) cand$pid else "—",
      decision  = disp,
      category  = if (disp != "identical") (input$new_category %||% "") else "",
      note      = if (disp == "similar") trimws(input$variant_note %||% "") else "",
      off_code  = if (isTRUE(off$found)) off$code else "",
      nutrition = if (disp == "new") manual_nutrition_str(off) else "",
      stringsAsFactors = FALSE
    )))

    # Next undecided item in the current queue order (wraps around)
    q    <- filtered_pending()
    done <- log_data()$pid
    cur  <- match(prod$pid, q$pid)
    ord  <- if (is.na(cur)) seq_len(nrow(q)) else c(seq_len(nrow(q))[-seq_len(cur)], seq_len(cur))
    nxt  <- ord[!(q$pid[ord] %in% done)][1]
    label <- c(identical = "identical duplicate", similar = "similar but distinct", new = "new product")[[disp]]
    if (is.na(nxt)) {
      sel_product(NULL); sel_candidate(NULL)
      showNotification(paste0(prod$pid, " saved as ", label, ". Queue complete — every item has a decision."),
                       type = "message", duration = 5)
    } else {
      selectPage(queue_proxy, ceiling(nxt / 8))
      selectRows(queue_proxy, nxt)
      sel_product(q[nxt, , drop = FALSE]); sel_candidate(NULL)
      showNotification(paste0(prod$pid, " saved as ", label, " → now reviewing ", q$pid[nxt]),
                       type = "message", duration = 4)
    }
  })

  # Threshold Tuner [Q7 · C6]
  # Measured at startup (see eval_thresholds); switches with the dataset toggle
  thr_curve <- reactive(thr_eval_r()[[input$dataset]])

  output$thr_kpis <- renderUI({
    t  <- input$threshold
    cm <- thr_curve()$cand_mean[thr_curve()$Threshold == t]
    ec <- if (length(cm)) format(round(cm, 1), big.mark = ",") else "—"
    pr <- thr_curve()$precision[thr_curve()$Threshold == t]
    ep <- if (length(pr) && !is.na(pr)) round(pr, 1) else "n/a "
    div(class = "row g-2",
      div(class = "col-6",
        div(class = "kpi-card tone-neutral",
          div(div(class = "kpi-num sm", paste0(t, "%")),
              div(class = "kpi-lbl", "Threshold"))
        )
      ),
      div(class = "col-6",
        div(class = "kpi-card tone-neutral",
          div(div(class = "kpi-num sm", ec),
              div(class = "kpi-lbl", "Candidates / item"))
        )
      ),
      div(class = "col-12 mt-1",
        div(class = "kpi-card tone-brand",
          div(div(class = "kpi-num sm", if (is.numeric(ep)) paste0(ep, "%") else "n/a"),
              div(class = "kpi-lbl",
                  if (is.numeric(ep)) "Match precision (barcode-verified sample)"
                  else if (input$dataset == "text") "Match precision — no candidates surface"
                  else "Match precision — no ground truth for numeric data"))
        )
      )
    )
  })

  output$pr_curve <- renderPlotly({
    df <- thr_curve(); t <- input$threshold
    fmt <- function(x) format(round(x, 1), big.mark = ",", nsmall = 0)

    gt <- if (df$n_labelled[1] > 0) {
      paste0(
        "<br><br><b>Barcode-verified duplicates (n = ", df$n_labelled, ")</b>",
        "<br>Found (true positives): ", df$TP,
        "<br>Missed (false negatives): ", df$FN,
        ifelse(df$FN > 0, paste0(" ", hint(df$fn_ids)), ""),
        "<br>Wrong candidates surfaced (false positives): ", format(df$FP, big.mark = ","),
        " ", hint(paste0("(", fmt(df$FP / df$n_labelled), " per item)"))
      )
    } else {
      paste0("<br><br>", hint("False positive / negative counts need ground truth;<br>the numeric dataset has no barcodes to verify against."))
    }
    df$hover <- paste0(
      "<b>Threshold ", df$Threshold, "%</b>",
      "<br>Candidates per pending item: ", fmt(df$cand_mean),
      " ", hint(paste0("(median ", fmt(df$cand_median), ")")),
      gt
    )

    # Log axis can't show 0, so zero points sit on a floor tick labelled "0"
    floor_y <- 0.01
    df$y <- pmax(df$cand_mean, floor_y)
    df$is_cur <- df$Threshold == t

    # Dynamic callout at the slider position
    cur  <- df[df$is_cur, ]
    knee <- find_knee(df)
    kr   <- df[df$Threshold == knee, ]
    prec_txt <- function(r) if (!is.na(r$precision))
      paste0(round(r$precision, 1), "% precision · ", r$TP, "/", r$n_labelled, " known duplicates found")
      else if (r$n_labelled > 0) "precision n/a (no candidates surface)"
      else "precision n/a (no ground truth)"
    flip_left <- t >= 75
    far_right <- t > 86   # no room right of the line: lift the callout, knee label stays left
    high_left <- !flip_left && log10(cur$y) > 2.5
    callout <- list(
      x = t, y = log10(cur$y), xref = "x", yref = "y",
      text = paste0("<b>At ", t, "%</b>: ", fmt(cur$cand_mean), " candidates/item surface",
                    "<br>", prec_txt(cur)),
      showarrow = TRUE, arrowhead = 0, arrowcolor = PAL$accent, arrowwidth = 1.2,
      # From 75% up the box flips to the LEFT of the dashed line, so it never
      # sits on top of the sweet-spot label (which then moves right, see below).
      # Far right: tuck it into the empty upper-right corner along the dashed line.
      # High-count left end: pin it in the empty lower-left area (y = 0.1, data
      # coords) so it is neither clipped at the top nor drawn across the curve.
      axref = if (high_left) "x" else "pixel",
      ax = if (far_right) -12 else if (flip_left) -110 else if (high_left) min(THR_SEQ) + 1 else 110,
      ayref = if (high_left) "y" else "pixel",
      ay = if (far_right) -180 else if (high_left) -1 else -60,
      xanchor = if (flip_left) "right" else "left", align = "left",
      bgcolor = PAL$card, bordercolor = PAL$accent, borderwidth = 1, borderpad = 5,
      font = list(size = 11, color = PAL$text)
    )
    knee_note <- list(
      x = knee, y = log10(kr$y), xref = "x", yref = "y",
      text = paste0("<b>Sweet spot ≈ ", knee, "%</b><br>",
                    "<span style='font-size:10px'>curve bends hardest here</span>"),
      showarrow = TRUE, arrowhead = 0, arrowcolor = PAL$primary,
      # Opposite side from the threshold callout. When the callout is flipped
      # left, the knee box starts just right of the dashed line (data coords),
      # so the two can't overlap; near the right edge the callout is lifted instead.
      axref = if (flip_left && !far_right) "x" else "pixel",
      ax = if (flip_left && !far_right) t + 1.5 else -75, ay = 55,
      xanchor = if (flip_left && !far_right) "left" else "right", align = "center",
      bgcolor = PAL$brand_soft, bordercolor = PAL$primary, borderwidth = 1, borderpad = 4,
      font = list(size = 11, color = PAL$primary)
    )
    df$hover[df$Threshold == knee] <- paste0(df$hover[df$Threshold == knee],
       "<br><br><b style='color:", PAL$primary, "'>Sweet spot</b>: point of maximum curvature —<br>",
      "raising the threshold past here removes few extra candidates.")

    plot_ly(df, x = ~Threshold, y = ~y, type = "scatter", mode = "lines+markers",
            line = list(color = PAL$primary, width = 2.5),
            marker = list(color = ifelse(df$is_cur, PAL$accent, PAL$primary),
                          size  = ifelse(df$is_cur, 12, 7),
                          line  = list(color = PAL$card, width = 1.5)),
            text = ~hover, hoverinfo = "text", name = "Candidates") %>%
      add_trace(data = kr, x = ~Threshold, y = ~y, type = "scatter", mode = "markers",
                marker = list(symbol = "diamond-open", size = 20, color = PAL$primary,
                              line = list(width = 2.5)),
                hoverinfo = "skip", inherit = FALSE, name = "Sweet spot") %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "Similarity threshold", ticksuffix = "%",
                     dtick = 5, zeroline = FALSE, showgrid = FALSE,
                     showline = TRUE, linecolor = PAL$border,
                     tickfont = list(color = PAL$muted), titlefont = list(color = PAL$muted)),
        yaxis = list(title = "Candidates surfaced per pending item (mean)",
                     type = "log", gridcolor = PAL$grid, zeroline = FALSE,
                     tickfont = list(color = PAL$muted), titlefont = list(color = PAL$muted),
                     tickvals = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     ticktext = c("0", "0.1", "1", "10", "100", "1k", "10k"),
                     range = c(-2.2, log10(max(df$y, 1)) + 0.3)),
        shapes = list(
          list(type = "line", x0 = t, x1 = t, y0 = 0, y1 = 1, yref = "paper",
               line = list(color = PAL$accent, dash = "dash", width = 1.5)),
          list(type = "line", x0 = knee, x1 = knee, y0 = 0, y1 = 1, yref = "paper",
               line = list(color = PAL$bar_muted, dash = "dot", width = 1))
        ),
        annotations = list(callout, knee_note),
        margin = list(t = 30)
      ) %>%
      pal_plotly()
  })

  # Data Quality [Q8 · C7]
  missing_quality <- reactive({
    df   <- if (input$dataset == "text") text_data() else df_num
    cols <- setdiff(colnames(df), c("row_id", "pid", "active", "cat_display"))
    per_group <- function(d, g) bind_rows(lapply(cols, function(cl) {
      miss <- is.na(d[[cl]])
      data.frame(Field = cl, Group = g, pct = mean(miss) * 100,
                 n_miss = sum(miss), n = nrow(d), ids = sample_ids(d$pid[miss]))
    }))
    bind_rows(
      per_group(df %>% filter(active == 0), "Pending (active = 0)"),
      per_group(df %>% filter(active == 1), "Validated (active = 1)")
    )
  })

  output$missing_heatmap <- renderPlotly({
    df <- missing_quality(); req(nrow(df) > 0)
    df$hover <- paste0(
      "<b>", df$Field, "</b> · ", df$Group,
      "<br>", round(df$pct, 1), "% missing (",
      format(df$n_miss, big.mark = ","), " of ", format(df$n, big.mark = ","), ")",
      "<br>", hint(paste0("e.g. ", df$ids)),
      "<br><i>Click to open pending items missing this field</i>"
    )
    fields <- unique(df$Field); groups <- unique(df$Group)
    z    <- matrix(df$pct,   nrow = length(groups), byrow = TRUE)
    txt  <- matrix(df$hover, nrow = length(groups), byrow = TRUE)
    lbl  <- matrix(paste0(round(df$pct), "%"), nrow = length(groups), byrow = TRUE)
    plot_ly(x = fields, y = groups, z = z, type = "heatmap", source = "miss_heat",
            text = txt, hoverinfo = "text", xgap = 2, ygap = 2,
            zmin = 0, zmax = 100,
            # neutral → terracotta: only the gaps draw the eye
            colorscale = list(c(0, PAL$bg), c(1, PAL$accent)),
            colorbar = list(title = list(text = "% missing", font = list(color = PAL$muted)),
                            ticksuffix = "%", len = .9, outlinewidth = 0,
                            tickfont = list(color = PAL$muted))) %>%
      add_annotations(x = rep(fields, each = length(groups)),
                      y = rep(groups, times = length(fields)),
                      text = as.vector(lbl), showarrow = FALSE,
                      # dark text reads better than white even on full terracotta (4.9:1 vs 3.6:1)
                      font = list(size = 10, color = PAL$text)) %>%
      event_register("plotly_click") %>%
      layout(xaxis = list(title = "", tickangle = -40, showgrid = FALSE,
                          tickfont = list(color = PAL$slate)),
             yaxis = list(title = "", showgrid = FALSE, tickfont = list(color = PAL$slate)),
             margin = list(l = 10, b = 80)) %>%
      pal_plotly()
  })

  # Data Quality: encoding issues table [Q8 / batch fix]
  output$encoding_issues_table <- renderDT({
    fx <- mojibake_repairs(text_data()) %>% filter(active == 0, field == "name")
    if (nrow(fx) == 0) {
      return(datatable(data.frame(Message = "No encoding issues left in the pending text queue."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    cats <- text_data()$cat_display[fx$row]
    datatable(
      data.frame(ID = fx$pid, `Product name (raw)` = fx$before, `After repair` = fx$after,
                 Category = cats, check.names = FALSE),
      rownames = FALSE,
      options = list(pageLength = 5, dom = "ftp", language = list(search = "Filter:"))
    )
  })

  # Batch fix: preview modal with before/after, then apply + re-score
  repairs_preview <- reactive({
    fx <- mojibake_repairs(text_data())
    if (!isTRUE(input$fix_include_validated)) fx <- fx %>% filter(active == 0)
    fx
  })

  observeEvent(input$batch_fix_btn, {
    all_fx <- mojibake_repairs(text_data())
    n_p <- sum(all_fx$active == 0); n_v <- sum(all_fx$active == 1)
    showModal(modalDialog(
      title = "Repair UTF-8 encoding",
      size = "l", easyClose = TRUE,
      if (nrow(all_fx) == 0) p("Nothing to repair — all names and brands are clean.") else tagList(
        p(class = "small text-muted",
          "Each value is re-encoded to its original bytes and decoded as UTF-8. ",
          "Only values that convert cleanly are listed. Names and brands are both scored, so ",
          "repairing the validated reference records too lets pending items match them."),
        checkboxInput("fix_include_validated",
          paste0("Also repair ", n_v, " validated reference values (recommended)"), value = TRUE),
        DTOutput("repairs_table")
      ),
      footer = tagList(
        modalButton("Cancel"),
        if (nrow(all_fx) > 0) uiOutput("apply_btn_ui", inline = TRUE)
      )
    ))
  })

  output$apply_btn_ui <- renderUI({
    actionButton("batch_fix_apply", paste0("Apply ", nrow(repairs_preview()), " repairs & re-score"),
                 class = "btn btn-primary", icon = icon("check"))
  })

  output$repairs_table <- renderDT({
    fx <- repairs_preview()
    datatable(
      data.frame(ID = fx$pid, Status = ifelse(fx$active == 0, "pending", "validated"),
                 Field = fx$field, Before = fx$before, After = fx$after),
      rownames = FALSE, options = list(pageLength = 8, dom = "ftp", language = list(search = "Filter:"))
    )
  })

  observeEvent(input$batch_fix_apply, {
    fx <- repairs_preview(); req(nrow(fx) > 0)
    d  <- text_data()
    for (col in c("name", "brands")) {
      f <- fx[fx$field == col, ]
      if (!nrow(f)) next
      d[[col]][f$row] <- f$after
      d[[paste0(col, "_search")]][f$row] <- tolower(f$after)   # search keys are lower-cased copies
    }
    removeModal()
    withProgress(message = "Re-scoring the queue…", value = .4, {
      text_data(d)
      ev <- thr_eval_r(); ev$text <- eval_thresholds(d, "text"); thr_eval_r(ev)
    })
    sel_product(NULL); sel_candidate(NULL)
    showNotification(paste0("Repaired ", nrow(fx), " values (", sum(fx$active == 0), " pending). ",
                            "Candidates and threshold curve re-scored. Changes last for this session only."),
                     type = "message", duration = 6)
  })

  # Guided tour (cicerone) ----------------------------------------------------

  # == Dashboard (landing page) =================================================
  observeEvent(input$dash_go_validate, nav_select("main_nav", "Validation"))

  # plotly entrance animation (bars grow, lines draw) after a short stagger
  dash_animate <- function(p, delay = 0)
    htmlwidgets::onRender(p, sprintf("function(el){ dashAnimate(el, %d); }", delay))

  dash_stats <- reactive({
    tx <- text_data(); done <- log_data()
    pend_t <- tx[tx$active == 0, ]; pend_n <- df_num[df_num$active == 0, ]
    left_t <- sum(!(pend_t$pid %in% done$pid)); left_n <- sum(!(pend_n$pid %in% done$pid))
    enc <- tryCatch(nrow(mojibake_repairs(tx) %>% filter(active == 0, field == "name")), error = function(e) 0L)
    uncat <- sum(pend_t$cat_display == "(No category)") + sum(pend_n$cat_display == "(No category)")
    list(pend_t = nrow(pend_t), pend_n = nrow(pend_n), left_t = left_t, left_n = left_n,
         ref_t = sum(tx$active == 1), ref_n = sum(df_num$active == 1),
         done = nrow(done), done_t = sum(grepl("^T-", done$pid)), done_n = sum(grepl("^N-", done$pid)),
         enc = enc, uncat = uncat)
  })

  observe({
    st <- dash_stats()
    session$sendCustomMessage("countUp", list(
      list(id = "dk_pending", value = st$left_t + st$left_n),
      list(id = "dk_ref",     value = st$ref_t + st$ref_n),
      list(id = "dk_done",    value = st$done),
      list(id = "dk_issues",  value = st$uncat + st$enc)))
  })
  fmt <- function(x) format(x, big.mark = ",")
  output$dk_pending_sub <- renderText({ st <- dash_stats(); paste0(fmt(st$left_t), " text · ", fmt(st$left_n), " nutrition") })
  output$dk_ref_sub     <- renderText({ st <- dash_stats(); paste0(fmt(st$ref_t), " text · ", fmt(st$ref_n), " nutrition") })
  output$dk_done_sub    <- renderText({ st <- dash_stats()
    paste0(round(100 * st$done / max(1, st$pend_t + st$pend_n), 1), "% of the queue reviewed") })
  output$dk_issues_sub  <- renderText({ st <- dash_stats(); paste0(st$uncat, " uncategorized · ", st$enc, " encoding") })

  # Pending queue by category — both datasets, stacked, top 10
  output$dash_cat <- renderPlotly({
    done <- log_data()$pid
    pt <- text_data() %>% filter(active == 0, !(pid %in% done)) %>% count(cat_display, name = "text")
    pn <- df_num %>% filter(active == 0, !(pid %in% done)) %>% count(cat_display, name = "nutrition")
    cc <- full_join(pt, pn, by = "cat_display") %>%
      mutate(across(c(text, nutrition), ~ coalesce(.x, 0L)), total = text + nutrition) %>%
      arrange(desc(total)) %>% head(10) %>% arrange(total)
    if (!nrow(cc)) return(plotly_empty() %>% pal_plotly())
    lv <- cc$cat_display
    tick <- ifelse(lv == "(No category)", paste0("<b style='color:", PAL$accent, "'>", lv, "</b>"), lv)
    plot_ly(cc, y = ~factor(cat_display, levels = lv), x = ~text, type = "bar", orientation = "h",
            name = "Text / names", marker = list(color = PAL$primary),
            hovertemplate = "%{y}<br>Text: %{x}<extra></extra>") %>%
      add_trace(x = ~nutrition, name = "Nutrition", marker = list(color = "#74A88F"),
                hovertemplate = "%{y}<br>Nutrition: %{x}<extra></extra>") %>%
      layout(barmode = "stack", bargap = .35,
             xaxis = list(title = "", range = c(0, max(cc$total) * 1.08), gridcolor = PAL$grid, zeroline = FALSE),
             yaxis = list(title = "", tickvals = lv, ticktext = tick, automargin = TRUE),
             legend = list(orientation = "h", x = 1, xanchor = "right", y = -0.1, yanchor = "top", font = list(size = 11), traceorder = "normal"),
             margin = list(l = 10, r = 10, t = 6, b = 10)) %>%
      pal_plotly() %>% dash_animate(250)
  })

  # Review progress — two animated rings + next item
  ring <- function(pct, label, sub, color) {
    r <- 46; L <- 2 * pi * r; off <- L * (1 - pct / 100)
    div(class = "ring",
      HTML(sprintf(
        '<svg width="124" height="124" viewBox="0 0 124 124">
           <circle cx="62" cy="62" r="%1$s" fill="none" stroke="%6$s" stroke-width="10"/>
           <circle class="arc" cx="62" cy="62" r="%1$s" fill="none" stroke="%5$s" stroke-width="10"
             stroke-linecap="round" transform="rotate(-90 62 62)"
             style="--len:%2$.2f; stroke-dasharray:%2$.2f; stroke-dashoffset:%3$.2f"/>
           <text class="ring-pct" x="62" y="64" text-anchor="middle">%4$s%%</text>
           <text class="ring-cap" x="62" y="80" text-anchor="middle">reviewed</text>
         </svg>', r, L, off, round(pct), color, PAL$grid)),
      div(class = "ring-lbl", label), div(class = "ring-sub", sub))
  }
  output$dash_progress <- renderUI({
    st <- dash_stats()
    nxt <- filtered_pending() %>% filter(!(pid %in% log_data()$pid)) %>% head(1)
    tagList(
      div(class = "ring-row",
        ring(100 * st$done_t / max(1, st$pend_t), "Text / names",
             paste0(st$done_t, " of ", st$pend_t, " decided"), PAL$primary),
        ring(100 * st$done_n / max(1, st$pend_n), "Nutrition",
             paste0(st$done_n, " of ", st$pend_n, " decided"), "#74A88F")),
      div(class = "next-up",
        div(div(class = "lbl", "Next up"),
            div(class = "val", if (nrow(nxt)) paste0(nxt$pid, " · ", nxt$name %||% nxt$cat_display) else "Queue complete")),
        actionButton("dash_go_validate2", "Continue", icon = icon("arrow-right"), class = "btn-dash-primary btn-sm"))
    )
  })
  observeEvent(input$dash_go_validate2, nav_select("main_nav", "Validation"))

  # Threshold trade-off — candidates per item vs threshold, both datasets
  output$dash_thr_badge <- renderText(paste0("Threshold ", input$threshold, "%"))
  output$dash_thr <- renderPlotly({
    te <- thr_eval_r()
    d <- bind_rows(te$text %>% mutate(set = "Text / names"), te$numeric %>% mutate(set = "Nutrition"))
    t <- input$threshold
    d$set <- factor(d$set, levels = c("Text / names", "Nutrition"))
    at <- d %>% filter(Threshold == t) %>% arrange(set)
    plot_ly(d, x = ~Threshold, y = ~cand_mean + 0.01, color = ~set, colors = c(PAL$primary, "#74A88F"),
            type = "scatter", mode = "lines+markers", line = list(width = 2.5, shape = "spline"),
            marker = list(size = 5),
            hovertemplate = "%{fullData.name}<br>%{x}%: %{customdata:.2f} candidates / item<extra></extra>",
            customdata = ~cand_mean) %>%
      layout(shapes = list(list(type = "line", x0 = t, x1 = t, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = PAL$accent, dash = "dash", width = 1.5))),
             annotations = if (nrow(at)) list(list(x = t, y = 1.02, yref = "paper", yanchor = "bottom",
               xanchor = if (t > 67) "right" else "left",
               text = paste0(t, "% · ", paste0(tolower(sub(" / names", "", as.character(at$set))), " ",
                             trimws(format(round(at$cand_mean, 1), big.mark = ",")), collapse = " · "), " per item"),
               showarrow = FALSE, font = list(size = 10, color = PAL$accent))) else NULL,
             xaxis = list(title = "Similarity threshold (%)", gridcolor = PAL$grid, zeroline = FALSE, dtick = 10),
             yaxis = list(title = "Candidates / item", type = "log", gridcolor = PAL$grid, zeroline = FALSE),
             legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 11)),
             margin = list(l = 10, r = 10, t = 24, b = 10)) %>%
      pal_plotly() %>% dash_animate(350)
  })

  # Completeness of pending records (% of pending rows with the field filled)
  output$dash_complete <- renderPlotly({
    pt <- text_data() %>% filter(active == 0); pn <- df_num %>% filter(active == 0)
    filled <- function(v) 100 * mean(!is.na(v) & nzchar(trimws(as.character(v))))
    d <- data.frame(
      field = c("Name", "Brand", "Category", "Barcode", "Category ", "All core nutrients"),
      set   = c("Text", "Text", "Text", "Text", "Nutrition", "Nutrition"),
      pct   = c(filled(pt$name), filled(pt$brands), filled(pt$categories), filled(pt$barcode),
                filled(pn$categories), 100 * mean(stats::complete.cases(pn[, CORE]))))
    d$lab <- paste0(d$set, " · ", trimws(d$field))
    d <- d[rev(seq_len(nrow(d))), ]
    plot_ly(d, y = ~factor(lab, levels = lab), x = ~pct, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$pct >= 70, PAL$primary, PAL$accent)),
            text = ~paste0(round(pct), "%"), textposition = "outside", cliponaxis = FALSE,
            textfont = list(size = 11, color = PAL$text),
            hovertemplate = "%{y}: %{x:.1f}% filled<extra></extra>") %>%
      layout(xaxis = list(title = "", range = c(0, 112), gridcolor = PAL$grid, zeroline = FALSE, ticksuffix = "%"),
             yaxis = list(title = "", automargin = TRUE), bargap = .4, showlegend = FALSE,
             margin = list(l = 10, r = 20, t = 5, b = 20)) %>%
      pal_plotly() %>% dash_animate(450)
  })

  # Recent decisions — decision mix + live list
  DEC_COL <- c(identical = PAL$slate, similar = "#74A88F", new = PAL$primary)
  DEC_LBL <- c(identical = "Identical duplicate", similar = "Similar but distinct", new = "New product")
  output$dash_mix <- renderPlotly({
    ld <- log_data()
    n <- vapply(names(DEC_LBL), function(k) sum(ld$decision == k), numeric(1))
    plot_ly(x = unname(DEC_LBL), y = n, type = "bar", marker = list(color = unname(DEC_COL)),
            text = n, textposition = "outside", cliponaxis = FALSE, textfont = list(size = 12, color = PAL$text),
            hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      layout(yaxis = list(title = "", range = c(0, max(3, n) * 1.25), gridcolor = PAL$grid, zeroline = FALSE, dtick = max(1, ceiling(max(3, n) / 3))),
             xaxis = list(title = "", tickfont = list(size = 10.5), categoryorder = "array", categoryarray = unname(DEC_LBL)),
             bargap = .45, showlegend = FALSE,
             transition = list(duration = 600, easing = "cubic-in-out"),
             margin = list(l = 10, r = 10, t = 10, b = 10)) %>%
      pal_plotly() %>% dash_animate(550)
  })
  output$dash_recent <- renderUI({
    ld <- log_data()
    if (!nrow(ld)) return(div(class = "dash-empty", icon("inbox"),
      div("No decisions yet this session."), div("Start validating and they will appear here live.")))
    ld <- tail(ld, 6)[rev(seq_len(min(6, nrow(ld)))), ]
    tags$ul(class = "recent-list", lapply(seq_len(nrow(ld)), function(i) {
      r <- ld[i, ]
      tags$li(style = sprintf("animation-delay:%.2fs", (i - 1) * .05),
        span(class = "dot", style = paste0("background:", DEC_COL[[r$decision]])),
        tags$b(r$pid), span(DEC_LBL[[r$decision]]),
        if (r$candidate != "—") span(class = "text-muted", paste0("→ ", r$candidate)),
        span(class = "t", r$time))
    }))
  })

  # Make sure there is something to show: pick the first queue item (and its
  # best candidate) if nothing is selected, then start once the UI has rendered.
  observeEvent(input$tour_btn, {
    nav_select("main_nav", "Validation")
    # Demo on the first queue item (on the current page) that has candidates
    if (is.null(sel_product())) {
      q <- filtered_pending()
      if (nrow(q) > 0) {
        has <- vapply(seq_len(min(nrow(q), 30)), function(i) nrow(cands_for(q[i, , drop = FALSE])) > 0, logical(1))
        i <- if (any(has)) which(has)[1] else 1
        selectPage(queue_proxy, ceiling(i / 8)); selectRows(queue_proxy, i)
        sel_product(q[i, , drop = FALSE])
      }
    }
    if (!is.null(sel_product()) && is.null(sel_candidate())) {
      cands <- tryCatch(cands_for(sel_product()), error = function(e) NULL)
      if (!is.null(cands) && nrow(cands) > 0) sel_candidate(cands[1, , drop = FALSE])
    }
    session$sendCustomMessage("tourDelay", 1200)
  })

  observeEvent(input$tour_ready, {
    has_cand <- !is.null(sel_candidate())
    # Steps target cards, not uiOutput()s: those are display:contents and have
    # no box for driver.js to highlight.
    # cicerone 1.0.4's own tab/tab_id switching throws "step is not defined"
    # (its hook is re-evaluated out of scope), so switch bslib tabs ourselves —
    # via the Bootstrap Tab API, not .click(): driver.js sees a document click
    # and its step counter drifts.
    to_tab <- function(tab) sprintf(
      "function(el){var a=document.querySelector('#main_nav .nav-link[data-value=\"%s\"]');if(a&&!a.classList.contains('active')){bootstrap.Tab.getOrCreateInstance(a).show();}}",
      tab)
    g <- Cicerone$new(allow_close = TRUE, padding = 6, opacity = .55)
    g$step("filter_bar", "1 · Filter the queue",
           "Pick the dataset and a category, sort the queue, and set the match threshold. The KPIs on the right update live.",
           position = "bottom", on_highlight_started = to_tab("Validation"))
    g$step("queue_table", "2 · Choose a pending product",
           "Click a row to load it. Tip: on Data Quality, clicking a bar or heatmap cell jumps here pre-filtered.",
           position = "right", on_highlight_started = to_tab("Validation"))
    if (!is.null(sel_product())) {
      g$step("step2_card", "3 · Inspect candidates",
             "Closest validated records above the threshold, with barcode and confidence. Click one to compare.",
             position = "right", on_highlight_started = to_tab("Validation"))
    }
    if (has_cand) {
      g$step("score_strip", "4 · Why this score?",
             "Hover each segment to see how name, brand and category contribute to the similarity score.",
             position = "right", on_highlight_started = to_tab("Validation"))
      g$step("step3_card", "5 · Compare text, flags & nutrition",
             "Terracotta = differs, green = matches, grey — = missing. Terracotta flags (e.g. pudding, light, a flavour) on only one side usually mean a different formulation.",
             position = "left", on_highlight_started = to_tab("Validation"))
    }
    if (!is.null(sel_product())) {
      g$step("step4_card", "6 · Confirm a decision",
             "Click a card or press 1, 2 or 3 — the 'Suggested' pill is a hint, nothing saves until you confirm. Enter saves and loads the next unreviewed product. The panel under the cards changes with your choice.",
             position = "left", on_highlight_started = to_tab("Validation"))
    }
    g$step("tuner_settings", "7 · Tune the threshold",
           "This slider is synced with the Validation tab. Watch candidates per item and measured precision change.",
           position = "right", on_highlight_started = to_tab("Threshold Tuner"))
    g$step("curve_card", "8 · Read the curve",
           "The terracotta callout follows your threshold; the green diamond marks the sweet spot where raising the threshold stops paying off.",
           position = "bottom", on_highlight_started = to_tab("Threshold Tuner"))
    g$init()$start()
  })
}

# -- 5. LAUNCH -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
