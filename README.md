# Food Product Validation Dashboard

Interactive decision-support dashboard developed for the **Professional Bachelor in Applied Computer Science** (*Data Visualisation & Analytics*).

---

##  Architecture & User Stories Implementation

The dashboard directly implements the reactive architecture defined in our **Mermaid reactive diagram** and answers user questions **Q1 through Q8**:

| View / Module | User Questions | User Stories | Key Components & Techniques |
|---|---|---|---|
| **1. Review Queue & Matching** | **Q1, Q2, Q3, Q4, Q6, Q5** | **S1, S2, S3, S4, S5, S6** | • `output$queue_bar` (Q1): Category breakdown of active=0 items.<br>• `output$queue_table` (Q1): Sortable DT pending table with row-click selection.<br>• `output$text_table` (Q2): Fuzzy matching (TF-IDF + Jaro-Winkler) + exact barcode match rule.<br>• `output$numeric_table` (Q3): kNN / normalized Euclidean distance on 6 core nutrients.<br>• `output$nutrient_bar` (Q3/Q4): Diverging bar chart showing nutrient differences (Pending - Candidate).<br>• `output$detail_panel` (Q4): Side-by-side comparison cards.<br>• `output$decision_panel` (Q6): 3-choice disposition radio + confirmation action.<br>• `output$completion_table` (Q5): `eventReactive` online lookup (Open Food Facts / Nubel reference) with source links. |
| **2. Threshold Tuner & Evaluation** | **Q7** | **S7, S8** | • `output$pr_curve` (Q7): Sensitivity and workload tradeoff curve with dynamic threshold vertical line.<br>• Real-time KPI cards: Estimated candidate yield & expected duplicate precision. |
| **3. Data Quality & Missingness** | **Q8** | **S9** | • `output$missing_heatmap` (Q8): Interactive completeness matrix comparing pending (`active = 0`) vs validated (`active = 1`) records across all fields. |

---

##  How to Run the Dashboard

### Option A: From Terminal
Simply run the launcher script:
```bash
./run_dashboard.sh
```
Or run directly via R:
```bash
R -e "shiny::runApp('.', port=8080, launch.browser=TRUE)"
```

### Option B: In RStudio
1. Open the project folder in RStudio.
2. Open `app.R`.
3. Click the **"Run App"** button at the top right of the editor.

---

##  Required R Packages
All dependencies are installed:
- `shiny`
- `bslib`
- `DT`
- `plotly`
- `ggplot2`
- `dplyr`
- `tidyr`
- `stringdist`
- `httr`
- `jsonlite`
- `readxl`
