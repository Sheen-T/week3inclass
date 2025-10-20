
library(table1)
library(Hmisc)
library(kableExtra)
library(dplyr)
library(webshot2)

conflict <- read.csv("data/analytical/finaldataset.csv", header = TRUE, check.names = FALSE)


if ("armed_conflict" %in% names(conflict)) {
  conflict$armed_conflict <- factor(conflict$armed_conflict,
                                    levels = c(0,1,"0","1","No","Yes"),
                                    labels = c("No","Yes","No","Yes","No","Yes"))
} else {
  # create a single-level factor so the code still runs
  conflict$armed_conflict <- factor("Total")
}

nm_map <- c(
  maternal_mortality = "maternal_mortality",
  infant_mortality   = "infant_mortality",
  neonatal_mortality = "neonatal_mortality",
  under5_mortality   = "under5_mortality"
)
norm <- function(x) gsub("[ _]", "", tolower(x))
for (j in names(nm_map)) {
  if (!nm_map[j] %in% names(conflict)) {
    hit <- names(conflict)[norm(names(conflict)) == norm(j)]
    if (length(hit)) nm_map[j] <- hit[1]
  }
}
missing_cols <- nm_map[!nm_map %in% names(conflict)]
if (length(missing_cols)) stop("Missing columns in CSV. Edit nm_map: ", paste(names(missing_cols), collapse = ", "))

# ---- Labels that include missing counts (so they show in the table) ----
add_missing_to_label <- function(x, base_label) {
  nmis <- sum(is.na(x))
  sprintf("%s (missing: %s)", base_label, format(nmis, big.mark = ","))
}
label(conflict[[nm_map["maternal_mortality"]]]) <- add_missing_to_label(conflict[[nm_map["maternal_mortality"]]], "Maternal mortality")
label(conflict[[nm_map["infant_mortality"]]])   <- add_missing_to_label(conflict[[nm_map["infant_mortality"]]],   "Infant mortality")
label(conflict[[nm_map["neonatal_mortality"]]]) <- add_missing_to_label(conflict[[nm_map["neonatal_mortality"]]], "Neonatal mortality")
label(conflict[[nm_map["under5_mortality"]]])   <- add_missing_to_label(conflict[[nm_map["under5_mortality"]]],   "Under 5 mortality")

vars_rhs <- paste(
  c(nm_map["maternal_mortality"], nm_map["infant_mortality"],
    nm_map["neonatal_mortality"], nm_map["under5_mortality"]),
  collapse = " + "
)
fml <- as.formula(paste0("~ ", vars_rhs, " | armed_conflict"))

mytable1 <- table1(
  fml,
  data = conflict,
  render.continuous = c(. = "Median [Min, Max]"),
  overall = c(left = "Total")
)

kbl_obj <- t1kable(mytable1) %>%
  add_header_above(c(" " = 2, "Armed conflict" = 2)) %>%
  kable_styling(full_width = FALSE)

print(kbl_obj)  # ensures it shows up in RStudio Viewer

html_path <- "table1_outcomes_by_conflict.html"
save_kable(kbl_obj, file = html_path)

pdf_path <- "table1_outcomes_by_conflict.pdf"
if (requireNamespace("pagedown", quietly = TRUE)) {
  pagedown::chrome_print(input = html_path, output = pdf_path)
} else if (requireNamespace("webshot2", quietly = TRUE)) {
  webshot2::webshot(url = html_path, file = pdf_path, vwidth = 1300, vheight = 900, zoom = 1)
}

