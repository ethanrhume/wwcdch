scripts <- list.files("scripts", pattern = "^\\d+_.*\\.R$", full.names = TRUE)
scripts <- scripts[!grepl("00_run_all\\.R$", scripts)]
scripts <- c(scripts, "scripts/report.Rmd")

# All of the below places a commented timestamp at the end of report.Rmd
# to fix a caching issue that broke plots each time it was reloaded

rmd_path <- "scripts/report.Rmd"
rmd_lines <- readLines(rmd_path)

comment_prefix <- "<!-- Last run: "
comment_suffix <- " -->"

# Find existing comment line (anywhere in file)
line_idx <- grep(paste0("^", comment_prefix), rmd_lines)

timestamp_line <- paste0(comment_prefix, Sys.time(), comment_suffix)

if (length(line_idx) == 1) {
  # Replace existing comment line
  rmd_lines[line_idx] <- timestamp_line
} else {
  # Append new comment line at end of file
  rmd_lines <- c(rmd_lines, "", timestamp_line)
}

writeLines(rmd_lines, rmd_path)
cat("Updated invisible timestamp comment at bottom of Rmd\n")

for (f in scripts[!grepl("\\.Rmd$", scripts)]) {
  cat("\nRunning", f, "\n")
  source(f, local = new.env())
}

rmd_files <- scripts[grepl("\\.Rmd$", scripts)]
if (length(rmd_files) > 0) {
  cat("\nRunning", rmd_files, "\n")
  unlink("__files__", recursive = TRUE, force = TRUE)
  unlink("__shinytemp__", recursive = TRUE, force = TRUE)
  rmarkdown::run(rmd_files[1], shiny_args = list(launch.browser = TRUE))
}
