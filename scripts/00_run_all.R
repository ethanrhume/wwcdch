# Defines a list of all scripts using standardized notation for this project
# Runs all of them along with a message

scripts <- list.files("scripts", pattern = "^\\d+_.*\\.R$", full.names = TRUE)
for (f in scripts) {
  message("Running ", f)
  source(f)
}