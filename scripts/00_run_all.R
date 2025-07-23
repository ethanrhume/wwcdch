scripts <- list.files("scripts", pattern = "^\\d+_.*\\.R$", full.names = TRUE)
for (f in scripts) {
  message("Running ", f)
  source(f)
}