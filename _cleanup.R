# Cleanup script for Quarto
files_to_remove <- c("elsarticle-num.bst", "manuscript.spl", "elsarticle.cls")

# Iterate over files and delete them if they exist
for (file in files_to_remove) {
  if (file.exists(file)) {
    file.remove(file)
    cat("Deleted:", file, "\n")
  } else {
    cat("File not found:", file, "\n")
  }
}