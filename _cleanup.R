# Cleanup script for Quarto
files_to_remove <- list.files(
  pattern = "\\.(bst|spl|cls|ipynb|DS_Store)$"
)

# Iterate over files and delete them if they exist
for (file in files_to_remove) {
  if (file.exists(file)) {
    file.remove(file)
    cat("Deleted:", file, "\n")
  } else {
    cat("File not found:", file, "\n")
  }
}

# Fix the alignment of tfoot in the Quarto-generated HTML

# Path to the HTML file
html_file <- "_manuscript/index.html"

# Check if the file exists
if (file.exists(html_file)) {
  # Read the file content
  html_content <- readLines(html_file)
  
  # Locate the line with <tfoot class="gt_footnotes">
  html_content <- gsub(
    pattern = '<tfoot class="gt_footnotes">',
    replacement = '<tfoot class="gt_footnotes" style="text-align: left;">',
    x = html_content
  )
  
  # Write the updated content back to the file
  writeLines(html_content, html_file)
  cat("Updated text alignment in:", html_file, "\n")
} else {
  cat("File not found:", html_file, "\n")
}

# Copy over the docx manuscript
# source_file <- "_submission/Revision 1/article_rev1.docx"
# destination_file <- "_manuscript/jamil2025archives.docx"
# file.copy(source_file, destination_file, overwrite = TRUE)