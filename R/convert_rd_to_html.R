

#' Convert Rd to HTML with Custom CSS
#'
#' Converts an Rd file to an HTML file and adds a custom CSS link to the HTML header.
#'
#' @param rd_file A string specifying the name of the Rd file (with extension) located in the "man" directory.
#' @param output_dir A string specifying the directory where the HTML file will be saved. Defaults to "man/html-Rd".
#' @param css_file A string specifying the path to the CSS file that will be linked in the HTML header. Defaults to "man/html-Rd/styles.css".
#'
#' @return None. The function is used for its side effect of generating an HTML file from an Rd file.
#'
#' @examples
#' convert_rd_to_html("strsplit1.Rd")
#' convert_rd_to_html("strsplit1.Rd", output_dir = "man/html-Rd", css_file = "man/html-Rd/styles.css")
#'
#' @export
convert_rd_to_html <- function(rd_file_name) {
  # Define the input Rd file path
  rd_file_path <- file.path("man", rd_file_name)
  
  # Define the output HTML file path
  html_file_name <- sub("\\.Rd$", ".html", rd_file_name)
  html_file_path <- file.path("man/html-Rd", html_file_name)
  
  # Convert the Rd file to HTML
  tools::Rd2HTML(rd_file_path, out = html_file_path)
  
  # Read the generated HTML file
  html_content <- readLines(html_file_path)
  
  # Define the CSS link to be inserted in the header
  css_link <- '<link rel="stylesheet" href="styles.css" />'
  
  # Insert the CSS link into the HTML header (after the <head> tag)
  head_index <- grep("<head>", html_content, fixed = TRUE)
  if (length(head_index) > 0) {
    html_content <- append(html_content, css_link, after = head_index)
  }
  
  # Write the modified HTML content back to the file
  writeLines(html_content, html_file_path)
  
  # Message to confirm the process is complete
  message(paste("HTML file created at:", html_file_path))
}

# Example usage
# convert_rd_to_html("strsplit1.Rd")
