




library(htmltools)


# Filter for the top 10 BPS names
top_bps <- bps_data %>%
  group_by(BPS_NAME) %>%
  summarize(ACRES = sum(ACRES),
            REL_PERCENT = sum(REL_PERCENT)) %>%
  arrange(desc(REL_PERCENT)) %>%
  top_n(n = 10, wt = REL_PERCENT)

# Get unique model codes for the top 10 BPS names
unique_model_codes <- unique(bps_data$BPS_MODEL[bps_data$BPS_NAME %in% top_bps$BPS_NAME])

# Filter the dataframe to include only rows with unique model codes and top 10 BPS names
bps_urls <- bps_data %>% 
  filter(BPS_NAME %in% top_bps$BPS_NAME) %>%
  distinct(BPS_MODEL, .keep_all = TRUE) %>%
  select(c(BPS_NAME, BPS_MODEL))

# URL template
url_template <- "https://github.com/rswaty/bps_docs_parse/raw/main/all_bps_docs/10080_1_2_3_7.docx"

# Generate URLs by replacing the model_code in the URL template
urls <- sapply(unique_model_codes, function(code) {
  gsub("10080_1_2_3_7", paste0(code), url_template)
})

# Create clickable hyperlinks with BPS names
clickable_names <- sapply(seq_along(urls), function(i) {
  paste0('<a href="', urls[i], '" target="_blank">', bps_urls$BPS_NAME[i], '</a>')
})

# Render the list of hyperlinked BPS names as HTML
html_output <- HTML(paste(clickable_names, collapse = "<br>"))

# Print the HTML output
html_output

