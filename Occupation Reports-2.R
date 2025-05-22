# Libraries #----

library(tidyverse)
library(data.table)
library(httr)
library(janitor)
library(gt)
library(furrr)
library(future)
library(tidycensus)
library(tigris)
library(readxl)
library(sf)
library(scales)
library(tictoc)

setwd("***YOUR WORKING DIRECTORy HERE***")
tic()
# Read in BLS Data #----
fread_bls <- function(url){
  
  headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "en-US,en;q=0.9",
    "Connection" = "keep-alive",
    "Host" = "download.bls.gov",
    "Referer" = "https://download.bls.gov/pub/time.series/",
    "Sec-Ch-Ua" = 'Not_A Brand";v="8", "Chromium";v="120", "Google Chrome";v="120"',
    "Sec-Ch-Ua-Mobile" = "?0",
    "Sec-Ch-Ua-Platform" = '"Windows"',
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "same-origin",
    "Sec-Fetch-User" = "?1",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  )
  
  response <- GET(url, add_headers(.headers = headers))
  
  # Check for successful response
  stop_for_status(response)
  
  # Use binary mode to avoid building a giant character object
  raw_data <- content(response, as = "raw")
  
  # Write to a temporary file to avoid loading it all into memory as a string
  temp_file <- tempfile(fileext = ".csv")
  writeBin(raw_data, temp_file)
  
  # fread directly from the file (very efficient)
  return_data <- fread(temp_file)
  
  return(return_data)
}

oews_current <- fread_bls("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current")
oews_series <- fread_bls("https://download.bls.gov/pub/time.series/oe/oe.series")
oews_occupation <- fread_bls("https://download.bls.gov/pub/time.series/oe/oe.occupation")
oews_area <- fread_bls("https://download.bls.gov/pub/time.series/oe/oe.area")
oews_datatype <- fread_bls("https://download.bls.gov/pub/time.series/oe/oe.datatype")

oews_import <- oews_current %>% select(-footnote_codes) %>%
  left_join(oews_series) %>% select(-footnote_codes) %>%
  left_join(oews_occupation) %>%
  left_join(oews_area) %>%
  left_join(oews_datatype)

# Set series and area filters #----
# This shows a couple of options for filtering, by words within job title or SOC code.
selected_series <- oews_import %>% select(occupation_code, occupation_name) %>% distinct() %>% filter(str_detect(occupation_name, "omputer")) %>% pull(occupation_code)
# selected_series <- c("472031", "472061", "472111", "471011", "472152", "119021", "472141", "472073", "119199", "439061", "472051", "499021", "472081")

state_names <- state.name

# Manually populated list of areas from OEWS for inclusion in GT table and bar chart
selected_areas <- c("Nevada Statewide", "Carson City, NV", "Las Vegas-Henderson-North Las Vegas, NV", "Reno, NV", "Balance of Nevada nonmetropolitan area",
                    "Idaho Statewide", "Boise City, ID", "Twin Falls, ID", "Utah Statewide", "Salt Lake City-Murray, UT", "Arizona Statewide", "Phoenix-Mesa-Chandler, AZ", "Flagstaff, AZ", "Oregon Statewide", "Eastern Oregon nonmetropolitan area",
                    "San Jose-Sunnyvale-Santa Clara, CA", "Sacramento-Roseville-Folsom, CA", "Fresno, CA", "Los Angeles-Long Beach-Anaheim, CA", "Chico, CA", "Bakersfield-Delano, CA", "California Statewide")

state_code_lookup <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct() %>%
  mutate(state_code = as.integer(state_code))

# Clean, Filter, Reorder Data #----
oews_data <- oews_import %>%
  mutate(is_state = if_else(state_code == "32", 0,1),
         is_state_name = if_else(area_name %in% state_names, 0, 1),
         area_name = if_else(is_state_name == 0, paste0(area_name, " Statewide"), area_name)
         ) %>%
  filter(
    #occupation_code %in% selected_series,
    area_name != "National"
    ) %>%
  select(area_name, year, datatype_name, occupation_name, state_code, area_code, value, is_state, is_state_name) %>%
  pivot_wider(names_from = datatype_name, values_from = value) %>%
  clean_names() %>%
  arrange(is_state, is_state_name) %>%
  select(is_state, occupation_name, area_name, area_code, state_code, employment, hourly_10th_percentile_wage, hourly_25th_percentile_wage, hourly_mean_wage, hourly_median_wage, hourly_75th_percentile_wage, hourly_90th_percentile_wage) %>%
  mutate(
    across(employment:hourly_90th_percentile_wage, .fns = as.numeric)
  ) %>%
  left_join(state_code_lookup, by = "state_code")
toc()


# GT Table #----

generate_gt_for_occ <- function(occ_name="All Occupations") {
  
  safe_occ_name <- gsub("[^A-Za-z0-9 _-]", "", occ_name)

occ_gt <- oews_data %>%
  filter(occupation_name == occ_name,
         area_name %in% selected_areas) %>%
  arrange(is_state, -employment) %>%
  gt(groupname_col = "state_name") %>%
  cols_hide(
    columns = c("area_code", "state_code", "occupation_name", "is_state")
  ) %>%
  fmt_number(
    columns = employment, decimals = 0
  ) %>%
  fmt_currency(
    columns = c(contains("_wage")),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgrey"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_spanner(
    label = "Hourly Wages by Percentile",
    columns = c(hourly_10th_percentile_wage, hourly_25th_percentile_wage,
                hourly_median_wage,hourly_75th_percentile_wage,
                hourly_90th_percentile_wage)
  ) %>%
  tab_header(
    title = paste0("Data for ",occ_name," in 2024")
  ) %>%
  tab_source_note(
    source_note = "Data from U.S. Bureau of Labor Statistics, Occupational Employment and Wage Statistics"
  ) %>%
  cols_label(
    area_name = "Area",
    employment = "Employment",
    hourly_10th_percentile_wage = "10th", hourly_25th_percentile_wage = "25th",
    hourly_mean_wage = "Mean Wage", hourly_median_wage = "Median",
    hourly_75th_percentile_wage = "75th",hourly_90th_percentile_wage = "90th"
  ) %>%
  cols_move(
    columns = hourly_mean_wage,
    after = employment
  )
    gtsave(data = occ_gt, filename = paste0("GT Tables/Wage Table for ",safe_occ_name," 2024.html"))
    return(occ_gt)
}


generate_gt_for_occ("Carpenters")

lv_occs <- oews_data %>% 
  filter(area_name == "Las Vegas-Henderson-North Las Vegas, NV") %>%
  pull(occupation_name) %>%
  unique()

tic()
walk(lv_occs, generate_gt_for_occ)
toc()

plan(multisession, workers = 10)

tic()
future_walk(lv_occs, generate_gt_for_occ, .progress = TRUE, .options = furrr_options(seed=1138))
toc()

# Wage Bar Chart #---- 

generate_bar_chart_for_occ <- function(occ_name="All Occupations") {
  
  safe_occ_name <- gsub("[^A-Za-z0-9 _-]", "", occ_name)
  
bar_occ <- oews_data %>%
  filter(occupation_name == occ_name,
         area_name %in% selected_areas) %>%
  mutate(
    area_type = case_when(
      str_detect(area_name, " Statewide") ~ "state",
      str_detect(area_name, "nonmetropolitan") ~ "non_msa",
      TRUE ~ "msa"
    )
  ) %>%
  ggplot(aes(x = hourly_median_wage,
             y = reorder(area_name, hourly_median_wage))) +
  geom_col(aes(fill = area_type)) +
  geom_label(aes(label = hourly_median_wage)) +
  labs(
    title = paste0("Median Wage for ", occ_name),
    caption = "Data from U.S. Bureau of Labor Statistics, Occupational Employment and Wage Statistics",
    x = NULL, y = NULL
  ) +
  facet_wrap(~state_name, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = dollar) +
  scale_fill_manual(
    values = c(
      state = "#66c2a5",   # soft green
      msa = "#8da0cb",     # soft blue-purple
      non_msa = "#fc8d62"  # muted orange
    )
  ) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face="bold")
  )

return(bar_occ)

}

generate_bar_chart_for_occ("Carpenters")

# Generate Map #----


library(cowplot)
library(grid)
library(gtable)




generate_oews_maps <- function(plot_data, show_map=FALSE) {
  
occ_name <- plot_data %>% pull(occupation_name) %>% unique()

base_map <- ggplot(plot_data) + 
  geom_sf(aes(fill = hourly_median_wage)) +
  labs(
    title = paste0("Median Hourly Wage for ", occ_name),
    subtitle = "Nevada and neighboring states",
    caption = "Data from U.S. Bureau of Labor Statistics, Occupational Employment and Wage Statistics",
    fill = NULL
  ) +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 20, barheight = 1), labels = dollar)+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0),  # remove all margins
    legend.position = "bottom",
    axis.text = element_blank()
  ) +
  coord_sf(expand = FALSE)

safe_occ_name <- gsub("[^A-Za-z0-9 _-]", "", occ_name)

ggsave(
  filename = paste0("Occupation Maps/Wages for ", safe_occ_name, ", Western US, 2024.png"),
  plot = base_map,
  width = 6, height = 9, dpi = 300
)

if(show_map){return(base_map)}

}


area_definitions <- read_excel("area_definitions_m2024.xlsx") %>%
  clean_names() %>%
  mutate(GEOID = paste0(fips,county_code)) %>%
  select(new_area, GEOID, msa)

area_shapes <- counties()

oews_areas <- area_shapes %>%
  left_join(area_definitions, by = "GEOID") %>%
  group_by(msa, new_area) %>%
  summarize(geometry = st_union(geometry))


lv_occs <- oews_data %>% 
  filter(area_name == "Las Vegas-Henderson-North Las Vegas, NV") %>%
  pull(occupation_name) %>%
  unique()

oews_msas <- oews_data %>%
  filter(
    state_code %in% c(4, 6, 16, 32, 41, 49), 
    !(area_name %in% state_names)) %>%
  mutate(
    across(contains("_wage"), .fns = as.numeric)
  )

oews_map <- oews_areas %>%
  mutate(new_area = as.integer(new_area)) %>%
  inner_join(oews_msas, by = c("new_area" = "area_code"))

lv_occ_data <- oews_map %>%
  filter(occupation_name %in% lv_occs) %>%
  group_by(occupation_name) %>%
  group_split()

tic()
plan(multisession, workers=10)
future_map(lv_occ_data, generate_oews_maps, .progress = TRUE, .options = furrr_options(seed=1138))
toc()

tic()
map(lv_occ_data, generate_oews_maps, .progress = TRUE, .options = furrr_options(seed=1138))
toc()



# Single Occ Map Example
single_occ <- oews_map %>%
  filter(occupation_name == "First-Line Supervisors of Transportation Workers, Except Aircraft Cargo Handling Supervisors")

generate_oews_maps(single_occ, show_map = TRUE)





# Stage Iterative Reports #----

save(oews_data, oews_areas, file = "Occupation Data2.RData")


render_occ = function(occ_name) {
  
  safe_occ_name <- gsub("[^A-Za-z0-9 _-]", "", occ_name)
  
  rmarkdown::render(
    "Occupation Report.RMD", 
    params = list(
      occ_name = occ_name
    ),
    output_file = paste0("Individual Occupations/Report for ", safe_occ_name, ".html")
  )
}

tic()
walk(lv_occs, render_occ)
toc()

