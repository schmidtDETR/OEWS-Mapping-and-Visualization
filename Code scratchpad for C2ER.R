# Clean, Filter, Reorder Data #----
state_code_lookup <- fips_codes %>% # Use dataset from tigris that has fips and names
  select(state_code, state_name) %>% # Don't need MSA data
  distinct() %>% # Get only unique rows
  mutate(state_code = as.integer(state_code)) # make code an integer to join to OEWS

oews_data <- oews_import %>%
  mutate(
    value = as.numeric(value), # make values numeric
    is_state = if_else(state_code == "32", 0,1), # sortable Nevada filter
    is_state_name = if_else(areatype_code == "S", 0, 1), # Is it a statewide area?
    area_name = if_else(is_state_name == 0, paste0(area_name, " Statewide"), area_name) # Change name
  ) %>%
  filter(area_name != "National") %>% # Removing national data
  select(area_name, year, datatype_name, occupation_name,
         state_code, area_code, value, is_state, is_state_name) %>% # Choosing only certain columns.
  pivot_wider(names_from = datatype_name, values_from = value) %>% # Putting data types in columns
  clean_names() %>% # Standardize column names with lowercase and underscores
  arrange(is_state, is_state_name) %>% # sort by Nevada, then by statewide/MSA
  select(is_state, occupation_name, area_name, area_code, state_code, employment, hourly_10th_percentile_wage, hourly_25th_percentile_wage, hourly_mean_wage, hourly_median_wage, hourly_75th_percentile_wage, hourly_90th_percentile_wage) %>%
  left_join(state_code_lookup, by = "state_code") # add in state names for all areas

# GT Table #----

selected_areas <- c("Nevada Statewide", "Carson City, NV", "Las Vegas-Henderson-North Las Vegas, NV", "Reno, NV",
                    "Balance of Nevada nonmetropolitan area", "Idaho Statewide", "Boise City, ID", "Twin Falls, ID",
                    "Utah Statewide", "Salt Lake City-Murray, UT", "Arizona Statewide", "Phoenix-Mesa-Chandler, AZ",
                    "Flagstaff, AZ", "Oregon Statewide", "Eastern Oregon nonmetropolitan area", "San Jose-Sunnyvale-Santa Clara, CA",
                    "Sacramento-Roseville-Folsom, CA", "Fresno, CA", "Los Angeles-Long Beach-Anaheim, CA", "Chico, CA",
                    "Bakersfield-Delano, CA", "California Statewide")

oews_data %>%
  filter(occupation_name == "Carpenters",
         area_name %in% selected_areas) %>%
  arrange(is_state, -employment) %>%
  gt()


oews_data %>%
  filter(occupation_name == "Carpenters",
         area_name %in% selected_areas) %>%
  arrange(is_state, -employment) %>%
  gt(groupname_col = "state_name") %>%
  cols_hide(
    columns = c("area_code", "state_code", "occupation_name", "is_state")
  ) %>%
  fmt_number(
    columns = employment,
    decimals = 0
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
    columns = c(hourly_10th_percentile_wage,
                hourly_25th_percentile_wage,
                hourly_median_wage,
                hourly_75th_percentile_wage,
                hourly_90th_percentile_wage)
  ) %>%
  tab_header(
    title = "Data for Carpenters in 2024"
  ) %>%
  tab_source_note(
    source_note = "Data from U.S. Bureau of Labor Statistics, Occupational Employment and Wage Statistics"
  ) %>%
  cols_label(
    area_name = "Area",
    employment = "Employment",
    hourly_10th_percentile_wage = "10th",
    hourly_25th_percentile_wage = "25th",
    hourly_mean_wage = "Mean Wage",
    hourly_median_wage = "Median",
    hourly_75th_percentile_wage = "75th",
    hourly_90th_percentile_wage = "90th"
  ) %>%
  cols_move(
    columns = hourly_mean_wage,
    after = employment
  )




oews_data %>%
  filter(occupation_name == "Carpenters",
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
    title = "Median Wage for Carpenters",
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




oews_map %>%
  filter(occupation_name == "Carpenters") %>%
  ggplot() + 
  geom_sf(aes(fill = hourly_median_wage)) +
  labs(
    title = "Median Hourly Wage for Carpenters",
    subtitle = "Nevada and neighboring states",
    caption = "Data from U.S. Bureau of Labor Statistics, Occupational Employment and Wage Statistics",
    fill = NULL
  ) +
  scale_fill_viridis_c(
    guide = guide_colorbar(
      barwidth = 20, barheight = 1
      ),
    labels = dollar)+
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white", 
      color = NA),
    plot.margin = margin(0, 0, 0, 0),  # remove all margins
    legend.position = "bottom",
    axis.text = element_blank()
  ) +
  coord_sf(expand = FALSE)



add_5 <- function(input_number = 10){
  input_number + 5
}
add_5()
add_5(25)





# GT Table #----

generate_gt_for_occ <- function(occ_name="All Occupations") {
  
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

  return(occ_gt)
}

