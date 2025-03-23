library(tidyverse)
library(fixest)

# financial health measures used:

  # Liquidity Measures
  # current ratio: current asset / current liabilities
  # current ratio_unrestricted: current_assets_total_over_current_liabilities_unrestricted
  # Cash availability : months_of_operating_cash_total
  # Cash availability: months_of_operating_cash_unrestricted
  
  
  # Solvency Measures
  # debt service impact: total debt service expense / total expense
  # leverage ratio: total loan / total asset
  # inverse of debt service coverage ratio: total debt service expense / net operating income
  
  
  # Sustainability Measures
  # unrestricted net assets / total operating expenses
  # unrestricted program revenue / total expenses



################################ Data Cleaning #################################
nonprofit <- read.csv("All Completes 02.04.2022.csv")
# organizations_id is the unique id

nonprofit_sub <- nonprofit %>%
  select(c(organizations_id, fiscal_year, year, profile_state, organizations_name, 
           organizations_longitude, organizations_latitude,
           full_time_employees_permanent,
           profile_org_year_founded,
           profile_cdp_taxonomy_name,
           total_earned_revenue_operating_program,      # has five negative values--convert to NA
           total_earned_revenue_operating_non_program,  # has many large negative values
           total_earned_revenue_operating,
           total_contributed_revenue_operating, #has one negative value--convert to NA
           investment_income_operating_total,  # has many large negative values
           total_operating_revenue_formula,    # has many large negative values due to negative investment income
           total_operating_revenue_unrestricted, # has many large negative values
           total_revenue_formula, # has many large negative values
           total_net_assets, 
           total_net_assets_unrestricted, 
           total_liabilities_formula, 
           total_interest_expense_formula, debt_service_impact,
           loans_current_portion_total_formula,
           current_assets_total, current_liabilities_total, cash_equivalents_total_formula,
           current_assets_total_over_current_liabilities_total, # has many large negative values
           current_assets_total_over_current_liabilities_unrestricted,
           total_operating_expenses_formula, operating_margin,
           total_expenses_formula,
           total_employees_people, fundraising_efficiency,
           leverage_ratio,
           months_of_operating_cash_total,
           months_of_operating_cash_unrestricted,
           self_sufficiency_ratio)) %>%
  mutate(
    # remove negative values in earned revenue and contributions for creating commercialization measure
    total_earned_revenue_operating_program = ifelse(total_earned_revenue_operating_program < 0, NA, total_earned_revenue_operating_program),
    total_contributed_revenue_operating = ifelse(total_contributed_revenue_operating < 0, NA, total_contributed_revenue_operating)
  ) %>%
  arrange(organizations_id, year) 


# Create balanced panel data
# Step 1: Identify organizations with data for at least 5 years from 2011 to 2019 (This time period covers 2714 organizations)
complete_orgs <- nonprofit_sub %>%
  filter(year >= 2011 & year <= 2019) %>%
  group_by(organizations_id) %>%
  summarize(year_count = n(),
            #has_all_years = n_distinct(year) == 11,
            min_year = min(year),
            max_year = max(year)) %>%
  filter(#has_all_years == TRUE & 
           min_year <= 2011 & 
           max_year >= 2019 & 
           year_count >= 5) %>%
  pull(organizations_id)

# Step 2: Create the complete balanced panel and create additional variables
balanced_panel_2009_2019 <- nonprofit_sub %>%
  filter(organizations_id %in% complete_orgs & 
           year >= 2011 & 
           year <= 2019) %>%
  arrange(organizations_id, year) %>%
  mutate(
    total_operating_revenue_exclude_investment = total_earned_revenue_operating_program + total_contributed_revenue_operating,
    commercialization = total_earned_revenue_operating_program/total_operating_revenue_exclude_investment,
    debt_service_coverage_ratio_inverse = (loans_current_portion_total_formula + total_interest_expense_formula) / (total_operating_revenue_formula - (total_operating_expenses_formula - total_interest_expense_formula)),
    unres_net_asset_ratio = total_net_assets_unrestricted / total_operating_expenses_formula,
    unres_program_revenue_ratio = total_operating_revenue_unrestricted / total_expenses_formula
    ) 


panel_check <- balanced_panel_2009_2019 %>%
  group_by(organizations_id) %>%
  summarize(year_count = n()) %>%
  summarize(
    org_count = n(),
    all_have_5plus_years = all(year_count >= 5)
  )



# Function create a histogram between 5th and 95th percentiles
plot_percentile_histogram <- function(data, variable_name, bins = 30, title = NULL) {
  # Compute the 5th and 95th percentiles
  p05 <- quantile(data[[variable_name]], 0.05, na.rm = TRUE)
  p95 <- quantile(data[[variable_name]], 0.95, na.rm = TRUE)
  
  # Subset the data to include only values between 5th and 95th percentiles
  filtered_data <- data[[variable_name]][data[[variable_name]] >= p05 & data[[variable_name]] <= p95]
  
  # Create a title if none is provided
  if (is.null(title)) {
    title <- paste("Histogram of", variable_name, "(5th-95th percentiles)")
  }
  
  # Create the histogram
  hist(filtered_data, 
       breaks = bins,
       main = title,
       xlab = variable_name,
       col = "skyblue",
       border = "white")
  
  # Add vertical lines for the percentiles
  abline(v = p05, col = "red", lty = 2, lwd = 2)
  abline(v = p95, col = "red", lty = 2, lwd = 2)
  
  # Add a legend
  legend("topright", 
         legend = c("5th percentile", "95th percentile"), 
         col = c("red", "red"),
         lty = c(2, 2),
         lwd = c(2, 2))
}


# Liquidity Measures
# current ratio
plot_percentile_histogram(balanced_panel_2009_2019, "current_assets_total_over_current_liabilities_total") 
# current ratio_unrestricted
plot_percentile_histogram(balanced_panel_2009_2019, "current_assets_total_over_current_liabilities_unrestricted") 
# Cash availability
plot_percentile_histogram(balanced_panel_2009_2019, "months_of_operating_cash_total")
# Cash availability
plot_percentile_histogram(balanced_panel_2009_2019, "months_of_operating_cash_unrestricted")


# Solvency Measures
# total debt service expense / total expense
plot_percentile_histogram(balanced_panel_2009_2019, "debt_service_impact")
# total loan / total asset
plot_percentile_histogram(balanced_panel_2009_2019, "leverage_ratio")
# total debt service expense / net operating income
plot_percentile_histogram(balanced_panel_2009_2019, "debt_service_coverage_ratio_inverse")


# Sustainability Measures
# unrestricted net assets / total operating expenses
plot_percentile_histogram(balanced_panel_2009_2019, "unres_net_asset_ratio")
# unrestricted program revenue / total expenses
plot_percentile_histogram(balanced_panel_2009_2019, "unres_program_revenue_ratio")


hist(balanced_panel_2009_2019$current_assets_total_over_current_liabilities_total)
boxplot(balanced_panel_2009_2019$current_assets_total_over_current_liabilities_total)




################################ Fixed Effects Model ###########################

############# Three-way fixed effects: org, time, state ########################
############# commercialization is associated with negative financial health: 
############# less cash, less sustainability, but no impact on solvency

fe_model <- feols(
  log(unres_net_asset_ratio)~  # outcome variable
    commercialization +
   log(total_employees_people) + # organization size
   fundraising_efficiency + # efficiency in obtaining contributions
   operating_margin        # efficiency in obtaining commercial revenues
   | organizations_id + year 
   + profile_cdp_taxonomy_name
   + profile_state
  , 
  data = balanced_panel_2009_2019
)

summary(fe_model)

################################################################################


# problems need to solve:
# (1) what to do with the negative values in totals.
# (2) how to deal with outliers.





