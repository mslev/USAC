################################################################################
# STATUS OF THE LIFELINE PROGRAM IN MASSACHUSETTS: EXPLORATORY DATA ANALYSIS 
################################################################################
# Created by:  Marina Levy
# Last Updated: 5/20/2023 
#
# Purpose:
#   Analyze USAC Lifeline Disbursement data to calculate number of 
#   Massachusetts subscribers in the program and create graphs to
#   show subscriber trends
#
# Inputs:   Lifeline Disbursement API JSON file
#
# Outputs:  total_subscribers.png
#           subs_by_service_type.png
#           subs_by_provider.png
#
################################################################################


library(RSocrata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggtext)


# Load data from Socrata data portal 
df <- read.socrata("https://opendata.usac.org/resource/tfxa-gt3x.json")

# Narrow it down to Massachusetts data, for recent years
df <- df %>%
      filter(state == "MA" & support_year >= 2018) %>% 
      select(sac_name,
             support_year,
             support_month,
             support_month_date_format,
             technology_type,
             service_type,
             submission_type,
             subscriber_count)

################################################################################
# Clean and flatten the dataset
################################################################################

# Make subscriber_count be numeric (Socrata imports as character)
df$subscriber_count <- as.numeric(df$subscriber_count)


# There should be 7 approved providers in Massachusetts
#Add DBAs for each of them, with a quick check to make sure USAC is also
#displaying 7 providers
if (length(unique(df$sac_name)) == 7) {
  df <- df %>%
      mutate(provider = case_when(
        startsWith(sac_name, "CITY") ~ 'Westfield',
        startsWith(sac_name, "GLOBAL") ~ 'StandUp Wireless/Global Connection',
        startsWith(sac_name, "GRANBY") ~ 'Granby',
        startsWith(sac_name, "TRACFONE") ~ 'Tracfone/Safelink',
        startsWith(sac_name, "VERIZON") ~ 'Verizon',
        startsWith(sac_name, "VIRGIN") ~ 'Assurance/Virgin/T-Mobile',
        startsWith(sac_name, "TRUCONNECT") ~ 'TruConnect'
        ))
}  else {
    stop("There has been a change in the number of unique companies")
      }

# Providers will submit a claim for a specific support month stating they had 
# a certain number of subscribers, but will continue to adjust that amount
# months after the fact, either repaying disbursed funds or receiving
# additional funds

# In order to avoid having multiple entries for one single provider-month-service type
# combination, we flatten all the information a provider submits for a given month,
# aggregating their counts to reach one subscriber count number in a month, per service type

subscribers <- df %>%
                #Adjustment rows relate to disbursed funds, not subscribers, so remove them:
                filter(!submission_type == "ADJUSTMENT") %>% 
                select(provider, 
                       support_year,
                       support_month = support_month_date_format,
                       technology_type,
                       service_type,
                       subscriber_count) %>%
                group_by(provider,
                         support_year,
                         support_month,
                         technology_type,
                         service_type) %>%
                summarize(subscriber_count = sum(subscriber_count), 
                          .groups = "drop") %>%
                #Add proper capitalization to tech + service categories
                mutate_at(vars(technology_type, service_type), str_to_sentence)

################################################################################
# Identify missing data
################################################################################

# Providers may choose to wait to file claims for their Lifeline subscribers.
# While this means they don't receive funding disbursement until the claim
# is filed, they still have Lifeline subscribers that they serve, so we need
# to know what the last month of complete data for all providers is if we want 
# to know how many total subscribers there are in Massachusetts

# Identify where data is missing
# First, create a data frame of all possible provider-month combinations
providers <- unique(subscribers$provider)
support_months <- unique(subscribers$support_month)

all_combos <- expand.grid(provider = providers, support_month = support_months)

# Next, combine all possible combos against existing combinations to highlight
# missing combinations
missing_combos <- all_combos %>%
                left_join(subscribers, by = c("provider", "support_month")) %>%
                filter(is.na(subscriber_count))

# TruConnect and Westfield are missing for dates prior to their joining the Lifeline
# program, which makes sense. But Verizon, Tracfone and Assurance are late submitting
# their claims

# Identify latest month of complete data (where all 7 providers submitted claims)
latest <- subscribers %>%
          group_by(support_month) %>%
          summarize(distinct_providers = n_distinct(provider)) %>%
          filter(distinct_providers == 7) %>%
          arrange(desc(support_month)) %>%
          slice(1) 

latest_month <- latest$support_month[1]

################################################################################
# Visualize Trends
################################################################################

# Graph overall totals, excluding the recent months that aren't complete

waiver_end_date <-  as.POSIXct("2021-05-01") # Date that Lifeline non-usage pandemic waiver expired 

p1 <- subscribers %>%
          # Aggregate all subscriber counts to the support_month level
          filter(support_month <= latest_month) %>% # Keep data up to most recent complete month
          group_by(support_month) %>%
          summarise(subscribers = sum(subscriber_count)) %>%
          # Plot
          ggplot(aes(x=support_month, y=subscribers)) +
          geom_line(linewidth = 1) +
          xlab("Support Month") +
          scale_y_comma(limits=c(0,200000)) +
          scale_x_datetime(date_labels = "%Y-%m") +
          geom_vline(xintercept = waiver_end_date, linetype="dashed",
                     color = "red", alpha = 0.4, size=1) +
          annotate("text", x = (waiver_end_date + 50), y = 43000, 
                   label = "End of Non-Usage Waiver \n", angle=90) +
          labs(x="Support Month", y="Total Subscribers",
               title="Total Lifeline Subscribers in Massachusetts",
               caption="From the USAC Open Data Portal (Lifeline Disbursements)") +
       guides(x = guide_axis(angle = 45)) +
          theme_ipsum_rc()
p1

ggsave("total_subscribers.png", plot = p1)

# Graph state totals by service type (broadband, voice, bundled)

p2 <-  subscribers %>%
          # Aggregate subscriber counts by support_month and service type
          select(support_month, service_type, subscriber_count) %>%
          filter(support_month <= latest_month) %>% 
          group_by(support_month, service_type) %>%
          summarise(subscribers = sum(subscriber_count)) %>%
          # Plot
          ggplot(aes(x=support_month , y=subscribers, color = service_type)) +
          geom_line(linewidth = 1) +
          scale_y_comma(limits = c(0, 125000)) +
          scale_x_datetime(date_labels = "%Y-%m") +
          guides(x = guide_axis(angle = 45)) +
          geom_vline(xintercept = waiver_end_date, linetype = "dashed", 
                     color = "red", alpha = 0.4, size = 1) +
          annotate("text", x = (waiver_end_date + 50), y = 85000, 
                   label = "End of Non-Usage Waiver \n", angle = -90) +
          labs(x="Support Month", y="Total Subscribers",
               title="Lifeline Subscribers in Massachusetts. 
               <br><span style = 'color:#1736ff;'>Broadband</span>, 
               <span style = 'color:#45e0ff;'>Voice</span>, 
               and <span style = 'color:#cc00ff;'>Bundled</span> subscribers <br>by month",
               caption="From the USAC Open Data Portal (Lifeline Disbursements)") +
          theme_ipsum_rc() +
          theme(panel.grid.major.x = element_blank(),
                plot.title = element_markdown(),
                legend.position = "none") +
          scale_color_manual(values = c("#1736ff", "#cc00ff", "#45e0ff")) 

p2

ggsave("subs_by_service_type.png", plot = p2)

# Graph state totals by provider

p3 <- subscribers %>%
        # Aggregate subscriber counts by support_month and provider
        select(support_month, provider, subscriber_count) %>%
        group_by(support_month, provider) %>%
        summarise(subscribers = sum(subscriber_count)) %>%
        # Plot
        ggplot(aes(x=support_month , y=subscribers, color = provider)) +
        geom_line(size = 1) +
        scale_y_comma(limits = c(0, 125000)) +
        scale_x_datetime(date_labels = "%Y-%m") +
        guides(x = guide_axis(angle = 45)) +
        geom_vline(xintercept = waiver_end_date, linetype = "dashed", 
                   color = "red", alpha = 0.4, size = 1) +
        annotate("text", x = (waiver_end_date + 50), y = 85000, 
                 label = "End of Non-Usage Waiver \n", angle = -90) +
        labs(x="Support Month", y="Total Subscribers",
             title="Lifeline Subscribers in Massachusetts by Provider",
             caption="From the USAC Open Data Portal (Lifeline Disbursements)",
             col = "Provider: ") +
        theme_ipsum_rc() 

p3 <- p3+ theme(legend.position="bottom") 

p3

ggsave("subs_by_provider.png", plot = p3, width = 8)
