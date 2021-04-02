###############################################################################
# Start/Import
###############################################################################

  #Load Libraries
  library(RSocrata)
  library(tidyverse)
  library(plotly)
  library(reshape2)

  #Bring in data from Socrata API
  df <- read.socrata("https://opendata.usac.org/resource/tfxa-gt3x.json")

  #Limit data to the state of Massachusetts for recent years  
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

###############################################################################
# Explore + Clean
###############################################################################
  
#Start out by getting a sense of what the dataset is actually showing

  #Filter out observations that don't add or remove subscribers
  df_explore <- df %>%
    filter(!submission_type == "ADJUSTMENT" & !subscriber_count == "0")
  
  #Confirm no variation in names for 2018, 2019 and 2020 support years
    #Add a built-in assumption that there are only 5 providers in the state
    #Standardize their names
  if (nrow(table(df_explore$sac_name)) == 5) { 
    df_explore$sac_name[df_explore$sac_name=="GLOBAL CONNECTION INC OF AMERICA - MA"] <- "StandUp Wireless/Global Connection"
    df_explore$sac_name[df_explore$sac_name=="GRANBY TEL. & TELE. CO.-MA"] <- "Granby"
    df_explore$sac_name[df_explore$sac_name=="TRACFONE WIRELESS INC. - MA"] <- "Tracfone/Safelink"
    df_explore$sac_name[df_explore$sac_name=="VERIZON NEW ENGLAND - MA"] <- "Verizon"
    df_explore$sac_name[df_explore$sac_name=="VIRGIN MOBILE USA, LP - MA"] <- "Assurance/Virgin/T-Mobile"
  }
  
  #How many updates/corrections have been made for each month-year claim?
  check_multiple <- as.data.frame(table(df_explore$support_month_date_format, df_explore$sac_name)) %>%
    select(support_month_date = Var1, company = Var2, frequency = Freq) %>%
    arrange(desc(frequency))
  
  #Claims vs. adjustments
  table(df_explore$submission_type)


  #Let's flatten the df, accounting for corrections over time
  #Sidenote/to-do: Would it be worth it to keep track of the original numbers 
    #before corrections? To see if there's a pattern around subscriber counts that
    #require corrections
  df_flattened <- df_explore %>%
    select(-submission_type) %>%
    group_by(sac_name, 
             support_year,
             support_month,
             support_month_date_format,
             technology_type,
             service_type) %>%
    summarise(subscriber_count = sum(as.numeric(subscriber_count)))
  
  #Add rows showing total counts by provider
  df_withtotals <- spread(df_flattened, service_type, subscriber_count) %>%
    select(sac_name, support_year, support_month, support_month_date_format, technology_type,
           broadband = BROADBAND, bundled = BUNDLED, voice = VOICE) %>%
    mutate(total = sum(broadband, bundled, voice, na.rm=TRUE)) %>%
    melt(id.vars=c("sac_name",
                   "support_year",
                   "support_month",
                   "support_month_date_format",
                   "technology_type"))
  
  #Isolate just the total counts into their own dataframe
  just_totals <- df_withtotals %>%
    filter(as.character(variable) == "total")
  
###############################################################################
# Visualize
###############################################################################
  
  #Tidy up the totals df to prepare for visualization
  just_totals <- just_totals %>%
    select(sac_name, support_month_date_format, total = value) %>%
    spread(key = sac_name, value = total) 
  
  just_totals <- just_totals %>%
    rowwise() %>%
    mutate(Total = sum(`Assurance/Virgin/T-Mobile`,
                       `Granby`,
                       `StandUp Wireless/Global Connection`,
                       `Tracfone/Safelink`,
                       `Verizon`, na.rm=TRUE)) %>%
    melt(id.vars="support_month_date_format")
  
  #Graph 1: Overall Totals
  p1 <- just_totals %>%
    filter(as.character(variable) == "Total") %>%
    ggplot(aes(support_month_date_format, value)) +
    geom_line(aes(color = factor(variable))) +
    #geom_point() +
    theme_bw() + 
    theme(legend.title=element_blank()) +
    labs(x="Support Month", y = "Subscriptions", title = "Total Subscriptions")
  p1
  ggplotly(p1)  #The trend is overwhelmingly pointing to a decrease in subscriptions
  # But the pandemic in 2020 seems to have led to a small uptick until the last
  # available month of data
  
  #Add table to easily see if last result is lower because data is missing
  totals_exist <- just_totals %>%
    filter(!as.character(variable) == "Total") %>%
    spread(variable, value)
  
  totals_exist[is.na(totals_exist)] <- "Missing"  # So there isn't a decline in 
                                                  #the last month, it's just missing
  
  #Graph 2: Totals by Provider
  p2 <- just_totals %>%
    filter(!as.character(variable) == "Total") %>%
    ggplot(aes(support_month_date_format, value)) +
    geom_line(aes(color = factor(variable))) +
    #geom_point() +
    theme_bw() + 
    theme(legend.title=element_blank()) +
    labs(x="Support Month", y = "Subscriptions", title = "Total Subscriptions by Provider")
  p2
  ggplotly(p2) #Assurance Wireless is fairing the best so far
  
  #Totals by technology
  totals_tech <- df_withtotals %>%
    filter(as.character(variable) == "broadband" |
             as.character(variable) == "bundled" |
             as.character(variable) == "voice") %>%
    select(support_month_date_format, 
           variable,
           value) %>%
    group_by(support_month_date_format, variable) %>%
    summarise(Total = sum(value, na.rm=TRUE))
  
  p3 <- totals_tech %>%
    mutate(variable = str_to_title(variable)) %>%
    ggplot(aes(support_month_date_format, Total)) +
    geom_line(aes(color = factor(variable))) +
    theme_bw() + 
    theme(legend.title=element_blank()) +
    labs(x="Support Month", y = "Subscriptions", title = "Total Subscriptions by Technology")
  p3
  ggplotly(p3)  #Voice-only subscriptions are in decline, but the pandemic response seems to
                #be leading to an uptick in broadband subscriptions
  
  #Subscribers by provider and by technology
  p4 <- df_withtotals %>%
    filter(!as.character(variable) == "Total") %>%
    ggplot(aes(support_month_date_format, value)) +
    geom_line(aes(color = factor(sac_name), linetype=factor(variable))) +
    theme_bw() + 
    theme(legend.title=element_blank())
  p4
  ggplotly(p4) #Something strange happened with Tracfone voice subscriptions
  #from 2019 to 2020 that we should look into in more detail
