library(tidyverse)
library(lme4)

generate_tutor_data <- function(f='ds1607_tx_All_Data_28_2023_0329_220524.txt') {
  # Annotate relevant events
  d_tutor <- read_delim(f, delim='\t') %>% 
    janitor::clean_names() %>% 
    arrange(anon_student_id, time) %>% 
    mutate(g1_tutor = case_when(
      outcome %in% c('CORRECT', 'INCORRECT') ~ 'attempt',
      outcome == 'HINT' ~ 'hint'
    )) 
  
  d_tutor <- d_tutor %>% 
    mutate(g1_tutor_split = case_when(
      outcome == 'HINT' & attempt_at_step == 1 ~ 'hint-firstattempt',
      outcome == 'HINT' & attempt_at_step > 1 ~ 'hint-posterror',
      outcome %in% c('CORRECT') ~ 'attempt-correct',
      outcome %in% c('INCORRECT') ~ 'attempt-incorrect',
    ))
  
  # get last attempt
  join_this <- d_tutor %>% 
    filter(!is.na(g1_tutor)) %>% 
    mutate(prev_g1_tutor = lag(g1_tutor)) %>% 
    mutate(prev_g1_tutor = ifelse(anon_student_id != lag(anon_student_id, 1), NA, prev_g1_tutor)) %>% 
    select(transaction_id, prev_g1_tutor)
  
  d_tutor <- d_tutor %>% 
    left_join(join_this, by='transaction_id') %>% 
    mutate(g2_tutor = case_when(
      outcome == 'HINT' ~ 'hint',
      outcome %in% c('CORRECT', 'INCORRECT') & attempt_at_step == 1 ~ 'initial-attempt',
      outcome %in% c('CORRECT', 'INCORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'attempt' ~ 'reattempt-posterror',
      outcome %in% c('CORRECT', 'INCORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'hint' ~ 'reattempt-posthint'
    ))
  
  d_tutor <- d_tutor %>% 
    mutate(g2_tutor_split = case_when(
      outcome == 'HINT' & attempt_at_step == 1 ~ 'hint-firstattempt',
      outcome == 'HINT' & attempt_at_step > 1 ~ 'hint-posterror',
      outcome %in% c('CORRECT') & attempt_at_step == 1 ~ 'initial-attempt-correct',
      outcome %in% c('INCORRECT') & attempt_at_step == 1 ~ 'initial-attempt-incorrect',
      outcome %in% c('CORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'attempt' ~ 'reattempt-posterror-correct',
      outcome %in% c('INCORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'attempt' ~ 'reattempt-posterror-incorrect',
      outcome %in% c('CORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'hint' ~ 'reattempt-posthint-correct',
      outcome %in% c('INCORRECT') & attempt_at_step > 1 & prev_g1_tutor == 'hint' ~ 'reattempt-posthint-incorrect'
    ))
  
  return(d_tutor)
}

# Quantitative ITS log data analysis
d_tutor_a <- generate_tutor_data('ds1606_tx_All_Data_27_2023_0901_220244.txt')
d_tutor_b <- generate_tutor_data('ds1607_tx_All_Data_28_2023_0329_220524.txt')
d_tutor_germany <- bind_rows(d_tutor_a, d_tutor_b) %>% mutate(sample='germany')
d_tutor_usa <- generate_tutor_data('ds5371_tx_All_Data_7671_2023_0520_042939.txt') %>% mutate(sample='usa')
d_tutor <- bind_rows(d_tutor_usa %>% mutate(duration_sec=as.character(duration_sec)), d_tutor_germany)

# Remove tutor actions in ORCCA
d_tutor <- d_tutor %>% 
  filter(!(action_24 %in% c('setWorkspaceHeight', 'setGivens')))

# Final cleaned user reference
d_tutor$anon_student_id %>% unique %>% sort
d_tutor['date'] <- d_tutor$time %>% as.POSIXct() %>% as.Date() %>% as.character()

# These crosswalks are needed as some users had two separate IDs and are in DataShop
d_tutor <- d_tutor %>% 
  left_join(read_csv('crosswalk-usa.csv') %>% rename(user_fill = user)) %>% 
  left_join(read_csv('crosswalk-germany-with-date.csv') %>% mutate(date=as.character(date)), by=c('anon_student_id', 'date')) %>% 
  mutate(user = coalesce(user, user_fill)) %>% 
  select(-user_fill)

# Create platform
d_tutor['platform'] <- ifelse(str_detect(d_tutor$problem_name %>% tolower(), 'stoich'), 'Stoich', 'ORCCA')

# Clean up and filter relevant logs
d_tutor_final <- d_tutor %>% 
  filter(!is.na(user)) %>% 
  filter(transaction_id!='bade72827a7c9ee2d63b93ee656bd47c') # incorrectly logged one hour later

# N Steps, correctness
d_desc_actions <- d_tutor_final %>% 
  group_by(user, problem_name) %>% 
  mutate(n_steps = n()) %>% 
  summarize(
    n_steps = n(),
    correctness = mean(outcome[outcome %in% c('CORRECT', 'INCORRECT', 'HINT')]=='CORRECT', na.rm=TRUE),
    correctness_first = mean(outcome[outcome %in% c('CORRECT', 'INCORRECT', 'HINT') & attempt_at_step==1]=='CORRECT', na.rm=TRUE),
    platform = unique(platform),
    country = ifelse(unique(str_detect(user, 'German')), 'DE', 'USA')
  ) %>% 
  ungroup()

# Table 1
d_desc_actions %>% 
  group_by(platform, country) %>% 
  summarize(
    correctness = mean(correctness),
    correctness_first = mean(correctness_first, na.rm=TRUE),
    n_steps = mean(n_steps)
  )

d_desc_actions %>% 
  group_by(country) %>% 
  summarize(
    correctness = mean(correctness),
    correctness_first = mean(correctness_first, na.rm=TRUE),
    n_steps = mean(n_steps)
  )

# Time taken to solve exercises
# Idle time during setup removed from computation
trim_inactive_times <- function(datetime_vector) {
  # Calculate time differences between consecutive elements
  time_diffs <- c(0, diff(datetime_vector))
  
  # Find the indices where time difference exceeds 5 minutes
  exceeding_indices <- which(time_diffs > 5*60)
  
  # Trim off the portions with inactivity at the beginning and end
  if (length(exceeding_indices) > 0) {
    start_index <- max(exceeding_indices)
    trimmed_vector <- datetime_vector[(start_index+1):length(datetime_vector)]
    trimmed_vector <- trimmed_vector[!is.na(trimmed_vector)]
    if (length(trimmed_vector) < 2) {
      trimmed_vector <- datetime_vector[datetime_vector]
    }
  } else {
    # If no inactivity exceeds 5 minutes, return the original vector
    trimmed_vector <- datetime_vector
  }
  
  return(trimmed_vector)
}

d_desc_time <- d_tutor_final %>% 
  group_by(user, problem_name) %>% 
  summarize(
    time_spent = max(as.numeric(trim_inactive_times(time))) - min(as.numeric(trim_inactive_times(time))),
    platform = unique(platform),
    completed = sum(selection_22=='done' & outcome=='CORRECT', na.rm = TRUE)>0,
    n_trans = n(),
    n_steps = length(unique(step_name)),
    country = ifelse(unique(str_detect(user, 'German')), 'DE', 'USA')
  ) %>% 
  ungroup() 

d_desc_time %>% 
  filter(completed & !is.na(time_spent)) %>% 
  group_by(country) %>% 
  summarize(
    n_trans = mean(n_trans),
    n_steps = mean(n_steps),
    median_time_spent_mins = as.numeric(median(time_spent))/60
  )

d_desc_time %>% 
  filter(completed & !is.na(time_spent)) %>% 
  group_by(platform, country) %>% 
  summarize(
    n_problems = n(),
    n_steps = mean(n_steps),
    mean_mins = as.numeric(mean(time_spent))/60,
    sd_mins = as.numeric(sd(time_spent))/60,
    median_mins = as.numeric(median(time_spent))/60,
    iqr_mins = as.numeric(IQR(time_spent))/60
  ) 

d_desc_time %>% 
  filter(completed & !is.na(time_spent)) %>% 
  group_by(country) %>% 
  summarize(
    n_steps = mean(n_steps),
    mean_mins = as.numeric(mean(time_spent))/60,
    sd_mins = as.numeric(sd(time_spent))/60,
    median_mins = as.numeric(median(time_spent))/60,
    iqr_mins = as.numeric(IQR(time_spent))/60
  )
