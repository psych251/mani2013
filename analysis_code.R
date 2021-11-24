############################################
#####   PREPARE DATA FOR ANALYSIS     ######
############################################ 
library(tidyverse) 

#### Import data from Cognition.Run

setwd("~/Desktop/mani2013/data")
easy_data <- read_csv("easy_data.csv") # raw data files for easy condition
hard_data <- read_csv("hard_data.csv") # raw data files for hard condition

# filter columns to ensure DF are the same size
easy_data_filtered <- easy_data %>%
  select(c(run_id,
           trial_index,
           subject_id,
           group,
           rt,
           response,
           task,
           correct_response,
           accuracy,
           block))

hard_data_filtered <- hard_data %>%
  select(c(run_id,
           trial_index,
           subject_id,
           group,
           rt,
           response,
           task,
           correct_response,
           accuracy,
           block))

data <- rbind(easy_data_filtered, hard_data_filtered) # merge into one file

#### DATA CLEANING AND FILTERING

# remove JSON text from survey responses
data$response <- gsub('\\{"Q0":"','', data$response)
data$response <- gsub('"\\}', '', data$response)

data_filtered <- data %>% 
  filter(group == "EASY" |
           group == "HARD")


#### INCOME AND SES ANALYSIS  

# determine SES groups using federal poverty guidelines
income_data <- data_filtered %>%
  select(c(run_id,
           task,
           response)) %>%
  filter(task == "household_size" |
           task == "income") %>%
  pivot_wider(names_from = "task", 
              values_from = "response") %>%
  mutate(ses_group = case_when(as.numeric(income) < 12880 & as.numeric(household_size) == 1 ~ "POOR",
                               as.numeric(income) < 17420 & as.numeric(household_size) == 2 ~ "POOR",
                               as.numeric(income) < 21960 & as.numeric(household_size) == 3 ~ "POOR",
                               as.numeric(income) < 26500 & as.numeric(household_size) == 4 ~ "POOR",
                               as.numeric(income) < 31040 & as.numeric(household_size) == 5 ~ "POOR",
                               as.numeric(income) < 35580 & as.numeric(household_size) == 6 ~ "POOR",
                               as.numeric(income) < 40120 & as.numeric(household_size) == 7 ~ "POOR",
                               as.numeric(income) < 44660 & as.numeric(household_size) == 8 ~ "POOR",
                               as.numeric(income) < 49200 & as.numeric(household_size) == 9 ~ "POOR",
                               as.numeric(income) < 53740 & as.numeric(household_size) == 10 ~ "POOR",
                               as.numeric(income) > 12880 & as.numeric(household_size) == 1 ~ "RICH",
                               as.numeric(income) > 17420 & as.numeric(household_size) == 2 ~ "RICH",
                               as.numeric(income) > 21960 & as.numeric(household_size) == 3 ~ "RICH",
                               as.numeric(income) > 26500 & as.numeric(household_size) == 4 ~ "RICH",
                               as.numeric(income) > 31040 & as.numeric(household_size) == 5 ~ "RICH",
                               as.numeric(income) > 35580 & as.numeric(household_size) == 6 ~ "RICH",
                               as.numeric(income) > 40120 & as.numeric(household_size) == 7 ~ "RICH",
                               as.numeric(income) > 44660 & as.numeric(household_size) == 8 ~ "RICH",
                               as.numeric(income) > 49200 & as.numeric(household_size) == 9 ~ "RICH",
                               as.numeric(income) > 53740 & as.numeric(household_size) == 10 ~ "RICH"))

income_data$ses_group <- factor(income_data$ses_group, levels = c("POOR",
                                                                  "RICH"))

data_filtered <- merge(data_filtered, income_data, by = c('run_id')) # add to main dataframe

############################################
###########   TASK ANALYSIS    #############
############################################ 
task_perf <- data_filtered %>%
  filter(task == "response")

# RT and accuracy need to be numeric
task_perf$rt <- as.numeric(task_perf$rt)
task_perf$accuracy <- as.numeric(task_perf$accuracy)

# create summary statistics
task_perf <- task_perf %>%
  group_by(group, ses_group) %>%
  summarize(mean_acc = mean(accuracy),
            mean_rt = mean(rt),
            acc_sd = sd(accuracy),
            acc_n_obs = length(accuracy),
            acc_sem = acc_sd / sqrt(acc_n_obs),
            acc_ci = acc_sem * 1.96,
            rt_sd = sd(rt),
            rt_n_obs = length(rt),
            rt_sem = rt_sd / sqrt(rt_n_obs),
            rt_ci = rt_sem * 1.96)


############################################
######   ACCURACY AND RT FIGURES     #######
############################################  
# Create Figure 1 - Accuracy Bar Chart
figure1 <- ggplot(task_perf, aes(x = ses_group, y = mean_acc, 
                                 fill = group)) +
  geom_bar(position="dodge", stat="identity") + # ADD COLOR-BLIND PALETTE
  geom_errorbar(aes(ymin = mean_acc - acc_ci, 
                    ymax = mean_acc + acc_ci), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Figure 1: Accuracy by Group")
figure1

# Create Figure 2 - Accuracy Regression Chart
figure2 <- ggscatter(task_perf, x = income, y = mean_acc,
          color = "condition", palette = "", 
          add = "reg.line", conf.int = TRUE) + # add reg line
  stat_cor(aes(color = condition)) + # add correlation coefficient
  ggtitle("Figure 2: Accuracy Scatterplot") +
  labs(y = "Mean Accuracy", x = "Income (in thousands)")
figure2

# Create Figure 3 - RT Bar Chart
ggplot(task_perf, aes(x = ses_group, y = mean_rt, 
                         fill = group)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="") + # ADD COLOR-BLIND PALETTE
  geom_errorbar(aes(ymin = mean_rt - acc_ci, 
                    ymax = mean_rt + acc_ci), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Figure 3: RT by Group") 

# Create Figure 4 - Accuracy Regression Chart
ggscatter(task_perf, x = income, y = mean_rt,
          color = "group", palette = "", 
          add = "reg.line", conf.int = TRUE) + # add reg line
  stat_cor(aes(color = group)) + # add correlation coefficient
  ggtitle("Figure 4: Reaction Time Scatterplot") +
  labs(y = "Mean RT", x = "Income (in thousands)")

############################################
#######    STATISTICAL ANALYSIS     ########
############################################ 

### t.tests 

t.test(task_perf$group, task_perf$mean_acc)

### TWO-WAY ANOVA
accuracy_aov <- aov(mean_acc ~ group * ses_group,
                    task_perf) #same thing as reg?

rt_aov <- aov(mean_rt ~ group * ses_group,
              task_perf) #same thing as reg?

### Regression Analysis
accuracy_model <- lm(mean_acc ~ group * ses_group, 
                     task_perf)

reactiontime_model <- lm(mean_rt ~ group * ses_group, 
                         task_perf)