1+1
sqrt(4)
5^2

### Objects

x <- 3 + 5
x * 2
x + x

#### Vectorization

y <- c(1,2,3,4,5)      # combine to vector
z <- seq(0,10, by = 2) # seq func
2 * y
y/z

# Custom Function
mean(1:5) # base

add_two <- function(x){  # custom
  x+2
}

# Libraries + Package

library(ggplot2)

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Horsepower",
       y = "Miles per Gallon (MPG)")


####################################
# Load Data 
####################################

install.packages("tidyverse")
library(tidyverse)
HR_dat <- read_csv("https://raw.githubusercontent.com/DanSimonet/METRO-Workshop/main/IBM_HR_Data.csv")

head(HR_dat)
glimpse(HR_dat)
names(HR_dat)

####################################
# The Pipe
####################################

mean(1:10)
1:10 %>% mean()

new_cars <- mtcars %>%
  mutate(cyl = factor(cyl)) %>%   # convert to factor
  select(mpg, cyl, hp)   


####################################
# Dplyr 
####################################

# Select
HR_dat %>% select(Age, JobLevel, Attrition)
HR_dat %>% select(Age:BusinessTravel)
HR_dat %>% select(-Attrition)
HR_dat %>% select(contains("Job"))

# Filter
HR_dat %>% filter(Gender == "Male")
HR_dat %>% filter(Age > 40)
HR_dat %>% filter(Gender == "Male", Age > 40)

# Mutate
HR_dat %>%
  mutate(AnnualIncome = MonthlyIncome * 12, 
         WorkLifeAtComp = YearsAtCompany/TotalWorkingYears)

HR_dat %>%
  mutate(Attrition = factor(Attrition),
         TenComp = (YearsAtCompany + YearsInCurrentRole + YearsWithCurrManager)/3)

# Arrange
HR_dat %>% arrange(MonthlyIncome)
HR_dat %>% arrange(desc(MonthlyIncome))

# Combine Dplyr Verbs
HR_dat_s <- HR_dat %>%
  filter(DistanceFromHome <= 28) %>%
  mutate(OvTenure = (YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager)/3,
         Attrition = factor(Attrition),
         Department = factor(Department),
         Gender = factor(Gender)) %>%
  select(-contains("Years"))

# Summarize
HR_dat %>% summarize(JS_M = mean(JobSatisfaction),  # mean 
                     JS_SD = sd(JobSatisfaction),   # sd
                     JS_SE = JS_SD/sqrt(n()),       # standard error
                     n = n())                       # number participants

# Group_by + Summarize
HR_dat %>% 
  group_by(Department) %>%
  summarize(JS_M = mean(JobSatisfaction),  
            JS_SD = sd(JobSatisfaction),   
            JS_SE = JS_SD/sqrt(n()),       
            n = n())


####################################
### Describe Data
####################################

# Descriptives

library(modelsummary)
datasummary_skim(HR_dat)
datasummary_skim(HR_dat, type = "categorical")

# Correlations

HR_dat_sn <- HR_dat                                   # create new df
colnames(HR_dat_sn) <- abbreviate(names(HR_dat), 7)   # abbreviate variable names

corrplot::corrplot(cor(select_if(HR_dat_sn, is.numeric)),           # run correlation on only numeric
                   type = 'lower', diag = FALSE, tl.cex = .45)      # modify appearance



####################################
### Visualization
####################################


# General ggplot syntax - density distribution of monthly income
ggplot(data = HR_dat, aes(x = MonthlyIncome)) +
  geom_histogram(fill = "blue")


# First plot - JS and Income
ggplot(data = HR_dat_s,
       aes(y = JobSatisfaction, x = MonthlyIncome)) +
  geom_point(position = "jitter", alpha = .40) + #<<
  geom_smooth(method = "lm")

# Second Plot - JS and H Income by Dep and Gender
ggplot(data = HR_dat,
       aes(y = JobSatisfaction, x = HourlyRate, color = Gender)) +   #<<
  geom_point(position = "jitter", alpha = .40) + 
  geom_smooth(method = "lm", se = F) +  #<<
  facet_wrap(~Department) +   
  theme_classic() + 
  theme(text = element_text(size = 16, family = "serif"),
        legend.position = "bottom") +   #<<  
  labs(title = "Hourly Pay Slightly Less Satisfied",
       x = "Hourly Rates ($)",
       y = "Overall Job Satisfaction") 



####################################
### Statistical Inference
####################################

# Describe and Visualize Income by Groups
HR_dat_s %>%
  group_by(Department, Gender) %>%
  summarize(
    Income_M = mean(MonthlyIncome),
    Income_SD = sd(MonthlyIncome),
    n = n()
  )

ggplot(data = HR_dat_s,                                          
       aes(x = Department, y = MonthlyIncome, color = Gender)) +  
  geom_boxplot()    

########
# Run Base R Anova
########

inc_aov_m <- aov(MonthlyIncome ~ Department + Gender, data = HR_dat_s)   # main        
inc_aov_i <- aov(MonthlyIncome ~ Department * Gender, data = HR_dat_s)   # int   

summary(inc_aov_i) # summary to get output

########
# Afex ANOVA
########

HR_dat_s <- HR_dat_s %>% mutate(id = row_number())    # Add row_id

inc_aov <- aov_car(MonthlyIncome ~ Department * Gender + Error(id),  #add Error(id)
                   data = HR_dat_s)
inc_aov


#######
# Posthocs
#######

ph <- emmeans(inc_aov, c("Department", "Gender"))
pairs(ph)


#######
# Regression
#######

JS_mod <- lm(JobSatisfaction ~ HourlyRate + WorkLifeBalance + NumCompaniesWorked,
             data = HR_dat_s)

summary(JS_mod) # Summarize reg model

#######
# Prettier Regession with sjPlot
#######

tab_model(JS_mod)

tab_model(JS_mod, show.ci = F, show.se = T,   # change display
          show.std = T, show.stat = T)

########
# GLM and Logistic
########

tur_mod <- glm(Attrition ~ MonthlyIncome + WorkLifeBalance +  JobSatisfaction, 
               data = HR_dat_s, family = "binomial")                              # set family
tab_model(tur_mod)



########
# NLP Example and HR Twitter
########

library(rtweet)
HR_rt <- search_tweets(
  "#HR", n = 20000, include_rts = FALSE)

HR_rt_s <- HR_rt %>% select(created_at, screen_name, text) %>% # subset columns
  filter(!str_detect(text, "^RT")) %>%                         # filter retweets
  mutate(text = str_to_lower(text) %>%                         # make loewr cose
           str_remove_all("[:punct:]") %>%                     # remove puncutation
           str_remove_all("[:digit:]") %>%                     # remove digits
           str_replace_all("https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", ""))


####################
### TidyText and Refine
####################

tidy_HR <- HR_rt_s %>%
  unnest_tokens(word, text) %>% # break out sentences (or other text) into individual words
  filter(!word %in% stop_words$word,  # remove stop words
         str_detect(word, "[a-z]"),   # retain unigrams with letters
         word != "hr", word != "amp") # remove hr and amp

head(tidy_HR)


####################
### Word Frequency
####################

# Create frequency count
HR_freq <- tidy_HR %>% 
  count(word, sort = TRUE) 
head(HR_freq)

# Graph as bar chart turned sideways
HR_freq %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Common Words for HR Hashtags (September, 2021)") +
  theme_classic()

# Make into wordcloud

HR_freq %>% 
  with(wordcloud(
    word, n, max.words = 30, 
    colors = plasma(n=15, direction = -1),
    rot.per = .5))


####################
### Sentiment and Dictionaries
####################

hr_sentiment <- tidy_HR %>%
  inner_join(get_sentiments("bing"))  # join a sentiment dictionary 

# Create graph 
hr_sentiment %>%
  count(sentiment, word) %>%
  ungroup() %>%
  filter(n >= 75) %>%
  mutate(n = ifelse(
    sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = F) +
  labs(x = "Contribution to sentiment", y = NULL)

### Leadership sentiment cloud comparison

HR_rt_s %>% 
  filter(str_detect(text, "leader")) %>%  #extract leader tweets
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word, 
         str_detect(word, "[a-z]"),
         word != "hr", word != "amp") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%       # count by pos and neg
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%  # create matrix
  comparison.cloud(colors = c("gray20", "gray80"),    # run in wordcloud
                   max.words = 100)


############################
## Bigram Network Function
############################


library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
    theme_void()
}

tidy_bigrams <- count_bigrams(HR_rt)
tidy_bigrams %>%
  filter(n > 50,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()



###############################
## Topic Modeling 
###############################

library(topicmodels)

LDA_top <- tidy_HR %>% filter(!str_detect(word, "tco")) %>%
  count(screen_name, word) %>%
  cast_dtm(screen_name, word, n) %>%   #<< convert to document-term matrix
  LDA(k = 2, control = list(seed=123)) #<< run Latent Dirichlet allocation (LDA)

LDA_top %>% tidy(matrix = "beta") %>%    #<< tidy the LDA output
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme(text = element_text(size = 6))