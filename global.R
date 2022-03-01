## objects created in global.R
## can be used in server.R

# packages ----------------------------------------------------------------

# packages that are used
library(shiny)
library(DT)
library(tidyverse)
library(explore)


# data --------------------------------------------------------------------

# create data
fakedata = function(obs = 1000, 
                    target_name = "target_ind",
                    factorise_target = FALSE,
                    target1_prob = 0.5, 
                    add_extreme = TRUE,
                    flip_gender = FALSE,
                    seed = 123) {
  
  # set seed (randomization)
  set.seed(seed)
  
  # create basic dataset
  data <- data.frame(
    id = seq(from = 100100100, to = 100100100 + obs - 1),
    period = rep(202012, obs),
    target_ind = sample(c(0, 1),
                        obs,
                        prob = c(1 - target1_prob, target1_prob),
                        replace = TRUE)
  )
  
  # add features
  data <- data %>%
    dplyr::mutate(
      age = round(ifelse(target_ind == 1,
                         rnorm(obs, mean = 45, sd = 10),
                         rnorm(obs, mean = 60, sd = 10)
      ), 0),
      city_ind = ifelse(target_ind == 1,
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.4, 0.6)),
                        sample(c(0, 1), obs, replace = TRUE, prob = c(0.6, 0.4))
      ),
      female_ind = ifelse(target_ind == 1,
                          sample(c(0, 1), obs, replace = TRUE, prob = c(0.3, 0.7)),
                          sample(c(0, 1), obs, replace = TRUE, prob = c(0.7, 0.3))
      ),
      fixedvoice_ind = ifelse(age > 70,
                              sample(c(0, 1), obs, replace = TRUE, prob = c(0.3, 0.7)),
                              sample(c(0, 1), obs, replace = TRUE, prob = c(0.95, 0.05))
      ),
      fixeddata_ind = 1,
      fixedtv_ind = ifelse(target_ind == 1,
                           sample(c(0, 1), obs, replace = TRUE, prob = c(0.4, 0.6)),
                           sample(c(0, 1), obs, replace = TRUE, prob = c(0.8, 0.2))
      ),
      mobilevoice_ind = sample(c(0, 1), obs, replace = TRUE, prob = c(0.4, 0.6)),
      mobiledata_ind = sample(c("NO","MOBILE STICK", "BUSINESS"), obs, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
      bbi_speed_ind = ifelse(age > 60,
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.9, 0.1)),
                             sample(c(0, 1), obs, replace = TRUE, prob = c(0.2, 0.8))
      ),
      bbi_usg_gb = ifelse(age > 75,
                          round(rnorm(obs, mean = 10, sd = 1)),
                          round(rnorm(obs, mean = 50, sd = 10))
      ) + city_ind * 20 + target_ind * 10,
      hh_single = ifelse(age < 35 & city_ind == 1,
                         sample(c(0, 1), obs, replace = TRUE, prob = c(0.2, 0.8)),
                         sample(c(0, 1), obs, replace = TRUE, prob = c(0.7, 0.3))
      ),
      event_web = 1
      
    ) # mutate
  
  # factorise target?
  if (factorise_target) {
    data$buy <- factor(data$buy,
                       levels = c(0, 1),
                       labels = c("no", "yes"))
  }
  
  # add extreme values?
  if (add_extreme) {
    extreme <- data[nrow(data), ]
    extreme$bbi_usg_gb <- 100000
    extreme$target_ind <- 0
    data[nrow(data), ] <- extreme[1, ]
  }

  # flip gender?
  if (flip_gender) {
    data[["female_ind"]] <- ifelse(data[["female_ind"]] == 1, 0, 1)
  }
  
  # rename target?
  if (target_name != "target_ind") {
    
    data[[target_name]] <- data$target_ind
    data$target_ind <- NULL
    
  }
  
  # return data
  data
  
} # fakedata


data <- fakedata(obs = 1000, flip_gender = TRUE, target_name = "buy", target1_prob = 1/6, seed = 123)
data <- data %>% select(-id, -period)
data <- data %>% 
  mutate(gender = ifelse(female_ind == 1, "female", "male")) %>% 
  select(-female_ind) %>%
  select(age, gender, everything())

# add description for help-tab
data_title <- "To who fits our new product 'Smart Vacuum Cleaner'?"
  
data_description <- paste(
  "This artificial dataset contains customers that got offered the product.", 
  "Each row represents a customer, each column represents a property.",
  "Customers who accepted the offer, are encoded buy = 1, others buy = 0"
)

data_variables <- tribble(
  ~variable, ~description,
  "period", "year/month (yyyymm)",
  "age", "age of the customer",
  "city_ind", "located in city (0 = no, 1 = yes)",
  "female_ind", "is female (0 = no, 1 = yes)",
  "fixedvoice_ind", "has fixed voice (0 = no, 1 = yes)",
  "fixeddata_ind", "has fixed data (0 = no, 1 = yes)",
  "fixedtv_ind", "has fixed tv (0 = no, 1 = yes)",
  "mobilevoice_ind", "has mobile voice (0 = no, 1 = yes)",
  "mobiledata_ind", "has mobile data (0 = no, 1 = yes)",
  "bbi_speed_ind", "has broadband speed (0 = no, 1 = yes)",
  "bbi_usg_gb", "broadband usage in gigabyte",
  "hh_single", "is single-household (0 = no, 1 = yes)",
  "event_web", "web-event was recognised (0 = no, 1 = yes)",
  "buy", "customer bought the product (0 = no, 1 = yes)"
)

# prepare for exploration -------------------------------------------------

data <- data
data_title <- data_title
data_description <- data_description
data_variables <- data_variables

target_quo = NA
target_text = NA

# define variables for CRAN-package check
type <- NULL
variable <- NULL

tbl_guesstarget <- describe(data) %>%
  filter(type %in% c("lgl","int","dbl","num","fct","chr")) %>%
  select(variable)
guesstarget <- as.character(tbl_guesstarget[[1]])

# check all variables if usable
for (i in names(data))  {
  if (explore::get_type(data[[i]]) == "other")  {
    data[[i]] <- "<hide>"
  }
}
