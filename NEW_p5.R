# USED files:
#            full_geomerged_df_4.csv

library(car)
library(dplyr)
library(sampleSelection)
library(sjstats)
library(ggplot2)
library(lme4)
library(stats)
library(tictoc)
library(GLMMadaptive)
library(glmmTMB)
library(stargazer)
library(MASS)
library(pscl)
library(glmm)
library(glmmADMB)
library(R2admb)
library(lmtest)
library(margins)
library(VGAM)
library(plotly)

input <- read.csv("full_geomerged_df_4.csv")
brazil_df <- input

# ----------------------------------- #
# 1. Get correct data representations # --------------------------------------- 
# ----------------------------------- #


cols <- c("bef_message_bool",
          "max_price_disc",
          "item_count_disc",
          "urbanity_disc",
          "freight_issue_bool",
          "review_score",
          "north",
          "northeast",
          "centerwest",
          "south",
          "southeast",
          "y_2016",
          "y_2017",
          "y_2018",
          "year",
          "top2box",
          "experience_goods",
          "search_goods",
          "intimate_goods",
          "review_sent_wknd",
          "review_answer_wknd",
          "sent_sun",
          "sent_mon",
          "sent_tue",
          "sent_wed",
          "sent_thu",
          "sent_fri",
          "sent_sat",
          "title_bool",
          "title_or_message",
          "title_and_message",
          "title_nor_message")

brazil_df[,cols] <- lapply(brazil_df[cols], function(x) as.factor(x))


# Fix order 
brazil_df <- brazil_df %>%
  mutate(item_count_disc = factor(item_count_disc, 
                                  levels = c("single", "multiple", "large")))
# fix order
brazil_df <- brazil_df %>%
  mutate(hdi_class_col = factor(hdi_class_col, levels = c("low_medium", 
                                                          "high", 
                                                          "very high")))

# For interpretation sake, reverse the 1-0 config of this dummy
# Now it goes: if there was an issue, = 1, otherwise 0. 
brazil_df <- brazil_df %>%
  mutate(other_issue = ifelse(diff_est_deliv > 1, 1, 0))

# -------------- #
# 2. Missingness # ------------------------------------------------------------
# -------------- #


# 2.1. Assessment
# ---------------

colSums(is.na(brazil_df))

# product_id.y is missing for 722 cases

missing_subset <- brazil_df %>%
  filter(is.na(product_id.y)) # All of them have to do with order status problems

missing_subset_2 <- brazil_df %>% # ASll related to order status again, e.g. shipped, unavailable
  filter(is.na(other_issue))

brazil_df <- brazil_df %>%
  mutate(status_problem = ifelse(order_status != "delivered" & order_status != "shipped", 1,0 ))

missing_logit <- glm(status_problem ~ region + new_urbanity + new_idhm, data = brazil_df, family = "binomial")

#In south east regions there is some substantial stuff. Otherwise, nothing interesting.
summary(missing_logit)

# 2.2. Solution
# -------------

brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

brazil_df <- brazil_df %>%
  filter(! is.na(product_category_name))

brazil_df <- brazil_df %>%
  filter(!is.na(bef_message_bool))

brazil_df <- brazil_df %>%
  filter(!is.na(diff_est_deliv))

brazil_df <- brazil_df %>%
  filter(!is.na(product_height_cm))

colSums(is.na(brazil_df)) # 94539

# -------------------------------- #
# 3. Create above median variables # ------------------------------------------
# -------------------------------- #

cats <- unique(brazil_df$product_category_name)
nrep <- rep(0, length(cats))
prod_uniques <- as.data.frame(nrep, cats)
prod_uniques$median_approx <- 0
counter <- 1

cat_1 <- brazil_df[brazil_df$product_category_name == cats[1],c("product_category_name","product_id.y","max_price")]

sum_cat_1 <- cat_1 %>% group_by(product_id.y) %>% summarise(mean(max_price))


prod_uniques[2,1] <- 1
counter <- 1
for (i in cats) {
  cat_1 <- brazil_df[brazil_df$product_category_name == i,c("product_category_name","product_id.y","max_price")]
  sum_cat_1 <- cat_1 %>% group_by(product_id.y) %>% summarise(mean(max_price))
  prod_uniques[counter,1] <- median(sum_cat_1$`mean(max_price)`)
  counter <- counter + 1
}


median(sum_cat_1$`mean(max_price)`)


for (i in cats)) {
  prod_uniques[counter,1] <- length(unique(brazil_df[brazil_df$product_category_name == i,]$product_id.y))
  counter <- counter + 1 
}

# ----------------- #
# 4. First Insights # ---------------------------------------------------------
# ----------------- #

# 4.1. Check how many people have more than 2 orders
# --------------------------------------------------

more_than_2 <- brazil_df %>% 
  group_by(customer_unique_id) %>% 
  summarise(count = n())

more_than_2 <- more_than_2 %>% 
  mutate(two_or_one = ifelse(count >2, 0, 1), 
         one = ifelse(count == 1, 1, 0))

mean(more_than_2$two_or_one)
mean(more_than_2$one)

# 4.1 Check how many people per nest
# -----------------------------------

# For metro data
singletons_metros <- brazil_df[brazil_df$udh_indicator == 1,] %>% 
  group_by(customer_city) %>% 
  summarise(count = n())
# mean number of observations per nest
mean(singletons_metros$count)
# Create single indicator
singletons_metros <- singletons_metros %>%
  mutate(single = ifelse(count == 1, 1, 0))
# mean number of singles in the data
mean(singletons_metros$single)


# For municipality data
singletons_nonmetros <- brazil_df[brazil_df$udh_indicator == 0,] %>% 
  group_by(customer_city) %>% 
  summarise(count = n())
# mean observations per nest
mean(singletons_nonmetros$count)
# Create single indicator
singletons_nonmetros <- singletons_nonmetros %>%
  mutate(single = ifelse(count == 1, 1, 0))
# mean number of singles in the data
mean(singletons_nonmetros$single)



# 4.3 Check how many items per order
# ----------------------------------

