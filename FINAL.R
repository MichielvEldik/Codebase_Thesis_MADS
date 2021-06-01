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

# Load data ------------------------------------------------------------------ 
input <- read.csv("full_geomerged_df_5.csv")
input <- read.csv("full_geomerged_df_5_new.csv")
brazil_df <- input

# PART 1: DATA ENGINEERING
################################################################################
# Factor type ----------------------------------------------------------------- 

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

# Work in progress (for missingness) -----------------------------------------
# Order status to "delivered"
table(brazil_df$order_status)

non_delivered <- brazil_df %>%
  filter(!order_status == "delivered") # Though we should look at what causes non-deliveries.


# Category names
nrow(non_delivered)
colSums(is.na(non_delivered))

# MISSINGNESS ----------------------------------------------------------------- 


# only keep delivered ones
brazil_df <- brazil_df %>%
  filter(order_status == "delivered")

brazil_df <- brazil_df %>%
  filter(! is.na(product_category_name))

brazil_df <- brazil_df %>%
  filter(!is.na(bef_message_bool))


brazil_df <- brazil_df %>%
  filter(!is.na(diff_est_deliv))

brazil_df <- brazil_df %>%
  mutate(item_count_bin = ifelse(item_count_disc == "single", 1, 0))

brazil_df <- brazil_df %>%
  filter(!is.na(product_height_cm))

colSums(is.na(brazil_df))


# Categories ------------------------------------------------------------------




# unique per category

# emptyvec 
cats <- as.character(levels(brazil_df$product_category_name))
nrep <- rep(0, length(cats))
prod_uniques <- as.data.frame( nrep, cats)
prod_uniques$median_approx <- 0
counter <- 1
for (i in levels(brazil_df$product_category_name)) {
  prod_uniques[counter,1] <- length(unique(brazil_df[brazil_df$product_category_name == i,]$product_id.y))
  counter <- counter + 1 
}


# Work in progress above median variable -------------------------------------

all_names <- levels(brazil_df$product_category_name)

hi <-brazil_df[brazil_df$product_category_name == all_names[1],]


# Above median variable ------------------------------------------------------- 


# Summarise per product
cat_1 <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(mean = mean(max_price),
            median = median(max_price),
            sd = sd(max_price),
            count = n())

# get rid of NA
cat_1 <- cat_1[1:71,]

# view
stargazer(cat_1, type = "text")

# merge with original dataframe
brazil_df <- merge(brazil_df, cat_1,
                   by.x = "product_category_name",
                   by.y = "product_category_name",
                   all.x = TRUE)
# calculate derived variables 
brazil_df <- brazil_df %>%
  mutate(above_median = ifelse(max_price > median, 1, 0),
         above_median = as.factor(above_median),
         above_mean = ifelse(max_price > mean, 1, 0),
         above_mean = as.factor(above_mean))
# Check out the end product...
table(brazil_df$above_median)

str(brazil_df$above_median)


# Mean centering before analyses  --------------------------------------------- 
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
brazil_df$mc_new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$mc_new_young_ratio <- center_scale(brazil_df$new_young_ratio)
brazil_df$new_urbanity <- center_scale(brazil_df$new_urbanity)

hist(brazil_df$mc_new_idhm)
hist(brazil_df$mc_new_young_ratio)



stargazer(brazil_df[brazil_df$udh_indicator == 1,c("new_idhm", "new_young_ratio")],
          brazil_df[brazil_df$udh_indicator == 0,c("new_idhm", "new_young_ratio")],
          flip = TRUE,
          type = "text")

stargazer(summary(brazil_df[,"bef_message_bool"]), type = "text")


# FIX FUCKUP ------------------------------------------------------------------
brazil_df <- brazil_df %>%
  mutate(other_issue = ifelse(diff_est_deliv > 1, 1, 0))


# PART 2: FIRST INSIGHTS
################################################################################
# -- Check: how many people have more than 2? --------------------------------- 

more_than_2 <- brazil_df %>% 
  group_by(customer_unique_id) %>% 
  summarise(count = n())

more_than_2 <- more_than_2 %>% 
  mutate(two_or_one = ifelse(count >2, 0, 1), 
         one = ifelse(count == 1, 1, 0))

mean(more_than_2$two_or_one)
mean(more_than_2$one)


singletons <- brazil_df %>% group_by(customer_city)

# Creating the big table ------------------------------------------------------

pop <- brazil_df %>%
  group_by(region, hdi_class_col, bef_message_bool) %>%
  summarise(n = n())

ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap( ~ region, scales = "free")


ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap( ~ region, scales = "free") 

zero_low <- sum(pop[pop$hdi_class_col == "low_medium" & pop$bef_message_bool == 0,]$n)
zero_high <- sum(pop[pop$hdi_class_col == "high" & pop$bef_message_bool == 0,]$n)
zero_veryhigh <- sum(pop[pop$hdi_class_col == "very high" & pop$bef_message_bool == 0,]$n)

one_low <- sum(pop[pop$hdi_class_col == "low_medium" & pop$bef_message_bool == 1,]$n)
one_high <- sum(pop[pop$hdi_class_col == "high" & pop$bef_message_bool == 1,]$n)
one_veryhigh <- sum(pop[pop$hdi_class_col == "very high" & pop$bef_message_bool == 1,]$n)

pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA
pop[nrow(pop)+1,] <- NA


# Add Full dist
pop <- pop %>%
  mutate(region = as.character(region),
         hdi_class_col = as.character(hdi_class_col),
         bef_message_bool = as.character(bef_message_bool))

pop[31:36,1] <- "full"
pop[31:36,2] <- c("low_medium", "low_medium",
                  "high", "high",
                  "very high", "very high")
pop[31:36,3] <- c("0", "1",
                  "0", "1",
                  "0", "1")
pop[31:36,4] <- c(zero_low, one_low,
                  zero_high, one_high,
                  zero_veryhigh, one_veryhigh)
pop <- pop %>%
  mutate(region = as.factor(region),
         hdi_class_col = as.factor(hdi_class_col),
         bef_message_bool = as.factor(bef_message_bool))

pop <- pop %>%
  mutate(region = factor(region, levels = c("centerwest",
                                            "north",
                                            "northeast",
                                            "south",
                                            "southeast",
                                            "full")),
         hdi_class_col = factor(hdi_class_col, levels = c("low_medium",
                                                          "high",
                                                          "very high")))
to_go <- c(2,
           4,
           6,
           8,
           10,
           12,
           14,
           16,
           18,
           20,
           22,
           24,
           26,
           28,
           30,
           32,
           34,
           36)

new_vec <- rep("0", length(to_go))

counter <- 1
for (i in to_go){
  outcome <- pop[i,"n"] / (pop[i,"n"] +  pop[i-1,"n"])
  new_vec[counter] <- outcome
  counter <- counter + 1
}

round(new_vec[[2]], digits = 2)

test <- c("", paste(as.character(round(new_vec[[1]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[2]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[3]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[4]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[5]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[6]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[7]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[8]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[9]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[10]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[11]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[12]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[13]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[14]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[15]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[16]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[17]], digits = 2)), "%", sep = ""),
          "", paste(as.character(round(new_vec[[18]], digits = 2)), "%", sep = ""))

# Do visual again
ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3.3, aes(label = test, family = "serif"), vjust = -1) + 
  ylab("count (n)") + 
  xlab("Human Development Index Category") +
  theme_bw() + 
  labs(title = expression(bold("Figure 6")),
       subtitle = expression(italic("State Counts of HDI and Review Incidence Across Regions"))) +
  labs(fill = "Review sent, yes (1) no (2)") + 
  theme(text=element_text(size=13,  family="serif")) +
  facet_wrap( ~ region, 
              scales = "free",
              labeller =labeller(region = c(
                "centerwest" = "Centerwest (n = 3,537)",
                "north" = "North (n = 1,779)",
                "northeast" = "Northeast (n = 8,996)",
                "south" = "South (n = 13,736)",
                "southeast" = "Southeast (n = 64,714)",
                "full" = "Full (n = 92,762)"))) 

note = expression(paste(italic("Note. "), "Zero-length reviews were excluded from this plot."))
ggplot(brazil_df[brazil_df$bef_message_bool == 1,], aes(x= bef_nwords)) + 
  geom_histogram(color="black", fill="coral", size = 0.1, bins = 50) +
  labs(caption = note) +
  labs(title = expression(bold("Figure 7")),
       subtitle = expression(italic("Distribution of Review Message Length Frequencies"))) +
  xlab("Number of Words") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(hjust = 0))
# sample table ----------------------------------------------------------------

hai <- as.data.frame(brazil_df %>% 
                       select(region, 
                              new_idhm,
                              new_young_ratio,
                              diff_pur_est,
                              new_urbanity,
                              bef_message_bool) %>% 
                       group_by(region) %>%
                       summarise(
                         count_idhm = n(),
                         mean_idhm = mean(new_idhm),
                         sd_idhm = sd(new_idhm),
                         mean_yr = mean(new_young_ratio),
                         sd_yr = sd(new_young_ratio),
                         mean_diff = mean(diff_pur_est),
                         mean_urban = mean(new_urbanity),
                         mean_rate = mean((as.integer(bef_message_bool) - 1))))

stargazer(hai, type = "text", align = TRUE)

hai_2 <- as.data.frame(brazil_df %>% 
                       select(region, 
                              review_score,
                              bef_message_bool,
                              new_idhm,
                              other_issue,
                              diff_est_deliv,
                              new_young_ratio,
                              new_urbanity,
                              max_price,
                              above_median) %>% 
                       group_by(region) %>%
                       summarise(
                        count = n(),
                        mean_review_score = mean(as.numeric(review_score)),
                        mean_bef_message = mean(as.integer(bef_message_bool) - 1),
                        mean_idhm = mean(new_idhm),
                        sum_issue_freight = sum(other_issue),
                        mean_issue_freight = mean(as.integer(other_issue)),
                        mean_urbanity = mean(new_urbanity),
                        mean_young = mean(new_young_ratio),
                        mean_maxprice = mean(max_price),
                        mean_abovemedian = mean(as.integer(above_median) - 1)
                       ))

hai_2_transpose <- t(as.matrix(hai_2))


stargazer(hai_2,                 # Export txt
          summary = FALSE,
          type = "html",
          out = "descriptives.html")

stargazer(hai_2_transpose,                 # Export txt
          summary = FALSE,
          type = "html",
          out = "hai_transpose.html",
          title = "Table 1: ")


stargazer(hai, type = "text", align = TRUE)



# PART 3: SUBSETTING 
################################################################################
# Robustness checks part -------------------------------------------------------










brazil_df <- brazil_df %>%
  mutate(metro = ifelse(is.na(metro), paste(as.character(customer_state), "county", sep = "_"), 
                        as.character(metro)))


brazil_df <- brazil_df %>%
  mutate(dec = ifelse(review_sent_moy == "dec", 1, 0))



# Define train and test sets -------------------------------------------------- 
# Regular split ---------------------------------------------------------------
train_size <- floor(0.75 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]

colSums(is.na(test))
colSums(is.na(train))



# Pos/neg split ---------------------------------------------------------------
train_pos <- train[
  train$review_score == 4 | 
  train$review_score == 5 | 
  train$review_score == 3,]
train_neg <- train[
  train$review_score == 1 | 
  train$review_score == 2,]

test_pos <- train[
  test$review_score == 4 | 
  test$review_score == 5 | 
  test$review_score == 3,]
test_neg <- train[
  test$review_score == 1 | 
  test$review_score == 2,]



# Freight issue split ---------------------------------------------------------

train_no_iss <- train[train$freight_issue_bool == 1 | train$other_issue == 1,]
brazil_no_iss <- brazil_df[brazil_df$freight_issue_bool == 0 | brazil_df$other_issue == 0,]

# PART 4: MODELING -------------------------------------------------------------
################################################################################

# logit intercept vs. mixed logit intercept ------------------------------------
# (4.1.) Fit null model
null_model <- glm(formula = bef_message_bool ~ 1,
                  data = train,
                  family = binomial(link = "logit")
                  )
AIC(null_model)
summary(null_model)
model_or <- exp(coef(null_model)) # Odds ratio (checks out)
probability_success <- nrow(train[train$bef_message_bool == 1,]) / nrow(train)
probability_failure <- nrow(train[train$bef_message_bool == 0,]) / nrow(train)
manual_or <- probability_success / probability_failure # Checks out

# (4.2.) Nested null model with city
nested_null_model_1 <- glmer(
  formula = bef_message_bool ~ 1 + (1 | customer_city),
  family = binomial(link = "logit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
    )
  )
icc(nested_null_model_1)
AIC(nested_null_model_1)
summary(nested_null_model_1)
all_intercepts <- coef(nested_null_model_1)
all_intercepts <- all_intercepts[["customer_city"]]
mean_intercept <- mean(all_intercepts$`(Intercept)`) 
hist(all_intercepts$`(Intercept)`)
var(all_intercepts$`(Intercept)`)
# Comparison
lrtest(nested_null_model_1, null_model) # nested model has better fit

# (4.3.) Nested null model with city and state
nested_null_model_2 <- glmer(
  formula = bef_message_bool ~ 1 + (1 | customer_city) + (1 | metro),
  family = binomial(link = "logit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
as.data.frame(VarCorr(nested_null_model_2))[,c("grp","vcov")]
summary(nested_null_model_2)
var(resid(nested_null_model_2))

icc(nested_null_model_2)
AIC(nested_null_model_2)

all_intercepts <- coef(nested_null_model_2)
all_intercepts <- all_intercepts[["customer_state"]]
mean_intercept <- mean(all_intercepts$`(Intercept)`) 
hist(all_intercepts$`(Intercept)`)
var(all_intercepts$`(Intercept)`)
mean(all_intercepts$`(Intercept)`)
dispersion_index <- sd(all_intercepts$`(Intercept)`) / mean(all_intercepts$`(Intercept)`)

nested_null_model_3 <- glmer(
  formula = bef_message_bool ~ 1 + (1 | customer_city) + (1 | customer_state) + (1 | metro),
  family = binomial(link = "logit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
icc(nested_null_model_3)
AIC(nested_null_model_3)
summary(nested_null_model_3)
all_intercepts <- coef(nested_null_model_3)
all_intercepts <- all_intercepts[["customer_state"]]
mean_intercept <- mean(all_intercepts$`(Intercept)`) 
hist(all_intercepts$`(Intercept)`)
var(all_intercepts$`(Intercept)`)
mean(all_intercepts$`(Intercept)`)


# Comparison
lrtest(nested_null_model_2, nested_null_model_3) # Multilevel 

# FULL Comparisons
stargazer(null_model, nested_null_model_1, nested_null_model_2,  type = "text", title = "Table 1. Results")


levels(brazil_df$customer_state)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------ 
# Logit full nested vs. mixed full nested --------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (4.4.) Fit model on full train
full_model <- glm(formula = bef_message_bool 
                  ~ 1
                  + mc_new_idhm
                  + region
                  + new_urbanity
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + other_issue
                  + intimate_goods
                  + experience_goods
                  + item_count_disc
                  + review_sent_wknd
                  + above_median*region,
                  data = broccoli,
                  family = binomial(link = "logit")
)
AIC(full_model)
summary(full_model)
vif(full_model)
stargazer(full_model, type = "text")

summary(lm(average_price ~ region, data = brazil_df))
summary(lm(mc_new_idhm ~ max_price, data = brazil_df))


nested_null_model_3 <- glmer(
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + year
  + dec
  + intimate_goods
  + item_count_disc
  + experience_goods
  + review_sent_wknd
  + max_price_disc*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(nested_null_model_3)

icc(nested_null_model_2)

used_data <- cbind(nested_null_model_2$model$bef_message_bool, fitted.values(nested_null_model_2))
used_data <- as.data.frame(used_data)
used_data <- used_data %>%
  mutate(V3 = ifelse(V1 == 1, V1-V2, V2))

hist(used_data$V3)




nested_null_model_2 <- glmer( # FINAAALllllllllllllllll********
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(nested_null_model_2)
stargazer(nested_null_model_2, type = "text", out = "brazil_full_out.txt")
cc <- confint(nested_null_model_2,parm="beta_")  ## slow (~ 11 seconds)
confint(nested_null_model_2,parm="beta_",method="Wald")  # Faster
ctab <- cbind(est=fixef(gm1),cc)
vif(nested_null_model_2)


se <- sqrt(diag(vcov(nested_null_model_2)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(nested_null_model_2), 
             LL = fixef(nested_null_model_2) - 1.96 * se, 
             UL = fixef(nested_null_model_2) + 1.96 * se)
print(exp(tab), digits=3)



nested_null_model_2_metronly <- glmer( # metro_only
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = brazil_df[brazil_df$udh_indicator == 1,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(nested_null_model_2_metronly)
stargazer(nested_null_model_2_metronly, type = "html", out = "metronly.html")
cc <- confint(nested_null_model_2_metronly,parm="beta_")  ## slow (~ 11 seconds)
confint(nested_null_model_2_metronly,parm="beta_",method="Wald")  # Faster
ctab <- cbind(est=fixef(gm1),cc)


se <- sqrt(diag(vcov(nested_null_model_2_metronly)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(nested_null_model_2_metronly), 
             LL = fixef(nested_null_model_2_metronly) - 1.96 * se, 
             UL = fixef(nested_null_model_2_metronly) + 1.96 * se)
print(exp(tab), digits=3)





nested_null_model_2_nometro <- glmer( # non metro_only
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = brazil_df[brazil_df$udh_indicator == 0,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)

summary(nested_null_model_2_nometro)
stargazer(nested_null_model_2_nometro, type = "html", out = "notmetros.html")

se <- sqrt(diag(vcov(nested_null_model_2_nometro)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(nested_null_model_2_nometro), LL = fixef(nested_null_model_2_nometro) - 1.96 * se, UL = fixef(nested_null_model_2_nometro) + 1.96 * se)
print(exp(tab), digits=3)




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------ 
# Validation -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted 1's
  CustomersSorted <- Evaluate[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate == 1) #total number of real 1's in the evaluation set
  CustomerCumulative=seq(length(Evaluate))/length(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted==1),2,cumsum)/SumChurnReal #cumulative fraction of 1's
  ProbTD = sum(CustomersSorted[1:floor(length(Evaluate)*.1)]==1)/floor(length(Evaluate)*.1) #probability of 1 in 1st decile
  ProbOverall = SumChurnReal / length(Evaluate) #overall probability of 1's
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,length(Evaluate))-CustomerCumulative)),na.rm=T)/length(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted probability of 1's)",ylab="Cumulative fraction of real 1's")
  grid()
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}

confusionMatrix(data = validation_df$pred_check_ins,
                reference = validation_df$actual_check_ins,
                positive = "yes",
                mode = "everything")




validate_full <- glmer( # FINAAAL
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = train,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)


validate_metro <- glmer( # FINAAAL
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = train[train$udh_indicator == 1,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)

validate_non_metro <- glmer( # FINAAAL
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "logit"),
  data = train[train$udh_indicator == 0,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)






validation_function <- function(fitted_model, test_set, multilevel, probability){
  if (multilevel == 1)
  {
    probabilities <- fitted_model %>% predict(test_set, 
                                              type = "response",
                                              allow.new.levels = TRUE)
  } 
  else 
  {
    probabilities <- fitted_model %>% predict(test_set, 
                                              type = "response",
                                              allow.new.levels = FALSE)
  }
  # Perform classification based on probability
  predicted.classes <- ifelse(probabilities > probability, 1, 0)
  predicted.classes
  
  # bind 
  probies <- cbind(as.character(test_set$bef_message_bool), predicted.classes)
  probies <- as.data.frame(probies)
  
  probies <- probies %>%
    mutate(true_positives = ifelse(V1 == "1" & predicted.classes == "1", 1, 0),
           true_negatives = ifelse(V1 == "0" & predicted.classes == "0", 1, 0),
           false_positives = ifelse(V1 == "0" & predicted.classes == "1", 1, 0),
           false_negatives = ifelse(V1 == "1" & predicted.classes == "0", 1, 0),
           hit_rate = ifelse(true_positives == "1" | true_negatives == "1", 1, 0)
    )
  
  # Compute hitrate
  hit_rate <- mean(probies$hit_rate, na.rm = TRUE)
  
  return(hit_rate)
}



validation_function(validate_full, test, 1, 0.50)
validation_function(validate_metro, test[test$udh_indicator == 1,], 1, 0.55)
validation_function(validate_non_metro, test[test$udh_indicator == 0,], 1, 0.55)

actual <- test[test$udh_indicator == 1,]$bef_message_bool
pred_prob_logit <- predict(validate_metro,
                               newdata = test[test$udh_indicator == 1,],
                               type = "response",
                               allow.new.levels = TRUE)
makeLiftPlot(pred_prob_logit, actual, "Logit")

# ----------------------------------------------------------------------------

plot(density(rstandard(full_prob_model, type='deviance')))
lines(density(resid(m1, type='response')), col='red')

scatter.smooth(rstandard(full_prob_model, type='deviance'), col='gray')

scatter.smooth(predict(full_prob_model, type='response'), rstandard(full_prob_model, type='deviance'), col='gray')

probies <- full_prob_model %>% predict(test, type = "response", allow.new.levels = TRUE)
hist(probies)

both <- cbind(probies, train[! is.na(train$product_category_name),]$bef_message_bool)

hist(residuals(full_prob_model))
summary(full_probit_model)
vif(full_probit_model)

# Logit full vs. mixed full (POS/NEG) -----------------------------------------


# (4.5.) Fit null model on fullpositives only
pos_model <- glm(formula = bef_message_bool 
                  ~ 1
                  + mc_new_idhm
                  + region
                  + urbanity_disc
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + intimate_goods
                  + experience_goods
                  + review_sent_wknd
                  + item_count_disc
                  + above_median
                  + region*above_median,
                  data = train_pos,
                  family = binomial(link = "logit")
)
AIC(pos_model)
summary(pos_model)
vif(pos_model)

# (4.6.) Fit null model on negatives only
neg_model <- glm(formula = bef_message_bool 
                 ~ 1
                 + mc_new_idhm
                 + region
                 + urbanity_disc
                 + mc_new_young_ratio
                 + review_score
                 + year
                 + intimate_goods
                 + experience_goods
                 + review_sent_wknd
                 + item_count_disc
                 + above_median
                 + region*above_median,
                 data = train_neg,
                 family = binomial(link = "logit")
)
AIC(neg_model)
summary(neg_model)
vif(neg_model)

stargazer(pos_model, neg_model, type = "text")


# HECKKKKIEEE ----------------------------------------------------------------


vif(heckie_seleccie_full)
heckie_seleccie_full <- glmer( # FINAAAL
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "probit"),
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(heckie_seleccie_full)
# both of these are the same but we go with predict()
probit_lp = predict(heckie_seleccie_full)
#probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)


brazil_df$mills <- mills0

# Truncated fsys 
truncated_brazil_df <- brazil_df[brazil_df$bef_message_bool == 1,]

library(VGAM)
# zero truncated
m1 <- vglm(bef_nwords # FULL BRAZIL
           ~  mc_new_idhm
           + mills
           + region
           + new_urbanity
           + mc_new_young_ratio
           + review_score
           + review_sent_moy
           + year
           + other_issue
           + intimate_goods
           + experience_goods
           + item_count_disc, family = posnegbinomial(), data = truncated_brazil_df)
summary(m1)
coef(m1)
coef(m1)[, "Std. Error"]
stargazer(coeftest(m1), type = "html", out = "zerotrunc_full.html")
AIC(m1)
df.residual(m1)
logLik(m1)
exp(cbind(coef(m1), confint(m1))) # Odds ratios
vif(m1)

se <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 * se)
print(exp(tab), digits=3)
vcov(m1)
residuals(m1)
residuals(heckie_seleccie_full)




# zero truncated
library(VGAM)
m1 <- vglm(bef_nwords)

predict(m1)



hoi <- as.data.frame(cbind(residuals(heckie_seleccie_full), brazil_df$bef_message_bool))
hoi$V2 <- hoi$V2 - 1
hi <- hoi[hoi$V2 == 1,]

hii <- cbind(hi, residuals(m1))


library(ggExtra)
scatter.hist(x=hii$`loglink(munb)`, y=hii$V1, density=TRUE, ellipse=TRUE)


den3d <- kde2d(hii$`loglink(munb)`, hii$V1)
persp(den3d, box=TRUE)

den3d

m1.1 <- vglm(bef_nwords # FULL BRAZIL
           ~  mc_new_idhm
           + mills
           + region
           + new_urbanity
           + mc_new_young_ratio
           + review_score
           + review_sent_moy
           + year
           + other_issue
           + intimate_goods
           + experience_goods
           + item_count_disc, family = posnegbinomial(), data = truncated_brazil_df[truncated_brazil_df$top2box == 1,])
















data_nb <- heckie_seleccie_full
nrow(data_nb)
nrow(truncated_brazil_df)
summary(full_prob_model)
# termplot(full_prob_model)

used_data <- cbind(full_prob_model$model$bef_message_bool, fitted.values(full_prob_model))
used_data <- as.data.frame(used_data)
used_data <- used_data %>%
  mutate(V3 = ifelse(V1 == 1, V1-V2, V2))
# Only interested in deviance for "1" predictions

used_data <- used_data[used_data$V1 == 1,]
scatter.hist(x=truncated_brazil_df$`residuals(full_prob_model, type = "response")`, y=truncated_brazil_df$`residuals(m2)`, density=TRUE, ellipse=TRUE)





den3d <- kde2d(truncated_brazil_df$`residuals(full_prob_model, type = "response")`, truncated_pos$`residuals(m2)`)
persp(den3d, box=TRUE)





m1_metros <- vglm(bef_nwords # FULL emtrosonly
           ~  mc_new_idhm
           + mills
           + region
           + new_urbanity
           + mc_new_young_ratio
           + review_score
           + review_sent_moy
           + year
           + other_issue
           + intimate_goods
           + experience_goods
           + above_median
           + item_count_disc, family = posnegbinomial(), data = truncated_brazil_df[truncated_brazil_df$udh_indicator == 1,])
summary(m1)
coef(m1)
coef(m1)[, "Std. Error"]
stargazer(coeftest(m1), type = "html", out = "zerotrunc_full.html")
Loglik(m1)
library(ipeglim)

forcat <- truncated_brazil_df %>% select(mc_new_idhm, mc_new_young_ratio, new_urbanity)


# Metro
# -----
heckie_seleccie_metro <- glmer( #  METRO ONLY
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "probit"),
  data = brazil_df[brazil_df$udh_indicator == 1,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)

# both of these are the same but we go with predict()
probit_lp = predict(heckie_seleccie_metro)
#probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)


brazil_df[brazil_df$udh_indicator == 1,]$mills <- mills0

# Truncated fsys 
truncated_brazil_df <- brazil_df[brazil_df$bef_message_bool == 1 & brazil_df$udh_indicator == 1,]


m1_metro <- vglm(bef_nwords
           ~  mc_new_idhm
           + mills
           + region
           + new_urbanity
           + mc_new_young_ratio
           + review_score
           + review_sent_moy
           + year
           + other_issue
           + intimate_goods
           + experience_goods
           + item_count_disc, family = posnegbinomial(), data = truncated_brazil_df)






# Metro
# -----
heckie_seleccie_nonmetro <- glmer( #  nonMETRO ONLY
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "probit"),
  data = brazil_df[brazil_df$udh_indicator == 0,],
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)





# Heckman ------------------------------------------------------------------



train$bef_message_bool <- as.integer(train$bef_message_bool) - 1

# (4.5.) Fit probit model on full train
full_prob_model <- glm(formula = bef_message_bool 
                       ~ mc_new_idhm
                       + urbanity_disc
                       + mc_new_young_ratio
                       + total_freight_amount
                       + review_score
                       + year
                       + intimate_goods
                       + experience_goods
                       + review_sent_wknd,
                       data = brazil_df[brazil_df$top2box == 1,],
                       family = binomial(link = "probit"))
summary(full_prob_model)

vif(full_prob_model)
residuals(full_prob_model, type = "response")
hist(residuals(full_prob_model, type = "response"))

train<- cbind(train, residuals(full_prob_model, type = "response"))

# both of these are the same but we go with predict()
probit_lp = predict(full_prob_model)
#probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)

train$mills <- mills0



# Truncated fsys 
truncated_train <- train[train$bef_message_bool == 1,]


# Negative binomial 
m2 <- glm.nb(bef_nwords # FULL BRAZIL
             ~  mc_new_idhm
             + mills
             + region
             + new_urbanity
             + mc_new_young_ratio
             + review_score
             + review_sent_moy
             + year
             + other_issue
             + intimate_goods
             + experience_goods
             + item_count_disc, 
             data = truncated_brazil_df)
summary(m2)
vif(m2)

# Visualss -------------------------------------------------------------------

hist(residuals(m2))

truncated_pos <- cbind(truncated_pos, residuals(m2))

res <- hexbin(truncated_brazil_df$`residuals(full_prob_model, type = "response")`, truncated_brazil_df$`residuals(m2)`, xbins=20)
plot(res)

library(psych)
scatter.hist(x=truncated_brazil_df$`residuals(full_prob_model, type = "response")`, y=truncated_brazil_df$`residuals(m2)`, density=TRUE, ellipse=TRUE)

cor(truncated_pos$`residuals(full_prob_model, type = "response")`, truncated_pos$`residuals(m2)`)



p2 <- ggplot(truncated_pos, aes(x = `residuals(full_prob_model, type = "response")`, y = `residuals(m2)`)) +
  geom_point(alpha = .5) +
  geom_density_2d()

p2

colnames(truncated_pos)

den3d <- kde2d(truncated_pos$`residuals(full_prob_model, type = "response")`, truncated_pos$`residuals(m2)`)
persp(den3d, box=TRUE)


den3d <- kde2d(scale(truncated_pos$`residuals(full_prob_model, type = "response")`), scale(truncated_pos$`residuals(m2)`))

library(plotly)
plot_ly(x =den3d$x,
        y = den3d$y,
        z = den3d$z) %>% add_surface()



plot_ly(x =den3d$x,
        y = den3d$y,
        z = den3d$z) %>% add_surface(
          contours = list(
            z = list(
              show=TRUE,
              usecolormap=TRUE,
              highlightcolor="#ff0000",
              project=list(z=TRUE)
            )
          )
        )

# Can we assume bivariate normal distribution?
library(normwhn.test)

hist(truncated_pos$`residuals(full_prob_model, type = "response")`)

# ---------------------------------------------------------------------------------
data_nb <- m2$model
nrow(data_nb)
nrow(truncated_brazil_df)
summary(full_prob_model)
# termplot(full_prob_model)

used_data <- cbind(full_prob_model$model$bef_message_bool, fitted.values(full_prob_model))
used_data <- as.data.frame(used_data)
used_data <- used_data %>%
  mutate(V3 = ifelse(V1 == 1, V1-V2, V2))
# Only interested in deviance for "1" predictions

used_data <- used_data[used_data$V1 == 1,]


hist(used_data[used_data$V1 == 1,]$V3)
residuals(full_prob_model, type = "response")



# both of these are the same but we go with predict()
probit_lp = predict(full_prob_model)
#probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)


train_pos$mills <- mills0
train_pos$residuals <- 


hist(train_pos$mills)

# Truncated fsys 
truncated_pos <- train_pos[train_pos$bef_message_bool == 1,]

# Linear model

# Zero-truncated negative binomial
m1 <- vglm(bef_nwords ~ new_idhm + mills, family = posnegbinomial(), data = truncated_pos)
summary(m1)
AIC(m1)

# Negative binomial 
m2 <- glm.nb(bef_nwords ~ mc_new_idhm 
             + region 
             + review_score 
             + other_issue 
             + mills
             + , data = truncated_pos)
summary(m2)
hist(residuals(m2))
qqnorm(residuals(m2), pch = 1, frame = FALSE)
qqline(residuals(m2), col = "steelblue", lwd = 2)



library(hexbin)
res <- hexbin(residuals(m2), used_data[used_data$V1 == 1,]$V3, xbins=20)
plot(res)


# Mixed effects negative binomial
m.nb <- glmer.nb(bef_nwords ~ new_idhm + region + review_score + mills + (1 | customer_city), data = truncated_pos, verbose=TRUE)
summary(m.nb)
performance::icc(m.nb)



hurry <- fit_zinbinom1_bs(bef_nwords ~ new_idhm + mills,
                       data = truncated_pos,
                       family=list(family="truncated_nbinom1",link="log"))

hurP1 <- glmmadmb(bef_nchar ~ 1 + new_idhm + mills (1 | customer_city), 
                  data = truncated_pos, family = "binomial")


simpy_glm <- lm(bef_nchar ~ new_idhm + mills, data = truncated)
summary(simpy_glm)
AIC(simpy_glm)
plot(simpy_glm)


# What predicts HDI? ----------------------------------------------------------


hdi_predict <- lm(mc_new_idhm
                  ~ urbanity_disc
                  + mc_new_young_ratio
                  + review_score
                  + year
                  + region
                  + intimate_goods
                  + experience_goods
                  + review_sent_wknd
                  + above_median
                  + diff_pur_est
                  + freight_issue_bool,
                  data = brazil_df)
summary(hdi_predict)                    
hist(residuals(hdi_predict))
plot(hdi_predict)
plot(hdi_predict)
vif(hdi_predict)
plot(cooks.distance(hdi_predict))


hdi_predict <- lm(mc_new_idhm
                  ~ region
                  + review_score
                  + item_count_bin
                  + urbanity_disc
                  + mc_new_young_ratio,
                  data = train)
vif(hdi_predict)
summary(hdi_predict)                    
plot(hdi_predict)

plot(train$mc_new_young_ratio, train$mc_new_idhm)

hist(train$mc_new_young_ratio)

hai <- as.data.frame(train %>% 
                       select(region, 
                              new_idhm,
                              new_young_ratio,
                              diff_pur_est,
                              new_urbanity,
                              bef_message_bool) %>% 
                       group_by(region) %>%
                       summarise(
                         count_idhm = n(),
                         mean_idhm = mean(new_idhm),
                         sd_idhm = sd(new_idhm),
                         mean_yr = mean(new_young_ratio),
                         sd_yr = sd(new_young_ratio),
                         mean_diff = mean(diff_pur_est),
                         mean_urban = mean(new_urbanity),
                         mean_rate = mean((as.integer(bef_message_bool) - 1))))

stargazer(hai, type = "text", align = TRUE)

