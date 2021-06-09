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
# 1. Get correct data representations ------------------------------------------ 
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


# ------------------------ #
# 2. Fixing some variables -----------------------------------------------------
# ------------------------ #

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# For interpretation sake, reverse the 1-0 config of this dummy
# Now it goes: if there was an issue, = 1, otherwise 0. 
brazil_df <- brazil_df %>%
  mutate(other_issue = ifelse(diff_est_deliv > 1, 1, 0),
         late = ifelse(diff_est_deliv > 10, 1, 0),
         early = ifelse(diff_est_deliv < -10, 1, 0))


# Fix NAs for metro by calling them {state} + "county"
brazil_df <- brazil_df %>%
  mutate(metro = ifelse(is.na(metro), paste(as.character(customer_state), "county", sep = "_"), 
                        as.character(metro)))

# December dummy variable
brazil_df <- brazil_df %>%
  mutate(dec = ifelse(review_sent_moy == "dec", 1, 0))


# -------------- #
# 3. Missingness --------------------------------------------------------------
# -------------- #


# ---- 3.1. Assessment ---- 
# ------------------------- #

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

# ---- 3.2. Solution ----
# ----------------------- #

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
# 4. Create above median variables --------------------------------------------
# -------------------------------- #

# ---- 4.1. Basic insights ----
# ----------------------------- #
cat_overview <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(mean = mean(max_price),
            median = median(max_price),
            sd = sd(max_price),
            count = n())

# Now do the same but only unique product_ids
uniq_prodids <- brazil_df[!duplicated(brazil_df$product_id.y), ]
uniq_cat_overview <- uniq_prodids %>%
  group_by(product_category_name) %>%
  summarise(mean = mean(max_price),
            median = median(max_price),
            sd = sd(max_price),
            quantile = as.numeric(quantile(max_price)[4]),
            max = max(max_price),
            min = min(max_price),
            count = n())
stargazer(uniq_cat_overview, summary = FALSE, type = "html", out = "Unique_cat_overview.html")

nrow(uniq_cat_overview[uniq_cat_overview$count < 91,])

# ---- 4.2. Incorporate with data set ----
# ---------------------------------------- #

# Merge
brazil_df <- merge(uniq_cat_overview[,c("product_category_name", "median", "quantile")],
                      brazil_df,
                      by.x = "product_category_name",
                      by.y = "product_category_name",
                      all.y = TRUE)

brazil_df <- brazil_df %>% 
  rename(median_of_cat = median,
         third_quartile_of_cat = quantile)

brazil_df <- brazil_df %>%
  mutate(above_median = ifelse(max_price > median_of_cat, 1 , 0),
         above_median_extent = max_price - median_of_cat,
         above_third_quartile = ifelse(max_price > third_quartile_of_cat, 1, 0))

# ---- 4.3. How often does above_median occur? ---- 
# ------------------------------------------------- #
table(brazil_df$above_median)
table(brazil_df$above_third_quartile)

# ----------------- #
# 5. First Insights -----------------------------------------------------------
# ----------------- #

# ---- 5.1. Check how many people have more than 2 orders ----
# ------------------------------------------------------------ #

more_than_2 <- brazil_df %>% 
  group_by(customer_unique_id) %>% 
  summarise(count = n())

more_than_2 <- more_than_2 %>% 
  mutate(two_or_one = ifelse(count >2, 0, 1), 
         one = ifelse(count == 1, 1, 0))

mean(more_than_2$two_or_one)
mean(more_than_2$one)

# ---- 5.2 Check how many people per nest ----
# -------------------------------------------- #

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


# ---- 5.3 Check how many items per order? ----
# --------------------------------------------- # 
table(brazil_df$item_count)


# ---- 5.4. Visualization of state counts and stuff ----
# ------------------------------------------------------ #

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
windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
# Do visual again
ggplot(pop, aes(fill=bef_message_bool, y=n, x=hdi_class_col)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3.3, aes(label = test, family = "Times New Roman"), vjust = -1) + 
  ylab("Case count (n)") + 
  xlab("Human Development Index Category") +
  theme_bw() + 
  labs(title = expression(bold("Figure 5")),
       subtitle = expression(italic("State Counts of HDI and Review Incidence Across Regions"))) +
  labs(fill = expression(atop("Review provided:", paste("No (0), Yes (1)")))) + 
  theme(text=element_text(size=13,  family="Times New Roman")) +
  facet_wrap( ~ region, 
              scales = "free",
              labeller =labeller(region = c(
                "centerwest" = "Central-West (n = 3,467)",
                "north" = "North (n = 1,748)",
                "northeast" = "Northeast (n = 8,884)",
                "south" = "South (n = 13,533)",
                "southeast" = "Southeast (n = 66,907)",
                "full" = "Full (n = 94,539)"))) +scale_fill_manual(values=c("coral", "steelblue"))


# Other figures

ggplot(brazil_df, aes(x= new_idhm)) + 
  geom_histogram(color="black", fill="coral", size = 0.1, bins = 40) + 
  facet_wrap(~udh_indicator) +
  xlim(0, 1.00) + 
  theme_dark() +
  labs(title = expression(bold("Figure 3")),
       subtitle = expression(italic("Difference in Frequencies of Human Development Indices between UDH (1) and Municipal Cases (0)"))) +
  xlab("Human Development Index associated with the location of the related to case") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

ggplot(brazil_df, aes(x= new_urbanity)) + 
  geom_histogram(color="black", fill="coral", size = 0.1) + 
  facet_wrap(~udh_indicator) +
  theme_bw() +
  labs(title = expression(bold("Figure 4")),
       subtitle = expression(italic("Difference in Frequencies of Urbanicity Cases between UDH (1) and Municipal Cases (0)"))) +
  xlab("Ratio of urban population to total population of a single spatial unit associated with a case") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))



# ---- 5.5. Histogram of message length ----
# ------------------------------------------ #

windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
note = expression(paste(italic("Note. "), "Zero-length reviews were excluded from this plot."))
ggplot(brazil_df[brazil_df$bef_message_bool == 1,], aes(x= log(bef_nwords))) + 
  geom_histogram(color="black", fill="coral", size = 0.1, bins = 15) +
  labs(caption = note) +
  labs(title = expression(bold("Figure 7")),
       subtitle = expression(italic("Distribution of Review Message Length Frequencies"))) +
  xlab("Number of Words") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 18),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0))


# ---- 5.6. sample overview in table ---- 
# --------------------------------------- #
library(data.table)
hai <- as.data.frame(brazil_df %>% 
                       select(region, 
                              new_idhm,
                              new_young_ratio,
                              diff_pur_est,
                              new_urbanity,
                              bef_message_bool,
                              review_score) %>% 
                       group_by(region) %>%
                       summarise(
                         count_idhm = n(),
                         mean_idhm = mean(new_idhm),
                         sd_idhm = sd(new_idhm),
                         mean_yr = mean(new_young_ratio),
                         sd_yr = sd(new_young_ratio),
                         mean_diff = mean(diff_pur_est),
                         sd_diff = sd(diff_pur_est),
                         mean_urban = mean(new_urbanity),
                         sd_urban = sd(new_urbanity),
                         mean_review = mean(as.numeric(review_score)),
                         sd_review = sd(as.numeric(review_score)),
                         mean_rate = mean((as.integer(bef_message_bool) - 1))))

hii<-transpose(hai) # Use this and fix the other stuff in google sheets.

stargazer(hii, summary = FALSE, type = "html", out = "overview_continuous.html")

# ---------------------- #
# 5. Train test split   -------------------------------------------------------
# ---------------------- #


train_size <- floor(0.90 * nrow(brazil_df))

set.seed(777)
train_ind <- sample(seq_len(nrow(brazil_df)), size = train_size)

train <- brazil_df[train_ind, ]
test <- brazil_df[-train_ind, ]


# -------------------------------------- #
# 7. Mean centering continuous variables ---------------------------------------
# -------------------------------------- #

center_scale <- function(x) {
  scale(x, scale = TRUE)
}

# apply it
brazil_df$cs_new_idhm <- center_scale(brazil_df$new_idhm)
brazil_df$cs_new_young_ratio <- center_scale(brazil_df$new_young_ratio)
brazil_df$cs_new_urbanity <- center_scale(brazil_df$new_urbanity)
brazil_df$cs_bef_nwords <- center_scale(brazil_df$bef_nwords)


# ------------------------------------ #
# Final before sending it off to STATA ----------------------------------------
# ------------------------------------ #
library(fastDummies)

brazil_df$log_nwords <- log(brazil_df$bef_nwords + 1)

brazil_df <- brazil_df %>%
  mutate(positive = ifelse(review_score == 4 | review_score == 5, 1,0),
         neutral = ifelse(review_score == 3, 1, 0),
         negative = ifelse(review_score == 1 | review_score == 2, 1, 0))

dataf <- dummy_cols(brazil_df, select_columns = "review_sent_moy")

write.csv(dataf, file = "englishmaninnewyork.csv")

# -------------- #
# Heckman Tryout # ------------------------------------------------------------
# -------------- #

library(sampleSelection)
library(car)

heckie <- selection(select = bef_message_bool 
                    ~ cs_new_idhm
                    + cs_new_urbanity
                    + region
                    + item_count
                    + review_sent_wknd
                    + early
                    + late
                    + negative
                    + positive
                    + intimate_goods
                    + experience_goods
                    + review_sent_moy
                    + year
                    + product_photos_qty
                    + above_median*region
                    ,
                    
                    outcome = log(bef_nwords)
                    ~ cs_new_idhm
                    + cs_new_urbanity
                    + region
                    + item_count
                    + negative
                    + positive
                    + early
                    + late
                    + intimate_goods
                    + experience_goods
                    + review_sent_moy
                    + year
                    + product_photos_qty
                    + above_median*region
                    ,
                    data = brazil_df)
summary(heckie)
stargazer(heckie, part = "selection", type = "text")
hist(fitted(heckie, part = "selection"))
hist(fitted(heckie, part = "outcome"))
fits <- (fitted(heckie, part = "outcome"))
hist(fits)
vif(heckie)
library(jtools)

plot_summs(prbit, scale = TRUE)
selections <- fitted(heckie, part = "selection")
with_selections <- cbin(brazil_df, selections)
selections_valences <- with_selections[, c("review_score", "selections", "bef_message_bool")]


selections_valences <- selections_valences %>%
  mutate(err = (as.numeric(bef_message_bool) - 1) - selections,
         ab_err = abs(err),
         scale_ab_err = scale(ab_err))

selections_valences %>% group_by(review_score) %>%
  summarise(mean_err = mean(scale_ab_err))

margins(heckie)

library(psych)
scatter.hist(x=fitted(heckie, part = "selection"), 
             y=fitted(heckie, part = "outcome"), 
             density=FALSE, ellipse=FALSE, smooth = FALSE)


###################################################################


new_dfje <- as.data.frame(cbind(residuals(heckie, part = "selection"),
                  residuals(heckie, part = "outcome")))

new_dfje <- cbind(new_dfje, brazil_df$review_score)



library(MASS)
library(viridis)

library(ggExtra)
new_dfje <- new_dfje %>%
  rename("Values of residuals outcome model" = V2,
         "Values of residuals selection model" = V1)

names(new_dfje)[names(new_dfje) == "brazil_df$review_score"] <- "Review score"

windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
p <- ggplot(new_dfje, aes(`Values of residuals selection model`, `Values of residuals outcome model`)) + 
  geom_point(aes(colour = `Review score`))+
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme_bw()  +
  theme(legend.position="bottom") +
  labs(title = expression(bold("Figure 8")),
       subtitle = expression(italic("Bivariate Distribution of Residual Plot from the Selection and Outcome Models"))) +
  theme(text = element_text(family = "Times New Roman", size = 15),
        plot.title = element_text(size = 15, vjust = -0.7),
        plot.subtitle = element_text(size = 15, vjust = -0.7),
        plot.caption = element_text(hjust = 0))
p
ggExtra::ggMarginal(p, type = "histogram", fill = "coral", bins = 100)

p_2 <- ggplot(new_dfje, aes(`Values of residuals selection model`, `Values of residuals outcome model`)) + 
  geom_bin2d(bins = 300) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()  +
  labs(title = expression(bold("Figure 8")),
       subtitle = expression(italic("Bivariate Distribution of Residual Plot from the Selection and Outcome Models"))) +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0))
p_2


library(broomExtra)

library(dotwhisker)

dwplot(prbit, df_method = "wald")
m1_df <- tidy(prbit) # create data.frame of regression results
m1_df # a tidy data.frame available for dwplot

m2 <- as.data.frame(coef(summary(heckie))[, c("Estimate","Std. Error")])
m2<- m2 %>% rownames_to_column("term")
m2 <- m2 %>% rename("estimate" = Estimate)
m2 <- m2 %>% rename("std.error" = `Std. Error`)
dwplot(m2) + theme_bw()  +
  labs(title = expression(bold("Figure 8")),
       subtitle = expression(italic("Bivariate Distribution of Residual Plot from the Selection and Outcome Models"))) +
  theme(text = element_text(family = "Times New Roman", size = 15),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(hjust = 0))



qqPlot(fitted(heckie, part = "outcome"))

quants <- fitted(heckie, part = "outcome")

dev_quants <- quants[quants < 3 & !is.na(quants)]
labeltjes <- labels(dev_quants)

brazil_df <- brazil_df %>% 
  mutate(weird_guys <- ifelse(rownames(brazil_df) %in% labeltjes, 1, 0))

weird_preds <- glm(`weird_guys <- ifelse(rownames(brazil_df) %in% labeltjes, 1, 0)` 
                   ~ new_idhm
                   + new_urbanity
                   + new_young_ratio
                   + region
                   , data = brazil_df, 
                   family = binomial(link = "probit")
                     
                     )
summary(weird_preds)

prbit <- glm(bef_message_bool 
             ~ new_idhm
             + new_urbanity
             + region
             + item_count
             + review_sent_wknd
             + intimate_goods
             + experience_goods
             + review_sent_moy
             + year
             + positive
             + negative
             + early
             + late
             + region
             + above_median
             + above_median*region
             + product_photos_qty
             , data = brazil_df,
             family = binomial(link = "probit"))
summary(prbit)
AIC(prbit)
vif(prbit)
plot(prbit)
margins(prbit)
library(mfx)

mfxer <- probitmfx(
  bef_message_bool 
  ~ new_idhm
  + new_urbanity
  + new_young_ratio
  + region
  + item_count
  + review_sent_wknd
  + other_issue
  + above_median
  + intimate_goods
  + experience_goods
  + review_sent_moy
  + top2box
  + year
  + above_median*region
  , data = brazil_df
  , atmean = TRUE)

mfxer


truncated_brazil_df <- brazil_df[brazil_df$bef_message_bool == 1,]

lm <- lm(log(bef_nwords)
         ~ new_idhm
         + new_urbanity
         + new_young_ratio
         + region
         + item_count
         + other_issue
         + above_median
         + intimate_goods
         + experience_goods
         + review_sent_moy
         + year
         + above_median*region
         , data = truncated_brazil_df)


vif(lm)
plot(cooks.distance(lm))

# cor matrix
library(corrplot)
corrie <- cor(brazil_df[,c("new_idhm",
                 "new_urbanity",
                 "new_young_ratio",
                 "item_count")])
corrplot(corrie, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

install.packages("PerformanceAnalytics")



# Hand marginale ffectrs

coffies <- coef(heckie)
coffies <- coffies[2:32]


library("stats")
m <- lm(y ~ x1 * x2 * x3)
cf <- coef(m)[-1] # drop beta_0
me_x1 <- cf[1] + cf[4]*x2 + cf[6]*x3 + cf[7]*x2*x3
me_x2 <- cf[2] + cf[4]*x1 + cf[5]*x3 + cf[7]*x1*x3
me_x3 <- cf[3] + cf[5]*x2 + cf[6]*x1 + cf[7]*x1*x2

























# Above median
above_med <- 

# ------------------------ #
# 6. Modeling Outcome part ----------------------------------------------------
# ------------------------ #


# ---- 6.1. Main probit model with full brazil_df dataset ----
# ------------------------------------------------------------ #

glm_probit <- glmer( 
  formula = bef_message_bool 
  ~ 1
  + cs_new_idhm
  # + region
  + cs_new_urbanity
  + cs_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "probit"),
  nAGQ = 0,
  data = brazil_df,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)

summary(glm_probit)
AIC(glm_probit)
saveRDS(glm_probit, file = "probit_full.RDS")

# Quantile Residuals
glm_probit_simulation <- simulateResiduals(fittedModel = glm_probit, plot = T)
# histogram of DARHma residuals
hist(residuals(glm_probit_simulation))
# normal plot of quantile residuals
hist(residuals(glm_probit_simulation, 
               quantileFunction = qnorm, 
               outlierValues = c(-7,7)))



# ---- 6.2. Metros only ----
# -------------------------- #



# ---- 6.3. Municipality only ----
# -------------------------------- #




rev_star_probit <- probitmfx(top2box ~
                       new_idhm +
                       new_urbanity +
                       region + 
                       freight_issue_bool +
                       review_sent_moy +
                       year +
                       intimate_goods +
                       experience_goods +
                       item_count, 
                       data = brazil_df[brazil_df$udh_indicator == 1,],
                       atmean = TRUE)

rev_star_probit
vif(rev_star_probit)

mean(brazil_df)






# predict.merMod
glm_probit_full_lp = predict(glm_probit)
# Get mills
mills_full = dnorm(glm_probit_full_lp)/pnorm(glm_probit_full_lp) 
hist(mills_full)
saveRDS(mills_full, file = "mills_full.RDS") # To retrieve it when visualizations

# add mills to df
brazil_df$mills <- mills

# ---- 6.2. Main Linear Regression model with full brazil_df ----
# --------------------------------------------------------------- #

library(DHARMa) # Residuals of (generalized) linear mixed models
library(JWileymisc)
citation("DHARMa")
vignette("DHARMa")







# ---- 6.2.1. Non-multilevel linear model ----
# -------------------------------------------- #


reg_lm <- lm(
  formula = bef_nwords
  ~ 1
  + mills
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
  + item_count
  + max_price*region,
  data = truncated_brazil_df)
AIC(reg_lm)
summary(reg_lm)
vif(reg_lm)



truncated_brazil_df <- brazil_df[brazil_df$bef_message_bool == 1,]


# normal mod
lin_mod_fixed <- lm(bef_nwords
                    ~ 
                    #+ mills
                      cs_new_idhm
                    + region
                    + cs_new_urbanity
                    + cs_new_young_ratio
                    + review_score
                    + review_sent_moy
                    + year
                    + other_issue
                    + intimate_goods
                    + experience_goods
                    + item_count
                    + weekend,
                    data = truncated_brazil_df)

summary(lin_mod_fixed)
vif(lin_mod_fixed)

lin_mod_fixedlinear_mod <- lmer(
  formula = bef_nwords
  ~ 1
  #+ mills
  + cs_new_idhm
  + region
  + cs_new_urbanity
  + cs_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count
  + (1 | customer_city),
  REML = FALSE,
  data = truncated_brazil_df)
AIC(linear_mod)
summary(linear_mod)
performance:icc(linear_mod)
saveRDS(linear_mod, file = "linear_full.RDS")

hist(residuals(linear_mod))
qqnorm(residuals(linear_mod))
vif(linear_mod)
extract_eq(linear_mod)

# ---- 6.3. Alternative outcome models  ----
# ------------------------------------------ # 
library(equatiomatic)
# negative binomial
nb_mod <- glmer.nb(formula = bef_nwords
  ~ 1
  + mills
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
  + item_count
  + (1  | customer_city),
  data = truncated_brazil_df,
  nAGQ = 0,
  verbose=TRUE,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  ))

summary(nb_mod)

saveRDS(nb_mod, file = "negative_binomial_full.RDS")

AIC(nb_mod)
hist(residuals(nb_mod))
qqnorm(residuals(nb_mod))

plot(nb_mod)

simulationOutput <- simulateResiduals(fittedModel = nb_mod, plot = T)
residuals(simulationOutput)
hist(residuals(simulationOutput)) # Uniform (flat) IS WHAT WE WANT!
qqnorm(residuals(simulationOutput))

hist(residuals(simulationOutput, 
               quantileFunction = qnorm, 
               outlierValues = c(-7,7)))

boxplot(residuals(simulationOutput))



# Dunn smith residuals.
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12552


hist(residuals(simulationOutput, 
               quantileFunction = qnorm, 
               outlierValues = c(-7,7)))


# Gamma Regression
gamma <- glmer(formula = bef_nchar
             ~ 1
             + mills
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
             + item_count
             + (1 | customer_city),
            data = truncated_brazil_df,
            family=Gamma(link = "log"),
            control = glmerControl(
              optimizer = "bobyqa", 
              optCtrl = list(maxfun=2e5)
            ))

summary(gamma)
AIC(gamma)



simulationOutput <- simulateResiduals(fittedModel = nb_mod, plot = T)
residuals(simulationOutput)
hist(residuals(simulationOutput))





(coef(linear_mod[4]))
exp(coef(gamma))


# Which one is best?
library(lmtest)
anova(nb_mod, gamma) 





# Influential STUFFFIES

library(influence.ME)

cooks.distance(linear_mod)

summary(linear_mod)

qqnorm(residuals(linear_mod))




# Are values even influential in a maximum likelihood thing?


# Sped up
glm_probit_nagq <- glmer( 
  formula = bef_message_bool 
  ~ 1
  + cs_new_idhm
  + region
  + cs_new_urbanity
  + cs_new_young_ratio
  + review_score
  + review_sent_moy
  + year
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + (1 | customer_city),
  family = binomial(link = "probit"),
  data = brazil_df,
  nAGQ = 0,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(glm_probit_nagq)
library(equatiomatic)
library(ggeffects)

extract_eq(glm_probit_nagq)
ggpredict(glm_probit_nagq, "region")

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5970551/

# ---- robustness checks ----------------------------------------------------

# ---- 6.4. Positives only  ----
# ------------------------------ #



# ---- 6.5. Negatives only  ----
# ------------------------------ #


# ---- 6.6. Freight issues only  ----
# ----------------------------------- #


# ---- 6.7. No freight issues  ----
# --------------------------------- #


# ---- 6.8. North  ----
# --------------------- #

# ---- 6.8. South  ----
# --------------------- #









# Bootstrapped confidence intervals
confint(glm_probit, 
        level = 0.95,
        method = "boot", 
        nsim = 10)



data("mtcars")
mpg = mtcars$mpg
n = length(mpg)
print(mean(mpg))
hist(x = mpg, probability = TRUE, xlab = "MPG", main = "Histogram of MPG")




B = 10 ## number of bootstraps
results = numeric(B) ## vector to hold results
for(b in 1:B){
  i = sample(x = 1:n, size = n, replace = TRUE) ## sample indices
  bootSample = mpg[i] ## get data
  thetaHat = mean(bootSample) ## calculate the mean for bootstrap sample
  results[b] = thetaHat ## store results
}

hist(x = results, probability = TRUE, 
     main = "Bootstrapped Samples of Mean_mpg",
     xlab = "theta estimates")


n_bootstrap <- 100
standard_error <- matrix(0, 36, n_bootstrap)
fixed_effect <- matrix(0, 36, n_bootstrap)
lower_ci <- matrix(0, 36, n_bootstrap)
upper_ci <- matrix(0, 36, n_bootstrap)
p_values <- matrix(0, 36, n_bootstrap)


counter <- 1
for (i in 1:n_bootstrap){
  sampy <- brazil_df[sample(nrow(brazil_df), nrow(brazil_df), replace = TRUE),]
  glm_probit <- glmer( 
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
    data = sampy[sampy$udh_indicator == 1,],
    nAGQ = 0,
    control = glmerControl(
      optimizer = "bobyqa", 
      optCtrl = list(maxfun=2e5)
    )
  )
  all_se <- sqrt(diag(vcov(glm_probit)))
  connie <- confint(glm_probit,parm="beta_",method="Wald")
  standard_error[1:36,counter] <- all_se
  lower_ci[1:36, counter] <- connie[1:36, 1]
  upper_ci[1:36, counter] <- connie[1:36, 2]
  fixed_effect[1:36, counter] <- fixef(glm_probit) 
  p_values[1:36, counter] <- coef(summary(glm_probit))[,4]
  
  
  print(counter)
  counter <- counter + 1
}














# https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html



# Speed up 
# nAGQ = 0
# bobyqa --> nloptwrap
# calc.derivs = FALSE 

# Bootmer 

# Disable convergence tests + less acurate --> [g]lmerControl(calc.derivs = FALSE)

library(optimx)
library(lmerTest)


fast_glm_probit <- glmer( # FINAAAL
  formula = bef_message_bool 
  ~ 1
  + mc_new_idhm
  + region
  + new_urbanity
  + mc_new_young_ratio
  + review_score
  + year
  + review_sent_moy
  + other_issue
  + intimate_goods
  + experience_goods
  + item_count_disc
  + review_sent_wknd
  + above_median*region
  + (1 | customer_city),
  family = binomial(link = "probit"),
  data = brazil_df[brazil_df$udh_indicator == 0,],
  nAGQ = 0,
  control = glmerControl(
    optimizer = "optimx", calc.derivs = FALSE,
    optCtrl = list(method = "nlminb", 
                   starttests = FALSE, 
                   kkt = FALSE)))
summary(fast_glm_probit)


vif(fast_glm_probit)

cc <- confint(glm_probit,parm="beta_")  ## slow (~ 11 seconds)

  
anova(glm_probit, type=2, ddf="Kenward-Roger")


# I could bootstrap the super fast model.
confint(fast_glm_probit,parm="beta_",method="Wald")  # Faster



# Bootstrapping 
# -------------
library(margins)

bootMer(fast_glm_probit)




margins(glm_probit, brazil_df[brazil_df$udh_indicator == 1,])




# Bootstrapping
library(lmeresampler)


bootMer(glm_probit, nsim = 1, type = "parametric", re.form = NA)


coef(glm_probit$region)

bootstrap(glm_probit, )


vcmodA <- lmer(scale(mc_new_idhm) ~ 1 + (1 | customer_city), data = brazil_df)

mySumm <- function(.) {
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}




# Alternative fast 


glm_probit <- glmer(
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
  nAGQ = 0,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun=2e5)
  )
)
summary(glm_probit)
warnings(glm_probit)



# --------------------- #
# ?. Normal logit model --------------------------------------------------------
# --------------------- #

myprobit <- glm(bef_message_bool 
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
                + above_median*region, 
                family = binomial(link = "probit"), 
                data = brazil_df)



summary(myprobit)
summary(glm_probit)



# ------------------- #
# ?. Stratification   ----------------------------------------------------------
# ------------------- #

# positive / negative split
# Freight issue split
# Region split? 
