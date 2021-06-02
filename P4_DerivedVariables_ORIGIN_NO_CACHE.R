# USED files:
#            full_geomerged_df_3.csv
# 
# WRITE files:
#            full_geomerged_df_4.csv

library(lubridate)
library(dplyr)

input <- read.csv("full_geomerged_df_3.csv")
brazil_df <- input


# --------- #
# 1. Rename # ---------------------------------------------------------------- #
# --------- #

brazil_df <- brazil_df %>%
  rename(bef_nchar_perword = nchar_perword)


# ------------- #
# 2. NA to zero # ------------------------------------------------------------ #
# ------------- #


brazil_df <- brazil_df %>%
  mutate(new_total_rural = ifelse(is.na(new_total_rural), 0, new_total_rural))

colSums(is.na(brazil_df))


# ------------ #
# 3. Date data # ------------------------------------------------------------- #
# ------------ #

# To date format
brazil_df <- brazil_df %>%
  mutate(
    review_creation_date = as.Date(review_creation_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    review_answer_timestamp = as.Date(review_answer_timestamp,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_purchase_timestamp = as.Date(order_purchase_timestamp, 
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_approved_at = as.Date(order_approved_at,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_delivered_carrier_date = as.Date(order_delivered_carrier_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_delivered_customer_date = as.Date(order_delivered_customer_date,
                                      format = "%Y-%m-%d %H:%M:%S"),
    order_estimated_delivery_date = as.Date(order_estimated_delivery_date,
                                      format = "%Y-%m-%d %H:%M:%S")
        )

brazil_df <- brazil_df %>%
  mutate(
    diff_est_deliv = order_estimated_delivery_date - order_delivered_customer_date,
    diff_pur_est  = order_estimated_delivery_date - order_purchase_timestamp,
    diff_pur_deliv = order_delivered_customer_date - order_purchase_timestamp,
    diff_rev_crea_ans = review_creation_date - review_answer_timestamp,
    diff_rev_est_ans = order_estimated_delivery_date - review_answer_timestamp,
    diff_rev_deliv_ans = order_delivered_customer_date - review_answer_timestamp
        )

brazil_df <- brazil_df %>%
  mutate(
    # show weekday of a date
    review_sent_dow = wday(review_creation_date, label = TRUE),
    review_answer_dow = wday(review_answer_timestamp, label = TRUE),
    # Dummy variable weekend yes/no
    review_sent_wknd = ifelse(
      review_sent_dow == "zo" | review_sent_dow == "za", 1, 0),
    review_answer_wknd = ifelse(
      review_answer_dow == "zo" | review_answer_dow == "za", 1, 0),
    # Month for seasonality
    review_sent_moy = month(review_creation_date, label = TRUE)
        )

# Year as seperate variables
brazil_df<- brazil_df %>%
  mutate(
    y_2016 = ifelse(year(order_purchase_timestamp) == "2016", 1, 0),
    y_2017 = ifelse(year(order_purchase_timestamp) == "2017", 1, 0),
    y_2018 = ifelse(year(order_purchase_timestamp) == "2016", 1, 0)
        )
# Year as 1 factor variable 
brazil_df$year <- "2016"
brazil_df <- brazil_df %>%
  mutate(year = ifelse(y_2017 == 1, "2017", year),
         year = ifelse(y_2018 == 1, "2018", year),
         year = as.factor(year))

brazil_df <- brazil_df %>%
  mutate(sent_sun = ifelse(review_sent_dow == "zo", 1, 0),
         sent_mon = ifelse(review_sent_dow == "ma", 1, 0),
         sent_tue = ifelse(review_sent_dow == "di", 1, 0),
         sent_wed = ifelse(review_sent_dow == "wo", 1, 0),
         sent_thu = ifelse(review_sent_dow == "do", 1, 0),
         sent_fri = ifelse(review_sent_dow == "vr", 1, 0),
         sent_sat = ifelse(review_sent_dow == "za", 1, 0)
        )

# -------------------------------- #
# 4. Message and title derivatives # ------------------------------------------
# -------------------------------- #

# Create dummy for message vs no message [BASED ON AFTER LEMMA]
brazil_df <- brazil_df %>%
  mutate(review_comment_message = as.character(review_comment_message),
         message_length = nchar(review_comment_message),
         message_bool = ifelse(is.na(review_comment_message), 0, 1)
        )
  
# Create dummy for message vs no message [BASED ON BEFORE LEMMA]
brazil_df <- brazil_df %>%
  mutate(bef_message_bool = ifelse(bef_nchar == 0, 0, 1))

# Create a dummy variable for title message
brazil_df <- brazil_df %>%
  mutate(review_comment_title = as.character(review_comment_title),
         title_length = nchar(review_comment_title),
         title_bool = ifelse(is.na(review_comment_title), 0, 1)
        )

# Create a dummy variable for title OR, AND message
brazil_df <- brazil_df %>%
  mutate(title_or_message = ifelse(message_bool == 1 | title_bool == 1, 1, 0),
         title_and_message = ifelse(message_bool == 1 & title_bool == 1, 1, 0),
         title_nor_message = ifelse(message_bool == 0 & title_bool == 0, 1, 0)
        )

# Top2box transformation
brazil_df <- brazil_df %>%
  mutate(top2box = ifelse(review_score > 3, 1, 0))


# ---------------------------------- #
# 5. SEC categorizations of products # ----------------------------------------
# ---------------------------------- #

# Work in progress! 
table(brazil_df$product_category_name)

prop.table(table(brazil_df$product_category_name,
                 brazil_df$bef_message_bool), margin = 1)


# Taxonomy Donal Vitaliano, 2007
search_goods <- c("furniture_bedroom",
                  "furniture_living_room",
                  "furniture_bedroom",
                  "office_furniture",
                  "kitchen_dining_laundry_garden_furniture ",
                  "music",
                  "pet_shop",
                  "housewares",
                  "books_technical",
                  "toys",
                  "books_general_interest",
                  "stationery",
                  ""
                 )

experience_goods <- c("auto",
                      "food",
                      "drinks",
                      "food_drink",
                      "home_appliances",
                      "home_appliances_2",
                      "fashio_female_clothing",
                      "fashion_male_clothing",
                      "fashion_bags_accessories",
                      "fashion_childrens_clothes",
                      "fashion_sport",
                      "fashion_shoes",
                      "security_and_services",
                      "telephony",
                      "la cuisine",
                      "arts_and_craftmanship",
                      "fashion_shoes",
                      "party_supplies",
                      "musical_instruments",
                      "computers",
                      "construction_tools_tools",
                      "construction_tools_garden"
                      
                     )

intimate <- c("baby",
              "perfumery",
              "diapers_and_hygiene",
              "health_beauty",
              "fashion_underwear_beach")



brazil_df <- brazil_df %>%
  mutate(search_goods = ifelse(product_category_name %in% search_goods, 1, 0),
         experience_goods = ifelse(product_category_name %in% experience_goods, 1, 0),
         intimate_goods = ifelse(product_category_name %in% intimate, 1, 0)
        )

test <- brazil_df %>%
  select(product_category_name,
         search_goods,
         experience_goods,
         intimate_goods)


product_cats <- brazil_df %>%
  group_by(product_category_name) %>%
  summarise(freq = n(),
            mes_ratio = mean(message_bool),
            mean_price = mean(max_price),
            spread_sd = sd(max_price),
            char_length = mean(bef_nchar)) # this is wrong, as it takes into asccount many zeros



# -------------------- #
# 6. Regions variables # ------------------------------------------------------
# -------------------- #

c_north <- c("AC","AP","AM","PA", "RO", "RR", "TO")
c_south <- c("SC", "RS", "PR")
c_southeast <- c("SP", "RJ", "MG", "ES")
c_northeast <- c("AL", "BA", "CE", "MA", "RN", "SE", "PI", "PB", "PE")
c_centerwest <- c("MT", "MS", "GO", "DF")

brazil_df$north <- 0
brazil_df$south <- 0
brazil_df$southeast <- 0
brazil_df$northeast <- 0
brazil_df$centerwest <- 0

brazil_df <- brazil_df %>% 
  mutate(north = ifelse(customer_state %in% c_north, 1,0),
         south = ifelse(customer_state %in% c_south, 1,0),
         southeast = ifelse(customer_state %in% c_southeast, 1,0),
         northeast = ifelse(customer_state %in% c_northeast, 1,0),
         centerwest = ifelse(customer_state %in% c_centerwest, 1,0),
        )

brazil_df <- brazil_df %>% 
  mutate(region = ifelse(north == 1, "north", ""),
         region = ifelse(northeast == 1, "northeast", region),
         region = ifelse(centerwest == 1, "centerwest", region),
         region = ifelse(southeast == 1, "southeast", region),
         region = ifelse(south == 1, "south", region),
         region = ifelse(customer_state == "DF", "southeast", region), # Belongs to southeast, culturally
         region = as.factor(region))

# --------------------------------------- #
# 7. Distinguish freight-related messages # -----------------------------------
# --------------------------------------- #

# Due to lemmatization we don't need to worry about tenses
listje <- c("receb", # received
            "aguar", # wait 
            "ainda", # yet 
            "faltou", # missed
            "faltar", # missed 
            "incompleto", # incomplete 
            "nunca chegar", # never came / arrived
            "chegar", # To arrive
            "entregar", # deliver 
            "nao entregar", # not delivered 
            "antar do prazo", # before the term / deadline
            "prazo" # term / deadline
           )

# To fill up
brazil_df$freight_issue_bool <- 0

# If any of the words in the list can be found, add "1"
for (i in listje){
  brazil_df$freight_issue_bool <- ifelse(
    grepl(i, brazil_df$message_and_title), 
    1, 
    brazil_df$freight_issue_bool)
}

# Sanity test to see if it worked
test_out <- brazil_df[
  brazil_df$freight_issue_bool == 1, 
  c("review_comment_message",
    "freight_issue_bool",
    "review_score"
    )]

# When order is delivered after the estimated date, we name it "order_issue"
brazil_df <- brazil_df %>%
  mutate(other_issue = ifelse(diff_est_deliv > 1, 1, 0))


# ----------------- #
# 8. Discretization # ----------------------------------------------------------
# ----------------- #
# Beuzen, T., Marshall, L., & Splinter, K. D. (2018)
library(CORElearn)
library(arulesCBA)

brazil_df <- brazil_df %>%
  mutate(bef_message_bool = as.factor(bef_message_bool))

# urbanity
# --------
disc_urbanity <- discretizeDF.supervised(
  bef_message_bool ~ new_urbanity,
  brazil_df[,c("bef_message_bool", "new_urbanity")])
# apparently 0.85 is fine.
table(disc_urbanity$new_urbanity)
brazil_df <- brazil_df %>%
  mutate(urbanity_disc = ifelse(new_urbanity > 0.84, 1, 0))
# check: did it work? 
testje <- brazil_df %>%
  select(new_urbanity, urbanity_disc)

# HDI (will be done manually anyways following official categories)
# -----------------------------------------------------------------
disc_hdi <- discretizeDF.supervised(
  bef_message_bool ~ new_idhm,
  data = brazil_df[,c("bef_message_bool", "new_idhm")])
table(disc_hdi)

# HDI discretization manually according to website
brazil_df <- brazil_df %>%
  mutate(hdi_class = ifelse(new_idhm < 0.551, "low", ""),
         hdi_class = ifelse(new_idhm > 0.550 & new_idhm < 0.700, "medium", hdi_class),
         hdi_class = ifelse(new_idhm > 0.699 & new_idhm < 0.800, "high", hdi_class),
         hdi_class = ifelse(new_idhm > 0.799, "very high",  hdi_class),
         # Get the factor levels into the right order
         hdi_class = factor(hdi_class, levels = c("low", "medium", "high", "very high"))
        )
levels(brazil_df$hdi_class)
# Collapse because there 
brazil_df <- brazil_df %>%
  mutate(hdi_class_col = ifelse(new_idhm < 0.700, "low_medium", ""),
         hdi_class_col = ifelse(new_idhm > 0.699 & new_idhm < 0.800, "high", hdi_class_col),
         hdi_class_col = ifelse(new_idhm > 0.799, "very high",  hdi_class_col),
         hdi_class_col = as.factor(hdi_class_col),
         hdi_class_col = factor(hdi_class_col, levels = c("low_medium", "high", "very high"))
        )
levels(brazil_df$hdi_class_col)


# check: did it work? 
testje <- brazil_df %>%
  select(new_idhm, hdi_class)

# Max price
# ---------
disc_max_price <- discretizeDF.supervised(
  bef_message_bool ~ max_price,
  data = brazil_df[,c("bef_message_bool", "max_price")])
# Three categories it is, I guess.
table(disc_max_price)
# Apply discretizations
brazil_df <- brazil_df %>%
  mutate(max_price_disc = ifelse(max_price < 50, "low", ""),
         max_price_disc = ifelse(max_price > 49 & max_price < 192, "medium", max_price_disc),
         max_price_disc = ifelse(max_price > 191, "high", max_price_disc)
        )
# check: did it work?
testje <- brazil_df %>%
  select(max_price, max_price_disc)


# item count
# ----------
disc_item_count <- discretizeDF.supervised(
  bef_message_bool ~ item_count,
  data = brazil_df[,c("bef_message_bool", "item_count")])
# This doesn't really make sense, especially because item is already discrete
table(disc_item_count)
# Apply discretizations
brazil_df <- brazil_df %>%
  mutate(item_count_disc = ifelse(item_count == 1, "single", ""),
         item_count_disc = ifelse(item_count > 1 & item_count < 6, "multiple", item_count_disc),
         item_count_disc = ifelse(item_count > 5, "large", item_count_disc)
        )

# message length
# --------------

# including zeros
hist(brazil_df$bef_nchar)
# excluding zeros
hist(brazil_df[brazil_df$bef_nchar > 0,]$bef_nchar)

disc_item_count <- discretizeDF.supervised(
  bef_message_bool ~ item_count,
  data = brazil_df[,c("bef_message_bool", "item_count")])


# ----------------------------- #
# 9. To the right type (factor) # ---------------------------------------------
# ----------------------------- #
levels(brazil_df$year)

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


cols_2 <- c("diff_est_deliv",
            "diff_pur_est",
            "diff_pur_deliv",
            "diff_rev_crea_ans",
            "diff_rev_est_ans",
            "diff_rev_deliv_ans")

brazil_df[,cols_2] <- lapply(brazil_df[cols_2], function(x) as.integer(x))



# -------------- #
# 10. Write file # -------------------------------------------------------------
# -------------- #

write.csv(brazil_df, "full_geomerged_df_4.csv")


