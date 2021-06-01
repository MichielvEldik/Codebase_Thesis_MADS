# USED files:
#            Olist kaggle data
#            dictionary_optimal_orders.csv
#            dictionary_optimal_productids.csv
# 
# WRITE files:
#            dictionary_optimal_orders.csv
#            dictionary_optimal_productids.csv
#            full_geomerged_df.csv

library(dplyr)



customers_df <- read.csv("./olist_data/olist_customers_dataset.csv")
reviews_df <- read.csv("./olist_data/olist_order_reviews_dataset.csv")
items_df <- read.csv("./olist_data/olist_order_items_dataset.csv")
products_df <- read.csv("./olist_data/olist_products_dataset.csv")
orders_df <- read.csv("./olist_data/olist_orders_dataset.csv")
sellers_df <- read.csv("./olist_data/olist_sellers_dataset.csv")
geo_df <- read.csv("./olist_data/olist_geolocation_dataset.csv")
translate_df <- read.csv("./olist_data/product_category_name_translation.csv")
payment_df <- read.csv("./olist_data/olist_order_payments_dataset.csv")

translated_products_df <- merge(products_df, 
                                translate_df, 
                                by.x = "product_category_name", 
                                by.y = "product_category_name", 
                                all.x = TRUE)

translated_products_df$product_category_name <- translated_products_df$product_category_name_english

translated_products_df <- translated_products_df[,1:9]


# ---------------------- #
# 1. Review-order merge  # ----------------------------------------------------
# ---------------------- #


# A unique review_id can be related to multiple unique order_id
# Task: find most expensive order_id to review_id
# --------------------------------------------------------------

reviews_plus_orders_df <- merge(reviews_df, 
                                orders_df, 
                                by.x = "order_id", 
                                by.y = "order_id")

revord_plus_items_df <- merge(reviews_plus_orders_df,
                              items_df,
                              by.x = "order_id", 
                              by.y = "order_id",
                              all.x = TRUE)

semi_df <- merge(revord_plus_items_df, 
                 translated_products_df, 
                 by.x = "product_id", 
                 by.y = "product_id", 
                 all.x = TRUE)

multiples_cases_rev_df <- reviews_df %>%
  group_by(review_id) %>% 
  filter(n()>1)

# 802 unique review_ids that a decision needs to be made about
culprit_vector <- unique(multiples_cases_rev_df$review_id)


counter <- 0
# To fill with the order_ids corresponding to highest price per "culprit"
table = data.frame()

# Loop
# ----
# 1. Goes through all review_ids in the semi-merged data set.
# 2. It identifies if a review_id is among the 'culprits' (has multiple order_ids related to it).
# 3. If it encounters a 'culprit', it looks for the `order_id` related to the highest priced product.
# 4. It keeps track of these cases by adding them to a data table.
# ----
for (i in semi_df$review_id) {
  if (i %in% culprit_vector) {
    interim_df <- semi_df[semi_df$review_id == i,]
    table <- rbind(table, interim_df[,2:3][which.max(interim_df$price),])
    print(counter)
    counter = counter + 1
  }
}
# Keep unique rows only 
optimal_orders <- table[!duplicated(table), ]

# Cache
write.csv(optimal_orders,"dictionary_optimal_orders.csv", row.names = FALSE)

# Join
merger_reviews_df <- merge(reviews_df,
                           optimal_orders,
                           by.x = "review_id",
                           by.y = "review_id",
                           all.x = TRUE)

# separate the "culprits" from the non-"culprits"
subbie_2 <- merger_reviews_df[!is.na(merger_reviews_df$order_id.y),] 
subbie <- merger_reviews_df[is.na(merger_reviews_df$order_id.y),]

# get columns set up in the right way
subbie$order_id.y <- subbie$order_id.x

# bind them back together
happy_days <- rbind(subbie_2, subbie)

# get rid of order.id.x, not necessary anymore
happy_days <- happy_days %>%
  select(-order_id.x)

# get rid of duplicate rows
happy_days <- unique(happy_days)

# Does it make sense?
length(unique(happy_days$review_id))
nrow(happy_days)

# full merge
reviews_orders <- merge(happy_days, 
                        orders_df, 
                        by.x = "order_id.y", 
                        by.y = "order_id",
                        all.x = TRUE)

# Does it still make sense?
nrow(reviews_orders) # 99179
length(unique(reviews_orders$review_id)) #99173


# -------------------------- #
# 2. Items & customer merge  # ------------------------------------------------
# -------------------------- #
# Challenge: Aggregate to review level requires choice of which product_id goes 
# in multi-item orders

# Join orders with items 
roi_df <- merge(reviews_orders,
                items_df,
                by.x = "order_id.y", 
                by.y = "order_id",
                all.x = TRUE)

# Get aggregate data 
roi_df_2 <- roi_df %>%
  group_by(review_id) %>%
  mutate(total_price_amount = sum(price)) %>%
  mutate(item_count = max(order_item_id)) %>%
  mutate(average_price = mean(price)) %>%
  mutate(sd_price = sd(price)) %>%
  mutate(max_price = max(price)) %>% 
  mutate(min_price = min(price)) %>% 
  mutate(total_freight_amount = sum(freight_value)) %>%
  mutate(average_freight_amount = mean(freight_value)) %>%
  mutate(sd_freight_amount = sd(freight_value)) %>%
  mutate(max_freight_amount = max(freight_value)) %>%
  mutate(min_freight_amount = min(freight_value)) %>%
  ungroup()

roi_df_2 <- as.data.frame(roi_df_2)

# Get multi-item cases 
multicases_df <- roi_df_2 %>%
  group_by(review_id) %>% 
  filter(n()>1)

multicases_df <- multicases_df[,c(2, 16, 19)]

multicases_df <- as.data.frame(multicases_df)

# Avoid duplicates for speed
unique_multicases_df <- unique(multicases_df)

# save as vector to loop over
disco <- unique(multicases_df$review_id) #9770

# Initiate new df to hold new data
table_2 <- data.frame()

# Loop
# ----
# 1. Go through all unique `review_id`s that have a duplicate in the data
# 2. For each `review_id`, return the highest price in the list
# 3. Return `product_id` and `price`. 
# 4. Replace all `product_id`s in the multi-item order with this newly found `product_id`
# ----

for (d in disco) {
  interim_df_2 <- multicases_df[multicases_df$review_id == d,]
  table_2 <- rbind(table_2, interim_df_2[which.max(interim_df_2$price),])
}

# Cache
write.csv(table_2,"dictionary_optimal_productids.csv", row.names = FALSE)

# Join optimals
merger <- merge(roi_df_2,
                table_2,
                by.x = "review_id",
                by.y = "review_id",
                all.x = TRUE)

nick <- merger[is.na(merger$product_id.y),] 
simon <-merger[! is.na(merger$product_id.y),]

# This makes 1 column with all product_ids, including replacements we've found
nick$product_id.y <- nick$product_id.x
nick_simon <- rbind(nick, simon)

# We need to get rid of data on orderline scale because we want to get it to a
# higher level of aggregation with the `unique()` method.
nick_simon <- nick_simon %>% select(
  -price.x,
  -price.y,
  -product_id.x,
  -freight_value,
  -order_item_id
)
# Using `unique()` method, we remove all duplicates and end up with all unique 
# `review_id`s in the dataset.
bicep <- unique(nick_simon)

nrow(bicep) -length(unique(bicep$review_id)) # should be zero, is 1349 :(

# To resolve, we need customer data, hence merge.
bicep_df <- merge(bicep,
                  customers_df,
                  by.x = "customer_id", 
                  by.y = "customer_id", 
                  all.x = TRUE)

# Remove columns that cause variation problems in unique() func.
# Should have been removed initially
bicep_df <- bicep_df %>%
  select(
    - order_id.y,
    - customer_id,
    - seller_id,
    - shipping_limit_date 
  )

bicep_df <- unique(bicep_df)

# check again.
nrow(bicep_df) -length(unique(bicep_df$review_id)) # only 6, yay!

# Who are the duplicates?
bicep_df_dups <- bicep_df %>%
  group_by(review_id) %>% 
  filter(n()>1)

# ------------------------ #
# 3. merge with products   # --------------------------------------------------
# ------------------------ #

bicep_df <- merge(bicep_df, 
                  translated_products_df,
                  by.x = "product_id.y", 
                  by.y = "product_id", 
                  all.x = TRUE) 


# ------------------------ #
# 4. Geo Merge             # --------------------------------------------------
# ------------------------ #

# Sanity check
length(unique(bicep_df$review_id))
nrow(bicep_df)

findCentroidandMerge <- function(name_of_state){
  # Function works per state, as prefixes can be duplicate across states
  x_STATE_rev_df <- bicep_df[bicep_df$customer_state == name_of_state,]
  x_STATE_geo_df <- geo_df[geo_df$geolocation_state == name_of_state,]
  # get centroid of zip code prefix coordinate
  centroids_x_STATE_geo_df <- x_STATE_geo_df %>%
    group_by(geolocation_zip_code_prefix) %>%
    summarise(
      centroid_lat = mean(geolocation_lat),
      centroid_long = mean(geolocation_lng)
    ) %>%
    ungroup()
  # merge centroid coordinate with zip code prefix
  geo_rev_STATE_df <- merge(x_STATE_rev_df,
                            centroids_x_STATE_geo_df,
                            by.x = "customer_zip_code_prefix",
                            by.y = "geolocation_zip_code_prefix",
                            all.x = TRUE)
  return(geo_rev_STATE_df)
}

# State names needed to loop over all states in function
state_names <- c(levels(bicep_df$customer_state))
# list object necessary to capture multiple dataframes
empty_list <- vector(mode = "list", length = length(state_names))

# Call centroid_merge func. for all states and store in list
for (i in 1:length(state_names)){
  empty_list[[i]] <- findCentroidandMerge(state_names[i])
}

# Turn list into dataframe
base_df <- empty_list[[1]]
for (i in 2:27){
  base_df <- rbind(base_df, empty_list[[i]])
}

# Sanity check
length(unique(base_df$review_id))
nrow(base_df)

# Cache full dataset for next script
write.csv(base_df, "full_geomerged_df.csv", row.names = FALSE)

