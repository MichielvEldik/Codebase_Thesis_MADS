df <- readRDS("zhu_faded.rds")
library(sampleSelection)

# ---- Full Brazil ------------------------------------------------------------ 
full_brazil_mle <- selection(select = bef_message_bool 
                    ~ mc_new_idhm
                    + mc_new_urbanity
                    + region
                    + item_count
                    + early
                    + late
                    + intimate_goods
                    + experience_goods
                    + product_photos_qty
                    + review_sent_moy
                    + review_sent_wknd
                    + year
                    + negative
                    + positive
                    + above_median*region
                    ,
                    
                    outcome = log(bef_nwords)
                    ~ mc_new_idhm
                    + mc_new_urbanity
                    + region
                    + item_count
                    + early
                    + late
                    + intimate_goods
                    + experience_goods
                    + product_photos_qty
                    + review_sent_moy
                    + year
                    + negative
                    + positive
                    + above_median*region
                    ,
                    data = brazil_df)
summary(heckie)
