df <- readRDS("zhu_faded.rds")
library(sampleSelection)

df$adjusted_mc_new_idhm <- df$mc_new_idhm*100
df$adjusted_mc_new_urbanity <- df$mc_new_urbanity*100



glm_probit <- glmer( 
        formula = bef_message_bool 
        ~ 1
        + adjusted_mc_new_idhm
        + adjusted_mc_new_urbanity
        + north
        + south
        + southeast
        + northeast
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
        + north*above_median
        + (1|customer_city),
        family = binomial(link = "probit"),
        nAGQ = 0,
        data = df,
        control = glmerControl(
                optimizer = "bobyqa", 
                optCtrl = list(maxfun=2e5)
        )
)


summary(glm_probit)

prbit <- glm(bef_message_bool
             ~ adjusted_mc_new_idhm
             + adjusted_mc_new_urbanity
             + north
             + south
             + southeast
             + northeast
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
             + north*above_median
             , data = df,
             family = binomial(link = "probit"))
summary(prbit)


probit_lp = predict(glm_probit)
#probabilities <- model %>% predict(train, type = "response",allow.new.levels = TRUE)

mills0 = dnorm(probit_lp)/pnorm(probit_lp) # this works correctly

# Somewhat multimodal; is this a concern? 
hist(mills0)


df$mills <- mills0

# Truncated fsys 
truncated_df <- df[df$bef_message_bool == 1,]




multilev_outcome <- lmer(
                         formula = log_nwords  
                         ~ 1
                         + adjusted_mc_new_idhm
                         + adjusted_mc_new_urbanity
                         + north
                         + south
                         + southeast
                         + northeast
                         + item_count
                         + early
                         + late 
                         + intimate_goods
                         + experience_goods
                         + product_photos_qty
                         + review_sent_moy
                         + mills
                         + year
                         + negative
                         + positive
                         + north*above_median
                         + (1|customer_city),
                         data = truncated_df)


summary(multilev_outcome)

library(boot)
library(sjstats)

bootje <- bootMer(lmer(
        formula = log_nwords  
        ~ 1
        + adjusted_mc_new_idhm
        + adjusted_mc_new_urbanity
        + north
        + south
        + southeast
        + northeast
        + item_count
        + early
        + late 
        + intimate_goods
        + experience_goods
        + product_photos_qty
        + review_sent_moy
        + mills
        + year
        + negative
        + positive
        + north*above_median
        + (1|customer_city),
        data = truncated_df),
        fixef,
        nsim = 100)

bootje

# fixef
# p-value


# PLOTJES 
library(dotwhisker)


full_selection <- read.csv("forwd_full_SELECTION.csv")
metros_selection <- read.csv("forwk_METROS_SELECTION.csv")
nonmetros_selections <- read.csv("forwk_SELECTION_NONMETROS.csv")
non_freight_selections <- read.csv("forwkd_SELECTION_NOFREIGHTISS.csv")
below_four_selection <- read.csv("forwkd_SELECTION_BELOWTOPTWO.csv")
two_step_selection <- read.csv("forwkd_SELECTION_NONMLE_MULTILEVEL.csv")


two_models <- rbind(full_selection, metros_selection)
three_models <- rbind(two_models, nonmetros_selections)
four_models <- rbind(three_models, non_freight_selections)
five_models <- rbind(four_models, below_four_selection)
six_models <- rbind(five_models, two_step_selection)



six_models <- six_models %>%  relabel_predictors(c(mc_new_idhm = "Human Development Index",
                                                   mc_new_urbanity = "Urbantity Ratio",
                                                   `1.north` = "North",
                                                   `1.northeast` = "Northeast",
                                                   `1.southeast` = "Southeast",
                                                   `1.south` = "South",
                                                   `1.above_median*1.north` = "North*Above median"))

windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
dwplot(six_models %>% filter(term != 'mc_new_idhm' & term != "mc_new_urbanity"),
       whisker_args = aes(size = 1.3),
       dot_args = aes(size = 3)) + 
        theme_bw()  +
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
        labs(title = expression(bold("Figure 4")),
             subtitle = expression(italic("Estimates of Key variables Across Various Models and Data, with 95% Confidence Intervals"))) +
        theme(text = element_text(family = "Times New Roman", size = 15),
              plot.title = element_text(size = 15),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(hjust = 0))

dwplot(six_models %>% filter(term == 'mc_new_idhm' | term == "mc_new_urbanity"),
       whisker_args = aes(size = 1.3),
       dot_args = aes(size = 3)) + 
        theme_bw()  +
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
        theme(text = element_text(family = "Times New Roman", size = 15),
              plot.title = element_text(size = 15),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(hjust = 0))


note = expression(paste(italic("Note. "), "For a close-up of continuous variables HDI and Urbanity, see Appendix XYX"))
dwplot(six_models,
       whisker_args = aes(size = 1.3),
       dot_args = aes(size = 3)) + 
        theme_bw()  +
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
        labs(title = expression(bold("Figure 4")),
             subtitle = expression(italic("Estimates of Key Parameters Across Various Models and Data, with 95% Confidence Intervals"))) +
        theme(text = element_text(family = "Times New Roman", size = 15),
              plot.title = element_text(size = 15),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(hjust = 0)) +
        labs(caption = note)


library(lmtest)
lrtest(prbit, glm_probit)

icc(glm_probit)


# ---- Full Brazil ------------------------------------------------------------ 
full_brazil_mle <- selection(select = bef_message_bool 
                    ~ adjusted_mc_new_idhm
                    + adjusted_mc_new_urbanity
                    + north
                    + south
                    + southeast
                    + northeast
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
                    + north*above_median
                    ,
                    
                    outcome = log(bef_nwords)
                    ~ mc_new_idhm
                    + mc_new_urbanity
                    + north
                    + south
                    + southeast
                    + northeast
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
                    + north*above_median
                    ,
                    data = df)
summary(full_brazil_mle)
AIC(full_brazil_mle)

df$realistic_idhm <- df$mc_new_idhm*100
df$realistic_urbabity <- df$mc_new_urbanity*100
df$mc_realistic_idhm <- df$new_
hhhi <- glm(bef_message_bool 
            ~ adjusted_mc_new_idhm
            + adjusted_mc_new_urbanity
            + north
            + south
            + southeast
            + northeast
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
            + north*above_median,
       data = df,
       family = binomial(link = "probit"))
AIC(hhhi)
summary(hhhi)
vif(hhhi)
length(coef(hhhi))
library(car)



ggplot(df, aes(x= new_idhm)) + 
        geom_histogram(color="black", fill="coral", size = 0.1, bins = 40) + 
        facet_wrap(~udh_indicator) +
        xlim(0, 1.00) + 
        labs(title = expression(bold("Figure C1")),
             subtitle = expression(italic("Difference in frequencies of Human Development Indices between UDH (1) and municipal data (0) cases"))) +
        xlab("Human Development Index associated with the location of the related to case") + ylab("Count") +
        theme(text = element_text(family = "Times New Roman", size = 14),
              plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 14))

ggplot(df[df$bef_message_bool ==1 ,], aes(x= bef_nwords)) + 
        geom_histogram(color="black", fill="coral", size = 0.1) + 
        labs(title = expression(bold("Figure J1")),
             subtitle = expression(italic("Histogram of Word Length, Excluding Zero-Length Reviews"))) +
        xlab("Words used in a Message") + ylab("Count") +
        theme(text = element_text(family = "Times New Roman", size = 14),
              plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 14))

ggplot(df[df$bef_message_bool ==1 ,], aes(x= log_nwords)) + 
        geom_histogram(color="black", fill="coral", size = 0.1) + 
        labs(title = expression(bold("Figure J2")),
             subtitle = expression(italic("Histogram of Log Transformed Word Length, Excluding Zero-Length Reviews"))) +
        xlab("Percentage of Word Length") + ylab("Count") +
        theme(text = element_text(family = "Times New Roman", size = 14),
              plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 14))



# probbie 


karl <- glm(order)





