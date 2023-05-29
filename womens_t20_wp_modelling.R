## Create XG Boost Model for Women's T20

library(cricketdata)
library(tidyverse)
library(tidymodels)
library(workflows)
library(rsample)
library(xgboost)
library(parsnip)
library(doParallel)
library(dials)
library(tune)
library(vip)
library(pROC)


wt20 <- data.frame()
for(comp in c("wbb", "wsl", "wtc", "wpl", "ssm")){
  data <- fetch_cricsheet("bbb", "female", comp)
  df <- data.frame(data) %>%
    mutate(competition_code = comp,
           competition = case_when(competition_code == "wbb" ~ "Women's Big Bash League",
                                   competition_code == "wsl" ~ "Women's Super League",
                                   competition_code == "wtc" ~ "Women's T20 Challenge",
                                   competition_code == "wcl" ~ "Women's Caribbean Premier League",
                                   competition_code == "wpl" ~ "Women's Premier League",
                                   competition_code == "ssm" ~ "Super Smash League",
                                   competition_code == "blz" ~ "T20 Blaze"),
           season = as.character(season),
           match_id = as.character(match_id))
  wt20 <- bind_rows(wt20, df)
}

wt20_cleaning <- wt20 %>%
  mutate(runs_gained = runs_off_bat+extras,
         win = case_when(innings == 1 & innings1_total>innings2_total ~ 1,
                         innings == 1 & innings1_total<innings2_total ~ 0,
                         innings == 2 & innings2_total>innings1_total ~ 1,
                         innings == 2 & innings2_total<innings1_total ~ 0,
                         innings1_total==innings2_total ~ 0),
         win = as.factor(win)) %>%
  drop_na(innings1_total, innings2_total) 

wt20_eda <- wt20_cleaning %>%
  group_by(match_id, competition, season) %>%
  summarise(max_innings1 = max(innings1_total, na.rm= TRUE),
            max_innings2 = max(innings2_total, na.rm = TRUE)) %>%
  ungroup()

wt20_eda %>%
  ggplot() +
  geom_boxplot(aes(competition, max_innings1)) +
  coord_flip()

wt20_eda %>%
  ggplot() +
  geom_boxplot(aes(competition, max_innings2)) +
  coord_flip()


#Removed Caribbean Premier League and T20 blaze as were outliers in terms of average innings scores


  


# Innings 1 Modelling -----------------------------------------------------

wt20_cleaning1 <- wt20_cleaning %>%
  filter(innings == 1) %>%
  dplyr::select(win, balls_remaining, runs_scored_yet, wickets_lost_yet)

set.seed(123)
innings1_split <- initial_split(wt20_cleaning1, strata = win)
innings1_train <- training(innings1_split)
innings1_train_df <- innings1_train %>% as.data.frame()
innings1_test <- testing(innings1_split)
train_x = data.matrix(innings1_train_df[,2:4])
train_y = innings1_train_df[,1] %>% as.matrix()
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)

xgb_spec <- boost_tree(
  trees = 100,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()) %>%                          ## step size
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_spec

xgb_grid <- dials::grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), innings1_train),
  learn_rate(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(win ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
innings1_folds <- vfold_cv(innings1_train, v = 5)
innings1_folds

doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = innings1_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res

collect_metrics(xgb_res)
show_best(xgb_res, "roc_auc")
## don't want rmse for a classification model
best_accuracy <- select_best(xgb_res, "roc_auc")
best_accuracy

final_xgb_innings1_workflow <- finalize_workflow(
  xgb_wf,
  best_accuracy
)

saveRDS(final_xgb_innings1_workflow, "wbb/final_xgb_innings1_workflow.rds")

final_xgb_innings1_workflow <- readRDS("wbb/final_xgb_innings1_workflow.rds")

final_xgb_innings1_workflow

final_xgb_innings1_workflow %>%
  fit(data = innings1_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb_innings1_workflow, innings1_split)

collect_metrics(final_res)

collect_predictions(final_res) %>%
  ggplot(aes(x = .pred_1, y = win)) +
  geom_point() +
  geom_smooth() +
  labs(title = "QB xgBoosted Model")

#eta = learn_rate,
#gamma = loss_reduction,
#subsample = sample_size,
#colsample_bytree = mtry/number of train columns (include ppr)
#max_depth = tree_depth,
#min_child_weight = min_n
#nrounds = trees

df_hs_predict_xgb <- xgb.DMatrix(data = df_hs_predict)

inn

innings1_train_matrix <- innings1_train %>% as.matrix()


final_xgb_innings1_xg <-  xgboost(data = xgb_train, max.depth = 6, nrounds = 100, verbose = 0, min_child_weight = 2,
                            gamma = 0.00000000339, subsample = 0.604496, colsample_bytree = 0.5,
                            objective ="binary:logistic")

xgb.save(final_xgb_innings1_xg, "final_xgb_innings1_xg")

final_xgb_innings1_workflow


wt20_testing1 <- wt20_cleaning %>%
  filter(innings == 1) %>% 
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet) %>% as.matrix()

wt20_testing1_predict <- predict(final_xgb_innings1_xg, wt20_testing1) %>% as.data.frame()

wt20_testing_df_1 <- wt20_cleaning %>%
  filter(innings == 1) 

wt20_testing_df_1$wp <- wt20_testing1_predict$.


## Logistic Regression

wp_model1_log <- glm(win ~ .,data=wt20_cleaning1,family=binomial(link="logit"))
summary(wp_model1_log)
OR <- coef(wp_model1_log) %>% exp()
CI <- confint(wp_model1_log) %>% exp()
cbind(OR,CI)

predicted <- predict(wp_model1_log, wt20_cleaning1, type="response")
auc(wt20_cleaning1$win, predicted)

saveRDS(wp_model1_log, file = "wbb/wp_model1_log.rds")


# Innings 2 Modelling -----------------------------------------------------

wt20_cleaning2 <- wt20_cleaning %>%
  filter(innings == 2) %>%
  mutate(run_chase = target-runs_scored_yet) %>%
  dplyr::select(win, balls_remaining, runs_scored_yet, wickets_lost_yet, run_chase)

set.seed(123)
innings2_split <- initial_split(wt20_cleaning2, strata = win)
innings2_train <- training(innings2_split)
innings2_train_df <- innings2_train %>% as.data.frame()
innings2_test <- testing(innings2_split)
train_x = data.matrix(innings2_train_df[,2:5])
train_y = innings2_train_df[,1] %>% as.matrix()
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)

xgb_spec <- boost_tree(
  trees = 100,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()) %>%                          ## step size
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_spec

xgb_grid <- dials::grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), innings1_train),
  learn_rate(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(win ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
innings2_folds <- vfold_cv(innings2_train, v = 5)
innings2_folds

doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = innings2_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res

collect_metrics(xgb_res)
show_best(xgb_res, "roc_auc")
best_accuracy <- select_best(xgb_res, "roc_auc")
best_accuracy

final_xgb_innings2_workflow <- finalize_workflow(
  xgb_wf,
  best_accuracy
)

saveRDS(final_xgb_innings2_workflow, "wbb/final_xgb_innings2_workflow.rds")

final_xgb_innings2_workflow <- readRDS("wbb/final_xgb_innings2_workflow.rds")

final_xgb_innings2_workflow

final_xgb_innings2_workflow %>%
  fit(data = innings2_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb_innings2_workflow, innings2_split)

collect_metrics(final_res)

collect_predictions(final_res) %>%
  ggplot(aes(x = .pred_1, y = win)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Women's T20 Innings 2 XGBoosted Model")

#eta = learn_rate,
#gamma = loss_reduction,
#subsample = sample_size,
#colsample_bytree = mtry/number of train columns (include ppr)
#max_depth = tree_depth,
#min_child_weight = min_n
#nrounds = trees


final_xgb_innings2_xg <-  xgboost(data = xgb_train, max.depth = 11, nrounds = 100, verbose = 0, min_child_weight = 5,
                                  gamma = 0.000000543, subsample = 0.940, colsample_bytree = 0.2,
                                  objective ="binary:logistic")

xgb.save(final_xgb_innings2_xg, "final_xgb_innings2_xg")

final_xgb_innings2_workflow


wt20_testing2 <- wt20_cleaning %>%
  filter(innings == 2) %>%
  mutate(run_chase = target-runs_scored_yet) %>%
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet, run_chase) %>%
  as.matrix()


wt20_testing2_predict <- predict(final_xgb_innings2_xg, wt20_testing2) %>% as.data.frame()

wt20_testing_df_2 <- wt20_cleaning %>%
  filter(innings == 2) 

wt20_testing_df_2$wp <- wt20_testing2_predict$.

##Logistic Regression

wp_model2_log <- glm(win ~ .,data=wt20_cleaning2,family=binomial(link="logit"))
summary(wp_model2_log)
OR <- coef(wp_model2_log) %>% exp()
CI <- confint(wp_model2_log) %>% exp()
cbind(OR,CI)

predicted <- predict(wp_model2_log, wt20_cleaning2, type="response")
auc(wt20_cleaning2$win, predicted)

saveRDS(wp_model2_log, file = "wbb/wp_model2_log.rds")
