## College Football Bowl Pick 'em' Analysis
## Chris Peters
## Season 2011

# install.packages("reshape")
# install.packages("chron")
# install.packages("rpart")
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("stats")
# install.packages("svmpath")
# install.packages("e1071")
# install.packages("nnet")
# install.packages("gmm")
# install.packages("nlme")

library(reshape)
library(chron)
library(rpart)
library(ggplot2)
library(randomForest)
library(stats)
library(svmpath)
library(e1071)
library(nnet)
library(gmm)
library(nlme)


#==========================
# Bring in Regular Season Data
#=========================
# Import regular season stats
rs <- read.csv("C:/R_stuff/gentrys_beetus_folder/college_bowl_pickem/College-Bowl-Pick--Em-2011/college_football_2011_per_game_basis.csv", as.is = TRUE)

# Import strength of schedule, comes from (before bowls): http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team
sos <- read.csv("C:/R_stuff/gentrys_beetus_folder/college_bowl_pickem/College-Bowl-Pick--Em-2011/sos.csv")
sos$unique_id <- paste(sos$year, sos$team, sep = "_")

rs <- merge(rs, sos, by ="unique_id")

# Import bowl outcomes
bo <- read.csv("C:/R_stuff/gentrys_beetus_folder/college_bowl_pickem/College-Bowl-Pick--Em-2011/college_bowl_game_results_for_import_rand2.csv", as.is = TRUE)

# Bowl games have specific dates, need to associate with certain years since some
# bowl games happen in January of the next year.
bo$True_Year <- month.day.year(unclass(bo$Date))$year
bo$True_Month <- month.day.year(unclass(bo$Date))$month

bo$Year <- ifelse(bo$True_Month == 12, 
                    bo$True_Year,
                    bo$True_Year - 1)

bo$zscore <- bo$Score_W - bo$Score_L

bo$Winner[bo$Winner == "Brigham Young"] <- "BYU"
bo$Winner[bo$Winner == "Florida International"] <- "Florida Intl."
bo$Winner[bo$Winner == "North Carolina St."] <- "N.C. State"
bo$Winner[bo$Winner == "Texas Christian"] <- "TCU"
bo$Winner[bo$Winner == "Miami"] <- "Miami (FL)"
bo$Winner[bo$Winner == "Louisiana St."] <- "LSU"
bo$Winner[bo$Winner == "Middle Tennessee St."] <- "Middle Tenn St."
bo$Winner[bo$Winner == "Southern Methodist"] <- "SMU"
bo$Winner[bo$Winner == "Southern California"] <- "USC"
bo$Winner[bo$Winner == "Central Michigan"] <- "CMU"
bo$Winner[bo$Winner == "Southern Mississippi"] <- "Southern Miss"
bo$Winner[bo$Winner == "Texas Christian"] <- "TCU"
bo$Winner[bo$Winner == "Boston College"] <- "Boston Coll."

bo_indicator <- as.matrix(bo[ , c("Year", "Winner", "Loser", "zscore")])
colnames(bo_indicator) <- c("year", "winner", "loser", "zscore")
bo_indicator <- as.data.frame(bo_indicator)

bo_indicator$win_id <- paste(bo_indicator$year, bo_indicator$winner, sep = "_")
bo_indicator$lose_id <- paste(bo_indicator$year, bo_indicator$loser, sep = "_")

rs$win_id <- paste(rs$year.x, rs$team.x, sep ="_")
rs$lose_id <- paste(rs$year.x, rs$team.x, sep ="_")

bo_merge <- merge(bo_indicator, rs, by = "win_id")
bo_merge <- merge(bo_merge, rs, by = "lose_id")

all_data <- bo_merge
                  
all_data_for_reg <- as.data.frame(all_data$year)
colnames(all_data_for_reg) <- "year"

all_data_for_reg$winner <- all_data$winner
all_data_for_reg$loser <- all_data$loser
all_data_for_reg$zscore <- all_data$zscore
all_data_for_reg <- as.data.frame(cbind(all_data_for_reg, 
                          all_data[ , 10:38],
                          all_data[ , 47:73]))
all_data_for_reg$sos.x <- all_data$sos.x
all_data_for_reg$sos.y <- all_data$sos.y
all_data_for_reg$year <- droplevels(all_data_for_reg$year)

## Creating z vars

zdata <- all_data_for_reg[ c(5:31, 61)] - all_data_for_reg[ , c(34:60, 62)]
names(zdata) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8",
                  "v9", "v10", "v11", "v12", "v13", "v14", "v15",
                  "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                  "v23", "v24", "v25", "v26", "v27", "v28")

all_data_for_reg <- cbind(all_data_for_reg[ , 1:4], zdata)
all_data_for_reg$zscore <- as.numeric(levels(all_data_for_reg$zscore)[all_data_for_reg$zscore])

## Renameing column names
colnames(all_data_for_reg) <- rbind(as.matrix(colnames(all_data_for_reg[1:4])), 
      as.matrix(colnames(rs[4:30])), 
      as.matrix(colnames(rs[34])))

training <- all_data_for_reg[all_data_for_reg$year != 2009 &
                             all_data_for_reg$year != 2010 &
                             all_data_for_reg$year != 2011, ]
training <- training[ , -c(1:3)]

test1 <- all_data_for_reg[all_data_for_reg$year == 2009, ]
test2 <- all_data_for_reg[all_data_for_reg$year == 2010, ]

test1$actual <- test1$zscore
test2$actual <- test2$zscore

test1$zscore <- NA
test2$zscore <- NA

preds <- all_data_for_reg[all_data_for_reg$year == 2011, ]

################ Models #####################


############# Cross-validation ##############
test1_pred_set <- as.data.frame(glm_2_test1)
test2_pred_set <- as.data.frame(glm_2_test2)

names(test1_pred_set) <- c("z_pred")
names(test2_pred_set) <- c("z_pred")

test1_val <- as.data.frame(cbind(as.numeric(test1_pred_set$z_pred),
                                 as.numeric(test1$actual)))
names(test1_val) <- c("prediction", "actual")
test1_val <- droplevels(test1_val)
test1_val <- test1_val[order(-abs(test1_val$prediction)), ]
test1_val$correct <- ifelse((test1_val$prediction > 0 & test1_val$actual > 0) |
                            (test1_val$prediction < 0 & test1_val$actual < 0), 1, 0)
test1_val$group <- seq(nrow(test1_val), 1, by = -1)
test1_val$tot <- test1_val$correct * test1_val$group

                           
test2_val <- as.data.frame(cbind(as.numeric(test2_pred_set$z_pred),
                                 as.numeric(test2$actual)))
names(test2_val) <- c("prediction", "actual")
test2_val <- droplevels(test2_val)
test2_val <- test2_val[order(-abs(test2_val$prediction)), ]
test2_val$correct <- ifelse((test2_val$prediction > 0 & test2_val$actual > 0) |
                            (test2_val$prediction < 0 & test2_val$actual < 0), 1, 0)
test2_val$group <- seq(nrow(test2_val), 1, by = -1)
test2_val$tot <- test2_val$correct * test2_val$group

sum(test1_val$tot)
sum(test2_val$tot)
sum(test1_val$tot, test2_val$tot) / sum(test1_val$group, test2_val$group)
                                        
### Final Predictions ###
preds$zscore <- NA
glm_2_final_predictions <- as.matrix(predict(glm_2, preds))
final_predictions <- cbind(glm_2_final_predictions, preds)
final_predictions_ordered <- final_predictions[order(-abs(glm_2_final_predictions)), ]
#Note: I can't root against LSU in the national
#      championship, so since my model picks them
#      to lose, I'll rank them last, and winning.

write.csv(final_predictions_ordered, "C:/Users/Chris/Desktop/RCode/College Bowl Pick Em/Output/final_picks_2011.csv")