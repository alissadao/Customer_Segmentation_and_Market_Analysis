# load package----
library(dplyr)
library(ggplot2)
library(cluster)
library(purrr)
library(dendextend)

# load data----
protein <- read.csv("C:\\Users\\Admin\\OneDrive - Dickinson College\\Desktop\\DATA PROJECT\\Healthy Food Project\\protein data.csv")
names(protein)


# data wrangling ----
# separate data by each product 
labgrown <- protein[,c(15,20)]
edible <- protein[,c(14,21)]
plant <- protein[,c(16,22)]

# check if there are any missing values 
labgrown <- na.omit(labgrown)
edible <- na.omit(edible)
plant <- na.omit(plant)

# check the data type
for (i in 1:nrow(labgrown)) {
  row <- labgrown[i, ]
  for (element in row) {
    cat("Type:", typeof(element), "\n")
  }
}

for (i in 1:nrow(plant)) {
  row <- labgrown[i, ]
  for (element in row) {
    cat("Type:", typeof(element), "\n")
  }
}

for (i in 1:nrow(labgrown)) {
  row <- labgrown[i, ]  
  for (element in row) {
    cat("Type:", typeof(element), "\n")
  }
}

# perform clustering analysis using hierarchical clustering----

# plot the data 
plot_dendrogram <- function(table_name){
  indiv_distance <- dist(table_name, method = 'euclidean')
  hc_data <- hclust(indiv_distance,method = 'average')
  plot(hc_data)
}


plot_dendrogram(labgrown) #5 clusters
plot_dendrogram(edible) # 5 clusters
plot_dendrogram(plant) # 5 clusters 

#plot the dendrogram with colors of different clusters

color_dendrogram <- function(table_name, num_cluster){
  indiv_distance <- dist(table_name, method = 'euclidean')
  hc_data <- hclust(indiv_distance,method = 'average')
  dend_df <- as.dendrogram(hc_data)
  dend_color <- color_branches(dend_df, k = num_cluster)
  plot(dend_color)
}

color_dendrogram(labgrown, 5)
color_dendrogram(edible, 5)
color_dendrogram(plant, 5)

#add the cluster to the dataset: 
new_cluster <- function(old_table, new_table, num_cluster){
  indiv_distance <- dist(old_table, method = 'euclidean')
  hc_data <- hclust(indiv_distance,method = 'average')
  clust_data <- cutree(hc_data, k = num_cluster)
  new_table <- mutate(old_table, cluster = clust_data)
  return(new_table)
} 

labgrown_cluster <- new_cluster(labgrown, labgrown_segment, 5)
View(labgrown_cluster)

edible_cluster <- new_cluster(edible, edible_segment, 5)
View(edible_cluster)

plant_cluster <- new_cluster(plant, plant_segment, 5)
View(plant_cluster)

# analyze each group ----
char_cluster <- function(new_table, table, col1, col2) {
  new_table <- table %>% 
    group_by(cluster) %>%
    summarise(average_heard_about = mean(.data[[col1]]), average_try = mean(.data[[col2]]))
}


labgrown_result <- char_cluster(labgrown_result,labgrown_cluster,"heard_lab_grown_meat", "try_labgrown")
edible_result <- char_cluster(edible_result, edible_cluster, "heard_edible_insect", "try_edible")
plant_result <- char_cluster(plant_result, plant_cluster, "heard_plant_based_protein","try_plantbased")


# name the cluster ----

## labgrown meat product: 
#cluster 1: hear but don't know what mean, somewhat willing
#cluster 2: hear and somewhat unwilling
#cluster 3: heard, willing
#cluster 4: not heard, very unwilling
#cluster 5: heard, already tried

# => target cluster 1 and 3

## edible insects product 
#cluster 1: not heard, somewhat willing
#cluster 2: heard, very unwilling
#cluster 3: heard, somewhat willing
#cluster 3: no heard, very unwilling
#cluster 4: heard, already tried 

# -> target 1 and 3

## plant-based protein: 
#cluster 1: No, neither willing nor unwilling
#cluster 2: yes, willing
# cluster 3: yes, don't know
# cluster 4: yes, neither
# cluster 5: No, very unwilling

# -> target 1 and 2 

# mutate to the original dataset----
labgrown_with_cluster <- mutate(protein, cluster = labgrown_cluster$cluster)
edible_with_cluster <- mutate(protein, cluster = edible_cluster$cluster)
plant_with_cluster <- mutate(protein, cluster = plant_cluster$cluster)

# get the important variables for each targeted clusters----
labgrown_by_cluster <- labgrown_with_cluster %>% filter(cluster == 1|cluster == 3)
labgrown_variables <- labgrown_by_cluster[,c(1,2,6,7,8,9,11,12,168)]
labgrown_nona <- na.omit(labgrown_variables)
labgrown_char <- labgrown_nona %>%
  group_by(cluster) %>%
  summarize(across(.cols = everything(), .fns = mean, .names = "avg_{.col}"))

##cluster 1: Female, 45 - 54 years old, Associate degree, married/domestic partner, 
## £25,000 - 34,999, white background skilled working class

##cluster 3: Male, 35-44, Associate degree, lived as a couple, £35,000 - 44,999, 
## white background, skilled working class

edible_by_cluster <- edible_with_cluster %>% filter(cluster == 1|cluster == 3)
edible_variables <- edible_by_cluster[,c(1,2,6,7,8,9,11,12,168)]
edible_nona <- na.omit(edible_variables)
edible_char <- edible_nona %>%
  group_by(cluster) %>%
  summarize(across(.cols = everything(), .fns = mean, .names = "avg_{.col}"))

# cluster 1: female, 25-44, associate degree, domestic couple, £25,000 - 34,999,
# Any other White background,working, working class
# cluster 2: male, 35- 44, associate degree, domestic couple, £35,000 - 44,999, 
# Irish, Working, skilled working class

plant_by_cluster <- plant_with_cluster %>% filter(cluster == 1|cluster == 2)
plant_variables <- plant_by_cluster[,c(1,2,6,7,8,9,11,12,168)]
plant_nona <- na.omit(plant_variables)
plant_char <- plant_nona %>%
  group_by(cluster) %>%
  summarize(across(.cols = everything(), .fns = mean, .names = "avg_{.col}"))

## cluster 1: female, 35-44, lower than associate degree, married, £25,000 - 34,999, 
## Any other White background, working, working class
## cluster 2: female, 45-54, associate degree, lived as couples, £35,000 - 44,999, 
## irish, working, skilled working class

# factor the variables----
heard_list <- c("heard_edible_insect", "heard_lab_grown_meat", "heard_plant_based_protein")

for (i in heard_list) {
  protein[[i]] <- factor(protein[[i]], 
                  labels = c("Heard and know", "Heard but not know", "Not heard", "Don't know"))
}

safe_list <- c('safe_level_plantbasedprotein','safe_level_edibleinsect',
               'safe_level_labgrownmeat')

for (i in safe_list) {
  protein[[i]] <- factor(protein[[i]], 
                         labels = c("Very safe",
                                    "Somewhat safe",
                                    "Neither safe nor unsafe",
                                    "Somewhat unsafe",
                                    "Very unsafe",
                                    "Don't know"))
}

yes_no <- c('labgrown_unhealthy',
            'labgrown_expensive',
            'labgrown_unavailable',
            'labgrown_howtocook',
            'labgrown_offputting',
            'labgrown_unfamiliar',
            'labgrown_regulation',
            'labgrown_notsafe',
            'labgrown_badtaste',
            'labgrown_noneed',
            'labgrown_religious',
            'labgrown_dierestriction',
            'labgrown_ethical',
            'labgrown_traditionmeat',
            'labgrown_others',
            'labgrown_dontknow',
            'labgrown_healthier',
            'labgrown_available',
            'labgrown_lookgood',
            'labgrown_othertried',
            'labgrown_celebtried',
            'labgrown_regulated',
            'labgrown_safe',
            'labgrown_expert',
            'labgrown_cheaper',
            'labgrown_environment',
            'labgrown_animal',
            'labgrown_otherreason',
            'labgrown_nothing',
            'labgrown_noidea',
            'labgrown_healthreason',
            'labgrown_animalreason',
            'labgrown_environmentreason',
            'labgrown_financereason',
            'labgrown_change',
            'labgrown_friendsadvice',
            'labgrown_celebadvice',
            'labgrown_expertadvice',
            'labgrown_hatetradition',
            'labgrown_regulatedreason',
            'labgrown_safereason',
            'labgrown_innovative',
            'labgrown_greaterchoice',
            'labgrown_tryunfamiliar',
            'labgrown_otherreasontotry',
            'labgrown_noreasons',
            'plantbased_unhealthy',
            'plantbased_expensive',
            'plantbased_unavailable',
            'plantbased_howtocook',
            'plantbased_offputting',
            'plantbased_unfamiliar',
            'plantbased_regulation',
            'plantbased_notsafe',
            'plantbased_badtaste',
            'plantbased_noneed',
            'plantbased_religious',
            'plantbased_dierestriction',
            'plantbased_ethical',
            'plantbased_traditionmeat',
            'plantbased_others',
            'plantbased_dontknow',
            'plantbased_healthier',
            'plantbased_available',
            'plantbased_lookgood',
            'plantbased_othertried',
            'plantbased_celebtried',
            'plantbased_regulated',
            'plantbased_safe',
            'plantbased_expert',
            'plantbased_cheaper',
            'plantbased_environment',
            'plantbased_animal',
            'plantbased_otherreason',
            'plantbased_nothing',
            'plantbased_noidea',
            'plantbased_healthreason',
            'plantbased_animalreason',
            'plantbased_environmentreason',
            'plantbased_financereason',
            'plantbased_change',
            'plantbased_friendsadvice',
            'plantbased_celebadvice',
            'plantbased_expertadvice',
            'plantbased_hatetradition',
            'plantbased_regulatedreason',
            'plantbased_safereason',
            'plantbased_innovative',
            'plantbased_greaterchoice',
            'plantbased_tryunfamiliar',
            'plantbased_otherreasontotry',
            'plantbased_noreasons',
            'edible_unhealthy',
            'edible_expensive',
            'edible_unavailable',
            'edible_howtocook',
            'edible_offputting',
            'edible_unfamiliar',
            'edible_regulation',
            'edible_notsafe',
            'edible_badtaste',
            'edible_noneed',
            'edible_religious',
            'edible_dierestriction',
            'edible_ethical',
            'edible_traditionmeat',
            'edible_others',
            'edible_dontknow',
            'edible_healthier',
            'edible_available',
            'edible_lookgood',
            'edible_othertried',
            'edible_celebtried',
            'edible_regulated',
            'edible_safe',
            'edible_expert',
            'edible_cheaper',
            'edible_environment',
            'edible_animal',
            'edible_otherreason',
            'edible_nothing',
            'edible_noidea',
            'edible_healthreason',
            'edible_animalreason',
            'edible_environmentreason',
            'edible_financereason',
            'edible_change',
            'edible_friendsadvice',
            'edible_celebadvice',
            'edible_expertadvice',
            'edible_hatetradition',
            'edible_regulatedreason',
            'edible_safereason',
            'edible_innovative',
            'edible_greaterchoice',
            'edible_tryunfamiliar',
            'edible_otherreasontotry',
            'edible_noreasons'
)

for (i in yes_no) {
  protein[[i]] <- factor(protein[[i]], 
                         labels = c("No",
                                    "Yes"))
}

options <- c('option1_whole_edible',
             'option2_ground',
             'option3_sweets',
             'option4_drink',
             'option5_meal')

for (i in options) {
  protein[[i]] <- factor(protein[[i]], 
                         labels = c("Very willing",
                                    "Somewhat willing",
                                    "Neither willing nor unwilling",
                                    "Somewhat unwilling",
                                    "Very unwilling",
                                    "Don't know",
                                    "I've already tried this"))}

write.csv(protein, "C:\\Users\\Admin\\OneDrive - Dickinson College\\Desktop\\DATA PROJECT\\Healthy Food Project\\protein_final.csv")
