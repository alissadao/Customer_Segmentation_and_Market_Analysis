# 1. read the data in memory ----------------------
customer_segment <- read.csv("C:\\Users\\Admin\\Downloads\\sil_clus.csv")
colnames(customer_segment)


# 2. Data Preparation -------------------
# 2a. Data Selection --------------------

#define the response column 
response_col <- customer_segment[1:13]
response_name <- colnames(response_col)
response_name

#define the result column
result_col <- customer_segment[20:22]
result_name <- colnames(result_col)

# 2b. Identify which columns are suitable for chi-squared test------------

# Initialize a list to store significant columns for each result
columns_lists <- list()

for (result in result_name) {
  # Initialize a list for this result
  result_columns <- character(0)
  
  for (col in response_name) {
    contingencyTable <- table(customer_segment[[col]], customer_segment[[result]])
    expected_results <- chisq.test(contingencyTable)$expected
    
    # Check if all expected results are NOT less than 5
    if (!any(expected_results < 5)) {
      # Add the column name to the list for this result
      result_columns <- c(result_columns, col)
    }
  }
  
  # Store the list of significant columns for this result in the main list
  columns_lists[[result]] <- result_columns
}

# Print the lists of significant columns for each result
for (result in result_name) {
  cat("Result:", result, "\n")
  cat("Significant Columns:", paste(columns_lists[[result]], collapse = ", "), "\n")
}

# 3. Conduct chi-squared test for suitable columns--------

# 3a. Define function for chi-squared test ---------
chitest_function <- function(response_column, result_column) {
  significant_level <- 0.05
  for (col in response_column) {
    chitest_value <- chisq.test(customer_segment[[col]],result_column)
    if (chitest_value$p.value < significant_level) {
      print(col)
      print(chitest_value$p.value)
    }
  }
}

# 3b. test with edible insect ---------------
edible_insect_column <- c("DMQ_RESP_GENDER","DMQ_EMP01_RECODED")
edible_result <- chitest_function(edible_insect_column, customer_segment$Q2018263715Edibleinsectsegmealwormsgrasshoppers )

##both of them have relationship with the willingness to try edible insects


# 3c. test with  plant based -----------------
plant_based_column <- c("DMQ_RESP_GENDER","DMQ_RESP_AGE_RECODED","DMQ_EMP01_RECODED","DMQ_UK01SG_RECODED")
plant_based_result <- chitest_function(plant_based_column,customer_segment$Q2018263715Plantbasedproteinsegsoyhempseedquinoa)

##"DMQ_RESP_AGE_RECODED", "DMQ_EMP01_RECODED","DMQ_UK01SG_RECODED" are significant



# 4. Conduct Fisher's Exact Test -------------

# 4a. Define function ------------
fisher_function <- function(fisher_column, general_col){
  significant_level <- 0.05
  for (i in fisher_column) {
    table_contigency <- table(customer_segment[[i]],general_col)
    fisher_result <- fisher.test(table_contigency,simulate.p.value=TRUE)
    if (fisher_result$p.value <= significant_level) {
      print(i)
      print(fisher_result$p.value)
    }
    
  }
}


# 4b. lab grown ----------------- 
meat_lab_result <- fisher_function(response_name,customer_segment$Q2018263715Labgrownmeatsometimesreferredtoasculturedmeatorcu)
##DMQ_RESP_GENDER","DMQ_RESP_AGE_RECODED","DMQ_KIDS02",DMQ_UK02EDU,DMQ_UK01MAR,DMQ_UK02INC,DMQ_EMP01,DMQ_EMP01_RECODED


#4c. edible insects --------------------
fisher_edible_col <- c("DMQ_RESP_AGE_RECODED","DMQ_REGION","DMQ_HHCMP10","DMQ_KIDS02",
                       "DMQ_UK02EDU","DMQ_UK01MAR","DMQ_UK02INC","DMQ_UK02ETH","DMQ_EMP01",
                       "UK01SG","DMQ_UK01SG_RECODED")
fisher_edible_result <- fisher_function(fisher_edible_col, customer_segment$Q2018263715Edibleinsectsegmealwormsgrasshoppers)
##DMQ_RESP_AGE_RECODED,DMQ_KIDS02,DMQ_UK02EDU,DMQ_UK01MAR,DMQ_UK02INC,DMQ_EMP01



# 4c. plant-based protein ---------------
plant_col <- c("DMQ_REGION","DMQ_HHCMP10","DMQ_KIDS02","DMQ_UK02EDU","DMQ_UK01MAR","DMQ_UK02INC","DMQ_UK02ETH","DMQ_EMP01","UK01SG")
fisher_plant_result <- fisher_function(plant_col,customer_segment$Q2018263715Plantbasedproteinsegsoyhempseedquinoa)
##"DMQ_UK02EDU","DMQ_UK01MAR","DMQ_UK02INC,"DMQ_EMP01","UK01SG"


# 5. Summary ----------------

## in general, 
#for lab grown, DMQ_RESP_GENDER","DMQ_RESP_AGE_RECODED",DMQ_UK02EDU,DMQ_UK01MAR,DMQ_UK02INC,DMQ_EMP01,DMQ_EMP01_RECODED have significant 
#relationship with willingness to try 

#for edible insects, DMQ_RESP_AGE_RECODED,DMQ_UK02EDU,DMQ_UK01MAR,DMQ_UK02INC,DMQ_EMP01,DMQ_RESP_GENDER,DMQ_EMP01_RECODED
#have a significant relationship with willingness to try 

#for plant based protein, DMQ_UK02EDU","DMQ_UK01MAR","DMQ_UK02INC,"DMQ_EMP01","UK01SG,"DMQ_RESP_AGE_RECODED", 
#DMQ_EMP01_RECODED","DMQ_UK01SG_RECODED"
