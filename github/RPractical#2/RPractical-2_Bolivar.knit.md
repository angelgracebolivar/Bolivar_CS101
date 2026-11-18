---
title: "RPractical#2"
author: "Angel Grace Bolivar"
date: "2026-11-17"
output:
  pdf_document: default
  html_document: default
---



##Part A: Importing and Inspecting a CSV

A1

``` r
emp_df <- read.csv("employees.csv", stringsAsFactors = FALSE)
emp_df
```

```
##    emp_id  name department sex age salary years_service rating
## 1       1 Alice         IT   F  34  72000             8      4
## 2       2 Bruno      Sales   M  45  58000            12      3
## 3       3  Chen         IT   M  29  65000             4      5
## 4       4  Dana         HR   F  52  54000            20      3
## 5       5  Elif    Finance   F  38  78000            10      4
## 6       6 Farid      Sales   M  26  42000             2      2
## 7       7 Grace         IT   F  41  88000            15      5
## 8       8  Hiro         HR   M  33  47000             6      3
## 9       9  Ines    Finance   F  55  92000            25      4
## 10     10 Jamal      Sales   M  31  51000             5      4
## 11     11  Kara         IT   F  27  61000             3      3
## 12     12  Liam    Finance   M  48  72000            18      2
## 13     13   Mia         HR   F  36  56000             9      5
## 14     14  Noah      Sales   M  52  63000            22      4
## 15     15  Olga         IT   F  44  79000            11      4
## 16     16 Pedro    Finance   M  30  59000             5      3
## 17     17 Quinn         HR   F  25  39000             1      2
## 18     18   Raj         IT   M  50  95000            24      5
## 19     19  Sara      Sales   F  39  58000             8      3
## 20     20 Tomas    Finance   M  42  69000            14      4
```

A2

``` r
head(emp_df, 5)
```

```
##   emp_id  name department sex age salary years_service rating
## 1      1 Alice         IT   F  34  72000             8      4
## 2      2 Bruno      Sales   M  45  58000            12      3
## 3      3  Chen         IT   M  29  65000             4      5
## 4      4  Dana         HR   F  52  54000            20      3
## 5      5  Elif    Finance   F  38  78000            10      4
```

``` r
str(emp_df)
```

```
## 'data.frame':	20 obs. of  8 variables:
##  $ emp_id       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ name         : chr  "Alice" "Bruno" "Chen" "Dana" ...
##  $ department   : chr  "IT" "Sales" "IT" "HR" ...
##  $ sex          : chr  "F" "M" "M" "F" ...
##  $ age          : int  34 45 29 52 38 26 41 33 55 31 ...
##  $ salary       : int  72000 58000 65000 54000 78000 42000 88000 47000 92000 51000 ...
##  $ years_service: int  8 12 4 20 10 2 15 6 25 5 ...
##  $ rating       : int  4 3 5 3 4 2 5 3 4 4 ...
```

``` r
summary(emp_df)
```

```
##      emp_id             name        department        sex          age       
##  Min.   : 1.00   Length   :20   Length   :20   Length   :20   Min.   :25.00  
##  1st Qu.: 5.75   N.unique :20   N.unique : 4   N.unique : 2   1st Qu.:30.75  
##  Median :10.50   N.blank  : 0   N.blank  : 0   N.blank  : 0   Median :38.50  
##  Mean   :10.50   Min.nchar: 3   Min.nchar: 2   Min.nchar: 1   Mean   :38.85  
##  3rd Qu.:15.25   Max.nchar: 5   Max.nchar: 7   Max.nchar: 1   3rd Qu.:45.75  
##  Max.   :20.00                                                Max.   :55.00  
##      salary      years_service       rating   
##  Min.   :39000   Min.   : 1.00   Min.   :2.0  
##  1st Qu.:55500   1st Qu.: 5.00   1st Qu.:3.0  
##  Median :62000   Median : 9.50   Median :4.0  
##  Mean   :64900   Mean   :11.10   Mean   :3.6  
##  3rd Qu.:73500   3rd Qu.:15.75   3rd Qu.:4.0  
##  Max.   :95000   Max.   :25.00   Max.   :5.0
```

A3

``` r
nrow(emp_df)
```

```
## [1] 20
```

``` r
ncol(emp_df)
```

```
## [1] 8
```

``` r
sapply(emp_df, class)
```

```
##        emp_id          name    department           sex           age 
##     "integer"   "character"   "character"   "character"     "integer" 
##        salary years_service        rating 
##     "integer"     "integer"     "integer"
```

A4

``` r
mean(emp_df$salary)
```

```
## [1] 64900
```

``` r
max(emp_df$age)
```

```
## [1] 55
```

``` r
table(emp_df$department)
```

```
## 
## Finance      HR      IT   Sales 
##       5       4       6       5
```

##PART B: Creating Data Frames

B1

``` r
new_hires <- data.frame(
  emp_id = c(21, 22, 23),
  name = c("Uma", "Victor", "Wen"),
  department = c("HR", "IT", "Sales"),
  sex = c("F", "M", "F"),
  age = c(28, 37, 33),
  salary = c(45000, 83000, 60000),
  years_service = c(2, 9, 6),
  rating = c(3, 4, 5)
)

new_hires
```

```
##   emp_id   name department sex age salary years_service rating
## 1     21    Uma         HR   F  28  45000             2      3
## 2     22 Victor         IT   M  37  83000             9      4
## 3     23    Wen      Sales   F  33  60000             6      5
```

B2

``` r
emp_all <- rbind(emp_df, new_hires)
dim(emp_all)
```

```
## [1] 23  8
```

B3

``` r
emp_all$bunos <- round(emp_all$salary*0.05)
emp_all
```

```
##    emp_id   name department sex age salary years_service rating bunos
## 1       1  Alice         IT   F  34  72000             8      4  3600
## 2       2  Bruno      Sales   M  45  58000            12      3  2900
## 3       3   Chen         IT   M  29  65000             4      5  3250
## 4       4   Dana         HR   F  52  54000            20      3  2700
## 5       5   Elif    Finance   F  38  78000            10      4  3900
## 6       6  Farid      Sales   M  26  42000             2      2  2100
## 7       7  Grace         IT   F  41  88000            15      5  4400
## 8       8   Hiro         HR   M  33  47000             6      3  2350
## 9       9   Ines    Finance   F  55  92000            25      4  4600
## 10     10  Jamal      Sales   M  31  51000             5      4  2550
## 11     11   Kara         IT   F  27  61000             3      3  3050
## 12     12   Liam    Finance   M  48  72000            18      2  3600
## 13     13    Mia         HR   F  36  56000             9      5  2800
## 14     14   Noah      Sales   M  52  63000            22      4  3150
## 15     15   Olga         IT   F  44  79000            11      4  3950
## 16     16  Pedro    Finance   M  30  59000             5      3  2950
## 17     17  Quinn         HR   F  25  39000             1      2  1950
## 18     18    Raj         IT   M  50  95000            24      5  4750
## 19     19   Sara      Sales   F  39  58000             8      3  2900
## 20     20  Tomas    Finance   M  42  69000            14      4  3450
## 21     21    Uma         HR   F  28  45000             2      3  2250
## 22     22 Victor         IT   M  37  83000             9      4  4150
## 23     23    Wen      Sales   F  33  60000             6      5  3000
```

##PART C: Subsetting

C1

``` r
emp_all[1:5, c("name", "salary")]
```

```
##    name salary
## 1 Alice  72000
## 2 Bruno  58000
## 3  Chen  65000
## 4  Dana  54000
## 5  Elif  78000
```

C2

``` r
emp_all[emp_all$salary > 70000, ]
```

```
##    emp_id   name department sex age salary years_service rating bunos
## 1       1  Alice         IT   F  34  72000             8      4  3600
## 5       5   Elif    Finance   F  38  78000            10      4  3900
## 7       7  Grace         IT   F  41  88000            15      5  4400
## 9       9   Ines    Finance   F  55  92000            25      4  4600
## 12     12   Liam    Finance   M  48  72000            18      2  3600
## 15     15   Olga         IT   F  44  79000            11      4  3950
## 18     18    Raj         IT   M  50  95000            24      5  4750
## 22     22 Victor         IT   M  37  83000             9      4  4150
```

C3

``` r
subset(emp_all, department == "IT" & years_service >= 5, select = c(name, years_service, salary))
```

```
##      name years_service salary
## 1   Alice             8  72000
## 7   Grace            15  88000
## 15   Olga            11  79000
## 18    Raj            24  95000
## 22 Victor             9  83000
```

C4

``` r
which(emp_all$age > 50)
```

```
## [1]  4  9 14
```

``` r
emp_all$name[which(emp_all$age > 50)]
```

```
## [1] "Dana" "Ines" "Noah"
```

C5

``` r
new_data <- emp_all[, -which(names(emp_all) %in% c("emp_id", "bunos"))]

head(new_data, 3)
```

```
##    name department sex age salary years_service rating
## 1 Alice         IT   F  34  72000             8      4
## 2 Bruno      Sales   M  45  58000            12      3
## 3  Chen         IT   M  29  65000             4      5
```

C6

``` r
emp_all$salary[emp_all$name == "Mia"]
```

```
## [1] 56000
```

##PART D: Factors

D1

``` r
emp_all$department <- factor(emp_all$department)
emp_all$sex <- factor(emp_all$sex)

levels(emp_all$department)
```

```
## [1] "Finance" "HR"      "IT"      "Sales"
```

``` r
levels(emp_all$sex)
```

```
## [1] "F" "M"
```

D2

``` r
table(emp_all$department)
```

```
## 
## Finance      HR      IT   Sales 
##       5       5       7       6
```

``` r
table(emp_all$department, emp_all$sex)
```

```
##          
##           F M
##   Finance 2 3
##   HR      4 1
##   IT      4 3
##   Sales   2 4
```

D3

``` r
emp_all$rating_level <- factor(
  emp_all$rating,
  levels = 1:5,
  labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
  ordered = TRUE
)

table(emp_all$rating_level)
```

```
## 
##      Poor      Fair      Good Very Good Excellent 
##         0         3         7         8         5
```

D4

``` r
for (dept in levels(emp_all$department)) {
  avg_salary <- round(mean(emp_all$salary[emp_all$department == dept]))
  print(paste(dept, avg_salary))
}
```

```
## [1] "Finance 74000"
## [1] "HR 48200"
## [1] "IT 77571"
## [1] "Sales 55333"
```

## PART E:Conditional Statements

E1

``` r
emp_all$salary_band <- ifelse(
  emp_all$salary >= 70000, "High",
  ifelse(emp_all$salary >= 50000, "Medium", "Low")
)

table(emp_all$salary_band)
```

```
## 
##   High    Low Medium 
##      8      4     11
```

E2

``` r
emp_all$level <- ifelse(
  emp_all$years_service >= 8,
  "Senior",
  "Junior"
)

table(emp_all$level)
```

```
## 
## Junior Senior 
##      9     14
```

E3

``` r
raise_pct <- function(rating) {
  if (rating == 5) {
    return(0.10)
  } else if (rating == 4) {
    return(0.07)
  } else if (rating == 3) {
    return(0.04)
  } else {
    return(0)
  }
}

emp_all$raise <- sapply(emp_all$rating, raise_pct)

emp_all$new_salary <- round(emp_all$salary * (1 + emp_all$raise))

head(emp_all[, c("name", "rating", "raise", "new_salary")], 5)
```

```
##    name rating raise new_salary
## 1 Alice      4  0.07      77040
## 2 Bruno      3  0.04      60320
## 3  Chen      5  0.10      71500
## 4  Dana      3  0.04      56160
## 5  Elif      4  0.07      83460
```

E4

``` r
total <- sum(emp_all$new_salary)

if (total < 1600000) {
  print("Budget OK")
} else {
  print("Over budget")
}
```

```
## [1] "Budget OK"
```

``` r
print(total)
```

```
## [1] 1578770
```

##PART E: Conditional Statements

F1

``` r
sort(emp_all$salary, decreasing = TRUE)[1:5]
```

```
## [1] 95000 92000 88000 83000 79000
```

``` r
sort(emp_all$name)[1:5]
```

```
## [1] "Alice" "Bruno" "Chen"  "Dana"  "Elif"
```

F2

``` r
by_salary <- emp_all[order(emp_all$salary, decreasing = TRUE), ]
head(by_salary[, c("name", "department", "salary")], 5)
```

```
##      name department salary
## 18    Raj         IT  95000
## 9    Ines    Finance  92000
## 7   Grace         IT  88000
## 22 Victor         IT  83000
## 15   Olga         IT  79000
```

