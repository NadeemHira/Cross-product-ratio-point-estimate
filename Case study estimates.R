# reading the CYP-GUIDES Trial data from Harford Hospital
data1 <- read.csv("C://Users/Hira/Desktop/PHD Thesis/clinical data.csv", header = T)

# Looking at the summary of data
summary(data1)

# Subsetting the data based on the variables of interest
clinical.dat <- data.frame(data1$Assignment, data1$RAR)

# Summary for the clinical data
summary(clinical.dat)

# Subsetting the data based on the course of treatments in Assignment 
# variable for "G" and "S"

# The RCT recruited 1500 patients, genotyped CYP2D6 in 1459, and 
# randomized 477 to standard therapy (Group S), for whom 
# treatment-as-usual guidance was delivered without consideration of 
# patient CYP2D6 genotype, and 982 to genetically-guided therapy (Group G)
#where CYP2D6-based treatment recommendations were provided 
#via EMR to physicians.

assG <- subset(clinical.dat, data1.Assignment=="G")
assS <- subset(clinical.dat, data1.Assignment=="S")

# Direct-Direct sampling scheme
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count1 <- length(which(assG$data1.RAR == 1))
count1

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count2 <- length(which(assS$data1.RAR == 1))
count2

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count1/nrow(assG)
p1

# Determining the probability of number of successes in second sample for
# "S" treatment
p2<- count2/nrow(assS)
p2

# Computing the rho hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Direct-Inverse sampling scheme
# Determining the number of trials for RAR = 1 for the first sample for
# "G" treatment
count3 <- length(which(assG$data1.RAR == 1))
count3

# Determining the probability of number of successes in first sample for 
# "G" treatment
p1 <- count1/nrow(assG)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "s" treatment.
inv2 <- head(assS, -20)

# Counting the number of trials for RAR = 1 for the second inverse sample
# for "S" treatment
count4 <- length(which(inv2$data1.RAR == 1))
count4

# Determining the probability of success for the second sample with "s"
# treatment
p2 <- count4/nrow(inv2)
p2

# Computing the rho hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Inverse-Direct sampling scheme
# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv1 <- head(assG, -16)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count5 <- length(which(inv1$data1.RAR == 1))
count5

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count5/nrow(inv1)
p1

# Determining the number of trials for RAR = 1 for the second sample for
# "S" treatment
count6 <- length(which(assS$data1.RAR == 1))
count6

# Determining the probability of success for the second sample with "S"
# treatment
p2 <- count6/nrow(assS)
p2

# Computing the rho hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho


# Inverse-Inverse sampling scheme
# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv1 <- head(assG, -16)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count7 <- length(which(inv1$data1.RAR == 1))
count7

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count7/nrow(inv1)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "S" treatment.
inv2 <- head(assS, -20)

# Counting the number of trials for RAR = 1 for the second inverse sample
# for "S" treatment
count8 <- length(which(inv2$data1.RAR == 1))
count8

# Determining the probability of success for the second sample with "S"
# treatment
p2 <- count8/nrow(inv2)
p2

# Computing the rho hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho


# Special case of Direct-Inverse sampling scheme
# Reduced data of "G" treatment by fixing the number of successes 
# in the "S" treatment, based on the RAR value of 1.
# Therefore, by looking at the number of successes in S, we reduce the 
# data set by 550 observations
red <- head(assG, -550)

# Counting the number of successes for the reduced data set of G treatment,
# where the success is RAR = 1
count9 <- length(which(red$data1.RAR == 1))
count9

# T is the number of successes in the first sample
T <- count9

# Number of observations in the reduced data set
n <- nrow(red)

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 20 observations
red1 <- head(assS, -20)

# Observations in the "S" treatment groups
nuT <- nrow(red1)
nuT

# Computing the rho hat values
rho <- (nuT - T)/(n+1-T)
rho



# Gender-wise study of treatment for Readmission rate
clinical.dat2 <- data.frame(data1$GENDER,data1$Assignment, data1$RAR)

# Summary of the clinical data with three columns
summary(clinical.dat2)

# Dividing data based on gender = female and gender = male
fem.data <- subset(clinical.dat2, data1.GENDER == "F")
male.data <- subset(clinical.dat2, data1.GENDER == "M")

# Dividing the female data based on treatments "G" and "S"
fem.data.assG <- subset(fem.data, data1.Assignment == "G")
fem.data.assS <- subset(fem.data, data1.Assignment == "S")


# Dividing the female data based on treatments "G" and "S"
male.data.assG <- subset(male.data, data1.Assignment == "G")
male.data.assS <- subset(male.data, data1.Assignment == "S")

# Direct-Direct
# Female

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count13 <- length(which(fem.data.assG$data1.RAR == 1))
count13

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count14 <- length(which(fem.data.assS$data1.RAR == 1))
count14

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count13/nrow(fem.data.assG)
p1

# Determining the probability of number of successes in second sample for
# "S" treatment
p2<- count14/nrow(fem.data.assS)
p2

# Computing the rho-hat value
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Male

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count15 <- length(which(male.data.assG$data1.RAR == 1))
count15

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count16 <- length(which(male.data.assS$data1.RAR == 1))
count16

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count15/nrow(male.data.assG)
p1

# Determining the probability of number of successes in second sample for
# "S" treatment
p2<- count16/nrow(male.data.assS)
p2

# Computing rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho


# Direct-Inverse sampling scheme
# Female

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count17 <- length(which(fem.data.assG$data1.RAR == 1))
count17

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count17/nrow(fem.data.assG)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "s" treatment.
inv22 <- head(fem.data.assS, -14)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count18 <- length(which(inv22$data1.RAR == 1))
count18

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count18/nrow(inv22)
p2

# Estimate for rhohat value
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Male
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count19 <- length(which(male.data.assG$data1.RAR == 1))
count19

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count19/nrow(male.data.assG)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "s" treatment.
inv2m <- head(male.data.assS, -10)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count20 <- length(which(inv2m$data1.RAR == 1))
count20

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count20/nrow(inv2m)
p2

# Computing rhohat value
rho <- p1*(1-p1)/(p2*(1-p2))
rho


# Inverse-Direct sampling scheme
# Female

# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv2f <- head(fem.data.assG, -8)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count21 <- length(which(inv2f$data1.RAR == 1))
count21

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count21/nrow(inv2f)
p1

# Determining the number of trials for RAR = 1 for the second sample for
# "S" treatment
count22 <- length(which(fem.data.assS$data1.RAR == 1))
count22

# Determining the probability of success for the second sample with "S"
# treatment
p2 <- count22/nrow(fem.data.assS)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Male
# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv2m <- head(male.data.assG, -11)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count23 <- length(which(inv2m$data1.RAR == 1))
count23

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count23/nrow(inv2m)
p1

# Determining the number of trials for RAR = 1 for the second sample for
# "S" treatment
count24 <- length(which(male.data.assS$data1.RAR == 1))
count24

# Determining the probability of success for the second sample with "S"
# treatment
p2 <- count24/nrow(male.data.assS)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Inverse-Inverse sampling scheme
# Female
# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv1f <- head(fem.data.assG, -8)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count25 <- length(which(inv1f$data1.RAR == 1))
count25

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count25/nrow(inv1f)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "S" treatment.
inv2f <- head(fem.data.assS, -14)

# Counting the number of trials for RAR = 1 for the second inverse sample
# for "s" treatment
count26 <- length(which(inv2f$data1.RAR == 1))
count26

# Determining the probability of success for the second sample with "s"
# treatment
p2 <- count26/nrow(inv2f)
p2

# Computing the rhohat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Male
# For the inverse sampling scheme (first sample), we reduce the data set
# based on the number of successes for "G" treatment.
inv1m <- head(male.data.assG, -11)

# Counting the number of trials for RAR = 1 for the first inverse sample
# for "G" treatment
count27 <- length(which(inv1m$data1.RAR == 1))
count27

# Determining the probability of success for the first sample with "G"
# treatment
p1 <- count27/nrow(inv1m)
p1

# For the inverse sampling scheme (second sample), we reduce the data set
# based on the number of successes for "S" treatment.
inv2m <- head(male.data.assS, -10)

# Counting the number of trials for RAR = 1 for the second inverse sample
# for "S" treatment
count28 <- length(which(inv2m$data1.RAR == 1))
count28

# Determining the probability of success for the second sample with "S"
# treatment
p2 <- count28/nrow(inv2m)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Special case of Direct-Inverse sampling scheme
# Female
# Reduced data of "G" treatment by fixing the number of successes 
# in the "S" treatment, based on the RAR value of 1.
# Therefore, by looking at the number of successes in S, we reduce the 
# data set by 360 observations

red2 <- head(fem.data.assG, -360)

# Counting the number of successes for the reduced data set of G treatment,
# where the success is RAR = 1
n <- nrow(red2)

count29 <- length(which(red2$data1.RAR == 1))
count29

# T is the number of successes in the first sample
T <- count29

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 14 observations
inv2 <- head(fem.data.assS, -14)


count30 <- length(which(inv2$data1.RAR == 1))
count30

# Observations in the "S" treatment groups
nuT <- nrow(inv2)

# Computing the rho-hat values
rho <- (nuT - T)/(n+1-T)
rho

# Male
# Reduced data of "G" treatment by fixing the number of successes 
# in the "S" treatment, based on the RAR value of 1.
# Therefore, by looking at the number of successes in S, we reduce the 
# data set by 141 observations
red2m <- head(male.data.assG, -141)

# Counting the number of successes for the reduced data set of G treatment,
# where the success is RAR = 1
n <- nrow(red2m)
count31 <- length(which(red2m$data1.RAR == 1))
count31

# T is the number of successes in the first sample
T <- count31

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 10 observations
inv2 <- head(male.data.assS, -10)


count32 <- length(which(inv2$data1.RAR == 1))
count32

# Observations in the "S" treatment groups
nuT <- nrow(inv2)

# Computing the rho-hat values
rho <- (nuT - T)/(n+1-T)
rho

# Age wise study of treatments for the Readmission rates based on 
# people under 40 and over 40 years of age (based on median age)
clinical.dat3 <- data.frame(data1$AGE, data1$Assignment, data1$RAR)
summary(clinical.dat3)

# median Age
median(data1$AGE)

# subsetting data based on  the age of people under and over 40 years
age1 <- subset(clinical.dat3, data1.AGE <= 40)
age2 <- subset(clinical.dat3, data1.AGE > 40)

# Dividing the age-wise data based on treatments "G" and "S"
age1.assG <- subset(age1, data1.Assignment == "G")
age1.assS <- subset(age1, data1.Assignment == "S")

age2.assG <- subset(age2, data1.Assignment == "G")
age2.assS <- subset(age2, data1.Assignment == "S")

# Direct-Direct sampling scheme
# Age1
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count33 <- length(which(age1.assG$data1.RAR == 1))
count33

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count34 <- length(which(age1.assS$data1.RAR == 1))
count34

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count33/nrow(age1.assG)
p1

# Determining the probability of number of successes in second sample for
# "S" treatment
p2<- count34/nrow(age1.assS)
p2

# Computing rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Age2
# Direct-Direct sampling scheme
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count35 <- length(which(age2.assG$data1.RAR == 1))
count35

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count36 <- length(which(age2.assS$data1.RAR == 1))
count36

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count35/nrow(age2.assG)
p1

# Determining the probability of number of successes in second sample for
# "S" treatment
p2<- count36/nrow(age2.assS)
p2


# Computing rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho


# Direct-Inverse sampling scheme
# Age1
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count37 <- length(which(age1.assG$data1.RAR == 1))
count37

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count37/nrow(age1.assG)
p1

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 22 observations
inv3 <- head(age1.assS, -22)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count38 <- length(which(inv3$data1.RAR == 1))
count38

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count38/nrow(inv3)
p2

# Computing rhohat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Age2
# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count39 <- length(which(age2.assG$data1.RAR == 1))
count39

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count39/nrow(age2.assG)
p1

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 9 observations
inv3 <- head(age2.assS, -9)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count40 <- length(which(inv3$data1.RAR == 1))
count40

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count40/nrow(inv3)
p2

# Computing rhohat value
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Inverse-Direct
# Age1
# Reducing the data for "G" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 11 observations
inv21 <- head(age1.assG, -11)

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count41 <- length(which(inv21$data1.RAR == 1))
count41

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count41/nrow(inv21)
p1

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count42 <- length(which(age1.assS$data1.RAR == 1))
count42

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count42/nrow(age1.assS)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Age2
# Reducing the data for "G" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 8 observations
inv22 <- head(age2.assG, -8)

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count43 <- length(which(inv22$data1.RAR == 1))
count43

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count43/nrow(inv22)
p1

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count44 <- length(which(age2.assS$data1.RAR == 1))
count44

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count44/nrow(age2.assS)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Inverse-Inverse
# Age1
# Reducing the data for "G" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 11 observations
inv111 <- head(age1.assG, -11)

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count45 <- length(which(inv111$data1.RAR == 1))
count45

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count45/nrow(inv111)
p1

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 22 observations
inv222 <- head(age1.assS, -22)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count46 <- length(which(inv222$data1.RAR == 1))
count46

# Determining the probability of number of successes in second sample for
# "S" treatment
p2 <- count46/nrow(inv222)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Age2
# Reducing the data for "G" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 8 observations
inv111 <- head(age2.assG, -8)

# Determining the number of trials for RAR = 1 for the first sample for 
# "G" treatment
count47 <- length(which(inv111$data1.RAR == 1))
count47

# Determining the probability of number of successes in first sample for
# "G" treatment
p1 <- count47/nrow(inv111)
p1

# Reducing the data for "S" treatment where last success (RAR = 1) 
# is achieved. Thus, reducing the dataset by 9 observations
inv222 <- head(age2.assS, -9)

# Determining the number of trials for RAR = 1 for the second sample for 
# "S" treatment
count48 <- length(which(inv222$data1.RAR == 1))
count48

# Determining the probability of number of successes in second sample for
# "s" treatment
p2 <- count48/nrow(inv222)
p2

# Computing the rho-hat values
rho <- p1*(1-p1)/(p2*(1-p2))
rho

# Special
# Age1
# Reduced data of "G" treatment by fixing the number of successes 
# in the "S" treatment, based on the RAR value of 1.
# Therefore, by looking at the number of successes in S, we reduce the 
# data set by 232 observations
red3 <- head(age1.assG, -232)

# Counting the number of successes for the reduced data set of G treatment,
# where the success is RAR = 1
count49 <- length(which(red3$data1.RAR == 1))
count49

# T is the number of successes in the first sample
T <- count49

# number of trials in reduced data set
n <- nrow(red3)

# Reducing the dataset with last observation as success.
inv2 <- head(age1.assS, -22)

# Counting the number of successes for the reduced data set of S treatment,
# where the success is RAR = 1
count50 <- length(which(inv2$data1.RAR == 1))
count50

# The number of successes in the second sample
nuT <- nrow(inv2)

# Computing rhohat values
rho <- (nuT -T)/(n+1-T)
rho

# Age2
# Reduced data of "G" treatment by fixing the number of successes 
# in the "S" treatment, based on the RAR value of 1.
# Therefore, by looking at the number of successes in S, we reduce the 
# data set by 275 observations
red32 <- head(age2.assG, -275)

# Counting the number of successes for the reduced data set of G treatment,
# where the success is RAR = 1
count51 <- length(which(red32$data1.RAR == 1))
count51

# T is the number of successes in the first sample
T <- count51

# number of trials in reduced data set
n <- nrow(red32)

# Reducing the dataset with last observation as success.
inv2 <- head(age2.assS, -9)

# Counting the number of successes for the reduced data set of S treatment,
# where the success is RAR = 1
count52 <- length(which(inv2$data1.RAR == 1))
count52

# The number of successes in the second sample
nuT <- nrow(inv2)

# Computing rhohat values
rho <- (nuT -T)/(n+1-T)
rho
