### script parameters
patient_count <- 500
output_file <- "./Extras/Samples/simulated_emr.csv"
### gender
patient_df <- data.frame(
  gender = rbinom(patient_count, 1, 0.5) # 0 is male, 1 is female
)
### height
patient_df$height_inches <- sapply(
  patient_df$gender, 
  function(x){
    ifelse(
      x == 1,
      rnorm(n = 1, mean = 63.7, sd = 2.7), 
      rnorm(n = 1, mean = 69.1, sd = 2.9)
    )
  }
)
### age
age_range_df <- data.frame(
  lower_limit <- seq(from = 15, to = 100, by = 5) # exclude upper limit
)
age_range_df$female_fraction <- c( # weights derived from 2010 U.S. census data
  0.084523509,
  0.083225711,
  0.08239466,
  0.078453268,
  0.079807487,
  0.082636571,
  0.090528811,
  0.089468751,
  0.079835332,
  0.06880819,
  0.05182183,
  0.039631232,
  0.032555614,
  0.027151567,
  0.018473331,
  0.008061181,
  0.002274977,
  0.000347976
)
age_range_df$male_fraction <- c(
  0.093812274, 
  0.091409716, 
  0.088267734, 
  0.082963739, 
  0.083341539, 
  0.086262512, 
  0.09302732,
  0.090738288,
  0.079039409,
  0.067037424,
  0.048571919,
  0.035221906,
  0.026411525,
  0.019041649,
  0.010572177,
  0.003522106,
  0.000682724,
  0.000076038
)
age_range_df$female_fraction[1] <- 0.4 * age_range_df$female_fraction[1] # Only allow 18-19 year olds in the first bin.
age_range_df$male_fraction[1] <- 0.4 * age_range_df$male_fraction[1] 
age_range_df$female_fraction <- age_range_df$female_fraction / sum(age_range_df$female_fraction)
age_range_df$male_fraction <- age_range_df$male_fraction / sum(age_range_df$male_fraction)
age_range_df$female_cum_sum <- cumsum(age_range_df$female_fraction)
age_range_df$male_cum_sum <- cumsum(age_range_df$male_fraction)
random_age <- function(genderIn){
  if(genderIn == 1){
    cum_sum_v <- age_range_df$female_cum_sum
  } else{
    cum_sum_v <- age_range_df$male_cum_sum
  }
  temp_uniform <- runif(1, 0.0, 1.0)
  for(i in 1:18){
    if(temp_uniform <= cum_sum_v[i]){
      temp_index <- i
      break
    }
  }
  temp_lower_limit <- ifelse(temp_index > 1, 0, 3)
  return(10 + (temp_index * 5) + sample(temp_lower_limit:4, 1))
}
patient_df$age <- sapply(patient_df$gender, random_age)
### weight
# ideal weight per J.D. Robinson Formula
# males:  114.64 lbs + (height_inches - 60) * 4.19 lbs
# females:  108.03 + (height_incheas - 60) * 3.75 lbs
random_weight <- function(genderIn, heightInchesIn, ageIn){
  if(genderIn == 1){
    temp_constant <- -136.76
    temp_rate <- 4.19
  } else{
    temp_constant <- -116.97
    temp_rate <- 3.75
  }
  temp_ideal_weight <- temp_constant + (temp_rate * heightInchesIn)
  temp_increment <- ageIn * ifelse(genderIn == 1, 0.7, 1.2) * (rweibull(1, shape = 2, scale = 1.3) - 0.5)
  return(temp_ideal_weight + temp_increment)
}
patient_df$weight_lbs <- mapply(random_weight, genderIn = patient_df$gender, heightInchesIn = patient_df$height_inches, ageIn = patient_df$age)
### body-mass index = kg / height_meters ^ 2
patient_df$bmi <- patient_df$weight_lbs * 0.453592
patient_df$bmi <- patient_df$bmi / ((patient_df$height_inches * 0.0254) ^ 2)
### percent body fat 
random_pbf <- function(genderIn, bmiIn, ageIn){
  if(genderIn == 1){
    temp_intercept <- -3.819
    temp_bmi_coeff <- 0.918
    temp_age_coeff <- 0.153
  } else{
    temp_intercept <- -9.662
    temp_bmi_coeff <- 1.114
    temp_age_coeff <- 0.139
  }
  temp_nominal_pbf <- temp_intercept + (temp_bmi_coeff * bmiIn) + (temp_age_coeff * ageIn)
  return(temp_nominal_pbf + rnorm(1, 0, 2))
}
patient_df$pbf <- mapply(random_pbf, genderIn = patient_df$gender, bmiIn = patient_df$bmi, ageIn = patient_df$age)
### IQ
#patient_df$iq <- rnorm(nrow(patient_df), 100, 15)
### write to CSV
write.csv(x = patient_df, file = output_file, row.names = FALSE)
