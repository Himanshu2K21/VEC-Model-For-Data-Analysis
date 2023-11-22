# Load necessary libraries
library(vars)
library(lubridate)  
library(tseries)   

# Creating the dataset
your_data_df <- data.frame(
  Date = as.Date(c("10/21/2022", "10/24/2022", "10/25/2022", "10/27/2022", "10/28/2022",
                    "10/31/2022", "11/1/2022", "11/2/2022", "11/3/2022", "11/4/2022",
                    "11/7/2022", "11/9/2022", "11/10/2022", "11/11/2022", "11/14/2022",
                    "11/15/2022", "11/16/2022", "11/17/2022", "11/18/2022"), format="%m/%d/%Y"),
  Open = c(17622.84961, 17736.34961, 17808.30078, 17771.40039, 17756.40039,
           17910.19922, 18130.69922, 18177.90039, 17968.34961, 18053.40039,
           18211.75, 18288.25, 18044.34961, 18272.34961, 18376.40039,
           18362.75, 18398.25, 18358.69922, 18382.94922),
  Close = c(17576.30078, 17730.75, 17656.34961, 17736.94922, 17786.80078,
            18012.19922, 18145.40039, 18082.84961, 18052.69922, 18117.15039,
            18202.80078, 18157, 18028.19922, 18349.69922, 18329.15039,
            18403.40039, 18409.65039, 18343.90039, 18307.65039)
)

# Create a time series object
ts_data <- ts(your_data_df[, c("Open", "Close")], start = c(year(your_data_df$Date[1]), month(your_data_df$Date[1])), frequency = 252)

# Build a Vector Autoregression (VAR) model with BIC for lag order selection
var_model <- VAR(ts_data, type = "both", ic = "BIC")

# Test for cointegration using the ca.jo() function
cointegration_test <- ca.jo(ts_data, ecdet = "const", K = 2)  # Assuming 2 cointegrating relationships
print(summary(cointegration_test))

# Transform the VAR model into a Vector Error Correction (VEC) model
vec_model <- vec2var(cointegration_test, r = 1)  # Assuming 1 cointegrating relationship

# Interpret the VEC model using summary()
vec_model_summary <- summary(vec_model)
print(vec_model_summary)
