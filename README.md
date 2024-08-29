# Bird Population Dynamics at Jasper Ridge

## Overview
This project analyzes the population dynamics of birds at Jasper Ridge, focusing on understanding both temporal and spatial patterns in bird sightings. The analysis includes:
- Exploratory Data Analysis (EDA)
- Land cover analysis
- Predictive models to forecast bird counts across different seasons using:
  - Generalized Linear Models (GLMs)
  - Ridge regression
  - Lasso regression

## Data
The dataset includes bird sighting data collected from Jasper Ridge between 1999 and 2020. Key data features include:
- **Bird sighting counts**: Aggregated to obtain total bird counts
- **Date, Month, Year**: Temporal variables extracted from the data
- **Season**: Data split into Spring, Summer, Fall, and Winter for seasonal analysis

## Exploratory Data Analysis (EDA)
The EDA reveals insights into the distribution of bird sightings and land cover changes over time:
- Bird sightings show a left-skewed distribution typical of count data
- Temporal distribution by month highlights seasonality in sightings
- Land cover types (e.g., wetland, grassland, shrub, water) exhibit cyclical changes that may impact bird population dynamics

## Spatial & Temporal Autocorrelation
### Spatial Autocorrelation:
- **Moran's I Test**: Positive spatial autocorrelation (p < 0.05) indicates clustering in bird counts.
- **Spatial Filters**: Applied in GLMs by adding lagged land cover variables using the k-nearest neighbor (kNN) approach to account for spatial dependencies.

### Temporal Autocorrelation:
- **Generalized Least Squares (GLS)** models revealed moderate positive temporal correlation (AR(1) parameter = 0.165).
- Temporal effects were addressed by splitting the data into seasonal datasets.

## Predictive Modeling
Three predictive models were developed for each season:

1. **Generalized Linear Models (GLMs)**: Captured relationships between bird counts and lag variables of land cover types.
2. **Ridge Regression**: Used a penalty term to reduce overfitting and improve predictions.
3. **Lasso Regression**: Applied variable selection and regularization to enhance accuracy.

### Model Performance
Models were evaluated using **Mean Absolute Error (MAE)**:

| Season | GLM MAE  | Ridge MAE | Lasso MAE |
|--------|----------|-----------|-----------|
| Spring | 3.550265 | 3.552154  | 3.549097  |
| Summer | 3.373538 | 3.379585  | 3.372767  |
| Fall   | 3.556377 | 3.563616  | 3.564943  |
| Winter | 3.180541 | 3.204355  | 3.191702  |

- All models performed similarly across seasons.
- **Lasso Regression** slightly outperformed other models in Spring and Summer.
- **GLM** performed best in Winter.

## Conclusion
The analysis demonstrates that GLM, Ridge, and Lasso regression models can predict bird abundance at Jasper Ridge with only minor differences in performance across seasons. These insights can aid conservation efforts and improve ecological studies.

## How to Run the Code
This project was developed in **R**. To replicate the analysis:
1. Install required R packages such as `ggplot2`, `glmnet`, and `spdep`.
2. Load the dataset and preprocess it by aggregating bird counts and extracting temporal variables.
3. Perform EDA, run autocorrelation checks, and apply predictive models.
4. Evaluate model performance using MAE.

Refer to the provided R scripts for detailed code and comments.

## License
This project is open-source and licensed under the [MIT License](https://opensource.org/licenses/MIT).
