# volatility-forecasting-ml-sentiment
Volatility prediction using machine learning models and sentiment variables (R)

# Volatility Forecasting using Machine Learning and Sentiment Variables

## Objective
The goal of this project is to forecast financial market volatility by integrating traditional market variables with behavioral and sentiment indicators. The study aims to:

- Empirically evaluate the performance of Machine Learning models, particularly neural networks, against classical econometric benchmarks (e.g., GARCH).
- Assess the contribution of sentiment and emotional variables in improving predictive accuracy for modern financial markets.

---

## Data
The analysis is conducted over two distinct periods to capture different market dynamics:

- **1881–1990**: Historical period using traditional macro-financial variables and market data.
- **1991–2024**: Recent period integrating both market variables and behavioral indicators related to investor sentiment and market emotions.

The target variable is financial market volatility, measured using realized volatility and econometric-based volatility proxies. Sentiment and emotional variables are derived from market and behavioral indicators to capture investor psychology and market sentiment.

---

## Methodology
The methodology follows a comparative modeling approach:

1. **Preprocessing and Feature Engineering**
   - Data cleaning, normalization, and handling missing values
   - Construction of lagged features to account for volatility clustering
   - Creation of sentiment and emotional variables from market data sources

2. **Benchmark Models**
   - Traditional econometric models (e.g., GARCH, EWMA) for baseline comparison

3. **Machine Learning Models**
   - Neural Networks
   - Regularized regression models (Lasso, Ridge, ElasticNet)

4. **Model Training**
   - Rolling and expanding window frameworks
   - Separation of in-sample and out-of-sample periods
   - Hyperparameter tuning using cross-validation
   - Implemented entirely in **R** for reproducibility

---

## Evaluation
Model performance is assessed using out-of-sample forecasts with standard accuracy metrics:

- **Root Mean Squared Error (RMSE)**
- **Mean Absolute Error (MAE)**

---

## Results
The empirical results indicate:

- Machine Learning models, particularly neural networks, **outperform classical econometric
  benchmarks** in forecasting volatility.
- The inclusion of **sentiment and emotional variables** significantly improves predictive
  accuracy, especially in recent market periods characterized by uncertainty and rapid information flow.
- Findings suggest that **behavioral information is crucial** for modern volatility modeling
  and has practical implications for risk management, derivatives pricing, and volatility-based trading strategies.
 
