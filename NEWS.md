# bridgr 0.1.1

# bridgr 0.1.0.9000

* Initial CRAN submission:
  - Added `gdp`,`baro`, `wea` and `fcurve` datasets.

  - Added `bridge()`, `forecast()` and `summary()` functions.
 
  - Supports target variables on monthly, quarterly and yearly frequency, and 
    indicator variables on daily, weekly, monthly, quarterly and yearly frequency. 
    
  - Supports `auto.arima`, `ets` and other methods for indicator variable forecasting.
 
  - Supports aggregation of indicator variables to match the target's frequency using
    custom weighting functions, exponential Almon polynomials and other methods.
 

