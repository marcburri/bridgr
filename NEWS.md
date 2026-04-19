# bridgr (development version)

* Remove the `legendre` parametric aggregation option.

* Use analytic gradients for parametric aggregation optimization with
  `expalmon` weights, and improve the normalized beta polynomial gradient
  used in the optimizer.

# bridgr 0.1.2

* Solve dependency issues with xts


# bridgr 0.1.1

* Initial CRAN submission:
  - Added `gdp`,`baro`, `wea` and `fcurve` datasets.

  - Added `bridge()`, `forecast()` and `summary()` functions.
 
  - Supports target variables on monthly, quarterly and yearly frequency, and 
    indicator variables on daily, weekly, monthly, quarterly and yearly frequency. 
    
  - Supports `auto.arima`, `ets` and other methods for indicator variable forecasting.
 
  - Supports aggregation of indicator variables to match the target's frequency using
    custom weighting functions, exponential Almon polynomials and other methods.
