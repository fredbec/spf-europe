# SPF Median Consensus Clean Data

This dataset contains cleaned median consensus SPF (Survey of Professional Forecasters) quarterly forecasts and corresponding realized GDP growth.

## Variables

- **survey_period**  
  The quarter in which the SPF survey was conducted, formatted as `"YYYY QQ"` (e.g., `"2023 Q1"`).

- **spf_h0, spf_h1, spf_h2, spf_h3, spf_h4**  
  Filtered quarterly SPF median forecasts for horizons 0 through 4 consistent with ECB SPF annual projections.

- **rgdp**  
  First releases of annualized quarterly real GDP growth, calculated as:  
  \[
  100 \times \left(\left(\frac{GDP_t}{GDP_{t-1}}\right)^4 - 1\right)
  \]  
  where \(GDP_t\) is the real GDP level at quarter \(t\).

## Notes

- This dataset is intended for forecast evaluation and replication purposes.  
- Additional methodological details and data processing steps are described in the associated research paper and code repository.

---



# SPF Panel Clean Data

This dataset contains cleaned individual SPF (Survey of Professional Forecasters) quarterly forecasts from a panel of forecasters, along with corresponding realized GDP growth.

## Variables

- **forecaster_id**  
  Unique identifier for each individual forecaster in the panel.

- **survey_period**  
  The quarter in which the SPF survey was conducted, formatted as `"YYYY QQ"` (e.g., `"2023 Q1"`).

- **spf_h0, spf_h1, spf_h2, spf_h3, spf_h4**  
  Filtered quarterly SPF individual forecasts for horizons 0 through 4 consistent with ECB SPF annual projections.

- **rgdp**  
  First releases of annualized quarterly real GDP growth, calculated as:  
  \[
  100 \times \left(\left(\frac{GDP_t}{GDP_{t-1}}\right)^4 - 1\right)
  \]  
  where \(GDP_t\) is the real GDP level at quarter \(t\).

## Notes

- This dataset contains individual-level forecasts organized as a panel over forecasters and survey periods.  
- Intended for forecast evaluation and replication purposes.  
- Additional methodological details and data processing steps are described in the associated research paper and code repository.

---