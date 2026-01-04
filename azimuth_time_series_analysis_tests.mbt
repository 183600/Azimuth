// Time Series Analysis Tests for Azimuth Telemetry System
// This file contains test cases for time series data analysis and processing

// Test 1: Time Series Data Collection
test "time series data collection" {
  let collector = TimeSeriesCollector::new()
  
  // Create time series data points
  let base_timestamp = 1234567890L
  let time_series_data = []
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("cpu_usage"))
    Attributes::set(attrs, "host", StringValue("server-01"))
    Attributes::set(attrs, "region", StringValue("us-east"))
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L), // Every minute
      20.0 + (Float::random() * 60.0), // Random CPU usage between 20-80%
      attrs
    )
    
    time_series_data.push(data_point)
  }
  
  // Add data to collector
  for data_point in time_series_data {
    TimeSeriesCollector::add(collector, data_point)
  }
  
  // Verify data collection
  let collected_data = TimeSeriesCollector::get_all(collector)
  assert_eq(collected_data.length(), 100)
  
  // Verify data is sorted by timestamp
  for i in 1..(collected_data.length() - 1) {
    assert_true(collected_data[i-1].timestamp <= collected_data[i].timestamp)
  }
  
  // Test filtering by attributes
  let filter_attrs = [("metric", "cpu_usage"), ("host", "server-01")]
  let filtered_data = TimeSeriesCollector::filter_by_attributes(collector, filter_attrs)
  
  assert_eq(filtered_data.length(), 100) // All data points should match
  
  // Test filtering by time range
  let start_time = base_timestamp + 30 * 60L // 30 minutes after start
  let end_time = base_timestamp + 60 * 60L   // 60 minutes after start
  
  let time_filtered_data = TimeSeriesCollector::filter_by_time_range(
    collector,
    start_time,
    end_time
  )
  
  assert_eq(time_filtered_data.length(), 31) // Points from 30 to 60 inclusive
  
  // Test aggregation by time window
  let window_size = 10 * 60L // 10 minutes
  let aggregated_data = TimeSeriesCollector::aggregate_by_time_window(
    collector,
    window_size,
    Average
  )
  
  assert_eq(aggregated_data.length(), 10) // 100 points / 10-minute windows = 10 windows
  
  for window in aggregated_data {
    assert_true(window.value >= 20.0) // Minimum CPU usage
    assert_true(window.value <= 80.0) // Maximum CPU usage
    assert_eq(window.point_count, 10) // Each window should have 10 points
  }
}

// Test 2: Time Series Trend Analysis
test "time series trend analysis" {
  let analyzer = TimeSeriesAnalyzer::new()
  
  // Create time series with known trend
  let trend_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("memory_usage"))
    Attributes::set(attrs, "host", StringValue("server-02"))
    
    // Create upward trend: starting at 50MB, increasing by 1MB each hour
    let trend_value = 50.0 + (i as Float)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      trend_value,
      attrs
    )
    
    trend_data.push(data_point)
  }
  
  // Analyze trend
  let trend_result = TimeSeriesAnalyzer::calculate_trend(analyzer, trend_data)
  
  match trend_result {
    Some(trend) => {
      assert_true(trend.slope > 0.9) // Slope should be close to 1.0
      assert_true(trend.slope < 1.1)  // But not exactly 1.0 due to floating point
      assert_true(trend.intercept > 49.0) // Intercept should be close to 50.0
      assert_true(trend.intercept < 51.0)
      assert_eq(trend.direction, Upward)
      assert_true(trend.correlation > 0.99) // Strong correlation
    }
    None => assert_true(false) // Should detect trend
  }
  
  // Create time series with downward trend
  let downward_data = []
  
  for i in 1..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("disk_space"))
    Attributes::set(attrs, "host", StringValue("server-03"))
    
    // Create downward trend: starting at 100GB, decreasing by 1GB each hour
    let trend_value = 100.0 - (i as Float)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      trend_value,
      attrs
    )
    
    downward_data.push(data_point)
  }
  
  // Analyze downward trend
  let downward_result = TimeSeriesAnalyzer::calculate_trend(analyzer, downward_data)
  
  match downward_result {
    Some(trend) => {
      assert_true(trend.slope < -0.9) // Negative slope
      assert_true(trend.slope > -1.1)
      assert_true(trend.intercept > 99.0) // Intercept should be close to 100.0
      assert_true(trend.intercept < 101.0)
      assert_eq(trend.direction, Downward)
      assert_true(trend.correlation > 0.99) // Strong correlation
    }
    None => assert_true(false) // Should detect trend
  }
  
  // Create time series with no clear trend
  let no_trend_data = []
  
  for i in 1..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("random_metric"))
    Attributes::set(attrs, "host", StringValue("server-04"))
    
    // Random values around 50
    let random_value = 50.0 + (Float::random() * 10.0 - 5.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      random_value,
      attrs
    )
    
    no_trend_data.push(data_point)
  }
  
  // Analyze no trend
  let no_trend_result = TimeSeriesAnalyzer::calculate_trend(analyzer, no_trend_data)
  
  match no_trend_result {
    Some(trend) => {
      assert_true(trend.slope.abs() < 0.5) // Small slope
      assert_eq(trend.direction, Stable)
      assert_true(trend.correlation.abs() < 0.5) // Weak correlation
    }
    None => assert_true(false) // Should return a result even for weak trends
  }
}

// Test 3: Seasonality Detection
test "seasonality detection" {
  let analyzer = TimeSeriesAnalyzer::new()
  
  // Create time series with daily seasonality
  let seasonal_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=168 { // 7 days * 24 hours
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("web_traffic"))
    Attributes::set(attrs, "host", StringValue("web-server"))
    
    // Create daily pattern: higher during day (8-20), lower at night
    let hour_of_day = i % 24
    let base_traffic = 100.0
    let seasonal_factor = if hour_of_day >= 8 && hour_of_day <= 20 {
      1.5 + 0.5 * Float::sin((hour_of_day - 8) as Float * 3.14159 / 12.0)
    } else {
      0.5 + 0.3 * Float::sin((hour_of_day - 20) as Float * 3.14159 / 12.0)
    }
    
    let traffic_value = base_traffic * seasonal_factor
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      traffic_value,
      attrs
    )
    
    seasonal_data.push(data_point)
  }
  
  // Detect seasonality
  let seasonality_result = TimeSeriesAnalyzer::detect_seasonality(analyzer, seasonal_data)
  
  match seasonality_result {
    Some(seasonality) => {
      assert_eq(seasonality.period, 24) // 24-hour period
      assert_true(seasonality.strength > 0.7) // Strong seasonality
      assert_eq(seasonality.pattern_type, Daily)
      
      // Verify seasonal pattern peaks and troughs
      assert_true(seasonality.peak_hour >= 10) // Peak around midday
      assert_true(seasonality.peak_hour <= 16)
      assert_true(seasonality.trough_hour >= 2) // Trough around night
      assert_true(seasonality.trough_hour <= 6)
    }
    None => assert_true(false) // Should detect seasonality
  }
  
  // Create time series with weekly seasonality
  let weekly_data = []
  
  for i in 1..=28 { // 4 weeks * 7 days
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("sales"))
    Attributes::set(attrs, "host", StringValue("retail-server"))
    
    // Create weekly pattern: higher on weekdays, lower on weekends
    let day_of_week = i % 7
    let base_sales = 1000.0
    let weekly_factor = if day_of_week >= 1 && day_of_week <= 5 {
      1.2 // 20% higher on weekdays
    } else {
      0.7 // 30% lower on weekends
    }
    
    let sales_value = base_sales * weekly_factor
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 86400L), // Every day
      sales_value,
      attrs
    )
    
    weekly_data.push(data_point)
  }
  
  // Detect weekly seasonality
  let weekly_seasonality_result = TimeSeriesAnalyzer::detect_seasonality(analyzer, weekly_data)
  
  match weekly_seasonality_result {
    Some(seasonality) => {
      assert_eq(seasonality.period, 7) // 7-day period
      assert_true(seasonality.strength > 0.6) // Strong seasonality
      assert_eq(seasonality.pattern_type, Weekly)
    }
    None => assert_true(false) // Should detect seasonality
  }
  
  // Create time series with no seasonality
  let no_seasonality_data = []
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("random_walk"))
    Attributes::set(attrs, "host", StringValue("test-server"))
    
    // Random walk
    let random_value = if i == 1 { 50.0 } else {
      no_seasonality_data[i-2].value + (Float::random() * 2.0 - 1.0)
    }
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      random_value,
      attrs
    )
    
    no_seasonality_data.push(data_point)
  }
  
  // Detect no seasonality
  let no_seasonality_result = TimeSeriesAnalyzer::detect_seasonality(analyzer, no_seasonality_data)
  
  match no_seasonality_result {
    Some(seasonality) => {
      assert_true(seasonality.strength < 0.3) // Weak or no seasonality
      assert_eq(seasonality.pattern_type, None)
    }
    None => assert_true(true) // May not detect any seasonality
  }
}

// Test 4: Anomaly Detection in Time Series
test "anomaly detection in time series" {
  let detector = TimeSeriesAnomalyDetector::new()
  
  // Create baseline time series
  let baseline_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("response_time"))
    Attributes::set(attrs, "host", StringValue("api-server"))
    
    // Normal response time: 100ms ± 20ms
    let normal_value = 100.0 + (Float::random() * 40.0 - 20.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L), // Every minute
      normal_value,
      attrs
    )
    
    baseline_data.push(data_point)
  }
  
  // Train the anomaly detector
  TimeSeriesAnomalyDetector::train(detector, baseline_data)
  
  // Create test data with anomalies
  let test_data = []
  
  // Add normal data points
  for i in 1..=20 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("response_time"))
    Attributes::set(attrs, "host", StringValue("api-server"))
    
    let normal_value = 100.0 + (Float::random() * 40.0 - 20.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (100 + i) * 60L,
      normal_value,
      attrs
    )
    
    test_data.push(data_point)
  }
  
  // Add spike anomaly
  let spike_attrs = Attributes::new()
  Attributes::set(spike_attrs, "metric", StringValue("response_time"))
  Attributes::set(spike_attrs, "host", StringValue("api-server"))
  
  let spike_data_point = TimeSeriesDataPoint::new(
    base_timestamp + 121 * 60L,
    500.0, // Much higher than normal
    spike_attrs
  )
  
  test_data.push(spike_data_point)
  
  // Add drop anomaly
  let drop_attrs = Attributes::new()
  Attributes::set(drop_attrs, "metric", StringValue("response_time"))
  Attributes::set(drop_attrs, "host", StringValue("api-server"))
  
  let drop_data_point = TimeSeriesDataPoint::new(
    base_timestamp + 122 * 60L,
    10.0, // Much lower than normal
    drop_attrs
  )
  
  test_data.push(drop_data_point)
  
  // Add more normal data points
  for i in 123..=140 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("response_time"))
    Attributes::set(attrs, "host", StringValue("api-server"))
    
    let normal_value = 100.0 + (Float::random() * 40.0 - 20.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + i * 60L,
      normal_value,
      attrs
    )
    
    test_data.push(data_point)
  }
  
  // Detect anomalies
  let anomalies = TimeSeriesAnomalyDetector::detect(detector, test_data)
  
  // Should detect 2 anomalies
  assert_eq(anomalies.length(), 2)
  
  // Check first anomaly (spike)
  let first_anomaly = anomalies[0]
  assert_eq(first_anomaly.data_point.value, 500.0)
  assert_eq(first_anomaly.anomaly_type, Spike)
  assert_true(first_anomaly.anomaly_score > 0.8)
  
  // Check second anomaly (drop)
  let second_anomaly = anomalies[1]
  assert_eq(second_anomaly.data_point.value, 10.0)
  assert_eq(second_anomaly.anomaly_type, Drop)
  assert_true(second_anomaly.anomaly_score > 0.8)
  
  // Test with different anomaly detection methods
  let statistical_detector = StatisticalAnomalyDetector::new(2.0) // 2 sigma threshold
  let statistical_anomalies = StatisticalAnomalyDetector::detect(statistical_detector, test_data)
  
  assert_eq(statistical_anomalies.length(), 2)
  
  let ml_detector = MLAnomalyDetector::new(IsolationForest)
  let ml_anomalies = MLAnomalyDetector::detect(ml_detector, test_data)
  
  assert_true(ml_anomalies.length() >= 1) // ML method might detect more or fewer anomalies
}

// Test 5: Time Series Forecasting
test "time series forecasting" {
  let forecaster = TimeSeriesForecaster::new()
  
  // Create historical data
  let historical_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("cpu_usage"))
    Attributes::set(attrs, "host", StringValue("db-server"))
    
    // Create pattern with trend and seasonality
    let trend = 50.0 + (i as Float) * 0.1 // Upward trend
    let seasonal = 10.0 * Float::sin((i as Float) * 2.0 * 3.14159 / 24.0) // Daily seasonality
    let noise = Float::random() * 5.0 - 2.5 // Random noise
    
    let cpu_value = trend + seasonal + noise
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      cpu_value,
      attrs
    )
    
    historical_data.push(data_point)
  }
  
  // Train forecasting model
  TimeSeriesForecaster::train(forecaster, historical_data)
  
  // Generate forecasts for next 24 hours
  let forecast_result = TimeSeriesForecaster::forecast(forecaster, 24)
  
  match forecast_result {
    Some(forecasts) => {
      assert_eq(forecasts.length(), 24)
      
      // Verify forecast timestamps
      for i in 0..(forecasts.length() - 1) {
        assert_true(forecasts[i].timestamp < forecasts[i+1].timestamp)
        assert_eq(forecasts[i].timestamp, base_timestamp + (100 + i + 1) * 3600L)
      }
      
      // Verify forecast values are reasonable
      for forecast in forecasts {
        assert_true(forecast.value > 0.0) // CPU usage should be positive
        assert_true(forecast.value < 100.0) // CPU usage should be less than 100%
        assert_true(forecast.confidence_interval_lower > 0.0)
        assert_true(forecast.confidence_interval_upper > forecast.value)
      }
      
      // Verify trend continues
      let avg_forecast = forecasts.map(f => f.value).sum() / (forecasts.length() as Float)
      let avg_recent = historical_data.slice(-10).map(d => d.value).sum() / 10.0
      
      assert_true(avg_forecast > avg_recent) // Should continue upward trend
    }
    None => assert_true(false) // Should generate forecasts
  }
  
  // Test different forecasting methods
  let linear_forecaster = LinearTrendForecaster::new()
  LinearTrendForecaster::train(linear_forecaster, historical_data)
  
  let linear_forecasts = LinearTrendForecaster::forecast(linear_forecaster, 12)
  assert_eq(linear_forecasts.length(), 12)
  
  let seasonal_forecaster = SeasonalNaiveForecaster::new(24) // 24-hour seasonality
  SeasonalNaiveForecaster::train(seasonal_forecaster, historical_data)
  
  let seasonal_forecasts = SeasonalNaiveForecaster::forecast(seasonal_forecaster, 12)
  assert_eq(seasonal_forecasts.length(), 12)
  
  // Compare forecast accuracy using cross-validation
  let cross_validation_result = TimeSeriesForecaster::cross_validate(
    forecaster,
    historical_data,
    5, // 5-fold cross-validation
    12 // Forecast horizon
  )
  
  assert_true(cross_validation_result.mae > 0.0) // Mean absolute error
  assert_true(cross_validation_result.rmse > 0.0) // Root mean square error
  assert_true(cross_validation_result.mape < 50.0) // Mean absolute percentage error should be reasonable
}

// Test 6: Time Series Decomposition
test "time series decomposition" {
  let decomposer = TimeSeriesDecomposer::new()
  
  // Create time series with trend, seasonality, and noise
  let composite_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=168 { // 7 days * 24 hours
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("server_load"))
    Attributes::set(attrs, "host", StringValue("app-server"))
    
    // Create components
    let trend = 50.0 + (i as Float) * 0.05 // Slight upward trend
    let seasonal = 20.0 * Float::sin((i as Float) * 2.0 * 3.14159 / 24.0) // Daily seasonality
    let noise = Float::random() * 10.0 - 5.0 // Random noise
    
    let load_value = trend + seasonal + noise
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Every hour
      load_value,
      attrs
    )
    
    composite_data.push(data_point)
  }
  
  // Decompose time series
  let decomposition_result = TimeSeriesDecomposer::decompose(decomposer, composite_data, 24) // 24-hour seasonality
  
  match decomposition_result {
    Some(decomposition) => {
      assert_eq(decomposition.trend.length(), 168)
      assert_eq(decomposition.seasonal.length(), 168)
      assert_eq(decomposition.residual.length(), 168)
      
      // Verify components sum to original (approximately)
      for i in 0..(composite_data.length() - 1) {
        let reconstructed = decomposition.trend[i] + decomposition.seasonal[i] + decomposition.residual[i]
        let original = composite_data[i].value
        let error = (reconstructed - original).abs()
        
        assert_true(error < 0.001) // Should be very close
      }
      
      // Verify trend component
      let first_trend = decomposition.trend[0]
      let last_trend = decomposition.trend[167]
      assert_true(last_trend > first_trend) // Should show upward trend
      
      // Verify seasonal component
      let seasonal_sum = decomposition.seasonal.map(s => s).sum()
      assert_true(seasonal_sum.abs() < 0.001) // Seasonal component should sum to zero
      
      // Verify residual component
      let residual_mean = decomposition.residual.map(r => r).sum() / (decomposition.residual.length() as Float)
      assert_true(residual_mean.abs() < 0.001) // Residual should have zero mean
      
      // Verify seasonal pattern
      let daily_pattern = TimeSeriesDecomposer::extract_seasonal_pattern(decomposition, 24)
      assert_eq(daily_pattern.length(), 24)
      
      let pattern_sum = daily_pattern.map(p => p).sum()
      assert_true(pattern_sum.abs() < 0.001) // Pattern should sum to zero
    }
    None => assert_true(false) // Should decompose successfully
  }
  
  // Test STL (Seasonal and Trend decomposition using Loess)
  let stl_result = TimeSeriesDecomposer::stl_decompose(decomposer, composite_data, 24)
  
  match stl_result {
    Some(decomposition) => {
      assert_eq(decomposition.trend.length(), 168)
      assert_eq(decomposition.seasonal.length(), 168)
      assert_eq(decomposition.residual.length(), 168)
      
      // STL should handle outliers better
      let residual_std = TimeSeriesDecomposer::calculate_std(decomposition.residual)
      assert_true(residual_std < 10.0) // Residuals should have reasonable variance
    }
    None => assert_true(false) // Should decompose successfully
  }
}

// Test 7: Time Series Correlation Analysis
test "time series correlation analysis" {
  let analyzer = TimeSeriesCorrelationAnalyzer::new()
  
  // Create correlated time series
  let base_timestamp = 1234567890L
  let series1 = []
  let series2 = []
  
  for i in 1..=100 {
    // Series 1: CPU usage
    let attrs1 = Attributes::new()
    Attributes::set(attrs1, "metric", StringValue("cpu_usage"))
    Attributes::set(attrs1, "host", StringValue("server-01"))
    
    let cpu_value = 50.0 + (Float::random() * 30.0)
    
    let data_point1 = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L),
      cpu_value,
      attrs1
    )
    
    series1.push(data_point1)
    
    // Series 2: Memory usage (correlated with CPU)
    let attrs2 = Attributes::new()
    Attributes::set(attrs2, "metric", StringValue("memory_usage"))
    Attributes::set(attrs2, "host", StringValue("server-01"))
    
    let memory_value = 100.0 + (cpu_value * 2.0) + (Float::random() * 20.0 - 10.0)
    
    let data_point2 = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L),
      memory_value,
      attrs2
    )
    
    series2.push(data_point2)
  }
  
  // Calculate correlation
  let correlation_result = TimeSeriesCorrelationAnalyzer::calculate_correlation(
    analyzer,
    series1,
    series2
  )
  
  match correlation_result {
    Some(correlation) => {
      assert_true(correlation.pearson > 0.7) // Strong positive correlation
      assert_true(correlation.spearman > 0.7) // Strong monotonic correlation
      assert_eq(correlation.lag, 0) // No lag between series
      assert_true(correlation.p_value < 0.01) // Statistically significant
    }
    None => assert_true(false) // Should calculate correlation
  }
  
  // Create series with lag
  let lagged_series = []
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("disk_io"))
    Attributes::set(attrs, "host", StringValue("server-01"))
    
    // Disk IO lags behind CPU usage by 5 minutes
    let cpu_value = if i > 5 { series1[i-6].value } else { 50.0 }
    let disk_io_value = cpu_value * 0.5 + (Float::random() * 10.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L),
      disk_io_value,
      attrs
    )
    
    lagged_series.push(data_point)
  }
  
  // Calculate correlation with lag
  let lagged_correlation_result = TimeSeriesCorrelationAnalyzer::calculate_correlation_with_lag(
    analyzer,
    series1,
    lagged_series,
    10 // Check lags up to 10 periods
  )
  
  match lagged_correlation_result {
    Some(correlation) => {
      assert_true(correlation.pearson > 0.5) // Moderate positive correlation
      assert_eq(correlation.lag, 5) // Should detect 5-period lag
      assert_true(correlation.p_value < 0.01) // Statistically significant
    }
    None => assert_true(false) // Should calculate correlation
  }
  
  // Create uncorrelated series
  let uncorrelated_series = []
  
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("network_latency"))
    Attributes::set(attrs, "host", StringValue("server-01"))
    
    let latency_value = 10.0 + (Float::random() * 20.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L),
      latency_value,
      attrs
    )
    
    uncorrelated_series.push(data_point)
  }
  
  // Calculate correlation between uncorrelated series
  let uncorrelated_result = TimeSeriesCorrelationAnalyzer::calculate_correlation(
    analyzer,
    series1,
    uncorrelated_series
  )
  
  match uncorrelated_result {
    Some(correlation) => {
      assert_true(correlation.pearson.abs() < 0.3) // Weak or no correlation
      assert_true(correlation.spearman.abs() < 0.3) // Weak or no monotonic correlation
      assert_true(correlation.p_value > 0.05) // Not statistically significant
    }
    None => assert_true(false) // Should calculate correlation
  }
  
  // Calculate correlation matrix for multiple series
  let all_series = [series1, series2, lagged_series, uncorrelated_series]
  let correlation_matrix = TimeSeriesCorrelationAnalyzer::calculate_correlation_matrix(
    analyzer,
    all_series
  )
  
  assert_eq(correlation_matrix.length(), 4)
  assert_eq(correlation_matrix[0].length(), 4) // 4x4 matrix
  
  // Verify matrix properties
  for i in 0..3 {
    assert_eq(correlation_matrix[i][i], 1.0) // Diagonal should be 1
    for j in 0..3 {
      assert_eq(correlation_matrix[i][j], correlation_matrix[j][i]) // Should be symmetric
    }
  }
}

// Test 8: Time Series Downsampling and Upsampling
test "time series downsampling and upsampling" {
  let processor = TimeSeriesProcessor::new()
  
  // Create high-frequency time series (every minute)
  let high_freq_data = []
  let base_timestamp = 1234567890L
  
  for i in 1..=1440 { // 24 hours * 60 minutes
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("temperature"))
    Attributes::set(attrs, "sensor", StringValue("temp-sensor-01"))
    
    let temp_value = 20.0 + 5.0 * Float::sin((i as Float) * 2.0 * 3.14159 / 720.0) + (Float::random() * 2.0 - 1.0)
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 60L), // Every minute
      temp_value,
      attrs
    )
    
    high_freq_data.push(data_point)
  }
  
  // Downsample to hourly data
  let hourly_data = TimeSeriesProcessor::downsample(
    processor,
    high_freq_data,
    3600L, // 1 hour
    Average
  )
  
  assert_eq(hourly_data.length(), 24) // 24 hours
  
  for i in 0..(hourly_data.length() - 1) {
    assert_eq(hourly_data[i].timestamp, base_timestamp + (i + 1) * 3600L)
    assert_true(hourly_data[i].value >= 15.0) // Reasonable temperature range
    assert_true(hourly_data[i].value <= 25.0)
  }
  
  // Test different downsampling methods
  let max_hourly_data = TimeSeriesProcessor::downsample(
    processor,
    high_freq_data,
    3600L, // 1 hour
    Max
  )
  
  let min_hourly_data = TimeSeriesProcessor::downsample(
    processor,
    high_freq_data,
    3600L, // 1 hour
    Min
  )
  
  let sum_hourly_data = TimeSeriesProcessor::downsample(
    processor,
    high_freq_data,
    3600L, // 1 hour
    Sum
  )
  
  // Verify downsampling results
  for i in 0..23 {
    assert_true(max_hourly_data[i].value >= hourly_data[i].value) // Max >= Average
    assert_true(min_hourly_data[i].value <= hourly_data[i].value) // Min <= Average
    assert_true(sum_hourly_data[i].value >= max_hourly_data[i].value) // Sum >= Max
  }
  
  // Upsample back to minute-level using linear interpolation
  let upsampled_data = TimeSeriesProcessor::upsample(
    processor,
    hourly_data,
    60L, // 1 minute
    Linear
  )
  
  assert_eq(upsampled_data.length(), 1440) // Should match original length
  
  // Verify upsampling preserves original pattern
  let original_sum = high_freq_data.map(d => d.value).sum()
  let upsampled_sum = upsampled_data.map(d => d.value).sum()
  
  let sum_error = (original_sum - upsampled_sum).abs() / original_sum
  assert_true(sum_error < 0.05) // Should be within 5% of original
  
  // Test different upsampling methods
  let upsampled_nearest = TimeSeriesProcessor::upsample(
    processor,
    hourly_data,
    60L, // 1 minute
    Nearest
  )
  
  let upsampled_cubic = TimeSeriesProcessor::upsample(
    processor,
    hourly_data,
    60L, // 1 minute
    Cubic
  )
  
  // Verify upsampling methods produce different results
  for i in 0..(upsampled_data.length() - 1) {
    if i % 60 != 0 { // Not at original hourly points
      assert_true(upsampled_nearest[i].value != upsampled_data[i].value) // Nearest should differ from Linear
      assert_true(upsampled_cubic[i].value != upsampled_data[i].value) // Cubic should differ from Linear
    }
  }
  
  // Test time series resampling with missing data
  let sparse_data = []
  
  for i in [1, 3, 6, 10, 15, 21, 28, 36, 45, 55] { // Irregular intervals
    let attrs = Attributes::new()
    Attributes::set(attrs, "metric", StringValue("irregular_metric"))
    Attributes::set(attrs, "sensor", StringValue("irregular-sensor"))
    
    let data_point = TimeSeriesDataPoint::new(
      base_timestamp + (i * 3600L), // Irregular hours
      (i as Float) * 2.0,
      attrs
    )
    
    sparse_data.push(data_point)
  }
  
  // Resample to regular hourly intervals using interpolation
  let regular_data = TimeSeriesProcessor::resample(
    processor,
    sparse_data,
    3600L, // 1 hour
    Linear
  )
  
  assert_eq(regular_data.length(), 55) // From 1 to 55 hours
  
  // Verify interpolated values are reasonable
  for i in 0..(regular_data.length() - 1) {
    assert_true(regular_data[i].value > 0.0) // Should be positive
    assert_eq(regular_data[i].timestamp, base_timestamp + (i + 1) * 3600L) // Regular intervals
  }
}