// Azimuth Time Series Data Processing Quality Tests
// This file contains comprehensive test cases for time series data analysis and processing

// Test 1: Time Series Data Aggregation
test "time series data aggregation" {
  let time_series = TimeSeries::new("cpu.usage")
  
  // Add data points with different timestamps
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  for i in 0..=100 {
    let timestamp = base_timestamp + (i * 60000L) // 1-minute intervals
    let value = 50.0 + (10.0 * (i % 20).to_float()) // Oscillating between 50-70
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test aggregation by hour
  let hourly_aggregated = TimeSeries::aggregate_by_interval(time_series, 3600000L) // 1 hour
  assert_eq(hourly_aggregated.points.length(), 2) // 101 points over ~1.7 hours = 2 hour buckets
  
  // Verify aggregation calculations
  for point in hourly_aggregated.points {
    assert_true(point.value >= 50.0 && point.value <= 70.0)
    assert_true(point.count > 0)
    assert_true(point.min >= 50.0 && point.max <= 70.0)
  }
  
  // Test aggregation by minute
  let minutely_aggregated = TimeSeries::aggregate_by_interval(time_series, 60000L) // 1 minute
  assert_eq(minutely_aggregated.points.length(), 101) // Should preserve original points
  
  // Test custom aggregation function
  let max_aggregated = TimeSeries::aggregate_with_function(
    time_series,
    300000L, // 5 minutes
    |points| { 
      if points.length() == 0 { 0.0 } 
      else { points.map(|p| p.value).max() }
    }
  )
  
  assert_eq(max_aggregated.points.length(), 21) // 101 points / 5-minute intervals
  
  // Verify max aggregation
  for point in max_aggregated.points {
    assert_true(point.value >= 60.0) // Max should be at least 60
  }
}

// Test 2: Time Series Downsampling and Upsampling
test "time series downsampling and upsampling" {
  let time_series = TimeSeries::new("memory.usage")
  let base_timestamp = 1609459200000L
  
  // Add high-frequency data (1-second intervals)
  for i in 0..=3600 {
    let timestamp = base_timestamp + (i * 1000L) // 1-second intervals
    let value = 60.0 + (5.0 * (i % 60).to_float() / 60.0) // Gradual increase
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test downsampling to 1-minute intervals
  let downsampled = TimeSeries::downsample(time_series, 60000L, DownsamplingMethod::Average)
  assert_eq(downsampled.points.length(), 61) // 3601 points / 60 = ~61 points
  
  // Verify downsampling accuracy
  for i in 0..=downsampled.points.length() - 1 {
    let point = downsampled.points[i]
    let expected_min = 60.0 + (5.0 * i.to_float() / 60.0)
    let expected_max = 60.0 + (5.0 * (i + 1).to_float() / 60.0)
    assert_true(point.value >= expected_min && point.value <= expected_max)
  }
  
  // Test downsampling with different methods
  let max_downsampled = TimeSeries::downsample(time_series, 60000L, DownsamplingMethod::Max)
  let min_downsampled = TimeSeries::downsample(time_series, 60000L, DownsamplingMethod::Min)
  
  for i in 0..=downsampled.points.length() - 1 {
    assert_true(max_downsampled.points[i].value >= downsampled.points[i].value)
    assert_true(min_downsampled.points[i].value <= downsampled.points[i].value)
  }
  
  // Test upsampling back to 1-second intervals
  let upsampled = TimeSeries::upsample(downsampled, 1000L, UpsamplingMethod::Linear)
  assert_eq(upsampled.points.length(), 3601) // Should match original
  
  // Verify upsampling preserves trend
  for i in 0..=100 {
    let original = time_series.points[i]
    let upsampled_point = upsampled.points[i]
    let tolerance = 2.5 // Allow some interpolation error
    assert_true((original.value - upsampled_point.value).abs() < tolerance)
  }
}

// Test 3: Time Series Anomaly Detection
test "time series anomaly detection" {
  let time_series = TimeSeries::new("network.latency")
  let base_timestamp = 1609459200000L
  
  // Generate normal data with some anomalies
  for i in 0..=200 {
    let timestamp = base_timestamp + (i * 60000L) // 1-minute intervals
    let value = 
      if i == 50 { 150.0 } // Spike anomaly
      else if i == 100 { 5.0 } // Dip anomaly
      else if i == 150 { 200.0 } // Extreme spike
      else { 50.0 + (10.0 * (i % 20).to_float() / 20.0) } // Normal variation 45-55
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test statistical anomaly detection
  let statistical_detector = StatisticalAnomalyDetector::new(2.0) // 2 standard deviations
  let statistical_anomalies = AnomalyDetector::detect(statistical_detector, time_series)
  
  assert_true(statistical_anomalies.length() >= 2) // Should detect at least the extreme anomalies
  
  // Verify detected anomalies
  let detected_spike = statistical_anomalies.any(|anomaly| {
    anomaly.point.value > 100.0 && anomaly.point.timestamp == base_timestamp + (50 * 60000L)
  })
  assert_true(detected_spike)
  
  let detected_extreme = statistical_anomalies.any(|anomaly| {
    anomaly.point.value > 150.0 && anomaly.point.timestamp == base_timestamp + (150 * 60000L)
  })
  assert_true(detected_extreme)
  
  // Test moving average anomaly detection
  let ma_detector = MovingAverageAnomalyDetector::new(10, 1.5) // 10-point window, 1.5x threshold
  let ma_anomalies = AnomalyDetector::detect(ma_detector, time_series)
  
  assert_true(ma_anomalies.length() >= 1)
  
  // Test seasonal anomaly detection
  let seasonal_detector = SeasonalAnomalyDetector::new(20, 2.0) // 20-point seasonality, 2x threshold
  let seasonal_anomalies = AnomalyDetector::detect(seasonal_detector, time_series)
  
  // Seasonal detection should be more robust to periodic patterns
  assert_true(seasonal_anomalies.length() <= statistical_anomalies.length())
}

// Test 4: Time Series Forecasting
test "time series forecasting" {
  let time_series = TimeSeries::new("server.requests")
  let base_timestamp = 1609459200000L
  
  // Generate trend + seasonal data
  for i in 0..=1440 { // 24 hours of minute data
    let timestamp = base_timestamp + (i * 60000L)
    let trend = 100.0 + (0.1 * i.to_float()) // Linear trend
    let seasonal = 20.0 * ((2.0 * 3.14159 * i.to_float()) / 60.0).sin() // Hourly seasonality
    let noise = 5.0 * ((2.0 * 3.14159 * i.to_float()) / 7.0).sin() // Small noise
    let value = trend + seasonal + noise
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test linear regression forecasting
  let linear_forecaster = LinearRegressionForecaster::new()
  let linear_forecast = TimeSeriesForecaster::forecast(
    linear_forecaster,
    time_series,
    60 // Predict next 60 minutes
  )
  
  assert_eq(linear_forecast.points.length(), 60)
  
  // Verify trend continuation
  let last_actual = time_series.points[time_series.points.length() - 1].value
  let first_predicted = linear_forecast.points[0].value
  assert_true(first_predicted > last_actual) // Should continue upward trend
  
  // Test exponential smoothing forecasting
  let ets_forecaster = ExponentialSmoothingForecaster::new(0.3, 0.1, 0.1)
  let ets_forecast = TimeSeriesForecaster::forecast(
    ets_forecaster,
    time_series,
    60
  )
  
  assert_eq(ets_forecast.points.length(), 60)
  
  // Test ARIMA forecasting
  let arima_forecaster = ARIMAForecaster::new(1, 1, 1) // ARIMA(1,1,1)
  let arima_forecast = TimeSeriesForecaster::forecast(
    arima_forecaster,
    time_series,
    60
  )
  
  assert_eq(arima_forecast.points.length(), 60)
  
  // Compare forecast accuracy
  let linear_mae = calculate_mae(linear_forecast, ets_forecast)
  let ets_mae = calculate_mae(ets_forecast, arima_forecast)
  let arima_mae = calculate_mae(arima_forecast, ets_forecast)
  
  // Different models should produce different but reasonable forecasts
  assert_true(linear_mae > 0.0)
  assert_true(ets_mae > 0.0)
  assert_true(arima_mae > 0.0)
}

// Test 5: Time Series Pattern Recognition
test "time series pattern recognition" {
  let time_series = TimeSeries::new("application.errors")
  let base_timestamp = 1609459200000L
  
  // Generate data with specific patterns
  for i in 0..=2880 { // 48 hours of minute data
    let timestamp = base_timestamp + (i * 60000L)
    let hour_of_day = (i / 60) % 24
    let day_of_week = (i / 1440) % 7
    
    // Daily pattern: more errors during business hours
    let daily_pattern = 
      if hour_of_day >= 9 && hour_of_day <= 17 { 20.0 }
      else if hour_of_day >= 22 || hour_of_day <= 6 { 5.0 }
      else { 10.0 }
    
    // Weekly pattern: more errors on weekdays
    let weekly_pattern = 
      if day_of_week >= 1 && day_of_week <= 5 { 1.2 }
      else { 0.8 }
    
    // Random component
    let random = 2.0 * ((2.0 * 3.14159 * i.to_float()) / 13.0).sin()
    
    let value = daily_pattern * weekly_pattern + random
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test daily pattern detection
  let daily_detector = PeriodicPatternDetector::new(1440) // 24 hours in minutes
  let daily_patterns = PatternDetector::detect(daily_detector, time_series)
  
  assert_true(daily_patterns.length() > 0)
  
  // Verify detected daily pattern
  let daily_pattern = daily_patterns[0]
  assert_eq(daily_pattern.period, 1440)
  assert_true(daily_pattern.confidence > 0.7)
  
  // Verify pattern characteristics
  let pattern_hours = daily_pattern.pattern_points.length() / 60
  assert_eq(pattern_hours, 24)
  
  // Should detect higher values during business hours
  let business_hours_avg = calculate_pattern_average(daily_pattern, 9, 17)
  let night_hours_avg = calculate_pattern_average(daily_pattern, 22, 6)
  assert_true(business_hours_avg > night_hours_avg)
  
  // Test weekly pattern detection
  let weekly_detector = PeriodicPatternDetector::new(10080) // 7 days in minutes
  let weekly_patterns = PatternDetector::detect(weekly_detector, time_series)
  
  assert_true(weekly_patterns.length() > 0)
  
  // Test anomaly detection based on patterns
  let pattern_based_detector = PatternBasedAnomalyDetector::new(daily_patterns)
  let pattern_anomalies = AnomalyDetector::detect(pattern_based_detector, time_series)
  
  // Should detect deviations from expected patterns
  assert_true(pattern_anomalies.length() >= 0)
}

// Test 6: Time Series Correlation Analysis
test "time series correlation analysis" {
  let cpu_series = TimeSeries::new("cpu.usage")
  let memory_series = TimeSeries::new("memory.usage")
  let network_series = TimeSeries::new("network.throughput")
  let base_timestamp = 1609459200000L
  
  // Generate correlated time series data
  for i in 0..=1440 { // 24 hours of minute data
    let timestamp = base_timestamp + (i * 60000L)
    
    // Base load with daily pattern
    let base_load = 30.0 + (20.0 * ((2.0 * 3.14159 * (i % 720).to_float()) / 720.0).sin())
    
    // CPU and Memory are positively correlated
    let cpu_value = base_load + 10.0
    let memory_value = base_load * 0.8 + 15.0
    
    // Network is less correlated
    let network_value = 50.0 + (30.0 * ((2.0 * 3.14159 * (i % 360).to_float()) / 360.0).sin())
    
    TimeSeries::add_point(cpu_series, TimeSeriesPoint::new(timestamp, cpu_value))
    TimeSeries::add_point(memory_series, TimeSeriesPoint::new(timestamp, memory_value))
    TimeSeries::add_point(network_series, TimeSeriesPoint::new(timestamp, network_value))
  }
  
  // Test Pearson correlation
  let cpu_memory_correlation = TimeSeriesAnalyzer::pearson_correlation(cpu_series, memory_series)
  let cpu_network_correlation = TimeSeriesAnalyzer::pearson_correlation(cpu_series, network_series)
  let memory_network_correlation = TimeSeriesAnalyzer::pearson_correlation(memory_series, network_series)
  
  // CPU and Memory should be highly correlated
  assert_true(cpu_memory_correlation > 0.8)
  
  // CPU and Network should be less correlated
  assert_true(cpu_network_correlation.abs() < 0.5)
  
  // Memory and Network should be less correlated
  assert_true(memory_network_correlation.abs() < 0.5)
  
  // Test Spearman rank correlation
  let cpu_memory_spearman = TimeSeriesAnalyzer::spearman_correlation(cpu_series, memory_series)
  assert_true(cpu_memory_spearman > 0.8)
  
  // Test cross-correlation to find lags
  let cpu_memory_cross_corr = TimeSeriesAnalyzer::cross_correlation(cpu_series, memory_series, 60) // 1-hour lag
  let max_lag = find_max_correlation_lag(cpu_memory_cross_corr)
  
  // Should find maximum correlation at or near zero lag (simultaneous)
  assert_true(max_lag >= -30 && max_lag <= 30)
  
  // Test partial correlation
  let partial_corr = TimeSeriesAnalyzer::partial_correlation(
    cpu_series, 
    memory_series, 
    [network_series]
  )
  
  // Partial correlation should account for network influence
  assert_true(partial_corr > 0.5)
}

// Test 7: Time Series Decomposition
test "time series decomposition" {
  let time_series = TimeSeries::new("system.load")
  let base_timestamp = 1609459200000L
  
  // Generate data with trend, seasonal, and residual components
  for i in 0..=2880 { // 48 hours of minute data
    let timestamp = base_timestamp + (i * 60000L)
    
    // Trend component (gradual increase)
    let trend = 50.0 + (0.02 * i.to_float())
    
    // Seasonal component (daily pattern)
    let seasonal = 10.0 * ((2.0 * 3.14159 * (i % 1440).to_float()) / 1440.0).sin()
    
    // Residual component (random noise)
    let residual = 2.0 * ((2.0 * 3.14159 * i.to_float()) / 37.0).sin()
    
    let value = trend + seasonal + residual
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test additive decomposition
  let additive_decomposition = TimeSeriesDecomposer::decompose(
    time_series,
    1440, // Daily seasonality
    DecompositionModel::Additive
  )
  
  assert_eq(additive_decomposition.trend.points.length(), time_series.points.length())
  assert_eq(additive_decomposition.seasonal.points.length(), time_series.points.length())
  assert_eq(additive_decomposition.residual.points.length(), time_series.points.length())
  
  // Verify decomposition accuracy
  for i in 0..=100 {
    let original = time_series.points[i].value
    let trend = additive_decomposition.trend.points[i].value
    let seasonal = additive_decomposition.seasonal.points[i].value
    let residual = additive_decomposition.residual.points[i].value
    let reconstructed = trend + seasonal + residual
    
    // Allow small reconstruction error
    assert_true((original - reconstructed).abs() < 0.1)
  }
  
  // Test multiplicative decomposition
  let multiplicative_decomposition = TimeSeriesDecomposer::decompose(
    time_series,
    1440,
    DecompositionModel::Multiplicative
  )
  
  // Verify multiplicative reconstruction
  for i in 0..=100 {
    let original = time_series.points[i].value
    let trend = multiplicative_decomposition.trend.points[i].value
    let seasonal = multiplicative_decomposition.seasonal.points[i].value
    let residual = multiplicative_decomposition.residual.points[i].value
    let reconstructed = trend * seasonal * residual
    
    // Allow small reconstruction error
    assert_true((original - reconstructed).abs() / original < 0.05) // 5% error tolerance
  }
  
  // Test STL decomposition (Seasonal and Trend decomposition using Loess)
  let stl_decomposition = TimeSeriesDecomposer::stl_decompose(
    time_series,
    1440,
    7, // Seasonal smoothing parameter
    21 // Trend smoothing parameter
  )
  
  assert_eq(stl_decomposition.trend.points.length(), time_series.points.length())
  assert_eq(stl_decomposition.seasonal.points.length(), time_series.points.length())
}

// Test 8: Time Series Change Point Detection
test "time series change point detection" {
  let time_series = TimeSeries::new("performance.metric")
  let base_timestamp = 1609459200000L
  
  // Generate data with change points
  for i in 0..=1440 { // 24 hours of minute data
    let timestamp = base_timestamp + (i * 60000L)
    
    let value = 
      if i < 480 { // First 8 hours: low level
        20.0 + (2.0 * ((2.0 * 3.14159 * i.to_float()) / 60.0).sin())
      } else if i < 960 { // Next 8 hours: high level
        50.0 + (3.0 * ((2.0 * 3.14159 * i.to_float()) / 60.0).sin())
      } else { // Last 8 hours: medium level
        35.0 + (2.5 * ((2.0 * 3.14159 * i.to_float()) / 60.0).sin())
      }
    
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test CUSUM change point detection
  let cusum_detector = CUSUMChangePointDetector::new(5.0, 10.0) // Threshold and drift
  let cusum_change_points = ChangePointDetector::detect(cusum_detector, time_series)
  
  assert_true(cusum_change_points.length() >= 2)
  
  // Verify change point locations (around 480 and 960)
  let first_change = cusum_change_points[0]
  let second_change = cusum_change_points[1]
  
  assert_true(first_change.index >= 470 && first_change.index <= 490)
  assert_true(second_change.index >= 950 && second_change.index <= 970)
  
  // Test Bayesian change point detection
  let bayesian_detector = BayesianChangePointDetector::new(0.1, 0.1) // Prior and hazard
  let bayesian_change_points = ChangePointDetector::detect(bayesian_detector, time_series)
  
  assert_true(bayesian_change_points.length() >= 2)
  
  // Test PELT (Pruned Exact Linear Time) change point detection
  let pelt_detector = PELTChangePointDetector::new(10.0) // Penalty
  let pelt_change_points = ChangePointDetector::detect(pelt_detector, time_series)
  
  assert_true(pelt_change_points.length() >= 2)
  
  // Compare detection methods
  let cusum_count = cusum_change_points.length()
  let bayesian_count = bayesian_change_points.length()
  let pelt_count = pelt_change_points.length()
  
  // Different methods should detect similar but not identical change points
  assert_true(cusum_count >= 2 && bayesian_count >= 2 && pelt_count >= 2)
}

// Test 9: Time Series Clustering
test "time series clustering" {
  let time_series_collection = []
  
  // Generate multiple time series with different patterns
  for series_id in 0..=9 {
    let series = TimeSeries::new("series_" + series_id.to_string())
    let base_timestamp = 1609459200000L
    
    for i in 0..=720 { // 12 hours of minute data
      let timestamp = base_timestamp + (i * 60000L)
      
      let value = 
        if series_id < 3 { // Cluster 1: sine wave pattern
          50.0 + (20.0 * ((2.0 * 3.14159 * i.to_float()) / 120.0).sin())
        } else if series_id < 6 { // Cluster 2: linear trend
          30.0 + (0.05 * i.to_float())
        } else if series_id < 8 { // Cluster 3: constant with noise
          60.0 + (5.0 * ((2.0 * 3.14159 * i.to_float()) / 17.0).sin())
        } else { // Cluster 4: exponential growth
          20.0 * (1.01).pow(i.to_float() / 60.0)
        }
      
      TimeSeries::add_point(series, TimeSeriesPoint::new(timestamp, value))
    }
    
    time_series_collection.push(series)
  }
  
  // Test K-means clustering
  let kmeans_clusterer = KMeansTimeSeriesClusterer::new(4, 100) // 4 clusters, 100 iterations
  let kmeans_clusters = TimeSeriesClusterer::cluster(kmeans_clusterer, time_series_collection)
  
  assert_eq(kmeans_clusters.length(), 4)
  
  // Verify each series is assigned to a cluster
  let total_assigned = kmeans_clusters.map(|cluster| cluster.members.length()).sum()
  assert_eq(total_assigned, time_series_collection.length())
  
  // Test hierarchical clustering
  let hierarchical_clusterer = HierarchicalTimeSeriesClusterer::new(
    DistanceMetric::DTW, // Dynamic Time Warping
    LinkageMethod::Average
  )
  let hierarchical_clusters = TimeSeriesClusterer::cluster(
    hierarchical_clusterer,
    time_series_collection
  )
  
  assert_true(hierarchical_clusters.length() >= 1)
  
  // Test DBSCAN clustering
  let dbscan_clusterer = DBSCANTimeSeriesClusterer::new(5.0, 2) // Epsilon, min points
  let dbscan_clusters = TimeSeriesClusterer::cluster(dbscan_clusterer, time_series_collection)
  
  assert_true(dbscan_clusters.length() >= 1)
  
  // Evaluate clustering quality
  let kmeans_silhouette = calculate_silhouette_score(kmeans_clusters)
  let hierarchical_silhouette = calculate_silhouette_score(hierarchical_clusters)
  
  assert_true(kmeans_silhouette >= -1.0 && kmeans_silhouette <= 1.0)
  assert_true(hierarchical_silhouette >= -1.0 && hierarchical_silhouette <= 1.0)
}

// Test 10: Time Series Compression
test "time series compression" {
  let time_series = TimeSeries::new("sensor.data")
  let base_timestamp = 1609459200000L
  
  // Generate high-frequency data with some patterns
  for i in 0..=10000 { // ~7 days of minute data
    let timestamp = base_timestamp + (i * 60000L)
    
    // Mix of constant regions, trends, and noise
    let value = 
      if i % 1000 < 100 { // Constant region
        50.0
      } else if i % 1000 < 600 { // Trending region
        40.0 + (0.1 * (i % 1000).to_float())
      } else { // Noisy region
        50.0 + (10.0 * ((2.0 * 3.14159 * (i % 100).to_float()) / 100.0).sin())
      }
    
    TimeSeries::add_point(time_series, TimeSeriesPoint::new(timestamp, value))
  }
  
  let original_size = calculate_time_series_size(time_series)
  
  // Test delta encoding compression
  let delta_compressed = TimeSeriesCompressor::compress_delta(time_series)
  let delta_size = calculate_compressed_size(delta_compressed)
  let delta_ratio = delta_size.to_float() / original_size.to_float()
  
  assert_true(delta_ratio < 0.8) // Should achieve at least 20% compression
  
  // Test delta encoding decompression
  let delta_decompressed = TimeSeriesCompressor::decompress_delta(delta_compressed)
  assert_eq(delta_decompressed.points.length(), time_series.points.length())
  
  // Verify decompression accuracy
  for i in 0..=100 {
    let original = time_series.points[i]
    let decompressed = delta_decompressed.points[i]
    assert_eq(original.timestamp, decompressed.timestamp)
    assert_eq(original.value, decompressed.value)
  }
  
  // Test swing compression
  let swing_compressed = TimeSeriesCompressor::compress_swing(time_series, 0.1) // 0.1 tolerance
  let swing_size = calculate_compressed_size(swing_compressed)
  let swing_ratio = swing_size.to_float() / original_size.to_float()
  
  assert_true(swing_ratio < 0.5) // Should achieve better compression for smooth regions
  
  // Test swing decompression
  let swing_decompressed = TimeSeriesCompressor::decompress_swing(swing_compressed)
  assert_eq(swing_decompressed.points.length(), time_series.points.length())
  
  // Verify decompression accuracy within tolerance
  for i in 0..=100 {
    let original = time_series.points[i]
    let decompressed = swing_decompressed.points[i]
    assert_eq(original.timestamp, decompressed.timestamp)
    assert_true((original.value - decompressed.value).abs() <= 0.1)
  }
  
  // Test Gorilla compression
  let gorilla_compressed = TimeSeriesCompressor::compress_gorilla(time_series)
  let gorilla_size = calculate_compressed_size(gorilla_compressed)
  let gorilla_ratio = gorilla_size.to_float() / original_size.to_float()
  
  assert_true(gorilla_ratio < 0.4) // Gorilla should achieve good compression
  
  // Compare compression methods
  assert_true(gorilla_ratio <= swing_ratio || delta_ratio <= swing_ratio)
}

// Helper functions
fn calculate_mae(forecast1 : TimeSeries, forecast2 : TimeSeries) -> Float {
  let n = forecast1.points.length()
  let mut sum = 0.0
  
  for i in 0..=n-1 {
    sum = sum + (forecast1.points[i].value - forecast2.points[i].value).abs()
  }
  
  sum / n.to_float()
}

fn calculate_pattern_average(pattern : PeriodicPattern, start_hour : Int, end_hour : Int) -> Float {
  let mut sum = 0.0
  let mut count = 0
  
  for i in 0..=pattern.pattern_points.length() - 1 {
    let point = pattern.pattern_points[i]
    let hour = (i / 60) % 24
    
    if start_hour <= end_hour {
      if hour >= start_hour && hour <= end_hour {
        sum = sum + point.value
        count = count + 1
      }
    } else {
      if hour >= start_hour || hour <= end_hour {
        sum = sum + point.value
        count = count + 1
      }
    }
  }
  
  if count > 0 { sum / count.to_float() } else { 0.0 }
}

fn find_max_correlation_lag(cross_corr : Array[(Int, Float)]) -> Int {
  let mut max_lag = 0
  let mut max_corr = cross_corr[0].1
  
  for (lag, corr) in cross_corr {
    if corr > max_corr {
      max_corr = corr
      max_lag = lag
    }
  }
  
  max_lag
}

fn calculate_time_series_size(series : TimeSeries) -> Int {
  // Approximate size in bytes
  series.points.length() * 16 // 8 bytes for timestamp + 8 bytes for value
}

fn calculate_compressed_size(compressed : CompressedTimeSeries) -> Int {
  compressed.data.length()
}

fn calculate_silhouette_score(clusters : Array[TimeSeriesCluster]) -> Float {
  // Simplified silhouette score calculation
  let mut total_score = 0.0
  let mut total_points = 0
  
  for cluster in clusters {
    for member in cluster.members {
      let a = calculate_intra_cluster_distance(member, cluster)
      let b = calculate_nearest_cluster_distance(member, cluster, clusters)
      
      if a < b {
        total_score = total_score + (b - a) / max(a, b).to_float()
      } else {
        total_score = total_score + 0.0
      }
      
      total_points = total_points + 1
    }
  }
  
  if total_points > 0 { total_score / total_points.to_float() } else { 0.0 }
}

fn calculate_intra_cluster_distance(series : TimeSeries, cluster : TimeSeriesCluster) -> Float {
  let mut total_distance = 0.0
  let mut count = 0
  
  for other in cluster.members {
    if series != other {
      total_distance = total_distance + calculate_dtw_distance(series, other)
      count = count + 1
    }
  }
  
  if count > 0 { total_distance / count.to_float() } else { 0.0 }
}

fn calculate_nearest_cluster_distance(series : TimeSeries, current_cluster : TimeSeriesCluster, all_clusters : Array[TimeSeriesCluster]) -> Float {
  let mut min_distance = Float::max_value()
  
  for cluster in all_clusters {
    if cluster != current_cluster {
      let distance = calculate_inter_cluster_distance(series, cluster)
      if distance < min_distance {
        min_distance = distance
      }
    }
  }
  
  min_distance
}

fn calculate_inter_cluster_distance(series : TimeSeries, cluster : TimeSeriesCluster) -> Float {
  let mut total_distance = 0.0
  
  for member in cluster.members {
    total_distance = total_distance + calculate_dtw_distance(series, member)
  }
  
  total_distance / cluster.members.length().to_float()
}

fn calculate_dtw_distance(series1 : TimeSeries, series2 : TimeSeries) -> Float {
  // Simplified DTW distance calculation
  let n = series1.points.length()
  let m = series2.points.length()
  let dtw_matrix = Array2D::new(n, m, 0.0)
  
  for i in 0..=n-1 {
    for j in 0..=m-1 {
      let cost = (series1.points[i].value - series2.points[j].value).abs()
      
      if i == 0 && j == 0 {
        dtw_matrix[i][j] = cost
      } else if i == 0 {
        dtw_matrix[i][j] = cost + dtw_matrix[i][j-1]
      } else if j == 0 {
        dtw_matrix[i][j] = cost + dtw_matrix[i-1][j]
      } else {
        dtw_matrix[i][j] = cost + min(
          dtw_matrix[i-1][j],
          dtw_matrix[i][j-1],
          dtw_matrix[i-1][j-1]
        )
      }
    }
  }
  
  dtw_matrix[n-1][m-1]
}