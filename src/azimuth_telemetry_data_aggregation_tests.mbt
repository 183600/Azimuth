// Azimuth Telemetry System - Data Aggregation Tests
// This file contains test cases for telemetry data aggregation and analysis

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation functionality" {
  // Create a telemetry data aggregator
  let aggregator = TelemetryAggregator::new()
  
  // Add multiple data points
  TelemetryAggregator::add_data_point(aggregator, 100.0)
  TelemetryAggregator::add_data_point(aggregator, 200.0)
  TelemetryAggregator::add_data_point(aggregator, 150.0)
  TelemetryAggregator::add_data_point(aggregator, 300.0)
  TelemetryAggregator::add_data_point(aggregator, 250.0)
  
  // Test aggregation results
  let stats = TelemetryAggregator::calculate_statistics(aggregator)
  assert_eq(TelemetryStats::count(stats), 5)
  assert_eq(TelemetryStats::sum(stats), 1000.0)
  assert_eq(TelemetryStats::average(stats), 200.0)
  assert_eq(TelemetryStats::min(stats), 100.0)
  assert_eq(TelemetryStats::max(stats), 300.0)
  
  // Test percentile calculations
  assert_eq(TelemetryStats::percentile(stats, 50), 200.0) // Median
  assert_eq(TelemetryStats::percentile(stats, 90), 300.0) // 90th percentile
}

// Test 2: Time-series Data Analysis
test "time-series data analysis functionality" {
  // Create a time-series analyzer
  let analyzer = TimeSeriesAnalyzer::new()
  
  // Add time-series data points
  let timestamp1 = 1609459200L // 2021-01-01 00:00:00
  let timestamp2 = 1609459260L // 2021-01-01 00:01:00
  let timestamp3 = 1609459320L // 2021-01-01 00:02:00
  
  TimeSeriesAnalyzer::add_point(analyzer, timestamp1, 100.0)
  TimeSeriesAnalyzer::add_point(analyzer, timestamp2, 150.0)
  TimeSeriesAnalyzer::add_point(analyzer, timestamp3, 200.0)
  
  // Test trend analysis
  let trend = TimeSeriesAnalyzer::calculate_trend(analyzer)
  match trend {
    Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test rate of change
  let rate = TimeSeriesAnalyzer::calculate_rate_of_change(analyzer)
  assert_true(rate > 0.0) // Should be positive for increasing trend
  
  // Test anomaly detection
  // Add an anomaly point
  TimeSeriesAnalyzer::add_point(analyzer, 1609459380L, 500.0) // Much higher than expected
  let anomalies = TimeSeriesAnalyzer::detect_anomalies(analyzer, 2.0) // 2 standard deviations
  assert_eq(anomalies.length(), 1)
}