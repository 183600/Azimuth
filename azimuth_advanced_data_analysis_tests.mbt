// Advanced Data Analysis Tests for Azimuth Telemetry System
// This file contains test cases for advanced data analysis functionality

// Test 1: Statistical Analysis Functions
test "statistical analysis functions" {
  let data_points = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  
  // Test mean calculation
  let mean_value = DataAnalysis::calculate_mean(data_points)
  assert_eq(mean_value, 5.5)
  
  // Test median calculation
  let median_value = DataAnalysis::calculate_median(data_points)
  assert_eq(median_value, 5.5)
  
  // Test standard deviation
  let std_dev = DataAnalysis::calculate_std_deviation(data_points)
  assert_true(std_dev > 3.0 && std_dev < 3.1)
  
  // Test variance
  let variance = DataAnalysis::calculate_variance(data_points)
  assert_true(variance > 8.0 && variance < 9.0)
}

// Test 2: Time Series Analysis
test "time series analysis" {
  let time_series_data = [
    (1000L, 10.0),
    (2000L, 15.0),
    (3000L, 12.0),
    (4000L, 18.0),
    (5000L, 22.0)
  ]
  
  // Test trend analysis
  let trend = TimeSeriesAnalysis::calculate_trend(time_series_data)
  assert_true(trend > 0.0) // Positive trend
  
  // Test moving average
  let moving_avg = TimeSeriesAnalysis::calculate_moving_average(time_series_data, 3)
  assert_eq(moving_avg.length(), 3)
  
  // Test seasonal decomposition
  let seasonal_result = TimeSeriesAnalysis::seasonal_decomposition(time_series_data)
  match seasonal_result {
    Some(result) => {
      assert_true(result.trend.length() > 0)
      assert_true(result.seasonal.length() > 0)
      assert_true(result.residual.length() > 0)
    }
    None => assert_true(false)
  }
}

// Test 3: Anomaly Detection
test "anomaly detection algorithms" {
  let normal_data = [10.0, 12.0, 11.0, 13.0, 9.0, 14.0, 10.0, 12.0, 11.0, 13.0]
  let anomalous_data = [10.0, 12.0, 11.0, 50.0, 9.0, 14.0, 10.0, 12.0, 11.0, 13.0]
  
  // Test z-score anomaly detection
  let normal_z_scores = AnomalyDetection::z_score_analysis(normal_data)
  let anomalous_z_scores = AnomalyDetection::z_score_analysis(anomalous_data)
  
  assert_true(normal_z_scores.max() < 3.0)
  assert_true(anomalous_z_scores.max() > 3.0)
  
  // Test isolation forest anomaly detection
  let normal_if_scores = AnomalyDetection::isolation_forest(normal_data)
  let anomalous_if_scores = AnomalyDetection::isolation_forest(anomalous_data)
  
  assert_true(normal_if_scores.average() < anomalous_if_scores.average())
  
  // Test DBSCAN clustering for anomaly detection
  let clusters = AnomalyDetection::dbscan_clustering(anomalous_data, 2.0, 2)
  assert_true(clusters.length() >= 1)
}

// Test 4: Correlation Analysis
test "correlation analysis" {
  let data_series_1 = [1.0, 2.0, 3.0, 4.0, 5.0]
  let data_series_2 = [2.0, 4.0, 6.0, 8.0, 10.0] // Perfect positive correlation
  let data_series_3 = [5.0, 4.0, 3.0, 2.0, 1.0] // Perfect negative correlation
  let data_series_4 = [3.0, 1.0, 4.0, 1.0, 5.0] // Low correlation
  
  // Test Pearson correlation
  let pos_correlation = CorrelationAnalysis::pearson_correlation(data_series_1, data_series_2)
  assert_true(pos_correlation > 0.99)
  
  let neg_correlation = CorrelationAnalysis::pearson_correlation(data_series_1, data_series_3)
  assert_true(neg_correlation < -0.99)
  
  let low_correlation = CorrelationAnalysis::pearson_correlation(data_series_1, data_series_4)
  assert_true(low_correlation > -0.5 && low_correlation < 0.5)
  
  // Test Spearman correlation
  let spearman_pos = CorrelationAnalysis::spearman_correlation(data_series_1, data_series_2)
  assert_true(spearman_pos > 0.99)
}

// Test 5: Pattern Recognition
test "pattern recognition algorithms" {
  let pattern_data = [
    [1.0, 2.0, 3.0, 4.0, 5.0],
    [2.0, 3.0, 4.0, 5.0, 6.0],
    [1.1, 2.1, 3.1, 4.1, 5.1],
    [10.0, 20.0, 30.0, 40.0, 50.0]
  ]
  
  // Test pattern similarity
  let similarity_1 = PatternRecognition::calculate_similarity(pattern_data[0], pattern_data[1])
  let similarity_2 = PatternRecognition::calculate_similarity(pattern_data[0], pattern_data[2])
  let similarity_3 = PatternRecognition::calculate_similarity(pattern_data[0], pattern_data[3])
  
  assert_true(similarity_2 > similarity_1)
  assert_true(similarity_1 > similarity_3)
  
  // Test pattern clustering
  let clusters = PatternRecognition::cluster_patterns(pattern_data, 2)
  assert_eq(clusters.length(), 2)
  
  // Test sequence pattern detection
  let sequence = [1.0, 2.0, 3.0, 2.0, 3.0, 4.0, 3.0, 4.0, 5.0]
  let patterns = PatternRecognition::detect_repeating_patterns(sequence, 3)
  assert_true(patterns.length() > 0)
}

// Test 6: Data Aggregation and Grouping
test "data aggregation and grouping" {
  let raw_data = [
    ("group1", 10.0),
    ("group2", 20.0),
    ("group1", 15.0),
    ("group3", 30.0),
    ("group2", 25.0),
    ("group1", 12.0)
  ]
  
  // Test grouping by key
  let grouped_data = DataAggregation::group_by(raw_data, lambda { (key, _) => key })
  assert_eq(grouped_data.length(), 3)
  assert_true(grouped_data.contains_key("group1"))
  assert_true(grouped_data.contains_key("group2"))
  assert_true(grouped_data.contains_key("group3"))
  
  // Test aggregation functions
  let sum_by_group = DataAggregation::aggregate_by(raw_data, lambda { (key, value) => key }, lambda { values => values.sum() })
  assert_eq(sum_by_group.get("group1"), Some(37.0))
  assert_eq(sum_by_group.get("group2"), Some(45.0))
  assert_eq(sum_by_group.get("group3"), Some(30.0))
  
  let avg_by_group = DataAggregation::aggregate_by(raw_data, lambda { (key, value) => key }, lambda { values => values.sum() / values.length().to_float() })
  assert_eq(avg_by_group.get("group1"), Some(12.333333333333334))
}

// Test 7: Data Filtering and Transformation
test "data filtering and transformation" {
  let sample_data = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  
  // Test filtering
  let filtered_data = DataTransformation::filter(sample_data, lambda { x => x > 5.0 })
  assert_eq(filtered_data, [6.0, 7.0, 8.0, 9.0, 10.0])
  
  // Test mapping
  let mapped_data = DataTransformation::map(sample_data, lambda { x => x * 2.0 })
  assert_eq(mapped_data, [2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0])
  
  // Test reduction
  let sum = DataTransformation::reduce(sample_data, 0.0, lambda { acc, x => acc + x })
  assert_eq(sum, 55.0)
  
  // Test windowing
  let windows = DataTransformation::sliding_window(sample_data, 3)
  assert_eq(windows.length(), 8)
  assert_eq(windows[0], [1.0, 2.0, 3.0])
  assert_eq(windows[7], [8.0, 9.0, 10.0])
}

// Test 8: Predictive Analytics
test "predictive analytics models" {
  let training_data = [
    (1.0, 2.0),
    (2.0, 4.0),
    (3.0, 6.0),
    (4.0, 8.0),
    (5.0, 10.0)
  ]
  
  // Test linear regression
  let regression_model = PredictiveAnalytics::linear_regression(training_data)
  let prediction = PredictiveAnalytics::predict(regression_model, 6.0)
  assert_true(prediction > 11.5 && prediction < 12.5)
  
  // Test polynomial regression
  let poly_model = PredictiveAnalytics::polynomial_regression(training_data, 2)
  let poly_prediction = PredictiveAnalytics::predict(poly_model, 6.0)
  assert_true(poly_prediction > 11.5 && poly_prediction < 12.5)
  
  // Test model evaluation
  let r_squared = PredictiveAnalytics::evaluate_model(regression_model, training_data)
  assert_true(r_squared > 0.99)
}

// Test 9: Data Quality Assessment
test "data quality assessment" {
  let clean_data = [1.0, 2.0, 3.0, 4.0, 5.0]
  let noisy_data = [1.0, 2.0, null, 4.0, 5.0]
  let outlier_data = [1.0, 2.0, 3.0, 4.0, 50.0]
  
  // Test completeness check
  let completeness_score = DataQuality::check_completeness(clean_data)
  assert_eq(completeness_score, 1.0)
  
  let noisy_completeness = DataQuality::check_completeness(noisy_data)
  assert_true(noisy_completeness < 1.0 && noisy_completeness > 0.0)
  
  // Test outlier detection
  let outlier_score = DataQuality::detect_outliers(outlier_data)
  assert_true(outlier_score > 0.0)
  
  // Test consistency check
  let consistency_score = DataQuality::check_consistency(clean_data)
  assert_true(consistency_score > 0.8)
}

// Test 10: Advanced Data Visualization Preparation
test "data visualization preparation" {
  let viz_data = [
    ("category1", 10.0),
    ("category2", 20.0),
    ("category3", 15.0),
    ("category4", 25.0),
    ("category5", 18.0)
  ]
  
  // Test histogram data preparation
  let histogram_data = DataVisualization::prepare_histogram_data(viz_data, 5)
  assert_eq(histogram_data.length(), 5)
  
  // Test pie chart data preparation
  let pie_data = DataVisualization::prepare_pie_chart_data(viz_data)
  assert_eq(pie_data.length(), 5)
  
  let total_percentage = pie_data.map(lambda { (_, percentage) => percentage }).sum()
  assert_true(total_percentage > 99.0 && total_percentage < 101.0)
  
  // Test time series visualization data
  let time_viz_data = DataVisualization::prepare_time_series_data([
    (1000L, 10.0),
    (2000L, 20.0),
    (3000L, 15.0)
  ])
  assert_eq(time_viz_data.length(), 3)
}