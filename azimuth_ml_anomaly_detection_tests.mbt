// Azimuth Telemetry System - ML Anomaly Detection Tests
// This file contains comprehensive test cases for machine learning-based anomaly detection

// Test 1: Basic Anomaly Detection Model Initialization
test "ml anomaly detection model initialization" {
  let model = AnomalyDetectionModel::new()
  
  // Test model is properly initialized
  assert_true(AnomalyDetectionModel::is_initialized(model))
  
  // Test default threshold value
  let default_threshold = AnomalyDetectionModel::get_threshold(model)
  assert_eq(default_threshold, 0.95)
  
  // Test initial training data count
  let training_data_count = AnomalyDetectionModel::get_training_data_count(model)
  assert_eq(training_data_count, 0)
}

// Test 2: Training Data Collection and Processing
test "ml anomaly detection training data collection" {
  let model = AnomalyDetectionModel::new()
  
  // Add normal training data samples
  let normal_samples = [
    MetricData::new("cpu_usage", 45.2, 1234567890L),
    MetricData::new("cpu_usage", 48.7, 1234567891L),
    MetricData::new("cpu_usage", 42.1, 1234567892L),
    MetricData::new("cpu_usage", 50.3, 1234567893L),
    MetricData::new("cpu_usage", 46.8, 1234567894L)
  ]
  
  for sample in normal_samples {
    AnomalyDetectionModel::add_training_data(model, sample)
  }
  
  // Verify training data count
  let training_data_count = AnomalyDetectionModel::get_training_data_count(model)
  assert_eq(training_data_count, 5)
  
  // Train the model
  let training_result = AnomalyDetectionModel::train(model)
  assert_true(training_result)
  
  // Verify model is trained
  assert_true(AnomalyDetectionModel::is_trained(model))
}

// Test 3: Anomaly Detection with Normal Data
test "ml anomaly detection with normal data" {
  let model = AnomalyDetectionModel::new()
  
  // Add training data
  let normal_samples = [
    MetricData::new("memory_usage", 60.2, 1234567890L),
    MetricData::new("memory_usage", 62.7, 1234567891L),
    MetricData::new("memory_usage", 58.1, 1234567892L),
    MetricData::new("memory_usage", 65.3, 1234567893L),
    MetricData::new("memory_usage", 61.8, 1234567894L)
  ]
  
  for sample in normal_samples {
    AnomalyDetectionModel::add_training_data(model, sample)
  }
  
  // Train the model
  AnomalyDetectionModel::train(model)
  
  // Test with normal data (should not be detected as anomaly)
  let normal_test_data = MetricData::new("memory_usage", 63.5, 1234567895L)
  let anomaly_score = AnomalyDetectionModel::calculate_anomaly_score(model, normal_test_data)
  let is_anomaly = AnomalyDetectionModel::is_anomaly(model, normal_test_data)
  
  assert_true(anomaly_score < 0.95)  // Normal data should have low anomaly score
  assert_false(is_anomaly)           // Should not be detected as anomaly
}

// Test 4: Anomaly Detection with Abnormal Data
test "ml anomaly detection with abnormal data" {
  let model = AnomalyDetectionModel::new()
  
  // Add training data
  let normal_samples = [
    MetricData::new("response_time", 120.2, 1234567890L),
    MetricData::new("response_time", 125.7, 1234567891L),
    MetricData::new("response_time", 118.1, 1234567892L),
    MetricData::new("response_time", 130.3, 1234567893L),
    MetricData::new("response_time", 122.8, 1234567894L)
  ]
  
  for sample in normal_samples {
    AnomalyDetectionModel::add_training_data(model, sample)
  }
  
  // Train the model
  AnomalyDetectionModel::train(model)
  
  // Test with abnormal data (should be detected as anomaly)
  let abnormal_test_data = MetricData::new("response_time", 500.5, 1234567895L)
  let anomaly_score = AnomalyDetectionModel::calculate_anomaly_score(model, abnormal_test_data)
  let is_anomaly = AnomalyDetectionModel::is_anomaly(model, abnormal_test_data)
  
  assert_true(anomaly_score >= 0.95)  // Abnormal data should have high anomaly score
  assert_true(is_anomaly)             // Should be detected as anomaly
}

// Test 5: Multi-dimensional Anomaly Detection
test "ml multi-dimensional anomaly detection" {
  let model = AnomalyDetectionModel::new()
  
  // Add multi-dimensional training data
  let multi_dim_samples = [
    MultiDimMetricData::new([
      ("cpu_usage", 45.2),
      ("memory_usage", 60.5),
      ("disk_io", 120.3)
    ], 1234567890L),
    MultiDimMetricData::new([
      ("cpu_usage", 48.7),
      ("memory_usage", 62.1),
      ("disk_io", 125.8)
    ], 1234567891L),
    MultiDimMetricData::new([
      ("cpu_usage", 42.1),
      ("memory_usage", 58.9),
      ("disk_io", 118.2)
    ], 1234567892L)
  ]
  
  for sample in multi_dim_samples {
    AnomalyDetectionModel::add_multi_dim_training_data(model, sample)
  }
  
  // Train the model
  AnomalyDetectionModel::train(model)
  
  // Test with normal multi-dimensional data
  let normal_multi_dim = MultiDimMetricData::new([
    ("cpu_usage", 46.5),
    ("memory_usage", 61.2),
    ("disk_io", 122.1)
  ], 1234567895L)
  
  let normal_anomaly_score = AnomalyDetectionModel::calculate_multi_dim_anomaly_score(model, normal_multi_dim)
  assert_true(normal_anomaly_score < 0.95)
  
  // Test with abnormal multi-dimensional data
  let abnormal_multi_dim = MultiDimMetricData::new([
    ("cpu_usage", 95.5),
    ("memory_usage", 95.2),
    ("disk_io", 500.8)
  ], 1234567896L)
  
  let abnormal_anomaly_score = AnomalyDetectionModel::calculate_multi_dim_anomaly_score(model, abnormal_multi_dim)
  assert_true(abnormal_anomaly_score >= 0.95)
}

// Test 6: Adaptive Threshold Adjustment
test "ml anomaly detection adaptive threshold adjustment" {
  let model = AnomalyDetectionModel::new()
  
  // Set initial threshold
  AnomalyDetectionModel::set_threshold(model, 0.9)
  assert_eq(AnomalyDetectionModel::get_threshold(model), 0.9)
  
  // Add training data
  let samples = [
    MetricData::new("network_latency", 50.2, 1234567890L),
    MetricData::new("network_latency", 52.7, 1234567891L),
    MetricData::new("network_latency", 48.1, 1234567892L)
  ]
  
  for sample in samples {
    AnomalyDetectionModel::add_training_data(model, sample)
  }
  
  // Train the model
  AnomalyDetectionModel::train(model)
  
  // Test adaptive threshold adjustment based on false positives
  AnomalyDetectionModel::adjust_threshold_adaptive(model, 0.1)  // 10% false positive rate
  let adjusted_threshold = AnomalyDetectionModel::get_threshold(model)
  
  // Threshold should be adjusted to reduce false positives
  assert_true(adjusted_threshold > 0.9)
}

// Test 7: Anomaly Detection Model Persistence
test "ml anomaly detection model persistence" {
  let model = AnomalyDetectionModel::new()
  
  // Add training data and train
  let samples = [
    MetricData::new("error_rate", 0.05, 1234567890L),
    MetricData::new("error_rate", 0.07, 1234567891L),
    MetricData::new("error_rate", 0.03, 1234567892L)
  ]
  
  for sample in samples {
    AnomalyDetectionModel::add_training_data(model, sample)
  }
  
  AnomalyDetectionModel::train(model)
  
  // Serialize model
  let serialized_model = AnomalyDetectionModel::serialize(model)
  assert_true(serialized_model.length() > 0)
  
  // Deserialize model
  let deserialized_model = AnomalyDetectionModel::deserialize(serialized_model)
  assert_true(AnomalyDetectionModel::is_trained(deserialized_model))
  
  // Test deserialized model works correctly
  let test_data = MetricData::new("error_rate", 0.25, 1234567893L)
  let anomaly_score = AnomalyDetectionModel::calculate_anomaly_score(deserialized_model, test_data)
  assert_true(anomaly_score > 0.0)
}

// Test 8: Anomaly Detection with Time Series Data
test "ml anomaly detection with time series data" {
  let model = AnomalyDetectionModel::new()
  
  // Add time series training data
  let time_series_samples = [
    TimeSeriesDataPoint::new("throughput", 1000.0, 1234567890L),
    TimeSeriesDataPoint::new("throughput", 1050.0, 1234567891L),
    TimeSeriesDataPoint::new("throughput", 980.0, 1234567892L),
    TimeSeriesDataPoint::new("throughput", 1020.0, 1234567893L),
    TimeSeriesDataPoint::new("throughput", 990.0, 1234567894L)
  ]
  
  for sample in time_series_samples {
    AnomalyDetectionModel::add_time_series_training_data(model, sample)
  }
  
  // Train the model with time series data
  AnomalyDetectionModel::train_time_series(model)
  
  // Test with normal time series pattern
  let normal_time_series = TimeSeriesDataPoint::new("throughput", 1015.0, 1234567895L)
  let normal_anomaly_score = AnomalyDetectionModel::calculate_time_series_anomaly_score(model, normal_time_series)
  assert_true(normal_anomaly_score < 0.95)
  
  // Test with abnormal time series pattern (sudden spike)
  let abnormal_time_series = TimeSeriesDataPoint::new("throughput", 5000.0, 1234567896L)
  let abnormal_anomaly_score = AnomalyDetectionModel::calculate_time_series_anomaly_score(model, abnormal_time_series)
  assert_true(abnormal_anomaly_score >= 0.95)
}

// Test 9: Anomaly Detection with Seasonal Patterns
test "ml anomaly detection with seasonal patterns" {
  let model = AnomalyDetectionModel::new()
  
  // Add seasonal training data (simulating daily patterns)
  let seasonal_samples = [
    // Morning pattern (low usage)
    TimeSeriesDataPoint::new("request_count", 100.0, 1234567200L),  // 8:00 AM
    TimeSeriesDataPoint::new("request_count", 150.0, 1234567260L),  // 8:01 AM
    TimeSeriesDataPoint::new("request_count", 200.0, 1234567320L),  // 8:02 AM
    
    // Afternoon pattern (high usage)
    TimeSeriesDataPoint::new("request_count", 800.0, 1234588800L),  // 2:00 PM
    TimeSeriesDataPoint::new("request_count", 850.0, 1234588860L),  // 2:01 PM
    TimeSeriesDataPoint::new("request_count", 900.0, 1234588920L),  // 2:02 PM
  ]
  
  for sample in seasonal_samples {
    AnomalyDetectionModel::add_time_series_training_data(model, sample)
  }
  
  // Train the model with seasonal pattern detection
  AnomalyDetectionModel::train_seasonal(model)
  
  // Test with normal seasonal pattern (morning)
  let normal_morning = TimeSeriesDataPoint::new("request_count", 180.0, 1234567380L)  // 8:03 AM
  let normal_morning_score = AnomalyDetectionModel::calculate_seasonal_anomaly_score(model, normal_morning)
  assert_true(normal_morning_score < 0.95)
  
  // Test with abnormal seasonal pattern (unusual morning spike)
  let abnormal_morning = TimeSeriesDataPoint::new("request_count", 900.0, 1234567380L)  // 8:03 AM
  let abnormal_morning_score = AnomalyDetectionModel::calculate_seasonal_anomaly_score(model, abnormal_morning)
  assert_true(abnormal_morning_score >= 0.95)
}

// Test 10: Anomaly Detection Ensemble Model
test "ml anomaly detection ensemble model" {
  let ensemble_model = AnomalyDetectionEnsemble::new()
  
  // Add different types of models to ensemble
  let statistical_model = StatisticalAnomalyModel::new()
  let neural_network_model = NeuralNetworkAnomalyModel::new()
  let time_series_model = TimeSeriesAnomalyModel::new()
  
  AnomalyDetectionEnsemble::add_model(ensemble_model, statistical_model)
  AnomalyDetectionEnsemble::add_model(ensemble_model, neural_network_model)
  AnomalyDetectionEnsemble::add_model(ensemble_model, time_series_model)
  
  // Train all models in ensemble
  let training_data = [
    MetricData::new("system_load", 2.5, 1234567890L),
    MetricData::new("system_load", 2.8, 1234567891L),
    MetricData::new("system_load", 2.3, 1234567892L),
    MetricData::new("system_load", 3.0, 1234567893L),
    MetricData::new("system_load", 2.6, 1234567894L)
  ]
  
  for data in training_data {
    AnomalyDetectionEnsemble::add_training_data(ensemble_model, data)
  }
  
  AnomalyDetectionEnsemble::train(ensemble_model)
  
  // Test ensemble prediction
  let test_data = MetricData::new("system_load", 8.5, 1234567895L)  // Abnormal value
  let ensemble_result = AnomalyDetectionEnsemble::detect_anomaly(ensemble_model, test_data)
  
  // Check ensemble result
  assert_true(ensemble_result.is_anomaly)
  assert_true(ensemble_result.confidence > 0.8)
  assert_eq(ensemble_result.voting_models, 3)  // All models should agree
  
  // Test with normal data
  let normal_test_data = MetricData::new("system_load", 2.7, 1234567896L)
  let normal_ensemble_result = AnomalyDetectionEnsemble::detect_anomaly(ensemble_model, normal_test_data)
  
  assert_false(normal_ensemble_result.is_anomaly)
  assert_true(normal_ensemble_result.confidence < 0.5)
}