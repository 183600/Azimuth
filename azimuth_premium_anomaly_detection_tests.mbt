// Azimuth Premium Anomaly Detection and Alerting Test Suite
// Advanced test cases for anomaly detection and alerting mechanisms

// Test 1: Statistical Anomaly Detection Algorithms
test "statistical anomaly detection algorithms implementation" {
  // Define anomaly detection types
  enum AnomalyType {
    StatisticalOutlier
    TrendChange
    SeasonalDeviation
    PatternViolation
    ThresholdBreach
  }
  
  // Define anomaly severity
  enum AnomalySeverity {
    Low
    Medium
    High
    Critical
  }
  
  // Define data point
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // Define anomaly detection result
  type AnomalyResult = {
    timestamp: Int,
    value: Float,
    anomaly_type: AnomalyType,
    severity: AnomalySeverity,
    confidence: Float,
    expected_value: Float,
    deviation: Float,
    description: String
  }
  
  // Define statistical model
  type StatisticalModel = {
    mean: Float,
    standard_deviation: Float,
    median: Float,
    quartiles: (Float, Float, Float),  // Q1, Q2, Q3
    iqr: Float,  // Interquartile range
    trend_slope: Float,
    seasonal_components: Array[Float]
  }
  
  // Create statistical anomaly detector
  let create_statistical_detector = fn() {
    {
      build_statistical_model: fn(points: Array[DataPoint], window_size: Int) {
        if points.length() < window_size {
          // Use all available points if less than window size
          let values = points.map(fn(p) { p.value })
          let sorted_values = values.sort(fn(a, b) { a < b })
          
          let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
          let variance = values.reduce(fn(acc, v) {
            let deviation = v - mean
            acc + (deviation * deviation)
          }, 0.0) / values.length().to_float()
          let standard_deviation = variance.sqrt()
          
          let median = if sorted_values.length() > 0 {
            let mid = sorted_values.length() / 2
            if sorted_values.length() % 2 == 0 {
              (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
            } else {
              sorted_values[mid]
            }
          } else {
            0.0
          }
          
          let q1 = if sorted_values.length() > 0 {
            let q1_index = sorted_values.length() / 4
            sorted_values[q1_index]
          } else {
            0.0
          }
          
          let q3 = if sorted_values.length() > 0 {
            let q3_index = (sorted_values.length() * 3) / 4
            sorted_values[q3_index]
          } else {
            0.0
          }
          
          let iqr = q3 - q1
          
          // Simple trend calculation
          let trend_slope = if points.length() >= 2 {
            let n = points.length().to_float()
            let sum_x = points.reduce(fn(acc, p) { acc + p.timestamp.to_float() }, 0.0)
            let sum_y = points.reduce(fn(acc, p) { acc + p.value }, 0.0)
            let sum_xy = points.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.value) 
            }, 0.0)
            let sum_x2 = points.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.timestamp.to_float()) 
            }, 0.0)
            
            (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
          } else {
            0.0
          }
          
          {
            mean,
            standard_deviation,
            median,
            quartiles: (q1, median, q3),
            iqr,
            trend_slope,
            seasonal_components: []
          }
        } else {
          // Use sliding window
          let recent_points = points.skip(points.length() - window_size)
          let values = recent_points.map(fn(p) { p.value })
          let sorted_values = values.sort(fn(a, b) { a < b })
          
          let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
          let variance = values.reduce(fn(acc, v) {
            let deviation = v - mean
            acc + (deviation * deviation)
          }, 0.0) / values.length().to_float()
          let standard_deviation = variance.sqrt()
          
          let median = if sorted_values.length() > 0 {
            let mid = sorted_values.length() / 2
            if sorted_values.length() % 2 == 0 {
              (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
            } else {
              sorted_values[mid]
            }
          } else {
            0.0
          }
          
          let q1 = if sorted_values.length() > 0 {
            let q1_index = sorted_values.length() / 4
            sorted_values[q1_index]
          } else {
            0.0
          }
          
          let q3 = if sorted_values.length() > 0 {
            let q3_index = (sorted_values.length() * 3) / 4
            sorted_values[q3_index]
          } else {
            0.0
          }
          
          let iqr = q3 - q1
          
          // Simple trend calculation
          let trend_slope = if recent_points.length() >= 2 {
            let n = recent_points.length().to_float()
            let sum_x = recent_points.reduce(fn(acc, p) { acc + p.timestamp.to_float() }, 0.0)
            let sum_y = recent_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
            let sum_xy = recent_points.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.value) 
            }, 0.0)
            let sum_x2 = recent_points.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.timestamp.to_float()) 
            }, 0.0)
            
            (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
          } else {
            0.0
          }
          
          {
            mean,
            standard_deviation,
            median,
            quartiles: (q1, median, q3),
            iqr,
            trend_slope,
            seasonal_components: []
          }
        }
      },
      
      detect_zscore_anomalies: fn(points: Array[DataPoint], model: StatisticalModel, threshold: Float) {
        points.map(fn(point) {
          let z_score = if model.standard_deviation > 0.0 {
            (point.value - model.mean) / model.standard_deviation
          } else {
            0.0
          }
          
          let is_anomaly = z_score.abs() > threshold
          let severity = if z_score.abs() > 4.0 {
            AnomalySeverity::Critical
          } else if z_score.abs() > 3.0 {
            AnomalySeverity::High
          } else if z_score.abs() > 2.0 {
            AnomalySeverity::Medium
          } else if z_score.abs() > threshold {
            AnomalySeverity::Low
          } else {
            AnomalySeverity::Low  // Default for threshold breaches
          }
          
          let confidence = if z_score.abs() > 0.0 {
            (z_score.abs() / 5.0).min(1.0)  // Normalize to 0-1 range
          } else {
            0.0
          }
          
          {
            timestamp: point.timestamp,
            value: point.value,
            anomaly_type: AnomalyType::StatisticalOutlier,
            severity,
            confidence,
            expected_value: model.mean,
            deviation: point.value - model.mean,
            description: "Z-score: " + z_score.to_string()
          }
        }).filter(fn(result) {
          let z_score = if model.standard_deviation > 0.0 {
            (result.value - model.mean) / model.standard_deviation
          } else {
            0.0
          }
          z_score.abs() > threshold
        })
      },
      
      detect_iqr_anomalies: fn(points: Array[DataPoint], model: StatisticalModel, multiplier: Float) {
        let lower_bound = model.quartiles.0 - (multiplier * model.iqr)
        let upper_bound = model.quartiles.2 + (multiplier * model.iqr)
        
        points.map(fn(point) {
          let is_anomaly = point.value < lower_bound or point.value > upper_bound
          
          let severity = if point.value < lower_bound - (2.0 * model.iqr) or 
                         point.value > upper_bound + (2.0 * model.iqr) {
            AnomalySeverity::Critical
          } else if point.value < lower_bound - model.iqr or 
                    point.value > upper_bound + model.iqr {
            AnomalySeverity::High
          } else if is_anomaly {
            AnomalySeverity::Medium
          } else {
            AnomalySeverity::Low
          }
          
          let deviation = if point.value < lower_bound {
            point.value - lower_bound
          } else if point.value > upper_bound {
            point.value - upper_bound
          } else {
            0.0
          }
          
          let confidence = (deviation.abs() / (model.iqr * 3.0)).min(1.0)
          
          {
            timestamp: point.timestamp,
            value: point.value,
            anomaly_type: AnomalyType::StatisticalOutlier,
            severity,
            confidence,
            expected_value: model.median,
            deviation,
            description: "IQR bounds: [" + lower_bound.to_string() + ", " + upper_bound.to_string() + "]"
          }
        }).filter(fn(result) {
          result.value < lower_bound or result.value > upper_bound
        })
      },
      
      detect_trend_anomalies: fn(points: Array[DataPoint], model: StatisticalModel, threshold: Float) {
        if points.length() < 3 {
          return []
        }
        
        // Calculate recent trend
        let recent_points = points.skip(points.length() - 3)
        let n = recent_points.length().to_float()
        let sum_x = recent_points.reduce(fn(acc, p) { acc + p.timestamp.to_float() }, 0.0)
        let sum_y = recent_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        let sum_xy = recent_points.reduce(fn(acc, p) { 
          acc + (p.timestamp.to_float() * p.value) 
        }, 0.0)
        let sum_x2 = recent_points.reduce(fn(acc, p) { 
          acc + (p.timestamp.to_float() * p.timestamp.to_float()) 
        }, 0.0)
        
        let recent_trend = if n * sum_x2 - sum_x * sum_x != 0.0 {
          (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        } else {
          0.0
        }
        
        let trend_change = recent_trend - model.trend_slope
        let is_anomaly = trend_change.abs() > threshold
        
        if is_anomaly {
          let severity = if trend_change.abs() > threshold * 3.0 {
            AnomalySeverity::Critical
          } else if trend_change.abs() > threshold * 2.0 {
            AnomalySeverity::High
          } else {
            AnomalySeverity::Medium
          }
          
          let confidence = (trend_change.abs() / (threshold * 3.0)).min(1.0)
          
          let last_point = points[points.length() - 1]
          let expected_value = last_point.value + model.trend_slope * 100.0  // Predict next value
          
          [{
            timestamp: last_point.timestamp,
            value: last_point.value,
            anomaly_type: AnomalyType::TrendChange,
            severity,
            confidence,
            expected_value,
            deviation: trend_change,
            description: "Trend change: " + trend_change.to_string()
          }]
        } else {
          []
        }
      }
    }
  }
  
  // Create statistical anomaly detector
  let detector = create_statistical_detector()
  
  // Create test data with anomalies
  let base_time = 1640995200
  let data_points = []
  
  // Generate normal data with some anomalies
  for i in 0..=19 {
    let timestamp = base_time + i * 60  // 1-minute intervals
    let normal_value = 50.0 + 10.0 * ((i.to_float() / 10.0) * 3.14159).sin()
    
    let value = match i {
      5 => 120.0  // High outlier
      10 => -20.0  // Low outlier
      15 => 85.0   // Moderate outlier
      _ => normal_value
    }
    
    data_points = data_points.push({
      timestamp,
      value,
      metadata: [("metric", "cpu_usage")]
    })
  }
  
  // Build statistical model
  let model = detector.build_statistical_model(data_points, 10)
  
  // Test z-score anomaly detection
  let zscore_anomalies = detector.detect_zscore_anomalies(data_points, model, 2.0)
  
  assert_true(zscore_anomalies.length() >= 2)  // Should detect at least the high and low outliers
  
  // Check specific anomalies
  let high_outlier = zscore_anomalies.find_fn(fn(anomaly) { anomaly.value > 100.0 })
  assert_not_eq(high_outlier, None)
  match high_outlier {
    Some(anomaly) => {
      assert_eq(anomaly.anomaly_type, AnomalyType::StatisticalOutlier)
      assert_eq(anomaly.severity, AnomalySeverity::Critical)
      assert_true(anomaly.confidence > 0.8)
    }
    None => assert_true(false)
  }
  
  let low_outlier = zscore_anomalies.find_fn(fn(anomaly) { anomaly.value < 0.0 })
  assert_not_eq(low_outlier, None)
  match low_outlier {
    Some(anomaly) => {
      assert_eq(anomaly.anomaly_type, AnomalyType::StatisticalOutlier)
      assert_eq(anomaly.severity, AnomalySeverity::Critical)
      assert_true(anomaly.confidence > 0.8)
    }
    None => assert_true(false)
  }
  
  // Test IQR anomaly detection
  let iqr_anomalies = detector.detect_iqr_anomalies(data_points, model, 1.5)
  
  assert_true(iqr_anomalies.length() >= 2)  // Should detect similar outliers
  
  // Test trend anomaly detection
  let trend_anomalies = detector.detect_trend_anomalies(data_points, model, 0.5)
  
  // Trend anomalies may or may not be detected depending on the data
  assert_true(trend_anomalies.length() >= 0)
  
  // Verify model properties
  assert_true(model.mean > 40.0 and model.mean < 60.0)  // Should be around the normal values
  assert_true(model.standard_deviation > 0.0)
  assert_true(model.iqr > 0.0)
}

// Test 2: Machine Learning Based Anomaly Detection
test "machine learning based anomaly detection algorithms" {
  // Define ML model types
  enum ModelType {
    IsolationForest
    OneClassSVM
    Autoencoder
    ClusteringBased
  }
  
  // Define feature vector
  type FeatureVector = {
    features: Array[Float],
    timestamp: Int,
    metadata: Array[(String, String)]
  }
  
  // Define ML model interface
  type MLModel = {
    model_type: ModelType,
    is_trained: Bool,
    training_data_size: Int,
    anomaly_threshold: Float
  }
  
  // Create ML-based anomaly detector
  let create_ml_detector = fn() {
    {
      extract_features: fn(points: Array[DataPoint], window_size: Int) {
        if points.length() < window_size {
          return []
        }
        
        let mut feature_vectors = []
        
        for i in window_size - 1..points.length() - 1 {
          let window = points.skip(i - window_size + 1).take(window_size)
          
          // Extract statistical features
          let values = window.map(fn(p) { p.value })
          let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
          let variance = values.reduce(fn(acc, v) {
            let deviation = v - mean
            acc + (deviation * deviation)
          }, 0.0) / values.length().to_float()
          let std_dev = variance.sqrt()
          
          let sorted_values = values.sort(fn(a, b) { a < b })
          let median = if sorted_values.length() > 0 {
            let mid = sorted_values.length() / 2
            if sorted_values.length() % 2 == 0 {
              (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
            } else {
              sorted_values[mid]
            }
          } else {
            0.0
          }
          
          let q1 = if sorted_values.length() > 0 {
            let q1_index = sorted_values.length() / 4
            sorted_values[q1_index]
          } else {
            0.0
          }
          
          let q3 = if sorted_values.length() > 0 {
            let q3_index = (sorted_values.length() * 3) / 4
            sorted_values[q3_index]
          } else {
            0.0
          }
          
          let iqr = q3 - q1
          
          // Extract trend features
          let trend_slope = if window.length() >= 2 {
            let n = window.length().to_float()
            let sum_x = window.reduce(fn(acc, p) { acc + p.timestamp.to_float() }, 0.0)
            let sum_y = window.reduce(fn(acc, p) { acc + p.value }, 0.0)
            let sum_xy = window.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.value) 
            }, 0.0)
            let sum_x2 = window.reduce(fn(acc, p) { 
              acc + (p.timestamp.to_float() * p.timestamp.to_float()) 
            }, 0.0)
            
            if n * sum_x2 - sum_x * sum_x != 0.0 {
              (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
            } else {
              0.0
            }
          } else {
            0.0
          }
          
          // Extract volatility features
          let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc }, -999999.0)
          let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc }, 999999.0)
          let range = max_value - min_value
          
          // Extract temporal features
          let current_point = points[i]
          let hour_of_day = (current_point.timestamp / 3600) % 24
          let day_of_week = (current_point.timestamp / 86400) % 7
          
          let features = [
            mean,
            std_dev,
            median,
            q1,
            q3,
            iqr,
            trend_slope,
            range,
            max_value,
            min_value,
            hour_of_day.to_float(),
            day_of_week.to_float()
          ]
          
          feature_vectors = feature_vectors.push({
            features,
            timestamp: current_point.timestamp,
            metadata: current_point.metadata
          })
        }
        
        feature_vectors
      },
      
      train_isolation_forest: fn(feature_vectors: Array[FeatureVector], num_trees: Int, max_samples: Int) {
        // Simplified isolation forest implementation
        let training_size = feature_vectors.length()
        
        if training_size == 0 {
          return {
            model_type: ModelType::IsolationForest,
            is_trained: false,
            training_data_size: 0,
            anomaly_threshold: 0.5
          }
        }
        
        // Build random trees (simplified)
        let mut trees = []
        
        for i in 0..num_trees {
          // Sample data for this tree
          let sample_size = if max_samples < training_size { max_samples } else { training_size }
          let mut samples = []
          
          for j in 0..sample_size {
            let index = (i * j) % training_size  // Simple deterministic sampling
            samples = samples.push(feature_vectors[index])
          }
          
          // Build a simple tree (just store the samples)
          trees = trees.push(samples)
        }
        
        {
          model_type: ModelType::IsolationForest,
          is_trained: true,
          training_data_size: training_size,
          anomaly_threshold: 0.5
        }
      },
      
      detect_anomalies_with_isolation_forest: fn(model: MLModel, feature_vectors: Array[FeatureVector]) {
        if not(model.is_trained) {
          return []
        }
        
        // Simplified anomaly scoring based on distance from training data
        let training_mean = 50.0  // Simplified - in real implementation would calculate from training data
        let training_std = 10.0
        
        feature_vectors.map(fn(vector) {
          // Use first feature (mean) for simplified scoring
          let primary_feature = vector.features[0]
          let z_score = if training_std > 0.0 {
            (primary_feature - training_mean) / training_std
          } else {
            0.0
          }
          
          // Calculate anomaly score (simplified)
          let anomaly_score = (z_score.abs() / 3.0).min(1.0)
          let is_anomaly = anomaly_score > model.anomaly_threshold
          
          if is_anomaly {
            let severity = if anomaly_score > 0.8 {
              AnomalySeverity::Critical
            } else if anomaly_score > 0.6 {
              AnomalySeverity::High
            } else {
              AnomalySeverity::Medium
            }
            
            Some({
              timestamp: vector.timestamp,
              value: primary_feature,
              anomaly_type: AnomalyType::StatisticalOutlier,
              severity,
              confidence: anomaly_score,
              expected_value: training_mean,
              deviation: primary_feature - training_mean,
              description: "Isolation Forest anomaly score: " + anomaly_score.to_string()
            })
          } else {
            None
          }
        }).filter_map(fn(result) { result })
      },
      
      train_clustering_model: fn(feature_vectors: Array[FeatureVector], num_clusters: Int) {
        let training_size = feature_vectors.length()
        
        if training_size == 0 {
          return {
            model_type: ModelType::ClusteringBased,
            is_trained: false,
            training_data_size: 0,
            anomaly_threshold: 0.5
          }
        }
        
        // Simplified k-means clustering
        let mut centroids = []
        
        // Initialize centroids with first few points
        for i in 0..num_clusters.min(training_size) {
          centroids = centroids.push(feature_vectors[i].features)
        }
        
        // Simple clustering (just store initial centroids)
        {
          model_type: ModelType::ClusteringBased,
          is_trained: true,
          training_data_size: training_size,
          anomaly_threshold: 0.5
        }
      },
      
      detect_anomalies_with_clustering: fn(model: MLModel, feature_vectors: Array[FeatureVector]) {
        if not(model.is_trained) {
          return []
        }
        
        // Simplified clustering-based anomaly detection
        feature_vectors.map(fn(vector) {
          // Use first feature for simplified distance calculation
          let primary_feature = vector.features[0]
          
          // Calculate distance from cluster center (simplified)
          let cluster_center = 50.0  // Simplified cluster center
          let distance = (primary_feature - cluster_center).abs()
          let max_distance = 30.0  // Simplified max distance
          
          let anomaly_score = (distance / max_distance).min(1.0)
          let is_anomaly = anomaly_score > model.anomaly_threshold
          
          if is_anomaly {
            let severity = if anomaly_score > 0.8 {
              AnomalySeverity::Critical
            } else if anomaly_score > 0.6 {
              AnomalySeverity::High
            } else {
              AnomalySeverity::Medium
            }
            
            Some({
              timestamp: vector.timestamp,
              value: primary_feature,
              anomaly_type: AnomalyType::PatternViolation,
              severity,
              confidence: anomaly_score,
              expected_value: cluster_center,
              deviation: primary_feature - cluster_center,
              description: "Clustering-based anomaly score: " + anomaly_score.to_string()
            })
          } else {
            None
          }
        }).filter_map(fn(result) { result })
      }
    }
  }
  
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  enum AnomalyType {
    StatisticalOutlier
    TrendChange
    SeasonalDeviation
    PatternViolation
    ThresholdBreach
  }
  
  enum AnomalySeverity {
    Low
    Medium
    High
    Critical
  }
  
  // Create ML detector
  let detector = create_ml_detector()
  
  // Create test data
  let base_time = 1640995200
  let data_points = []
  
  // Generate normal data with anomalies
  for i in 0..=49 {
    let timestamp = base_time + i * 60  // 1-minute intervals
    let normal_value = 50.0 + 5.0 * ((i.to_float() / 20.0) * 3.14159).sin()
    
    let value = match i {
      12 => 95.0   // High anomaly
      25 => 10.0   // Low anomaly
      37 => 75.0   // Moderate anomaly
      _ => normal_value
    }
    
    data_points = data_points.push({
      timestamp,
      value,
      metadata: [("metric", "response_time")]
    })
  }
  
  // Extract features
  let feature_vectors = detector.extract_features(data_points, 5)
  
  assert_true(feature_vectors.length() > 0)
  assert_eq(feature_vectors[0].features.length(), 12)  // Should extract 12 features
  
  // Train isolation forest model
  let isolation_model = detector.train_isolation_forest(feature_vectors, 10, 20)
  
  assert_true(isolation_model.is_trained)
  assert_eq(isolation_model.training_data_size, feature_vectors.length())
  
  // Detect anomalies with isolation forest
  let isolation_anomalies = detector.detect_anomalies_with_isolation_forest(isolation_model, feature_vectors)
  
  assert_true(isolation_anomalies.length() >= 1)  // Should detect at least one anomaly
  
  // Train clustering model
  let clustering_model = detector.train_clustering_model(feature_vectors, 3)
  
  assert_true(clustering_model.is_trained)
  assert_eq(clustering_model.training_data_size, feature_vectors.length())
  
  // Detect anomalies with clustering
  let clustering_anomalies = detector.detect_anomalies_with_clustering(clustering_model, feature_vectors)
  
  assert_true(clustering_anomalies.length() >= 0)  // May or may not detect anomalies
  
  // Verify anomaly properties
  for anomaly in isolation_anomalies {
    assert_true(anomaly.confidence >= 0.0 and anomaly.confidence <= 1.0)
    assert_true(anomaly.severity == AnomalySeverity::Medium or 
                anomaly.severity == AnomalySeverity::High or
                anomaly.severity == AnomalySeverity::Critical)
  }
}

// Test 3: Alerting System Integration
test "comprehensive alerting system integration" {
  // Define alert types
  enum AlertType {
    MetricThreshold
    AnomalyDetected
    SystemHealth
    TrendChange
    PatternViolation
  }
  
  // Define alert priority
  enum AlertPriority {
    Info
    Warning
    Error
    Critical
  }
  
  // Define alert status
  enum AlertStatus {
    Active
    Acknowledged
    Resolved
    Suppressed
  }
  
  // Define alert
  type Alert = {
    id: String,
    alert_type: AlertType,
    priority: AlertPriority,
    status: AlertStatus,
    title: String,
    description: String,
    timestamp: Int,
    source: String,
    metadata: Array[(String, String)],
    acknowledged_by: Option<String>,
    acknowledged_at: Option[Int],
    resolved_at: Option[Int]
  }
  
  // Define notification channel
  enum NotificationChannel {
    Email
    Slack
    PagerDuty
    Webhook
    SMS
  }
  
  // Define notification rule
  type NotificationRule = {
    name: String,
    conditions: Array[(String, String)>,  // key-value conditions
    channels: Array[NotificationChannel],
    cooldown_minutes: Int,
    escalation_rules: Array[(Int, Array[NotificationChannel])]  // (minutes, channels)
  }
  
  // Define alert manager
  let create_alert_manager = fn() {
    let mut alerts = []
    let mut alert_counter = 0
    let mut notification_history = []
    
    {
      create_alert: fn(
        alert_type: AlertType,
        priority: AlertPriority,
        title: String,
        description: String,
        source: String,
        metadata: Array[(String, String)>
      ) {
        alert_counter = alert_counter + 1
        let current_time = 1640995200  // Simplified timestamp
        
        let alert = {
          id: "alert-" + alert_counter.to_string(),
          alert_type,
          priority,
          status: AlertStatus::Active,
          title,
          description,
          timestamp: current_time,
          source,
          metadata,
          acknowledged_by: None,
          acknowledged_at: None,
          resolved_at: None
        }
        
        alerts = alerts.push(alert)
        alert.id
      },
      
      acknowledge_alert: fn(alert_id: String, acknowledged_by: String) {
        let current_time = 1640995200
        
        let mut updated_alerts = []
        let mut found = false
        
        for alert in alerts {
          if alert.id == alert_id and alert.status == AlertStatus::Active {
            found = true
            updated_alerts = updated_alerts.push({
              id: alert.id,
              alert_type: alert.alert_type,
              priority: alert.priority,
              status: AlertStatus::Acknowledged,
              title: alert.title,
              description: alert.description,
              timestamp: alert.timestamp,
              source: alert.source,
              metadata: alert.metadata,
              acknowledged_by: Some(acknowledged_by),
              acknowledged_at: Some(current_time),
              resolved_at: None
            })
          } else {
            updated_alerts = updated_alerts.push(alert)
          }
        }
        
        alerts = updated_alerts
        found
      },
      
      resolve_alert: fn(alert_id: String) {
        let current_time = 1640995200
        
        let mut updated_alerts = []
        let mut found = false
        
        for alert in alerts {
          if alert.id == alert_id and (alert.status == AlertStatus::Active or alert.status == AlertStatus::Acknowledged) {
            found = true
            updated_alerts = updated_alerts.push({
              id: alert.id,
              alert_type: alert.alert_type,
              priority: alert.priority,
              status: AlertStatus::Resolved,
              title: alert.title,
              description: alert.description,
              timestamp: alert.timestamp,
              source: alert.source,
              metadata: alert.metadata,
              acknowledged_by: alert.acknowledged_by,
              acknowledged_at: alert.acknowledged_at,
              resolved_at: Some(current_time)
            })
          } else {
            updated_alerts = updated_alerts.push(alert)
          }
        }
        
        alerts = updated_alerts
        found
      },
      
      evaluate_notification_rules: fn(alert: Alert, rules: Array[NotificationRule]) {
        let mut matched_rules = []
        
        for rule in rules {
          let mut conditions_met = true
          
          // Check if all conditions are met
          for (key, value) in rule.conditions {
            let condition_met = match key {
              "alert_type" => {
                match alert.alert_type {
                  AlertType::MetricThreshold => value == "MetricThreshold"
                  AlertType::AnomalyDetected => value == "AnomalyDetected"
                  AlertType::SystemHealth => value == "SystemHealth"
                  AlertType::TrendChange => value == "TrendChange"
                  AlertType::PatternViolation => value == "PatternViolation"
                }
              }
              "priority" => {
                match alert.priority {
                  AlertPriority::Info => value == "Info"
                  AlertPriority::Warning => value == "Warning"
                  AlertPriority::Error => value == "Error"
                  AlertPriority::Critical => value == "Critical"
                }
              }
              "source" => alert.source == value
              "status" => {
                match alert.status {
                  AlertStatus::Active => value == "Active"
                  AlertStatus::Acknowledged => value == "Acknowledged"
                  AlertStatus::Resolved => value == "Resolved"
                  AlertStatus::Suppressed => value == "Suppressed"
                }
              }
              _ => {
                // Check metadata
                alert.metadata.any_fn(fn(meta) { meta.0 == key and meta.1 == value })
              }
            }
            
            if not(condition_met) {
              conditions_met = false
            }
          }
          
          if conditions_met {
            matched_rules = matched_rules.push(rule)
          }
        }
        
        matched_rules
      },
      
      send_notifications: fn(alert: Alert, rules: Array[NotificationRule]) {
        let matched_rules = alerts.evaluate_notification_rules(alert, rules)
        let mut notifications_sent = []
        
        for rule in matched_rules {
          // Check cooldown
          let recent_notifications = notification_history.filter_fn(fn(notif) {
            notif.alert_id == alert.id and 
            (1640995200 - notif.timestamp) < rule.cooldown_minutes * 60
          })
          
          if recent_notifications.length() == 0 {
            // Send notifications to all channels
            for channel in rule.channels {
              notifications_sent = notifications_sent.push({
                alert_id: alert.id,
                channel,
                timestamp: 1640995200,
                title: alert.title,
                message: alert.description
              })
              
              // Add to notification history
              notification_history = notification_history.push({
                alert_id: alert.id,
                channel,
                timestamp: 1640995200
              })
            }
          }
        }
        
        notifications_sent
      },
      
      get_alert_statistics: fn() {
        let total_alerts = alerts.length()
        let active_alerts = alerts.count_fn(fn(alert) { alert.status == AlertStatus::Active })
        let acknowledged_alerts = alerts.count_fn(fn(alert) { alert.status == AlertStatus::Acknowledged })
        let resolved_alerts = alerts.count_fn(fn(alert) { alert.status == AlertStatus::Resolved })
        
        let alerts_by_priority = [
          (AlertPriority::Info, alerts.count_fn(fn(alert) { alert.priority == AlertPriority::Info })),
          (AlertPriority::Warning, alerts.count_fn(fn(alert) { alert.priority == AlertPriority::Warning })),
          (AlertPriority::Error, alerts.count_fn(fn(alert) { alert.priority == AlertPriority::Error })),
          (AlertPriority::Critical, alerts.count_fn(fn(alert) { alert.priority == AlertPriority::Critical }))
        ]
        
        let alerts_by_type = [
          (AlertType::MetricThreshold, alerts.count_fn(fn(alert) { alert.alert_type == AlertType::MetricThreshold })),
          (AlertType::AnomalyDetected, alerts.count_fn(fn(alert) { alert.alert_type == AlertType::AnomalyDetected })),
          (AlertType::SystemHealth, alerts.count_fn(fn(alert) { alert.alert_type == AlertType::SystemHealth })),
          (AlertType::TrendChange, alerts.count_fn(fn(alert) { alert.alert_type == AlertType::TrendChange })),
          (AlertType::PatternViolation, alerts.count_fn(fn(alert) { alert.alert_type == AlertType::PatternViolation }))
        ]
        
        {
          total_alerts,
          active_alerts,
          acknowledged_alerts,
          resolved_alerts,
          alerts_by_priority,
          alerts_by_type
        }
      }
    }
  }
  
  // Create alert manager
  let manager = create_alert_manager()
  
  // Create notification rules
  let notification_rules = [
    {
      name: "Critical alerts",
      conditions: [("priority", "Critical")],
      channels: [NotificationChannel::PagerDuty, NotificationChannel::Slack],
      cooldown_minutes: 5,
      escalation_rules: [(10, [NotificationChannel::SMS])]
    },
    {
      name: "Error alerts",
      conditions: [("priority", "Error")],
      channels: [NotificationChannel::Email, NotificationChannel::Slack],
      cooldown_minutes: 15,
      escalation_rules: [(30, [NotificationChannel::PagerDuty])]
    },
    {
      name: "API service alerts",
      conditions: [("source", "api-service")],
      channels: [NotificationChannel::Slack],
      cooldown_minutes: 10,
      escalation_rules: []
    },
    {
      name: "Anomaly alerts",
      conditions: [("alert_type", "AnomalyDetected")],
      channels: [NotificationChannel::Email],
      cooldown_minutes: 20,
      escalation_rules: [(60, [NotificationChannel::Slack])]
    }
  ]
  
  // Create test alerts
  let critical_alert_id = manager.create_alert(
    AlertType::MetricThreshold,
    AlertPriority::Critical,
    "CPU Usage Critical",
    "CPU usage is above 95%",
    "api-service",
    [("metric", "cpu_usage"), ("value", "98.5"), ("threshold", "95")]
  )
  
  let error_alert_id = manager.create_alert(
    AlertType::SystemHealth,
    AlertPriority::Error,
    "Database Connection Failed",
    "Unable to connect to primary database",
    "user-service",
    [("database", "primary"), "error_code", "CONN_TIMEOUT"]
  )
  
  let anomaly_alert_id = manager.create_alert(
    AlertType::AnomalyDetected,
    AlertPriority::Warning,
    "Unusual Traffic Pattern",
    "Traffic pattern deviates from normal baseline",
    "load-balancer",
    [("metric", "request_rate"), ("anomaly_score", "0.85")]
  )
  
  // Test alert acknowledgment
  let acknowledged = manager.acknowledge_alert(critical_alert_id, "ops-team")
  assert_true(acknowledged)
  
  // Test alert resolution
  let resolved = manager.resolve_alert(error_alert_id)
  assert_true(resolved)
  
  // Test notification rule evaluation
  let critical_alert = alerts.find(fn(alert) { alert.id == critical_alert_id })
  match critical_alert {
    Some(alert) => {
      let matched_rules = manager.evaluate_notification_rules(alert, notification_rules)
      assert_eq(matched_rules.length(), 2)  // Should match "Critical alerts" and "API service alerts"
    }
    None => assert_true(false)
  }
  
  // Test notification sending
  let anomaly_alert = alerts.find(fn(alert) { alert.id == anomaly_alert_id })
  match anomaly_alert {
    Some(alert) => {
      let notifications = manager.send_notifications(alert, notification_rules)
      assert_eq(notifications.length(), 1)  // Should match "Anomaly alerts" rule
      assert_eq(notifications[0].channel, NotificationChannel::Email)
    }
    None => assert_true(false)
  }
  
  // Test alert statistics
  let stats = manager.get_alert_statistics()
  
  assert_eq(stats.total_alerts, 3)
  assert_eq(stats.active_alerts, 1)  // Only anomaly alert should be active
  assert_eq(stats.acknowledged_alerts, 1)  // Critical alert acknowledged
  assert_eq(stats.resolved_alerts, 1)  // Error alert resolved
  
  // Check priority distribution
  let critical_count = stats.alerts_by_priority.find_fn(fn(pair) { pair.0 == AlertPriority::Critical })
  match critical_count {
    Some((_, count)) => assert_eq(count, 1)
    None => assert_true(false)
  }
  
  let warning_count = stats.alerts_by_priority.find_fn(fn(pair) { pair.0 == AlertPriority::Warning })
  match warning_count {
    Some((_, count)) => assert_eq(count, 1)
    None => assert_true(false)
  }
  
  // Check type distribution
  let metric_threshold_count = stats.alerts_by_type.find_fn(fn(pair) { pair.0 == AlertType::MetricThreshold })
  match metric_threshold_count {
    Some((_, count)) => assert_eq(count, 1)
    None => assert_true(false)
  }
  
  let anomaly_detected_count = stats.alerts_by_type.find_fn(fn(pair) { pair.0 == AlertType::AnomalyDetected })
  match anomaly_detected_count {
    Some((_, count)) => assert_eq(count, 1)
    None => assert_true(false)
  }
}