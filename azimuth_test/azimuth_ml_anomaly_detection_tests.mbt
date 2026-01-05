// Azimuth ML Anomaly Detection Test Suite
// This file contains test cases for machine learning-based anomaly detection functionality

// Test 1: Time Series Anomaly Detection
test "time series anomaly detection" {
  // Define time series data structure
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  type AnomalyDetectionModel = {
    model_id: String,
    name: String,
    algorithm: String,
    parameters: Array[(String, String)],
    sensitivity: Float,
    trained_at: Int,
    status: String
  }
  
  type AnomalyResult = {
    point_id: String,
    model_id: String,
    timestamp: Int,
    value: Float,
    anomaly_score: Float,
    is_anomaly: Bool,
    confidence: Float,
    explanation: String
  }
  
  type AnomalyThreshold = {
    metric_name: String,
    threshold_type: String,
    threshold_value: Float,
    direction: String  // "above", "below", "both"
  }
  
  // Create time series data
  let time_series_data = [
    {
      timestamp: 1640995000,
      value: 25.3,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995060,
      value: 25.1,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995120,
      value: 24.9,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995180,
      value: 25.2,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995240,
      value: 45.8,  // Anomaly - much higher than normal
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995300,
      value: 25.4,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995360,
      value: 25.0,
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    },
    {
      timestamp: 1640995420,
      value: 12.1,  // Anomaly - much lower than normal
      metadata: [
        ("sensor_id", "temp-001"),
        ("location", "warehouse-a"),
        ("unit", "celsius")
      ]
    }
  ]
  
  // Create anomaly detection models
  let anomaly_models = [
    {
      model_id: "model-001",
      name: "Temperature Statistical Model",
      algorithm: "statistical",
      parameters: [
        ("window_size", "5"),
        ("std_threshold", "2.5"),
        ("min_samples", "3")
      ],
      sensitivity: 0.8,
      trained_at: 1640994900,
      status: "active"
    },
    {
      model_id: "model-002",
      name: "Temperature Isolation Forest",
      algorithm: "isolation_forest",
      parameters: [
        ("n_estimators", "100"),
        ("contamination", "0.1"),
        ("max_features", "1.0")
      ],
      sensitivity: 0.7,
      trained_at: 1640994900,
      status: "active"
    }
  ]
  
  // Create anomaly thresholds
  let anomaly_thresholds = [
    {
      metric_name: "temperature",
      threshold_type: "static",
      threshold_value: 35.0,
      direction: "above"
    },
    {
      metric_name: "temperature",
      threshold_type: "static",
      threshold_value: 15.0,
      direction: "below"
    }
  ]
  
  // Calculate statistical metrics for time series
  let calculate_statistics = fn(data: Array[TimeSeriesPoint], window_size: Int) {
    let values = data.map(fn(p) { p.value })
    let n = values.length()
    
    if n < window_size {
      // Not enough data points
      {
        mean: 0.0,
        std_dev: 0.0,
        min: 0.0,
        max: 0.0,
        count: n
      }
    } else {
      // Use the most recent window_size points
      let recent_values = values.slice(n - window_size, n)
      let sum = recent_values.reduce(fn(acc, v) { acc + v }, 0.0)
      let mean = sum / recent_values.length().to_float()
      
      let variance = recent_values.reduce(fn(acc, v) {
        let diff = v - mean
        acc + diff * diff
      }, 0.0) / recent_values.length().to_float()
      
      let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
      let min = recent_values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, recent_values[0])
      let max = recent_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, recent_values[0])
      
      {
        mean,
        std_dev,
        min,
        max,
        count: recent_values.length()
      }
    }
  }
  
  // Detect anomalies using statistical method
  let detect_statistical_anomalies = fn(data: Array[TimeSeriesPoint], model: AnomalyDetectionModel, thresholds: Array[AnomalyThreshold]) {
    let window_size = model.parameters.filter(fn(p) { p.0 == "window_size" })[0].1.to_int()
    let std_threshold = model.parameters.filter(fn(p) { p.0 == "std_threshold" })[0].1.to_float()
    let min_samples = model.parameters.filter(fn(p) { p.0 == "min_samples" })[0].1.to_int()
    
    let mut results = []
    
    for i in 0..data.length() {
      if i >= min_samples - 1 {
        let point = data[i]
        let historical_data = data.slice(0, i)  // Data before this point
        let stats = calculate_statistics(historical_data, window_size)
        
        if stats.count > 0 {
          // Calculate z-score
          let z_score = if stats.std_dev > 0.0 {
            (point.value - stats.mean) / stats.std_dev
          } else {
            0.0
          }
          
          // Check against statistical threshold
          let is_statistical_anomaly = z_score.abs() > std_threshold
          
          // Check against static thresholds
          let mut is_static_anomaly = false
          let mut static_explanation = ""
          
          for threshold in thresholds {
            if threshold.metric_name == "temperature" {
              match threshold.direction {
                "above" => {
                  if point.value > threshold.threshold_value {
                    is_static_anomaly = true
                    static_explanation = "Value " + point.value.to_string() + 
                                       " exceeds maximum threshold " + 
                                       threshold.threshold_value.to_string()
                  }
                }
                
                "below" => {
                  if point.value < threshold.threshold_value {
                    is_static_anomaly = true
                    static_explanation = "Value " + point.value.to_string() + 
                                       " below minimum threshold " + 
                                       threshold.threshold_value.to_string()
                  }
                }
                
                _ => {}
              }
            }
          }
          
          let is_anomaly = is_statistical_anomaly or is_static_anomaly
          let anomaly_score = z_score.abs()
          
          let explanation = if is_statistical_anomaly and is_static_anomaly {
            "Statistical anomaly (z-score: " + z_score.to_string() + 
            ") and static threshold violation: " + static_explanation
          } else if is_statistical_anomaly {
            "Statistical anomaly with z-score: " + z_score.to_string()
          } else if is_static_anomaly {
            "Static threshold violation: " + static_explanation
          } else {
            "Normal value"
          }
          
          let result = {
            point_id: "point-" + i.to_string(),
            model_id: model.model_id,
            timestamp: point.timestamp,
            value: point.value,
            anomaly_score,
            is_anomaly,
            confidence: if is_anomaly { 0.9 } else { 0.1 },
            explanation
          }
          
          results = results.push(result)
        }
      }
    }
    
    results
  }
  
  // Detect anomalies using isolation forest method (simplified)
  let detect_isolation_forest_anomalies = fn(data: Array[TimeSeriesPoint], model: AnomalyDetectionModel) {
    let contamination = model.parameters.filter(fn(p) { p.0 == "contamination" })[0].1.to_float()
    
    // Simplified isolation forest implementation
    // In a real implementation, this would use actual isolation forest algorithm
    let values = data.map(fn(p) { p.value })
    let n = values.length()
    
    // Calculate mean and standard deviation for simplified anomaly detection
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let mean = sum / n.to_float()
    
    let variance = values.reduce(fn(acc, v) {
      let diff = v - mean
      acc + diff * diff
    }, 0.0) / n.to_float()
    
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    let mut results = []
    
    for i in 0..data.length() {
      let point = data[i]
      
      // Simplified anomaly score based on distance from mean
      let anomaly_score = if std_dev > 0.0 {
        (point.value - mean).abs() / (3.0 * std_dev)  // Normalize to 0-1 range
      } else {
        0.0
      }
      
      // Determine if it's an anomaly based on contamination rate
      let is_anomaly = anomaly_score > contamination
      
      let explanation = if is_anomaly {
        "Isolation forest detected anomaly with score: " + anomaly_score.to_string()
      } else {
        "Normal value with isolation forest score: " + anomaly_score.to_string()
      }
      
      let result = {
        point_id: "point-" + i.to_string(),
        model_id: model.model_id,
        timestamp: point.timestamp,
        value: point.value,
        anomaly_score,
        is_anomaly,
        confidence: if is_anomaly { 0.8 } else { 0.2 },
        explanation
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // Test statistical anomaly detection
  let statistical_model = anomaly_models.filter(fn(m) { m.model_id == "model-001" })[0]
  let statistical_results = detect_statistical_anomalies(time_series_data, statistical_model, anomaly_thresholds)
  
  assert_eq(statistical_results.length(), 6)  // We need at least 3 points to start detecting
  
  // Find anomalies
  let statistical_anomalies = statistical_results.filter(fn(r) { r.is_anomaly })
  assert_eq(statistical_anomalies.length(), 2)  // High and low temperature anomalies
  
  // Check high temperature anomaly
  let high_temp_anomaly = statistical_anomalies.filter(fn(a) { a.value > 40.0 })[0]
  assert_true(high_temp_anomaly.is_anomaly)
  assert_eq(high_temp_anomaly.value, 45.8)
  assert_true(high_temp_anomaly.explanation.contains("Statistical anomaly"))
  assert_true(high_temp_anomaly.explanation.contains("static threshold violation"))
  
  // Check low temperature anomaly
  let low_temp_anomaly = statistical_anomalies.filter(fn(a) { a.value < 15.0 })[0]
  assert_true(low_temp_anomaly.is_anomaly)
  assert_eq(low_temp_anomaly.value, 12.1)
  assert_true(low_temp_anomaly.explanation.contains("Statistical anomaly"))
  assert_true(low_temp_anomaly.explanation.contains("static threshold violation"))
  
  // Test isolation forest anomaly detection
  let isolation_model = anomaly_models.filter(fn(m) { m.model_id == "model-002" })[0]
  let isolation_results = detect_isolation_forest_anomalies(time_series_data, isolation_model)
  
  assert_eq(isolation_results.length(), 8)  // All points processed
  
  // Find anomalies
  let isolation_anomalies = isolation_results.filter(fn(r) { r.is_anomaly })
  assert_eq(isolation_anomalies.length(), 2)  // High and low temperature anomalies
  
  // Check high temperature anomaly
  let isolation_high_anomaly = isolation_anomalies.filter(fn(a) { a.value > 40.0 })[0]
  assert_true(isolation_high_anomaly.is_anomaly)
  assert_eq(isolation_high_anomaly.value, 45.8)
  assert_true(isolation_high_anomaly.explanation.contains("Isolation forest"))
  
  // Check low temperature anomaly
  let isolation_low_anomaly = isolation_anomalies.filter(fn(a) { a.value < 15.0 })[0]
  assert_true(isolation_low_anomaly.is_anomaly)
  assert_eq(isolation_low_anomaly.value, 12.1)
  assert_true(isolation_low_anomaly.explanation.contains("Isolation forest"))
  
  // Compare results from both models
  let compare_model_results = fn(results1: Array[AnomalyResult], results2: Array[AnomalyResult]) {
    let anomalies1 = results1.filter(fn(r) { r.is_anomaly })
    let anomalies2 = results2.filter(fn(r) { r.is_anomaly })
    
    let common_anomalies = anomalies1.filter(fn(a1) {
      anomalies2.any(fn(a2) { a1.timestamp == a2.timestamp })
    })
    
    let unique_to_model1 = anomalies1.filter(fn(a1) {
      not(anomalies2.any(fn(a2) { a1.timestamp == a2.timestamp }))
    })
    
    let unique_to_model2 = anomalies2.filter(fn(a2) {
      not(anomalies1.any(fn(a1) { a1.timestamp == a2.timestamp }))
    })
    
    {
      total_anomalies_model1: anomalies1.length(),
      total_anomalies_model2: anomalies2.length(),
      common_anomalies: common_anomalies.length(),
      unique_to_model1: unique_to_model1.length(),
      unique_to_model2: unique_to_model2.length()
    }
  }
  
  let comparison = compare_model_results(statistical_results, isolation_results)
  assert_eq(comparison.total_anomalies_model1, 2)
  assert_eq(comparison.total_anomalies_model2, 2)
  assert_eq(comparison.common_anomalies, 2)  // Both models detected the same anomalies
  assert_eq(comparison.unique_to_model1, 0)
  assert_eq(comparison.unique_to_model2, 0)
  
  // Calculate anomaly detection metrics
  let calculate_anomaly_metrics = fn(results: Array[AnomalyResult]) {
    let total_points = results.length()
    let anomaly_count = results.filter(fn(r) { r.is_anomaly }).length()
    let normal_count = total_points - anomaly_count
    
    let anomaly_rate = if total_points > 0 {
      (anomaly_count.to_float() / total_points.to_float()) * 100.0
    } else {
      0.0
    }
    
    let avg_anomaly_score = if anomaly_count > 0 {
      results.filter(fn(r) { r.is_anomaly })
        .reduce(fn(acc, r) { acc + r.anomaly_score }, 0.0) / 
      anomaly_count.to_float()
    } else {
      0.0
    }
    
    let avg_confidence = if total_points > 0 {
      results.reduce(fn(acc, r) { acc + r.confidence }, 0.0) / 
      total_points.to_float()
    } else {
      0.0
    }
    
    {
      total_points,
      anomaly_count,
      normal_count,
      anomaly_rate,
      avg_anomaly_score,
      avg_confidence
    }
  }
  
  let statistical_metrics = calculate_anomaly_metrics(statistical_results)
  assert_eq(statistical_metrics.total_points, 6)
  assert_eq(statistical_metrics.anomaly_count, 2)
  assert_eq(statistical_metrics.normal_count, 4)
  assert_eq(statistical_metrics.anomaly_rate, 33.33)  // 2/6 * 100
  assert_eq(statistical_metrics.avg_confidence, 0.63)  // (4*0.1 + 2*0.9) / 6
  
  let isolation_metrics = calculate_anomaly_metrics(isolation_results)
  assert_eq(isolation_metrics.total_points, 8)
  assert_eq(isolation_metrics.anomaly_count, 2)
  assert_eq(isolation_metrics.normal_count, 6)
  assert_eq(isolation_metrics.anomaly_rate, 25.0)  // 2/8 * 100
  assert_eq(isolation_metrics.avg_confidence, 0.35)  // (6*0.2 + 2*0.8) / 8
}

// Test 2: Multivariate Anomaly Detection
test "multivariate anomaly detection" {
  // Define multivariate data structure
  type MultivariateDataPoint = {
    timestamp: Int,
    features: Array[(String, Float)],
    metadata: Array[(String, String)]
  }
  
  type MultivariateModel = {
    model_id: String,
    name: String,
    algorithm: String,
    feature_names: Array[String],
    parameters: Array[(String, String)],
    sensitivity: Float,
    trained_at: Int,
    status: String
  }
  
  type MultivariateAnomalyResult = {
    point_id: String,
    model_id: String,
    timestamp: Int,
    features: Array[(String, Float)],
    anomaly_score: Float,
    is_anomaly: Bool,
    confidence: Float,
    feature_contributions: Array[(String, Float)],
    explanation: String
  }
  
  // Create multivariate data
  let multivariate_data = [
    {
      timestamp: 1640995000,
      features: [
        ("temperature", 25.3),
        ("humidity", 65.2),
        ("pressure", 1013.25),
        ("cpu_usage", 35.5),
        ("memory_usage", 45.2)
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    },
    {
      timestamp: 1640995060,
      features: [
        ("temperature", 25.1),
        ("humidity", 64.8),
        ("pressure", 1013.15),
        ("cpu_usage", 38.2),
        ("memory_usage", 46.1)
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    },
    {
      timestamp: 1640995120,
      features: [
        ("temperature", 24.9),
        ("humidity", 65.5),
        ("pressure", 1013.35),
        ("cpu_usage", 32.1),
        ("memory_usage", 44.8)
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    },
    {
      timestamp: 1640995180,
      features: [
        ("temperature", 25.2),
        ("humidity", 65.0),
        ("pressure", 1013.20),
        ("cpu_usage", 36.8),
        ("memory_usage", 45.5)
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    },
    {
      timestamp: 1640995240,
      features: [
        ("temperature", 45.8),  // Anomaly: high temperature
        ("humidity", 85.3),    // Anomaly: high humidity
        ("pressure", 1013.10),
        ("cpu_usage", 95.2),   // Anomaly: high CPU
        ("memory_usage", 92.5) // Anomaly: high memory
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    },
    {
      timestamp: 1640995300,
      features: [
        ("temperature", 25.4),
        ("humidity", 64.9),
        ("pressure", 1013.30),
        ("cpu_usage", 37.1),
        ("memory_usage", 45.8)
      ],
      metadata: [
        ("sensor_id", "sensor-001"),
        ("location", "server-room-1")
      ]
    }
  ]
  
  // Create multivariate model
  let multivariate_models = [
    {
      model_id: "multi-model-001",
      name: "Server Room Multivariate Model",
      algorithm: "mahalanobis_distance",
      feature_names: ["temperature", "humidity", "pressure", "cpu_usage", "memory_usage"],
      parameters: [
        ("threshold_percentile", "95"),
        ("min_samples", "3")
      ],
      sensitivity: 0.8,
      trained_at: 1640994900,
      status: "active"
    }
  ]
  
  // Calculate multivariate statistics
  let calculate_multivariate_statistics = fn(data: Array[MultivariateDataPoint], feature_names: Array[String]) {
    let n = data.length()
    
    if n < 3 {
      // Not enough data
      {
        means: [],
        covariances: [],
        count: n
      }
    } else {
      // Calculate means for each feature
      let means = feature_names.map(fn(feature) {
        let values = data.map(fn(p) {
          p.features.filter(fn(f) { f.0 == feature })[0].1
        })
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        sum / values.length().to_float()
      })
      
      // Calculate covariances (simplified - just diagonal elements)
      let covariances = feature_names.map(fn(feature) {
        let values = data.map(fn(p) {
          p.features.filter(fn(f) { f.0 == feature })[0].1
        })
        let mean = means[feature_names.index_of(feature).unwrap_or(0)]
        
        let variance = values.reduce(fn(acc, v) {
          let diff = v - mean
          acc + diff * diff
        }, 0.0) / values.length().to_float()
        
        variance
      })
      
      {
        means,
        covariances,
        count: n
      }
    }
  }
  
  // Calculate Mahalanobis distance (simplified)
  let calculate_mahalanobis_distance = fn(point: MultivariateDataPoint, means: Array[Float], covariances: Array[Float], feature_names: Array[String]) {
    let mut distance_squared = 0.0
    
    for i in 0..feature_names.length() {
      let feature = feature_names[i]
      let value = point.features.filter(fn(f) { f.0 == feature })[0].1
      let mean = means[i]
      let variance = covariances[i]
      
      if variance > 0.0 {
        let standardized_diff = (value - mean) / variance.sqrt()
        distance_squared = distance_squared + standardized_diff * standardized_diff
      }
    }
    
    distance_squared.sqrt()
  }
  
  // Detect multivariate anomalies
  let detect_multivariate_anomalies = fn(data: Array[MultivariateDataPoint], model: MultivariateModel) {
    let threshold_percentile = model.parameters.filter(fn(p) { p.0 == "threshold_percentile" })[0].1.to_float()
    let min_samples = model.parameters.filter(fn(p) { p.0 == "min_samples" })[0].1.to_int()
    
    let mut results = []
    
    for i in 0..data.length() {
      if i >= min_samples - 1 {
        let point = data[i]
        let historical_data = data.slice(0, i)
        let stats = calculate_multivariate_statistics(historical_data, model.feature_names)
        
        if stats.count >= min_samples {
          // Calculate Mahalanobis distance
          let distance = calculate_mahalanobis_distance(point, stats.means, stats.covariances, model.feature_names)
          
          // Calculate feature contributions
          let feature_contributions = model.feature_names.map(fn(feature) {
            let value = point.features.filter(fn(f) { f.0 == feature })[0].1
            let mean = stats.means[model.feature_names.index_of(feature).unwrap_or(0)]
            let variance = stats.covariances[model.feature_names.index_of(feature).unwrap_or(0)]
            
            let contribution = if variance > 0.0 {
              ((value - mean) / variance.sqrt()).abs()
            } else {
              0.0
            }
            
            (feature, contribution)
          })
          
          // Normalize threshold based on percentile
          // In a real implementation, we would calculate the actual percentile from historical distances
          let threshold = threshold_percentile / 100.0 * 10.0  // Simplified threshold calculation
          
          let is_anomaly = distance > threshold
          let confidence = if is_anomaly { 
            (distance / threshold - 1.0).min(1.0).max(0.0) 
          } else { 
            (1.0 - distance / threshold).min(1.0).max(0.0) 
          }
          
          let explanation = if is_anomaly {
            "Multivariate anomaly detected with Mahalanobis distance: " + distance.to_string()
          } else {
            "Normal multivariate pattern with distance: " + distance.to_string()
          }
          
          let result = {
            point_id: "multi-point-" + i.to_string(),
            model_id: model.model_id,
            timestamp: point.timestamp,
            features: point.features,
            anomaly_score: distance,
            is_anomaly,
            confidence,
            feature_contributions,
            explanation
          }
          
          results = results.push(result)
        }
      }
    }
    
    results
  }
  
  // Test multivariate anomaly detection
  let multivariate_model = multivariate_models[0]
  let multivariate_results = detect_multivariate_anomalies(multivariate_data, multivariate_model)
  
  assert_eq(multivariate_results.length(), 4)  // Need at least 3 points to start detecting
  
  // Find anomalies
  let multivariate_anomalies = multivariate_results.filter(fn(r) { r.is_anomaly })
  assert_eq(multivariate_anomalies.length(), 1)  // Only the point with multiple anomalies
  
  // Check the anomaly
  let anomaly_result = multivariate_anomalies[0]
  assert_true(anomaly_result.is_anomaly)
  assert_eq(anomaly_result.timestamp, 1640995240)
  assert_true(anomaly_result.explanation.contains("Multivariate anomaly detected"))
  
  // Check feature contributions
  let temp_contribution = anomaly_result.feature_contributions.filter(fn(fc) { fc.0 == "temperature" })[0]
  assert_true(temp_contribution.1 > 0.0)  // Temperature contributed to anomaly
  
  let cpu_contribution = anomaly_result.feature_contributions.filter(fn(fc) { fc.0 == "cpu_usage" })[0]
  assert_true(cpu_contribution.1 > 0.0)  // CPU usage contributed to anomaly
  
  // Find the most contributing feature
  let sorted_contributions = anomaly_result.feature_contributions.sort(fn(a, b) { b.1 - a.1 })
  let top_contributor = sorted_contributions[0]
  assert_true(top_contributor.1 > 0.0)
  
  // Calculate multivariate anomaly metrics
  let calculate_multivariate_metrics = fn(results: Array[MultivariateAnomalyResult]) {
    let total_points = results.length()
    let anomaly_count = results.filter(fn(r) { r.is_anomaly }).length()
    let normal_count = total_points - anomaly_count
    
    let anomaly_rate = if total_points > 0 {
      (anomaly_count.to_float() / total_points.to_float()) * 100.0
    } else {
      0.0
    }
    
    let avg_anomaly_score = if anomaly_count > 0 {
      results.filter(fn(r) { r.is_anomaly })
        .reduce(fn(acc, r) { acc + r.anomaly_score }, 0.0) / 
      anomaly_count.to_float()
    } else {
      0.0
    }
    
    let avg_confidence = if total_points > 0 {
      results.reduce(fn(acc, r) { acc + r.confidence }, 0.0) / 
      total_points.to_float()
    } else {
      0.0
    }
    
    // Calculate average feature contributions for anomalies
    let avg_feature_contributions = if anomaly_count > 0 {
      let anomaly_results = results.filter(fn(r) { r.is_anomaly })
      let feature_names = anomaly_results[0].feature_contributions.map(fn(fc) { fc.0 })
      
      feature_names.map(fn(feature) {
        let contributions = anomaly_results.map(fn(r) {
          r.feature_contributions.filter(fn(fc) { fc.0 == feature })[0].1
        })
        
        let avg_contribution = contributions.reduce(fn(acc, c) { acc + c }, 0.0) / 
                              contributions.length().to_float()
        
        (feature, avg_contribution)
      })
    } else {
      []
    }
    
    {
      total_points,
      anomaly_count,
      normal_count,
      anomaly_rate,
      avg_anomaly_score,
      avg_confidence,
      avg_feature_contributions
    }
  }
  
  let multivariate_metrics = calculate_multivariate_metrics(multivariate_results)
  assert_eq(multivariate_metrics.total_points, 4)
  assert_eq(multivariate_metrics.anomaly_count, 1)
  assert_eq(multivariate_metrics.normal_count, 3)
  assert_eq(multivariate_metrics.anomaly_rate, 25.0)  // 1/4 * 100
  assert_eq(multivariate_metrics.avg_feature_contributions.length(), 5)  // 5 features
}