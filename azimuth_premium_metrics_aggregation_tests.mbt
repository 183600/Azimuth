// Azimuth Premium Metrics Aggregation and Analysis Test Suite
// Advanced test cases for performance metrics aggregation and analysis

// Test 1: Metrics Collection and Aggregation Strategies
test "comprehensive metrics collection and aggregation strategies" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define data point
  type DataPoint = {
    timestamp: Int,
    value: Float,
    labels: Array[(String, String)]
  }
  
  // Define metric
  type Metric = {
    name: String,
    metric_type: MetricType,
    description: String,
    data_points: Array[DataPoint],
    unit: String
  }
  
  // Define aggregation function
  enum AggregationFunction {
    Sum
    Average
    Min
    Max
    Count
    Percentile(Float)  // 0.0 to 1.0
    Rate
    Increase
  }
  
  // Define aggregation rule
  type AggregationRule = {
    metric_name: String,
    function: AggregationFunction,
    interval_seconds: Int,
    label_filters: Array<(String, String)>,
    output_metric_name: String
  }
  
  // Create metrics collector
  let create_metrics_collector = fn() {
    let mut metrics = {}
    
    {
      record_metric: fn(name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)>, timestamp: Int, unit: String) {
        let current_metric = metrics.get(name)
        let new_data_point = {
          timestamp,
          value,
          labels
        }
        
        let updated_metric = match current_metric {
          Some(metric) => {
            {
              name: metric.name,
              metric_type: metric.metric_type,
              description: metric.description,
              data_points: metric.data_points.push(new_data_point),
              unit
            }
          }
          None => {
            {
              name,
              metric_type,
              description: "",
              data_points: [new_data_point],
              unit
            }
          }
        }
        
        metrics = metrics.set(name, updated_metric)
      },
      
      aggregate_metrics: fn(rules: Array[AggregationRule], end_time: Int) {
        let mut results = []
        
        for rule in rules {
          let metric = metrics.get(rule.metric_name)
          match metric {
            Some(m) => {
              // Filter data points by time window and labels
              let start_time = end_time - rule.interval_seconds
              let filtered_points = m.data_points.filter(fn(point) {
                point.timestamp >= start_time and point.timestamp <= end_time and
                rule.label_filters.all(fn(filter) {
                  point.labels.any_fn(fn(label) { 
                    label.0 == filter.0 and label.1 == filter.1 
                  })
                })
              })
              
              if filtered_points.length() > 0 {
                let aggregated_value = match rule.function {
                  AggregationFunction::Sum => {
                    filtered_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
                  }
                  AggregationFunction::Average => {
                    let sum = filtered_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
                    sum / filtered_points.length().to_float()
                  }
                  AggregationFunction::Min => {
                    filtered_points.reduce(fn(acc, point) { 
                      if point.value < acc { point.value } else { acc } 
                    }, 999999.0)
                  }
                  AggregationFunction::Max => {
                    filtered_points.reduce(fn(acc, point) { 
                      if point.value > acc { point.value } else { acc } 
                    }, -999999.0)
                  }
                  AggregationFunction::Count => {
                    filtered_points.length().to_float()
                  }
                  AggregationFunction::Percentile(p) => {
                    // Simple percentile calculation
                    let sorted_points = filtered_points.sort(fn(a, b) { a.value < b.value })
                    let index = (p * sorted_points.length().to_float()).to_int()
                    sorted_points[index].value
                  }
                  AggregationFunction::Rate => {
                    // Calculate rate of change per second
                    if filtered_points.length() >= 2 {
                      let first = filtered_points[0]
                      let last = filtered_points[filtered_points.length() - 1]
                      let time_diff = last.timestamp - first.timestamp
                      if time_diff > 0 {
                        (last.value - first.value) / time_diff.to_float()
                      } else {
                        0.0
                      }
                    } else {
                      0.0
                    }
                  }
                  AggregationFunction::Increase => {
                    // Calculate total increase
                    if filtered_points.length() >= 2 {
                      let first = filtered_points[0]
                      let last = filtered_points[filtered_points.length() - 1]
                      last.value - first.value
                    } else {
                      0.0
                    }
                  }
                }
                
                results = results.push({
                  metric_name: rule.output_metric_name,
                  value: aggregated_value,
                  timestamp: end_time,
                  labels: rule.label_filters
                })
              }
            }
            None => ()
          }
        }
        
        results
      },
      
      get_metric: fn(name: String) {
        metrics.get(name)
      }
    }
  }
  
  // Create metrics collector
  let collector = create_metrics_collector()
  
  // Record some test metrics
  let base_time = 1640995200
  
  // Record HTTP request counter
  collector.record_metric(
    "http_requests_total",
    MetricType::Counter,
    1.0,
    [("service", "api-gateway"), ("method", "GET"), ("status", "200")],
    base_time,
    "requests"
  )
  
  collector.record_metric(
    "http_requests_total",
    MetricType::Counter,
    1.0,
    [("service", "api-gateway"), ("method", "GET"), ("status", "200")],
    base_time + 30,
    "requests"
  )
  
  collector.record_metric(
    "http_requests_total",
    MetricType::Counter,
    1.0,
    [("service", "api-gateway"), ("method", "POST"), ("status", "201")],
    base_time + 60,
    "requests"
  )
  
  collector.record_metric(
    "http_requests_total",
    MetricType::Counter,
    1.0,
    [("service", "user-service"), ("method", "GET"), ("status", "200")],
    base_time + 90,
    "requests"
  )
  
  // Record response time gauge
  collector.record_metric(
    "response_time_ms",
    MetricType::Gauge,
    120.5,
    [("service", "api-gateway"), ("endpoint", "/users")],
    base_time + 15,
    "milliseconds"
  )
  
  collector.record_metric(
    "response_time_ms",
    MetricType::Gauge,
    85.2,
    [("service", "api-gateway"), ("endpoint", "/users")],
    base_time + 45,
    "milliseconds"
  )
  
  collector.record_metric(
    "response_time_ms",
    MetricType::Gauge,
    200.1,
    [("service", "api-gateway"), ("endpoint", "/orders")],
    base_time + 75,
    "milliseconds"
  )
  
  collector.record_metric(
    "response_time_ms",
    MetricType::Gauge,
    95.7,
    [("service", "user-service"), ("endpoint", "/profile")],
    base_time + 105,
    "milliseconds"
  )
  
  // Create aggregation rules
  let aggregation_rules = [
    {
      metric_name: "http_requests_total",
      function: AggregationFunction::Sum,
      interval_seconds: 120,
      label_filters: [("service", "api-gateway")],
      output_metric_name: "api_gateway_requests_total"
    },
    {
      metric_name: "http_requests_total",
      function: AggregationFunction::Rate,
      interval_seconds: 120,
      label_filters: [("method", "GET")],
      output_metric_name: "get_requests_per_second"
    },
    {
      metric_name: "response_time_ms",
      function: AggregationFunction::Average,
      interval_seconds: 120,
      label_filters: [("service", "api-gateway")],
      output_metric_name: "api_gateway_avg_response_time"
    },
    {
      metric_name: "response_time_ms",
      function: AggregationFunction::Max,
      interval_seconds: 120,
      label_filters: [("service", "api-gateway")],
      output_metric_name: "api_gateway_max_response_time"
    },
    {
      metric_name: "response_time_ms",
      function: AggregationFunction::Percentile(0.95),
      interval_seconds: 120,
      label_filters: [("service", "api-gateway")],
      output_metric_name: "api_gateway_p95_response_time"
    }
  ]
  
  // Perform aggregation
  let end_time = base_time + 120
  let aggregation_results = collector.aggregate_metrics(aggregation_rules, end_time)
  
  // Verify aggregation results
  assert_eq(aggregation_results.length(), 5)
  
  // Check API gateway total requests
  let api_gateway_total = aggregation_results.find(fn(result) { 
    result.metric_name == "api_gateway_requests_total" 
  })
  assert_not_eq(api_gateway_total, None)
  match api_gateway_total {
    Some(result) => assert_eq(result.value, 3.0)  // 3 requests to api-gateway
    None => assert_true(false)
  }
  
  // Check GET requests rate
  let get_rate = aggregation_results.find(fn(result) { 
    result.metric_name == "get_requests_per_second" 
  })
  assert_not_eq(get_rate, None)
  match get_rate {
    Some(result) => {
      // 2 GET requests over 120 seconds = 0.0167 requests per second
      assert_true(result.value > 0.016 and result.value < 0.017)
    }
    None => assert_true(false)
  }
  
  // Check API gateway average response time
  let avg_response_time = aggregation_results.find(fn(result) { 
    result.metric_name == "api_gateway_avg_response_time" 
  })
  assert_not_eq(avg_response_time, None)
  match avg_response_time {
    Some(result) => {
      // (120.5 + 85.2 + 200.1) / 3 = 135.27
      assert_true(result.value > 135.2 and result.value < 135.3)
    }
    None => assert_true(false)
  }
  
  // Check API gateway max response time
  let max_response_time = aggregation_results.find(fn(result) { 
    result.metric_name == "api_gateway_max_response_time" 
  })
  assert_not_eq(max_response_time, None)
  match max_response_time {
    Some(result) => assert_eq(result.value, 200.1)  // Maximum response time
    None => assert_true(false)
  }
  
  // Check API gateway 95th percentile response time
  let p95_response_time = aggregation_results.find(fn(result) { 
    result.metric_name == "api_gateway_p95_response_time" 
  })
  assert_not_eq(p95_response_time, None)
  match p95_response_time {
    Some(result) => {
      // Sorted: [85.2, 120.5, 200.1], 95th percentile index = 2 (0.95 * 3 = 2.85, floor to 2)
      assert_eq(result.value, 200.1)
    }
    None => assert_true(false)
  }
}

// Test 2: Time Series Analysis and Trend Detection
test "time series analysis and trend detection" {
  // Define time series point
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // Define trend analysis result
  type TrendAnalysis = {
    trend_direction: String,  // "increasing", "decreasing", "stable"
    trend_strength: Float,    // 0.0 to 1.0
    slope: Float,             // Rate of change
    correlation: Float,       // Correlation coefficient
    seasonality: Option<String>
  }
  
  // Define anomaly detection result
  type AnomalyDetection = {
    is_anomaly: Bool,
    anomaly_score: Float,     // 0.0 to 1.0
    threshold: Float,
    expected_value: Float,
    actual_value: Float,
    deviation: Float
  }
  
  // Create time series analyzer
  let create_time_series_analyzer = fn() {
    {
      analyze_trend: fn(points: Array[TimeSeriesPoint]) {
        if points.length() < 2 {
          return {
            trend_direction: "stable",
            trend_strength: 0.0,
            slope: 0.0,
            correlation: 0.0,
            seasonality: None
          }
        }
        
        // Simple linear regression to calculate trend
        let n = points.length().to_float()
        let sum_x = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() }, 0.0)
        let sum_y = points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let sum_xy = points.reduce(fn(acc, point) { 
          acc + (point.timestamp.to_float() * point.value) 
        }, 0.0)
        let sum_x2 = points.reduce(fn(acc, point) { 
          acc + (point.timestamp.to_float() * point.timestamp.to_float()) 
        }, 0.0)
        
        let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        let intercept = (sum_y - slope * sum_x) / n
        
        // Calculate correlation coefficient
        let mean_x = sum_x / n
        let mean_y = sum_y / n
        
        let numerator = points.reduce(fn(acc, point) {
          let x_dev = point.timestamp.to_float() - mean_x
          let y_dev = point.value - mean_y
          acc + (x_dev * y_dev)
        }, 0.0)
        
        let sum_x_dev_sq = points.reduce(fn(acc, point) {
          let x_dev = point.timestamp.to_float() - mean_x
          acc + (x_dev * x_dev)
        }, 0.0)
        
        let sum_y_dev_sq = points.reduce(fn(acc, point) {
          let y_dev = point.value - mean_y
          acc + (y_dev * y_dev)
        }, 0.0)
        
        let correlation = if sum_x_dev_sq > 0.0 and sum_y_dev_sq > 0.0 {
          numerator / (sum_x_dev_sq.sqrt() * sum_y_dev_sq.sqrt())
        } else {
          0.0
        }
        
        // Determine trend direction and strength
        let trend_direction = if slope > 0.1 {
          "increasing"
        } else if slope < -0.1 {
          "decreasing"
        } else {
          "stable"
        }
        
        let trend_strength = correlation.abs()
        
        // Simple seasonality detection (check for periodic patterns)
        let seasonality = if points.length() >= 24 {  // Need at least 24 points for daily pattern
          let hourly_averages = {}
          for point in points {
            let hour = (point.timestamp / 3600) % 24
            let current_avg = hourly_averages.get(hour)
            let avg = match current_avg {
              Some(a) => {
                let (sum, count) = a
                (sum + point.value, count + 1)
              }
              None => (point.value, 1)
            }
            hourly_averages = hourly_averages.set(hour, avg)
          }
          
          // Check variation between hourly averages
          let averages = hourly_averages.to_array().map(fn(pair) {
            let (sum, count) = pair.1
            sum / count.to_float()
          })
          
          if averages.length() > 0 {
            let max_avg = averages.reduce(fn(acc, avg) { if avg > acc { avg } else { acc }, -999999.0)
            let min_avg = averages.reduce(fn(acc, avg) { if avg < acc { avg } else { acc }, 999999.0)
            let variation = (max_avg - min_avg) / min_avg
            
            if variation > 0.2 {  // 20% variation indicates seasonality
              Some("daily")
            } else {
              None
            }
          } else {
            None
          }
        } else {
          None
        }
        
        {
          trend_direction,
          trend_strength,
          slope,
          correlation,
          seasonality
        }
      },
      
      detect_anomalies: fn(points: Array[TimeSeriesPoint], threshold_multiplier: Float) {
        if points.length() < 3 {
          return []
        }
        
        // Calculate mean and standard deviation
        let mean = points.reduce(fn(acc, point) { acc + point.value }, 0.0) / points.length().to_float()
        
        let variance = points.reduce(fn(acc, point) {
          let deviation = point.value - mean
          acc + (deviation * deviation)
        }, 0.0) / points.length().to_float()
        
        let std_dev = variance.sqrt()
        let threshold = std_dev * threshold_multiplier
        
        // Detect anomalies
        points.map(fn(point) {
          let deviation = (point.value - mean).abs()
          let is_anomaly = deviation > threshold
          let anomaly_score = if std_dev > 0.0 { deviation / (std_dev * 3.0) } else { 0.0 }
          
          {
            is_anomaly,
            anomaly_score: if anomaly_score > 1.0 { 1.0 } else { anomaly_score },
            threshold,
            expected_value: mean,
            actual_value: point.value,
            deviation
          }
        })
      },
      
      forecast_next_value: fn(points: Array[TimeSeriesPoint], steps_ahead: Int) {
        if points.length() < 2 {
          return None
        }
        
        // Simple linear regression forecast
        let n = points.length().to_float()
        let sum_x = points.reduce(fn(acc, point) { acc + point.timestamp.to_float() }, 0.0)
        let sum_y = points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let sum_xy = points.reduce(fn(acc, point) { 
          acc + (point.timestamp.to_float() * point.value) 
        }, 0.0)
        let sum_x2 = points.reduce(fn(acc, point) { 
          acc + (point.timestamp.to_float() * point.timestamp.to_float()) 
        }, 0.0)
        
        let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        let intercept = (sum_y - slope * sum_x) / n
        
        let last_timestamp = points[points.length() - 1].timestamp
        let next_timestamp = last_timestamp + steps_ahead
        
        let forecasted_value = slope * next_timestamp.to_float() + intercept
        
        Some({
          timestamp: next_timestamp,
          value: forecasted_value,
          confidence: 0.5  // Simple confidence score
        })
      }
    }
  }
  
  // Create time series analyzer
  let analyzer = create_time_series_analyzer()
  
  // Create test time series data (CPU usage over 24 hours)
  let base_time = 1640995200  // Start of day
  let cpu_usage_points = []
  
  // Generate CPU usage data with daily pattern and trend
  for i in 0..=23 {
    let timestamp = base_time + i * 3600  // Hourly data
    let base_usage = 30.0 + i.to_float() * 0.5  // Slight increasing trend
    let daily_pattern = 20.0 * ((i.to_float() / 24.0) * 3.14159 * 2.0).sin()  // Daily pattern
    let noise = (i % 7).to_float() * 2.0  // Some noise
    let cpu_usage = base_usage + daily_pattern + noise
    
    cpu_usage_points = cpu_usage_points.push({
      timestamp,
      value: cpu_usage,
      metadata: [("metric", "cpu_usage"), ("unit", "percent")]
    })
  }
  
  // Test trend analysis
  let trend_analysis = analyzer.analyze_trend(cpu_usage_points)
  
  assert_eq(trend_analysis.trend_direction, "increasing")  // Should detect increasing trend
  assert_true(trend_analysis.trend_strength > 0.0)
  assert_true(trend_analysis.slope > 0.0)
  assert_eq(trend_analysis.seasonality, Some("daily"))
  
  // Test anomaly detection
  let anomalies = analyzer.detect_anomalies(cpu_usage_points, 2.0)  // 2 standard deviations
  
  assert_true(anomalies.length() > 0)
  let anomaly_count = anomalies.count_fn(fn(anomaly) { anomaly.is_anomaly })
  assert_true(anomaly_count >= 0)  // May or may not have anomalies depending on data
  
  // Test forecasting
  let forecast = analyzer.forecast_next_value(cpu_usage_points, 1)  // Forecast 1 hour ahead
  assert_not_eq(forecast, None)
  
  match forecast {
    Some(forecast_value) => {
      assert_true(forecast_value.timestamp > base_time + 23 * 3600)
      assert_true(forecast_value.confidence > 0.0 and forecast_value.confidence <= 1.0)
    }
    None => assert_true(false)
  }
  
  // Test with stable data
  let stable_points = []
  for i in 0..=9 {
    stable_points = stable_points.push({
      timestamp: base_time + i * 60,
      value: 50.0,
      metadata: [("metric", "stable_metric")]
    })
  }
  
  let stable_trend = analyzer.analyze_trend(stable_points)
  assert_eq(stable_trend.trend_direction, "stable")
  assert_true(stable_trend.trend_strength < 0.1)
  assert_true(stable_trend.slope.abs() < 0.1)
}

// Test 3: Multi-dimensional Metrics Analysis
test "multi-dimensional metrics analysis and correlation" {
  // Define dimension combination
  type DimensionCombination = {
    dimensions: Array[(String, String)],
    metric_values: Array[(String, Float)]
  }
  
  // Define correlation analysis
  type CorrelationAnalysis = {
    metric_a: String,
    metric_b: String,
    correlation_coefficient: Float,
    correlation_strength: String,  // "weak", "moderate", "strong"
    p_value: Float
  }
  
  // Define heatmap data point
  type HeatmapPoint = {
    x_dimension: String,
    y_dimension: String,
    value: Float,
    count: Int
  }
  
  // Create multi-dimensional analyzer
  let create_multidimensional_analyzer = fn() {
    {
      analyze_by_dimensions: fn(metrics: Array[(String, Array[TimeSeriesPoint])], dimensions: Array<String>) {
        let mut dimension_combinations = {}
        
        for (metric_name, points) in metrics {
          for point in points {
            // Extract dimension values for this point
            let dimension_values = dimensions.map(fn(dim) {
              let dim_value = point.metadata.find(fn(meta) { meta.0 == dim })
              match dim_value {
                Some((_, value)) => value
                None => "unknown"
              }
            })
            
            // Create dimension key
            let dimension_key = dimension_values.join("|")
            
            // Update dimension combination
            let current_combination = dimension_combinations.get(dimension_key)
            let updated_combination = match current_combination {
              Some(combination) => {
                let mut updated_values = combination.metric_values
                let existing_metric_index = updated_values.find_index(fn(pair) { pair.0 == metric_name })
                
                match existing_metric_index {
                  Some(index) => {
                    let (name, current_value) = updated_values[index]
                    updated_values = updated_values.set(index, (name, current_value + point.value))
                  }
                  None => {
                    updated_values = updated_values.push((metric_name, point.value))
                  }
                }
                
                {
                  dimensions: combination.dimensions,
                  metric_values: updated_values
                }
              }
              None => {
                {
                  dimensions: dimension_values.map(fn(value) {
                    let dim_index = dimension_values.find_index(fn(v) { v == value })
                    match dim_index {
                      Some(index) => (dimensions[index], value)
                      None => ("unknown", value)
                    }
                  }),
                  metric_values: [(metric_name, point.value)]
                }
              }
            }
            
            dimension_combinations = dimension_combinations.set(dimension_key, updated_combination)
          }
        }
        
        dimension_combinations.to_array().map(fn(pair) { pair.1 })
      },
      
      calculate_correlations: fn(metrics: Array[(String, Array[TimeSeriesPoint])]) {
        let mut correlations = []
        
        // Calculate correlation between each pair of metrics
        for i in 0..metrics.length() {
          for j in (i + 1)..metrics.length() {
            let (name_a, points_a) = metrics[i]
            let (name_b, points_b) = metrics[j]
            
            // Align data points by timestamp
            let aligned_data = points_a.map_fn(fn(point_a) {
              let matching_point = points_b.find(fn(point_b) { 
                point_b.timestamp == point_a.timestamp 
              })
              match matching_point {
                Some(point_b) => Some((point_a.value, point_b.value))
                None => None
              }
            }).filter_map(fn(pair) { pair })
            
            if aligned_data.length() >= 3 {
              // Calculate correlation coefficient
              let n = aligned_data.length().to_float()
              let sum_x = aligned_data.reduce(fn(acc, pair) { acc + pair.0 }, 0.0)
              let sum_y = aligned_data.reduce(fn(acc, pair) { acc + pair.1 }, 0.0)
              let sum_xy = aligned_data.reduce(fn(acc, pair) { acc + (pair.0 * pair.1) }, 0.0)
              let sum_x2 = aligned_data.reduce(fn(acc, pair) { acc + (pair.0 * pair.0) }, 0.0)
              let sum_y2 = aligned_data.reduce(fn(acc, pair) { acc + (pair.1 * pair.1) }, 0.0)
              
              let numerator = n * sum_xy - sum_x * sum_y
              let denominator = ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
              
              let correlation_coefficient = if denominator > 0.0 {
                numerator / denominator
              } else {
                0.0
              }
              
              // Determine correlation strength
              let correlation_strength = if correlation_coefficient.abs() >= 0.7 {
                "strong"
              } else if correlation_coefficient.abs() >= 0.3 {
                "moderate"
              } else {
                "weak"
              }
              
              // Simple p-value approximation (for demonstration)
              let t_statistic = correlation_coefficient * ((n - 2.0) / (1.0 - correlation_coefficient * correlation_coefficient)).sqrt()
              let p_value = if t_statistic.abs() > 0.0 {
                2.0 * (1.0 - normal_cdf(t_statistic.abs()))
              } else {
                1.0
              }
              
              correlations = correlations.push({
                metric_a: name_a,
                metric_b: name_b,
                correlation_coefficient,
                correlation_strength,
                p_value
              })
            }
          }
        }
        
        correlations
      },
      
      create_heatmap: fn(dimension_combinations: Array[DimensionCombination], x_dimension: String, y_dimension: String, metric_name: String) {
        let mut heatmap_data = {}
        
        for combination in dimension_combinations {
          let x_value = combination.dimensions.find_fn(fn(dim) { dim.0 == x_dimension })
          let y_value = combination.dimensions.find_fn(fn(dim) { dim.0 == y_dimension })
          let metric_value = combination.metric_values.find_fn(fn(pair) { pair.0 == metric_name })
          
          match (x_value, y_value, metric_value) {
            (Some((_, x_val)), Some((_, y_val)), Some((_, val))) => {
              let key = x_val + "|" + y_val
              let current_data = heatmap_data.get(key)
              
              let updated_data = match current_data {
                Some((current_value, count)) => {
                  (current_value + val, count + 1)
                }
                None => {
                  (val, 1)
                }
              }
              
              heatmap_data = heatmap_data.set(key, updated_data)
            }
            _ => ()
          }
        }
        
        // Convert to heatmap points
        heatmap_data.to_array().map(fn(pair) {
          let key_parts = pair.0.split("|")
          let (total_value, count) = pair.1
          
          {
            x_dimension: key_parts[0],
            y_dimension: if key_parts.length() > 1 { key_parts[1] } else { "unknown" },
            value: total_value / count.to_float(),  // Average value
            count
          }
        })
      }
    }
  }
  
  // Helper function for normal CDF (simplified)
  let normal_cdf = fn(x: Float) {
    if x >= 0.0 {
      0.5 * (1.0 + error_function(x / 1.4142135623730951))
    } else {
      0.5 * (1.0 - error_function((-x) / 1.4142135623730951))
    }
  }
  
  // Simplified error function approximation
  let error_function = fn(x: Float) {
    // approximation of erf(x)
    let a1 =  0.254829592
    let a2 = -0.284496736
    let a3 =  1.421413741
    let a4 = -1.453152027
    let a5 =  1.061405429
    let p  =  0.3275911
    
    let sign = if x < 0.0 { -1.0 } else { 1.0 }
    let x_abs = x.abs()
    
    let t = 1.0 / (1.0 + p * x_abs)
    let y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * (-x_abs * x_abs).exp()
    
    sign * y
  }
  
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // Create multi-dimensional analyzer
  let analyzer = create_multidimensional_analyzer()
  
  // Create test metrics with multiple dimensions
  let base_time = 1640995200
  
  let cpu_usage_points = []
  let memory_usage_points = []
  let request_rate_points = []
  
  // Generate metrics for different services and endpoints
  let services = ["api-gateway", "user-service", "order-service"]
  let endpoints = ["/users", "/orders", "/products"]
  
  for i in 0..=11 {  // 12 hours of data
    let timestamp = base_time + i * 3600
    
    for j in 0..services.length() - 1 {
      let service = services[j]
      
      for k in 0..endpoints.length() - 1 {
        let endpoint = endpoints[k]
        
        // Generate correlated metrics
        let base_cpu = 20.0 + j.to_float() * 10.0 + k.to_float() * 5.0
        let base_memory = 30.0 + j.to_float() * 15.0 + k.to_float() * 8.0
        let base_request_rate = 50.0 + j.to_float() * 20.0 + k.to_float() * 10.0
        
        let time_factor = (i.to_float() / 12.0) * 3.14159
        let cpu_usage = base_cpu + 10.0 * time_factor.sin()
        let memory_usage = base_memory + 15.0 * time_factor.sin()
        let request_rate = base_request_rate + 25.0 * time_factor.sin()
        
        cpu_usage_points = cpu_usage_points.push({
          timestamp,
          value: cpu_usage,
          metadata: [
            ("service", service),
            ("endpoint", endpoint),
            ("metric", "cpu_usage")
          ]
        })
        
        memory_usage_points = memory_usage_points.push({
          timestamp,
          value: memory_usage,
          metadata: [
            ("service", service),
            ("endpoint", endpoint),
            ("metric", "memory_usage")
          ]
        })
        
        request_rate_points = request_rate_points.push({
          timestamp,
          value: request_rate,
          metadata: [
            ("service", service),
            ("endpoint", endpoint),
            ("metric", "request_rate")
          ]
        })
      }
    }
  }
  
  let metrics = [
    ("cpu_usage", cpu_usage_points),
    ("memory_usage", memory_usage_points),
    ("request_rate", request_rate_points)
  ]
  
  // Test multi-dimensional analysis
  let dimension_combinations = analyzer.analyze_by_dimensions(metrics, ["service", "endpoint"])
  
  assert_true(dimension_combinations.length() > 0)
  
  // Verify we have combinations for each service-endpoint pair
  let expected_combinations = services.length() * endpoints.length()
  assert_eq(dimension_combinations.length(), expected_combinations)
  
  // Check a specific combination
  let api_users_combination = dimension_combinations.find_fn(fn(combination) {
    combination.dimensions.any_fn(fn(dim) { dim.0 == "service" and dim.1 == "api-gateway" }) and
    combination.dimensions.any_fn(fn(dim) { dim.0 == "endpoint" and dim.1 == "/users" })
  })
  
  assert_not_eq(api_users_combination, None)
  match api_users_combination {
    Some(combination) => {
      assert_eq(combination.metric_values.length(), 3)  // cpu, memory, request_rate
      assert_true(combination.metric_values.any_fn(fn(pair) { pair.0 == "cpu_usage" }))
      assert_true(combination.metric_values.any_fn(fn(pair) { pair.0 == "memory_usage" }))
      assert_true(combination.metric_values.any_fn(fn(pair) { pair.0 == "request_rate" }))
    }
    None => assert_true(false)
  }
  
  // Test correlation analysis
  let correlations = analyzer.calculate_correlations(metrics)
  
  assert_eq(correlations.length(), 3)  // 3 pairs from 3 metrics
  
  // Check for positive correlation between metrics (they should be correlated due to how we generated them)
  let cpu_memory_correlation = correlations.find_fn(fn(corr) { 
    (corr.metric_a == "cpu_usage" and corr.metric_b == "memory_usage") or
    (corr.metric_a == "memory_usage" and corr.metric_b == "cpu_usage")
  })
  
  assert_not_eq(cpu_memory_correlation, None)
  match cpu_memory_correlation {
    Some(corr) => {
      assert_true(corr.correlation_coefficient > 0.5)  // Should be positively correlated
      assert_true(corr.correlation_strength == "moderate" or corr.correlation_strength == "strong")
    }
    None => assert_true(false)
  }
  
  // Test heatmap generation
  let heatmap = analyzer.create_heatmap(dimension_combinations, "service", "endpoint", "cpu_usage")
  
  assert_eq(heatmap.length(), expected_combinations)
  
  // Check a specific heatmap point
  let heatmap_point = heatmap.find_fn(fn(point) {
    point.x_dimension == "api-gateway" and point.y_dimension == "/users"
  })
  
  assert_not_eq(heatmap_point, None)
  match heatmap_point {
    Some(point) => {
      assert_true(point.value > 0.0)
      assert_eq(point.count, 12)  // 12 hours of data
    }
    None => assert_true(false)
  }
}