// Azimuth Telemetry System - Advanced Data Analysis Tests
// This file contains comprehensive test cases for advanced data analysis functionality

// Test 1: Pattern Recognition in Telemetry Data
test "pattern recognition in telemetry data" {
  let pattern_analyzer = PatternAnalyzer::new()
  
  // Create telemetry data with patterns
  let telemetry_data = TelemetryData::new()
  
  // Add spans with cyclical patterns (hourly, daily, weekly)
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  for day in 0..=30 { // 30 days of data
    for hour in 0..=23 { // 24 hours per day
      for minute in 0..=59 { // 60 minutes per hour
        let timestamp = base_timestamp + 
          day.to_long() * 24 * 3600 * 1000L + // days
          hour.to_long() * 3600 * 1000L +     // hours
          minute.to_long() * 60 * 1000L       // minutes
        
        // Create pattern: higher activity during business hours (9-17)
        let activity_level = if hour >= 9 && hour <= 17 {
          100 + (hour - 9) * 10 // Peak at 17:00
        } else {
          20 + (hour % 4) * 5 // Lower activity outside business hours
        }
        
        // Add weekend pattern: lower activity on weekends
        let day_of_week = (day % 7)
        let adjusted_level = if day_of_week >= 5 { // Saturday (5) and Sunday (6)
          activity_level / 2
        } else {
          activity_level
        }
        
        let span = TelemetryData::create_span(telemetry_data, "pattern_span")
        TelemetryData::set_timestamp(span, timestamp)
        TelemetryData::set_attribute(span, "activity_level", IntValue(adjusted_level))
        TelemetryData::set_attribute(span, "hour_of_day", IntValue(hour))
        TelemetryData::set_attribute(span, "day_of_week", IntValue(day_of_week))
      }
    }
  }
  
  // Analyze patterns
  let analysis_result = pattern_analyzer.analyze(telemetry_data)
  
  // Verify hourly pattern detection
  let hourly_pattern = pattern_analyzer.get_hourly_pattern(analysis_result)
  assert_true(hourly_pattern.length() == 24)
  
  // Verify business hours have higher activity
  for hour in 9..=17 {
    assert_true(hourly_pattern[hour] > hourly_pattern[8]) // Higher than before business hours
    assert_true(hourly_pattern[hour] > hourly_pattern[18]) // Higher than after business hours
  }
  
  // Verify daily pattern detection
  let daily_pattern = pattern_analyzer.get_daily_pattern(analysis_result)
  assert_true(daily_pattern.length() == 7)
  
  // Verify weekend pattern (lower activity)
  assert_true(daily_pattern[0] > daily_pattern[5]) // Monday > Saturday
  assert_true(daily_pattern[0] > daily_pattern[6]) // Monday > Sunday
  
  // Verify pattern confidence
  let hourly_confidence = pattern_analyzer.get_pattern_confidence(analysis_result, "hourly")
  let daily_confidence = pattern_analyzer.get_pattern_confidence(analysis_result, "daily")
  
  assert_true(hourly_confidence > 0.8) // High confidence in hourly pattern
  assert_true(daily_confidence > 0.7)  // Good confidence in daily pattern
  
  // Test pattern prediction
  let future_timestamp = base_timestamp + 35 * 24 * 3600 * 1000L // 35 days in the future
  let predicted_activity = pattern_analyzer.predict_activity(analysis_result, future_timestamp)
  
  // Verify prediction is reasonable
  assert_true(predicted_activity >= 20 && predicted_activity <= 200)
}

// Test 2: Anomaly Detection with Machine Learning
test "anomaly detection with machine learning" {
  let anomaly_detector = MLAnomalyDetector::new()
  
  // Create training data with normal patterns
  let training_data = TelemetryData::new()
  
  // Generate normal data (following a predictable pattern)
  let base_timestamp = 1609459200000L
  let base_value = 100.0
  
  for i in 0..=1000 {
    let timestamp = base_timestamp + i.to_long() * 60000L // 1 minute intervals
    // Normal pattern: sinusoidal with some noise
    let noise = (i % 10 - 5) * 2.0 // Random noise between -10 and 10
    let value = base_value + 50.0 * Math::sin(i * 0.1) + noise
    
    let metric = TelemetryData::create_metric(training_data, "cpu_usage")
    TelemetryData::set_timestamp(metric, timestamp)
    TelemetryData::add_measurement(metric, value)
  }
  
  // Train the anomaly detection model
  let training_result = anomaly_detector.train(training_data)
  assert_true(training_result.success)
  assert_true(training_result.model_accuracy > 0.9)
  
  // Create test data with anomalies
  let test_data = TelemetryData::new()
  
  // Add normal data points
  for i in 0..=100 {
    let timestamp = base_timestamp + (1000 + i).to_long() * 60000L
    let noise = (i % 10 - 5) * 2.0
    let value = base_value + 50.0 * Math::sin((1000 + i) * 0.1) + noise
    
    let metric = TelemetryData::create_metric(test_data, "cpu_usage")
    TelemetryData::set_timestamp(metric, timestamp)
    TelemetryData::add_measurement(metric, value)
  }
  
  // Add anomaly points
  // Sudden spike
  let spike_timestamp = base_timestamp + 1100.to_long() * 60000L
  let spike_metric = TelemetryData::create_metric(test_data, "cpu_usage")
  TelemetryData::set_timestamp(spike_metric, spike_timestamp)
  TelemetryData::add_measurement(spike_metric, 500.0) // Much higher than normal
  
  // Sudden drop
  let drop_timestamp = base_timestamp + 1105.to_long() * 60000L
  let drop_metric = TelemetryData::create_metric(test_data, "cpu_usage")
  TelemetryData::set_timestamp(drop_metric, drop_timestamp)
  TelemetryData::add_measurement(drop_metric, 10.0) // Much lower than normal
  
  // Pattern break
  let break_timestamp = base_timestamp + 1110.to_long() * 60000L
  let break_metric = TelemetryData::create_metric(test_data, "cpu_usage")
  TelemetryData::set_timestamp(break_metric, break_timestamp)
  TelemetryData::add_measurement(break_metric, 200.0) // Breaks the sinusoidal pattern
  
  // Detect anomalies
  let detection_result = anomaly_detector.detect(test_data)
  
  // Verify anomalies were detected
  assert_true(detection_result.anomalies.length() >= 3)
  
  // Verify specific anomalies
  let spike_anomaly = anomaly_detector.find_anomaly_at(detection_result, spike_timestamp)
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.type, "spike")
      assert_true(anomaly.severity > 0.8)
      assert_true(anomaly.confidence > 0.9)
    }
    None => assert_true(false)
  }
  
  let drop_anomaly = anomaly_detector.find_anomaly_at(detection_result, drop_timestamp)
  match drop_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.type, "drop")
      assert_true(anomaly.severity > 0.8)
      assert_true(anomaly.confidence > 0.9)
    }
    None => assert_true(false)
  }
  
  let break_anomaly = anomaly_detector.find_anomaly_at(detection_result, break_timestamp)
  match break_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.type, "pattern_break")
      assert_true(anomaly.severity > 0.7)
      assert_true(anomaly.confidence > 0.8)
    }
    None => assert_true(false)
  }
  
  // Verify normal points were not flagged as anomalies
  for i in 0..=100 {
    let timestamp = base_timestamp + (1000 + i).to_long() * 60000L
    let anomaly = anomaly_detector.find_anomaly_at(detection_result, timestamp)
    match anomaly {
      Some(_) => assert_true(false) // Should not find anomaly for normal points
      None => assert_true(true)
    }
  }
}

// Test 3: Correlation Analysis Between Metrics
test "correlation analysis between metrics" {
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // Create telemetry data with correlated metrics
  let telemetry_data = TelemetryData::new()
  
  let base_timestamp = 1609459200000L
  
  // Generate correlated metrics
  for i in 0..=500 {
    let timestamp = base_timestamp + i.to_long() * 60000L
    
    // CPU and Memory usage (positively correlated)
    let base_load = 50.0 + 30.0 * Math::sin(i * 0.05)
    let cpu_usage = base_load + (i % 10 - 5) * 2.0
    let memory_usage = base_load * 1.5 + (i % 10 - 5) * 3.0
    
    // Response time and Error rate (negatively correlated with CPU/Memory)
    let response_time = 1000.0 / (cpu_usage + 10.0) + (i % 10) * 10.0
    let error_rate = Math::max(0.0, 5.0 - cpu_usage / 20.0) + (i % 20) * 0.1
    
    // Add metrics to telemetry data
    let cpu_metric = TelemetryData::create_metric(telemetry_data, "cpu_usage")
    TelemetryData::set_timestamp(cpu_metric, timestamp)
    TelemetryData::add_measurement(cpu_metric, cpu_usage)
    
    let memory_metric = TelemetryData::create_metric(telemetry_data, "memory_usage")
    TelemetryData::set_timestamp(memory_metric, timestamp)
    TelemetryData::add_measurement(memory_metric, memory_usage)
    
    let response_metric = TelemetryData::create_metric(telemetry_data, "response_time")
    TelemetryData::set_timestamp(response_metric, timestamp)
    TelemetryData::add_measurement(response_metric, response_time)
    
    let error_metric = TelemetryData::create_metric(telemetry_data, "error_rate")
    TelemetryData::set_timestamp(error_metric, timestamp)
    TelemetryData::add_measurement(error_metric, error_rate)
  }
  
  // Analyze correlations
  let correlation_result = correlation_analyzer.analyze(telemetry_data)
  
  // Verify positive correlation between CPU and Memory
  let cpu_memory_correlation = correlation_analyzer.get_correlation(correlation_result, "cpu_usage", "memory_usage")
  assert_true(cpu_memory_correlation > 0.7) // Strong positive correlation
  
  // Verify negative correlation between CPU and Response Time
  let cpu_response_correlation = correlation_analyzer.get_correlation(correlation_result, "cpu_usage", "response_time")
  assert_true(cpu_response_correlation < -0.5) // Moderate negative correlation
  
  // Verify negative correlation between CPU and Error Rate
  let cpu_error_correlation = correlation_analyzer.get_correlation(correlation_result, "cpu_usage", "error_rate")
  assert_true(cpu_error_correlation < -0.5) // Moderate negative correlation
  
  // Verify correlation matrix
  let correlation_matrix = correlation_analyzer.get_correlation_matrix(correlation_result)
  assert_true(correlation_matrix.size() == 4) // 4 metrics
  
  // Verify diagonal elements are 1.0 (self-correlation)
  assert_eq(correlation_matrix.get("cpu_usage", "cpu_usage"), 1.0)
  assert_eq(correlation_matrix.get("memory_usage", "memory_usage"), 1.0)
  assert_eq(correlation_matrix.get("response_time", "response_time"), 1.0)
  assert_eq(correlation_matrix.get("error_rate", "error_rate"), 1.0)
  
  // Test correlation significance
  let cpu_memory_significance = correlation_analyzer.get_significance(correlation_result, "cpu_usage", "memory_usage")
  assert_true(cpu_memory_significance < 0.05) // Significant correlation
  
  // Test partial correlation (controlling for CPU usage)
  let memory_response_partial = correlation_analyzer.get_partial_correlation(
    correlation_result, "memory_usage", "response_time", ["cpu_usage"]
  )
  
  // Test lag correlation (time-shifted correlation)
  let lag_correlations = correlation_analyzer.get_lag_correlations(
    correlation_result, "cpu_usage", "memory_usage", 10
  )
  assert_true(lag_correlations.length() == 21) // -10 to +10 lag
}

// Test 4: Predictive Analytics for Capacity Planning
test "predictive analytics for capacity planning" {
  let predictive_analyzer = PredictiveAnalyzer::new()
  
  // Create historical telemetry data for capacity planning
  let historical_data = TelemetryData::new()
  
  let base_timestamp = 1609459200000L // 2021-01-01
  
  // Generate 6 months of historical data
  for day in 0..=180 {
    let day_timestamp = base_timestamp + day.to_long() * 24 * 3600 * 1000L
    
    // Simulate growth trend with seasonal variations
    let growth_factor = 1.0 + day.to_float() * 0.005 // 0.5% growth per day
    let seasonal_factor = 1.0 + 0.3 * Math::sin(day * 2 * Math::PI / 30) // Monthly seasonality
    
    // CPU usage trend
    let base_cpu = 30.0 * growth_factor * seasonal_factor
    let cpu_usage = base_cpu + (day % 7) * 5.0 // Weekly variations
    
    // Memory usage trend
    let base_memory = 40.0 * growth_factor * seasonal_factor
    let memory_usage = base_memory + (day % 7) * 3.0
    
    // Disk usage trend (monotonic increasing)
    let disk_usage = 20.0 + day.to_float() * 0.2 * (1.0 + 0.1 * Math::sin(day * 2 * Math::PI / 30))
    
    // Network traffic trend
    let base_network = 25.0 * growth_factor * seasonal_factor
    let network_usage = base_network + (day % 14) * 2.0 // Bi-weekly variations
    
    // Add daily metrics
    let cpu_metric = TelemetryData::create_metric(historical_data, "cpu_usage")
    TelemetryData::set_timestamp(cpu_metric, day_timestamp)
    TelemetryData::add_measurement(cpu_metric, cpu_usage)
    
    let memory_metric = TelemetryData::create_metric(historical_data, "memory_usage")
    TelemetryData::set_timestamp(memory_metric, day_timestamp)
    TelemetryData::add_measurement(memory_metric, memory_usage)
    
    let disk_metric = TelemetryData::create_metric(historical_data, "disk_usage")
    TelemetryData::set_timestamp(disk_metric, day_timestamp)
    TelemetryData::add_measurement(disk_metric, disk_usage)
    
    let network_metric = TelemetryData::create_metric(historical_data, "network_usage")
    TelemetryData::set_timestamp(network_metric, day_timestamp)
    TelemetryData::add_measurement(network_metric, network_usage)
  }
  
  // Train predictive models
  let training_result = predictive_analyzer.train_models(historical_data)
  assert_true(training_result.success)
  
  // Generate predictions for next 90 days
  let prediction_horizon = 90
  let predictions = predictive_analyzer.predict(historical_data, prediction_horizon)
  
  // Verify predictions
  assert_true(predictions.length() == prediction_horizon)
  
  // Verify CPU usage predictions
  let cpu_predictions = predictive_analyzer.get_metric_predictions(predictions, "cpu_usage")
  assert_true(cpu_predictions.length() == prediction_horizon)
  
  // Verify predictions follow expected trends
  for i in 0..=prediction_horizon {
    let predicted_value = cpu_predictions[i]
    assert_true(predicted_value > 0.0)
    assert_true(predicted_value < 100.0) // CPU usage should be under 100%
  }
  
  // Verify growth trend continues
  let last_historical_cpu = 30.0 * (1.0 + 180.0 * 0.005) // Approximate last historical value
  let first_predicted_cpu = cpu_predictions[0]
  let last_predicted_cpu = cpu_predictions[prediction_horizon]
  
  assert_true(first_predicted_cpu > last_historical_cpu) // Predictions should continue from historical
  assert_true(last_predicted_cpu > first_predicted_cpu) // Should show growth trend
  
  // Test capacity planning recommendations
  let capacity_plan = predictive_analyzer.generate_capacity_plan(predictions, [
    MetricThreshold::new("cpu_usage", 80.0, 90.0),
    MetricThreshold::new("memory_usage", 85.0, 95.0),
    MetricThreshold::new("disk_usage", 70.0, 85.0),
    MetricThreshold::new("network_usage", 75.0, 90.0)
  ])
  
  // Verify capacity plan
  assert_true(capacity_plan.recommendations.length() > 0)
  
  // Check for CPU capacity warnings
  let cpu_warnings = capacity_plan.get_warnings_for_metric("cpu_usage")
  if cpu_warnings.length() > 0 {
    for warning in cpu_warnings {
      assert_true(warning.metric == "cpu_usage")
      assert_true(warning.severity >= 1 && warning.severity <= 3) // 1=Warning, 2=Critical, 3=Alert
      assert_true(warning.days_until_threshold > 0)
    }
  }
  
  // Check for resource scaling recommendations
  let scaling_recommendations = capacity_plan.get_scaling_recommendations()
  for recommendation in scaling_recommendations {
    assert_true(recommendation.resource == "cpu" || 
                recommendation.resource == "memory" || 
                recommendation.resource == "disk" || 
                recommendation.resource == "network")
    assert_true(recommendation.scale_factor > 1.0) // Should recommend scaling up
  }
}

// Test 5: Root Cause Analysis for Incidents
test "root cause analysis for incidents" {
  let rca_analyzer = RootCauseAnalyzer::new()
  
  // Create telemetry data for an incident scenario
  let incident_data = TelemetryData::new()
  
  let incident_start = 1609459200000L // 2021-01-01 00:00:00 UTC
  let incident_duration = 3600000L // 1 hour
  
  // Generate normal data before incident
  for i in 0..=60 { // 1 hour before incident
    let timestamp = incident_start - (60 - i).to_long() * 60000L
    let cpu_usage = 30.0 + (i % 10) * 2.0
    let memory_usage = 40.0 + (i % 10) * 1.5
    let response_time = 100.0 + (i % 10) * 5.0
    let error_rate = 0.1 + (i % 20) * 0.05
    
    add_metric_data(incident_data, timestamp, cpu_usage, memory_usage, response_time, error_rate)
  }
  
  // Generate incident data (deployment failure)
  for i in 0..=60 { // 1 hour of incident
    let timestamp = incident_start + i.to_long() * 60000L
    
    // Deployment causes issues
    let deployment_progress = i.to_float() / 60.0
    let cpu_spike = if i < 20 { 80.0 + deployment_progress * 20.0 } else { 60.0 - deployment_progress * 10.0 }
    let memory_leak = 40.0 + i.to_float() * 0.8 // Memory leak during incident
    let response_degradation = 100.0 + i.to_float() * 10.0 // Response time increases
    let error_surge = if i < 30 { 0.1 + i.to_float() * 0.3 } else { 10.0 - (i - 30).to_float() * 0.2 }
    
    add_metric_data(incident_data, timestamp, cpu_spike, memory_leak, response_degradation, error_surge)
    
    // Add events indicating deployment
    if i == 0 {
      add_event_data(incident_data, timestamp, "deployment_started", [
        ("version", StringValue("v2.1.0")),
        ("service", StringValue("api-service"))
      ])
    } else if i == 30 {
      add_event_data(incident_data, timestamp, "deployment_failed", [
        ("error", StringValue("database connection timeout")),
        ("service", StringValue("api-service"))
      ])
    } else if i == 60 {
      add_event_data(incident_data, timestamp, "rollback_initiated", [
        ("from_version", StringValue("v2.1.0")),
        ("to_version", StringValue("v2.0.5")),
        ("service", StringValue("api-service"))
      ])
    }
  }
  
  // Generate recovery data
  for i in 0..=60 { // 1 hour after incident
    let timestamp = incident_start + incident_duration + i.to_long() * 60000L
    let recovery_progress = i.to_float() / 60.0
    let cpu_usage = 30.0 + (60 - i).to_float() * 0.5 + (i % 10) * 2.0
    let memory_usage = 70.0 - recovery_progress * 30.0 + (i % 10) * 1.5 // Memory recovers
    let response_time = 700.0 - recovery_progress * 600.0 + (i % 10) * 5.0 // Response time recovers
    let error_rate = 4.0 - recovery_progress * 3.9 + (i % 20) * 0.05 // Error rate recovers
    
    add_metric_data(incident_data, timestamp, cpu_usage, memory_usage, response_time, error_rate)
  }
  
  // Analyze incident
  let rca_result = rca_analyzer.analyze_incident(incident_data, incident_start, incident_start + incident_duration)
  
  // Verify incident detection
  assert_true(rca_result.incident_detected)
  assert_eq(rca_result.incident_start_time, incident_start)
  assert_eq(rca_result.incident_end_time, incident_start + incident_duration)
  
  // Verify anomaly detection during incident
  assert_true(rca_result.anomalies.length() > 0)
  
  // Verify root cause identification
  assert_true(rca_result.root_causes.length() > 0)
  
  // Check for deployment-related root cause
  let deployment_root_cause = rca_result.root_causes.find(fn(cause) {
    cause.cause_type == "deployment" && cause.confidence > 0.8
  })
  match deployment_root_cause {
    Some(cause) => {
      assert_true(cause.description.contains("deployment"))
      assert_true(cause.related_events.length() > 0)
    }
    None => assert_true(false)
  }
  
  // Verify timeline reconstruction
  let timeline = rca_result.incident_timeline
  assert_true(timeline.events.length() > 0)
  
  // Check for key events in timeline
  let deployment_started = timeline.events.find(fn(event) {
    event.event_type == "deployment_started"
  })
  assert_true(deployment_started != nil)
  
  let deployment_failed = timeline.events.find(fn(event) {
    event.event_type == "deployment_failed"
  })
  assert_true(deployment_failed != nil)
  
  let rollback_initiated = timeline.events.find(fn(event) {
    event.event_type == "rollback_initiated"
  })
  assert_true(rollback_initiated != nil)
  
  // Verify impact assessment
  let impact_assessment = rca_result.impact_assessment
  assert_true(impact_assessment.duration_minutes == 60)
  assert_true(impact_assessment.affected_services.length() > 0)
  
  // Verify recommendations
  assert_true(rca_result.recommendations.length() > 0)
  
  let deployment_recommendations = rca_result.recommendations.filter(fn(rec) {
    rec.category == "deployment"
  })
  assert_true(deployment_recommendations.length() > 0)
}

// Test 6: Trend Analysis and Forecasting
test "trend analysis and forecasting" {
  let trend_analyzer = TrendAnalyzer::new()
  
  // Create telemetry data with various trends
  let telemetry_data = TelemetryData::new()
  
  let base_timestamp = 1609459200000L // 2021-01-01
  
  // Generate data with different trends
  for day in 0..=365 { // 1 year of data
    let timestamp = base_timestamp + day.to_long() * 24 * 3600 * 1000L
    
    // Linear growth trend
    let linear_value = 50.0 + day.to_float() * 0.1
    
    // Exponential growth trend
    let exponential_value = 20.0 * Math::pow(1.002, day.to_float())
    
    // Logarithmic growth trend
    let logarithmic_value = 10.0 * Math::log(1.0 + day.to_float())
    
    // Seasonal trend (sinusoidal)
    let seasonal_value = 50.0 + 20.0 * Math::sin(day * 2 * Math::PI / 30) // Monthly seasonality
    
    // Add metrics
    let linear_metric = TelemetryData::create_metric(telemetry_data, "linear_growth")
    TelemetryData::set_timestamp(linear_metric, timestamp)
    TelemetryData::add_measurement(linear_metric, linear_value)
    
    let exponential_metric = TelemetryData::create_metric(telemetry_data, "exponential_growth")
    TelemetryData::set_timestamp(exponential_metric, timestamp)
    TelemetryData::add_measurement(exponential_metric, exponential_value)
    
    let logarithmic_metric = TelemetryData::create_metric(telemetry_data, "logarithmic_growth")
    TelemetryData::set_timestamp(logarithmic_metric, timestamp)
    TelemetryData::add_measurement(logarithmic_metric, logarithmic_value)
    
    let seasonal_metric = TelemetryData::create_metric(telemetry_data, "seasonal_pattern")
    TelemetryData::set_timestamp(seasonal_metric, timestamp)
    TelemetryData::add_measurement(seasonal_metric, seasonal_value)
  }
  
  // Analyze trends
  let trend_analysis = trend_analyzer.analyze_trends(telemetry_data)
  
  // Verify linear trend detection
  let linear_trend = trend_analyzer.get_trend_for_metric(trend_analysis, "linear_growth")
  assert_eq(linear_trend.type, "linear")
  assert_true(linear_trend.slope > 0.0) // Positive slope
  assert_true(linear_trend.confidence > 0.9) // High confidence
  
  // Verify exponential trend detection
  let exponential_trend = trend_analyzer.get_trend_for_metric(trend_analysis, "exponential_growth")
  assert_eq(exponential_trend.type, "exponential")
  assert_true(exponential_trend.growth_rate > 0.0) // Positive growth
  assert_true(exponential_trend.confidence > 0.8) // Good confidence
  
  // Verify logarithmic trend detection
  let logarithmic_trend = trend_analyzer.get_trend_for_metric(trend_analysis, "logarithmic_growth")
  assert_eq(logarithmic_trend.type, "logarithmic")
  assert_true(logarithmic_trend.confidence > 0.7) // Reasonable confidence
  
  // Verify seasonal trend detection
  let seasonal_trend = trend_analyzer.get_trend_for_metric(trend_analysis, "seasonal_pattern")
  assert_eq(seasonal_trend.type, "seasonal")
  assert_true(seasonal_trend.period == 30.0) // Monthly period
  assert_true(seasonal_trend.confidence > 0.8) // Good confidence
  
  // Generate forecasts
  let forecast_horizon = 90 // 90 days
  let forecasts = trend_analyzer.generate_forecasts(trend_analysis, forecast_horizon)
  
  // Verify forecasts
  assert_true(forecasts.length() == 4) // 4 metrics
  
  for forecast in forecasts {
    assert_true(forecast.values.length() == forecast_horizon)
    
    // Verify forecast values are reasonable
    for value in forecast.values {
      assert_true(value > 0.0)
    }
  }
  
  // Test forecast accuracy using historical data
  let accuracy_test = trend_analyzer.test_forecast_accuracy(telemetry_data, 30, 30) // Use 30 days to forecast next 30 days
  assert_true(accuracy_test.overall_accuracy > 0.7) // At least 70% accuracy
  
  // Test trend change detection
  let change_data = TelemetryData::new()
  
  // Generate data with trend change
  for day in 0..=180 {
    let timestamp = base_timestamp + day.to_long() * 24 * 3600 * 1000L
    
    let value = if day < 90 {
      50.0 + day.to_float() * 0.1 // Growing trend
    } else {
      59.0 - (day - 90).to_float() * 0.2 // Declining trend after day 90
    }
    
    let metric = TelemetryData::create_metric(change_data, "trend_change")
    TelemetryData::set_timestamp(metric, timestamp)
    TelemetryData::add_measurement(metric, value)
  }
  
  let change_analysis = trend_analyzer.analyze_trends(change_data)
  let trend_changes = trend_analyzer.detect_trend_changes(change_analysis)
  
  assert_true(trend_changes.length() > 0)
  
  // Verify trend change was detected around day 90
  let detected_change = trend_changes[0]
  assert_true(detected_change.change_day >= 85 && detected_change.change_day <= 95)
  assert_eq(detected_change.from_trend, "increasing")
  assert_eq(detected_change.to_trend, "decreasing")
}

// Test 7: Cohort Analysis for User Behavior
test "cohort analysis for user behavior" {
  let cohort_analyzer = CohortAnalyzer::new()
  
  // Create user activity telemetry data
  let telemetry_data = TelemetryData::new()
  
  let base_date = 1609459200000L // 2021-01-01
  
  // Generate user activity data for cohort analysis
  for week in 0..=11 { // 12 weeks of data
    let week_timestamp = base_date + week.to_long() * 7 * 24 * 3600 * 1000L
    
    // New users each week (cohort)
    let new_users_this_week = 100 + week * 10 // Growing acquisition
    
    for user in 0..=new_users_this_week {
      let user_id = "user_" + week.to_string() + "_" + user.to_string()
      
      // Generate activity for this user over subsequent weeks
      for subsequent_week in 0..=(11 - week) {
        let activity_timestamp = week_timestamp + subsequent_week.to_long() * 7 * 24 * 3600 * 1000L
        
        // Simulate retention pattern (higher retention for earlier cohorts)
        let base_retention = 0.8 - subsequent_week.to_float() * 0.05
        let cohort_bonus = week < 4 ? 0.1 : 0.0 // Better retention for early cohorts
        let retention_probability = base_retention + cohort_bonus
        
        // Simulate activity based on retention
        if Math::random() < retention_probability {
          let session_duration = 300 + Math::random() * 1800 // 5-35 minutes
          let page_views = 5 + Math::random() * 15 // 5-20 page views
          
          // Add user activity event
          let activity = TelemetryData::create_event(telemetry_data, "user_activity")
          TelemetryData::set_timestamp(activity, activity_timestamp)
          TelemetryData::add_event_attribute(activity, "user_id", StringValue(user_id))
          TelemetryData::add_event_attribute(activity, "cohort_week", IntValue(week))
          TelemetryData::add_event_attribute(activity, "activity_week", IntValue(subsequent_week))
          TelemetryData::add_event_attribute(activity, "session_duration", IntValue(session_duration))
          TelemetryData::add_event_attribute(activity, "page_views", IntValue(page_views))
        }
      }
    }
  }
  
  // Analyze cohorts
  let cohort_analysis = cohort_analyzer.analyze_cohorts(telemetry_data, "user_id", "cohort_week", "activity_week")
  
  // Verify cohort matrix
  let cohort_matrix = cohort_analysis.cohort_matrix
  assert_true(cohort_matrix.rows == 12) // 12 cohorts
  assert_true(cohort_matrix.columns == 12) // 12 weeks of activity
  
  // Verify retention patterns (should decrease over time)
  for cohort in 0..=11 {
    let first_week_retention = cohort_matrix.get(cohort, 0)
    let last_week_retention = cohort_matrix.get(cohort, 11 - cohort)
    
    assert_true(first_week_retention >= last_week_retention) // Retention should decrease
  }
  
  // Verify early cohorts have better retention
  let early_cohort_retention = cohort_matrix.get(0, 4) // Week 0 cohort, week 4 retention
  let late_cohort_retention = cohort_matrix.get(8, 4) // Week 8 cohort, week 4 retention
  
  assert_true(early_cohort_retention >= late_cohort_retention) // Early cohorts should have better retention
  
  // Test cohort comparisons
  let cohort_comparison = cohort_analyzer.compare_cohorts(cohort_analysis, [0, 4, 8])
  assert_true(cohort_comparison.comparisons.length() == 3) // 3 cohorts
  
  // Test behavioral metrics by cohort
  let behavioral_metrics = cohort_analyzer.analyze_behavior_by_cohort(telemetry_data, "user_id", "cohort_week")
  assert_true(behavioral_metrics.metrics.length() > 0)
  
  // Verify session duration analysis
  let session_duration_by_cohort = behavioral_metrics.get_metric("session_duration")
  assert_true(session_duration_by_cohort.cohorts.length() > 0)
  
  // Test predictive modeling for churn
  let churn_model = cohort_analyzer.build_churn_prediction_model(telemetry_data, "user_id", "cohort_week")
  assert_true(churn_model.model_accuracy > 0.7)
  
  // Test churn risk scoring
  let high_risk_users = cohort_analyzer.identify_high_risk_users(telemetry_data, churn_model, 0.8)
  assert_true(high_risk_users.length() > 0)
}

// Test 8: Funnel Analysis for Conversion Tracking
test "funnel analysis for conversion tracking" {
  let funnel_analyzer = FunnelAnalyzer::new()
  
  // Create user journey telemetry data
  let telemetry_data = TelemetryData::new()
  
  let base_timestamp = 1609459200000L // 2021-01-01
  
  // Define funnel steps
  let funnel_steps = [
    FunnelStep::new("page_view", "Page View"),
    FunnelStep::new("add_to_cart", "Add to Cart"),
    FunnelStep::new("checkout_start", "Checkout Start"),
    FunnelStep::new("payment_info", "Payment Info"),
    FunnelStep::new("purchase_complete", "Purchase Complete")
  ]
  
  // Generate user journey data
  let total_users = 10000
  
  for user in 0..=total_users {
    let user_id = "user_" + user.to_string()
    
    // Simulate conversion funnel with drop-off at each step
    let step_probabilities = [1.0, 0.7, 0.5, 0.4, 0.25] // Probability of reaching each step
    
    for step_index in 0..=funnel_steps.length() {
      if Math::random() < step_probabilities[step_index] {
        // User reaches this step
        let step_timestamp = base_timestamp + 
          user.to_long() * 1000L + // Spread users over time
          step_index.to_long() * 60000L // Add time between steps
        
        let step = funnel_steps[step_index]
        
        // Add funnel event
        let event = TelemetryData::create_event(telemetry_data, "funnel_step")
        TelemetryData::set_timestamp(event, step_timestamp)
        TelemetryData::add_event_attribute(event, "user_id", StringValue(user_id))
        TelemetryData::add_event_attribute(event, "step_name", StringValue(step.name))
        TelemetryData::add_event_attribute(event, "step_index", IntValue(step_index))
        
        // Add step-specific attributes
        match step.name {
          "page_view" => {
            TelemetryData::add_event_attribute(event, "page", StringValue("product_page"))
          }
          "add_to_cart" => {
            TelemetryData::add_event_attribute(event, "product_id", StringValue("prod_" + (user % 100).to_string()))
            TelemetryData::add_event_attribute(event, "quantity", IntValue(1 + user % 3))
          }
          "checkout_start" => {
            TelemetryData::add_event_attribute(event, "cart_value", IntValue(5000 + user % 10000)) // $50-150
          }
          "payment_info" => {
            TelemetryData::add_event_attribute(event, "payment_method", StringValue(if user % 3 == 0 { "credit_card" } else if user % 3 == 1 { "paypal" } else { "apple_pay" }))
          }
          "purchase_complete" => {
            TelemetryData::add_event_attribute(event, "order_id", StringValue("order_" + user.to_string()))
            TelemetryData::add_event_attribute(event, "purchase_amount", IntValue(5000 + user % 10000))
          }
          _ => {}
        }
      } else {
        break // User drops out of funnel
      }
    }
  }
  
  // Analyze funnel
  let funnel_analysis = funnel_analyzer.analyze_funnel(telemetry_data, funnel_steps)
  
  // Verify funnel steps
  assert_true(funnel_analysis.steps.length() == 5)
  
  // Verify conversion rates decrease through funnel
  for i in 1..=funnel_analysis.steps.length() {
    let previous_step_users = funnel_analysis.steps[i-1].unique_users
    let current_step_users = funnel_analysis.steps[i].unique_users
    
    let conversion_rate = current_step_users.to_float() / previous_step_users.to_float()
    assert_true(conversion_rate <= 1.0) // Can't have more than 100% conversion
    assert_true(conversion_rate > 0.0) // Should have some users at each step
  }
  
  // Verify overall conversion rate
  let first_step_users = funnel_analysis.steps[0].unique_users
  let last_step_users = funnel_analysis.steps[funnel_analysis.steps.length() - 1].unique_users
  let overall_conversion = last_step_users.to_float() / first_step_users.to_float()
  
  assert_true(overall_conversion > 0.1 && overall_conversion < 0.5) // Should be 10-50% based on our simulation
  
  // Test funnel segmentation
  let segmented_funnels = funnel_analyzer.segment_funnel(telemetry_data, funnel_steps, "payment_method")
  assert_true(segmented_funnels.segments.length() > 0)
  
  // Test time-to-convert analysis
  let time_analysis = funnel_analyzer.analyze_time_to_convert(telemetry_data, funnel_steps)
  assert_true(time_analysis.average_times.length() == funnel_steps.length() - 1)
  
  // Test funnel drop-off analysis
  let dropoff_analysis = funnel_analyzer.analyze_dropoffs(telemetry_data, funnel_steps)
  assert_true(dropoff_analysis.dropoff_points.length() > 0)
  
  // Verify highest drop-off point
  let highest_dropoff = dropoff_analysis.get_highest_dropoff_point()
  assert_true(highest_dropoff != nil)
}

// Test 9: A/B Testing Analysis
test "ab_testing_analysis" {
  let ab_test_analyzer = ABTestAnalyzer::new()
  
  // Create A/B test telemetry data
  let telemetry_data = TelemetryData::new()
  
  let test_start = 1609459200000L // 2021-01-01
  let test_duration = 14 * 24 * 3600 * 1000L // 2 weeks
  
  // Generate A/B test data
  let total_users = 20000
  
  for user in 0..=total_users {
    let user_id = "user_" + user.to_string()
    
    // Assign user to variant (50/50 split)
    let variant = if user % 2 == 0 { "control" } else { "variant_a" }
    
    // Simulate user journey during test period
    for day in 0..=13 {
      let day_timestamp = test_start + day.to_long() * 24 * 3600 * 1000L
      
      // Simulate different behavior based on variant
      let base_conversion_rate = if variant == "control" { 0.1 } else { 0.12 } // Variant A has 20% improvement
      let daily_engagement = if variant == "control" { 0.3 } else { 0.35 } // Variant A has better engagement
      
      // Simulate daily activity
      if Math::random() < daily_engagement {
        // User is active today
        let activity = TelemetryData::create_event(telemetry_data, "user_activity")
        TelemetryData::set_timestamp(activity, day_timestamp)
        TelemetryData::add_event_attribute(activity, "user_id", StringValue(user_id))
        TelemetryData::add_event_attribute(activity, "variant", StringValue(variant))
        TelemetryData::add_event_attribute(activity, "test_name", StringValue("checkout_flow_test"))
        TelemetryData::add_event_attribute(activity, "day", IntValue(day))
        
        // Simulate conversion
        if Math::random() < base_conversion_rate {
          let conversion = TelemetryData::create_event(telemetry_data, "conversion")
          TelemetryData::set_timestamp(conversion, day_timestamp + Math::random() * 86400000L) // Sometime during the day
          TelemetryData::add_event_attribute(conversion, "user_id", StringValue(user_id))
          TelemetryData::add_event_attribute(conversion, "variant", StringValue(variant))
          TelemetryData::add_event_attribute(conversion, "test_name", StringValue("checkout_flow_test"))
          TelemetryData::add_event_attribute(conversion, "revenue", IntValue(10000 + Math::random() * 90000)) // $10-100
        }
      }
    }
  }
  
  // Analyze A/B test
  let ab_test_analysis = ab_test_analyzer.analyze_test(telemetry_data, "checkout_flow_test", ["control", "variant_a"])
  
  // Verify test setup
  assert_eq(ab_test_analysis.test_name, "checkout_flow_test")
  assert_true(ab_test_analysis.variants.length() == 2)
  
  // Verify sample sizes
  let control_sample = ab_test_analysis.get_variant_stats("control")
  let variant_sample = ab_test_analysis.get_variant_stats("variant_a")
  
  assert_true(control_sample.sample_size > 0)
  assert_true(variant_sample.sample_size > 0)
  
  // Verify roughly equal sample sizes (within 5%)
  let sample_ratio = variant_sample.sample_size.to_float() / control_sample.sample_size.to_float()
  assert_true(sample_ratio > 0.95 && sample_ratio < 1.05)
  
  // Verify conversion rates
  assert_true(variant_sample.conversion_rate > control_sample.conversion_rate) // Variant should perform better
  
  // Verify statistical significance
  let significance_test = ab_test_analyzer.test_significance(control_sample, variant_sample)
  assert_true(significance_test.p_value < 0.05) // Should be statistically significant
  assert_true(significance_test.is_significant)
  
  // Verify confidence interval
  assert_true(significance_test.confidence_interval.lower > 0) // Improvement should be positive
  assert_true(significance_test.confidence_interval.upper > significance_test.confidence_interval.lower)
  
  // Test revenue impact
  let revenue_analysis = ab_test_analyzer.analyze_revenue_impact(telemetry_data, "checkout_flow_test", ["control", "variant_a"])
  assert_true(revenue_analysis.variant_a_revenue_per_user > revenue_analysis.control_revenue_per_user)
  
  // Test segment analysis
  let segment_analysis = ab_test_analyzer.analyze_by_segment(telemetry_data, "checkout_flow_test", ["control", "variant_a"], "day")
  assert_true(segment_analysis.segments.length() > 0)
  
  // Test early stopping analysis
  let early_stopping = ab_test_analyzer.check_early_stopping(ab_test_analysis, 0.05, 0.8) // 5% significance, 80% power
  assert_true(early_stopping.should_stop) // Should recommend stopping with clear winner
}

// Test 10: Multi-dimensional Analysis
test "multi-dimensional_analysis" {
  let multidimensional_analyzer = MultidimensionalAnalyzer::new()
  
  // Create complex telemetry data with multiple dimensions
  let telemetry_data = TelemetryData::new()
  
  let base_timestamp = 1609459200000L // 2021-01-01
  
  // Generate data with multiple dimensions
  for day in 0..=29 { // 30 days
    let day_timestamp = base_timestamp + day.to_long() * 24 * 3600 * 1000L
    
    for hour in 0..=23 { // 24 hours
      let hour_timestamp = day_timestamp + hour.to_long() * 3600 * 1000L
      
      // Different regions
      let regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
      
      for region in regions {
        // Different services
        let services = ["api-service", "auth-service", "payment-service", "notification-service"]
        
        for service in services {
          // Different environments
          let environments = ["production", "staging"]
          
          for environment in environments {
            // Generate metrics based on dimensions
            let base_value = 50.0
            
            // Region-based variation
            let region_factor = match region {
              "us-east-1" => 1.2, // Highest traffic
              "us-west-2" => 1.0,
              "eu-west-1" => 0.8, // Time zone difference
              "ap-southeast-1" => 0.6, // Lowest traffic
              _ => 1.0
            }
            
            // Service-based variation
            let service_factor = match service {
              "api-service" => 1.5, // Most used
              "auth-service" => 1.2,
              "payment-service" => 0.8,
              "notification-service" => 0.5,
              _ => 1.0
            }
            
            // Environment-based variation
            let environment_factor = match environment {
              "production" => 1.0,
              "staging" => 0.3, // Much less traffic
              _ => 1.0
            }
            
            // Time-based variation
            let time_factor = if hour >= 9 && hour <= 17 { 1.5 } else { 0.7 } // Business hours
            
            // Calculate final value
            let value = base_value * region_factor * service_factor * environment_factor * time_factor
            let value_with_noise = value + (Math::random() - 0.5) * 10.0 // Add some noise
            
            // Add metric
            let metric = TelemetryData::create_metric(telemetry_data, "request_count")
            TelemetryData::set_timestamp(metric, hour_timestamp)
            TelemetryData::add_measurement(metric, value_with_noise)
            
            // Add dimensions as attributes
            TelemetryData::add_attribute(metric, "region", StringValue(region))
            TelemetryData::add_attribute(metric, "service", StringValue(service))
            TelemetryData::add_attribute(metric, "environment", StringValue(environment))
            TelemetryData::add_attribute(metric, "hour", IntValue(hour))
            TelemetryData::add_attribute(metric, "day", IntValue(day))
          }
        }
      }
    }
  }
  
  // Define dimensions for analysis
  let dimensions = [
    Dimension::new("region", ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]),
    Dimension::new("service", ["api-service", "auth-service", "payment-service", "notification-service"]),
    Dimension::new("environment", ["production", "staging"]),
    Dimension::new("hour_of_day", Array::range(0, 24)),
    Dimension::new("day_of_month", Array::range(1, 31))
  ]
  
  // Perform multi-dimensional analysis
  let analysis_result = multidimensional_analyzer.analyze(telemetry_data, "request_count", dimensions)
  
  // Verify dimension combinations
  assert_true(analysis_result.dimension_combinations.length() > 0)
  
  // Verify top combinations by value
  let top_combinations = analysis_result.get_top_combinations(10)
  assert_true(top_combinations.length() == 10)
  
  // Verify top combination is production, api-service, us-east-1 during business hours
  let top_combination = top_combinations[0]
  assert_true(top_combination.dimensions.get("environment") == "production")
  assert_true(top_combination.dimensions.get("service") == "api-service")
  assert_true(top_combination.dimensions.get("region") == "us-east-1")
  
  let top_hour = top_combination.dimensions.get("hour_of_day")
  match top_hour {
    IntValue(hour) => assert_true(hour >= 9 && hour <= 17) // Should be business hours
    _ => assert_true(false)
  }
  
  // Verify dimension impact analysis
  let dimension_impacts = analysis_result.dimension_impacts
  assert_true(dimension_impacts.length() == dimensions.length())
  
  // Environment should have highest impact
  let environment_impact = dimension_impacts.find(fn(impact) { impact.dimension == "environment" })
  match environment_impact {
    Some(impact) => assert_true(impact.impact_score > 0.5) // High impact
    None => assert_true(false)
  }
  
  // Test drill-down analysis
  let drill_down = multidimensional_analyzer.drill_down(
    analysis_result, 
    [("environment", "production"), ("service", "api-service")]
  )
  
  assert_true(drill_down.dimension_combinations.length() > 0)
  
  // Verify all drill-down results have the specified dimensions
  for combination in drill_down.dimension_combinations {
    assert_eq(combination.dimensions.get("environment"), "production")
    assert_eq(combination.dimensions.get("service"), "api-service")
  }
  
  // Test roll-up analysis
  let roll_up = multidimensional_analyzer.roll_up(
    analysis_result, 
    ["hour_of_day", "day_of_month"] // Remove time dimensions
  )
  
  assert_true(roll_up.dimension_combinations.length() > 0)
  
  // Verify rolled-up combinations don't have time dimensions
  for combination in roll_up.dimension_combinations {
    assert_true(!combination.dimensions.contains("hour_of_day"))
    assert_true(!combination.dimensions.contains("day_of_month"))
  }
  
  // Test anomaly detection in multi-dimensional space
  let anomalies = multidimensional_analyzer.detect_anomalies(analysis_result, 2.0) // 2 standard deviations
  assert_true(anomalies.length() > 0)
  
  // Verify anomalies are unexpected combinations
  for anomaly in anomalies {
    assert_true(anomaly.anomaly_score > 2.0)
    
    // Check if anomaly is in staging environment with unusually high traffic
    let environment = anomaly.dimensions.get("environment")
    match environment {
      StringValue(env) => {
        if env == "staging" {
          assert_true(anomaly.expected_value < anomaly.actual_value) // Higher than expected
        }
      }
      _ => assert_true(false)
    }
  }
}

// Helper functions
fn add_metric_data(data : TelemetryData, timestamp : Int, cpu : Float, memory : Float, response : Float, error_rate : Float) {
  let cpu_metric = TelemetryData::create_metric(data, "cpu_usage")
  TelemetryData::set_timestamp(cpu_metric, timestamp)
  TelemetryData::add_measurement(cpu_metric, cpu)
  
  let memory_metric = TelemetryData::create_metric(data, "memory_usage")
  TelemetryData::set_timestamp(memory_metric, timestamp)
  TelemetryData::add_measurement(memory_metric, memory)
  
  let response_metric = TelemetryData::create_metric(data, "response_time")
  TelemetryData::set_timestamp(response_metric, timestamp)
  TelemetryData::add_measurement(response_metric, response)
  
  let error_metric = TelemetryData::create_metric(data, "error_rate")
  TelemetryData::set_timestamp(error_metric, timestamp)
  TelemetryData::add_measurement(error_metric, error_rate)
}

fn add_event_data(data : TelemetryData, timestamp : Int, event_name : String, attributes : Array<(String, AttributeValue)>) {
  let event = TelemetryData::create_event(data, event_name)
  TelemetryData::set_timestamp(event, timestamp)
  
  for (key, value) in attributes {
    TelemetryData::add_event_attribute(event, key, value)
  }
}