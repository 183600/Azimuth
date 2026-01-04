// Azimuth Telemetry System - Advanced Data Analytics Tests
// This file contains comprehensive test cases for advanced data analytics features

// Test 1: Time Series Analysis
test "time series analysis and forecasting" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // Generate sample time series data
  let time_series_data = []
  let base_value = 100.0
  for i in 0..=100 {
    let timestamp = 1640995200L + i * 3600L // Hourly data starting from Jan 1, 2022
    let seasonal_factor = 10.0 * (i % 24).to_float() / 23.0 // Daily seasonality
    let trend_factor = 0.5 * i.to_float() // Upward trend
    let noise = (Math::random() * 10.0) - 5.0 // Random noise
    
    let value = base_value + seasonal_factor + trend_factor + noise
    time_series_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // Test trend analysis
  let trend_analysis = TimeSeriesAnalyzer::analyze_trend(time_series_analyzer, time_series_data)
  assert_true(trend_analysis.slope > 0.4) // Should detect upward trend
  assert_true(trend_analysis.slope < 0.6) // But not too steep
  assert_true(trend_analysis.confidence > 0.8) // High confidence
  
  // Test seasonality detection
  let seasonality_analysis = TimeSeriesAnalyzer::detect_seasonality(time_series_analyzer, time_series_data)
  assert_true(seasonality_analysis.has_seasonality)
  assert_eq(seasonality_analysis.primary_period, 24) // 24-hour cycle
  assert_true(seasonality_analysis.seasonality_strength > 0.5)
  
  // Test anomaly detection
  let anomalies = TimeSeriesAnalyzer::detect_anomalies(time_series_analyzer, time_series_data)
  assert_true(anomalies.length() >= 0)
  
  // Add some obvious anomalies
  let anomalous_data = time_series_data.copy()
  anomalous_data[50] = TimeSeriesPoint::new(anomalous_data[50].timestamp, 500.0) // Spike
  anomalous_data[75] = TimeSeriesPoint::new(anomalous_data[75].timestamp, -100.0) // Dip
  
  let detected_anomalies = TimeSeriesAnalyzer::detect_anomalies(time_series_analyzer, anomalous_data)
  assert_true(detected_anomalies.length() >= 2)
  
  // Test forecasting
  let forecast = TimeSeriesAnalyzer::forecast(time_series_analyzer, time_series_data, 24) // 24-hour forecast
  assert_eq(forecast.values.length(), 24)
  
  // Verify forecast is reasonable (not too far from recent values)
  let last_actual = time_series_data[time_series_data.length() - 1].value
  let first_forecast = forecast.values[0]
  assert_true(Math::abs(first_forecast - last_actual) < 50.0)
  
  // Test forecast confidence intervals
  assert_true(forecast.upper_bounds.length() == 24)
  assert_true(forecast.lower_bounds.length() == 24)
  
  for i in 0..=23 {
    assert_true(forecast.upper_bounds[i] >= forecast.values[i])
    assert_true(forecast.lower_bounds[i] <= forecast.values[i])
  }
}

// Test 2: Statistical Analysis
test "statistical analysis and hypothesis testing" {
  let statistical_analyzer = StatisticalAnalyzer::new()
  
  // Generate sample datasets
  let dataset_a = []
  let dataset_b = []
  
  // Dataset A: Normal distribution around 100
  for i in 0..=1000 {
    let value = 100.0 + StatisticalAnalyzer::normal_random(statistical_analyzer, 0.0, 15.0)
    dataset_a.push(value)
  }
  
  // Dataset B: Normal distribution around 105
  for i in 0..=1000 {
    let value = 105.0 + StatisticalAnalyzer::normal_random(statistical_analyzer, 0.0, 15.0)
    dataset_b.push(value)
  }
  
  // Test descriptive statistics
  let stats_a = StatisticalAnalyzer::calculate_descriptive_stats(statistical_analyzer, dataset_a)
  let stats_b = StatisticalAnalyzer::calculate_descriptive_stats(statistical_analyzer, dataset_b)
  
  assert_true(Math::abs(stats_a.mean - 100.0) < 2.0)
  assert_true(Math::abs(stats_b.mean - 105.0) < 2.0)
  assert_true(Math::abs(stats_a.std_dev - 15.0) < 2.0)
  assert_true(Math::abs(stats_b.std_dev - 15.0) < 2.0)
  
  // Test correlation analysis
  let correlation = StatisticalAnalyzer::calculate_correlation(statistical_analyzer, dataset_a, dataset_b)
  assert_true(correlation >= -1.0 && correlation <= 1.0)
  
  // Test t-test
  let t_test_result = StatisticalAnalyzer::t_test(statistical_analyzer, dataset_a, dataset_b)
  assert_true(t_test_result.p_value < 0.05) // Should reject null hypothesis
  assert_true(t_test_result.statistically_significant)
  
  // Test chi-square test
  let observed = [50, 60, 40, 45, 55]
  let expected = [50, 50, 50, 50, 50]
  
  let chi_square_result = StatisticalAnalyzer::chi_square_test(statistical_analyzer, observed, expected)
  assert_true(chi_square_result.p_value >= 0.0)
  assert_true(chi_square_result.chi_square_statistic >= 0.0)
  
  // Test ANOVA
  let group1 = [10.0, 12.0, 11.0, 13.0, 12.0]
  let group2 = [15.0, 17.0, 16.0, 18.0, 17.0]
  let group3 = [20.0, 22.0, 21.0, 23.0, 22.0]
  
  let anova_result = StatisticalAnalyzer::anova(statistical_analyzer, [group1, group2, group3])
  assert_true(anova_result.p_value < 0.05) // Should reject null hypothesis
  assert_true(anova_result.f_statistic > 0.0)
}

// Test 3: Pattern Recognition and Machine Learning
test "pattern recognition and machine learning" {
  let ml_analyzer = MLAnalyzer::new()
  
  // Generate sample data for classification
  let training_data = []
  
  // Class A: Points around (0, 0)
  for i in 0..=100 {
    let x = StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 1.0)
    let y = StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 1.0)
    training_data.push(DataPoint::new([x, y], "A"))
  }
  
  // Class B: Points around (3, 3)
  for i in 0..=100 {
    let x = 3.0 + StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 1.0)
    let y = 3.0 + StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 1.0)
    training_data.push(DataPoint::new([x, y], "B"))
  }
  
  // Train classification model
  let classification_model = MLAnalyzer::train_classifier(ml_analyzer, training_data, "decision_tree")
  assert_true(MLAnalyzer::is_trained(classification_model))
  
  // Test classification accuracy
  let test_data = [
    DataPoint::new([0.5, 0.5], "A"), // Should be classified as A
    DataPoint::new([2.5, 2.5], "B"), // Should be classified as B
    DataPoint::new([-0.5, -0.5], "A"), // Should be classified as A
    DataPoint::new([3.5, 3.5], "B") // Should be classified as B
  ]
  
  let correct_predictions = 0
  for point in test_data {
    let prediction = MLAnalyzer::predict(classification_model, point.features)
    if prediction == point.label {
      correct_predictions = correct_predictions + 1
    }
  }
  
  let accuracy = correct_predictions.to_float() / test_data.length().to_float()
  assert_true(accuracy > 0.75) // Should be reasonably accurate
  
  // Test clustering
  let clustering_data = []
  
  // Cluster 1: Points around (0, 0)
  for i in 0..=50 {
    let x = StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    let y = StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    clustering_data.push([x, y])
  }
  
  // Cluster 2: Points around (5, 5)
  for i in 0..=50 {
    let x = 5.0 + StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    let y = 5.0 + StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    clustering_data.push([x, y])
  }
  
  // Cluster 3: Points around (10, 0)
  for i in 0..=50 {
    let x = 10.0 + StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    let y = StatisticalAnalyzer::normal_random(ml_analyzer, 0.0, 0.5)
    clustering_data.push([x, y])
  }
  
  let clustering_model = MLAnalyzer::train_clustering(ml_analyzer, clustering_data, 3) // 3 clusters
  let clusters = MLAnalyzer::get_clusters(clustering_model)
  assert_eq(clusters.length(), 3)
  
  // Verify clusters contain appropriate number of points
  for cluster in clusters {
    assert_true(cluster.points.length() >= 40) // Each cluster should have around 50 points
  }
  
  // Test anomaly detection
  let normal_data = []
  for i in 0..=200 {
    let x = StatisticalAnalyzer::normal_random(ml_analyzer, 5.0, 1.0)
    let y = StatisticalAnalyzer::normal_random(ml_analyzer, 5.0, 1.0)
    normal_data.push([x, y])
  }
  
  let anomaly_detector = MLAnalyzer::train_anomaly_detector(ml_analyzer, normal_data)
  
  // Test on normal data
  let normal_point = [5.0, 5.0]
  let normal_score = MLAnalyzer::anomaly_score(anomaly_detector, normal_point)
  assert_true(normal_score < 0.5) // Should be considered normal
  
  // Test on anomalous data
  let anomalous_point = [15.0, 15.0]
  let anomalous_score = MLAnalyzer::anomaly_score(anomaly_detector, anomalous_point)
  assert_true(anomalous_score > 0.5) // Should be considered anomalous
}

// Test 4: Data Visualization and Reporting
test "data visualization and reporting" {
  let visualization_engine = VisualizationEngine::new()
  
  // Generate sample data for visualization
  let time_series_data = []
  for i in 0..=30 {
    let timestamp = 1640995200L + i * 86400L // Daily data
    let value = 100.0 + 10.0 * Math::sin(i.to_float() * 0.2) + (Math::random() * 20.0) - 10.0
    time_series_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // Create line chart
  let line_chart = VisualizationEngine::create_line_chart(visualization_engine, time_series_data, [
    ("title", StringValue("Daily Metrics Over Time")),
    ("x_axis_label", StringValue("Date")),
    ("y_axis_label", StringValue("Value")),
    ("width", IntValue(800)),
    ("height", IntValue(400))
  ])
  
  assert_true(VisualizationEngine::is_valid(line_chart))
  
  // Generate sample categorical data
  let categorical_data = [
    CategoryDataPoint::new("Category A", 45),
    CategoryDataPoint::new("Category B", 30),
    CategoryDataPoint::new("Category C", 15),
    CategoryDataPoint::new("Category D", 10)
  ]
  
  // Create pie chart
  let pie_chart = VisualizationEngine::create_pie_chart(visualization_engine, categorical_data, [
    ("title", StringValue("Category Distribution")),
    ("width", IntValue(600)),
    ("height", IntValue(400))
  ])
  
  assert_true(VisualizationEngine::is_valid(pie_chart))
  
  // Create bar chart
  let bar_chart = VisualizationEngine::create_bar_chart(visualization_engine, categorical_data, [
    ("title", StringValue("Category Counts")),
    ("x_axis_label", StringValue("Category")),
    ("y_axis_label", StringValue("Count")),
    ("width", IntValue(800)),
    ("height", IntValue(400))
  ])
  
  assert_true(VisualizationEngine::is_valid(bar_chart))
  
  // Generate sample heatmap data
  let heatmap_data = []
  for i in 0..=10 {
    let row = []
    for j in 0..=10 {
      let value = Math::sin(i.to_float() * 0.5) * Math::cos(j.to_float() * 0.5)
      row.push(value)
    }
    heatmap_data.push(row)
  }
  
  // Create heatmap
  let heatmap = VisualizationEngine::create_heatmap(visualization_engine, heatmap_data, [
    ("title", StringValue("Correlation Matrix")),
    ("colormap", StringValue("viridis")),
    ("width", IntValue(600)),
    ("height", IntValue(600))
  ])
  
  assert_true(VisualizationEngine::is_valid(heatmap))
  
  // Create dashboard with multiple visualizations
  let dashboard = VisualizationEngine::create_dashboard(visualization_engine, [
    ("title", StringValue("Analytics Dashboard")),
    ("layout", StringValue("grid")),
    ("columns", IntValue(2))
  ])
  
  VisualizationEngine::add_to_dashboard(visualization_engine, dashboard, line_chart, 0, 0)
  VisualizationEngine::add_to_dashboard(visualization_engine, dashboard, pie_chart, 0, 1)
  VisualizationEngine::add_to_dashboard(visualization_engine, dashboard, bar_chart, 1, 0)
  VisualizationEngine::add_to_dashboard(visualization_engine, dashboard, heatmap, 1, 1)
  
  assert_true(VisualizationEngine::is_valid(dashboard))
  
  // Export visualizations
  let line_chart_svg = VisualizationEngine::export_svg(visualization_engine, line_chart)
  assert_true(line_chart_svg.length() > 0)
  assert_true(line_chart_svg.contains("<svg"))
  
  let dashboard_html = VisualizationEngine::export_html(visualization_engine, dashboard)
  assert_true(dashboard_html.length() > 0)
  assert_true(dashboard_html.contains("<html"))
}

// Test 5: Real-time Stream Processing
test "real-time stream processing" {
  let stream_processor = StreamProcessor::new()
  
  // Create stream processors for different analytics
  let moving_average_processor = StreamProcessor::create_moving_average_processor(stream_processor, 10)
  let anomaly_detector = StreamProcessor::create_anomaly_detector(stream_processor, 2.0) // 2 sigma threshold
  let trend_analyzer = StreamProcessor::create_trend_analyzer(stream_processor, 20) // 20-point window
  
  // Simulate real-time data stream
  let stream_data = []
  let base_value = 100.0
  for i in 0..=100 {
    let timestamp = 1640995200L + i * 60L // Minute-level data
    let trend = 0.1 * i.to_float() // Gradual increase
    let noise = (Math::random() * 10.0) - 5.0
    let value = base_value + trend + noise
    
    let data_point = StreamDataPoint::new(timestamp, value)
    stream_data.push(data_point)
    
    // Process each point through the pipeline
    let ma_result = StreamProcessor::process_point(moving_average_processor, data_point)
    let anomaly_result = StreamProcessor::process_point(anomaly_detector, data_point)
    let trend_result = StreamProcessor::process_point(trend_analyzer, data_point)
    
    // Verify moving average calculation
    if i >= 10 {
      assert_true(ma_result.has_value)
      assert_true(ma_result.value >= base_value - 10.0)
      assert_true(ma_result.value <= base_value + trend + 10.0)
    }
    
    // Verify anomaly detection
    if i >= 20 {
      if Math::abs(noise) > 8.0 { // Significant noise
        assert_true(anomaly_result.is_anomaly)
      }
    }
    
    // Verify trend analysis
    if i >= 20 {
      assert_true(trend_result.has_trend)
      assert_true(trend_result.trend_direction == "increasing")
    }
  }
  
  // Test window-based aggregations
  let window_size = 10
  let windows = StreamProcessor::create_windows(stream_processor, stream_data, window_size, 5) // 10-point windows with 5-point overlap
  
  assert_true(windows.length() > 0)
  
  for window in windows {
    assert_eq(window.points.length(), window_size)
    
    // Calculate window statistics
    let window_stats = StreamProcessor::calculate_window_stats(window)
    assert_true(window_stats.mean >= 0.0)
    assert_true(window_stats.std_dev >= 0.0)
    assert_true(window_stats.min <= window_stats.mean)
    assert_true(window_stats.max >= window_stats.mean)
  }
  
  // Test stream aggregation
  let aggregated_metrics = StreamProcessor::aggregate_stream(stream_processor, stream_data, [
    ("count", AggregationType::Count),
    ("sum", AggregationType::Sum),
    ("mean", AggregationType::Mean),
    ("min", AggregationType::Min),
    ("max", AggregationType::Max),
    ("std_dev", AggregationType::StandardDeviation)
  ])
  
  assert_eq(aggregated_metrics["count"], 101)
  assert_true(aggregated_metrics["sum"] > 0.0)
  assert_true(aggregated_metrics["mean"] > 0.0)
  assert_true(aggregated_metrics["min"] <= aggregated_metrics["mean"])
  assert_true(aggregated_metrics["max"] >= aggregated_metrics["mean"])
  assert_true(aggregated_metrics["std_dev"] >= 0.0)
}

// Test 6: Predictive Analytics
test "predictive analytics and forecasting models" {
  let predictive_analyzer = PredictiveAnalyzer::new()
  
  // Generate time series with trend and seasonality
  let historical_data = []
  for i in 0..=365 {
    let timestamp = 1640995200L + i * 86400L // Daily data for 1 year
    let trend = 0.1 * i.to_float() // Linear trend
    let seasonal = 20.0 * Math::sin(2.0 * Math::PI * i.to_float() / 365.0) // Annual seasonality
    let weekly = 5.0 * Math::sin(2.0 * Math::PI * i.to_float() / 7.0) // Weekly seasonality
    let noise = (Math::random() * 10.0) - 5.0
    
    let value = 100.0 + trend + seasonal + weekly + noise
    historical_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // Train different forecasting models
  let linear_model = PredictiveAnalyzer::train_linear_regression(predictive_analyzer, historical_data)
  let arima_model = PredictiveAnalyzer::train_arima(predictive_analyzer, historical_data, [1, 1, 1]) // ARIMA(1,1,1)
  let seasonal_model = PredictiveAnalyzer::train_seasonal_decomposition(predictive_analyzer, historical_data)
  
  // Generate forecasts
  let forecast_horizon = 30 // 30 days
  let linear_forecast = PredictiveAnalyzer::forecast(predictive_analyzer, linear_model, forecast_horizon)
  let arima_forecast = PredictiveAnalyzer::forecast(predictive_analyzer, arima_model, forecast_horizon)
  let seasonal_forecast = PredictiveAnalyzer::forecast(predictive_analyzer, seasonal_model, forecast_horizon)
  
  assert_eq(linear_forecast.values.length(), forecast_horizon)
  assert_eq(arima_forecast.values.length(), forecast_horizon)
  assert_eq(seasonal_forecast.values.length(), forecast_horizon)
  
  // Evaluate model accuracy using cross-validation
  let linear_accuracy = PredictiveAnalyzer::cross_validate(predictive_analyzer, linear_model, historical_data, 10)
  let arima_accuracy = PredictiveAnalyzer::cross_validate(predictive_analyzer, arima_model, historical_data, 10)
  let seasonal_accuracy = PredictiveAnalyzer::cross_validate(predictive_analyzer, seasonal_model, historical_data, 10)
  
  assert_true(linear_accuracy.rmse > 0.0)
  assert_true(arima_accuracy.rmse > 0.0)
  assert_true(seasonal_accuracy.rmse > 0.0)
  
  // Seasonal model should perform better on seasonal data
  assert_true(seasonal_accuracy.rmse < linear_accuracy.rmse)
  
  // Test ensemble forecasting
  let ensemble_model = PredictiveAnalyzer::create_ensemble(predictive_analyzer, 
    [linear_model, arima_model, seasonal_model], [0.3, 0.3, 0.4]) // Weighted ensemble
  let ensemble_forecast = PredictiveAnalyzer::forecast(predictive_analyzer, ensemble_model, forecast_horizon)
  
  assert_eq(ensemble_forecast.values.length(), forecast_horizon)
  
  // Test prediction intervals
  assert_true(ensemble_forecast.upper_bounds.length() == forecast_horizon)
  assert_true(ensemble_forecast.lower_bounds.length() == forecast_horizon)
  
  for i in 0..=forecast_horizon - 1 {
    assert_true(ensemble_forecast.upper_bounds[i] >= ensemble_forecast.values[i])
    assert_true(ensemble_forecast.lower_bounds[i] <= ensemble_forecast.values[i])
  }
  
  // Test feature importance analysis
  let features = [
    Feature::new("trend", [i.to_float() for i in 0..=365]),
    Feature::new("seasonal_annual", [Math::sin(2.0 * Math::PI * i.to_float() / 365.0) for i in 0..=365]),
    Feature::new("seasonal_weekly", [Math::sin(2.0 * Math::PI * i.to_float() / 7.0) for i in 0..=365])
  ]
  
  let target = [point.value for point in historical_data]
  let feature_importance = PredictiveAnalyzer::analyze_feature_importance(predictive_analyzer, features, target)
  
  assert_eq(feature_importance.length(), 3)
  assert_true(feature_importance[0].importance >= 0.0)
  assert_true(feature_importance[0].importance <= 1.0)
  
  // Features should sum to 1.0 (normalized importance)
  let total_importance = feature_importance.reduce(0.0, fn(acc, f) { acc + f.importance })
  assert_true(Math::abs(total_importance - 1.0) < 0.001)
}

// Test 7: Root Cause Analysis
test "root cause analysis and correlation" {
  let rca_analyzer = RootCauseAnalyzer::new()
  
  // Generate correlated telemetry data
  let telemetry_events = []
  
  // Normal operation period
  for i in 0..=100 {
    let timestamp = 1640995200L + i * 60L // Minute-level data
    let cpu_usage = 30.0 + (Math::random() * 20.0) // 30-50%
    let memory_usage = 40.0 + (Math::random() * 20.0) // 40-60%
    let response_time = 100.0 + (Math::random() * 50.0) // 100-150ms
    let error_rate = 0.01 + (Math::random() * 0.02) // 1-3%
    
    telemetry_events.push(TelemetryEvent::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("response_time", response_time),
      ("error_rate", error_rate)
    ]))
  }
  
  // Problem period - high CPU causing increased response time and errors
  for i in 101..=150 {
    let timestamp = 1640995200L + i * 60L
    let cpu_usage = 80.0 + (Math::random() * 15.0) // 80-95%
    let memory_usage = 45.0 + (Math::random() * 20.0) // 45-65%
    let response_time = 500.0 + (Math::random() * 200.0) // 500-700ms
    let error_rate = 0.05 + (Math::random() * 0.1) // 5-15%
    
    telemetry_events.push(TelemetryEvent::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("response_time", response_time),
      ("error_rate", error_rate)
    ]))
  }
  
  // Recovery period
  for i in 151..=200 {
    let timestamp = 1640995200L + i * 60L
    let cpu_usage = 35.0 + (Math::random() * 20.0) // 35-55%
    let memory_usage = 42.0 + (Math::random() * 20.0) // 42-62%
    let response_time = 120.0 + (Math::random() * 60.0) // 120-180ms
    let error_rate = 0.015 + (Math::random() * 0.025) // 1.5-4%
    
    telemetry_events.push(TelemetryEvent::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("response_time", response_time),
      ("error_rate", error_rate)
    ]))
  }
  
  // Detect anomalies in the telemetry data
  let anomalies = RootCauseAnalyzer::detect_anomalies(rca_analyzer, telemetry_events)
  assert_true(anomalies.length() > 0)
  
  // Most anomalies should be in the problem period (events 101-150)
  let problem_period_anomalies = anomalies.filter(fn(a) { a.event_index >= 101 && a.event_index <= 150 })
  assert_true(problem_period_anomalies.length() > anomalies.length() / 2)
  
  // Analyze correlations between metrics
  let correlations = RootCauseAnalyzer::analyze_correlations(rca_analyzer, telemetry_events)
  
  // CPU usage should be strongly correlated with response time and error rate
  let cpu_response_corr = correlations[("cpu_usage", "response_time")]
  let cpu_error_corr = correlations[("cpu_usage", "error_rate")]
  
  assert_true(cpu_response_corr > 0.5) // Strong positive correlation
  assert_true(cpu_error_corr > 0.5) // Strong positive correlation
  
  // Perform root cause analysis
  let rca_result = RootCauseAnalyzer::analyze_root_cause(rca_analyzer, telemetry_events, 125) // Analyze around event 125
  
  assert_true(rca_result.root_causes.length() > 0)
  
  // CPU usage should be identified as a root cause
  let cpu_root_cause = rca_result.root_causes.find(fn(rc) { rc.metric == "cpu_usage" })
  assert_true(cpu_root_cause.is_some)
  
  // Verify root cause confidence
  match cpu_root_cause {
    Some(rc) => assert_true(rc.confidence > 0.7),
    None => assert_true(false)
  }
  
  // Test causal analysis
  let causal_graph = RootCauseAnalyzer::build_causal_graph(rca_analyzer, telemetry_events)
  assert_true(causal_graph.nodes.length() >= 4) // At least 4 metrics
  assert_true(causal_graph.edges.length() > 0)
  
  // CPU usage should have outgoing edges to response_time and error_rate
  let cpu_edges = causal_graph.edges.filter(fn(e) { e.from == "cpu_usage" })
  assert_true(cpu_edges.length() >= 2)
  
  // Test impact analysis
  let impact_analysis = RootCauseAnalyzer::analyze_impact(rca_analyzer, telemetry_events, "cpu_usage")
  assert_true(impact_analysis.affected_metrics.length() >= 2)
  assert_true(impact_analysis.affected_metrics.contains("response_time"))
  assert_true(impact_analysis.affected_metrics.contains("error_rate"))
}

// Test 8: Behavioral Analytics
test "behavioral analytics and user journey analysis" {
  let behavior_analyzer = BehaviorAnalyzer::new()
  
  // Generate user session data
  let user_sessions = []
  
  // Session 1: Normal user journey
  user_sessions.push(UserSession::new("user1", [
    UserAction::new("page_view", "/home", 1640995200L),
    UserAction::new("page_view", "/products", 1640995260L),
    UserAction::new("click", "product_details", 1640995320L),
    UserAction::new("add_to_cart", "product_123", 1640995380L),
    UserAction::new("page_view", "/cart", 1640995440L),
    UserAction::new("click", "checkout", 1640995500L),
    UserAction::new("page_view", "/checkout", 1640995560L),
    UserAction::new("submit", "payment", 1640995620L),
    UserAction::new("page_view", "/confirmation", 1640995680L)
  ]))
  
  // Session 2: Abandoned cart
  user_sessions.push(UserSession::new("user2", [
    UserAction::new("page_view", "/home", 1640996000L),
    UserAction::new("page_view", "/products", 1640996060L),
    UserAction::new("click", "product_details", 1640996120L),
    UserAction::new("add_to_cart", "product_456", 1640996180L),
    UserAction::new("page_view", "/cart", 1640996240L),
    UserAction::new("exit", "", 1640996300L) // User left
  ]))
  
  // Session 3: Browser only
  user_sessions.push(UserSession::new("user3", [
    UserAction::new("page_view", "/home", 1640997000L),
    UserAction::new("page_view", "/products", 1640997060L),
    UserAction::new("page_view", "/about", 1640997120L),
    UserAction::new("page_view", "/contact", 1640997180L),
    UserAction::new("exit", "", 1640997240L)
  ]))
  
  // More sessions...
  for i in 0..=50 {
    let user_id = "user" + (i + 4).to_string()
    let actions = []
    
    // Random journey
    let base_time = 1640998000L + i * 1000L
    let num_actions = 3 + (Math::random() * 10.0).to_int()
    
    for j in 0..=num_actions {
      let action_type = match (Math::random() * 4.0).to_int() {
        0 => "page_view",
        1 => "click",
        2 => "add_to_cart",
        _ => "exit"
      }
      
      let page = match (Math::random() * 4.0).to_int() {
        0 => "/home",
        1 => "/products",
        2 => "/cart",
        _ => "/checkout"
      }
      
      actions.push(UserAction::new(action_type, page, base_time + j * 60L))
    }
    
    user_sessions.push(UserSession::new(user_id, actions))
  }
  
  // Analyze user behavior patterns
  let behavior_patterns = BehaviorAnalyzer::identify_patterns(behavior_analyzer, user_sessions)
  assert_true(behavior_patterns.length() > 0)
  
  // Should identify common patterns like "browse -> add to cart -> checkout"
  let purchase_pattern = behavior_patterns.find(fn(p) { 
    p.name == "purchase_funnel" || p.actions.contains("checkout") 
  })
  assert_true(purchase_pattern.is_some)
  
  // Analyze conversion funnel
  let funnel_analysis = BehaviorAnalyzer::analyze_conversion_funnel(behavior_analyzer, user_sessions, [
    "page_view:/home",
    "page_view:/products",
    "add_to_cart",
    "page_view:/checkout",
    "submit:payment"
  ])
  
  assert_eq(funnel_analysis.stages.length(), 5)
  assert_true(funnel_analysis.stages[0].users >= funnel_analysis.stages[1].users) // Funnel should decrease
  assert_true(funnel_analysis.stages[1].users >= funnel_analysis.stages[2].users)
  
  // Calculate conversion rates
  let conversion_rates = BehaviorAnalyzer::calculate_conversion_rates(behavior_analyzer, funnel_analysis)
  assert_eq(conversion_rates.length(), 4) // 4 conversion rates between 5 stages
  
  for rate in conversion_rates {
    assert_true(rate >= 0.0 && rate <= 1.0)
  }
  
  // Analyze user segmentation
  let segments = BehaviorAnalyzer::segment_users(behavior_analyzer, user_sessions, 3) // 3 segments
  assert_eq(segments.length(), 3)
  
  // Verify each segment has users
  for segment in segments {
    assert_true(segment.users.length() > 0)
    assert_true(segment.characteristics.length() > 0)
  }
  
  // Analyze user journeys
  let journeys = BehaviorAnalyzer::analyze_user_journeys(behavior_analyzer, user_sessions)
  assert_true(journeys.length() > 0)
  
  // Should identify common journeys
  let common_journeys = journeys.filter(fn(j) { j.frequency > 1 })
  assert_true(common_journeys.length() > 0)
  
  // Test journey similarity analysis
  let journey1 = user_sessions[0].actions
  let journey2 = user_sessions[1].actions
  let similarity = BehaviorAnalyzer::calculate_journey_similarity(behavior_analyzer, journey1, journey2)
  
  assert_true(similarity >= 0.0 && similarity <= 1.0)
}

// Test 9: Performance Analytics
test "performance analytics and optimization" {
  let performance_analyzer = PerformanceAnalyzer::new()
  
  // Generate performance metrics data
  let performance_data = []
  
  for i in 0..=1000 {
    let timestamp = 1640995200L + i * 60L // Minute-level data
    let cpu_usage = 30.0 + (Math::random() * 40.0) // 30-70%
    let memory_usage = 40.0 + (Math::random() * 30.0) // 40-70%
    let disk_io = 50.0 + (Math::random() * 100.0) // 50-150 IOPS
    let network_io = 100.0 + (Math::random() * 200.0) // 100-300 KB/s
    let response_time = 100.0 + (Math::random() * 400.0) // 100-500ms
    let throughput = 500.0 + (Math::random() * 1000.0) // 500-1500 req/s
    
    performance_data.push(PerformanceMetrics::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("disk_io", disk_io),
      ("network_io", network_io),
      ("response_time", response_time),
      ("throughput", throughput)
    ]))
  }
  
  // Identify performance bottlenecks
  let bottlenecks = PerformanceAnalyzer::identify_bottlenecks(performance_analyzer, performance_data)
  assert_true(bottlenecks.length() > 0)
  
  // Each bottleneck should have a metric and severity
  for bottleneck in bottlenecks {
    assert_true(bottleneck.metric.length() > 0)
    assert_true(bottleneck.severity >= 0.0 && bottleneck.severity <= 1.0)
  }
  
  // Analyze performance trends
  let trends = PerformanceAnalyzer::analyze_trends(performance_analyzer, performance_data)
  assert_true(trends.length() >= 6) // At least one trend per metric
  
  // Verify trend analysis
  for trend in trends {
    assert_true(trend.direction == "increasing" || trend.direction == "decreasing" || trend.direction == "stable")
    assert_true(trend.strength >= 0.0 && trend.strength <= 1.0)
    assert_true(trend.confidence >= 0.0 && trend.confidence <= 1.0)
  }
  
  // Find performance correlations
  let correlations = PerformanceAnalyzer::find_correlations(performance_analyzer, performance_data)
  assert_true(correlations.length() > 0)
  
  // CPU usage should be correlated with response time
  let cpu_response_corr = correlations.find(fn(c) { 
    (c.metric1 == "cpu_usage" && c.metric2 == "response_time") ||
    (c.metric1 == "response_time" && c.metric2 == "cpu_usage")
  })
  assert_true(cpu_response_corr.is_some)
  
  // Optimize performance parameters
  let optimization_result = PerformanceAnalyzer::optimize_parameters(performance_analyzer, performance_data, [
    ("cpu_threshold", 80.0),
    ("memory_threshold", 80.0),
    ("response_time_threshold", 300.0)
  ])
  
  assert_true(optimization_result.optimized_parameters.length() > 0)
  assert_true(optimization_result.expected_improvement > 0.0)
  
  // Predict performance under load
  let load_scenarios = [
    LoadScenario::new("low", 1.2), // 20% increase
    LoadScenario::new("medium", 1.5), // 50% increase
    LoadScenario::new("high", 2.0) // 100% increase
  ]
  
  let performance_predictions = PerformanceAnalyzer::predict_under_load(
    performance_analyzer, performance_data, load_scenarios)
  
  assert_eq(performance_predictions.length(), 3)
  
  for prediction in performance_predictions {
    assert_true(prediction.predicted_metrics.length() >= 6) // All metrics
    assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
  }
  
  // Higher load should result in higher resource usage
  let low_prediction = performance_predictions[0]
  let high_prediction = performance_predictions[2]
  
  let low_cpu = low_prediction.predicted_metrics.find(fn(m) { m.metric == "cpu_usage" }).value
  let high_cpu = high_prediction.predicted_metrics.find(fn(m) { m.metric == "cpu_usage" }).value
  
  assert_true(high_cpu > low_cpu)
}

// Test 10: Anomaly Detection and Alerting
test "anomaly detection and intelligent alerting" {
  let anomaly_detector = AnomalyDetector::new()
  
  // Generate normal baseline data
  let baseline_data = []
  for i in 0..=500 {
    let timestamp = 1640995200L + i * 60L // Minute-level data
    let cpu_usage = 40.0 + (Math::random() * 20.0) // 40-60%
    let memory_usage = 50.0 + (Math::random() * 15.0) // 50-65%
    let response_time = 150.0 + (Math::random() * 100.0) // 150-250ms
    
    baseline_data.push(MetricsDataPoint::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("response_time", response_time)
    ]))
  }
  
  // Train anomaly detection models
  let statistical_model = AnomalyDetector::train_statistical_model(anomaly_detector, baseline_data)
  let ml_model = AnomalyDetector::train_ml_model(anomaly_detector, baseline_data)
  
  // Generate test data with anomalies
  let test_data = []
  
  // Normal data
  for i in 0..=50 {
    let timestamp = 1640995200L + (500 + i) * 60L
    let cpu_usage = 40.0 + (Math::random() * 20.0)
    let memory_usage = 50.0 + (Math::random() * 15.0)
    let response_time = 150.0 + (Math::random() * 100.0)
    
    test_data.push(MetricsDataPoint::new(timestamp, [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("response_time", response_time)
    ]))
  }
  
  // Add anomalies
  test_data.push(MetricsDataPoint::new(1640995200L + 551 * 60L, [
    ("cpu_usage", 95.0), // CPU spike
    ("memory_usage", 55.0),
    ("response_time", 180.0)
  ]))
  
  test_data.push(MetricsDataPoint::new(1640995200L + 552 * 60L, [
    ("cpu_usage", 45.0),
    ("memory_usage", 95.0), // Memory spike
    ("response_time", 200.0)
  ]))
  
  test_data.push(MetricsDataPoint::new(1640995200L + 553 * 60L, [
    ("cpu_usage", 42.0),
    ("memory_usage", 52.0),
    ("response_time", 800.0) // Response time spike
  ]))
  
  // Detect anomalies using both models
  let statistical_anomalies = AnomalyDetector::detect_anomalies(anomaly_detector, test_data, statistical_model)
  let ml_anomalies = AnomalyDetector::detect_anomalies(anomaly_detector, test_data, ml_model)
  
  assert_true(statistical_anomalies.length() >= 3)
  assert_true(ml_anomalies.length() >= 3)
  
  // Verify anomalies are detected at the right points
  let statistical_anomaly_indices = statistical_anomalies.map(fn(a) { a.data_point_index })
  let ml_anomaly_indices = ml_anomalies.map(fn(a) { a.data_point_index })
  
  assert_true(statistical_anomaly_indices.contains(51)) // CPU spike
  assert_true(statistical_anomaly_indices.contains(52)) // Memory spike
  assert_true(statistical_anomaly_indices.contains(53)) // Response time spike
  
  assert_true(ml_anomaly_indices.contains(51)) // CPU spike
  assert_true(ml_anomaly_indices.contains(52)) // Memory spike
  assert_true(ml_anomaly_indices.contains(53)) // Response time spike
  
  // Test ensemble anomaly detection
  let ensemble_model = AnomalyDetector::create_ensemble(anomaly_detector, 
    [statistical_model, ml_model], [0.5, 0.5])
  let ensemble_anomalies = AnomalyDetector::detect_anomalies(anomaly_detector, test_data, ensemble_model)
  
  assert_true(ensemble_anomalies.length() >= 3)
  
  // Test intelligent alerting
  let alert_manager = AlertManager::new()
  
  // Configure alert rules
  AlertManager::add_rule(alert_manager, AlertRule::new("cpu_spike", [
    AlertCondition::new("cpu_usage", ">", 80.0),
    AlertCondition::new("duration", ">", 300) // 5 minutes
  ], [
    ("severity", StringValue("warning")),
    ("cooldown", IntValue(900)) // 15 minutes cooldown
  ]))
  
  AlertManager::add_rule(alert_manager, AlertRule::new("memory_spike", [
    AlertCondition::new("memory_usage", ">", 80.0)
  ], [
    ("severity", StringValue("warning")),
    ("cooldown", IntValue(900))
  ]))
  
  AlertManager::add_rule(alert_manager, AlertRule::new("response_time_spike", [
    AlertCondition::new("response_time", ">", 500.0)
  ], [
    ("severity", StringValue("critical")),
    ("cooldown", IntValue(600)) // 10 minutes cooldown
  ]))
  
  // Process anomalies and generate alerts
  let alerts = []
  for anomaly in ensemble_anomalies {
    let data_point = test_data[anomaly.data_point_index]
    let alert = AlertManager::evaluate_rules(alert_manager, data_point)
    if alert.is_some {
      alerts.push(alert.get)
    }
  }
  
  assert_true(alerts.length() >= 2) // At least CPU and response time alerts
  
  // Verify alert properties
  for alert in alerts {
    assert_true(alert.rule_name.length() > 0)
    assert_true(alert.severity == "warning" || alert.severity == "critical")
    assert_true(alert.timestamp > 0)
    assert_true(alert.triggering_conditions.length() > 0)
  }
  
  // Test alert deduplication
  let deduplicated_alerts = AlertManager::deduplicate_alerts(alert_manager, alerts)
  assert_true(deduplicated_alerts.length() <= alerts.length())
  
  // Test alert escalation
  let escalated_alerts = AlertManager::escalate_alerts(alert_manager, deduplicated_alerts, [
    ("response_time_spike", "critical") // Escalate response time alerts to critical
  ])
  
  let critical_alerts = escalated_alerts.filter(fn(a) { a.severity == "critical" })
  assert_true(critical_alerts.length() >= 1) // At least the response time alert
  
  // Test alert notification
  let notification_channels = [
    NotificationChannel::new("email", ["ops@example.com"]),
    NotificationChannel::new("slack", ["#alerts"]),
    NotificationChannel::new("pagerduty", ["oncall"])
  ]
  
  let notifications = []
  for alert in escalated_alerts {
    let channel_notifications = AlertManager::send_notifications(alert_manager, alert, notification_channels)
    for notification in channel_notifications {
      notifications.push(notification)
    }
  }
  
  assert_true(notifications.length() >= escalated_alerts.length() * notification_channels.length())
  
  // Verify notification properties
  for notification in notifications {
    assert_true(notification.channel.length() > 0)
    assert_true(notification.message.length() > 0)
    assert_true(notification.recipients.length() > 0)
    assert_true(notification.sent_at > 0)
  }
}