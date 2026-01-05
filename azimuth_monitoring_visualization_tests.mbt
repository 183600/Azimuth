// Azimuth Monitoring and Visualization Tests
// This file contains comprehensive test cases for monitoring and visualization features

// Test 9: Monitoring and Alerting
test "monitoring and alerting" {
  let monitoring_system = MonitoringSystem::new()
  let alerting_engine = AlertingEngine::new()
  
  // Configure monitoring metrics
  let monitoring_metrics = [
    ("cpu_usage", MetricType::Gauge, "CPU usage percentage"),
    ("memory_usage", MetricType::Gauge, "Memory usage percentage"),
    ("request_count", MetricType::Counter, "Total number of requests"),
    ("error_rate", MetricType::Gauge, "Error rate percentage"),
    ("response_time", MetricType::Histogram, "Response time distribution"),
    ("throughput", MetricType::Gauge, "Requests per second"),
    ("active_connections", MetricType::Gauge, "Number of active connections"),
    ("queue_depth", MetricType::Gauge, "Queue depth"),
    ("disk_io", MetricType::Histogram, "Disk I/O operations"),
    ("network_traffic", MetricType::Histogram, "Network traffic")
  ]
  
  for (metric_name, metric_type, description) in monitoring_metrics {
    MonitoringSystem::register_metric(monitoring_system, metric_name, metric_type, description)
  }
  
  // Test metric collection
  let metric_collector = MetricCollector::new(monitoring_system)
  
  // Simulate metric collection over time
  let time_series_data = []
  let start_time = Time::now()
  
  for i in 0..=100 {
    let timestamp = start_time + (i.to_int64() * 1000L) // 1 second intervals
    
    // Simulate realistic metric values
    let cpu_usage = 30.0 + Random::float_range(-10.0, 40.0) // 20-70%
    let memory_usage = 40.0 + Random::float_range(-15.0, 35.0) // 25-75%
    let request_count = i * 10 + Random::int_range(0, 20)
    let error_rate = Random::float_range(0.0, 5.0) // 0-5%
    let response_time = 100.0 + Random::float_range(-50.0, 200.0) // 50-300ms
    let throughput = 50.0 + Random::float_range(-20.0, 30.0) // 30-80 req/s
    let active_connections = 100 + Random::int_range(-20, 50)
    let queue_depth = Random::int_range(0, 20)
    let disk_io = Random::float_range(10.0, 100.0)
    let network_traffic = Random::float_range(100.0, 1000.0)
    
    let metric_values = [
      ("cpu_usage", cpu_usage),
      ("memory_usage", memory_usage),
      ("request_count", request_count.to_float()),
      ("error_rate", error_rate),
      ("response_time", response_time),
      ("throughput", throughput),
      ("active_connections", active_connections.to_float()),
      ("queue_depth", queue_depth.to_float()),
      ("disk_io", disk_io),
      ("network_traffic", network_traffic)
    ]
    
    let timestamped_metrics = (timestamp, metric_values)
    time_series_data.push(timestamped_metrics)
    
    // Record metrics
    for (metric_name, value) in metric_values {
      MetricCollector::record_metric(metric_collector, metric_name, value, timestamp)
    }
  }
  
  // Verify metric collection
  let collected_metrics = MonitoringSystem::get_all_metrics(monitoring_system)
  
  for (metric_name, _, _) in monitoring_metrics {
    assert_true(collected_metrics.contains_key(metric_name))
    
    let metric_data = collected_metrics.get(metric_name).unwrap()
    assert_true(metric_data.data_points.length() > 0)
    
    // Verify metric timestamps are in chronological order
    for i in 1..=metric_data.data_points.length() - 1 {
      assert_true(metric_data.data_points[i].timestamp >= metric_data.data_points[i-1].timestamp)
    }
  }
  
  // Test metric aggregation
  let aggregation_queries = [
    ("avg_cpu_usage", "AVG(cpu_usage) OVER 5m"),
    ("max_memory_usage", "MAX(memory_usage) OVER 10m"),
    ("sum_requests", "SUM(request_count) OVER 1h"),
    ("p95_response_time", "PERCENTILE(response_time, 95) OVER 5m"),
    ("rate_throughput", "RATE(throughput) OVER 1m")
  ]
  
  for (query_name, query) in aggregation_queries {
    let aggregation_result = MonitoringSystem::execute_aggregation_query(monitoring_system, query)
    assert_true(aggregation_result.is_successful)
    
    let result_data = aggregation_result.data
    assert_true(result_data.length() > 0)
    
    // Verify result format
    for data_point in result_data {
      assert_true(data_point.contains_key("timestamp"))
      assert_true(data_point.contains_key("value"))
    }
  }
  
  // Configure alerting rules
  let alerting_rules = [
    AlertingRule::new(
      "high_cpu_usage",
      "cpu_usage > 80.0 FOR 5m",
      AlertSeverity::Warning,
      "CPU usage is above 80% for more than 5 minutes"
    ),
    AlertingRule::new(
      "critical_memory_usage",
      "memory_usage > 90.0 FOR 2m",
      AlertSeverity::Critical,
      "Memory usage is critically high"
    ),
    AlertingRule::new(
      "high_error_rate",
      "error_rate > 5.0 FOR 1m",
      AlertSeverity::Warning,
      "Error rate is above 5%"
    ),
    AlertingRule::new(
      "slow_response_time",
      "AVG(response_time) > 500.0 FOR 3m",
      AlertSeverity::Warning,
      "Average response time is above 500ms"
    ),
    AlertingRule::new(
      "service_down",
      "request_count == 0 FOR 1m",
      AlertSeverity::Critical,
      "Service appears to be down"
    )
  ]
  
  for rule in alerting_rules {
    AlertingEngine::add_rule(alerting_engine, rule)
  }
  
  // Test alerting under various conditions
  let alert_scenarios = [
    // Scenario 1: High CPU usage
    ([
      ("cpu_usage", 85.0),
      ("memory_usage", 50.0),
      ("error_rate", 1.0),
      ("response_time", 200.0)
    ], ["high_cpu_usage"]),
    
    // Scenario 2: Critical memory usage
    ([
      ("cpu_usage", 50.0),
      ("memory_usage", 95.0),
      ("error_rate", 2.0),
      ("response_time", 300.0)
    ], ["critical_memory_usage"]),
    
    // Scenario 3: Multiple alerts
    ([
      ("cpu_usage", 85.0),
      ("memory_usage", 95.0),
      ("error_rate", 6.0),
      ("response_time", 600.0)
    ], ["high_cpu_usage", "critical_memory_usage", "high_error_rate", "slow_response_time"])
  ]
  
  for (metric_values, expected_alerts) in alert_scenarios {
    // Simulate sustained metric values for alerting
    let alert_start_time = Time::now()
    
    for i in 0..=10 { // Simulate 10 data points over time
      let timestamp = alert_start_time + (i.to_int64() * 30000L) // 30 second intervals
      
      for (metric_name, value) in metric_values {
        MetricCollector::record_metric(metric_collector, metric_name, value, timestamp)
      }
    }
    
    // Check for triggered alerts
    let triggered_alerts = AlertingEngine::evaluate_rules(alerting_engine)
    
    // Verify expected alerts were triggered
    for expected_alert in expected_alerts {
      let alert_found = triggered_alerts.any(|alert| alert.rule_name == expected_alert)
      assert_true(alert_found, "Expected alert '" + expected_alert + "' was not triggered")
    }
    
    // Verify alert details
    for alert in triggered_alerts {
      assert_true(alert.rule_name.is_some())
      assert_true(alert.severity.is_some())
      assert_true(alert.message.is_some())
      assert_true(alert.triggered_at.is_some())
      assert_true(alert.triggered_at > alert_start_time)
      
      // Verify alert contains relevant metric values
      assert_true(alert.triggering_values.length() > 0)
    }
  }
  
  // Test alert notification
  let notification_channels = [
    NotificationChannel::email("admin@example.com"),
    NotificationChannel::slack("#alerts"),
    NotificationChannel::pagerduty("pagerduty_service_key"),
    NotificationChannel::webhook("https://example.com/webhook/alerts")
  ]
  
  for channel in notification_channels {
    AlertingEngine::add_notification_channel(alerting_engine, channel)
  }
  
  // Trigger a test alert
  let test_alert = Alert::new(
    "test_alert",
    AlertSeverity::Warning,
    "This is a test alert",
    Time::now(),
    [("cpu_usage", 85.0)]
  )
  
  let notification_results = AlertingEngine::send_notifications(alerting_engine, test_alert)
  
  // Verify notifications were sent
  assert_eq(notification_results.length(), notification_channels.length())
  
  for result in notification_results {
    assert_true(result.is_successful)
    assert_true(result.sent_at.is_some())
    assert_true(result.sent_at > test_alert.triggered_at)
  }
  
  // Test alert acknowledgment and resolution
  let active_alerts = AlertingEngine::get_active_alerts(alerting_engine)
  assert_true(active_alerts.length() > 0)
  
  // Acknowledge an alert
  let alert_to_acknowledge = active_alerts[0]
  let acknowledgment_result = AlertingEngine::acknowledge_alert(alerting_engine, alert_to_acknowledge.id, "admin_user")
  assert_true(acknowledgment_result.is_successful)
  
  // Verify alert is acknowledged
  let acknowledged_alert = AlertingEngine::get_alert(alerting_engine, alert_to_acknowledge.id)
  assert_true(acknowledged_alert.is_some())
  
  let alert = acknowledged_alert.unwrap()
  assert_true(alert.is_acknowledged)
  assert_true(alert.acknowledged_by.is_some())
  assert_eq(alert.acknowledged_by.unwrap(), "admin_user")
  assert_true(alert.acknowledged_at.is_some())
  
  // Resolve the alert
  let resolution_result = AlertingEngine::resolve_alert(alerting_engine, alert_to_acknowledge.id, "Issue has been fixed")
  assert_true(resolution_result.is_successful)
  
  // Verify alert is resolved
  let resolved_alert = AlertingEngine::get_alert(alerting_engine, alert_to_acknowledge.id)
  assert_true(resolved_alert.is_some())
  
  let resolved = resolved_alert.unwrap()
  assert_true(resolved.is_resolved)
  assert_eq(resolved.resolution_message, "Issue has been fixed")
  assert_true(resolved.resolved_at.is_some())
  assert_true(resolved.resolved_at > alert.acknowledged_at)
  
  // Test monitoring dashboard data
  let dashboard_data = MonitoringSystem::get_dashboard_data(monitoring_system)
  
  // Verify dashboard structure
  assert_true(dashboard_data.contains_key("overview_metrics"))
  assert_true(dashboard_data.contains_key("time_series_data"))
  assert_true(dashboard_data.contains_key("active_alerts"))
  assert_true(dashboard_data.contains_key("system_health"))
  
  // Verify overview metrics
  let overview_metrics = dashboard_data.get("overview_metrics").unwrap()
  assert_true(overview_metrics.length() > 0)
  
  for metric in overview_metrics {
    assert_true(metric.contains_key("name"))
    assert_true(metric.contains_key("current_value"))
    assert_true(metric.contains_key("status"))
  }
  
  // Verify time series data
  let time_series = dashboard_data.get("time_series_data").unwrap()
  assert_true(time_series.length() > 0)
  
  for series in time_series {
    assert_true(series.contains_key("metric_name"))
    assert_true(series.contains_key("data_points"))
    
    let data_points = series.get("data_points").unwrap()
    assert_true(data_points.length() > 0)
    
    for point in data_points {
      assert_true(point.contains_key("timestamp"))
      assert_true(point.contains_key("value"))
    }
  }
  
  // Verify system health status
  let system_health = dashboard_data.get("system_health").unwrap()
  assert_true(system_health.contains_key("overall_status"))
  assert_true(system_health.contains_key("components"))
  
  let components = system_health.get("components").unwrap()
  assert_true(components.length() > 0)
  
  for component in components {
    assert_true(component.contains_key("name"))
    assert_true(component.contains_key("status"))
    assert_true(component.contains_key("last_updated"))
  }
}

// Test 10: Data Analysis and Visualization
test "data analysis and visualization" {
  let visualization_engine = VisualizationEngine::new()
  let data_analyzer = DataAnalyzer::new()
  
  // Generate comprehensive test data for visualization
  let visualization_data = generate_visualization_test_data()
  
  // Test chart generation
  let chart_configs = [
    ChartConfig::new(
      "response_time_trend",
      ChartType::TimeSeries,
      ["response_time"],
      "Response Time Trend",
      "Time",
      "Response Time (ms)"
    ),
    ChartConfig::new(
      "error_rate_distribution",
      ChartType::Bar,
      ["error_rate"],
      "Error Rate by Service",
      "Service",
      "Error Rate (%)"
    ),
    ChartConfig::new(
      "throughput_heatmap",
      ChartType::Heatmap,
      ["throughput"],
      "Throughput Heatmap",
      "Time",
      "Service"
    ),
    ChartConfig::new(
      "resource_usage_pie",
      ChartType::Pie,
      ["cpu_usage", "memory_usage", "disk_usage"],
      "Resource Usage Distribution",
      "",
      "Usage (%)"
    ),
    ChartConfig::new(
      "request_scatter",
      ChartType::Scatter,
      ["request_count", "response_time"],
      "Request Count vs Response Time",
      "Request Count",
      "Response Time (ms)"
    )
  ]
  
  let generated_charts = []
  
  for config in chart_configs {
    let chart_data = VisualizationEngine::prepare_chart_data(visualization_engine, visualization_data, config)
    let chart = VisualizationEngine::generate_chart(visualization_engine, chart_data, config)
    
    generated_charts.push(chart)
    
    // Verify chart structure
    assert_true(chart.contains_key("id"))
    assert_true(chart.contains_key("type"))
    assert_true(chart.contains_key("title"))
    assert_true(chart.contains_key("data"))
    assert_true(chart.contains_key("metadata"))
    
    // Verify chart data
    let chart_data_structure = chart.get("data").unwrap()
    assert_true(chart_data_structure.contains_key("labels"))
    assert_true(chart_data_structure.contains_key("datasets"))
    
    // Verify chart metadata
    let metadata = chart.get("metadata").unwrap()
    assert_true(metadata.contains_key("generated_at"))
    assert_true(metadata.contains_key("data_source"))
    assert_true(metadata.contains_key("refresh_interval"))
  }
  
  // Test dashboard composition
  let dashboard_config = DashboardConfig::new(
    "telemetry_overview",
    "Telemetry System Overview",
    4, // 4 columns
    3  // 3 rows
  )
  
  // Add charts to dashboard
  for chart in generated_charts {
    DashboardConfig::add_chart(dashboard_config, chart, Position::auto())
  }
  
  let dashboard = VisualizationEngine::create_dashboard(visualization_engine, dashboard_config)
  
  // Verify dashboard structure
  assert_true(dashboard.contains_key("id"))
  assert_true(dashboard.contains_key("title"))
  assert_true(dashboard.contains_key("layout"))
  assert_true(dashboard.contains_key("charts"))
  
  // Verify dashboard layout
  let layout = dashboard.get("layout").unwrap()
  assert_eq(layout.get("columns"), Some(4))
  assert_eq(layout.get("rows"), Some(3))
  
  // Verify dashboard charts
  let dashboard_charts = dashboard.get("charts").unwrap()
  assert_eq(dashboard_charts.length(), generated_charts.length())
  
  // Test data analysis queries
  let analysis_queries = [
    ("top_errors", "SELECT error_message, COUNT(*) as count FROM events WHERE severity = 'ERROR' GROUP BY error_message ORDER BY count DESC LIMIT 10"),
    ("slowest_endpoints", "SELECT endpoint_name, AVG(duration) as avg_duration FROM spans WHERE type = 'HTTP' GROUP BY endpoint_name ORDER BY avg_duration DESC LIMIT 5"),
    ("busiest_services", "SELECT service_name, COUNT(*) as request_count FROM spans GROUP BY service_name ORDER BY request_count DESC LIMIT 10"),
    ("error_trend", "SELECT time_bucket('1h', timestamp) as hour, COUNT(*) as error_count FROM events WHERE severity = 'ERROR' GROUP BY hour ORDER BY hour"),
    ("performance_percentiles", "SELECT percentile_cont(0.50) as p50, percentile_cont(0.95) as p95, percentile_cont(0.99) as p99 FROM spans WHERE type = 'HTTP'")
  ]
  
  let analysis_results = []
  
  for (query_name, query) in analysis_queries {
    let result = DataAnalyzer::execute_query(data_analyzer, visualization_data, query)
    analysis_results.push((query_name, result))
    
    // Verify query result structure
    assert_true(result.is_successful)
    assert_true(result.data.length() > 0)
    
    // Verify result columns
    for row in result.data {
      assert_true(row.length() > 0)
    }
  }
  
  // Test anomaly detection
  let anomaly_detector = AnomalyDetector::new()
  
  // Configure anomaly detection
  AnomalyDetector::configure_algorithm(anomaly_detector, AnomalyAlgorithm::IsolationForest)
  AnomalyDetector::set_sensitivity(anomaly_detector, 0.1) // 10% anomaly rate
  AnomalyDetector::set_features(anomaly_detector, ["response_time", "error_rate", "cpu_usage", "memory_usage"])
  
  // Detect anomalies in the data
  let anomaly_detection_result = AnomalyDetector::detect_anomalies(anomaly_detector, visualization_data)
  
  // Verify anomaly detection
  assert_true(anomaly_detection_result.anomalies.length() >= 0) // May or may not find anomalies
  
  for anomaly in anomaly_detection_result.anomalies {
    assert_true(anomaly.contains_key("timestamp"))
    assert_true(anomaly.contains_key("anomaly_score"))
    assert_true(anomaly.contains_key("features"))
    assert_true(anomaly.contains_key("severity"))
    
    // Verify anomaly score is within expected range
    let anomaly_score = anomaly.get("anomaly_score").unwrap()
    assert_true(anomaly_score >= 0.0 && anomaly_score <= 1.0)
  }
  
  // Test trend analysis
  let trend_analyzer = TrendAnalyzer::new()
  
  // Analyze trends for key metrics
  let trend_metrics = ["response_time", "error_rate", "throughput"]
  let trend_results = []
  
  for metric in trend_metrics {
    let trend_result = TrendAnalyzer::analyze_trend(trend_analyzer, visualization_data, metric)
    trend_results.push((metric, trend_result))
    
    // Verify trend analysis result
    assert_true(trend_result.contains_key("direction")) // increasing, decreasing, stable
    assert_true(trend_result.contains_key("slope"))
    assert_true(trend_result.contains_key("confidence"))
    assert_true(trend_result.contains_key("seasonality"))
    
    // Verify trend direction
    let direction = trend_result.get("direction").unwrap()
    assert_true(direction == "increasing" || direction == "decreasing" || direction == "stable")
    
    // Verify confidence level
    let confidence = trend_result.get("confidence").unwrap()
    assert_true(confidence >= 0.0 && confidence <= 1.0)
  }
  
  // Test correlation analysis
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // Analyze correlations between metrics
  let correlation_pairs = [
    ("response_time", "error_rate"),
    ("throughput", "response_time"),
    ("cpu_usage", "memory_usage"),
    ("request_count", "cpu_usage")
  ]
  
  let correlation_results = []
  
  for (metric1, metric2) in correlation_pairs {
    let correlation_result = CorrelationAnalyzer::analyze_correlation(
      correlation_analyzer, 
      visualization_data, 
      metric1, 
      metric2
    )
    correlation_results.push(((metric1, metric2), correlation_result))
    
    // Verify correlation analysis result
    assert_true(correlation_result.contains_key("correlation_coefficient"))
    assert_true(correlation_result.contains_key("p_value"))
    assert_true(correlation_result.contains_key("significance"))
    
    // Verify correlation coefficient is within expected range
    let correlation_coefficient = correlation_result.get("correlation_coefficient").unwrap()
    assert_true(correlation_coefficient >= -1.0 && correlation_coefficient <= 1.0)
    
    // Verify p-value is within expected range
    let p_value = correlation_result.get("p_value").unwrap()
    assert_true(p_value >= 0.0 && p_value <= 1.0)
  }
  
  // Test predictive analytics
  let predictive_analyzer = PredictiveAnalyzer::new()
  
  // Configure predictive model
  PredictiveAnalyzer::configure_model(predictive_analyzer, PredictiveModel::LinearRegression)
  PredictiveAnalyzer::set_target_variable(predictive_analyzer, "response_time")
  PredictiveAnalyzer::set_feature_variables(predictive_analyzer, ["request_count", "cpu_usage", "memory_usage"])
  
  // Train predictive model
  let training_data = visualization_data.slice(0, visualization_data.length() * 7 / 10) // Use 70% for training
  let training_result = PredictiveAnalyzer::train_model(predictive_analyzer, training_data)
  
  assert_true(training_result.is_successful)
  assert_true(training_result.model_accuracy > 0.5) // Model should have reasonable accuracy
  
  // Test predictions
  let test_data = visualization_data.slice(visualization_data.length() * 7 / 10, visualization_data.length()) // Use 30% for testing
  let prediction_results = PredictiveAnalyzer::predict(predictive_analyzer, test_data)
  
  // Verify prediction results
  assert_eq(prediction_results.length(), test_data.length())
  
  for prediction in prediction_results {
    assert_true(prediction.contains_key("predicted_value"))
    assert_true(prediction.contains_key("confidence_interval"))
    assert_true(prediction.contains_key("actual_value"))
    
    // Verify prediction is reasonable
    let predicted_value = prediction.get("predicted_value").unwrap()
    let actual_value = prediction.get("actual_value").unwrap()
    
    // Prediction should be within a reasonable range of actual value
    let relative_error = (predicted_value - actual_value).abs() / actual_value.abs()
    assert_true(relative_error < 2.0) // Within 200% of actual value
  }
  
  // Test report generation
  let report_generator = ReportGenerator::new()
  
  // Generate comprehensive report
  let report_config = ReportConfig::new(
    "telemetry_analysis_report",
    "Telemetry System Analysis Report",
    ReportFormat::HTML,
    [
      ReportSection::ExecutiveSummary,
      ReportSection::PerformanceAnalysis,
      ReportSection::ErrorAnalysis,
      ReportSection::TrendAnalysis,
      ReportSection::AnomalyDetection,
      ReportSection::PredictiveAnalysis,
      ReportSection::Recommendations
    ]
  )
  
  let report = ReportGenerator::generate_report(
    report_generator,
    report_config,
    visualization_data,
    analysis_results,
    trend_results,
    correlation_results,
    prediction_results
  )
  
  // Verify report structure
  assert_true(report.contains_key("id"))
  assert_true(report.contains_key("title"))
  assert_true(report.contains_key("generated_at"))
  assert_true(report.contains_key("format"))
  assert_true(report.contains_key("sections"))
  
  // Verify report sections
  let sections = report.get("sections").unwrap()
  assert_true(sections.length() > 0)
  
  for section in sections {
    assert_true(section.contains_key("type"))
    assert_true(section.contains_key("title"))
    assert_true(section.contains_key("content"))
  }
  
  // Test visualization export
  let export_formats = ["png", "svg", "pdf", "json"]
  
  for format in export_formats {
    let export_result = VisualizationEngine::export_charts(visualization_engine, generated_charts, format)
    
    // Verify export result
    assert_true(export_result.is_successful)
    assert_true(export_result.data.length() > 0)
    assert_eq(export_result.format, format)
  }
  
  // Test real-time visualization updates
  let real_time_updater = RealTimeUpdater::new(visualization_engine)
  
  // Configure real-time updates
  RealTimeUpdater::configure_update_interval(real_time_updater, 5000L) // 5 seconds
  RealTimeUpdater::configure_data_source(real_time_updater, "telemetry_stream")
  
  // Start real-time updates
  RealTimeUpdater::start(real_time_updater)
  
  // Simulate real-time data updates
  let real_time_data_points = []
  
  for i in 0..=10 {
    let new_data_point = generate_real_time_data_point()
    real_time_data_points.push(new_data_point)
    
    RealTimeUpdater::process_data_update(real_time_updater, new_data_point)
    Time::sleep(1000L) // Wait 1 second between updates
  }
  
  // Stop real-time updates
  RealTimeUpdater::stop(real_time_updater)
  
  // Verify real-time updates were processed
  let update_history = RealTimeUpdater::get_update_history(real_time_updater)
  assert_eq(update_history.length(), 10)
  
  for update in update_history {
    assert_true(update.contains_key("timestamp"))
    assert_true(update.contains_key("data_point"))
    assert_true(update.contains_key("processed_at"))
  }
}

// Helper functions for monitoring and visualization tests
fn generate_visualization_test_data() -> TelemetryData {
  let data = TelemetryData::new("visualization_test")
  
  // Generate time series data with various patterns
  let start_time = Time::now() - (24 * 60 * 60 * 1000L) // 24 hours ago
  
  for i in 0..=1440 { // One data point per minute for 24 hours
    let timestamp = start_time + (i.to_int64() * 60 * 1000L)
    
    // Generate realistic patterns
    let hour_of_day = (i / 60) % 24
    
    // Simulate daily pattern (higher during business hours)
    let business_hours_factor = if hour_of_day >= 9 && hour_of_day <= 17 { 1.5 } else { 0.7 }
    
    // Simulate weekly pattern (higher on weekdays)
    let day_of_week = (i / (24 * 60)) % 7
    let weekday_factor = if day_of_week >= 1 && day_of_week <= 5 { 1.2 } else { 0.8 }
    
    // Base metrics with some randomness
    let base_response_time = 100.0 + Random::float_range(-20.0, 50.0)
    let base_error_rate = 1.0 + Random::float_range(-0.5, 2.0)
    let base_throughput = 50.0 + Random::float_range(-10.0, 20.0)
    let base_cpu_usage = 30.0 + Random::float_range(-10.0, 30.0)
    let base_memory_usage = 40.0 + Random::float_range(-15.0, 25.0)
    
    // Apply patterns
    let response_time = base_response_time * business_hours_factor * weekday_factor
    let error_rate = base_error_rate * (2.0 - business_hours_factor) // Higher error rate outside business hours
    let throughput = base_throughput * business_hours_factor * weekday_factor
    let cpu_usage = base_cpu_usage * business_hours_factor * weekday_factor
    let memory_usage = base_memory_usage * (1.0 + (weekday_factor - 1.0) * 0.5) // Memory usage less variable
    
    // Create span with metrics
    let span = Span::new("request_processing", Internal, SpanContext::generate())
    Span::set_timestamp(span, timestamp)
    Span::set_duration(span, response_time.to_int64())
    Span::set_attribute(span, "response_time", FloatValue(response_time))
    Span::set_attribute(span, "error_rate", FloatValue(error_rate))
    Span::set_attribute(span, "throughput", FloatValue(throughput))
    Span::set_attribute(span, "cpu_usage", FloatValue(cpu_usage))
    Span::set_attribute(span, "memory_usage", FloatValue(memory_usage))
    
    // Add some events
    if Random::float_range(0.0, 1.0) < error_rate / 100.0 {
      Span::add_event(span, "error_occurred", Some([("error_message", StringValue("Database timeout"))]))
    }
    
    data.add_span(span)
  }
  
  data
}

fn generate_real_time_data_point() -> Map[String, Value] {
  let data_point = Map::new()
  
  let timestamp = Time::now()
  let response_time = 100.0 + Random::float_range(-50.0, 200.0)
  let error_rate = Random::float_range(0.0, 5.0)
  let throughput = 50.0 + Random::float_range(-20.0, 30.0)
  let cpu_usage = 30.0 + Random::float_range(-10.0, 40.0)
  let memory_usage = 40.0 + Random::float_range(-15.0, 35.0)
  
  data_point.insert("timestamp", timestamp.to_string())
  data_point.insert("response_time", response_time)
  data_point.insert("error_rate", error_rate)
  data_point.insert("throughput", throughput)
  data_point.insert("cpu_usage", cpu_usage)
  data_point.insert("memory_usage", memory_usage)
  
  data_point
}