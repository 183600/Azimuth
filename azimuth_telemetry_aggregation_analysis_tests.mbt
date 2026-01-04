// Azimuth High-Quality Telemetry Data Aggregation and Analysis Tests
// This file contains comprehensive test cases for telemetry data aggregation and analysis

// Test 1: Real-time Telemetry Data Aggregation
test "real-time telemetry data aggregation" {
  let aggregator = RealTimeTelemetryAggregator::new()
  
  // Configure aggregation rules
  aggregator.add_aggregation_rule(
    "response_time",
    AggregationType::TimeWindowed,
    TimeWindow::Seconds(10),
    [AggregationFunction::Average, AggregationFunction::P95, AggregationFunction::P99]
  )
  
  aggregator.add_aggregation_rule(
    "error_rate",
    AggregationType::TimeWindowed,
    TimeWindow::Minutes(1),
    [AggregationFunction::Average, AggregationFunction::Max]
  )
  
  aggregator.add_aggregation_rule(
    "throughput",
    AggregationType::SlidingWindow,
    SlidingWindowConfig::new(100, 10), // 100 samples, slide every 10
    [AggregationFunction::Sum, AggregationFunction::Rate]
  )
  
  // Start aggregation
  aggregator.start()
  
  // Generate telemetry data
  let telemetry_points = []
  let current_time = get_current_timestamp()
  
  for i in 0..=1000 {
    let point = TelemetryPoint::new(
      "api-service",
      "response_time",
      100.0 + (i % 200).to_float(), // Response times between 100-300ms
      current_time + Duration::from_millis(i * 10)
    )
    telemetry_points.push(point)
  }
  
  // Ingest data
  for point in telemetry_points {
    aggregator.ingest(point)
  }
  
  // Wait for aggregation to process
  sleep(Duration::from_seconds(2))
  
  // Get aggregated results
  let response_time_aggregates = aggregator.get_aggregates("response_time")
  
  assert_true(response_time_aggregates.length() > 0)
  
  for aggregate in response_time_aggregates {
    assert_true(aggregate.timestamp > 0)
    assert_true(aggregate.window_start > 0)
    assert_true(aggregate.window_end > aggregate.window_start)
    assert_true(aggregate.values.contains_key("average"))
    assert_true(aggregate.values.contains_key("p95"))
    assert_true(aggregate.values.contains_key("p99"))
    
    // Verify aggregation values are reasonable
    assert_true(aggregate.values.get("average") >= 100.0)
    assert_true(aggregate.values.get("average") <= 300.0)
    assert_true(aggregate.values.get("p95") >= aggregate.values.get("average"))
    assert_true(aggregate.values.get("p99") >= aggregate.values.get("p95"))
  }
  
  // Test multi-dimensional aggregation
  aggregator.add_dimensional_aggregation(
    "response_time",
    ["service_name", "endpoint"],
    AggregationType::TimeWindowed,
    TimeWindow::Seconds(30),
    [AggregationFunction::Average]
  )
  
  // Generate multi-dimensional data
  let services = ["auth-service", "user-service", "order-service"]
  let endpoints = ["/login", "/profile", "/orders"]
  
  for i in 0..=500 {
    let service = services[i % services.length()]
    let endpoint = endpoints[i % endpoints.length()]
    
    let point = TelemetryPoint::new(service, "response_time", 50.0 + (i % 150).to_float(), current_time + Duration::from_millis(i * 20))
    point.add_dimension("endpoint", endpoint)
    
    aggregator.ingest(point)
  }
  
  sleep(Duration::from_seconds(1))
  
  let dimensional_aggregates = aggregator.get_dimensional_aggregates("response_time")
  
  assert_true(dimensional_aggregates.length() > 0)
  
  // Verify we have aggregates for each service-endpoint combination
  let service_endpoint_combinations = dimensional_aggregates.map(|a| {
    a.dimensions.get("service_name") + "_" + a.dimensions.get("endpoint")
  }).unique()
  
  assert_true(service_endpoint_combinations.length() >= services.length() * endpoints.length())
  
  // Stop aggregation
  aggregator.stop()
  
  // Verify aggregation metrics
  let metrics = aggregator.get_metrics()
  
  assert_true(metrics.total_points_processed > 0)
  assert_true(metrics.aggregation_windows_created > 0)
  assert_true(metrics.average_processing_time_ms > 0)
  assert_true(metrics.memory_usage_mb > 0)
}

// Test 2: Time Series Telemetry Analysis
test "time series telemetry analysis" {
  let analyzer = TimeSeriesAnalyzer::new()
  
  // Generate time series data
  let time_series_data = generate_time_series_data(
    "cpu_usage",
    Duration::from_days(30), // 30 days of data
    Duration::from_hours(1)  // Hourly data points
  )
  
  // Test trend analysis
  let trend_analysis = analyzer.analyze_trend(time_series_data.clone())
  
  assert_true(trend_analysis.trend_direction == "increasing" || 
              trend_analysis.trend_direction == "decreasing" || 
              trend_analysis.trend_direction == "stable")
  assert_true(trend_analysis.trend_strength >= 0.0 && trend_analysis.trend_strength <= 1.0)
  assert_true(trend_analysis.confidence >= 0.0 && trend_analysis.confidence <= 1.0)
  
  // Test seasonality detection
  let seasonality_analysis = analyzer.detect_seasonality(time_series_data.clone())
  
  assert_true(seasonality_analysis.has_seasonality == true || seasonality_analysis.has_seasonality == false)
  
  if seasonality_analysis.has_seasonality {
    assert_true(seasonality_analysis.period_hours > 0)
    assert_true(seasonality_analysis.strength >= 0.0 && seasonality_analysis.strength <= 1.0)
  }
  
  // Test anomaly detection
  let anomaly_detection = analyzer.detect_anomalies(
    time_series_data.clone(),
    AnomalyDetectionMethod::ZScore,
    3.0 // 3 standard deviations
  )
  
  assert_true(anomaly_detection.anomalies.length() >= 0)
  
  for anomaly in anomaly_detection.anomalies {
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.value != 0.0)
    assert_true(anomaly.anomaly_score > 0.0)
    assert_true(anomaly.reason.length() > 0)
  }
  
  // Test forecasting
  let forecast_result = analyzer.forecast(
    time_series_data.clone(),
    ForecastMethod::ARIMA,
    Duration::from_days(7) // 7-day forecast
  )
  
  assert_eq(forecast_result.forecast_values.length(), 168) // 7 days * 24 hours
  assert_true(forecast_result.confidence_intervals.length() == forecast_result.forecast_values.length())
  
  // Verify confidence intervals
  for i in 0..=forecast_result.forecast_values.length() - 1 {
    let interval = forecast_result.confidence_intervals[i]
    assert_true(interval.lower < forecast_result.forecast_values[i])
    assert_true(forecast_result.forecast_values[i] < interval.upper)
  }
  
  // Test change point detection
  let change_points = analyzer.detect_change_points(time_series_data.clone())
  
  assert_true(change_points.length() >= 0)
  
  for change_point in change_points {
    assert_true(change_point.timestamp > 0)
    assert_true(change_point.confidence >= 0.0 && change_point.confidence <= 1.0)
    assert_true(change_point.magnitude > 0.0)
  }
  
  // Test correlation analysis
  let memory_data = generate_time_series_data(
    "memory_usage",
    Duration::from_days(30),
    Duration::from_hours(1)
  )
  
  let correlation = analyzer.calculate_correlation(time_series_data, memory_data)
  
  assert_true(correlation.coefficient >= -1.0 && correlation.coefficient <= 1.0)
  assert_true(correlation.p_value >= 0.0 && correlation.p_value <= 1.0)
  assert_true(correlation.confidence_interval.lower <= correlation.coefficient)
  assert_true(correlation.coefficient <= correlation.confidence_interval.upper)
}

// Test 3: Distributed Telemetry Data Correlation
test "distributed telemetry data correlation" {
  let correlator = DistributedTelemetryCorrelator::new()
  
  // Generate distributed trace data
  let trace_data = generate_distributed_trace_data()
  
  // Test trace correlation
  let correlated_traces = correlator.correlate_traces(trace_data)
  
  assert_true(correlated_traces.length() > 0)
  
  for trace in correlated_traces {
    assert_true(trace.trace_id.length() > 0)
    assert_true(trace.spans.length() > 0)
    
    // Verify span ordering
    for i in 0..=trace.spans.length() - 2 {
      assert_true(trace.spans[i].start_time <= trace.spans[i + 1].start_time)
    }
    
    // Verify parent-child relationships
    for span in trace.spans {
      if span.parent_span_id.length() > 0 {
        let parent_span = trace.spans.find(|s| s.span_id == span.parent_span_id)
        assert_true(parent_span.is_some())
        assert_true(parent_span.unwrap().start_time <= span.start_time)
      }
    }
  }
  
  // Test cross-service correlation
  let service_graph = correlator.build_service_dependency_graph(correlated_traces)
  
  assert_true(service_graph.nodes.length() > 0)
  assert_true(service_graph.edges.length() > 0)
  
  for node in service_graph.nodes {
    assert_true(node.service_name.length() > 0)
    assert_true(node.request_count > 0)
    assert_true(node.error_rate >= 0.0 && node.error_rate <= 1.0)
    assert_true(node.average_response_time > 0.0)
  }
  
  for edge in service_graph.edges {
    assert_true(edge.source_service.length() > 0)
    assert_true(edge.target_service.length() > 0)
    assert_true(edge.call_count > 0)
    assert_true(edge.average_latency > 0.0)
  }
  
  // Test performance bottleneck detection
  let bottlenecks = correlator.detect_performance_bottlenecks(correlated_traces)
  
  assert_true(bottlenecks.length() >= 0)
  
  for bottleneck in bottlenecks {
    assert_true(bottleneck.service_name.length() > 0)
    assert_true(bottleneck.operation_name.length() > 0)
    assert_true(bottleneck.impact_score > 0.0)
    assert_true(bottleneck.description.length() > 0)
  }
  
  // Test error propagation analysis
  let error_propagation = correlator.analyze_error_propagation(correlated_traces)
  
  assert_true(error_propagation.error_paths.length() >= 0)
  
  for path in error_propagation.error_paths {
    assert_true(path.trace_id.length() > 0)
    assert_true(path.error_spans.length() > 0)
    assert_true(path.propagation_time_ms > 0)
    
    // Verify error propagation sequence
    for i in 0..=path.error_spans.length() - 2 {
      assert_true(path.error_spans[i].start_time <= path.error_spans[i + 1].start_time)
    }
  }
  
  // Test critical path analysis
  let critical_paths = correlator.analyze_critical_paths(correlated_traces)
  
  assert_true(critical_paths.length() > 0)
  
  for path in critical_paths {
    assert_true(path.trace_id.length() > 0)
    assert_true(path.total_duration_ms > 0)
    assert_true(path.criticality_score >= 0.0 && path.criticality_score <= 1.0)
    assert_true(path.spans.length() > 0)
  }
}

// Test 4: Telemetry Data Sampling and Filtering
test "telemetry data sampling and filtering" {
  let sampler = TelemetrySampler::new()
  
  // Configure sampling strategies
  sampler.add_sampling_rule(
    "high_volume_metrics",
    SamplingStrategy::Probabilistic(0.1), // 10% sampling
    ["request_count", "response_time"]
  )
  
  sampler.add_sampling_rule(
    "error_metrics",
    SamplingStrategy::Deterministic(1.0), // 100% sampling
    ["error_count", "error_rate"]
  )
  
  sampler.add_sampling_rule(
    "custom_metrics",
    SamplingStrategy::Adaptive(1000, 0.1), // Target 1000 samples, min 10%
    ["custom_business_metric"]
  )
  
  // Generate test data
  let telemetry_data = []
  let current_time = get_current_timestamp()
  
  // High volume metrics (10000 points)
  for i in 0..=9999 {
    let point = TelemetryPoint::new(
      "api-service",
      "request_count",
      i.to_float(),
      current_time + Duration::from_millis(i)
    )
    telemetry_data.push(point)
  }
  
  // Error metrics (100 points)
  for i in 0..=99 {
    let point = TelemetryPoint::new(
      "api-service",
      "error_count",
      (i % 5).to_float(),
      current_time + Duration::from_millis(i * 10)
    )
    telemetry_data.push(point)
  }
  
  // Custom metrics (5000 points)
  for i in 0..=4999 {
    let point = TelemetryPoint::new(
      "business-service",
      "custom_business_metric",
      (i * 1.5).to_float(),
      current_time + Duration::from_millis(i * 2)
    )
    telemetry_data.push(point)
  }
  
  // Apply sampling
  let sampled_data = sampler.sample(telemetry_data)
  
  // Verify sampling results
  let request_count_samples = sampled_data.filter(|p| p.metric_name == "request_count")
  let error_count_samples = sampled_data.filter(|p| p.metric_name == "error_count")
  let custom_metric_samples = sampled_data.filter(|p| p.metric_name == "custom_business_metric")
  
  // Request count should be approximately 10% (probabilistic sampling)
  assert_true(request_count_samples.length() > 500 && request_count_samples.length() < 1500)
  
  // Error count should be 100% (deterministic sampling)
  assert_eq(error_count_samples.length(), 100)
  
  // Custom metric should be around 1000 or 10% (adaptive sampling)
  assert_true(custom_metric_samples.length() >= 500 && custom_metric_samples.length() <= 1500)
  
  // Test filtering
  let filter = TelemetryFilter::new()
  
  // Add filter conditions
  filter.add_condition("service_name", FilterOperator::Equals, "api-service")
  filter.add_condition("timestamp", FilterOperator::GreaterThan, current_time + Duration::from_seconds(10))
  filter.add_condition("value", FilterOperator::GreaterThan, 50.0)
  
  // Apply filter
  let filtered_data = filter.filter(sampled_data)
  
  // Verify filtering
  for point in filtered_data {
    assert_eq(point.service_name, "api-service")
    assert_true(point.timestamp > current_time + Duration::from_seconds(10))
    assert_true(point.value > 50.0)
  }
  
  // Test complex filter expressions
  let complex_filter = TelemetryFilter::new()
  complex_filter.set_expression("(service_name = 'api-service' AND value > 100) OR (service_name = 'business-service' AND value < 1000)")
  
  let complex_filtered = complex_filter.filter(sampled_data)
  
  // Verify complex filtering
  for point in complex_filtered {
    let condition_met = 
      (point.service_name == "api-service" && point.value > 100.0) ||
      (point.service_name == "business-service" && point.value < 1000.0)
    
    assert_true(condition_met)
  }
  
  // Test sampling metrics
  let sampling_metrics = sampler.get_metrics()
  
  assert_true(sampling_metrics.total_points_processed > 0)
  assert_true(sampling_metrics.total_sampled_points > 0)
  assert_true(sampling_metrics.sampling_rate >= 0.0 && sampling_metrics.sampling_rate <= 1.0)
  
  for (rule_name, rule_metrics) in sampling_metrics.rule_metrics {
    assert_true(rule_metrics.input_points > 0)
    assert_true(rule_metrics.output_points >= 0)
    assert_true(rule_metrics.effective_rate >= 0.0 && rule_metrics.effective_rate <= 1.0)
  }
}

// Test 5: Telemetry Data Visualization
test "telemetry data visualization" {
  let visualizer = TelemetryVisualizer::new()
  
  // Generate test data for visualization
  let time_series_data = generate_time_series_data(
    "system_metrics",
    Duration::from_days(7), // 7 days of data
    Duration::from_hours(1)  // Hourly data points
  )
  
  // Test time series chart
  let chart_config = ChartConfig::new()
    .with_title("System Metrics Over Time")
    .with_x_axis_label("Time")
    .with_y_axis_label("Value")
    .with_chart_type(ChartType::Line)
    .with_time_range(TimeRange::Last7Days)
  
  let time_series_chart = visualizer.create_time_series_chart(time_series_data, chart_config)
  
  assert_true(time_series_chart.chart_data.length() > 0)
  assert_eq(time_series_chart.metadata.title, "System Metrics Over Time")
  assert_eq(time_series_chart.metadata.x_axis_label, "Time")
  assert_eq(time_series_chart.metadata.y_axis_label, "Value")
  assert_eq(time_series_chart.metadata.chart_type, ChartType::Line)
  
  // Verify chart data points
  for data_point in time_series_chart.chart_data {
    assert_true(data_point.x >= 0) // Timestamp
    assert_true(data_point.y != 0.0) // Value
  }
  
  // Test heatmap visualization
  let heatmap_data = generate_heatmap_data(
    ["service-a", "service-b", "service-c"],
    ["endpoint-1", "endpoint-2", "endpoint-3", "endpoint-4"],
    || service, endpoint {
      // Generate correlation values between services and endpoints
      (service.length() + endpoint.length()).to_float() * 10.0
    }
  )
  
  let heatmap_config = HeatmapConfig::new()
    .with_title("Service-Endpoint Correlation Heatmap")
    .with_color_scheme(ColorScheme::Viridis)
    .with_value_range(0.0, 100.0)
  
  let heatmap = visualizer.create_heatmap(heatmap_data, heatmap_config)
  
  assert_eq(heatmap.rows, 3) // 3 services
  assert_eq(heatmap.cols, 4) // 4 endpoints
  assert_eq(heatmap.data.length(), 12) // 3 * 4 = 12 cells
  
  for cell in heatmap.data {
    assert_true(cell.row >= 0 && cell.row < 3)
    assert_true(cell.col >= 0 && cell.col < 4)
    assert_true(cell.value >= 0.0 && cell.value <= 100.0)
    assert_true(cell.color.length() > 0) // Hex color code
  }
  
  // Test histogram visualization
  let histogram_data = generate_histogram_data(1000, || {
    // Generate normal distribution data
    generate_normal_random(50.0, 10.0)
  })
  
  let histogram_config = HistogramConfig::new()
    .with_title("Response Time Distribution")
    .with_x_axis_label("Response Time (ms)")
    .with_y_axis_label("Frequency")
    .with_bins(20)
  
  let histogram = visualizer.create_histogram(histogram_data, histogram_config)
  
  assert_eq(histogram.bins, 20)
  assert_eq(histogram.data.length(), 20)
  
  let total_frequency = histogram.data.reduce(0, |sum, bin| sum + bin.frequency)
  assert_eq(total_frequency, 1000) // All data points should be accounted for
  
  // Test scatter plot visualization
  let scatter_data = generate_scatter_data(500, || {
    let x = generate_uniform_random(0.0, 100.0)
    let y = x * 1.5 + generate_normal_random(0.0, 10.0) // Linear relationship with noise
    ScatterPoint::new(x, y)
  })
  
  let scatter_config = ScatterConfig::new()
    .with_title("CPU Usage vs Response Time")
    .with_x_axis_label("CPU Usage (%)")
    .with_y_axis_label("Response Time (ms)")
  
  let scatter_plot = visualizer.create_scatter_plot(scatter_data, scatter_config)
  
  assert_eq(scatter_plot.points.length(), 500)
  
  for point in scatter_plot.points {
    assert_true(point.x >= 0.0 && point.x <= 100.0)
    assert_true(point.y >= 0.0) // Response times should be positive
  }
  
  // Test dashboard creation
  let dashboard_config = DashboardConfig::new()
    .with_title("System Monitoring Dashboard")
    .with_layout(DashboardLayout::Grid(2, 2)) // 2x2 grid
    .with_refresh_interval(Duration::from_minutes(5))
  
  let dashboard = visualizer.create_dashboard(dashboard_config)
  
  // Add visualizations to dashboard
  dashboard.add_visualization("time_series", time_series_chart, 0, 0)
  dashboard.add_visualization("heatmap", heatmap, 0, 1)
  dashboard.add_visualization("histogram", histogram, 1, 0)
  dashboard.add_visualization("scatter_plot", scatter_plot, 1, 1)
  
  assert_eq(dashboard.visualizations.length(), 4)
  assert_eq(dashboard.metadata.title, "System Monitoring Dashboard")
  assert_eq(dashboard.metadata.layout, DashboardLayout::Grid(2, 2))
  assert_eq(dashboard.metadata.refresh_interval, Duration::from_minutes(5))
  
  // Test dashboard export
  let export_config = DashboardExportConfig::new()
    .with_format(ExportFormat::HTML)
    .with_include_data(true)
    .with_include_interactivity(true)
  
  let exported_dashboard = visualizer.export_dashboard(dashboard, export_config)
  
  assert_true(exported_dashboard.content.length() > 0)
  assert_eq(exported_dashboard.format, ExportFormat::HTML)
  assert_true(exported_dashboard.size_bytes > 0)
}

// Test 6: Telemetry Data Alerting
test "telemetry data alerting" {
  let alert_manager = AlertManager::new()
  
  // Configure alert rules
  alert_manager.add_rule(
    "high_response_time",
    AlertRule::new()
      .with_metric("response_time")
      .with_condition(AlertCondition::GreaterThan(500.0))
      .with_duration(Duration::from_minutes(5))
      .with_severity(AlertSeverity::Warning)
      .with_message("Response time is above 500ms for 5 minutes")
  )
  
  alert_manager.add_rule(
    "critical_error_rate",
    AlertRule::new()
      .with_metric("error_rate")
      .with_condition(AlertCondition::GreaterThan(0.05)) // 5%
      .with_duration(Duration::from_minutes(1))
      .with_severity(AlertSeverity::Critical)
      .with_message("Error rate is above 5% for 1 minute")
  )
  
  alert_manager.add_rule(
    "low_throughput",
    AlertRule::new()
      .with_metric("throughput")
      .with_condition(AlertCondition::LessThan(100.0))
      .with_duration(Duration::from_minutes(10))
      .with_severity(AlertSeverity::Warning)
      .with_message("Throughput is below 100 requests/second for 10 minutes")
  )
  
  // Start alert manager
  alert_manager.start()
  
  // Generate telemetry data that triggers alerts
  let current_time = get_current_timestamp()
  
  // High response time data (should trigger alert after 5 minutes)
  for i in 0..=30 {
    let point = TelemetryPoint::new(
      "api-service",
      "response_time",
      600.0, // Above 500ms threshold
      current_time + Duration::from_minutes(i)
    )
    alert_manager.process_telemetry(point)
  }
  
  // Critical error rate data (should trigger alert after 1 minute)
  for i in 0..=5 {
    let point = TelemetryPoint::new(
      "api-service",
      "error_rate",
      0.1, // Above 5% threshold
      current_time + Duration::from_minutes(i)
    )
    alert_manager.process_telemetry(point)
  }
  
  // Wait for alert evaluation
  sleep(Duration::from_seconds(2))
  
  // Check for triggered alerts
  let active_alerts = alert_manager.get_active_alerts()
  
  assert_true(active_alerts.length() >= 1)
  
  let critical_error_alert = active_alerts.find(|a| a.rule_name == "critical_error_rate")
  assert_true(critical_error_alert.is_some())
  
  if let Some(alert) = critical_error_alert {
    assert_eq(alert.severity, AlertSeverity::Critical)
    assert_eq(alert.metric, "error_rate")
    assert_true(alert.triggered_at > 0)
    assert_true(alert.last_updated >= alert.triggered_at)
    assert_true(alert.message.contains("Error rate is above 5%"))
  }
  
  // Test alert acknowledgment
  if let Some(alert) = critical_error_alert {
    let ack_result = alert_manager.acknowledge_alert(alert.id, "Investigating the issue")
    assert_true(ack_result.is_success())
    
    let updated_alert = alert_manager.get_alert(alert.id)
    assert_true(updated_alert.is_some())
    assert_true(updated_alert.unwrap().is_acknowledged)
    assert_eq(updated_alert.unwrap().acknowledgment_message, "Investigating the issue")
  }
  
  // Test alert resolution
  // Generate normal data to resolve the alert
  for i in 0..=10 {
    let point = TelemetryPoint::new(
      "api-service",
      "error_rate",
      0.01, // Below 5% threshold
      current_time + Duration::from_minutes(10 + i)
    )
    alert_manager.process_telemetry(point)
  }
  
  sleep(Duration::from_seconds(1))
  
  // Check if alert is resolved
  let resolved_alert = alert_manager.get_alert(critical_error_alert.unwrap().id)
  assert_true(resolved_alert.is_some())
  assert_true(resolved_alert.unwrap().is_resolved)
  
  // Test alert notification channels
  let notification_manager = NotificationManager::new()
  
  // Configure notification channels
  notification_manager.add_channel(
    "email",
    EmailNotificationChannel::new("alerts@example.com")
  )
  
  notification_manager.add_channel(
    "slack",
    SlackNotificationChannel::new("#alerts-channel")
  )
  
  notification_manager.add_channel(
    "pagerduty",
    PagerDutyNotificationChannel::new("integration-key")
  )
  
  // Test alert notification
  let test_alert = Alert::new(
    "test_alert",
    AlertSeverity::Warning,
    "Test alert message",
    current_time
  )
  
  let notification_result = notification_manager.send_alert(test_alert, ["email", "slack"])
  assert_true(notification_result.is_success())
  
  // Test alert escalation
  let escalation_policy = EscalationPolicy::new()
    .add_level(AlertSeverity::Warning, ["email"], Duration::from_minutes(5))
    .add_level(AlertSeverity::Critical, ["slack", "pagerduty"], Duration::from_minutes(1))
  
  notification_manager.set_escalation_policy(escalation_policy)
  
  let critical_alert = Alert::new(
    "critical_test",
    AlertSeverity::Critical,
    "Critical test alert",
    current_time
  )
  
  let escalation_result = notification_manager.send_alert_with_escalation(critical_alert)
  assert_true(escalation_result.is_success())
  
  // Stop alert manager
  alert_manager.stop()
  
  // Test alert metrics
  let alert_metrics = alert_manager.get_metrics()
  
  assert_true(alert_metrics.total_alerts_triggered > 0)
  assert_true(alert_metrics.alerts_resolved > 0)
  assert_true(alert_metrics.alerts_acknowledged >= 0)
  assert_true(alert_metrics.average_resolution_time_ms > 0)
}

// Test 7: Telemetry Data Retention and Archival
test "telemetry data retention and archival" {
  let retention_manager = TelemetryRetentionManager::new()
  
  // Configure retention policies
  retention_manager.add_policy(
    "raw_telemetry",
    RetentionPolicy::new()
      .with_retention_period(Duration::from_days(7))
      .with_archival_period(Duration::from_days(1))
      .with_compression(true)
      .with_encryption(true)
  )
  
  retention_manager.add_policy(
    "aggregated_metrics",
    RetentionPolicy::new()
      .with_retention_period(Duration::from_days(90))
      .with_archival_period(Duration::from_days(7))
      .with_compression(true)
      .with_encryption(false)
  )
  
  retention_manager.add_policy(
    "alerts",
    RetentionPolicy::new()
      .with_retention_period(Duration::from_days(365))
      .with_archival_period(Duration::from_days(30))
      .with_compression(false)
      .with_encryption(true)
  )
  
  // Generate test data with different ages
  let current_time = get_current_timestamp()
  
  // Raw telemetry data (various ages)
  let raw_data = []
  for i in 0..=100 {
    let timestamp = current_time - Duration::from_days(i)
    let data = TelemetryData::new()
      .with_type("raw_telemetry")
      .with_timestamp(timestamp)
      .with_size(1024) // 1KB each
    raw_data.push(data)
  }
  
  // Aggregated metrics data
  let aggregated_data = []
  for i in 0..=100 {
    let timestamp = current_time - Duration::from_days(i)
    let data = TelemetryData::new()
      .with_type("aggregated_metrics")
      .with_timestamp(timestamp)
      .with_size(512) // 512B each
    aggregated_data.push(data)
  }
  
  // Alerts data
  let alerts_data = []
  for i in 0..=50 {
    let timestamp = current_time - Duration::from_days(i)
    let data = TelemetryData::new()
      .with_type("alerts")
      .with_timestamp(timestamp)
      .with_size(256) // 256B each
    alerts_data.push(data)
  }
  
  // Store data
  for data in raw_data {
    retention_manager.store(data)
  }
  
  for data in aggregated_data {
    retention_manager.store(data)
  }
  
  for data in alerts_data {
    retention_manager.store(data)
  }
  
  // Run retention enforcement
  let retention_result = retention_manager.enforce_retention()
  
  assert_true(retention_result.is_success())
  assert_true(retention_result.deleted_count > 0)
  assert_true(retention_result.archived_count > 0)
  assert_true(retention_result.compressed_count > 0)
  
  // Verify data states
  let raw_metrics = retention_manager.get_metrics("raw_telemetry")
  assert_true(raw_metrics.active_count <= 7 * 24) // At most 7 days of hourly data
  assert_true(raw_metrics.archived_count > 0)
  
  let aggregated_metrics = retention_manager.get_metrics("aggregated_metrics")
  assert_true(aggregated_metrics.active_count <= 90 * 24) // At most 90 days of hourly data
  assert_true(aggregated_metrics.archived_count > 0)
  
  let alerts_metrics = retention_manager.get_metrics("alerts")
  assert_true(alerts_metrics.active_count <= 365) // At most 365 days of alerts
  assert_true(alerts_metrics.archived_count > 0)
  
  // Test data retrieval from archive
  let archived_data = retention_manager.retrieve_from_archive(
    "raw_telemetry",
    current_time - Duration::from_days(10),
    current_time - Duration::from_days(8)
  )
  
  assert_true(archived_data.length() > 0)
  
  for data in archived_data {
    assert_eq(data.type, "raw_telemetry")
    assert_true(data.timestamp >= current_time - Duration::from_days(10))
    assert_true(data.timestamp <= current_time - Duration::from_days(8))
  }
  
  // Test data compression
  let compression_stats = retention_manager.get_compression_stats()
  
  assert_true(compression_stats.total_compressed_size > 0)
  assert_true(compression_stats.total_uncompressed_size > compression_stats.total_compressed_size)
  assert_true(compression_stats.compression_ratio > 1.0)
  
  // Test data encryption
  let encryption_stats = retention_manager.get_encryption_stats()
  
  assert_true(encryption_stats.encrypted_data_count > 0)
  assert_true(encryption_stats.encryption_keys_count > 0)
  assert_true(encryption_stats.last_key_rotation > 0)
  
  // Test retention schedule
  let schedule = RetentionSchedule::new()
    .add_task("daily_retention", Duration::from_days(1), "02:00")
    .add_task("weekly_archival", Duration::from_days(7), "03:00")
    .add_task("monthly_cleanup", Duration::from_days(30), "04:00")
  
  retention_manager.set_schedule(schedule)
  
  // Verify scheduled tasks
  let scheduled_tasks = retention_manager.get_scheduled_tasks()
  
  assert_true(scheduled_tasks.length() == 3)
  
  for task in scheduled_tasks {
    assert_true(task.name.length() > 0)
    assert_true(task.interval > Duration::from_zero())
    assert_true(task.execution_time.length() > 0)
    assert_true(task.next_run_time > 0)
  }
}

// Test 8: Telemetry Data Querying and Analysis
test "telemetry data querying and analysis" {
  let query_engine = TelemetryQueryEngine::new()
  
  // Generate test data
  let telemetry_data = generate_comprehensive_telemetry_data(
    Duration::from_days(30),
    Duration::from_minutes(5)
  )
  
  // Store data in query engine
  for data in telemetry_data {
    query_engine.store(data)
  }
  
  // Test simple queries
  let simple_query = "SELECT service_name, metric_name, AVG(value) as avg_value FROM telemetry WHERE timestamp > '2024-01-01' GROUP BY service_name, metric_name"
  
  let simple_result = query_engine.execute(simple_query)
  
  assert_true(simple_result.is_success())
  assert_true(simple_result.rows.length() > 0)
  
  // Verify query result structure
  for row in simple_result.rows {
    assert_true(row.contains_key("service_name"))
    assert_true(row.contains_key("metric_name"))
    assert_true(row.contains_key("avg_value"))
    
    let avg_value = row.get("avg_value").to_float()
    assert_true(avg_value > 0.0)
  }
  
  // Test time range queries
  let time_range_query = "SELECT timestamp, value FROM telemetry WHERE service_name = 'api-service' AND metric_name = 'response_time' AND timestamp BETWEEN '2024-01-01' AND '2024-01-07' ORDER BY timestamp"
  
  let time_range_result = query_engine.execute(time_range_query)
  
  assert_true(time_range_result.is_success())
  assert_true(time_range_result.rows.length() > 0)
  
  // Verify time ordering
  for i in 0..=time_range_result.rows.length() - 2 {
    let current_time = time_range_result.rows[i].get("timestamp").to_timestamp()
    let next_time = time_range_result.rows[i + 1].get("timestamp").to_timestamp()
    assert_true(current_time <= next_time)
  }
  
  // Test aggregation queries
  let aggregation_query = "
    SELECT 
      service_name,
      DATE_TRUNC(timestamp, 'hour') as hour,
      COUNT(*) as count,
      AVG(value) as avg_value,
      MIN(value) as min_value,
      MAX(value) as max_value,
      PERCENTILE(value, 95) as p95_value,
      STDDEV(value) as stddev_value
    FROM telemetry
    WHERE metric_name = 'response_time'
    GROUP BY service_name, DATE_TRUNC(timestamp, 'hour')
    ORDER BY hour
  "
  
  let aggregation_result = query_engine.execute(aggregation_query)
  
  assert_true(aggregation_result.is_success())
  assert_true(aggregation_result.rows.length() > 0)
  
  // Verify aggregation results
  for row in aggregation_result.rows {
    assert_true(row.contains_key("service_name"))
    assert_true(row.contains_key("hour"))
    assert_true(row.contains_key("count"))
    assert_true(row.contains_key("avg_value"))
    assert_true(row.contains_key("min_value"))
    assert_true(row.contains_key("max_value"))
    assert_true(row.contains_key("p95_value"))
    assert_true(row.contains_key("stddev_value"))
    
    let count = row.get("count").to_int()
    let avg_value = row.get("avg_value").to_float()
    let min_value = row.get("min_value").to_float()
    let max_value = row.get("max_value").to_float()
    let p95_value = row.get("p95_value").to_float()
    let stddev_value = row.get("stddev_value").to_float()
    
    assert_true(count > 0)
    assert_true(avg_value > 0.0)
    assert_true(min_value <= avg_value)
    assert_true(max_value >= avg_value)
    assert_true(p95_value >= avg_value)
    assert_true(p95_value <= max_value)
    assert_true(stddev_value >= 0.0)
  }
  
  // Test complex analytical queries
  let complex_query = "
    WITH service_metrics AS (
      SELECT
        service_name,
        metric_name,
        AVG(value) as avg_value,
        COUNT(*) as sample_count
      FROM telemetry
      WHERE timestamp >= NOW() - INTERVAL '7 days'
      GROUP BY service_name, metric_name
    ),
    service_performance AS (
      SELECT
        service_name,
        avg_value as avg_response_time
      FROM service_metrics
      WHERE metric_name = 'response_time'
    ),
    service_errors AS (
      SELECT
        service_name,
        avg_value as error_rate
      FROM service_metrics
      WHERE metric_name = 'error_rate'
    )
    SELECT
      sp.service_name,
      sp.avg_response_time,
      COALESCE(se.error_rate, 0) as error_rate,
      CASE
        WHEN sp.avg_response_time > 500 OR COALESCE(se.error_rate, 0) > 0.05 THEN 'poor'
        WHEN sp.avg_response_time > 200 OR COALESCE(se.error_rate, 0) > 0.01 THEN 'fair'
        ELSE 'good'
      END as performance_grade
    FROM service_performance sp
    LEFT JOIN service_errors se ON sp.service_name = se.service_name
    ORDER BY sp.avg_response_time DESC
  "
  
  let complex_result = query_engine.execute(complex_query)
  
  assert_true(complex_result.is_success())
  assert_true(complex_result.rows.length() > 0)
  
  // Verify complex query results
  for row in complex_result.rows {
    assert_true(row.contains_key("service_name"))
    assert_true(row.contains_key("avg_response_time"))
    assert_true(row.contains_key("error_rate"))
    assert_true(row.contains_key("performance_grade"))
    
    let avg_response_time = row.get("avg_response_time").to_float()
    let error_rate = row.get("error_rate").to_float()
    let performance_grade = row.get("performance_grade").to_string()
    
    assert_true(avg_response_time > 0.0)
    assert_true(error_rate >= 0.0 && error_rate <= 1.0)
    assert_true(performance_grade == "good" || performance_grade == "fair" || performance_grade == "poor")
    
    // Verify performance grade logic
    if performance_grade == "poor" {
      assert_true(avg_response_time > 500.0 || error_rate > 0.05)
    } else if performance_grade == "fair" {
      assert_true(avg_response_time > 200.0 || error_rate > 0.01)
      assert_true(avg_response_time <= 500.0 && error_rate <= 0.05)
    } else {
      assert_true(avg_response_time <= 200.0 && error_rate <= 0.01)
    }
  }
  
  // Test query performance metrics
  let query_metrics = query_engine.get_query_metrics()
  
  assert_true(query_metrics.total_queries > 0)
  assert_true(query_metrics.average_execution_time_ms > 0)
  assert_true(query_metrics.cache_hit_rate >= 0.0 && query_metrics.cache_hit_rate <= 1.0)
  assert_true(query_metrics.rows_returned > 0)
}

// Test 9: Telemetry Data Machine Learning Analysis
test "telemetry data machine learning analysis" {
  let ml_analyzer = MLTelemetryAnalyzer::new()
  
  // Generate training data
  let training_data = generate_training_telemetry_data(
    Duration::from_days(30),
    Duration::from_hours(1)
  )
  
  // Test anomaly detection model
  let anomaly_model = ml_analyzer.train_anomaly_detection_model(
    training_data.clone(),
    AnomalyDetectionAlgorithm::IsolationForest,
    {
      "n_estimators": 100,
      "contamination": 0.1,
      "random_state": 42
    }
  )
  
  assert_true(anomaly_model.is_trained)
  assert_true(anomaly_model.accuracy > 0.8)
  assert_true(anomaly_model.precision > 0.7)
  assert_true(anomaly_model.recall > 0.7)
  
  // Test anomaly detection
  let test_data = generate_test_telemetry_data(
    Duration::from_days(1),
    Duration::from_hours(1)
  )
  
  let anomaly_predictions = ml_analyzer.detect_anomalies(anomaly_model, test_data)
  
  assert_eq(anomaly_predictions.length(), test_data.length())
  
  let anomaly_count = anomaly_predictions.filter(|p| p.is_anomaly).length()
  assert_true(anomaly_count >= 0 && anomaly_count <= test_data.length())
  
  for prediction in anomaly_predictions {
    assert_true(prediction.anomaly_score >= 0.0 && prediction.anomaly_score <= 1.0)
    assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
    
    if prediction.is_anomaly {
      assert_true(prediction.anomaly_score > 0.5)
    }
  }
  
  // Test forecasting model
  let forecasting_model = ml_analyzer.train_forecasting_model(
    training_data.clone(),
    ForecastingAlgorithm::LSTM,
    {
      "units": [64, 32],
      "dropout": 0.2,
      "epochs": 100,
      "batch_size": 32
    }
  )
  
  assert_true(forecasting_model.is_trained)
  assert_true(forecasting_model.mae < 100.0) // Mean Absolute Error
  assert_true(forecasting_model.rmse < 150.0) // Root Mean Square Error
  assert_true(forecasting_model.mape < 0.2) // Mean Absolute Percentage Error
  
  // Test forecasting
  let forecast_horizon = 24 // 24 hours
  let forecast = ml_analyzer.generate_forecast(
    forecasting_model,
    "response_time",
    forecast_horizon
  )
  
  assert_eq(forecast.values.length(), forecast_horizon)
  assert_eq(forecast.confidence_intervals.length(), forecast_horizon)
  
  // Verify confidence intervals
  for i in 0..=forecast_horizon - 1 {
    let interval = forecast.confidence_intervals[i]
    assert_true(interval.lower < forecast.values[i])
    assert_true(forecast.values[i] < interval.upper)
  }
  
  // Test clustering model
  let clustering_model = ml_analyzer.train_clustering_model(
    training_data.clone(),
    ClusteringAlgorithm::KMeans,
    {
      "n_clusters": 5,
      "random_state": 42
    }
  )
  
  assert_true(clustering_model.is_trained)
  assert_true(clustering_model.inertia > 0.0)
  assert_true(clustering_model.silhouette_score >= -1.0 && clustering_model.silhouette_score <= 1.0)
  
  // Test clustering
  let cluster_predictions = ml_analyzer.predict_clusters(clustering_model, test_data)
  
  assert_eq(cluster_predictions.length(), test_data.length())
  
  for prediction in cluster_predictions {
    assert_true(prediction.cluster_id >= 0)
    assert_true(prediction.cluster_id < 5) // We trained with 5 clusters
    assert_true(prediction.distance_to_centroid >= 0.0)
  }
  
  // Test classification model
  let classification_data = generate_classification_telemetry_data(training_data)
  let classification_model = ml_analyzer.train_classification_model(
    classification_data.training_data,
    ClassificationAlgorithm::RandomForest,
    {
      "n_estimators": 100,
      "max_depth": 10,
      "random_state": 42
    }
  )
  
  assert_true(classification_model.is_trained)
  assert_true(classification_model.accuracy > 0.8)
  assert_true(classification_model.precision > 0.7)
  assert_true(classification_model.recall > 0.7)
  assert_true(classification_model.f1_score > 0.7)
  
  // Test classification
  let classification_predictions = ml_analyzer.predict_classification(
    classification_model,
    classification_data.test_data
  )
  
  assert_eq(classification_predictions.length(), classification_data.test_data.length())
  
  for prediction in classification_predictions {
    assert_true(prediction.predicted_class.length() > 0)
    assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
    assert_true(prediction.class_probabilities.length() > 0)
    
    // Verify probabilities sum to 1
    let probability_sum = prediction.class_probabilities.reduce(0.0, |sum, p| sum + p.value)
    assert_true(abs(probability_sum - 1.0) < 0.001)
  }
  
  // Test model explainability
  let explainability = ml_analyzer.explain_predictions(
    classification_model,
    classification_data.test_data.slice(0, 10) // Explain first 10 predictions
  )
  
  assert_eq(explainability.explanations.length(), 10)
  
  for explanation in explainability.explanations {
    assert_true(explanation.feature_importance.length() > 0)
    
    // Verify feature importance values
    for feature in explanation.feature_importance {
      assert_true(feature.importance >= -1.0 && feature.importance <= 1.0)
    }
  }
}

// Test 10: Telemetry Data Integration and Export
test "telemetry data integration and export" {
  let integration_manager = TelemetryIntegrationManager::new()
  
  // Configure data sources
  integration_manager.add_source(
    "prometheus",
    PrometheusSource::new("http://prometheus:9090")
  )
  
  integration_manager.add_source(
    "influxdb",
    InfluxDBSource::new("http://influxdb:8086", "telemetry")
  )
  
  integration_manager.add_source(
    "elasticsearch",
    ElasticsearchSource::new("http://elasticsearch:9200", "telemetry-*")
  )
  
  // Configure data sinks
  integration_manager.add_sink(
    "graphite",
    GraphiteSink::new("graphite:2003")
  )
  
  integration_manager.add_sink(
    "kafka",
    KafkaSink::new("kafka:9092", "telemetry-topic")
  )
  
  integration_manager.add_sink(
    "splunk",
    SplunkSink::new("https://splunk:8088", "telemetry-index")
  )
  
  // Test data collection from sources
  let collection_tasks = []
  
  for source in ["prometheus", "influxdb", "elasticsearch"] {
    let task = integration_manager.collect_from_source(source, Duration::from_minutes(5))
    collection_tasks.push(task)
  }
  
  // Wait for collection to complete
  let collected_data = []
  for task in collection_tasks {
    let result = task.wait()
    assert_true(result.is_success())
    
    for data_point in result.unwrap() {
      collected_data.push(data_point)
    }
  }
  
  assert_true(collected_data.length() > 0)
  
  // Verify collected data
  for data_point in collected_data {
    assert_true(data_point.metric_name.length() > 0)
    assert_true(data_point.value != 0.0)
    assert_true(data_point.timestamp > 0)
    assert_true(data_point.source.length() > 0)
  }
  
  // Test data transformation
  let transformer = DataTransformer::new()
  
  // Add transformation rules
  transformer.add_rule(
    "unit_conversion",
    |data_point| {
      if data_point.metric_name == "cpu_usage" && data_point.unit == "percent" {
        data_point.value = data_point.value / 100.0 // Convert to decimal
        data_point.unit = "decimal"
      }
      data_point
    }
  )
  
  transformer.add_rule(
    "metric_naming",
    |data_point| {
      // Convert camelCase to snake_case
      let snake_case_name = data_point.metric_name.replace(/([a-z])([A-Z])/g, "${1}_${2}").to_lowercase()
      data_point.metric_name = snake_case_name
      data_point
    }
  )
  
  // Apply transformations
  let transformed_data = []
  for data_point in collected_data {
    let transformed = transformer.transform(data_point)
    transformed_data.push(transformed)
  }
  
  assert_eq(transformed_data.length(), collected_data.length())
  
  // Test data enrichment
  let enricher = DataEnricher::new()
  
  // Add enrichment rules
  enricher.add_rule(
    "environment",
    |data_point| {
      data_point.add_tag("environment", "production")
      data_point
    }
  )
  
  enricher.add_rule(
    "region",
    |data_point| {
      data_point.add_tag("region", "us-west-2")
      data_point
    }
  )
  
  // Apply enrichment
  let enriched_data = []
  for data_point in transformed_data {
    let enriched = enricher.enrich(data_point)
    enriched_data.push(enriched)
  }
  
  assert_eq(enriched_data.length(), transformed_data.length())
  
  // Verify enrichment
  for data_point in enriched_data {
    assert_eq(data_point.get_tag("environment"), "production")
    assert_eq(data_point.get_tag("region"), "us-west-2")
  }
  
  // Test data export to sinks
  let export_tasks = []
  
  for sink in ["graphite", "kafka", "splunk"] {
    let task = integration_manager.export_to_sink(sink, enriched_data.clone())
    export_tasks.push(task)
  }
  
  // Wait for export to complete
  for task in export_tasks {
    let result = task.wait()
    assert_true(result.is_success())
    assert_true(result.unwrap().exported_count > 0)
  }
  
  // Test export to different formats
  let json_exporter = JSONExporter::new()
  let csv_exporter = CSVExporter::new()
  let parquet_exporter = ParquetExporter::new()
  
  // Export to JSON
  let json_result = json_exporter.export(enriched_data.clone())
  assert_true(json_result.is_success())
  assert_true(json_result.unwrap().content.length() > 0)
  assert_eq(json_result.unwrap().format, "json")
  
  // Export to CSV
  let csv_result = csv_exporter.export(enriched_data.clone())
  assert_true(csv_result.is_success())
  assert_true(csv_result.unwrap().content.length() > 0)
  assert_eq(csv_result.unwrap().format, "csv")
  
  // Export to Parquet
  let parquet_result = parquet_exporter.export(enriched_data.clone())
  assert_true(parquet_result.is_success())
  assert_true(parquet_result.unwrap().content.length() > 0)
  assert_eq(parquet_result.unwrap().format, "parquet")
  
  // Test scheduled integration
  let schedule = IntegrationSchedule::new()
    .add_job("collect_prometheus", "prometheus", Duration::from_minutes(1))
    .add_job("collect_influxdb", "influxdb", Duration::from_minutes(5))
    .add_job("export_kafka", "kafka", Duration::from_minutes(2))
  
  integration_manager.set_schedule(schedule)
  
  // Verify scheduled jobs
  let scheduled_jobs = integration_manager.get_scheduled_jobs()
  
  assert_true(scheduled_jobs.length() == 3)
  
  for job in scheduled_jobs {
    assert_true(job.name.length() > 0)
    assert_true(job.target.length() > 0)
    assert_true(job.interval > Duration::from_zero())
    assert_true(job.last_run_time >= 0)
    assert_true(job.next_run_time > job.last_run_time)
  }
  
  // Test integration metrics
  let integration_metrics = integration_manager.get_metrics()
  
  assert_true(integration_metrics.total_collection_operations > 0)
  assert_true(integration_metrics.total_export_operations > 0)
  assert_true(integration_metrics.total_data_points_processed > 0)
  assert_true(integration_metrics.average_collection_time_ms > 0)
  assert_true(integration_metrics.average_export_time_ms > 0)
  assert_true(integration_metrics.error_rate >= 0.0 && integration_metrics.error_rate <= 1.0)
}