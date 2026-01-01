// Azimuth Real-time Dashboard Streaming Tests
// This file contains tests for real-time dashboard streaming functionality in the Azimuth telemetry system

// Test 1: Real-time metrics streaming
pub test "real-time metrics streaming" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "realtime-dashboard-test")
  
  // Configure streaming dashboard
  let dashboard_config = azimuth::DashboardConfig::new()
    .with_update_interval(1000)  // 1 second updates
    .with_buffer_size(100)
    .with_max_data_points(1000)
    .with_compression_enabled(true)
  
  let dashboard = azimuth::RealtimeDashboard::new(dashboard_config)
  
  // Create metrics for streaming
  let request_counter = azimuth::Meter::create_counter(meter, "dashboard.requests", Some("Dashboard requests"), Some("count"))
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "dashboard.response_time", Some("Response time"), Some("ms"))
  let cpu_gauge = azimuth::Meter::create_gauge(meter, "dashboard.cpu", Some("CPU usage"), Some("percent"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "dashboard.memory", Some("Memory usage"), Some("bytes"))
  
  // Register metrics with dashboard
  azimuth::RealtimeDashboard::register_metric(dashboard, "requests", request_counter)
  azimuth::RealtimeDashboard::register_metric(dashboard, "response_time", response_time_histogram)
  azimuth::RealtimeDashboard::register_metric(dashboard, "cpu", cpu_gauge)
  azimuth::RealtimeDashboard::register_metric(dashboard, "memory", memory_gauge)
  
  // Start streaming
  let stream_handle = azimuth::RealtimeDashboard::start_streaming(dashboard)
  
  // Simulate real-time data generation
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = 5000000000L  // 5 seconds
  
  while azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time < duration {
    // Generate request count
    azimuth::Counter::add(request_counter, 1, [("endpoint", "api/dashboard"), ("method", "GET")])
    
    // Generate response time
    let response_time = 50.0 + (azimuth::Clock::now_unix_millis(azimuth::Clock::system()) % 200).to_double()
    azimuth::Histogram::record(response_time_histogram, response_time, [("endpoint", "api/dashboard")])
    
    // Generate CPU usage
    let cpu_usage = 20.0 + (azimuth::Clock::now_unix_millis(azimuth::Clock::system()) % 60).to_double()
    azimuth::Gauge::set(cpu_gauge, cpu_usage.to_long(), [("instance", "dashboard-server")])
    
    // Generate memory usage
    let memory_usage = (1024 * 1024 * 1024).to_long() + (azimuth::Clock::now_unix_millis(azimuth::Clock::system()) % 100 * 1024 * 1024).to_long()
    azimuth::Gauge::set(memory_gauge, memory_usage, [("instance", "dashboard-server")])
    
    // Wait for next update interval
    azimuth::Clock::sleep(100)  // 100ms
  }
  
  // Stop streaming
  azimuth::RealtimeDashboard::stop_streaming(dashboard, stream_handle)
  
  // Verify streaming metrics
  let streaming_stats = azimuth::RealtimeDashboard::get_streaming_statistics(dashboard)
  assert_true(streaming_stats.total_updates >= 5)  // At least 5 updates in 5 seconds
  assert_true(streaming_stats.data_points_sent >= 20)  // At least 4 metrics * 5 updates
  assert_true(streaming_stats.bytes_transferred > 0)
  assert_true(streaming_stats.average_update_time > 0.0)
  
  // Test data retrieval
  let requests_data = azimuth::RealtimeDashboard::get_metric_data(dashboard, "requests")
  let response_time_data = azimuth::RealtimeDashboard::get_metric_data(dashboard, "response_time")
  let cpu_data = azimuth::RealtimeDashboard::get_metric_data(dashboard, "cpu")
  let memory_data = azimuth::RealtimeDashboard::get_metric_data(dashboard, "memory")
  
  assert_true(requests_data.length() >= 5)
  assert_true(response_time_data.length() >= 5)
  assert_true(cpu_data.length() >= 5)
  assert_true(memory_data.length() >= 5)
}

// Test 2: WebSocket streaming for live updates
pub test "websocket streaming for live updates" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "websocket-test")
  
  // Configure WebSocket server
  let ws_config = azimuth::WebSocketConfig::new()
    .with_port(8080)
    .with_path("/telemetry")
    .with_max_connections(100)
    .with_heartbeat_interval(30000)  // 30 seconds
    .with_message_buffer_size(1024)
  
  let ws_server = azimuth::WebSocketServer::new(ws_config)
  
  // Start WebSocket server
  let server_handle = azimuth::WebSocketServer::start(ws_server)
  
  // Create metrics for WebSocket streaming
  let active_users = azimuth::Meter::create_gauge(meter, "active.users", Some("Active users"), Some("count"))
  let message_rate = azimuth::Meter::create_counter(meter, "message.rate", Some("Message rate"), Some("msg/s"))
  let error_rate = azimuth::Meter::create_counter(meter, "error.rate", Some("Error rate"), Some("errors/s"))
  
  // Configure WebSocket streaming
  let ws_stream = azimuth::WebSocketStreamer::new(ws_server)
  azimuth::WebSocketStreamer::add_metric(ws_stream, "active_users", active_users)
  azimuth::WebSocketStreamer::add_metric(ws_stream, "message_rate", message_rate)
  azimuth::WebSocketStreamer::add_metric(ws_stream, "error_rate", error_rate)
  
  // Start streaming to WebSocket clients
  let stream_handle = azimuth::WebSocketStreamer::start(ws_stream)
  
  // Simulate WebSocket clients
  let clients = []
  for i = 0; i < 5; i = i + 1 {
    let client = azimuth::WebSocketClient::new("ws://localhost:8080/telemetry")
    let client_id = "client-" + i.to_string()
    azimuth::WebSocketClient::connect(client, client_id)
    clients.push(client)
  }
  
  // Simulate real-time data updates
  for i = 0; i < 20; i = i + 1 {
    // Update active users
    let user_count = 100 + (i % 50)
    azimuth::Gauge::set(active_users, user_count.to_long(), [("service", "chat")])
    
    // Update message rate
    let msg_count = 10 + (i % 20)
    azimuth::Counter::add(message_rate, msg_count.to_long(), [("service", "chat")])
    
    // Update error rate
    if i % 10 == 0 {
      azimuth::Counter::add(error_rate, 1, [("service", "chat"), ("error.type", "timeout")])
    }
    
    // Broadcast updates to clients
    azimuth::WebSocketStreamer::broadcast(ws_stream)
    
    // Wait for next update
    azimuth::Clock::sleep(200)  // 200ms
  }
  
  // Verify client connections and message delivery
  let server_stats = azimuth::WebSocketServer::get_statistics(ws_server)
  assert_true(server_stats.active_connections >= 5)
  assert_true(server_stats.total_messages_sent >= 20 * 5)  // 20 updates * 5 clients
  assert_true(server_stats.total_bytes_transferred > 0)
  
  // Verify client message reception
  for client in clients {
    let client_stats = azimuth::WebSocketClient::get_statistics(client)
    assert_true(client_stats.messages_received >= 20)
    assert_true(client_stats.bytes_received > 0)
    assert_true(client_stats.connection_duration > 0)
  }
  
  // Test message format and content
  let sample_client = clients[0]
  let messages = azimuth::WebSocketClient::get_messages(sample_client)
  assert_true(messages.length() >= 20)
  
  // Verify message structure
  for message in messages.take(5) {
    assert_true(message.contains("active_users"))
    assert_true(message.contains("message_rate"))
    assert_true(message.contains("timestamp"))
    assert_true(message.contains("data"))
  }
  
  // Clean up
  azimuth::WebSocketStreamer::stop(ws_stream, stream_handle)
  for client in clients {
    azimuth::WebSocketClient::disconnect(client)
  }
  azimuth::WebSocketServer::stop(ws_server, server_handle)
}

// Test 3: Real-time alerting and notifications
pub test "real-time alerting and notifications" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "alerting-test")
  
  // Configure alerting system
  let alert_config = azimuth::AlertConfig::new()
    .with_evaluation_interval(5000)  // 5 seconds
    .with_notification_channels(["email", "slack", "webhook"])
    .with_cooldown_period(30000)     // 30 seconds
    .with_max_alerts_per_hour(100)
  
  let alert_manager = azimuth::AlertManager::new(alert_config)
  
  // Create metrics for alerting
  let error_rate = azimuth::Meter::create_counter(meter, "error.rate", Some("Error rate"), Some("errors/min"))
  let response_time = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  let cpu_usage = azimuth::Meter::create_gauge(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  let memory_usage = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("percent"))
  
  // Configure alert rules
  let error_rate_rule = azimuth::AlertRule::new("high_error_rate")
    .with_metric("error.rate")
    .with_condition("rate > 10")
    .with_duration("5m")
    .with_severity("critical")
    .with_message("Error rate is too high: {{value}} errors/min")
  
  let response_time_rule = azimuth::AlertRule::new("slow_response")
    .with_metric("response.time")
    .with_condition("p95 > 1000")
    .with_duration("2m")
    .with_severity("warning")
    .with_message("Response time is slow: {{value}}ms")
  
  let cpu_rule = azimuth::AlertRule::new("high_cpu")
    .with_metric("cpu.usage")
    .with_condition("value > 80")
    .with_duration("1m")
    .with_severity("warning")
    .with_message("CPU usage is high: {{value}}%")
  
  let memory_rule = azimuth::AlertRule::new("high_memory")
    .with_metric("memory.usage")
    .with_condition("value > 90")
    .with_duration("30s")
    .with_severity("critical")
    .with_message("Memory usage is critical: {{value}}%")
  
  // Register alert rules
  azimuth::AlertManager::add_rule(alert_manager, error_rate_rule)
  azimuth::AlertManager::add_rule(alert_manager, response_time_rule)
  azimuth::AlertManager::add_rule(alert_manager, cpu_rule)
  azimuth::AlertManager::add_rule(alert_manager, memory_rule)
  
  // Start alert evaluation
  let alert_handle = azimuth::AlertManager::start_evaluation(alert_manager)
  
  // Simulate metrics that trigger alerts
  for i = 0; i < 30; i = i + 1 {
    // Generate high error rate (will trigger after 5 minutes in real scenario, but we'll trigger immediately for testing)
    azimuth::Counter::add(error_rate, 15, [("service", "api"), ("endpoint", "/data")])
    
    // Generate slow response times
    let slow_response = 1200.0 + (i % 200).to_double()
    azimuth::Histogram::record(response_time, slow_response, [("service", "api"), ("endpoint", "/data")])
    
    // Generate high CPU usage
    let high_cpu = 85.0 + (i % 10).to_double()
    azimuth::Gauge::set(cpu_usage, high_cpu.to_long(), [("instance", "server-1")])
    
    // Generate high memory usage
    let high_memory = 92.0 + (i % 5).to_double()
    azimuth::Gauge::set(memory_usage, high_memory.to_long(), [("instance", "server-1")])
    
    // Wait for next evaluation cycle
    azimuth::Clock::sleep(1000)  // 1 second
  }
  
  // Force alert evaluation
  azimuth::AlertManager::evaluate_now(alert_manager)
  
  // Check for triggered alerts
  let active_alerts = azimuth::AlertManager::get_active_alerts(alert_manager)
  assert_true(active_alerts.length() >= 2)  // At least CPU and memory alerts should trigger
  
  // Verify alert details
  for alert in active_alerts {
    assert_true(alert.rule_name == "high_cpu" || alert.rule_name == "high_memory" || 
               alert.rule_name == "high_error_rate" || alert.rule_name == "slow_response")
    assert_true(alert.severity == "warning" || alert.severity == "critical")
    assert_true(alert.message.length() > 0)
    assert_true(alert.triggered_at > 0)
  }
  
  // Test notification delivery
  let notification_stats = azimuth::AlertManager::get_notification_statistics(alert_manager)
  assert_true(notification_stats.total_notifications >= active_alerts.length() * 3)  # 3 channels per alert
  assert_true(notification_stats.successful_notifications >= 0)
  assert_true(notification_stats.failed_notifications >= 0)
  
  // Test alert resolution
  // Generate normal metrics to resolve alerts
  for i = 0; i < 10; i = i + 1 {
    azimuth::Counter::add(error_rate, 2, [("service", "api"), ("endpoint", "/data")])
    azimuth::Histogram::record(response_time, 200.0, [("service", "api"), ("endpoint", "/data")])
    azimuth::Gauge::set(cpu_usage, 45.to_long(), [("instance", "server-1")])
    azimuth::Gauge::set(memory_usage, 60.to_long(), [("instance", "server-1")])
    azimuth::Clock::sleep(1000)
  }
  
  // Force evaluation to resolve alerts
  azimuth::AlertManager::evaluate_now(alert_manager)
  
  // Check resolved alerts
  let resolved_alerts = azimuth::AlertManager::get_resolved_alerts(alert_manager)
  assert_true(resolved_alerts.length() >= 1)
  
  // Clean up
  azimuth::AlertManager::stop_evaluation(alert_manager, alert_handle)
}

// Test 4: Dashboard widget rendering and updates
pub test "dashboard widget rendering and updates" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "widget-test")
  
  // Create dashboard with widgets
  let dashboard = azimuth::Dashboard::new("Test Dashboard")
  
  // Create metrics for widgets
  let total_requests = azimuth::Meter::create_counter(meter, "total.requests", Some("Total requests"), Some("count"))
  let avg_response_time = azimuth::Meter::create_histogram(meter, "avg.response.time", Some("Average response time"), Some("ms"))
  let error_percentage = azimuth::Meter::create_counter(meter, "error.percentage", Some("Error percentage"), Some("percent"))
  let active_services = azimuth::Meter::create_gauge(meter, "active.services", Some("Active services"), Some("count"))
  
  // Create widgets
  let counter_widget = azimuth::CounterWidget::new("Total Requests", total_requests)
    .with_format("number")
    .with_unit("requests")
    .with_color("#00ff00")
  
  let gauge_widget = azimuth::GaugeWidget::new("Response Time", avg_response_time)
    .with_min(0)
    .with_max(2000)
    .with_thresholds([
      ("green", 500),
      ("yellow", 1000),
      ("red", 1500)
    ])
  
  let pie_widget = azimuth::PieWidget::new("Error Distribution", error_percentage)
    .with_segments([
      ("Success", "#00ff00"),
      ("Client Error", "#ffff00"),
      ("Server Error", "#ff0000")
    ])
  
  let status_widget = azimuth::StatusWidget::new("Service Status", active_services)
    .with_good_threshold(5)
    .with_warning_threshold(3)
    .with_critical_threshold(1)
  
  // Add widgets to dashboard
  azimuth::Dashboard::add_widget(dashboard, counter_widget)
  azimuth::Dashboard::add_widget(dashboard, gauge_widget)
  azimuth::Dashboard::add_widget(dashboard, pie_widget)
  azimuth::Dashboard::add_widget(dashboard, status_widget)
  
  // Configure dashboard layout
  azimuth::Dashboard::set_layout(dashboard, [
    [("Total Requests", 2, 1)],  // 2 columns, 1 row
    [("Response Time", 1, 1), ("Error Distribution", 1, 1)],  // 2 widgets side by side
    [("Service Status", 2, 1)]   # 2 columns, 1 row
  ])
  
  // Start dashboard updates
  let update_handle = azimuth::Dashboard::start_updates(dashboard, 2000)  # 2 second updates
  
  // Generate data for widgets
  for i = 0; i < 20; i = i + 1 {
    // Update total requests
    azimuth::Counter::add(total_requests, 10, [("service", "web"), ("method", "GET")])
    
    // Update response time
    let response_time = 100.0 + (i % 400).to_double()
    azimuth::Histogram::record(avg_response_time, response_time, [("service", "web")])
    
    // Update error percentage
    if i % 10 == 0 {
      azimuth::Counter::add(error_percentage, 1, [("service", "web"), ("error.type", "500")])
    } else {
      azimuth::Counter::add(error_percentage, 9, [("service", "web"), ("status", "200")])
    }
    
    // Update active services
    let service_count = 3 + (i % 5)
    azimuth::Gauge::set(active_services, service_count.to_long(), [("environment", "production")])
    
    azimuth::Clock::sleep(500)  # 500ms
  }
  
  // Test widget rendering
  let dashboard_html = azimuth::Dashboard::render_html(dashboard)
  assert_true(dashboard_html.contains("Total Requests"))
  assert_true(dashboard_html.contains("Response Time"))
  assert_true(dashboard_html.contains("Error Distribution"))
  assert_true(dashboard_html.contains("Service Status"))
  
  // Test widget data
  let counter_data = azimuth::Dashboard::get_widget_data(dashboard, "Total Requests")
  let gauge_data = azimuth::Dashboard::get_widget_data(dashboard, "Response Time")
  let pie_data = azimuth::Dashboard::get_widget_data(dashboard, "Error Distribution")
  let status_data = azimuth::Dashboard::get_widget_data(dashboard, "Service Status")
  
  assert_true(counter_data.value > 0)
  assert_true(gauge_data.value > 0.0)
  assert_true(pie_data.segments.length() >= 2)
  assert_true(status_data.status == "good" || status_data.status == "warning" || status_data.status == "critical")
  
  // Test dashboard export
  let json_export = azimuth::Dashboard::export_json(dashboard)
  assert_true(json_export.contains("widgets"))
  assert_true(json_export.contains("layout"))
  assert_true(json_export.contains("data"))
  
  let csv_export = azimuth::Dashboard::export_csv(dashboard)
  assert_true(csv_export.contains("Total Requests"))
  assert_true(csv_export.contains("Response Time"))
  assert_true(csv_export.contains("timestamp"))
  
  // Clean up
  azimuth::Dashboard::stop_updates(dashboard, update_handle)
}

// Test 5: Real-time data aggregation and rollups
pub test "real-time data aggregation and rollups" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "aggregation-test")
  
  // Configure aggregation engine
  let agg_config = azimuth::AggregationConfig::new()
    .with_window_size(60000)      # 1 minute windows
    .with_rollup_intervals([300000, 900000, 3600000])  # 5min, 15min, 1hour
    .with_retention_period(86400000)  # 24 hours
    .with_max_series(10000)
  
  let aggregation_engine = azimuth::AggregationEngine::new(agg_config)
  
  // Create metrics for aggregation
  let request_counter = azimuth::Meter::create_counter(meter, "requests", Some("Requests"), Some("count"))
  let latency_histogram = azimuth::Meter::create_histogram(meter, "latency", Some("Latency"), Some("ms"))
  let error_counter = azimuth::Meter::create_counter(meter, "errors", Some("Errors"), Some("count"))
  
  // Register metrics for aggregation
  azimuth::AggregationEngine::register_metric(aggregation_engine, "requests", request_counter)
  azimuth::AggregationEngine::register_metric(aggregation_engine, "latency", latency_histogram)
  azimuth::AggregationEngine::register_metric(aggregation_engine, "errors", error_counter)
  
  // Start aggregation
  let agg_handle = azimuth::AggregationEngine::start(aggregation_engine)
  
  // Generate high-frequency data
  for i = 0; i < 300; i = i + 1 {
    let endpoint = if i % 3 == 0 { "/api/users" } else if i % 3 == 1 { "/api/orders" } else { "/api/products" }
    let response_time = 50.0 + (i % 500).to_double()
    
    azimuth::Counter::add(request_counter, 1, [("endpoint", endpoint), ("method", "GET")])
    azimuth::Histogram::record(latency_histogram, response_time, [("endpoint", endpoint)])
    
    if i % 20 == 0 {
      azimuth::Counter::add(error_counter, 1, [("endpoint", endpoint), ("error.type", "timeout")])
    }
    
    azimuth::Clock::sleep(100)  # 100ms
  }
  
  # Force aggregation
  azimuth::AggregationEngine::aggregate_now(aggregation_engine)
  
  # Test aggregated data
  let minute_data = azimuth::AggregationEngine::get_data(aggregation_engine, "1m")
  let five_minute_data = azimuth::AggregationEngine::get_data(aggregation_engine, "5m")
  let fifteen_minute_data = azimuth::AggregationEngine::get_data(aggregation_engine, "15m")
  let hour_data = azimuth::AggregationEngine::get_data(aggregation_engine, "1h")
  
  assert_true(minute_data.length() >= 1)
  assert_true(five_minute_data.length() >= 1)
  assert_true(fifteen_minute_data.length() >= 1)
  assert_true(hour_data.length() >= 0)  # May be empty if not enough data
  
  # Test aggregated statistics
  let request_stats = azimuth::AggregationEngine::get_statistics(aggregation_engine, "requests")
  let latency_stats = azimuth::AggregationEngine::get_statistics(aggregation_engine, "latency")
  let error_stats = azimuth::AggregationEngine::get_statistics(aggregation_engine, "errors")
  
  assert_true(request_stats.sum >= 300)  # At least 300 requests
  assert_true(latency_stats.count >= 300)  # At least 300 latency measurements
  assert_true(error_stats.sum >= 15)  # At least 15 errors (300/20)
  assert_true(latency_stats.avg >= 50.0 && latency_stats.avg <= 550.0)  # Reasonable average
  
  # Test rollup data
  let rollups = azimuth::AggregationEngine::get_rollups(aggregation_engine)
  assert_true(rollups.length() >= 3)  # 5min, 15min, 1hour
  
  for rollup in rollups {
    assert_true(rollup.interval == 300000 || rollup.interval == 900000 || rollup.interval == 3600000)
    assert_true(rollup.data_points.length() >= 1)
  }
  
  # Test aggregation performance
  let perf_stats = azimuth::AggregationEngine::get_performance_statistics(aggregation_engine)
  assert_true(perf_stats.aggregation_time_avg > 0.0)
  assert_true(perf_stats.aggregation_time_max >= perf_stats.aggregation_time_avg)
  assert_true(perf_stats.memory_usage > 0)
  assert_true(perf_stats.series_count >= 3)  # At least 3 metrics
  
  # Clean up
  azimuth::AggregationEngine::stop(aggregation_engine, agg_handle)
}