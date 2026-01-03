// Custom Metrics Tests for Azimuth Telemetry System
// This file contains test cases for custom metrics functionality

// Test 1: Custom Metric Creation and Registration
test "custom metric creation and registration" {
  let metrics_registry = MetricsRegistry::new()
  
  // Test custom counter creation
  let custom_counter = CustomMetric::create_counter(
    metrics_registry,
    "user_interactions_total",
    "Total number of user interactions",
    "interactions"
  )
  
  assert_eq(CustomMetric::name(custom_counter), "user_interactions_total")
  assert_eq(CustomMetric::description(custom_counter), "Total number of user interactions")
  assert_eq(CustomMetric::unit(custom_counter), "interactions")
  
  // Test custom gauge creation
  let custom_gauge = CustomMetric::create_gauge(
    metrics_registry,
    "active_users",
    "Number of currently active users",
    "users"
  )
  
  assert_eq(CustomMetric::name(custom_gauge), "active_users")
  assert_eq(CustomMetric::description(custom_gauge), "Number of currently active users")
  assert_eq(CustomMetric::unit(custom_gauge), "users")
  
  // Test custom histogram creation
  let custom_histogram = CustomMetric::create_histogram(
    metrics_registry,
    "request_duration_ms",
    "Request duration in milliseconds",
    "ms",
    [1.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
  )
  
  assert_eq(CustomMetric::name(custom_histogram), "request_duration_ms")
  assert_eq(CustomMetric::bucket_boundaries(custom_histogram).length(), 9)
  
  // Test metric registration
  let is_registered = MetricsRegistry::is_registered(metrics_registry, "user_interactions_total")
  assert_true(is_registered)
  
  let registered_metric = MetricsRegistry::get_metric(metrics_registry, "user_interactions_total")
  assert_true(registered_metric.is_some())
}

// Test 2: Metric Operations
test "metric operations" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metrics for testing
  let counter = CustomMetric::create_counter(
    metrics_registry,
    "api_requests_total",
    "Total API requests",
    "requests"
  )
  
  let gauge = CustomMetric::create_gauge(
    metrics_registry,
    "queue_size",
    "Current queue size",
    "items"
  )
  
  let histogram = CustomMetric::create_histogram(
    metrics_registry,
    "response_time_ms",
    "Response time in milliseconds",
    "ms",
    [10.0, 50.0, 100.0, 200.0, 500.0]
  )
  
  // Test counter operations
  CustomMetric::counter_add(counter, 1.0)
  CustomMetric::counter_add(counter, 5.0)
  CustomMetric::counter_add(counter, 3.0, [("endpoint", "/api/users")])
  
  let counter_value = CustomMetric::get_counter_value(counter)
  assert_eq(counter_value, 9.0) // 1 + 5 + 3
  
  let labeled_counter_value = CustomMetric::get_counter_value_with_labels(counter, [("endpoint", "/api/users")])
  assert_eq(labeled_counter_value, 3.0)
  
  // Test gauge operations
  CustomMetric::gauge_set(gauge, 10.0)
  CustomMetric::gauge_add(gauge, 5.0)
  CustomMetric::gauge_sub(gauge, 2.0)
  
  let gauge_value = CustomMetric::get_gauge_value(gauge)
  assert_eq(gauge_value, 13.0) // 10 + 5 - 2
  
  // Test histogram operations
  CustomMetric::histogram_observe(histogram, 15.0)
  CustomMetric::histogram_observe(histogram, 75.0)
  CustomMetric::histogram_observe(histogram, 150.0, [("status", "200")])
  
  let histogram_stats = CustomMetric::get_histogram_stats(histogram)
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 240.0)
  
  let bucket_counts = CustomMetric::get_histogram_bucket_counts(histogram)
  assert_true(bucket_counts.get("le=10.0") == Some(0.0))
  assert_true(bucket_counts.get("le=50.0") == Some(1.0))
  assert_true(bucket_counts.get("le=100.0") == Some(1.0))
  assert_true(bucket_counts.get("le=200.0") == Some(2.0))
}

// Test 3: Metric Aggregation
test "metric aggregation" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create multiple counters for aggregation
  let login_counter = CustomMetric::create_counter(
    metrics_registry,
    "user_logins_total",
    "Total user logins",
    "logins"
  )
  
  let logout_counter = CustomMetric::create_counter(
    metrics_registry,
    "user_logouts_total",
    "Total user logouts",
    "logouts"
  )
  
  let registration_counter = CustomMetric::create_counter(
    metrics_registry,
    "user_registrations_total",
    "Total user registrations",
    "registrations"
  )
  
  // Add values to counters
  CustomMetric::counter_add(login_counter, 100.0)
  CustomMetric::counter_add(logout_counter, 95.0)
  CustomMetric::counter_add(registration_counter, 25.0)
  
  // Test sum aggregation
  let sum_aggregation = MetricAggregator::sum([login_counter, logout_counter, registration_counter])
  assert_eq(sum_aggregation, 220.0) // 100 + 95 + 25
  
  // Test average aggregation
  let avg_aggregation = MetricAggregator::average([login_counter, logout_counter, registration_counter])
  assert_eq(avg_aggregation, 73.33333333333333) // 220 / 3
  
  // Test max aggregation
  let max_aggregation = MetricAggregator::max([login_counter, logout_counter, registration_counter])
  assert_eq(max_aggregation, 100.0)
  
  // Test min aggregation
  let min_aggregation = MetricAggregator::min([login_counter, logout_counter, registration_counter])
  assert_eq(min_aggregation, 25.0)
  
  // Test percentile aggregation
  let histogram = CustomMetric::create_histogram(
    metrics_registry,
    "response_times",
    "Response times",
    "ms",
    [10.0, 50.0, 100.0, 200.0, 500.0]
  )
  
  // Add histogram observations
  for i = 1; i <= 100; i = i + 1 {
    CustomMetric::histogram_observe(histogram, i.to_float())
  }
  
  let p50 = MetricAggregator::percentile(histogram, 50.0)
  let p90 = MetricAggregator::percentile(histogram, 90.0)
  let p95 = MetricAggregator::percentile(histogram, 95.0)
  
  assert_true(p50 > 45.0 && p50 < 55.0)
  assert_true(p90 > 85.0 && p90 < 95.0)
  assert_true(p95 > 90.0 && p95 < 100.0)
}

// Test 4: Metric Labels and Dimensions
test "metric labels and dimensions" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metric with labels
  let request_counter = CustomMetric::create_counter(
    metrics_registry,
    "http_requests_total",
    "Total HTTP requests",
    "requests"
  )
  
  // Add values with different label combinations
  CustomMetric::counter_add(request_counter, 10.0, [("method", "GET"), ("status", "200")])
  CustomMetric::counter_add(request_counter, 5.0, [("method", "GET"), ("status", "404")])
  CustomMetric::counter_add(request_counter, 3.0, [("method", "POST"), ("status", "200")])
  CustomMetric::counter_add(request_counter, 2.0, [("method", "POST"), ("status", "500")])
  
  // Test getting values by label combinations
  let get_200 = CustomMetric::get_counter_value_with_labels(request_counter, [("method", "GET"), ("status", "200")])
  let get_404 = CustomMetric::get_counter_value_with_labels(request_counter, [("method", "GET"), ("status", "404")])
  let post_200 = CustomMetric::get_counter_value_with_labels(request_counter, [("method", "POST"), ("status", "200")])
  let post_500 = CustomMetric::get_counter_value_with_labels(request_counter, [("method", "POST"), ("status", "500")])
  
  assert_eq(get_200, 10.0)
  assert_eq(get_404, 5.0)
  assert_eq(post_200, 3.0)
  assert_eq(post_500, 2.0)
  
  // Test aggregation by label
  let get_total = MetricAggregator::sum_by_label(request_counter, "method", "GET")
  let post_total = MetricAggregator::sum_by_label(request_counter, "method", "POST")
  
  assert_eq(get_total, 15.0) // 10 + 5
  assert_eq(post_total, 5.0)  // 3 + 2
  
  let success_total = MetricAggregator::sum_by_label(request_counter, "status", "200")
  let error_total = MetricAggregator::sum_by_label(request_counter, "status", "404") + 
                    MetricAggregator::sum_by_label(request_counter, "status", "500")
  
  assert_eq(success_total, 13.0) // 10 + 3
  assert_eq(error_total, 7.0)    // 5 + 2
  
  // Test getting all label combinations
  let label_combinations = CustomMetric::get_label_combinations(request_counter)
  assert_eq(label_combinations.length(), 4)
}

// Test 5: Metric Export and Formatting
test "metric export and formatting" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metrics
  let counter = CustomMetric::create_counter(
    metrics_registry,
    "test_counter",
    "Test counter metric",
    "items"
  )
  
  let gauge = CustomMetric::create_gauge(
    metrics_registry,
    "test_gauge",
    "Test gauge metric",
    "units"
  )
  
  let histogram = CustomMetric::create_histogram(
    metrics_registry,
    "test_histogram",
    "Test histogram metric",
    "ms",
    [1.0, 5.0, 10.0, 25.0, 50.0]
  )
  
  // Add values
  CustomMetric::counter_add(counter, 42.0)
  CustomMetric::gauge_set(gauge, 17.5)
  CustomMetric::histogram_observe(histogram, 3.0)
  CustomMetric::histogram_observe(histogram, 7.0)
  CustomMetric::histogram_observe(histogram, 15.0)
  
  // Test Prometheus format export
  let prometheus_export = MetricsExporter::prometheus_format(metrics_registry)
  assert_true(prometheus_export.contains("# HELP test_counter Test counter metric"))
  assert_true(prometheus_export.contains("# TYPE test_counter counter"))
  assert_true(prometheus_export.contains("test_counter 42"))
  
  assert_true(prometheus_export.contains("# HELP test_gauge Test gauge metric"))
  assert_true(prometheus_export.contains("# TYPE test_gauge gauge"))
  assert_true(prometheus_export.contains("test_gauge 17.5"))
  
  assert_true(prometheus_export.contains("# HELP test_histogram Test histogram metric"))
  assert_true(prometheus_export.contains("# TYPE test_histogram histogram"))
  assert_true(prometheus_export.contains("test_histogram_bucket{le=\"1\"} 0"))
  assert_true(prometheus_export.contains("test_histogram_bucket{le=\"5\"} 1"))
  assert_true(prometheus_export.contains("test_histogram_bucket{le=\"+Inf\"} 3"))
  assert_true(prometheus_export.contains("test_histogram_sum 25"))
  assert_true(prometheus_export.contains("test_histogram_count 3"))
  
  // Test JSON format export
  let json_export = MetricsExporter::json_format(metrics_registry)
  assert_true(json_export.contains("\"name\":\"test_counter\""))
  assert_true(json_export.contains("\"value\":42"))
  assert_true(json_export.contains("\"name\":\"test_gauge\""))
  assert_true(json_export.contains("\"value\":17.5"))
  
  // Test InfluxDB format export
  let influxdb_export = MetricsExporter::influxdb_format(metrics_registry)
  assert_true(influxdb_export.contains("test_counter value=42"))
  assert_true(influxdb_export.contains("test_gauge value=17.5"))
  
  // Test Graphite format export
  let graphite_export = MetricsExporter::graphite_format(metrics_registry)
  assert_true(graphite_export.contains("test_counter 42"))
  assert_true(graphite_export.contains("test_gauge 17.5"))
}

// Test 6: Metric Filtering and Querying
test "metric filtering and querying" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metrics with different names and labels
  let http_requests = CustomMetric::create_counter(
    metrics_registry,
    "http_requests_total",
    "Total HTTP requests",
    "requests"
  )
  
  let db_queries = CustomMetric::create_counter(
    metrics_registry,
    "db_queries_total",
    "Total database queries",
    "queries"
  )
  
  let cache_hits = CustomMetric::create_counter(
    metrics_registry,
    "cache_hits_total",
    "Total cache hits",
    "hits"
  )
  
  // Add values with labels
  CustomMetric::counter_add(http_requests, 100.0, [("method", "GET"), ("status", "200")])
  CustomMetric::counter_add(http_requests, 10.0, [("method", "GET"), ("status", "404")])
  CustomMetric::counter_add(http_requests, 50.0, [("method", "POST"), ("status", "200")])
  
  CustomMetric::counter_add(db_queries, 25.0, [("table", "users"), ("operation", "SELECT")])
  CustomMetric::counter_add(db_queries, 5.0, [("table", "users"), ("operation", "INSERT")])
  CustomMetric::counter_add(db_queries, 15.0, [("table", "products"), ("operation", "SELECT")])
  
  CustomMetric::counter_add(cache_hits, 80.0, [("cache", "redis")])
  CustomMetric::counter_add(cache_hits, 20.0, [("cache", "memcached")])
  
  // Test filtering by metric name pattern
  let total_metrics = MetricsQuery::filter_by_name_pattern(metrics_registry, "*_total")
  assert_eq(total_metrics.length(), 3)
  
  let http_metrics = MetricsQuery::filter_by_name_pattern(metrics_registry, "http_*")
  assert_eq(http_metrics.length(), 1)
  
  // Test filtering by labels
  let get_requests = MetricsQuery::filter_by_labels(metrics_registry, [("method", "GET")])
  assert_eq(get_requests.length(), 1)
  
  let user_operations = MetricsQuery::filter_by_labels(metrics_registry, [("table", "users")])
  assert_eq(user_operations.length(), 1)
  
  // Test complex queries
  let query = MetricQuery::new()
    .name_pattern("*_total")
    .label_equals("method", "GET")
    .label_not_equals("status", "404")
  
  let results = MetricsQuery::execute(metrics_registry, query)
  assert_eq(results.length(), 1)
  
  // Test metric value extraction
  let value = MetricsQuery::get_value(metrics_registry, "http_requests_total", [("method", "GET"), ("status", "200")])
  assert_eq(value, Some(100.0))
  
  // Test time series data
  let time_series = MetricsQuery::get_time_series(metrics_registry, "http_requests_total", [("method", "GET")])
  assert_eq(time_series.length(), 2) // Two status codes
}

// Test 7: Metric Groups and Collections
test "metric groups and collections" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metric group
  let api_metrics = MetricGroup::new("api_metrics", "API-related metrics")
  
  // Add metrics to group
  let request_counter = CustomMetric::create_counter(
    metrics_registry,
    "api_requests_total",
    "Total API requests",
    "requests"
  )
  
  let response_time = CustomMetric::create_histogram(
    metrics_registry,
    "api_response_time_ms",
    "API response time in milliseconds",
    "ms",
    [10.0, 50.0, 100.0, 200.0, 500.0]
  )
  
  let error_rate = CustomMetric::create_gauge(
    metrics_registry,
    "api_error_rate",
    "API error rate",
    "percentage"
  )
  
  MetricGroup::add_metric(api_metrics, request_counter)
  MetricGroup::add_metric(api_metrics, response_time)
  MetricGroup::add_metric(api_metrics, error_rate)
  
  // Test group operations
  let group_metrics = MetricGroup::get_metrics(api_metrics)
  assert_eq(group_metrics.length(), 3)
  
  // Test group aggregation
  let group_summary = MetricGroup::get_summary(api_metrics)
  assert_eq(group_summary.metric_count, 3)
  assert_eq(group_summary.total_count, 0) // Initially zero
  
  // Add values to metrics
  CustomMetric::counter_add(request_counter, 100.0)
  CustomMetric::histogram_observe(response_time, 50.0)
  CustomMetric::gauge_set(error_rate, 5.0)
  
  let updated_summary = MetricGroup::get_summary(api_metrics)
  assert_eq(updated_summary.total_count, 102.0) // 100 (counter) + 1 (histogram count) + 1 (gauge)
  
  // Test metric collection
  let collection = MetricCollection::new()
  MetricCollection::add_group(collection, api_metrics)
  
  // Create another group
  let db_metrics = MetricGroup::new("db_metrics", "Database-related metrics")
  let db_connections = CustomMetric::create_gauge(
    metrics_registry,
    "db_connections",
    "Active database connections",
    "connections"
  )
  
  MetricGroup::add_metric(db_metrics, db_connections)
  MetricCollection::add_group(collection, db_metrics)
  
  // Test collection operations
  let all_groups = MetricCollection::get_groups(collection)
  assert_eq(all_groups.length(), 2)
  
  let all_metrics = MetricCollection::get_all_metrics(collection)
  assert_eq(all_metrics.length(), 4)
  
  // Test collection export
  let collection_export = MetricCollection::export(collection, ExportFormat::Prometheus)
  assert_true(collection_export.contains("api_requests_total"))
  assert_true(collection_export.contains("db_connections"))
}

// Test 8: Metric Transformations
test "metric transformations" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create source metrics
  let request_count = CustomMetric::create_counter(
    metrics_registry,
    "request_count",
    "Total request count",
    "requests"
  )
  
  let error_count = CustomMetric::create_counter(
    metrics_registry,
    "error_count",
    "Total error count",
    "errors"
  )
  
  let response_times = CustomMetric::create_histogram(
    metrics_registry,
    "response_time_ms",
    "Response time in milliseconds",
    "ms",
    [10.0, 50.0, 100.0, 200.0, 500.0]
  )
  
  // Add values
  CustomMetric::counter_add(request_count, 1000.0)
  CustomMetric::counter_add(error_count, 50.0)
  
  for i = 1; i <= 100; i = i + 1 {
    CustomMetric::histogram_observe(response_times, Random::int(1000).to_float())
  }
  
  // Test ratio transformation
  let error_rate = MetricTransformer::ratio(error_count, request_count, "error_rate")
  let error_rate_value = CustomMetric::get_gauge_value(error_rate)
  assert_eq(error_rate_value, 0.05) // 50 / 1000
  
  // Test rate transformation
  let request_rate = MetricTransformer::rate(request_count, "request_rate_per_second")
  assert_true(CustomMetric::get_gauge_value(request_rate) >= 0.0)
  
  // Test percentile transformation
  let p95_response_time = MetricTransformer::percentile(response_times, 95.0, "response_time_p95")
  assert_true(CustomMetric::get_gauge_value(p95_response_time) > 0.0)
  
  // Test derivative transformation
  let derivative = MetricTransformer::derivative(request_count, "request_derivative")
  assert_true(CustomMetric::get_gauge_value(derivative) >= 0.0)
  
  // Test exponential moving average transformation
  let ema = MetricTransformer::exponential_moving_average(request_count, 0.1, "request_ema")
  assert_true(CustomMetric::get_gauge_value(ema) >= 0.0)
  
  // Test custom transformation
  let custom_metric = MetricTransformer::custom(
    [request_count, error_count],
    lambda { values => values[0] - values[1] },
    "successful_requests"
  )
  
  let custom_value = CustomMetric::get_gauge_value(custom_metric)
  assert_eq(custom_value, 950.0) // 1000 - 50
}

// Test 9: Metric Alerts and Thresholds
test "metric alerts and thresholds" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metrics
  let cpu_usage = CustomMetric::create_gauge(
    metrics_registry,
    "cpu_usage_percentage",
    "CPU usage percentage",
    "percentage"
  )
  
  let memory_usage = CustomMetric::create_gauge(
    metrics_registry,
    "memory_usage_percentage",
    "Memory usage percentage",
    "percentage"
  )
  
  let error_rate = CustomMetric::create_gauge(
    metrics_registry,
    "error_rate_percentage",
    "Error rate percentage",
    "percentage"
  )
  
  // Create alert manager
  let alert_manager = MetricAlertManager::new()
  
  // Define alert rules
  let cpu_alert = MetricAlert::new(
    "high_cpu_usage",
    "cpu_usage_percentage",
    AlertType::GreaterThan,
    80.0,
    AlertSeverity::Warning,
    "CPU usage is above 80%"
  )
  
  let memory_alert = MetricAlert::new(
    "high_memory_usage",
    "memory_usage_percentage",
    AlertType::GreaterThan,
    90.0,
    AlertSeverity::Critical,
    "Memory usage is above 90%"
  )
  
  let error_rate_alert = MetricAlert::new(
    "high_error_rate",
    "error_rate_percentage",
    AlertType::GreaterThan,
    5.0,
    AlertSeverity::Critical,
    "Error rate is above 5%"
  )
  
  MetricAlertManager::add_alert(alert_manager, cpu_alert)
  MetricAlertManager::add_alert(alert_manager, memory_alert)
  MetricAlertManager::add_alert(alert_manager, error_rate_alert)
  
  // Test normal values (no alerts)
  CustomMetric::gauge_set(cpu_usage, 50.0)
  CustomMetric::gauge_set(memory_usage, 60.0)
  CustomMetric::gauge_set(error_rate, 2.0)
  
  let triggered_alerts = MetricAlertManager::check_alerts(alert_manager, metrics_registry)
  assert_eq(triggered_alerts.length(), 0)
  
  // Test warning alert
  CustomMetric::gauge_set(cpu_usage, 85.0)
  
  let warning_alerts = MetricAlertManager::check_alerts(alert_manager, metrics_registry)
  assert_eq(warning_alerts.length(), 1)
  assert_eq(warning_alerts[0].alert_name, "high_cpu_usage")
  assert_eq(warning_alerts[0].severity, AlertSeverity::Warning)
  
  // Test critical alerts
  CustomMetric::gauge_set(memory_usage, 95.0)
  CustomMetric::gauge_set(error_rate, 10.0)
  
  let critical_alerts = MetricAlertManager::check_alerts(alert_manager, metrics_registry)
  assert_eq(critical_alerts.length(), 3) // CPU warning + 2 critical alerts
  
  // Test alert cooldown
  MetricAlertManager::set_cooldown_period(alert_manager, 60) // 60 seconds
  
  CustomMetric::gauge_set(cpu_usage, 40.0) // Back to normal
  let cooldown_alerts = MetricAlertManager::check_alerts(alert_manager, metrics_registry)
  
  // CPU alert should still be in cooldown, but memory and error alerts should trigger
  assert_eq(cooldown_alerts.length(), 2)
  
  // Test alert history
  let alert_history = MetricAlertManager::get_alert_history(alert_manager, 10)
  assert_true(alert_history.length() > 0)
  
  // Test alert statistics
  let alert_stats = MetricAlertManager::get_alert_statistics(alert_manager)
  assert_true(alert_stats.total_alerts > 0)
  assert_true(alert_stats.warning_count > 0)
  assert_true(alert_stats.critical_count > 0)
}

// Test 10: Metric Persistence and Storage
test "metric persistence and storage" {
  let metrics_registry = MetricsRegistry::new()
  
  // Create metrics
  let counter = CustomMetric::create_counter(
    metrics_registry,
    "persistent_counter",
    "Persistent counter metric",
    "items"
  )
  
  let gauge = CustomMetric::create_gauge(
    metrics_registry,
    "persistent_gauge",
    "Persistent gauge metric",
    "units"
  )
  
  let histogram = CustomMetric::create_histogram(
    metrics_registry,
    "persistent_histogram",
    "Persistent histogram metric",
    "ms",
    [1.0, 5.0, 10.0, 25.0, 50.0]
  )
  
  // Add values
  CustomMetric::counter_add(counter, 42.0)
  CustomMetric::gauge_set(gauge, 17.5)
  CustomMetric::histogram_observe(histogram, 3.0)
  CustomMetric::histogram_observe(histogram, 7.0)
  CustomMetric::histogram_observe(histogram, 15.0)
  
  // Test persistence to file
  let storage = MetricStorage::new_file_storage("/tmp/metrics_backup.json")
  let save_result = MetricStorage::save(storage, metrics_registry)
  assert_true(save_result.is_success)
  
  // Test loading from file
  let new_registry = MetricsRegistry::new()
  let load_result = MetricStorage::load(storage, new_registry)
  assert_true(load_result.is_success)
  
  // Verify loaded metrics
  let loaded_counter = MetricsRegistry::get_metric(new_registry, "persistent_counter")
  assert_true(loaded_counter.is_some())
  
  match loaded_counter {
    Some(metric) => {
      let loaded_value = CustomMetric::get_counter_value(metric)
      assert_eq(loaded_value, 42.0)
    }
    None => assert_true(false)
  }
  
  let loaded_gauge = MetricsRegistry::get_metric(new_registry, "persistent_gauge")
  assert_true(loaded_gauge.is_some())
  
  match loaded_gauge {
    Some(metric) => {
      let loaded_value = CustomMetric::get_gauge_value(metric)
      assert_eq(loaded_value, 17.5)
    }
    None => assert_true(false)
  }
  
  // Test database persistence
  let db_storage = MetricStorage::new_database_storage(
    "localhost",
    5432,
    "metrics_db",
    "user",
    "password"
  )
  
  let db_save_result = MetricStorage::save(db_storage, metrics_registry)
  assert_true(db_save_result.is_success)
  
  let db_registry = MetricsRegistry::new()
  let db_load_result = MetricStorage::load(db_storage, db_registry)
  assert_true(db_load_result.is_success)
  
  // Test time series storage
  let ts_storage = MetricStorage::new_time_series_storage(
    "localhost",
    8086,
    "metrics_ts_db",
    "user",
    "password"
  )
  
  // Store time series data points
  let timestamp = Time::now()
  MetricStorage::store_time_series_point(ts_storage, "persistent_counter", timestamp, 42.0)
  MetricStorage::store_time_series_point(ts_storage, "persistent_gauge", timestamp, 17.5)
  
  // Query time series data
  let ts_data = MetricStorage::query_time_series(
    ts_storage,
    "persistent_counter",
    timestamp - 3600, // 1 hour ago
    timestamp
  )
  
  assert_eq(ts_data.length(), 1)
  assert_eq(ts_data[0].value, 42.0)
  
  // Test retention policies
  MetricStorage::set_retention_policy(ts_storage, "persistent_counter", 7 * 24 * 3600) // 7 days
  MetricStorage::set_retention_policy(ts_storage, "persistent_gauge", 30 * 24 * 3600) // 30 days
  
  let retention_policies = MetricStorage::get_retention_policies(ts_storage)
  assert_eq(retention_policies.get("persistent_counter"), Some(7 * 24 * 3600))
  assert_eq(retention_policies.get("persistent_gauge"), Some(30 * 24 * 3600))
}