// Azimuth Telemetry System - High Quality Premium Test Suite
// This file contains 10 high-quality test cases covering advanced telemetry scenarios

// Test 1: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  let serializer = TelemetrySerializer::new()
  
  // Create complex telemetry data with nested attributes
  let original_data = TelemetryData::new(
    "service_performance",
    85.5,
    Attributes::with_data([
      ("service_name", StringValue("payment_service")),
      ("region", StringValue("us-west")),
      ("version", StringValue("1.2.3")),
      ("metadata", MapValue([
        ("request_id", StringValue("req-12345")),
        ("user_id", StringValue("user-67890")),
        ("session_id", StringValue("sess-abcdef"))
      ]))
    ]),
    1640995200000L
  )
  
  // Serialize to JSON
  let json_data = TelemetrySerializer::to_json(serializer, original_data)
  assert_true(json_data.length() > 0)
  
  // Deserialize from JSON
  let deserialized_data = TelemetrySerializer::from_json(serializer, json_data)
  
  // Verify data integrity
  assert_eq(TelemetryData::metric_name(original_data), TelemetryData::metric_name(deserialized_data))
  assert_eq(TelemetryData::value(original_data), TelemetryData::value(deserialized_data))
  assert_eq(TelemetryData::timestamp(original_data), TelemetryData::timestamp(deserialized_data))
  
  // Test nested attributes preservation
  let original_attrs = TelemetryData::attributes(original_data)
  let deserialized_attrs = TelemetryData::attributes(deserialized_data)
  
  match Attributes::get(original_attrs, "metadata") {
    Some(MapValue(metadata)) => {
      match Attributes::get(deserialized_attrs, "metadata") {
        Some(MapValue(deserialized_metadata)) => {
          assert_eq(metadata.length(), deserialized_metadata.length())
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test binary serialization
  let binary_data = TelemetrySerializer::to_binary(serializer, original_data)
  let binary_deserialized = TelemetrySerializer::from_binary(serializer, binary_data)
  
  assert_eq(TelemetryData::metric_name(original_data), TelemetryData::metric_name(binary_deserialized))
  assert_eq(TelemetryData::value(original_data), TelemetryData::value(binary_deserialized))
}

// Test 2: Distributed Tracing Context Propagation
test "distributed tracing context propagation" {
  let tracer = Tracer::new("payment_service")
  let propagator = TraceContextPropagator::new()
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "process_payment")
  Span::set_attribute(root_span, "payment.amount", 99.99)
  Span::set_attribute(root_span, "payment.currency", "USD")
  
  // Extract context from root span
  let context = Span::context(root_span)
  let headers = TraceContextPropagator::inject(propagator, context)
  
  // Simulate cross-service call
  let downstream_service = Tracer::new("inventory_service")
  let extracted_context = TraceContextPropagator::extract(propagator, headers)
  
  // Create child span in downstream service
  let child_span = Tracer::start_span_with_context(downstream_service, "check_inventory", extracted_context)
  Span::set_attribute(child_span, "product.id", "prod-12345")
  Span::set_attribute(child_span, "product.quantity", 2)
  
  // Verify context propagation
  let child_context = Span::context(child_span)
  assert_eq(Context::trace_id(context), Context::trace_id(child_context))
  assert_eq(Context::span_id(context), Context::parent_span_id(child_context))
  
  // End spans
  Span::end(child_span)
  Span::end(root_span)
  
  // Verify trace hierarchy
  let trace_data = Tracer::get_trace_data(tracer)
  assert_eq(trace_data.spans.length(), 2)
  assert_eq(trace_data.spans[0].name, "process_payment")
  assert_eq(trace_data.spans[1].name, "check_inventory")
  assert_eq(trace_data.spans[1].parent_span_id, trace_data.spans[0].span_id)
}

// Test 3: Custom Metrics Aggregation
test "custom metrics aggregation" {
  let meter_provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(meter_provider, "business_metrics")
  
  // Create custom metrics
  let revenue_counter = Meter::create_counter(meter, "revenue_total", Some("Total revenue"), Some("USD"))
  let order_size_histogram = Meter::create_histogram(meter, "order_size", Some("Order size"), Some("items"))
  let customer_satisfaction_gauge = Meter::create_gauge(meter, "customer_satisfaction", Some("Customer satisfaction"), Some("score"))
  
  // Simulate business operations
  let orders = [
    { id: "order-1", amount: 99.99, items: 3, satisfaction: 4.5 },
    { id: "order-2", amount: 149.99, items: 5, satisfaction: 4.2 },
    { id: "order-3", amount: 79.99, items: 2, satisfaction: 4.8 },
    { id: "order-4", amount: 199.99, items: 8, satisfaction: 3.9 },
    { id: "order-5", amount: 129.99, items: 4, satisfaction: 4.6 }
  ]
  
  // Process orders and record metrics
  for order in orders {
    Counter::add(revenue_counter, order.amount, Some(Attributes::with_data([
      ("order_id", StringValue(order.id)),
      ("category", StringValue(if order.amount > 100.0 { "premium" } else { "standard" }))
    ])))
    
    Histogram::record(order_size_histogram, order.items.to_float(), Some(Attributes::with_data([
      ("order_id", StringValue(order.id))
    ])))
    
    Gauge::set(customer_satisfaction_gauge, order.satisfaction, Some(Attributes::with_data([
      ("order_id", StringValue(order.id))
    ])))
  }
  
  // Create custom aggregation
  let aggregator = CustomAggregator::new()
  let revenue_by_category = CustomAggregator::group_by(aggregator, revenue_counter, "category")
  
  assert_eq(revenue_by_category.length(), 2)
  
  let premium_revenue = revenue_by_category.find(fn(entry) { entry.key == "premium" })
  match premium_revenue {
    Some(entry) => {
      // Expected: 149.99 + 199.99 + 129.99 = 479.97
      assert_eq(entry.value, 479.97)
    }
    None => assert_true(false)
  }
  
  let standard_revenue = revenue_by_category.find(fn(entry) { entry.key == "standard" })
  match standard_revenue {
    Some(entry) => {
      // Expected: 99.99 + 79.99 = 179.98
      assert_eq(entry.value, 179.98)
    }
    None => assert_true(false)
  }
  
  // Test percentile calculation
  let order_size_percentiles = CustomAggregator::calculate_percentiles(aggregator, order_size_histogram, [50.0, 90.0, 99.0])
  assert_eq(order_size_percentiles[50.0], 4.0) // Median
  assert_eq(order_size_percentiles[90.0], 8.0) // 90th percentile
}

// Test 4: Telemetry Anomaly Detection and Recovery
test "telemetry anomaly detection and recovery" {
  let anomaly_detector = AnomalyDetector::new()
  let recovery_manager = RecoveryManager::new()
  
  // Create baseline telemetry data
  let baseline_data = [
    TelemetryPoint::new("cpu_usage", 45.0, Attributes::with_data([("instance", StringValue("server-1"))])),
    TelemetryPoint::new("cpu_usage", 47.0, Attributes::with_data([("instance", StringValue("server-1"))])),
    TelemetryPoint::new("cpu_usage", 43.0, Attributes::with_data([("instance", StringValue("server-1"))])),
    TelemetryPoint::new("cpu_usage", 46.0, Attributes::with_data([("instance", StringValue("server-1"))])),
    TelemetryPoint::new("cpu_usage", 44.0, Attributes::with_data([("instance", StringValue("server-1"))]))
  ]
  
  // Train anomaly detection model
  AnomalyDetector::train(anomaly_detector, baseline_data, "cpu_usage")
  
  // Test with normal data
  let normal_data = TelemetryPoint::new("cpu_usage", 48.0, Attributes::with_data([("instance", StringValue("server-1"))]))
  let normal_result = AnomalyDetector::detect(anomaly_detector, normal_data)
  assert_false(normal_result.is_anomaly)
  
  // Test with anomalous data
  let anomalous_data = TelemetryPoint::new("cpu_usage", 95.0, Attributes::with_data([("instance", StringValue("server-1"))]))
  let anomaly_result = AnomalyDetector::detect(anomaly_detector, anomalous_data)
  assert_true(anomaly_result.is_anomaly)
  assert_eq(anomaly_result.severity, High)
  
  // Test recovery strategies
  let recovery_strategy = RecoveryManager::get_strategy(recovery_manager, "high_cpu")
  let recovery_actions = RecoveryStrategy::execute(recovery_strategy, anomaly_result)
  
  assert_eq(recovery_actions.length(), 3)
  assert_eq(recovery_actions[0].type, "scale_up")
  assert_eq(recovery_actions[1].type, "load_balance")
  assert_eq(recovery_actions[2].type, "alert")
  
  // Verify recovery effectiveness
  let post_recovery_data = TelemetryPoint::new("cpu_usage", 55.0, Attributes::with_data([("instance", StringValue("server-1"))]))
  let post_recovery_result = AnomalyDetector::detect(anomaly_detector, post_recovery_data)
  assert_false(post_recovery_result.is_anomaly)
  
  // Test anomaly pattern recognition
  let pattern_data = [
    TelemetryPoint::new("memory_usage", 60.0, Attributes::with_data([("instance", StringValue("server-2"))])),
    TelemetryPoint::new("memory_usage", 65.0, Attributes::with_data([("instance", StringValue("server-2"))])),
    TelemetryPoint::new("memory_usage", 70.0, Attributes::with_data([("instance", StringValue("server-2"))])),
    TelemetryPoint::new("memory_usage", 75.0, Attributes::with_data([("instance", StringValue("server-2"))])),
    TelemetryPoint::new("memory_usage", 80.0, Attributes::with_data([("instance", StringValue("server-2"))]))
  ]
  
  let pattern_anomaly = AnomalyDetector::detect_pattern(anomaly_detector, pattern_data, "memory_usage")
  assert_true(pattern_anomaly.is_anomaly)
  assert_eq(pattern_anomaly.pattern_type, "gradual_increase")
}

// Test 5: Cross-Service Telemetry Consistency
test "cross-service telemetry consistency" {
  let consistency_checker = ConsistencyChecker::new()
  
  // Create telemetry data from multiple services
  let auth_service_data = [
    TelemetryPoint::new("request_count", 100, Attributes::with_data([
      ("service", StringValue("auth")),
      ("endpoint", StringValue("/login")),
      ("status", StringValue("200"))
    ])),
    TelemetryPoint::new("request_count", 5, Attributes::with_data([
      ("service", StringValue("auth")),
      ("endpoint", StringValue("/login")),
      ("status", StringValue("401"))
    ]))
  ]
  
  let api_gateway_data = [
    TelemetryPoint::new("request_count", 105, Attributes::with_data([
      ("service", StringValue("gateway")),
      ("downstream", StringValue("auth")),
      ("endpoint", StringValue("/login"))
    ]))
  ]
  
  let user_service_data = [
    TelemetryPoint::new("user_sessions", 95, Attributes::with_data([
      ("service", StringValue("user")),
      ("auth_status", StringValue("success"))
    ])),
    TelemetryPoint::new("user_sessions", 5, Attributes::with_data([
      ("service", StringValue("user")),
      ("auth_status", StringValue("failed"))
    ]))
  ]
  
  // Check consistency
  let consistency_report = ConsistencyChecker::check_cross_service_consistency(
    consistency_checker,
    [
      ("auth", auth_service_data),
      ("gateway", api_gateway_data),
      ("user", user_service_data)
    ]
  )
  
  assert_true(consistency_report.overall_consistency)
  assert_eq(consistency_report.inconsistencies.length(), 0)
  
  // Test with inconsistent data
  let inconsistent_gateway_data = [
    TelemetryPoint::new("request_count", 110, Attributes::with_data([
      ("service", StringValue("gateway")),
      ("downstream", StringValue("auth")),
      ("endpoint", StringValue("/login"))
    ]))
  ]
  
  let inconsistency_report = ConsistencyChecker::check_cross_service_consistency(
    consistency_checker,
    [
      ("auth", auth_service_data),
      ("gateway", inconsistent_gateway_data),
      ("user", user_service_data)
    ]
  )
  
  assert_false(inconsistency_report.overall_consistency)
  assert_eq(inconsistency_report.inconsistencies.length(), 1)
  assert_eq(inconsistency_report.inconsistencies[0].type, "count_mismatch")
  assert_eq(inconsistency_report.inconsistencies[0].expected, 105)
  assert_eq(inconsistency_report.inconsistencies[0].actual, 110)
  
  // Test temporal consistency
  let time_series_data = [
    TimeSeriesPoint::new(1000L, 100.0),
    TimeSeriesPoint::new(2000L, 105.0),
    TimeSeriesPoint::new(3000L, 102.0),
    TimeSeriesPoint::new(4000L, 108.0),
    TimeSeriesPoint::new(5000L, 110.0)
  ]
  
  let temporal_consistency = ConsistencyChecker::check_temporal_consistency(
    consistency_checker,
    time_series_data,
    0.1 // 10% tolerance
  )
  
  assert_true(temporal_consistency.is_consistent)
}

// Test 6: High Concurrency Telemetry Safety
test "high concurrency telemetry safety" {
  let telemetry_pool = ConcurrentTelemetryPool::new(100) // 100 concurrent workers
  let safety_checker = ConcurrentSafetyChecker::new()
  
  // Create concurrent telemetry operations
  let operations = []
  for i in 0..=1000 {
    operations.push(ConcurrentOperation::new(
      "metric_" + (i % 10).to_string(),
      i.to_float(),
      Attributes::with_data([
        ("worker_id", StringValue(i.to_string())),
        ("operation_type", StringValue(if i % 3 == 0 { "write" } else { "read" }))
      ])
    ))
  }
  
  // Execute operations concurrently
  let results = ConcurrentTelemetryPool::execute_all(telemetry_pool, operations)
  
  // Verify all operations completed
  assert_eq(results.length(), 1001)
  assert_eq(results.count(fn(r) { r.status == "completed" }), 1001)
  
  // Check for race conditions
  let race_condition_report = ConcurrentSafetyChecker::detect_race_conditions(safety_checker, results)
  assert_eq(race_condition_report.race_conditions_detected, 0)
  
  // Verify data integrity
  let integrity_report = ConcurrentSafetyChecker::verify_data_integrity(safety_checker, results)
  assert_true(integrity_report.data_integrity_maintained)
  
  // Test lock contention
  let contention_report = ConcurrentSafetyChecker::analyze_lock_contention(safety_checker, results)
  assert_true(contention_report.average_contention_time < 10.0) // Less than 10ms average
  
  // Test thread safety of shared resources
  let shared_resource = SharedTelemetryResource::new()
  let concurrent_access_results = []
  
  for i in 0..=100 {
    let result = SharedTelemetryResource::concurrent_access(shared_resource, i)
    concurrent_access_results.push(result)
  }
  
  let thread_safety_report = ConcurrentSafetyChecker::verify_thread_safety(
    safety_checker,
    concurrent_access_results
  )
  
  assert_true(thread_safety_report.thread_safety_verified)
  assert_eq(thread_safety_report.data_corruption_instances, 0)
}

// Test 7: Dynamic Configuration Management
test "dynamic configuration management" {
  let config_manager = DynamicConfigManager::new()
  
  // Set initial configuration
  let initial_config = Config::new()
    .set("telemetry.sampling_rate", 0.1)
    .set("telemetry.batch_size", 100)
    .set("telemetry.flush_interval", 5000)
    .set("telemetry.enabled_metrics", ["cpu", "memory", "disk"])
  
  DynamicConfigManager::apply_config(config_manager, initial_config)
  
  // Verify initial configuration
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.sampling_rate"), 0.1)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.batch_size"), 100)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.flush_interval"), 5000)
  
  // Test dynamic configuration update
  let updated_config = Config::new()
    .set("telemetry.sampling_rate", 0.2)
    .set("telemetry.batch_size", 200)
    .set("telemetry.new_metric", "network")
  
  DynamicConfigManager::update_config(config_manager, updated_config)
  
  // Verify configuration updates
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.sampling_rate"), 0.2)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.batch_size"), 200)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.flush_interval"), 5000) // Unchanged
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.new_metric"), "network")
  
  // Test configuration validation
  let invalid_config = Config::new()
    .set("telemetry.sampling_rate", 1.5) // Invalid: > 1.0
    .set("telemetry.batch_size", -10) // Invalid: negative
  
  let validation_result = DynamicConfigManager::validate_config(config_manager, invalid_config)
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 2)
  
  // Test configuration rollback
  let config_history = DynamicConfigManager::get_history(config_manager)
  assert_eq(config_history.length(), 2)
  
  DynamicConfigManager::rollback(config_manager, 1) // Rollback to initial config
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.sampling_rate"), 0.1)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.batch_size"), 100)
  
  // Test configuration templates
  let production_template = ConfigTemplate::new("production")
    .set("telemetry.sampling_rate", 0.01)
    .set("telemetry.batch_size", 500)
    .set("telemetry.flush_interval", 10000)
  
  DynamicConfigManager::register_template(config_manager, production_template)
  DynamicConfigManager::apply_template(config_manager, "production")
  
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.sampling_rate"), 0.01)
  assert_eq(DynamicConfigManager::get(config_manager, "telemetry.batch_size"), 500)
}

// Test 8: Multi-Tenant Telemetry Isolation
test "multi-tenant telemetry isolation" {
  let tenant_manager = MultiTenantManager::new()
  
  // Create tenants
  let tenant1 = Tenant::new("tenant-1", "Acme Corp")
  let tenant2 = Tenant::new("tenant-2", "Global Inc")
  let tenant3 = Tenant::new("tenant-3", "Startup LLC")
  
  MultiTenantManager::register_tenant(tenant_manager, tenant1)
  MultiTenantManager::register_tenant(tenant_manager, tenant2)
  MultiTenantManager::register_tenant(tenant_manager, tenant3)
  
  // Create tenant-specific telemetry data
  let tenant1_data = [
    TelemetryPoint::new("api_requests", 1000, Attributes::with_data([
      ("tenant_id", StringValue("tenant-1")),
      ("endpoint", StringValue("/api/users"))
    ])),
    TelemetryPoint::new("api_requests", 500, Attributes::with_data([
      ("tenant_id", StringValue("tenant-1")),
      ("endpoint", StringValue("/api/orders"))
    ]))
  ]
  
  let tenant2_data = [
    TelemetryPoint::new("api_requests", 2000, Attributes::with_data([
      ("tenant_id", StringValue("tenant-2")),
      ("endpoint", StringValue("/api/users"))
    ])),
    TelemetryPoint::new("api_requests", 1500, Attributes::with_data([
      ("tenant_id", StringValue("tenant-2")),
      ("endpoint", StringValue("/api/products"))
    ]))
  ]
  
  // Store tenant data
  MultiTenantManager::store_telemetry(tenant_manager, "tenant-1", tenant1_data)
  MultiTenantManager::store_telemetry(tenant_manager, "tenant-2", tenant2_data)
  
  // Test data isolation
  let tenant1_retrieved = MultiTenantManager::get_telemetry(tenant_manager, "tenant-1")
  let tenant2_retrieved = MultiTenantManager::get_telemetry(tenant_manager, "tenant-2")
  let tenant3_retrieved = MultiTenantManager::get_telemetry(tenant_manager, "tenant-3")
  
  assert_eq(tenant1_retrieved.length(), 2)
  assert_eq(tenant2_retrieved.length(), 2)
  assert_eq(tenant3_retrieved.length(), 0)
  
  // Verify data isolation
  for data in tenant1_retrieved {
    match TelemetryPoint::attributes(data).get("tenant_id") {
      Some(StringValue(tenant_id)) => assert_eq(tenant_id, "tenant-1")
      _ => assert_true(false)
    }
  }
  
  for data in tenant2_retrieved {
    match TelemetryPoint::attributes(data).get("tenant_id") {
      Some(StringValue(tenant_id)) => assert_eq(tenant_id, "tenant-2")
      _ => assert_true(false)
    }
  }
  
  // Test cross-tenant query restrictions
  let cross_tenant_query = TelemetryQuery::new()
    .set_filter("tenant_id", ["tenant-1", "tenant-2"])
  
  let query_result = MultiTenantManager::execute_query(tenant_manager, "tenant-1", cross_tenant_query)
  assert_eq(query_result.data.length(), 2) // Only tenant-1 data should be returned
  
  // Test tenant resource quotas
  let quota_manager = TenantQuotaManager::new()
  TenantQuotaManager::set_quota(quota_manager, "tenant-1", 1000) // 1000 data points
  TenantQuotaManager::set_quota(quota_manager, "tenant-2", 500) // 500 data points
  
  // Test quota enforcement
  let large_dataset = []
  for i in 0..=600 {
    large_dataset.push(TelemetryPoint::new("test_metric", i.to_float(), Attributes::with_data([
      ("tenant_id", StringValue("tenant-2"))
    ])))
  }
  
  let quota_result = MultiTenantManager::store_telemetry_with_quota(tenant_manager, "tenant-2", large_dataset)
  assert_false(quota_result.success)
  assert_eq(quota_result.reason, "quota_exceeded")
}

// Test 9: Telemetry Data Lifecycle Management
test "telemetry data lifecycle management" {
  let lifecycle_manager = TelemetryLifecycleManager::new()
  
  // Configure lifecycle policies
  let hot_storage_policy = StoragePolicy::new("hot", 7) // 7 days
  let warm_storage_policy = StoragePolicy::new("warm", 30) // 30 days
  let cold_storage_policy = StoragePolicy::new("cold", 365) // 365 days
  let deletion_policy = StoragePolicy::new("delete", 0) // Delete after cold period
  
  LifecycleManager::add_policy(lifecycle_manager, hot_storage_policy)
  LifecycleManager::add_policy(lifecycle_manager, warm_storage_policy)
  LifecycleManager::add_policy(lifecycle_manager, cold_storage_policy)
  LifecycleManager::add_policy(lifecycle_manager, deletion_policy)
  
  // Create telemetry data with different timestamps
  let current_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  let old_data = []
  
  // Hot data (1 day old)
  old_data.push(TelemetryPoint::with_timestamp("cpu_usage", 50.0, Attributes::new(), current_time - 86400000L))
  
  // Warm data (10 days old)
  old_data.push(TelemetryPoint::with_timestamp("memory_usage", 60.0, Attributes::new(), current_time - 864000000L))
  
  // Cold data (100 days old)
  old_data.push(TelemetryPoint::with_timestamp("disk_usage", 70.0, Attributes::new(), current_time - 8640000000L))
  
  // Expired data (400 days old)
  old_data.push(TelemetryPoint::with_timestamp("network_usage", 80.0, Attributes::new(), current_time - 34560000000L))
  
  // Store data
  for data in old_data {
    LifecycleManager::store(lifecycle_manager, data)
  }
  
  // Run lifecycle management
  let lifecycle_report = LifecycleManager::process_lifecycle(lifecycle_manager, current_time)
  
  // Verify lifecycle transitions
  assert_eq(lifecycle_report.hot_to_warm, 1)
  assert_eq(lifecycle_report.warm_to_cold, 1)
  assert_eq(lifecycle_report.cold_to_deleted, 1)
  assert_eq(lifecycle_report.remaining_in_hot, 1)
  
  // Test data retrieval from different storage tiers
  let hot_data = LifecycleManager::retrieve_from_storage(lifecycle_manager, "hot")
  let warm_data = LifecycleManager::retrieve_from_storage(lifecycle_manager, "warm")
  let cold_data = LifecycleManager::retrieve_from_storage(lifecycle_manager, "cold")
  
  assert_eq(hot_data.length(), 1)
  assert_eq(warm_data.length(), 1)
  assert_eq(cold_data.length(), 1)
  
  // Test data compression during tier transitions
  let compression_stats = LifecycleManager::get_compression_stats(lifecycle_manager)
  assert_true(compression_stats.compression_ratio > 1.0) // Data should be compressed
  
  // Test data integrity across lifecycle transitions
  let integrity_check = LifecycleManager::verify_data_integrity(lifecycle_manager)
  assert_true(integrity_check.all_data_intact)
  
  // Test custom lifecycle rules
  let custom_rule = LifecycleRule::new("high_value_metrics")
    .set_condition(fn(data) { TelemetryPoint::metric_name(data) == "cpu_usage" })
    .set_action("extend_hot_storage", 14) // Keep in hot storage for 14 days
  
  LifecycleManager::add_custom_rule(lifecycle_manager, custom_rule)
  LifecycleManager::process_custom_rules(lifecycle_manager, current_time)
  
  // Verify custom rule application
  let cpu_usage_data = LifecycleManager::find_by_metric(lifecycle_manager, "cpu_usage")
  assert_eq(cpu_usage_data.storage_tier, "hot")
}

// Test 10: Edge Computing Telemetry Optimization
test "edge computing telemetry optimization" {
  let edge_optimizer = EdgeTelemetryOptimizer::new()
  
  // Configure edge constraints
  let edge_constraints = EdgeConstraints::new()
    .set_bandwidth_limit(1000) // 1000 KB/s
    .set_storage_limit(10000) // 10000 KB
    .set_processing_power(50) // 50% CPU usage
    .set_battery_power(true) // Battery powered device
  
  EdgeTelemetryOptimizer::set_constraints(edge_optimizer, edge_constraints)
  
  // Create high-frequency telemetry data
  let high_frequency_data = []
  for i in 0..=1000 {
    high_frequency_data.push(TelemetryPoint::new(
      "sensor_reading",
      (i % 100).to_float(),
      Attributes::with_data([
        ("sensor_id", StringValue("sensor-" + (i % 10).to_string())),
        ("location", StringValue("edge-node-1")),
        ("timestamp", IntValue(1640995200000L + i * 1000L))
      ])
    ))
  }
  
  // Test data sampling
  let sampling_strategy = EdgeTelemetryOptimizer::create_sampling_strategy(edge_optimizer, 0.1) // 10% sampling
  let sampled_data = EdgeTelemetryOptimizer::apply_sampling(edge_optimizer, high_frequency_data, sampling_strategy)
  
  assert_eq(sampled_data.length(), 100) // 10% of 1000
  
  // Test data compression
  let compressed_data = EdgeTelemetryOptimizer::compress_for_edge(edge_optimizer, sampled_data)
  let compression_ratio = compressed_data.length().to_float() / sampled_data.length().to_float()
  assert_true(compression_ratio < 0.5) // At least 50% compression
  
  // Test data batching
  let batched_data = EdgeTelemetryOptimizer::batch_for_transmission(edge_optimizer, compressed_data, 100) // 100 KB batches
  assert_true(batched_data.length() > 1) // Should create multiple batches
  
  // Test adaptive transmission scheduling
  let network_conditions = NetworkConditions::new()
    .set_bandwidth(500) // 500 KB/s (below our limit)
    .set_latency(100) // 100ms
    .set_packet_loss(0.01) // 1% packet loss
  
  let transmission_schedule = EdgeTelemetryOptimizer::schedule_transmission(
    edge_optimizer,
    batched_data,
    network_conditions
  )
  
  assert_true(transmission_schedule.total_duration > 0)
  assert_true(transmission_schedule.respects_bandwidth_constraints)
  
  // Test edge analytics
  let edge_analytics = EdgeAnalytics::new()
  
  // Perform analytics at edge
  let analytics_result = EdgeAnalytics::analyze(edge_analytics, sampled_data, [
    AnalyticsOperation::average("sensor_reading"),
    AnalyticsOperation::max("sensor_reading"),
    AnalyticsOperation::min("sensor_reading"),
    AnalyticsOperation::trend_detection("sensor_reading")
  ])
  
  assert_true(analytics_result.contains_key("average"))
  assert_true(analytics_result.contains_key("max"))
  assert_true(analytics_result.contains_key("min"))
  assert_true(analytics_result.contains_key("trend"))
  
  // Test anomaly detection at edge
  let anomaly_threshold = 80.0
  let edge_anomalies = EdgeAnalytics::detect_anomalies(edge_analytics, sampled_data, "sensor_reading", anomaly_threshold)
  
  assert_eq(edge_anomalies.length(), 20) // Values 80-99 should be flagged
  
  // Test edge-cloud synchronization
  let sync_manager = EdgeCloudSyncManager::new()
  
  // Sync only aggregated data to cloud
  let cloud_sync_data = EdgeCloudSyncManager::prepare_for_sync(sync_manager, analytics_result, edge_anomalies)
  assert_true(cloud_sync_data.size < compressed_data.size) // Should be smaller than raw data
  
  // Test offline data buffering
  let offline_buffer = OfflineBuffer::new(50000) // 50KB buffer
  OfflineBuffer::store(offline_buffer, compressed_data)
  
  // Simulate network outage
  EdgeTelemetryOptimizer::handle_network_outage(edge_optimizer, offline_buffer)
  
  // Verify data is buffered
  assert_true(OfflineBuffer::is_data_buffered(offline_buffer))
  assert_eq(OfflineBuffer::get_buffer_size(offline_buffer), compressed_data.size)
  
  // Test data recovery after reconnection
  let recovered_data = EdgeTelemetryOptimizer::recover_buffered_data(edge_optimizer, offline_buffer)
  assert_eq(recovered_data.length(), compressed_data.length)
}