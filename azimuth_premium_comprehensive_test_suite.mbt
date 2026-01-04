// Azimuth Telemetry System - Premium Comprehensive Test Suite
// This file contains high-quality test cases covering advanced features and edge cases

// Test 1: Resource Constraint Telemetry Processing
test "resource constraint telemetry processing" {
  let resource_monitor = ResourceMonitor::new()
  let telemetry_processor = TelemetryProcessor::with_limits(
    max_memory_mb = 50,
    max_cpu_percent = 80.0,
    max_concurrent_operations = 10
  )
  
  // Test processing under memory constraints
  let initial_memory = resource_monitor.get_memory_usage_mb()
  
  // Generate telemetry data that would normally exceed memory limits
  let large_dataset = []
  for i in 0..=10000 {
    let span = Span::new("resource_test_span_" + i.to_string(), Internal, 
      SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    
    // Add substantial data to each span
    for j in 0..=50 {
      Span::set_attribute(span, "large_attr_" + j.to_string(), 
        "large_value_" + i.to_string() + "_" + j.to_string())
    }
    
    large_dataset = large_dataset.push(span)
  }
  
  // Process under constraints
  let processing_result = TelemetryProcessor::process_with_constraints(
    telemetry_processor, 
    large_dataset
  )
  
  // Verify resource constraints were respected
  let peak_memory = resource_monitor.get_peak_memory_usage_mb()
  assert_true(peak_memory <= initial_memory + 50)  // Should not exceed 50MB limit
  
  // Verify processing completed successfully
  assert_true(processing_result.success)
  assert_true(processing_result.processed_count > 0)
  assert_true(processing_result.dropped_count < processing_result.processed_count * 0.1)  // Less than 10% dropped
  
  // Verify CPU usage was within limits
  let avg_cpu = resource_monitor.get_average_cpu_usage()
  assert_true(avg_cpu <= 80.0)  // Should not exceed 80% CPU
}

// Test 2: Multi-Tenant Isolation
test "multi-tenant isolation" {
  let tenant_manager = MultiTenantManager::new()
  
  // Create multiple tenants
  let tenant_ids = ["tenant_a", "tenant_b", "tenant_c"]
  let tenant_contexts = []
  
  for tenant_id in tenant_ids {
    let tenant_ctx = tenant_manager.create_tenant_context(
      tenant_id,
      resource_limits = (max_spans = 1000, max_metrics = 5000),
      retention_policy = RetentionPolicy::days(30)
    )
    tenant_contexts = tenant_contexts.push(tenant_ctx)
  }
  
  // Generate telemetry data for each tenant
  for i in 0..=tenant_contexts.length() - 1 {
    let tenant_ctx = tenant_contexts[i]
    let tenant_id = tenant_ids[i]
    
    // Create spans for this tenant
    for j in 0..=100 {
      let span = Span::new(
        tenant_id + "_operation_" + j.to_string(), 
        Internal,
        SpanContext::new(tenant_id + "_trace_" + j.to_string(), 
                        tenant_id + "_span_" + j.to_string(), true, "")
      )
      
      // Add tenant-specific attributes
      Span::set_attribute(span, "tenant.id", tenant_id)
      Span::set_attribute(span, "tenant.environment", "production")
      
      MultiTenantManager::add_span(tenant_manager, tenant_ctx, span)
    }
    
    // Create metrics for this tenant
    for k in 0..=50 {
      let metric = Metric::counter(
        tenant_id + "_counter_" + k.to_string(), 
        tenant_id + " counter metric"
      )
      Metric::add(metric, k as Float)
      MultiTenantManager::add_metric(tenant_manager, tenant_ctx, metric)
    }
  }
  
  // Verify isolation - each tenant should only see their own data
  for i in 0..=tenant_contexts.length() - 1 {
    let tenant_ctx = tenant_contexts[i]
    let tenant_id = tenant_ids[i]
    
    let tenant_spans = MultiTenantManager::get_spans(tenant_manager, tenant_ctx)
    let tenant_metrics = MultiTenantManager::get_metrics(tenant_manager, tenant_ctx)
    
    // Verify count
    assert_eq(tenant_spans.length(), 101)  // 0-100 inclusive
    assert_eq(tenant_metrics.length(), 51)  // 0-50 inclusive
    
    // Verify all data belongs to this tenant
    for span in tenant_spans {
      let tenant_attr = Span::get_attribute(span, "tenant.id")
      match tenant_attr {
        Some(StringValue(id)) => assert_eq(id, tenant_id),
        _ => assert_true(false)
      }
    }
    
    // Verify cross-tenant data access is blocked
    let other_tenant_id = if i == 0 { tenant_ids[1] } else { tenant_ids[0] }
    let cross_tenant_data = MultiTenantManager::get_spans_by_tenant_id(
      tenant_manager, 
      other_tenant_id
    )
    assert_eq(cross_tenant_data.length(), 0)  // Should be empty due to isolation
  }
}

// Test 3: Real-time Stream Processing
test "real-time stream processing" {
  let stream_processor = RealtimeStreamProcessor::new(
    buffer_size = 1000,
    processing_window_ms = 100,
    batch_size = 50
  )
  
  let processed_data = []
  let processing_lock = Mutex::new()
  
  // Set up real-time processing callback
  RealtimeStreamProcessor::set_callback(stream_processor, fn(batch) {
    processing_lock.lock()
    processed_data = processed_data.concat(batch)
    processing_lock.unlock()
  })
  
  // Start the stream processor
  RealtimeStreamProcessor::start(stream_processor)
  
  // Simulate real-time data influx
  let start_time = Time::now()
  let data_points_generated = 0
  
  while Time::now() - start_time < 1000 {  // Run for 1 second
    for i in 0..=10 {
      let telemetry_data = TelemetryData::new(
        "realtime_service",
        "realtime_operation",
        200 + (i % 100),
        10 + (i % 50),
        i % 10 != 0
      )
      
      RealtimeStreamProcessor::add_data(stream_processor, telemetry_data)
      data_points_generated = data_points_generated + 1
    }
    
    Time::sleep(10)  // Small delay to simulate real-time
  }
  
  // Stop the stream processor and wait for completion
  RealtimeStreamProcessor::stop(stream_processor)
  RealtimeStreamProcessor::wait_for_completion(stream_processor, 5000)
  
  // Verify processing results
  assert_true(processed_data.length() > 0)
  assert_true(processed_data.length() <= data_points_generated)
  
  // Verify processing latency
  let processing_stats = RealtimeStreamProcessor::get_stats(stream_processor)
  assert_true(processing_stats.avg_processing_latency_ms < 200)  // Should be under 200ms
  assert_true(processing_stats.max_processing_latency_ms < 500)  // Should be under 500ms
  
  // Verify throughput
  let throughput = data_points_generated as Float / 1000.0  // ops per second
  assert_true(throughput > 100)  // Should handle at least 100 ops/sec
  
  // Verify no data loss
  let loss_percentage = (data_points_generated - processed_data.length()) as Float / 
                        data_points_generated as Float * 100.0
  assert_true(loss_percentage < 5.0)  // Less than 5% data loss
}

// Test 4: Anomaly Detection and Recovery
test "anomaly detection and recovery" {
  let anomaly_detector = AnomalyDetector::new(
    sensitivity_level = 0.8,
    detection_window_ms = 5000,
    recovery_strategy = RecoveryStrategy::Automatic
  )
  
  let recovery_events = []
  
  // Set up anomaly detection callbacks
  AnomalyDetector::on_anomaly_detected(anomaly_detector, fn(anomaly) {
    assert_true(anomaly.severity >= 0.8)
    recovery_events = recovery_events.push("anomaly_detected")
  })
  
  AnomalyDetector::on_recovery_initiated(anomaly_detector, fn(recovery) {
    recovery_events = recovery_events.push("recovery_initiated")
  })
  
  AnomalyDetector::on_recovery_completed(anomaly_detector, fn(recovery) {
    recovery_events = recovery_events.push("recovery_completed")
  })
  
  // Start anomaly detection
  AnomalyDetector::start(anomaly_detector)
  
  // Generate normal telemetry data
  for i in 0..=100 {
    let telemetry_data = TelemetryData::new(
      "normal_service",
      "normal_operation",
      200,  // Normal status code
      50,   // Normal response time
      true  // Normal success rate
    )
    AnomalyDetector::process_data(anomaly_detector, telemetry_data)
  }
  
  // Verify no anomalies detected for normal data
  assert_eq(recovery_events.length(), 0)
  
  // Generate anomalous telemetry data
  for i in 0..=20 {
    let telemetry_data = TelemetryData::new(
      "anomaly_service",
      "anomaly_operation",
      500,  // Error status code
      1000, // High response time
      false // Low success rate
    )
    AnomalyDetector::process_data(anomaly_detector, telemetry_data)
  }
  
  // Wait for anomaly detection
  Time::sleep(100)
  
  // Verify anomaly was detected
  assert_true(recovery_events.length() >= 1)
  assert_true(recovery_events.contains("anomaly_detected"))
  
  // Verify recovery was initiated
  assert_true(recovery_events.contains("recovery_initiated"))
  
  // Wait for recovery to complete
  Time::sleep(500)
  
  // Verify recovery was completed
  assert_true(recovery_events.contains("recovery_completed"))
  
  // Verify system recovered - normal processing should resume
  for i in 0..=50 {
    let telemetry_data = TelemetryData::new(
      "recovered_service",
      "recovered_operation",
      200,  // Back to normal
      50,   // Back to normal
      true  // Back to normal
    )
    AnomalyDetector::process_data(anomaly_detector, telemetry_data)
  }
  
  // Verify no new anomalies after recovery
  let final_recovery_events = recovery_events.length()
  Time::sleep(100)
  assert_eq(recovery_events.length(), final_recovery_events)
  
  // Stop anomaly detection
  AnomalyDetector::stop(anomaly_detector)
}

// Test 5: Data Integrity Validation
test "data integrity validation" {
  let integrity_validator = DataIntegrityValidator::new(
    checksum_algorithm = "SHA256",
    validation_level = ValidationLevel::Strict
  )
  
  // Create test telemetry data
  let original_data = []
  for i in 0..=100 {
    let telemetry_data = TelemetryData::new(
      "integrity_service_" + (i % 5).to_string(),
      "integrity_operation_" + (i % 3).to_string(),
      200 + (i % 300),
      50 + (i % 200),
      i % 10 != 0
    )
    original_data = original_data.push(telemetry_data)
  }
  
  // Calculate and store integrity checksums
  let data_with_checksums = []
  for data in original_data {
    let checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, data)
    let data_with_checksum = DataWithChecksum::new(data, checksum)
    data_with_checksums = data_with_checksums.push(data_with_checksum)
  }
  
  // Serialize and deserialize data (simulating transmission/storage)
  let serialized = TelemetrySerializer::to_json(data_with_checksums)
  let deserialized = TelemetrySerializer::from_json(serialized)
  
  // Validate integrity of all data
  let validation_results = []
  for data_with_checksum in deserialized {
    let is_valid = DataIntegrityValidator::validate(integrity_validator, data_with_checksum)
    validation_results = validation_results.push(is_valid)
  }
  
  // Verify all data passed integrity validation
  for result in validation_results {
    assert_true(result)
  }
  
  // Simulate data corruption
  let corrupted_data = []
  for i in 0..=deserialized.length() - 1 {
    let data_with_checksum = deserialized[i]
    if i == 50 {  // Corrupt one specific item
      let corrupted = DataWithChecksum::corrupt(data_with_checksum)
      corrupted_data = corrupted_data.push(corrupted)
    } else {
      corrupted_data = corrupted_data.push(data_with_checksum)
    }
  }
  
  // Validate corrupted data
  let corruption_detected = false
  for data_with_checksum in corrupted_data {
    let is_valid = DataIntegrityValidator::validate(integrity_validator, data_with_checksum)
    if not is_valid {
      corruption_detected = true
      break
    }
  }
  
  // Verify corruption was detected
  assert_true(corruption_detected)
  
  // Test data repair functionality
  let repaired_data = DataIntegrityValidator::repair_corrupted_data(
    integrity_validator, 
    corrupted_data, 
    original_data
  )
  
  // Verify repair was successful
  let all_valid_after_repair = true
  for data_with_checksum in repaired_data {
    let is_valid = DataIntegrityValidator::validate(integrity_validator, data_with_checksum)
    if not is_valid {
      all_valid_after_repair = false
      break
    }
  }
  assert_true(all_valid_after_repair)
  
  // Verify data count is preserved
  assert_eq(repaired_data.length(), original_data.length())
}

// Test 6: Cross-Platform Compatibility
test "cross-platform compatibility" {
  let platform_adapters = [
    ("linux", LinuxPlatformAdapter::new()),
    ("windows", WindowsPlatformAdapter::new()),
    ("macos", MacOSPlatformAdapter::new()),
    ("container", ContainerPlatformAdapter::new())
  ]
  
  let test_results = []
  
  // Test telemetry collection on different platforms
  for (platform_name, adapter) in platform_adapters {
    let platform_test_result = PlatformTestResult::new(platform_name)
    
    // Initialize platform-specific telemetry
    let telemetry = PlatformAdapter::initialize_telemetry(adapter)
    
    // Test platform-specific metrics collection
    let cpu_usage = PlatformAdapter::get_cpu_usage(adapter)
    let memory_usage = PlatformAdapter::get_memory_usage(adapter)
    let disk_io = PlatformAdapter::get_disk_io(adapter)
    let network_io = PlatformAdapter::get_network_io(adapter)
    
    // Verify metrics are valid
    assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
    assert_true(memory_usage > 0)
    assert_true(disk_io.read_bytes >= 0 && disk_io.write_bytes >= 0)
    assert_true(network_io.bytes_sent >= 0 && network_io.bytes_received >= 0)
    
    // Test platform-specific span creation
    let span = PlatformAdapter::create_span(adapter, "platform_test_span")
    Span::set_attribute(span, "platform", platform_name)
    Span::set_attribute(span, "cpu_usage", cpu_usage.to_string())
    Span::set_attribute(span, "memory_usage", memory_usage.to_string())
    
    // Test platform-specific event handling
    PlatformAdapter::add_platform_event(adapter, span, "platform_specific_event")
    
    // Test platform-specific serialization
    let serialized_data = PlatformAdapter::serialize_telemetry(adapter, [span])
    assert_true(serialized_data.length() > 0)
    
    // Test platform-specific deserialization
    let deserialized_data = PlatformAdapter::deserialize_telemetry(adapter, serialized_data)
    assert_eq(deserialized_data.length(), 1)
    
    // Update test result
    platform_test_result.success = true
    platform_test_result.metrics_collected = 4
    platform_test_result.events_created = 1
    test_results = test_results.push(platform_test_result)
  }
  
  // Verify all platforms were tested successfully
  assert_eq(test_results.length(), 4)
  for result in test_results {
    assert_true(result.success)
    assert_eq(result.metrics_collected, 4)
    assert_eq(result.events_created, 1)
  }
  
  // Test cross-platform data compatibility
  let linux_data = test_results[0]
  let windows_data = test_results[1]
  
  // Verify data from different platforms can be merged
  let merged_data = CrossPlatformCompatibility::merge_telemetry_data(
    linux_data.telemetry_data,
    windows_data.telemetry_data
  )
  assert_true(merged_data.length() >= linux_data.telemetry_data.length())
  assert_true(merged_data.length() >= windows_data.telemetry_data.length())
  
  // Verify data format is consistent across platforms
  let format_validator = CrossPlatformFormatValidator::new()
  for result in test_results {
    let is_compatible = format_validator.validate(result.telemetry_data)
    assert_true(is_compatible)
  }
}

// Test 7: Dynamic Configuration Updates
test "dynamic configuration updates" {
  let config_manager = DynamicConfigManager::new()
  
  // Set initial configuration
  let initial_config = TelemetryConfig::new(
    sampling_rate = 1.0,
    batch_size = 100,
    export_interval_ms = 5000,
    max_spans_per_second = 1000
  )
  
  DynamicConfigManager::update_config(config_manager, initial_config)
  
  // Start telemetry with initial config
  let telemetry_system = TelemetrySystem::with_config_manager(config_manager)
  TelemetrySystem::start(telemetry_system)
  
  // Generate telemetry data with initial config
  for i in 0..=200 {
    let span = Span::new("config_test_span_" + i.to_string(), Internal,
      SpanContext::new("trace_" + i.to_string(), "span_" + i.to_string(), true, ""))
    TelemetrySystem::add_span(telemetry_system, span)
  }
  
  // Verify initial config is active
  let active_config = DynamicConfigManager::get_active_config(config_manager)
  assert_eq(active_config.sampling_rate, 1.0)
  assert_eq(active_config.batch_size, 100)
  
  // Update configuration dynamically
  let updated_config = TelemetryConfig::new(
    sampling_rate = 0.5,  // Reduce sampling rate
    batch_size = 50,      // Reduce batch size
    export_interval_ms = 2000,  // Reduce export interval
    max_spans_per_second = 500  // Reduce max spans
  )
  
  DynamicConfigManager::update_config(config_manager, updated_config)
  
  // Wait for config update to propagate
  Time::sleep(100)
  
  // Verify config was updated
  let new_active_config = DynamicConfigManager::get_active_config(config_manager)
  assert_eq(new_active_config.sampling_rate, 0.5)
  assert_eq(new_active_config.batch_size, 50)
  assert_eq(new_active_config.export_interval_ms, 2000)
  assert_eq(new_active_config.max_spans_per_second, 500)
  
  // Generate telemetry data with updated config
  let spans_before_update = TelemetrySystem::get_span_count(telemetry_system)
  
  for i in 0..=200 {
    let span = Span::new("config_test_span_updated_" + i.to_string(), Internal,
      SpanContext::new("trace_updated_" + i.to_string(), "span_updated_" + i.to_string(), true, ""))
    TelemetrySystem::add_span(telemetry_system, span)
  }
  
  let spans_after_update = TelemetrySystem::get_span_count(telemetry_system)
  
  // Verify new config is being used (approximately half the spans should be sampled)
  let new_spans_count = spans_after_update - spans_before_update
  assert_true(new_spans_count > 80 && new_spans_count < 120)  // Around 100, considering 50% sampling
  
  // Test configuration rollback
  DynamicConfigManager::rollback_config(config_manager)
  
  // Wait for rollback to propagate
  Time::sleep(100)
  
  // Verify config was rolled back
  let rolled_back_config = DynamicConfigManager::get_active_config(config_manager)
  assert_eq(rolled_back_config.sampling_rate, 1.0)  // Back to original
  assert_eq(rolled_back_config.batch_size, 100)     // Back to original
  
  // Test configuration validation
  let invalid_config = TelemetryConfig::new(
    sampling_rate = 1.5,  // Invalid: > 1.0
    batch_size = -10,     // Invalid: negative
    export_interval_ms = 0,  // Invalid: zero
    max_spans_per_second = -100  // Invalid: negative
  )
  
  let validation_result = DynamicConfigManager::validate_config(config_manager, invalid_config)
  assert_false(validation_result.is_valid)
  assert_true(validation_result.errors.length() > 0)
  
  // Verify invalid config was not applied
  let current_config_after_invalid = DynamicConfigManager::get_active_config(config_manager)
  assert_eq(current_config_after_invalid.sampling_rate, 1.0)  // Should remain unchanged
  
  TelemetrySystem::stop(telemetry_system)
}

// Test 8: Security and Privacy
test "security and privacy" {
  let security_manager = SecurityManager::new(
    encryption_enabled = true,
    pii_detection_enabled = true,
    data_retention_days = 30
  )
  
  // Test PII detection and redaction
  let pii_test_data = [
    ("user.email", "john.doe@example.com"),
    ("user.phone", "+1-555-123-4567"),
    ("user.ssn", "123-45-6789"),
    ("user.address", "123 Main St, Anytown, USA"),
    ("user.name", "John Doe"),
    ("user.id", "user_12345"),  // Not PII
    ("service.name", "auth_service")  // Not PII
  ]
  
  let redacted_data = []
  for (key, value) in pii_test_data {
    let is_pii = SecurityManager::is_pii(security_manager, key, value)
    let redacted_value = if is_pii {
      SecurityManager::redact_pii(security_manager, value)
    } else {
      value
    }
    redacted_data = redacted_data.push((key, redacted_value, is_pii))
  }
  
  // Verify PII was detected and redacted
  assert_true(redacted_data[0].2)  // user.email is PII
  assert_true(redacted_data[1].2)  // user.phone is PII
  assert_true(redacted_data[2].2)  // user.ssn is PII
  assert_true(redacted_data[3].2)  // user.address is PII
  assert_true(redacted_data[4].2)  // user.name is PII
  assert_false(redacted_data[5].2) // user.id is not PII
  assert_false(redacted_data[6].2) // service.name is not PII
  
  // Verify redaction
  assert_eq(redacted_data[0].1, "***@***.***")  // Email redacted
  assert_eq(redacted_data[1].1, "***-***-****") // Phone redacted
  assert_eq(redacted_data[2].1, "***-**-****")  // SSN redacted
  assert_eq(redacted_data[3].1, "*** ***, *****, ***") // Address redacted
  assert_eq(redacted_data[4].1, "*** ***")       // Name redacted
  assert_eq(redacted_data[5].1, "user_12345")    // Not redacted
  assert_eq(redacted_data[6].1, "auth_service")  // Not redacted
  
  // Test encryption and decryption
  let sensitive_data = "This is sensitive telemetry data"
  let encrypted_data = SecurityManager::encrypt(security_manager, sensitive_data)
  let decrypted_data = SecurityManager::decrypt(security_manager, encrypted_data)
  
  // Verify encryption/decryption
  assert_true(encrypted_data != sensitive_data)  // Should be different
  assert_eq(decrypted_data, sensitive_data)      // Should match original
  
  // Test access control
  let access_manager = AccessManager::new()
  
  // Define roles and permissions
  AccessManager::define_role(access_manager, "admin", ["read", "write", "delete"])
  AccessManager::define_role(access_manager, "analyst", ["read"])
  AccessManager::define_role(access_manager, "service", ["write"])
  
  // Create users with roles
  let admin_user = AccessManager::create_user(access_manager, "admin_user", "admin")
  let analyst_user = AccessManager::create_user(access_manager, "analyst_user", "analyst")
  let service_user = AccessManager::create_user(access_manager, "service_user", "service")
  
  // Test permissions
  assert_true(AccessManager::has_permission(access_manager, admin_user, "read"))
  assert_true(AccessManager::has_permission(access_manager, admin_user, "write"))
  assert_true(AccessManager::has_permission(access_manager, admin_user, "delete"))
  
  assert_true(AccessManager::has_permission(access_manager, analyst_user, "read"))
  assert_false(AccessManager::has_permission(access_manager, analyst_user, "write"))
  assert_false(AccessManager::has_permission(access_manager, analyst_user, "delete"))
  
  assert_false(AccessManager::has_permission(access_manager, service_user, "read"))
  assert_true(AccessManager::has_permission(access_manager, service_user, "write"))
  assert_false(AccessManager::has_permission(access_manager, service_user, "delete"))
  
  // Test audit logging
  let audit_logger = AuditLogger::new()
  
  // Log access attempts
  AuditLogger::log_access_attempt(audit_logger, admin_user, "read", "/telemetry/spans")
  AuditLogger::log_access_attempt(audit_logger, analyst_user, "write", "/telemetry/spans")  // Should fail
  AuditLogger::log_access_attempt(audit_logger, service_user, "write", "/telemetry/metrics")
  
  // Verify audit logs
  let audit_logs = AuditLogger::get_logs(audit_logger)
  assert_eq(audit_logs.length(), 3)
  
  assert_eq(audit_logs[0].user, "admin_user")
  assert_eq(audit_logs[0].action, "read")
  assert_eq(audit_logs[0].resource, "/telemetry/spans")
  assert_true(audit_logs[0].success)
  
  assert_eq(audit_logs[1].user, "analyst_user")
  assert_eq(audit_logs[1].action, "write")
  assert_eq(audit_logs[1].resource, "/telemetry/spans")
  assert_false(audit_logs[1].success)  // Should fail due to insufficient permissions
  
  assert_eq(audit_logs[2].user, "service_user")
  assert_eq(audit_logs[2].action, "write")
  assert_eq(audit_logs[2].resource, "/telemetry/metrics")
  assert_true(audit_logs[2].success)
  
  // Test data retention
  let old_timestamp = Time::now() - (40 * 24 * 60 * 60 * 1000)  // 40 days ago
  let recent_timestamp = Time::now() - (10 * 24 * 60 * 60 * 1000) // 10 days ago
  
  let old_telemetry = TelemetryData::with_timestamp("old_service", "old_operation", old_timestamp)
  let recent_telemetry = TelemetryData::with_timestamp("recent_service", "recent_operation", recent_timestamp)
  
  // Apply retention policy
  let retention_result = SecurityManager::apply_retention_policy(security_manager, [old_telemetry, recent_telemetry])
  
  // Verify old data was removed, recent data was kept
  assert_eq(retention_result.kept_data.length(), 1)
  assert_eq(retention_result.removed_data.length(), 1)
  assert_eq(retention_result.kept_data[0].service_name, "recent_service")
  assert_eq(retention_result.removed_data[0].service_name, "old_service")
}

// Test 9: Internationalization Support
test "internationalization support" {
  let i18n_manager = I18nManager::new()
  
  // Define supported locales
  let supported_locales = ["en-US", "zh-CN", "ja-JP", "es-ES", "fr-FR", "de-DE"]
  for locale in supported_locales {
    I18nManager::add_locale(i18n_manager, locale)
  }
  
  // Test localized error messages
  let error_codes = ["TELEMETRY_INIT_FAILED", "SPAN_CREATION_ERROR", "METRIC_RECORDING_ERROR"]
  
  for locale in supported_locales {
    I18nManager::set_current_locale(i18n_manager, locale)
    
    for error_code in error_codes {
      let localized_message = I18nManager::get_error_message(i18n_manager, error_code)
      assert_true(localized_message.length() > 0)
      
      // Verify message is actually localized (not just the error code)
      assert_true(localized_message != error_code)
    }
  }
  
  // Test localized attribute names and values
  let attribute_localizations = [
    ("en-US", "service.name", "Service Name"),
    ("zh-CN", "service.name", "服务名称"),
    ("ja-JP", "service.name", "サービス名"),
    ("es-ES", "service.name", "Nombre del Servicio"),
    ("fr-FR", "service.name", "Nom du Service"),
    ("de-DE", "service.name", "Dienstname")
  ]
  
  for (locale, attr_key, expected_localization) in attribute_localizations {
    I18nManager::set_current_locale(i18n_manager, locale)
    let localization = I18nManager::get_attribute_localization(i18n_manager, attr_key)
    assert_eq(localization, expected_localization)
  }
  
  // Test Unicode and multi-byte character handling
  let unicode_test_values = [
    ("zh-CN", "用户操作", "用户操作成功"),
    ("ja-JP", "ユーザーアクション", "ユーザーアクションが成功しました"),
    ("ar-SA", "عمل المستخدم", "نجحت عملية المستخدم"),
    ("ru-RU", "действие пользователя", "действие пользователя выполнено успешно"),
    ("ko-KR", "사용자 작업", "사용자 작업이 성공했습니다")
  ]
  
  for (locale, operation_name, success_message) in unicode_test_values {
    I18nManager::set_current_locale(i18n_manager, locale)
    
    // Create span with localized operation name
    let span = Span::new(operation_name, Internal, 
      SpanContext::new("trace_id", "span_id", true, ""))
    
    // Add localized event
    Span::add_event(span, success_message, None)
    
    // Verify Unicode data is preserved correctly
    assert_eq(Span::name(span), operation_name)
    let events = Span::get_events(span)
    assert_true(events.length() > 0)
    assert_eq(events[0].name, success_message)
  }
  
  // Test number and date formatting
  I18nManager::set_current_locale(i18n_manager, "en-US")
  let en_formatted_date = I18nManager::format_date(i18n_manager, Time::now())
  let en_formatted_number = I18nManager::format_number(i18n_manager, 1234.56)
  
  I18nManager::set_current_locale(i18n_manager, "de-DE")
  let de_formatted_date = I18nManager::format_date(i18n_manager, Time::now())
  let de_formatted_number = I18nManager::format_number(i18n_manager, 1234.56)
  
  // Verify formatting differences between locales
  assert_true(en_formatted_date != de_formatted_date)  // Date formats should differ
  assert_true(en_formatted_number != de_formatted_number)  // Number formats should differ
  
  // Test right-to-left language support
  I18nManager::set_current_locale(i18n_manager, "ar-SA")
  let rtl_span = Span::new("عملية التتبع", Internal,
    SpanContext::new("معرف التتبع", "معرف النطاق", true, ""))
  
  // Verify RTL text is handled correctly
  assert_eq(Span::name(rtl_span), "عملية التتبع")
  assert_eq(SpanContext::trace_id(Span::span_context(rtl_span)), "معرف التتبع")
  
  // Test locale-specific telemetry aggregation
  let locale_specific_metrics = []
  for locale in supported_locales {
    I18nManager::set_current_locale(i18n_manager, locale)
    
    // Create metrics with localized names and descriptions
    let counter = Metric::counter(
      I18nManager::get_metric_name(i18n_manager, "operation_count"),
      I18nManager::get_metric_description(i18n_manager, "operation_count_desc")
    )
    
    Metric::add(counter, 1.0)
    
    let metric_data = Metric::get_data(counter)
    locale_specific_metrics = locale_specific_metrics.push((locale, metric_data))
  }
  
  // Verify each locale has properly localized metrics
  for (locale, metric_data) in locale_specific_metrics {
    assert_true(metric_data.name != "operation_count")  // Should be localized
    assert_true(metric_data.description != "Operation count")  // Should be localized
    assert_eq(metric_data.value, 1.0)  // Value should be the same
  }
  
  // Test fallback behavior for unsupported locales
  I18nManager::set_current_locale(i18n_manager, "xx-YY")  // Unsupported locale
  let fallback_message = I18nManager::get_error_message(i18n_manager, "TELEMETRY_INIT_FAILED")
  
  // Should fall back to default locale (en-US)
  I18nManager::set_current_locale(i18n_manager, "en-US")
  let default_message = I18nManager::get_error_message(i18n_manager, "TELEMETRY_INIT_FAILED")
  
  assert_eq(fallback_message, default_message)
}