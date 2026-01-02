// Azimuth Enhanced Security and Privacy Test Suite
// 增强安全性和隐私测试套件

// Test 1: 数据加密和隐私保护
test "data encryption and privacy protection" {
  let encryption_service = @azimuth.EncryptionService::new()
  
  // 配置加密算法
  let encryption_config = @azimuth.EncryptionConfig::new()
  @azimuth.EncryptionConfig::set_algorithm(encryption_config, @azimuth.EncryptionAlgorithm::AES256_GCM)
  @azimuth.EncryptionConfig::set_key_derivation(encryption_config, @azimuth.KeyDerivation::PBKDF2)
  @azimuth.EncryptionConfig::set_key_iterations(encryption_config, 100000)
  
  @azimuth.EncryptionService::configure(encryption_service, encryption_config)
  
  // 创建敏感遥测数据
  let sensitive_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("user.id", @azimuth.StringValue("user-12345")),
      ("email", @azimuth.StringValue("user@example.com")),
      ("payment.token", @azimuth.StringValue("tok_1A2B3C4D5E6F7G8H")),
      ("ip.address", @azimuth.StringValue("192.168.1.100")),
      ("session.id", @azimuth.StringValue("sess-abcdef123456")),
      ("personal.data", @azimuth.StringValue("{\"name\":\"John Doe\",\"age\":35}"))
    ]
  )
  
  // 配置隐私策略
  let privacy_policy = @azimuth.PrivacyPolicy::new()
  @azimuth.PrivacyPolicy::add_field_rule(privacy_policy, "user.id", @azimuth.PrivacyRule::Hash)
  @azimuth.PrivacyPolicy::add_field_rule(privacy_policy, "email", @azimuth.PrivacyRule::Mask)
  @azimuth.PrivacyPolicy::add_field_rule(privacy_policy, "payment.token", @azimuth.PrivacyRule::Redact)
  @azimuth.PrivacyPolicy::add_field_rule(privacy_policy, "ip.address", @azimuth.PrivacyRule::Anonymize)
  @azimuth.PrivacyPolicy::add_field_rule(privacy_policy, "personal.data", @azimuth.PrivacyRule::Encrypt)
  
  // 应用隐私保护
  let protected_data = @azimuth.PrivacyPolicy::apply(privacy_policy, sensitive_data)
  
  // 验证隐私保护效果
  let user_id_attr = @azimuth.TelemetryData::get_attribute(protected_data, "user.id")
  match user_id_attr {
    Some(@azimuth.StringValue(value)) => {
      assert_false(value == "user-12345") // 应该被哈希
      assert_eq(value.length(), 64) // SHA256哈希长度
    }
    _ => assert_true(false)
  }
  
  let email_attr = @azimuth.TelemetryData::get_attribute(protected_data, "email")
  match email_attr {
    Some(@azimuth.StringValue(value)) => {
      assert_false(value == "user@example.com") // 应该被掩码
      assert_true(value.contains("u***@e***.com"))
    }
    _ => assert_true(false)
  }
  
  let payment_attr = @azimuth.TelemetryData::get_attribute(protected_data, "payment.token")
  match payment_attr {
    Some(@azimuth.StringValue(value)) => {
      assert_eq(value, "[REDACTED]") // 应该被完全删除
    }
    _ => assert_true(false)
  }
  
  let ip_attr = @azimuth.TelemetryData::get_attribute(protected_data, "ip.address")
  match ip_attr {
    Some(@azimuth.StringValue(value)) => {
      assert_false(value == "192.168.1.100") // 应该被匿名化
      assert_true(value.contains("192.168."))
      assert_false(value.contains("1.100"))
    }
    _ => assert_true(false)
  }
  
  // 测试端到端加密
  let encrypted_payload = @azimuth.EncryptionService::encrypt_payload(encryption_service, protected_data)
  let decrypted_payload = @azimuth.EncryptionService::decrypt_payload(encryption_service, encrypted_payload)
  
  // 验证加密/解密循环
  assert_eq(@azimuth.TelemetryData::attributes_count(decrypted_payload), @azimuth.TelemetryData::attributes_count(protected_data))
  
  // 验证加密数据不可读
  assert_false(encrypted_payload.contains("user-12345"))
  assert_false(encrypted_payload.contains("user@example.com"))
}

// Test 2: 多租户隔离和访问控制
test "multi-tenant isolation and access control" {
  let tenant_manager = @azimuth.TenantManager::new()
  
  // 创建租户
  let tenant_a = @azimuth.Tenant::new("tenant-a", "Company A", @azimuth.TenantTier::Enterprise)
  let tenant_b = @azimuth.Tenant::new("tenant-b", "Company B", @azimuth.TenantTier::Standard)
  let tenant_c = @azimuth.Tenant::new("tenant-c", "Company C", @azimuth.TenantTier::Basic)
  
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant_a)
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant_b)
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant_c)
  
  // 配置租户资源配额
  @azimuth.TenantManager::set_quota(tenant_manager, "tenant-a", @azimuth.ResourceQuota {
    max_spans_per_second: 10000,
    max_metrics_per_second: 50000,
    max_storage_gb: 100,
    max_data_retention_days: 365
  })
  
  @azimuth.TenantManager::set_quota(tenant_manager, "tenant-b", @azimuth.ResourceQuota {
    max_spans_per_second: 1000,
    max_metrics_per_second: 5000,
    max_storage_gb: 10,
    max_data_retention_days: 90
  })
  
  @azimuth.TenantManager::set_quota(tenant_manager, "tenant-c", @azimuth.ResourceQuota {
    max_spans_per_second: 100,
    max_metrics_per_second: 500,
    max_storage_gb: 1,
    max_data_retention_days: 30
  })
  
  // 创建租户特定的遥测数据
  let tenant_a_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("tenant.id", @azimuth.StringValue("tenant-a")),
      ("service.name", @azimuth.StringValue("payment-service")),
      ("transaction.amount", @azimuth.FloatValue(125.50)),
      ("customer.id", @azimuth.StringValue("cust-a-12345"))
    ]
  )
  
  let tenant_b_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("tenant.id", @azimuth.StringValue("tenant-b")),
      ("service.name", @azimuth.StringValue("user-service")),
      ("user.action", @azimuth.StringValue("login")),
      ("user.id", @azimuth.StringValue("user-b-67890"))
    ]
  )
  
  // 测试数据隔离
  @azimuth.TenantManager::store_telemetry(tenant_manager, tenant_a_data)
  @azimuth.TenantManager::store_telemetry(tenant_manager, tenant_b_data)
  
  // 验证租户A只能访问自己的数据
  let tenant_a_query = @azimuth.TenantManager::create_query(tenant_manager, "tenant-a")
  let tenant_a_results = @azimuth.TenantManager::execute_query(tenant_manager, tenant_a_query)
  
  assert_eq(tenant_a_results.length(), 1)
  assert_eq(@azimuth.TelemetryData::get_attribute(tenant_a_results[0], "tenant.id"), Some(@azimuth.StringValue("tenant-a")))
  
  // 验证租户B只能访问自己的数据
  let tenant_b_query = @azimuth.TenantManager::create_query(tenant_manager, "tenant-b")
  let tenant_b_results = @azimuth.TenantManager::execute_query(tenant_manager, tenant_b_query)
  
  assert_eq(tenant_b_results.length(), 1)
  assert_eq(@azimuth.TelemetryData::get_attribute(tenant_b_results[0], "tenant.id"), Some(@azimuth.StringValue("tenant-b")))
  
  // 测试跨租户访问控制
  let cross_tenant_access = @azimuth.TenantManager::attempt_cross_tenant_access(tenant_manager, "tenant-a", "tenant-b")
  assert_false(cross_tenant_access.allowed)
  assert_eq(cross_tenant_access.reason, "Access denied: cross-tenant access not permitted")
  
  // 测试资源配额限制
  let quota_check_a = @azimuth.TenantManager::check_quota(tenant_manager, "tenant-a", @azimuth.ResourceType::Spans, 5000)
  assert_true(quota_check_a.within_limit)
  
  let quota_check_c = @azimuth.TenantManager::check_quota(tenant_manager, "tenant-c", @azimuth.ResourceType::Spans, 150)
  assert_false(quota_check_c.within_limit)
  assert_eq(quota_check_c.violation_type, @azimuth.QuotaViolation::SpanRateExceeded)
}

// Test 3: 实时流处理和复杂事件处理
test "real-time stream processing and complex event processing" {
  let stream_processor = @azimuth.StreamProcessor::new()
  
  // 配置流处理
  let stream_config = @azimuth.StreamConfig::new()
  @azimuth.StreamConfig::set_buffer_size(stream_config, 10000)
  @azimuth.StreamConfig::set_processing_window(stream_config, @azimuth.TimeWindow::Seconds(5))
  @azimuth.StreamConfig::set_watermark_strategy(stream_config, @azimuth.WatermarkStrategy::BoundedOutOfOrderness(1000))
  @azimuth.StreamConfig::enable_state_management(stream_config, true)
  
  @azimuth.StreamProcessor::configure(stream_processor, stream_config)
  
  // 创建数据流
  let telemetry_stream = @azimuth.TelemetryStream::new("sensor-data-stream")
  
  // 生成实时传感器数据
  let mut event_count = 0
  for device_id in 1..=10 {
    for reading in 1..=100 {
      let timestamp = @azimuth.Time::now_unix_nanos() + (reading * 1000000) // 每个读取间隔1ms
      let temperature = 20.0 + @azimuth.Random::next_float() * 10.0
      let humidity = 40.0 + @azimuth.Random::next_float() * 20.0
      
      let event = @azimuth.TelemetryEvent::new(
        "sensor-reading",
        timestamp,
        [
          ("device.id", @azimuth.StringValue("device-#{device_id}")),
          ("sensor.type", @azimuth.StringValue(if reading % 2 == 0 { "temperature" } else { "humidity" })),
          ("value", @azimuth.FloatValue(if reading % 2 == 0 { temperature } else { humidity })),
          ("location", @azimuth.StringValue("warehouse-#{device_id % 3}"))
        ]
      )
      
      @azimuth.TelemetryStream::add_event(telemetry_stream, event)
      event_count = event_count + 1
    }
  }
  
  // 定义复杂事件处理规则
  let anomaly_rule = @azimuth.CEPRule::new("temperature-anomaly", @azimuth.CEPOperator::Sequence)
  @azimuth.CEPRule::add_pattern(anomaly_rule, @azimuth.EventPattern::new("temperature-reading", [
    ("sensor.type", @azimuth.StringValue("temperature")),
    ("value", @azimuth.ComparisonOp::GreaterThan, @azimuth.FloatValue(28.0))
  ]))
  @azimuth.CEPRule::add_pattern(anomaly_rule, @azimuth.EventPattern::new("temperature-reading", [
    ("sensor.type", @azimuth.StringValue("temperature")),
    ("value", @azimuth.ComparisonOp::GreaterThan, @azimuth.FloatValue(29.0))
  ]))
  @azimuth.CEPRule::set_time_window(anomaly_rule, @azimuth.TimeWindow::Seconds(10))
  
  let performance_rule = @azimuth.CEPRule::new("performance-degradation", @azimuth.CEPOperator::Aggregation)
  @azimuth.CEPRule::set_aggregation_function(performance_rule, @azimuth.AggregationFunction::Average)
  @azimuth.CEPRule::set_aggregation_field(performance_rule, "value")
  @azimuth.CEPRule::set_aggregation_window(performance_rule, @azimuth.TimeWindow::Seconds(30))
  @azimuth.CEPRule::add_condition(performance_rule, @azimuth.ComparisonOp::LessThan, @azimuth.FloatValue(15.0))
  
  // 注册CEP规则
  @azimuth.StreamProcessor::register_rule(stream_processor, anomaly_rule)
  @azimuth.StreamProcessor::register_rule(stream_processor, performance_rule)
  
  // 处理数据流
  let processing_results = @azimuth.StreamProcessor::process_stream(stream_processor, telemetry_stream)
  
  // 验证流处理结果
  assert_eq(processing_results.total_events_processed, event_count)
  assert_true(processing_results.processing_time_ms > 0)
  assert_true(processing_results.throughput_events_per_second > 0)
  
  // 验证复杂事件检测
  let anomaly_matches = @azimuth.StreamProcessor::get_rule_matches(stream_processor, "temperature-anomaly")
  assert_true(anomaly_matches.length() >= 0)
  
  for match in anomaly_matches {
    assert_eq(@azimuth.CEPMatch::rule_name(match), "temperature-anomaly")
    assert_true(@azimuth.CEPMatch::matched_events(match).length() >= 2)
    assert_true(@azimuth.CEPMatch::confidence(match) > 0.0)
  }
  
  // 测试窗口聚合
  let window_aggregates = @azimuth.StreamProcessor::get_window_aggregates(stream_processor)
  assert_true(window_aggregates.length() > 0)
  
  for aggregate in window_aggregates {
    assert_true(@azimuth.WindowAggregate::event_count(aggregate) > 0)
    assert_true(@azimuth.WindowAggregate::window_start(aggregate) < @azimuth.WindowAggregate::window_end(aggregate))
    assert_true(@azimuth.WindowAggregate::aggregated_values(aggregate).length() > 0)
  }
  
  // 测试流状态管理
  let stream_state = @azimuth.StreamProcessor::get_stream_state(stream_processor)
  assert_true(stream_state.is_healthy)
  assert_eq(stream_state.buffer_size, stream_config.buffer_size)
  assert_true(stream_state.processed_events >= event_count)
  assert_true(stream_state.dropped_events == 0)
}

// Test 4: 数据完整性验证和校验
test "data integrity verification and validation" {
  let integrity_validator = @azimuth.IntegrityValidator::new()
  
  // 配置完整性验证
  let validation_config = @azimuth.ValidationConfig::new()
  @azimuth.ValidationConfig::enable_checksum(validation_config, true)
  @azimuth.ValidationConfig::enable_digital_signature(validation_config, true)
  @azimuth.ValidationConfig::enable_hash_verification(validation_config, true)
  @azimuth.ValidationConfig::set_hash_algorithm(validation_config, @azimuth.HashAlgorithm::SHA256)
  
  @azimuth.IntegrityValidator::configure(integrity_validator, validation_config)
  
  // 创建测试数据集
  let test_datasets = []
  for dataset_id in 1..=5 {
    let mut dataset = []
    for record_id in 1..=100 {
      let record = @azimuth.TelemetryData::new(
        @azimuth.TraceId::generate(),
        @azimuth.SpanId::generate(),
        [
          ("dataset.id", @azimuth.StringValue("dataset-#{dataset_id}")),
          ("record.id", @azimuth.StringValue("record-#{record_id}")),
          ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
          ("metric.value", @azimuth.FloatValue(@azimuth.Random::next_float() * 100.0)),
          ("data.source", @azimuth.StringValue("sensor-#{record_id % 10}"))
        ]
      )
      dataset = dataset.push(record)
    }
    test_datasets = test_datasets.push(dataset)
  }
  
  // 为每个数据集计算完整性校验值
  let mut integrity_checksums = []
  for dataset in test_datasets {
    let checksum = @azimuth.IntegrityValidator::calculate_checksum(integrity_validator, dataset)
    let signature = @azimuth.IntegrityValidator::sign_dataset(integrity_validator, dataset)
    
    integrity_checksums = integrity_checksums.push((checksum, signature))
  }
  
  // 验证数据完整性
  for i in 0..=test_datasets.length() - 1 {
    let dataset = test_datasets[i]
    let expected_checksum = integrity_checksums[i].0
    let expected_signature = integrity_checksums[i].1
    
    // 验证校验和
    let actual_checksum = @azimuth.IntegrityValidator::calculate_checksum(integrity_validator, dataset)
    assert_eq(actual_checksum, expected_checksum)
    
    // 验证数字签名
    let signature_valid = @azimuth.IntegrityValidator::verify_signature(integrity_validator, dataset, expected_signature)
    assert_true(signature_valid)
  }
  
  // 测试数据篡改检测
  let tampered_dataset = test_datasets[0]
  let modified_record = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("dataset.id", @azimuth.StringValue("dataset-1")),
      ("record.id", @azimuth.StringValue("record-50")),
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
      ("metric.value", @azimuth.FloatValue(999.99)), // 篡改的值
      ("data.source", @azimuth.StringValue("malicious-source"))
    ]
  )
  
  // 替换原始记录
  let mut modified_dataset = []
  for record in tampered_dataset {
    match @azimuth.TelemetryData::get_attribute(record, "record.id") {
      Some(@azimuth.StringValue(id)) => {
        if id == "record-50" {
          modified_dataset = modified_dataset.push(modified_record)
        } else {
          modified_dataset = modified_dataset.push(record)
        }
      }
      _ => modified_dataset = modified_dataset.push(record)
    }
  }
  
  // 验证篡改检测
  let tampered_checksum = @azimuth.IntegrityValidator::calculate_checksum(integrity_validator, modified_dataset)
  assert_not_eq(tampered_checksum, integrity_checksums[0].0)
  
  let tampered_signature_valid = @azimuth.IntegrityValidator::verify_signature(integrity_validator, modified_dataset, integrity_checksums[0].1)
  assert_false(tampered_signature_valid)
  
  // 测试数据修复
  let repair_result = @azimuth.IntegrityValidator::attempt_repair(integrity_validator, modified_dataset, integrity_checksums[0].0)
  
  if repair_result.repair_possible {
    let repaired_dataset = repair_result.repaired_data
    let repaired_checksum = @azimuth.IntegrityValidator::calculate_checksum(integrity_validator, repaired_dataset)
    assert_eq(repaired_checksum, integrity_checksums[0].0)
  }
  
  // 测试批量验证
  let batch_validation_result = @azimuth.IntegrityValidator::validate_batch(integrity_validator, test_datasets, integrity_checksums.map(|(cs, _)| cs))
  
  assert_eq(batch_validation_result.total_datasets, test_datasets.length())
  assert_eq(batch_validation_result.valid_datasets, test_datasets.length())
  assert_eq(batch_validation_result.invalid_datasets, 0)
  assert_true(batch_validation_result.overall_integrity)
}

// Test 5: 故障恢复和容错机制
test "fault recovery and fault tolerance mechanisms" {
  let fault_tolerance_manager = @azimuth.FaultToleranceManager::new()
  
  // 配置容错策略
  let fault_tolerance_config = @azimuth.FaultToleranceConfig::new()
  @azimuth.FaultToleranceConfig::enable_circuit_breaker(fault_tolerance_config, true)
  @azimuth.FaultToleranceConfig::set_circuit_breaker_threshold(fault_tolerance_config, 5) // 5次失败后断路
  @azimuth.FaultToleranceConfig::set_circuit_breaker_timeout(fault_tolerance_config, 30000) // 30秒后尝试恢复
  @azimuth.FaultToleranceConfig::enable_retry(fault_tolerance_config, true)
  @azimuth.FaultToleranceConfig::set_max_retries(fault_tolerance_config, 3)
  @azimuth.FaultToleranceConfig::set_retry_backoff(fault_tolerance_config, @azimuth.BackoffStrategy::Exponential(1000, 2.0))
  @azimuth.FaultToleranceConfig::enable_bulkhead(fault_tolerance_config, true)
  @azimuth.FaultToleranceConfig::set_bulkhead_limit(fault_tolerance_config, 10)
  
  @azimuth.FaultToleranceManager::configure(fault_tolerance_manager, fault_tolerance_config)
  
  // 创建测试服务
  let telemetry_service = @azimuth.TelemetryService::new("primary-service")
  let backup_service = @azimuth.TelemetryService::new("backup-service")
  
  // 配置服务故障模拟
  @azimuth.TelemetryService::simulate_failure(telemetry_service, @azimuth.FailureType::NetworkTimeout, 0.7) // 70%失败率
  @azimuth.TelemetryService::simulate_failure(backup_service, @azimuth.FailureType::NetworkTimeout, 0.2) // 20%失败率
  
  // 注册服务
  @azimuth.FaultToleranceManager::register_service(fault_tolerance_manager, telemetry_service)
  @azimuth.FaultToleranceManager::register_backup_service(fault_tolerance_manager, telemetry_service, backup_service)
  
  // 创建测试数据
  let test_requests = []
  for i in 1..=50 {
    let request = @azimuth.TelemetryRequest::new(
      "request-#{i}",
      @azimuth.RequestType::SubmitSpan,
      @azimuth.SpanData::new(
        @azimuth.TraceId::generate(),
        @azimuth.SpanId::generate(),
        "operation-#{i}",
        @azimuth.Time::now_unix_nanos()
      )
    )
    test_requests = test_requests.push(request)
  }
  
  // 执行请求并测试容错机制
  let mut successful_requests = 0
  let mut failed_requests = 0
  let mut circuit_breaker_activations = 0
  let mut backup_service_calls = 0
  
  for request in test_requests {
    let result = @azimuth.FaultToleranceManager::execute_request(fault_tolerance_manager, telemetry_service, request)
    
    match result.outcome {
      @azimuth.RequestOutcome::Success => {
        successful_requests = successful_requests + 1
        if result.used_backup_service {
          backup_service_calls = backup_service_calls + 1
        }
      }
      @azimuth.RequestOutcome::Failed => {
        failed_requests = failed_requests + 1
      }
      @azimuth.RequestOutcome::CircuitBreakerOpen => {
        circuit_breaker_activations = circuit_breaker_activations + 1
      }
    }
    
    // 检查断路器状态
    let circuit_breaker_state = @azimuth.FaultToleranceManager::get_circuit_breaker_state(fault_tolerance_manager, telemetry_service)
    if circuit_breaker_state == @azimuth.CircuitBreakerState::Open {
      // 等待断路器半开状态
      @azimuth.Time::sleep(31000) // 等待超过超时时间
    }
  }
  
  // 验证容错效果
  let success_rate = (successful_requests as Float) / (test_requests.length() as Float)
  assert_true(success_rate > 0.8) // 至少80%成功率
  
  assert_true(backup_service_calls > 0) // 确保备份服务被调用
  assert_true(circuit_breaker_activations > 0) // 确保断路器被激活
  
  // 测试故障恢复
  @azimuth.TelemetryService::stop_failure_simulation(telemetry_service)
  
  let recovery_requests = []
  for i in 51..=60 {
    let request = @azimuth.TelemetryRequest::new(
      "recovery-request-#{i}",
      @azimuth.RequestType::SubmitSpan,
      @azimuth.SpanData::new(
        @azimuth.TraceId::generate(),
        @azimuth.SpanId::generate(),
        "recovery-operation-#{i}",
        @azimuth.Time::now_unix_nanos()
      )
    )
    recovery_requests = recovery_requests.push(request)
  }
  
  // 执行恢复请求
  let mut recovery_successes = 0
  for request in recovery_requests {
    let result = @azimuth.FaultToleranceManager::execute_request(fault_tolerance_manager, telemetry_service, request)
    if result.outcome == @azimuth.RequestOutcome::Success {
      recovery_successes = recovery_successes + 1
    }
  }
  
  // 验证恢复效果
  let recovery_success_rate = (recovery_successes as Float) / (recovery_requests.length() as Float)
  assert_true(recovery_success_rate > 0.9) // 恢复后成功率应该更高
  
  // 测试舱壁隔离
  let bulkhead_stats = @azimuth.FaultToleranceManager::get_bulkhead_stats(fault_tolerance_manager, telemetry_service)
  assert_true(bulkhead_stats.active_executions <= fault_tolerance_config.bulkhead_limit)
  assert_true(bulkhead_stats.rejected_executions >= 0)
  
  // 测试故障监控和报告
  let fault_report = @azimuth.FaultToleranceManager::generate_fault_report(fault_tolerance_manager)
  assert_true(fault_report.total_requests > 0)
  assert_true(fault_report.successful_requests > 0)
  assert_true(fault_report.failed_requests >= 0)
  assert_true(fault_report.circuit_breaker_activations > 0)
  assert_true(fault_report.backup_service_calls > 0)
  assert_true(fault_report.average_response_time_ms > 0)
}

// Test 6: 国际化和本地化支持
test "internationalization and localization support" {
  let i18n_manager = @azimuth.I18nManager::new()
  
  // 配置支持的语言
  let supported_locales = [
    @azimuth.Locale::new("en", "US"),
    @azimuth.Locale::new("zh", "CN"),
    @azimuth.Locale::new("ja", "JP"),
    @azimuth.Locale::new("es", "ES"),
    @azimuth.Locale::new("fr", "FR"),
    @azimuth.Locale::new("de", "DE")
  ]
  
  for locale in supported_locales {
    @azimuth.I18nManager::register_locale(i18n_manager, locale)
  }
  
  // 添加本地化资源
  let error_messages = @azimuth.ResourceBundle::new("error_messages")
  @azimuth.ResourceBundle::add_message(error_messages, "en_US", "connection.timeout", "Connection timeout occurred")
  @azimuth.ResourceBundle::add_message(error_messages, "zh_CN", "connection.timeout", "连接超时")
  @azimuth.ResourceBundle::add_message(error_messages, "ja_JP", "connection.timeout", "接続タイムアウト")
  @azimuth.ResourceBundle::add_message(error_messages, "es_ES", "connection.timeout", "Tiempo de conexión agotado")
  @azimuth.ResourceBundle::add_message(error_messages, "fr_FR", "connection.timeout", "Délai de connexion dépassé")
  @azimuth.ResourceBundle::add_message(error_messages, "de_DE", "connection.timeout", "Verbindungszeitüberschreitung")
  
  @azimuth.ResourceBundle::add_message(error_messages, "en_US", "authentication.failed", "Authentication failed")
  @azimuth.ResourceBundle::add_message(error_messages, "zh_CN", "authentication.failed", "身份验证失败")
  @azimuth.ResourceBundle::add_message(error_messages, "ja_JP", "authentication.failed", "認証に失敗しました")
  @azimuth.ResourceBundle::add_message(error_messages, "es_ES", "authentication.failed", "Error de autenticación")
  @azimuth.ResourceBundle::add_message(error_messages, "fr_FR", "authentication.failed", "Échec de l'authentification")
  @azimuth.ResourceBundle::add_message(error_messages, "de_DE", "authentication.failed", "Authentifizierung fehlgeschlagen")
  
  @azimuth.I18nManager::register_resource_bundle(i18n_manager, error_messages)
  
  let ui_messages = @azimuth.ResourceBundle::new("ui_messages")
  @azimuth.ResourceBundle::add_message(ui_messages, "en_US", "button.save", "Save")
  @azimuth.ResourceBundle::add_message(ui_messages, "zh_CN", "button.save", "保存")
  @azimuth.ResourceBundle::add_message(ui_messages, "ja_JP", "button.save", "保存")
  @azimuth.ResourceBundle::add_message(ui_messages, "es_ES", "button.save", "Guardar")
  @azimuth.ResourceBundle::add_message(ui_messages, "fr_FR", "button.save", "Enregistrer")
  @azimuth.ResourceBundle::add_message(ui_messages, "de_DE", "button.save", "Speichern")
  
  @azimuth.I18nManager::register_resource_bundle(i18n_manager, ui_messages)
  
  // 测试本地化消息获取
  let en_us_locale = @azimuth.Locale::new("en", "US")
  let zh_cn_locale = @azimuth.Locale::new("zh", "CN")
  let ja_jp_locale = @azimuth.Locale::new("ja", "JP")
  
  let en_timeout_msg = @azimuth.I18nManager::get_message(i18n_manager, en_us_locale, "error_messages", "connection.timeout")
  let zh_timeout_msg = @azimuth.I18nManager::get_message(i18n_manager, zh_cn_locale, "error_messages", "connection.timeout")
  let ja_timeout_msg = @azimuth.I18nManager::get_message(i18n_manager, ja_jp_locale, "error_messages", "connection.timeout")
  
  assert_eq(en_timeout_msg, "Connection timeout occurred")
  assert_eq(zh_timeout_msg, "连接超时")
  assert_eq(ja_timeout_msg, "接続タイムアウト")
  
  // 测试格式化和本地化
  let number_formatter = @azimuth.NumberFormatter::new()
  @azimuth.NumberFormatter::set_locale(number_formatter, en_us_locale)
  let en_formatted_number = @azimuth.NumberFormatter::format(number_formatter, 1234567.89)
  assert_eq(en_formatted_number, "1,234,567.89")
  
  @azimuth.NumberFormatter::set_locale(number_formatter, de_de_locale)
  let de_formatted_number = @azimuth.NumberFormatter::format(number_formatter, 1234567.89)
  assert_eq(de_formatted_number, "1.234.567,89")
  
  let date_formatter = @azimuth.DateFormatter::new()
  @azimuth.DateFormatter::set_locale(date_formatter, en_us_locale)
  @azimuth.DateFormatter::set_pattern(date_formatter, "yyyy-MM-dd HH:mm:ss")
  let test_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  let en_formatted_date = @azimuth.DateFormatter::format(date_formatter, test_timestamp)
  
  @azimuth.DateFormatter::set_locale(date_formatter, zh_cn_locale)
  let zh_formatted_date = @azimuth.DateFormatter::format(date_formatter, test_timestamp)
  
  assert_not_eq(en_formatted_date, zh_formatted_date)
  
  // 测试多语言遥测数据
  let localized_telemetry = []
  for locale in supported_locales {
    let telemetry_data = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("locale", @azimuth.StringValue(@azimuth.Locale::to_string(locale))),
        ("error.message", @azimuth.StringValue(@azimuth.I18nManager::get_message(i18n_manager, locale, "error_messages", "connection.timeout"))),
        ("ui.button", @azimuth.StringValue(@azimuth.I18nManager::get_message(i18n_manager, locale, "ui_messages", "button.save"))),
        ("formatted.number", @azimuth.StringValue({
          @azimuth.NumberFormatter::set_locale(number_formatter, locale)
          @azimuth.NumberFormatter::format(number_formatter, 1234567.89)
        })),
        ("timestamp", @azimuth.IntValue(test_timestamp))
      ]
    )
    localized_telemetry = localized_telemetry.push(telemetry_data)
  }
  
  // 验证多语言数据
  for data in localized_telemetry {
    match @azimuth.TelemetryData::get_attribute(data, "locale") {
      Some(@azimuth.StringValue(locale_str)) => {
        let locale = @azimuth.Locale::from_string(locale_str)
        let error_msg = @azimuth.I18nManager::get_message(i18n_manager, locale, "error_messages", "connection.timeout")
        let ui_button = @azimuth.I18nManager::get_message(i18n_manager, locale, "ui_messages", "button.save")
        
        match @azimuth.TelemetryData::get_attribute(data, "error.message") {
          Some(@azimuth.StringValue(msg)) => assert_eq(msg, error_msg)
          _ => assert_true(false)
        }
        
        match @azimuth.TelemetryData::get_attribute(data, "ui.button") {
          Some(@azimuth.StringValue(button)) => assert_eq(button, ui_button)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试回退机制
  let unsupported_locale = @azimuth.Locale::new("ko", "KR")
  let fallback_msg = @azimuth.I18nManager::get_message(i18n_manager, unsupported_locale, "error_messages", "connection.timeout")
  assert_eq(fallback_msg, "Connection timeout occurred") // 回退到默认语言(英语)
  
  // 测试复数形式
  let plural_resource = @azimuth.ResourceBundle::new("plural_messages")
  @azimuth.ResourceBundle::add_plural_message(plural_resource, "en_US", "items.count", [
    (0, "No items"),
    (1, "One item"),
    (2, "Two items"),
    (@azimuth.PluralRule::Other, "{0} items")
  ])
  
  @azimuth.ResourceBundle::add_plural_message(plural_resource, "zh_CN", "items.count", [
    (0, "没有项目"),
    (1, "一个项目"),
    (2, "两个项目"),
    (@azimuth.PluralRule::Other, "{0}个项目")
  ])
  
  @azimuth.I18nManager::register_resource_bundle(i18n_manager, plural_resource)
  
  let en_plural_1 = @azimuth.I18nManager::get_plural_message(i18n_manager, en_us_locale, "plural_messages", "items.count", 1)
  let en_plural_5 = @azimuth.I18nManager::get_plural_message(i18n_manager, en_us_locale, "plural_messages", "items.count", 5)
  let zh_plural_1 = @azimuth.I18nManager::get_plural_message(i18n_manager, zh_cn_locale, "plural_messages", "items.count", 1)
  let zh_plural_5 = @azimuth.I18nManager::get_plural_message(i18n_manager, zh_cn_locale, "plural_messages", "items.count", 5)
  
  assert_eq(en_plural_1, "One item")
  assert_eq(en_plural_5, "5 items")
  assert_eq(zh_plural_1, "一个项目")
  assert_eq(zh_plural_5, "5个项目")
}

// Test 7: 自适应采样策略
test "adaptive sampling strategies" {
  let adaptive_sampler = @azimuth.AdaptiveSampler::new()
  
  // 配置自适应采样
  let sampling_config = @azimuth.SamplingConfig::new()
  @azimuth.SamplingConfig::set_base_sampling_rate(sampling_config, 0.1) // 基础采样率10%
  @azimuth.SamplingConfig::enable_throughput_adaptation(sampling_config, true)
  @azimuth.SamplingConfig::set_max_sampling_rate(sampling_config, 1.0) // 最大采样率100%
  @azimuth.SamplingConfig::set_min_sampling_rate(sampling_config, 0.01) // 最小采样率1%
  @azimuth.SamplingConfig::set_adaptation_window(sampling_config, @azimuth.TimeWindow::Minutes(5))
  @azimuth.SamplingConfig::set_target_throughput(sampling_config, 1000) // 目标吞吐量1000 spans/秒
  
  @azimuth.AdaptiveSampler::configure(adaptive_sampler, sampling_config)
  
  // 模拟不同负载情况
  let low_load_trace_ids = []
  let medium_load_trace_ids = []
  let high_load_trace_ids = []
  
  // 低负载场景
  for i in 1..=100 {
    let trace_id = @azimuth.TraceId::generate()
    low_load_trace_ids = low_load_trace_ids.push(trace_id)
    
    let sampling_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, trace_id, [
      ("service.name", @azimuth.StringValue("low-load-service")),
      ("operation.name", @azimuth.StringValue("low-load-operation"))
    ])
    
    // 低负载时应该有较高的采样率
    if i <= 20 {
      assert_true(sampling_decision.sampled) // 前20个应该被采样以建立基线
    }
  }
  
  // 中等负载场景
  for i in 1..=500 {
    let trace_id = @azimuth.TraceId::generate()
    medium_load_trace_ids = medium_load_trace_ids.push(trace_id)
    
    let sampling_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, trace_id, [
      ("service.name", @azimuth.StringValue("medium-load-service")),
      ("operation.name", @azimuth.StringValue("medium-load-operation"))
    ])
  }
  
  // 高负载场景
  for i in 1..=2000 {
    let trace_id = @azimuth.TraceId::generate()
    high_load_trace_ids = high_load_trace_ids.push(trace_id)
    
    let sampling_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, trace_id, [
      ("service.name", @azimuth.StringValue("high-load-service")),
      ("operation.name", @azimuth.StringValue("high-load-operation"))
    ])
  }
  
  // 等待自适应调整
  @azimuth.Time::sleep(6000) // 等待6秒以超过5秒的适应窗口
  
  // 测试高优先级操作的采样
  let high_priority_decisions = []
  for i in 1..=100 {
    let trace_id = @azimuth.TraceId::generate()
    let sampling_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, trace_id, [
      ("service.name", @azimuth.StringValue("critical-service")),
      ("operation.name", @azimuth.StringValue("critical-operation")),
      ("priority", @azimuth.StringValue("high"))
    ])
    high_priority_decisions = high_priority_decisions.push(sampling_decision.sampled)
  }
  
  // 高优先级操作应该有更高的采样率
  let high_priority_sample_rate = (high_priority_decisions.filter(fn(d) { d }).length() as Float) / (high_priority_decisions.length() as Float)
  assert_true(high_priority_sample_rate > 0.5) // 至少50%的采样率
  
  // 测试错误操作的采样
  let error_decisions = []
  for i in 1..=50 {
    let trace_id = @azimuth.TraceId::generate()
    let sampling_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, trace_id, [
      ("service.name", @azimuth.StringValue("error-prone-service")),
      ("operation.name", @azimuth.StringValue("error-operation")),
      ("error.type", @azimuth.StringValue("TimeoutError"))
    ])
    error_decisions = error_decisions.push(sampling_decision.sampled)
  }
  
  // 错误操作应该总是被采样
  let error_sample_rate = (error_decisions.filter(fn(d) { d }).length() as Float) / (error_decisions.length() as Float)
  assert_eq(error_sample_rate, 1.0) // 100%的采样率
  
  // 获取采样统计
  let sampling_stats = @azimuth.AdaptiveSampler::get_sampling_statistics(adaptive_sampler)
  assert_true(sampling_stats.total_traces_sampled > 0)
  assert_true(sampling_stats.total_traces_seen > 0)
  assert_true(sampling_stats.current_sampling_rate >= sampling_config.min_sampling_rate)
  assert_true(sampling_stats.current_sampling_rate <= sampling_config.max_sampling_rate)
  
  // 测试基于属性的采样规则
  let attribute_based_rules = [
    @azimuth.SamplingRule::new("debug-traces", @azimuth.AttributeFilter::Equals("debug", @azimuth.StringValue("true")), 1.0),
    @azimuth.SamplingRule::new("health-checks", @azimuth.AttributeFilter::Equals("operation.name", @azimuth.StringValue("health-check")), 0.0),
    @azimuth.SamplingRule::new("api-calls", @azimuth.AttributeFilter::StartsWith("operation.name", @azimuth.StringValue("api.")), 0.2)
  ]
  
  for rule in attribute_based_rules {
    @azimuth.AdaptiveSampler::add_sampling_rule(adaptive_sampler, rule)
  }
  
  // 测试调试追踪
  let debug_trace_id = @azimuth.TraceId::generate()
  let debug_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, debug_trace_id, [
    ("debug", @azimuth.StringValue("true"))
  ])
  assert_true(debug_decision.sampled)
  
  // 测试健康检查
  let health_check_trace_id = @azimuth.TraceId::generate()
  let health_check_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, health_check_trace_id, [
    ("operation.name", @azimuth.StringValue("health-check"))
  ])
  assert_false(health_check_decision.sampled)
  
  // 测试API调用
  let api_trace_id = @azimuth.TraceId::generate()
  let api_decision = @azimuth.AdaptiveSampler::should_sample(adaptive_sampler, api_trace_id, [
    ("operation.name", @azimuth.StringValue("api.get_user"))
  ])
  // API调用应该有20%的采样率，但由于是单个测试，我们无法精确验证概率
  
  // 测试采样率调整历史
  let rate_adjustment_history = @azimuth.AdaptiveSampler::get_rate_adjustment_history(adaptive_sampler)
  assert_true(rate_adjustment_history.length() > 0)
  
  for adjustment in rate_adjustment_history {
    assert_true(adjustment.old_rate >= sampling_config.min_sampling_rate)
    assert_true(adjustment.old_rate <= sampling_config.max_sampling_rate)
    assert_true(adjustment.new_rate >= sampling_config.min_sampling_rate)
    assert_true(adjustment.new_rate <= sampling_config.max_sampling_rate)
    assert_true(adjustment.adjustment_reason.length() > 0)
    assert_true(adjustment.timestamp > 0)
  }
}

// Test 8: 数据生命周期管理
test "data lifecycle management" {
  let lifecycle_manager = @azimuth.LifecycleManager::new()
  
  // 配置生命周期策略
  let lifecycle_policy = @azimuth.LifecyclePolicy::new()
  @azimuth.LifecyclePolicy::set_retention_period(lifecycle_policy, @azimuth.DataCategory::Spans, @azimuth.TimeDuration::Days(30))
  @azimuth.LifecyclePolicy::set_retention_period(lifecycle_policy, @azimuth.DataCategory::Metrics, @azimuth.TimeDuration::Days(90))
  @azimuth.LifecyclePolicy::set_retention_period(lifecycle_policy, @azimuth.DataCategory::Logs, @azimuth.TimeDuration::Days(7))
  @azimuth.LifecyclePolicy::set_retention_period(lifecycle_policy, @azimuth.DataCategory::ErrorTraces, @azimuth.TimeDuration::Days(365))
  
  // 配置数据分层存储
  @azimuth.LifecyclePolicy::add_storage_tier(lifecycle_policy, @azimuth.StorageTier::Hot, @azimuth.TimeDuration::Days(7))
  @azimuth.LifecyclePolicy::add_storage_tier(lifecycle_policy, @azimuth.StorageTier::Warm, @azimuth.TimeDuration::Days(30))
  @azimuth.LifecyclePolicy::add_storage_tier(lifecycle_policy, @azimuth.StorageTier::Cold, @azimuth.TimeDuration::Days(90))
  @azimuth.LifecyclePolicy::add_storage_tier(lifecycle_policy, @azimuth.StorageTier::Archive, @azimuth.TimeDuration::Days(365))
  
  @azimuth.LifecycleManager::set_policy(lifecycle_manager, lifecycle_policy)
  
  // 创建不同时间的数据
  let current_time = @azimuth.Time::now_unix_nanos()
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  let recent_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("data.category", @azimuth.StringValue("spans")),
      ("timestamp", @azimuth.IntValue(current_time)),
      ("storage.tier", @azimuth.StringValue("hot"))
    ]
  )
  
  let old_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("data.category", @azimuth.StringValue("spans")),
      ("timestamp", @azimuth.IntValue(current_time - (40 * day_in_nanos))), // 40天前
      ("storage.tier", @azimuth.StringValue("cold"))
    ]
  )
  
  let very_old_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("data.category", @azimuth.StringValue("logs")),
      ("timestamp", @azimuth.IntValue(current_time - (10 * day_in_nanos))), // 10天前
      ("storage.tier", @azimuth.StringValue("cold"))
    ]
  )
  
  let error_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("data.category", @azimuth.StringValue("error_traces")),
      ("timestamp", @azimuth.IntValue(current_time - (200 * day_in_nanos))), // 200天前
      ("storage.tier", @azimuth.StringValue("warm"))
    ]
  )
  
  // 存储数据
  @azimuth.LifecycleManager::store_data(lifecycle_manager, recent_data)
  @azimuth.LifecycleManager::store_data(lifecycle_manager, old_data)
  @azimuth.LifecycleManager::store_data(lifecycle_manager, very_old_data)
  @azimuth.LifecycleManager::store_data(lifecycle_manager, error_data)
  
  // 执行生命周期检查
  let lifecycle_check_result = @azimuth.LifecycleManager::execute_lifecycle_check(lifecycle_manager)
  
  // 验证生命周期检查结果
  assert_true(lifecycle_check_result.total_data_checked == 4)
  assert_true(lifecycle_check_result.data_moved > 0)
  assert_true(lifecycle_check_result.data_expired > 0)
  
  // 验证数据分层
  let recent_data_tier = @azimuth.LifecycleManager::get_data_tier(lifecycle_manager, recent_data)
  let old_data_tier = @azimuth.LifecycleManager::get_data_tier(lifecycle_manager, old_data)
  let very_old_data_tier = @azimuth.LifecycleManager::get_data_tier(lifecycle_manager, very_old_data)
  let error_data_tier = @azimuth.LifecycleManager::get_data_tier(lifecycle_manager, error_data)
  
  assert_eq(recent_data_tier, @azimuth.StorageTier::Hot)
  assert_eq(old_data_tier, @azimuth.StorageTier::Cold)
  assert_eq(very_old_data_tier, @azimuth.StorageTier::Archive) // 日志数据超过7天应该归档
  assert_eq(error_data_tier, @azimuth.StorageTier::Warm) // 错误追踪保留时间长
  
  // 测试数据过期
  let expired_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("data.category", @azimuth.StringValue("logs")),
      ("timestamp", @azimuth.IntValue(current_time - (8 * day_in_nanos))), // 8天前的日志
      ("storage.tier", @azimuth.StringValue("cold"))
    ]
  )
  
  @azimuth.LifecycleManager::store_data(lifecycle_manager, expired_data)
  
  let is_expired = @azimuth.LifecycleManager::is_data_expired(lifecycle_manager, expired_data)
  assert_true(is_expired) // 日志超过7天保留期
  
  // 测试数据保留策略
  let retention_policies = @azimuth.LifecycleManager::get_retention_policies(lifecycle_manager)
  assert_eq(retention_policies.length(), 4) // 四种数据类型
  
  let span_retention = retention_policies.filter(fn(p) { p.data_category == @azimuth.DataCategory::Spans })[0]
  assert_eq(span_retention.retention_period, @azimuth.TimeDuration::Days(30))
  
  let log_retention = retention_policies.filter(fn(p) { p.data_category == @azimuth.DataCategory::Logs })[0]
  assert_eq(log_retention.retention_period, @azimuth.TimeDuration::Days(7))
  
  // 测试数据归档
  let archive_candidates = @azimuth.LifecycleManager::get_archive_candidates(lifecycle_manager)
  assert_true(archive_candidates.length() > 0)
  
  for candidate in archive_candidates {
    assert_true(candidate.age_in_days >= 7) // 至少7天的数据
    assert_eq(candidate.current_tier, @azimuth.StorageTier::Cold) // 冷存储数据
  }
  
  // 执行数据归档
  let archive_result = @azimuth.LifecycleManager::execute_archive(lifecycle_manager)
  
  // 验证归档结果
  assert_true(archive_result.success)
  assert_true(archive_result.archived_count > 0)
  assert_true(archive_result.archive_size_bytes > 0)
  
  // 测试数据清理
  let cleanup_candidates = @azimuth.LifecycleManager::get_cleanup_candidates(lifecycle_manager)
  assert_true(cleanup_candidates.length() > 0)
  
  // 执行数据清理
  let cleanup_result = @azimuth.LifecycleManager::execute_cleanup(lifecycle_manager)
  
  // 验证清理结果
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.cleaned_count > 0)
  assert_true(cleanup_result.freed_space_bytes > 0)
  
  // 测试生命周期统计
  let lifecycle_stats = @azimuth.LifecycleManager::get_lifecycle_statistics(lifecycle_manager)
  assert_true(lifecycle_stats.total_data_objects > 0)
  assert_true(lifecycle_stats.hot_tier_objects >= 0)
  assert_true(lifecycle_stats.warm_tier_objects >= 0)
  assert_true(lifecycle_stats.cold_tier_objects >= 0)
  assert_true(lifecycle_stats.archive_tier_objects >= 0)
  assert_true(lifecycle_stats.expired_objects >= 0)
  assert_true(lifecycle_stats.total_storage_size_bytes > 0)
}

// Test 9: 跨平台兼容性
test "cross-platform compatibility" {
  let platform_adapter = @azimuth.PlatformAdapter::new()
  
  // 检测当前平台
  let current_platform = @azimuth.PlatformAdapter::detect_platform(platform_adapter)
  assert_true(current_platform.os_type != @azimuth.OSType::Unknown)
  assert_true(current_platform.architecture != @azimuth.Architecture::Unknown)
  
  // 创建平台特定的配置
  let platform_config = @azimuth.PlatformConfig::new()
  
  match current_platform.os_type {
    @azimuth.OSType::Linux => {
      @azimuth.PlatformConfig::set_signal_handler(platform_config, @azimuth.SignalType::SIGTERM, true)
      @azimuth.PlatformConfig::set_file_permissions(platform_config, 0o644)
      @azimuth.PlatformConfig::set_shared_library_extension(platform_config, "so")
    }
    @azimuth.OSType::Windows => {
      @azimuth.PlatformConfig::set_signal_handler(platform_config, @azimuth.SignalType::CTRL_C, true)
      @azimuth.PlatformConfig::set_file_permissions(platform_config, 0o666)
      @azimuth.PlatformConfig::set_shared_library_extension(platform_config, "dll")
    }
    @azimuth.OSType::MacOS => {
      @azimuth.PlatformConfig::set_signal_handler(platform_config, @azimuth.SignalType::SIGINT, true)
      @azimuth.PlatformConfig::set_file_permissions(platform_config, 0o644)
      @azimuth.PlatformConfig::set_shared_library_extension(platform_config, "dylib")
    }
    _ => assert_true(false)
  }
  
  match current_platform.architecture {
    @azimuth.Architecture::X86_64 => @azimuth.PlatformConfig::set_cpu_optimization(platform_config, @azimuth.CPUOptimization::SSE4_2)
    @azimuth.Architecture::ARM64 => @azimuth.PlatformConfig::set_cpu_optimization(platform_config, @azimuth.CPUOptimization::NEON)
    @azimuth.Architecture::X86 => @azimuth.PlatformConfig::set_cpu_optimization(platform_config, @azimuth.CPUOptimization::SSE2)
    _ => @azimuth.PlatformConfig::set_cpu_optimization(platform_config, @azimuth.CPUOptimization::Generic)
  }
  
  @azimuth.PlatformAdapter::configure(platform_adapter, platform_config)
  
  // 测试平台特定的文件系统操作
  let test_file_path = @azimuth.PlatformAdapter::get_temp_path(platform_adapter, "azimuth_test")
  assert_true(test_file_path.length() > 0)
  
  let test_data = "Azimuth cross-platform test data"
  let write_result = @azimuth.PlatformAdapter::write_file(platform_adapter, test_file_path, test_data)
  assert_true(write_result.success)
  
  let read_result = @azimuth.PlatformAdapter::read_file(platform_adapter, test_file_path)
  assert_true(read_result.success)
  assert_eq(read_result.content, test_data)
  
  // 清理测试文件
  let delete_result = @azimuth.PlatformAdapter::delete_file(platform_adapter, test_file_path)
  assert_true(delete_result.success)
  
  // 测试平台特定的网络配置
  let network_config = @azimuth.NetworkConfig::new()
  @azimuth.NetworkConfig::set_socket_buffer_size(network_config, 65536)
  @azimuth.NetworkConfig::set_connection_timeout(network_config, 5000)
  @azimuth.NetworkConfig::set_keep_alive(network_config, true)
  
  match current_platform.os_type {
    @azimuth.OSType::Linux => {
      @azimuth.NetworkConfig::set_tcp_nodelay(network_config, true)
      @azimuth.NetworkConfig::set_reuse_address(network_config, true)
    }
    @azimuth.OSType::Windows => {
      @azimuth.NetworkConfig::set_exclusive_address_use(network_config, false)
      @azimuth.NetworkConfig::set_io_buffering(network_config, true)
    }
    _ => {}
  }
  
  // 测试平台特定的性能监控
  let performance_monitor = @azimuth.PerformanceMonitor::new()
  @azimuth.PerformanceMonitor::configure_for_platform(performance_monitor, current_platform)
  
  // 执行性能测试
  let performance_metrics = @azimuth.PerformanceMonitor::measure_system_performance(performance_monitor)
  
  assert_true(performance_metrics.cpu_usage >= 0.0 && performance_metrics.cpu_usage <= 1.0)
  assert_true(performance_metrics.memory_usage_bytes > 0)
  assert_true(performance_metrics.available_memory_bytes > 0)
  assert_true(performance_metrics.disk_usage_bytes > 0)
  assert_true(performance_metrics.network_io_bytes_sent >= 0)
  assert_true(performance_metrics.network_io_bytes_received >= 0)
  
  // 测试平台特定的信号处理
  let signal_handler = @azimuth.SignalHandler::new()
  
  match current_platform.os_type {
    @azimuth.OSType::Linux | @azimuth.OSType::MacOS => {
      @azimuth.SignalHandler::register_handler(signal_handler, @azimuth.SignalType::SIGTERM, fn() {
        @azimuth.Logger::info("Received SIGTERM, shutting down gracefully")
      })
      @azimuth.SignalHandler::register_handler(signal_handler, @azimuth.SignalType::SIGINT, fn() {
        @azimuth.Logger::info("Received SIGINT, shutting down gracefully")
      })
    }
    @azimuth.OSType::Windows => {
      @azimuth.SignalHandler::register_handler(signal_handler, @azimuth.SignalType::CTRL_C, fn() {
        @azimuth.Logger::info("Received Ctrl+C, shutting down gracefully")
      })
      @azimuth.SignalHandler::register_handler(signal_handler, @azimuth.SignalType::CTRL_BREAK, fn() {
        @azimuth.Logger::info("Received Ctrl+Break, shutting down gracefully")
      })
    }
    _ => {}
  }
  
  // 测试平台特定的线程管理
  let thread_manager = @azimuth.ThreadManager::new()
  @azimuth.ThreadManager::configure_for_platform(thread_manager, current_platform)
  
  let thread_pool_size = @azimuth.ThreadManager::get_optimal_thread_pool_size(thread_manager)
  assert_true(thread_pool_size > 0)
  
  // 测试线程创建和执行
  let test_tasks = []
  for i in 1..=10 {
    let task = @azimuth.Task::new("platform-test-#{i}", fn() {
      @azimuth.Time::sleep(100) // 睡眠100ms
      i * i
    })
    test_tasks = test_tasks.push(task)
  }
  
  let task_results = @azimuth.ThreadManager::execute_tasks(thread_manager, test_tasks)
  assert_eq(task_results.length(), test_tasks.length())
  
  for i in 0..=task_results.length() - 1 {
    let expected_result = (i + 1) * (i + 1)
    assert_eq(task_results[i], expected_result)
  }
  
  // 测试平台特定的内存管理
  let memory_manager = @azimuth.MemoryManager::new()
  @azimuth.MemoryManager::configure_for_platform(memory_manager, current_platform)
  
  // 分配内存块
  let memory_blocks = []
  for i in 1..=100 {
    let block_size = 1024 * (i % 10 + 1) // 1KB到10KB
    let memory_block = @azimuth.MemoryManager::allocate(memory_manager, block_size)
    memory_blocks = memory_blocks.push(memory_block)
  }
  
  // 验证内存分配
  let total_allocated = @azimuth.MemoryManager::get_allocated_memory(memory_manager)
  assert_true(total_allocated > 0)
  
  // 释放内存块
  for block in memory_blocks {
    @azimuth.MemoryManager::deallocate(memory_manager, block)
  }
  
  // 验证内存释放
  let total_freed = @azimuth.MemoryManager::get_freed_memory(memory_manager)
  assert_true(total_freed > 0)
  
  // 测试平台特定的时区和时间处理
  let time_handler = @azimuth.TimeHandler::new()
  @azimuth.TimeHandler::configure_for_platform(time_handler, current_platform)
  
  let local_time = @azimuth.TimeHandler::get_local_time(time_handler)
  let utc_time = @azimuth.TimeHandler::get_utc_time(time_handler)
  let timezone_offset = @azimuth.TimeHandler::get_timezone_offset(time_handler)
  
  assert_true(local_time > 0)
  assert_true(utc_time > 0)
  assert_true(timezone_offset >= -12 * 3600 && timezone_offset <= 14 * 3600) // 合理的时区偏移范围
  
  // 测试平台特定的序列化
  let serializer = @azimuth.Serializer::new()
  @azimuth.PlatformAdapter::configure_serializer(platform_adapter, serializer, current_platform)
  
  let test_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("platform", @azimuth.StringValue(@azimuth.Platform::to_string(current_platform))),
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
      ("test.value", @azimuth.FloatValue(42.0))
    ]
  )
  
  let serialized_data = @azimuth.Serializer::serialize(serializer, test_data)
  assert_true(serialized_data.length() > 0)
  
  let deserialized_data = @azimuth.Serializer::deserialize(serializer, serialized_data)
  assert_eq(@azimuth.TelemetryData::get_attribute(deserialized_data, "test.value"), Some(@azimuth.FloatValue(42.0)))
}

// Test 10: 高级查询和过滤功能
test "advanced querying and filtering capabilities" {
  let query_engine = @azimuth.QueryEngine::new()
  
  // 配置查询引擎
  let query_config = @azimuth.QueryConfig::new()
  @azimuth.QueryConfig::set_max_results(query_config, 1000)
  @azimuth.QueryConfig::set_query_timeout(query_config, 30000) // 30秒超时
  @azimuth.QueryConfig::enable_caching(query_config, true)
  @azimuth.QueryConfig::set_cache_size(query_config, 100)
  @azimuth.QueryConfig::enable_indexing(query_config, true)
  
  @azimuth.QueryEngine::configure(query_engine, query_config)
  
  // 创建测试数据集
  let test_data = []
  let current_time = @azimuth.Time::now_unix_nanos()
  let hour_in_nanos = 60 * 60 * 1000000000L
  
  for i in 1..=500 {
    let timestamp = current_time - (i * hour_in_nanos) // 每个记录间隔一小时
    
    let data_point = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("timestamp", @azimuth.IntValue(timestamp)),
        ("service.name", @azimuth.StringValue(if i % 3 == 0 { "payment-service" } else if i % 3 == 1 { "user-service" } else { "order-service" })),
        ("operation.name", @azimuth.StringValue(if i % 4 == 0 { "process-payment" } else if i % 4 == 1 { "authenticate-user" } else if i % 4 == 2 { "create-order" } else { "fetch-order" })),
        ("status", @azimuth.StringValue(if i % 10 == 0 { "error" } else if i % 10 == 1 { "timeout" } else { "success" })),
        ("duration", @azimuth.IntValue(50 + (i % 200))), // 50到249ms
        ("user.id", @azimuth.StringValue("user-#{i % 100}")), // 100个不同用户
        ("region", @azimuth.StringValue(if i % 5 == 0 { "us-west-2" } else if i % 5 == 1 { "us-east-1" } else if i % 5 == 2 { "eu-west-1" } else if i % 5 == 3 { "ap-southeast-1" } else { "ap-northeast-1" })),
        ("error.type", @azimuth.StringValue(if i % 10 == 0 { "PaymentError" } else if i % 10 == 1 { "TimeoutError" } else { "" })),
        ("response.size", @azimuth.IntValue(1024 * (i % 10 + 1))) // 1KB到10KB
      ]
    )
    test_data = test_data.push(data_point)
  }
  
  // 索引数据
  @azimuth.QueryEngine::index_data(query_engine, test_data)
  
  // 测试简单查询
  let simple_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(simple_query, @azimuth.Filter::Equals("service.name", @azimuth.StringValue("payment-service")))
  
  let simple_results = @azimuth.QueryEngine::execute(query_engine, simple_query)
  assert_eq(simple_results.length(), 167) // 500/3 ≈ 166.67，向上取整
  
  for result in simple_results {
    match @azimuth.TelemetryData::get_attribute(result, "service.name") {
      Some(@azimuth.StringValue(service_name)) => assert_eq(service_name, "payment-service")
      _ => assert_true(false)
    }
  }
  
  // 测试范围查询
  let range_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(range_query, @azimuth.Filter::Range("duration", @azimuth.IntValue(100), @azimuth.IntValue(200)))
  
  let range_results = @azimuth.QueryEngine::execute(query_engine, range_query)
  assert_true(range_results.length() > 0)
  
  for result in range_results {
    match @azimuth.TelemetryData::get_attribute(result, "duration") {
      Some(@azimuth.IntValue(duration)) => assert_true(duration >= 100 && duration <= 200)
      _ => assert_true(false)
    }
  }
  
  // 测试时间范围查询
  let time_range_query = @azimuth.Query::new()
  let start_time = current_time - (100 * hour_in_nanos)
  let end_time = current_time - (50 * hour_in_nanos)
  @azimuth.Query::add_filter(time_range_query, @azimuth.Filter::TimeRange("timestamp", start_time, end_time))
  
  let time_range_results = @azimuth.QueryEngine::execute(query_engine, time_range_query)
  assert_eq(time_range_results.length(), 50) // 100-50+1 = 51，但由于索引从1开始，实际是50个
  
  // 测试复合查询
  let complex_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(complex_query, @azimuth.Filter::Equals("status", @azimuth.StringValue("error")))
  @azimuth.Query::add_filter(complex_query, @azimuth.Filter::In("region", [@azimuth.StringValue("us-west-2"), @azimuth.StringValue("us-east-1")]))
  @azimuth.Query::add_filter(complex_query, @azimuth.Filter::GreaterThan("duration", @azimuth.IntValue(100)))
  
  let complex_results = @azimuth.QueryEngine::execute(query_engine, complex_query)
  assert_true(complex_results.length() >= 0)
  
  for result in complex_results {
    match @azimuth.TelemetryData::get_attribute(result, "status") {
      Some(@azimuth.StringValue(status)) => assert_eq(status, "error")
      _ => assert_true(false)
    }
    
    match @azimuth.TelemetryData::get_attribute(result, "region") {
      Some(@azimuth.StringValue(region)) => assert_true(region == "us-west-2" || region == "us-east-1")
      _ => assert_true(false)
    }
    
    match @azimuth.TelemetryData::get_attribute(result, "duration") {
      Some(@azimuth.IntValue(duration)) => assert_true(duration > 100)
      _ => assert_true(false)
    }
  }
  
  // 测试聚合查询
  let aggregation_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(aggregation_query, @azimuth.Filter::Equals("service.name", @azimuth.StringValue("user-service")))
  @azimuth.Query::add_aggregation(aggregation_query, @azimuth.Aggregation::Average("duration"))
  @azimuth.Query::add_aggregation(aggregation_query, @azimuth.Aggregation::Count("operation.name"))
  @azimuth.Query::add_aggregation(aggregation_query, @azimuth.Aggregation::Sum("response.size"))
  @azimuth.Query::add_group_by(aggregation_query, "operation.name")
  
  let aggregation_results = @azimuth.QueryEngine::execute_aggregation(query_engine, aggregation_query)
  assert_true(aggregation_results.length() > 0)
  
  for result in aggregation_results {
    assert_true(result.group_key.is_some())
    assert_true(result.aggregations.length() == 3)
    
    for aggregation in result.aggregations {
      match aggregation.function {
        @azimuth.AggregationFunction::Average => assert_true(aggregation.value > 0.0)
        @azimuth.AggregationFunction::Count => assert_true(aggregation.value > 0.0)
        @azimuth.AggregationFunction::Sum => assert_true(aggregation.value > 0.0)
        _ => assert_true(false)
      }
    }
  }
  
  // 测试排序查询
  let sort_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(sort_query, @azimuth.Filter::Equals("service.name", @azimuth.StringValue("order-service")))
  @azimuth.Query::add_sort(sort_query, @azimuth.Sort::Desc("duration"))
  @azimuth.Query::set_limit(sort_query, 10)
  
  let sort_results = @azimuth.QueryEngine::execute(query_engine, sort_query)
  assert_eq(sort_results.length(), 10)
  
  // 验证排序结果
  for i in 0..=sort_results.length() - 2 {
    let current_duration = match @azimuth.TelemetryData::get_attribute(sort_results[i], "duration") {
      Some(@azimuth.IntValue(d)) => d
      _ => 0
    }
    
    let next_duration = match @azimuth.TelemetryData::get_attribute(sort_results[i + 1], "duration") {
      Some(@azimuth.IntValue(d)) => d
      _ => 0
    }
    
    assert_true(current_duration >= next_duration) // 降序排列
  }
  
  // 测试模糊查询
  let fuzzy_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(fuzzy_query, @azimuth.Filter::Contains("operation.name", @azimuth.StringValue("order")))
  
  let fuzzy_results = @azimuth.QueryEngine::execute(query_engine, fuzzy_query)
  assert_true(fuzzy_results.length() > 0)
  
  for result in fuzzy_results {
    match @azimuth.TelemetryData::get_attribute(result, "operation.name") {
      Some(@azimuth.StringValue(operation_name)) => assert_true(operation_name.contains("order"))
      _ => assert_true(false)
    }
  }
  
  // 测试正则表达式查询
  let regex_query = @azimuth.Query::new()
  @azimuth.Query::add_filter(regex_query, @azimuth.Filter::Regex("user.id", @azimuth.StringValue("user-[1-9][0-9]")))
  
  let regex_results = @azimuth.QueryEngine::execute(query_engine, regex_query)
  assert_true(regex_results.length() > 0)
  
  for result in regex_results {
    match @azimuth.TelemetryData::get_attribute(result, "user.id") {
      Some(@azimuth.StringValue(user_id)) => {
        assert_true(user_id.starts_with("user-"))
        let user_num_str = user_id.substring(5, user_id.length())
        assert_true(user_num_str.length() >= 2)
        assert_true(user_num_str[0] != '0')
      }
      _ => assert_true(false)
    }
  }
  
  // 测试查询缓存
  let cache_stats_before = @azimuth.QueryEngine::get_cache_statistics(query_engine)
  
  // 重复执行相同查询
  @azimuth.QueryEngine::execute(query_engine, simple_query)
  @azimuth.QueryEngine::execute(query_engine, simple_query)
  
  let cache_stats_after = @azimuth.QueryEngine::get_cache_statistics(query_engine)
  assert_true(cache_stats_after.cache_hits > cache_stats_before.cache_hits)
  
  // 测试查询性能统计
  let query_performance = @azimuth.QueryEngine::get_query_performance(query_engine)
  assert_true(query_performance.total_queries > 0)
  assert_true(query_performance.average_execution_time_ms > 0)
  assert_true(query_performance.min_execution_time_ms > 0)
  assert_true(query_performance.max_execution_time_ms >= query_performance.min_execution_time_ms)
}