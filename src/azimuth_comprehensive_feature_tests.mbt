// Azimuth Telemetry System - Comprehensive Feature Tests
// 高级功能测试用例，涵盖序列化、时间序列、异常处理、性能等核心功能

// Test 1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 创建复杂的遥测数据结构
  let telemetry_data = azimuth::TelemetryData {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    parent_span_id: Some("b7ad6b7169203330"),
    operation_name: "http_request",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("http.method", azimuth::AttributeValue::StringValue("GET")),
      ("http.url", azimuth::AttributeValue::StringValue("https://api.example.com/users")),
      ("http.status_code", azimuth::AttributeValue::IntValue(200)),
      ("user.id", azimuth::AttributeValue::StringValue("user123"))
    ],
    events: [
      azimuth::SpanEvent {
        name: "http.request.started",
        timestamp: 1234567891L,
        attributes: [
          ("component", azimuth::AttributeValue::StringValue("http-client"))
        ]
      },
      azimuth::SpanEvent {
        name: "http.response.received",
        timestamp: 1234567894L,
        attributes: [
          ("response.size", azimuth::AttributeValue::IntValue(1024))
        ]
      }
    ]
  }
  
  // 序列化遥测数据
  let serialized = azimuth::TelemetrySerializer::serialize(telemetry_data)
  assert_true(serialized.length() > 0)
  
  // 反序列化遥测数据
  let deserialized = azimuth::TelemetrySerializer::deserialize(serialized)
  
  // 验证反序列化后的数据完整性
  assert_eq(deserialized.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized.span_id, telemetry_data.span_id)
  assert_eq(deserialized.operation_name, telemetry_data.operation_name)
  assert_eq(deserialized.start_time, telemetry_data.start_time)
  assert_eq(deserialized.end_time, telemetry_data.end_time)
  assert_eq(deserialized.attributes.length(), telemetry_data.attributes.length())
  assert_eq(deserialized.events.length(), telemetry_data.events.length())
}

// Test 2: 时间序列数据处理
test "时间序列数据处理" {
  // 创建时间序列数据点
  let time_series_data = [
    azimuth::TimeSeriesPoint {
      timestamp: 1234567890L,
      value: azimuth::MetricValue::Double(10.5),
      labels: [
        ("service", "payment"),
        ("region", "us-west-1")
      ]
    },
    azimuth::TimeSeriesPoint {
      timestamp: 1234567891L,
      value: azimuth::MetricValue::Double(15.2),
      labels: [
        ("service", "payment"),
        ("region", "us-west-1")
      ]
    },
    azimuth::TimeSeriesPoint {
      timestamp: 1234567892L,
      value: azimuth::MetricValue::Double(8.7),
      labels: [
        ("service", "payment"),
        ("region", "us-west-1")
      ]
    }
  ]
  
  // 创建时间序列
  let time_series = azimuth::TimeSeries::new("response_time", time_series_data)
  
  // 验证时间序列基本属性
  assert_eq(time_series.metric_name, "response_time")
  assert_eq(time_series.data_points.length(), 3)
  
  // 计算聚合统计
  let aggregator = azimuth::TimeSeriesAggregator::new()
  let stats = aggregator.aggregate(time_series)
  
  // 验证聚合结果
  match stats {
    azimuth::AggregationStats {
      count, 
      sum, 
      min, 
      max, 
      avg
    } => {
      assert_eq(count, 3)
      assert_true(sum > 34.0 && sum < 35.0) // 10.5 + 15.2 + 8.7 = 34.4
      assert_true(min > 8.0 && min < 9.0) // 8.7
      assert_true(max > 15.0 && max < 16.0) // 15.2
      assert_true(avg > 11.0 && avg < 12.0) // 34.4 / 3 ≈ 11.47
    }
  }
  
  // 测试时间窗口查询
  let window_start = 1234567890L
  let window_end = 1234567891L
  let window_data = azimuth::TimeSeries::query_window(time_series, window_start, window_end)
  
  assert_eq(window_data.length(), 2)
  assert_eq(window_data[0].timestamp, 1234567890L)
  assert_eq(window_data[1].timestamp, 1234567891L)
}

// Test 3: 异常处理和恢复机制
test "异常处理和恢复机制" {
  // 创建异常处理器
  let exception_handler = azimuth::ExceptionHandler::new()
  
  // 测试网络连接异常
  let network_error = azimuth::TelemetryError::NetworkError("Connection timeout")
  let recovery_strategy = exception_handler.handle_error(network_error)
  
  match recovery_strategy {
    azimuth::RecoveryStrategy::Retry { max_attempts, delay_ms } => {
      assert_true(max_attempts > 0)
      assert_true(delay_ms > 0)
    }
    _ => assert_true(false)
  }
  
  // 测试数据格式异常
  let format_error = azimuth::TelemetryError::FormatError("Invalid JSON format")
  let format_recovery = exception_handler.handle_error(format_error)
  
  match format_recovery {
    azimuth::RecoveryStrategy::Skip { log_level } => {
      assert_eq(log_level, azimuth::LogLevel::Warn)
    }
    _ => assert_true(false)
  }
  
  // 测试资源耗尽异常
  let resource_error = azimuth::TelemetryError::ResourceExhausted("Memory limit exceeded")
  let resource_recovery = exception_handler.handle_error(resource_error)
  
  match resource_recovery {
    azimuth::RecoveryStrategy::CircuitBreaker { timeout_ms } => {
      assert_true(timeout_ms > 0)
    }
    _ => assert_true(false)
  }
  
  // 测试恢复机制
  let recovered = exception_handler.attempt_recovery(recovery_strategy)
  assert_true(recovered)
}

// Test 4: 性能基准测试
test "性能基准测试" {
  // 创建性能基准测试器
  let benchmark = azimuth::PerformanceBenchmark::new()
  
  // 测试属性创建性能
  let attr_creation_time = benchmark.measure_func(fn() {
    let attrs = azimuth::Attributes::new()
    Attributes::set(attrs, "test.key", StringValue("test.value"))
    Attributes::get(attrs, "test.key")
  })
  
  // 验证属性创建性能（应该在合理范围内）
  assert_true(attr_creation_time < 1000) // 小于1毫秒
  
  // 测试序列化性能
  let test_data = azimuth::TelemetryData {
    trace_id: "test_trace_id",
    span_id: "test_span_id",
    parent_span_id: None,
    operation_name: "test_operation",
    start_time: 0L,
    end_time: 100L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("test.attr", azimuth::AttributeValue::StringValue("test_value"))
    ],
    events: []
  }
  
  let serialization_time = benchmark.measure_func(fn() {
    azimuth::TelemetrySerializer::serialize(test_data)
  })
  
  // 验证序列化性能
  assert_true(serialization_time < 5000) // 小于5毫秒
  
  // 测试批量处理性能
  let batch_size = 100
  let batch_data = [test_data; batch_size]
  
  let batch_processing_time = benchmark.measure_func(fn() {
    for data in batch_data {
      azimuth::TelemetrySerializer::serialize(data)
    }
  })
  
  // 验证批量处理性能（平均每个数据点处理时间）
  let avg_per_item = batch_processing_time / batch_size
  assert_true(avg_per_item < 100) // 平均每个数据点小于0.1毫秒
}

// Test 5: 数据验证和完整性检查
test "数据验证和完整性检查" {
  // 创建数据验证器
  let validator = azimuth::DataValidator::new()
  
  // 测试有效的追踪ID
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  assert_true(validator.is_valid_trace_id(valid_trace_id))
  
  // 测试无效的追踪ID
  let invalid_trace_id = "invalid_trace_id"
  assert_false(validator.is_valid_trace_id(invalid_trace_id))
  
  // 测试有效的跨度ID
  let valid_span_id = "b7ad6b7169203331"
  assert_true(validator.is_valid_span_id(valid_span_id))
  
  // 测试无效的跨度ID
  let invalid_span_id = "invalid_span_id"
  assert_false(validator.is_valid_span_id(invalid_span_id))
  
  // 测试时间戳验证
  let current_time = 1234567890L
  assert_true(validator.is_valid_timestamp(current_time))
  
  let invalid_timestamp = -1L
  assert_false(validator.is_valid_timestamp(invalid_timestamp))
  
  // 测试属性值验证
  let valid_string_attr = azimuth::AttributeValue::StringValue("valid_string")
  assert_true(validator.is_valid_attribute_value(valid_string_attr))
  
  let valid_int_attr = azimuth::AttributeValue::IntValue(42)
  assert_true(validator.is_valid_attribute_value(valid_int_attr))
  
  let valid_float_attr = azimuth::AttributeValue::FloatValue(3.14)
  assert_true(validator.is_valid_attribute_value(valid_float_attr))
  
  let valid_bool_attr = azimuth::AttributeValue::BoolValue(true)
  assert_true(validator.is_valid_attribute_value(valid_bool_attr))
  
  // 测试遥测数据完整性
  let valid_telemetry_data = azimuth::TelemetryData {
    trace_id: valid_trace_id,
    span_id: valid_span_id,
    parent_span_id: Some(valid_span_id),
    operation_name: "valid_operation",
    start_time: current_time,
    end_time: current_time + 100L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("valid.attr", azimuth::AttributeValue::StringValue("valid_value"))
    ],
    events: []
  }
  
  let validation_result = validator.validate_telemetry_data(valid_telemetry_data)
  assert_true(validation_result.is_valid)
  
  // 测试无效的遥测数据
  let invalid_telemetry_data = azimuth::TelemetryData {
    trace_id: invalid_trace_id,
    span_id: invalid_span_id,
    parent_span_id: None,
    operation_name: "",
    start_time: invalid_timestamp,
    end_time: current_time,
    status: azimuth::SpanStatus::Error,
    attributes: [],
    events: []
  }
  
  let invalid_validation_result = validator.validate_telemetry_data(invalid_telemetry_data)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
}

// Test 6: 配置管理和动态更新
test "配置管理和动态更新" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new()
  
  // 设置初始配置
  let initial_config = [
    ("service.name", "azimuth-service"),
    ("service.version", "1.0.0"),
    ("telemetry.enabled", "true"),
    ("telemetry.sampling.rate", "0.1"),
    ("exporter.endpoint", "http://localhost:4317")
  ]
  
  for (key, value) in initial_config {
    config_manager.set(key, value)
  }
  
  // 验证初始配置
  assert_eq(config_manager.get("service.name"), Some("azimuth-service"))
  assert_eq(config_manager.get("service.version"), Some("1.0.0"))
  assert_eq(config_manager.get("telemetry.enabled"), Some("true"))
  assert_eq(config_manager.get("telemetry.sampling.rate"), Some("0.1"))
  assert_eq(config_manager.get("exporter.endpoint"), Some("http://localhost:4317"))
  
  // 测试配置更新
  config_manager.set("service.version", "1.1.0")
  config_manager.set("telemetry.sampling.rate", "0.2")
  
  // 验证配置更新
  assert_eq(config_manager.get("service.version"), Some("1.1.0"))
  assert_eq(config_manager.get("telemetry.sampling.rate"), Some("0.2"))
  
  // 测试配置监听器
  let mut listener_called = false
  let config_listener = azimuth::ConfigListener {
    on_change: fn(key : String, old_value : Option<String>, new_value : String) {
      if key == "service.version" {
        listener_called = true
      }
    }
  }
  
  config_manager.add_listener(config_listener)
  config_manager.set("service.version", "1.2.0")
  
  // 验证监听器被调用
  assert_true(listener_called)
  
  // 测试配置验证
  let valid_config = [
    ("telemetry.sampling.rate", "0.5"),
    ("exporter.timeout", "5000")
  ]
  
  for (key, value) in valid_config {
    assert_true(config_manager.validate_config(key, value))
  }
  
  let invalid_config = [
    ("telemetry.sampling.rate", "1.5"), // 超出范围
    ("exporter.timeout", "invalid") // 无效格式
  ]
  
  for (key, value) in invalid_config {
    assert_false(config_manager.validate_config(key, value))
  }
  
  // 测试配置持久化
  let config_snapshot = config_manager.snapshot()
  assert_true(config_snapshot.contains("service.name"))
  assert_true(config_snapshot.contains("service.version"))
  assert_true(config_snapshot.contains("telemetry.enabled"))
  
  // 测试配置恢复
  config_manager.set("service.name", "modified-service")
  assert_eq(config_manager.get("service.name"), Some("modified-service"))
  
  config_manager.restore(config_snapshot)
  assert_eq(config_manager.get("service.name"), Some("azimuth-service"))
}

// Test 7: 跨平台兼容性
test "跨平台兼容性" {
  // 创建平台检测器
  let platform_detector = azimuth::PlatformDetector::new()
  
  // 检测当前平台
  let current_platform = platform_detector.detect_platform()
  assert_true(current_platform.is_supported)
  
  // 测试不同平台的配置
  let linux_config = platform_detector.get_platform_config("linux")
  let windows_config = platform_detector.get_platform_config("windows")
  let macos_config = platform_detector.get_platform_config("macos")
  
  // 验证平台配置存在
  assert_true(linux_config.is_some())
  assert_true(windows_config.is_some())
  assert_true(macos_config.is_some())
  
  // 测试路径处理
  let path_handler = azimuth::PathHandler::new()
  
  // 测试不同平台的路径格式
  let unix_path = "/var/log/azimuth/telemetry.log"
  let windows_path = "C:\\ProgramData\\Azimuth\\telemetry.log"
  
  let normalized_unix_path = path_handler.normalize_path(unix_path)
  let normalized_windows_path = path_handler.normalize_path(windows_path)
  
  // 验证路径标准化
  assert_true(normalized_unix_path.length() > 0)
  assert_true(normalized_windows_path.length() > 0)
  
  // 测试文件系统操作
  let file_ops = azimuth::FileOperations::new()
  
  // 测试临时文件创建
  let temp_file = file_ops.create_temp_file("azimuth_test")
  assert_true(temp_file.is_some())
  
  // 测试文件权限检查
  match temp_file {
    Some(file_path) => {
      assert_true(file_ops.check_permissions(file_path, azimuth::FilePermission::Read))
      assert_true(file_ops.check_permissions(file_path, azimuth::FilePermission::Write))
      
      // 清理临时文件
      file_ops.delete_file(file_path)
    }
    None => assert_true(false)
  }
  
  // 测试网络兼容性
  let network_compat = azimuth::NetworkCompatibility::new()
  
  // 测试IPv4和IPv6支持
  assert_true(network_compat.supports_ipv4())
  assert_true(network_compat.supports_ipv6())
  
  // 测试TLS版本支持
  let supported_tls_versions = network_compat.get_supported_tls_versions()
  assert_true(supported_tls_versions.length() > 0)
  assert_true(supported_tls_versions.contains("1.2"))
  assert_true(supported_tls_versions.contains("1.3"))
}

// Test 8: 安全性和隐私保护
test "安全性和隐私保护" {
  // 创建安全管理器
  let security_manager = azimuth::SecurityManager::new()
  
  // 测试敏感数据检测
  let sensitive_data = [
    "password=secret123",
    "api_key=abc123def456",
    "token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9",
    "credit_card=4111111111111111",
    "ssn=123-45-6789"
  ]
  
  for data in sensitive_data {
    assert_true(security_manager.is_sensitive_data(data))
  }
  
  // 测试数据脱敏
  let password_data = "password=secret123"
  let masked_password = security_manager.mask_sensitive_data(password_data)
  assert_eq(masked_password, "password=******")
  
  let api_key_data = "api_key=abc123def456"
  let masked_api_key = security_manager.mask_sensitive_data(api_key_data)
  assert_eq(masked_api_key, "api_key=************")
  
  // 测试加密和解密
  let original_data = "sensitive_telemetry_data"
  let encrypted_data = security_manager.encrypt(original_data)
  
  assert_ne(encrypted_data, original_data)
  assert_true(encrypted_data.length() > 0)
  
  let decrypted_data = security_manager.decrypt(encrypted_data)
  assert_eq(decrypted_data, original_data)
  
  // 测试数字签名
  let message = "important_telemetry_message"
  let signature = security_manager.sign_message(message)
  
  assert_true(signature.length() > 0)
  assert_true(security_manager.verify_signature(message, signature))
  
  // 测试篡改检测
  let tampered_message = "tampered_telemetry_message"
  assert_false(security_manager.verify_signature(tampered_message, signature))
  
  // 测试访问控制
  let access_control = azimuth::AccessControl::new()
  
  // 创建角色和权限
  access_control.create_role("admin", ["read", "write", "delete"])
  access_control.create_role("viewer", ["read"])
  access_control.create_role("editor", ["read", "write"])
  
  // 测试权限检查
  assert_true(access_control.check_permission("admin", "delete"))
  assert_false(access_control.check_permission("viewer", "delete"))
  assert_true(access_control.check_permission("editor", "write"))
  assert_false(access_control.check_permission("editor", "delete"))
  
  // 测试用户角色分配
  access_control.assign_role("user1", "admin")
  access_control.assign_role("user2", "viewer")
  
  assert_true(access_control.has_permission("user1", "delete"))
  assert_false(access_control.has_permission("user2", "delete"))
  assert_true(access_control.has_permission("user2", "read"))
}

// Test 9: 资源管理和清理
test "资源管理和清理" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 测试资源注册
  let resource1 = azimuth::Resource {
    id: "resource1",
    type: "memory",
    size: 1024,
    created_at: 1234567890L
  }
  
  let resource2 = azimuth::Resource {
    id: "resource2",
    type: "connection",
    size: 64,
    created_at: 1234567891L
  }
  
  resource_manager.register_resource(resource1)
  resource_manager.register_resource(resource2)
  
  // 验证资源注册
  assert_eq(resource_manager.get_resource_count(), 2)
  assert_true(resource_manager.has_resource("resource1"))
  assert_true(resource_manager.has_resource("resource2"))
  
  // 测试资源使用统计
  let total_memory = resource_manager.get_total_memory_usage()
  assert_eq(total_memory, 1024)
  
  let connection_count = resource_manager.get_resource_count_by_type("connection")
  assert_eq(connection_count, 1)
  
  // 测试资源清理
  let cleaned_resources = resource_manager.cleanup_expired_resources(1234567895L)
  assert_eq(cleaned_resources.length(), 2)
  
  // 验证资源清理后的状态
  assert_eq(resource_manager.get_resource_count(), 0)
  assert_false(resource_manager.has_resource("resource1"))
  assert_false(resource_manager.has_resource("resource2"))
  
  // 测试内存泄漏检测
  let memory_leak_detector = azimuth::MemoryLeakDetector::new()
  
  // 模拟内存分配
  memory_leak_detector.allocate_memory("test_allocation_1", 512)
  memory_leak_detector.allocate_memory("test_allocation_2", 256)
  
  // 检测内存泄漏
  let leak_report = memory_leak_detector.detect_leaks()
  assert_eq(leak_report.allocations.length(), 2)
  assert_eq(leak_report.total_allocated, 768)
  
  // 模拟内存释放
  memory_leak_detector.free_memory("test_allocation_1")
  
  // 再次检测内存泄漏
  let updated_leak_report = memory_leak_detector.detect_leaks()
  assert_eq(updated_leak_report.allocations.length(), 1)
  assert_eq(updated_leak_report.total_allocated, 256)
  
  // 测试资源池管理
  let resource_pool = azimuth::ResourcePool::new(5) // 最大5个资源
  
  // 测试资源获取和释放
  let pooled_resource1 = resource_pool.acquire()
  let pooled_resource2 = resource_pool.acquire()
  
  assert_true(pooled_resource1.is_some())
  assert_true(pooled_resource2.is_some())
  assert_eq(resource_pool.get_available_count(), 3)
  
  // 释放资源
  resource_pool.release(pooled_resource1.unwrap())
  assert_eq(resource_pool.get_available_count(), 4)
  
  // 测试资源池耗尽
  let mut acquired_resources = []
  for i in 0..=7 {
    let resource = resource_pool.acquire()
    if resource.is_some() {
      acquired_resources = acquired_resources + [resource.unwrap()]
    }
  }
  
  assert_eq(acquired_resources.length(), 5) // 最多5个资源
  assert_eq(resource_pool.get_available_count(), 0)
  
  // 释放所有资源
  for resource in acquired_resources {
    resource_pool.release(resource)
  }
  
  assert_eq(resource_pool.get_available_count(), 5)
}

// Test 10: 分布式追踪一致性
test "分布式追踪一致性" {
  // 创建分布式追踪管理器
  let distributed_tracer = azimuth::DistributedTracer::new()
  
  // 创建根跨度
  let root_span = distributed_tracer.create_root_span("user_request", [
    ("user.id", "user123"),
    ("request.id", "req-456")
  ])
  
  // 验证根跨度属性
  assert_eq(root_span.operation_name, "user_request")
  assert_true(root_span.is_root)
  assert_true(root_span.trace_id.length() > 0)
  assert_true(root_span.span_id.length() > 0)
  assert_eq(root_span.parent_span_id, None)
  
  // 创建子跨度
  let child_span1 = distributed_tracer.create_child_span(root_span, "database_query", [
    ("db.type", "postgresql"),
    ("db.statement", "SELECT * FROM users WHERE id = $1")
  ])
  
  let child_span2 = distributed_tracer.create_child_span(root_span, "cache_lookup", [
    ("cache.type", "redis"),
    ("cache.key", "user:123")
  ])
  
  // 验证子跨度属性
  assert_eq(child_span1.operation_name, "database_query")
  assert_false(child_span1.is_root)
  assert_eq(child_span1.trace_id, root_span.trace_id) // 相同的追踪ID
  assert_ne(child_span1.span_id, root_span.span_id) // 不同的跨度ID
  assert_eq(child_span1.parent_span_id, Some(root_span.span_id)) // 父跨度ID正确
  
  assert_eq(child_span2.operation_name, "cache_lookup")
  assert_false(child_span2.is_root)
  assert_eq(child_span2.trace_id, root_span.trace_id) // 相同的追踪ID
  assert_ne(child_span2.span_id, root_span.span_id) // 不同的跨度ID
  assert_eq(child_span2.parent_span_id, Some(root_span.span_id)) // 父跨度ID正确
  
  // 测试跨服务传播
  let propagation_context = distributed_tracer.extract_propagation_context(root_span)
  
  // 模拟跨服务调用
  let remote_span = distributed_tracer.create_remote_span("service_b", "process_payment", propagation_context, [
    ("payment.amount", "99.99"),
    ("payment.currency", "USD")
  ])
  
  // 验证远程跨度属性
  assert_eq(remote_span.operation_name, "process_payment")
  assert_eq(remote_span.trace_id, root_span.trace_id) // 保持追踪一致性
  assert_ne(remote_span.span_id, root_span.span_id) // 不同的跨度ID
  assert_eq(remote_span.parent_span_id, Some(root_span.span_id)) // 父跨度ID正确
  
  // 测试追踪上下文传播
  let injected_headers = distributed_tracer.inject_headers(remote_span)
  
  // 验证注入的头包含追踪信息
  assert_true(injected_headers.contains("traceparent"))
  assert_true(injected_headers.contains("tracestate"))
  
  // 模拟从另一个服务提取追踪上下文
  let extracted_context = distributed_tracer.extract_headers(injected_headers)
  
  // 验证提取的上下文
  assert_eq(extracted_context.trace_id, root_span.trace_id)
  assert_eq(extracted_context.span_id, remote_span.span_id)
  
  // 创建另一个远程服务的跨度
  let another_remote_span = distributed_tracer.create_remote_span("service_c", "send_notification", extracted_context, [
    ("notification.type", "email"),
    ("notification.recipient", "user@example.com")
  ])
  
  // 验证追踪链完整性
  assert_eq(another_remote_span.trace_id, root_span.trace_id) // 整个追踪链使用相同的追踪ID
  assert_eq(another_remote_span.parent_span_id, Some(remote_span.span_id)) // 正确的父跨度关系
  
  // 测试追踪采样
  let sampling_config = azimuth::SamplingConfig {
    sampling_rate: 0.1, // 10%采样率
    sample_based_on_trace_id: true,
    always_sample_parent_spans: true
  }
  
  distributed_tracer.configure_sampling(sampling_config)
  
  // 创建多个跨度测试采样
  let mut sampled_spans = []
  for i in 0..=19 {
    let span = distributed_tracer.create_root_span("test_operation_" + i.to_string(), [])
    if span.sampled {
      sampled_spans = sampled_spans + [span]
    }
  }
  
  // 验证采样率（大约10%）
  let sampling_rate = sampled_spans.length() as Double / 20.0
  assert_true(sampling_rate > 0.05 && sampling_rate < 0.15) // 允许一些偏差
  
  // 测试追踪一致性验证
  let trace_consistency_validator = azimuth::TraceConsistencyValidator::new()
  
  // 构建完整的追踪链
  let trace_chain = [root_span, child_span1, child_span2, remote_span, another_remote_span]
  
  // 验证追踪一致性
  let consistency_result = trace_consistency_validator.validate_trace_chain(trace_chain)
  
  assert_true(consistency_result.is_consistent)
  assert_eq(consistency_result.root_span_id, root_span.span_id)
  assert_eq(consistency_result.total_spans, 5)
  assert_eq(consistency_result.unique_span_ids, 5) // 所有跨度ID都是唯一的
  assert_eq(consistency_result.services_involved, 3) // service_a, service_b, service_c
}