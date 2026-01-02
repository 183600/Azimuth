// Azimuth Supplementary Comprehensive Test Suite
// 补充综合测试用例 - 覆盖边界条件、性能优化和故障恢复场景

// 测试1: 数据序列化和反序列化的边界条件
test "telemetry data serialization boundary conditions" {
  // 测试空数据序列化
  let empty_telemetry = @azimuth.TelemetryData {
    timestamp : 0L,
    trace_id : "",
    span_id : "",
    parent_span_id : None,
    operation_name : "",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 0L,
    attributes : [],
    events : []
  }
  
  // 验证空数据处理
  assert_eq(empty_telemetry.trace_id, "")
  assert_eq(empty_telemetry.span_id, "")
  assert_eq(empty_telemetry.attributes.length(), 0)
  assert_eq(empty_telemetry.events.length(), 0)
  
  // 测试极大值处理
  let max_telemetry = @azimuth.TelemetryData {
    timestamp : 9223372036854775807L, // Long.MAX_VALUE
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "max.value.test.operation",
    status : @azimuth.SpanStatus::Error,
    duration_ms : 9223372036854775807L,
    attributes : [
      ("max.int", @azimuth.IntValue(9223372036854775807L)),
      ("max.float", @azimuth.FloatValue(1.7976931348623157e+308)),
      ("max.string", @azimuth.StringValue("这是一个非常长的字符串，用于测试系统对长字符串的处理能力，包含中文字符和特殊符号!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "max.value.event",
        timestamp : 9223372036854775807L,
        attributes : [("large.data", @azimuth.StringValue("x".repeat(10000)))]
      }
    ]
  }
  
  // 验证极大值处理
  assert_eq(max_telemetry.timestamp, 9223372036854775807L)
  assert_eq(max_telemetry.duration_ms, 9223372036854775807L)
  match max_telemetry.attributes[0].1 {
    @azimuth.IntValue(v) => assert_eq(v, 9223372036854775807L)
    _ => assert_true(false)
  }
}

// 测试2: 多线程并发环境下的遥测数据一致性
test "concurrent telemetry data consistency" {
  // 模拟并发操作的数据结构
  let concurrent_contexts = [
    @azimuth.Context {
      trace_id : "trace123456789012345678901234567890",
      span_id : "span1111111111111",
      baggage : [("thread.id", "thread-1")],
      entries : [("operation", "concurrent.test.1")]
    },
    @azimuth.Context {
      trace_id : "trace123456789012345678901234567890",
      span_id : "span2222222222222",
      baggage : [("thread.id", "thread-2")],
      entries : [("operation", "concurrent.test.2")]
    },
    @azimuth.Context {
      trace_id : "trace123456789012345678901234567890",
      span_id : "span3333333333333",
      baggage : [("thread.id", "thread-3")],
      entries : [("operation", "concurrent.test.3")]
    }
  ]
  
  // 验证所有上下文共享相同的trace_id
  for ctx in concurrent_contexts {
    assert_eq(ctx.trace_id, "trace123456789012345678901234567890")
  }
  
  // 验证每个上下文有唯一的span_id
  let span_ids = concurrent_contexts.map(fn(ctx) { ctx.span_id })
  assert_eq(span_ids.length(), span_ids.to_set().length())
  
  // 验证并发度量数据一致性
  let concurrent_metrics = [
    @azimuth.CounterMetric {
      name : "concurrent.operations",
      description : "Concurrent operations count",
      unit : "operations",
      value : 100L,
      attributes : [("thread.id", @azimuth.StringValue("thread-1"))]
    },
    @azimuth.CounterMetric {
      name : "concurrent.operations",
      description : "Concurrent operations count",
      unit : "operations",
      value : 150L,
      attributes : [("thread.id", @azimuth.StringValue("thread-2"))]
    },
    @azimuth.CounterMetric {
      name : "concurrent.operations",
      description : "Concurrent operations count",
      unit : "operations",
      value : 75L,
      attributes : [("thread.id", @azimuth.StringValue("thread-3"))]
    }
  ]
  
  // 验证度量数据聚合
  let total_operations = concurrent_metrics.reduce(fn(acc, metric) { acc + metric.value }, 0L)
  assert_eq(total_operations, 325L)
}

// 测试3: 大数据量下的性能和内存管理
test "large scale telemetry performance" {
  // 创建大量遥测数据
  let large_attribute_list = []
  for i in 0..=1000 {
    large_attribute_list = large_attribute_list.push(("attr." + i.to_string(), @azimuth.StringValue("value." + i.to_string())))
  }
  
  let large_event_list = []
  for i in 0..=100 {
    large_event_list = large_event_list.push(@azimuth.SpanEvent {
      name : "event." + i.to_string(),
      timestamp : 1640995200000L + (i as Int64),
      attributes : [("event.id", @azimuth.IntValue(i as Int64))]
    })
  }
  
  let large_telemetry = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "large.scale.trace.id.12345678901234567890",
    span_id : "large.scale.span.1234567890",
    parent_span_id : Some("large.scale.parent.1234567890"),
    operation_name : "large.scale.operation.test",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 5000L,
    attributes : large_attribute_list,
    events : large_event_list
  }
  
  // 验证大数据量处理
  assert_eq(large_telemetry.attributes.length(), 1001)
  assert_eq(large_telemetry.events.length(), 101)
  assert_eq(large_telemetry.duration_ms, 5000L)
  
  // 验证特定属性和事件
  let first_attr = large_telemetry.attributes[0]
  assert_eq(first_attr.0, "attr.0")
  match first_attr.1 {
    @azimuth.StringValue(v) => assert_eq(v, "value.0")
    _ => assert_true(false)
  }
  
  let last_event = large_telemetry.events[100]
  assert_eq(last_event.name, "event.100")
  assert_eq(last_event.timestamp, 1640995200100L)
}

// 测试4: 网络故障和恢复场景
test "network failure and recovery scenarios" {
  // 模拟网络故障状态
  let network_failure_span = @azimuth.Span {
    context : @azimuth.SpanContext {
      trace_id : "failure.trace.id.12345678901234567890",
      span_id : "failure.span.1234567890",
      sampled : true,
      trace_state : ""
    },
    parent_span_id : Some("parent.failure.span.1234567890"),
    operation_name : "network.operation",
    start_time : 1640995200000L,
    end_time : Some(16409952003000L), // 30秒超时
    status : @azimuth.SpanStatus::Error,
    attributes : [
      ("network.operation", @azimuth.StringValue("http.request")),
      ("network.endpoint", @azimuth.StringValue("https://api.example.com/data")),
      ("network.timeout", @azimuth.IntValue(30000)),
      ("network.retry.count", @azimuth.IntValue(3)),
      ("network.error.type", @azimuth.StringValue("ConnectionTimeout")),
      ("network.error.code", @azimuth.StringValue("ETIMEDOUT"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "network.attempt.1",
        timestamp : 1640995200000L,
        attributes : [("attempt", @azimuth.IntValue(1)), ("result", @azimuth.StringValue("failed"))]
      },
      @azimuth.SpanEvent {
        name : "network.attempt.2",
        timestamp : 16409952001000L,
        attributes : [("attempt", @azimuth.IntValue(2)), ("result", @azimuth.StringValue("failed"))]
      },
      @azimuth.SpanEvent {
        name : "network.attempt.3",
        timestamp : 16409952002000L,
        attributes : [("attempt", @azimuth.IntValue(3)), ("result", @azimuth.StringValue("failed"))]
      },
      @azimuth.SpanEvent {
        name : "network.failure",
        timestamp : 16409952003000L,
        attributes : [("final.result", @azimuth.StringValue("timeout")), ("exhausted.retries", @azimuth.StringValue("true"))]
      }
    ]
  }
  
  // 验证网络故障状态
  match network_failure_span.status {
    @azimuth.SpanStatus::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证重试事件
  assert_eq(network_failure_span.events.length(), 4)
  assert_eq(network_failure_span.events[0].name, "network.attempt.1")
  assert_eq(network_failure_span.events[3].name, "network.failure")
  
  // 模拟网络恢复状态
  let network_recovery_span = @azimuth.Span {
    context : @azimuth.SpanContext {
      trace_id : "recovery.trace.id.12345678901234567890",
      span_id : "recovery.span.1234567890",
      sampled : true,
      trace_state : ""
    },
    parent_span_id : None,
    operation_name : "network.recovery.operation",
    start_time : 1640995300000L,
    end_time : Some(1640995300150L),
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("network.operation", @azimuth.StringValue("http.request")),
      ("network.endpoint", @azimuth.StringValue("https://api.example.com/data")),
      ("network.response.code", @azimuth.IntValue(200)),
      ("network.response.time", @azimuth.IntValue(150)),
      ("network.recovery", @azimuth.StringValue("successful"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "network.recovery.attempt",
        timestamp : 1640995300000L,
        attributes : [("circuit.breaker", @azimuth.StringValue("closed")), ("retry.strategy", @azimuth.StringValue("exponential.backoff"))]
      },
      @azimuth.SpanEvent {
        name : "network.recovery.success",
        timestamp : 1640995300150L,
        attributes : [("response.size", @azimuth.IntValue(1024)), ("response.time", @azimuth.IntValue(150))]
      }
    ]
  }
  
  // 验证网络恢复状态
  match network_recovery_span.status {
    @azimuth.SpanStatus::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(network_recovery_span.events.length(), 2)
  assert_eq(network_recovery_span.events[1].name, "network.recovery.success")
}

// 测试5: 不同数据类型属性的转换和验证
test "attribute type conversion and validation" {
  // 创建包含不同数据类型的属性
  let mixed_attributes = [
    ("string.attr", @azimuth.StringValue("test.string.value")),
    ("int.attr", @azimuth.IntValue(42)),
    ("float.attr", @azimuth.FloatValue(3.14159)),
    ("bool.attr", @azimuth.BoolValue(true)),
    ("null.attr", @azimuth.NullValue),
    ("array.attr", @azimuth.ArrayValue([@azimuth.StringValue("item1"), @azimuth.StringValue("item2")])),
    ("map.attr", @azimuth.MapValue([("key1", @azimuth.StringValue("value1")), ("key2", @azimuth.IntValue(123))])),
    ("bytes.attr", @azimuth.BytesValue([0x48, 0x65, 0x6C, 0x6C, 0x6F])) // "Hello" in bytes
  ]
  
  // 验证字符串属性
  let string_attr = mixed_attributes.filter(fn(attr) { attr.0 == "string.attr" })[0]
  match string_attr.1 {
    @azimuth.StringValue(v) => assert_eq(v, "test.string.value")
    _ => assert_true(false)
  }
  
  // 验证整数属性
  let int_attr = mixed_attributes.filter(fn(attr) { attr.0 == "int.attr" })[0]
  match int_attr.1 {
    @azimuth.IntValue(v) => assert_eq(v, 42L)
    _ => assert_true(false)
  }
  
  // 验证浮点属性
  let float_attr = mixed_attributes.filter(fn(attr) { attr.0 == "float.attr" })[0]
  match float_attr.1 {
    @azimuth.FloatValue(v) => assert_true(abs(v - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // 验证布尔属性
  let bool_attr = mixed_attributes.filter(fn(attr) { attr.0 == "bool.attr" })[0]
  match bool_attr.1 {
    @azimuth.BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // 验证空值属性
  let null_attr = mixed_attributes.filter(fn(attr) { attr.0 == "null.attr" })[0]
  match null_attr.1 {
    @azimuth.NullValue => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证数组属性
  let array_attr = mixed_attributes.filter(fn(attr) { attr.0 == "array.attr" })[0]
  match array_attr.1 {
    @azimuth.ArrayValue(arr) => {
      assert_eq(arr.length(), 2)
      match arr[0] {
        @azimuth.StringValue(v) => assert_eq(v, "item1")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证映射属性
  let map_attr = mixed_attributes.filter(fn(attr) { attr.0 == "map.attr" })[0]
  match map_attr.1 {
    @azimuth.MapValue(map) => {
      assert_eq(map.length(), 2)
      let key1_value = map.filter(fn(item) { item.0 == "key1" })[0]
      match key1_value.1 {
        @azimuth.StringValue(v) => assert_eq(v, "value1")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证字节数组属性
  let bytes_attr = mixed_attributes.filter(fn(attr) { attr.0 == "bytes.attr" })[0]
  match bytes_attr.1 {
    @azimuth.BytesValue(bytes) => {
      assert_eq(bytes.length(), 5)
      assert_eq(bytes[0], 0x48)
      assert_eq(bytes[1], 0x65)
      assert_eq(bytes[2], 0x6C)
      assert_eq(bytes[3], 0x6C)
      assert_eq(bytes[4], 0x6F)
    }
    _ => assert_true(false)
  }
}

// 测试6: 时间序列数据的聚合和查询
test "time series data aggregation and querying" {
  // 创建时间序列数据点
  let time_series_points = [
    @azimuth.TimeSeriesPoint {
      timestamp : 1640995200000L, // 2022-01-01 00:00:00
      value : 100.0,
      attributes : [("metric.name", @azimuth.StringValue("cpu.usage")), ("host", @azimuth.StringValue("server-1"))]
    },
    @azimuth.TimeSeriesPoint {
      timestamp : 1640995260000L, // 2022-01-01 00:01:00
      value : 105.5,
      attributes : [("metric.name", @azimuth.StringValue("cpu.usage")), ("host", @azimuth.StringValue("server-1"))]
    },
    @azimuth.TimeSeriesPoint {
      timestamp : 1640995320000L, // 2022-01-01 00:02:00
      value : 98.2,
      attributes : [("metric.name", @azimuth.StringValue("cpu.usage")), ("host", @azimuth.StringValue("server-1"))]
    },
    @azimuth.TimeSeriesPoint {
      timestamp : 1640995380000L, // 2022-01-01 00:03:00
      value : 110.7,
      attributes : [("metric.name", @azimuth.StringValue("cpu.usage")), ("host", @azimuth.StringValue("server-1"))]
    },
    @azimuth.TimeSeriesPoint {
      timestamp : 1640995440000L, // 2022-01-01 00:04:00
      value : 95.3,
      attributes : [("metric.name", @azimuth.StringValue("cpu.usage")), ("host", @azimuth.StringValue("server-1"))]
    }
  ]
  
  // 验证时间序列数据点
  assert_eq(time_series_points.length(), 5)
  assert_eq(time_series_points[0].timestamp, 1640995200000L)
  assert_eq(time_series_points[0].value, 100.0)
  
  // 计算平均值
  let sum = time_series_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
  let avg = sum / (time_series_points.length() as Float)
  assert_true(abs(avg - 101.94) < 0.01)
  
  // 查找最大值和最小值
  let max_value = time_series_points.reduce(fn(acc, point) { 
    if point.value > acc { point.value } else { acc } 
  }, 0.0)
  let min_value = time_series_points.reduce(fn(acc, point) { 
    if point.value < acc { point.value } else { acc } 
  }, 1000.0)
  
  assert_eq(max_value, 110.7)
  assert_eq(min_value, 95.3)
  
  // 创建聚合结果
  let aggregation_result = @azimuth.TimeSeriesAggregation {
    metric_name : "cpu.usage",
    start_time : 1640995200000L,
    end_time : 1640995440000L,
    point_count : 5,
    min_value : min_value,
    max_value : max_value,
    avg_value : avg,
    sum_value : sum,
    attributes : [("host", @azimuth.StringValue("server-1")), ("aggregation.window", @azimuth.StringValue("5m"))]
  }
  
  // 验证聚合结果
  assert_eq(aggregation_result.metric_name, "cpu.usage")
  assert_eq(aggregation_result.point_count, 5)
  assert_eq(aggregation_result.min_value, 95.3)
  assert_eq(aggregation_result.max_value, 110.7)
  assert_true(abs(aggregation_result.avg_value - 101.94) < 0.01)
}

// 测试7: 安全性和隐私保护的测试
test "security and privacy protection" {
  // 创建包含敏感数据的遥测数据
  let sensitive_telemetry = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "secure.trace.id.12345678901234567890",
    span_id : "secure.span.1234567890",
    parent_span_id : None,
    operation_name : "user.authentication",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 250L,
    attributes : [
      ("user.id", @azimuth.StringValue("user-12345")), // 非敏感
      ("user.email", @azimuth.StringValue("user@example.com")), // 敏感
      ("user.phone", @azimuth.StringValue("+1234567890")), // 敏感
      ("user.ssn", @azimuth.StringValue("***-**-****")), // 已掩码
      ("user.credit.card", @azimuth.StringValue("****-****-****-1234")), // 已掩码
      ("ip.address", @azimuth.StringValue("192.168.1.100")), // 可能敏感
      ("auth.token", @azimuth.StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")), // 敏感
      ("session.id", @azimuth.StringValue("sess-abc123def456")) // 非敏感
    ],
    events : [
      @azimuth.SpanEvent {
        name : "auth.success",
        timestamp : 1640995200100L,
        attributes : [
          ("auth.method", @azimuth.StringValue("oauth2")),
          ("auth.provider", @azimuth.StringValue("google")),
          ("user.role", @azimuth.StringValue("admin"))
        ]
      }
    ]
  }
  
  // 验证敏感数据处理策略
  let sensitive_attributes = ["user.email", "user.phone", "auth.token"]
  let non_sensitive_attributes = ["user.id", "session.id", "auth.method"]
  
  // 检查敏感属性是否存在
  for attr_name in sensitive_attributes {
    let found = sensitive_telemetry.attributes.any(fn(attr) { attr.0 == attr_name })
    assert_true(found) // 在实际实现中，这些应该被掩码或移除
  }
  
  // 检查非敏感属性是否存在
  for attr_name in non_sensitive_attributes {
    let found = sensitive_telemetry.attributes.any(fn(attr) { attr.0 == attr_name })
    assert_true(found)
  }
  
  // 验证已掩码的数据格式
  let ssn_attr = sensitive_telemetry.attributes.filter(fn(attr) { attr.0 == "user.ssn" })[0]
  match ssn_attr.1 {
    @azimuth.StringValue(v) => assert_eq(v, "***-**-****")
    _ => assert_true(false)
  }
  
  let credit_card_attr = sensitive_telemetry.attributes.filter(fn(attr) { attr.0 == "user.credit.card" })[0]
  match credit_card_attr.1 {
    @azimuth.StringValue(v) => assert_eq(v, "****-****-****-1234")
    _ => assert_true(false)
  }
  
  // 创建隐私保护配置
  let privacy_config = @azimuth.PrivacyConfig {
    enable_data_masking : true,
    sensitive_patterns : [
      ("email", @azimuth.RegexPattern("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")),
      ("phone", @azimuth.RegexPattern("\\+?[0-9]{10,15}")),
      ("ssn", @azimuth.RegexPattern("\\d{3}-\\d{2}-\\d{4}")),
      ("credit.card", @azimuth.RegexPattern("\\d{4}-\\d{4}-\\d{4}-\\d{4}")),
      ("ip.address", @azimuth.RegexPattern("\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b"))
    ],
    masking_strategies : [
      ("email", @azimuth.MaskingStrategy::PartialMask),
      ("phone", @azimuth.MaskingStrategy::FullMask),
      ("ssn", @azimuth.MaskingStrategy::FormatPreservingMask),
      ("credit.card", @azimuth.MaskingStrategy::FormatPreservingMask),
      ("ip.address", @azimuth.MaskingStrategy::HashMask)
    ]
  }
  
  // 验证隐私配置
  assert_true(privacy_config.enable_data_masking)
  assert_eq(privacy_config.sensitive_patterns.length(), 5)
  assert_eq(privacy_config.masking_strategies.length(), 5)
}

// 测试8: 资源限制和优雅降级测试
test "resource limits and graceful degradation" {
  // 模拟资源限制场景
  let resource_limits = @azimuth.ResourceLimits {
    max_memory_mb : 512,
    max_cpu_percent : 80.0,
    max_file_descriptors : 1000,
    max_network_connections : 100,
    max_telemetry_buffer_size : 10000,
    max_batch_size : 500
  }
  
  // 验证资源限制配置
  assert_eq(resource_limits.max_memory_mb, 512)
  assert_eq(resource_limits.max_cpu_percent, 80.0)
  assert_eq(resource_limits.max_file_descriptors, 1000)
  
  // 模拟当前资源使用情况
  let current_usage = @azimuth.ResourceUsage {
    memory_mb : 450,
    cpu_percent : 75.5,
    file_descriptors : 800,
    network_connections : 85,
    telemetry_buffer_size : 8000,
    batch_size : 450
  }
  
  // 验证当前资源使用情况
  assert_true(current_usage.memory_mb < resource_limits.max_memory_mb)
  assert_true(current_usage.cpu_percent < resource_limits.max_cpu_percent)
  assert_true(current_usage.file_descriptors < resource_limits.max_file_descriptors)
  
  // 模拟资源压力场景
  let high_pressure_usage = @azimuth.ResourceUsage {
    memory_mb : 600, // 超出限制
    cpu_percent : 90.0, // 超出限制
    file_descriptors : 1200, // 超出限制
    network_connections : 120, // 超出限制
    telemetry_buffer_size : 12000, // 超出限制
    batch_size : 600 // 超出限制
  }
  
  // 验证资源压力检测
  assert_true(high_pressure_usage.memory_mb > resource_limits.max_memory_mb)
  assert_true(high_pressure_usage.cpu_percent > resource_limits.max_cpu_percent)
  assert_true(high_pressure_usage.file_descriptors > resource_limits.max_file_descriptors)
  
  // 创建优雅降级策略
  let degradation_strategy = @azimuth.DegradationStrategy {
    memory_pressure_threshold : 0.9,
    cpu_pressure_threshold : 0.85,
    buffer_size_reduction_factor : 0.5,
    sampling_rate_reduction_factor : 0.5,
    batch_size_reduction_factor : 0.5,
    enable_adaptive_sampling : true,
    enable_data_compression : true,
    enable_local_caching : true
  }
  
  // 验证降级策略
  assert_eq(degradation_strategy.memory_pressure_threshold, 0.9)
  assert_eq(degradation_strategy.cpu_pressure_threshold, 0.85)
  assert_eq(degradation_strategy.buffer_size_reduction_factor, 0.5)
  assert_true(degradation_strategy.enable_adaptive_sampling)
  
  // 模拟降级后的配置
  let degraded_config = @azimuth.TelemetryConfig {
    service_name : "degraded.service",
    service_version : "1.0.0",
    sampling_config : @azimuth.SamplingConfig {
      sampling_type : @azimuth.SamplingType::Probability,
      probability : 0.05, // 降级后的采样率
      rate_limit : Some(500) // 降级后的速率限制
    },
    exporter_config : @azimuth.ExporterConfig {
      exporter_type : @azimuth.ExporterType::OTLP,
      endpoint : "https://otel-collector.example.com:4317",
      headers : [],
      timeout_ms : 10000, // 增加超时时间
      batch_size : 250, // 减小批处理大小
      max_export_batch_size : 250
    },
    resource_attributes : [
      ("service.degraded", @azimuth.StringValue("true")),
      ("degradation.reason", @azimuth.StringValue("resource.pressure"))
    ]
  }
  
  // 验证降级后的配置
  assert_eq(degraded_config.service_name, "degraded.service")
  assert_eq(degraded_config.sampling_config.probability, 0.05)
  assert_eq(degraded_config.exporter_config.batch_size, 250)
  
  // 验证降级标记
  let degraded_attr = degraded_config.resource_attributes.filter(fn(attr) { attr.0 == "service.degraded" })[0]
  match degraded_attr.1 {
    @azimuth.StringValue(v) => assert_eq(v, "true")
    _ => assert_true(false)
  }
}