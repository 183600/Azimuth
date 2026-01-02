// Azimuth Additional Integration Tests
// 集成测试和边缘场景测试用例

test "telemetry data validation and sanitization" {
  // 测试遥测数据的验证和清理
  let invalid_telemetry = @azimuth.TelemetryData {
    timestamp : -1L, // 无效时间戳
    trace_id : "", // 空trace ID
    span_id : "invalid", // 无效span ID
    parent_span_id : Some(""),
    operation_name : "", // 空操作名
    status : @azimuth.SpanStatus::Ok,
    duration_ms : -100L, // 负持续时间
    attributes : [
      ("", @azimuth.StringValue("empty_key")), // 空键
      ("valid.key", @azimuth.StringValue("")),
      ("special.chars!@#", @azimuth.StringValue("value"))
    ],
    events : []
  }
  
  // 验证数据清理功能
  let sanitized = @azimuth.sanitize_telemetry_data(invalid_telemetry)
  
  // 验证时间戳修正
  assert_true(sanitized.timestamp >= 0L)
  
  // 验证ID生成
  assert_true(sanitized.trace_id.length() > 0)
  assert_true(sanitized.span_id.length() > 0)
  
  // 验证持续时间修正
  assert_true(sanitized.duration_ms >= 0L)
  
  // 验证属性清理
  let valid_attrs = sanitized.attributes.filter(fn(attr) { 
    attr.0.length() > 0 && attr.0.match_regex("^[a-zA-Z0-9._-]+$")
  })
  assert_true(valid_attrs.length() <= sanitized.attributes.length())
}

test "concurrent telemetry processing" {
  // 测试并发遥测数据处理
  let telemetry_batch = [
    @azimuth.TelemetryData {
      timestamp : 1640995200000L,
      trace_id : "trace1",
      span_id : "span1",
      parent_span_id : None,
      operation_name : "operation1",
      status : @azimuth.SpanStatus::Ok,
      duration_ms : 100L,
      attributes : [],
      events : []
    },
    @azimuth.TelemetryData {
      timestamp : 1640995200100L,
      trace_id : "trace2",
      span_id : "span2",
      parent_span_id : None,
      operation_name : "operation2",
      status : @azimuth.SpanStatus::Ok,
      duration_ms : 150L,
      attributes : [],
      events : []
    },
    @azimuth.TelemetryData {
      timestamp : 1640995200200L,
      trace_id : "trace3",
      span_id : "span3",
      parent_span_id : None,
      operation_name : "operation3",
      status : @azimuth.SpanStatus::Error,
      duration_ms : 200L,
      attributes : [],
      events : []
    }
  ]
  
  // 并发处理遥测批次
  let processed_batch = @azimuth.process_telemetry_batch_concurrent(telemetry_batch, 4)
  
  // 验证处理结果
  assert_eq(processed_batch.length(), telemetry_batch.length())
  
  // 验证时间顺序保持
  for i in 1..processed_batch.length() {
    assert_true(processed_batch[i-1].timestamp <= processed_batch[i].timestamp)
  }
  
  // 验证错误状态统计
  let error_count = processed_batch.filter(fn(data) { 
    match data.status {
      @azimuth.SpanStatus::Error => true
      _ => false
    }
  }).length()
  assert_eq(error_count, 1)
}

test "trace sampling strategies" {
  // 测试追踪采样策略
  let test_trace_id = "1234567890abcdef1234567890abcdef"
  
  // 测试概率采样
  let probability_sampler = @azimuth.ProbabilitySampler { probability : 0.5 }
  let sampled_results = []
  
  for i in 0..=100 {
    let should_sample = @azimuth.should_sample_trace(probability_sampler, test_trace_id)
    sampled_results = sampled_results.push(should_sample)
  }
  
  let sampled_count = sampled_results.filter(fn(x) { x }).length()
  let sample_rate = (sampled_count as Float) / (sampled_results.length() as Float)
  
  // 验证采样率接近预期概率（允许10%误差）
  assert_true(abs(sample_rate - 0.5) < 0.1)
  
  // 测试确定性采样 - 相同trace ID应产生相同结果
  let deterministic_sampler = @azimuth.DeterministicSampler { sample_rate : 0.1 }
  let result1 = @azimuth.should_sample_trace(deterministic_sampler, test_trace_id)
  let result2 = @azimuth.should_sample_trace(deterministic_sampler, test_trace_id)
  assert_eq(result1, result2)
  
  // 测试不同trace ID的采样结果
  let other_trace_id = "fedcba0987654321fedcba0987654321"
  let result3 = @azimuth.should_sample_trace(deterministic_sampler, other_trace_id)
  // 结果可能相同或不同，但应该是确定性的
  let result3_again = @azimuth.should_sample_trace(deterministic_sampler, other_trace_id)
  assert_eq(result3, result3_again)
}

test "metric aggregation performance optimization" {
  // 测试指标聚合性能优化
  let metrics_data = []
  
  // 生成大量指标数据
  for i in 0..=1000 {
    let metric = @azimuth.MetricPoint {
      timestamp : 1640995200000L + (i as Long),
      name : "response.time",
      value : (i % 100) as Float,
      attributes : [
        ("service", @azimuth.StringValue("api-gateway")),
        ("endpoint", @azimuth.StringValue("/api/users/" + (i % 10).to_string())),
        ("method", @azimuth.StringValue(if i % 2 == 0 { "GET" } else { "POST" }))
      ]
    }
    metrics_data = metrics_data.push(metric)
  }
  
  // 测试时间窗口聚合
  let windowed_aggregates = @azimuth.aggregate_metrics_time_window(
    metrics_data, 
    60000L // 1分钟窗口
  )
  
  // 验证聚合结果
  assert_true(windowed_aggregates.length() > 0)
  
  // 验证每个聚合窗口包含正确的指标点
  for aggregate in windowed_aggregates {
    assert_true(aggregate.count > 0)
    assert_true(aggregate.avg >= aggregate.min && aggregate.avg <= aggregate.max)
    assert_eq(aggregate.sum, aggregate.avg * (aggregate.count as Float))
  }
  
  // 测试属性维度聚合
  let dimension_aggregates = @azimuth.aggregate_metrics_by_dimensions(
    metrics_data,
    ["service", "method"]
  )
  
  // 验证维度聚合结果
  assert_true(dimension_aggregates.length() > 0)
  
  // 验证每个维度组合的聚合
  for dim_aggregate in dimension_aggregates {
    assert_true(dim_aggregate.dimensions.length() == 2)
    assert_true(dim_aggregate.count > 0)
  }
}

test "context propagation with baggage limits" {
  // 测试带限制的上下文传播
  let original_context = @azimuth.Context {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "parent1234567890",
    baggage : [
      ("user.id", "user123"),
      ("request.id", "req-456"),
      ("session.id", "sess-789"),
      ("tenant.id", "tenant-001"),
      ("region", "us-west-2"),
      ("az", "usw2-az1"),
      ("instance.id", "i-1234567890abcdef0"),
      ("version", "v1.2.3"),
      ("build.number", "build-456"),
      ("deployment.id", "deploy-789")
    ],
    entries : []
  }
  
  // 测试baggage数量限制
  let limited_context = @azimuth.limit_context_baggage(original_context, 5)
  assert_true(limited_context.baggage.length() <= 5)
  
  // 验证优先级保留（重要项优先保留）
  let priority_keys = ["user.id", "request.id", "session.id", "tenant.id", "region"]
  for key in priority_keys {
    let found = limited_context.baggage.some(fn(item) { item.0 == key })
    assert_true(found)
  }
  
  // 测试baggage大小限制
  let large_value_context = @azimuth.Context {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "parent1234567890",
    baggage : [
      ("small", "value"),
      ("large", "x".repeat(1000)) // 超大值
    ],
    entries : []
  }
  
  let size_limited_context = @azimuth.limit_context_baggage_size(large_value_context, 100)
  let total_size = size_limited_context.baggage.reduce(fn(acc, item) { 
    acc + item.0.length() + item.1.length() 
  }, 0)
  
  assert_true(total_size <= 100)
}

test "telemetry data compression efficiency" {
  // 测试遥测数据压缩效率
  let large_telemetry = @azimuth.TelemetryData {
    timestamp : 1640995200000L,
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "span1234567890",
    parent_span_id : Some("parent1234567890"),
    operation_name : "very.long.operation.name.with.many.dots.and.underscores",
    status : @azimuth.SpanStatus::Ok,
    duration_ms : 1500L,
    attributes : [
      ("http.method", @azimuth.StringValue("GET")),
      ("http.url", @azimuth.StringValue("https://api.example.com/v1/users/123456789/profile?include=detailed&format=json")),
      ("http.user_agent", @azimuth.StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")),
      ("http.headers", @azimuth.StringValue("Authorization: Bearer very.long.jwt.token.with.many.claims.and.signature;Content-Type: application/json;Accept: application/json;X-Custom-Header: custom-value-with-long-content")),
      ("db.query", @azimuth.StringValue("SELECT u.*, p.*, s.* FROM users u JOIN profiles p ON u.id = p.user_id JOIN settings s ON u.id = s.user_id WHERE u.id = $1 AND u.status = 'active' AND p.verified = true ORDER BY u.created_at DESC LIMIT 1")),
      ("cache.keys", @azimuth.StringValue("user:123456789:profile,user:123456789:settings,user:123456789:permissions,session:abc123def456,auth:token789")),
      ("custom.json.payload", @azimuth.StringValue("{\"user\":{\"id\":123456789,\"name\":\"John Doe\",\"email\":\"john.doe@example.com\",\"roles\":[\"user\",\"admin\",\"moderator\"],\"preferences\":{\"theme\":\"dark\",\"language\":\"en-US\",\"timezone\":\"America/Los_Angeles\",\"notifications\":{\"email\":true,\"push\":false,\"sms\":true}},\"metadata\":{\"source\":\"web\",\"campaign\":\"summer2023\",\"referral\":\"friend123\"}}}"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "cache.query",
        timestamp : 1640995200050L,
        attributes : [
          ("cache.operation", @azimuth.StringValue("get_multi")),
          ("cache.keys_requested", @azimuth.StringValue("5")),
          ("cache.keys_found", @azimuth.StringValue("3")),
          ("cache.miss_ratio", @azimuth.StringValue("0.4"))
        ]
      },
      @azimuth.SpanEvent {
        name : "database.query",
        timestamp : 1640995200100L,
        attributes : [
          ("db.type", @azimuth.StringValue("postgresql")),
          ("db.statement", @azimuth.StringValue("SELECT * FROM users WHERE id = $1")),
          ("db.rows_affected", @azimuth.StringValue("1")),
          ("db.execution_time_ms", @azimuth.StringValue("25"))
        ]
      }
    ]
  }
  
  // 测试序列化大小
  let serialized = @azimuth.serialize_telemetry(large_telemetry)
  let original_size = serialized.length()
  
  // 测试压缩后大小
  let compressed = @azimuth.compress_telemetry_data(large_telemetry)
  let compressed_size = compressed.length()
  
  // 验证压缩效率（应该至少减少30%）
  let compression_ratio = (compressed_size as Float) / (original_size as Float)
  assert_true(compression_ratio < 0.7)
  
  // 验证压缩和解压缩的往返一致性
  let decompressed = @azimuth.decompress_telemetry_data(compressed)
  let round_trip_serialized = @azimuth.serialize_telemetry(decompressed)
  assert_eq(serialized, round_trip_serialized)
}

test "telemetry data retention and cleanup" {
  // 测试遥测数据保留和清理
  let current_time = 1640995200000L // 2022-01-01 00:00:00
  let retention_days = 30
  let retention_ms = (retention_days * 24 * 60 * 60 * 1000) as Long
  
  let old_telemetry_data = [
    @azimuth.TelemetryData {
      timestamp : current_time - retention_ms - 86400000L, // 超过保留期1天
      trace_id : "old_trace1",
      span_id : "old_span1",
      parent_span_id : None,
      operation_name : "old_operation1",
      status : @azimuth.SpanStatus::Ok,
      duration_ms : 100L,
      attributes : [],
      events : []
    },
    @azimuth.TelemetryData {
      timestamp : current_time - retention_ms + 86400000L, // 在保留期内1天
      trace_id : "recent_trace1",
      span_id : "recent_span1",
      parent_span_id : None,
      operation_name : "recent_operation1",
      status : @azimuth.SpanStatus::Ok,
      duration_ms : 150L,
      attributes : [],
      events : []
    },
    @azimuth.TelemetryData {
      timestamp : current_time - (retention_ms / 2), // 在保留期中点
      trace_id : "recent_trace2",
      span_id : "recent_span2",
      parent_span_id : None,
      operation_name : "recent_operation2",
      status : @azimuth.SpanStatus::Error,
      duration_ms : 200L,
      attributes : [],
      events : []
    }
  ]
  
  // 执行清理操作
  let cleanup_result = @azimuth.cleanup_old_telemetry_data(old_telemetry_data, current_time, retention_days)
  
  // 验证清理结果
  assert_eq(cleanup_result.removed_count, 1) // 只有一个过期记录
  assert_eq(cleanup_result.remaining_data.length(), 2)
  
  // 验证剩余数据都在保留期内
  for telemetry in cleanup_result.remaining_data {
    let age_ms = current_time - telemetry.timestamp
    assert_true(age_ms <= retention_ms)
  }
  
  // 验证特定记录的保留
  let remaining_trace_ids = cleanup_result.remaining_data.map(fn(data) { data.trace_id })
  assert_true(remaining_trace_ids.contains("recent_trace1"))
  assert_true(remaining_trace_ids.contains("recent_trace2"))
  assert_false(remaining_trace_ids.contains("old_trace1"))
}

test "telemetry system resource monitoring" {
  // 测试遥测系统资源监控
  let system_metrics = @azimuth.SystemMetrics {
    timestamp : 1640995200000L,
    memory_metrics : @azimuth.MemoryMetrics {
      total_heap : 1073741824, // 1GB
      used_heap : 536870912,   // 512MB
      non_heap : 134217728,    // 128MB
      direct_memory : 67108864 // 64MB
    },
    cpu_metrics : @azimuth.CpuMetrics {
      process_cpu_usage : 0.25,  // 25%
      system_cpu_usage : 0.65,   // 65%
      available_processors : 8
    },
    gc_metrics : @azimuth.GcMetrics {
      young_gc_count : 100,
      young_gc_time : 5000,      // 5 seconds
      old_gc_count : 10,
      old_gc_time : 2000,        // 2 seconds
      total_gc_time : 7000       // 7 seconds
    },
    thread_metrics : @azimuth.ThreadMetrics {
      thread_count : 45,
      daemon_thread_count : 15,
      peak_thread_count : 50
    }
  }
  
  // 验证内存使用率计算
  let heap_usage_ratio = (system_metrics.memory_metrics.used_heap as Float) / 
                        (system_metrics.memory_metrics.total_heap as Float)
  assert_eq(heap_usage_ratio, 0.5) // 50%
  
  // 验证GC效率计算
  let gc_efficiency = 1.0 - ((system_metrics.gc_metrics.total_gc_time as Float) / 
                           (system_metrics.timestamp as Float / 1000.0))
  assert_true(gc_efficiency > 0.0 && gc_efficiency <= 1.0)
  
  // 测试资源告警阈值
  let memory_alert = @azimuth.check_memory_alert(system_metrics.memory_metrics, 0.8) // 80%阈值
  assert_false(memory_alert.triggered) // 当前使用率50%，未触发
  
  let cpu_alert = @azimuth.check_cpu_alert(system_metrics.cpu_metrics, 0.5) // 50%阈值
  assert_true(cpu_alert.triggered) // 系统CPU使用率65%，触发告警
  
  // 测试资源趋势分析
  let metrics_history = [
    system_metrics,
    @azimuth.SystemMetrics { 
      timestamp : system_metrics.timestamp + 60000L, // 1分钟后
      memory_metrics : @azimuth.MemoryMetrics {
        total_heap : 1073741824,
        used_heap : 644245094, // 614MB，增长趋势
        non_heap : 134217728,
        direct_memory : 67108864
      },
      cpu_metrics : system_metrics.cpu_metrics,
      gc_metrics : system_metrics.gc_metrics,
      thread_metrics : system_metrics.thread_metrics
    }
  ]
  
  let memory_trend = @azimuth.analyze_memory_trend(metrics_history)
  match memory_trend {
    @azimuth.TrendDirection::Increasing => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证趋势幅度
  let memory_growth_rate = @azimuth.calculate_memory_growth_rate(metrics_history)
  assert_true(memory_growth_rate > 0.0) // 正增长
}