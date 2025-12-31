// 资源限制测试 - 验证在资源受限环境下的遥测行为
test "telemetry_resource_limits_memory_constraints" {
  // 测试目标：验证内存受限环境下的遥测系统行为
  
  // 1. 模拟内存限制场景
  let memory_limit_mb = 100 // 100MB内存限制
  let current_memory_usage = 75 // 当前使用75MB
  
  // 2. 创建内存感知的遥测配置
  let memory_conscious_attributes = [
    ("system.memory.limit_mb", @azimuth.telemetry.api.common.AttributeValue::int(memory_limit_mb)),
    ("system.memory.current_mb", @azimuth.telemetry.api.common.AttributeValue::int(current_memory_usage)),
    ("system.memory.usage_percent", @azimuth.telemetry.api.common.AttributeValue::float((current_memory_usage.to_double() / memory_limit_mb.to_double()) * 100.0)),
    ("telemetry.optimization.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  // 3. 在内存受限环境下创建轻量级Span
  let lightweight_span = @azimuth.telemetry.api.trace.Span{
    name: "memory-constrained-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "memory=constrained"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200050000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: memory_conscious_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("lightweight")),
      ("attribute.count", @azimuth.telemetry.api.common.AttributeValue::int(5)) // 限制属性数量
    ],
    events: [], // 内存受限时不记录事件
    links: []    // 内存受限时不记录链接
  }
  
  // 4. 验证轻量级Span的内存优化
  assert_eq(lightweight_span.attributes.length(), 5) // 只保留关键属性
  assert_eq(lightweight_span.events.length(), 0)     // 不记录事件以节省内存
  assert_eq(lightweight_span.links.length(), 0)      // 不记录链接以节省内存
  
  // 5. 创建内存使用监控度量
  let memory_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: current_memory_usage.to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("memory.usage.current_mb")),
        ("memory.limit", @azimuth.telemetry.api.common.AttributeValue::int(memory_limit_mb))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: ((current_memory_usage.to_double() / memory_limit_mb.to_double()) * 100.0),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("memory.usage.percent"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: (memory_limit_mb - current_memory_usage).to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("memory.available_mb"))
      ]
    }
  ]
  
  // 6. 验证内存度量
  assert_eq(memory_metrics[0].value, 75.0, "当前内存使用应该为75MB")
  assert_eq(memory_metrics[1].value, 75.0, "内存使用率应该为75%")
  assert_eq(memory_metrics[2].value, 25.0, "可用内存应该为25MB")
  
  // 7. 模拟内存压力下的自适应行为
  let high_memory_usage = 95 // 95MB使用，接近限制
  let adaptive_attributes = [
    ("system.memory.current_mb", @azimuth.telemetry.api.common.AttributeValue::int(high_memory_usage)),
    ("system.memory.usage_percent", @azimuth.telemetry.api.common.AttributeValue::float((high_memory_usage.to_double() / memory_limit_mb.to_double()) * 100.0)),
    ("telemetry.sampling.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.1)), // 降低采样率
    ("telemetry.batch.size", @azimuth.telemetry.api.common.AttributeValue::int(10)),      // 减小批次大小
    ("telemetry.compression.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true)) // 启用压缩
  ]
  
  let adaptive_span = @azimuth.telemetry.api.trace.Span{
    name: "memory-adaptive-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29],
      trace_flags: 0x01,
      trace_state: "memory=adaptive"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200100000000,
    end_time_unix_nanos: Some(1640995200120000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: adaptive_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("minimal")),
      ("attribute.count", @azimuth.telemetry.api.common.AttributeValue::int(3)) // 进一步减少属性
    ],
    events: [],
    links: []
  }
  
  // 8. 验证自适应行为
  assert_eq(adaptive_span.attributes.length(), 3)
  
  let sampling_rate = adaptive_span.attributes.find(fn((key, _) { key == "telemetry.sampling.rate" })
  let batch_size = adaptive_span.attributes.find(fn((key, _) { key == "telemetry.batch.size" })
  let compression_enabled = adaptive_span.attributes.find(fn((key, _) { key == "telemetry.compression.enabled" })
  
  match sampling_rate {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::FloatValue(rate))) => {
      assert(rate < 1.0, "内存压力下应该降低采样率")
    }
    _ => assert(false, "采样率属性应该存在且为浮点类型")
  }
  
  match batch_size {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(size))) => {
      assert(size < 100, "内存压力下应该减小批次大小")
    }
    _ => assert(false, "批次大小属性应该存在且为整数类型")
  }
  
  match compression_enabled {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(enabled))) => {
      assert(enabled, "内存压力下应该启用压缩")
    }
    _ => assert(false, "压缩启用属性应该存在且为布尔类型")
  }
  
  println("✓ 内存限制测试通过")
}

test "telemetry_resource_limits_cpu_constraints" {
  // 测试目标：验证CPU受限环境下的遥测系统行为
  
  // 1. 模拟CPU限制场景
  let cpu_limit_percent = 50.0 // 50% CPU限制
  let current_cpu_usage = 45.0  // 当前使用45% CPU
  
  // 2. 创建CPU感知的遥测配置
  let cpu_conscious_attributes = [
    ("system.cpu.limit_percent", @azimuth.telemetry.api.common.AttributeValue::float(cpu_limit_percent)),
    ("system.cpu.current_percent", @azimuth.telemetry.api.common.AttributeValue::float(current_cpu_usage)),
    ("system.cpu.usage_ratio", @azimuth.telemetry.api.common.AttributeValue::float(current_cpu_usage / cpu_limit_percent)),
    ("telemetry.cpu.optimization", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  // 3. 在CPU受限环境下创建低开销Span
  let low_overhead_span = @azimuth.telemetry.api.trace.Span{
    name: "cpu-constrained-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x40],
      trace_flags: 0x01,
      trace_state: "cpu=constrained"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200030000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: cpu_conscious_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("low_cpu")),
      ("processing.complexity", @azimuth.telemetry.api.common.AttributeValue::string("simple"))
    ],
    events: [], // CPU受限时不记录复杂事件
    links: []    // CPU受限时不处理复杂链接
  }
  
  // 4. 验证低开销Span的CPU优化
  assert_eq(low_overhead_span.events.length(), 0)
  assert_eq(low_overhead_span.links.length(), 0)
  
  // 5. 创建CPU使用监控度量
  let cpu_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: current_cpu_usage,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("cpu.usage.current_percent")),
        ("cpu.limit", @azimuth.telemetry.api.common.AttributeValue::float(cpu_limit_percent))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: (current_cpu_usage / cpu_limit_percent) * 100.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("cpu.usage.ratio_percent"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: cpu_limit_percent - current_cpu_usage,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("cpu.available_percent"))
      ]
    }
  ]
  
  // 6. 验证CPU度量
  assert_eq(cpu_metrics[0].value, 45.0, "当前CPU使用应该为45%")
  assert_eq(cpu_metrics[1].value, 90.0, "CPU使用率比例应该为90%")
  assert_eq(cpu_metrics[2].value, 5.0, "可用CPU应该为5%")
  
  // 7. 模拟高CPU压力下的自适应行为
  let high_cpu_usage = 48.0 // 接近50%限制
  let cpu_adaptive_attributes = [
    ("system.cpu.current_percent", @azimuth.telemetry.api.common.AttributeValue::float(high_cpu_usage)),
    ("system.cpu.usage_ratio", @azimuth.telemetry.api.common.AttributeValue::float(high_cpu_usage / cpu_limit_percent)),
    ("telemetry.async.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true)), // 启用异步处理
    ("telemetry.thread.pool.size", @azimuth.telemetry.api.common.AttributeValue::int(2)), // 减少线程池大小
    ("telemetry.batch.processing", @azimuth.telemetry.api.common.AttributeValue::bool(true)), // 启用批处理
    ("telemetry.compression.level", @azimuth.telemetry.api.common.AttributeValue::int(1)) // 降低压缩级别
  ]
  
  let cpu_adaptive_span = @azimuth.telemetry.api.trace.Span{
    name: "cpu-adaptive-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x50, 0x51],
      trace_flags: 0x01,
      trace_state: "cpu=adaptive"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200100000000,
    end_time_unix_nanos: Some(1640995200115000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: cpu_adaptive_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("cpu_efficient"))
    ],
    events: [],
    links: []
  }
  
  // 8. 验证CPU自适应行为
  let async_enabled = cpu_adaptive_span.attributes.find(fn((key, _) { key == "telemetry.async.enabled" })
  let thread_pool_size = cpu_adaptive_span.attributes.find(fn((key, _) { key == "telemetry.thread.pool.size" })
  let batch_processing = cpu_adaptive_span.attributes.find(fn((key, _) { key == "telemetry.batch.processing" })
  
  match async_enabled {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(enabled))) => {
      assert(enabled, "CPU压力下应该启用异步处理")
    }
    _ => assert(false, "异步处理属性应该存在且为布尔类型")
  }
  
  match thread_pool_size {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(size))) => {
      assert(size <= 4, "CPU压力下应该减少线程池大小")
    }
    _ => assert(false, "线程池大小属性应该存在且为整数类型")
  }
  
  match batch_processing {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(enabled))) => {
      assert(enabled, "CPU压力下应该启用批处理")
    }
    _ => assert(false, "批处理属性应该存在且为布尔类型")
  }
  
  println("✓ CPU限制测试通过")
}

test "telemetry_resource_limits_storage_constraints" {
  // 测试目标：验证存储受限环境下的遥测系统行为
  
  // 1. 模拟存储限制场景
  let storage_limit_mb = 500 // 500MB存储限制
  let current_storage_usage = 450 // 当前使用450MB
  
  // 2. 创建存储感知的遥测配置
  let storage_conscious_attributes = [
    ("system.storage.limit_mb", @azimuth.telemetry.api.common.AttributeValue::int(storage_limit_mb)),
    ("system.storage.current_mb", @azimuth.telemetry.api.common.AttributeValue::int(current_storage_usage)),
    ("system.storage.usage_percent", @azimuth.telemetry.api.common.AttributeValue::float((current_storage_usage.to_double() / storage_limit_mb.to_double()) * 100.0)),
    ("telemetry.storage.optimization", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  // 3. 在存储受限环境下创建存储优化Span
  let storage_optimized_span = @azimuth.telemetry.api.trace.Span{
    name: "storage-constrained-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99],
      span_id: [0x55, 0x56, 0x57, 0x58, 0x59, 0x60, 0x61, 0x62],
      trace_flags: 0x01,
      trace_state: "storage=constrained"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200040000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: storage_conscious_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("storage_efficient")),
      ("data.retention.hours", @azimuth.telemetry.api.common.AttributeValue::int(24)) // 缩短数据保留期
    ],
    events: [], // 存储受限时不记录事件
    links: []    // 存储受限时不记录链接
  }
  
  // 4. 验证存储优化Span
  assert_eq(storage_optimized_span.events.length(), 0)
  assert_eq(storage_optimized_span.links.length(), 0)
  
  // 5. 创建存储使用监控度量
  let storage_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: current_storage_usage.to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("storage.usage.current_mb")),
        ("storage.limit", @azimuth.telemetry.api.common.AttributeValue::int(storage_limit_mb))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: ((current_storage_usage.to_double() / storage_limit_mb.to_double()) * 100.0),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("storage.usage.percent"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: (storage_limit_mb - current_storage_usage).to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("storage.available_mb"))
      ]
    }
  ]
  
  // 6. 验证存储度量
  assert_eq(storage_metrics[0].value, 450.0, "当前存储使用应该为450MB")
  assert_eq(storage_metrics[1].value, 90.0, "存储使用率应该为90%")
  assert_eq(storage_metrics[2].value, 50.0, "可用存储应该为50MB")
  
  // 7. 模拟存储压力下的数据清理策略
  let critical_storage_usage = 480 // 接近500MB限制
  let storage_cleanup_attributes = [
    ("system.storage.current_mb", @azimuth.telemetry.api.common.AttributeValue::int(critical_storage_usage)),
    ("system.storage.usage_percent", @azimuth.telemetry.api.common.AttributeValue::float((critical_storage_usage.to_double() / storage_limit_mb.to_double()) * 100.0)),
    ("telemetry.cleanup.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("telemetry.cleanup.threshold_percent", @azimuth.telemetry.api.common.AttributeValue::float(85.0)),
    ("telemetry.retention.days", @azimuth.telemetry.api.common.AttributeValue::int(7)), // 进一步缩短保留期
    ("telemetry.compression.algorithm", @azimuth.telemetry.api.common.AttributeValue::string("lz4")),
    ("telemetry.batch.compression", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  let storage_cleanup_span = @azimuth.telemetry.api.trace.Span{
    name: "storage-cleanup-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99],
      span_id: [0x66, 0x67, 0x68, 0x69, 0x70, 0x71, 0x72, 0x73],
      trace_flags: 0x01,
      trace_state: "storage=cleanup"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200100000000,
    end_time_unix_nanos: Some(1640995200180000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: Some("Storage cleanup completed successfully"),
    attributes: storage_cleanup_attributes + [
      ("span.optimization", @azimuth.telemetry.api.common.AttributeValue::string("cleanup_mode")),
      ("data.freed_mb", @azimuth.telemetry.api.common.AttributeValue::int(50)),
      ("cleanup.duration_ms", @azimuth.telemetry.api.common.AttributeValue::int(8000))
    ],
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "cleanup.started",
        timestamp_unix_nanos: 1640995200110000000,
        attributes: [
          ("cleanup.strategy", @azimuth.telemetry.api.common.AttributeValue::string("old_data_first"))
        ]
      },
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "cleanup.completed",
        timestamp_unix_nanos: 1640995200170000000,
        attributes: [
          ("files.deleted", @azimuth.telemetry.api.common.AttributeValue::int(1250)),
          ("space.freed_mb", @azimuth.telemetry.api.common.AttributeValue::int(50))
        ]
      }
    ],
    links: []
  }
  
  // 8. 验证存储清理行为
  assert_eq(storage_cleanup_span.events.length(), 2)
  
  let cleanup_enabled = storage_cleanup_span.attributes.find(fn((key, _) { key == "telemetry.cleanup.enabled" })
  let retention_days = storage_cleanup_span.attributes.find(fn((key, _) { key == "telemetry.retention.days" })
  let compression_algorithm = storage_cleanup_span.attributes.find(fn((key, _) { key == "telemetry.compression.algorithm" })
  
  match cleanup_enabled {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(enabled))) => {
      assert(enabled, "存储压力下应该启用清理")
    }
    _ => assert(false, "清理启用属性应该存在且为布尔类型")
  }
  
  match retention_days {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(days))) => {
      assert(days <= 7, "存储压力下应该缩短数据保留期")
    }
    _ => assert(false, "保留期属性应该存在且为整数类型")
  }
  
  match compression_algorithm {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(algorithm))) => {
      assert(algorithm != "", "存储压力下应该启用压缩算法")
    }
    _ => assert(false, "压缩算法属性应该存在且为字符串类型")
  }
  
  // 9. 验证清理效果
  let data_freed = storage_cleanup_span.attributes.find(fn((key, _) { key == "data.freed_mb" })
  match data_freed {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(freed))) => {
      assert(freed > 0, "应该释放存储空间")
    }
    _ => assert(false, "释放空间属性应该存在且为整数类型")
  }
  
  println("✓ 存储限制测试通过")
}