// 资源限制和降级综合测试 - 验证遥测系统在资源受限情况下的降级处理能力

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, Span}
use azimuth.telemetry.api.metrics.{MetricType, Instrument, Measurement}
use azimuth.telemetry.api.logs.{LogRecord, Severity, Logger}
use azimuth.telemetry.api.context.{Context, Baggage, ContextKey}
use azimuth.telemetry.api.propagation.{TextMapPropagator, Propagator}

test "memory_pressure_degradation" {
  // 测试内存压力下的降级处理
  
  let trace_id = "trace_memory_pressure_123"
  let memory_pressure_time = 1641012000L
  
  // 1. 正常内存状态下的操作
  let normal_memory_span = Span::{
    trace_id: trace_id,
    span_id: "span_normal_memory",
    parent_span_id: None,
    name: "operation_with_normal_memory",
    kind: SpanKind::Server,
    start_time: memory_pressure_time - 2000L,
    end_time: Some(memory_pressure_time - 1500L),
    status: StatusCode::Ok,
    attributes: [
      ("memory.usage.mb", AttributeValue::float(512.0)),
      ("memory.available.mb", AttributeValue::float(512.0)),
      ("telemetry.mode", AttributeValue::string("full")),
      ("span.attributes.count", AttributeValue::int(10L)),
      ("events.count", AttributeValue::int(3L)),
      ("sampling.enabled", AttributeValue::bool(true))
    ],
    events: [
      ("detailed_operation_start", memory_pressure_time - 1900L, [
        ("operation.details", AttributeValue::string("Full telemetry collection with all attributes")),
        ("memory.allocated.mb", AttributeValue::float(2.5))
      ]),
      ("performance_metrics_collected", memory_pressure_time - 1700L, [
        ("cpu.usage", AttributeValue::float(45.2)),
        ("gc.collections", AttributeValue::int(5L))
      ])
    ],
    links: []
  }
  
  // 2. 内存压力检测和降级
  let memory_pressure_span = Span::{
    trace_id: trace_id,
    span_id: "span_memory_pressure",
    parent_span_id: Some("span_normal_memory"),
    name: "operation_under_memory_pressure",
    kind: SpanKind::Server,
    start_time: memory_pressure_time,
    end_time: Some(memory_pressure_time + 1000L),
    status: StatusCode::Ok,
    attributes: [
      ("memory.usage.mb", AttributeValue::float(1536.0)), // 75%内存使用
      ("memory.available.mb", AttributeValue::float(512.0)),
      ("telemetry.mode", AttributeValue::string("degraded")),
      ("degradation.level", AttributeValue::string("medium")),
      ("span.attributes.count", AttributeValue::int(5L)), // 减少属性
      ("events.count", AttributeValue::int(1L)), // 减少事件
      ("sampling.enabled", AttributeValue::bool(true)),
      ("sampling.rate.reduced", AttributeValue::float(0.5)) // 降低采样率
    ],
    events: [
      ("memory_pressure_detected", memory_pressure_time + 200L, [
        ("pressure.threshold", AttributeValue::float(0.75)),
        ("mitigation.activated", AttributeValue::bool(true)),
        ("degradation.strategy", AttributeValue::string("reduce_telemetry_overhead"))
      ]),
      ("telemetry_degradation_applied", memory_pressure_time + 500L, [
        ("attributes.reduced", AttributeValue::bool(true)),
        ("events.filtered", AttributeValue::bool(true)),
        ("sampling.adjusted", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 3. 严重内存压力下的进一步降级
  let severe_pressure_span = Span::{
    trace_id: trace_id,
    span_id: "span_severe_memory_pressure",
    parent_span_id: Some("span_memory_pressure"),
    name: "operation_under_severe_memory_pressure",
    kind: SpanKind::Server,
    start_time: memory_pressure_time + 2000L,
    end_time: Some(memory_pressure_time + 2500L),
    status: StatusCode::Ok,
    attributes: [
      ("memory.usage.mb", AttributeValue::float(1792.0)), // 87.5%内存使用
      ("memory.available.mb", AttributeValue::float(256.0)),
      ("telemetry.mode", AttributeValue::string("minimal")),
      ("degradation.level", AttributeValue::string("high")),
      ("span.attributes.count", AttributeValue::int(2L)), // 最少属性
      ("events.count", AttributeValue::int(0L)), // 无事件
      ("sampling.enabled", AttributeValue::bool(true)),
      ("sampling.rate.reduced", AttributeValue::float(0.1)) // 极低采样率
    ],
    events: [], // 严重压力下不记录事件
    links: []
  }
  
  // 4. 内存恢复后的正常操作
  let memory_recovered_span = Span::{
    trace_id: trace_id,
    span_id: "span_memory_recovered",
    parent_span_id: Some("span_severe_memory_pressure"),
    name: "operation_after_memory_recovery",
    kind: SpanKind::Server,
    start_time: memory_pressure_time + 4000L,
    end_time: Some(memory_pressure_time + 4500L),
    status: StatusCode::Ok,
    attributes: [
      ("memory.usage.mb", AttributeValue::float(768.0)), // 37.5%内存使用
      ("memory.available.mb", AttributeValue::float(1280.0)),
      ("telemetry.mode", AttributeValue::string("full")),
      ("degradation.level", AttributeValue::string("none")),
      ("span.attributes.count", AttributeValue::int(10L)), // 恢复完整属性
      ("events.count", AttributeValue::int(3L)), // 恢复事件记录
      ("sampling.enabled", AttributeValue::bool(true)),
      ("sampling.rate.restored", AttributeValue::float(1.0)) // 恢复正常采样
    ],
    events: [
      ("memory_pressure_recovered", memory_pressure_time + 4100L, [
        ("recovery.threshold", AttributeValue::float(0.4)),
        ("telemetry.restored", AttributeValue::bool(true)),
        ("full.functionality.resumed", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 5. 记录内存压力指标
  let memory_metrics = [
    Measurement::{
      instrument: Instrument::gauge("memory_usage_percentage", "Memory usage percentage", "percent"),
      value: 25.0, // 正常状态
      attributes: [
        ("telemetry.mode", AttributeValue::string("full")),
        ("degradation.level", AttributeValue::string("none"))
      ],
      time: memory_pressure_time - 1500L
    },
    Measurement::{
      instrument: Instrument::gauge("memory_usage_percentage", "Memory usage percentage", "percent"),
      value: 75.0, // 内存压力
      attributes: [
        ("telemetry.mode", AttributeValue::string("degraded")),
        ("degradation.level", AttributeValue::string("medium"))
      ],
      time: memory_pressure_time + 1000L
    },
    Measurement::{
      instrument: Instrument::gauge("memory_usage_percentage", "Memory usage percentage", "percent"),
      value: 87.5, // 严重压力
      attributes: [
        ("telemetry.mode", AttributeValue::string("minimal")),
        ("degradation.level", AttributeValue::string("high"))
      ],
      time: memory_pressure_time + 2500L
    },
    Measurement::{
      instrument: Instrument::counter("telemetry_degradations_total", "Total telemetry degradations", "count"),
      value: 2.0,
      attributes: [
        ("degradation.reason", AttributeValue::string("memory_pressure")),
        ("recovery.successful", AttributeValue::bool(true))
      ],
      time: memory_pressure_time + 4500L
    }
  ]
  
  // 6. 记录内存压力日志
  let memory_pressure_log = LogRecord::{
    timestamp: memory_pressure_time + 1000L,
    observed_timestamp: Some(memory_pressure_time + 1001L),
    severity: Some(Severity::Warn),
    severity_text: Some("WARN"),
    body: Some("Memory pressure detected, telemetry degradation activated"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("memory.usage.percentage", AttributeValue::float(75.0)),
      ("degradation.level", AttributeValue::string("medium")),
      ("telemetry.overhead.reduced", AttributeValue::bool(true)),
      ("user.impact", AttributeValue::string("minimal"))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_memory_pressure"),
    trace_flags: Some(1)
  }
  
  // 7. 验证内存压力降级的正确性
  assert_eq(normal_memory_span.status, StatusCode::Ok)
  assert_eq(memory_pressure_span.status, StatusCode::Ok)
  assert_eq(severe_pressure_span.status, StatusCode::Ok)
  assert_eq(memory_recovered_span.status, StatusCode::Ok)
  
  // 验证降级级别的正确性
  let normal_mode = normal_memory_span.attributes[2]
  match normal_mode.1 {
    StringValue(mode) => assert_eq(mode, "full")
    _ => assert_eq(false, true)
  }
  
  let degraded_mode = memory_pressure_span.attributes[2]
  match degraded_mode.1 {
    StringValue(mode) => assert_eq(mode, "degraded")
    _ => assert_eq(false, true)
  }
  
  let minimal_mode = severe_pressure_span.attributes[2]
  match minimal_mode.1 {
    StringValue(mode) => assert_eq(mode, "minimal")
    _ => assert_eq(false, true)
  }
  
  // 验证属性数量的变化
  let normal_attr_count = normal_memory_span.attributes[3]
  match normal_attr_count.1 {
    IntValue(count) => assert_eq(count, 10L)
    _ => assert_eq(false, true)
  }
  
  let minimal_attr_count = severe_pressure_span.attributes[3]
  match minimal_attr_count.1 {
    IntValue(count) => assert_eq(count, 2L)
    _ => assert_eq(false, true)
  }
  
  // 验证事件数量的变化
  let normal_event_count = normal_memory_span.attributes[4]
  match normal_event_count.1 {
    IntValue(count) => assert_eq(count, 3L)
    _ => assert_eq(false, true)
  }
  
  let severe_event_count = severe_pressure_span.attributes[4]
  match severe_event_count.1 {
    IntValue(count) => assert_eq(count, 0L)
    _ => assert_eq(false, true)
  }
}

test "cpu_throttling_adaptation" {
  // 测试CPU限制下的自适应处理
  
  let trace_id = "trace_cpu_throttling_456"
  let cpu_throttling_time = 1641013000L
  
  // 1. 正常CPU状态下的操作
  let normal_cpu_span = Span::{
    trace_id: trace_id,
    span_id: "span_normal_cpu",
    parent_span_id: None,
    name: "operation_with_normal_cpu",
    kind: SpanKind::Server,
    start_time: cpu_throttling_time - 2000L,
    end_time: Some(cpu_throttling_time - 1000L),
    status: StatusCode::Ok,
    attributes: [
      ("cpu.usage.percentage", AttributeValue::float(35.0)),
      ("processing.mode", AttributeValue::string("real_time")),
      ("batch.processing.enabled", AttributeValue::bool(true)),
      ("async.processing.enabled", AttributeValue::bool(true)),
      ("complex.calculations.enabled", AttributeValue::bool(true)),
      ("metric.aggregation.enabled", AttributeValue::bool(true))
    ],
    events: [
      ("complex_processing_started", cpu_throttling_time - 1800L, [
        ("processing.type", AttributeValue::string("real_time_aggregation")),
        ("cpu.allocated", AttributeValue::float(40.0))
      ])
    ],
    links: []
  }
  
  // 2. CPU限制检测和自适应
  let cpu_throttled_span = Span::{
    trace_id: trace_id,
    span_id: "span_cpu_throttled",
    parent_span_id: Some("span_normal_cpu"),
    name: "operation_with_cpu_throttling",
    kind: SpanKind::Server,
    start_time: cpu_throttling_time,
    end_time: Some(cpu_throttling_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("cpu.usage.percentage", AttributeValue::float(85.0)), // CPU高使用率
      ("processing.mode", AttributeValue::string("adaptive")),
      ("throttling.level", AttributeValue::string("medium")),
      ("batch.processing.enabled", AttributeValue::bool(true)), // 保持批处理
      ("async.processing.enabled", AttributeValue::bool(false)), // 禁用异步处理
      ("complex.calculations.enabled", AttributeValue::bool(false)), // 禁用复杂计算
      ("metric.aggregation.enabled", AttributeValue::bool(true)), // 保持聚合
      ("processing.latency.increased", AttributeValue::bool(true))
    ],
    events: [
      ("cpu_throttling_detected", cpu_throttling_time + 200L, [
        ("throttling.threshold", AttributeValue::float(0.8)),
        ("adaptation.strategy", AttributeValue::string("reduce_complexity"))
      ]),
      ("processing_mode_changed", cpu_throttling_time + 800L, [
        ("previous.mode", AttributeValue::string("real_time")),
        ("new.mode", AttributeValue::string("batch_priority")),
        ("complexity.reduced", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 3. 严重CPU限制下的最小化处理
  let severe_cpu_span = Span::{
    trace_id: trace_id,
    span_id: "span_severe_cpu_throttling",
    parent_span_id: Some("span_cpu_throttled"),
    name: "operation_with_severe_cpu_throttling",
    kind: SpanKind::Server,
    start_time: cpu_throttling_time + 3000L,
    end_time: Some(cpu_throttling_time + 4000L),
    status: StatusCode::Ok,
    attributes: [
      ("cpu.usage.percentage", AttributeValue::float(95.0)), // 极高CPU使用率
      ("processing.mode", AttributeValue::string("minimal")),
      ("throttling.level", AttributeValue::string("high")),
      ("batch.processing.enabled", AttributeValue::bool(false)), // 禁用批处理
      ("async.processing.enabled", AttributeValue::bool(false)),
      ("complex.calculations.enabled", AttributeValue::bool(false)),
      ("metric.aggregation.enabled", AttributeValue::bool(false)), // 禁用聚合
      ("essential.processing.only", AttributeValue::bool(true))
    ],
    events: [], // 严重CPU限制下不记录事件
    links: []
  }
  
  // 4. CPU恢复后的自适应恢复
  let cpu_recovered_span = Span::{
    trace_id: trace_id,
    span_id: "span_cpu_recovered",
    parent_span_id: Some("span_severe_cpu_throttling"),
    name: "operation_after_cpu_recovery",
    kind: SpanKind::Server,
    start_time: cpu_throttling_time + 6000L,
    end_time: Some(cpu_throttling_time + 7000L),
    status: StatusCode::Ok,
    attributes: [
      ("cpu.usage.percentage", AttributeValue::float(45.0)), // 正常CPU使用率
      ("processing.mode", AttributeValue::string("real_time")),
      ("throttling.level", AttributeValue::string("none")),
      ("batch.processing.enabled", AttributeValue::bool(true)), // 恢复所有功能
      ("async.processing.enabled", AttributeValue::bool(true)),
      ("complex.calculations.enabled", AttributeValue::bool(true)),
      ("metric.aggregation.enabled", AttributeValue::bool(true)),
      ("full.functionality.restored", AttributeValue::bool(true))
    ],
    events: [
      ("cpu_throttling_recovered", cpu_throttling_time + 6100L, [
        ("recovery.threshold", AttributeValue::float(0.5)),
        ("processing.restored", AttributeValue::bool(true)),
        ("performance.optimized", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 5. 记录CPU限制指标
  let cpu_metrics = [
    Measurement::{
      instrument: Instrument::gauge("cpu_usage_percentage", "CPU usage percentage", "percent"),
      value: 35.0, // 正常状态
      attributes: [
        ("processing.mode", AttributeValue::string("real_time")),
        ("throttling.level", AttributeValue::string("none"))
      ],
      time: cpu_throttling_time - 1000L
    },
    Measurement::{
      instrument: Instrument::gauge("cpu_usage_percentage", "CPU usage percentage", "percent"),
      value: 85.0, // CPU限制
      attributes: [
        ("processing.mode", AttributeValue::string("adaptive")),
        ("throttling.level", AttributeValue::string("medium"))
      ],
      time: cpu_throttling_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("cpu_usage_percentage", "CPU usage percentage", "percent"),
      value: 95.0, // 严重限制
      attributes: [
        ("processing.mode", AttributeValue::string("minimal")),
        ("throttling.level", AttributeValue::string("high"))
      ],
      time: cpu_throttling_time + 4000L
    },
    Measurement::{
      instrument: Instrument::counter("cpu_adaptations_total", "Total CPU adaptations", "count"),
      value: 2.0,
      attributes: [
        ("adaptation.reason", AttributeValue::string("cpu_throttling")),
        ("recovery.successful", AttributeValue::bool(true))
      ],
      time: cpu_throttling_time + 7000L
    }
  ]
  
  // 6. 记录CPU限制日志
  let cpu_throttling_log = LogRecord::{
    timestamp: cpu_throttling_time + 2000L,
    observed_timestamp: Some(cpu_throttling_time + 2001L),
    severity: Some(Severity::Warn),
    severity_text: Some("WARN"),
    body: Some("CPU throttling detected, processing mode adapted"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("cpu.usage.percentage", AttributeValue::float(85.0)),
      ("throttling.level", AttributeValue::string("medium")),
      ("processing.complexity.reduced", AttributeValue::bool(true)),
      ("user.impact", AttributeValue::string("increased_latency"))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_cpu_throttled"),
    trace_flags: Some(1)
  }
  
  // 7. 验证CPU限制自适应的正确性
  assert_eq(normal_cpu_span.status, StatusCode::Ok)
  assert_eq(cpu_throttled_span.status, StatusCode::Ok)
  assert_eq(severe_cpu_span.status, StatusCode::Ok)
  assert_eq(cpu_recovered_span.status, StatusCode::Ok)
  
  // 验证处理模式的正确性
  let normal_mode = normal_cpu_span.attributes[1]
  match normal_mode.1 {
    StringValue(mode) => assert_eq(mode, "real_time")
    _ => assert_eq(false, true)
  }
  
  let throttled_mode = cpu_throttled_span.attributes[1]
  match throttled_mode.1 {
    StringValue(mode) => assert_eq(mode, "adaptive")
    _ => assert_eq(false, true)
  }
  
  let minimal_mode = severe_cpu_span.attributes[1]
  match minimal_mode.1 {
    StringValue(mode) => assert_eq(mode, "minimal")
    _ => assert_eq(false, true)
  }
  
  // 验证功能开关的状态
  let normal_async = normal_cpu_span.attributes[3]
  match normal_async.1 {
    BoolValue(enabled) => assert_eq(enabled, true)
    _ => assert_eq(false, true)
  }
  
  let throttled_async = cpu_throttled_span.attributes[3]
  match throttled_async.1 {
    BoolValue(enabled) => assert_eq(enabled, false)
    _ => assert_eq(false, true)
  }
  
  // 验证CPU使用率的阈值
  let severe_cpu_usage = severe_cpu_span.attributes[0]
  match severe_cpu_usage.1 {
    FloatValue(usage) => assert_eq(usage, 95.0)
    _ => assert_eq(false, true)
  }
}

test "disk_space_exhaustion_handling" {
  // 测试磁盘空间不足的处理
  
  let trace_id = "trace_disk_exhaustion_789"
  let disk_exhaustion_time = 1641014000L
  
  // 1. 正常磁盘空间状态
  let normal_disk_span = Span::{
    trace_id: trace_id,
    span_id: "span_normal_disk",
    parent_span_id: None,
    name: "operation_with_normal_disk_space",
    kind: SpanKind::Server,
    start_time: disk_exhaustion_time - 3000L,
    end_time: Some(disk_exhaustion_time - 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("disk.usage.percentage", AttributeValue::float(40.0)),
      ("disk.available.gb", AttributeValue::float(60.0)),
      ("storage.mode", AttributeValue::string("full")),
      ("file.logging.enabled", AttributeValue::bool(true)),
      ("metric.export.enabled", AttributeValue::bool(true)),
      ("trace.export.enabled", AttributeValue::bool(true)),
      ("local.cache.enabled", AttributeValue::bool(true))
    ],
    events: [
      ("data_written_to_disk", disk_exhaustion_time - 2800L, [
        ("data.type", AttributeValue::string("telemetry_batch")),
        ("data.size.mb", AttributeValue::float(10.5))
      ])
    ],
    links: []
  }
  
  // 2. 磁盘空间警告
  let disk_warning_span = Span::{
    trace_id: trace_id,
    span_id: "span_disk_warning",
    parent_span_id: Some("span_normal_disk"),
    name: "operation_with_disk_space_warning",
    kind: SpanKind::Server,
    start_time: disk_exhaustion_time - 1000L,
    end_time: Some(disk_exhaustion_time + 500L),
    status: StatusCode::Ok,
    attributes: [
      ("disk.usage.percentage", AttributeValue::float(80.0)), // 警告级别
      ("disk.available.gb", AttributeValue::float(20.0)),
      ("storage.mode", AttributeValue::string("conservative")),
      ("file.logging.enabled", AttributeValue::bool(true)),
      ("metric.export.enabled", AttributeValue::bool(true)),
      ("trace.export.enabled", AttributeValue::bool(false)), // 禁用trace导出
      ("local.cache.enabled", AttributeValue::bool(false)), // 禁用本地缓存
      ("cleanup.activated", AttributeValue::bool(true))
    ],
    events: [
      ("disk_space_warning", disk_exhaustion_time - 800L, [
        ("warning.threshold", AttributeValue::float(0.8)),
        ("cleanup.initiated", AttributeValue::bool(true))
      ]),
      ("old_files_cleaned", disk_exhaustion_time - 200L, [
        ("files.deleted", AttributeValue::int(50L)),
        ("space.freed.mb", AttributeValue::float(500.0))
      ])
    ],
    links: []
  }
  
  // 3. 磁盘空间严重不足
  let disk_critical_span = Span::{
    trace_id: trace_id,
    span_id: "span_disk_critical",
    parent_span_id: Some("span_disk_warning"),
    name: "operation_with_critical_disk_space",
    kind: SpanKind::Server,
    start_time: disk_exhaustion_time + 1000L,
    end_time: Some(disk_exhaustion_time + 1500L),
    status: StatusCode::Ok,
    attributes: [
      ("disk.usage.percentage", AttributeValue::float(95.0)), // 严重级别
      ("disk.available.gb", AttributeValue::float(5.0)),
      ("storage.mode", AttributeValue::string("minimal")),
      ("file.logging.enabled", AttributeValue::bool(false)), // 禁用文件日志
      ("metric.export.enabled", AttributeValue::bool(true)), // 保持指标导出
      ("trace.export.enabled", AttributeValue::bool(false)),
      ("local.cache.enabled", AttributeValue::bool(false)),
      ("emergency.cleanup.active", AttributeValue::bool(true))
    ],
    events: [], // 严重磁盘空间不足时不记录事件
    links: []
  }
  
  // 4. 磁盘空间恢复
  let disk_recovered_span = Span::{
    trace_id: trace_id,
    span_id: "span_disk_recovered",
    parent_span_id: Some("span_disk_critical"),
    name: "operation_after_disk_recovery",
    kind: SpanKind::Server,
    start_time: disk_exhaustion_time + 3000L,
    end_time: Some(disk_exhaustion_time + 3500L),
    status: StatusCode::Ok,
    attributes: [
      ("disk.usage.percentage", AttributeValue::float(50.0)), // 恢复正常
      ("disk.available.gb", AttributeValue::float(50.0)),
      ("storage.mode", AttributeValue::string("full")),
      ("file.logging.enabled", AttributeValue::bool(true)), // 恢复所有功能
      ("metric.export.enabled", AttributeValue::bool(true)),
      ("trace.export.enabled", AttributeValue::bool(true)),
      ("local.cache.enabled", AttributeValue::bool(true)),
      ("full.functionality.restored", AttributeValue::bool(true))
    ],
    events: [
      ("disk_space_recovered", disk_exhaustion_time + 3200L, [
        ("recovery.threshold", AttributeValue::float(0.6)),
        ("all.services.restored", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 5. 记录磁盘空间指标
  let disk_metrics = [
    Measurement::{
      instrument: Instrument::gauge("disk_usage_percentage", "Disk usage percentage", "percent"),
      value: 40.0, // 正常状态
      attributes: [
        ("storage.mode", AttributeValue::string("full")),
        ("cleanup.status", AttributeValue::string("not_needed"))
      ],
      time: disk_exhaustion_time - 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("disk_usage_percentage", "Disk usage percentage", "percent"),
      value: 80.0, // 警告状态
      attributes: [
        ("storage.mode", AttributeValue::string("conservative")),
        ("cleanup.status", AttributeValue::string("in_progress"))
      ],
      time: disk_exhaustion_time + 500L
    },
    Measurement::{
      instrument: Instrument::gauge("disk_usage_percentage", "Disk usage percentage", "percent"),
      value: 95.0, // 严重状态
      attributes: [
        ("storage.mode", AttributeValue::string("minimal")),
        ("cleanup.status", AttributeValue::string("emergency"))
      ],
      time: disk_exhaustion_time + 1500L
    },
    Measurement::{
      instrument: Instrument::counter("disk_cleanup_operations_total", "Total disk cleanup operations", "count"),
      value: 2.0,
      attributes: [
        ("cleanup.type", AttributeValue::string("automatic")),
        ("recovery.successful", AttributeValue::bool(true))
      ],
      time: disk_exhaustion_time + 3500L
    }
  ]
  
  // 6. 记录磁盘空间日志
  let disk_warning_log = LogRecord::{
    timestamp: disk_exhaustion_time + 500L,
    observed_timestamp: Some(disk_exhaustion_time + 501L),
    severity: Some(Severity::Warn),
    severity_text: Some("WARN"),
    body: Some("Disk space running low, cleanup operations initiated"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("disk.usage.percentage", AttributeValue::float(80.0)),
      ("disk.available.gb", AttributeValue::float(20.0)),
      ("cleanup.activated", AttributeValue::bool(true)),
      ("non.essential.features.disabled", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_disk_warning"),
    trace_flags: Some(1)
  }
  
  // 7. 验证磁盘空间处理的正确性
  assert_eq(normal_disk_span.status, StatusCode::Ok)
  assert_eq(disk_warning_span.status, StatusCode::Ok)
  assert_eq(disk_critical_span.status, StatusCode::Ok)
  assert_eq(disk_recovered_span.status, StatusCode::Ok)
  
  // 验证存储模式的正确性
  let normal_storage = normal_disk_span.attributes[2]
  match normal_storage.1 {
    StringValue(mode) => assert_eq(mode, "full")
    _ => assert_eq(false, true)
  }
  
  let warning_storage = disk_warning_span.attributes[2]
  match warning_storage.1 {
    StringValue(mode) => assert_eq(mode, "conservative")
    _ => assert_eq(false, true)
  }
  
  let critical_storage = disk_critical_span.attributes[2]
  match critical_storage.1 {
    StringValue(mode) => assert_eq(mode, "minimal")
    _ => assert_eq(false, true)
  }
  
  // 验证功能开关的状态
  let normal_file_logging = normal_disk_span.attributes[3]
  match normal_file_logging.1 {
    BoolValue(enabled) => assert_eq(enabled, true)
    _ => assert_eq(false, true)
  }
  
  let critical_file_logging = disk_critical_span.attributes[3]
  match critical_file_logging.1 {
    BoolValue(enabled) => assert_eq(enabled, false)
    _ => assert_eq(false, true)
  }
  
  // 验证磁盘使用率的阈值
  let critical_disk_usage = disk_critical_span.attributes[0]
  match critical_disk_usage.1 {
    FloatValue(usage) => assert_eq(usage, 95.0)
    _ => assert_eq(false, true)
  }
}

test "network_bandwidth_limitation" {
  // 测试网络带宽限制的处理
  
  let trace_id = "trace_network_bandwidth_999"
  let bandwidth_limit_time = 1641015000L
  
  // 1. 正常网络带宽状态
  let normal_network_span = Span::{
    trace_id: trace_id,
    span_id: "span_normal_network",
    parent_span_id: None,
    name: "operation_with_normal_bandwidth",
    kind: SpanKind::Client,
    start_time: bandwidth_limit_time - 2000L,
    end_time: Some(bandwidth_limit_time - 1000L),
    status: StatusCode::Ok,
    attributes: [
      ("network.bandwidth.utilization", AttributeValue::float(30.0)),
      ("data.transmission.rate", AttributeValue::float(1048576.0)), // 1MB/s
      ("transmission.mode", AttributeValue::string("full")),
      ("compression.enabled", AttributeValue::bool(false)),
      ("batch.size", AttributeValue::int(1000L)),
      ("export.frequency", AttributeValue::string("real_time")),
      ("redundancy.enabled", AttributeValue::bool(true))
    ],
    events: [
      ("data_transmitted", bandwidth_limit_time - 1800L, [
        ("data.size.bytes", AttributeValue::int(1048576L)),
        ("transmission.time.ms", AttributeValue::int(1000L))
      ])
    ],
    links: []
  }
  
  // 2. 网络带宽限制检测
  let bandwidth_limited_span = Span::{
    trace_id: trace_id,
    span_id: "span_bandwidth_limited",
    parent_span_id: Some("span_normal_network"),
    name: "operation_with_bandwidth_limitation",
    kind: SpanKind::Client,
    start_time: bandwidth_limit_time,
    end_time: Some(bandwidth_limit_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("network.bandwidth.utilization", AttributeValue::float(80.0)), // 高使用率
      ("data.transmission.rate", AttributeValue::float(524288.0)), // 512KB/s
      ("transmission.mode", AttributeValue::string("optimized")),
      ("compression.enabled", AttributeValue::bool(true)), // 启用压缩
      ("batch.size", AttributeValue::int(500L)), // 减少批次大小
      ("export.frequency", AttributeValue::string("batched")), // 批量导出
      ("redundancy.enabled", AttributeValue::bool(false)), // 禁用冗余
      ("compression.ratio", AttributeValue::float(0.6)) // 60%压缩率
    ],
    events: [
      ("bandwidth_limitation_detected", bandwidth_limit_time + 200L, [
        ("limitation.threshold", AttributeValue::float(0.75)),
        ("optimization.activated", AttributeValue::bool(true))
      ]),
      ("compression_enabled", bandwidth_limit_time + 800L, [
        ("compression.algorithm", AttributeValue::string("gzip")),
        ("compression.level", AttributeValue::int(6L))
      ])
    ],
    links: []
  }
  
  // 3. 严重网络带宽限制
  let severe_bandwidth_span = Span::{
    trace_id: trace_id,
    span_id: "span_severe_bandwidth_limitation",
    parent_span_id: Some("span_bandwidth_limited"),
    name: "operation_with_severe_bandwidth_limitation",
    kind: SpanKind::Client,
    start_time: bandwidth_limit_time + 3000L,
    end_time: Some(bandwidth_limit_time + 4000L),
    status: StatusCode::Ok,
    attributes: [
      ("network.bandwidth.utilization", AttributeValue::float(95.0)), // 极高使用率
      ("data.transmission.rate", AttributeValue::float(131072.0)), // 128KB/s
      ("transmission.mode", AttributeValue::string("minimal")),
      ("compression.enabled", AttributeValue::bool(true)),
      ("batch.size", AttributeValue::int(100L)), // 最小批次
      ("export.frequency", AttributeValue::string("periodic")), // 定期导出
      ("redundancy.enabled", AttributeValue::bool(false)),
      ("compression.ratio", AttributeValue::float(0.4)), // 更高压缩率
      ("essential.data.only", AttributeValue::bool(true))
    ],
    events: [], // 严重带宽限制时不记录事件
    links: []
  }
  
  // 4. 网络带宽恢复
  let network_recovered_span = Span::{
    trace_id: trace_id,
    span_id: "span_network_recovered",
    parent_span_id: Some("span_severe_bandwidth_limitation"),
    name: "operation_after_bandwidth_recovery",
    kind: SpanKind::Client,
    start_time: bandwidth_limit_time + 6000L,
    end_time: Some(bandwidth_limit_time + 7000L),
    status: StatusCode::Ok,
    attributes: [
      ("network.bandwidth.utilization", AttributeValue::float(25.0)), // 正常使用率
      ("data.transmission.rate", AttributeValue::float(1048576.0)), // 恢复1MB/s
      ("transmission.mode", AttributeValue::string("full")),
      ("compression.enabled", AttributeValue::bool(false)), // 禁用压缩
      ("batch.size", AttributeValue::int(1000L)), // 恢复正常批次
      ("export.frequency", AttributeValue::string("real_time")),
      ("redundancy.enabled", AttributeValue::bool(true)), // 恢复冗余
      ("full.performance.restored", AttributeValue::bool(true))
    ],
    events: [
      ("bandwidth_recovered", bandwidth_limit_time + 6200L, [
        ("recovery.threshold", AttributeValue::float(0.3)),
        ("full.transmission.restored", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 5. 记录网络带宽指标
  let bandwidth_metrics = [
    Measurement::{
      instrument: Instrument::gauge("network_bandwidth_utilization", "Network bandwidth utilization", "percent"),
      value: 30.0, // 正常状态
      attributes: [
        ("transmission.mode", AttributeValue::string("full")),
        ("compression.enabled", AttributeValue::bool(false))
      ],
      time: bandwidth_limit_time - 1000L
    },
    Measurement::{
      instrument: Instrument::gauge("network_bandwidth_utilization", "Network bandwidth utilization", "percent"),
      value: 80.0, // 限制状态
      attributes: [
        ("transmission.mode", AttributeValue::string("optimized")),
        ("compression.enabled", AttributeValue::bool(true))
      ],
      time: bandwidth_limit_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("network_bandwidth_utilization", "Network bandwidth utilization", "percent"),
      value: 95.0, // 严重限制
      attributes: [
        ("transmission.mode", AttributeValue::string("minimal")),
        ("compression.enabled", AttributeValue::bool(true))
      ],
      time: bandwidth_limit_time + 4000L
    },
    Measurement::{
      instrument: Instrument::counter("bandwidth_optimizations_total", "Total bandwidth optimizations", "count"),
      value: 2.0,
      attributes: [
        ("optimization.type", AttributeValue::string("adaptive_compression")),
        ("recovery.successful", AttributeValue::bool(true))
      ],
      time: bandwidth_limit_time + 7000L
    }
  ]
  
  // 6. 记录网络带宽日志
  let bandwidth_limitation_log = LogRecord::{
    timestamp: bandwidth_limit_time + 2000L,
    observed_timestamp: Some(bandwidth_limit_time + 2001L),
    severity: Some(Severity::Warn),
    severity_text: Some("WARN"),
    body: Some("Network bandwidth limitation detected, transmission optimized"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("bandwidth.utilization", AttributeValue::float(80.0)),
      ("compression.activated", AttributeValue::bool(true)),
      ("batch.size.reduced", AttributeValue::bool(true)),
      ("transmission.optimized", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_bandwidth_limited"),
    trace_flags: Some(1)
  }
  
  // 7. 验证网络带宽限制处理的正确性
  assert_eq(normal_network_span.status, StatusCode::Ok)
  assert_eq(bandwidth_limited_span.status, StatusCode::Ok)
  assert_eq(severe_bandwidth_span.status, StatusCode::Ok)
  assert_eq(network_recovered_span.status, StatusCode::Ok)
  
  // 验证传输模式的正确性
  let normal_mode = normal_network_span.attributes[2]
  match normal_mode.1 {
    StringValue(mode) => assert_eq(mode, "full")
    _ => assert_eq(false, true)
  }
  
  let limited_mode = bandwidth_limited_span.attributes[2]
  match limited_mode.1 {
    StringValue(mode) => assert_eq(mode, "optimized")
    _ => assert_eq(false, true)
  }
  
  let severe_mode = severe_bandwidth_span.attributes[2]
  match severe_mode.1 {
    StringValue(mode) => assert_eq(mode, "minimal")
    _ => assert_eq(false, true)
  }
  
  // 验证压缩功能的状态
  let normal_compression = normal_network_span.attributes[3]
  match normal_compression.1 {
    BoolValue(enabled) => assert_eq(enabled, false)
    _ => assert_eq(false, true)
  }
  
  let limited_compression = bandwidth_limited_span.attributes[3]
  match limited_compression.1 {
    BoolValue(enabled) => assert_eq(enabled, true)
    _ => assert_eq(false, true)
  }
  
  // 验证批次大小的变化
  let normal_batch_size = normal_network_span.attributes[4]
  match normal_batch_size.1 {
    IntValue(size) => assert_eq(size, 1000L)
    _ => assert_eq(false, true)
  }
  
  let severe_batch_size = severe_bandwidth_span.attributes[4]
  match severe_batch_size.1 {
    IntValue(size) => assert_eq(size, 100L)
    _ => assert_eq(false, true)
  }
}