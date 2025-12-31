// Azimuth Telemetry - Data Consistency Test
// 测试遥测数据的一致性

use azimuth.telemetry.api.common
use azimuth.telemetry.api.context
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

// 数据一致性验证结构
pub struct ConsistencyValidator {
  expected_trace_id : Array[Byte]
  expected_span_id : Array[Byte]
  expected_attributes : Array[(String, common::AttributeValue)]
  expected_timestamp : Int64
}

pub fn ConsistencyValidator::new(
  trace_id : Array[Byte],
  span_id : Array[Byte],
  attributes : Array[(String, common::AttributeValue)],
  timestamp : Int64
) -> ConsistencyValidator {
  ConsistencyValidator::{
    expected_trace_id: trace_id,
    expected_span_id: span_id,
    expected_attributes: attributes,
    expected_timestamp: timestamp
  }
}

pub fn ConsistencyValidator::validate_trace_context(self : ConsistencyValidator, span : trace::Span) -> Bool {
  // 验证trace ID一致性
  if self.expected_trace_id.length() != span.context.trace_id.length() {
    return false
  }
  
  let mut i = 0
  while i < self.expected_trace_id.length() {
    if self.expected_trace_id[i] != span.context.trace_id[i] {
      return false
    }
    i = i + 1
  }
  
  // 验证span ID一致性
  if self.expected_span_id.length() != span.context.span_id.length() {
    return false
  }
  
  let mut j = 0
  while j < self.expected_span_id.length() {
    if self.expected_span_id[j] != span.context.span_id[j] {
      return false
    }
    j = j + 1
  }
  
  true
}

pub fn ConsistencyValidator::validate_attributes(self : ConsistencyValidator, attributes : Array[(String, common::AttributeValue)]) -> Bool {
  if self.expected_attributes.length() != attributes.length() {
    return false
  }
  
  let mut i = 0
  while i < self.expected_attributes.length() {
    let (expected_key, expected_value) = self.expected_attributes[i]
    let (actual_key, actual_value) = attributes[i]
    
    if expected_key != actual_key {
      return false
    }
    
    // 验证属性值类型和内容一致性
    match (expected_value, actual_value) {
      (common::StringValue(e), common::StringValue(a)) => {
        if e != a { return false }
      }
      (common::IntValue(e), common::IntValue(a)) => {
        if e != a { return false }
      }
      (common::FloatValue(e), common::FloatValue(a)) => {
        if e != a { return false }
      }
      (common::BoolValue(e), common::BoolValue(a)) => {
        if e != a { return false }
      }
      (common::ArrayStringValue(e), common::ArrayStringValue(a)) => {
        if e.length() != a.length() { return false }
        let mut j = 0
        while j < e.length() {
          if e[j] != a[j] { return false }
          j = j + 1
        }
      }
      (common::ArrayIntValue(e), common::ArrayIntValue(a)) => {
        if e.length() != a.length() { return false }
        let mut j = 0
        while j < e.length() {
          if e[j] != a[j] { return false }
          j = j + 1
        }
      }
      (common::ArrayFloatValue(e), common::ArrayFloatValue(a)) => {
        if e.length() != a.length() { return false }
        let mut j = 0
        while j < e.length() {
          if e[j] != a[j] { return false }
          j = j + 1
        }
      }
      (common::ArrayBoolValue(e), common::ArrayBoolValue(a)) => {
        if e.length() != a.length() { return false }
        let mut j = 0
        while j < e.length() {
          if e[j] != a[j] { return false }
          j = j + 1
        }
      }
      _ => { return false }
    }
    
    i = i + 1
  }
  
  true
}

test "trace_context_consistency" {
  // 测试Trace上下文的一致性
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("consistency-test-tracer", "1.0.0")
  let ctx = context::Context::new()
  
  // 创建Span并记录上下文信息
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "consistency-test-span",
    trace::Server,
    [
      ("operation.name", common::AttributeValue::string("consistency-test")),
      ("operation.type", common::AttributeValue::string("test"))
    ],
    1234567890L
  )
  
  // 创建一致性验证器
  let validator = ConsistencyValidator::new(
    span.context.trace_id,
    span.context.span_id,
    [
      ("operation.name", common::AttributeValue::string("consistency-test")),
      ("operation.type", common::AttributeValue::string("test"))
    ],
    1234567890L
  )
  
  // 验证Trace上下文一致性
  @assert(validator.validate_trace_context(span))
  @assert(validator.validate_attributes(span.attributes))
  
  // 验证Span的基本属性
  @assert(span.name == "consistency-test-span")
  @assert(span.kind == trace::Server)
  @assert(span.start_time_unix_nanos == 1234567890L)
  @assert(span.status == trace::Unset)
  @assert(span.events.length() == 0)
  @assert(span.links.length() == 0)
}

test "metrics_attributes_consistency" {
  // 测试Metrics属性的一致性
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("consistency-test-meter", "1.0.0")
  
  // 创建指标instruments
  let counter = meter.create_counter("consistency.counter", "count", "Consistency test counter")
  let histogram = meter.create_histogram("consistency.histogram", "ms", "Consistency test histogram")
  
  // 定义一致的属性集
  let consistent_attributes = [
    ("service.name", common::AttributeValue::string("consistency-test-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("operation.type", common::AttributeValue::string("metrics-consistency")),
    ("environment", common::AttributeValue::string("test"))
  ]
  
  // 记录指标使用一致的属性
  counter.add(1L, consistent_attributes)
  histogram.record(100.5, consistent_attributes)
  
  // 验证属性一致性
  let validator = ConsistencyValidator::new([], [], consistent_attributes, 0L)
  @assert(validator.validate_attributes(consistent_attributes))
  
  // 测试属性类型一致性
  let type_consistency_attributes = [
    ("string.attr", common::AttributeValue::string("test-string")),
    ("int.attr", common::AttributeValue::int(42L)),
    ("float.attr", common::AttributeValue::float(3.14)),
    ("bool.attr", common::AttributeValue::bool(true)),
    ("string.array.attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array.attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array.attr", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  counter.add(1L, type_consistency_attributes)
  histogram.record(200.5, type_consistency_attributes)
  
  let type_validator = ConsistencyValidator::new([], [], type_consistency_attributes, 0L)
  @assert(type_validator.validate_attributes(type_consistency_attributes))
}

test "logs_data_consistency" {
  // 测试Logs数据的一致性
  
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("consistency-test-logger", "1.0.0")
  
  // 创建一致的日志记录
  let consistent_attributes = [
    ("service.name", common::AttributeValue::string("consistency-test-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("log.type", common::AttributeValue::string("consistency-test")),
    ("environment", common::AttributeValue::string("test"))
  ]
  
  // 使用LogRecordBuilder创建结构化日志
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .severity_text("INFO")
    .body("Consistency test log message")
    .with_attribute("service.name", common::AttributeValue::string("consistency-test-service"))
    .with_attribute("service.version", common::AttributeValue::string("1.0.0"))
    .with_attribute("log.type", common::AttributeValue::string("consistency-test"))
    .with_attribute("environment", common::AttributeValue::string("test"))
    .build()
  
  logger.emit(log_record)
  
  // 验证日志记录的一致性
  @assert(log_record.timestamp_unix_nanos == 1234567890L)
  @assert(log_record.severity_number == logs::Info)
  @assert(log_record.severity_text.unwrap_or("") == "INFO")
  @assert(log_record.body.unwrap_or("") == "Consistency test log message")
  @assert(log_record.attributes.length() == 4)
  
  let validator = ConsistencyValidator::new([], [], log_record.attributes, 1234567890L)
  @assert(validator.validate_attributes(log_record.attributes))
  
  // 使用便捷方法记录日志并验证一致性
  logger.info("Info level consistency test", consistent_attributes)
  logger.warn("Warning level consistency test", consistent_attributes)
  logger.error("Error level consistency test", consistent_attributes)
  logger.debug("Debug level consistency test", consistent_attributes)
  logger.fatal("Fatal level consistency test", consistent_attributes)
}

test "cross_api_data_consistency" {
  // 测试跨API数据的一致性
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("cross-api-consistency-tracer", "1.0.0")
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("cross-api-consistency-meter", "1.0.0")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("cross-api-consistency-logger", "1.0.0")
  
  // 创建共享的上下文和属性
  let ctx = context::Context::new()
  let shared_attributes = [
    ("service.name", common::AttributeValue::string("cross-api-consistency-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("operation.id", common::AttributeValue::int(12345L)),
    ("trace.id", common::AttributeValue::string("trace-12345")),
    ("environment", common::AttributeValue::string("test"))
  ]
  
  // 在Trace中使用共享属性
  let (_, span) = tracer.start_span(
    ctx,
    "cross-api-consistency-span",
    trace::Server,
    shared_attributes,
    1234567890L
  )
  
  // 在Metrics中使用共享属性
  let counter = meter.create_counter("cross.api.operations", "count", "Cross API operations")
  let histogram = meter.create_histogram("cross.api.duration", "ms", "Cross API operation duration")
  
  counter.add(1L, shared_attributes)
  histogram.record(150.5, shared_attributes)
  
  // 在Logs中使用共享属性
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("Cross API consistency test")
    .with_attribute("service.name", common::AttributeValue::string("cross-api-consistency-service"))
    .with_attribute("service.version", common::AttributeValue::string("1.0.0"))
    .with_attribute("operation.id", common::AttributeValue::int(12345L))
    .with_attribute("trace.id", common::AttributeValue::string("trace-12345"))
    .with_attribute("environment", common::AttributeValue::string("test"))
    .build()
  
  logger.emit(log_record)
  
  // 验证跨API数据一致性
  let validator = ConsistencyValidator::new([], [], shared_attributes, 1234567890L)
  @assert(validator.validate_attributes(span.attributes))
  @assert(validator.validate_attributes(shared_attributes))
  @assert(validator.validate_attributes(log_record.attributes))
  
  // 验证所有API使用相同的属性类型系统
  let mut i = 0
  while i < shared_attributes.length() {
    let (key, value) = shared_attributes[i]
    @assert(match value {
      common::StringValue(_) => true
      common::IntValue(_) => true
      common::FloatValue(_) => true
      common::BoolValue(_) => true
      common::ArrayStringValue(_) => true
      common::ArrayIntValue(_) => true
      common::ArrayFloatValue(_) => true
      common::ArrayBoolValue(_) => true
    })
    i = i + 1
  }
}

test "timestamp_consistency" {
  // 测试时间戳一致性
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("timestamp-consistency-tracer", "1.0.0")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("timestamp-consistency-logger", "1.0.0")
  
  // 使用一致的时间戳
  let consistent_timestamp = 1234567890000000000L // 纳秒时间戳
  
  // 创建具有一致时间戳的Span
  let ctx = context::Context::new()
  let (_, span) = tracer.start_span(
    ctx,
    "timestamp-consistency-span",
    trace::Internal,
    [],
    consistent_timestamp
  )
  
  // 创建具有一致时间戳的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(consistent_timestamp)
    .severity(logs::Info)
    .body("Timestamp consistency test")
    .build()
  
  logger.emit(log_record)
  
  // 验证时间戳一致性
  @assert(span.start_time_unix_nanos == consistent_timestamp)
  @assert(log_record.timestamp_unix_nanos == consistent_timestamp)
  
  // 测试时间戳序列一致性
  let base_timestamp = 1234567890000000000L
  let mut i = 0
  while i < 10 {
    let expected_timestamp = base_timestamp + (i * 1000000L).to_int64() // 每次增加1毫秒
    
    let (_, test_span) = tracer.start_span(
      ctx,
      "sequential-span-" + i.to_string(),
      trace::Internal,
      [("sequence", common::AttributeValue::int(i.to_int64()))],
      expected_timestamp
    )
    
    let test_log_record = logs::LogRecord::builder()
      .timestamp(expected_timestamp)
      .severity(logs::Info)
      .body("Sequential log " + i.to_string())
      .with_attribute("sequence", common::AttributeValue::int(i.to_int64()))
      .build()
    
    logger.emit(test_log_record)
    
    // 验证时间戳序列
    @assert(test_span.start_time_unix_nanos == expected_timestamp)
    @assert(test_log_record.timestamp_unix_nanos == expected_timestamp)
    
    i = i + 1
  }
}

test "resource_consistency" {
  // 测试资源一致性
  
  // 创建一致的资源配置
  let resource = common::Resource::default("resource-consistency-service")
  
  // 验证资源的基本属性
  @assert(resource.service_name == "resource-consistency-service")
  @assert(resource.telemetry_sdk_name == "azimuth")
  @assert(resource.telemetry_sdk_version == "0.1.0")
  @assert(resource.attributes.length() == 0)
  
  // 创建带有额外属性的资源配置
  let resource_with_attributes = common::Resource::{
    service_name: "resource-consistency-service-with-attrs",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", common::AttributeValue::string("test")),
      ("service.instance.id", common::AttributeValue::string("instance-123")),
      ("environment", common::AttributeValue::string("test"))
    ]
  }
  
  // 验证资源属性一致性
  @assert(resource_with_attributes.service_name == "resource-consistency-service-with-attrs")
  @assert(resource_with_attributes.service_version.unwrap_or("") == "1.0.0")
  @assert(resource_with_attributes.attributes.length() == 3)
  
  // 验证资源属性类型一致性
  let mut i = 0
  while i < resource_with_attributes.attributes.length() {
    let (key, value) = resource_with_attributes.attributes[i]
    @assert(key.length() > 0)
    @assert(match value {
      common::StringValue(_) => true
      common::IntValue(_) => true
      common::FloatValue(_) => true
      common::BoolValue(_) => true
      common::ArrayStringValue(_) => true
      common::ArrayIntValue(_) => true
      common::ArrayFloatValue(_) => true
      common::ArrayBoolValue(_) => true
    })
    i = i + 1
  }
  
  // 测试资源在不同API间的一致性使用
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-consistency-tracer", "1.0.0")
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-consistency-meter", "1.0.0")
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("resource-consistency-logger", "1.0.0")
  
  // 所有API都使用相同的资源信息
  let resource_attributes = [
    ("service.name", common::AttributeValue::string(resource_with_attributes.service_name)),
    ("service.version", common::AttributeValue::string(resource_with_attributes.service_version.unwrap_or("unknown"))),
    ("sdk.name", common::AttributeValue::string(resource_with_attributes.telemetry_sdk_name)),
    ("sdk.version", common::AttributeValue::string(resource_with_attributes.telemetry_sdk_version))
  ]
  
  // 在所有API中使用一致的资源属性
  let (_, span) = tracer.start_span(
    context::Context::new(),
    "resource-consistency-span",
    trace::Server,
    resource_attributes
  )
  
  let counter = meter.create_counter("resource.consistency.operations", "count", "Resource consistency operations")
  counter.add(1L, resource_attributes)
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Resource consistency test")
    .with_attribute("service.name", common::AttributeValue::string(resource_with_attributes.service_name))
    .with_attribute("service.version", common::AttributeValue::string(resource_with_attributes.service_version.unwrap_or("unknown")))
    .with_attribute("sdk.name", common::AttributeValue::string(resource_with_attributes.telemetry_sdk_name))
    .with_attribute("sdk.version", common::AttributeValue::string(resource_with_attributes.telemetry_sdk_version))
    .build()
  
  logger.emit(log_record)
  
  // 验证资源属性在所有API中的一致性
  let validator = ConsistencyValidator::new([], [], resource_attributes, 0L)
  @assert(validator.validate_attributes(span.attributes))
  @assert(validator.validate_attributes(resource_attributes))
  @assert(validator.validate_attributes(log_record.attributes))
}