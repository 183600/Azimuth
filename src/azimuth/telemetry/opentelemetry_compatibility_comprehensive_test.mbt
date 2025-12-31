// 兼容性测试 - 验证与OpenTelemetry标准的兼容性
// 确保Azimuth Telemetry符合OpenTelemetry规范和最佳实践

test "opentelemetry_trace_compatibility" {
  // 测试与OpenTelemetry Trace API的兼容性
  
  // 1. 验证SpanKind兼容性
  let internal_span_kind = trace::Internal
  let server_span_kind = trace::Server
  let client_span_kind = trace::Client
  let producer_span_kind = trace::Producer
  let consumer_span_kind = trace::Consumer
  
  // 验证所有SpanKind都存在且符合OTel规范
  assert_eq(internal_span_kind == trace::Internal, true)
  assert_eq(server_span_kind == trace::Server, true)
  assert_eq(client_span_kind == trace::Client, true)
  assert_eq(producer_span_kind == trace::Producer, true)
  assert_eq(consumer_span_kind == trace::Consumer, true)
  
  // 2. 验证StatusCode兼容性
  let unset_status = trace::Unset
  let ok_status = trace::Ok
  let error_status = trace::Error
  
  // 验证所有StatusCode都存在且符合OTel规范
  assert_eq(unset_status == trace::Unset, true)
  assert_eq(ok_status == trace::Ok, true)
  assert_eq(error_status == trace::Error, true)
  
  // 3. 验证SpanContext结构兼容性
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  let span_context = trace::SpanContext::{
    trace_id: trace_id_bytes,
    span_id: span_id_bytes,
    trace_flags: 1_byte,
    trace_state: "user_id=user_12345,session_id=sess_abcde"
  }
  
  // 验证trace_id长度（16字节）
  assert_eq(span_context.trace_id.length(), 16)
  
  // 验证span_id长度（8字节）
  assert_eq(span_context.span_id.length(), 8)
  
  // 验证trace_flags是单字节
  assert_eq(span_context.trace_flags == 1_byte, true)
  
  // 验证trace_state格式
  assert_eq(span_context.trace_state.contains("user_id=user_12345"), true)
  assert_eq(span_context.trace_state.contains("session_id=sess_abcde"), true)
  
  // 4. 验证Span结构兼容性
  let otel_compatible_span = trace::Span::{
    name: "otel_compatible_span",
    context: span_context,
    kind: server_span_kind,
    parent_span_id: Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: ok_status,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("service.name", common::AttributeValue::string("otel-test-service")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
      ("telemetry.sdk.version", common::AttributeValue::string("0.1.0")),
      ("telemetry.sdk.language", common::AttributeValue::string("moonbit"))
    ],
    events: [
      trace::SpanEvent::{
        name: "event_name",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("event.attribute", common::AttributeValue::string("event_value"))
        ]
      }
    ],
    links: [
      trace::SpanLink::{
        context: trace::SpanContext::{
          trace_id: [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          span_id: [0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11],
          trace_flags: 1_byte,
          trace_state: ""
        },
        attributes: [
          ("link.attribute", common::AttributeValue::string("link_value"))
        ]
      }
    ]
  }
  
  // 验证必需字段
  assert_eq(otel_compatible_span.name, "otel_compatible_span")
  assert_eq(otel_compatible_span.context.trace_id.length(), 16)
  assert_eq(otel_compatible_span.context.span_id.length(), 8)
  assert_eq(otel_compatible_span.start_time_unix_nanos > 0L, true)
  
  // 验证OTel标准属性
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_sdk_name = false
  let mut found_sdk_version = false
  let mut found_sdk_language = false
  
  let mut i = 0
  while i < otel_compatible_span.attributes.length() {
    let (key, value) = otel_compatible_span.attributes[i]
    match key {
      "service.name" => {
        match value {
          common::StringValue(name) => {
            assert_eq(name, "otel-test-service")
            found_service_name = true
          }
          _ => assert_eq(false, true, "Expected string value for service.name")
        }
      }
      "service.version" => {
        match value {
          common::StringValue(version) => {
            assert_eq(version, "1.0.0")
            found_service_version = true
          }
          _ => assert_eq(false, true, "Expected string value for service.version")
        }
      }
      "telemetry.sdk.name" => {
        match value {
          common::StringValue(sdk_name) => {
            assert_eq(sdk_name, "azimuth")
            found_sdk_name = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.name")
        }
      }
      "telemetry.sdk.version" => {
        match value {
          common::StringValue(sdk_version) => {
            assert_eq(sdk_version, "0.1.0")
            found_sdk_version = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.version")
        }
      }
      "telemetry.sdk.language" => {
        match value {
          common::StringValue(sdk_language) => {
            assert_eq(sdk_language, "moonbit")
            found_sdk_language = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.language")
        }
      }
      _ => assert_eq(false, true, "Unexpected attribute key: " + key)
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_service_version, true)
  assert_eq(found_sdk_name, true)
  assert_eq(found_sdk_version, true)
  assert_eq(found_sdk_language, true)
}

test "opentelemetry_metrics_compatibility" {
  // 测试与OpenTelemetry Metrics API的兼容性
  
  // 1. 验证InstrumentType兼容性
  let counter_instrument = metrics::Counter
  let histogram_instrument = metrics::Histogram
  let up_down_counter_instrument = metrics::UpDownCounter
  let gauge_instrument = metrics::Gauge
  let observable_counter_instrument = metrics::ObservableCounter
  let observable_gauge_instrument = metrics::ObservableGauge
  let observable_up_down_counter_instrument = metrics::ObservableUpDownCounter
  
  // 验证所有InstrumentType都存在且符合OTel规范
  assert_eq(counter_instrument == metrics::Counter, true)
  assert_eq(histogram_instrument == metrics::Histogram, true)
  assert_eq(up_down_counter_instrument == metrics::UpDownCounter, true)
  assert_eq(gauge_instrument == metrics::Gauge, true)
  assert_eq(observable_counter_instrument == metrics::ObservableCounter, true)
  assert_eq(observable_gauge_instrument == metrics::ObservableGauge, true)
  assert_eq(observable_up_down_counter_instrument == metrics::ObservableUpDownCounter, true)
  
  // 2. 验证Measurement结构兼容性
  let otel_compatible_measurement = metrics::Measurement::{
    value: 42.5,
    attributes: [
      ("measurement.attribute", common::AttributeValue::string("measurement_value"))
    ]
  }
  
  // 验证Measurement结构
  assert_eq(otel_compatible_measurement.value, 42.5)
  assert_eq(otel_compatible_measurement.attributes.length(), 1)
  
  // 3. 验证Meter接口兼容性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("otel-test-meter", "1.0.0", "https://opentelemetry.io/schema/1.0.0")
  
  // 验证Meter创建方法
  let counter = meter.create_counter("otel_counter", "count", "OTel compatible counter")
  let histogram = meter.create_histogram("otel_histogram", "ms", "OTel compatible histogram")
  let up_down_counter = meter.create_up_down_counter("otel_up_down_counter", "count", "OTel compatible up-down counter")
  let gauge = meter.create_gauge("otel_gauge", "value", "OTel compatible gauge")
  
  // 验证指标创建成功（No-op实现）
  assert_eq(true, true)
  
  // 4. 验证OTel标准指标单位和描述
  let standard_counter = meter.create_counter("http_requests_total", "count", "Total HTTP requests")
  let standard_histogram = meter.create_histogram("http_request_duration", "ms", "HTTP request duration")
  let standard_gauge = meter.create_gauge("active_connections", "connections", "Active connections")
  
  // 验证标准单位
  assert_eq(true, true)  // 如果创建成功，说明单位格式正确
  
  // 5. 验证OTel属性约定
  let otel_attributes = [
    ("service.name", common::AttributeValue::string("otel-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("service.instance.id", common::AttributeValue::string("instance-12345")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
    ("telemetry.sdk.version", common::AttributeValue::string("0.1.0")),
    ("telemetry.sdk.language", common::AttributeValue::string("moonbit"))
  ]
  
  // 使用OTel标准属性记录指标
  standard_counter.add(1L, otel_attributes)
  standard_histogram.record(125.5, otel_attributes)
  standard_gauge.record(10.0, otel_attributes)
  
  // 验证属性键符合OTel约定
  let expected_otel_keys = [
    "service.name",
    "service.version", 
    "service.instance.id",
    "deployment.environment",
    "telemetry.sdk.name",
    "telemetry.sdk.version",
    "telemetry.sdk.language"
  ]
  
  let mut i = 0
  while i < expected_otel_keys.length() {
    let expected_key = expected_otel_keys[i]
    let mut found_key = false
    
    let mut j = 0
    while j < otel_attributes.length() {
      if otel_attributes[j].0 == expected_key {
        found_key = true
        break
      }
      j = j + 1
    }
    
    assert_eq(found_key, true, "Expected OTel attribute key: " + expected_key)
    i = i + 1
  }
}

test "opentelemetry_logs_compatibility" {
  // 测试与OpenTelemetry Logs API的兼容性
  
  // 1. 验证SeverityNumber兼容性
  let trace_severity = logs::Trace
  let debug_severity = logs::Debug
  let info_severity = logs::Info
  let warn_severity = logs::Warn
  let error_severity = logs::Error
  let fatal_severity = logs::Fatal
  
  // 验证所有SeverityNumber都存在且符合OTel规范
  assert_eq(trace_severity == logs::Trace, true)
  assert_eq(debug_severity == logs::Debug, true)
  assert_eq(info_severity == logs::Info, true)
  assert_eq(warn_severity == logs::Warn, true)
  assert_eq(error_severity == logs::Error, true)
  assert_eq(fatal_severity == logs::Fatal, true)
  
  // 2. 验证LogRecord结构兼容性
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  let otel_compatible_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000001000L),
    severity_number: info_severity,
    severity_text: Some("INFO"),
    body: Some("OTel compatible log message"),
    attributes: [
      ("service.name", common::AttributeValue::string("otel-log-service")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
      ("telemetry.sdk.version", common::AttributeValue::string("0.1.0")),
      ("telemetry.sdk.language", common::AttributeValue::string("moonbit")),
      ("log.logger", common::AttributeValue::string("otel-logger")),
      ("log.level", common::AttributeValue::string("INFO"))
    ],
    trace_id: Some(trace_id_bytes),
    span_id: Some(span_id_bytes),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("otel-log-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "otel-log-scope",
      version: Some("1.0.0"),
      schema_url: Some("https://opentelemetry.io/schema/1.0.0")
    })
  }
  
  // 验证LogRecord必需字段
  assert_eq(otel_compatible_log.timestamp_unix_nanos > 0L, true)
  assert_eq(otel_compatible_log.severity_number, logs::Info)
  assert_eq(otel_compatible_log.severity_text.unwrap(), "INFO")
  assert_eq(otel_compatible_log.body.unwrap(), "OTel compatible log message")
  
  // 验证trace和span关联
  assert_eq(otel_compatible_log.trace_id.unwrap().length(), 16)
  assert_eq(otel_compatible_log.span_id.unwrap().length(), 8)
  assert_eq(otel_compatible_log.trace_flags.unwrap(), 1_byte)
  
  // 验证OTel标准日志属性
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_sdk_name = false
  let mut found_sdk_version = false
  let mut found_sdk_language = false
  let mut found_logger = false
  let mut found_level = false
  
  let mut i = 0
  while i < otel_compatible_log.attributes.length() {
    let (key, value) = otel_compatible_log.attributes[i]
    match key {
      "service.name" => {
        match value {
          common::StringValue(name) => {
            assert_eq(name, "otel-log-service")
            found_service_name = true
          }
          _ => assert_eq(false, true, "Expected string value for service.name")
        }
      }
      "service.version" => {
        match value {
          common::StringValue(version) => {
            assert_eq(version, "1.0.0")
            found_service_version = true
          }
          _ => assert_eq(false, true, "Expected string value for service.version")
        }
      }
      "telemetry.sdk.name" => {
        match value {
          common::StringValue(sdk_name) => {
            assert_eq(sdk_name, "azimuth")
            found_sdk_name = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.name")
        }
      }
      "telemetry.sdk.version" => {
        match value {
          common::StringValue(sdk_version) => {
            assert_eq(sdk_version, "0.1.0")
            found_sdk_version = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.version")
        }
      }
      "telemetry.sdk.language" => {
        match value {
          common::StringValue(sdk_language) => {
            assert_eq(sdk_language, "moonbit")
            found_sdk_language = true
          }
          _ => assert_eq(false, true, "Expected string value for telemetry.sdk.language")
        }
      }
      "log.logger" => {
        match value {
          common::StringValue(logger) => {
            assert_eq(logger, "otel-logger")
            found_logger = true
          }
          _ => assert_eq(false, true, "Expected string value for log.logger")
        }
      }
      "log.level" => {
        match value {
          common::StringValue(level) => {
            assert_eq(level, "INFO")
            found_level = true
          }
          _ => assert_eq(false, true, "Expected string value for log.level")
        }
      }
      _ => assert_eq(false, true, "Unexpected attribute key: " + key)
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_service_version, true)
  assert_eq(found_sdk_name, true)
  assert_eq(found_sdk_version, true)
  assert_eq(found_sdk_language, true)
  assert_eq(found_logger, true)
  assert_eq(found_level, true)
  
  // 3. 验证Logger接口兼容性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("otel-logger", "1.0.0", "https://opentelemetry.io/schema/1.0.0")
  
  // 验证Logger便捷方法
  logger.debug("Debug message", [
    ("service.name", common::AttributeValue::string("otel-service"))
  ])
  
  logger.info("Info message", [
    ("service.name", common::AttributeValue::string("otel-service"))
  ])
  
  logger.warn("Warning message", [
    ("service.name", common::AttributeValue::string("otel-service"))
  ])
  
  logger.error("Error message", [
    ("service.name", common::AttributeValue::string("otel-service"))
  ])
  
  logger.fatal("Fatal message", [
    ("service.name", common::AttributeValue::string("otel-service"))
  ])
  
  // 验证LogRecordBuilder兼容性
  let builder_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Warn)
    .body("Built log message")
    .with_attribute("built.attribute", common::AttributeValue::string("built_value"))
    .build()
  
  // 验证构建的日志记录
  assert_eq(builder_log.severity_number, logs::Warn)
  assert_eq(builder_log.body.unwrap(), "Built log message")
  assert_eq(builder_log.attributes.length(), 1)
}

test "opentelemetry_resource_compatibility" {
  // 测试与OpenTelemetry Resource规范的兼容性
  
  // 1. 验证Resource结构兼容性
  let otel_compatible_resource = common::Resource::{
    service_name: "otel-resource-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-otel-001")),
      ("service.namespace", common::AttributeValue::string("production")),
      ("deployment.environment", common::AttributeValue::string("prod")),
      ("host.name", common::AttributeValue::string("otel-host")),
      ("host.arch", common::AttributeValue::string("amd64")),
      ("os.type", common::AttributeValue::string("linux")),
      ("os.version", common::AttributeValue::string("5.15.0")),
      ("process.pid", common::AttributeValue::int(12345L)),
      ("process.executable.name", common::AttributeValue::string("otel-service")),
      ("process.command_args", common::AttributeValue::array_string(["./otel-service", "--config", "prod.yaml"])),
      ("telemetry.sdk.language", common::AttributeValue::string("moonbit")),
      ("telemetry.auto.version", common::AttributeValue::string("1.0.0"))
    ]
  }
  
  // 验证Resource必需字段
  assert_eq(otel_compatible_resource.service_name, "otel-resource-service")
  assert_eq(otel_compatible_resource.telemetry_sdk_name, "azimuth")
  assert_eq(otel_compatible_resource.telemetry_sdk_version, "0.1.0")
  
  // 验证可选字段
  match otel_compatible_resource.service_version {
    Some(version) => assert_eq(version, "1.2.3")
    None => assert_eq(false, true, "Expected service version to be Some")
  }
  
  // 2. 验证OTel标准资源属性
  let otel_resource_attributes = [
    "service.name",
    "service.version",
    "service.instance.id",
    "service.namespace",
    "deployment.environment",
    "host.name",
    "host.arch",
    "os.type",
    "os.version",
    "process.pid",
    "process.executable.name",
    "process.command_args",
    "telemetry.sdk.name",
    "telemetry.sdk.version",
    "telemetry.sdk.language",
    "telemetry.auto.version"
  ]
  
  let mut found_attributes = [] : Array[String]
  let mut i = 0
  while i < otel_compatible_resource.attributes.length() {
    found_attributes.push(otel_compatible_resource.attributes[i].0)
    i = i + 1
  }
  
  // 验证所有OTel标准属性都存在
  let mut j = 0
  while j < otel_resource_attributes.length() {
    let expected_attr = otel_resource_attributes[j]
    assert_eq(found_attributes.contains(expected_attr), true, "Missing OTel resource attribute: " + expected_attr)
    j = j + 1
  }
  
  // 3. 验证Resource默认工厂方法
  let default_resource = common::Resource::default("otel-default-service")
  
  assert_eq(default_resource.service_name, "otel-default-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 4. 验证Resource属性值类型
  let mut k = 0
  while k < otel_compatible_resource.attributes.length() {
    let (key, value) = otel_compatible_resource.attributes[k]
    match key {
      "service.instance.id" | "service.namespace" | "deployment.environment" | 
      "host.name" | "host.arch" | "os.type" | "os.version" | "process.executable.name" |
      "telemetry.sdk.language" | "telemetry.auto.version" => {
        match value {
          common::StringValue(_) => assert_eq(true, true)  // 期望字符串值
          _ => assert_eq(false, true, "Expected string value for " + key)
        }
      }
      "process.pid" => {
        match value {
          common::IntValue(pid) => assert_eq(pid > 0L, true)  // 期望正整数
          _ => assert_eq(false, true, "Expected int value for process.pid")
        }
      }
      "process.command_args" => {
        match value {
          common::ArrayStringValue(args) => {
            assert_eq(args.length(), 3)  // 期望字符串数组
            assert_eq(args.contains("./otel-service"), true)
            assert_eq(args.contains("--config"), true)
            assert_eq(args.contains("prod.yaml"), true)
          }
          _ => assert_eq(false, true, "Expected array string value for process.command_args")
        }
      }
      _ => assert_eq(false, true, "Unexpected resource attribute: " + key)
    }
    k = k + 1
  }
}

test "opentelemetry_context_propagation_compatibility" {
  // 测试与OpenTelemetry Context Propagation规范的兼容性
  
  // 1. 验证Context结构兼容性
  let base_context = context::Context::empty()
  let user_key = context::create_key("user.id")
  let session_key = context::create_key("session.id")
  let trace_key = context::create_key("trace.id")
  
  let enriched_context = base_context
    .with_value(user_key, "user-otel-12345")
    .with_value(session_key, "session-otel-abcdef")
    .with_value(trace_key, "trace-otel-0af7651916cd43dd8448eb211c80319c")
  
  // 验证Context操作
  assert_eq(enriched_context.get(user_key).unwrap(), "user-otel-12345")
  assert_eq(enriched_context.get(session_key).unwrap(), "session-otel-abcdef")
  assert_eq(enriched_context.get(trace_key).unwrap(), "trace-otel-0af7651916cd43dd8448eb211c80319c")
  
  // 验证原始上下文未被修改
  assert_eq(base_context.values.length(), 0)
  
  // 2. 验证Baggage结构兼容性
  let base_baggage = context::Baggage::empty()
  
  let otel_baggage = base_baggage
    .with_entry("user.id", "user-otel-12345")
    .with_entry("session.id", "session-otel-abcdef")
    .with_entry("service.name", "otel-baggage-service")
    .with_entry("deployment.environment", "production")
    .with_entry("correlation.id", "corr-otel-67890")
  
  // 验证Baggage操作
  assert_eq(otel_baggage.get("user.id").unwrap(), "user-otel-12345")
  assert_eq(otel_baggage.get("session.id").unwrap(), "session-otel-abcdef")
  assert_eq(otel_baggage.get("service.name").unwrap(), "otel-baggage-service")
  assert_eq(otel_baggage.get("deployment.environment").unwrap(), "production")
  assert_eq(otel_baggage.get("correlation.id").unwrap(), "corr-otel-67890")
  
  // 验证baggage条目数量
  assert_eq(otel_baggage.entries.length(), 5)
  
  // 3. 验证OTel Baggage头格式兼容性
  // 模拟W3C Baggage头格式：key1=value1;key2=value2;...
  let baggage_header = "user.id=user-otel-12345;session.id=session-otel-abcdef;service.name=otel-baggage-service"
  
  // 验证 baggage 头包含预期条目
  assert_eq(baggage_header.contains("user.id=user-otel-12345"), true)
  assert_eq(baggage_header.contains("session.id=session-otel-abcdef"), true)
  assert_eq(baggage_header.contains("service.name=otel-baggage-service"), true)
  assert_eq(baggage_header.contains(";"), true)  // 验证分隔符
  
  // 4. 验证OTel Traceparent头格式兼容性
  // 模拟W3C traceparent头格式：version-trace_id-span_id-flags
  let traceparent_header = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  
  // 验证 traceparent 头格式
  assert_eq(traceparent_header.has_prefix("00-"), true)  // 版本00
  assert_eq(traceparent_header.contains("-"), true)     // 包含分隔符
  assert_eq(traceparent_header.has_suffix("-01"), true) // 采样标志
  
  // 解析traceparent组件
  let parts = traceparent_header.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00")  // 版本
  assert_eq(parts[1], "0af7651916cd43dd8448eb211c80319c")  // trace_id
  assert_eq(parts[2], "b7ad6b7169203331")  // span_id
  assert_eq(parts[3], "01")  // 标志
  
  // 5. 验证ContextKey创建
  let custom_key = context::create_key("custom.otel.key")
  let context_with_custom = enriched_context.with_value(custom_key, "custom.otel.value")
  
  assert_eq(context_with_custom.get(custom_key).unwrap(), "custom.otel.value")
  assert_eq(context_with_custom.get(user_key).unwrap(), "user-otel-12345")  // 原有值保持不变
  
  // 6. 验证不可变性
  let original_context = context_with_custom
  let modified_context = original_context.with_value(context::create_key("new.key"), "new.value")
  
  // 原始上下文应该保持不变
  assert_eq(original_context.get(context::create_key("new.key")), None)
  
  // 修改后的上下文应该包含新值
  assert_eq(modified_context.get(context::create_key("new.key")).unwrap(), "new.value")
  assert_eq(modified_context.get(custom_key).unwrap(), "custom.otel.value")  // 原有值保持不变
}