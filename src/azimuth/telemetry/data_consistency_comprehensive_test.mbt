// 数据一致性测试 - 测试跨模块数据一致性

test "trace_context_consistency_across_modules" {
  // 测试Trace Context在不同模块间的一致性
  
  // 1. 创建初始TraceContext
  let initial_trace_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
  let initial_span_id = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
  let initial_span_context = SpanContext::{
    trace_id: initial_trace_id,
    span_id: initial_span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 2. 在Trace模块中创建Span
  let (_, parent_span) = NoopTracer::start_span(
    Context::empty(),
    "parent_operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("test-service")),
      ("operation.type", AttributeValue::string("parent"))
    ])
  )
  
  // 3. 在Context模块中传播Trace信息
  let ctx_with_trace = Context::empty()
    .with_value(create_key("trace.id"), "1234567890abcdef1234567890abcdef")
    .with_value(create_key("span.id"), "1122334455667788")
    .with_value(create_key("trace.flags"), "01")
    .with_value(create_key("trace.state"), "key1=value1,key2=value2")
  
  // 4. 在Metrics模块中使用Trace信息
  let meter = NoopMeter::{}
  let operation_counter = meter.create_counter("operations_total", Some("operations"), Some("Total operations"))
  
  let trace_attributes = [
    ("trace.id", AttributeValue::string("1234567890abcdef1234567890abcdef")),
    ("span.id", AttributeValue::string("1122334455667788")),
    ("service.name", AttributeValue::string("test-service")),
    ("operation.type", AttributeValue::string("traced_operation"))
  ]
  
  operation_counter.add(1L, Some(trace_attributes))
  
  // 5. 在Logs模块中使用Trace信息
  let logger = NoopLogger::{}
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("TRACE_CONSISTENCY_TEST"),
    body: Some("Testing trace consistency across modules"),
    attributes: [
      ("trace.id", AttributeValue::string("1234567890abcdef1234567890abcdef")),
      ("span.id", AttributeValue::string("1122334455667788")),
      ("service.name", AttributeValue::string("test-service")),
      ("log.type", AttributeValue::string("consistency_check"))
    ],
    trace_id: Some(initial_trace_id),
    span_id: Some(initial_span_id),
    trace_flags: Some(1_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  logger.emit(log_record)
  
  // 6. 验证Trace ID一致性
  let expected_trace_id_str = "1234567890abcdef1234567890abcdef"
  let expected_span_id_str = "1122334455667788"
  
  // 验证Context中的Trace信息
  match ctx_with_trace.get(create_key("trace.id")) {
    Some(trace_id) => assert_eq(trace_id, expected_trace_id_str)
    None => @test.fail("Test failed")
  }
  
  match ctx_with_trace.get(create_key("span.id")) {
    Some(span_id) => assert_eq(span_id, expected_span_id_str)
    None => @test.fail("Test failed")
  }
  
  // 验证Metrics中的Trace属性
  assert_eq(trace_attributes[0].0, "trace.id")
  match trace_attributes[0].1 {
    StringValue(trace_id) => assert_eq(trace_id, expected_trace_id_str)
    _ => @test.fail("Test failed")
  }
  
  assert_eq(trace_attributes[1].0, "span.id")
  match trace_attributes[1].1 {
    StringValue(span_id) => assert_eq(span_id, expected_span_id_str)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的Trace信息
  match log_record.trace_id {
    Some(trace_id_bytes) => {
      assert_eq(trace_id_bytes.length(), 16)
      assert_eq(trace_id_bytes[0], 0x12_byte)
      assert_eq(trace_id_bytes[15], 0x88_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(span_id_bytes) => {
      assert_eq(span_id_bytes.length(), 8)
      assert_eq(span_id_bytes[0], 0x11_byte)
      assert_eq(span_id_bytes[7], 0x88_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Test failed")
  }
  
  // 验证LogRecord属性中的Trace信息
  assert_eq(log_record.attributes[0].0, "trace.id")
  match log_record.attributes[0].1 {
    StringValue(trace_id) => assert_eq(trace_id, expected_trace_id_str)
    _ => @test.fail("Test failed")
  }
}

test "baggage_consistency_across_apis" {
  // 测试Baggage在不同API间的一致性
  
  // 1. 创建初始Baggage
  let initial_baggage = Baggage::empty()
    .with_entry("user.id", "user123")
    .with_entry("session.id", "sess456")
    .with_entry("request.id", "req789")
    .with_entry("service.name", "api-gateway")
    .with_entry("service.version", "2.1.0")
    .with_entry("deployment.env", "production")
    .with_entry("region", "us-west-2")
    .with_entry("trace.sampled", "true")
  
  // 2. 在Context中传递Baggage信息
  let ctx_with_baggage = Context::empty()
    .with_value(create_key("baggage.user.id"), "user123")
    .with_value(create_key("baggage.session.id"), "sess456")
    .with_value(create_key("baggage.request.id"), "req789")
    .with_value(create_key("baggage.service.name"), "api-gateway")
    .with_value(create_key("baggage.deployment.env"), "production")
    .with_value(create_key("baggage.region"), "us-west-2")
  
  // 3. 在Trace中使用Baggage信息
  let baggage_attributes = [
    ("user.id", AttributeValue::string("user123")),
    ("session.id", AttributeValue::string("sess456")),
    ("request.id", AttributeValue::string("req789")),
    ("service.name", AttributeValue::string("api-gateway")),
    ("deployment.env", AttributeValue::string("production")),
    ("region", AttributeValue::string("us-west-2"))
  ]
  
  let (_, baggage_span) = NoopTracer::start_span(
    ctx_with_baggage,
    "operation_with_baggage",
    Server,
    Some(baggage_attributes)
  )
  
  // 4. 在Metrics中使用Baggage信息
  let meter = NoopMeter::{}
  let baggage_counter = meter.create_counter("baggage_operations_total", Some("operations"), Some("Operations with baggage"))
  
  baggage_counter.add(1L, Some(baggage_attributes))
  
  // 5. 在Logs中使用Baggage信息
  let logger = NoopLogger::{}
  
  logger.info("Operation with baggage", Some([
    ("user.id", AttributeValue::string("user123")),
    ("session.id", AttributeValue::string("sess456")),
    ("request.id", AttributeValue::string("req789")),
    ("service.name", AttributeValue::string("api-gateway")),
    ("deployment.env", AttributeValue::string("production")),
    ("region", AttributeValue::string("us-west-2")),
    ("log.type", AttributeValue::string("baggage_consistency"))
  ]))
  
  // 6. 验证Baggage一致性
  // 验证原始Baggage
  match initial_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => @test.fail("Test failed")
  }
  
  match initial_baggage.get("deployment.env") {
    Some(env) => assert_eq(env, "production")
    None => @test.fail("Test failed")
  }
  
  // 验证Context中的Baggage信息
  match ctx_with_baggage.get(create_key("baggage.user.id")) {
    Some(user_id) => assert_eq(user_id, "user123")
    None => @test.fail("Test failed")
  }
  
  // 验证Span中的Baggage属性
  assert_eq(baggage_span.attributes.length(), 6)
  assert_eq(baggage_span.attributes[0].0, "user.id")
  match baggage_span.attributes[0].1 {
    StringValue(user_id) => assert_eq(user_id, "user123")
    _ => @test.fail("Test failed")
  }
  
  // 验证所有模块中的用户ID一致
  let expected_user_id = "user123"
  
  match initial_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, expected_user_id)
    None => @test.fail("Test failed")
  }
  
  match ctx_with_baggage.get(create_key("baggage.user.id")) {
    Some(user_id) => assert_eq(user_id, expected_user_id)
    None => @test.fail("Test failed")
  }
  
  match baggage_span.attributes[0].1 {
    StringValue(user_id) => assert_eq(user_id, expected_user_id)
    _ => @test.fail("Test failed")
  }
}

test "resource_consistency_across_telemetry" {
  // 测试Resource信息在不同遥测组件中的一致性
  
  // 1. 创建统一的Resource
  let service_resource = Resource::default("payment-service")
  let additional_resource_attrs = [
    ("service.namespace", AttributeValue::string("financial")),
    ("service.version", AttributeValue::string("3.2.1")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("payment-server-01")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("payment-service")),
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.region", AttributeValue::string("us-east-1")),
    ("cloud.availability_zone", AttributeValue::string("us-east-1a"))
  ]
  
  // 2. 在Trace中使用Resource信息
  let resource_trace_attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.namespace", AttributeValue::string("financial")),
    ("service.version", AttributeValue::string("3.2.1")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("payment-server-01"))
  ]
  
  let (_, resource_span) = NoopTracer::start_span(
    Context::empty(),
    "payment_operation",
    Server,
    Some(resource_trace_attributes)
  )
  
  // 3. 在Metrics中使用Resource信息
  let meter = NoopMeter::{}
  let payment_counter = meter.create_counter("payments_total", Some("payments"), Some("Total payments"))
  
  let resource_metrics_attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.namespace", AttributeValue::string("financial")),
    ("service.version", AttributeValue::string("3.2.1")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("payment-server-01")),
    ("cloud.region", AttributeValue::string("us-east-1"))
  ]
  
  payment_counter.add(1L, Some(resource_metrics_attributes))
  
  // 4. 在Logs中使用Resource信息
  let logger = NoopLogger::{}
  
  let resource_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("RESOURCE_CONSISTENCY"),
    body: Some("Testing resource consistency across telemetry"),
    attributes: [
      ("service.name", AttributeValue::string("payment-service")),
      ("service.namespace", AttributeValue::string("financial")),
      ("service.version", AttributeValue::string("3.2.1")),
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("payment-server-01")),
      ("cloud.region", AttributeValue::string("us-east-1"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(service_resource),
    instrumentation_scope: None
  }
  
  logger.emit(resource_log_record)
  
  // 5. 验证Resource一致性
  let expected_service_name = "payment-service"
  let expected_service_version = "3.2.1"
  let expected_deployment_env = "production"
  let expected_host_name = "payment-server-01"
  
  // 验证基础Resource
  assert_eq(service_resource.service_name, expected_service_name)
  assert_eq(service_resource.telemetry_sdk_name, "azimuth")
  assert_eq(service_resource.telemetry_sdk_version, "0.1.0")
  
  // 验证Span中的Resource属性
  assert_eq(resource_span.attributes.length(), 5)
  match resource_span.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
  
  match resource_span.attributes[2].1 {
    StringValue(service_version) => assert_eq(service_version, expected_service_version)
    _ => @test.fail("Test failed")
  }
  
  // 验证Metrics中的Resource属性
  assert_eq(resource_metrics_attributes.length(), 6)
  match resource_metrics_attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的Resource属性
  assert_eq(resource_log_record.attributes.length(), 6)
  match resource_log_record.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord关联的Resource
  match resource_log_record.resource {
    Some(resource) => assert_eq(resource.service_name, expected_service_name)
    None => @test.fail("Test failed")
  }
  
  // 验证所有遥测组件中的服务名称一致
  assert_eq(service_resource.service_name, expected_service_name)
  match resource_span.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
  match resource_metrics_attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
  match resource_log_record.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, expected_service_name)
    _ => @test.fail("Test failed")
  }
}

test "instrumentation_scope_consistency" {
  // 测试InstrumentationScope在不同遥测组件中的一致性
  
  // 1. 创建统一的InstrumentationScope
  let instrumentation_scope = InstrumentationScope::{
    name: "payment-processor",
    version: Some("1.5.2"),
    schema_url: Some("https://example.com/schemas/payment")
  }
  
  // 2. 在Trace中使用InstrumentationScope
  let scope_trace_attributes = [
    ("instrumentation.scope.name", AttributeValue::string("payment-processor")),
    ("instrumentation.scope.version", AttributeValue::string("1.5.2")),
    ("instrumentation.scope.schema", AttributeValue::string("https://example.com/schemas/payment"))
  ]
  
  let (_, scope_span) = NoopTracer::start_span(
    Context::empty(),
    "process_payment",
    Server,
    Some(scope_trace_attributes)
  )
  
  // 3. 在Metrics中使用InstrumentationScope
  let meter = NoopMeter::{}
  let scope_counter = meter.create_counter("payment_operations_total", Some("operations"), Some("Payment operations"))
  
  let scope_metrics_attributes = [
    ("instrumentation.scope.name", AttributeValue::string("payment-processor")),
    ("instrumentation.scope.version", AttributeValue::string("1.5.2")),
    ("operation.type", AttributeValue::string("process_payment"))
  ]
  
  scope_counter.add(1L, Some(scope_metrics_attributes))
  
  // 4. 在Logs中使用InstrumentationScope
  let logger = NoopLogger::{}
  
  let scope_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("SCOPE_CONSISTENCY"),
    body: Some("Testing instrumentation scope consistency"),
    attributes: [
      ("instrumentation.scope.name", AttributeValue::string("payment-processor")),
      ("instrumentation.scope.version", AttributeValue::string("1.5.2")),
      ("operation.type", AttributeValue::string("process_payment"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  logger.emit(scope_log_record)
  
  // 5. 验证InstrumentationScope一致性
  let expected_scope_name = "payment-processor"
  let expected_scope_version = "1.5.2"
  let expected_schema_url = "https://example.com/schemas/payment"
  
  // 验证基础InstrumentationScope
  assert_eq(instrumentation_scope.name, expected_scope_name)
  match instrumentation_scope.version {
    Some(version) => assert_eq(version, expected_scope_version)
    None => @test.fail("Test failed")
  }
  
  match instrumentation_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, expected_schema_url)
    None => @test.fail("Test failed")
  }
  
  // 验证Span中的InstrumentationScope属性
  assert_eq(scope_span.attributes.length(), 3)
  match scope_span.attributes[0].1 {
    StringValue(scope_name) => assert_eq(scope_name, expected_scope_name)
    _ => @test.fail("Test failed")
  }
  
  match scope_span.attributes[1].1 {
    StringValue(scope_version) => assert_eq(scope_version, expected_scope_version)
    _ => @test.fail("Test failed")
  }
  
  // 验证Metrics中的InstrumentationScope属性
  assert_eq(scope_metrics_attributes.length(), 3)
  match scope_metrics_attributes[0].1 {
    StringValue(scope_name) => assert_eq(scope_name, expected_scope_name)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的InstrumentationScope属性
  assert_eq(scope_log_record.attributes.length(), 3)
  match scope_log_record.attributes[0].1 {
    StringValue(scope_name) => assert_eq(scope_name, expected_scope_name)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord关联的InstrumentationScope
  match scope_log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, expected_scope_name)
      match scope.version {
        Some(version) => assert_eq(version, expected_scope_version)
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "attribute_value_type_consistency" {
  // 测试AttributeValue类型在不同遥测组件中的一致性
  
  // 1. 创建各种类型的AttributeValue
  let string_attr = AttributeValue::string("test_value")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  let string_array_attr = AttributeValue::array_string(["item1", "item2", "item3"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 2. 在Trace中使用这些属性
  let mixed_attributes = [
    ("string.key", string_attr),
    ("int.key", int_attr),
    ("float.key", float_attr),
    ("bool.key", bool_attr),
    ("string.array.key", string_array_attr),
    ("int.array.key", int_array_attr),
    ("float.array.key", float_array_attr),
    ("bool.array.key", bool_array_attr)
  ]
  
  let (_, mixed_span) = NoopTracer::start_span(
    Context::empty(),
    "mixed_attributes_test",
    Server,
    Some(mixed_attributes)
  )
  
  // 3. 在Metrics中使用这些属性
  let meter = NoopMeter::{}
  let mixed_counter = meter.create_counter("mixed_operations_total", Some("operations"), Some("Operations with mixed attributes"))
  
  mixed_counter.add(1L, Some(mixed_attributes))
  
  // 4. 在Logs中使用这些属性
  let logger = NoopLogger::{}
  
  let mixed_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("MIXED_ATTRIBUTES"),
    body: Some("Testing mixed attribute types"),
    attributes: mixed_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  logger.emit(mixed_log_record)
  
  // 5. 验证AttributeValue类型一致性
  // 验证Span中的属性类型
  assert_eq(mixed_span.attributes.length(), 8)
  
  match mixed_span.attributes[0].1 {
    StringValue(value) => assert_eq(value, "test_value")
    _ => @test.fail("Test failed")
  }
  
  match mixed_span.attributes[1].1 {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Test failed")
  }
  
  match mixed_span.attributes[2].1 {
    FloatValue(value) => assert_eq(value, 3.14159)
    _ => @test.fail("Test failed")
  }
  
  match mixed_span.attributes[3].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  match mixed_span.attributes[4].1 {
    ArrayStringValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], "item1")
      assert_eq(array[1], "item2")
      assert_eq(array[2], "item3")
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的属性类型
  assert_eq(mixed_log_record.attributes.length(), 8)
  
  match mixed_log_record.attributes[0].1 {
    StringValue(value) => assert_eq(value, "test_value")
    _ => @test.fail("Test failed")
  }
  
  match mixed_log_record.attributes[1].1 {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Test failed")
  }
  
  match mixed_log_record.attributes[2].1 {
    FloatValue(value) => assert_eq(value, 3.14159)
    _ => @test.fail("Test failed")
  }
  
  match mixed_log_record.attributes[3].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证数组类型的一致性
  match mixed_span.attributes[5].1 {
    ArrayIntValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], 1L)
      assert_eq(array[1], 2L)
      assert_eq(array[2], 3L)
    }
    _ => @test.fail("Test failed")
  }
  
  match mixed_log_record.attributes[5].1 {
    ArrayIntValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], 1L)
      assert_eq(array[1], 2L)
      assert_eq(array[2], 3L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证Span和LogRecord中相同位置的属性类型一致
  let mut i = 0
  while i < mixed_attributes.length() {
    let span_attr_type = match mixed_span.attributes[i].1 {
      StringValue(_) => "string"
      IntValue(_) => "int"
      FloatValue(_) => "float"
      BoolValue(_) => "bool"
      ArrayStringValue(_) => "array_string"
      ArrayIntValue(_) => "array_int"
      ArrayFloatValue(_) => "array_float"
      ArrayBoolValue(_) => "array_bool"
    }
    
    let log_attr_type = match mixed_log_record.attributes[i].1 {
      StringValue(_) => "string"
      IntValue(_) => "int"
      FloatValue(_) => "float"
      BoolValue(_) => "bool"
      ArrayStringValue(_) => "array_string"
      ArrayIntValue(_) => "array_int"
      ArrayFloatValue(_) => "array_float"
      ArrayBoolValue(_) => "array_bool"
    }
    
    assert_eq(span_attr_type, log_attr_type)
    i = i + 1
  }
}

test "cross_module_data_integrity" {
  // 测试跨模块数据完整性
  
  // 1. 创建复杂的数据结构
  let complex_data = [
    ("transaction.id", AttributeValue::string("txn_123456789")),
    ("transaction.amount", AttributeValue::float(299.99)),
    ("transaction.currency", AttributeValue::string("USD")),
    ("user.id", AttributeValue::string("user_987654321")),
    ("user.tier", AttributeValue::string("premium")),
    ("merchant.id", AttributeValue::string("merchant_555666777")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("payment.gateway", AttributeValue::string("acme_bank")),
    ("geolocation.country", AttributeValue::string("US")),
    ("geolocation.region", AttributeValue::string("CA")),
    ("device.type", AttributeValue::string("mobile")),
    ("device.os", AttributeValue::string("iOS")),
    ("risk.score", AttributeValue::int(25L)),
    ("fraud.detected", AttributeValue::bool(false)),
    ("tags", AttributeValue::array_string(["api", "payment", "mobile", "premium"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 400L])),
    ("latency.thresholds", AttributeValue::array_float([0.1, 0.5, 1.0, 2.5])),
    ("feature.flags", AttributeValue::array_bool([true, false, true, true, false]))
  ]
  
  // 2. 在Context中传递复杂数据
  let ctx_with_complex_data = Context::empty()
    .with_value(create_key("transaction.id"), "txn_123456789")
    .with_value(create_key("transaction.amount"), "299.99")
    .with_value(create_key("user.id"), "user_987654321")
    .with_value(create_key("payment.method"), "credit_card")
    .with_value(create_key("risk.score"), "25")
    .with_value(create_key("fraud.detected"), "false")
  
  // 3. 在Trace中使用复杂数据
  let (_, complex_span) = NoopTracer::start_span(
    ctx_with_complex_data,
    "complex_transaction",
    Server,
    Some(complex_data)
  )
  
  // 4. 在Metrics中使用复杂数据
  let meter = NoopMeter::{}
  let complex_counter = meter.create_counter("complex_transactions_total", Some("transactions"), Some("Complex transactions"))
  let complex_histogram = meter.create_histogram("transaction_amount", Some("USD"), Some("Transaction amount"))
  let complex_gauge = meter.create_gauge("risk_score", Some("score"), Some("Risk score"))
  
  complex_counter.add(1L, Some(complex_data))
  complex_histogram.record(299.99, Some(complex_data))
  complex_gauge.record(25.0, Some(complex_data))
  
  // 5. 在Logs中使用复杂数据
  let logger = NoopLogger::{}
  
  let complex_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("COMPLEX_TRANSACTION"),
    body: Some("Complex transaction processed successfully"),
    attributes: complex_data,
    trace_id: Some([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]),
    span_id: Some([0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]),
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("payment-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "payment-processor",
      version: Some("1.5.2"),
      schema_url: None
    })
  }
  
  logger.emit(complex_log_record)
  
  // 6. 验证数据完整性
  let expected_transaction_id = "txn_123456789"
  let expected_user_id = "user_987654321"
  let expected_amount = 299.99
  let expected_risk_score = 25L
  
  // 验证Context中的数据
  match ctx_with_complex_data.get(create_key("transaction.id")) {
    Some(txn_id) => assert_eq(txn_id, expected_transaction_id)
    None => @test.fail("Test failed")
  }
  
  match ctx_with_complex_data.get(create_key("user.id")) {
    Some(user_id) => assert_eq(user_id, expected_user_id)
    None => @test.fail("Test failed")
  }
  
  // 验证Span中的复杂数据
  assert_eq(complex_span.attributes.length(), 19)
  match complex_span.attributes[0].1 {
    StringValue(txn_id) => assert_eq(txn_id, expected_transaction_id)
    _ => @test.fail("Test failed")
  }
  
  match complex_span.attributes[1].1 {
    FloatValue(amount) => assert_eq(amount, expected_amount)
    _ => @test.fail("Test failed")
  }
  
  match complex_span.attributes[3].1 {
    StringValue(user_id) => assert_eq(user_id, expected_user_id)
    _ => @test.fail("Test failed")
  }
  
  match complex_span.attributes[12].1 {
    IntValue(risk_score) => assert_eq(risk_score, expected_risk_score)
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的复杂数据
  assert_eq(complex_log_record.attributes.length(), 19)
  match complex_log_record.attributes[0].1 {
    StringValue(txn_id) => assert_eq(txn_id, expected_transaction_id)
    _ => @test.fail("Test failed")
  }
  
  match complex_log_record.attributes[1].1 {
    FloatValue(amount) => assert_eq(amount, expected_amount)
    _ => @test.fail("Test failed")
  }
  
  // 验证数组数据的完整性
  match complex_span.attributes[15].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 4)
      assert_eq(tags[0], "api")
      assert_eq(tags[1], "payment")
      assert_eq(tags[2], "mobile")
      assert_eq(tags[3], "premium")
    }
    _ => @test.fail("Test failed")
  }
  
  match complex_log_record.attributes[15].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 4)
      assert_eq(tags[0], "api")
      assert_eq(tags[1], "payment")
      assert_eq(tags[2], "mobile")
      assert_eq(tags[3], "premium")
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证Span和LogRecord中复杂数据的一致性
  let mut i = 0
  while i < complex_data.length() {
    let span_attr = complex_span.attributes[i]
    let log_attr = complex_log_record.attributes[i]
    
    assert_eq(span_attr.0, log_attr.0)
    
    match (span_attr.1, log_attr.1) {
      (StringValue(span_val), StringValue(log_val)) => assert_eq(span_val, log_val)
      (IntValue(span_val), IntValue(log_val)) => assert_eq(span_val, log_val)
      (FloatValue(span_val), FloatValue(log_val)) => assert_eq(span_val, log_val)
      (BoolValue(span_val), BoolValue(log_val)) => assert_eq(span_val, log_val)
      (ArrayStringValue(span_val), ArrayStringValue(log_val)) => {
        assert_eq(span_val.length(), log_val.length())
        let mut j = 0
        while j < span_val.length() {
          assert_eq(span_val[j], log_val[j])
          j = j + 1
        }
      }
      (ArrayIntValue(span_val), ArrayIntValue(log_val)) => {
        assert_eq(span_val.length(), log_val.length())
        let mut j = 0
        while j < span_val.length() {
          assert_eq(span_val[j], log_val[j])
          j = j + 1
        }
      }
      (ArrayFloatValue(span_val), ArrayFloatValue(log_val)) => {
        assert_eq(span_val.length(), log_val.length())
        let mut j = 0
        while j < span_val.length() {
          assert_eq(span_val[j], log_val[j])
          j = j + 1
        }
      }
      (ArrayBoolValue(span_val), ArrayBoolValue(log_val)) => {
        assert_eq(span_val.length(), log_val.length())
        let mut j = 0
        while j < span_val.length() {
          assert_eq(span_val[j], log_val[j])
          j = j + 1
        }
      }
      _ => @test.fail("Attribute type mismatch")
    }
    
    i = i + 1
  }
}