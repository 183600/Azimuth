// 跨模块集成测试用例

test "cross_module_context_logs_integration" {
  // 测试上下文和日志模块的集成
  
  // 创建带有追踪信息的上下文
  let base_context = Context::empty()
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  let user_id_key = create_key("user_id")
  
  let enriched_context = base_context
    .with_value(trace_id_key, "0af7651916cd43dd8448eb211c80319c")
    .with_value(span_id_key, "b7ad6b7169203331")
    .with_value(user_id_key, "user-12345")
  
  // 从上下文提取信息创建日志记录
  let trace_id = enriched_context.get(trace_id_key).unwrap_or("unknown")
  let span_id = enriched_context.get(span_id_key).unwrap_or("unknown")
  let user_id = enriched_context.get(user_id_key).unwrap_or("anonymous")
  
  // 创建包含上下文信息的日志记录
  let log_record = LogRecord::builder()
    .severity(Info)
    .body("User action completed successfully")
    .with_attribute("trace.id", AttributeValue::string(trace_id))
    .with_attribute("span.id", AttributeValue::string(span_id))
    .with_attribute("user.id", AttributeValue::string(user_id))
    .with_attribute("action.type", AttributeValue::string("purchase"))
    .with_attribute("action.result", AttributeValue::string("success"))
    .build()
  
  // 验证集成结果
  assert_eq(log_record.severity_number, Info)
  assert_eq(log_record.body, Some("User action completed successfully"))
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证上下文信息正确传递到日志
  let mut trace_found = false
  let mut span_found = false
  let mut user_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match (key, value) {
      ("trace.id", StringValue(id)) => {
        assert_eq(id, "0af7651916cd43dd8448eb211c80319c")
        trace_found = true
      }
      ("span.id", StringValue(id)) => {
        assert_eq(id, "b7ad6b7169203331")
        span_found = true
      }
      ("user.id", StringValue(id)) => {
        assert_eq(id, "user-12345")
        user_found = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(trace_found, true)
  assert_eq(span_found, true)
  assert_eq(user_found, true)
}

test "cross_module_metrics_context_integration" {
  // 测试指标和上下文模块的集成
  
  // 创建服务资源
  let resource = Resource::default("order-service")
  
  // 创建带有上下文信息的指标
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("order-meter", "1.0.0")
  let counter = meter.create_counter("orders_processed", "count", "Number of processed orders")
  
  // 创建上下文
  let context = Context::empty()
  let region_key = create_key("region")
  let version_key = create_key("service_version")
  
  let enriched_context = context
    .with_value(region_key, "us-west-2")
    .with_value(version_key, "2.1.0")
  
  // 从上下文提取属性用于指标
  let region = enriched_context.get(region_key).unwrap_or("unknown")
  let version = enriched_context.get(version_key).unwrap_or("unknown")
  
  // 创建带有上下文属性的指标记录
  let metric_attributes = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("service.region", AttributeValue::string(region)),
    ("service.version", AttributeValue::string(version)),
    ("order.status", AttributeValue::string("completed")),
    ("order.type", AttributeValue::string("online"))
  ]
  
  // 记录指标
  counter.add(1L, metric_attributes)
  
  // 验证集成操作不会崩溃
  assert_eq(true, true)
}

test "cross_module_propagation_logs_integration" {
  // 测试传播和日志模块的集成
  
  // 创建传播器
  let propagator = W3CTraceContextPropagator::{}
  let context = Context::empty()
  let carrier = MapCarrier::new()
  
  // 注入追踪信息到载体
  propagator.inject(context, carrier)
  
  // 从载体提取追踪信息用于日志
  let trace_parent = carrier.get("traceparent").unwrap_or("")
  
  // 解析traceparent格式（简化版本）
  let trace_info = if trace_parent.length() > 0 {
    let parts = trace_parent.split("-")
    if parts.length() >= 3 {
      let trace_id = parts[1]
      let span_id = parts[2]
      (trace_id, span_id)
    } else {
      ("unknown", "unknown")
    }
  } else {
    ("unknown", "unknown")
  }
  
  // 创建包含传播信息的日志记录
  let log_record = LogRecord::builder()
    .severity(Debug)
    .body("Trace context propagated successfully")
    .with_attribute("trace.id", AttributeValue::string(trace_info.0))
    .with_attribute("span.id", AttributeValue::string(trace_info.1))
    .with_attribute("propagation.format", AttributeValue::string("w3c-tracecontext"))
    .build()
  
  // 验证集成结果
  assert_eq(log_record.severity_number, Debug)
  assert_eq(log_record.body, Some("Trace context propagated successfully"))
  assert_eq(log_record.attributes.length(), 3)
}

test "cross_module_resource_context_integration" {
  // 测试资源和上下文模块的集成
  
  // 创建详细的资源信息
  let resource = Resource::{
    service_name: "payment-service",
    service_version: Some("3.2.1"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", AttributeValue::string("finance")),
      ("service.instance.id", AttributeValue::string("payment-001")),
      ("deployment.environment", AttributeValue::string("production")),
      ("datacenter.region", AttributeValue::string("eu-central-1"))
    ]
  }
  
  // 创建上下文并添加资源信息
  let context = Context::empty()
  let service_name_key = create_key("service_name")
  let service_version_key = create_key("service_version")
  let instance_id_key = create_key("instance_id")
  let region_key = create_key("region")
  
  let enriched_context = context
    .with_value(service_name_key, resource.service_name)
    .with_value(service_version_key, resource.service_version.unwrap_or("unknown"))
    .with_value(instance_id_key, "payment-001")
    .with_value(region_key, "eu-central-1")
  
  // 创建日志记录，结合资源和上下文信息
  let log_record = LogRecord::builder()
    .severity(Info)
    .body("Payment processing initiated")
    .with_attribute("service.name", AttributeValue::string(enriched_context.get(service_name_key).unwrap_or("")))
    .with_attribute("service.version", AttributeValue::string(enriched_context.get(service_version_key).unwrap_or("")))
    .with_attribute("service.instance.id", AttributeValue::string(enriched_context.get(instance_id_key).unwrap_or("")))
    .with_attribute("datacenter.region", AttributeValue::string(enriched_context.get(region_key).unwrap_or("")))
    .with_attribute("payment.amount", AttributeValue::float(99.99))
    .with_attribute("payment.currency", AttributeValue::string("USD"))
    .build()
  
  // 验证集成结果
  assert_eq(log_record.body, Some("Payment processing initiated"))
  assert_eq(log_record.attributes.length(), 7)
  
  // 验证资源信息正确传递
  let mut service_name_found = false
  let mut service_version_found = false
  let mut instance_id_found = false
  let mut region_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match (key, value) {
      ("service.name", StringValue(name)) => {
        assert_eq(name, "payment-service")
        service_name_found = true
      }
      ("service.version", StringValue(version)) => {
        assert_eq(version, "3.2.1")
        service_version_found = true
      }
      ("service.instance.id", StringValue(instance)) => {
        assert_eq(instance, "payment-001")
        instance_id_found = true
      }
      ("datacenter.region", StringValue(region)) => {
        assert_eq(region, "eu-central-1")
        region_found = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(service_name_found, true)
  assert_eq(service_version_found, true)
  assert_eq(instance_id_found, true)
  assert_eq(region_found, true)
}