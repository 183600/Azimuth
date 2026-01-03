// 跨服务遥测一致性测试 - Cross-Service Telemetry Consistency Tests
// 专注于分布式系统中多个服务间遥测数据的一致性和完整性

test "分布式链路追踪一致性测试" {
  // 模拟一个跨多个服务的请求链路
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let services = ["gateway", "auth", "user-service", "order-service", "payment-service", "notification-service"]
  
  // 每个服务生成自己的span
  let spans = []
  for i = 0; i < services.length(); i = i + 1 {
    let service_name = services[i]
    let span_id = "span_" + i.to_string()
    let parent_span_id = if i > 0 { "span_" + (i-1).to_string() } else { "" }
    
    let span = {
      "trace_id": trace_id,
      "span_id": span_id,
      "parent_span_id": parent_span_id,
      "service_name": service_name,
      "operation_name": "process_request",
      "start_time": 1000000000L + i * 1000000L,
      "end_time": 1000000000L + (i + 1) * 1000000L,
      "status": "Ok",
      "attributes": [
        ("service.name", StringValue(service_name)),
        ("service.version", StringValue("1.0.0")),
        ("service.instance.id", StringValue(service_name + "-instance-1"))
      ]
    }
    spans.push(span)
  }
  
  // 验证所有span属于同一trace
  for span in spans {
    assert_eq(span["trace_id"], trace_id)
  }
  
  // 验证span层次结构
  assert_eq(spans[0]["parent_span_id"], "") // root span
  for i = 1; i < spans.length(); i = i + 1 {
    let expected_parent = "span_" + (i-1).to_string()
    assert_eq(spans[i]["parent_span_id"], expected_parent)
  }
  
  // 验证时间顺序
  for i = 1; i < spans.length(); i = i + 1 {
    let prev_end_time = spans[i-1]["end_time"]
    let curr_start_time = spans[i]["start_time"]
    assert_true(prev_end_time <= curr_start_time)
  }
  
  // 验证所有span都有必要的属性
  for span in spans {
    let attrs = span["attributes"]
    assert_true(attrs.some(fn(attr) { attr.0 == "service.name" }))
    assert_true(attrs.some(fn(attr) { attr.0 == "service.version" }))
    assert_true(attrs.some(fn(attr) { attr.0 == "service.instance.id" }))
  }
}

test "跨服务Context传播一致性测试" {
  // 测试Context在多个服务间传播时的一致性
  let initial_context = Context::root()
  
  // Gateway服务：添加初始上下文信息
  let gateway_key = ContextKey::new("gateway.request.id")
  let gateway_context = Context::with_value(initial_context, gateway_key, "req-12345")
  
  // Auth服务：添加认证信息
  let auth_key = ContextKey::new("auth.user.id")
  let auth_context = Context::with_value(gateway_context, auth_key, "user-67890")
  
  // User服务：添加用户信息
  let user_key = ContextKey::new("user.tenant.id")
  let user_context = Context::with_value(auth_context, user_key, "tenant-abc")
  
  // Order服务：添加订单信息
  let order_key = ContextKey::new("order.id")
  let order_context = Context::with_value(user_context, order_key, "order-xyz")
  
  // Payment服务：添加支付信息
  let payment_key = ContextKey::new("payment.transaction.id")
  let payment_context = Context::with_value(order_context, payment_key, "txn-pqr")
  
  // 验证上下文传播的完整性
  let gateway_value = Context::get(payment_context, gateway_key)
  match gateway_value {
    Some(value) => assert_eq(value, "req-12345")
    None => assert_true(false)
  }
  
  let auth_value = Context::get(payment_context, auth_key)
  match auth_value {
    Some(value) => assert_eq(value, "user-67890")
    None => assert_true(false)
  }
  
  let user_value = Context::get(payment_context, user_key)
  match user_value {
    Some(value) => assert_eq(value, "tenant-abc")
    None => assert_true(false)
  }
  
  let order_value = Context::get(payment_context, order_key)
  match order_value {
    Some(value) => assert_eq(value, "order-xyz")
    None => assert_true(false)
  }
  
  let payment_value = Context::get(payment_context, payment_key)
  match payment_value {
    Some(value) => assert_eq(value, "txn-pqr")
    None => assert_true(false)
  }
  
  // 测试通过CompositePropagator的传播
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, payment_context, carrier)
  
  let extracted_context = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}

test "跨服务Baggage传播一致性测试" {
  // 测试Baggage在服务链中的传播和累积
  let services = ["frontend", "gateway", "auth", "api", "database"]
  
  // 初始baggage为空
  let current_baggage = Baggage::new()
  
  // Frontend服务：添加用户界面信息
  current_baggage = Baggage::set_entry(current_baggage, "ui.theme", "dark")
  current_baggage = Baggage::set_entry(current_baggage, "ui.language", "zh-CN")
  
  // Gateway服务：添加请求信息
  current_baggage = Baggage::set_entry(current_baggage, "request.id", "req-12345")
  current_baggage = Baggage::set_entry(current_baggage, "request.source", "web")
  
  // Auth服务：添加认证信息
  current_baggage = Baggage::set_entry(current_baggage, "auth.method", "oauth2")
  current_baggage = Baggage::set_entry(current_baggage, "auth.realm", "company")
  
  // API服务：添加API信息
  current_baggage = Baggage::set_entry(current_baggage, "api.version", "v1")
  current_baggage = Baggage::set_entry(current_baggage, "api.rate.limit", "1000")
  
  // Database服务：添加数据库信息
  current_baggage = Baggage::set_entry(current_baggage, "db.shard", "shard-3")
  current_baggage = Baggage::set_entry(current_baggage, "db.replica", "primary")
  
  // 验证所有baggage条目都存在
  let expected_entries = [
    ("ui.theme", "dark"),
    ("ui.language", "zh-CN"),
    ("request.id", "req-12345"),
    ("request.source", "web"),
    ("auth.method", "oauth2"),
    ("auth.realm", "company"),
    ("api.version", "v1"),
    ("api.rate.limit", "1000"),
    ("db.shard", "shard-3"),
    ("db.replica", "primary")
  ]
  
  for entry in expected_entries {
    let key = entry.0
    let expected_value = entry.1
    let actual_value = Baggage::get_entry(current_baggage, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
  
  // 测试baggage条目的选择性移除
  let filtered_baggage = Baggage::remove_entry(current_baggage, "db.replica")
  let removed_value = Baggage::get_entry(filtered_baggage, "db.replica")
  assert_true(removed_value is None)
  
  // 验证其他条目仍然存在
  let retained_value = Baggage::get_entry(filtered_baggage, "db.shard")
  match retained_value {
    Some(value) => assert_eq(value, "shard-3")
    None => assert_true(false)
  }
}

test "跨服务Metrics一致性测试" {
  // 测试跨服务Metrics的一致性和关联性
  let services = ["service-a", "service-b", "service-c"]
  let operation_name = "user.registration"
  
  // 每个服务创建相关metrics
  let service_metrics = []
  
  for service in services {
    let meter_provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(meter_provider, service)
    
    let request_counter = Meter::create_counter(meter, service + ".requests.total")
    let duration_histogram = Meter::create_histogram(meter, service + ".duration.ms")
    let error_counter = Meter::create_counter(meter, service + ".errors.total")
    
    // 模拟记录metrics
    Counter::add(request_counter, 100.0)
    Histogram::record(duration_histogram, 150.0)
    Counter::add(error_counter, 5.0)
    
    service_metrics.push({
      "service": service,
      "operation": operation_name,
      "requests": 100.0,
      "duration": 150.0,
      "errors": 5.0
    })
  }
  
  // 验证metrics的一致性
  for metrics in service_metrics {
    assert_eq(metrics["operation"], operation_name)
    assert_true(metrics["requests"] > 0.0)
    assert_true(metrics["duration"] > 0.0)
    assert_true(metrics["errors"] >= 0.0)
    assert_true(metrics["errors"] <= metrics["requests"])
  }
  
  // 计算跨服务聚合指标
  let total_requests = service_metrics.map(fn(m) { m["requests"] }).reduce(fn(acc, x) { acc + x }, 0.0)
  let total_errors = service_metrics.map(fn(m) { m["errors"] }).reduce(fn(acc, x) { acc + x }, 0.0)
  let avg_duration = service_metrics.map(fn(m) { m["duration"] }).reduce(fn(acc, x) { acc + x }, 0.0) / services.length().to_double()
  
  assert_eq(total_requests, 300.0)
  assert_eq(total_errors, 15.0)
  assert_eq(avg_duration, 150.0)
  
  // 验证错误率计算
  let error_rate = total_errors / total_requests
  assert_eq(error_rate, 0.05) // 5%错误率
}

test "跨服务日志关联性测试" {
  // 测试跨服务日志的关联性和一致性
  let trace_id = "trace-12345"
  let services = ["web", "api", "database"]
  
  let log_records = []
  
  // 每个服务生成相关日志
  for i = 0; i < services.length(); i = i + 1 {
    let service = services[i]
    let span_id = "span-" + i.to_string()
    
    // 为每个服务生成多个日志记录
    for j = 0; j < 3; j = j + 1 {
      let log_level = if j == 0 { Info } else if j == 1 { Info } else { Info }
      let log_message = service + " operation step " + j.to_string()
      
      let log = LogRecord::new_with_context(
        log_level,
        Some(log_message),
        Some(Attributes::with_attributes(Attributes::new(), [
          ("service.name", StringValue(service)),
          ("service.instance", StringValue(service + "-instance-1")),
          ("operation.step", IntValue(j))
        ])),
        Some(1000000000L + (i * 3 + j) * 1000000L),
        Some(1000000000L + (i * 3 + j) * 1000000L + 100L),
        Some(trace_id),
        Some(span_id),
        Some(Context::root())
      )
      
      log_records.push(log)
    }
  }
  
  // 验证所有日志记录属于同一trace
  for log in log_records {
    assert_eq(LogRecord::trace_id(log), Some(trace_id))
  }
  
  // 验证同一服务的日志有相同的span_id
  for i = 0; i < services.length(); i = i + 1 {
    let service_span_id = "span-" + i.to_string()
    let service_logs = log_records.filter(fn(log) { 
      LogRecord::span_id(log) == Some(service_span_id) 
    })
    assert_eq(service_logs.length(), 3)
  }
  
  // 验证日志时间顺序
  for i = 1; i < log_records.length(); i = i + 1 {
    let prev_timestamp = log_records[i-1].timestamp
    let curr_timestamp = log_records[i].timestamp
    match (prev_timestamp, curr_timestamp) {
      (Some(prev), Some(curr)) => assert_true(prev <= curr)
      _ => assert_true(false)
    }
  }
  
  // 验证日志内容的一致性
  for i = 0; i < services.length(); i = i + 1 {
    let service = services[i]
    let service_logs = log_records.filter(fn(log) {
      match log.attributes {
        Some(attrs) => attrs.values.some(fn(attr) { 
          match attr { (key, StringValue(value)) => key == "service.name" && value == service | _ => false }
        })
        None => false
      }
    })
    assert_eq(service_logs.length(), 3)
  }
}

test "跨服务Resource一致性测试" {
  // 测试跨服务Resource信息的一致性和继承
  let global_resource = Resource::with_attributes(Resource::new(), [
    ("service.namespace", StringValue("production")),
    ("service.environment", StringValue("prod")),
    ("deployment.version", StringValue("v1.2.3")),
    ("datacenter.region", StringValue("us-west-2")),
    ("datacenter.zone", StringValue("us-west-2a"))
  ])
  
  let services = ["user-service", "order-service", "payment-service"]
  let service_resources = []
  
  // 每个服务继承全局资源并添加特定属性
  for service in services {
    let service_specific_attrs = [
      ("service.name", StringValue(service)),
      ("service.version", StringValue("1.0.0")),
      ("service.instance.id", StringValue(service + "-instance-1")),
      ("container.id", StringValue("container-" + service)),
      ("k8s.pod.name", StringValue(service + "-pod-12345"))
    ]
    
    let service_resource = Resource::with_attributes(Resource::new(), service_specific_attrs)
    let merged_resource = Resource::merge(global_resource, service_resource)
    service_resources.push(merged_resource)
  }
  
  // 验证所有服务资源都包含全局属性
  for resource in service_resources {
    let namespace = Resource::get_attribute(resource, "service.namespace")
    match namespace {
      Some(StringValue(value)) => assert_eq(value, "production")
      _ => assert_true(false)
    }
    
    let environment = Resource::get_attribute(resource, "service.environment")
    match environment {
      Some(StringValue(value)) => assert_eq(value, "prod")
      _ => assert_true(false)
    }
    
    let version = Resource::get_attribute(resource, "deployment.version")
    match version {
      Some(StringValue(value)) => assert_eq(value, "v1.2.3")
      _ => assert_true(false)
    }
  }
  
  // 验证每个服务资源都包含特定属性
  for i = 0; i < services.length(); i = i + 1 {
    let service = services[i]
    let resource = service_resources[i]
    
    let service_name = Resource::get_attribute(resource, "service.name")
    match service_name {
      Some(StringValue(value)) => assert_eq(value, service)
      _ => assert_true(false)
    }
    
    let instance_id = Resource::get_attribute(resource, "service.instance.id")
    match instance_id {
      Some(StringValue(value)) => assert_eq(value, service + "-instance-1")
      _ => assert_true(false)
    }
  }
}

test "跨服务异常传播一致性测试" {
  // 测试异常和错误在服务链中的传播和一致性
  let services = ["gateway", "auth", "api", "database"]
  
  // 模拟异常在服务链中的传播
  let error_trace_id = "error-trace-12345"
  let root_error = "Database connection timeout"
  let error_chain = []
  
  // Database服务：原始错误
  let db_error = {
    "service": "database",
    "error.type": "timeout",
    "error.message": root_error,
    "error.code": "DB_TIMEOUT",
    "trace_id": error_trace_id,
    "span_id": "db-span-123",
    "parent_span_id": "api-span-456"
  }
  error_chain.push(db_error)
  
  // API服务：包装并传播错误
  let api_error = {
    "service": "api",
    "error.type": "dependency_error",
    "error.message": "Failed to process request due to database error: " + root_error,
    "error.code": "DEPENDENCY_ERROR",
    "trace_id": error_trace_id,
    "span_id": "api-span-456",
    "parent_span_id": "auth-span-789",
    "cause": db_error
  }
  error_chain.push(api_error)
  
  // Auth服务：进一步传播错误
  let auth_error = {
    "service": "auth",
    "error.type": "service_error",
    "error.message": "Authentication service unavailable: " + api_error["error.message"],
    "error.code": "SERVICE_ERROR",
    "trace_id": error_trace_id,
    "span_id": "auth-span-789",
    "parent_span_id": "gateway-span-001",
    "cause": api_error
  }
  error_chain.push(auth_error)
  
  // Gateway服务：最终返回给客户端的错误
  let gateway_error = {
    "service": "gateway",
    "error.type": "upstream_error",
    "error.message": "Service temporarily unavailable",
    "error.code": "UPSTREAM_ERROR",
    "trace_id": error_trace_id,
    "span_id": "gateway-span-001",
    "parent_span_id": "",
    "cause": auth_error
  }
  error_chain.push(gateway_error)
  
  // 验证错误链的一致性
  for i = 0; i < error_chain.length(); i = i + 1 {
    let error = error_chain[i]
    assert_eq(error["trace_id"], error_trace_id)
    assert_true(error["error.message"].length() > 0)
    assert_true(error["error.code"].length() > 0)
    
    // 验证span层次结构
    if i < error_chain.length() - 1 {
      let next_error = error_chain[i+1]
      assert_eq(error["span_id"], next_error["parent_span_id"])
    }
  }
  
  // 验证根错误信息在整个链路中保持
  let final_error = error_chain[error_chain.length() - 1]
  assert_true(final_error["error.message"].contains("temporarily")) // 最终用户友好的消息
  
  // 验证可以追溯到根错误
  let current_error = final_error
  while current_error.contains_key("cause") {
    current_error = current_error["cause"]
  }
  assert_eq(current_error["error.message"], root_error)
}