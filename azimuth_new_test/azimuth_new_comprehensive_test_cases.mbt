// Azimuth 新增综合测试用例
// 包含遥测系统的序列化、分布式追踪、性能测试、安全性等多方面测试

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)],
    events: Array[(Int, String, Array[(String, String)])],
    links: Array[(String, String, Array[(String, String)])]
  }
  
  // 简单的JSON序列化函数
  let serialize_to_json = fn(data: TelemetryData) {
    let attributes_json = data.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    let events_json = data.events.map(fn(event) {
      let (timestamp, name, event_attrs) = event
      let event_attrs_json = event_attrs.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      "{\"timestamp\":" + timestamp.to_string() + ",\"name\":\"" + name + "\",\"attributes\":{" + event_attrs_json + "}}"
    }).join(",")
    
    let links_json = data.links.map(fn(link) {
      let (trace_id, span_id, link_attrs) = link
      let link_attrs_json = link_attrs.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      "{\"trace_id\":\"" + trace_id + "\",\"span_id\":\"" + span_id + "\",\"attributes\":{" + link_attrs_json + "}}"
    }).join(",")
    
    let parent_json = match data.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + data.trace_id + "\"," +
    "\"span_id\":\"" + data.span_id + "\"," +
    "\"parent_span_id\":" + parent_json + "," +
    "\"operation_name\":\"" + data.operation_name + "\"," +
    "\"start_time\":" + data.start_time.to_string() + "," +
    "\"end_time\":" + data.end_time.to_string() + "," +
    "\"status\":\"" + data.status + "\"," +
    "\"attributes\":{" + attributes_json + "}," +
    "\"events\":[" + events_json + "]," +
    "\"links\":[" + links_json + "]" +
    "}"
  }
  
  // 简单的JSON反序列化函数（简化版）
  let deserialize_from_json = fn(json: String) {
    // 在实际实现中，这里会有完整的JSON解析逻辑
    // 这里简化为检查JSON格式是否正确
    let has_trace_id = json.contains("\"trace_id\"")
    let has_span_id = json.contains("\"span_id\"")
    let has_operation_name = json.contains("\"operation_name\"")
    let has_attributes = json.contains("\"attributes\"")
    let has_events = json.contains("\"events\"")
    let has_links = json.contains("\"links\"")
    
    has_trace_id && has_span_id && has_operation_name && has_attributes && has_events && has_links
  }
  
  // 创建测试数据
  let test_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "http.request",
    start_time: 1634567890000,
    end_time: 1634567890500,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200")
    ],
    events: [
      (1634567890100, "http.request.headers", [("user-agent", "Mozilla/5.0")]),
      (1634567890200, "http.response.headers", [("content-type", "application/json")])
    ],
    links: [
      ("trace-54321", "span-98765", [("service", "auth-service")])
    ]
  }
  
  // 测试序列化
  let json = serialize_to_json(test_data)
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"operation_name\":\"http.request\""))
  assert_true(json.contains("\"http.method\":\"GET\""))
  
  // 测试反序列化
  let is_valid_json = deserialize_from_json(json)
  assert_true(is_valid_json)
  
  // 测试空属性和事件
  let minimal_data = {
    trace_id: "trace-minimal",
    span_id: "span-minimal",
    parent_span_id: None,
    operation_name: "minimal.op",
    start_time: 1000,
    end_time: 2000,
    status: "ok",
    attributes: [],
    events: [],
    links: []
  }
  
  let minimal_json = serialize_to_json(minimal_data)
  assert_true(minimal_json.contains("\"attributes\":{}"))
  assert_true(minimal_json.contains("\"events\":[]"))
  assert_true(minimal_json.contains("\"links\":[]"))
  
  let minimal_is_valid = deserialize_from_json(minimal_json)
  assert_true(minimal_is_valid)
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String,
    is_remote: Bool
  }
  
  // 定义传播器接口
  trait TextMapPropagator {
    inject(Context, Array[(String, String)]) -> Array[(String, String)]
    extract(Array[(String, String)]) -> Context
  }
  
  // 简单的追踪传播器实现
  type SimpleContext = {
    trace_context: Option[TraceContext],
    baggage: Array[(String, String)]
  }
  
  // 创建上下文
  let create_context = fn(trace_id: String, span_id: String) {
    {
      trace_context: Some({
        trace_id: trace_id,
        span_id: span_id,
        trace_flags: 1,
        trace_state: "key1=value1,key2=value2",
        is_remote: false
      }),
      baggage: []
    }
  }
  
  // 注入追踪头
  let inject_headers = fn(context: SimpleContext, carrier: Array[(String, String)]) {
    match context.trace_context {
      Some(trace_ctx) => {
        let traceparent = "00-" + trace_ctx.trace_id + "-" + trace_ctx.span_id + "-" + trace_ctx.trace_flags.to_string(16)
        let tracestate = trace_ctx.trace_state
        
        let new_carrier = carrier.push(("traceparent", traceparent))
        let final_carrier = new_carrier.push(("tracestate", tracestate))
        
        // 注入baggage
        let with_baggage = context.baggage.reduce(final_carrier, fn(acc, item) {
          let (key, value) = item
          acc.push(("baggage-" + key, value))
        })
        
        with_baggage
      }
      None => carrier
    }
  }
  
  // 提取追踪头
  let extract_headers = fn(carrier: Array[(String, String)]) {
    let traceparent = carrier.find(fn(header) { header.0 == "traceparent" })
    let tracestate = carrier.find(fn(header) { header.0 == "tracestate" })
    
    match (traceparent, tracestate) {
      (Some((_, tp_value)), Some((_, ts_value))) => {
        // 解析traceparent: 00-trace_id-span_id-flags
        let parts = tp_value.split("-")
        if parts.length() == 4 {
          let trace_id = parts[1]
          let span_id = parts[2]
          let flags = parts[3]
          
          let trace_context = Some({
            trace_id: trace_id,
            span_id: span_id,
            trace_flags: flags.from_hex_radix(),
            trace_state: ts_value,
            is_remote: true
          })
          
          // 提取baggage
          let baggage = carrier.filter(fn(header) { 
            let (key, _) = header
            key.starts_with("baggage-")
          }).map(fn(header) {
            let (key, value) = header
            (key.substring(8), value)  // 移除"baggage-"前缀
          })
          
          {
            trace_context: trace_context,
            baggage: baggage
          }
        } else {
          {
            trace_context: None,
            baggage: []
          }
        }
      }
      _ => {
        {
          trace_context: None,
          baggage: []
        }
      }
    }
  }
  
  // 测试上下文传播
  let original_context = create_context("trace-12345", "span-67890")
  let context_with_baggage = {
    trace_context: original_context.trace_context,
    baggage: [
      ("user.id", "user123"),
      ("request.id", "req456")
    ]
  }
  
  // 注入到载体
  let carrier = []
  let injected_carrier = inject_headers(context_with_baggage, carrier)
  
  // 验证注入的头
  let traceparent_header = injected_carrier.find(fn(h) { h.0 == "traceparent" })
  let tracestate_header = injected_carrier.find(fn(h) { h.0 == "tracestate" })
  let baggage_user_id = injected_carrier.find(fn(h) { h.0 == "baggage-user.id" })
  let baggage_request_id = injected_carrier.find(fn(h) { h.0 == "baggage-request.id" })
  
  assert_true(traceparent_header.is_some())
  assert_true(tracestate_header.is_some())
  assert_true(baggage_user_id.is_some())
  assert_true(baggage_request_id.is_some())
  
  match traceparent_header {
    Some((_, value)) => {
      assert_true(value.contains("trace-12345"))
      assert_true(value.contains("span-67890"))
    }
    None => assert_true(false)
  }
  
  match baggage_user_id {
    Some((_, value)) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  // 提取上下文
  let extracted_context = extract_headers(injected_carrier)
  
  // 验证提取的上下文
  assert_true(extracted_context.trace_context.is_some())
  assert_eq(extracted_context.baggage.length(), 2)
  
  match extracted_context.trace_context {
    Some(trace_ctx) => {
      assert_eq(trace_ctx.trace_id, "trace-12345")
      assert_eq(trace_ctx.span_id, "span-67890")
      assert_true(trace_ctx.is_remote)
    }
    None => assert_true(false)
  }
  
  // 验证提取的baggage
  let extracted_user_id = extracted_context.baggage.find(fn(item) { item.0 == "user.id" })
  let extracted_request_id = extracted_context.baggage.find(fn(item) { item.0 == "request.id" })
  
  match extracted_user_id {
    Some((_, value)) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  match extracted_request_id {
    Some((_, value)) => assert_eq(value, "req456")
    None => assert_true(false)
  }
}

// 测试3: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  // 性能测试辅助函数
  let measure_time = fn(operation: () -> Unit) {
    let start_time = Time::now()
    operation()
    let end_time = Time::now()
    end_time - start_time
  }
  
  // 批量生成遥测数据
  let generate_telemetry_batch = fn(size: Int) {
    let batch = []
    for i in 0..size {
      let item = {
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        operation_name: "operation-" + (i % 10).to_string(),
        start_time: 1000000 + i * 100,
        end_time: 1000100 + i * 100,
        attributes: [
          ("index", i.to_string()),
          ("batch", "true")
        ]
      }
      batch = batch.push(item)
    }
    batch
  }
  
  // 批量处理遥测数据
  let process_telemetry_batch = fn(batch: Array[Array[String]]) {
    let processed = []
    for item in batch {
      // 模拟处理：计算持续时间
      let start_time = item[3].to_int()
      let end_time = item[4].to_int()
      let duration = end_time - start_time
      
      let processed_item = item.push(duration.to_string())
      processed = processed.push(processed_item)
    }
    processed
  }
  
  // 测试小批量处理性能
  let small_batch = generate_telemetry_batch(100)
  let small_batch_time = measure_time(fn() {
    let _ = process_telemetry_batch(small_batch)
  })
  
  // 测试中等批量处理性能
  let medium_batch = generate_telemetry_batch(1000)
  let medium_batch_time = measure_time(fn() {
    let _ = process_telemetry_batch(medium_batch)
  })
  
  // 测试大批量处理性能
  let large_batch = generate_telemetry_batch(10000)
  let large_batch_time = measure_time(fn() {
    let _ = process_telemetry_batch(large_batch)
  })
  
  // 性能断言（这些值需要根据实际环境调整）
  assert_true(small_batch_time < 100)  // 小批量应在100ms内完成
  assert_true(medium_batch_time < 1000)  // 中等批量应在1秒内完成
  assert_true(large_batch_time < 10000)  // 大批量应在10秒内完成
  
  // 测试批量大小与处理时间的线性关系
  let time_per_item_small = small_batch_time / small_batch.length()
  let time_per_item_medium = medium_batch_time / medium_batch.length()
  let time_per_item_large = large_batch_time / large_batch.length()
  
  // 大批量处理应该更高效（每项处理时间更少）
  assert_true(time_per_item_large <= time_per_item_medium)
  assert_true(time_per_item_medium <= time_per_item_small)
  
  // 内存使用测试
  let measure_memory_usage = fn(operation: () -> Unit) {
    let initial_memory = get_memory_usage()  // 假设的函数
    operation()
    let final_memory = get_memory_usage()
    final_memory - initial_memory
  }
  
  // 测试内存泄漏
  let memory_growth = measure_memory_usage(fn() {
    for i in 0..100 {
      let batch = generate_telemetry_batch(100)
      let _ = process_telemetry_batch(batch)
      // 在实际实现中，这里应该释放batch的内存
    }
  })
  
  // 内存增长应该是合理的（这里假设小于10MB）
  assert_true(memory_growth < 10 * 1024 * 1024)
  
  // 并发处理性能测试
  let concurrent_process = fn(batches: Array[Array[Array[String]]]) {
    let results = []
    for batch in batches {
      let result = process_telemetry_batch(batch)
      results = results.push(result)
    }
    results
  }
  
  let concurrent_batches = [
    generate_telemetry_batch(1000),
    generate_telemetry_batch(1000),
    generate_telemetry_batch(1000),
    generate_telemetry_batch(1000)
  ]
  
  let sequential_time = measure_time(fn() {
    for batch in concurrent_batches {
      let _ = process_telemetry_batch(batch)
    }
  })
  
  let concurrent_time = measure_time(fn() {
    let _ = concurrent_process(concurrent_batches)
  })
  
  // 并发处理应该更快（理想情况下）
  // 注意：在单线程环境中，这个测试可能不会显示并发优势
  assert_true(concurrent_time <= sequential_time * 1.1)  // 允许10%的误差
}

// 测试4: 遥测数据安全性和隐私保护
test "遥测数据安全性和隐私保护" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    Password
    CreditCard
    SSN
    Email
    PhoneNumber
    Custom(String)
  }
  
  // 定义PII检测器
  let detect_pii = fn(value: String) -> Option[SensitiveDataType] {
    // 简单的PII检测模式
    if value.contains("@") && value.contains(".") {
      Some(SensitiveDataType::Email)
    } else if value.length() == 16 && value.chars().all(fn(c) { c.is_digit() }) {
      Some(SensitiveDataType::CreditCard)
    } else if value.length() == 9 && value.chars().all(fn(c) { c.is_digit() }) {
      Some(SensitiveDataType::SSN)
    } else if value.starts_with("password") || value.contains("pwd") {
      Some(SensitiveDataType::Password)
    } else if value.matches("\\+?\\d{10,15}") {
      Some(SensitiveDataType::PhoneNumber)
    } else {
      None
    }
  }
  
  // 定义数据脱敏函数
  let mask_sensitive_data = fn(value: String, data_type: SensitiveDataType) {
    match data_type {
      SensitiveDataType::Email => {
        let parts = value.split("@")
        if parts.length() == 2 {
          let username = parts[0]
          let domain = parts[1]
          let masked_username = if username.length() > 2 {
            username.substring(0, 2) + "*".repeat(username.length() - 2)
          } else {
            "*".repeat(username.length())
          }
          masked_username + "@" + domain
        } else {
          "*".repeat(value.length())
        }
      }
      SensitiveDataType::CreditCard => {
        "****-****-****-" + value.substring(value.length() - 4)
      }
      SensitiveDataType::SSN => {
        "***-**-" + value.substring(value.length() - 4)
      }
      SensitiveDataType::Password => {
        "*".repeat(value.length())
      }
      SensitiveDataType::PhoneNumber => {
        if value.length() > 4 {
          "*".repeat(value.length() - 4) + value.substring(value.length() - 4)
        } else {
          "*".repeat(value.length())
        }
      }
      SensitiveDataType::Custom(_) => {
        "*".repeat(value.length())
      }
    }
  }
  
  // 属性过滤器
  let filter_sensitive_attributes = fn(attributes: Array[(String, String)]) {
    attributes.map(fn(attr) {
      let (key, value) = attr
      match detect_pii(value) {
        Some(data_type) => (key, mask_sensitive_data(value, data_type))
        None => attr
      }
    })
  }
  
  // 测试PII检测
  assert_eq(detect_pii("user@example.com"), Some(SensitiveDataType::Email))
  assert_eq(detect_pii("1234567890123456"), Some(SensitiveDataType::CreditCard))
  assert_eq(detect_pii("123456789"), Some(SensitiveDataType::SSN))
  assert_eq(detect_pii("password123"), Some(SensitiveDataType::Password))
  assert_eq(detect_pii("+1234567890"), Some(SensitiveDataType::PhoneNumber))
  assert_eq(detect_pii("normal_value"), None)
  
  // 测试数据脱敏
  assert_eq(mask_sensitive_data("user@example.com", SensitiveDataType::Email), "us********@example.com")
  assert_eq(mask_sensitive_data("1234567890123456", SensitiveDataType::CreditCard), "****-****-****-3456")
  assert_eq(mask_sensitive_data("123456789", SensitiveDataType::SSN), "***-**-6789")
  assert_eq(mask_sensitive_data("password123", SensitiveDataType::Password), "***********")
  assert_eq(mask_sensitive_data("+1234567890", SensitiveDataType::PhoneNumber), "******7890")
  
  // 测试属性过滤
  let attributes = [
    ("username", "john.doe"),
    ("email", "john.doe@example.com"),
    ("credit_card", "1234567890123456"),
    ("ssn", "123456789"),
    ("password", "secret123"),
    ("phone", "+1234567890"),
    ("user_id", "user123"),
    ("session_id", "session456")
  ]
  
  let filtered_attributes = filter_sensitive_attributes(attributes)
  
  // 验证非敏感数据未被修改
  let username_attr = filtered_attributes.find(fn(attr) { attr.0 == "username" })
  let user_id_attr = filtered_attributes.find(fn(attr) { attr.0 == "user_id" })
  let session_id_attr = filtered_attributes.find(fn(attr) { attr.0 == "session_id" })
  
  match username_attr {
    Some((_, value)) => assert_eq(value, "john.doe")
    None => assert_true(false)
  }
  
  match user_id_attr {
    Some((_, value)) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  match session_id_attr {
    Some((_, value)) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  // 验证敏感数据已被脱敏
  let email_attr = filtered_attributes.find(fn(attr) { attr.0 == "email" })
  let credit_card_attr = filtered_attributes.find(fn(attr) { attr.0 == "credit_card" })
  let ssn_attr = filtered_attributes.find(fn(attr) { attr.0 == "ssn" })
  let password_attr = filtered_attributes.find(fn(attr) { attr.0 == "password" })
  let phone_attr = filtered_attributes.find(fn(attr) { attr.0 == "phone" })
  
  match email_attr {
    Some((_, value)) => assert_eq(value, "jo********@example.com")
    None => assert_true(false)
  }
  
  match credit_card_attr {
    Some((_, value)) => assert_eq(value, "****-****-****-3456")
    None => assert_true(false)
  }
  
  match ssn_attr {
    Some((_, value)) => assert_eq(value, "***-**-6789")
    None => assert_true(false)
  }
  
  match password_attr {
    Some((_, value)) => assert_eq(value, "*********")
    None => assert_true(false)
  }
  
  match phone_attr {
    Some((_, value)) => assert_eq(value, "******7890")
    None => assert_true(false)
  }
  
  // 测试自定义敏感数据类型
  let custom_attributes = [
    ("api_key", "sk-1234567890abcdef"),
    ("secret_token", "token_secret_value")
  ]
  
  let custom_filtered = custom_attributes.map(fn(attr) {
    let (key, value) = attr
    if key.contains("key") || key.contains("secret") || key.contains("token") {
      (key, mask_sensitive_data(value, SensitiveDataType::Custom("secret")))
    } else {
      attr
    }
  })
  
  let api_key_attr = custom_filtered.find(fn(attr) { attr.0 == "api_key" })
  let secret_token_attr = custom_filtered.find(fn(attr) { attr.0 == "secret_token" })
  
  match api_key_attr {
    Some((_, value)) => assert_eq(value, "******************")
    None => assert_true(false)
  }
  
  match secret_token_attr {
    Some((_, value)) => assert_eq(value, "*******************")
    None => assert_true(false)
  }
}

// 测试5: 遥测资源管理和内存优化
test "遥测资源管理和内存优化" {
  // 定义资源池
  type ResourcePool[T] = {
    available: Array[T],
    in_use: Array[T],
    factory: () -> T,
    reset_fn: (T) -> Unit,
    max_size: Int
  }
  
  // 创建资源池
  let create_resource_pool = fn(factory: () -> T, reset_fn: (T) -> Unit, max_size: Int) {
    {
      available: [],
      in_use: [],
      factory: factory,
      reset_fn: reset_fn,
      max_size: max_size
    }
  }
  
  // 从池中获取资源
  let acquire_resource = fn(pool: ResourcePool[T]) {
    if pool.available.length() > 0 {
      let resource = pool.available[0]
      let updated_available = pool.available.slice(1)
      let updated_in_use = pool.in_use.push(resource)
      
      ({
        available: updated_available,
        in_use: updated_in_use,
        factory: pool.factory,
        reset_fn: pool.reset_fn,
        max_size: pool.max_size
      }, resource)
    } else if pool.in_use.length() < pool.max_size {
      let resource = pool.factory()
      let updated_in_use = pool.in_use.push(resource)
      
      ({
        available: pool.available,
        in_use: updated_in_use,
        factory: pool.factory,
        reset_fn: pool.reset_fn,
        max_size: pool.max_size
      }, resource)
    } else {
      // 池已满，返回错误或等待
      panic("资源池已满")
    }
  }
  
  // 将资源返回池中
  let release_resource = fn(pool: ResourcePool[T], resource: T) {
    pool.reset_fn(resource)
    let updated_in_use = pool.in_use.filter(fn(r) { r != resource })
    let updated_available = pool.available.push(resource)
    
    {
      available: updated_available,
      in_use: updated_in_use,
      factory: pool.factory,
      reset_fn: pool.reset_fn,
      max_size: pool.max_size
    }
  }
  
  // 定义缓冲区资源
  type Buffer = {
    data: Array[String],
    capacity: Int,
    size: Int
  }
  
  // 创建缓冲区工厂
  let buffer_factory = fn() {
    {
      data: Array::with_capacity(1000),
      capacity: 1000,
      size: 0
    }
  }
  
  // 重置缓冲区
  let reset_buffer = fn(buffer: Buffer) {
    buffer.size = 0
    // 在实际实现中，这里可能需要清空数据
  }
  
  // 创建缓冲区池
  let buffer_pool = create_resource_pool(buffer_factory, reset_buffer, 10)
  
  // 测试资源池
  let (pool1, buffer1) = acquire_resource(buffer_pool)
  assert_eq(pool1.available.length(), 0)
  assert_eq(pool1.in_use.length(), 1)
  
  let (pool2, buffer2) = acquire_resource(pool1)
  assert_eq(pool2.available.length(), 0)
  assert_eq(pool2.in_use.length(), 2)
  
  // 释放资源
  let pool3 = release_resource(pool2, buffer1)
  assert_eq(pool3.available.length(), 1)
  assert_eq(pool3.in_use.length(), 1)
  
  let pool4 = release_resource(pool3, buffer2)
  assert_eq(pool4.available.length(), 2)
  assert_eq(pool4.in_use.length(), 0)
  
  // 再次获取资源，应该重用已释放的资源
  let (pool5, buffer3) = acquire_resource(pool4)
  assert_eq(pool5.available.length(), 1)
  assert_eq(pool5.in_use.length(), 1)
  
  // 测试内存使用优化
  let measure_pool_memory_usage = fn(pool: ResourcePool[Buffer], operations: Int) {
    let initial_memory = get_memory_usage()
    
    for i in 0..operations {
      let (current_pool, buffer) = acquire_resource(pool)
      // 使用缓冲区
      buffer.size = 100
      let _ = release_resource(current_pool, buffer)
    }
    
    let final_memory = get_memory_usage()
    final_memory - initial_memory
  }
  
  // 使用资源池的内存增长应该较小
  let pooled_memory_growth = measure_pool_memory_usage(buffer_pool, 1000)
  
  // 不使用资源池的内存增长
  let non_pooled_memory_growth = measure_memory_usage(fn() {
    for i in 0..1000 {
      let buffer = buffer_factory()
      buffer.size = 100
      // 缓冲区在这里被丢弃，可能导致内存碎片
    }
  })
  
  // 资源池应该减少内存增长
  assert_true(pooled_memory_growth < non_pooled_memory_growth)
  
  // 测试批量数据处理优化
  let process_batch_optimized = fn(data: Array[String], batch_size: Int) {
    let batches = []
    let mut i = 0
    
    while i < data.length() {
      let end = if i + batch_size < data.length() { i + batch_size } else { data.length() }
      let batch = data.slice(i, end)
      batches = batches.push(batch)
      i = i + batch_size
    }
    
    let results = []
    for batch in batches {
      let (current_pool, buffer) = acquire_resource(buffer_pool)
      
      // 处理批次数据
      for item in batch {
        buffer.data.push(item)
        buffer.size = buffer.size + 1
      }
      
      // 模拟处理结果
      let result = "Processed " + buffer.size.to_string() + " items"
      results = results.push(result)
      
      let _ = release_resource(current_pool, buffer)
    }
    
    results
  }
  
  // 创建测试数据
  let test_data = []
  for i in 0..10000 {
    test_data = test_data.push("item-" + i.to_string())
  }
  
  // 测试批量处理
  let batch_results = process_batch_optimized(test_data, 1000)
  assert_eq(batch_results.length(), 10)
  
  for result in batch_results {
    assert_true(result.contains("Processed"))
    assert_true(result.contains("items"))
  }
  
  // 测试资源池限制
  let test_pool_limit = fn(pool: ResourcePool[Buffer]) {
    let buffers = []
    let current_pool = pool
    
    // 尝试获取超过池大小的资源
    try {
      for i in 0..15 {  // 超过最大大小10
        let (new_pool, buffer) = acquire_resource(current_pool)
        buffers = buffers.push(buffer)
        current_pool = new_pool
      }
      assert_true(false)  // 不应该到达这里
    } catch {
      _ => assert_true(true)  // 应该抛出异常
    }
  }
  
  test_pool_limit(buffer_pool)
}

// 测试6: 遥测数据持久化和存储
test "遥测数据持久化和存储" {
  // 定义存储接口
  trait StorageEngine {
    store(String, String) -> Result[Unit, String]
    retrieve(String) -> Result[String, String]
    delete(String) -> Result[Unit, String]
    list_keys(String) -> Result<Array[String], String]
  }
  
  // 简单的内存存储实现
  type MemoryStorage = {
    data: Map[String, String]
  }
  
  let create_memory_storage = fn() {
    { data: Map::empty() }
  }
  
  let memory_store = fn(storage: MemoryStorage, key: String, value: String) {
    let updated_data = Map::insert(storage.data, key, value)
    Ok({ data: updated_data })
  }
  
  let memory_retrieve = fn(storage: MemoryStorage, key: String) {
    match Map::get(storage.data, key) {
      Some(value) => Ok(value)
      None => Err("Key not found: " + key)
    }
  }
  
  let memory_delete = fn(storage: MemoryStorage, key: String) {
    match Map::get(storage.data, key) {
      Some(_) => {
        let updated_data = Map::remove(storage.data, key)
        Ok({ data: updated_data })
      }
      None => Err("Key not found: " + key)
    }
  }
  
  let memory_list_keys = fn(storage: MemoryStorage, prefix: String) {
    let keys = Map::keys(storage.data).filter(fn(key) { key.starts_with(prefix) })
    Ok(keys)
  }
  
  // 创建存储实例
  let storage = create_memory_storage()
  
  // 测试基本存储操作
  let store_result = memory_store(storage, "trace:12345", "{\"trace_id\":\"12345\",\"operation\":\"test\"}")
  assert_true(store_result.is_ok())
  
  let retrieve_result = memory_retrieve(storage, "trace:12345")
  assert_true(retrieve_result.is_ok())
  
  match retrieve_result {
    Ok(value) => {
      assert_true(value.contains("12345"))
      assert_true(value.contains("test"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试不存在的键
  let missing_result = memory_retrieve(storage, "trace:99999")
  assert_true(missing_result.is_err())
  
  // 测试列出键
  let store_result2 = memory_store(storage, "trace:67890", "{\"trace_id\":\"67890\",\"operation\":\"test2\"}")
  let store_result3 = memory_store(storage, "metric:12345", "{\"metric_name\":\"cpu\",\"value\":0.5}")
  
  let list_result = memory_list_keys(storage, "trace:")
  assert_true(list_result.is_ok())
  
  match list_result {
    Ok(keys) => {
      assert_eq(keys.length(), 2)
      assert_true(keys.contains("trace:12345"))
      assert_true(keys.contains("trace:67890"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试删除操作
  let delete_result = memory_delete(storage, "trace:12345")
  assert_true(delete_result.is_ok())
  
  let after_delete_retrieve = memory_retrieve(storage, "trace:12345")
  assert_true(after_delete_retrieve.is_err())
  
  // 定义遥测数据存储管理器
  type TelemetryStorageManager = {
    storage: MemoryStorage,
    key_prefix: String,
    ttl_seconds: Int
  }
  
  let create_telemetry_storage_manager = fn(storage: MemoryStorage, key_prefix: String, ttl_seconds: Int) {
    {
      storage: storage,
      key_prefix: key_prefix,
      ttl_seconds: ttl_seconds
    }
  }
  
  // 存储遥测数据
  let store_telemetry = fn(manager: TelemetryStorageManager, trace_id: String, data: String) {
    let key = manager.key_prefix + trace_id
    let value_with_timestamp = Time::now().to_string() + "|" + data
    memory_store(manager.storage, key, value_with_timestamp)
  }
  
  // 检索遥测数据
  let retrieve_telemetry = fn(manager: TelemetryStorageManager, trace_id: String) {
    let key = manager.key_prefix + trace_id
    match memory_retrieve(manager.storage, key) {
      Ok(value_with_timestamp) => {
        let parts = value_with_timestamp.split("|")
        if parts.length() >= 2 {
          let timestamp = parts[0].to_int()
          let data = parts.slice(1).join("|")
          
          // 检查TTL
          let current_time = Time::now()
          if current_time - timestamp < manager.ttl_seconds * 1000 {
            Ok(data)
          } else {
            Err("Data expired")
          }
        } else {
          Err("Invalid data format")
        }
      }
      Err(msg) => Err(msg)
    }
  }
  
  // 创建遥测存储管理器
  let telemetry_manager = create_telemetry_storage_manager(storage, "telemetry:", 3600)  // 1小时TTL
  
  // 测试遥测数据存储和检索
  let telemetry_data = "{\"trace_id\":\"abc123\",\"spans\":[{\"span_id\":\"def456\",\"operation\":\"http.request\"}]}"
  let store_telemetry_result = store_telemetry(telemetry_manager, "abc123", telemetry_data)
  assert_true(store_telemetry_result.is_ok())
  
  let retrieve_telemetry_result = retrieve_telemetry(telemetry_manager, "abc123")
  assert_true(retrieve_telemetry_result.is_ok())
  
  match retrieve_telemetry_result {
    Ok(data) => {
      assert_true(data.contains("abc123"))
      assert_true(data.contains("def456"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试批量存储
  let store_batch_telemetry = fn(manager: TelemetryStorageManager, batch_data: Array[(String, String)]) {
    let results = []
    for (trace_id, data) in batch_data {
      let result = store_telemetry(manager, trace_id, data)
      results = results.push(result)
    }
    results
  }
  
  let batch_data = [
    ("batch1", "{\"trace_id\":\"batch1\",\"operation\":\"op1\"}"),
    ("batch2", "{\"trace_id\":\"batch2\",\"operation\":\"op2\"}"),
    ("batch3", "{\"trace_id\":\"batch3\",\"operation\":\"op3\"}")
  ]
  
  let batch_results = store_batch_telemetry(telemetry_manager, batch_data)
  assert_eq(batch_results.length(), 3)
  assert_true(batch_results.all(fn(result) { result.is_ok() }))
  
  // 测试批量检索
  let retrieve_batch_telemetry = fn(manager: TelemetryStorageManager, trace_ids: Array[String]) {
    let results = []
    for trace_id in trace_ids {
      let result = retrieve_telemetry(manager, trace_id)
      results = results.push(result)
    }
    results
  }
  
  let batch_trace_ids = ["batch1", "batch2", "batch3", "nonexistent"]
  let batch_retrieve_results = retrieve_batch_telemetry(telemetry_manager, batch_trace_ids)
  
  assert_eq(batch_retrieve_results.length(), 4)
  assert_true(batch_retrieve_results[0].is_ok())  // batch1
  assert_true(batch_retrieve_results[1].is_ok())  // batch2
  assert_true(batch_retrieve_results[2].is_ok())  // batch3
  assert_true(batch_retrieve_results[3].is_err())  // nonexistent
  
  // 测试数据压缩存储
  let compress_data = fn(data: String) {
    // 简化的压缩：移除空格和换行符
    data.replace(" ", "").replace("\n", "").replace("\r", "")
  }
  
  let decompress_data = fn(compressed_data: String) {
    // 简化的解压缩：这里只是返回原始数据
    // 在实际实现中，这里会有真正的解压缩逻辑
    compressed_data
  }
  
  let store_compressed_telemetry = fn(manager: TelemetryStorageManager, trace_id: String, data: String) {
    let compressed_data = compress_data(data)
    let key = manager.key_prefix + "compressed:" + trace_id
    let value_with_timestamp = Time::now().to_string() + "|compressed|" + compressed_data
    memory_store(manager.storage, key, value_with_timestamp)
  }
  
  let retrieve_compressed_telemetry = fn(manager: TelemetryStorageManager, trace_id: String) {
    let key = manager.key_prefix + "compressed:" + trace_id
    match memory_retrieve(manager.storage, key) {
      Ok(value_with_timestamp) => {
        let parts = value_with_timestamp.split("|")
        if parts.length() >= 3 && parts[1] == "compressed" {
          let timestamp = parts[0].to_int()
          let compressed_data = parts.slice(2).join("|")
          
          // 检查TTL
          let current_time = Time::now()
          if current_time - timestamp < manager.ttl_seconds * 1000 {
            let data = decompress_data(compressed_data)
            Ok(data)
          } else {
            Err("Data expired")
          }
        } else {
          Err("Invalid compressed data format")
        }
      }
      Err(msg) => Err(msg)
    }
  }
  
  // 测试压缩存储
  let large_telemetry_data = "{\n  \"trace_id\": \"large123\",\n  \"spans\": [\n    {\n      \"span_id\": \"span1\",\n      \"operation\": \"large.operation\",\n      \"attributes\": {\n        \"key1\": \"value1\",\n        \"key2\": \"value2\"\n      }\n    }\n  ]\n}"
  
  let compressed_store_result = store_compressed_telemetry(telemetry_manager, "large123", large_telemetry_data)
  assert_true(compressed_store_result.is_ok())
  
  let compressed_retrieve_result = retrieve_compressed_telemetry(telemetry_manager, "large123")
  assert_true(compressed_retrieve_result.is_ok())
  
  match compressed_retrieve_result {
    Ok(data) => {
      assert_true(data.contains("large123"))
      assert_true(data.contains("span1"))
    }
    Err(_) => assert_true(false)
  }
}

// 测试7: 遥测仪表板数据聚合
test "遥测仪表板数据聚合" {
  // 定义仪表板数据点
  type DashboardDataPoint = {
    timestamp: Int,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 定义聚合指标
  type AggregatedMetrics = {
    service_name: String,
    operation_name: String,
    total_requests: Int,
    successful_requests: Int,
    failed_requests: Int,
    avg_duration: Float,
    min_duration: Int,
    max_duration: Int,
    p50_duration: Int,
    p95_duration: Int,
    p99_duration: Int
  }
  
  // 创建测试数据
  let create_test_dashboard_data = fn() {
    [
      { timestamp: 1000, service_name: "api-service", operation_name: "GET /users", duration_ms: 120, status: "success", attributes: [("user_id", "123")] },
      { timestamp: 2000, service_name: "api-service", operation_name: "GET /users", duration_ms: 150, status: "success", attributes: [("user_id", "456")] },
      { timestamp: 3000, service_name: "api-service", operation_name: "GET /users", duration_ms: 200, status: "error", attributes: [("user_id", "789")] },
      { timestamp: 4000, service_name: "api-service", operation_name: "POST /users", duration_ms: 300, status: "success", attributes: [("user_id", "123")] },
      { timestamp: 5000, service_name: "api-service", operation_name: "POST /users", duration_ms: 250, status: "success", attributes: [("user_id", "456")] },
      { timestamp: 6000, service_name: "payment-service", operation_name: "process_payment", duration_ms: 500, status: "success", attributes: [("payment_id", "pay123")] },
      { timestamp: 7000, service_name: "payment-service", operation_name: "process_payment", duration_ms: 800, status: "error", attributes: [("payment_id", "pay456")] },
      { timestamp: 8000, service_name: "payment-service", operation_name: "process_payment", duration_ms: 600, status: "success", attributes: [("payment_id", "pay789")] },
      { timestamp: 9000, service_name: "notification-service", operation_name: "send_email", duration_ms: 100, status: "success", attributes: [("email", "user@example.com")] },
      { timestamp: 10000, service_name: "notification-service", operation_name: "send_email", duration_ms: 150, status: "success", attributes: [("email", "admin@example.com")] }
    ]
  }
  
  // 按服务和操作分组数据
  let group_by_service_and_operation = fn(data: Array[DashboardDataPoint]) {
    let groups = Map::empty()
    
    for point in data {
      let key = point.service_name + "|" + point.operation_name
      
      let group = match Map::get(groups, key) {
        Some(points) => points
        None => []
      }
      
      let updated_group = group.push(point)
      let _ = Map::insert(groups, key, updated_group)
    }
    
    groups
  }
  
  // 计算百分位数
  let calculate_percentile = fn(sorted_values: Array[Int], percentile: Float) {
    if sorted_values.length() == 0 {
      0
    } else {
      let index = ((sorted_values.length() as Float) * percentile / 100.0) as Int
      if index >= sorted_values.length() {
        sorted_values[sorted_values.length() - 1]
      } else {
        sorted_values[index]
      }
    }
  }
  
  // 聚合指标
  let aggregate_metrics = fn(data: Array[DashboardDataPoint]) {
    if data.length() == 0 {
      return {
        service_name: "",
        operation_name: "",
        total_requests: 0,
        successful_requests: 0,
        failed_requests: 0,
        avg_duration: 0.0,
        min_duration: 0,
        max_duration: 0,
        p50_duration: 0,
        p95_duration: 0,
        p99_duration: 0
      }
    }
    
    let service_name = data[0].service_name
    let operation_name = data[0].operation_name
    
    let total_requests = data.length()
    let successful_requests = data.filter(fn(point) { point.status == "success" }).length()
    let failed_requests = total_requests - successful_requests
    
    let durations = data.map(fn(point) { point.duration_ms })
    let sorted_durations = durations.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    
    let min_duration = sorted_durations[0]
    let max_duration = sorted_durations[sorted_durations.length() - 1]
    
    let total_duration = durations.reduce(fn(acc, duration) { acc + duration }, 0)
    let avg_duration = (total_duration as Float) / (total_requests as Float)
    
    let p50_duration = calculate_percentile(sorted_durations, 50.0)
    let p95_duration = calculate_percentile(sorted_durations, 95.0)
    let p99_duration = calculate_percentile(sorted_durations, 99.0)
    
    {
      service_name: service_name,
      operation_name: operation_name,
      total_requests: total_requests,
      successful_requests: successful_requests,
      failed_requests: failed_requests,
      avg_duration: avg_duration,
      min_duration: min_duration,
      max_duration: max_duration,
      p50_duration: p50_duration,
      p95_duration: p95_duration,
      p99_duration: p99_duration
    }
  }
  
  // 聚合所有数据
  let aggregate_all_data = fn(data: Array[DashboardDataPoint]) {
    let groups = group_by_service_and_operation(data)
    let results = []
    
    for (key, group_data) in groups {
      let metrics = aggregate_metrics(group_data)
      results = results.push(metrics)
    }
    
    results
  }
  
  // 测试数据聚合
  let test_data = create_test_dashboard_data()
  let aggregated_results = aggregate_all_data(test_data)
  
  assert_eq(aggregated_results.length(), 4)  // 4个不同的操作
  
  // 验证API服务GET /users聚合结果
  let api_get_users = aggregated_results.find(fn(metrics) {
    metrics.service_name == "api-service" && metrics.operation_name == "GET /users"
  })
  
  match api_get_users {
    Some(metrics) => {
      assert_eq(metrics.total_requests, 3)
      assert_eq(metrics.successful_requests, 2)
      assert_eq(metrics.failed_requests, 1)
      assert_eq(metrics.min_duration, 120)
      assert_eq(metrics.max_duration, 200)
      assert_eq(metrics.avg_duration.round(), 156.67)
    }
    None => assert_true(false)
  }
  
  // 验证支付服务process_payment聚合结果
  let payment_process = aggregated_results.find(fn(metrics) {
    metrics.service_name == "payment-service" && metrics.operation_name == "process_payment"
  })
  
  match payment_process {
    Some(metrics) => {
      assert_eq(metrics.total_requests, 3)
      assert_eq(metrics.successful_requests, 2)
      assert_eq(metrics.failed_requests, 1)
      assert_eq(metrics.min_duration, 500)
      assert_eq(metrics.max_duration, 800)
    }
    None => assert_true(false)
  }
  
  // 按时间窗口聚合数据
  let aggregate_by_time_window = fn(data: Array[DashboardDataPoint], window_size_ms: Int) {
    if data.length() == 0 {
      return []
    }
    
    let sorted_data = data.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    
    let windows = []
    let start_time = sorted_data[0].timestamp
    let end_time = sorted_data[sorted_data.length() - 1].timestamp
    
    let mut window_start = start_time
    while window_start <= end_time {
      let window_end = window_start + window_size_ms
      let window_data = sorted_data.filter(fn(point) {
        point.timestamp >= window_start && point.timestamp < window_end
      })
      
      if window_data.length() > 0 {
        let window_metrics = aggregate_metrics(window_data)
        let timed_metrics = { window_metrics | window_start: window_start, window_end: window_end }
        windows = windows.push(timed_metrics)
      }
      
      window_start = window_end
    }
    
    windows
  }
  
  // 测试时间窗口聚合（5秒窗口）
  let time_windows = aggregate_by_time_window(test_data, 5000)
  
  // 应该有2个窗口：0-5秒和5-10秒
  assert_eq(time_windows.length(), 2)
  
  // 验证第一个窗口（0-5秒）
  let first_window = time_windows[0]
  assert_eq(first_window.window_start, 1000)
  assert_eq(first_window.window_end, 6000)
  assert_eq(first_window.total_requests, 5)  // 前5个数据点
  
  // 验证第二个窗口（5-10秒）
  let second_window = time_windows[1]
  assert_eq(second_window.window_start, 6000)
  assert_eq(second_window.window_end, 11000)
  assert_eq(second_window.total_requests, 5)  // 后5个数据点
  
  // 计算服务级别指标
  let calculate_service_metrics = fn(data: Array[DashboardDataPoint], service_name: String) {
    let service_data = data.filter(fn(point) { point.service_name == service_name })
    
    if service_data.length() == 0 {
      return {
        service_name: service_name,
        total_requests: 0,
        successful_requests: 0,
        failed_requests: 0,
        avg_duration: 0.0,
        error_rate: 0.0
      }
    }
    
    let total_requests = service_data.length()
    let successful_requests = service_data.filter(fn(point) { point.status == "success" }).length()
    let failed_requests = total_requests - successful_requests
    
    let durations = service_data.map(fn(point) { point.duration_ms })
    let total_duration = durations.reduce(fn(acc, duration) { acc + duration }, 0)
    let avg_duration = (total_duration as Float) / (total_requests as Float)
    
    let error_rate = (failed_requests as Float) / (total_requests as Float) * 100.0
    
    {
      service_name: service_name,
      total_requests: total_requests,
      successful_requests: successful_requests,
      failed_requests: failed_requests,
      avg_duration: avg_duration,
      error_rate: error_rate
    }
  }
  
  // 测试服务级别指标
  let api_metrics = calculate_service_metrics(test_data, "api-service")
  assert_eq(api_metrics.service_name, "api-service")
  assert_eq(api_metrics.total_requests, 5)
  assert_eq(api_metrics.successful_requests, 4)
  assert_eq(api_metrics.failed_requests, 1)
  assert_eq(api_metrics.error_rate.round(), 20.0)
  
  let payment_metrics = calculate_service_metrics(test_data, "payment-service")
  assert_eq(payment_metrics.service_name, "payment-service")
  assert_eq(payment_metrics.total_requests, 3)
  assert_eq(payment_metrics.successful_requests, 2)
  assert_eq(payment_metrics.failed_requests, 1)
  assert_eq(payment_metrics.error_rate.round(), 33.33)
  
  let notification_metrics = calculate_service_metrics(test_data, "notification-service")
  assert_eq(notification_metrics.service_name, "notification-service")
  assert_eq(notification_metrics.total_requests, 2)
  assert_eq(notification_metrics.successful_requests, 2)
  assert_eq(notification_metrics.failed_requests, 0)
  assert_eq(notification_metrics.error_rate, 0.0)
}

// 测试8: 遥测数据异常检测和告警
test "遥测数据异常检测和告警" {
  // 定义异常类型
  enum AnomalyType {
    HighErrorRate
    HighLatency
    LowThroughput
    UnusualPattern
    ResourceExhaustion
  }
  
  // 定义告警级别
  enum AlertSeverity {
    Info
    Warning
    Critical
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    anomaly_type: AnomalyType,
    severity: AlertSeverity,
    message: String,
    detected_at: Int,
    affected_services: Array[String],
    metrics: Array[(String, Float)]
  }
  
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建测试时间序列数据
  let create_test_time_series = fn() {
    [
      { timestamp: 1000, value: 100.0, tags: [("service", "api"), ("metric", "latency")] },
      { timestamp: 2000, value: 120.0, tags: [("service", "api"), ("metric", "latency")] },
      { timestamp: 3000, value: 110.0, tags: [("service", "api"), ("metric", "latency")] },
      { timestamp: 4000, value: 500.0, tags: [("service", "api"), ("metric", "latency")] },  // 异常高延迟
      { timestamp: 5000, value: 130.0, tags: [("service", "api"), ("metric", "latency")] },
      { timestamp: 6000, value: 90.0, tags: [("service", "payment"), ("metric", "latency")] },
      { timestamp: 7000, value: 110.0, tags: [("service", "payment"), ("metric", "latency")] },
      { timestamp: 8000, value: 95.0, tags: [("service", "payment"), ("metric", "latency")] },
      { timestamp: 9000, value: 105.0, tags: [("service", "payment"), ("metric", "latency")] },
      { timestamp: 10000, value: 100.0, tags: [("service", "payment"), ("metric", "latency")] }
    ]
  }
  
  // 创建错误率测试数据
  let create_error_rate_time_series = fn() {
    [
      { timestamp: 1000, value: 1.0, tags: [("service", "api"), ("metric", "error_rate")] },
      { timestamp: 2000, value: 2.0, tags: [("service", "api"), ("metric", "error_rate")] },
      { timestamp: 3000, value: 1.5, tags: [("service", "api"), ("metric", "error_rate")] },
      { timestamp: 4000, value: 25.0, tags: [("service", "api"), ("metric", "error_rate")] },  // 异常高错误率
      { timestamp: 5000, value: 3.0, tags: [("service", "api"), ("metric", "error_rate")] },
      { timestamp: 6000, value: 0.5, tags: [("service", "payment"), ("metric", "error_rate")] },
      { timestamp: 7000, value: 1.0, tags: [("service", "payment"), ("metric", "error_rate")] },
      { timestamp: 8000, value: 0.8, tags: [("service", "payment"), ("metric", "error_rate")] },
      { timestamp: 9000, value: 1.2, tags: [("service", "payment"), ("metric", "error_rate")] },
      { timestamp: 10000, value: 0.9, tags: [("service", "payment"), ("metric", "error_rate")] }
    ]
  }
  
  // 计算移动平均
  let calculate_moving_average = fn(data: Array[TimeSeriesPoint], window_size: Int) {
    if data.length() < window_size {
      return []
    }
    
    let moving_averages = []
    for i in (window_size - 1)..(data.length() - 1) {
      let window_start = i - window_size + 1
      let window = data.slice(window_start, i + 1)
      let sum = window.reduce(fn(acc, point) { acc + point.value }, 0.0)
      let average = sum / (window.length() as Float)
      
      let avg_point = {
        timestamp: data[i].timestamp,
        value: average,
        tags: data[i].tags
      }
      moving_averages = moving_averages.push(avg_point)
    }
    
    moving_averages
  }
  
  // 检测高延迟异常
  let detect_high_latency_anomaly = fn(data: Array[TimeSeriesPoint], threshold_multiplier: Float) {
    if data.length() < 5 {
      return []
    }
    
    let anomalies = []
    
    // 按服务分组
    let groups = Map::empty()
    for point in data {
      let service = match point.tags.find(fn(tag) { tag.0 == "service" }) {
        Some((_, service)) => service
        None => "unknown"
      }
      
      let service_data = match Map::get(groups, service) {
        Some(points) => points
        None => []
      }
      
      let updated_service_data = service_data.push(point)
      let _ = Map::insert(groups, service, updated_service_data)
    }
    
    // 对每个服务进行异常检测
    for (service, service_data) in groups {
      if service_data.length() >= 5 {
        // 计算移动平均
        let moving_averages = calculate_moving_average(service_data, 3)
        
        // 计算基线平均值（前几个点）
        let baseline_points = service_data.slice(0, 3)
        let baseline_sum = baseline_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
        let baseline_avg = baseline_sum / (baseline_points.length() as Float)
        let threshold = baseline_avg * threshold_multiplier
        
        // 检查每个点是否超过阈值
        for point in service_data {
          if point.value > threshold {
            let severity = if point.value > baseline_avg * (threshold_multiplier * 1.5) {
              AlertSeverity::Critical
            } else {
              AlertSeverity::Warning
            }
            
            let anomaly = {
              anomaly_type: AnomalyType::HighLatency,
              severity: severity,
              message: "检测到高延迟异常: " + point.value.to_string() + "ms (阈值: " + threshold.to_string() + "ms)",
              detected_at: point.timestamp,
              affected_services: [service],
              metrics: [("latency", point.value), ("threshold", threshold)]
            }
            anomalies = anomalies.push(anomaly)
          }
        }
      }
    }
    
    anomalies
  }
  
  // 检测高错误率异常
  let detect_high_error_rate_anomaly = fn(data: Array[TimeSeriesPoint], threshold: Float) {
    let anomalies = []
    
    for point in data {
      if point.value > threshold {
        let severity = if point.value > threshold * 2.0 {
          AlertSeverity::Critical
        } else {
          AlertSeverity::Warning
        }
        
        let service = match point.tags.find(fn(tag) { tag.0 == "service" }) {
          Some((_, service)) => service
          None => "unknown"
        }
        
        let anomaly = {
          anomaly_type: AnomalyType::HighErrorRate,
          severity: severity,
          message: "检测到高错误率异常: " + point.value.to_string() + "% (阈值: " + threshold.to_string() + "%)",
          detected_at: point.timestamp,
          affected_services: [service],
          metrics: [("error_rate", point.value), ("threshold", threshold)]
        }
        anomalies = anomalies.push(anomaly)
      }
    }
    
    anomalies
  }
  
  // 测试延迟异常检测
  let latency_data = create_test_time_series()
  let latency_anomalies = detect_high_latency_anomaly(latency_data, 2.0)  // 2倍阈值
  
  assert_eq(latency_anomalies.length(), 1)  // 应该检测到1个异常
  
  match latency_anomalies[0] {
    anomaly => {
      assert_eq(anomaly.anomaly_type, AnomalyType::HighLatency)
      assert_eq(anomaly.severity, AlertSeverity::Critical)  // 500ms是基线的5倍，应该是Critical
      assert_eq(anomaly.affected_services, ["api"])
      assert_true(anomaly.message.contains("500.0"))
    }
  }
  
  // 测试错误率异常检测
  let error_rate_data = create_error_rate_time_series()
  let error_rate_anomalies = detect_high_error_rate_anomaly(error_rate_data, 10.0)  // 10%阈值
  
  assert_eq(error_rate_anomalies.length(), 1)  // 应该检测到1个异常
  
  match error_rate_anomalies[0] {
    anomaly => {
      assert_eq(anomaly.anomaly_type, AnomalyType::HighErrorRate)
      assert_eq(anomaly.severity, AlertSeverity::Warning)  // 25%是阈值的2.5倍，但不到2倍，应该是Warning
      assert_eq(anomaly.affected_services, ["api"])
      assert_true(anomaly.message.contains("25.0"))
    }
  }
  
  // 检测低吞吐量异常
  let detect_low_throughput_anomaly = fn(data: Array[TimeSeriesPoint], threshold: Float) {
    let anomalies = []
    
    if data.length() >= 5 {
      // 计算移动平均
      let moving_averages = calculate_moving_average(data, 3)
      
      // 计算基线平均值
      let baseline_points = data.slice(0, 3)
      let baseline_sum = baseline_points.reduce(fn(acc, point) { acc + point.value }, 0.0)
      let baseline_avg = baseline_sum / (baseline_points.length() as Float)
      
      // 检查每个移动平均是否低于阈值
      for avg_point in moving_averages {
        if avg_point.value < baseline_avg * threshold {
          let service = match avg_point.tags.find(fn(tag) { tag.0 == "service" }) {
            Some((_, service)) => service
            None => "unknown"
          }
          
          let anomaly = {
            anomaly_type: AnomalyType::LowThroughput,
            severity: AlertSeverity::Warning,
            message: "检测到低吞吐量异常: " + avg_point.value.to_string() + " (基线: " + baseline_avg.to_string() + ")",
            detected_at: avg_point.timestamp,
            affected_services: [service],
            metrics: [("throughput", avg_point.value), ("baseline", baseline_avg)]
          }
          anomalies = anomalies.push(anomaly)
        }
      }
    }
    
    anomalies
  }
  
  // 创建吞吐量测试数据
  let create_throughput_time_series = fn() {
    [
      { timestamp: 1000, value: 1000.0, tags: [("service", "api"), ("metric", "throughput")] },
      { timestamp: 2000, value: 1200.0, tags: [("service", "api"), ("metric", "throughput")] },
      { timestamp: 3000, value: 1100.0, tags: [("service", "api"), ("metric", "throughput")] },
      { timestamp: 4000, value: 300.0, tags: [("service", "api"), ("metric", "throughput")] },  // 异常低吞吐量
      { timestamp: 5000, value: 900.0, tags: [("service", "api"), ("metric", "throughput")] },
      { timestamp: 6000, value: 800.0, tags: [("service", "payment"), ("metric", "throughput")] },
      { timestamp: 7000, value: 900.0, tags: [("service", "payment"), ("metric", "throughput")] },
      { timestamp: 8000, value: 850.0, tags: [("service", "payment"), ("metric", "throughput")] },
      { timestamp: 9000, value: 950.0, tags: [("service", "payment"), ("metric", "throughput")] },
      { timestamp: 10000, value: 1000.0, tags: [("service", "payment"), ("metric", "throughput")] }
    ]
  }
  
  // 测试低吞吐量异常检测
  let throughput_data = create_throughput_time_series()
  let throughput_anomalies = detect_low_throughput_anomaly(throughput_data, 0.5)  // 50%阈值
  
  assert_eq(throughput_anomalies.length(), 1)  // 应该检测到1个异常
  
  match throughput_anomalies[0] {
    anomaly => {
      assert_eq(anomaly.anomaly_type, AnomalyType::LowThroughput)
      assert_eq(anomaly.severity, AlertSeverity::Warning)
      assert_eq(anomaly.affected_services, ["api"])
      assert_true(anomaly.message.contains("300.0"))
    }
  }
  
  // 综合异常检测
  let comprehensive_anomaly_detection = fn(latency_data: Array[TimeSeriesPoint], error_rate_data: Array[TimeSeriesPoint], throughput_data: Array[TimeSeriesPoint]) {
    let latency_anomalies = detect_high_latency_anomaly(latency_data, 2.0)
    let error_rate_anomalies = detect_high_error_rate_anomaly(error_rate_data, 10.0)
    let throughput_anomalies = detect_low_throughput_anomaly(throughput_data, 0.5)
    
    latency_anomalies + error_rate_anomalies + throughput_anomalies
  }
  
  // 测试综合异常检测
  let all_anomalies = comprehensive_anomaly_detection(latency_data, error_rate_data, throughput_data)
  assert_eq(all_anomalies.length(), 3)  // 应该检测到3个异常
  
  // 验证异常类型
  let anomaly_types = all_anomalies.map(fn(anomaly) { anomaly.anomaly_type })
  assert_true(anomaly_types.contains(AnomalyType::HighLatency))
  assert_true(anomaly_types.contains(AnomalyType::HighErrorRate))
  assert_true(anomaly_types.contains(AnomalyType::LowThroughput))
  
  // 按严重程度分组异常
  let group_anomalies_by_severity = fn(anomalies: Array[AnomalyDetectionResult]) {
    let critical = anomalies.filter(fn(anomaly) { anomaly.severity == AlertSeverity::Critical })
    let warning = anomalies.filter(fn(anomaly) { anomaly.severity == AlertSeverity::Warning })
    let info = anomalies.filter(fn(anomaly) { anomaly.severity == AlertSeverity::Info })
    
    { critical: critical, warning: warning, info: info }
  }
  
  let grouped_anomalies = group_anomalies_by_severity(all_anomalies)
  assert_eq(grouped_anomalies.critical.length(), 1)  // 1个Critical异常（高延迟）
  assert_eq(grouped_anomalies.warning.length(), 2)   // 2个Warning异常（高错误率和低吞吐量）
  assert_eq(grouped_anomalies.info.length(), 0)      // 0个Info异常
}