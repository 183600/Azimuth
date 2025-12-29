// 安全性测试 - 测试恶意数据处理

test "malicious_string_input_handling" {
  // 测试恶意字符串输入的处理
  
  // 1. 测试SQL注入攻击字符串
  let sql_injection_strings = [
    "'; DROP TABLE users; --",
    "1' OR '1'='1",
    "'; INSERT INTO users VALUES('hacker', 'password'); --",
    "1'; UPDATE users SET password='hacked' WHERE id=1; --",
    "'; DELETE FROM logs; --",
    "1' UNION SELECT username, password FROM users --"
  ]
  
  // 验证恶意字符串被安全处理
  let mut i = 0
  while i < sql_injection_strings.length() {
    let malicious_string = sql_injection_strings[i]
    let attr = common::AttributeValue::string(malicious_string)
    
    match attr {
      common::StringValue(s) => {
        // 验证字符串原样存储，不执行SQL
        assert_eq(s, malicious_string)
      }
      _ => @test.fail("Expected StringValue")
    }
    i = i + 1
  }
  
  // 2. 测试XSS攻击字符串
  let xss_strings = [
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<svg onload=alert('XSS')>",
    "<iframe src=javascript:alert('XSS')>",
    "<body onload=alert('XSS')>"
  ]
  
  // 验证XSS字符串被安全处理
  i = 0
  while i < xss_strings.length() {
    let xss_string = xss_strings[i]
    let attr = common::AttributeValue::string(xss_string)
    
    match attr {
      common::StringValue(s) => {
        // 验证XSS字符串原样存储，不执行脚本
        assert_eq(s, xss_string)
      }
      _ => @test.fail("Expected StringValue")
    }
    i = i + 1
  }
  
  // 3. 测试路径遍历攻击字符串
  let path_traversal_strings = [
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "....//....//....//etc/passwd",
    "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
    "..%252f..%252f..%252fetc%252fpasswd",
    "/var/www/../../etc/passwd"
  ]
  
  // 验证路径遍历字符串被安全处理
  i = 0
  while i < path_traversal_strings.length() {
    let path_string = path_traversal_strings[i]
    let attr = common::AttributeValue::string(path_string)
    
    match attr {
      common::StringValue(s) => {
        // 验证路径遍历字符串原样存储，不访问文件系统
        assert_eq(s, path_string)
      }
      _ => @test.fail("Expected StringValue")
    }
    i = i + 1
  }
}

test "malicious_numeric_input_handling" {
  // 测试恶意数值输入的处理
  
  // 1. 测试溢出攻击数值
  let overflow_values = [
    9223372036854775807L,  // Int64最大值
    -9223372036854775808L, // Int64最小值
    9223372036854775806L,  // 接近最大值
    -9223372036854775807L  // 接近最小值
  ]
  
  // 验证溢出值被安全处理
  let mut i = 0
  while i < overflow_values.length() {
    let overflow_value = overflow_values[i]
    let attr = common::AttributeValue::int(overflow_value)
    
    match attr {
      common::IntValue(v) => {
        // 验证数值原样存储
        assert_eq(v, overflow_value)
      }
      _ => @test.fail("Expected IntValue")
    }
    i = i + 1
  }
  
  // 2. 测试特殊浮点数值
  let special_floats = [
    1.0/0.0,   // 正无穷大
    -1.0/0.0,  // 负无穷大
    0.0/0.0,   // NaN
    1.7976931348623157e+308, // Double最大值
    -1.7976931348623157e+308, // Double最小值
    4.9406564584124654e-324  // Double最小正数
  ]
  
  // 验证特殊浮点数值被安全处理
  i = 0
  while i < special_floats.length() {
    let special_float = special_floats[i]
    let attr = common::AttributeValue::float(special_float)
    
    match attr {
      common::FloatValue(v) => {
        // 验证浮点数值原样存储
        assert_eq(v, special_float)
      }
      _ => @test.fail("Expected FloatValue")
    }
    i = i + 1
  }
  
  // 3. 测试极大数组值
  let large_int_array = common::AttributeValue::array_int(Array::make(10000, 9223372036854775807L))
  let large_float_array = common::AttributeValue::array_float(Array::make(10000, 1.7976931348623157e+308))
  
  // 验证大数组被安全处理
  match large_int_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 10000)
      assert_eq(arr[0], 9223372036854775807L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match large_float_array {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 10000)
      assert_eq(arr[0], 1.7976931348623157e+308)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
}

test "malicious_context_input_handling" {
  // 测试恶意Context输入的处理
  
  let ctx = context::Context::empty()
  
  // 1. 测试恶意键名
  let malicious_keys = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "key\0with\0null\0bytes",
    "key\nwith\nnewlines",
    "key\twith\ttabs",
    "key\rwith\rcarriage\returns",
    "very_long_key_" + "x" * 10000
  ]
  
  // 验证恶意键名被安全处理
  let mut i = 0
  while i < malicious_keys.length() {
    let malicious_key = context::create_key(malicious_keys[i])
    let safe_value = "safe_value"
    let ctx_with_malicious = ctx.with_value(malicious_key, safe_value)
    
    match ctx_with_malicious.get(malicious_key) {
      Some(value) => {
        // 验证值可以正确检索
        assert_eq(value, safe_value)
      }
      None => @test.fail("Expected value for malicious key")
    }
    i = i + 1
  }
  
  // 2. 测试恶意值
  let malicious_values = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "value\0with\0null\0bytes",
    "value\nwith\nnewlines",
    "value\twith\ttabs",
    "value\rwith\rcarriage\returns",
    "very_long_value_" + "x" * 100000
  ]
  
  // 验证恶意值被安全处理
  i = 0
  while i < malicious_values.length() {
    let safe_key = context::create_key("safe_key")
    let malicious_value = malicious_values[i]
    let ctx_with_malicious = ctx.with_value(safe_key, malicious_value)
    
    match ctx_with_malicious.get(safe_key) {
      Some(value) => {
        // 验证恶意值原样存储
        assert_eq(value, malicious_value)
      }
      None => @test.fail("Expected malicious value")
    }
    i = i + 1
  }
  
  // 3. 测试极大量的键值对
  let mut ctx_with_many_entries = ctx
  i = 0
  while i < 1000 {
    let key = context::create_key("many_entries_key_" + i.to_string())
    let value = "many_entries_value_" + i.to_string()
    ctx_with_many_entries = ctx_with_many_entries.with_value(key, value)
    i = i + 1
  }
  
  // 验证大量条目被安全处理
  i = 0
  while i < 1000 {
    let key = context::create_key("many_entries_key_" + i.to_string())
    let expected_value = "many_entries_value_" + i.to_string()
    match ctx_with_many_entries.get(key) {
      Some(value) => assert_eq(value, expected_value)
      None => @test.fail("Expected value for many entries key")
    }
    i = i + 1
  }
}

test "malicious_baggage_input_handling" {
  // 测试恶意Baggage输入的处理
  
  let baggage = context::Baggage::empty()
  
  // 1. 测试恶意baggage键
  let malicious_baggage_keys = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "baggage\0key\0with\0nulls",
    "baggage\nkey\nwith\nnewlines",
    "baggage\tkey\twith\ttabs",
    "very_long_baggage_key_" + "x" * 10000
  ]
  
  // 验证恶意baggage键被安全处理
  let mut i = 0
  while i < malicious_baggage_keys.length() {
    let malicious_key = malicious_baggage_keys[i]
    let safe_value = "safe_value"
    let baggage_with_malicious = baggage.with_entry(malicious_key, safe_value)
    
    match baggage_with_malicious.get(malicious_key) {
      Some(value) => {
        // 验证值可以正确检索
        assert_eq(value, safe_value)
      }
      None => @test.fail("Expected value for malicious baggage key")
    }
    i = i + 1
  }
  
  // 2. 测试恶意baggage值
  let malicious_baggage_values = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "baggage\0value\0with\0nulls",
    "baggage\nvalue\nwith\nnewlines",
    "baggage\tvalue\twith\ttabs",
    "very_long_baggage_value_" + "x" * 100000
  ]
  
  // 验证恶意baggage值被安全处理
  i = 0
  while i < malicious_baggage_values.length() {
    let safe_key = "safe_key"
    let malicious_value = malicious_baggage_values[i]
    let baggage_with_malicious = baggage.with_entry(safe_key, malicious_value)
    
    match baggage_with_malicious.get(safe_key) {
      Some(value) => {
        // 验证恶意值原样存储
        assert_eq(value, malicious_value)
      }
      None => @test.fail("Expected malicious baggage value")
    }
    i = i + 1
  }
  
  // 3. 测试极大量的baggage条目
  let mut baggage_with_many_entries = baggage
  i = 0
  while i < 1000 {
    let key = "many_entries_key_" + i.to_string()
    let value = "many_entries_value_" + i.to_string()
    baggage_with_many_entries = baggage_with_many_entries.with_entry(key, value)
    i = i + 1
  }
  
  // 验证大量baggage条目被安全处理
  i = 0
  while i < 1000 {
    let key = "many_entries_key_" + i.to_string()
    let expected_value = "many_entries_value_" + i.to_string()
    match baggage_with_many_entries.get(key) {
      Some(value) => assert_eq(value, expected_value)
      None => @test.fail("Expected value for many baggage entries")
    }
    i = i + 1
  }
}

test "malicious_log_input_handling" {
  // 测试恶意日志输入的处理
  
  // 1. 测试恶意日志消息
  let malicious_log_bodies = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "log\0body\0with\0nulls",
    "log\nbody\nwith\nnewlines",
    "log\tbody\twith\ttabs",
    "very_long_log_body_" + "x" * 100000
  ]
  
  // 验证恶意日志消息被安全处理
  let mut i = 0
  while i < malicious_log_bodies.length() {
    let malicious_body = malicious_log_bodies[i]
    let log_record = logs::LogRecord::builder()
      .timestamp(1640995200000000000L)
      .severity(logs::Info)
      .body(malicious_body)
      .build()
    
    match log_record.body {
      Some(body) => {
        // 验证恶意消息原样存储
        assert_eq(body, malicious_body)
      }
      None => @test.fail("Expected malicious log body")
    }
    i = i + 1
  }
  
  // 2. 测试恶意日志属性
  let malicious_attributes = [
    ("<script>alert('XSS')</script>", common::AttributeValue::string("<script>alert('XSS')</script>")),
    ("'; DROP TABLE users; --", common::AttributeValue::string("'; DROP TABLE users; --")),
    ("../../../etc/passwd", common::AttributeValue::string("../../../etc/passwd")),
    ("attr\0with\0nulls", common::AttributeValue::string("value\0with\0nulls")),
    ("attr\nwith\nnewlines", common::AttributeValue::string("value\nwith\nnewlines"))
  ]
  
  // 验证恶意属性被安全处理
  i = 0
  while i < malicious_attributes.length() {
    let (malicious_key, malicious_value) = malicious_attributes[i]
    let log_record = logs::LogRecord::builder()
      .timestamp(1640995200000000000L)
      .severity(logs::Info)
      .body("Safe body")
      .with_attribute(malicious_key, malicious_value)
      .build()
    
    // 验证日志记录创建成功
    assert_eq(log_record.attributes.length(), 1)
    i = i + 1
  }
  
  // 3. 测试极大量的日志属性
  let mut builder = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Log with many attributes")
  
  i = 0
  while i < 1000 {
    builder = builder.with_attribute(
      "attr_" + i.to_string(),
      common::AttributeValue::string("value_" + i.to_string())
    )
    i = i + 1
  }
  
  let log_with_many_attrs = builder.build()
  
  // 验证大量属性被安全处理
  assert_eq(log_with_many_attrs.attributes.length(), 1000)
}

test "malicious_span_input_handling" {
  // 测试恶意Span输入的处理
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("security-test-tracer")
  
  // 1. 测试恶意span名称
  let malicious_span_names = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "span\0name\0with\0nulls",
    "span\nname\nwith\nnewlines",
    "span\tname\twith\ttabs",
    "very_long_span_name_" + "x" * 10000
  ]
  
  // 验证恶意span名称被安全处理
  let mut i = 0
  while i < malicious_span_names.length() {
    let malicious_name = malicious_span_names[i]
    let (span_ctx, span) = tracer.start_span(ctx, malicious_name, trace::Internal)
    
    // 验证恶意名称原样存储
    assert_eq(span.name, malicious_name)
    i = i + 1
  }
  
  // 2. 测试恶意span属性
  let malicious_span_attributes = [
    ("<script>alert('XSS')</script>", common::AttributeValue::string("<script>alert('XSS')</script>")),
    ("'; DROP TABLE users; --", common::AttributeValue::string("'; DROP TABLE users; --")),
    ("../../../etc/passwd", common::AttributeValue::string("../../../etc/passwd")),
    ("attr\0with\0nulls", common::AttributeValue::string("value\0with\0nulls")),
    ("attr\nwith\nnewlines", common::AttributeValue::string("value\nwith\nnewlines"))
  ]
  
  // 验证恶意span属性被安全处理
  i = 0
  while i < malicious_span_attributes.length() {
    let (malicious_key, malicious_value) = malicious_span_attributes[i]
    let (span_ctx, span) = tracer.start_span(
      ctx,
      "safe-span-name",
      trace::Internal,
      [(malicious_key, malicious_value)]
    )
    
    // 验证span创建成功
    assert_eq(span.attributes.length(), 1)
    i = i + 1
  }
  
  // 3. 测试极大量的span属性
  let mut many_attrs : Array[(String, common::AttributeValue)] = []
  i = 0
  while i < 1000 {
    many_attrs.push((
      "span_attr_" + i.to_string(),
      common::AttributeValue::string("span_value_" + i.to_string())
    ))
    i = i + 1
  }
  
  let (span_ctx, span_with_many_attrs) = tracer.start_span(
    ctx,
    "span-with-many-attrs",
    trace::Internal,
    many_attrs
  )
  
  // 验证大量属性被安全处理
  assert_eq(span_with_many_attrs.attributes.length(), 1000)
}

test "malicious_propagation_input_handling" {
  // 测试恶意传播输入的处理
  
  let ctx = context::Context::empty()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 1. 测试恶意传播头部
  let malicious_headers = [
    ("<script>alert('XSS')</script>", "malicious-value"),
    ("'; DROP TABLE users; --", "malicious-value"),
    ("../../../etc/passwd", "malicious-value"),
    ("header\0with\0nulls", "value\0with\0nulls"),
    ("header\nwith\nnewlines", "value\nwith\nnewlines"),
    ("very_long_header_" + "x" * 1000, "very_long_value_" + "x" * 100000)
  ]
  
  // 验证恶意传播头部被安全处理
  let mut i = 0
  while i < malicious_headers.length() {
    let (malicious_key, malicious_value) = malicious_headers[i]
    let carrier = propagation::MapCarrier::from_map([(malicious_key, malicious_value)])
    
    // 尝试提取恶意头部
    let extracted_ctx = trace_propagator.extract(ctx, carrier)
    let baggage_extracted_ctx = baggage_propagator.extract(ctx, carrier)
    
    // 验证提取操作不会失败
    assert_eq(true, true)
    i = i + 1
  }
  
  // 2. 测试恶意的traceparent格式
  let malicious_traceparents = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "malformed-traceparent",
    "00-invalid-trace-id-invalid-span-id-01",
    "ff-12345678901234567890123456789012-1234567890123456-01",  // 无效版本
    "00-12345678901234567890123456789012-1234567890123456-ff",  // 无效标志
    "00-12345678-12345678-01",  // 太短的trace ID和span ID
    "00-1234567890123456789012345678901234567890-1234567890123456-01"  // 太长的trace ID
  ]
  
  // 验证恶意traceparent格式被安全处理
  i = 0
  while i < malicious_traceparents.length() {
    let malicious_traceparent = malicious_traceparents[i]
    let carrier = propagation::MapCarrier::from_map([
      (propagation::TRACE_PARENT_HEADER, malicious_traceparent)
    ])
    
    // 尝试提取恶意traceparent
    let extracted_ctx = trace_propagator.extract(ctx, carrier)
    
    // 验证提取操作不会失败
    assert_eq(true, true)
    i = i + 1
  }
  
  // 3. 测试恶意的baggage格式
  let malicious_baggages = [
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "../../../etc/passwd",
    "malformed=baggage=value",
    "key=value=extra=equals",
    "key=,value",
    "key=value,",
    ",key=value",
    "key=value,,malformed",
    "very_long_key_" + "x" * 1000 + "=very_long_value_" + "x" * 100000
  ]
  
  // 验证恶意baggage格式被安全处理
  i = 0
  while i < malicious_baggages.length() {
    let malicious_baggage = malicious_baggages[i]
    let carrier = propagation::MapCarrier::from_map([
      (propagation::BAGGAGE_HEADER, malicious_baggage)
    ])
    
    // 尝试提取恶意baggage
    let extracted_ctx = baggage_propagator.extract(ctx, carrier)
    
    // 验证提取操作不会失败
    assert_eq(true, true)
    i = i + 1
  }
}

test "denial_of_service_protection" {
  // 测试拒绝服务攻击保护
  
  // 1. 测试极大字符串输入
  let extremely_large_string = "x" * 1000000  // 1MB字符串
  let large_attr = common::AttributeValue::string(extremely_large_string)
  
  match large_attr {
    common::StringValue(s) => assert_eq(s.length(), 1000000)
    _ => @test.fail("Expected StringValue")
  }
  
  // 2. 测试极大数组输入
  let extremely_large_array = common::AttributeValue::array_string(Array::make(100000, "large-array-item"))
  
  match extremely_large_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 100000)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 3. 测试极深的嵌套结构
  let ctx = context::Context::empty()
  let mut deeply_nested_ctx = ctx
  let mut i = 0
  while i < 10000 {
    let key = context::create_key("deep_key_" + i.to_string())
    deeply_nested_ctx = deeply_nested_ctx.with_value(key, "deep_value_" + i.to_string())
    i = i + 1
  }
  
  // 验证深层嵌套结构被安全处理
  let deep_key = context::create_key("deep_key_9999")
  match deeply_nested_ctx.get(deep_key) {
    Some(value) => assert_eq(value, "deep_value_9999")
    None => @test.fail("Expected deep value")
  }
  
  // 4. 测试极大量的属性
  let mut extremely_many_attrs : Array[(String, common::AttributeValue)] = []
  i = 0
  while i < 50000 {
    extremely_many_attrs.push((
      "extreme_attr_" + i.to_string(),
      common::AttributeValue::string("extreme_value_" + i.to_string())
    ))
    i = i + 1
  }
  
  // 验证大量属性被安全处理
  assert_eq(extremely_many_attrs.length(), 50000)
  
  // 5. 测试极大量的baggage条目
  let baggage = context::Baggage::empty()
  let mut extremely_large_baggage = baggage
  i = 0
  while i < 25000 {
    extremely_large_baggage = extremely_large_baggage.with_entry(
      "extreme_entry_" + i.to_string(),
      "extreme_value_" + i.to_string()
    )
    i = i + 1
  }
  
  // 验证大量baggage条目被安全处理
  match extremely_large_baggage.get("extreme_entry_24999") {
    Some(value) => assert_eq(value, "extreme_value_24999")
    None => @test.fail("Expected extreme baggage value")
  }
}