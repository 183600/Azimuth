// 更多遥测相关的测试用例，使用基本MoonBit语法

test "telemetry_baggage_items" {
  // 测试遥测行李项功能
  
  let baggage_key1 = "user.id"
  let baggage_value1 = "user-12345"
  let baggage_key2 = "request.id"
  let baggage_value2 = "req-67890"
  let baggage_key3 = "session.id"
  let baggage_value3 = "sess-abcde"
  
  // 验证行李项键
  assert_eq(baggage_key1.has_suffix(".id"), true)
  assert_eq(baggage_key2.has_suffix(".id"), true)
  assert_eq(baggage_key3.has_suffix(".id"), true)
  
  // 验证行李项值
  assert_eq(baggage_value1.has_prefix("user-"), true)
  assert_eq(baggage_value2.has_prefix("req-"), true)
  assert_eq(baggage_value3.has_prefix("sess-"), true)
  
  // 创建行李项数组
  let baggage_items = [
    (baggage_key1, baggage_value1),
    (baggage_key2, baggage_value2),
    (baggage_key3, baggage_value3)
  ]
  
  // 验证行李项数组
  assert_eq(baggage_items.length(), 3)
  assert_eq(baggage_items[0].0, "user.id")
  assert_eq(baggage_items[0].1, "user-12345")
  assert_eq(baggage_items[2].0, "session.id")
  assert_eq(baggage_items[2].1, "sess-abcde")
}

test "telemetry_span_events" {
  // 测试遥测Span事件
  
  let event_name1 = "database.query"
  let event_name2 = "cache.hit"
  let event_name3 = "http.request"
  
  let event_timestamp1 = 1640995200000000000L
  let event_timestamp2 = 1640995200000000100L
  let event_timestamp3 = 1640995200000000200L
  
  // 验证事件名称
  assert_eq(event_name1.has_prefix("database"), true)
  assert_eq(event_name2.has_prefix("cache"), true)
  assert_eq(event_name3.has_prefix("http"), true)
  
  assert_eq(event_name1.has_suffix(".query"), true)
  assert_eq(event_name2.has_suffix(".hit"), true)
  assert_eq(event_name3.has_suffix(".request"), true)
  
  // 验证时间戳顺序
  assert_eq(event_timestamp1 < event_timestamp2, true)
  assert_eq(event_timestamp2 < event_timestamp3, true)
  
  // 创建事件数组
  let span_events = [
    (event_name1, event_timestamp1),
    (event_name2, event_timestamp2),
    (event_name3, event_timestamp3)
  ]
  
  // 验证事件数组
  assert_eq(span_events.length(), 3)
  assert_eq(span_events[0].0, "database.query")
  assert_eq(span_events[1].0, "cache.hit")
  assert_eq(span_events[2].0, "http.request")
}

test "telemetry_status_codes" {
  // 测试遥测状态码
  
  let status_unset = "UNSET"
  let status_ok = "OK"
  let status_error = "ERROR"
  
  // 验证状态码字符串
  assert_eq(status_unset, "UNSET")
  assert_eq(status_ok, "OK")
  assert_eq(status_error, "ERROR")
  
  // 验证状态码长度
  assert_eq(status_unset.length(), 5)
  assert_eq(status_ok.length(), 2)
  assert_eq(status_error.length(), 5)
  
  // 创建状态码数组
  let status_codes = [status_unset, status_ok, status_error]
  
  // 验证状态码数组
  assert_eq(status_codes.length(), 3)
  assert_eq(status_codes[0], "UNSET")
  assert_eq(status_codes[1], "OK")
  assert_eq(status_codes[2], "ERROR")
  
  // 验证状态码比较
  assert_eq(status_ok != status_error, true)
  assert_eq(status_unset != status_ok, true)
  assert_eq(status_error != status_unset, true)
}

test "telemetry_instrumentation_scope" {
  // 测试遥测仪器作用域
  
  let scope_name = "database-client"
  let scope_version = "2.1.0"
  let scope_schema_url = "https://opentelemetry.io/schemas/1.20.0"
  
  // 验证作用域名称
  assert_eq(scope_name.has_prefix("database"), true)
  assert_eq(scope_name.has_suffix("-client"), true)
  assert_eq(scope_name.length(), 15)
  
  // 验证作用域版本
  assert_eq(scope_version.has_prefix("2."), true)
  assert_eq(scope_version.has_suffix(".0"), true)
  assert_eq(scope_version.length(), 5)
  
  // 验证模式URL
  assert_eq(scope_schema_url.has_prefix("https://"), true)
  assert_eq(scope_schema_url.has_suffix("/1.20.0"), true)
  assert_eq(scope_schema_url.contains("opentelemetry.io"), true)
  
  // 创建作用域元数据数组
  let scope_metadata = [
    ("name", scope_name),
    ("version", scope_version),
    ("schema_url", scope_schema_url)
  ]
  
  // 验证作用域元数据数组
  assert_eq(scope_metadata.length(), 3)
  assert_eq(scope_metadata[0].0, "name")
  assert_eq(scope_metadata[0].1, "database-client")
  assert_eq(scope_metadata[2].0, "schema_url")
  assert_eq(scope_metadata[2].1, "https://opentelemetry.io/schemas/1.20.0")
}

test "telemetry_propagation_headers" {
  // 测试遥测传播头
  
  let traceparent_header = "traceparent"
  let traceparent_value = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  
  let baggage_header = "baggage"
  let baggage_value = "key1=value1,key2=value2,key3=value3"
  
  // 验证头名称
  assert_eq(traceparent_header, "traceparent")
  assert_eq(baggage_header, "baggage")
  
  // 验证traceparent值
  assert_eq(traceparent_value.has_prefix("00-"), true)
  assert_eq(traceparent_value.contains("-"), true)
  assert_eq(traceparent_value.length(), 55)
  
  // 验证baggage值
  assert_eq(baggage_value.has_prefix("key1="), true)
  assert_eq(baggage_value.has_suffix("value3"), true)
  assert_eq(baggage_value.contains(","), true)
  
  // 创建传播头数组
  let propagation_headers = [
    (traceparent_header, traceparent_value),
    (baggage_header, baggage_value)
  ]
  
  // 验证传播头数组
  assert_eq(propagation_headers.length(), 2)
  assert_eq(propagation_headers[0].0, "traceparent")
  assert_eq(propagation_headers[0].1, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  assert_eq(propagation_headers[1].0, "baggage")
  assert_eq(propagation_headers[1].1, "key1=value1,key2=value2,key3=value3")
}