// Azimuth Telemetry - Context Propagation Test
// 上下文传播测试

test "context_basic_operations" {
  // 测试Context基本操作
  let ctx = context::Context::empty()
  
  // 验证空上下文
  let key1 = context::create_key("test-key-1")
  let value1 = ctx.get(key1)
  assert(value1.is_none(), "Empty context should not contain any values")
  
  // 添加值到上下文
  let ctx_with_value = ctx.with_value(key1, "test-value-1")
  let retrieved_value = ctx_with_value.get(key1)
  match retrieved_value {
    Some(v) => assert_eq(v, "test-value-1")
    None => assert(false, "Expected value in context")
  }
  
  // 验证原始上下文未改变
  let original_value = ctx.get(key1)
  assert(original_value.is_none(), "Original context should remain unchanged")
}

test "context_multiple_values" {
  // 测试在Context中存储多个值
  let ctx = context::Context::empty()
  
  // 创建多个key
  let key1 = context::create_key("user-id")
  let key2 = context::create_key("request-id")
  let key3 = context::create_key("trace-id")
  let key4 = context::create_key("session-id")
  
  // 逐步添加值
  let ctx1 = ctx.with_value(key1, "user-123")
  let ctx2 = ctx1.with_value(key2, "req-456")
  let ctx3 = ctx2.with_value(key3, "trace-789")
  let ctx4 = ctx3.with_value(key4, "session-abc")
  
  // 验证所有值都存在
  assert_eq(ctx4.get(key1), Some("user-123"))
  assert_eq(ctx4.get(key2), Some("req-456"))
  assert_eq(ctx4.get(key3), Some("trace-789"))
  assert_eq(ctx4.get(key4), Some("session-abc"))
  
  // 验证中间上下文状态
  assert_eq(ctx1.get(key1), Some("user-123"))
  assert_eq(ctx1.get(key2), None)
  assert_eq(ctx2.get(key1), Some("user-123"))
  assert_eq(ctx2.get(key2), Some("req-456"))
  assert_eq(ctx2.get(key3), None)
}

test "context_value_overwrite" {
  // 测试覆盖Context中的值
  let ctx = context::Context::empty()
  let key = context::create_key("overwrite-test")
  
  // 添加初始值
  let ctx1 = ctx.with_value(key, "initial-value")
  assert_eq(ctx1.get(key), Some("initial-value"))
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "updated-value")
  assert_eq(ctx2.get(key), Some("updated-value"))
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final-value")
  assert_eq(ctx3.get(key), Some("final-value"))
  
  // 验证历史上下文不受影响
  assert_eq(ctx1.get(key), Some("initial-value"))
  assert_eq(ctx2.get(key), Some("updated-value"))
}

test "context_complex_values" {
  // 测试复杂值的存储（JSON字符串、序列化数据等）
  let ctx = context::Context::empty()
  
  // 存储JSON字符串
  let json_key = context::create_key("json-data")
  let json_value = "{\"user\":{\"id\":123,\"name\":\"John\"},\"roles\":[\"admin\",\"user\"]}"
  let ctx1 = ctx.with_value(json_key, json_value)
  
  // 存储序列化数据
  let serialized_key = context::create_key("serialized-data")
  let serialized_value = "user:123|session:abc|timestamp:1640995200"
  let ctx2 = ctx1.with_value(serialized_key, serialized_value)
  
  // 存储长字符串
  let long_key = context::create_key("long-data")
  let long_value = "x".repeat(1000)
  let ctx3 = ctx2.with_value(long_key, long_value)
  
  // 存储特殊字符
  let special_key = context::create_key("special-chars")
  let special_value = "Special: !@#$%^&*()_+-=[]{}|;':\",./<>? Unicode: 🚀 🌟 中文"
  let ctx4 = ctx3.with_value(special_key, special_value)
  
  // 验证所有值
  assert_eq(ctx4.get(json_key), Some(json_value))
  assert_eq(ctx4.get(serialized_key), Some(serialized_value))
  assert_eq(ctx4.get(long_key), Some(long_value))
  assert_eq(ctx4.get(special_key), Some(special_value))
}

test "baggage_basic_operations" {
  // 测试Baggage基本操作
  let baggage = context::Baggage::empty()
  
  // 验证空baggage
  let value = baggage.get("test-key")
  assert(value.is_none(), "Empty baggage should not contain any entries")
  
  // 添加条目
  let baggage1 = baggage.with_entry("user-id", "12345")
  let retrieved_value = baggage1.get("user-id")
  match retrieved_value {
    Some(v) => assert_eq(v, "12345")
    None => assert(false, "Expected value in baggage")
  }
  
  // 验证原始baggage未改变
  let original_value = baggage.get("user-id")
  assert(original_value.is_none(), "Original baggage should remain unchanged")
}

test "baggage_multiple_entries" {
  // 测试在Baggage中存储多个条目
  let baggage = context::Baggage::empty()
  
  // 逐步添加条目
  let baggage1 = baggage.with_entry("user-id", "12345")
  let baggage2 = baggage1.with_entry("session-id", "session-abc")
  let baggage3 = baggage2.with_entry("request-id", "req-789")
  let baggage4 = baggage3.with_entry("trace-id", "trace-xyz")
  
  // 验证所有条目都存在
  assert_eq(baggage4.get("user-id"), Some("12345"))
  assert_eq(baggage4.get("session-id"), Some("session-abc"))
  assert_eq(baggage4.get("request-id"), Some("req-789"))
  assert_eq(baggage4.get("trace-id"), Some("trace-xyz"))
  
  // 验证不存在的键
  assert_eq(baggage4.get("non-existent"), None)
}

test "baggage_entry_overwrite" {
  // 测试覆盖Baggage中的条目
  let baggage = context::Baggage::empty()
  
  // 添加初始条目
  let baggage1 = baggage.with_entry("key", "value1")
  assert_eq(baggage1.get("key"), Some("value1"))
  
  // 覆盖条目
  let baggage2 = baggage1.with_entry("key", "value2")
  assert_eq(baggage2.get("key"), Some("value2"))
  
  // 再次覆盖
  let baggage3 = baggage2.with_entry("key", "value3")
  assert_eq(baggage3.get("key"), Some("value3"))
  
  // 验证历史baggage不受影响
  assert_eq(baggage1.get("key"), Some("value1"))
  assert_eq(baggage2.get("key"), Some("value2"))
}

test "context_and_baggage_integration" {
  // 测试Context和Baggage的集成使用
  let ctx = context::Context::empty()
  let baggage = context::Baggage::empty()
  
  // 在Context中存储Baggage
  let baggage_key = context::create_key("baggage")
  let baggage_str = "user-id=12345,session-id=session-abc"
  let ctx_with_baggage = ctx.with_value(baggage_key, baggage_str)
  
  // 在Context中存储其他信息
  let trace_key = context::create_key("trace-info")
  let trace_info = "trace-id=trace-789,span-id=span-123"
  let ctx_final = ctx_with_baggage.with_value(trace_key, trace_info)
  
  // 验证Context中的信息
  assert_eq(ctx_final.get(baggage_key), Some(baggage_str))
  assert_eq(ctx_final.get(trace_key), Some(trace_info))
  
  // 同时维护独立的Baggage
  let baggage_final = baggage
    .with_entry("correlation-id", "corr-xyz")
    .with_entry("request-source", "mobile-app")
  
  assert_eq(baggage_final.get("correlation-id"), Some("corr-xyz"))
  assert_eq(baggage_final.get("request-source"), Some("mobile-app"))
}

test "context_propagation_simulation" {
  // 模拟跨服务上下文传播
  let ctx = context::Context::empty()
  
  // 服务A：创建初始上下文
  let user_key = context::create_key("user-id")
  let request_key = context::create_key("request-id")
  let ctx_a = ctx
    .with_value(user_key, "user-123")
    .with_value(request_key, "req-abc")
  
  // 服务B：从服务A接收上下文并添加信息
  let service_key = context::create_key("service-name")
  let operation_key = context::create_key("operation")
  let ctx_b = ctx_a
    .with_value(service_key, "service-B")
    .with_value(operation_key, "process-data")
  
  // 服务C：从服务B接收上下文并添加信息
  let duration_key = context::create_key("duration-ms")
  let status_key = context::create_key("status")
  let ctx_c = ctx_b
    .with_value(duration_key, "150")
    .with_value(status_key, "success")
  
  // 验证最终上下文包含所有信息
  assert_eq(ctx_c.get(user_key), Some("user-123"))
  assert_eq(ctx_c.get(request_key), Some("req-abc"))
  assert_eq(ctx_c.get(service_key), Some("service-B"))
  assert_eq(ctx_c.get(operation_key), Some("process-data"))
  assert_eq(ctx_c.get(duration_key), Some("150"))
  assert_eq(ctx_c.get(status_key), Some("success"))
  
  // 验证中间上下文状态
  assert_eq(ctx_a.get(user_key), Some("user-123"))
  assert_eq(ctx_a.get(service_key), None)
  assert_eq(ctx_b.get(user_key), Some("user-123"))
  assert_eq(ctx_b.get(service_key), Some("service-B"))
  assert_eq(ctx_b.get(duration_key), None)
}

test "context_edge_cases" {
  // 测试Context边界情况
  let ctx = context::Context::empty()
  
  // 测试空键名
  let empty_key = context::create_key("")
  let ctx1 = ctx.with_value(empty_key, "empty-key-value")
  assert_eq(ctx1.get(empty_key), Some("empty-key-value"))
  
  // 测试空值
  let key = context::create_key("empty-value")
  let ctx2 = ctx1.with_value(key, "")
  assert_eq(ctx2.get(key), Some(""))
  
  // 测试长键名
  let long_key_name = "key-".repeat(100)
  let long_key = context::create_key(long_key_name)
  let ctx3 = ctx2.with_value(long_key, "long-key-value")
  assert_eq(ctx3.get(long_key), Some("long-key-value"))
  
  // 测试长值
  let long_value = "value-".repeat(1000)
  let ctx4 = ctx3.with_value(key, long_value)
  assert_eq(ctx4.get(key), Some(long_value))
  
  // 测试特殊字符键名
  let special_key_name = "key.special@chars#123"
  let special_key = context::create_key(special_key_name)
  let ctx5 = ctx4.with_value(special_key, "special-value")
  assert_eq(ctx5.get(special_key), Some("special-value"))
  
  // 测试特殊字符值
  let special_value = "value!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let ctx6 = ctx5.with_value(key, special_value)
  assert_eq(ctx6.get(key), Some(special_value))
}

test "baggage_edge_cases" {
  // 测试Baggage边界情况
  let baggage = context::Baggage::empty()
  
  // 测试空键名
  let baggage1 = baggage.with_entry("", "empty-key-value")
  assert_eq(baggage1.get(""), Some("empty-key-value"))
  
  // 测试空值
  let baggage2 = baggage1.with_entry("empty-value", "")
  assert_eq(baggage2.get("empty-value"), Some(""))
  
  // 测试长键名
  let long_key_name = "key-".repeat(100)
  let baggage3 = baggage2.with_entry(long_key_name, "long-key-value")
  assert_eq(baggage3.get(long_key_name), Some("long-key-value"))
  
  // 测试长值
  let long_value = "value-".repeat(1000)
  let baggage4 = baggage3.with_entry("long-value", long_value)
  assert_eq(baggage4.get("long-value"), Some(long_value))
  
  // 测试特殊字符键名
  let special_key_name = "key.special@chars#123"
  let baggage5 = baggage4.with_entry(special_key_name, "special-value")
  assert_eq(baggage5.get(special_key_name), Some("special-value"))
  
  // 测试特殊字符值
  let special_value = "value!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let baggage6 = baggage5.with_entry("special-value", special_value)
  assert_eq(baggage6.get("special-value"), Some(special_value))
  
  // 测试Unicode
  let unicode_key = "unicode-键"
  let unicode_value = "unicode-值-🚀-🌟"
  let baggage7 = baggage6.with_entry(unicode_key, unicode_value)
  assert_eq(baggage7.get(unicode_key), Some(unicode_value))
}