// 遥测行李传播测试用例

test "basic_baggage_operations" {
  // 测试基本行李操作
  
  // 创建空行李
  let empty_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
  assert_eq(empty_baggage.size(), 0)
  assert_eq(empty_baggage.is_empty(), true)
  
  // 添加行李项
  let baggage1 = empty_baggage.with("user.id", "12345")
  assert_eq(baggage1.size(), 1)
  assert_eq(baggage1.is_empty(), false)
  assert_eq(baggage1.get("user.id").unwrap(), "12345")
  
  let baggage2 = baggage1.with("session.id", "session-67890")
  assert_eq(baggage2.size(), 2)
  assert_eq(baggage2.get("user.id").unwrap(), "12345")
  assert_eq(baggage2.get("session.id").unwrap(), "session-67890")
  
  // 测试覆盖现有行李项
  let baggage3 = baggage2.with("user.id", "99999")
  assert_eq(baggage3.size(), 2)  // 覆盖不增加大小
  assert_eq(baggage3.get("user.id").unwrap(), "99999")
  assert_eq(baggage3.get("session.id").unwrap(), "session-67890")
}

test "baggage_serialization_and_deserialization" {
  // 测试行李的序列化和反序列化
  
  // 创建复杂行李
  let original_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
    .with("trace.origin", "frontend")
    .with("service.version", "1.2.3")
  
  // 序列化为字符串（W3C Baggage格式）
  let serialized = original_baggage.to_string()
  assert_eq(serialized.length() > 0, true)
  assert_eq(serialized.contains("user.id"), true)
  assert_eq(serialized.contains("12345"), true)
  assert_eq(serialized.contains("session.id"), true)
  
  // 反序列化
  let deserialized_baggage = azimuth::telemetry::api::propagation::Baggage::from_string(serialized)
  assert_eq(deserialized_baggage.size(), original_baggage.size())
  
  // 验证反序列化的值
  assert_eq(deserialized_baggage.get("user.id").unwrap(), "12345")
  assert_eq(deserialized_baggage.get("session.id").unwrap(), "session-67890")
  assert_eq(deserialized_baggage.get("request.id").unwrap(), "req-abc-123")
  assert_eq(deserialized_baggage.get("trace.origin").unwrap(), "frontend")
  assert_eq(deserialized_baggage.get("service.version").unwrap(), "1.2.3")
}

test "baggage_with_metadata" {
  // 测试带元数据的行李
  
  // 创建带元数据的行李项
  let baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with_metadata("user.id", "12345", azimuth::telemetry::api::propagation::BaggageMetadata::new("propagate"))
    .with_metadata("session.id", "session-67890", azimuth::telemetry::api::propagation::BaggageMetadata::new("no-propagate"))
    .with("simple.key", "simple.value")  // 无元数据
  
  assert_eq(baggage.size(), 3)
  
  // 获取带元数据的值
  let user_id_entry = baggage.get_entry("user.id")
  assert_eq(user_id_entry.unwrap().get_value(), "12345")
  assert_eq(user_id_entry.unwrap().get_metadata().get_properties().get("propagate").is_some(), true)
  
  let session_id_entry = baggage.get_entry("session.id")
  assert_eq(session_id_entry.unwrap().get_value(), "session-67890")
  assert_eq(session_id_entry.unwrap().get_metadata().get_properties().get("no-propagate").is_some(), true)
  
  let simple_entry = baggage.get_entry("simple.key")
  assert_eq(simple_entry.unwrap().get_value(), "simple.value")
  assert_eq(simple_entry.unwrap().get_metadata().get_properties().size(), 0)
}

test "baggage_propagation_headers" {
  // 测试行李传播头部
  
  // 创建行李
  let baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
  
  // 生成传播头部
  let headers = baggage.to_headers()
  assert_eq(headers.contains("baggage"), true)
  
  let baggage_header = headers.get("baggage").unwrap()
  assert_eq(baggage_header.length() > 0, true)
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("session.id=session-67890"), true)
  assert_eq(baggage_header.contains("request.id=req-abc-123"), true)
  
  // 从头部恢复行李
  let extracted_baggage = azimuth::telemetry::api::propagation::Baggage::from_headers(headers)
  assert_eq(extracted_baggage.size(), baggage.size())
  assert_eq(extracted_baggage.get("user.id").unwrap(), "12345")
  assert_eq(extracted_baggage.get("session.id").unwrap(), "session-67890")
  assert_eq(extracted_baggage.get("request.id").unwrap(), "req-abc-123")
}

test "baggage_filtering_and_selection" {
  // 测试行李的过滤和选择
  
  // 创建包含多种行李项的集合
  let baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("user.name", "张三")
    .with("user.role", "admin")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
    .with("service.name", "auth-service")
    .with("service.version", "1.2.3")
    .with("trace.origin", "frontend")
  
  assert_eq(baggage.size(), 8)
  
  // 按前缀过滤
  let user_baggage = baggage.filter_by_prefix("user.")
  assert_eq(user_baggage.size(), 3)
  assert_eq(user_baggage.get("user.id").unwrap(), "12345")
  assert_eq(user_baggage.get("user.name").unwrap(), "张三")
  assert_eq(user_baggage.get("user.role").unwrap(), "admin")
  
  let service_baggage = baggage.filter_by_prefix("service.")
  assert_eq(service_baggage.size(), 2)
  assert_eq(service_baggage.get("service.name").unwrap(), "auth-service")
  assert_eq(service_baggage.get("service.version").unwrap(), "1.2.3")
  
  // 按键选择
  let selected_baggage = baggage.select(["user.id", "session.id", "trace.origin"])
  assert_eq(selected_baggage.size(), 3)
  assert_eq(selected_baggage.get("user.id").unwrap(), "12345")
  assert_eq(selected_baggage.get("session.id").unwrap(), "session-67890")
  assert_eq(selected_baggage.get("trace.origin").unwrap(), "frontend")
  
  // 排除指定键
  let filtered_baggage = baggage.exclude(["user.name", "user.role"])
  assert_eq(filtered_baggage.size(), 6)
  assert_eq(filtered_baggage.get("user.name"), None)
  assert_eq(filtered_baggage.get("user.role"), None)
  assert_eq(filtered_baggage.get("user.id").unwrap(), "12345")
}

test "baggage_merge_and_combination" {
  // 测试行李的合并和组合
  
  // 创建第一个行李集合
  let baggage1 = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
  
  // 创建第二个行李集合
  let baggage2 = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("service.name", "auth-service")
    .with("service.version", "1.2.3")
    .with("session.id", "session-99999")  // 覆盖session.id
  
  // 合并行李
  let merged_baggage = baggage1.merge(baggage2)
  assert_eq(merged_baggage.size(), 4)  // session.id被覆盖
  
  // 验证合并结果
  assert_eq(merged_baggage.get("user.id").unwrap(), "12345")
  assert_eq(merged_baggage.get("request.id").unwrap(), "req-abc-123")
  assert_eq(merged_baggage.get("service.name").unwrap(), "auth-service")
  assert_eq(merged_baggage.get("service.version").unwrap(), "1.2.3")
  assert_eq(merged_baggage.get("session.id").unwrap(), "session-99999")  // 被覆盖的值
}

test "baggage_size_limits_and_validation" {
  // 测试行李大小限制和验证
  
  // 测试空键和空值
  let baggage1 = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("", "empty_key_value")
    .with("empty_value", "")
    .with("normal", "normal_value")
  
  assert_eq(baggage1.size(), 3)
  assert_eq(baggage1.get("").unwrap(), "empty_key_value")
  assert_eq(baggage1.get("empty_value").unwrap(), "")
  assert_eq(baggage1.get("normal").unwrap(), "normal_value")
  
  // 测试特殊字符键
  let special_key = "key.with.special@chars#and$symbols"
  let baggage2 = baggage1.with(special_key, "special_value")
  assert_eq(baggage2.size(), 4)
  assert_eq(baggage2.get(special_key).unwrap(), "special_value")
  
  // 测试Unicode键和值
  let unicode_key = "键名.包含.中文"
  let unicode_value = "值.包含.中文.和.🚀.emoji"
  let baggage3 = baggage2.with(unicode_key, unicode_value)
  assert_eq(baggage3.size(), 5)
  assert_eq(baggage3.get(unicode_key).unwrap(), unicode_value)
  
  // 测试大小限制
  let large_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
  let max_items = 180  // W3C Baggage规范建议的最大项数
  
  for i in range(0, max_items) {
    large_baggage = large_baggage.with("key" + i.to_string(), "value" + i.to_string())
  }
  
  assert_eq(large_baggage.size(), max_items)
  
  // 测试超出大小限制
  let oversized_baggage = large_baggage.with("oversized.key", "oversized.value")
  // 应该被限制或拒绝，具体实现取决于策略
  assert_eq(oversized_baggage.size() <= max_items, true)
}

test "baggage_cross_service_propagation" {
  // 测试跨服务行李传播
  
  // 模拟前端服务创建的行李
  let frontend_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("session.id", "session-67890")
    .with("client.version", "2.1.0")
    .with("request.id", "req-abc-123")
  
  // 前端传播到API网关
  let gateway_headers = frontend_baggage.to_headers()
  let gateway_baggage = azimuth::telemetry::api::propagation::Baggage::from_headers(gateway_headers)
  
  // API网关添加自己的行李项
  let enhanced_baggage = gateway_baggage
    .with("gateway.request.id", "gw-def-456")
    .with("gateway.timestamp", "2023-12-01T10:00:00Z")
    .with("api.version", "v1")
  
  // API网关传播到后端服务
  let backend_headers = enhanced_baggage.to_headers()
  let backend_baggage = azimuth::telemetry::api::propagation::Baggage::from_headers(backend_headers)
  
  // 验证传播的完整性
  assert_eq(backend_baggage.get("user.id").unwrap(), "12345")
  assert_eq(backend_baggage.get("session.id").unwrap(), "session-67890")
  assert_eq(backend_baggage.get("client.version").unwrap(), "2.1.0")
  assert_eq(backend_baggage.get("request.id").unwrap(), "req-abc-123")
  assert_eq(backend_baggage.get("gateway.request.id").unwrap(), "gw-def-456")
  assert_eq(backend_baggage.get("gateway.timestamp").unwrap(), "2023-12-01T10:00:00Z")
  assert_eq(backend_baggage.get("api.version").unwrap(), "v1")
}

test "baggage_context_integration" {
  // 测试行李与上下文的集成
  
  // 创建上下文
  let context = azimuth::telemetry::api::context::Context::current()
  
  // 在上下文中设置行李
  let baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("correlation.id", "corr-12345")
    .with("user.id", "user-67890")
    .with("trace.origin", "mobile-app")
  
  let context_with_baggage = azimuth::telemetry::api::propagation::Baggage::set_context(context, baggage)
  
  // 从上下文获取行李
  let extracted_baggage = azimuth::telemetry::api::propagation::Baggage::from_context(context_with_baggage)
  assert_eq(extracted_baggage.size(), 3)
  assert_eq(extracted_baggage.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(extracted_baggage.get("user.id").unwrap(), "user-67890")
  assert_eq(extracted_baggage.get("trace.origin").unwrap(), "mobile-app")
  
  // 验证原始上下文未被修改
  let original_baggage = azimuth::telemetry::api::propagation::Baggage::from_context(context)
  assert_eq(original_baggage.is_empty(), true)
}

test "baggage_security_and_privacy" {
  // 测试行李的安全性和隐私
  
  // 创建包含敏感信息的行李
  let sensitive_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("user.id", "12345")
    .with("session.token", "sensitive-token-abc123")
    .with("credit.card.hash", "hashed-credit-card-number")
    .with("public.info", "this-is-public")
  
  // 创建安全过滤器（移除敏感信息）
  let secure_baggage = sensitive_baggage.filter_secure([
    "session.token",
    "credit.card.hash"
  ])
  
  // 验证敏感信息被移除
  assert_eq(secure_baggage.get("user.id").unwrap(), "12345")
  assert_eq(secure_baggage.get("public.info").unwrap(), "this-is-public")
  assert_eq(secure_baggage.get("session.token"), None)
  assert_eq(secure_baggage.get("credit.card.hash"), None)
  
  // 测试PII检测和过滤
  let pii_baggage = azimuth::telemetry::api::propagation::Baggage::empty()
    .with("email", "user@example.com")
    .with("phone", "+1234567890")
    .with("ssn", "123-45-6789")
    .with("user.id", "12345")
  
  let filtered_pii_baggage = pii_baggage.filter_pii()
  assert_eq(filtered_pii_baggage.get("user.id").unwrap(), "12345")
  assert_eq(filtered_pii_baggage.get("email"), None)
  assert_eq(filtered_pii_baggage.get("phone"), None)
  assert_eq(filtered_pii_baggage.get("ssn"), None)
}