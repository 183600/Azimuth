// 遥测关联上下文测试用例

test "correlation_context_creation_and_basic_operations" {
  // 测试关联上下文的创建和基本操作
  
  // 创建空关联上下文
  let empty_context = azimuth::telemetry::api::context::CorrelationContext::empty()
  assert_eq(empty_context.size(), 0)
  assert_eq(empty_context.is_empty(), true)
  
  // 添加关联项
  let context1 = empty_context.with("correlation.id", "corr-12345")
  assert_eq(context1.size(), 1)
  assert_eq(context1.is_empty(), false)
  assert_eq(context1.get("correlation.id").unwrap(), "corr-12345")
  
  let context2 = context1.with("session.id", "session-67890")
  assert_eq(context2.size(), 2)
  assert_eq(context2.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(context2.get("session.id").unwrap(), "session-67890")
  
  // 测试覆盖现有关联项
  let context3 = context2.with("correlation.id", "corr-99999")
  assert_eq(context3.size(), 2)  // 覆盖不增加大小
  assert_eq(context3.get("correlation.id").unwrap(), "corr-99999")
  assert_eq(context3.get("session.id").unwrap(), "session-67890")
}

test "correlation_context_propagation" {
  // 测试关联上下文的传播
  
  // 创建父级关联上下文
  let parent_context = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("request.id", "req-abc-123")
    .with("user.id", "user-456")
    .with("trace.id", "trace-789")
  
  // 创建子级关联上下文并继承父级
  let child_context = azimuth::telemetry::api::context::CorrelationContext::from_parent(parent_context)
    .with("operation.id", "op-def-456")
    .with("service.name", "payment-service")
  
  // 验证子级上下文包含父级的关联项
  assert_eq(child_context.size(), 5)
  assert_eq(child_context.get("request.id").unwrap(), "req-abc-123")
  assert_eq(child_context.get("user.id").unwrap(), "user-456")
  assert_eq(child_context.get("trace.id").unwrap(), "trace-789")
  assert_eq(child_context.get("operation.id").unwrap(), "op-def-456")
  assert_eq(child_context.get("service.name").unwrap(), "payment-service")
  
  // 验证父级上下文不受子级影响
  assert_eq(parent_context.size(), 3)
  assert_eq(parent_context.get("operation.id"), None)  // 父级不应包含子级的关联项
}

test "correlation_context_serialization" {
  // 测试关联上下文的序列化
  
  // 创建复杂关联上下文
  let original_context = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("correlation.id", "corr-12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
    .with("user.id", "user-456")
    .with("trace.id", "trace-789")
    .with("operation.type", "payment")
    .with("service.name", "billing-service")
    .with("service.version", "1.2.3")
  
  // 序列化为字符串
  let serialized = original_context.to_string()
  assert_eq(serialized.length() > 0, true)
  assert_eq(serialized.contains("correlation.id"), true)
  assert_eq(serialized.contains("session.id"), true)
  
  // 反序列化
  let deserialized_context = azimuth::telemetry::api::context::CorrelationContext::from_string(serialized)
  assert_eq(deserialized_context.size(), original_context.size())
  
  // 验证反序列化的值
  assert_eq(deserialized_context.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(deserialized_context.get("session.id").unwrap(), "session-67890")
  assert_eq(deserialized_context.get("request.id").unwrap(), "req-abc-123")
  assert_eq(deserialized_context.get("user.id").unwrap(), "user-456")
  assert_eq(deserialized_context.get("trace.id").unwrap(), "trace-789")
  assert_eq(deserialized_context.get("operation.type").unwrap(), "payment")
  assert_eq(deserialized_context.get("service.name").unwrap(), "billing-service")
  assert_eq(deserialized_context.get("service.version").unwrap(), "1.2.3")
}

test "correlation_context_filtering_and_selection" {
  // 测试关联上下文的过滤和选择
  
  // 创建包含多种关联项的上下文
  let context = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("correlation.id", "corr-12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
    .with("user.id", "user-456")
    .with("trace.id", "trace-789")
    .with("operation.id", "op-def-456")
    .with("service.name", "auth-service")
    .with("service.version", "2.1.0")
    .with("client.ip", "192.168.1.100")
    .with("client.user_agent", "Mozilla/5.0")
  
  assert_eq(context.size(), 10)
  
  // 按前缀过滤
  let service_context = context.filter_by_prefix("service.")
  assert_eq(service_context.size(), 2)
  assert_eq(service_context.get("service.name").unwrap(), "auth-service")
  assert_eq(service_context.get("service.version").unwrap(), "2.1.0")
  
  // 按键选择
  let selected_context = context.select(["correlation.id", "user.id", "trace.id"])
  assert_eq(selected_context.size(), 3)
  assert_eq(selected_context.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(selected_context.get("user.id").unwrap(), "user-456")
  assert_eq(selected_context.get("trace.id").unwrap(), "trace-789")
  
  // 排除指定键
  let filtered_context = context.exclude(["client.ip", "client.user_agent"])
  assert_eq(filtered_context.size(), 8)
  assert_eq(filtered_context.get("client.ip"), None)
  assert_eq(filtered_context.get("client.user_agent"), None)
  assert_eq(filtered_context.get("correlation.id").unwrap(), "corr-12345")
}

test "correlation_context_merge_and_combination" {
  // 测试关联上下文的合并和组合
  
  // 创建第一个上下文
  let context1 = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("correlation.id", "corr-12345")
    .with("session.id", "session-67890")
    .with("request.id", "req-abc-123")
  
  // 创建第二个上下文
  let context2 = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("user.id", "user-456")
    .with("trace.id", "trace-789")
    .with("session.id", "session-99999")  // 覆盖session.id
  
  // 合并上下文
  let merged_context = context1.merge(context2)
  assert_eq(merged_context.size(), 4)  // session.id被覆盖
  
  // 验证合并结果
  assert_eq(merged_context.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(merged_context.get("request.id").unwrap(), "req-abc-123")
  assert_eq(merged_context.get("user.id").unwrap(), "user-456")
  assert_eq(merged_context.get("trace.id").unwrap(), "trace-789")
  assert_eq(merged_context.get("session.id").unwrap(), "session-99999")  // 被覆盖的值
}

test "correlation_context_validation_and_edge_cases" {
  // 测试关联上下文的验证和边界情况
  
  // 测试空键和空值
  let context1 = azimuth::telemetry::api::context::CorrelationContext::empty()
    .with("", "empty_key_value")
    .with("empty_value", "")
    .with("normal", "normal_value")
  
  assert_eq(context1.size(), 3)
  assert_eq(context1.get("").unwrap(), "empty_key_value")
  assert_eq(context1.get("empty_value").unwrap(), "")
  assert_eq(context1.get("normal").unwrap(), "normal_value")
  
  // 测试特殊字符键
  let special_key = "key.with.special@chars#and$symbols"
  let context2 = context1.with(special_key, "special_value")
  assert_eq(context2.size(), 4)
  assert_eq(context2.get(special_key).unwrap(), "special_value")
  
  // 测试Unicode键和值
  let unicode_key = "键名.包含.中文"
  let unicode_value = "值.包含.中文.和.🚀.emoji"
  let context3 = context2.with(unicode_key, unicode_value)
  assert_eq(context3.size(), 5)
  assert_eq(context3.get(unicode_key).unwrap(), unicode_value)
  
  // 测试极长的键和值
  let long_key = "this.is.a.very.long.correlation.key.name.that.exceeds.normal.expectations"
  let long_value = "this.is.a.very.long.correlation.value.that.contains.a.lot.of.information"
  let context4 = context3.with(long_key, long_value)
  assert_eq(context4.size(), 6)
  assert_eq(context4.get(long_key).unwrap(), long_value)
  
  // 测试大小写敏感性
  let context5 = context4
    .with("CaseSensitive", "value1")
    .with("casesensitive", "value2")
  
  assert_eq(context5.size(), 8)
  assert_eq(context5.get("CaseSensitive").unwrap(), "value1")
  assert_eq(context5.get("casesensitive").unwrap(), "value2")
}