// 遥测Span种类测试用例

test "basic_span_kinds" {
  // 测试基本Span种类
  
  // 测试预定义的Span种类
  let server_kind = azimuth::telemetry::api::trace::SpanKind::SERVER
  assert_eq(server_kind.get_name(), "SERVER")
  assert_eq(server_kind.get_description(), "Server span represents the handling of a request from a remote client")
  
  let client_kind = azimuth::telemetry::api::trace::SpanKind::CLIENT
  assert_eq(client_kind.get_name(), "CLIENT")
  assert_eq(client_kind.get_description(), "Client span represents an outgoing request to a remote service")
  
  let producer_kind = azimuth::telemetry::api::trace::SpanKind::PRODUCER
  assert_eq(producer_kind.get_name(), "PRODUCER")
  assert_eq(producer_kind.get_description(), "Producer span represents the creation of a message or event")
  
  let consumer_kind = azimuth::telemetry::api::trace::SpanKind::CONSUMER
  assert_eq(consumer_kind.get_name(), "CONSUMER")
  assert_eq(consumer_kind.get_description(), "Consumer span represents the processing of a message or event")
  
  let internal_kind = azimuth::telemetry::api::trace::SpanKind::INTERNAL
  assert_eq(internal_kind.get_name(), "INTERNAL")
  assert_eq(internal_kind.get_description(), "Internal span represents an operation within an application")
}

test "span_kind_characteristics" {
  // 测试Span种类特征
  
  // 测试SERVER特征
  let server_kind = azimuth::telemetry::api::trace::SpanKind::SERVER
  assert_eq(server_kind.is_remote(), true)      // 处理远程请求
  assert_eq(server_kind.is_inbound(), true)     // 入站操作
  assert_eq(server_kind.is_outbound(), false)   // 不是出站操作
  assert_eq(server_kind.is_messaging(), false)  // 不是消息操作
  
  // 测试CLIENT特征
  let client_kind = azimuth::telemetry::api::trace::SpanKind::CLIENT
  assert_eq(client_kind.is_remote(), true)      // 远程调用
  assert_eq(client_kind.is_inbound(), false)    // 不是入站操作
  assert_eq(client_kind.is_outbound(), true)    // 出站操作
  assert_eq(client_kind.is_messaging(), false)  // 不是消息操作
  
  // 测试PRODUCER特征
  let producer_kind = azimuth::telemetry::api::trace::SpanKind::PRODUCER
  assert_eq(producer_kind.is_remote(), true)      // 发送到远程
  assert_eq(producer_kind.is_inbound(), false)    // 不是入站操作
  assert_eq(producer_kind.is_outbound(), true)    // 出站操作
  assert_eq(producer_kind.is_messaging(), true)   // 消息操作
  
  // 测试CONSUMER特征
  let consumer_kind = azimuth::telemetry::api::trace::SpanKind::CONSUMER
  assert_eq(consumer_kind.is_remote(), true)      // 从远程接收
  assert_eq(consumer_kind.is_inbound(), true)     // 入站操作
  assert_eq(consumer_kind.is_outbound(), false)   // 不是出站操作
  assert_eq(consumer_kind.is_messaging(), true)   // 消息操作
  
  // 测试INTERNAL特征
  let internal_kind = azimuth::telemetry::api::trace::SpanKind::INTERNAL
  assert_eq(internal_kind.is_remote(), false)     // 内部操作
  assert_eq(internal_kind.is_inbound(), false)    // 不是入站操作
  assert_eq(internal_kind.is_outbound(), false)   // 不是出站操作
  assert_eq(internal_kind.is_messaging(), false)  // 不是消息操作
}

test "span_kind_with_attributes" {
  // 测试带属性的Span种类
  
  // 为SERVER种类创建属性
  let server_attributes = azimuth::telemetry::api::trace::SpanKind::SERVER.get_default_attributes()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("http.target", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
    .with("net.host.name", azimuth::telemetry::api::common::AttributeValue::String("api.example.com"))
    .with("net.host.port", azimuth::telemetry::api::common::AttributeValue::Int64(443))
  
  assert_eq(server_attributes.get("http.method").unwrap().to_string(), "GET")
  assert_eq(server_attributes.get("http.target").unwrap().to_string(), "/api/users")
  assert_eq(server_attributes.get("net.host.name").unwrap().to_string(), "api.example.com")
  assert_eq(server_attributes.get("net.host.port").unwrap().to_string(), "443")
  
  // 为CLIENT种类创建属性
  let client_attributes = azimuth::telemetry::api::trace::SpanKind::CLIENT.get_default_attributes()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("POST"))
    .with("http.url", azimuth::telemetry::api::common::AttributeValue::String("https://external.api.com/data"))
    .with("net.peer.name", azimuth::telemetry::api::common::AttributeValue::String("external.api.com"))
    .with("net.peer.port", azimuth::telemetry::api::common::AttributeValue::Int64(443))
  
  assert_eq(client_attributes.get("http.method").unwrap().to_string(), "POST")
  assert_eq(client_attributes.get("http.url").unwrap().to_string(), "https://external.api.com/data")
  assert_eq(client_attributes.get("net.peer.name").unwrap().to_string(), "external.api.com")
  assert_eq(client_attributes.get("net.peer.port").unwrap().to_string(), "443")
}

test "span_kind_hierarchy_and_relationships" {
  // 测试Span种类层次和关系
  
  // 测试种类层次结构
  let server_kind = azimuth::telemetry::api::trace::SpanKind::SERVER
  let client_kind = azimuth::telemetry::api::trace::SpanKind::CLIENT
  let producer_kind = azimuth::telemetry::api::trace::SpanKind::PRODUCER
  let consumer_kind = azimuth::telemetry::api::trace::SpanKind::CONSUMER
  let internal_kind = azimuth::telemetry::api::trace::SpanKind::INTERNAL
  
  // 测试父子关系
  assert_eq(server_kind.is_parent_of(client_kind), true)   // 服务器可以创建客户端调用
  assert_eq(client_kind.is_child_of(server_kind), true)   // 客户端调用可以是服务器的子项
  
  assert_eq(producer_kind.is_parent_of(consumer_kind), true)  // 生产者创建消息，消费者处理
  assert_eq(consumer_kind.is_child_of(producer_kind), true)   // 消费者是生产者的子项
  
  // 测试兼容性
  assert_eq(server_kind.is_compatible_with(client_kind), true)    // 服务器和客户端兼容
  assert_eq(client_kind.is_compatible_with(server_kind), true)    // 客户端和服务器兼容
  assert_eq(producer_kind.is_compatible_with(consumer_kind), true) // 生产者和消费者兼容
  assert_eq(consumer_kind.is_compatible_with(producer_kind), true) // 消费者和生产者兼容
  assert_eq(internal_kind.is_compatible_with(server_kind), true)   // 内部操作与服务器兼容
  assert_eq(internal_kind.is_compatible_with(client_kind), true)   // 内部操作与客户端兼容
}

test "span_kind_validation_and_constraints" {
  // 测试Span种类验证和约束
  
  // 测试有效的种类名称
  let valid_names = ["SERVER", "CLIENT", "PRODUCER", "CONSUMER", "INTERNAL"]
  for name in valid_names {
    let kind = azimuth::telemetry::api::trace::SpanKind::from_name(name)
    assert_eq(kind.is_some(), true)
    assert_eq(kind.unwrap().get_name(), name)
  }
  
  // 测试无效的种类名称
  let invalid_names = ["INVALID", "SERVER123", "", "server", "Server"]
  for name in invalid_names {
    let kind = azimuth::telemetry::api::trace::SpanKind::from_name(name)
    assert_eq(kind.is_none(), true)
  }
  
  // 测试自定义种类创建
  let custom_kind = azimuth::telemetry::api::trace::SpanKind::custom(
    "CUSTOM",
    "Custom span kind for specific use cases",
    false,  // is_remote
    false,  // is_inbound
    false,  // is_outbound
    false   // is_messaging
  )
  
  assert_eq(custom_kind.get_name(), "CUSTOM")
  assert_eq(custom_kind.get_description(), "Custom span kind for specific use cases")
  assert_eq(custom_kind.is_remote(), false)
  assert_eq(custom_kind.is_inbound(), false)
  assert_eq(custom_kind.is_outbound(), false)
  assert_eq(custom_kind.is_messaging(), false)
}

test "span_kind_serialization" {
  // 测试Span种类序列化
  
  // 测试预定义种类的序列化
  let server_kind = azimuth::telemetry::api::trace::SpanKind::SERVER
  let json_server = server_kind.to_json()
  assert_eq(json_server.contains("SERVER"), true)
  assert_eq(json_server.contains("Server span represents"), true)
  
  let restored_server = azimuth::telemetry::api::trace::SpanKind::from_json(json_server)
  assert_eq(restored_server.get_name(), server_kind.get_name())
  assert_eq(restored_server.get_description(), server_kind.get_description())
  
  // 测试自定义种类的序列化
  let custom_kind = azimuth::telemetry::api::trace::SpanKind::custom(
    "BATCH_PROCESSOR",
    "Batch processing span kind",
    false,
    true,
    false,
    false
  )
  
  let json_custom = custom_kind.to_json()
  assert_eq(json_custom.contains("BATCH_PROCESSOR"), true)
  assert_eq(json_custom.contains("Batch processing span kind"), true)
  
  let restored_custom = azimuth::telemetry::api::trace::SpanKind::from_json(json_custom)
  assert_eq(restored_custom.get_name(), custom_kind.get_name())
  assert_eq(restored_custom.get_description(), custom_kind.get_description())
  assert_eq(restored_custom.is_remote(), custom_kind.is_remote())
  assert_eq(restored_custom.is_inbound(), custom_kind.is_inbound())
}

test "span_kind_usage_patterns" {
  // 测试Span种类使用模式
  
  // 模拟HTTP服务器请求处理
  let server_span = azimuth::telemetry::api::trace::SpanBuilder::new("handle-request")
    .with_kind(azimuth::telemetry::api::trace::SpanKind::SERVER)
    .with_attribute("http.method", "GET")
    .with_attribute("http.target", "/api/users")
    .start()
  
  // 模拟数据库客户端调用
  let db_client_span = azimuth::telemetry::api::trace::SpanBuilder::new("db-query")
    .with_kind(azimuth::telemetry::api::trace::SpanKind::CLIENT)
    .with_attribute("db.system", "postgresql")
    .with_attribute("db.statement", "SELECT * FROM users")
    .as_child_of(server_span)
    .start()
  
  // 模拟内部业务逻辑
  let business_span = azimuth::telemetry::api::trace::SpanBuilder::new("business-logic")
    .with_kind(azimuth::telemetry::api::trace::SpanKind::INTERNAL)
    .with_attribute("operation", "user-validation")
    .as_child_of(db_client_span)
    .start()
  
  // 验证Span种类设置
  assert_eq(server_span.get_kind().get_name(), "SERVER")
  assert_eq(db_client_span.get_kind().get_name(), "CLIENT")
  assert_eq(business_span.get_kind().get_name(), "INTERNAL")
  
  // 验证父子关系
  assert_eq(db_client_span.get_parent_span_id(), server_span.get_span_id())
  assert_eq(business_span.get_parent_span_id(), db_client_span.get_span_id())
}

test "span_kind_in_messaging_systems" {
  // 测试消息系统中的Span种类
  
  // 模拟消息生产者
  let producer_span = azimuth::telemetry::api::trace::SpanBuilder::new("publish-message")
    .with_kind(azimuth::telemetry::api::trace::SpanKind::PRODUCER)
    .with_attribute("messaging.system", "kafka")
    .with_attribute("messaging.destination", "user-events")
    .with_attribute("messaging.message_id", "msg-12345")
    .start()
  
  // 模拟消息消费者
  let consumer_span = azimuth::telemetry::api::trace::SpanBuilder::new("process-message")
    .with_kind(azimuth::telemetry::api::trace::SpanKind::CONSUMER)
    .with_attribute("messaging.system", "kafka")
    .with_attribute("messaging.destination", "user-events")
    .with_attribute("messaging.message_id", "msg-12345")
    .with_attribute("messaging.consumer_group", "user-processors")
    .start()
  
  // 验证消息相关属性
  assert_eq(producer_span.get_kind().is_messaging(), true)
  assert_eq(consumer_span.get_kind().is_messaging(), true)
  assert_eq(producer_span.get_kind().is_outbound(), true)
  assert_eq(consumer_span.get_kind().is_inbound(), true)
  
  // 验证消息属性
  assert_eq(producer_span.get_attribute("messaging.system").unwrap().to_string(), "kafka")
  assert_eq(consumer_span.get_attribute("messaging.system").unwrap().to_string(), "kafka")
  assert_eq(producer_span.get_attribute("messaging.destination").unwrap().to_string(), "user-events")
  assert_eq(consumer_span.get_attribute("messaging.destination").unwrap().to_string(), "user-events")
}

test "span_kind_performance_optimization" {
  // 测试Span种类性能优化
  
  // 测试种类查找性能
  let start_time = azimuth::telemetry::api::common::time::now()
  
  for i in range(0, 10000) {
    let kind = azimuth::telemetry::api::trace::SpanKind::from_name("SERVER")
    assert_eq(kind.is_some(), true)
  }
  
  let end_time = azimuth::telemetry::api::common::time::now()
  let duration = end_time - start_time
  
  // 验证查找性能（应该在合理时间内完成）
  assert_eq(duration < 1000000, true)  // 小于1秒（微秒单位）
  
  // 测试属性创建性能
  start_time = azimuth::telemetry::api::common::time::now()
  
  for i in range(0, 10000) {
    let attributes = azimuth::telemetry::api::trace::SpanKind::CLIENT.get_default_attributes()
      .with("request.id", azimuth::telemetry::api::common::AttributeValue::String("req-" + i.to_string()))
  }
  
  end_time = azimuth::telemetry::api::common::time::now()
  duration = end_time - start_time
  
  // 验证属性创建性能
  assert_eq(duration < 5000000, true)  // 小于5秒（微秒单位）
}

test "span_kind_edge_cases_and_error_handling" {
  // 测试Span种类边界情况和错误处理
  
  // 测试空名称
  let empty_kind = azimuth::telemetry::api::trace::SpanKind::from_name("")
  assert_eq(empty_kind.is_none(), true)
  
  // 测试null名称
  let null_kind = azimuth::telemetry::api::trace::SpanKind::from_name("\0")
  assert_eq(null_kind.is_none(), true)
  
  // 测试极长的名称
  let long_name = "THIS_IS_A_VERY_LONG_SPAN_KIND_NAME_THAT_EXCEEDS_NORMAL_LIMITS_AND_TESTS_SYSTEM_BEHAVIOR_WITH_EXTREMELY_LONG_IDENTIFIERS"
  let long_kind = azimuth::telemetry::api::trace::SpanKind::from_name(long_name)
  assert_eq(long_kind.is_none(), true)
  
  // 测试特殊字符名称
  let special_names = ["KIND@WITH#SPECIAL", "KIND-WITH-DASHES", "KIND.WITH.DOTS", "KIND WITH SPACES"]
  for name in special_names {
    let kind = azimuth::telemetry::api::trace::SpanKind::from_name(name)
    assert_eq(kind.is_none(), true)
  }
  
  // 测试Unicode名称
  let unicode_name = "种类"
  let unicode_kind = azimuth::telemetry::api::trace::SpanKind::from_name(unicode_name)
  assert_eq(unicode_kind.is_none(), true)
  
  // 测试大小写敏感性
  let lowercase_kind = azimuth::telemetry::api::trace::SpanKind::from_name("server")
  assert_eq(lowercase_kind.is_none(), true)  // 应该区分大小写
  
  let mixed_case_kind = azimuth::telemetry::api::trace::SpanKind::from_name("Server")
  assert_eq(mixed_case_kind.is_none(), true)  // 应该区分大小写
  
  // 测试自定义种类验证
  let result = azimuth::telemetry::api::trace::SpanKind::validate_custom_kind(
    "",  // 空名称
    "description"
  )
  assert_eq(result.is_valid(), false)
  
  let result2 = azimuth::telemetry::api::trace::SpanKind::validate_custom_kind(
    "VALID_NAME",
    ""  // 空描述
  )
  assert_eq(result2.is_valid(), false)  // 描述不能为空
}