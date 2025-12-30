// 跨服务遥测数据传播测试用例
// 专注于测试不同服务间的遥测数据传播和上下文传递

test "cross_service_trace_context_injection" {
  // 测试跨服务追踪上下文注入
  
  let source_service = "user-service"
  let target_service = "order-service"
  let trace_id = "abc123def45678901234567890123456"
  let span_id = "fedcba0987654321"
  let baggage_items = [
    ("user.id", "user_12345"),
    ("request.id", "req_67890"),
    ("session.id", "sess_abcdef")
  ]
  
  // 验证基本参数
  assert_eq(source_service, "user-service")
  assert_eq(target_service, "order-service")
  assert_eq(trace_id.length(), 32)
  assert_eq(span_id.length(), 16)
  assert_eq(baggage_items.length(), 3)
  
  // 创建追踪上下文
  let trace_context = {
    "trace_id" => trace_id,
    "span_id" => span_id,
    "trace_flags" => "01",
    "trace_state" => "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证追踪上下文
  assert_eq(trace_context["trace_id"], trace_id)
  assert_eq(trace_context["span_id"], span_id)
  assert_eq(trace_context["trace_flags"], "01")
  
  // 注入baggage项
  let mut injected_context = trace_context
  let mut i = 0
  while i < baggage_items.length() {
    let key = baggage_items[i].0
    let value = baggage_items[i].1
    injected_context["baggage." + key] = value
    i = i + 1
  }
  
  // 验证注入的baggage
  assert_eq(injected_context["baggage.user.id"], "user_12345")
  assert_eq(injected_context["baggage.request.id"], "req_67890")
  assert_eq(injected_context["baggage.session.id"], "sess_abcdef")
  
  // 创建HTTP头传播格式
  let traceparent_header = "00-" + trace_id + "-" + span_id + "-01"
  let tracestate_header = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  let baggage_header = "user.id=user_12345,request.id=req_67890,session.id=sess_abcdef"
  
  // 验证HTTP头格式
  assert_eq(traceparent_header.length(), 55)
  assert_eq(traceparent_header.has_prefix("00-"), true)
  assert_eq(tracestate_header.contains("rojo="), true)
  assert_eq(baggage_header.contains("user.id="), true)
  
  // 创建完整的传播头集合
  let propagation_headers = {
    "traceparent" => traceparent_header,
    "tracestate" => tracestate_header,
    "baggage" => baggage_header,
    "x-request-id" => "req_67890",
    "x-origin-service" => source_service
  }
  
  // 验证传播头
  assert_eq(propagation_headers.length(), 5)
  assert_eq(propagation_headers["x-origin-service"], source_service)
  assert_eq(propagation_headers["x-request-id"], "req_67890")
}

test "cross_service_context_extraction" {
  // 测试跨服务上下文提取
  
  let incoming_headers = {
    "traceparent" => "00-def1234567890abcdef1234567890abcdef-fedcba0987654321-01",
    "tracestate" => "vendor1=value1,vendor2=value2",
    "baggage" => "user.id=user_98765,tenant.id=tenant_123,region=us-east-1",
    "x-request-id" => "req_abc123",
    "x-origin-service" => "api-gateway",
    "x-client-version" => "1.2.3"
  }
  
  // 验证传入头
  assert_eq(incoming_headers.length(), 6)
  assert_eq(incoming_headers["traceparent"].length(), 55)
  assert_eq(incoming_headers["x-origin-service"], "api-gateway")
  
  // 提取traceparent信息
  let traceparent = incoming_headers["traceparent"]
  let trace_id = traceparent.split("-")[1]
  let parent_span_id = traceparent.split("-")[2]
  let trace_flags = traceparent.split("-")[3]
  
  // 验证提取的信息
  assert_eq(trace_id.length(), 32)
  assert_eq(parent_span_id.length(), 16)
  assert_eq(trace_flags, "01")
  
  // 提取baggage项
  let baggage_header = incoming_headers["baggage"]
  let baggage_items = baggage_header.split(",")
  let extracted_baggage = {}
  
  let mut i = 0
  while i < baggage_items.length() {
    let item = baggage_items[i]
    let key_value = item.split("=")
    if key_value.length() == 2 {
      extracted_baggage[key_value[0]] = key_value[1]
    }
    i = i + 1
  }
  
  // 验证提取的baggage
  assert_eq(extracted_baggage["user.id"], "user_98765")
  assert_eq(extracted_baggage["tenant.id"], "tenant_123")
  assert_eq(extracted_baggage["region"], "us-east-1")
  
  // 创建提取的上下文
  let extracted_context = {
    "trace_id" => trace_id,
    "parent_span_id" => parent_span_id,
    "trace_flags" => trace_flags,
    "trace_state" => incoming_headers["tracestate"],
    "request_id" => incoming_headers["x-request-id"],
    "origin_service" => incoming_headers["x-origin-service"],
    "client_version" => incoming_headers["x-client-version"],
    "baggage" => extracted_baggage
  }
  
  // 验证提取的上下文
  assert_eq(extracted_context["trace_id"], trace_id)
  assert_eq(extracted_context["parent_span_id"], parent_span_id)
  assert_eq(extracted_context["origin_service"], "api-gateway")
  assert_eq(extracted_context["baggage"]["user.id"], "user_98765")
}

test "cross_service_async_context_propagation" {
  // 测试跨服务异步上下文传播
  
  let main_trace_id = "async1234567890abcdef1234567890abcd"
  let main_span_id = "main1234567890ab"
  let async_operations = [
    "send_email",
    "process_payment", 
    "update_inventory",
    "notify_user"
  ]
  
  // 验证异步操作
  assert_eq(async_operations.length(), 4)
  assert_eq(async_operations[0], "send_email")
  assert_eq(async_operations[3], "notify_user")
  
  // 创建主上下文
  let main_context = {
    "trace_id" => main_trace_id,
    "span_id" => main_span_id,
    "operation_name" => "process_order",
    "user_id" => "user_55566",
    "order_id" => "order_77788"
  }
  
  // 验证主上下文
  assert_eq(main_context["trace_id"], main_trace_id)
  assert_eq(main_context["span_id"], main_span_id)
  assert_eq(main_context["operation_name"], "process_order")
  
  // 为每个异步操作创建子上下文
  let async_contexts = []
  let mut i = 0
  while i < async_operations.length() {
    let operation = async_operations[i]
    let child_span_id = "async_" + i.to_string() + "_" + operation.substring(0, 4)
    
    let async_context = {
      "trace_id" => main_trace_id, // 继承主trace ID
      "parent_span_id" => main_span_id, // 设置父span ID
      "span_id" => child_span_id,
      "operation_name" => operation,
      "user_id" => main_context["user_id"], // 继承用户ID
      "order_id" => main_context["order_id"], // 继承订单ID
      "async_correlation_id" => "corr_" + i.to_string()
    }
    async_contexts.push(async_context)
    i = i + 1
  }
  
  // 验证异步上下文
  assert_eq(async_contexts.length(), 4)
  
  // 验证所有异步操作都继承相同的trace ID
  i = 0
  while i < async_contexts.length() {
    assert_eq(async_contexts[i]["trace_id"], main_trace_id)
    assert_eq(async_contexts[i]["parent_span_id"], main_span_id)
    assert_eq(async_contexts[i]["user_id"], "user_55566")
    assert_eq(async_contexts[i]["order_id"], "order_77788")
    i = i + 1
  }
  
  // 验证每个操作有唯一的span ID
  assert_eq(async_contexts[0]["span_id"], "async_0_send")
  assert_eq(async_contexts[1]["span_id"], "async_1_proc")
  assert_eq(async_contexts[2]["span_id"], "async_2_upda")
  assert_eq(async_contexts[3]["span_id"], "async_3_noti")
  
  // 验证异步关联ID
  assert_eq(async_contexts[0]["async_correlation_id"], "corr_0")
  assert_eq(async_contexts[3]["async_correlation_id"], "corr_3")
}

test "cross_service_message_queue_propagation" {
  // 测试跨服务消息队列传播
  
  let producer_service = "order-service"
  let consumer_service = "notification-service"
  let queue_name = "order_notifications"
  let message_id = "msg_123456789"
  
  // 验证基本参数
  assert_eq(producer_service, "order-service")
  assert_eq(consumer_service, "notification-service")
  assert_eq(queue_name, "order_notifications")
  
  // 创建生产者上下文
  let producer_context = {
    "trace_id" => "queue1234567890abcdef1234567890ab",
    "span_id" => "producer12345678",
    "operation_name" => "send_notification",
    "message_id" => message_id,
    "queue_name" => queue_name,
    "producer_service" => producer_service,
    "timestamp" => "1640995200000"
  }
  
  // 验证生产者上下文
  assert_eq(producer_context["trace_id"].length(), 32)
  assert_eq(producer_context["span_id"].length(), 16)
  assert_eq(producer_context["message_id"], message_id)
  
  // 创建消息头（用于队列传播）
  let message_headers = {
    "trace_id" => producer_context["trace_id"],
    "parent_span_id" => producer_context["span_id"],
    "message_id" => message_id,
    "producer_service" => producer_service,
    "queue_name" => queue_name,
    "content_type" => "application/json",
    "correlation_id" => "corr_" + message_id
  }
  
  // 验证消息头
  assert_eq(message_headers.length(), 7)
  assert_eq(message_headers["trace_id"], producer_context["trace_id"])
  assert_eq(message_headers["correlation_id"], "corr_msg_123456789")
  
  // 模拟消息体
  let message_body = {
    "order_id" => "order_98765",
    "user_id" => "user_54321",
    "notification_type" => "order_confirmation",
    "email" => "user@example.com"
  }
  
  // 创建完整消息
  let queue_message = {
    "headers" => message_headers,
    "body" => message_body,
    "metadata" => {
      "queue" => queue_name,
      "timestamp" => producer_context["timestamp"],
      "priority" => "normal"
    }
  }
  
  // 验证完整消息
  assert_eq(queue_message["headers"]["trace_id"], producer_context["trace_id"])
  assert_eq(queue_message["body"]["order_id"], "order_98765")
  
  // 消费者端提取上下文
  let consumer_context = {
    "trace_id" => queue_message["headers"]["trace_id"],
    "parent_span_id" => queue_message["headers"]["parent_span_id"],
    "span_id" => "consumer12345678",
    "operation_name" => "process_notification",
    "consumer_service" => consumer_service,
    "message_id" => queue_message["headers"]["message_id"],
    "correlation_id" => queue_message["headers"]["correlation_id"],
    "order_id" => queue_message["body"]["order_id"],
    "user_id" => queue_message["body"]["user_id"]
  }
  
  // 验证消费者上下文
  assert_eq(consumer_context["trace_id"], producer_context["trace_id"])
  assert_eq(consumer_context["parent_span_id"], producer_context["span_id"])
  assert_eq(consumer_context["consumer_service"], consumer_service)
  assert_eq(consumer_context["order_id"], "order_98765")
  
  // 验证端到端追踪链路
  assert_eq(consumer_context["trace_id"], producer_context["trace_id"])
  assert_eq(consumer_context["parent_span_id"], producer_context["span_id"])
  assert_eq(consumer_context["message_id"], message_id)
  assert_eq(consumer_context["correlation_id"], "corr_msg_123456789")
}

test "cross_service_grpc_metadata_propagation" {
  // 测试跨服务gRPC元数据传播
  
  let client_service = "frontend-service"
  let server_service = "user-service"
  let method_name = "GetUserProfile"
  let trace_id = "grpc1234567890abcdef1234567890ab"
  
  // 验证基本参数
  assert_eq(client_service, "frontend-service")
  assert_eq(server_service, "user-service")
  assert_eq(method_name, "GetUserProfile")
  assert_eq(trace_id.length(), 32)
  
  // 创建客户端gRPC元数据
  let grpc_metadata = {
    "traceparent" => "00-" + trace_id + "-client1234567890-01",
    "x-client-service" => client_service,
    "x-request-id" => "grpc_req_001",
    "x-user-id" => "user_11223",
    "authorization" => "Bearer token123456",
    "x-call-attempt" => "1"
  }
  
  // 验证gRPC元数据
  assert_eq(grpc_metadata.length(), 6)
  assert_eq(grpc_metadata["traceparent"].contains(trace_id), true)
  assert_eq(grpc_metadata["x-client-service"], client_service)
  
  // 模拟gRPC调用上下文
  let grpc_call_context = {
    "method" => method_name,
    "deadline" => "1640995260000", // 60秒超时
    "metadata" => grpc_metadata,
    "peer_info" => {
      "address" => "192.168.1.100:50051",
      "authority" => "user-service.example.com"
    }
  }
  
  // 验证gRPC调用上下文
  assert_eq(grpc_call_context["method"], method_name)
  assert_eq(grpc_call_context["metadata"]["x-client-service"], client_service)
  assert_eq(grpc_call_context["peer_info"]["address"], "192.168.1.100:50051")
  
  // 服务器端提取和处理
  let server_traceparent = grpc_call_context["metadata"]["traceparent"]
  let server_trace_id = server_traceparent.split("-")[1]
  let client_span_id = server_traceparent.split("-")[2]
  
  // 创建服务器端上下文
  let server_context = {
    "trace_id" => server_trace_id,
    "parent_span_id" => client_span_id,
    "span_id" => "server1234567890",
    "method_name" => method_name,
    "server_service" => server_service,
    "client_service" => grpc_call_context["metadata"]["x-client-service"],
    "request_id" => grpc_call_context["metadata"]["x-request-id"],
    "user_id" => grpc_call_context["metadata"]["x-user-id"],
    "call_attempt" => grpc_call_context["metadata"]["x-call-attempt"].to_int()
  }
  
  // 验证服务器端上下文
  assert_eq(server_context["trace_id"], trace_id)
  assert_eq(server_context["parent_span_id"], "client1234567890")
  assert_eq(server_context["method_name"], method_name)
  assert_eq(server_context["server_service"], server_service)
  assert_eq(server_context["client_service"], client_service)
  assert_eq(server_context["user_id"], "user_11223")
  assert_eq(server_context["call_attempt"], 1)
  
  // 创建响应元数据（用于返回客户端）
  let response_metadata = {
    "traceparent" => "00-" + server_context["trace_id"] + "-" + server_context["span_id"] + "-01",
    "x-server-service" => server_service,
    "x-response-id" => "grpc_resp_001",
    "x-processing-time" => "150", // 毫秒
    "x-status-code" => "0" // OK
  }
  
  // 验证响应元数据
  assert_eq(response_metadata["traceparent"].contains(server_context["trace_id"]), true)
  assert_eq(response_metadata["x-server-service"], server_service)
  assert_eq(response_metadata["x-processing-time"], "150")
  
  // 验证完整的gRPC追踪链路
  assert_eq(response_metadata["traceparent"].split("-")[1], server_context["trace_id"])
  assert_eq(server_context["trace_id"], trace_id)
  assert_eq(server_context["parent_span_id"], "client1234567890")
}

test "cross_service_database_context_propagation" {
  // 测试跨服务数据库操作上下文传播
  
  let application_service = "order-service"
  let database_type = "postgresql"
  let operation_type = "transaction"
  let trace_id = "db1234567890abcdef1234567890abcde"
  
  // 验证基本参数
  assert_eq(application_service, "order-service")
  assert_eq(database_type, "postgresql")
  assert_eq(operation_type, "transaction")
  assert_eq(trace_id.length(), 32)
  
  // 创建应用服务上下文
  let app_context = {
    "trace_id" => trace_id,
    "span_id" => "app1234567890ab",
    "operation_name" => "create_order_transaction",
    "service_name" => application_service,
    "user_id" => "user_99887",
    "order_id" => "order_55667"
  }
  
  // 验证应用上下文
  assert_eq(app_context["trace_id"], trace_id)
  assert_eq(app_context["service_name"], application_service)
  assert_eq(app_context["order_id"], "order_55667")
  
  // 创建数据库操作上下文
  let db_operations = [
    ("BEGIN", "start_transaction"),
    ("INSERT", "orders", "INSERT INTO orders (id, user_id, total) VALUES (?, ?, ?)"),
    ("UPDATE", "inventory", "UPDATE inventory SET quantity = quantity - ? WHERE product_id = ?"),
    ("INSERT", "order_items", "INSERT INTO order_items (order_id, product_id, quantity) VALUES (?, ?, ?)"),
    ("COMMIT", "commit_transaction")
  ]
  
  // 验证数据库操作
  assert_eq(db_operations.length(), 5)
  assert_eq(db_operations[0].0, "BEGIN")
  assert_eq(db_operations[4].0, "COMMIT")
  
  // 为每个数据库操作创建span上下文
  let db_span_contexts = []
  let mut i = 0
  while i < db_operations.length() {
    let operation = db_operations[i]
    let db_span_id = "db_" + i.to_string() + "_" + operation[0].substring(0, 3)
    
    let db_span_context = {
      "trace_id" => app_context["trace_id"],
      "parent_span_id" => app_context["span_id"],
      "span_id" => db_span_id,
      "operation_name" => operation[0] + " " + operation[1],
      "database_type" => database_type,
      "database_statement" => operation.length() > 2 ? operation[2] : "",
      "service_name" => application_service,
      "user_id" => app_context["user_id"],
      "order_id" => app_context["order_id"],
      "operation_type" => operation_type
    }
    db_span_contexts.push(db_span_context)
    i = i + 1
  }
  
  // 验证数据库span上下文
  assert_eq(db_span_contexts.length(), 5)
  
  // 验证所有数据库操作都继承相同的上下文
  i = 0
  while i < db_span_contexts.length() {
    assert_eq(db_span_contexts[i]["trace_id"], trace_id)
    assert_eq(db_span_contexts[i]["parent_span_id"], app_context["span_id"])
    assert_eq(db_span_contexts[i]["service_name"], application_service)
    assert_eq(db_span_contexts[i]["user_id"], "user_99887")
    assert_eq(db_span_contexts[i]["order_id"], "order_55667")
    i = i + 1
  }
  
  // 验证特定的数据库操作信息
  assert_eq(db_span_contexts[0]["operation_name"], "BEGIN orders")
  assert_eq(db_span_contexts[1]["operation_name"], "INSERT orders")
  assert_eq(db_span_contexts[2]["operation_name"], "UPDATE inventory")
  assert_eq(db_span_contexts[4]["operation_name"], "COMMIT commit_transaction")
  
  // 验证SQL语句
  assert_eq(db_span_contexts[1]["database_statement"].contains("INSERT INTO orders"), true)
  assert_eq(db_span_contexts[2]["database_statement"].contains("UPDATE inventory"), true)
  
  // 创建数据库会话上下文（用于连接池管理）
  let db_session_context = {
    "session_id" => "db_session_12345",
    "connection_id" => "conn_67890",
    "trace_id" => trace_id,
    "service_name" => application_service,
    "transaction_id" => "txn_" + app_context["order_id"],
    "isolation_level" => "READ_COMMITTED",
    "auto_commit" => false,
    "spans" => db_span_contexts
  }
  
  // 验证数据库会话上下文
  assert_eq(db_session_context["session_id"], "db_session_12345")
  assert_eq(db_session_context["trace_id"], trace_id)
  assert_eq(db_session_context["transaction_id"], "txn_order_55667")
  assert_eq(db_session_context["spans"].length(), 5)
}