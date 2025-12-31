// 遥测跨服务数据传播测试用例
// 测试遥测数据在不同服务之间的传播能力，包括分布式追踪和上下文传播

test "telemetry_w3c_trace_context_propagation" {
  // 测试W3C Trace Context传播
  
  let original_trace_context = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 解析traceparent头
  let traceparent_parts = original_trace_context.get("traceparent", "").split("-")
  assert_eq(traceparent_parts.length(), 4)
  assert_eq(traceparent_parts[0], "00")  // version
  assert_eq(traceparent_parts[1], "0af7651916cd43dd8448eb211c80319c")  // trace_id
  assert_eq(traceparent_parts[2], "b7ad6b7169203331")  // span_id
  assert_eq(traceparent_parts[3], "01")  // flags
  
  // 验证trace_id格式（32个十六进制字符）
  let trace_id = traceparent_parts[1]
  assert_eq(trace_id.length(), 32)
  assert_eq(is_valid_hex(trace_id), true)
  
  // 验证span_id格式（16个十六进制字符）
  let span_id = traceparent_parts[2]
  assert_eq(span_id.length(), 16)
  assert_eq(is_valid_hex(span_id), true)
  
  // 验证flags
  let flags = traceparent_parts[3]
  assert_eq(flags.length(), 2)
  let sampled_flag = flags[1] == '1'
  assert_eq(sampled_flag, true)
  
  // 解析tracestate头
  let tracestate = original_trace_context.get("tracestate", "")
  let tracestate_entries = tracestate.split(",")
  
  assert_eq(tracestate_entries.length(), 2)
  assert_eq(tracestate_entries[0], "rojo=00f067aa0ba902b7")
  assert_eq(tracestate_entries[1], "congo=t61rcWkgMzE")
  
  // 模拟服务间传播
  let service_a_to_b_headers = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    "x-request-id": "req-12345",
    "user-agent": "service-a/1.0.0"
  }
  
  // 服务B接收并生成新的span
  let service_b_span_id = generate_span_id()
  let service_b_traceparent = "00-0af7651916cd43dd8448eb211c80319c-" + service_b_span_id + "-01"
  
  // 服务B传播给服务C
  let service_b_to_c_headers = {
    "traceparent": service_b_traceparent,
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE,service_b=added_value",
    "x-request-id": "req-12345",
    "x-service-b-timestamp": "1672531200"
  }
  
  // 验证传播的traceparent
  let b_to_c_traceparent = service_b_to_c_headers.get("traceparent", "")
  let b_to_c_parts = b_to_c_traceparent.split("-")
  
  assert_eq(b_to_c_parts[1], trace_id)  // trace_id应该保持不变
  assert_eq(b_to_c_parts[2], service_b_span_id)  // span_id应该是服务B生成的
  assert_eq(b_to_c_parts[3], "01")  // flags应该保持
  
  // 验证tracestate的更新
  let b_to_c_tracestate = service_b_to_c_headers.get("tracestate", "")
  let b_to_c_entries = b_to_c_tracestate.split(",")
  
  assert_eq(b_to_c_entries.length(), 3)  // 增加了一个新的条目
  assert_eq(b_to_c_entries[0], "rojo=00f067aa0ba902b7")
  assert_eq(b_to_c_entries[1], "congo=t61rcWkgMzE")
  assert_eq(b_to_c_entries[2], "service_b=added_value")
  
  // 模拟服务链路追踪
  let service_chain = [
    {
      "service_name": "gateway",
      "span_id": "b7ad6b7169203331",
      "parent_span_id": "",
      "operation": "http.request",
      "start_time": 1672531200000000000,
      "end_time": 1672531200050000000,
      "duration_ms": 5
    },
    {
      "service_name": "auth-service",
      "span_id": service_b_span_id,
      "parent_span_id": "b7ad6b7169203331",
      "operation": "authenticate",
      "start_time": 1672531200010000000,
      "end_time": 1672531200040000000,
      "duration_ms": 3
    },
    {
      "service_name": "user-service",
      "span_id": generate_span_id(),
      "parent_span_id": service_b_span_id,
      "operation": "get_user_profile",
      "start_time": 1672531200020000000,
      "end_time": 1672531200080000000,
      "duration_ms": 6
    }
  ]
  
  // 验证服务链路
  assert_eq(service_chain.length(), 3)
  
  // 验证父子关系
  assert_eq(service_chain[1].get("parent_span_id", ""), service_chain[0].get("span_id", ""))
  assert_eq(service_chain[2].get("parent_span_id", ""), service_chain[1].get("span_id", ""))
  
  // 验证时间顺序
  assert_eq(service_chain[0].get("start_time", 0) <= service_chain[1].get("start_time", 0), true)
  assert_eq(service_chain[1].get("start_time", 0) <= service_chain[2].get("start_time", 0), true)
  
  // 计算总链路时间
  let chain_start_time = service_chain[0].get("start_time", 0)
  let chain_end_time = service_chain[2].get("end_time", 0)
  let total_chain_duration = (chain_end_time - chain_start_time) / 1000000  // 转换为毫秒
  
  assert_eq(total_chain_duration, 8)  // 8毫秒总链路时间
}

test "telemetry_baggage_propagation" {
  // 测试Baggage传播
  
  let original_baggage = {
    "user-id": "user-12345",
    "tenant-id": "tenant-67890",
    "request-source": "mobile-app",
    "session-id": "session-abcdef",
    "feature-flags": "new-ui,experimental-api"
  }
  
  // 转换为HTTP头格式
  let baggage_header = encode_baggage_header(original_baggage)
  
  // 验证编码结果
  assert_eq(baggage_header.contains("user-id=user-12345"), true)
  assert_eq(baggage_header.contains("tenant-id=tenant-67890"), true)
  assert_eq(baggage_header.contains("request-source=mobile-app"), true)
  assert_eq(baggage_header.contains("session-id=session-abcdef"), true)
  assert_eq(baggage_header.contains("feature-flags=new-ui,experimental-api"), true)
  
  // 解析baggage头
  let parsed_baggage = decode_baggage_header(baggage_header)
  
  // 验证解析结果
  assert_eq(parsed_baggage.get("user-id", ""), "user-12345")
  assert_eq(parsed_baggage.get("tenant-id", ""), "tenant-67890")
  assert_eq(parsed_baggage.get("request-source", ""), "mobile-app")
  assert_eq(parsed_baggage.get("session-id", ""), "session-abcdef")
  assert_eq(parsed_baggage.get("feature-flags", ""), "new-ui,experimental-api")
  
  // 模拟服务间baggage传播
  let service_flow = [
    {
      "service": "api-gateway",
      "received_baggage": {},
      "added_baggage": {"gateway-version": "2.1.0"},
      "removed_baggage": [],
      "forwarded_baggage": {}
    },
    {
      "service": "order-service",
      "received_baggage": {"user-id": "user-12345", "tenant-id": "tenant-67890"},
      "added_baggage": {"order-type": "premium", "processing-priority": "high"},
      "removed_baggage": [],
      "forwarded_baggage": {}
    },
    {
      "service": "payment-service",
      "received_baggage": {"user-id": "user-12345", "tenant-id": "tenant-67890", "order-type": "premium"},
      "added_baggage": {"payment-method": "credit-card", "currency": "USD"},
      "removed_baggage": ["processing-priority"],  // 移除敏感信息
      "forwarded_baggage": {}
    }
  ]
  
  // 模拟baggage在服务链中的传播
  let mut current_baggage = original_baggage
  
  let mut i = 0
  while i < service_flow.length() {
    let service = service_flow[i]
    let service_name = service.get("service", "")
    
    // 服务接收当前baggage
    service.set("received_baggage", current_baggage)
    
    // 服务添加新的baggage项
    let added = service.get("added_baggage", {})
    let added_keys = added.keys()
    let mut j = 0
    
    while j < added_keys.length() {
      let key = added_keys[j]
      current_baggage.set(key, added.get(key, ""))
      j = j + 1
    }
    
    // 服务移除baggage项
    let removed = service.get("removed_baggage", [])
    let mut k = 0
    
    while k < removed.length() {
      let key = removed[k]
      current_baggage.remove(key)
      k = k + 1
    }
    
    // 服务转发更新后的baggage
    service.set("forwarded_baggage", current_baggage)
    
    i = i + 1
  }
  
  // 验证传播结果
  let gateway_service = service_flow[0]
  let order_service = service_flow[1]
  let payment_service = service_flow[2]
  
  // 验证gateway服务
  let gateway_received = gateway_service.get("received_baggage", {})
  let gateway_forwarded = gateway_service.get("forwarded_baggage", {})
  
  assert_eq(gateway_received.get("user-id", ""), "user-12345")
  assert_eq(gateway_forwarded.get("gateway-version", ""), "2.1.0")
  
  // 验证order服务
  let order_received = order_service.get("received_baggage", {})
  let order_forwarded = order_service.get("forwarded_baggage", {})
  
  assert_eq(order_received.get("gateway-version", ""), "2.1.0")
  assert_eq(order_forwarded.get("order-type", ""), "premium")
  assert_eq(order_forwarded.get("processing-priority", ""), "high")
  
  // 验证payment服务
  let payment_received = payment_service.get("received_baggage", {})
  let payment_forwarded = payment_service.get("forwarded_baggage", {})
  
  assert_eq(payment_received.get("order-type", ""), "premium")
  assert_eq(payment_forwarded.get("payment-method", ""), "credit-card")
  assert_eq(payment_forwarded.get("currency", ""), "USD")
  
  // 验证敏感信息被移除
  assert_eq(payment_forwarded.contains("processing-priority"), false)
  
  // 验证原始信息仍然存在
  assert_eq(payment_forwarded.get("user-id", ""), "user-12345")
  assert_eq(payment_forwarded.get("tenant-id", ""), "tenant-67890")
  
  // 测试baggage大小限制
  let large_baggage = {}
  let mut i = 0
  while i < 100 {  // 创建100个baggage项
    large_baggage.set("key-" + i.to_string(), "value-" + i.to_string())
    i = i + 1
  }
  
  let large_baggage_header = encode_baggage_header(large_baggage)
  
  // 验证大小限制（假设最大8KB）
  let max_baggage_size = 8192
  let truncated_baggage = truncate_baggage(large_baggage_header, max_baggage_size)
  
  assert_eq(truncated_baggage.length() <= max_baggage_size, true)
  assert_eq(truncated_baggage.contains("..."), true)  // 应该包含截断标记
}

test "telemetry_cross_service_metrics_correlation" {
  // 测试跨服务指标关联
  
  let distributed_transaction = {
    "transaction_id": "txn-abc123",
    "trace_id": "def45678901234567890123456789012",
    "start_time": 1672531200000000000,
    "services": [
      {
        "name": "api-gateway",
        "span_id": "1111111111111111",
        "metrics": {
          "request_count": 1,
          "request_duration_ms": 15,
          "cpu_usage_percent": 25.5,
          "memory_usage_mb": 128
        },
        "attributes": {
          "http.method": "POST",
          "http.path": "/api/orders",
          "http.status_code": 200
        }
      },
      {
        "name": "order-service",
        "span_id": "2222222222222222",
        "metrics": {
          "request_count": 1,
          "request_duration_ms": 45,
          "database_query_count": 3,
          "database_duration_ms": 20,
          "cpu_usage_percent": 65.2,
          "memory_usage_mb": 256
        },
        "attributes": {
          "operation": "create_order",
          "order_type": "premium",
          "customer_tier": "gold"
        }
      },
      {
        "name": "payment-service",
        "span_id": "3333333333333333",
        "metrics": {
          "request_count": 1,
          "request_duration_ms": 120,
          "external_api_calls": 2,
          "external_api_duration_ms": 85,
          "cpu_usage_percent": 45.8,
          "memory_usage_mb": 192
        },
        "attributes": {
          "payment_method": "credit_card",
          "payment_gateway": "stripe",
          "currency": "USD",
          "amount": 99.99
        }
      }
    ]
  }
  
  // 验证分布式事务结构
  assert_eq(distributed_transaction.get("transaction_id", ""), "txn-abc123")
  assert_eq(distributed_transaction.get("trace_id", ""), "def45678901234567890123456789012")
  
  let services = distributed_transaction.get("services", [])
  assert_eq(services.length(), 3)
  
  // 计算跨服务指标汇总
  let mut total_request_count = 0
  let mut total_request_duration = 0
  let mut max_cpu_usage = 0.0
  let mut total_memory_usage = 0
  let mut service_durations = []
  
  let mut i = 0
  while i < services.length() {
    let service = services[i]
    let service_name = service.get("name", "")
    let metrics = service.get("metrics", {})
    
    total_request_count = total_request_count + metrics.get("request_count", 0)
    total_request_duration = total_request_duration + metrics.get("request_duration_ms", 0)
    
    let cpu_usage = metrics.get("cpu_usage_percent", 0.0)
    if cpu_usage > max_cpu_usage {
      max_cpu_usage = cpu_usage
    }
    
    total_memory_usage = total_memory_usage + metrics.get("memory_usage_mb", 0)
    
    service_durations.push({
      "service": service_name,
      "duration_ms": metrics.get("request_duration_ms", 0)
    })
    
    i = i + 1
  }
  
  // 验证汇总指标
  assert_eq(total_request_count, 3)
  assert_eq(total_request_duration, 180)  // 15 + 45 + 120
  assert_eq(max_cpu_usage, 65.2)  // order-service有最高的CPU使用率
  assert_eq(total_memory_usage, 576)  // 128 + 256 + 192
  
  // 计算服务间时间分布
  let mut total_percentage = 0.0
  i = 0
  while i < service_durations.length() {
    let duration_info = service_durations[i]
    let duration = duration_info.get("duration_ms", 0)
    let percentage = (duration.to_double() / total_request_duration.to_double()) * 100.0
    duration_info.set("percentage", percentage)
    total_percentage = total_percentage + percentage
    i = i + 1
  }
  
  // 验证百分比总和
  assert_eq(abs(total_percentage - 100.0) < 0.01, true)
  
  // 验证具体服务的时间占比
  let gateway_duration = service_durations[0]
  let order_duration = service_durations[1]
  let payment_duration = service_durations[2]
  
  assert_eq(gateway_duration.get("percentage", 0.0), 8.33)  // 15/180 * 100
  assert_eq(order_duration.get("percentage", 0.0), 25.0)   // 45/180 * 100
  assert_eq(payment_duration.get("percentage", 0.0), 66.67) // 120/180 * 100
  
  // 测试跨服务错误传播和关联
  let error_scenario = {
    "transaction_id": "txn-error-456",
    "trace_id": "error78901234567890123456789012345678",
    "error_chain": [
      {
        "service": "api-gateway",
        "span_id": "aaaa111111111111",
        "error": {
          "type": "timeout",
          "message": "Upstream service timeout",
          "timestamp": 1672531200050000000,
          "recovered": false
        }
      },
      {
        "service": "inventory-service",
        "span_id": "bbbb222222222222",
        "error": {
          "type": "database_error",
          "message": "Connection to database failed",
          "timestamp": 1672531200080000000,
          "recovered": true,
          "retry_count": 3
        }
      },
      {
        "service": "notification-service",
        "span_id": "cccc333333333333",
        "error": {
          "type": "external_service_error",
          "message": "Email service unavailable",
          "timestamp": 1672531200150000000,
          "recovered": true,
          "fallback_used": true
        }
      }
    ]
  }
  
  // 分析错误链
  let error_chain = error_scenario.get("error_chain", [])
  assert_eq(error_chain.length(), 3)
  
  // 统计错误类型
  let mut timeout_errors = 0
  let mut database_errors = 0
  let mut external_service_errors = 0
  let mut recovered_errors = 0
  let mut unrecovered_errors = 0
  
  let mut i = 0
  while i < error_chain.length() {
    let error_info = error_chain[i]
    let error = error_info.get("error", {})
    let error_type = error.get("type", "")
    let recovered = error.get("recovered", false)
    
    if error_type == "timeout" {
      timeout_errors = timeout_errors + 1
    } else if error_type == "database_error" {
      database_errors = database_errors + 1
    } else if error_type == "external_service_error" {
      external_service_errors = external_service_errors + 1
    }
    
    if recovered {
      recovered_errors = recovered_errors + 1
    } else {
      unrecovered_errors = unrecovered_errors + 1
    }
    
    i = i + 1
  }
  
  // 验证错误统计
  assert_eq(timeout_errors, 1)
  assert_eq(database_errors, 1)
  assert_eq(external_service_errors, 1)
  assert_eq(recovered_errors, 2)
  assert_eq(unrecovered_errors, 1)
  
  // 计算错误恢复率
  let total_errors = error_chain.length()
  let recovery_rate = recovered_errors.to_double() / total_errors.to_double()
  assert_eq(recovery_rate, 0.6666666666666666)  // 2/3
}

test "telemetry_cross_protocol_propagation" {
  // 测试跨协议传播（HTTP -> gRPC -> Message Queue）
  
  let http_request = {
    "method": "POST",
    "url": "https://api.example.com/v1/orders",
    "headers": {
      "traceparent": "00-abc123456789012345678901234567890-def1234567890123-01",
      "tracestate": "vendor=acme,env=production",
      "baggage": "user-id=user-789,session-id=session-456",
      "x-request-id": "req-http-001",
      "content-type": "application/json"
    },
    "body": "{\"product_id\": \"prod-123\", \"quantity\": 2}",
    "timestamp": 1672531200000000000
  }
  
  // HTTP到gRPC的传播
  let grpc_request = {
    "method": "/orderService/CreateOrder",
    "metadata": {
      "traceparent": "00-abc123456789012345678901234567890-def1234567890123-01",
      "tracestate": "vendor=acme,env=production",
      "baggage": "user-id=user-789,session-id=session-456,protocol=http",
      "x-request-id": "req-http-001",
      "user-agent": "order-service/1.0.0"
    },
    "payload": {
      "product_id": "prod-123",
      "quantity": 2,
      "user_id": "user-789"
    },
    "timestamp": 1672531200020000000
  }
  
  // 验证HTTP到gRPC的传播
  let http_headers = http_request.get("headers", {})
  let grpc_metadata = grpc_request.get("metadata", {})
  
  assert_eq(grpc_metadata.get("traceparent", ""), http_headers.get("traceparent", ""))
  assert_eq(grpc_metadata.get("tracestate", ""), http_headers.get("tracestate", ""))
  assert_eq(grpc_metadata.get("x-request-id", ""), http_headers.get("x-request-id", ""))
  
  // 验证baggage的增强
  let http_baggage = http_headers.get("baggage", "")
  let grpc_baggage = grpc_metadata.get("baggage", "")
  
  assert_eq(grpc_baggage.contains(http_baggage), true)
  assert_eq(grpc_baggage.contains("protocol=http"), true)
  
  // gRPC到Message Queue的传播
  let message_queue_event = {
    "topic": "order_events",
    "event_type": "order_created",
    "headers": {
      "traceparent": "00-abc123456789012345678901234567890-ghi7890123456789-01",
      "tracestate": "vendor=acme,env=production",
      "baggage": "user-id=user-789,session-id=session-456,protocol=http,protocol=grpc",
      "x-request-id": "req-http-001",
      "event-source": "order-service",
      "content-type": "application/json"
    },
    "payload": {
      "order_id": "order-456",
      "user_id": "user-789",
      "product_id": "prod-123",
      "quantity": 2,
      "status": "created",
      "timestamp": 1672531200050000000
    },
    "timestamp": 1672531200050000000
  }
  
  // 验证gRPC到Message Queue的传播
  let mq_headers = message_queue_event.get("headers", {})
  
  // trace_id应该保持不变，span_id应该更新
  let mq_traceparent = mq_headers.get("traceparent", "")
  let mq_traceparent_parts = mq_traceparent.split("-")
  let http_traceparent_parts = http_headers.get("traceparent", "").split("-")
  
  assert_eq(mq_traceparent_parts[1], http_traceparent_parts[1])  // trace_id相同
  assert_eq(mq_traceparent_parts[2], "ghi7890123456789")  // 新的span_id
  assert_eq(mq_traceparent_parts[3], "01")  // flags相同
  
  // 验证baggage的累积
  let mq_baggage = mq_headers.get("baggage", "")
  assert_eq(mq_baggage.contains("protocol=http"), true)
  assert_eq(mq_baggage.contains("protocol=grpc"), true)
  
  // 验证请求ID的持续传播
  assert_eq(mq_headers.get("x-request-id", ""), "req-http-001")
  
  // 模拟异步处理后的响应传播
  let async_response = {
    "response_to": "order-456",
    "headers": {
      "traceparent": "00-abc123456789012345678901234567890-jkl0123456789012-01",
      "tracestate": "vendor=acme,env=production",
      "baggage": "user-id=user-789,session-id=session-456,protocol=http,protocol=grpc,protocol=mq",
      "x-request-id": "req-http-001",
      "correlation-id": "corr-789",
      "async-operation": "inventory_check"
    },
    "result": {
      "success": true,
      "inventory_available": true,
      "reserved_quantity": 2
    },
    "timestamp": 1672531200100000000
  }
  
  // 验证异步响应传播
  let response_headers = async_response.get("headers", {})
  let response_traceparent = response_headers.get("traceparent", "")
  let response_traceparent_parts = response_traceparent.split("-")
  
  assert_eq(response_traceparent_parts[1], http_traceparent_parts[1])  // trace_id仍然相同
  assert_eq(response_traceparent_parts[2], "jkl0123456789012")  // 又一个新的span_id
  
  // 验证协议链的累积
  let response_baggage = response_headers.get("baggage", "")
  assert_eq(response_baggage.contains("protocol=mq"), true)
  
  // 测试跨协议的上下文清理
  let context_cleanup = {
    "original_headers": {
      "traceparent": "00-abc123456789012345678901234567890-def1234567890123-01",
      "tracestate": "vendor=acme,env=production,sensitive=data",
      "baggage": "user-id=user-789,password=secret123,session-id=session-456",
      "authorization": "Bearer token123"
    },
    "cleanup_rules": {
      "tracestate": ["sensitive"],
      "baggage": ["password"],
      "headers": ["authorization"]
    },
    "cleaned_headers": {}
  }
  
  // 执行上下文清理
  let original_headers = context_cleanup.get("original_headers", {})
  let cleanup_rules = context_cleanup.get("cleanup_rules", {})
  let mut cleaned_headers = {}
  
  // 清理tracestate
  let original_tracestate = original_headers.get("tracestate", "")
  let tracestate_entries = original_tracestate.split(",")
  let mut clean_tracestate_entries = []
  
  let mut i = 0
  while i < tracestate_entries.length() {
    let entry = tracestate_entries[i]
    let entry_parts = entry.split("=")
    let entry_key = entry_parts[0]
    
    let mut should_remove = false
    let mut j = 0
    let sensitive_keys = cleanup_rules.get("tracestate", [])
    
    while j < sensitive_keys.length() {
      if entry_key == sensitive_keys[j] {
        should_remove = true
        break
      }
      j = j + 1
    }
    
    if not should_remove {
      clean_tracestate_entries.push(entry)
    }
    
    i = i + 1
  }
  
  cleaned_headers.set("tracestate", clean_tracestate_entries.join(","))
  
  // 清理baggage
  let original_baggage = original_headers.get("baggage", "")
  let baggage_entries = original_baggage.split(",")
  let mut clean_baggage_entries = []
  
  i = 0
  while i < baggage_entries.length() {
    let entry = baggage_entries[i]
    let entry_parts = entry.split("=")
    let entry_key = entry_parts[0]
    
    let mut should_remove = false
    let mut j = 0
    let sensitive_baggage_keys = cleanup_rules.get("baggage", [])
    
    while j < sensitive_baggage_keys.length() {
      if entry_key == sensitive_baggage_keys[j] {
        should_remove = true
        break
      }
      j = j + 1
    }
    
    if not should_remove {
      clean_baggage_entries.push(entry)
    }
    
    i = i + 1
  }
  
  cleaned_headers.set("baggage", clean_baggage_entries.join(","))
  
  // 保留非敏感的headers
  cleaned_headers.set("traceparent", original_headers.get("traceparent", ""))
  
  // 验证清理结果
  assert_eq(cleaned_headers.get("traceparent", ""), original_headers.get("traceparent", ""))
  assert_eq(cleaned_headers.get("tracestate", ""), "vendor=acme,env=production")
  assert_eq(cleaned_headers.get("baggage", ""), "user-id=user-789,session-id=session-456")
  assert_eq(cleaned_headers.contains("authorization"), false)
  
  context_cleanup.set("cleaned_headers", cleaned_headers)
}

// 辅助函数：生成span ID
fn generate_span_id() {
  let characters = "0123456789abcdef"
  let mut result = ""
  let mut i = 0
  
  while i < 16 {
    let index = (i * 3) % 16  // 简化的伪随机数生成
    result = result + characters[index].to_string()
    i = i + 1
  }
  
  result
}

// 辅助函数：验证十六进制字符串
fn is_valid_hex(hex_string) {
  let valid_chars = "0123456789abcdefABCDEF"
  let mut i = 0
  let mut valid = true
  
  while i < hex_string.length() {
    let char = hex_string[i]
    if not valid_chars.contains(char) {
      valid = false
      break
    }
    i = i + 1
  }
  
  valid
}

// 辅助函数：编码baggage头
fn encode_baggage_header(baggage_map) {
  let entries = []
  let keys = baggage_map.keys()
  let mut i = 0
  
  while i < keys.length() {
    let key = keys[i]
    let value = baggage_map.get(key, "")
    entries.push(key + "=" + value)
    i = i + 1
  }
  
  entries.join(",")
}

// 辅助函数：解码baggage头
fn decode_baggage_header(baggage_header) {
  let result = {}
  let entries = baggage_header.split(",")
  let mut i = 0
  
  while i < entries.length() {
    let entry = entries[i]
    let parts = entry.split("=")
    
    if parts.length() == 2 {
      result.set(parts[0], parts[1])
    }
    
    i = i + 1
  }
  
  result
}

// 辅助函数：截断baggage
fn truncate_baggage(baggage_string, max_size) {
  if baggage_string.length() <= max_size {
    baggage_string
  } else {
    baggage_string.substring(0, max_size - 3) + "..."
  }
}
