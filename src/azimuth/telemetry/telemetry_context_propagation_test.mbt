// 遥测上下文传播测试
// 测试遥测上下文在不同组件和服务间的传播功能

test "trace_context_propagation_test" {
  // 测试追踪上下文传播
  
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "11112222"
  let trace_flags = "01"
  let trace_state = "vendor1=value1,vendor2=value2"
  
  // 创建W3C traceparent格式
  let traceparent = "00-" + trace_id + "-" + span_id + "-" + trace_flags
  assert_eq(traceparent.has_prefix("00-"), true)
  assert_eq(traceparent.contains(trace_id), true)
  assert_eq(traceparent.contains(span_id), true)
  assert_eq(traceparent.has_suffix("-" + trace_flags), true)
  
  // 创建W3C tracestate格式
  let tracestate = trace_state
  assert_eq(tracestate.contains("vendor1=value1"), true)
  assert_eq(tracestate.contains("vendor2=value2"), true)
  
  // 验证traceparent解析
  let traceparent_parts = traceparent.split("-")
  assert_eq(traceparent_parts.length(), 4)
  assert_eq(traceparent_parts[0], "00")
  assert_eq(traceparent_parts[1], trace_id)
  assert_eq(traceparent_parts[2], span_id)
  assert_eq(traceparent_parts[3], trace_flags)
  
  // 验证tracestate解析
  let tracestate_vendors = tracestate.split(",")
  assert_eq(tracestate_vendors.length(), 2)
  assert_eq(tracestate_vendors[0], "vendor1=value1")
  assert_eq(tracestate_vendors[1], "vendor2=value2")
}

test "baggage_propagation_test" {
  // 测试行李(baggage)传播
  
  let baggage_entries = [
    ("user.id", "12345"),
    ("request.id", "req-abcdef"),
    ("tenant.id", "tenant-001"),
    ("debug.mode", "true")
  ]
  
  // 创建baggage头
  let mut baggage_header = ""
  for i = 0; i < baggage_entries.length(); i = i + 1 {
    baggage_header = baggage_header + baggage_entries[i].0 + "=" + baggage_entries[i].1
    if i < baggage_entries.length() - 1 {
      baggage_header = baggage_header + ","
    }
  }
  
  // 验证baggage头格式
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("request.id=req-abcdef"), true)
  assert_eq(baggage_header.contains("tenant.id=tenant-001"), true)
  assert_eq(baggage_header.contains("debug.mode=true"), true)
  
  // 解析baggage头
  let parsed_entries = baggage_header.split(",")
  assert_eq(parsed_entries.length(), 4)
  
  for i = 0; i < parsed_entries.length(); i = i + 1 {
    let entry_parts = parsed_entries[i].split("=")
    assert_eq(entry_parts.length(), 2)
    assert_eq(entry_parts[0], baggage_entries[i].0)
    assert_eq(entry_parts[1], baggage_entries[i].1)
  }
}

test "http_header_propagation_test" {
  // 测试HTTP头传播
  
  let trace_parent_header = "00-abcdef1234567890abcdef1234567890-11112222-01"
  let trace_state_header = "vendor1=value1,vendor2=value2"
  let baggage_header = "user.id=12345,request.id=req-abc"
  let correlation_id_header = "corr-1234567890"
  
  // 创建HTTP头集合
  let http_headers = [
    ("traceparent", trace_parent_header),
    ("tracestate", trace_state_header),
    ("baggage", baggage_header),
    ("x-correlation-id", correlation_id_header)
  ]
  
  // 验证HTTP头传播
  for header in http_headers {
    assert_eq(header.1.length() > 0, true)
  }
  
  // 验证traceparent头
  assert_eq(http_headers[0].0, "traceparent")
  assert_eq(http_headers[0].1.has_prefix("00-"), true)
  
  // 验证tracestate头
  assert_eq(http_headers[1].0, "tracestate")
  assert_eq(http_headers[1].1.contains("vendor1=value1"), true)
  
  // 验证baggage头
  assert_eq(http_headers[2].0, "baggage")
  assert_eq(http_headers[2].1.contains("user.id=12345"), true)
  
  // 验证关联ID头
  assert_eq(http_headers[3].0, "x-correlation-id")
  assert_eq(http_headers[3].1.has_prefix("corr-"), true)
}

test "grpc_metadata_propagation_test" {
  // 测试gRPC元数据传播
  
  let grpc_trace_parent = "00-fedcba0987654321fedcba0987654321-33334444-01"
  let grpc_trace_state = "grpc.vendor=test,value=example"
  let grpc_baggage = "service.name=payment,user.id=67890"
  let grpc_user_agent = "azimuth-telemetry/1.0.0"
  
  // 创建gRPC元数据
  let grpc_metadata = [
    ("grpc-trace-bin", grpc_trace_parent),
    ("grpc-trace-state", grpc_trace_state),
    ("grpc-baggage", grpc_baggage),
    ("user-agent", grpc_user_agent)
  ]
  
  // 验证gRPC元数据传播
  for metadata in grpc_metadata {
    assert_eq(metadata.1.length() > 0, true)
  }
  
  // 验证grpc-trace-bin
  assert_eq(grpc_metadata[0].0, "grpc-trace-bin")
  assert_eq(grpc_metadata[0].1.has_prefix("00-"), true)
  
  // 验证grpc-trace-state
  assert_eq(grpc_metadata[1].0, "grpc-trace-state")
  assert_eq(grpc_metadata[1].1.contains("grpc.vendor=test"), true)
  
  // 验证grpc-baggage
  assert_eq(grpc_metadata[2].0, "grpc-baggage")
  assert_eq(grpc_metadata[2].1.contains("service.name=payment"), true)
  
  // 验证user-agent
  assert_eq(grpc_metadata[3].0, "user-agent")
  assert_eq(grpc_metadata[3].1.contains("azimuth-telemetry"), true)
}

test "message_queue_context_propagation_test" {
  // 测试消息队列上下文传播
  
  let mq_trace_id = "msg11112222333344445555666677778888"
  let mq_span_id = "msgaaaabbbb"
  let mq_correlation_id = "msg-corr-12345"
  let mq_message_id = "msg-id-98765"
  
  // 创建消息属性
  let message_properties = [
    ("trace_id", mq_trace_id),
    ("span_id", mq_span_id),
    ("correlation_id", mq_correlation_id),
    ("message_id", mq_message_id)
  ]
  
  // 验证消息属性传播
  for prop in message_properties {
    assert_eq(prop.1.length() > 0, true)
  }
  
  // 验证trace_id
  assert_eq(message_properties[0].0, "trace_id")
  assert_eq(message_properties[0].1.length(), 32)
  
  // 验证span_id
  assert_eq(message_properties[1].0, "span_id")
  assert_eq(message_properties[1].1.length(), 8)
  
  // 验证correlation_id
  assert_eq(message_properties[2].0, "correlation_id")
  assert_eq(message_properties[2].1.has_prefix("msg-corr-"), true)
  
  // 验证message_id
  assert_eq(message_properties[3].0, "message_id")
  assert_eq(message_properties[3].1.has_prefix("msg-id-"), true)
  
  // 创建消息头字符串
  let mut message_headers = ""
  for prop in message_properties {
    message_headers = message_headers + prop.0 + ":" + prop.1 + "\n"
  }
  
  // 验证消息头字符串
  assert_eq(message_headers.contains("trace_id:" + mq_trace_id), true)
  assert_eq(message_headers.contains("span_id:" + mq_span_id), true)
  assert_eq(message_headers.contains("correlation_id:" + mq_correlation_id), true)
  assert_eq(message_headers.contains("message_id:" + mq_message_id), true)
}

test "cross_thread_context_propagation_test" {
  // 测试跨线程上下文传播
  
  let main_trace_id = "thread1234567890abcdef1234567890ab"
  let main_span_id = "threadcccc"
  let thread_id = "worker-thread-001"
  let operation_name = "async-processing"
  
  // 创建主线程上下文
  let main_context = "trace_id=" + main_trace_id + ":span_id=" + main_span_id + ":thread=main"
  assert_eq(main_context.contains("trace_id=" + main_trace_id), true)
  assert_eq(main_context.contains("span_id=" + main_span_id), true)
  assert_eq(main_context.contains("thread=main"), true)
  
  // 创建工作线程上下文
  let worker_context = "trace_id=" + main_trace_id + ":span_id=" + main_span_id + ":thread=" + thread_id + ":operation=" + operation_name
  assert_eq(worker_context.contains("trace_id=" + main_trace_id), true)
  assert_eq(worker_context.contains("span_id=" + main_span_id), true)
  assert_eq(worker_context.contains("thread=" + thread_id), true)
  assert_eq(worker_context.contains("operation=" + operation_name), true)
  
  // 验证上下文传播
  assert_eq(main_context.contains(main_trace_id), true)
  assert_eq(worker_context.contains(main_trace_id), true)
  assert_eq(main_context.contains(main_span_id), true)
  assert_eq(worker_context.contains(main_span_id), true)
  
  // 验证线程标识不同
  assert_eq(main_context.contains("thread=main"), true)
  assert_eq(worker_context.contains("thread=" + thread_id), true)
  assert_eq(worker_context.contains("operation=" + operation_name), true)
}

test "context_extraction_injection_test" {
  // 测试上下文提取和注入
  
  let original_trace_id = "inject1234567890abcdef1234567890"
  let original_span_id = "injectdddd"
  let original_baggage = "user.id=11111,session.id=22222"
  
  // 注入上下文到载体
  let carrier_inject = "traceparent:00-" + original_trace_id + "-" + original_span_id + "-01\n"
  carrier_inject = carrier_inject + "baggage:" + original_baggage + "\n"
  carrier_inject = carrier_inject + "x-request-id:req-inject-12345"
  
  // 验证注入的载体
  assert_eq(carrier_inject.contains("traceparent:00-" + original_trace_id), true)
  assert_eq(carrier_inject.contains(original_span_id), true)
  assert_eq(carrier_inject.contains("baggage:" + original_baggage), true)
  assert_eq(carrier_inject.contains("x-request-id:req-inject-12345"), true)
  
  // 从载体提取上下文
  let carrier_lines = carrier_inject.split("\n")
  let mut extracted_trace_id = ""
  let mut extracted_span_id = ""
  let mut extracted_baggage = ""
  let mut extracted_request_id = ""
  
  for line in carrier_lines {
    if line.has_prefix("traceparent:") {
      let traceparent_value = line.split(":")[1]
      let traceparent_parts = traceparent_value.split("-")
      extracted_trace_id = traceparent_parts[1]
      extracted_span_id = traceparent_parts[2]
    } else if line.has_prefix("baggage:") {
      extracted_baggage = line.split(":")[1]
    } else if line.has_prefix("x-request-id:") {
      extracted_request_id = line.split(":")[1]
    }
  }
  
  // 验证提取的上下文
  assert_eq(extracted_trace_id, original_trace_id)
  assert_eq(extracted_span_id, original_span_id)
  assert_eq(extracted_baggage, original_baggage)
  assert_eq(extracted_request_id, "req-inject-12345")
  
  // 创建提取的上下文对象
  let extracted_context = "trace_id=" + extracted_trace_id + ":span_id=" + extracted_span_id + ":baggage=" + extracted_baggage + ":request_id=" + extracted_request_id
  assert_eq(extracted_context.contains(original_trace_id), true)
  assert_eq(extracted_context.contains(original_span_id), true)
  assert_eq(extracted_context.contains(original_baggage), true)
  assert_eq(extracted_context.contains("req-inject-12345"), true)
}