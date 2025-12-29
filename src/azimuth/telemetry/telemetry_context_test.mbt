// 遥测上下文测试用例
// 测试上下文传播和管理

test "telemetry_context_creation" {
  // 测试遥测上下文创建
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = "01"
  
  // 创建上下文字符串
  let context = trace_id + ":" + span_id + ":" + trace_flags
  
  // 验证上下文格式
  assert_eq(context.length(), 51) // 32 + 1 + 16 + 1 + 2
  assert_eq(context.has_prefix(trace_id), true)
  assert_eq(context.contains(":" + span_id + ":"), true)
  assert_eq(context.has_suffix(trace_flags), true)
  
  // 解析上下文
  let parts = context.split(":")
  assert_eq(parts.length(), 3)
  assert_eq(parts[0], trace_id)
  assert_eq(parts[1], span_id)
  assert_eq(parts[2], trace_flags)
}

test "telemetry_context_propagation" {
  // 测试遥测上下文传播
  
  let parent_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  
  // 创建子span
  let child_span_id = "c8d9e0f1a2b3c4d5"
  
  // 验证子span继承父trace ID
  assert_eq(child_span_id.length(), 16)
  assert_eq(child_span_id.has_prefix("c8d9"), true)
  assert_eq(child_span_id.has_suffix("c4d5"), true)
  
  // 创建父子关系
  let parent_context = parent_trace_id + ":" + parent_span_id
  let child_context = parent_trace_id + ":" + child_span_id
  
  // 验证trace ID传播
  assert_eq(child_context.has_prefix(parent_trace_id), true)
  assert_eq(child_context.contains(parent_trace_id), true)
  
  // 验证span ID不同
  assert_eq(child_context.contains(parent_span_id), false)
  assert_eq(child_context.contains(child_span_id), true)
}

test "telemetry_context_baggage" {
  // 测试遥测上下文行李(baggage)传播
  
  let baggage_items = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("tenant.id", "tenant-abc")
  ]
  
  // 创建行李字符串
  let mut baggage_string = ""
  let mut i = 0
  while i < baggage_items.length() {
    if i > 0 {
      baggage_string = baggage_string + ","
    }
    baggage_string = baggage_string + baggage_items[i].0 + "=" + baggage_items[i].1
    i = i + 1
  }
  
  // 验证行李格式
  assert_eq(baggage_string.contains("user.id=12345"), true)
  assert_eq(baggage_string.contains("request.id=req-67890"), true)
  assert_eq(baggage_string.contains("tenant.id=tenant-abc"), true)
  assert_eq(baggage_string.contains(","), true) // 应该有分隔符
  
  // 解析行李项
  let parsed_items = baggage_string.split(",")
  assert_eq(parsed_items.length(), 3)
  
  // 验证每个行李项
  assert_eq(parsed_items[0], "user.id=12345")
  assert_eq(parsed_items[1], "request.id=req-67890")
  assert_eq(parsed_items[2], "tenant.id=tenant-abc")
}

test "telemetry_context_extraction" {
  // 测试遥测上下文提取
  
  let headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("x-trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("x-span-id", "b7ad6b7169203331"),
    ("baggage", "user.id=12345,request.id=req-67890")
  ]
  
  // 提取trace信息
  let mut trace_id = ""
  let mut span_id = ""
  let mut baggage = ""
  
  let mut i = 0
  while i < headers.length() {
    let header_name = headers[i].0
    let header_value = headers[i].1
    
    if header_name == "traceparent" {
      // 解析traceparent格式: 00-trace_id-span_id-flags
      let parts = header_value.split("-")
      if parts.length() >= 3 {
        trace_id = parts[1]
        span_id = parts[2]
      }
    } else if header_name == "x-trace-id" {
      trace_id = header_value
    } else if header_name == "x-span-id" {
      span_id = header_value
    } else if header_name == "baggage" {
      baggage = header_value
    }
    i = i + 1
  }
  
  // 验证提取的trace信息
  assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(span_id, "b7ad6b7169203331")
  assert_eq(baggage, "user.id=12345,request.id=req-67890")
  
  // 创建提取的上下文
  let extracted_context = trace_id + ":" + span_id + ":" + baggage
  assert_eq(extracted_context.has_prefix(trace_id), true)
  assert_eq(extracted_context.contains(span_id), true)
  assert_eq(extracted_context.has_suffix(baggage), true)
}

test "telemetry_context_injection" {
  // 测试遥测上下文注入
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let baggage = "user.id=12345,tenant.id=tenant-abc"
  
  // 创建注入的头部
  let injected_headers = [
    ("traceparent", "00-" + trace_id + "-" + span_id + "-01"),
    ("x-trace-id", trace_id),
    ("x-span-id", span_id),
    ("baggage", baggage)
  ]
  
  // 验证注入的头部
  assert_eq(injected_headers.length(), 4)
  
  // 验证traceparent格式
  assert_eq(injected_headers[0].0, "traceparent")
  assert_eq(injected_headers[0].1.has_prefix("00-"), true)
  assert_eq(injected_headers[0].1.contains("-" + trace_id + "-"), true)
  assert_eq(injected_headers[0].1.contains("-" + span_id + "-"), true)
  assert_eq(injected_headers[0].1.has_suffix("-01"), true)
  
  // 验证其他头部
  assert_eq(injected_headers[1].0, "x-trace-id")
  assert_eq(injected_headers[1].1, trace_id)
  
  assert_eq(injected_headers[2].0, "x-span-id")
  assert_eq(injected_headers[2].1, span_id)
  
  assert_eq(injected_headers[3].0, "baggage")
  assert_eq(injected_headers[3].1, baggage)
}

test "telemetry_context_validation" {
  // 测试遥测上下文验证
  
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let invalid_trace_id_too_short = "0af76519"
  let invalid_trace_id_too_long = "0af7651916cd43dd8448eb211c80319c1234567890"
  
  let valid_span_id = "b7ad6b7169203331"
  let invalid_span_id_too_short = "b7ad6b71"
  let invalid_span_id_too_long = "b7ad6b71692033311234567890"
  
  // 验证trace ID
  assert_eq(valid_trace_id.length(), 32)
  assert_eq(invalid_trace_id_too_short.length(), 8)
  assert_eq(invalid_trace_id_too_long.length(), 42)
  
  // 验证span ID
  assert_eq(valid_span_id.length(), 16)
  assert_eq(invalid_span_id_too_short.length(), 8)
  assert_eq(invalid_span_id_too_long.length(), 22)
  
  // 验证十六进制字符
  let hex_chars = "0123456789abcdef"
  let mut valid_trace_chars = true
  let mut i = 0
  while i < valid_trace_id.length() {
    if !hex_chars.contains(valid_trace_id[i].to_string()) {
      valid_trace_chars = false
    }
    i = i + 1
  }
  
  assert_eq(valid_trace_chars, true)
  
  // 验证上下文格式
  let valid_context = valid_trace_id + ":" + valid_span_id
  assert_eq(valid_context.contains(":"), true)
  assert_eq(valid_context.split(":").length(), 2)
  
  let invalid_context_empty = ""
  let invalid_context_no_separator = valid_trace_id + valid_span_id
  assert_eq(invalid_context_empty.length(), 0)
  assert_eq(invalid_context_no_separator.contains(":"), false)
}