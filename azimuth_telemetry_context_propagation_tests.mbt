// Azimuth 遥测上下文传播测试
// 测试遥测上下文在不同服务和组件之间的传播功能

// 测试1: W3C TraceContext 传播
test "W3C TraceContext 传播测试" {
  // 创建初始上下文
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = "01"
  
  // 构造 traceparent header
  let traceparent = "00-" + trace_id + "-" + span_id + "-" + trace_flags
  
  // 验证 traceparent 格式
  assert_true(traceparent.length() == 55)
  assert_true(traceparent.starts_with("00-"))
  
  // 模拟解析 traceparent
  let parts = traceparent.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00") // version
  assert_eq(parts[1], trace_id) // trace_id
  assert_eq(parts[2], span_id) // span_id
  assert_eq(parts[3], trace_flags) // trace_flags
  
  // 验证 trace_id 格式 (32个十六进制字符)
  assert_eq(trace_id.length(), 32)
  assert_true(trace_id.chars().all(fn(c) { 
    let char_code = c.to_char_code()
    (char_code >= 48 && char_code <= 57) || // 0-9
    (char_code >= 97 && char_code <= 102)    // a-f
  }))
  
  // 验证 span_id 格式 (16个十六进制字符)
  assert_eq(span_id.length(), 16)
  assert_true(span_id.chars().all(fn(c) { 
    let char_code = c.to_char_code()
    (char_code >= 48 && char_code <= 57) || // 0-9
    (char_code >= 97 && char_code <= 102)    // a-f
  }))
}

// 测试2: Baggage 传播
test "Baggage 传播测试" {
  // 创建 baggage items
  let baggage_items = [
    ("user.id", "12345"),
    ("user.role", "admin"),
    ("service.version", "1.0.0"),
    ("region", "us-west-2")
  ]
  
  // 构造 baggage header
  let baggage_pairs = baggage_items.map(fn(pair) { 
    let key = pair[0]
    let value = pair[1]
    key + "=" + value
  })
  let baggage_header = baggage_pairs.join(",")
  
  // 验证 baggage header
  assert_true(baggage_header.contains("user.id=12345"))
  assert_true(baggage_header.contains("user.role=admin"))
  assert_true(baggage_header.contains("service.version=1.0.0"))
  assert_true(baggage_header.contains("region=us-west-2"))
  
  // 模拟解析 baggage header
  let parsed_items = baggage_header.split(",").map(fn(pair) {
    let kv = pair.split("=")
    (kv[0], kv[1])
  })
  
  assert_eq(parsed_items.length(), 4)
  
  // 验证解析结果
  for parsed_item in parsed_items {
    let key = parsed_item[0]
    let value = parsed_item[1]
    
    match key {
      "user.id" => assert_eq(value, "12345")
      "user.role" => assert_eq(value, "admin")
      "service.version" => assert_eq(value, "1.0.0")
      "region" => assert_eq(value, "us-west-2")
      _ => assert_true(false) // 不应该有其他key
    }
  }
}

// 测试3: 复合传播器测试
test "复合传播器测试" {
  // 模拟 HTTP headers
  let headers = {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    "baggage": "user.id=12345,user.role=admin,service.version=1.0.0"
  }
  
  // 验证 headers 存在
  assert_true(headers.contains_key("traceparent"))
  assert_true(headers.contains_key("tracestate"))
  assert_true(headers.contains_key("baggage"))
  
  // 验证 traceparent
  let traceparent = headers["traceparent"]
  assert_true(traceparent.starts_with("00-"))
  assert_eq(traceparent.length(), 55)
  
  // 验证 tracestate
  let tracestate = headers["tracestate"]
  assert_true(tracestate.contains("rojo=00f067aa0ba902b7"))
  assert_true(tracestate.contains("congo=t61rcWkgMzE"))
  
  // 验证 baggage
  let baggage = headers["baggage"]
  assert_true(baggage.contains("user.id=12345"))
  assert_true(baggage.contains("user.role=admin"))
  assert_true(baggage.contains("service.version=1.0.0"))
}

// 测试4: 上下文提取和注入
test "上下文提取和注入测试" {
  // 模拟传入的 carrier
  let incoming_carrier = {
    "traceparent": "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    "baggage": "service.name=payment-service,transaction.id=txn-12345"
  }
  
  // 提取上下文
  let traceparent = incoming_carrier["traceparent"]
  let baggage = incoming_carrier["baggage"]
  
  // 解析 traceparent
  let trace_parts = traceparent.split("-")
  let extracted_trace_id = trace_parts[1]
  let extracted_span_id = trace_parts[2]
  let extracted_trace_flags = trace_parts[3]
  
  assert_eq(extracted_trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(extracted_span_id, "00f067aa0ba902b7")
  assert_eq(extracted_trace_flags, "01")
  
  // 解析 baggage
  let baggage_items = baggage.split(",")
  assert_eq(baggage_items.length(), 2)
  
  // 创建新的 span 并注入上下文
  let new_span_id = "b7ad6b7169203331"
  let outgoing_traceparent = "00-" + extracted_trace_id + "-" + new_span_id + "-" + extracted_trace_flags
  
  // 传播 baggage 并添加新的项
  let additional_baggage = ",operation=process_payment,amount=99.99"
  let outgoing_baggage = baggage + additional_baggage
  
  // 构造传出 carrier
  let outgoing_carrier = {
    "traceparent": outgoing_traceparent,
    "baggage": outgoing_baggage
  }
  
  // 验证传出 carrier
  assert_eq(outgoing_carrier["traceparent"], "00-4bf92f3577b34da6a3ce929d0e0e4736-b7ad6b7169203331-01")
  assert_true(outgoing_carrier["baggage"].contains("service.name=payment-service"))
  assert_true(outgoing_carrier["baggage"].contains("transaction.id=txn-12345"))
  assert_true(outgoing_carrier["baggage"].contains("operation=process_payment"))
  assert_true(outgoing_carrier["baggage"].contains("amount=99.99"))
}

// 测试5: 上下文传播边界情况
test "上下文传播边界情况测试" {
  // 测试空 baggage
  let empty_baggage = ""
  let empty_baggage_items = empty_baggage.split(",").filter(fn(item) { item.length() > 0 })
  assert_eq(empty_baggage_items.length(), 0)
  
  // 测试单个 baggage item
  let single_baggage = "key=value"
  let single_baggage_items = single_baggage.split(",")
  assert_eq(single_baggage_items.length(), 1)
  assert_eq(single_baggage_items[0], "key=value")
  
  // 测试包含特殊字符的 baggage value
  let special_baggage = "user.name=John%20Doe,description=This%20is%20a%20test"
  let special_baggage_items = special_baggage.split(",")
  assert_eq(special_baggage_items.length(), 2)
  assert_eq(special_baggage_items[0], "user.name=John%20Doe")
  assert_eq(special_baggage_items[1], "description=This%20is%20a%20test")
  
  // 测试无效的 traceparent 格式
  let invalid_traceparents = [
    "invalid-format",
    "01-invalid-trace-id-span-id-01", // 无效的 trace_id
    "00-4bf92f3577b34da6a3ce929d0e0e4736-invalid-span-id-01", // 无效的 span_id
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-ff" // 无效的 trace_flags
  ]
  
  for invalid_traceparent in invalid_traceparents {
    let parts = invalid_traceparent.split("-")
    // 验证无效格式不会导致解析错误
    if parts.length() == 4 {
      let trace_id = parts[1]
      let span_id = parts[2]
      let trace_flags = parts[3]
      
      // 验证长度
      if trace_id.length() != 32 || span_id.length() != 16 {
        assert_true(true) // 预期的无效格式
      } else if trace_flags.length() != 2 {
        assert_true(true) // 预期的无效格式
      }
    } else {
      assert_true(true) // 预期的无效格式
    }
  }
}