// 遥测链路追踪上下文提取测试
// 测试从各种格式和场景中提取链路追踪上下文

test "trace_context_extraction_from_headers" {
  // 从HTTP头部提取追踪上下文
  let traceparent_header = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let tracestate_header = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // 解析traceparent格式: version-trace_id-span_id-trace_flags
  let parse_traceparent = fn(header : String) -> (String, String, String)? {
    let parts = header.split("-")
    if parts.length != 4 {
      None
    } else {
      Some((parts[0], parts[1], parts[2]))
    }
  }
  
  let parsed = parse_traceparent(traceparent_header)
  @assertion.assert_some(parsed)?
  
  let (version, trace_id, span_id) = parsed.unwrap()
  @assertion.assert_eq(version, "00")?
  @assertion.assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")?
  @assertion.assert_eq(span_id, "b7ad6b7169203331")?
  
  // 解析tracestate
  let parse_tracestate = fn(header : String) -> Array[(String, String)] {
    let members = header.split(",")
    members.map(fn(member) {
      let key_value = member.split("=")
      if key_value.length == 2 {
        (key_value[0], key_value[1])
      } else {
        ("", "")
      }
    }).filter(fn((key, _)) { key.length > 0 })
  }
  
  let tracestate_pairs = parse_tracestate(tracestate_header)
  @assertion.assert_eq(tracestate_pairs.length, 2)?
  @assertion.assert_eq(tracestate_pairs[0], ("rojo", "00f067aa0ba902b7"))?
  @assertion.assert_eq(tracestate_pairs[1], ("congo", "t61rcWkgMzE"))?
}

test "trace_context_extraction_from_text" {
  // 从文本格式提取追踪上下文
  let log_line = "[INFO] Request processed trace_id=abc123 span_id=def456 user_id=789"
  
  let extract_trace_context = fn(log : String) -> (String, String)? {
    let words = log.split(" ")
    let mut trace_id = ""
    let mut span_id = ""
    
    for word in words {
      if word.starts_with("trace_id=") {
        trace_id = word.substring(9)
      } else if word.starts_with("span_id=") {
        span_id = word.substring(8)
      }
    }
    
    if trace_id.length > 0 && span_id.length > 0 {
      Some((trace_id, span_id))
    } else {
      None
    }
  }
  
  let context = extract_trace_context(log_line)
  @assertion.assert_some(context)?
  
  let (trace_id, span_id) = context.unwrap()
  @assertion.assert_eq(trace_id, "abc123")?
  @assertion.assert_eq(span_id, "def456")?
}

test "trace_context_extraction_binary_format" {
  // 从二进制格式提取追踪上下文
  // 模拟二进制数据: trace_id (16 bytes) + span_id (8 bytes) + trace_flags (1 byte)
  let binary_data = [
    0x0a_byte, 0xf7_byte, 0x65_byte, 0x19_byte,  // trace_id 部分1
    0x16_byte, 0xcd_byte, 0x43_byte, 0xdd_byte,  // trace_id 部分2
    0x84_byte, 0x48_byte, 0xeb_byte, 0x21_byte,  // trace_id 部分3
    0x1c_byte, 0x80_byte, 0x31_byte, 0x9c_byte,  // trace_id 部分4
    0xb7_byte, 0xad_byte, 0x6b_byte, 0x71_byte,  // span_id 部分1
    0x69_byte, 0x20_byte, 0x33_byte, 0x31_byte,  // span_id 部分2
    0x01_byte  // trace_flags
  ]
  
  let extract_from_binary = fn(data : Array[Byte]) -> (String, String, Byte)? {
    if data.length < 25 {
      None
    } else {
      let trace_id_bytes = data.slice_to(16)
      let span_id_bytes = data.slice_from(16, 8)
      let trace_flags = data[24]
      
      let bytes_to_hex = fn(bytes : Array[Byte]) -> String {
        bytes.map(fn(byte) {
          let hex = @int.to_string(byte.to_int(), 16)
          if hex.length == 1 {
            "0" + hex
          } else {
            hex
          }
        }).join("")
      }
      
      Some((bytes_to_hex(trace_id_bytes), bytes_to_hex(span_id_bytes), trace_flags))
    }
  }
  
  let extracted = extract_from_binary(binary_data)
  @assertion.assert_some(extracted)?
  
  let (trace_id, span_id, trace_flags) = extracted.unwrap()
  @assertion.assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")?
  @assertion.assert_eq(span_id, "b7ad6b7169203331")?
  @assertion.assert_eq(trace_flags, 0x01_byte)?
}

test "trace_context_extraction_invalid_formats" {
  // 测试无效格式的处理
  let invalid_headers = [
    "",                                    // 空字符串
    "invalid",                             // 完全无效
    "00-0af7651916cd43dd8448eb211c80319c", // 缺少span_id和flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331", // 缺少flags
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra", // 额外字段
    "not-a-number-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" // 无效版本
  ]
  
  let parse_traceparent = fn(header : String) -> (String, String, String)? {
    let parts = header.split("-")
    if parts.length != 4 {
      None
    } else {
      Some((parts[0], parts[1], parts[2]))
    }
  }
  
  for header in invalid_headers {
    let result = parse_traceparent(header)
    @assertion.assert_none(result)?
  }
}

test "trace_context_extraction_grpc_metadata" {
  // 从gRPC元数据提取追踪上下文
  let grpc_metadata = [
    ("grpc-trace-bin", "AAAAAAE="),  // Base64编码的二进制追踪数据
    ("x-client-id", "12345"),
    ("user-agent", "grpc-go/1.45.0")
  ]
  
  let decode_base64 = fn(encoded : String) -> Array[Byte]? {
    // 简化的Base64解码（仅用于测试）
    if encoded == "AAAAAAE=" {
      [0x00_byte, 0x00_byte, 0x00_byte, 0x01_byte]
    } else {
      None
    }
  }
  
  let extract_grpc_trace = fn(metadata : Array[(String, String)]) -> Array[Byte]? {
    for (key, value) in metadata {
      if key == "grpc-trace-bin" {
        return decode_base64(value)
      }
    }
    None
  }
  
  let trace_data = extract_grpc_trace(grpc_metadata)
  @assertion.assert_some(trace_data)?
  
  let decoded = trace_data.unwrap()
  @assertion.assert_eq(decoded.length, 4)?
  @assertion.assert_eq(decoded[3], 0x01_byte)?
}

test "trace_context_extraction_mixed_sources" {
  // 从混合来源提取追踪上下文
  let request_data = {
    "headers": [
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("x-request-id", "req-123456")
    ],
    "query_params": [
      ("debug", "true"),
      ("trace_id", "fallback-trace-id")
    ],
    "body": {
      "operation": "process_data",
      "correlation_id": "corr-789"
    }
  }
  
  // 优先级：HTTP头部 > 查询参数 > 请求体
  let extract_context_priority = fn(data : { "headers": Array[(String, String)], "query_params": Array[(String, String)], "body": { "correlation_id": String } }) -> String {
    // 首先尝试从头部提取
    for (key, value) in data.headers {
      if key == "traceparent" {
        let parts = value.split("-")
        if parts.length >= 2 {
          return parts[1]  // 返回trace_id
        }
      }
    }
    
    // 然后尝试从查询参数提取
    for (key, value) in data.query_params {
      if key == "trace_id" {
        return value
      }
    }
    
    // 最后使用correlation_id作为fallback
    data.body.correlation_id
  }
  
  let trace_id = extract_context_priority(request_data)
  @assertion.assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")?
}

test "trace_context_extraction_with_validation" {
  // 带验证的上下文提取
  let validate_trace_id = fn(trace_id : String) -> Bool {
    // Trace ID应该是32个十六进制字符
    if trace_id.length != 32 {
      false
    } else {
      trace_id.all_chars(fn(c) {
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
      })
    }
  }
  
  let validate_span_id = fn(span_id : String) -> Bool {
    // Span ID应该是16个十六进制字符
    if span_id.length != 16 {
      false
    } else {
      span_id.all_chars(fn(c) {
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
      })
    }
  }
  
  let test_cases = [
    ("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true),
    ("invalid_trace_id", "b7ad6b7169203331", false),
    ("0af7651916cd43dd8448eb211c80319c", "invalid_span_id", false),
    ("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true),
    ("0AF7651916CD43DD8448EB211C80319C", "B7AD6B7169203331", true)  // 大写也应该有效
  ]
  
  for (trace_id, span_id, expected) in test_cases {
    let trace_valid = validate_trace_id(trace_id)
    let span_valid = validate_span_id(span_id)
    let both_valid = trace_valid && span_valid
    @assertion.assert_eq(both_valid, expected)?
  }
}