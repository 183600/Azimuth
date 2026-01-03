// Azimuth Premium Serialization Tests
// 高质量序列化反序列化完整性测试用例
// 专注于测试遥测系统数据的序列化和反序列化完整性和一致性

// 测试1: AttributeValue序列化反序列化测试
test "AttributeValue序列化反序列化测试" {
  // 1. 测试StringValue序列化反序列化
  let original_string = AttributeValue::StringValue("遥测数据处理系统")
  
  // 模拟序列化过程
  let serialized_string = "StringValue:遥测数据处理系统"
  
  // 模拟反序列化过程
  let deserialized_string = match serialized_string.split(":")[0] {
    "StringValue" => AttributeValue::StringValue(serialized_string.split(":")[1])
    _ => AttributeValue::StringValue("") // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_string {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "遥测数据处理系统")
      assert_eq(s.length(), 6)
    }
    _ => assert_true(false, "StringValue反序列化失败")
  }
  
  // 2. 测试IntValue序列化反序列化
  let original_int = AttributeValue::IntValue(42)
  
  // 模拟序列化过程
  let serialized_int = "IntValue:42"
  
  // 模拟反序列化过程
  let deserialized_int = match serialized_int.split(":")[0] {
    "IntValue" => AttributeValue::IntValue(serialized_int.split(":")[1].to_int())
    _ => AttributeValue::IntValue(0) // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_int {
    AttributeValue::IntValue(i) => {
      assert_eq(i, 42)
    }
    _ => assert_true(false, "IntValue反序列化失败")
  }
  
  // 3. 测试FloatValue序列化反序列化
  let original_float = AttributeValue::FloatValue(3.14159)
  
  // 模拟序列化过程
  let serialized_float = "FloatValue:3.14159"
  
  // 模拟反序列化过程
  let deserialized_float = match serialized_float.split(":")[0] {
    "FloatValue" => AttributeValue::FloatValue(serialized_float.split(":")[1].to_double())
    _ => AttributeValue::FloatValue(0.0) // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_float {
    AttributeValue::FloatValue(f) => {
      assert_true(f > 3.14 && f < 3.15)
    }
    _ => assert_true(false, "FloatValue反序列化失败")
  }
  
  // 4. 测试BoolValue序列化反序列化
  let original_bool = AttributeValue::BoolValue(true)
  
  // 模拟序列化过程
  let serialized_bool = "BoolValue:true"
  
  // 模拟反序列化过程
  let deserialized_bool = match serialized_bool.split(":")[0] {
    "BoolValue" => AttributeValue::BoolValue(serialized_bool.split(":")[1] == "true")
    _ => AttributeValue::BoolValue(false) // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_bool {
    AttributeValue::BoolValue(b) => {
      assert_true(b)
    }
    _ => assert_true(false, "BoolValue反序列化失败")
  }
  
  // 5. 测试ArrayStringValue序列化反序列化
  let original_string_array = AttributeValue::ArrayStringValue(["item1", "item2", "item3"])
  
  // 模拟序列化过程
  let serialized_string_array = "ArrayStringValue:item1,item2,item3"
  
  // 模拟反序列化过程
  let deserialized_string_array = match serialized_string_array.split(":")[0] {
    "ArrayStringValue" => {
      let items = serialized_string_array.split(":")[1].split(",")
      AttributeValue::ArrayStringValue(items)
    }
    _ => AttributeValue::ArrayStringValue([]) // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_string_array {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_true(false, "ArrayStringValue反序列化失败")
  }
  
  // 6. 测试ArrayIntValue序列化反序列化
  let original_int_array = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  
  // 模拟序列化过程
  let serialized_int_array = "ArrayIntValue:1,2,3,4,5"
  
  // 模拟反序列化过程
  let deserialized_int_array = match serialized_int_array.split(":")[0] {
    "ArrayIntValue" => {
      let items_str = serialized_int_array.split(":")[1]
      let items_str_list = items_str.split(",")
      let mut items = []
      for item_str in items_str_list {
        items = items.push(item_str.to_int())
      }
      AttributeValue::ArrayIntValue(items)
    }
    _ => AttributeValue::ArrayIntValue([]) // 默认值
  }
  
  // 验证序列化反序列化一致性
  match deserialized_int_array {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false, "ArrayIntValue反序列化失败")
  }
}

// 测试2: SpanContext序列化反序列化测试
test "SpanContext序列化反序列化测试" {
  // 1. 测试基本SpanContext序列化反序列化
  let original_context = SpanContext({
    trace_id: "12345678901234567890123456789012",
    span_id: "1234567890123456",
    sampled: true,
    trace_state: "key=value1,key2=value2"
  })
  
  // 模拟序列化过程
  let serialized_context = original_context.trace_id + "|" + 
                          original_context.span_id + "|" + 
                          (if original_context.sampled { "true" } else { "false" }) + "|" + 
                          original_context.trace_state
  
  // 模拟反序列化过程
  let parts = serialized_context.split("|")
  let deserialized_context = SpanContext({
    trace_id: parts[0],
    span_id: parts[1],
    sampled: parts[2] == "true",
    trace_state: parts[3]
  })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_context.trace_id, original_context.trace_id)
  assert_eq(deserialized_context.span_id, original_context.span_id)
  assert_eq(deserialized_context.sampled, original_context.sampled)
  assert_eq(deserialized_context.trace_state, original_context.trace_state)
  
  // 2. 测试未采样的SpanContext序列化反序列化
  let unsampled_context = SpanContext({
    trace_id: "abcdef0123456789abcdef0123456789",
    span_id: "abcdef0123456789",
    sampled: false,
    trace_state: ""
  })
  
  // 模拟序列化过程
  let serialized_unsampled = unsampled_context.trace_id + "|" + 
                            unsampled_context.span_id + "|" + 
                            (if unsampled_context.sampled { "true" } else { "false" }) + "|" + 
                            unsampled_context.trace_state
  
  // 模拟反序列化过程
  let unsampled_parts = serialized_unsampled.split("|")
  let deserialized_unsampled = SpanContext({
    trace_id: unsampled_parts[0],
    span_id: unsampled_parts[1],
    sampled: unsampled_parts[2] == "true",
    trace_state: unsampled_parts[3]
  })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_unsampled.trace_id, unsampled_context.trace_id)
  assert_eq(deserialized_unsampled.span_id, unsampled_context.span_id)
  assert_eq(deserialized_unsampled.sampled, unsampled_context.sampled)
  assert_eq(deserialized_unsampled.trace_state, unsampled_context.trace_state)
  
  // 3. 测试包含特殊字符的trace_state序列化反序列化
  let special_context = SpanContext({
    trace_id: "11111111111111111111111111111111",
    span_id: "1111111111111111",
    sampled: true,
    trace_state: "key=value!@#$%,key2=中文测试,key3=🌟"
  })
  
  // 模拟序列化过程
  let serialized_special = special_context.trace_id + "|" + 
                           special_context.span_id + "|" + 
                           (if special_context.sampled { "true" } else { "false" }) + "|" + 
                           special_context.trace_state
  
  // 模拟反序列化过程
  let special_parts = serialized_special.split("|")
  let deserialized_special = SpanContext({
    trace_id: special_parts[0],
    span_id: special_parts[1],
    sampled: special_parts[2] == "true",
    trace_state: special_parts[3]
  })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_special.trace_id, special_context.trace_id)
  assert_eq(deserialized_special.span_id, special_context.span_id)
  assert_eq(deserialized_special.sampled, special_context.sampled)
  assert_eq(deserialized_special.trace_state, special_context.trace_state)
  assert_true(deserialized_special.trace_state.contains("中文测试"))
  assert_true(deserialized_special.trace_state.contains("🌟"))
}

// 测试3: Baggage序列化反序列化测试
test "Baggage序列化反序列化测试" {
  // 1. 测试基本Baggage序列化反序列化
  let original_baggage = Baggage({
    entries: [
      ("user.id", "user123"),
      ("request.id", "req456"),
      ("session.id", "sess789")
    ]
  })
  
  // 模拟序列化过程
  let mut serialized_pairs = []
  for (key, value) in original_baggage.entries {
    serialized_pairs = serialized_pairs.push(key + "=" + value)
  }
  let serialized_baggage = serialized_pairs.join(",")
  
  // 模拟反序列化过程
  let pairs = serialized_baggage.split(",")
  let mut deserialized_entries = []
  for pair in pairs {
    let kv = pair.split("=")
    if kv.length() >= 2 {
      deserialized_entries = deserialized_entries.push((kv[0], kv[1]))
    }
  }
  let deserialized_baggage = Baggage({ entries: deserialized_entries })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_baggage.entries.length(), original_baggage.entries.length())
  
  for (key, value) in original_baggage.entries {
    let mut found = false
    for (d_key, d_value) in deserialized_baggage.entries {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "键值对 " + key + "=" + value + " 应该在反序列化结果中找到")
  }
  
  // 2. 测试包含特殊字符的Baggage序列化反序列化
  let special_baggage = Baggage({
    entries: [
      ("special.key!@#$%", "special.value!@#$%"),
      ("unicode.key.中文", "unicode.value.🌟"),
      ("empty.value", ""),
      ("equals.key=value", "value=with=equals")
    ]
  })
  
  // 模拟序列化过程（需要转义特殊字符）
  let mut special_serialized_pairs = []
  for (key, value) in special_baggage.entries {
    // 简化的转义处理
    let escaped_key = key.replace("=", "\\=")
    let escaped_value = value.replace("=", "\\=")
    special_serialized_pairs = special_serialized_pairs.push(escaped_key + "=" + escaped_value)
  }
  let serialized_special_baggage = special_serialized_pairs.join(",")
  
  // 模拟反序列化过程（需要反转义）
  let special_pairs = serialized_special_baggage.split(",")
  let mut special_deserialized_entries = []
  for pair in special_pairs {
    let kv = pair.split("=")
    if kv.length() >= 2 {
      // 简化的反转义处理
      let unescaped_key = kv[0].replace("\\=", "=")
      let unescaped_value = kv[1].replace("\\=", "=")
      special_deserialized_entries = special_deserialized_entries.push((unescaped_key, unescaped_value))
    }
  }
  let deserialized_special_baggage = Baggage({ entries: special_deserialized_entries })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_special_baggage.entries.length(), special_baggage.entries.length())
  
  for (key, value) in special_baggage.entries {
    let mut found = false
    for (d_key, d_value) in deserialized_special_baggage.entries {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "特殊键值对 " + key + "=" + value + " 应该在反序列化结果中找到")
  }
  
  // 3. 测试空Baggage序列化反序列化
  let empty_baggage = Baggage({ entries: [] })
  
  // 模拟序列化过程
  let serialized_empty_baggage = ""
  
  // 模拟反序列化过程
  let deserialized_empty_baggage = if serialized_empty_baggage == "" {
    Baggage({ entries: [] })
  } else {
    let pairs = serialized_empty_baggage.split(",")
    let mut entries = []
    for pair in pairs {
      let kv = pair.split("=")
      if kv.length() >= 2 {
        entries = entries.push((kv[0], kv[1]))
      }
    }
    Baggage({ entries: entries })
  }
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_empty_baggage.entries.length(), 0)
}

// 测试4: TextMapCarrier序列化反序列化测试
test "TextMapCarrier序列化反序列化测试" {
  // 1. 测试基本TextMapCarrier序列化反序列化
  let original_carrier = TextMapCarrier({
    headers: [
      ("traceparent", "00-12345678901234567890123456789012-1234567890123456-01"),
      ("tracestate", "key=value1,key2=value2"),
      ("baggage", "user.id=user123,request.id=req456"),
      ("x-request-id", "req-123456"),
      ("content-type", "application/json")
    ]
  })
  
  // 模拟序列化过程
  let mut serialized_headers = []
  for (key, value) in original_carrier.headers {
    serialized_headers = serialized_headers.push(key + ":" + value)
  }
  let serialized_carrier = serialized_headers.join("\n")
  
  // 模拟反序列化过程
  let header_lines = serialized_carrier.split("\n")
  let mut deserialized_headers = []
  for line in header_lines {
    let kv = line.split(":")
    if kv.length() >= 2 {
      deserialized_headers = deserialized_headers.push((kv[0], kv[1]))
    }
  }
  let deserialized_carrier = TextMapCarrier({ headers: deserialized_headers })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_carrier.headers.length(), original_carrier.headers.length())
  
  for (key, value) in original_carrier.headers {
    let mut found = false
    for (d_key, d_value) in deserialized_carrier.headers {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "Header " + key + ":" + value + " 应该在反序列化结果中找到")
  }
  
  // 2. 测试包含特殊字符的TextMapCarrier序列化反序列化
  let special_carrier = TextMapCarrier({
    headers: [
      ("special.header!@#$%", "special.value!@#$%"),
      ("unicode.header.中文", "unicode.value.🌟"),
      ("empty.value", ""),
      ("colon.header:", "value:with:colons"),
      ("newline.header\n", "value\nwith\nnewlines")
    ]
  })
  
  // 模拟序列化过程（需要转义特殊字符）
  let mut special_serialized_headers = []
  for (key, value) in special_carrier.headers {
    // 简化的转义处理
    let escaped_key = key.replace(":", "\\:").replace("\n", "\\n")
    let escaped_value = value.replace(":", "\\:").replace("\n", "\\n")
    special_serialized_headers = special_serialized_headers.push(escaped_key + ":" + escaped_value)
  }
  let serialized_special_carrier = special_serialized_headers.join("\n")
  
  // 模拟反序列化过程（需要反转义）
  let special_header_lines = serialized_special_carrier.split("\n")
  let mut special_deserialized_headers = []
  for line in special_header_lines {
    let kv = line.split(":")
    if kv.length() >= 2 {
      // 简化的反转义处理
      let unescaped_key = kv[0].replace("\\:", ":").replace("\\n", "\n")
      let unescaped_value = kv[1].replace("\\:", ":").replace("\\n", "\n")
      special_deserialized_headers = special_deserialized_headers.push((unescaped_key, unescaped_value))
    }
  }
  let deserialized_special_carrier = TextMapCarrier({ headers: special_deserialized_headers })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_special_carrier.headers.length(), special_carrier.headers.length())
  
  for (key, value) in special_carrier.headers {
    let mut found = false
    for (d_key, d_value) in deserialized_special_carrier.headers {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "特殊Header " + key + ":" + value + " 应该在反序列化结果中找到")
  }
  
  // 3. 测试空TextMapCarrier序列化反序列化
  let empty_carrier = TextMapCarrier({ headers: [] })
  
  // 模拟序列化过程
  let serialized_empty_carrier = ""
  
  // 模拟反序列化过程
  let deserialized_empty_carrier = if serialized_empty_carrier == "" {
    TextMapCarrier({ headers: [] })
  } else {
    let header_lines = serialized_empty_carrier.split("\n")
    let mut headers = []
    for line in header_lines {
      let kv = line.split(":")
      if kv.length() >= 2 {
        headers = headers.push((kv[0], kv[1]))
      }
    }
    TextMapCarrier({ headers: headers })
  }
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_empty_carrier.headers.length(), 0)
}

// 测试5: InstrumentationScope序列化反序列化测试
test "InstrumentationScope序列化反序列化测试" {
  // 1. 测试基本InstrumentationScope序列化反序列化
  let original_scope = InstrumentationScope({
    name: "azimuth.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 模拟序列化过程
  let version_str = match original_scope.version {
    Some(v) => v
    None => ""
  }
  let schema_url_str = match original_scope.schema_url {
    Some(url) => url
    None => ""
  }
  let serialized_scope = original_scope.name + "|" + version_str + "|" + schema_url_str
  
  // 模拟反序列化过程
  let scope_parts = serialized_scope.split("|")
  let deserialized_version = if scope_parts[1] == "" { None } else { Some(scope_parts[1]) }
  let deserialized_schema_url = if scope_parts[2] == "" { None } else { Some(scope_parts[2]) }
  let deserialized_scope = InstrumentationScope({
    name: scope_parts[0],
    version: deserialized_version,
    schema_url: deserialized_schema_url
  })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_scope.name, original_scope.name)
  
  match deserialized_scope.version {
    Some(v) => {
      match original_scope.version {
        Some(ov) => assert_eq(v, ov)
        None => assert_true(false, "反序列化版本不应为Some")
      }
    }
    None => {
      match original_scope.version {
        Some(_) => assert_true(false, "反序列化版本不应为None")
        None => assert_true(true)
      }
    }
  }
  
  match deserialized_scope.schema_url {
    Some(url) => {
      match original_scope.schema_url {
        Some(ourl) => assert_eq(url, ourl)
        None => assert_true(false, "反序列化schema_url不应为Some")
      }
    }
    None => {
      match original_scope.schema_url {
        Some(_) => assert_true(false, "反序列化schema_url不应为None")
        None => assert_true(true)
      }
    }
  }
  
  // 2. 测试部分字段的InstrumentationScope序列化反序列化
  let partial_scope = InstrumentationScope({
    name: "partial.scope",
    version: Some("2.1.0"),
    schema_url: None
  })
  
  // 模拟序列化过程
  let partial_version_str = match partial_scope.version {
    Some(v) => v
    None => ""
  }
  let partial_schema_url_str = match partial_scope.schema_url {
    Some(url) => url
    None => ""
  }
  let serialized_partial_scope = partial_scope.name + "|" + partial_version_str + "|" + partial_schema_url_str
  
  // 模拟反序列化过程
  let partial_scope_parts = serialized_partial_scope.split("|")
  let deserialized_partial_version = if partial_scope_parts[1] == "" { None } else { Some(partial_scope_parts[1]) }
  let deserialized_partial_schema_url = if partial_scope_parts[2] == "" { None } else { Some(partial_scope_parts[2]) }
  let deserialized_partial_scope = InstrumentationScope({
    name: partial_scope_parts[0],
    version: deserialized_partial_version,
    schema_url: deserialized_partial_schema_url
  })
  
  // 验证序列化反序列化一致性
  assert_eq(deserialized_partial_scope.name, partial_scope.name)
  match deserialized_partial_scope.version {
    Some(v) => assert_eq(v, "2.1.0")
    None => assert_true(false, "版本不应为None")
  }
  match deserialized_partial_scope.schema_url {
    None => assert_true(true)
    Some(_) => assert_true(false, "Schema URL应为None")
  }
}

// 测试6: 复合数据结构序列化反序列化测试
test "复合数据结构序列化反序列化测试" {
  // 1. 创建复合数据结构
  let original_context = SpanContext({
    trace_id: "12345678901234567890123456789012",
    span_id: "1234567890123456",
    sampled: true,
    trace_state: "service=test,operation=process"
  })
  
  let original_baggage = Baggage({
    entries: [
      ("user.id", "user123"),
      ("request.id", "req456"),
      ("operation.name", "data_processing")
    ]
  })
  
  let original_carrier = TextMapCarrier({
    headers: [
      ("traceparent", "00-12345678901234567890123456789012-1234567890123456-01"),
      ("tracestate", "service=test,operation=process"),
      ("baggage", "user.id=user123,request.id=req456,operation.name=data_processing"),
      ("x-request-id", "req-456"),
      ("content-type", "application/json")
    ]
  })
  
  let original_scope = InstrumentationScope({
    name: "test.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 2. 模拟复合序列化过程
  // 序列化SpanContext
  let serialized_context = original_context.trace_id + "|" + 
                          original_context.span_id + "|" + 
                          (if original_context.sampled { "true" } else { "false" }) + "|" + 
                          original_context.trace_state
  
  // 序列化Baggage
  let mut baggage_pairs = []
  for (key, value) in original_baggage.entries {
    baggage_pairs = baggage_pairs.push(key + "=" + value)
  }
  let serialized_baggage = baggage_pairs.join(",")
  
  // 序列化TextMapCarrier
  let mut carrier_headers = []
  for (key, value) in original_carrier.headers {
    carrier_headers = carrier_headers.push(key + ":" + value)
  }
  let serialized_carrier = carrier_headers.join("\n")
  
  // 序列化InstrumentationScope
  let scope_version = match original_scope.version {
    Some(v) => v
    None => ""
  }
  let scope_schema_url = match original_scope.schema_url {
    Some(url) => url
    None => ""
  }
  let serialized_scope = original_scope.name + "|" + scope_version + "|" + scope_schema_url
  
  // 组合所有序列化结果
  let serialized_complex = "SpanContext:" + serialized_context + "\n" + 
                          "Baggage:" + serialized_baggage + "\n" + 
                          "TextMapCarrier:" + serialized_carrier + "\n" + 
                          "InstrumentationScope:" + serialized_scope
  
  // 3. 模拟复合反序列化过程
  let complex_sections = serialized_complex.split("\n")
  
  // 反序列化SpanContext
  let context_section = complex_sections[0]
  let context_data = context_section.split(":")[1]
  let context_parts = context_data.split("|")
  let deserialized_context = SpanContext({
    trace_id: context_parts[0],
    span_id: context_parts[1],
    sampled: context_parts[2] == "true",
    trace_state: context_parts[3]
  })
  
  // 反序列化Baggage
  let baggage_section = complex_sections[1]
  let baggage_data = baggage_section.split(":")[1]
  let baggage_pairs_deser = baggage_data.split(",")
  let mut baggage_entries = []
  for pair in baggage_pairs_deser {
    let kv = pair.split("=")
    if kv.length() >= 2 {
      baggage_entries = baggage_entries.push((kv[0], kv[1]))
    }
  }
  let deserialized_baggage = Baggage({ entries: baggage_entries })
  
  // 反序列化TextMapCarrier
  let carrier_section = complex_sections[2]
  let carrier_data = carrier_section.split(":")[1]
  let carrier_lines = carrier_data.split("\n")
  let mut carrier_headers = []
  for line in carrier_lines {
    let kv = line.split(":")
    if kv.length() >= 2 {
      carrier_headers = carrier_headers.push((kv[0], kv[1]))
    }
  }
  let deserialized_carrier = TextMapCarrier({ headers: carrier_headers })
  
  // 反序列化InstrumentationScope
  let scope_section = complex_sections[3]
  let scope_data = scope_section.split(":")[1]
  let scope_parts = scope_data.split("|")
  let deserialized_scope_version = if scope_parts[1] == "" { None } else { Some(scope_parts[1]) }
  let deserialized_scope_schema_url = if scope_parts[2] == "" { None } else { Some(scope_parts[2]) }
  let deserialized_scope = InstrumentationScope({
    name: scope_parts[0],
    version: deserialized_scope_version,
    schema_url: deserialized_scope_schema_url
  })
  
  // 4. 验证复合序列化反序列化一致性
  
  // 验证SpanContext
  assert_eq(deserialized_context.trace_id, original_context.trace_id)
  assert_eq(deserialized_context.span_id, original_context.span_id)
  assert_eq(deserialized_context.sampled, original_context.sampled)
  assert_eq(deserialized_context.trace_state, original_context.trace_state)
  
  // 验证Baggage
  assert_eq(deserialized_baggage.entries.length(), original_baggage.entries.length())
  for (key, value) in original_baggage.entries {
    let mut found = false
    for (d_key, d_value) in deserialized_baggage.entries {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "Baggage键值对 " + key + "=" + value + " 应该在反序列化结果中找到")
  }
  
  // 验证TextMapCarrier
  assert_eq(deserialized_carrier.headers.length(), original_carrier.headers.length())
  for (key, value) in original_carrier.headers {
    let mut found = false
    for (d_key, d_value) in deserialized_carrier.headers {
      if d_key == key && d_value == value {
        found = true
        break
      }
    }
    assert_true(found, "Carrier header " + key + ":" + value + " 应该在反序列化结果中找到")
  }
  
  // 验证InstrumentationScope
  assert_eq(deserialized_scope.name, original_scope.name)
  match deserialized_scope.version {
    Some(v) => {
      match original_scope.version {
        Some(ov) => assert_eq(v, ov)
        None => assert_true(false, "反序列化版本不应为Some")
      }
    }
    None => {
      match original_scope.version {
        Some(_) => assert_true(false, "反序列化版本不应为None")
        None => assert_true(true)
      }
    }
  }
  match deserialized_scope.schema_url {
    Some(url) => {
      match original_scope.schema_url {
        Some(ourl) => assert_eq(url, ourl)
        None => assert_true(false, "反序列化schema_url不应为Some")
      }
    }
    None => {
      match original_scope.schema_url {
        Some(_) => assert_true(false, "反序列化schema_url不应为None")
        None => assert_true(true)
      }
    }
  }
}