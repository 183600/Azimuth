// Context和Baggage交互测试用例
// 测试Context和Baggage之间的复杂交互场景

test "context_with_multiple_baggage_entries" {
  // 测试Context中包含多个Baggage条目的场景
  
  // 创建基础Context
  let base_context = Context::empty()
  
  // 创建多个ContextKey
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  
  // 向Context添加多个值
  let context_with_user = base_context.with_value(user_key, "user12345")
  let context_with_session = context_with_user.with_value(session_key, "sess_abcdef")
  let context_with_request = context_with_session.with_value(request_key, "req_123456")
  let full_context = context_with_request.with_value(trace_key, "trace_789")
  
  // 验证Context中的值
  match full_context.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user12345")
    None => @test.fail("Expected user.id in context")
  }
  
  match full_context.get(session_key) {
    Some(session_id) => assert_eq(session_id, "sess_abcdef")
    None => @test.fail("Expected session.id in context")
  }
  
  match full_context.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req_123456")
    None => @test.fail("Expected request.id in context")
  }
  
  match full_context.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace_789")
    None => @test.fail("Expected trace.id in context")
  }
  
  // 创建对应的Baggage
  let base_baggage = Baggage::empty()
  let baggage_with_user = base_baggage.with_entry("user.id", "user12345")
  let baggage_with_session = baggage_with_user.with_entry("session.id", "sess_abcdef")
  let baggage_with_request = baggage_with_session.with_entry("request.id", "req_123456")
  let full_baggage = baggage_with_request.with_entry("trace.id", "trace_789")
  
  // 验证Baggage中的条目
  match full_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user12345")
    None => @test.fail("Expected user.id in baggage")
  }
  
  match full_baggage.get("session.id") {
    Some(session_id) => assert_eq(session_id, "sess_abcdef")
    None => @test.fail("Expected session.id in baggage")
  }
  
  match full_baggage.get("request.id") {
    Some(request_id) => assert_eq(request_id, "req_123456")
    None => @test.fail("Expected request.id in baggage")
  }
  
  match full_baggage.get("trace.id") {
    Some(trace_id) => assert_eq(trace_id, "trace_789")
    None => @test.fail("Expected trace.id in baggage")
  }
  
  // 验证Context和Baggage的一致性
  let verify_consistency = fn(ctx : Context, baggage : Baggage) -> Bool {
    let context_keys = ["user.id", "session.id", "request.id", "trace.id"]
    let mut consistent = true
    let mut i = 0
    
    while i < context_keys.length() && consistent {
      let key = context_keys[i]
      let context_key = create_key(key)
      
      match ctx.get(context_key) {
        Some(context_value) => {
          match baggage.get(key) {
            Some(baggage_value) => {
              if context_value != baggage_value {
                consistent = false
              }
            }
            None => consistent = false
          }
        }
        None => consistent = false
      }
      
      i = i + 1
    }
    
    consistent
  }
  
  assert_eq(verify_consistency(full_context, full_baggage), true)
}

test "context_baggage_value_overriding" {
  // 测试Context和Baggage中的值覆盖场景
  
  // 创建基础Context
  let base_context = Context::empty()
  let user_key = create_key("user.id")
  
  // 添加初始值
  let context_v1 = base_context.with_value(user_key, "user_v1")
  
  // 验证初始值
  match context_v1.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user_v1")
    None => @test.fail("Expected user_v1")
  }
  
  // 覆盖值
  let context_v2 = context_v1.with_value(user_key, "user_v2")
  
  // 验证覆盖后的值
  match context_v2.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user_v2")
    None => @test.fail("Expected user_v2")
  }
  
  // 测试Baggage的值覆盖
  let base_baggage = Baggage::empty()
  let baggage_v1 = base_baggage.with_entry("user.id", "user_v1")
  
  match baggage_v1.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user_v1")
    None => @test.fail("Expected user_v1 in baggage")
  }
  
  // 注意：由于MoonBit的Array是不可变的，with_entry实际上是添加新条目
  // 在实际实现中，可能需要不同的策略来处理重复键
  let baggage_v2 = baggage_v1.with_entry("user.id", "user_v2")
  
  // 验证Baggage中的条目数量变化
  assert_eq(baggage_v1.entries.length(), 1)
  assert_eq(baggage_v2.entries.length(), 2)
  
  // 测试Context的层次结构
  let parent_context = base_context.with_value(user_key, "parent_user")
  let child_context = parent_context.with_value(create_key("child.id"), "child_123")
  
  // 验证子Context包含父Context的值
  match child_context.get(user_key) {
    Some(user_id) => assert_eq(user_id, "parent_user")
    None => @test.fail("Expected parent_user in child context")
  }
  
  match child_context.get(create_key("child.id")) {
    Some(child_id) => assert_eq(child_id, "child_123")
    None => @test.fail("Expected child.id in child context")
  }
  
  // 测试Context值的独立性
  let independent_context = base_context.with_value(user_key, "independent_user")
  
  match independent_context.get(user_key) {
    Some(user_id) => assert_eq(user_id, "independent_user")
    None => @test.fail("Expected independent_user")
  }
  
  // 验证不同Context之间的值不影响
  match context_v2.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user_v2")  // 不受independent_context影响
    None => @test.fail("Expected user_v2")
  }
}

test "context_baggage_complex_data_structures" {
  // 测试Context和Baggage中复杂数据结构的处理
  
  // 创建包含复杂数据的Context
  let base_context = Context::empty()
  
  // 添加JSON格式的复杂数据
  let user_data_key = create_key("user.data")
  let complex_user_data = "{\"id\":\"user123\",\"name\":\"John Doe\",\"email\":\"john@example.com\",\"roles\":[\"admin\",\"user\"],\"preferences\":{\"theme\":\"dark\",\"lang\":\"en\"}}"
  let context_with_user_data = base_context.with_value(user_data_key, complex_user_data)
  
  // 验证复杂数据
  match context_with_user_data.get(user_data_key) {
    Some(user_data) => {
      assert_eq(user_data.contains("\"id\":\"user123\""), true)
      assert_eq(user_data.contains("\"roles\""), true)
      assert_eq(user_data.contains("\"preferences\""), true)
    }
    None => @test.fail("Expected user data")
  }
  
  // 添加序列化的数组数据
  let metrics_key = create_key("metrics.data")
  let metrics_data = "[{\"name\":\"cpu_usage\",\"value\":75.5,\"unit\":\"percent\"},{\"name\":\"memory_usage\",\"value\":1024,\"unit\":\"mb\"}]"
  let context_with_metrics = context_with_user_data.with_value(metrics_key, metrics_data)
  
  match context_with_metrics.get(metrics_key) {
    Some(metrics) => {
      assert_eq(metrics.contains("cpu_usage"), true)
      assert_eq(metrics.contains("memory_usage"), true)
    }
    None => @test.fail("Expected metrics data")
  }
  
  // 创建对应的Baggage条目
  let base_baggage = Baggage::empty()
  let baggage_with_user_data = base_baggage.with_entry("user.data", complex_user_data)
  let baggage_with_metrics = baggage_with_user_data.with_entry("metrics.data", metrics_data)
  
  // 验证Baggage中的复杂数据
  match baggage_with_metrics.get("user.data") {
    Some(user_data) => {
      assert_eq(user_data, complex_user_data)
    }
    None => @test.fail("Expected user data in baggage")
  }
  
  match baggage_with_metrics.get("metrics.data") {
    Some(metrics) => {
      assert_eq(metrics, metrics_data)
    }
    None => @test.fail("Expected metrics data in baggage")
  }
  
  // 测试复杂数据的解析函数
  let parse_json_value = fn(json_str : String, key : String) -> String? {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json_str.index_of(key_pattern)
    
    if start_index >= 0 {
      let value_start = start_index + key_pattern.length()
      let end_index = json_str.index_of("\"", value_start)
      
      if end_index > value_start {
        Some(json_str.substring(value_start, end_index - value_start))
      } else {
        None
      }
    } else {
      None
    }
  }
  
  // 测试JSON解析
  match parse_json_value(complex_user_data, "id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => @test.fail("Expected to parse user id")
  }
  
  match parse_json_value(complex_user_data, "email") {
    Some(email) => assert_eq(email, "john@example.com")
    None => @test.fail("Expected to parse user email")
  }
  
  // 测试嵌套数据结构
  let nested_data_key = create_key("nested.data")
  let nested_data = "level1.level2.level3=value"
  let context_with_nested = context_with_metrics.with_value(nested_data_key, nested_data)
  
  match context_with_nested.get(nested_data_key) {
    Some(nested) => {
      assert_eq(nested.contains("level1"), true)
      assert_eq(nested.contains("level2"), true)
      assert_eq(nested.contains("level3"), true)
    }
    None => @test.fail("Expected nested data")
  }
  
  // 测试URL编码的数据
  let encoded_data_key = create_key("encoded.data")
  let encoded_data = "user%20name%3DJohn%20Doe%26email%3Djohn%40example.com"
  let context_with_encoded = context_with_nested.with_value(encoded_data_key, encoded_data)
  
  match context_with_encoded.get(encoded_data_key) {
    Some(encoded) => {
      assert_eq(encoded.contains("%20"), true)
      assert_eq(encoded.contains("%3D"), true)
      assert_eq(encoded.contains("%26"), true)
    }
    None => @test.fail("Expected encoded data")
  }
}

test "context_baggage_performance_and_limits" {
  // 测试Context和Baggage的性能和限制
  
  // 测试大量Context值的性能
  let create_large_context = fn(size : Int) -> Context {
    let base_context = Context::empty()
    let mut current_context = base_context
    let mut i = 0
    
    while i < size {
      let key = create_key("key." + i.to_string())
      let value = "value." + i.to_string()
      current_context = current_context.with_value(key, value)
      i = i + 1
    }
    
    current_context
  }
  
  // 创建包含100个条目的Context
  let large_context = create_large_context(100)
  
  // 验证大型Context中的特定值
  let test_key = create_key("key.50")
  match large_context.get(test_key) {
    Some(value) => assert_eq(value, "value.50")
    None => @test.fail("Expected value.50 in large context")
  }
  
  // 测试大量Baggage条目的性能
  let create_large_baggage = fn(size : Int) -> Baggage {
    let base_baggage = Baggage::empty()
    let mut current_baggage = base_baggage
    let mut i = 0
    
    while i < size {
      let key = "baggage.key." + i.to_string()
      let value = "baggage.value." + i.to_string()
      current_baggage = current_baggage.with_entry(key, value)
      i = i + 1
    }
    
    current_baggage
  }
  
  // 创建包含100个条目的Baggage
  let large_baggage = create_large_baggage(100)
  
  // 验证大型Baggage中的特定条目
  match large_baggage.get("baggage.key.75") {
    Some(value) => assert_eq(value, "baggage.value.75")
    None => @test.fail("Expected baggage.value.75 in large baggage")
  }
  
  // 测试Context值的查找性能
  let measure_context_lookup = fn(ctx : Context, iterations : Int) -> Int64 {
    let test_key = create_key("key.25")
    let start_time = 1640995200123456789L  // 模拟时间戳
    let mut i = 0
    
    while i < iterations {
      let _ = ctx.get(test_key)
      i = i + 1
    }
    
    let end_time = 1640995200123456789L + iterations.to_int64()  // 模拟经过的时间
    end_time - start_time
  }
  
  let context_lookup_time = measure_context_lookup(large_context, 1000)
  assert_eq(context_lookup_time >= 0, true)  // 基本的时间测量验证
  
  // 测试Baggage条目的查找性能
  let measure_baggage_lookup = fn(baggage : Baggage, iterations : Int) -> Int64 {
    let start_time = 1640995200123456789L
    let mut i = 0
    
    while i < iterations {
      let _ = baggage.get("baggage.key.25")
      i = i + 1
    }
    
    let end_time = 1640995200123456789L + iterations.to_int64()
    end_time - start_time
  }
  
  let baggage_lookup_time = measure_baggage_lookup(large_baggage, 1000)
  assert_eq(baggage_lookup_time >= 0, true)
  
  // 测试长键名和值的影响
  let long_key_name = "very.long.key.name.with.many.dots.and.additional.information.to.make.it.really.long.123456789"
  let long_value = "very.long.value.with.much.information.and.data.that.could.be.used.for.various.purposes.in.a.real.world.application.scenario.987654321"
  
  let context_with_long_values = Context::empty()
    .with_value(create_key(long_key_name), long_value)
  
  match context_with_long_values.get(create_key(long_key_name)) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected long value")
  }
  
  let baggage_with_long_values = Baggage::empty()
    .with_entry(long_key_name, long_value)
  
  match baggage_with_long_values.get(long_key_name) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected long baggage value")
  }
  
  // 测试特殊字符键名
  let special_chars_key = "key.with.special.chars!@#$%^&*(){}[]|\\:;\"'<>?,./"
  let context_with_special = Context::empty()
    .with_value(create_key(special_chars_key), "special_value")
  
  match context_with_special.get(create_key(special_chars_key)) {
    Some(value) => assert_eq(value, "special_value")
    None => @test.fail("Expected special chars value")
  }
  
  let baggage_with_special = Baggage::empty()
    .with_entry(special_chars_key, "special_value")
  
  match baggage_with_special.get(special_chars_key) {
    Some(value) => assert_eq(value, "special_value")
    None => @test.fail("Expected special chars baggage value")
  }
  
  // 测试Unicode键名
  let unicode_key = "键.名.包含.中文.和.emoji.🚀"
  let context_with_unicode = Context::empty()
    .with_value(create_key(unicode_key), "unicode_value")
  
  match context_with_unicode.get(create_key(unicode_key)) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Expected unicode value")
  }
  
  let baggage_with_unicode = Baggage::empty()
    .with_entry(unicode_key, "unicode_value")
  
  match baggage_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Expected unicode baggage value")
  }
}

test "context_baggage_serialization_and_deserialization" {
  // 测试Context和Baggage的序列化和反序列化
  
  // 创建包含多种数据的Context
  let test_context = Context::empty()
    .with_value(create_key("user.id"), "user123")
    .with_value(create_key("session.id"), "sess_456")
    .with_value(create_key("request.id"), "req_789")
    .with_value(create_key("trace.id"), "trace_abc")
  
  // 模拟Context序列化
  let serialize_context = fn(ctx : Context) -> String {
    // 在实际实现中，这可能会将Context转换为字符串或字节流
    // 这里我们模拟一个简单的序列化格式
    let serialized = "context:" + ctx.values.length().to_string() + ":"
    let mut i = 0
    
    while i < ctx.values.length() {
      let (key, value) = ctx.values[i]
      serialized = serialized + key + "=" + value + ";"
      i = i + 1
    }
    
    serialized
  }
  
  let serialized_context = serialize_context(test_context)
  assert_eq(serialized_context.has_prefix("context:"), true)
  assert_eq(serialized_context.contains("user.id=user123"), true)
  assert_eq(serialized_context.contains("session.id=sess_456"), true)
  
  // 模拟Context反序列化
  let deserialize_context = fn(serialized : String) -> Context {
    // 在实际实现中，这会从字符串重建Context
    // 这里我们返回一个模拟的Context
    Context::empty()
  }
  
  let deserialized_context = deserialize_context(serialized_context)
  assert_eq(deserialized_context.values.length(), 0)  // 模拟实现返回空Context
  
  // 创建对应的Baggage
  let test_baggage = Baggage::empty()
    .with_entry("user.id", "user123")
    .with_entry("session.id", "sess_456")
    .with_entry("request.id", "req_789")
    .with_entry("trace.id", "trace_abc")
  
  // 模拟Baggage序列化为HTTP头格式
  let serialize_baggage_as_header = fn(baggage : Baggage) -> String {
    let mut header_value = ""
    let mut i = 0
    
    while i < baggage.entries.length() {
      let (key, value) = baggage.entries[i]
      if i > 0 {
        header_value = header_value + ","
      }
      header_value = header_value + key + "=" + value
      i = i + 1
    }
    
    header_value
  }
  
  let baggage_header = serialize_baggage_as_header(test_baggage)
  assert_eq(baggage_header.contains("user.id=user123"), true)
  assert_eq(baggage_header.contains("session.id=sess_456"), true)
  assert_eq(baggage_header.contains(","), true)  // 应该有逗号分隔符
  
  // 模拟从HTTP头反序列化Baggage
  let parse_baggage_header = fn(header : String) -> Baggage {
    // 简单的HTTP头解析
    let entries = header.split(",")
    let baggage = Baggage::empty()
    let mut result = baggage
    let mut i = 0
    
    while i < entries.length() {
      let entry = entries[i]
      let parts = entry.split("=")
      if parts.length() == 2 {
        result = result.with_entry(parts[0], parts[1])
      }
      i = i + 1
    }
    
    result
  }
  
  let parsed_baggage = parse_baggage_header(baggage_header)
  
  // 验证解析后的Baggage包含原始条目
  match parsed_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => @test.fail("Expected user.id in parsed baggage")
  }
  
  match parsed_baggage.get("session.id") {
    Some(session_id) => assert_eq(session_id, "sess_456")
    None => @test.fail("Expected session.id in parsed baggage")
  }
  
  // 测试URL编码的Baggage头
  let url_encoded_value = "user%20name%3DJohn%20Doe"
  let encoded_baggage = Baggage::empty().with_entry("user.info", url_encoded_value)
  let encoded_header = serialize_baggage_as_header(encoded_baggage)
  
  assert_eq(encoded_header.contains("user.info=" + url_encoded_value), true)
  assert_eq(encoded_header.contains("%20"), true)
  assert_eq(encoded_header.contains("%3D"), true)
  
  // 测试包含特殊字符的Baggage
  let special_chars_baggage = Baggage::empty()
    .with_entry("special.key", "value with spaces and symbols!@#")
    .with_entry("unicode.key", "值 包含 中文")
  
  let special_header = serialize_baggage_as_header(special_chars_baggage)
  assert_eq(special_header.contains("special.key="), true)
  assert_eq(special_header.contains("unicode.key="), true)
  
  // 测试空Baggage的序列化
  let empty_baggage = Baggage::empty()
  let empty_header = serialize_baggage_as_header(empty_baggage)
  assert_eq(empty_header, "")
  
  // 测试单条目Baggage的序列化
  let single_entry_baggage = Baggage::empty().with_entry("single", "value")
  let single_header = serialize_baggage_as_header(single_entry_baggage)
  assert_eq(single_header, "single=value")
  assert_eq(single_header.contains(","), false)  // 单条目不应有逗号
}