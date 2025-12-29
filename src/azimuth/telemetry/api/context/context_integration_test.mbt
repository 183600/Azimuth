// Context模块边界和集成测试用例
test "context_key_collision_and_overwrite" {
  let context = Context::empty()
  let key = create_key("test.key")
  
  // 第一次设置值
  let context1 = context.with_value(key, "value1")
  match context1.get(key) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value1")
  }
  
  // 第二次设置相同键的值（应该覆盖）
  let context2 = context1.with_value(key, "value2")
  match context2.get(key) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value2 (overwritten)")
  }
  
  // 再次设置不同值
  let context3 = context2.with_value(key, "value3")
  match context3.get(key) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3 (overwritten again)")
  }
  
  // 验证覆盖不会影响其他键
  let other_key = create_key("other.key")
  let context4 = context3.with_value(other_key, "other.value")
  
  match context4.get(key) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3 to persist")
  }
  
  match context4.get(other_key) {
    Some(value) => assert_eq(value, "other.value")
    None => @test.fail("Expected other.value")
  }
}

test "baggage_key_collision_and_overwrite" {
  let baggage = Baggage::empty()
  
  // 第一次设置值
  let baggage1 = baggage.with_entry("test.key", "value1")
  match baggage1.get("test.key") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value1 in baggage")
  }
  
  // 第二次设置相同键的值（应该覆盖）
  let baggage2 = baggage1.with_entry("test.key", "value2")
  match baggage2.get("test.key") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value2 (overwritten) in baggage")
  }
  
  // 再次设置不同值
  let baggage3 = baggage2.with_entry("test.key", "value3")
  match baggage3.get("test.key") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3 (overwritten again) in baggage")
  }
  
  // 验证覆盖不会影响其他键
  let baggage4 = baggage3.with_entry("other.key", "other.value")
  
  match baggage4.get("test.key") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3 to persist in baggage")
  }
  
  match baggage4.get("other.key") {
    Some(value) => assert_eq(value, "other.value")
    None => @test.fail("Expected other.value in baggage")
  }
}

test "context_baggage_cross_integration" {
  // 测试Context和Baggage的复杂集成场景
  
  let context = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中存储元数据
  let metadata_key = create_key("metadata")
  let context_with_metadata = context.with_value(
    metadata_key, 
    "service.name=auth-service,service.version=1.2.3"
  )
  
  // 在Baggage中存储用户跟踪信息
  let baggage_with_user = baggage.with_entry("user.id", "user-12345")
  let baggage_with_session = baggage_with_user.with_entry("session.id", "sess-abcdef")
  let baggage_with_request = baggage_with_session.with_entry("request.id", "req-67890")
  
  // 模拟从Baggage构建Context
  let baggage_key = create_key("baggage.data")
  let baggage_string = "user.id=user-12345,session.id=sess-abcdef,request.id=req-67890"
  let context_with_baggage = context_with_metadata.with_value(baggage_key, baggage_string)
  
  // 验证Context中的所有数据
  match context_with_baggage.get(metadata_key) {
    Some(value) => assert_eq(value, "service.name=auth-service,service.version=1.2.3")
    None => @test.fail("Expected metadata in context")
  }
  
  match context_with_baggage.get(baggage_key) {
    Some(value) => assert_eq(value, baggage_string)
    None => @test.fail("Expected baggage data in context")
  }
  
  // 验证Baggage中的数据
  match baggage_with_request.get("user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Expected user.id in baggage")
  }
  
  match baggage_with_request.get("session.id") {
    Some(value) => assert_eq(value, "sess-abcdef")
    None => @test.fail("Expected session.id in baggage")
  }
  
  match baggage_with_request.get("request.id") {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Expected request.id in baggage")
  }
}

test "context_with_complex_nested_structures" {
  // 测试Context存储复杂的嵌套结构
  
  let context = Context::empty()
  
  // 存储JSON风格的嵌套数据（作为字符串）
  let complex_key = create_key("complex.data")
  let complex_data = """{
    "user": {
      "id": "user-12345",
      "profile": {
        "name": "John Doe",
        "email": "john@example.com",
        "preferences": {
          "theme": "dark",
          "language": "en",
          "notifications": {
            "email": true,
            "push": false,
            "sms": true
          }
        }
      }
    },
    "session": {
      "id": "sess-abcdef",
      "created_at": "2023-01-01T00:00:00Z",
      "expires_at": "2023-01-01T24:00:00Z",
      "metadata": {
        "ip_address": "192.168.1.100",
        "user_agent": "Mozilla/5.0...",
        "location": {
          "country": "US",
          "city": "New York",
          "coordinates": {
            "lat": 40.7128,
            "lng": -74.0060
          }
        }
      }
    }
  }"""
  
  let context_with_complex = context.with_value(complex_key, complex_data)
  
  match context_with_complex.get(complex_key) {
    Some(value) => {
      assert_eq(value, complex_data)
      
      // 验证JSON结构的基本存在
      assert_eq(value.contains("\"user\": {"), true)
      assert_eq(value.contains("\"session\": {"), true)
      assert_eq(value.contains("\"preferences\": {"), true)
      assert_eq(value.contains("\"notifications\": {"), true)
      assert_eq(value.contains("\"location\": {"), true)
      assert_eq(value.contains("\"coordinates\": {"), true)
    }
    None => @test.fail("Expected complex data in context")
  }
  
  // 存储其他类型的复杂数据
  let array_key = create_key("array.data")
  let array_data = "[1, 2, 3, \"string\", true, null, {\"nested\": \"object\"}, [4, 5, 6]]"
  
  let context_with_array = context_with_complex.with_value(array_key, array_data)
  
  match context_with_array.get(array_key) {
    Some(value) => assert_eq(value, array_data)
    None => @test.fail("Expected array data in context")
  }
}

test "baggage_with_http_header_simulation" {
  // 模拟HTTP头中的Baggage传播
  
  let baggage = Baggage::empty()
  
  // 模拟从HTTP头解析的baggage
  let http_baggage_header = "key1=value1;key2=value2;key3=value3"
  
  // 简单解析（在实际实现中会更复杂）
  let entries = http_baggage_header.split(";")
  let mut baggage = Baggage::empty()
  let mut i = 0
  while i < entries.length() {
    let entry = entries[i]
    let parts = entry.split("=")
    if parts.length() == 2 {
      baggage = baggage.with_entry(parts[0], parts[1])
    }
    i = i + 1
  }
  
  // 验证解析结果
  match baggage.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected key1=value1")
  }
  
  match baggage.get("key2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected key2=value2")
  }
  
  match baggage.get("key3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected key3=value3")
  }
  
  // 模拟复杂的HTTP baggage头（包含属性）
  let complex_http_header = "user.id=user123;vendor.name=acme;vendor.version=1.0;transaction.id=tx456"
  
  let entries = complex_http_header.split(";")
  let mut complex_baggage = Baggage::empty()
  let mut i = 0
  while i < entries.length() {
    let entry = entries[i]
    let parts = entry.split("=")
    if parts.length() == 2 {
      complex_baggage = complex_baggage.with_entry(parts[0], parts[1])
    }
    i = i + 1
  }
  
  // 验证复杂baggage
  match complex_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user.id=user123")
  }
  
  match complex_baggage.get("vendor.name") {
    Some(value) => assert_eq(value, "acme")
    None => @test.fail("Expected vendor.name=acme")
  }
  
  match complex_baggage.get("vendor.version") {
    Some(value) => assert_eq(value, "1.0")
    None => @test.fail("Expected vendor.version=1.0")
  }
  
  match complex_baggage.get("transaction.id") {
    Some(value) => assert_eq(value, "tx456")
    None => @test.fail("Expected transaction.id=tx456")
  }
}

test "context_baggage_concurrent_operations_simulation" {
  // 模拟并发操作（虽然MoonBit是单线程的，但可以模拟并发场景）
  
  let base_context = Context::empty()
  let base_baggage = Baggage::empty()
  
  // 模拟多个"并发"操作
  let context1 = base_context.with_value(create_key("thread1"), "value1")
  let context2 = base_context.with_value(create_key("thread2"), "value2")
  let context3 = base_context.with_value(create_key("thread3"), "value3")
  
  let baggage1 = base_baggage.with_entry("process1", "proc1")
  let baggage2 = base_baggage.with_entry("process2", "proc2")
  let baggage3 = base_baggage.with_entry("process3", "proc3")
  
  // 验证独立性
  match context1.get(create_key("thread1")) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected thread1 value")
  }
  
  match context2.get(create_key("thread2")) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected thread2 value")
  }
  
  match context3.get(create_key("thread3")) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected thread3 value")
  }
  
  match baggage1.get("process1") {
    Some(value) => assert_eq(value, "proc1")
    None => @test.fail("Expected process1 value")
  }
  
  match baggage2.get("process2") {
    Some(value) => assert_eq(value, "proc2")
    None => @test.fail("Expected process2 value")
  }
  
  match baggage3.get("process3") {
    Some(value) => assert_eq(value, "proc3")
    None => @test.fail("Expected process3 value")
  }
  
  // 模拟合并操作
  let merged_context = context1
    .with_value(create_key("thread2"), "value2")
    .with_value(create_key("thread3"), "value3")
    .with_value(create_key("merged"), "true")
  
  let merged_baggage = baggage1
    .with_entry("process2", "proc2")
    .with_entry("process3", "proc3")
    .with_entry("merged", "true")
  
  // 验证合并结果
  match merged_context.get(create_key("merged")) {
    Some(value) => assert_eq(value, "true")
    None => @test.fail("Expected merged flag in context")
  }
  
  match merged_baggage.get("merged") {
    Some(value) => assert_eq(value, "true")
    None => @test.fail("Expected merged flag in baggage")
  }
}

test "context_baggage_error_handling_and_recovery" {
  // 测试错误处理和恢复场景
  
  let context = Context::empty()
  let baggage = Baggage::empty()
  
  // 测试空键处理
  let empty_key_context = context.with_value(create_key(""), "empty.key.value")
  match empty_key_context.get(create_key("")) {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Expected empty key to work")
  }
  
  let empty_key_baggage = baggage.with_entry("", "empty.baggage.value")
  match empty_key_baggage.get("") {
    Some(value) => assert_eq(value, "empty.baggage.value")
    None => @test.fail("Expected empty baggage key to work")
  }
  
  // 测试空值处理
  let null_key = create_key("null.key")
  let null_value_context = context.with_value(null_key, "")
  match null_value_context.get(null_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty string value")
  }
  
  let null_value_baggage = baggage.with_entry("null.baggage.key", "")
  match null_value_baggage.get("null.baggage.key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty baggage value")
  }
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?~`\n\t\r"
  let special_key = create_key("special.chars.key")
  
  let special_context = context.with_value(special_key, special_chars)
  match special_context.get(special_key) {
    Some(value) => assert_eq(value, special_chars)
    None => @test.fail("Expected special characters in context")
  }
  
  let special_baggage = baggage.with_entry("special.baggage.key", special_chars)
  match special_baggage.get("special.baggage.key") {
    Some(value) => assert_eq(value, special_chars)
    None => @test.fail("Expected special characters in baggage")
  }
  
  // 测试超长字符串处理
  let long_string = "a".repeat(100000)  // 100K字符
  let long_key = create_key("long.key")
  
  let long_context = context.with_value(long_key, long_string)
  match long_context.get(long_key) {
    Some(value) => assert_eq(value.length(), 100000)
    None => @test.fail("Expected long string in context")
  }
  
  let long_baggage = baggage.with_entry("long.baggage.key", long_string)
  match long_baggage.get("long.baggage.key") {
    Some(value) => assert_eq(value.length(), 100000)
    None => @test.fail("Expected long string in baggage")
  }
}