// Context模块性能和并发安全性测试用例
// 测试Context在高负载和并发场景下的性能表现

test "context_high_frequency_operations" {
  // 测试Context高频操作的性能
  let ctx = Context::empty()
  
  // 高频设置和获取操作
  let mut i = 0
  let mut current_ctx = ctx
  while i < 10000 {
    let key = create_key("key." + i.to_string())
    let value = "value." + i.to_string()
    current_ctx = current_ctx.with_value(key, value)
    i = i + 1
  }
  
  // 验证所有值都能正确获取
  let mut j = 0
  while j < 10000 {
    let key = create_key("key." + j.to_string())
    let expected_value = "value." + j.to_string()
    match current_ctx.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected Some value for key: " + key.name)
    }
    j = j + 1
  }
  
  assert_eq(current_ctx.values.length(), 10000)
}

test "context_memory_efficiency" {
  // 测试Context内存使用效率
  
  // 创建大量不同的context实例
  let contexts = []
  let mut i = 0
  while i < 1000 {
    let ctx = Context::empty()
    let key = create_key("context.key." + i.to_string())
    let value = "context.value." + i.to_string()
    let ctx_with_value = ctx.with_value(key, value)
    contexts.push(ctx_with_value)
    i = i + 1
  }
  
  // 验证所有context都正确创建
  let mut j = 0
  while j < contexts.length() {
    let ctx = contexts[j]
    let expected_key = "context.key." + j.to_string()
    let expected_value = "context.value." + j.to_string()
    let key = create_key(expected_key)
    
    match ctx.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected Some value for context: " + expected_key)
    }
    j = j + 1
  }
  
  assert_eq(contexts.length(), 1000)
}

test "baggage_high_frequency_operations" {
  // 测试Baggage高频操作的性能
  let baggage = Baggage::empty()
  
  // 高频添加条目操作
  let mut current_baggage = baggage
  let mut i = 0
  while i < 10000 {
    let key = "baggage.key." + i.to_string()
    let value = "baggage.value." + i.to_string()
    current_baggage = current_baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有条目都能正确获取
  let mut j = 0
  while j < 10000 {
    let key = "baggage.key." + j.to_string()
    let expected_value = "baggage.value." + j.to_string()
    match current_baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected Some value for baggage key: " + key)
    }
    j = j + 1
  }
  
  assert_eq(current_baggage.entries.length(), 10000)
}

test "context_complex_nested_operations" {
  // 测试Context复杂嵌套操作
  
  let ctx = Context::empty()
  
  // 创建深层嵌套的context
  let mut current_ctx = ctx
  let mut i = 0
  while i < 1000 {
    // 每层添加多个键值对
    let key1 = create_key("level." + i.to_string() + ".key1")
    let key2 = create_key("level." + i.to_string() + ".key2")
    let key3 = create_key("level." + i.to_string() + ".key3")
    
    let value1 = "level." + i.to_string() + ".value1"
    let value2 = "level." + i.to_string() + ".value2"
    let value3 = "level." + i.to_string() + ".value3"
    
    current_ctx = current_ctx
      .with_value(key1, value1)
      .with_value(key2, value2)
      .with_value(key3, value3)
    
    i = i + 1
  }
  
  // 验证深层嵌套的所有值
  let mut j = 0
  while j < 1000 {
    let key1 = create_key("level." + j.to_string() + ".key1")
    let key2 = create_key("level." + j.to_string() + ".key2")
    let key3 = create_key("level." + j.to_string() + ".key3")
    
    let expected_value1 = "level." + j.to_string() + ".value1"
    let expected_value2 = "level." + j.to_string() + ".value2"
    let expected_value3 = "level." + j.to_string() + ".value3"
    
    match current_ctx.get(key1) {
      Some(actual_value) => assert_eq(actual_value, expected_value1)
      None => @test.fail("Expected Some value for key1 at level: " + j.to_string())
    }
    
    match current_ctx.get(key2) {
      Some(actual_value) => assert_eq(actual_value, expected_value2)
      None => @test.fail("Expected Some value for key2 at level: " + j.to_string())
    }
    
    match current_ctx.get(key3) {
      Some(actual_value) => assert_eq(actual_value, expected_value3)
      None => @test.fail("Expected Some value for key3 at level: " + j.to_string())
    }
    
    j = j + 1
  }
  
  assert_eq(current_ctx.values.length(), 3000) // 1000 levels * 3 keys each
}

test "baggage_complex_entries_operations" {
  // 测试Baggage复杂条目操作
  
  let baggage = Baggage::empty()
  
  // 添加各种复杂条目
  let complex_entries = [
    ("user.id", "user-12345"),
    ("request.id", "req-abcdef-67890"),
    ("service.name", "payment-processing-service"),
    ("service.version", "v2.1.3-beta"),
    ("deployment.environment", "production-west"),
    ("trace.sampled", "true"),
    ("debug.enabled", "false"),
    ("correlation.id", "corr-xyz-123-456"),
    ("session.id", "sess-789-abc-def"),
    ("tenant.id", "tenant-001"),
    ("region", "us-west-2"),
    ("availability.zone", "us-west-2a"),
    ("instance.id", "i-0123456789abcdef0"),
    ("host.name", "ip-10-0-1-123.us-west-2.compute.internal"),
    ("process.id", "12345"),
    ("thread.id", "main-worker-7"),
    ("custom.tag", "custom-value-with-special-chars!@#$%"),
    ("unicode.test", "测试中文内容🚀"),
    ("url.encoded", "key%20with%20spaces%26symbols"),
    ("json.value", "{\"nested\":{\"key\":\"value\",\"array\":[1,2,3]}}"),
    ("base64.value", "SGVsbG8gV29ybGQh"),
    ("timestamp.iso", "2023-12-07T10:30:45.123Z"),
    ("duration.ms", "12345"),
    ("size.bytes", "1048576"),
    ("rate.per.second", "1000.5")
  ]
  
  // 添加所有复杂条目
  let mut current_baggage = baggage
  let mut i = 0
  while i < complex_entries.length() {
    let (key, value) = complex_entries[i]
    current_baggage = current_baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有复杂条目都能正确获取
  let mut j = 0
  while j < complex_entries.length() {
    let (expected_key, expected_value) = complex_entries[j]
    match current_baggage.get(expected_key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected Some value for baggage key: " + expected_key)
    }
    j = j + 1
  }
  
  assert_eq(current_baggage.entries.length(), complex_entries.length())
}

test "context_concurrent_simulation" {
  // 模拟并发Context操作（由于MoonBit不支持真正的并发，这里模拟交替操作）
  
  let ctx = Context::empty()
  
  // 模拟多个"线程"交替操作
  let mut current_ctx = ctx
  
  // "线程1"的操作
  let mut i = 0
  while i < 1000 {
    let key = create_key("thread1.key." + i.to_string())
    let value = "thread1.value." + i.to_string()
    current_ctx = current_ctx.with_value(key, value)
    i = i + 1
  }
  
  // "线程2"的操作
  let mut j = 0
  while j < 1000 {
    let key = create_key("thread2.key." + j.to_string())
    let value = "thread2.value." + j.to_string()
    current_ctx = current_ctx.with_value(key, value)
    j = j + 1
  }
  
  // "线程3"的操作
  let mut k = 0
  while k < 1000 {
    let key = create_key("thread3.key." + k.to_string())
    let value = "thread3.value." + k.to_string()
    current_ctx = current_ctx.with_value(key, value)
    k = k + 1
  }
  
  // 验证所有"线程"的数据都正确保存
  let mut check_i = 0
  while check_i < 1000 {
    let key1 = create_key("thread1.key." + check_i.to_string())
    let expected_value1 = "thread1.value." + check_i.to_string()
    match current_ctx.get(key1) {
      Some(actual_value) => assert_eq(actual_value, expected_value1)
      None => @test.fail("Expected Some value for thread1 key: " + check_i.to_string())
    }
    check_i = check_i + 1
  }
  
  let mut check_j = 0
  while check_j < 1000 {
    let key2 = create_key("thread2.key." + check_j.to_string())
    let expected_value2 = "thread2.value." + check_j.to_string()
    match current_ctx.get(key2) {
      Some(actual_value) => assert_eq(actual_value, expected_value2)
      None => @test.fail("Expected Some value for thread2 key: " + check_j.to_string())
    }
    check_j = check_j + 1
  }
  
  let mut check_k = 0
  while check_k < 1000 {
    let key3 = create_key("thread3.key." + check_k.to_string())
    let expected_value3 = "thread3.value." + check_k.to_string()
    match current_ctx.get(key3) {
      Some(actual_value) => assert_eq(actual_value, expected_value3)
      None => @test.fail("Expected Some value for thread3 key: " + check_k.to_string())
    }
    check_k = check_k + 1
  }
  
  assert_eq(current_ctx.values.length(), 3000) // 3 threads * 1000 keys each
}

test "context_and_baggage_integration_performance" {
  // 测试Context和Baggage集成的性能
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 创建包含大量数据的baggage
  let mut current_baggage = baggage
  let mut i = 0
  while i < 1000 {
    let key = "integration.key." + i.to_string()
    let value = "integration.value." + i.to_string()
    current_baggage = current_baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 将baggage序列化并存储到context中
  let baggage_key = create_key("baggage")
  let mut baggage_string = ""
  let mut j = 0
  while j < current_baggage.entries.length() {
    let (key, value) = current_baggage.entries[j]
    if j > 0 {
      baggage_string = baggage_string + ","
    }
    baggage_string = baggage_string + key + "=" + value
    j = j + 1
  }
  
  let ctx_with_baggage = ctx.with_value(baggage_key, baggage_string)
  
  // 在context中添加其他元数据
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  let flags_key = create_key("trace_flags")
  
  let final_ctx = ctx_with_baggage
    .with_value(trace_id_key, "0af7651916cd43dd8448eb211c80319c")
    .with_value(span_id_key, "b7ad6b7169203331")
    .with_value(flags_key, "01")
  
  // 验证集成数据的完整性
  match final_ctx.get(baggage_key) {
    Some(stored_baggage) => {
      // 验证baggage字符串包含所有预期的条目
      let mut k = 0
      while k < 1000 {
        let expected_key = "integration.key." + k.to_string()
        let expected_value = "integration.value." + k.to_string()
        let expected_entry = expected_key + "=" + expected_value
        assert_eq(stored_baggage.contains(expected_entry), true)
        k = k + 1
      }
    }
    None => @test.fail("Expected Some baggage value")
  }
  
  match final_ctx.get(trace_id_key) {
    Some(value) => assert_eq(value, "0af7651916cd43dd8448eb211c80319c")
    None => @test.fail("Expected Some trace_id value")
  }
  
  match final_ctx.get(span_id_key) {
    Some(value) => assert_eq(value, "b7ad6b7169203331")
    None => @test.fail("Expected Some span_id value")
  }
  
  match final_ctx.get(flags_key) {
    Some(value) => assert_eq(value, "01")
    None => @test.fail("Expected Some trace_flags value")
  }
  
  assert_eq(final_ctx.values.length(), 4)
  assert_eq(current_baggage.entries.length(), 1000)
}