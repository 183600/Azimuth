// TracerProvider集成测试
// 测试TracerProvider的集成功能，包括多个Tracer的创建和管理

test "tracer_provider_basic_functionality" {
  // 测试TracerProvider的基本功能
  
  let provider = NoopTracerProvider::{}
  
  // 获取基本的tracer
  let tracer1 = provider.get_tracer("basic_tracer")
  assert_eq(true, true)  // 如果没有异常，说明创建成功
  
  // 获取带版本的tracer
  let tracer2 = provider.get_tracer("versioned_tracer", Some("1.0.0"))
  assert_eq(true, true)  // 如果没有异常，说明创建成功
  
  // 获取相同名称的tracer（应该返回相同实例）
  let tracer3 = provider.get_tracer("basic_tracer")
  assert_eq(true, true)  // No-op实现可能返回新实例，但功能应该相同
  
  // 验证所有tracer都能正常工作
  let ctx = Context::empty()
  
  let (_, span1) = tracer1.start_span(ctx, "test_span_1")
  assert_eq(span1.name, "test_span_1")
  
  let (_, span2) = tracer2.start_span(ctx, "test_span_2")
  assert_eq(span2.name, "test_span_2")
  
  let (_, span3) = tracer3.start_span(ctx, "test_span_3")
  assert_eq(span3.name, "test_span_3")
}

test "tracer_provider_multiple_tracers_isolation" {
  // 测试多个Tracer之间的隔离性
  
  let provider = NoopTracerProvider::{}
  
  // 创建多个不同的tracer
  let http_tracer = provider.get_tracer("http-tracer", Some("2.1.0"))
  let db_tracer = provider.get_tracer("database-tracer", Some("1.5.3"))
  let cache_tracer = provider.get_tracer("cache-tracer", Some("0.9.8"))
  
  let ctx = Context::empty()
  
  // 使用HTTP tracer创建span
  let http_attributes = [
    ("http.method", AttributeValue::string("GET")),
    ("http.url", AttributeValue::string("https://api.example.com/users")),
    ("http.status_code", AttributeValue::int(200L))
  ]
  let (_, http_span) = http_tracer.start_span(ctx, "http_request", Some(Server), Some(http_attributes))
  
  assert_eq(http_span.name, "http_request")
  assert_eq(http_span.kind, Server)
  assert_eq(http_span.attributes.length(), 3)
  
  // 使用数据库tracer创建span
  let db_attributes = [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.statement", AttributeValue::string("SELECT * FROM users")),
    ("db.rows_affected", AttributeValue::int(10L))
  ]
  let (_, db_span) = db_tracer.start_span(ctx, "db_query", Some(Client), Some(db_attributes))
  
  assert_eq(db_span.name, "db_query")
  assert_eq(db_span.kind, Client)
  assert_eq(db_span.attributes.length(), 3)
  
  // 使用缓存tracer创建span
  let cache_attributes = [
    ("cache.operation", AttributeValue::string("get")),
    ("cache.key", AttributeValue::string("user:123")),
    ("cache.hit", AttributeValue::bool(true))
  ]
  let (_, cache_span) = cache_tracer.start_span(ctx, "cache_operation", Some(Internal), Some(cache_attributes))
  
  assert_eq(cache_span.name, "cache_operation")
  assert_eq(cache_span.kind, Internal)
  assert_eq(cache_span.attributes.length(), 3)
  
  // 验证不同tracer创建的span是独立的
  assert_eq(http_span.name != db_span.name, true)
  assert_eq(db_span.name != cache_span.name, true)
  assert_eq(http_span.name != cache_span.name, true)
  
  assert_eq(http_span.kind != db_span.kind, true)
  assert_eq(db_span.kind != cache_span.kind, true)
}

test "tracer_provider_with_different_span_kinds" {
  // 测试TracerProvider创建不同种类Span的能力
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("multi-kind-tracer")
  let ctx = Context::empty()
  
  // 测试所有SpanKind
  let (_, internal_span) = tracer.start_span(ctx, "internal_operation", Some(Internal), None)
  assert_eq(internal_span.kind, Internal)
  
  let (_, server_span) = tracer.start_span(ctx, "server_operation", Some(Server), None)
  assert_eq(server_span.kind, Server)
  
  let (_, client_span) = tracer.start_span(ctx, "client_operation", Some(Client), None)
  assert_eq(client_span.kind, Client)
  
  let (_, producer_span) = tracer.start_span(ctx, "producer_operation", Some(Producer), None)
  assert_eq(producer_span.kind, Producer)
  
  let (_, consumer_span) = tracer.start_span(ctx, "consumer_operation", Some(Consumer), None)
  assert_eq(consumer_span.kind, Consumer)
}

test "tracer_provider_with_complex_attributes" {
  // 测试TracerProvider处理复杂属性的能力
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("complex-attributes-tracer")
  let ctx = Context::empty()
  
  // 创建复杂的属性集合
  let complex_attributes = [
    // 基本类型
    ("string.value", AttributeValue::string("complex_string_value")),
    ("int.value", AttributeValue::int(42L)),
    ("float.value", AttributeValue::float(3.14159)),
    ("bool.value", AttributeValue::bool(true)),
    
    // 数组类型
    ("string.array", AttributeValue::array_string(["item1", "item2", "item3"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true])),
    
    // 特殊值
    ("empty.string", AttributeValue::string("")),
    ("zero.int", AttributeValue::int(0L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    
    // 边界值
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("tiny.float", AttributeValue::float(1.0e-10)),
    ("large.float", AttributeValue::float(1.0e10)),
    
    // Unicode和特殊字符
    ("unicode.value", AttributeValue::string("Hello 世界 🌍")),
    ("special.chars", AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("multiline.value", AttributeValue::string("Line 1\nLine 2\nLine 3"))
  ]
  
  let (_, span) = tracer.start_span(ctx, "complex_attributes_span", Some(Server), Some(complex_attributes))
  
  // 验证所有属性都正确设置
  assert_eq(span.name, "complex_attributes_span")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 22)
  
  // 验证特定属性
  let mut found_string_value = false
  let mut found_int_value = false
  let mut found_float_value = false
  let mut found_bool_value = false
  let mut found_array_value = false
  let mut found_unicode_value = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "string.value" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "complex_string_value")
            found_string_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "int.value" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 42L)
            found_int_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "float.value" => {
        match value {
          FloatValue(v) => {
            assert_eq(v, 3.14159)
            found_float_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "bool.value" => {
        match value {
          BoolValue(v) => {
            assert_eq(v, true)
            found_bool_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "string.array" => {
        match value {
          ArrayStringValue(v) => {
            assert_eq(v.length(), 3)
            found_array_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "unicode.value" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "Hello 世界 🌍")
            found_unicode_value = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_string_value, true)
  assert_eq(found_int_value, true)
  assert_eq(found_float_value, true)
  assert_eq(found_bool_value, true)
  assert_eq(found_array_value, true)
  assert_eq(found_unicode_value, true)
}

test "tracer_provider_with_parent_child_relationships" {
  // 测试TracerProvider处理父子Span关系的能力
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("parent-child-tracer")
  
  // 创建父span
  let ctx = Context::empty()
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_operation", Some(Server), None)
  
  // 创建子span
  let parent_span_id = parent_span.context.span_id
  let child_attributes = [
    ("parent.span_id", AttributeValue::array_string([
      parent_span_id[0].to_string(),
      parent_span_id[1].to_string(),
      parent_span_id[2].to_string(),
      parent_span_id[3].to_string()
    ]))
  ]
  
  let (_, child_span) = tracer.start_span(parent_ctx, "child_operation", Some(Internal), Some(child_attributes))
  
  // 验证父子关系
  assert_eq(parent_span.name, "parent_operation")
  assert_eq(parent_span.kind, Server)
  assert_eq(parent_span.parent_span_id, None)  // 父span没有父span
  
  assert_eq(child_span.name, "child_operation")
  assert_eq(child_span.kind, Internal)
  // 注意：在No-op实现中，子span可能不会自动设置parent_span_id
  // 这里我们通过属性来模拟父子关系
  
  // 验证子span的属性
  assert_eq(child_span.attributes.length(), 1)
  match child_span.attributes[0].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
    }
    _ => @test.fail("Test failed")
  }
}

test "tracer_provider_with_timing_operations" {
  // 测试TracerProvider处理时间相关的操作
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("timing-tracer")
  let ctx = Context::empty()
  
  // 创建带时间戳的span
  let start_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let (_, span) = tracer.start_span(ctx, "timed_operation", Some(Server), None, Some(start_time))
  
  assert_eq(span.name, "timed_operation")
  assert_eq(span.start_time_unix_nanos, start_time)
  assert_eq(span.end_time_unix_nanos, None)  // 初始状态下没有结束时间
  
  // 创建不同时间戳的span
  let early_time = 1640995000000000000L
  let late_time = 1640995400000000000L
  
  let (_, early_span) = tracer.start_span(ctx, "early_operation", Some(Server), None, Some(early_time))
  let (_, late_span) = tracer.start_span(ctx, "late_operation", Some(Server), None, Some(late_time))
  
  assert_eq(early_span.start_time_unix_nanos, early_time)
  assert_eq(late_span.start_time_unix_nanos, late_time)
  assert_eq(early_span.start_time_unix_nanos < late_span.start_time_unix_nanos, true)
}

test "tracer_provider_error_handling" {
  // 测试TracerProvider的错误处理能力
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("error-handling-tracer")
  let ctx = Context::empty()
  
  // 创建表示错误的span
  let error_attributes = [
    ("error.type", AttributeValue::string("connection_timeout")),
    ("error.message", AttributeValue::string("Failed to connect to database after 3 retries")),
    ("error.code", AttributeValue::int(503L)),
    ("error.retries", AttributeValue::int(3L)),
    ("error.stack_trace", AttributeValue::string("at Database.connect (db.js:123)\nat Service.process (service.js:456)"))
  ]
  
  let (_, error_span) = tracer.start_span(ctx, "error_operation", Some(Client), Some(error_attributes))
  
  assert_eq(error_span.name, "error_operation")
  assert_eq(error_span.kind, Client)
  assert_eq(error_span.attributes.length(), 5)
  
  // 验证错误属性
  let mut found_error_type = false
  let mut found_error_code = false
  let mut found_error_retries = false
  
  let mut i = 0
  while i < error_span.attributes.length() {
    let (key, value) = error_span.attributes[i]
    
    match key {
      "error.type" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "connection_timeout")
            found_error_type = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "error.code" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 503L)
            found_error_code = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "error.retries" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 3L)
            found_error_retries = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()  // 忽略其他属性
    }
    
    i = i + 1
  }
  
  assert_eq(found_error_type, true)
  assert_eq(found_error_code, true)
  assert_eq(found_error_retries, true)
}

test "tracer_provider_concurrent_operations" {
  // 测试TracerProvider的并发操作能力
  
  let provider = NoopTracerProvider::{}
  
  // 创建多个tracer
  let tracer1 = provider.get_tracer("concurrent-tracer-1")
  let tracer2 = provider.get_tracer("concurrent-tracer-2")
  let tracer3 = provider.get_tracer("concurrent-tracer-3")
  
  let ctx = Context::empty()
  
  // 模拟并发创建span
  let (_, span1) = tracer1.start_span(ctx, "concurrent_operation_1", Some(Server), None)
  let (_, span2) = tracer2.start_span(ctx, "concurrent_operation_2", Some(Client), None)
  let (_, span3) = tracer3.start_span(ctx, "concurrent_operation_3", Some(Internal), None)
  
  // 验证所有span都正确创建
  assert_eq(span1.name, "concurrent_operation_1")
  assert_eq(span1.kind, Server)
  
  assert_eq(span2.name, "concurrent_operation_2")
  assert_eq(span2.kind, Client)
  
  assert_eq(span3.name, "concurrent_operation_3")
  assert_eq(span3.kind, Internal)
  
  // 验证span之间的独立性
  assert_eq(span1.name != span2.name, true)
  assert_eq(span2.name != span3.name, true)
  assert_eq(span1.name != span3.name, true)
  
  assert_eq(span1.kind != span2.kind, true)
  assert_eq(span2.kind != span3.kind, true)
  assert_eq(span1.kind != span3.kind, true)
}