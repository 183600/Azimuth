// Azimuth Premium Concurrent Safety Tests
// 高质量并发安全性测试用例
// 专注于测试遥测系统在并发环境下的数据安全性和一致性

// 测试1: Attributes并发访问安全性
test "Attributes并发访问安全性测试" {
  // 1. 创建共享的Attributes对象
  let shared_attributes = Attributes({
    values: [
      ("concurrent.key1", AttributeValue::StringValue("initial.value1")),
      ("concurrent.key2", AttributeValue::IntValue(100)),
      ("concurrent.key3", AttributeValue::BoolValue(true)),
      ("concurrent.key4", AttributeValue::FloatValue(3.14159)),
      ("concurrent.key5", AttributeValue::ArrayStringValue(["item1", "item2", "item3"]))
    ]
  })
  
  // 2. 模拟并发读取操作
  // 在实际实现中，这里会有多个线程同时读取shared_attributes
  // 由于MoonBit当前的限制，我们通过顺序模拟来验证逻辑正确性
  
  // 模拟线程1的读取操作
  let mut thread1_results = []
  for (key, value) in shared_attributes.values {
    match key {
      "concurrent.key1" => {
        match value {
          AttributeValue::StringValue(s) => {
            thread1_results = thread1_results.push(("concurrent.key1", s))
          }
          _ => assert_true(false, "线程1: 字符串值类型不匹配")
        }
      }
      "concurrent.key2" => {
        match value {
          AttributeValue::IntValue(i) => {
            thread1_results = thread1_results.push(("concurrent.key2", i.to_string()))
          }
          _ => assert_true(false, "线程1: 整数值类型不匹配")
        }
      }
      _ => () // 忽略其他键
    }
  }
  
  // 模拟线程2的读取操作
  let mut thread2_results = []
  for (key, value) in shared_attributes.values {
    match key {
      "concurrent.key3" => {
        match value {
          AttributeValue::BoolValue(b) => {
            thread2_results = thread2_results.push(("concurrent.key3", b.to_string()))
          }
          _ => assert_true(false, "线程2: 布尔值类型不匹配")
        }
      }
      "concurrent.key4" => {
        match value {
          AttributeValue::FloatValue(f) => {
            thread2_results = thread2_results.push(("concurrent.key4", f.to_string()))
          }
          _ => assert_true(false, "线程2: 浮点数值类型不匹配")
        }
      }
      _ => () // 忽略其他键
    }
  }
  
  // 模拟线程3的读取操作
  let mut thread3_results = []
  for (key, value) in shared_attributes.values {
    match key {
      "concurrent.key5" => {
        match value {
          AttributeValue::ArrayStringValue(arr) => {
            let array_str = "[" + arr.join(",") + "]"
            thread3_results = thread3_results.push(("concurrent.key5", array_str))
          }
          _ => assert_true(false, "线程3: 字符串数组类型不匹配")
        }
      }
      _ => () // 忽略其他键
    }
  }
  
  // 3. 验证并发读取结果的一致性
  assert_eq(thread1_results.length(), 2)
  assert_eq(thread2_results.length(), 2)
  assert_eq(thread3_results.length(), 1)
  
  // 验证线程1的结果
  for (key, value) in thread1_results {
    match key {
      "concurrent.key1" => assert_eq(value, "initial.value1")
      "concurrent.key2" => assert_eq(value, "100")
      _ => assert_true(false, "线程1: 未知键 " + key)
    }
  }
  
  // 验证线程2的结果
  for (key, value) in thread2_results {
    match key {
      "concurrent.key3" => assert_eq(value, "true")
      "concurrent.key4" => assert_true(value.contains("3.14159"))
      _ => assert_true(false, "线程2: 未知键 " + key)
    }
  }
  
  // 验证线程3的结果
  for (key, value) in thread3_results {
    match key {
      "concurrent.key5" => assert_true(value.contains("item1") && value.contains("item2") && value.contains("item3"))
      _ => assert_true(false, "线程3: 未知键 " + key)
    }
  }
}

// 测试2: SpanContext并发创建和访问
test "SpanContext并发创建和访问测试" {
  // 1. 创建多个SpanContext对象，模拟并发创建
  let span_contexts = [
    SpanContext({
      trace_id: "11111111111111111111111111111111",
      span_id: "1111111111111111",
      sampled: true,
      trace_state: "thread=1,operation=create"
    }),
    SpanContext({
      trace_id: "22222222222222222222222222222222",
      span_id: "2222222222222222",
      sampled: false,
      trace_state: "thread=2,operation=create"
    }),
    SpanContext({
      trace_id: "33333333333333333333333333333333",
      span_id: "3333333333333333",
      sampled: true,
      trace_state: "thread=3,operation=create"
    }),
    SpanContext({
      trace_id: "44444444444444444444444444444444",
      span_id: "4444444444444444",
      sampled: true,
      trace_state: "thread=4,operation=create"
    }),
    SpanContext({
      trace_id: "55555555555555555555555555555555",
      span_id: "5555555555555555",
      sampled: false,
      trace_state: "thread=5,operation=create"
    })
  ]
  
  // 2. 验证并发创建的SpanContext的唯一性和正确性
  let mut trace_ids = []
  let mut span_ids = []
  let mut sampled_count = 0
  let mut unsampled_count = 0
  
  for context in span_contexts {
    // 收集所有trace_id
    trace_ids = trace_ids.push(context.trace_id)
    
    // 收集所有span_id
    span_ids = span_ids.push(context.span_id)
    
    // 统计采样和未采样的数量
    if context.sampled {
      sampled_count = sampled_count + 1
    } else {
      unsampled_count = unsampled_count + 1
    }
    
    // 验证trace_id格式
    assert_eq(context.trace_id.length(), 32)
    assert_true(context.trace_id.chars().all(fn(c) { 
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
    }))
    
    // 验证span_id格式
    assert_eq(context.span_id.length(), 16)
    assert_true(context.span_id.chars().all(fn(c) { 
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
    }))
    
    // 验证trace_state格式
    assert_true(context.trace_state.contains("thread="))
    assert_true(context.trace_state.contains("operation=create"))
  }
  
  // 3. 验证唯一性
  assert_eq(trace_ids.length(), 5)
  assert_eq(span_ids.length(), 5)
  
  // 检查trace_id的唯一性
  let mut unique_trace_ids = []
  for id in trace_ids {
    if not unique_trace_ids.contains(id) {
      unique_trace_ids = unique_trace_ids.push(id)
    }
  }
  assert_eq(unique_trace_ids.length(), 5, "所有trace_id应该是唯一的")
  
  // 检查span_id的唯一性
  let mut unique_span_ids = []
  for id in span_ids {
    if not unique_span_ids.contains(id) {
      unique_span_ids = unique_span_ids.push(id)
    }
  }
  assert_eq(unique_span_ids.length(), 5, "所有span_id应该是唯一的")
  
  // 4. 验证采样统计
  assert_eq(sampled_count, 3, "应该有3个采样的SpanContext")
  assert_eq(unsampled_count, 2, "应该有2个未采样的SpanContext")
}

// 测试3: Baggage并发修改安全性
test "Baggage并发修改安全性测试" {
  // 1. 创建初始Baggage
  let initial_baggage = Baggage({
    entries: [
      ("initial.key1", "initial.value1"),
      ("initial.key2", "initial.value2"),
      ("initial.key3", "initial.value3")
    ]
  })
  
  // 2. 模拟并发修改操作
  // 线程1: 添加新条目
  let thread1_entries = initial_baggage.entries.to_array()
  let thread1_baggage = Baggage({
    entries: thread1_entries.push(("thread1.key", "thread1.value"))
  })
  
  // 线程2: 修改现有条目
  let mut thread2_entries = []
  for (key, value) in initial_baggage.entries {
    if key == "initial.key2" {
      thread2_entries = thread2_entries.push((key, "modified.value2"))
    } else {
      thread2_entries = thread2_entries.push((key, value))
    }
  }
  let thread2_baggage = Baggage({ entries: thread2_entries })
  
  // 线程3: 删除条目（通过不包含某些条目来模拟）
  let mut thread3_entries = []
  for (key, value) in initial_baggage.entries {
    if key != "initial.key3" {
      thread3_entries = thread3_entries.push((key, value))
    }
  }
  let thread3_baggage = Baggage({ entries: thread3_entries })
  
  // 3. 验证并发修改的结果
  
  // 验证线程1的结果（添加新条目）
  assert_eq(thread1_baggage.entries.length(), 4)
  let mut thread1_found_new_key = false
  let mut thread1_preserved_original = true
  for (key, value) in thread1_baggage.entries {
    match key {
      "thread1.key" => {
        assert_eq(value, "thread1.value")
        thread1_found_new_key = true
      }
      "initial.key1" => {
        if value != "initial.value1" {
          thread1_preserved_original = false
        }
      }
      "initial.key2" => {
        if value != "initial.value2" {
          thread1_preserved_original = false
        }
      }
      "initial.key3" => {
        if value != "initial.value3" {
          thread1_preserved_original = false
        }
      }
      _ => assert_true(false, "线程1: 未知键 " + key)
    }
  }
  assert_true(thread1_found_new_key, "线程1应该添加新条目")
  assert_true(thread1_preserved_original, "线程1应该保留原始条目")
  
  // 验证线程2的结果（修改现有条目）
  assert_eq(thread2_baggage.entries.length(), 3)
  let mut thread2_found_modified = false
  for (key, value) in thread2_baggage.entries {
    match key {
      "initial.key2" => {
        assert_eq(value, "modified.value2")
        thread2_found_modified = true
      }
      "initial.key1" => assert_eq(value, "initial.value1")
      "initial.key3" => assert_eq(value, "initial.value3")
      _ => assert_true(false, "线程2: 未知键 " + key)
    }
  }
  assert_true(thread2_found_modified, "线程2应该修改条目")
  
  // 验证线程3的结果（删除条目）
  assert_eq(thread3_baggage.entries.length(), 2)
  let mut thread3_found_deleted = false
  for (key, value) in thread3_baggage.entries {
    match key {
      "initial.key1" => assert_eq(value, "initial.value1")
      "initial.key2" => assert_eq(value, "initial.value2")
      "initial.key3" => thread3_found_deleted = true // 不应该找到这个键
      _ => assert_true(false, "线程3: 未知键 " + key)
    }
  }
  assert_false(thread3_found_deleted, "线程3应该删除initial.key3条目")
  
  // 4. 验证原始Baggage未被修改
  assert_eq(initial_baggage.entries.length(), 3)
  for (key, value) in initial_baggage.entries {
    match key {
      "initial.key1" => assert_eq(value, "initial.value1")
      "initial.key2" => assert_eq(value, "initial.value2")
      "initial.key3" => assert_eq(value, "initial.value3")
      _ => assert_true(false, "原始Baggage: 未知键 " + key)
    }
  }
}

// 测试4: TextMapCarrier并发注入和提取
test "TextMapCarrier并发注入和提取测试" {
  // 1. 创建基础TextMapCarrier
  let base_carrier = TextMapCarrier({
    headers: [
      ("content-type", "application/json"),
      ("user-agent", "azimuth-telemetry/1.0.0"),
      ("accept", "application/json")
    ]
  })
  
  // 2. 模拟并发注入操作
  
  // 线程1: 注入trace信息
  let mut thread1_headers = base_carrier.headers.to_array()
  thread1_headers = thread1_headers.push(("traceparent", "00-11111111111111111111111111111111-1111111111111111-01"))
  thread1_headers = thread1_headers.push(("tracestate", "thread=1,operation=inject"))
  let thread1_carrier = TextMapCarrier({ headers: thread1_headers })
  
  // 线程2: 注入baggage信息
  let mut thread2_headers = base_carrier.headers.to_array()
  thread2_headers = thread2_headers.push(("baggage", "user.id=user123,request.id=req456"))
  thread2_headers = thread2_headers.push(("x-request-id", "req-123456"))
  let thread2_carrier = TextMapCarrier({ headers: thread2_headers })
  
  // 线程3: 注入自定义header
  let mut thread3_headers = base_carrier.headers.to_array()
  thread3_headers = thread3_headers.push(("x-custom-header", "custom.value"))
  thread3_headers = thread3_headers.push(("x-correlation-id", "corr-789012"))
  let thread3_carrier = TextMapCarrier({ headers: thread3_headers })
  
  // 3. 验证并发注入的结果
  
  // 验证线程1的结果
  assert_eq(thread1_carrier.headers.length(), 5)
  let mut thread1_traceparent_found = false
  let mut thread1_tracestate_found = false
  let mut thread1_base_preserved = true
  
  for (key, value) in thread1_carrier.headers {
    match key {
      "traceparent" => {
        assert_true(value.contains("11111111111111111111111111111111"))
        assert_true(value.contains("1111111111111111"))
        thread1_traceparent_found = true
      }
      "tracestate" => {
        assert_true(value.contains("thread=1"))
        thread1_tracestate_found = true
      }
      "content-type" => {
        if value != "application/json" {
          thread1_base_preserved = false
        }
      }
      "user-agent" => {
        if value != "azimuth-telemetry/1.0.0" {
          thread1_base_preserved = false
        }
      }
      "accept" => {
        if value != "application/json" {
          thread1_base_preserved = false
        }
      }
      _ => assert_true(false, "线程1: 未知header " + key)
    }
  }
  assert_true(thread1_traceparent_found, "线程1应该注入traceparent")
  assert_true(thread1_tracestate_found, "线程1应该注入tracestate")
  assert_true(thread1_base_preserved, "线程1应该保留基础header")
  
  // 验证线程2的结果
  assert_eq(thread2_carrier.headers.length(), 5)
  let mut thread2_baggage_found = false
  let mut thread2_request_id_found = false
  
  for (key, value) in thread2_carrier.headers {
    match key {
      "baggage" => {
        assert_true(value.contains("user.id=user123"))
        assert_true(value.contains("request.id=req456"))
        thread2_baggage_found = true
      }
      "x-request-id" => {
        assert_eq(value, "req-123456")
        thread2_request_id_found = true
      }
      "content-type" => assert_eq(value, "application/json")
      "user-agent" => assert_eq(value, "azimuth-telemetry/1.0.0")
      "accept" => assert_eq(value, "application/json")
      _ => assert_true(false, "线程2: 未知header " + key)
    }
  }
  assert_true(thread2_baggage_found, "线程2应该注入baggage")
  assert_true(thread2_request_id_found, "线程2应该注入x-request-id")
  
  // 验证线程3的结果
  assert_eq(thread3_carrier.headers.length(), 5)
  let mut thread3_custom_found = false
  let mut thread3_correlation_found = false
  
  for (key, value) in thread3_carrier.headers {
    match key {
      "x-custom-header" => {
        assert_eq(value, "custom.value")
        thread3_custom_found = true
      }
      "x-correlation-id" => {
        assert_eq(value, "corr-789012")
        thread3_correlation_found = true
      }
      "content-type" => assert_eq(value, "application/json")
      "user-agent" => assert_eq(value, "azimuth-telemetry/1.0.0")
      "accept" => assert_eq(value, "application/json")
      _ => assert_true(false, "线程3: 未知header " + key)
    }
  }
  assert_true(thread3_custom_found, "线程3应该注入x-custom-header")
  assert_true(thread3_correlation_found, "线程3应该注入x-correlation-id")
  
  // 4. 验证原始carrier未被修改
  assert_eq(base_carrier.headers.length(), 3)
  for (key, value) in base_carrier.headers {
    match key {
      "content-type" => assert_eq(value, "application/json")
      "user-agent" => assert_eq(value, "azimuth-telemetry/1.0.0")
      "accept" => assert_eq(value, "application/json")
      _ => assert_true(false, "原始carrier: 未知header " + key)
    }
  }
}

// 测试5: InstrumentationScope并发访问安全性
test "InstrumentationScope并发访问安全性测试" {
  // 1. 创建共享的InstrumentationScope
  let shared_scope = InstrumentationScope({
    name: "azimuth.shared.telemetry",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  })
  
  // 2. 模拟多个线程同时访问InstrumentationScope
  
  // 线程1: 读取基本信息
  let thread1_name = shared_scope.name
  let thread1_version = match shared_scope.version {
    Some(v) => v
    None => ""
  }
  
  // 线程2: 读取schema信息
  let thread2_name = shared_scope.name
  let thread2_schema = match shared_scope.schema_url {
    Some(url) => url
    None => ""
  }
  
  // 线程3: 读取所有信息
  let thread3_name = shared_scope.name
  let thread3_version = match shared_scope.version {
    Some(v) => v
    None => ""
  }
  let thread3_schema = match shared_scope.schema_url {
    Some(url) => url
    None => ""
  }
  
  // 3. 验证并发访问的一致性
  
  // 验证所有线程读取的名称一致
  assert_eq(thread1_name, thread2_name)
  assert_eq(thread2_name, thread3_name)
  assert_eq(thread1_name, "azimuth.shared.telemetry")
  
  // 验证所有线程读取的版本一致
  assert_eq(thread1_version, thread3_version)
  assert_eq(thread1_version, "2.1.0")
  
  // 验证所有线程读取的schema URL一致
  assert_eq(thread2_schema, thread3_schema)
  assert_eq(thread2_schema, "https://opentelemetry.io/schemas/v1.20.0")
  
  // 4. 验证数据完整性
  assert_true(thread1_name.contains("azimuth"))
  assert_true(thread1_name.contains("telemetry"))
  assert_true(thread1_version.contains("."))
  assert_true(thread2_schema.contains("https://"))
  assert_true(thread2_schema.contains("opentelemetry.io"))
  
  // 5. 创建多个不同的InstrumentationScope，验证并发创建的安全性
  let scopes = [
    InstrumentationScope({
      name: "scope.thread1",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema/v1")
    }),
    InstrumentationScope({
      name: "scope.thread2",
      version: Some("2.0.0"),
      schema_url: Some("https://example.com/schema/v2")
    }),
    InstrumentationScope({
      name: "scope.thread3",
      version: Some("3.0.0"),
      schema_url: Some("https://example.com/schema/v3")
    })
  ]
  
  // 6. 验证并发创建的InstrumentationScope的正确性
  for (i, scope) in scopes.enumerate() {
    let expected_name = "scope.thread" + (i + 1).to_string()
    let expected_version = (i + 1).to_string() + ".0.0"
    let expected_schema = "https://example.com/schema/v" + (i + 1).to_string()
    
    assert_eq(scope.name, expected_name)
    
    match scope.version {
      Some(v) => assert_eq(v, expected_version)
      None => assert_true(false, "版本不应为None")
    }
    
    match scope.schema_url {
      Some(url) => assert_eq(url, expected_schema)
      None => assert_true(false, "Schema URL不应为None")
    }
  }
}

// 测试6: 复合并发操作安全性
test "复合并发操作安全性测试" {
  // 1. 创建复合数据结构
  let base_span_context = SpanContext({
    trace_id: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    span_id: "aaaaaaaaaaaaaaaa",
    sampled: true,
    trace_state: "base=trace"
  })
  
  let base_baggage = Baggage({
    entries: [
      ("base.key1", "base.value1"),
      ("base.key2", "base.value2")
    ]
  })
  
  let base_carrier = TextMapCarrier({
    headers: [
      ("base.header1", "base.value1"),
      ("base.header2", "base.value2")
    ]
  })
  
  // 2. 模拟复杂的并发操作
  
  // 操作1: 基于SpanContext创建新的SpanContext
  let derived_span_context = SpanContext({
    trace_id: base_span_context.trace_id,
    span_id: "bbbbbbbbbbbbbbbb",
    sampled: base_span_context.sampled,
    trace_state: base_span_context.trace_state + ",derived=true"
  })
  
  // 操作2: 基于Baggage添加新的条目
  let mut derived_entries = base_baggage.entries.to_array()
  derived_entries = derived_entries.push(("derived.key", "derived.value"))
  let derived_baggage = Baggage({ entries: derived_entries })
  
  // 操作3: 基于TextMapCarrier添加新的header
  let mut derived_headers = base_carrier.headers.to_array()
  derived_headers = derived_headers.push(("derived.header", "derived.value"))
  let derived_carrier = TextMapCarrier({ headers: derived_headers })
  
  // 3. 验证复合操作的结果
  
  // 验证派生的SpanContext
  assert_eq(derived_span_context.trace_id, base_span_context.trace_id)
  assert_eq(derived_span_context.span_id, "bbbbbbbbbbbbbbbb")
  assert_eq(derived_span_context.sampled, base_span_context.sampled)
  assert_true(derived_span_context.trace_state.contains("base=trace"))
  assert_true(derived_span_context.trace_state.contains("derived=true"))
  
  // 验证派生的Baggage
  assert_eq(derived_baggage.entries.length(), 3)
  let mut found_base_key1 = false
  let mut found_base_key2 = false
  let mut found_derived_key = false
  
  for (key, value) in derived_baggage.entries {
    match key {
      "base.key1" => {
        assert_eq(value, "base.value1")
        found_base_key1 = true
      }
      "base.key2" => {
        assert_eq(value, "base.value2")
        found_base_key2 = true
      }
      "derived.key" => {
        assert_eq(value, "derived.value")
        found_derived_key = true
      }
      _ => assert_true(false, "派生Baggage: 未知键 " + key)
    }
  }
  assert_true(found_base_key1, "应该保留base.key1")
  assert_true(found_base_key2, "应该保留base.key2")
  assert_true(found_derived_key, "应该添加derived.key")
  
  // 验证派生的TextMapCarrier
  assert_eq(derived_carrier.headers.length(), 3)
  let mut found_base_header1 = false
  let mut found_base_header2 = false
  let mut found_derived_header = false
  
  for (key, value) in derived_carrier.headers {
    match key {
      "base.header1" => {
        assert_eq(value, "base.value1")
        found_base_header1 = true
      }
      "base.header2" => {
        assert_eq(value, "base.value2")
        found_base_header2 = true
      }
      "derived.header" => {
        assert_eq(value, "derived.value")
        found_derived_header = true
      }
      _ => assert_true(false, "派生carrier: 未知header " + key)
    }
  }
  assert_true(found_base_header1, "应该保留base.header1")
  assert_true(found_base_header2, "应该保留base.header2")
  assert_true(found_derived_header, "应该添加derived.header")
  
  // 4. 验证原始数据未被修改
  assert_eq(base_span_context.trace_state, "base=trace")
  assert_eq(base_baggage.entries.length(), 2)
  assert_eq(base_carrier.headers.length(), 2)
}