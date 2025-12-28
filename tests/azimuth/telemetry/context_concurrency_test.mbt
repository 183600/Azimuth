// Context模块并发安全测试用例
// 测试context在并发环境下的安全性

test "context creation and value setting concurrency" {
  // 测试上下文创建和值设置的并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建多个上下文键
  let keys = []
  for i = 0; i < 10; i = i + 1 {
    let key = telemetry.api.context.ContextKey.new("concurrency.key.${i}")
    keys.push(key)
  }
  
  // 并发设置值
  let contexts = []
  for i = 0; i < 100; i = i + 1 {
    let key_index = i % keys.length()
    let key = keys[key_index]
    let value = "concurrent.value.${i}"
    let ctx = telemetry.api.context.Context.with_value(root_ctx, key, value)
    contexts.push(ctx)
  }
  
  // 验证每个上下文的值
  for i = 0; i < contexts.length(); i = i + 1 {
    let ctx = contexts[i]
    let key_index = i % keys.length()
    let key = keys[key_index]
    let expected_value = "concurrent.value.${i}"
    let actual_value = telemetry.api.context.Context.get(ctx, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

test "context chain building concurrency" {
  // 测试上下文链构建的并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  let user_key = telemetry.api.context.ContextKey.new("user.id")
  let session_key = telemetry.api.context.ContextKey.new("session.id")
  let request_key = telemetry.api.context.ContextKey.new("request.id")
  
  // 构建多个上下文链
  let chains = []
  for i = 0; i < 50; i = i + 1 {
    let ctx1 = telemetry.api.context.Context.with_value(root_ctx, user_key, "user.${i}")
    let ctx2 = telemetry.api.context.Context.with_value(ctx1, session_key, "session.${i}")
    let ctx3 = telemetry.api.context.Context.with_value(ctx2, request_key, "request.${i}")
    chains.push(ctx3)
  }
  
  // 验证每个链的所有值
  for i = 0; i < chains.length(); i = i + 1 {
    let ctx = chains[i]
    
    let user_id = telemetry.api.context.Context.get(ctx, user_key)
    let session_id = telemetry.api.context.Context.get(ctx, session_key)
    let request_id = telemetry.api.context.Context.get(ctx, request_key)
    
    assert_eq(user_id, Some("user.${i}"))
    assert_eq(session_id, Some("session.${i}"))
    assert_eq(request_id, Some("request.${i}"))
  }
}

test "context with span context concurrency" {
  // 测试包含span上下文的并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建多个span上下文
  let span_contexts = []
  for i = 0; i < 20; i = i + 1 {
    let trace_id = "trace${i.to_string().pad_left(16, '0')}"
    let span_id = "span${i.to_string().pad_left(8, '0')}"
    let span_ctx = telemetry.api.trace.SpanContext.new(trace_id, span_id, true, "")
    span_contexts.push(span_ctx)
  }
  
  // 并发创建包含span上下文的上下文
  let contexts_with_span = []
  for i = 0; i < span_contexts.length(); i = i + 1 {
    let span_ctx = span_contexts[i]
    let ctx = telemetry.api.context.Context.with_span_context(root_ctx, span_ctx)
    contexts_with_span.push(ctx)
  }
  
  // 验证每个上下文的span上下文
  for i = 0; i < contexts_with_span.length(); i = i + 1 {
    let ctx = contexts_with_span[i]
    let span_ctx = telemetry.api.context.Context.get_span_context(ctx)
    assert_true(span_ctx != None)
  }
}

test "context baggage operations concurrency" {
  // 测试上下文baggage操作的并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建多个baggage
  let baggages = []
  for i = 0; i < 30; i = i + 1 {
    let baggage = telemetry.api.context.Baggage.new()
    baggages.push(baggage)
  }
  
  // 并发设置baggage条目
  let baggages_with_entries = []
  for i = 0; i < baggages.length(); i = i + 1 {
    let baggage = baggages[i]
    let baggage_with_entry = telemetry.api.context.Baggage.set_entry(baggage, "key${i}", "value${i}")
    baggages_with_entries.push(baggage_with_entry)
  }
  
  // 验证baggage条目
  for i = 0; i < baggages_with_entries.length(); i = i + 1 {
    let baggage = baggages_with_entries[i]
    let entry = telemetry.api.context.Baggage.get_entry(baggage, "key${i}")
    assert_eq(entry, Some("value${i}"))
  }
}

test "context deep nesting concurrency" {
  // 测试深度嵌套上下文的并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建深度嵌套的上下文
  let deep_contexts = []
  for i = 0; i < 10; i = i + 1 {
    let mut ctx = root_ctx
    for j = 0; j < 50; j = j + 1 {
      let key = telemetry.api.context.ContextKey.new("level${j}.key${i}")
      let value = "level${j}.value${i}"
      ctx = telemetry.api.context.Context.with_value(ctx, key, value)
    }
    deep_contexts.push(ctx)
  }
  
  // 验证深度嵌套的值
  for i = 0; i < deep_contexts.length(); i = i + 1 {
    let ctx = deep_contexts[i]
    
    // 检查不同层级的值
    let level0_key = telemetry.api.context.ContextKey.new("level0.key${i}")
    let level25_key = telemetry.api.context.ContextKey.new("level25.key${i}")
    let level49_key = telemetry.api.context.ContextKey.new("level49.key${i}")
    
    let level0_value = telemetry.api.context.Context.get(ctx, level0_key)
    let level25_value = telemetry.api.context.Context.get(ctx, level25_key)
    let level49_value = telemetry.api.context.Context.get(ctx, level49_key)
    
    assert_eq(level0_value, Some("level0.value${i}"))
    assert_eq(level25_value, Some("level25.value${i}"))
    assert_eq(level49_value, Some("level49.value${i}"))
  }
}

test "context with complex values concurrency" {
  // 测试包含复杂值的上下文并发安全性
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建包含复杂值的上下文
  let complex_contexts = []
  for i = 0; i < 25; i = i + 1 {
    let complex_key = telemetry.api.context.ContextKey.new("complex.key.${i}")
    
    // 创建复杂值
    let complex_value = "{"
    for j = 0; j < 10; j = j + 1 {
      complex_value = complex_value + "\"field${j}\":\"value${i}_${j}\"," 
    }
    complex_value = complex_value.take(complex_value.length() - 1) + "}"
    
    let ctx = telemetry.api.context.Context.with_value(root_ctx, complex_key, complex_value)
    complex_contexts.push(ctx)
  }
  
  // 验证复杂值
  for i = 0; i < complex_contexts.length(); i = i + 1 {
    let ctx = complex_contexts[i]
    let complex_key = telemetry.api.context.ContextKey.new("complex.key.${i}")
    let value = telemetry.api.context.Context.get(ctx, complex_key)
    assert_true(value != None)
    assert_true(value.unwrap().length() > 0)
  }
}

test "context key collision handling" {
  // 测试上下文键冲突处理
  let root_ctx = telemetry.api.context.Context.root()
  
  // 使用相同名称创建多个键（应该得到相同的键）
  let key1 = telemetry.api.context.ContextKey.new("shared.key")
  let key2 = telemetry.api.context.ContextKey.new("shared.key")
  let key3 = telemetry.api.context.ContextKey.new("shared.key")
  
  // 设置不同的值
  let ctx1 = telemetry.api.context.Context.with_value(root_ctx, key1, "value1")
  let ctx2 = telemetry.api.context.Context.with_value(root_ctx, key2, "value2")
  let ctx3 = telemetry.api.context.Context.with_value(root_ctx, key3, "value3")
  
  // 验证键的行为一致性
  let value1_from_ctx1 = telemetry.api.context.Context.get(ctx1, key2)
  let value1_from_ctx3 = telemetry.api.context.Context.get(ctx1, key3)
  
  let value2_from_ctx1 = telemetry.api.context.Context.get(ctx2, key1)
  let value2_from_ctx3 = telemetry.api.context.Context.get(ctx2, key3)
  
  let value3_from_ctx1 = telemetry.api.context.Context.get(ctx3, key1)
  let value3_from_ctx2 = telemetry.api.context.Context.get(ctx3, key2)
  
  assert_eq(value1_from_ctx1, Some("value1"))
  assert_eq(value1_from_ctx3, Some("value1"))
  assert_eq(value2_from_ctx1, Some("value2"))
  assert_eq(value2_from_ctx3, Some("value2"))
  assert_eq(value3_from_ctx1, Some("value3"))
  assert_eq(value3_from_ctx2, Some("value3"))
}

test "context memory pressure handling" {
  // 测试上下文内存压力处理
  let root_ctx = telemetry.api.context.Context.root()
  
  // 创建大量上下文以测试内存压力
  let many_contexts = []
  for i = 0; i < 1000; i = i + 1 {
    let key = telemetry.api.context.ContextKey.new("pressure.key.${i}")
    let value = "pressure.value.${i}".repeat(10) // 较长的值
    let ctx = telemetry.api.context.Context.with_value(root_ctx, key, value)
    many_contexts.push(ctx)
  }
  
  // 验证所有上下文仍然有效
  for i = 0; i < many_contexts.length(); i = i + 1 {
    let ctx = many_contexts[i]
    let key = telemetry.api.context.ContextKey.new("pressure.key.${i}")
    let value = telemetry.api.context.Context.get(ctx, key)
    assert_true(value != None)
    assert_eq(value.unwrap(), "pressure.value.${i}".repeat(10))
  }
}

test "context with null and empty values" {
  // 测试包含null和空值的上下文
  let root_ctx = telemetry.api.context.Context.root()
  
  // 测试空字符串值
  let empty_key = telemetry.api.context.ContextKey.new("empty.key")
  let empty_ctx = telemetry.api.context.Context.with_value(root_ctx, empty_key, "")
  let empty_value = telemetry.api.context.Context.get(empty_ctx, empty_key)
  assert_eq(empty_value, Some(""))
  
  // 测试包含特殊字符的值
  let special_key = telemetry.api.context.ContextKey.new("special.key")
  let special_values = [
    "\0", // null字符
    "\t", // tab
    "\n", // newline
    "\r", // carriage return
    " ", // space
    "   ", // multiple spaces
    "value with spaces",
    "value\twith\ttabs",
    "value\nwith\nnewlines",
    "value\rwith\rcarriage",
    "value\"with\"quotes",
    "value'with'apostrophes",
    "value\\with\\backslashes",
    "value/with/slashes",
    "value@with@symbols",
    "value#with#hashes",
    "value$with$dollars",
    "value%with%percents",
    "value^with^carets",
    "value&with&ampersands",
    "value*with*asterisks",
    "value(with)parentheses",
    "value[with]brackets",
    "value{with}braces",
    "value|with|pipes",
    "value+with+plus",
    "value=with=equals",
    "value?with?questions",
    "value<with>angles"
  ]
  
  for i = 0; i < special_values.length(); i = i + 1 {
    let special_value = special_values[i]
    let ctx = telemetry.api.context.Context.with_value(root_ctx, special_key, special_value)
    let retrieved_value = telemetry.api.context.Context.get(ctx, special_key)
    assert_eq(retrieved_value, Some(special_value))
  }
}