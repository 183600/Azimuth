// Span Lifecycle Comprehensive Tests
// 测试Span生命周期的完整管理

test "span_creation_and_initialization" {
  // Span创建和初始化测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let parent_span_context = azimuth.SpanContext.new(
    "parent_trace_1234567890123456",
    "parent_span_1234567890123456",
    true,
    ""
  )
  
  // 创建具有父Span的子Span
  let span = azimuth.Tracer.start_span_with_parent(
    tracer,
    "operation-name",
    parent_span_context,
    azimuth.SpanKind.INTERNAL
  )
  
  // 验证Span基本属性
  @assertion.assert_equal(azimuth.Span.get_name(span), "operation-name")
  @assertion.assert_equal(azimuth.Span.get_kind(span), azimuth.SpanKind.INTERNAL)
  
  // 验证Span上下文
  let span_context = azimuth.Span.get_context(span)
  @assertion.assert_equal(azimuth.SpanContext.get_trace_id(span_context), "parent_trace_1234567890123456")
  @assertion.assert_not_equal(azimuth.SpanContext.get_span_id(span_context), "parent_span_1234567890123456")
  @assertion.assert_true(azimuth.SpanContext.is_sampled(span_context))
  
  // 验证Span状态
  @assertion.assert_true(azimuth.Span.is_recording(span))
  @assertion.assert_false(azimuth.Span.is_ended(span))
}

test "span_attribute_management" {
  // Span属性管理测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "test-operation")
  
  // 添加各种类型的属性
  azimuth.Span.set_attribute_string(span, "string_attr", "test_value")
  azimuth.Span.set_attribute_int(span, "int_attr", 42)
  azimuth.Span.set_attribute_float(span, "float_attr", 3.14159)
  azimuth.Span.set_attribute_bool(span, "bool_attr", true)
  
  // 添加数组属性
  let string_array = ["value1", "value2", "value3"]
  azimuth.Span.set_attribute_string_array(span, "array_attr", string_array)
  
  let int_array = [1, 2, 3, 4, 5]
  azimuth.Span.set_attribute_int_array(span, "int_array_attr", int_array)
  
  // 验证属性数量
  let attributes = azimuth.Span.get_attributes(span)
  @assertion.assert_equal(azimuth.Attributes.len(attributes), 6)
  
  // 验证特定属性值
  match azimuth.Attributes.get_string(attributes, "string_attr") {
    Some(v) => @assertion.assert_equal(v, "test_value")
    _ => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_int(attributes, "int_attr") {
    Some(v) => @assertion.assert_equal(v, 42)
    _ => @assertion.assert_true(false)
  }
  
  match azimuth.Attributes.get_bool(attributes, "bool_attr") {
    Some(v) => @assertion.assert_true(v)
    _ => @assertion.assert_true(false)
  }
}

test "span_event_tracking" {
  // Span事件跟踪测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "operation-with-events")
  
  // 添加不同类型的事件
  let start_time = @unsafe.Time.now()
  
  azimuth.Span.add_event_with_attributes(span, "operation_started", [
    ("step", azimuth.AttributeValue.StringValue("initialization")),
    ("component", azimuth.AttributeValue.StringValue("auth"))
  ])
  
  azimuth.Span.add_event_with_attributes(span, "user_authenticated", [
    ("user_id", azimuth.AttributeValue.StringValue("user_12345")),
    ("auth_method", azimuth.AttributeValue.StringValue("oauth"))
  ])
  
  azimuth.Span.add_event_with_attributes(span, "database_query", [
    ("query", azimuth.AttributeValue.StringValue("SELECT * FROM users")),
    ("duration_ms", azimuth.AttributeValue.IntValue(150))
  ])
  
  azimuth.Span.add_event_with_attributes(span, "operation_completed", [
    ("result", azimuth.AttributeValue.StringValue("success")),
    ("total_duration_ms", azimuth.AttributeValue.IntValue(500))
  ])
  
  // 验证事件数量
  let events = azimuth.Span.get_events(span)
  @assertion.assert_equal(@unsafe.Array.length(events), 4)
  
  // 验证事件时间戳顺序
  for i = 1; i < @unsafe.Array.length(events); i + 1 {
    let prev_event = @unsafe.Array.get(events, i - 1)
    let curr_event = @unsafe.Array.get(events, i)
    
    let prev_time = azimuth.Event.get_timestamp(prev_event)
    let curr_time = azimuth.Event.get_timestamp(curr_event)
    
    @assertion.assert_true(prev_time <= curr_time)
  }
}

test "span_status_management" {
  // Span状态管理测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "operation-with-status")
  
  // 初始状态应该是UNSET
  let initial_status = azimuth.Span.get_status(span)
  @assertion.assert_equal(azimuth.Status.get_code(initial_status), azimuth.StatusCode.UNSET)
  
  // 设置OK状态
  azimuth.Span.set_status(span, azimuth.StatusCode.OK, "Operation completed successfully")
  let ok_status = azimuth.Span.get_status(span)
  @assertion.assert_equal(azimuth.Status.get_code(ok_status), azimuth.StatusCode.OK)
  @assertion.assert_equal(azimuth.Status.get_description(ok_status), "Operation completed successfully")
  
  // 设置ERROR状态
  azimuth.Span.set_status(span, azimuth.StatusCode.ERROR, "Database connection failed")
  let error_status = azimuth.Span.get_status(span)
  @assertion.assert_equal(azimuth.Status.get_code(error_status), azimuth.StatusCode.ERROR)
  @assertion.assert_equal(azimuth.Status.get_description(error_status), "Database connection failed")
}

test "span_parent_child_relationships" {
  // Span父子关系测试
  
  let tracer = azimuth.Tracer.new("test-service")
  
  // 创建根Span
  let root_span = azimuth.Tracer.start_span(tracer, "root-operation")
  let root_context = azimuth.Span.get_context(root_span)
  
  // 创建第一层子Span
  let child1 = azimuth.Tracer.start_span_with_parent(
    tracer,
    "child-operation-1",
    root_context,
    azimuth.SpanKind.CLIENT
  )
  
  let child2 = azimuth.Tracer.start_span_with_parent(
    tracer,
    "child-operation-2",
    root_context,
    azimuth.SpanKind.SERVER
  )
  
  // 创建第二层子Span（嵌套）
  let child1_context = azimuth.Span.get_context(child1)
  let grandchild = azimuth.Tracer.start_span_with_parent(
    tracer,
    "grandchild-operation",
    child1_context,
    azimuth.SpanKind.INTERNAL
  )
  
  // 验证Trace ID一致性
  let root_trace_id = azimuth.SpanContext.get_trace_id(root_context)
  let child1_trace_id = azimuth.SpanContext.get_trace_id(azimuth.Span.get_context(child1))
  let child2_trace_id = azimuth.SpanContext.get_trace_id(azimuth.Span.get_context(child2))
  let grandchild_trace_id = azimuth.SpanContext.get_trace_id(azimuth.Span.get_context(grandchild))
  
  @assertion.assert_equal(root_trace_id, child1_trace_id)
  @assertion.assert_equal(root_trace_id, child2_trace_id)
  @assertion.assert_equal(root_trace_id, grandchild_trace_id)
  
  // 验证Span ID唯一性
  let root_span_id = azimuth.SpanContext.get_span_id(root_context)
  let child1_span_id = azimuth.SpanContext.get_span_id(azimuth.Span.get_context(child1))
  let child2_span_id = azimuth.SpanContext.get_span_id(azimuth.Span.get_context(child2))
  let grandchild_span_id = azimuth.SpanContext.get_span_id(azimuth.Span.get_context(grandchild))
  
  @assertion.assert_not_equal(root_span_id, child1_span_id)
  @assertion.assert_not_equal(root_span_id, child2_span_id)
  @assertion.assert_not_equal(root_span_id, grandchild_span_id)
  @assertion.assert_not_equal(child1_span_id, child2_span_id)
  @assertion.assert_not_equal(child1_span_id, grandchild_span_id)
  @assertion.assert_not_equal(child2_span_id, grandchild_span_id)
}

test "span_termination_and_cleanup" {
  // Span终止和清理测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "operation-to-end")
  
  // 验证Span初始状态
  @assertion.assert_true(azimuth.Span.is_recording(span))
  @assertion.assert_false(azimuth.Span.is_ended(span))
  
  // 添加一些数据和事件
  azimuth.Span.set_attribute_string(span, "result", "success")
  azimuth.Span.add_event(span, "operation_completed")
  
  // 结束Span
  azimuth.Span.end(span)
  
  // 验证Span结束状态
  @assertion.assert_false(azimuth.Span.is_recording(span))
  @assertion.assert_true(azimuth.Span.is_ended(span))
  
  // 尝试在已结束的Span上添加属性（应该被忽略或处理）
  azimuth.Span.set_attribute_string(span, "after_end", "should_be_ignored")
  
  // 验证数据完整性
  let attributes = azimuth.Span.get_attributes(span)
  match azimuth.Attributes.get_string(attributes, "result") {
    Some(v) => @assertion.assert_equal(v, "success")
    _ => @assertion.assert_true(false)
  }
  
  // 验证结束后的属性没有被添加
  match azimuth.Attributes.get_string(attributes, "after_end") {
    Some(_) => @assertion.assert_true(false) // 不应该存在
    None => @assertion.assert_true(true) // 正确
  }
}

test "span_error_handling_and_recovery" {
  // Span错误处理和恢复测试
  
  let tracer = azimuth.Tracer.new("test-service")
  let span = azimuth.Tracer.start_span(tracer, "error-prone-operation")
  
  // 模拟正常操作
  azimuth.Span.add_event(span, "operation_started")
  
  // 模拟错误情况
  try {
    // 模拟可能抛出异常的操作
    azimuth.Span.set_status(span, azimuth.StatusCode.ERROR, "Simulated error occurred")
    azimuth.Span.add_event_with_attributes(span, "error_occurred", [
      ("error_type", azimuth.AttributeValue.StringValue("validation_error")),
      ("error_code", azimuth.AttributeValue.IntValue(400))
    ])
    
    // 即使发生错误，Span应该仍然可用
    @assertion.assert_true(azimuth.Span.is_recording(span))
    
  } catch {
    _ => {
      // 错误处理：确保Span仍然可以正常结束
      azimuth.Span.set_status(span, azimuth.StatusCode.ERROR, "Unhandled exception")
    }
  }
  
  // 恢复操作
  azimuth.Span.add_event(span, "recovery_attempted")
  azimuth.Span.set_attribute_string(span, "recovery_action", "retry_with_fallback")
  
  // 正常结束Span
  azimuth.Span.end(span)
  
  // 验证最终状态
  let final_status = azimuth.Span.get_status(span)
  @assertion.assert_equal(azimuth.Status.get_code(final_status), azimuth.StatusCode.ERROR)
  @assertion.assert_true(azimuth.Span.is_ended(span))
  
  // 验证事件和属性都被正确记录
  let events = azimuth.Span.get_events(span)
  @assertion.assert_true(@unsafe.Array.length(events) >= 3) // started, error, recovery
  
  let attributes = azimuth.Span.get_attributes(span)
  match azimuth.Attributes.get_string(attributes, "recovery_action") {
    Some(v) => @assertion.assert_equal(v, "retry_with_fallback")
    _ => @assertion.assert_true(false)
  }
}