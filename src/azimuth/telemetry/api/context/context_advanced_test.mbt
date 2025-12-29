// Context 嵌套和覆盖操作高级测试用例
// 测试Context的嵌套、覆盖、值存储和检索操作

test "context_basic_operations" {
  // 测试Context的基本操作
  
  // 创建空Context
  let empty_context = context::Context::empty()
  assert_eq(empty_context.values.length(), 0)
  
  // 创建ContextKey
  let user_key = context::create_key("user.id")
  let session_key = context::create_key("session.id")
  let request_key = context::create_key("request.id")
  
  // 验证ContextKey的创建
  assert_eq(user_key.name, "user.id")
  assert_eq(session_key.name, "session.id")
  assert_eq(request_key.name, "request.id")
  
  // 测试在空Context中获取值
  match empty_context.get(user_key) {
    None => assert_eq(true, true)
    Some(_) => assert_eq(false, true)
  }
  
  // 添加值到Context
  let context_with_user = empty_context.with_value(user_key, "user-12345")
  assert_eq(context_with_user.values.length(), 1)
  
  // 验证添加的值
  match context_with_user.get(user_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_eq(false, true)
  }
  
  // 验证其他key仍然没有值
  match context_with_user.get(session_key) {
    None => assert_eq(true, true)
    Some(_) => assert_eq(false, true)
  }
}

test "context_nested_operations" {
  // 测试Context的嵌套操作
  
  let empty_context = context::Context::empty()
  
  // 创建多个ContextKey
  let user_key = context::create_key("user.id")
  let session_key = context::create_key("session.id")
  let request_key = context::create_key("request.id")
  let trace_key = context::create_key("trace.id")
  
  // 嵌套添加值
  let context1 = empty_context.with_value(user_key, "user-123")
  let context2 = context1.with_value(session_key, "session-abc")
  let context3 = context2.with_value(request_key, "req-456")
  let context4 = context3.with_value(trace_key, "trace-789")
  
  // 验证嵌套Context的值数量
  assert_eq(context1.values.length(), 1)
  assert_eq(context2.values.length(), 2)
  assert_eq(context3.values.length(), 3)
  assert_eq(context4.values.length(), 4)
  
  // 验证所有值都可以访问
  match context4.get(user_key) {
    Some(value) => assert_eq(value, "user-123")
    None => assert_eq(false, true)
  }
  
  match context4.get(session_key) {
    Some(value) => assert_eq(value, "session-abc")
    None => assert_eq(false, true)
  }
  
  match context4.get(request_key) {
    Some(value) => assert_eq(value, "req-456")
    None => assert_eq(false, true)
  }
  
  match context4.get(trace_key) {
    Some(value) => assert_eq(value, "trace-789")
    None => assert_eq(false, true)
  }
  
  // 验证中间Context的值
  match context2.get(user_key) {
    Some(value) => assert_eq(value, "user-123")
    None => assert_eq(false, true)
  }
  
  match context2.get(request_key) {
    None => assert_eq(true, true)  // context2中没有request_key
    Some(_) => assert_eq(false, true)
  }
}

test "context_value_override_operations" {
  // 测试Context的值覆盖操作
  
  let empty_context = context::Context::empty()
  let user_key = context::create_key("user.id")
  let status_key = context::create_key("status")
  
  // 添加初始值
  let context1 = empty_context.with_value(user_key, "user-123")
  let context2 = context1.with_value(status_key, "pending")
  
  // 验证初始值
  match context2.get(user_key) {
    Some(value) => assert_eq(value, "user-123")
    None => assert_eq(false, true)
  }
  
  match context2.get(status_key) {
    Some(value) => assert_eq(value, "pending")
    None => assert_eq(false, true)
  }
  
  // 覆盖现有值
  let context3 = context2.with_value(user_key, "user-456")
  let context4 = context3.with_value(status_key, "completed")
  
  // 验证覆盖后的值
  match context4.get(user_key) {
    Some(value) => assert_eq(value, "user-456")  // 新值
    None => assert_eq(false, true)
  }
  
  match context4.get(status_key) {
    Some(value) => assert_eq(value, "completed")  // 新值
    None => assert_eq(false, true)
  }
  
  // 验证值数量
  assert_eq(context2.values.length(), 2)
  assert_eq(context4.values.length(), 2)  // 覆盖不会增加数量
  
  // 多次覆盖同一个值
  let context5 = context4.with_value(user_key, "user-789")
  let context6 = context5.with_value(user_key, "user-999")
  
  match context6.get(user_key) {
    Some(value) => assert_eq(value, "user-999")  // 最终值
    None => assert_eq(false, true)
  }
  
  assert_eq(context6.values.length(), 2)  // 数量仍然不变
}

test "context_complex_scenarios" {
  // 测试Context的复杂场景
  
  let empty_context = context::Context::empty()
  
  // 创建多个相关的ContextKey
  let user_id_key = context::create_key("user.id")
  let user_name_key = context::create_key("user.name")
  let user_role_key = context::create_key("user.role")
  let session_id_key = context::create_key("session.id")
  let request_id_key = context::create_key("request.id")
  let correlation_id_key = context::create_key("correlation.id")
  let trace_id_key = context::create_key("trace.id")
  let span_id_key = context::create_key("span.id")
  
  // 构建复杂的Context层次结构
  let user_context = empty_context
    .with_value(user_id_key, "user-12345")
    .with_value(user_name_key, "John Doe")
    .with_value(user_role_key, "admin")
  
  let session_context = user_context
    .with_value(session_id_key, "session-abcdef")
    .with_value(correlation_id_key, "corr-123456")
  
  let request_context = session_context
    .with_value(request_id_key, "req-789012")
    .with_value(trace_id_key, "trace-345678")
  
  let span_context = request_context
    .with_value(span_id_key, "span-901234")
  
  // 验证最终的Context包含所有值
  assert_eq(span_context.values.length(), 8)
  
  // 验证用户相关值
  match span_context.get(user_id_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_eq(false, true)
  }
  
  match span_context.get(user_name_key) {
    Some(value) => assert_eq(value, "John Doe")
    None => assert_eq(false, true)
  }
  
  match span_context.get(user_role_key) {
    Some(value) => assert_eq(value, "admin")
    None => assert_eq(false, true)
  }
  
  // 验证会话相关值
  match span_context.get(session_id_key) {
    Some(value) => assert_eq(value, "session-abcdef")
    None => assert_eq(false, true)
  }
  
  match span_context.get(correlation_id_key) {
    Some(value) => assert_eq(value, "corr-123456")
    None => assert_eq(false, true)
  }
  
  // 验证请求相关值
  match span_context.get(request_id_key) {
    Some(value) => assert_eq(value, "req-789012")
    None => assert_eq(false, true)
  }
  
  match span_context.get(trace_id_key) {
    Some(value) => assert_eq(value, "trace-345678")
    None => assert_eq(false, true)
  }
  
  match span_context.get(span_id_key) {
    Some(value) => assert_eq(value, "span-901234")
    None => assert_eq(false, true)
  }
  
  // 验证中间Context的完整性
  assert_eq(user_context.values.length(), 3)
  assert_eq(session_context.values.length(), 5)
  assert_eq(request_context.values.length(), 7)
  assert_eq(span_context.values.length(), 8)
}

test "context_edge_cases_and_boundary_values" {
  // 测试Context的边界情况和特殊值
  
  let empty_context = context::Context::empty()
  
  // 测试空字符串值
  let empty_key = context::create_key("empty.value")
  let context_with_empty = empty_context.with_value(empty_key, "")
  
  match context_with_empty.get(empty_key) {
    Some(value) => {
      assert_eq(value, "")
      assert_eq(value.length(), 0)
    }
    None => assert_eq(false, true)
  }
  
  // 测试长字符串值
  let long_key = context::create_key("long.value")
  let long_string = "a".repeat(1000) + "b".repeat(1000) + "c".repeat(1000)
  let context_with_long = empty_context.with_value(long_key, long_string)
  
  match context_with_long.get(long_key) {
    Some(value) => assert_eq(value.length(), 3000)
    None => assert_eq(false, true)
  }
  
  // 测试特殊字符值
  let special_key = context::create_key("special.value")
  let special_string = "特殊字符!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let context_with_special = empty_context.with_value(special_key, special_string)
  
  match context_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_string)
    None => assert_eq(false, true)
  }
  
  // 测试数字字符串值
  let numeric_key = context::create_key("numeric.value")
  let numeric_string = "1234567890.0987654321"
  let context_with_numeric = empty_context.with_value(numeric_key, numeric_string)
  
  match context_with_numeric.get(numeric_key) {
    Some(value) => assert_eq(value, numeric_string)
    None => assert_eq(false, true)
  }
  
  // 测试JSON字符串值
  let json_key = context::create_key("json.value")
  let json_string = "{\"key\":\"value\",\"number\":42,\"array\":[1,2,3]}"
  let context_with_json = empty_context.with_value(json_key, json_string)
  
  match context_with_json.get(json_key) {
    Some(value) => assert_eq(value, json_string)
    None => assert_eq(false, true)
  }
  
  // 测试ContextKey的边界情况
  let empty_name_key = context::create_key("")
  let context_with_empty_key = empty_context.with_value(empty_name_key, "value")
  
  match context_with_empty_key.get(empty_name_key) {
    Some(value) => assert_eq(value, "value")
    None => assert_eq(false, true)
  }
  
  let long_name_key = context::create_key("this.is.a.very.long.key.name.with.many.dots.and.words")
  let context_with_long_key = empty_context.with_value(long_name_key, "long-key-value")
  
  match context_with_long_key.get(long_name_key) {
    Some(value) => assert_eq(value, "long-key-value")
    None => assert_eq(false, true)
  }
}

test "context_performance_and_large_scale_operations" {
  // 测试Context的性能和大规模操作
  
  let empty_context = context::Context::empty()
  
  // 创建大量ContextKey
  let keys = []
  let mut index = 0
  while index < 100 {
    let key = context::create_key("key." + index.to_string())
    keys.push(key)
    index = index + 1
  }
  
  // 添加大量值到Context
  let mut current_context = empty_context
  let mut index2 = 0
  while index2 < keys.length() {
    let key = keys[index2]
    let value = "value." + index2.to_string()
    current_context = current_context.with_value(key, value)
    index2 = index2 + 1
  }
  
  // 验证所有值都存在
  assert_eq(current_context.values.length(), 100)
  
  // 随机访问一些值
  match current_context.get(keys[0]) {
    Some(value) => assert_eq(value, "value.0")
    None => assert_eq(false, true)
  }
  
  match current_context.get(keys[49]) {
    Some(value) => assert_eq(value, "value.49")
    None => assert_eq(false, true)
  }
  
  match current_context.get(keys[99]) {
    Some(value) => assert_eq(value, "value.99")
    None => assert_eq(false, true)
  }
  
  // 测试在大规模Context中覆盖值
  let updated_context = current_context.with_value(keys[50], "updated.value.50")
  
  match updated_context.get(keys[50]) {
    Some(value) => assert_eq(value, "updated.value.50")
    None => assert_eq(false, true)
  }
  
  // 验证覆盖后数量不变
  assert_eq(updated_context.values.length(), 100)
  
  // 验证其他值不受影响
  match updated_context.get(keys[49]) {
    Some(value) => assert_eq(value, "value.49")
    None => assert_eq(false, true)
  }
  
  match updated_context.get(keys[51]) {
    Some(value) => assert_eq(value, "value.51")
    None => assert_eq(false, true)
  }
  
  // 测试从大规模Context中创建子Context
  let subset_context = updated_context.with_value(
    context::create_key("new.key"), 
    "new.value"
  )
  
  assert_eq(subset_context.values.length(), 101)
  
  // 验证新值存在
  match subset_context.get(context::create_key("new.key")) {
    Some(value) => assert_eq(value, "new.value")
    None => assert_eq(false, true)
  }
  
  // 验证原有值仍然存在
  match subset_context.get(keys[25]) {
    Some(value) => assert_eq(value, "value.25")
    None => assert_eq(false, true)
  }
}