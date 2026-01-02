// Azimuth 简单数据测试用例
// 专注于测试遥测系统的基本数据操作

// 测试1: 基本字符串操作
test "基本字符串操作测试" {
  let service_name = "azimuth-telemetry"
  let operation_name = "data.processing"
  
  // 测试字符串长度
  assert_eq(service_name.length(), 17)
  assert_eq(operation_name.length(), 15)
  
  // 测试字符串连接
  let full_name = service_name + "." + operation_name
  assert_eq(full_name, "azimuth-telemetry.data.processing")
  
  // 测试字符串包含
  assert_true(service_name.contains("azimuth"))
  assert_true(service_name.contains("telemetry"))
  assert_false(service_name.contains("metrics"))
}

// 测试2: 基本数组操作
test "基本数组操作测试" {
  let trace_ids = ["trace-001", "trace-002", "trace-003"]
  let span_ids = ["span-001", "span-002", "span-003"]
  
  // 测试数组长度
  assert_eq(trace_ids.length(), 3)
  assert_eq(span_ids.length(), 3)
  
  // 测试数组索引访问
  assert_eq(trace_ids[0], "trace-001")
  assert_eq(trace_ids[2], "trace-003")
  assert_eq(span_ids[1], "span-002")
  
  // 测试数组连接
  let combined = trace_ids + span_ids
  assert_eq(combined.length(), 6)
  assert_eq(combined[0], "trace-001")
  assert_eq(combined[3], "span-001")
  assert_eq(combined[5], "span-003")
}

// 测试3: 基本数值操作
test "基本数值操作测试" {
  let request_count = 100
  let error_count = 5
  let response_time_ms = 125.5
  let success_rate = 95.0
  
  // 测试整数运算
  assert_eq(request_count + error_count, 105)
  assert_eq(request_count - error_count, 95)
  assert_eq(request_count * 2, 200)
  assert_eq(request_count / 4, 25)
  
  // 测试浮点数运算
  assert_eq(response_time_ms + 10.0, 135.5)
  assert_eq(response_time_ms - 25.5, 100.0)
  assert_eq(response_time_ms * 2, 251.0)
  assert_eq(response_time_ms / 2, 62.75)
  
  // 测试百分比计算
  let actual_success_rate = (request_count - error_count).to_float() / request_count.to_float() * 100.0
  assert_eq(actual_success_rate, 95.0)
  assert_eq(actual_success_rate, success_rate)
}

// 测试4: 基本布尔操作
test "基本布尔操作测试" {
  let is_enabled = true
  let is_debug_mode = false
  let has_error = false
  let is_healthy = true
  
  // 测试基本布尔值
  assert_true(is_enabled)
  assert_false(is_debug_mode)
  assert_false(has_error)
  assert_true(is_healthy)
  
  // 测试布尔运算
  assert_true(is_enabled && is_healthy)
  assert_false(is_enabled && has_error)
  assert_true(is_enabled || is_debug_mode)
  assert_true(is_debug_mode || has_error || is_enabled)
  assert_false(is_debug_mode || has_error)
  
  // 测试布尔否定
  assert_false(!is_enabled)
  assert_true(!is_debug_mode)
  assert_true(!has_error)
  assert_false(!is_healthy)
}

// 测试5: Option类型基本操作
test "Option类型基本操作测试" {
  let some_value = Some("test-value")
  let none_value = None
  let some_number = Some(42)
  let none_number = None
  
  // 测试Some值
  match some_value {
    Some(v) => assert_eq(v, "test-value")
    None => assert_true(false)
  }
  
  match some_number {
    Some(n) => assert_eq(n, 42)
    None => assert_true(false)
  }
  
  // 测试None值
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  match none_number {
    Some(n) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试Option映射
  let uppercased = some_value.map(fn(s) { s.to_uppercase() })
  match uppercased {
    Some(s) => assert_eq(s, "TEST-VALUE")
    None => assert_true(false)
  }
  
  let doubled = some_number.map(fn(n) { n * 2 })
  match doubled {
    Some(n) => assert_eq(n, 84)
    None => assert_true(false)
  }
  
  // 测试None的映射
  let none_mapped = none_value.map(fn(s) { s.to_uppercase() })
  assert_eq(none_mapped, None)
}

// 测试6: 元组操作
test "元组操作测试" {
  let trace_info = ("trace-123", "span-456", true)
  let metric_info = ("request.count", 100.0, Some("requests"))
  
  // 测试元组访问
  assert_eq(trace_info.0, "trace-123")
  assert_eq(trace_info.1, "span-456")
  assert_eq(trace_info.2, true)
  
  assert_eq(metric_info.0, "request.count")
  assert_eq(metric_info.1, 100.0)
  match metric_info.2 {
    Some(unit) => assert_eq(unit, "requests")
    None => assert_true(false)
  }
  
  // 测试元组模式匹配
  match trace_info {
    (trace_id, span_id, sampled) => {
      assert_eq(trace_id, "trace-123")
      assert_eq(span_id, "span-456")
      assert_eq(sampled, true)
    }
  }
  
  match metric_info {
    (name, value, unit) => {
      assert_eq(name, "request.count")
      assert_eq(value, 100.0)
      match unit {
        Some(u) => assert_eq(u, "requests")
        None => assert_true(false)
      }
    }
  }
}

// 测试7: 列表和数组操作
test "列表和数组操作测试" {
  let services = ["auth-service", "user-service", "payment-service"]
  let ports = [8080, 8081, 8082]
  
  // 测试数组操作
  assert_eq(services.length(), 3)
  assert_eq(ports.length(), 3)
  
  // 测试数组包含
  assert_true(services.contains("auth-service"))
  assert_true(services.contains("payment-service"))
  assert_false(services.contains("admin-service"))
  
  assert_true(ports.contains(8080))
  assert_true(ports.contains(8082))
  assert_false(ports.contains(9090))
  
  // 测试数组映射
  let upper_services = services.map(fn(s) { s.to_uppercase() })
  assert_eq(upper_services, ["AUTH-SERVICE", "USER-SERVICE", "PAYMENT-SERVICE"])
  
  let doubled_ports = ports.map(fn(p) { p * 2 })
  assert_eq(doubled_ports, [16160, 16162, 16164])
  
  // 测试数组过滤
  let long_services = services.filter(fn(s) { s.length() > 10 })
  assert_eq(long_services.length(), 2)
  assert_true(long_services.contains("user-service"))
  assert_true(long_services.contains("payment-service"))
  
  let high_ports = ports.filter(fn(p) { p > 8080 })
  assert_eq(high_ports, [8081, 8082])
}

// 测试8: 基本数据结构组合
test "基本数据结构组合测试" {
  // 创建服务信息列表
  let services = [
    ("auth-service", 8080, true, Some("1.0.0")),
    ("user-service", 8081, true, Some("1.2.3")),
    ("payment-service", 8082, false, None),
    ("notification-service", 8083, true, Some("2.0.1"))
  ]
  
  // 验证服务数量
  assert_eq(services.length(), 4)
  
  // 验证特定服务信息
  assert_eq(services[0], ("auth-service", 8080, true, Some("1.0.0")))
  assert_eq(services[1], ("user-service", 8081, true, Some("1.2.3")))
  assert_eq(services[2], ("payment-service", 8082, false, None))
  assert_eq(services[3], ("notification-service", 8083, true, Some("2.0.1")))
  
  // 过滤启用的服务
  let enabled_services = services.filter(fn(s) { s.2 })
  assert_eq(enabled_services.length(), 3)
  assert_true(enabled_services.contains(("auth-service", 8080, true, Some("1.0.0"))))
  assert_true(enabled_services.contains(("user-service", 8081, true, Some("1.2.3"))))
  assert_true(enabled_services.contains(("notification-service", 8083, true, Some("2.0.1"))))
  assert_false(enabled_services.contains(("payment-service", 8082, false, None)))
  
  // 提取服务名称
  let service_names = services.map(fn(s) { s.0 })
  assert_eq(service_names, ["auth-service", "user-service", "payment-service", "notification-service"])
  
  // 提取有版本号的服务
  let services_with_version = services.filter(fn(s) { s.3.is_some() })
  assert_eq(services_with_version.length(), 3)
  
  let version_numbers = services_with_version.map(fn(s) { 
    match s.3 {
      Some(v) => v
      None => "unknown"
    }
  })
  assert_eq(version_numbers, ["1.0.0", "1.2.3", "2.0.1"])
}

// 测试9: 基本错误处理
test "基本错误处理测试" {
  let valid_input = "123"
  let invalid_input = "abc"
  
  // 模拟可能失败的操作
  let parse_string_to_int = fn(s : String) -> Option[Int] {
    if s == "123" {
      Some(123)
    } else {
      None
    }
  }
  
  // 测试成功情况
  let valid_result = parse_string_to_int(valid_input)
  match valid_result {
    Some(n) => assert_eq(n, 123)
    None => assert_true(false)
  }
  
  // 测试失败情况
  let invalid_result = parse_string_to_int(invalid_input)
  match invalid_result {
    Some(n) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试默认值处理
  let valid_with_default = valid_result.map_or(0, fn(n) { n })
  assert_eq(valid_with_default, 123)
  
  let invalid_with_default = invalid_result.map_or(0, fn(n) { n })
  assert_eq(invalid_with_default, 0)
}

// 测试10: 基本数据转换
test "基本数据转换测试" {
  let int_values = [1, 2, 3, 4, 5]
  let string_values = ["1", "2", "3", "4", "5"]
  
  // 整数转字符串
  let int_to_string = int_values.map(fn(i) { i.to_string() })
  assert_eq(int_to_string, string_values)
  
  // 字符串转整数（模拟）
  let string_to_int = string_values.map(fn(s) { 
    if s == "1" { 1 }
    else if s == "2" { 2 }
    else if s == "3" { 3 }
    else if s == "4" { 4 }
    else if s == "5" { 5 }
    else { 0 }
  })
  assert_eq(string_to_int, int_values)
  
  // 整数转浮点数
  let int_to_float = int_values.map(fn(i) { i.to_float() })
  assert_eq(int_to_float, [1.0, 2.0, 3.0, 4.0, 5.0])
  
  // 浮点数转整数（模拟）
  let float_to_int = int_to_float.map(fn(f) { f.to_int() })
  assert_eq(float_to_int, int_values)
  
  // 布尔值转字符串
  let bool_values = [true, false, true, false, true]
  let bool_to_string = bool_values.map(fn(b) { 
    if b { "true" } else { "false" }
  })
  assert_eq(bool_to_string, ["true", "false", "true", "false", "true"])
}