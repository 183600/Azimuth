// Azimuth 简单MoonBit测试用例
// 专注于遥测系统的基本功能测试

// 测试1: 字符串操作和验证
test "遥测标识符字符串处理" {
  let trace_id = "trace-12345-67890-abcdef"
  let span_id = "span-98765-43210-fedcba"
  
  // 测试字符串长度
  assert_eq(trace_id.length(), 21)
  assert_eq(span_id.length(), 21)
  
  // 测试字符串包含
  assert_true(trace_id.contains("trace"))
  assert_true(span_id.contains("span"))
  
  // 测试字符串前缀
  assert_true(trace_id.starts_with("trace"))
  assert_true(span_id.starts_with("span"))
  
  // 测试字符串分割
  let trace_parts = trace_id.split("-")
  assert_eq(trace_parts.length(), 4)
  assert_eq(trace_parts[0], "trace")
  assert_eq(trace_parts[1], "12345")
  
  // 测试字符串连接
  let combined = trace_id + "|" + span_id
  assert_true(combined.contains("trace-12345"))
  assert_true(combined.contains("span-98765"))
}

// 测试2: 数组操作和过滤
test "遥测指标数组处理" {
  // 创建测试指标数组
  let metrics = [
    ("cpu.usage", 75.5, "api-service"),
    ("memory.usage", 60.2, "api-service"),
    ("response.time", 120.0, "web-service"),
    ("error.rate", 0.5, "api-service"),
    ("throughput", 1500.0, "web-service")
  ]
  
  // 测试数组长度
  assert_eq(metrics.length(), 5)
  
  // 测试数组过滤 - API服务指标
  let api_metrics = metrics.filter(fn(metric) {
    let (_, _, service) = metric
    service == "api-service"
  })
  assert_eq(api_metrics.length(), 3)
  
  // 测试数组过滤 - 高CPU使用率
  let high_cpu = metrics.filter(fn(metric) {
    let (name, value, _) = metric
    name == "cpu.usage" && value > 70.0
  })
  assert_eq(high_cpu.length(), 1)
  
  // 测试数组映射 - 提取指标名称
  let metric_names = metrics.map(fn(metric) {
    let (name, _, _) = metric
    name
  })
  assert_true(metric_names.contains("cpu.usage"))
  assert_true(metric_names.contains("memory.usage"))
  assert_true(metric_names.contains("response.time"))
  
  // 测试数组归约 - 计算平均值
  let cpu_values = metrics.filter(fn(metric) {
    let (name, _, _) = metric
    name == "cpu.usage"
  }).map(fn(metric) {
    let (_, value, _) = metric
    value
  })
  
  let avg_cpu = if cpu_values.length() > 0 {
    cpu_values.reduce(fn(acc, val) { acc + val }, 0.0) / (cpu_values.length() as Float)
  } else {
    0.0
  }
  
  assert_eq(avg_cpu, 75.5)
}

// 测试3: 选项类型处理
test "遥测数据可选值处理" {
  // 测试有值的情况
  let some_trace_id = Some("trace-12345")
  match some_trace_id {
    Some(id) => assert_eq(id, "trace-12345")
    None => assert_true(false)
  }
  
  // 测试无值的情况
  let no_trace_id = None
  match no_trace_id {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试选项映射
  let upper_trace = some_trace_id.map(fn(id) { id.to_uppercase() })
  match upper_trace {
    Some(id) => assert_eq(id, "TRACE-12345")
    None => assert_true(false)
  }
  
  // 测试选项链式操作
  let processed_trace = some_trace_id
    .map(fn(id) { id.replace("-", "_") })
    .map(fn(id) { id.to_uppercase() })
  
  match processed_trace {
    Some(id) => assert_eq(id, "TRACE_12345")
    None => assert_true(false)
  }
}

// 测试4: 结果类型处理
test "遥测操作结果处理" {
  // 测试成功结果
  let success_result = Ok("telemetry data processed")
  match success_result {
    Ok(message) => assert_eq(message, "telemetry data processed")
    Err(_) => assert_true(false)
  }
  
  // 测试错误结果
  let error_result = Err("processing failed")
  match error_result {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "processing failed")
  }
  
  // 测试结果映射
  let mapped_success = success_result.map(fn(msg) { msg.length() })
  match mapped_success {
    Ok(length) => assert_eq(length, 26)
    Err(_) => assert_true(false)
  }
  
  // 测试结果错误映射
  let mapped_error = error_result.map_err(fn(msg) { "Error: " + msg })
  match mapped_error {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "Error: processing failed")
  }
}

// 测试5: 数值计算和比较
test "遥测指标数值计算" {
  // 测试基本算术运算
  let cpu_usage = 75.5
  let memory_usage = 60.2
  let total_usage = cpu_usage + memory_usage
  assert_eq(total_usage.round(), 136.0)
  
  // 测试百分比计算
  let error_count = 5
  let total_requests = 1000
  let error_rate = (error_count as Float) / (total_requests as Float) * 100.0
  assert_eq(error_rate, 0.5)
  
  // 测试数值比较
  assert_true(cpu_usage > 70.0)
  assert_true(memory_usage < 80.0)
  assert_true(error_rate <= 1.0)
  
  // 测试最大值和最小值
  let response_times = [120.0, 85.5, 200.0, 95.0, 150.0]
  let max_time = response_times.reduce(fn(acc, time) { 
    if time > acc { time } else { acc } 
  }, 0.0)
  assert_eq(max_time, 200.0)
  
  let min_time = response_times.reduce(fn(acc, time) { 
    if time < acc { time } else { acc } 
  }, 999999.0)
  assert_eq(min_time, 85.5)
}

// 测试6: 布尔逻辑和条件判断
test "遥测告警条件判断" {
  // 定义告警条件
  let cpu_high = true
  let memory_high = false
  let error_rate_high = true
  let service_available = true
  
  // 测试基本布尔运算
  assert_true(cpu_high && service_available)
  assert_false(cpu_high && memory_high)
  assert_true(cpu_high || memory_high)
  assert_false(memory_high && error_rate_high)
  
  // 测试复杂条件
  let critical_alert = cpu_high && memory_high && error_rate_high
  assert_false(critical_alert)
  
  let warning_alert = (cpu_high && service_available) || (memory_high && error_rate_high)
  assert_true(warning_alert)
  
  // 测试条件表达式
  let alert_level = if cpu_high && memory_high {
    "critical"
  } else if cpu_high || memory_high || error_rate_high {
    "warning"
  } else {
    "normal"
  }
  
  assert_eq(alert_level, "warning")
  
  // 测试嵌套条件
  let should_notify = if service_available {
    if cpu_high || memory_high || error_rate_high {
      true
    } else {
      false
    }
  } else {
    false
  }
  
  assert_true(should_notify)
}

// 测试7: 元组操作
test "遥测数据元组处理" {
  // 创建元组
  let metric = ("cpu.usage", 75.5, "api-service")
  
  // 测试元组访问
  assert_eq(metric.0, "cpu.usage")
  assert_eq(metric.1, 75.5)
  assert_eq(metric.2, "api-service")
  
  // 测试元组解构
  let (name, value, service) = metric
  assert_eq(name, "cpu.usage")
  assert_eq(value, 75.5)
  assert_eq(service, "api-service")
  
  // 测试元组比较
  let same_metric = ("cpu.usage", 75.5, "api-service")
  let different_metric = ("memory.usage", 60.2, "api-service")
  
  assert_eq(metric.0, same_metric.0)
  assert_eq(metric.1, same_metric.1)
  assert_eq(metric.2, same_metric.2)
  
  assert_not_eq(metric.0, different_metric.0)
  assert_not_eq(metric.1, different_metric.1)
  
  // 测试元组数组
  let metrics = [
    ("cpu.usage", 75.5, "api-service"),
    ("memory.usage", 60.2, "api-service"),
    ("response.time", 120.0, "web-service")
  ]
  
  assert_eq(metrics.length(), 3)
  
  // 测试元组过滤
  let api_metrics = metrics.filter(fn(m) { m.2 == "api-service" })
  assert_eq(api_metrics.length(), 2)
  
  // 测试元组映射
  let metric_names = metrics.map(fn(m) { m.0 })
  assert_true(metric_names.contains("cpu.usage"))
  assert_true(metric_names.contains("memory.usage"))
  assert_true(metric_names.contains("response.time"))
}