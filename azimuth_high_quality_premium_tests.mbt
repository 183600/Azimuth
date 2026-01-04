// Azimuth 高级优质测试套件
// 专注于测试遥测系统的核心功能和边界情况

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化功能" {
  // 1. 定义遥测数据类型
  enum TelemetryValue {
    IntValue(Int)
    FloatValue(Float)
    StringValue(String)
    BoolValue(Bool)
    ArrayValue(Array[TelemetryValue])
  }
  
  // 2. 创建复杂的遥测数据结构
  let original_data = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("request.count", IntValue(1000)),
    ("response.time", FloatValue(125.5)),
    ("error.rate", FloatValue(0.01)),
    ("healthy", BoolValue(true)),
    ("endpoints", ArrayValue([
      StringValue("/api/pay"),
      StringValue("/api/validate"),
      StringValue("/api/status")
    ])),
    ("metrics", ArrayValue([
      IntValue(100),
      IntValue(200),
      IntValue(300)
    ]))
  ]
  
  // 3. 模拟序列化过程
  let serialize_telemetry_value = fn(value: TelemetryValue) {
    match value {
      IntValue(v) => "int:" + v.to_string()
      FloatValue(v) => "float:" + v.to_string()
      StringValue(v) => "string:" + v
      BoolValue(v) => "bool:" + (if v { "true" } else { "false" })
      ArrayValue(arr) => {
        let serialized_elements = arr.map(serialize_telemetry_value)
        "array:[" + serialized_elements.join(",") + "]"
      }
    }
  }
  
  // 4. 模拟反序列化过程
  let deserialize_telemetry_value = fn(serialized: String) {
    if serialized.starts_with("int:") {
      let number_str = serialized.substring(4, serialized.length() - 4)
      IntValue(number_str.to_int())
    } else if serialized.starts_with("float:") {
      let number_str = serialized.substring(6, serialized.length() - 6)
      FloatValue(number_str.to_float())
    } else if serialized.starts_with("string:") {
      let str_value = serialized.substring(7, serialized.length() - 7)
      StringValue(str_value)
    } else if serialized.starts_with("bool:") {
      let bool_str = serialized.substring(5, serialized.length() - 5)
      BoolValue(bool_str == "true")
    } else if serialized.starts_with("array:[") {
      let inner = serialized.substring(7, serialized.length() - 8)
      if inner.length() == 0 {
        ArrayValue([])
      } else {
        // 简化的数组反序列化
        ArrayValue([IntValue(1)])  // 简化实现
      }
    } else {
      StringValue("unknown")
    }
  }
  
  // 5. 执行序列化
  let serialized_data = original_data.map(fn(item) {
    (item.0, serialize_telemetry_value(item.1))
  })
  
  // 6. 验证序列化结果
  assert_eq(serialized_data.length(), original_data.length())
  assert_eq(serialized_data[0].0, "service.name")
  assert_eq(serialized_data[0].1, "string:payment-service")
  assert_eq(serialized_data[2].0, "request.count")
  assert_eq(serialized_data[2].1, "int:1000")
  
  // 7. 执行反序列化
  let deserialized_data = serialized_data.map(fn(item) {
    (item.0, deserialize_telemetry_value(item.1))
  })
  
  // 8. 验证反序列化结果
  assert_eq(deserialized_data.length(), original_data.length())
  
  // 验证具体值
  match deserialized_data[0].1 {
    StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  match deserialized_data[2].1 {
    IntValue(v) => assert_eq(v, 1000)
    _ => assert_true(false)
  }
  
  match deserialized_data[3].1 {
    FloatValue(v) => assert_true(v > 125.0 && v < 126.0)
    _ => assert_true(false)
  }
  
  match deserialized_data[4].1 {
    FloatValue(v) => assert_true(v > 0.009 && v < 0.011)
    _ => assert_true(false)
  }
  
  match deserialized_data[5].1 {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

// 测试2: 跨服务上下文传播
test "跨服务上下文传播机制" {
  // 1. 定义上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    trace_flags: Int
  }
  
  // 2. 创建初始上下文
  let initial_context = TraceContext({
    trace_id: "trace-1234567890",
    span_id: "span-1111111111",
    parent_span_id: None,
    baggage: [
      ("user.id", "user-12345"),
      ("request.id", "req-67890"),
      ("service.version", "1.2.3")
    ],
    trace_flags: 1
  })
  
  // 3. 模拟服务A创建子上下文
  let service_a_context = TraceContext({
    trace_id: initial_context.trace_id,  // 继承追踪ID
    span_id: "span-2222222222",          // 新的跨度ID
    parent_span_id: Some(initial_context.span_id),  // 设置父跨度ID
    baggage: initial_context.baggage,    // 继承行李属性
    trace_flags: initial_context.trace_flags  // 继承追踪标志
  })
  
  // 4. 服务A添加新的行李属性
  let service_a_context_with_baggage = TraceContext({
    trace_id: service_a_context.trace_id,
    span_id: service_a_context.span_id,
    parent_span_id: service_a_context.parent_span_id,
    baggage: service_a_context.baggage.push(("service.a.operation", "process_payment")),
    trace_flags: service_a_context.trace_flags
  })
  
  // 5. 模拟服务B创建子上下文
  let service_b_context = TraceContext({
    trace_id: service_a_context_with_baggage.trace_id,  // 继承追踪ID
    span_id: "span-3333333333",                         // 新的跨度ID
    parent_span_id: Some(service_a_context_with_baggage.span_id),  // 设置父跨度ID
    baggage: service_a_context_with_baggage.baggage,    // 继承行李属性
    trace_flags: service_a_context_with_baggage.trace_flags  // 继承追踪标志
  })
  
  // 6. 服务B添加新的行李属性
  let service_b_context_with_baggage = TraceContext({
    trace_id: service_b_context.trace_id,
    span_id: service_b_context.span_id,
    parent_span_id: service_b_context.parent_span_id,
    baggage: service_b_context.baggage.push(("service.b.operation", "validate_payment")),
    trace_flags: service_b_context.trace_flags
  })
  
  // 7. 验证上下文传播
  assert_eq(initial_context.trace_id, service_a_context_with_baggage.trace_id)
  assert_eq(initial_context.trace_id, service_b_context_with_baggage.trace_id)
  
  // 8. 验证父子关系
  assert_eq(service_a_context_with_baggage.parent_span_id, Some(initial_context.span_id))
  assert_eq(service_b_context_with_baggage.parent_span_id, Some(service_a_context_with_baggage.span_id))
  
  // 9. 验证行李属性传播
  assert_eq(service_b_context_with_baggage.baggage.length(), 5)  // 3个初始 + 2个新增
  
  // 验证初始行李属性存在
  let find_baggage_item = fn(baggage: Array[(String, String)], key: String) {
    let mut found = None
    for (k, v) in baggage {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  assert_eq(find_baggage_item(service_b_context_with_baggage.baggage, "user.id"), Some("user-12345"))
  assert_eq(find_baggage_item(service_b_context_with_baggage.baggage, "request.id"), Some("req-67890"))
  assert_eq(find_baggage_item(service_b_context_with_baggage.baggage, "service.version"), Some("1.2.3"))
  
  // 验证新增行李属性
  assert_eq(find_baggage_item(service_b_context_with_baggage.baggage, "service.a.operation"), Some("process_payment"))
  assert_eq(find_baggage_item(service_b_context_with_baggage.baggage, "service.b.operation"), Some("validate_payment"))
  
  // 10. 验证追踪标志传播
  assert_eq(initial_context.trace_flags, service_a_context_with_baggage.trace_flags)
  assert_eq(initial_context.trace_flags, service_b_context_with_baggage.trace_flags)
}

// 测试3: 内存泄漏检测
test "内存泄漏检测和资源清理" {
  // 1. 定义资源管理器
  type ResourceManager = {
    allocated_resources: Array[String],
    max_resources: Int,
    cleanup_called: Bool
  }
  
  // 2. 创建资源管理器
  let resource_manager = ResourceManager({
    allocated_resources: [],
    max_resources: 100,
    cleanup_called: false
  })
  
  // 3. 模拟资源分配函数
  let allocate_resource = fn(manager: ResourceManager, resource_id: String) {
    if manager.allocated_resources.length() < manager.max_resources {
      ResourceManager({
        allocated_resources: manager.allocated_resources.push(resource_id),
        max_resources: manager.max_resources,
        cleanup_called: manager.cleanup_called
      })
    } else {
      manager  // 资源达到上限，不分配
    }
  }
  
  // 4. 模拟资源释放函数
  let release_resource = fn(manager: ResourceManager, resource_id: String) {
    let filtered_resources = manager.allocated_resources.filter(fn(id) { id != resource_id })
    ResourceManager({
      allocated_resources: filtered_resources,
      max_resources: manager.max_resources,
      cleanup_called: manager.cleanup_called
    })
  }
  
  // 5. 模拟资源清理函数
  let cleanup_resources = fn(manager: ResourceManager) {
    ResourceManager({
      allocated_resources: [],
      max_resources: manager.max_resources,
      cleanup_called: true
    })
  }
  
  // 6. 分配资源
  let manager1 = allocate_resource(resource_manager, "resource-1")
  let manager2 = allocate_resource(manager1, "resource-2")
  let manager3 = allocate_resource(manager2, "resource-3")
  
  // 7. 验证资源分配
  assert_eq(manager3.allocated_resources.length(), 3)
  assert_true(manager3.allocated_resources.contains("resource-1"))
  assert_true(manager3.allocated_resources.contains("resource-2"))
  assert_true(manager3.allocated_resources.contains("resource-3"))
  assert_false(manager3.cleanup_called)
  
  // 8. 释放部分资源
  let manager4 = release_resource(manager3, "resource-2")
  
  // 9. 验证部分资源释放
  assert_eq(manager4.allocated_resources.length(), 2)
  assert_true(manager4.allocated_resources.contains("resource-1"))
  assert_false(manager4.allocated_resources.contains("resource-2"))
  assert_true(manager4.allocated_resources.contains("resource-3"))
  assert_false(manager4.cleanup_called)
  
  // 10. 执行资源清理
  let manager5 = cleanup_resources(manager4)
  
  // 11. 验证资源清理
  assert_eq(manager5.allocated_resources.length(), 0)
  assert_true(manager5.cleanup_called)
  
  // 12. 模拟内存泄漏检测
  let detect_memory_leak = fn(manager_before: ResourceManager, manager_after: ResourceManager) {
    let leak_detected = manager_before.allocated_resources.length() > 0 && 
                       manager_after.allocated_resources.length() > 0 &&
                       manager_before.allocated_resources.length() == manager_after.allocated_resources.length()
    leak_detected
  }
  
  // 13. 测试内存泄漏检测
  let leak_before_cleanup = detect_memory_leak(manager4, manager4)  // 没有清理，应该检测到泄漏
  let leak_after_cleanup = detect_memory_leak(manager5, manager5)   // 已清理，不应该检测到泄漏
  
  assert_true(leak_before_cleanup)
  assert_false(leak_after_cleanup)
}

// 测试4: 异常恢复机制
test "异常恢复和容错机制" {
  // 1. 定义异常类型
  enum TelemetryException {
    NetworkError(String)
    SerializationError(String)
    TimeoutError(Int)
    ResourceExhaustedError(String)
    UnknownError(String)
  }
  
  // 2. 定义操作结果类型
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryException],
    retry_count: Int
  }
  
  // 3. 创建成功结果
  let create_success = fn(data: T) {
    OperationResult({
      success: true,
      data: Some(data),
      error: None,
      retry_count: 0
    })
  }
  
  // 4. 创建错误结果
  let create_error = fn(error: TelemetryException, retry_count: Int) {
    OperationResult({
      success: false,
      data: None,
      error: Some(error),
      retry_count
    })
  }
  
  // 5. 模拟可重试操作
  let retryable_operation = fn(attempt: Int) {
    match attempt {
      1 => create_error(TelemetryException::NetworkError("Connection refused"), 1)
      2 => create_error(TelemetryException::TimeoutError(5000), 2)
      3 => create_success("Operation completed successfully")
      _ => create_error(TelemetryException::UnknownError("Unexpected error"), attempt)
    }
  }
  
  // 6. 实现重试机制
  let execute_with_retry = fn(operation: Int -> OperationResult[String], max_retries: Int) {
    let mut result = operation(1)
    let mut attempt = 1
    
    while not(result.success) and attempt < max_retries {
      attempt = attempt + 1
      result = operation(attempt)
    }
    
    result
  }
  
  // 7. 执行带重试的操作
  let retry_result = execute_with_retry(retryable_operation, 5)
  
  // 8. 验证重试结果
  assert_true(retry_result.success)
  assert_eq(retry_result.data, Some("Operation completed successfully"))
  assert_eq(retry_result.error, None)
  assert_eq(retry_result.retry_count, 0)  // 成功时重试计数为0
  
  // 9. 测试重试次数限制
  let always_failing_operation = fn(attempt: Int) {
    create_error(TelemetryException::NetworkError("Persistent failure"), attempt)
  }
  
  let failure_result = execute_with_retry(always_failing_operation, 3)
  
  // 10. 验证失败结果
  assert_false(failure_result.success)
  assert_eq(failure_result.data, None)
  assert_true(failure_result.error.is_some())
  
  match failure_result.error {
    Some(TelemetryException::NetworkError(msg)) => assert_eq(msg, "Persistent failure")
    _ => assert_true(false)
  }
  
  // 11. 测试断路器模式
  type CircuitBreaker = {
    failure_count: Int,
    failure_threshold: Int,
    state: String  // "closed", "open", "half-open"
    last_failure_time: Int
  }
  
  // 12. 创建断路器
  let circuit_breaker = CircuitBreaker({
    failure_count: 0,
    failure_threshold: 3,
    state: "closed",
    last_failure_time: 0
  })
  
  // 13. 模拟断路器操作
  let circuit_breaker_operation = fn(breaker: CircuitBreaker, operation_success: Bool) {
    match breaker.state {
      "open" => {
        // 断路器打开，直接返回失败
        (breaker, create_error(TelemetryException::ResourceExhaustedError("Circuit breaker is open"), 0))
      }
      "closed" => {
        if operation_success {
          // 操作成功，重置失败计数
          let reset_breaker = CircuitBreaker({
            failure_count: 0,
            failure_threshold: breaker.failure_threshold,
            state: "closed",
            last_failure_time: 0
          })
          (reset_breaker, create_success("Operation succeeded"))
        } else {
          // 操作失败，增加失败计数
          let new_failure_count = breaker.failure_count + 1
          let new_state = if new_failure_count >= breaker.failure_threshold { "open" } else { "closed" }
          let updated_breaker = CircuitBreaker({
            failure_count: new_failure_count,
            failure_threshold: breaker.failure_threshold,
            state: new_state,
            last_failure_time: 1640995200  // 当前时间戳
          })
          (updated_breaker, create_error(TelemetryException::NetworkError("Operation failed"), 0))
        }
      }
      _ => (breaker, create_error(TelemetryException::UnknownError("Invalid circuit breaker state"), 0))
    }
  }
  
  // 14. 测试断路器状态转换
  let (breaker1, result1) = circuit_breaker_operation(circuit_breaker, false)  // 第一次失败
  assert_false(result1.success)
  assert_eq(breaker1.failure_count, 1)
  assert_eq(breaker1.state, "closed")
  
  let (breaker2, result2) = circuit_breaker_operation(breaker1, false)  // 第二次失败
  assert_false(result2.success)
  assert_eq(breaker2.failure_count, 2)
  assert_eq(breaker2.state, "closed")
  
  let (breaker3, result3) = circuit_breaker_operation(breaker2, false)  // 第三次失败，触发断路器打开
  assert_false(result3.success)
  assert_eq(breaker3.failure_count, 3)
  assert_eq(breaker3.state, "open")
  
  let (breaker4, result4) = circuit_breaker_operation(breaker3, true)   // 断路器已打开，即使操作成功也被拒绝
  assert_false(result4.success)
  match result4.error {
    Some(TelemetryException::ResourceExhaustedError(msg)) => assert_eq(msg, "Circuit breaker is open")
    _ => assert_true(false)
  }
  assert_eq(breaker4.state, "open")  // 状态不变
}

// 测试5: 数据完整性验证
test "数据完整性验证和校验机制" {
  // 1. 定义数据校验器类型
  type DataValidator = {
    name: String,
    validate: String -> Bool,
    error_message: String
  }
  
  // 2. 创建校验器集合
  let validators = [
    DataValidator({
      name: "non_empty",
      validate: fn(s) { s.length() > 0 },
      error_message: "String cannot be empty"
    }),
    DataValidator({
      name: "max_length_100",
      validate: fn(s) { s.length() <= 100 },
      error_message: "String length cannot exceed 100 characters"
    }),
    DataValidator({
      name: "alphanumeric",
      validate: fn(s) {
        let mut is_valid = true
        for i in 0..s.length() {
          let c = s[i]
          if not((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')) {
            is_valid = false
          }
        }
        is_valid
      },
      error_message: "String must contain only alphanumeric characters"
    }),
    DataValidator({
      name: "no_special_chars",
      validate: fn(s) { not(s.contains("@") or s.contains("#") or s.contains("$")) },
      error_message: "String cannot contain special characters"
    })
  ]
  
  // 3. 定义校验结果类型
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String]
  }
  
  // 4. 实现校验函数
  let validate_string = fn(input: String, validator_list: Array[DataValidator]) {
    let mut errors = []
    
    for validator in validator_list {
      if not(validator.validate(input)) {
        errors = errors.push(validator.error_message)
      }
    }
    
    ValidationResult({
      is_valid: errors.length() == 0,
      errors
    })
  }
  
  // 5. 测试有效数据
  let valid_input = "ValidTrace123"
  let valid_result = validate_string(valid_input, validators)
  
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 6. 测试无效数据 - 空字符串
  let empty_input = ""
  let empty_result = validate_string(empty_input, validators)
  
  assert_false(empty_result.is_valid)
  assert_eq(empty_result.errors.length(), 2)  // non_empty 和 alphanumeric 校验失败
  
  // 7. 测试无效数据 - 超长字符串
  let long_input = "a".repeat(101)
  let long_result = validate_string(long_input, validators)
  
  assert_false(long_result.is_valid)
  assert_eq(long_result.errors.length(), 1)  // max_length_100 校验失败
  assert_true(long_result.errors.contains("String length cannot exceed 100 characters"))
  
  // 8. 测试无效数据 - 包含特殊字符
  let special_input = "Trace@123"
  let special_result = validate_string(special_input, validators)
  
  assert_false(special_result.is_valid)
  assert_eq(special_result.errors.length(), 1)  // no_special_chars 校验失败
  assert_true(special_result.errors.contains("String cannot contain special characters"))
  
  // 9. 测试复合校验错误
  let complex_invalid = "@"
  let complex_result = validate_string(complex_invalid, validators)
  
  assert_false(complex_result.is_valid)
  assert_eq(complex_result.errors.length(), 3)  // non_empty, alphanumeric, no_special_chars 校验失败
  
  // 10. 实现数据校验和修复
  type DataRepairResult = {
    original: String,
    repaired: String,
    repairs_applied: Array[String]
  }
  
  let repair_string = fn(input: String) {
    let mut repaired = input
    let mut repairs = []
    
    // 修复空字符串
    if repaired.length() == 0 {
      repaired = "default"
      repairs = repairs.push("Replaced empty string with default")
    }
    
    // 修复超长字符串
    if repaired.length() > 100 {
      repaired = repaired.substring(0, 100)
      repairs = repairs.push("Truncated string to maximum length")
    }
    
    // 移除特殊字符
    let has_special_chars = repaired.contains("@") or repaired.contains("#") or repaired.contains("$")
    if has_special_chars {
      repaired = repaired.replace("@", "").replace("#", "").replace("$", "")
      repairs = repairs.push("Removed special characters")
    }
    
    DataRepairResult({
      original: input,
      repaired,
      repairs_applied: repairs
    })
  }
  
  // 11. 测试数据修复
  let repair_result1 = repair_string("")
  assert_eq(repair_result1.original, "")
  assert_eq(repair_result1.repaired, "default")
  assert_eq(repair_result1.repairs_applied.length(), 1)
  
  let repair_result2 = repair_string("Trace@123#")
  assert_eq(repair_result2.original, "Trace@123#")
  assert_eq(repair_result2.repaired, "Trace123")
  assert_eq(repair_result2.repairs_applied.length(), 1)
  
  let repair_result3 = repair_string("a".repeat(105) + "@")
  assert_eq(repair_result3.original.length(), 106)
  assert_eq(repair_result3.repaired.length(), 100)
  assert_eq(repair_result3.repairs_applied.length(), 2)
  
  // 12. 验证修复后的数据
  let repaired_valid_result = validate_string(repair_result3.repaired, validators)
  assert_true(repaired_valid_result.is_valid)
}

// 测试6: 实时流处理
test "实时流处理和数据分析" {
  // 1. 定义遥测事件类型
  enum TelemetryEvent {
    MetricEvent(String, Float)  // (metric_name, value)
    LogEvent(String, String)    // (log_level, message)
    TraceEvent(String, String)  // (trace_id, span_id)
    ErrorEvent(String, String)  // (error_type, error_message)
  }
  
  // 2. 创建事件流
  let event_stream = [
    TelemetryEvent::MetricEvent("cpu.usage", 0.65),
    TelemetryEvent::MetricEvent("memory.usage", 0.78),
    TelemetryEvent::LogEvent("INFO", "Service started"),
    TelemetryEvent::TraceEvent("trace-123", "span-456"),
    TelemetryEvent::MetricEvent("cpu.usage", 0.72),
    TelemetryEvent::MetricEvent("memory.usage", 0.82),
    TelemetryEvent::ErrorEvent("TimeoutError", "Database connection timeout"),
    TelemetryEvent::LogEvent("WARN", "High memory usage detected"),
    TelemetryEvent::MetricEvent("cpu.usage", 0.68),
    TelemetryEvent::MetricEvent("memory.usage", 0.75),
    TelemetryEvent::LogEvent("INFO", "Service recovered"),
    TelemetryEvent::TraceEvent("trace-123", "span-789")
  ]
  
  // 3. 定义流处理器
  type StreamProcessor[T] = {
    filter: TelemetryEvent -> Bool,
    transform: TelemetryEvent -> T,
    aggregate: Array[T] -> T
  }
  
  // 4. 实现流处理函数
  let process_stream = fn(events: Array[TelemetryEvent], processor: StreamProcessor[T]) {
    let filtered_events = events.filter(processor.filter)
    let transformed_events = filtered_events.map(processor.transform)
    processor.aggregate(transformed_events)
  }
  
  // 5. 处理CPU使用率指标
  let cpu_processor = StreamProcessor({
    filter: fn(event) {
      match event {
        TelemetryEvent::MetricEvent(name, _) => name == "cpu.usage"
        _ => false
      }
    },
    transform: fn(event) {
      match event {
        TelemetryEvent::MetricEvent(_, value) => value
        _ => 0.0
      }
    },
    aggregate: fn(values) {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      sum / values.length().to_float()
    }
  })
  
  let cpu_average = process_stream(event_stream, cpu_processor)
  assert_true(cpu_average > 0.68 && cpu_average < 0.69)  // (0.65 + 0.72 + 0.68) / 3
  
  // 6. 处理内存使用率指标
  let memory_processor = StreamProcessor({
    filter: fn(event) {
      match event {
        TelemetryEvent::MetricEvent(name, _) => name == "memory.usage"
        _ => false
      }
    },
    transform: fn(event) {
      match event {
        TelemetryEvent::MetricEvent(_, value) => value
        _ => 0.0
      }
    },
    aggregate: fn(values) {
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, 0.0)
      max
    }
  })
  
  let memory_max = process_stream(event_stream, memory_processor)
  assert_eq(memory_max, 0.82)  // 最大值为0.82
  
  // 7. 处理日志事件
  type LogSummary = {
    info_count: Int,
    warn_count: Int,
    error_count: Int
  }
  
  let log_processor = StreamProcessor({
    filter: fn(event) {
      match event {
        TelemetryEvent::LogEvent(_, _) => true
        _ => false
      }
    },
    transform: fn(event) {
      match event {
        TelemetryEvent::LogEvent(level, _) => level
        _ => "UNKNOWN"
      }
    },
    aggregate: fn(levels) {
      let mut summary = LogSummary({ info_count: 0, warn_count: 0, error_count: 0 })
      
      for level in levels {
        match level {
          "INFO" => summary = { summary | info_count: summary.info_count + 1 }
          "WARN" => summary = { summary | warn_count: summary.warn_count + 1 }
          "ERROR" => summary = { summary | error_count: summary.error_count + 1 }
          _ => {}  // 忽略未知级别
        }
      }
      
      summary
    }
  })
  
  let log_summary = process_stream(event_stream, log_processor)
  assert_eq(log_summary.info_count, 2)
  assert_eq(log_summary.warn_count, 1)
  assert_eq(log_summary.error_count, 0)
  
  // 8. 处理错误事件
  let error_processor = StreamProcessor({
    filter: fn(event) {
      match event {
        TelemetryEvent::ErrorEvent(_, _) => true
        _ => false
      }
    },
    transform: fn(event) {
      match event {
        TelemetryEvent::ErrorEvent(error_type, _) => error_type
        _ => "UnknownError"
      }
    },
    aggregate: fn(error_types) {
      error_types
    }
  })
  
  let error_types = process_stream(event_stream, error_processor)
  assert_eq(error_types.length(), 1)
  assert_eq(error_types[0], "TimeoutError")
  
  // 9. 处理追踪事件
  type TraceSummary = {
    trace_ids: Array[String],
    span_count: Int
  }
  
  let trace_processor = StreamProcessor({
    filter: fn(event) {
      match event {
        TelemetryEvent::TraceEvent(_, _) => true
        _ => false
      }
    },
    transform: fn(event) {
      match event {
        TelemetryEvent::TraceEvent(trace_id, span_id) => (trace_id, span_id)
        _ => ("", "")
      }
    },
    aggregate: fn(trace_spans) {
      let mut trace_ids = []
      for (trace_id, _) in trace_spans {
        if not(trace_ids.contains(trace_id)) {
          trace_ids = trace_ids.push(trace_id)
        }
      }
      
      TraceSummary({
        trace_ids,
        span_count: trace_spans.length()
      })
    }
  })
  
  let trace_summary = process_stream(event_stream, trace_processor)
  assert_eq(trace_summary.trace_ids.length(), 1)
  assert_eq(trace_summary.trace_ids[0], "trace-123")
  assert_eq(trace_summary.span_count, 2)
  
  // 10. 实现窗口化流处理
  type WindowedResult[T] = {
    window_start: Int,
    window_end: Int,
    results: Array[T]
  }
  
  let process_windowed_stream = fn(events: Array[TelemetryEvent], window_size: Int, processor: StreamProcessor[T]) {
    let mut results = []
    
    for i in 0..events.length() {
      if i + window_size <= events.length() {
        let window = events.slice(i, i + window_size)
        let window_result = process_stream(window, processor)
        results = results.push(WindowedResult({
          window_start: i,
          window_end: i + window_size - 1,
          results: [window_result]
        }))
      }
    }
    
    results
  }
  
  // 11. 测试窗口化处理
  let windowed_cpu_results = process_windowed_stream(event_stream, 3, cpu_processor)
  assert_eq(windowed_cpu_results.length(), 10)  // 12个事件，窗口大小3，有10个窗口
  
  // 验证第一个窗口的结果
  let first_window = windowed_cpu_results[0]
  assert_eq(first_window.window_start, 0)
  assert_eq(first_window.window_end, 2)
  assert_eq(first_window.results.length(), 1)
  assert_eq(first_window.results[0], 0.65)  // 第一个窗口只有0.65一个CPU指标
}

// 测试7: 资源限制处理
test "资源限制处理和优雅降级" {
  // 1. 定义资源限制配置
  type ResourceLimits = {
    max_memory_mb: Int,
    max_cpu_percent: Int,
    max_disk_space_mb: Int,
    max_network_requests_per_second: Int
  }
  
  // 2. 定义资源使用情况
  type ResourceUsage = {
    memory_mb: Int,
    cpu_percent: Int,
    disk_space_mb: Int,
    network_requests_per_second: Int
  }
  
  // 3. 定义降级策略
  enum DegradationStrategy {
    NoOp           // 不执行任何操作
    ReduceSampling // 降低采样率
    CacheOnly      // 只使用缓存
    RejectRequests // 拒绝新请求
    Shutdown       // 关闭服务
  }
  
  // 4. 创建资源限制配置
  let limits = ResourceLimits({
    max_memory_mb: 1024,
    max_cpu_percent: 80,
    max_disk_space_mb: 2048,
    max_network_requests_per_second: 1000
  })
  
  // 5. 实现资源监控函数
  let check_resource_limits = fn(usage: ResourceUsage, limits: ResourceLimits) {
    let memory_exceeded = usage.memory_mb > limits.max_memory_mb
    let cpu_exceeded = usage.cpu_percent > limits.max_cpu_percent
    let disk_exceeded = usage.disk_space_mb > limits.max_disk_space_mb
    let network_exceeded = usage.network_requests_per_second > limits.max_network_requests_per_second
    
    {
      memory_exceeded,
      cpu_exceeded,
      disk_exceeded,
      network_exceeded,
      any_exceeded: memory_exceeded or cpu_exceeded or disk_exceeded or network_exceeded
    }
  }
  
  // 6. 实现降级策略选择函数
  let select_degradation_strategy = fn(exceeded_resources: { memory_exceeded: Bool, cpu_exceeded: Bool, disk_exceeded: Bool, network_exceeded: Bool, any_exceeded: Bool }) {
    match (exceeded_resources.memory_exceeded, exceeded_resources.cpu_exceeded, exceeded_resources.disk_exceeded, exceeded_resources.network_exceeded) {
      (true, true, _, _) => DegradationStrategy::RejectRequests  // 内存和CPU都超限
      (true, _, _, _) => DegradationStrategy::CacheOnly         // 内存超限
      (_, true, _, _) => DegradationStrategy::ReduceSampling    // CPU超限
      (_, _, true, _) => DegradationStrategy::RejectRequests    // 磁盘超限
      (_, _, _, true) => DegradationStrategy::ReduceSampling    // 网络超限
      (_, _, _, _) => DegradationStrategy::NoOp                 // 无超限
    }
  }
  
  // 7. 测试正常资源使用情况
  let normal_usage = ResourceUsage({
    memory_mb: 512,
    cpu_percent: 40,
    disk_space_mb: 1024,
    network_requests_per_second: 500
  })
  
  let normal_check = check_resource_limits(normal_usage, limits)
  assert_false(normal_check.any_exceeded)
  assert_false(normal_check.memory_exceeded)
  assert_false(normal_check.cpu_exceeded)
  assert_false(normal_check.disk_exceeded)
  assert_false(normal_check.network_exceeded)
  
  let normal_strategy = select_degradation_strategy(normal_check)
  match normal_strategy {
    DegradationStrategy::NoOp => assert_true(true)
    _ => assert_true(false)
  }
  
  // 8. 测试内存超限情况
  let memory_exceeded_usage = ResourceUsage({
    memory_mb: 1536,  // 超过1024MB限制
    cpu_percent: 40,
    disk_space_mb: 1024,
    network_requests_per_second: 500
  })
  
  let memory_check = check_resource_limits(memory_exceeded_usage, limits)
  assert_true(memory_check.any_exceeded)
  assert_true(memory_check.memory_exceeded)
  assert_false(memory_check.cpu_exceeded)
  assert_false(memory_check.disk_exceeded)
  assert_false(memory_check.network_exceeded)
  
  let memory_strategy = select_degradation_strategy(memory_check)
  match memory_strategy {
    DegradationStrategy::CacheOnly => assert_true(true)
    _ => assert_true(false)
  }
  
  // 9. 测试CPU超限情况
  let cpu_exceeded_usage = ResourceUsage({
    memory_mb: 512,
    cpu_percent: 90,  // 超过80%限制
    disk_space_mb: 1024,
    network_requests_per_second: 500
  })
  
  let cpu_check = check_resource_limits(cpu_exceeded_usage, limits)
  assert_true(cpu_check.any_exceeded)
  assert_false(cpu_check.memory_exceeded)
  assert_true(cpu_check.cpu_exceeded)
  assert_false(cpu_check.disk_exceeded)
  assert_false(cpu_check.network_exceeded)
  
  let cpu_strategy = select_degradation_strategy(cpu_check)
  match cpu_strategy {
    DegradationStrategy::ReduceSampling => assert_true(true)
    _ => assert_true(false)
  }
  
  // 10. 测试多重超限情况
  let multiple_exceeded_usage = ResourceUsage({
    memory_mb: 1536,  // 超过1024MB限制
    cpu_percent: 90,   // 超过80%限制
    disk_space_mb: 1024,
    network_requests_per_second: 500
  })
  
  let multiple_check = check_resource_limits(multiple_exceeded_usage, limits)
  assert_true(multiple_check.any_exceeded)
  assert_true(multiple_check.memory_exceeded)
  assert_true(multiple_check.cpu_exceeded)
  assert_false(multiple_check.disk_exceeded)
  assert_false(multiple_check.network_exceeded)
  
  let multiple_strategy = select_degradation_strategy(multiple_check)
  match multiple_strategy {
    DegradationStrategy::RejectRequests => assert_true(true)
    _ => assert_true(false)
  }
  
  // 11. 实现降级操作执行函数
  type DegradationResult = {
    strategy: DegradationStrategy,
    success: Bool,
    message: String
  }
  
  let execute_degradation_strategy = fn(strategy: DegradationStrategy) {
    match strategy {
      DegradationStrategy::NoOp => {
        DegradationResult({
          strategy,
          success: true,
          message: "No degradation needed"
        })
      }
      DegradationStrategy::ReduceSampling => {
        DegradationResult({
          strategy,
          success: true,
          message: "Reduced sampling rate from 100% to 50%"
        })
      }
      DegradationStrategy::CacheOnly => {
        DegradationResult({
          strategy,
          success: true,
          message: "Switched to cache-only mode"
        })
      }
      DegradationStrategy::RejectRequests => {
        DegradationResult({
          strategy,
          success: true,
          message: "Rejecting new requests, serving cached responses"
        })
      }
      DegradationStrategy::Shutdown => {
        DegradationResult({
          strategy,
          success: true,
          message: "Initiating graceful shutdown"
        })
      }
    }
  }
  
  // 12. 测试降级操作执行
  let memory_degradation = execute_degradation_strategy(memory_strategy)
  assert_true(memory_degradation.success)
  assert_eq(memory_degradation.message, "Switched to cache-only mode")
  
  let cpu_degradation = execute_degradation_strategy(cpu_strategy)
  assert_true(cpu_degradation.success)
  assert_eq(cpu_degradation.message, "Reduced sampling rate from 100% to 50%")
  
  let multiple_degradation = execute_degradation_strategy(multiple_strategy)
  assert_true(multiple_degradation.success)
  assert_eq(multiple_degradation.message, "Rejecting new requests, serving cached responses")
}

// 测试8: 多租户隔离
test "多租户隔离和数据安全" {
  // 1. 定义租户类型
  type Tenant = {
    id: String,
    name: String,
    tier: String,  // "basic", "premium", "enterprise"
    resource_limits: TenantResourceLimits
  }
  
  // 2. 定义租户资源限制
  type TenantResourceLimits = {
    max_spans_per_second: Int,
    max_metrics_per_minute: Int,
    max_storage_mb: Int,
    max_retention_days: Int
  }
  
  // 3. 定义租户数据
  type TenantData = {
    tenant_id: String,
    data_type: String,  // "span", "metric", "log"
    data: String,
    timestamp: Int
  }
  
  // 4. 创建租户
  let tenants = [
    Tenant({
      id: "tenant-001",
      name: "Acme Corp",
      tier: "enterprise",
      resource_limits: TenantResourceLimits({
        max_spans_per_second: 10000,
        max_metrics_per_minute: 60000,
        max_storage_mb: 10240,
        max_retention_days: 90
      })
    }),
    Tenant({
      id: "tenant-002",
      name: "Startup Inc",
      tier: "premium",
      resource_limits: TenantResourceLimits({
        max_spans_per_second: 1000,
        max_metrics_per_minute: 6000,
        max_storage_mb: 1024,
        max_retention_days: 30
      })
    }),
    Tenant({
      id: "tenant-003",
      name: "Small Business",
      tier: "basic",
      resource_limits: TenantResourceLimits({
        max_spans_per_second: 100,
        max_metrics_per_minute: 600,
        max_storage_mb: 100,
        max_retention_days: 7
      })
    })
  ]
  
  // 5. 创建租户数据
  let tenant_data = [
    TenantData({
      tenant_id: "tenant-001",
      data_type: "span",
      data: "trace-123-span-456",
      timestamp: 1640995200
    }),
    TenantData({
      tenant_id: "tenant-002",
      data_type: "metric",
      data: "cpu.usage:0.75",
      timestamp: 1640995201
    }),
    TenantData({
      tenant_id: "tenant-001",
      data_type: "log",
      data: "INFO: Service started",
      timestamp: 1640995202
    }),
    TenantData({
      tenant_id: "tenant-003",
      data_type: "span",
      data: "trace-789-span-012",
      timestamp: 1640995203
    }),
    TenantData({
      tenant_id: "tenant-002",
      data_type: "span",
      data: "trace-345-span-678",
      timestamp: 1640995204
    })
  ]
  
  // 6. 实现租户查找函数
  let find_tenant = fn(tenant_id: String, tenant_list: Array[Tenant]) {
    let mut found = None
    for tenant in tenant_list {
      if tenant.id == tenant_id {
        found = Some(tenant)
      }
    }
    found
  }
  
  // 7. 实现数据隔离函数
  let filter_data_by_tenant = fn(data_list: Array[TenantData], tenant_id: String) {
    data_list.filter(fn(data) { data.tenant_id == tenant_id })
  }
  
  // 8. 测试租户查找
  let tenant1 = find_tenant("tenant-001", tenants)
  assert_true(tenant1.is_some())
  match tenant1 {
    Some(t) => {
      assert_eq(t.id, "tenant-001")
      assert_eq(t.name, "Acme Corp")
      assert_eq(t.tier, "enterprise")
    }
    None => assert_true(false)
  }
  
  let tenant_nonexistent = find_tenant("tenant-999", tenants)
  assert_true(tenant_nonexistent.is_none())
  
  // 9. 测试数据隔离
  let tenant1_data = filter_data_by_tenant(tenant_data, "tenant-001")
  assert_eq(tenant1_data.length(), 2)
  assert_eq(tenant1_data[0].tenant_id, "tenant-001")
  assert_eq(tenant1_data[1].tenant_id, "tenant-001")
  assert_true(tenant1_data[0].data_type == "span")
  assert_true(tenant1_data[1].data_type == "log")
  
  let tenant2_data = filter_data_by_tenant(tenant_data, "tenant-002")
  assert_eq(tenant2_data.length(), 2)
  assert_eq(tenant2_data[0].tenant_id, "tenant-002")
  assert_eq(tenant2_data[1].tenant_id, "tenant-002")
  assert_true(tenant2_data[0].data_type == "metric")
  assert_true(tenant2_data[1].data_type == "span")
  
  let tenant3_data = filter_data_by_tenant(tenant_data, "tenant-003")
  assert_eq(tenant3_data.length(), 1)
  assert_eq(tenant3_data[0].tenant_id, "tenant-003")
  assert_true(tenant3_data[0].data_type == "span")
  
  // 10. 实现资源限制检查函数
  type ResourceUsage = {
    spans_per_second: Int,
    metrics_per_minute: Int,
    storage_mb: Int,
    retention_days: Int
  }
  
  let check_tenant_resource_limits = fn(usage: ResourceUsage, limits: TenantResourceLimits) {
    {
      spans_within_limit: usage.spans_per_second <= limits.max_spans_per_second,
      metrics_within_limit: usage.metrics_per_minute <= limits.max_metrics_per_minute,
      storage_within_limit: usage.storage_mb <= limits.max_storage_mb,
      retention_within_limit: usage.retention_days <= limits.max_retention_days,
      all_within_limit: usage.spans_per_second <= limits.max_spans_per_second &&
                       usage.metrics_per_minute <= limits.max_metrics_per_minute &&
                       usage.storage_mb <= limits.max_storage_mb &&
                       usage.retention_days <= limits.max_retention_days
    }
  }
  
  // 11. 测试企业级租户资源限制
  match tenant1 {
    Some(t) => {
      let enterprise_usage = ResourceUsage({
        spans_per_second: 8000,   // 低于10000限制
        metrics_per_minute: 50000, // 低于60000限制
        storage_mb: 8192,          // 低于10240限制
        retention_days: 60         // 低于90限制
      })
      
      let enterprise_check = check_tenant_resource_limits(enterprise_usage, t.resource_limits)
      assert_true(enterprise_check.all_within_limit)
      assert_true(enterprise_check.spans_within_limit)
      assert_true(enterprise_check.metrics_within_limit)
      assert_true(enterprise_check.storage_within_limit)
      assert_true(enterprise_check.retention_within_limit)
      
      // 测试超限情况
      let enterprise_exceeded_usage = ResourceUsage({
        spans_per_second: 12000,  // 超过10000限制
        metrics_per_minute: 50000,
        storage_mb: 8192,
        retention_days: 60
      })
      
      let enterprise_exceeded_check = check_tenant_resource_limits(enterprise_exceeded_usage, t.resource_limits)
      assert_false(enterprise_exceeded_check.all_within_limit)
      assert_false(enterprise_exceeded_check.spans_within_limit)
      assert_true(enterprise_exceeded_check.metrics_within_limit)
      assert_true(enterprise_exceeded_check.storage_within_limit)
      assert_true(enterprise_exceeded_check.retention_within_limit)
    }
    None => assert_true(false)
  }
  
  // 12. 测试基础级租户资源限制
  let tenant3_found = find_tenant("tenant-003", tenants)
  match tenant3_found {
    Some(t) => {
      let basic_usage = ResourceUsage({
        spans_per_second: 90,  // 低于100限制
        metrics_per_minute: 500, // 低于600限制
        storage_mb: 80,         // 低于100限制
        retention_days: 5       // 低于7限制
      })
      
      let basic_check = check_tenant_resource_limits(basic_usage, t.resource_limits)
      assert_true(basic_check.all_within_limit)
    }
    None => assert_true(false)
  }
  
  // 13. 实现数据访问控制
  type AccessRequest = {
    tenant_id: String,
    requested_data_type: String,
    requester_id: String
  }
  
  type AccessResult = {
    allowed: Bool,
    reason: String
  }
  
  let check_data_access = fn(request: AccessRequest, tenants: Array[Tenant]) {
    match find_tenant(request.tenant_id, tenants) {
      None => AccessResult({
        allowed: false,
        reason: "Tenant not found"
      })
      Some(tenant) => {
        // 简化的访问控制逻辑：只有租户自己可以访问自己的数据
        if request.requester_id.starts_with(request.tenant_id) {
          AccessResult({
            allowed: true,
            reason: "Access granted"
          })
        } else {
          AccessResult({
            allowed: false,
            reason: "Unauthorized access to tenant data"
          })
        }
      }
    }
  }
  
  // 14. 测试数据访问控制
  let valid_access_request = AccessRequest({
    tenant_id: "tenant-001",
    requested_data_type: "span",
    requester_id: "tenant-001-user-123"
  })
  
  let valid_access_result = check_data_access(valid_access_request, tenants)
  assert_true(valid_access_result.allowed)
  assert_eq(valid_access_result.reason, "Access granted")
  
  let invalid_access_request = AccessRequest({
    tenant_id: "tenant-001",
    requested_data_type: "span",
    requester_id: "tenant-002-user-456"  // 不同租户的用户
  })
  
  let invalid_access_result = check_data_access(invalid_access_request, tenants)
  assert_false(invalid_access_result.allowed)
  assert_eq(invalid_access_result.reason, "Unauthorized access to tenant data")
  
  let nonexistent_tenant_request = AccessRequest({
    tenant_id: "tenant-999",
    requested_data_type: "span",
    requester_id: "tenant-999-user-789"
  })
  
  let nonexistent_tenant_result = check_data_access(nonexistent_tenant_request, tenants)
  assert_false(nonexistent_tenant_result.allowed)
  assert_eq(nonexistent_tenant_result.reason, "Tenant not found")
}

// 测试9: 自适应采样策略
test "自适应采样策略和动态调整" {
  // 1. 定义采样策略类型
  enum SamplingStrategy {
    AlwaysOn       // 总是采样
    AlwaysOff      // 总是不采样
    Probabilistic(Float)  // 概率采样
    RateLimiting(Int)     // 速率限制采样（每秒最多采样数）
    Adaptive        // 自适应采样
  }
  
  // 2. 定义采样决策
  type SamplingDecision = {
    sample: Bool,
    strategy: SamplingStrategy,
    reason: String
  }
  
  // 3. 定义系统负载指标
  type SystemLoad = {
    cpu_usage: Float,
    memory_usage: Float,
    request_rate: Int,  // 每秒请求数
    error_rate: Float   // 错误率
  }
  
  // 4. 定义自适应采样配置
  type AdaptiveSamplingConfig = {
    low_load_threshold: Float,
    high_load_threshold: Float,
    low_error_threshold: Float,
    high_error_threshold: Float,
    base_sampling_rate: Float,
    max_sampling_rate: Float,
    min_sampling_rate: Float
  }
  
  // 5. 创建自适应采样配置
  let adaptive_config = AdaptiveSamplingConfig({
    low_load_threshold: 0.3,      // CPU/内存使用率低于30%认为低负载
    high_load_threshold: 0.8,     // CPU/内存使用率高于80%认为高负载
    low_error_threshold: 0.01,    // 错误率低于1%认为低错误率
    high_error_threshold: 0.1,    // 错误率高于10%认为高错误率
    base_sampling_rate: 0.1,      // 基础采样率10%
    max_sampling_rate: 1.0,       // 最大采样率100%
    min_sampling_rate: 0.01       // 最小采样率1%
  })
  
  // 6. 实现概率采样决策
  let probabilistic_sampling = fn(sampling_rate: Float) {
    let random_value = 0.5  // 简化的随机值，实际应该使用真实随机数生成器
    SamplingDecision({
      sample: random_value <= sampling_rate,
      strategy: SamplingStrategy::Probabilistic(sampling_rate),
      reason: "Probabilistic sampling with rate: " + sampling_rate.to_string()
    })
  }
  
  // 7. 实现速率限制采样决策
  let rate_limiting_sampling = fn(max_samples_per_second: Int, current_samples: Int) {
    SamplingDecision({
      sample: current_samples < max_samples_per_second,
      strategy: SamplingStrategy::RateLimiting(max_samples_per_second),
      reason: "Rate limiting sampling: " + current_samples.to_string() + "/" + max_samples_per_second.to_string()
    })
  }
  
  // 8. 实现自适应采样决策
  let adaptive_sampling = fn(load: SystemLoad, config: AdaptiveSamplingConfig, current_sampling_rate: Float) {
    let avg_load = (load.cpu_usage + load.memory_usage) / 2.0
    
    let new_sampling_rate = if avg_load > config.high_load_threshold or load.error_rate > config.high_error_threshold {
      // 高负载或高错误率，降低采样率
      let reduced_rate = current_sampling_rate * 0.5
      if reduced_rate < config.min_sampling_rate {
        config.min_sampling_rate
      } else {
        reduced_rate
      }
    } else if avg_load < config.low_load_threshold and load.error_rate < config.low_error_threshold {
      // 低负载和低错误率，提高采样率
      let increased_rate = current_sampling_rate * 1.5
      if increased_rate > config.max_sampling_rate {
        config.max_sampling_rate
      } else {
        increased_rate
      }
    } else {
      // 中等负载，保持当前采样率
      current_sampling_rate
    }
    
    let reason = "Adaptive sampling adjusted from " + current_sampling_rate.to_string() + 
                " to " + new_sampling_rate.to_string() + 
                " based on load: " + avg_load.to_string() + 
                " and error rate: " + load.error_rate.to_string()
    
    SamplingDecision({
      sample: true,  // 自适应采样总是尝试采样
      strategy: SamplingStrategy::Adaptive,
      reason
    })
  }
  
  // 9. 测试概率采样
  let high_rate_decision = probabilistic_sampling(0.8)
  assert_true(high_rate_decision.sample)
  match high_rate_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.8)
    _ => assert_true(false)
  }
  
  let low_rate_decision = probabilistic_sampling(0.1)
  assert_false(low_rate_decision.sample)  // 由于随机值是0.5 > 0.1
  match low_rate_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.1)
    _ => assert_true(false)
  }
  
  // 10. 测试速率限制采样
  let within_limit_decision = rate_limiting_sampling(100, 50)
  assert_true(within_limit_decision.sample)
  match within_limit_decision.strategy {
    SamplingStrategy::RateLimiting(limit) => assert_eq(limit, 100)
    _ => assert_true(false)
  }
  
  let exceeded_limit_decision = rate_limiting_sampling(100, 150)
  assert_false(exceeded_limit_decision.sample)
  match exceeded_limit_decision.strategy {
    SamplingStrategy::RateLimiting(limit) => assert_eq(limit, 100)
    _ => assert_true(false)
  }
  
  // 11. 测试自适应采样 - 高负载情况
  let high_load = SystemLoad({
    cpu_usage: 0.9,        // 高CPU使用率
    memory_usage: 0.85,     // 高内存使用率
    request_rate: 5000,     // 高请求率
    error_rate: 0.05        // 中等错误率
  })
  
  let high_load_decision = adaptive_sampling(high_load, adaptive_config, 0.2)
  assert_true(high_load_decision.sample)
  assert_true(high_load_decision.reason.contains("reduced"))
  match high_load_decision.strategy {
    SamplingStrategy::Adaptive => assert_true(true)
    _ => assert_true(false)
  }
  
  // 12. 测试自适应采样 - 低负载情况
  let low_load = SystemLoad({
    cpu_usage: 0.2,        // 低CPU使用率
    memory_usage: 0.25,     // 低内存使用率
    request_rate: 500,      // 低请求率
    error_rate: 0.005       // 低错误率
  })
  
  let low_load_decision = adaptive_sampling(low_load, adaptive_config, 0.1)
  assert_true(low_load_decision.sample)
  assert_true(low_load_decision.reason.contains("increased"))
  match low_load_decision.strategy {
    SamplingStrategy::Adaptive => assert_true(true)
    _ => assert_true(false)
  }
  
  // 13. 测试自适应采样 - 高错误率情况
  let high_error_load = SystemLoad({
    cpu_usage: 0.5,        // 中等CPU使用率
    memory_usage: 0.6,     // 中等内存使用率
    request_rate: 2000,     // 中等请求率
    error_rate: 0.15        // 高错误率
  })
  
  let high_error_decision = adaptive_sampling(high_error_load, adaptive_config, 0.3)
  assert_true(high_error_decision.sample)
  assert_true(high_error_decision.reason.contains("reduced"))
  
  // 14. 实现采样策略管理器
  type SamplingManager = {
    current_strategy: SamplingStrategy,
    current_sampling_rate: Float,
    samples_this_second: Int,
    max_samples_per_second: Int,
    adaptive_config: AdaptiveSamplingConfig
  }
  
  // 15. 创建采样管理器
  let sampling_manager = SamplingManager({
    current_strategy: SamplingStrategy::Adaptive,
    current_sampling_rate: 0.1,
    samples_this_second: 0,
    max_samples_per_second: 1000,
    adaptive_config
  })
  
  // 16. 实现采样决策函数
  let make_sampling_decision = fn(manager: SamplingManager, load: SystemLoad) {
    match manager.current_strategy {
      SamplingStrategy::AlwaysOn => SamplingDecision({
        sample: true,
        strategy: SamplingStrategy::AlwaysOn,
        reason: "Always-on sampling"
      })
      SamplingStrategy::AlwaysOff => SamplingDecision({
        sample: false,
        strategy: SamplingStrategy::AlwaysOff,
        reason: "Always-off sampling"
      })
      SamplingStrategy::Probabilistic(rate) => probabilistic_sampling(rate)
      SamplingStrategy::RateLimiting(limit) => rate_limiting_sampling(limit, manager.samples_this_second)
      SamplingStrategy::Adaptive => adaptive_sampling(load, manager.adaptive_config, manager.current_sampling_rate)
    }
  }
  
  // 17. 测试采样管理器
  let adaptive_decision = make_sampling_decision(sampling_manager, high_load)
  assert_true(adaptive_decision.sample)
  match adaptive_decision.strategy {
    SamplingStrategy::Adaptive => assert_true(true)
    _ => assert_true(false)
  }
  
  // 18. 实现策略切换逻辑
  let switch_sampling_strategy = fn(manager: SamplingManager, new_strategy: SamplingStrategy) {
    SamplingManager({
      current_strategy: new_strategy,
      current_sampling_rate: manager.current_sampling_rate,
      samples_this_second: manager.samples_this_second,
      max_samples_per_second: manager.max_samples_per_second,
      adaptive_config: manager.adaptive_config
    })
  }
  
  // 19. 测试策略切换
  let prob_manager = switch_sampling_strategy(sampling_manager, SamplingStrategy::Probabilistic(0.5))
  match prob_manager.current_strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.5)
    _ => assert_true(false)
  }
  
  let prob_decision = make_sampling_decision(prob_manager, low_load)
  assert_true(prob_decision.sample)  // 随机值0.5 <= 0.5
  match prob_decision.strategy {
    SamplingStrategy::Probabilistic(rate) => assert_eq(rate, 0.5)
    _ => assert_true(false)
  }
}

// 测试10: 遥测数据聚合
test "遥测数据聚合和统计分析" {
  // 1. 定义遥测数据点类型
  enum TelemetryDataPoint {
    Counter(String, Int)      // (metric_name, value)
    Gauge(String, Float)      // (metric_name, value)
    Histogram(String, Array[Float])  // (metric_name, values)
    Summary(String, Float, Float, Float, Int)  // (metric_name, sum, count, min, max)
  }
  
  // 2. 定义聚合结果类型
  type AggregationResult = {
    metric_name: String,
    aggregation_type: String,  // "sum", "avg", "min", "max", "count", "percentile"
    value: Float,
    timestamp: Int
  }
  
  // 3. 创建遥测数据点
  let telemetry_data = [
    TelemetryDataPoint::Counter("request.total", 100),
    TelemetryDataPoint::Counter("request.total", 150),
    TelemetryDataPoint::Counter("request.total", 200),
    TelemetryDataPoint::Gauge("cpu.usage", 0.65),
    TelemetryDataPoint::Gauge("cpu.usage", 0.72),
    TelemetryDataPoint::Gauge("cpu.usage", 0.68),
    TelemetryDataPoint::Histogram("response.time", [100.0, 150.0, 200.0, 250.0, 300.0]),
    TelemetryDataPoint::Histogram("response.time", [120.0, 180.0, 220.0, 280.0, 320.0]),
    TelemetryDataPoint::Summary("memory.usage", 1024.0, 512.0, 256.0, 2048.0, 4),
    TelemetryDataPoint::Summary("memory.usage", 1536.0, 768.0, 384.0, 3072.0, 4)
  ]
  
  // 4. 实现计数器聚合函数
  let aggregate_counters = fn(data_points: Array[TelemetryDataPoint], metric_name: String) {
    let counters = data_points.filter(fn(dp) {
      match dp {
        TelemetryDataPoint::Counter(name, _) => name == metric_name
        _ => false
      }
    })
    
    let sum = counters.reduce(fn(acc, dp) {
      match dp {
        TelemetryDataPoint::Counter(_, value) => acc + value
        _ => acc
      }
    }, 0)
    
    let count = counters.length()
    let avg = sum / count
    
    [
      AggregationResult({
        metric_name,
        aggregation_type: "sum",
        value: sum.to_float(),
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "count",
        value: count.to_float(),
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "avg",
        value: avg.to_float(),
        timestamp: 1640995200
      })
    ]
  }
  
  // 5. 实现仪表盘聚合函数
  let aggregate_gauges = fn(data_points: Array[TelemetryDataPoint], metric_name: String) {
    let gauges = data_points.filter(fn(dp) {
      match dp {
        TelemetryDataPoint::Gauge(name, _) => name == metric_name
        _ => false
      }
    })
    
    let values = gauges.map(fn(dp) {
      match dp {
        TelemetryDataPoint::Gauge(_, value) => value
        _ => 0.0
      }
    })
    
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let count = values.length().to_float()
    let avg = sum / count
    let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, 999999.0)
    let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, 0.0)
    
    [
      AggregationResult({
        metric_name,
        aggregation_type: "avg",
        value: avg,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "min",
        value: min,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "max",
        value: max,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "latest",
        value: values[values.length() - 1],
        timestamp: 1640995200
      })
    ]
  }
  
  // 6. 实现直方图聚合函数
  let aggregate_histograms = fn(data_points: Array[TelemetryDataPoint], metric_name: String) {
    let histograms = data_points.filter(fn(dp) {
      match dp {
        TelemetryDataPoint::Histogram(name, _) => name == metric_name
        _ => false
      }
    })
    
    let all_values = histograms.reduce(fn(acc, dp) {
      match dp {
        TelemetryDataPoint::Histogram(_, values) => acc + values
        _ => acc
      }
    }, [] as Array[Float])
    
    let count = all_values.length()
    let sum = all_values.reduce(fn(acc, v) { acc + v }, 0.0)
    let avg = sum / count.to_float()
    
    // 计算百分位数（简化版）
    let sorted_values = all_values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let p50_index = (count / 2).min(count - 1)
    let p95_index = ((count * 95) / 100).min(count - 1)
    let p99_index = ((count * 99) / 100).min(count - 1)
    
    let p50 = sorted_values[p50_index]
    let p95 = sorted_values[p95_index]
    let p99 = sorted_values[p99_index]
    
    [
      AggregationResult({
        metric_name,
        aggregation_type: "count",
        value: count.to_float(),
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "sum",
        value: sum,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "avg",
        value: avg,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "p50",
        value: p50,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "p95",
        value: p95,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "p99",
        value: p99,
        timestamp: 1640995200
      })
    ]
  }
  
  // 7. 实现摘要聚合函数
  let aggregate_summaries = fn(data_points: Array[TelemetryDataPoint], metric_name: String) {
    let summaries = data_points.filter(fn(dp) {
      match dp {
        TelemetryDataPoint::Summary(name, _, _, _, _) => name == metric_name
        _ => false
      }
    })
    
    let total_sum = summaries.reduce(fn(acc, dp) {
      match dp {
        TelemetryDataPoint::Summary(_, sum, _, _, _) => acc + sum
        _ => acc
      }
    }, 0.0)
    
    let total_count = summaries.reduce(fn(acc, dp) {
      match dp {
        TelemetryDataPoint::Summary(_, _, count, _, _) => acc + count
        _ => acc
      }
    }, 0)
    
    let all_mins = summaries.map(fn(dp) {
      match dp {
        TelemetryDataPoint::Summary(_, _, _, min, _) => min
        _ => 0.0
      }
    })
    
    let all_maxs = summaries.map(fn(dp) {
      match dp {
        TelemetryDataPoint::Summary(_, _, _, _, max) => max
        _ => 0.0
      }
    })
    
    let global_min = all_mins.reduce(fn(acc, v) { if v < acc { v } else { acc } }, 999999.0)
    let global_max = all_maxs.reduce(fn(acc, v) { if v > acc { v } else { acc } }, 0.0)
    
    [
      AggregationResult({
        metric_name,
        aggregation_type: "sum",
        value: total_sum,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "count",
        value: total_count.to_float(),
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "avg",
        value: total_sum / total_count.to_float(),
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "min",
        value: global_min,
        timestamp: 1640995200
      }),
      AggregationResult({
        metric_name,
        aggregation_type: "max",
        value: global_max,
        timestamp: 1640995200
      })
    ]
  }
  
  // 8. 测试计数器聚合
  let counter_results = aggregate_counters(telemetry_data, "request.total")
  assert_eq(counter_results.length(), 3)
  
  let sum_result = counter_results.filter(fn(r) { r.aggregation_type == "sum" })[0]
  assert_eq(sum_result.value, 450.0)  // 100 + 150 + 200
  
  let count_result = counter_results.filter(fn(r) { r.aggregation_type == "count" })[0]
  assert_eq(count_result.value, 3.0)
  
  let avg_result = counter_results.filter(fn(r) { r.aggregation_type == "avg" })[0]
  assert_eq(avg_result.value, 150.0)  // 450 / 3
  
  // 9. 测试仪表盘聚合
  let gauge_results = aggregate_gauges(telemetry_data, "cpu.usage")
  assert_eq(gauge_results.length(), 4)
  
  let avg_result = gauge_results.filter(fn(r) { r.aggregation_type == "avg" })[0]
  assert_true(avg_result.value > 0.68 && avg_result.value < 0.69)  // (0.65 + 0.72 + 0.68) / 3
  
  let min_result = gauge_results.filter(fn(r) { r.aggregation_type == "min" })[0]
  assert_eq(min_result.value, 0.65)
  
  let max_result = gauge_results.filter(fn(r) { r.aggregation_type == "max" })[0]
  assert_eq(max_result.value, 0.72)
  
  let latest_result = gauge_results.filter(fn(r) { r.aggregation_type == "latest" })[0]
  assert_eq(latest_result.value, 0.68)
  
  // 10. 测试直方图聚合
  let histogram_results = aggregate_histograms(telemetry_data, "response.time")
  assert_eq(histogram_results.length(), 6)
  
  let count_result = histogram_results.filter(fn(r) { r.aggregation_type == "count" })[0]
  assert_eq(count_result.value, 10.0)  // 5个值 + 5个值
  
  let sum_result = histogram_results.filter(fn(r) { r.aggregation_type == "sum" })[0]
  assert_eq(sum_result.value, 2120.0)  // (100+150+200+250+300) + (120+180+220+280+320)
  
  let avg_result = histogram_results.filter(fn(r) { r.aggregation_type == "avg" })[0]
  assert_eq(avg_result.value, 212.0)  // 2120 / 10
  
  // 11. 测试摘要聚合
  let summary_results = aggregate_summaries(telemetry_data, "memory.usage")
  assert_eq(summary_results.length(), 5)
  
  let sum_result = summary_results.filter(fn(r) { r.aggregation_type == "sum" })[0]
  assert_eq(sum_result.value, 2560.0)  // 1024 + 1536
  
  let count_result = summary_results.filter(fn(r) { r.aggregation_type == "count" })[0]
  assert_eq(count_result.value, 8.0)  // 4 + 4
  
  let avg_result = summary_results.filter(fn(r) { r.aggregation_type == "avg" })[0]
  assert_eq(avg_result.value, 320.0)  // 2560 / 8
  
  let min_result = summary_results.filter(fn(r) { r.aggregation_type == "min" })[0]
  assert_eq(min_result.value, 256.0)  // min(256, 384)
  
  let max_result = summary_results.filter(fn(r) { r.aggregation_type == "max" })[0]
  assert_eq(max_result.value, 3072.0)  // max(2048, 3072)
  
  // 12. 实现时间窗口聚合
  type TimeWindowedData = {
    data_point: TelemetryDataPoint,
    timestamp: Int
  }
  
  let aggregate_by_time_window = fn(data_points: Array[TimeWindowedData], window_size_seconds: Int, aggregation_fn: Array[TelemetryDataPoint] -> Array[AggregationResult]) {
    let mut windows = []
    
    // 简化的时间窗口处理：假设所有数据点都在同一窗口内
    let telemetry_only = data_points.map(fn(d) { d.data_point })
    let aggregated = aggregation_fn(telemetry_only)
    
    for result in aggregated {
      windows = windows.push({
        window_start: 1640995200,
        window_end: 1640995200 + window_size_seconds,
        results: [result]
      })
    }
    
    windows
  }
  
  // 13. 测试时间窗口聚合
  let time_windowed_data = [
    { data_point: telemetry_data[0], timestamp: 1640995200 },
    { data_point: telemetry_data[1], timestamp: 1640995201 },
    { data_point: telemetry_data[2], timestamp: 1640995202 }
  ]
  
  let windowed_results = aggregate_by_time_window(
    time_windowed_data, 
    60,  // 60秒窗口
    fn(points) { aggregate_counters(points, "request.total") }
  )
  
  assert_eq(windowed_results.length(), 3)  // 3个聚合结果
  assert_eq(windowed_results[0].window_start, 1640995200)
  assert_eq(windowed_results[0].window_end, 1640995260)
}