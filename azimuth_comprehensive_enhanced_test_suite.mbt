// Azimuth 综合增强测试套件
// 包含多种测试场景，覆盖基础特性、遥测功能和性能测试

// 测试1: 高效数据处理和转换
test "高效数据处理和转换管道" {
  // 定义数据处理管道
  let process_pipeline = fn(data: Array[Int]) {
    data
      .filter(fn(x) { x > 0 })  // 过滤正数
      .map(fn(x) { x * x })     // 平方
      .filter(fn(x) { x % 2 == 0 })  // 过滤偶数
      .map(fn(x) { x / 2 })     // 除以2
      .take(5)                  // 取前5个
  }
  
  let input_data = [-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let result = process_pipeline(input_data)
  
  // 预期结果: [2, 8, 18, 32, 50]
  // 1^2=1(奇数过滤掉), 2^2=4/2=2, 3^2=9(奇数过滤掉), 4^2=16/2=8, 5^2=25(奇数过滤掉), 6^2=36/2=18, 7^2=49(奇数过滤掉), 8^2=64/2=32, 9^2=81(奇数过滤掉), 10^2=100/2=50
  assert_eq(result, [2, 8, 18, 32, 50])
}

// 测试2: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样策略枚举
  enum SamplingStrategy {
    Always
    Never
    Probability(Float)  // 采样概率 0.0-1.0
    Count(Int)          // 每N个采样1个
    TimeWindow(Int)     // 时间窗口内采样1个
  }
  
  // 采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, counter: Int) {
    match strategy {
      SamplingStrategy::Always => true
      SamplingStrategy::Never => false
      SamplingStrategy::Probability(prob) => {
        // 简化的概率采样：基于trace_id的哈希
        let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
        let normalized = (hash % 100) as Float / 100.0
        normalized <= prob
      }
      SamplingStrategy::Count(n) => counter % n == 0
      SamplingStrategy::TimeWindow(window_ms) => {
        // 简化的时间窗口采样
        let current_time = Time::now()
        (current_time / window_ms) % 2 == 0
      }
    }
  }
  
  // 测试不同采样策略
  assert_true(should_sample(SamplingStrategy::Always, "trace-1", 0))
  assert_false(should_sample(SamplingStrategy::Never, "trace-2", 0))
  
  // 测试概率采样
  let prob_result1 = should_sample(SamplingStrategy::Probability(0.5), "trace-abc", 0)
  let prob_result2 = should_sample(SamplingStrategy::Probability(0.0), "trace-def", 0)
  let prob_result3 = should_sample(SamplingStrategy::Probability(1.0), "trace-ghi", 0)
  
  assert_false(prob_result2)  // 0概率应该永远不采样
  assert_true(prob_result3)   // 1概率应该永远采样
  
  // 测试计数采样
  assert_true(should_sample(SamplingStrategy::Count(3), "trace-1", 0))
  assert_false(should_sample(SamplingStrategy::Count(3), "trace-2", 1))
  assert_false(should_sample(SamplingStrategy::Count(3), "trace-3", 2))
  assert_true(should_sample(SamplingStrategy::Count(3), "trace-4", 3))
}

// 测试3: 遥测指标聚合和计算
test "遥测指标聚合和计算" {
  // 定义指标类型
  type MetricValue = {
    timestamp: Int,
    value: Float,
    labels: Array<(String, String)>
  }
  
  // 定义聚合函数
  let aggregate = fn(values: Array[MetricValue], window_ms: Int) {
    if values.length() == 0 {
      return { count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
    }
    
    let filtered_values = if window_ms > 0 {
      let current_time = Time::now()
      values.filter(fn(v) { current_time - v.timestamp <= window_ms })
    } else {
      values
    }
    
    if filtered_values.length() == 0 {
      return { count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
    }
    
    let numeric_values = filtered_values.map(fn(v) { v.value })
    let count = numeric_values.length()
    let sum = numeric_values.reduce(fn(acc, v) { acc + v }, 0.0)
    let avg = sum / (count as Float)
    let min = numeric_values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, numeric_values[0])
    let max = numeric_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, numeric_values[0])
    
    { count, sum, avg, min, max }
  }
  
  // 创建测试数据
  let current_time = Time::now()
  let test_metrics = [
    { timestamp: current_time - 10000, value: 10.5, labels: [("service", "api")] },
    { timestamp: current_time - 5000, value: 15.2, labels: [("service", "api")] },
    { timestamp: current_time - 2000, value: 8.7, labels: [("service", "web")] },
    { timestamp: current_time - 1000, value: 12.3, labels: [("service", "api")] },
    { timestamp: current_time - 500, value: 9.8, labels: [("service", "web")] }
  ]
  
  // 测试全量聚合
  let full_aggregation = aggregate(test_metrics, 0)
  assert_eq(full_aggregation.count, 5)
  assert_eq(full_aggregation.sum.round(), 56.5)
  assert_eq(full_aggregation.avg.round(), 11.3)
  assert_eq(full_aggregation.min, 8.7)
  assert_eq(full_aggregation.max, 15.2)
  
  // 测试时间窗口聚合（最近5秒）
  let window_aggregation = aggregate(test_metrics, 5000)
  assert_eq(window_aggregation.count, 3)  // 最后3个指标
  assert_eq(window_aggregation.sum.round(), 30.8)
  assert_eq(window_aggregation.avg.round(), 10.27)
}

// 测试4: 错误处理和恢复机制
test "错误处理和恢复机制" {
  // 定义错误类型
  enum ErrorType {
    NetworkError(String)
    TimeoutError(Int)
    ValidationError(String)
    RateLimitError(Int)
    UnknownError(String)
  }
  
  // 定义恢复策略
  enum RecoveryAction {
    Retry(Int)           // 重试次数
    Backoff(Int, Int)    // 初始延迟和最大延迟
    CircuitBreaker(Int)  // 失败阈值
    FailFast             // 快速失败
    Fallback(String)     // 备用方案
  }
  
  // 错误处理函数
  let handle_error = fn(error: ErrorType, strategy: RecoveryAction) {
    match (error, strategy) {
      (ErrorType::NetworkError(msg), RecoveryAction::Retry(max_retries)) => {
        "网络错误: " + msg + ", 将重试最多" + max_retries.to_string() + "次"
      }
      (ErrorType::TimeoutError(timeout), RecoveryAction::Backoff(initial, max)) => {
        "超时错误: " + timeout.to_string() + "ms, 使用退避策略"
      }
      (ErrorType::ValidationError(msg), RecoveryAction::FailFast) => {
        "验证错误: " + msg + ", 快速失败"
      }
      (ErrorType::RateLimitError(retry_after), RecoveryAction::Backoff(initial, max)) => {
        "限流错误: " + retry_after.to_string() + "s后重试"
      }
      (ErrorType::UnknownError(msg), RecoveryAction::Fallback(fallback)) => {
        "未知错误: " + msg + ", 使用备用方案: " + fallback
      }
      (ErrorType::NetworkError(msg), RecoveryAction::CircuitBreaker(threshold)) => {
        "网络错误: " + msg + ", 断路器阈值: " + threshold.to_string()
      }
      _ => "未处理的错误/策略组合"
    }
  }
  
  // 测试错误处理
  let network_error = ErrorType::NetworkError("连接超时")
  let retry_strategy = RecoveryAction::Retry(3)
  let result1 = handle_error(network_error, retry_strategy)
  assert_eq(result1, "网络错误: 连接超时, 将重试最多3次")
  
  let timeout_error = ErrorType::TimeoutError(5000)
  let backoff_strategy = RecoveryAction::Backoff(1000, 10000)
  let result2 = handle_error(timeout_error, backoff_strategy)
  assert_eq(result2, "超时错误: 5000ms, 使用退避策略")
  
  let validation_error = ErrorType::ValidationError("参数无效")
  let fail_fast_strategy = RecoveryAction::FailFast
  let result3 = handle_error(validation_error, fail_fast_strategy)
  assert_eq(result3, "验证错误: 参数无效, 快速失败")
}

// 测试5: 遥测上下文传播
test "遥测上下文传播" {
  // 定义上下文类型
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)]
  }
  
  // 创建新的上下文
  let create_context = fn(trace_id: String, span_id: String) {
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: None,
      baggage: []
    }
  }
  
  // 创建子上下文
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id: span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage
    }
  }
  
  // 添加baggage项
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      baggage: context.baggage.push((key, value))
    }
  }
  
  // 提取baggage值
  let get_baggage = fn(context: TraceContext, key: String) {
    context.baggage.find(fn(item) { item.0 == key }).map(fn(item) { item.1 })
  }
  
  // 测试上下文创建和传播
  let root_context = create_context("trace-123", "span-abc")
  assert_eq(root_context.trace_id, "trace-123")
  assert_eq(root_context.span_id, "span-abc")
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.length(), 0)
  
  // 创建子上下文
  let child_context = create_child_context(root_context, "span-def")
  assert_eq(child_context.trace_id, "trace-123")  // 继承trace_id
  assert_eq(child_context.span_id, "span-def")
  assert_eq(child_context.parent_span_id, Some("span-abc"))  // 父span_id
  assert_eq(child_context.baggage.length(), 0)  // 继承baggage
  
  // 添加baggage
  let context_with_baggage = add_baggage(root_context, "user.id", "user-456")
  assert_eq(context_with_baggage.baggage.length(), 1)
  assert_eq(context_with_baggage.baggage[0], ("user.id", "user-456"))
  
  // 提取baggage
  let user_id = get_baggage(context_with_baggage, "user.id")
  assert_eq(user_id, Some("user-456"))
  
  let non_existent = get_baggage(context_with_baggage, "non.existent")
  assert_eq(non_existent, None)
}

// 测试6: 高级数据结构操作
test "高级数据结构操作" {
  // 定义栈结构
  type Stack[T] = Array[T]
  
  let stack_push = fn(stack: Stack[T], item: T) {
    stack.push(item)
  }
  
  let stack_pop = fn(stack: Stack[T]) {
    if stack.length() > 0 {
      let item = stack[stack.length() - 1]
      let new_stack = stack.slice(0, stack.length() - 1)
      (Some(item), new_stack)
    } else {
      (None, stack)
    }
  }
  
  // 定义队列结构
  type Queue[T] = { front: Array[T], rear: Array[T] }
  
  let queue_enqueue = fn(queue: Queue[T], item: T) {
    { front: queue.front, rear: queue.rear.push(item) }
  }
  
  let queue_dequeue = fn(queue: Queue[T]) {
    if queue.front.length() > 0 {
      let item = queue.front[0]
      let new_front = queue.front.slice(1, queue.front.length())
      (Some(item), { front: new_front, rear: queue.rear })
    } else if queue.rear.length() > 0 {
      // 将rear反转作为新的front
      let new_front = queue.rear.reverse()
      let item = new_front[0]
      let remaining_front = new_front.slice(1, new_front.length())
      (Some(item), { front: remaining_front, rear: [] })
    } else {
      (None, queue)
    }
  }
  
  // 测试栈操作
  let empty_stack: Stack[Int] = []
  let stack1 = stack_push(empty_stack, 1)
  let stack2 = stack_push(stack1, 2)
  let stack3 = stack_push(stack2, 3)
  
  let (item3, stack_after_pop3) = stack_pop(stack3)
  assert_eq(item3, Some(3))
  assert_eq(stack_after_pop3.length(), 2)
  
  let (item2, stack_after_pop2) = stack_pop(stack_after_pop3)
  assert_eq(item2, Some(2))
  assert_eq(stack_after_pop2.length(), 1)
  
  let (item1, final_stack) = stack_pop(stack_after_pop2)
  assert_eq(item1, Some(1))
  assert_eq(final_stack.length(), 0)
  
  let (no_item, _) = stack_pop(final_stack)
  assert_eq(no_item, None)
  
  // 测试队列操作
  let empty_queue: Queue[Int] = { front: [], rear: [] }
  let queue1 = queue_enqueue(empty_queue, 1)
  let queue2 = queue_enqueue(queue1, 2)
  let queue3 = queue_enqueue(queue2, 3)
  
  let (item1, queue_after_dequeue1) = queue_dequeue(queue3)
  assert_eq(item1, Some(1))
  
  let (item2, queue_after_dequeue2) = queue_dequeue(queue_after_dequeue1)
  assert_eq(item2, Some(2))
  
  let (item3, final_queue) = queue_dequeue(queue_after_dequeue2)
  assert_eq(item3, Some(3))
  
  let (no_item_queue, _) = queue_dequeue(final_queue)
  assert_eq(no_item_queue, None)
}

// 测试7: 字符串处理和模式匹配
test "字符串处理和模式匹配" {
  // 定义字符串处理函数
  let camel_to_snake = fn(camel: String) {
    let chars = camel.to_char_array()
    let mut result = ""
    
    for i in 0..chars.length() {
      let c = chars[i]
      if c.is_uppercase() && i > 0 {
        result = result + "_" + c.to_lowercase()
      } else {
        result = result + c.to_lowercase()
      }
    }
    
    result
  }
  
  let snake_to_camel = fn(snake: String) {
    let parts = snake.split("_")
    let mut result = ""
    
    for i in 0..parts.length() {
      let part = parts[i]
      if part.length() > 0 {
        if i == 0 {
          result = result + part
        } else {
          let first_char = part[0].to_uppercase()
          let rest = part.slice(1, part.length())
          result = result + first_char + rest
        }
      }
    }
    
    result
  }
  
  let extract_numbers = fn(text: String) {
    let chars = text.to_char_array()
    let mut current_number = ""
    let mut numbers = []
    
    for c in chars {
      if c.is_digit() {
        current_number = current_number + c.to_string()
      } else {
        if current_number.length() > 0 {
          numbers = numbers.push(current_number.to_int())
          current_number = ""
        }
      }
    }
    
    if current_number.length() > 0 {
      numbers = numbers.push(current_number.to_int())
    }
    
    numbers
  }
  
  // 测试字符串转换
  assert_eq(camel_to_snake("camelCaseString"), "camel_case_string")
  assert_eq(camel_to_snake("XMLHttpRequest"), "xml_http_request")
  assert_eq(camel_to_snake("simple"), "simple")
  
  assert_eq(snake_to_camel("snake_case_string"), "snakeCaseString")
  assert_eq(snake_to_camel("xml_http_request"), "xmlHttpRequest")
  assert_eq(snake_to_camel("simple"), "simple")
  
  // 测试数字提取
  assert_eq(extract_numbers("abc123def456"), [123, 456])
  assert_eq(extract_numbers("no numbers here"), [])
  assert_eq(extract_numbers("123"), [123])
  assert_eq(extract_numbers("abc123def"), [123])
  assert_eq(extract_numbers("123def"), [123])
}

// 测试8: 时间和日期处理
test "时间和日期处理" {
  // 简化的时间戳处理
  let format_duration = fn(ms: Int) {
    if ms < 1000 {
      ms.to_string() + "ms"
    } else if ms < 60 * 1000 {
      let seconds = ms / 1000
      let remaining_ms = ms % 1000
      seconds.to_string() + "s " + remaining_ms.to_string() + "ms"
    } else if ms < 60 * 60 * 1000 {
      let minutes = ms / (60 * 1000)
      let remaining_ms = ms % (60 * 1000)
      let seconds = remaining_ms / 1000
      let final_ms = remaining_ms % 1000
      minutes.to_string() + "m " + seconds.to_string() + "s " + final_ms.to_string() + "ms"
    } else {
      let hours = ms / (60 * 60 * 1000)
      let remaining_ms = ms % (60 * 60 * 1000)
      let minutes = remaining_ms / (60 * 1000)
      let final_ms = remaining_ms % (60 * 1000)
      let seconds = final_ms / 1000
      let last_ms = final_ms % 1000
      hours.to_string() + "h " + minutes.to_string() + "m " + seconds.to_string() + "s " + last_ms.to_string() + "ms"
    }
  }
  
  let time_window_overlap = fn(start1: Int, end1: Int, start2: Int, end2: Int) {
    let overlap_start = if start1 > start2 { start1 } else { start2 }
    let overlap_end = if end1 < end2 { end1 } else { end2 }
    
    if overlap_start < overlap_end {
      Some(overlap_end - overlap_start)
    } else {
      None
    }
  }
  
  // 测试持续时间格式化
  assert_eq(format_duration(500), "500ms")
  assert_eq(format_duration(1500), "1s 500ms")
  assert_eq(format_duration(65000), "1m 5s 0ms")
  assert_eq(format_duration(3665000), "1h 1m 5s 0ms")
  
  // 测试时间窗口重叠
  // 完全重叠
  assert_eq(time_window_overlap(1000, 5000, 2000, 4000), Some(2000))
  
  // 部分重叠
  assert_eq(time_window_overlap(1000, 3000, 2000, 5000), Some(1000))
  
  // 无重叠
  assert_eq(time_window_overlap(1000, 2000, 3000, 4000), None)
  
  // 边界接触
  assert_eq(time_window_overlap(1000, 2000, 2000, 3000), None)
  
  // 包含关系
  assert_eq(time_window_overlap(1000, 6000, 2000, 3000), Some(1000))
}

// 测试9: 配置管理和验证
test "配置管理和验证" {
  // 定义配置类型
  type Config = {
    service_name: String,
    version: String,
    environment: String,
    log_level: String,
    max_connections: Int,
    timeout_ms: Int,
    enable_metrics: Bool,
    sampling_rate: Float
  }
  
  // 默认配置
  let default_config = {
    service_name: "azimuth-service",
    version: "1.0.0",
    environment: "development",
    log_level: "info",
    max_connections: 100,
    timeout_ms: 30000,
    enable_metrics: true,
    sampling_rate: 1.0
  }
  
  // 配置验证规则
  let validate_config = fn(config: Config) {
    let errors = []
    
    if config.service_name.length() == 0 {
      errors = errors.push("service_name不能为空")
    }
    
    if not ["debug", "info", "warn", "error"].contains(config.log_level) {
      errors = errors.push("log_level必须是debug、info、warn或error之一")
    }
    
    if config.max_connections <= 0 {
      errors = errors.push("max_connections必须大于0")
    }
    
    if config.timeout_ms <= 0 {
      errors = errors.push("timeout_ms必须大于0")
    }
    
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors = errors.push("sampling_rate必须在0.0到1.0之间")
    }
    
    errors
  }
  
  // 配置合并函数
  let merge_config = fn(base: Config, override: Config) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      version: if override.version != "" { override.version } else { base.version },
      environment: if override.environment != "" { override.environment } else { base.environment },
      log_level: if override.log_level != "" { override.log_level } else { base.log_level },
      max_connections: if override.max_connections != 0 { override.max_connections } else { base.max_connections },
      timeout_ms: if override.timeout_ms != 0 { override.timeout_ms } else { base.timeout_ms },
      enable_metrics: override.enable_metrics,
      sampling_rate: if override.sampling_rate >= 0.0 { override.sampling_rate } else { base.sampling_rate }
    }
  }
  
  // 测试默认配置验证
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = {
    service_name: "",
    version: "1.0.0",
    environment: "production",
    log_level: "invalid",
    max_connections: -1,
    timeout_ms: 0,
    enable_metrics: true,
    sampling_rate: 1.5
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 4)
  assert_true(invalid_errors.contains("service_name不能为空"))
  assert_true(invalid_errors.contains("log_level必须是debug、info、warn或error之一"))
  assert_true(invalid_errors.contains("max_connections必须大于0"))
  assert_true(invalid_errors.contains("sampling_rate必须在0.0到1.0之间"))
  
  // 测试配置合并
  let partial_override = {
    service_name: "payment-service",
    version: "",
    environment: "production",
    log_level: "debug",
    max_connections: 200,
    timeout_ms: 0,
    enable_metrics: false,
    sampling_rate: 0.5
  }
  
  let merged_config = merge_config(default_config, partial_override)
  assert_eq(merged_config.service_name, "payment-service")
  assert_eq(merged_config.version, "1.0.0")  // 保持默认值
  assert_eq(merged_config.environment, "production")
  assert_eq(merged_config.log_level, "debug")
  assert_eq(merged_config.max_connections, 200)
  assert_eq(merged_config.timeout_ms, 30000)  // 保持默认值
  assert_eq(merged_config.enable_metrics, false)
  assert_eq(merged_config.sampling_rate, 0.5)
  
  // 验证合并后的配置
  let merged_errors = validate_config(merged_config)
  assert_eq(merged_errors.length(), 0)
}