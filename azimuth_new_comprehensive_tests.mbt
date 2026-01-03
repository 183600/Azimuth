// Azimuth 综合测试套件
// 包含MoonBit语言特性和Azimuth遥测系统的综合测试用例

// 测试1: 高级数据结构操作
test "高级数据结构操作" {
  // 定义栈结构
  type Stack[T] = {
    items: Array[T],
    top: Int
  }
  
  // 栈操作
  let stack_push = fn(stack: Stack[T], item: T) {
    { items: stack.items.push(item), top: stack.top + 1 }
  }
  
  let stack_pop = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      let item = stack.items[stack.top]
      let new_items = stack.items.slice(0, stack.top)
      (Some(item), { items: new_items, top: stack.top - 1 })
    } else {
      (None, stack)
    }
  }
  
  let stack_peek = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      Some(stack.items[stack.top])
    } else {
      None
    }
  }
  
  // 测试栈操作
  let empty_stack: Stack[String] = { items: [], top: -1 }
  let stack1 = stack_push(empty_stack, "first")
  let stack2 = stack_push(stack1, "second")
  let stack3 = stack_push(stack2, "third")
  
  assert_eq(stack3.top, 2)
  assert_eq(stack_peek(stack3), Some("third"))
  
  let (popped_item, new_stack) = stack_pop(stack3)
  assert_eq(popped_item, Some("third"))
  assert_eq(new_stack.top, 1)
  assert_eq(stack_peek(new_stack), Some("second"))
  
  // 定义队列结构
  type Queue[T] = {
    enqueue: Array[T],
    dequeue: Array[T]
  }
  
  let queue_enqueue = fn(queue: Queue[T], item: T) {
    { enqueue: queue.enqueue.push(item), dequeue: queue.dequeue }
  }
  
  let queue_dequeue = fn(queue: Queue[T]) {
    if queue.dequeue.length() > 0 {
      let item = queue.dequeue[0]
      let new_dequeue = queue.dequeue.slice(1, queue.dequeue.length())
      (Some(item), { enqueue: queue.enqueue, dequeue: new_dequeue })
    } else if queue.enqueue.length() > 0 {
      let item = queue.enqueue[0]
      let new_enqueue = queue.enqueue.slice(1, queue.enqueue.length())
      (Some(item), { enqueue: new_enqueue, dequeue: [] })
    } else {
      (None, queue)
    }
  }
  
  // 测试队列操作
  let empty_queue: Queue[Int] = { enqueue: [], dequeue: [] }
  let queue1 = queue_enqueue(empty_queue, 10)
  let queue2 = queue_enqueue(queue1, 20)
  let queue3 = queue_enqueue(queue2, 30)
  
  let (dequeued1, queue_after_dequeue1) = queue_dequeue(queue3)
  assert_eq(dequeued1, Some(10))
  
  let (dequeued2, queue_after_dequeue2) = queue_dequeue(queue_after_dequeue1)
  assert_eq(dequeued2, Some(20))
  
  let (dequeued3, final_queue) = queue_dequeue(queue_after_dequeue2)
  assert_eq(dequeued3, Some(30))
  
  let (empty_dequeued, _) = queue_dequeue(final_queue)
  assert_eq(empty_dequeued, None)
}

// 测试2: 遥测数据处理和转换
test "遥测数据处理和转换" {
  // 定义遥测数据点
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 定义聚合窗口
  type AggregationWindow = {
    start_time: Int,
    end_time: Int,
    points: Array[TelemetryPoint]
  }
  
  // 创建时间窗口聚合器
  let create_time_windows = fn(points: Array[TelemetryPoint], window_size_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort(fn(a, b) {
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 }
    })
    
    let first_point = sorted_points[0]
    let mut windows = []
    let mut current_window = {
      start_time: first_point.timestamp,
      end_time: first_point.timestamp + window_size_ms,
      points: []
    }
    
    for point in sorted_points {
      if point.timestamp <= current_window.end_time {
        // 添加到当前窗口
        current_window = { 
          start_time: current_window.start_time,
          end_time: current_window.end_time,
          points: current_window.points.push(point)
        }
      } else {
        // 保存当前窗口并创建新窗口
        windows = windows.push(current_window)
        current_window = {
          start_time: point.timestamp,
          end_time: point.timestamp + window_size_ms,
          points: [point]
        }
      }
    }
    
    // 添加最后一个窗口
    windows = windows.push(current_window)
    windows
  }
  
  // 计算窗口统计信息
  let calculate_window_stats = fn(window: AggregationWindow) {
    if window.points.length() == 0 {
      return {
        count: 0,
        sum: 0.0,
        min: 0.0,
        max: 0.0,
        avg: 0.0
      }
    }
    
    let values = window.points.map(fn(p) { p.value })
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let min = sorted_values[0]
    let max = sorted_values[count - 1]
    let avg = sum / (count as Float)
    
    {
      count,
      sum,
      min,
      max,
      avg
    }
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, metric_name: "cpu_usage", value: 25.5, tags: [("host", "server1")] },
    { timestamp: 1500, metric_name: "cpu_usage", value: 30.2, tags: [("host", "server1")] },
    { timestamp: 2500, metric_name: "cpu_usage", value: 45.8, tags: [("host", "server1")] },
    { timestamp: 3500, metric_name: "cpu_usage", value: 40.1, tags: [("host", "server1")] },
    { timestamp: 5500, metric_name: "cpu_usage", value: 35.7, tags: [("host", "server1")] },
    { timestamp: 6500, metric_name: "cpu_usage", value: 28.9, tags: [("host", "server1")] }
  ]
  
  // 测试时间窗口创建 (2秒窗口)
  let windows = create_time_windows(test_points, 2000)
  assert_eq(windows.length(), 3)
  
  // 第一个窗口应该包含前两个点 (1000, 1500)
  let window1_stats = calculate_window_stats(windows[0])
  assert_eq(window1_stats.count, 2)
  assert_eq(window1_stats.sum.round(), 56.0)
  assert_eq(window1_stats.min, 25.5)
  assert_eq(window1_stats.max, 30.2)
  
  // 第二个窗口应该包含第3和第4个点 (2500, 3500)
  let window2_stats = calculate_window_stats(windows[1])
  assert_eq(window2_stats.count, 2)
  assert_eq(window2_stats.sum.round(), 86.0)
  assert_eq(window2_stats.min, 40.1)
  assert_eq(window2_stats.max, 45.8)
  
  // 第三个窗口应该包含最后两个点 (5500, 6500)
  let window3_stats = calculate_window_stats(windows[2])
  assert_eq(window3_stats.count, 2)
  assert_eq(window3_stats.sum.round(), 65.0)
  assert_eq(window3_stats.min, 28.9)
  assert_eq(window3_stats.max, 35.7)
}

// 测试3: 高级字符串处理和模式匹配
test "高级字符串处理和模式匹配" {
  // 定义日志级别枚举
  enum LogLevel {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // 定义日志条目
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    context: Array[(String, String)]
  }
  
  // 解析日志级别
  let parse_log_level = fn(level_str: String) {
    match level_str.to_lowercase() {
      "debug" => Some(LogLevel::Debug)
      "info" => Some(LogLevel::Info)
      "warning" => Some(LogLevel::Warning)
      "error" => Some(LogLevel::Error)
      "critical" => Some(LogLevel::Critical)
      _ => None
    }
  }
  
  // 格式化日志级别
  let format_log_level = fn(level: LogLevel) {
    match level {
      LogLevel::Debug => "DEBUG"
      LogLevel::Info => "INFO"
      LogLevel::Warning => "WARNING"
      LogLevel::Error => "ERROR"
      LogLevel::Critical => "CRITICAL"
    }
  }
  
  // 解析日志行
  let parse_log_line = fn(line: String) {
    // 简化的日志格式: [TIMESTAMP] [LEVEL] MESSAGE
    if line.length() < 10 {
      return None
    }
    
    let timestamp_start = line.index_of("[") + 1
    let timestamp_end = line.index_of("]", timestamp_start)
    
    if timestamp_start == 0 || timestamp_end == -1 {
      return None
    }
    
    let timestamp_str = line.substring(timestamp_start, timestamp_end - timestamp_start)
    let timestamp = match timestamp_str.to_int() {
      Some(t) => t
      None => return None
    }
    
    let level_start = line.index_of("[", timestamp_end) + 1
    let level_end = line.index_of("]", level_start)
    
    if level_start == 0 || level_end == -1 {
      return None
    }
    
    let level_str = line.substring(level_start, level_end - level_start)
    let level = match parse_log_level(level_str) {
      Some(l) => l
      None => return None
    }
    
    let message_start = level_end + 2  // 跳过 "] "
    let message = if message_start < line.length() {
      line.substring(message_start, line.length() - message_start)
    } else {
      ""
    }
    
    Some({
      timestamp,
      level,
      message,
      context: []
    })
  }
  
  // 测试日志级别解析
  assert_eq(parse_log_level("INFO"), Some(LogLevel::Info))
  assert_eq(parse_log_level("error"), Some(LogLevel::Error))
  assert_eq(parse_log_level("Warning"), Some(LogLevel::Warning))
  assert_eq(parse_log_level("invalid"), None)
  
  // 测试日志级别格式化
  assert_eq(format_log_level(LogLevel::Debug), "DEBUG")
  assert_eq(format_log_level(LogLevel::Critical), "CRITICAL")
  
  // 测试日志行解析
  let valid_log = "[1640995200] [INFO] Application started successfully"
  let parsed_log = parse_log_line(valid_log)
  
  match parsed_log {
    Some(entry) => {
      assert_eq(entry.timestamp, 1640995200)
      assert_eq(entry.level, LogLevel::Info)
      assert_eq(entry.message, "Application started successfully")
    }
    None => assert_true(false)
  }
  
  let invalid_log = "Invalid log format"
  let not_parsed = parse_log_line(invalid_log)
  assert_eq(not_parsed, None)
  
  // 按级别过滤日志
  let filter_logs_by_level = fn(logs: Array[LogEntry], min_level: LogLevel) {
    let level_value = fn(level: LogLevel) {
      match level {
        LogLevel::Debug => 0
        LogLevel::Info => 1
        LogLevel::Warning => 2
        LogLevel::Error => 3
        LogLevel::Critical => 4
      }
    }
    
    let min_value = level_value(min_level)
    logs.filter(fn(entry) { level_value(entry.level) >= min_value })
  }
  
  // 创建测试日志
  let test_logs = [
    { timestamp: 1000, level: LogLevel::Debug, message: "Debug message", context: [] },
    { timestamp: 2000, level: LogLevel::Info, message: "Info message", context: [] },
    { timestamp: 3000, level: LogLevel::Warning, message: "Warning message", context: [] },
    { timestamp: 4000, level: LogLevel::Error, message: "Error message", context: [] },
    { timestamp: 5000, level: LogLevel::Critical, message: "Critical message", context: [] }
  ]
  
  // 测试日志过滤
  let warning_and_above = filter_logs_by_level(test_logs, LogLevel::Warning)
  assert_eq(warning_and_above.length(), 3)
  assert_eq(warning_and_above[0].level, LogLevel::Warning)
  assert_eq(warning_and_above[1].level, LogLevel::Error)
  assert_eq(warning_and_above[2].level, LogLevel::Critical)
  
  let error_and_above = filter_logs_by_level(test_logs, LogLevel::Error)
  assert_eq(error_and_above.length(), 2)
  assert_eq(error_and_above[0].level, LogLevel::Error)
  assert_eq(error_and_above[1].level, LogLevel::Critical)
}

// 测试4: 遥测配置管理
test "遥测配置管理" {
  // 定义配置值类型
  enum ConfigValue {
    StringVal(String)
    IntVal(Int)
    FloatVal(Float)
    BoolVal(Bool)
    ArrayVal(Array[String])
  }
  
  // 定义配置
  type TelemetryConfig = {
    sampling_rate: Float,
    batch_size: Int,
    export_interval_ms: Int,
    enabled_metrics: Array[String],
    custom_attributes: Array[(String, String)]
  }
  
  // 默认配置
  let default_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    export_interval_ms: 5000,
    enabled_metrics: ["cpu", "memory", "disk"],
    custom_attributes: []
  }
  
  // 从环境变量加载配置
  let load_config_from_env = fn() {
    let config = default_config
    
    // 模拟环境变量读取
    let get_env = fn(key: String) {
      match key {
        "TELEMETRY_SAMPLING_RATE" => Some("0.5")
        "TELEMETRY_BATCH_SIZE" => Some("200")
        "TELEMETRY_EXPORT_INTERVAL" => Some("10000")
        "TELEMETRY_ENABLED_METRICS" => Some("cpu,memory,network,disk")
        _ => None
      }
    }
    
    // 更新采样率
    let sampling_rate = match get_env("TELEMETRY_SAMPLING_RATE") {
      Some(rate_str) => match rate_str.to_float() {
        Some(rate) => rate
        None => config.sampling_rate
      }
      None => config.sampling_rate
    }
    
    // 更新批处理大小
    let batch_size = match get_env("TELEMETRY_BATCH_SIZE") {
      Some(size_str) => match size_str.to_int() {
        Some(size) => size
        None => config.batch_size
      }
      None => config.batch_size
    }
    
    // 更新导出间隔
    let export_interval = match get_env("TELEMETRY_EXPORT_INTERVAL") {
      Some(interval_str) => match interval_str.to_int() {
        Some(interval) => interval
        None => config.export_interval_ms
      }
      None => config.export_interval_ms
    }
    
    // 更新启用的指标
    let enabled_metrics = match get_env("TELEMETRY_ENABLED_METRICS") {
      Some(metrics_str) => metrics_str.split(",")
      None => config.enabled_metrics
    }
    
    {
      sampling_rate,
      batch_size,
      export_interval_ms: export_interval,
      enabled_metrics,
      custom_attributes: config.custom_attributes
    }
  }
  
  // 配置验证
  let validate_config = fn(config: TelemetryConfig) {
    let errors = []
    
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors = errors.push("采样率必须在0.0到1.0之间")
    }
    
    if config.batch_size <= 0 {
      errors = errors.push("批处理大小必须大于0")
    }
    
    if config.export_interval_ms <= 0 {
      errors = errors.push("导出间隔必须大于0")
    }
    
    if config.enabled_metrics.length() == 0 {
      errors = errors.push("至少需要启用一个指标")
    }
    
    errors
  }
  
  // 测试配置加载
  let loaded_config = load_config_from_env()
  assert_eq(loaded_config.sampling_rate, 0.5)
  assert_eq(loaded_config.batch_size, 200)
  assert_eq(loaded_config.export_interval_ms, 10000)
  assert_eq(loaded_config.enabled_metrics, ["cpu", "memory", "network", "disk"])
  
  // 测试默认配置验证
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = {
    sampling_rate: 1.5,  // 无效：大于1.0
    batch_size: 0,       // 无效：等于0
    export_interval_ms: -1000,  // 无效：负数
    enabled_metrics: [],  // 无效：空数组
    custom_attributes: []
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 4)
  assert_true(invalid_errors.contains("采样率必须在0.0到1.0之间"))
  assert_true(invalid_errors.contains("批处理大小必须大于0"))
  assert_true(invalid_errors.contains("导出间隔必须大于0"))
  assert_true(invalid_errors.contains("至少需要启用一个指标"))
  
  // 配置合并
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      sampling_rate: override.sampling_rate,
      batch_size: override.batch_size,
      export_interval_ms: override.export_interval_ms,
      enabled_metrics: if override.enabled_metrics.length() > 0 {
        override.enabled_metrics
      } else {
        base.enabled_metrics
      },
      custom_attributes: base.custom_attributes + override.custom_attributes
    }
  }
  
  // 测试配置合并
  let partial_override = {
    sampling_rate: 0.8,
    batch_size: 150,
    export_interval_ms: 8000,
    enabled_metrics: [],  // 使用基础配置
    custom_attributes: [("env", "production")]
  }
  
  let merged_config = merge_configs(default_config, partial_override)
  assert_eq(merged_config.sampling_rate, 0.8)
  assert_eq(merged_config.batch_size, 150)
  assert_eq(merged_config.export_interval_ms, 8000)
  assert_eq(merged_config.enabled_metrics, ["cpu", "memory", "disk"])  // 来自基础配置
  assert_eq(merged_config.custom_attributes, [("env", "production")])
}

// 测试5: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义遥测事件
  type TelemetryEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: Array[(String, String)],
    metadata: Array[(String, String)]
  }
  
  // 简化的JSON编码器
  let json_encode_string = fn(s: String) {
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  }
  
  let json_encode_event = fn(event: TelemetryEvent) {
    let encode_pairs = fn(pairs: Array[(String, String)>) {
      if pairs.length() == 0 {
        "[]"
      } else {
        let encoded_pairs = pairs.map(fn(pair) {
          let (key, value) = pair
          json_encode_string(key) + ":" + json_encode_string(value)
        })
        "[" + encoded_pairs.join(",") + "]"
      }
    }
    
    "{"
      + "\"event_id\":" + json_encode_string(event.event_id) + ","
      + "\"timestamp\":" + event.timestamp.to_string() + ","
      + "\"event_type\":" + json_encode_string(event.event_type) + ","
      + "\"data\":" + encode_pairs(event.data) + ","
      + "\"metadata\":" + encode_pairs(event.metadata)
      + "}"
  }
  
  // 简化的JSON解析器
  let json_decode_string = fn(s: String) {
    if s.length() >= 2 && s[0] == '\"' && s[s.length() - 1] == '\"' {
      Some(s.substring(1, s.length() - 2).replace("\\\"", "\"").replace("\\\\", "\\"))
    } else {
      None
    }
  }
  
  let json_decode_int = fn(s: String) {
    s.to_int()
  }
  
  let json_decode_pairs = fn(s: String) {
    if s.length() >= 2 && s[0] == '[' && s[s.length() - 1] == ']' {
      let content = s.substring(1, s.length() - 2)
      if content.length() == 0 {
        return []
      }
      
      // 简化的键值对解析
      let pairs = content.split(",")
      let result = []
      
      for pair in pairs {
        let colon_index = pair.index_of(":")
        if colon_index > 0 {
          let key_part = pair.substring(0, colon_index)
          let value_part = pair.substring(colon_index + 1, pair.length() - colon_index - 1)
          
          match (json_decode_string(key_part), json_decode_string(value_part)) {
            (Some(key), Some(value)) => {
              result = result.push((key, value))
            }
            _ => {}  // 忽略无效的键值对
          }
        }
      }
      
      result
    } else {
      []
    }
  }
  
  let json_decode_event = fn(json: String) {
    // 简化的JSON解析，实际实现会更复杂
    let event_id_start = json.index_of("\"event_id\":") + 12
    let event_id_end = json.index_of("\"", event_id_start)
    let event_id_str = json.substring(event_id_start, event_id_end - event_id_start)
    
    let timestamp_start = json.index_of("\"timestamp\":") + 12
    let timestamp_end = json.index_of(",", timestamp_start)
    let timestamp_str = json.substring(timestamp_start, timestamp_end - timestamp_start)
    
    let event_type_start = json.index_of("\"event_type\":") + 14
    let event_type_end = json.index_of("\"", event_type_start)
    let event_type_str = json.substring(event_type_start, event_type_end - event_type_start)
    
    let data_start = json.index_of("\"data\":[") + 8
    let data_end = json.index_of("]", data_start) + 1
    let data_str = json.substring(data_start, data_end - data_start)
    
    let metadata_start = json.index_of("\"metadata\":[") + 12
    let metadata_end = json.index_of("]", metadata_start) + 1
    let metadata_str = json.substring(metadata_start, metadata_end - metadata_start)
    
    match (json_decode_string(event_id_str), json_decode_int(timestamp_str), json_decode_string(event_type_str)) {
      (Some(event_id), Some(timestamp), Some(event_type)) => {
        Some({
          event_id,
          timestamp,
          event_type,
          data: json_decode_pairs(data_str),
          metadata: json_decode_pairs(metadata_str)
        })
      }
      _ => None
    }
  }
  
  // 创建测试事件
  let test_event = {
    event_id: "evt-12345",
    timestamp: 1640995200,
    event_type: "user_action",
    data: [
      ("action", "click"),
      ("target", "submit_button"),
      ("page", "/checkout")
    ],
    metadata: [
      ("user_id", "user-67890"),
      ("session_id", "sess-abcdef"),
      ("ip_address", "192.168.1.1")
    ]
  }
  
  // 测试序列化
  let json = json_encode_event(test_event)
  assert_true(json.contains("\"event_id\":\"evt-12345\""))
  assert_true(json.contains("\"timestamp\":1640995200"))
  assert_true(json.contains("\"event_type\":\"user_action\""))
  assert_true(json.contains("\"action\":\"click\""))
  assert_true(json.contains("\"user_id\":\"user-67890\""))
  
  // 测试反序列化
  let decoded_event = json_decode_event(json)
  
  match decoded_event {
    Some(event) => {
      assert_eq(event.event_id, "evt-12345")
      assert_eq(event.timestamp, 1640995200)
      assert_eq(event.event_type, "user_action")
      assert_eq(event.data.length(), 3)
      assert_eq(event.metadata.length(), 3)
      
      // 检查数据字段
      let action_found = event.data.any(fn(pair) {
        let (key, value) = pair
        key == "action" && value == "click"
      })
      assert_true(action_found)
      
      // 检查元数据字段
      let user_id_found = event.metadata.any(fn(pair) {
        let (key, value) = pair
        key == "user_id" && value == "user-67890"
      })
      assert_true(user_id_found)
    }
    None => assert_true(false)
  }
  
  // 测试批量序列化
  let batch_serialize_events = fn(events: Array[TelemetryEvent]) {
    let json_events = events.map(json_encode_event)
    "[" + json_events.join(",") + "]"
  }
  
  let test_events = [
    test_event,
    {
      event_id: "evt-12346",
      timestamp: 1640995201,
      event_type: "page_view",
      data: [("page", "/home")],
      metadata: [("user_id", "user-67890")]
    }
  ]
  
  let batch_json = batch_serialize_events(test_events)
  assert_true(batch_json.startswith("["))
  assert_true(batch_json.endswith("]"))
  assert_true(batch_json.contains("\"evt-12345\""))
  assert_true(batch_json.contains("\"evt-12346\""))
}

// 测试6: 遥测数据压缩
test "遥测数据压缩" {
  // 定义压缩策略
  enum CompressionStrategy {
    NoCompression
    Gzip
    Lz4
    Custom(String)  // String表示自定义算法名称
  }
  
  // 定义压缩结果
  type CompressionResult = {
    compressed_data: Array[Byte],
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float
  }
  
  // 模拟压缩函数
  let compress_data = fn(data: Array[Byte], strategy: CompressionStrategy) {
    match strategy {
      CompressionStrategy::NoCompression => {
        {
          compressed_data: data,
          original_size: data.length(),
          compressed_size: data.length(),
          compression_ratio: 1.0
        }
      }
      CompressionStrategy::Gzip => {
        // 模拟Gzip压缩（实际压缩率约为30-70%）
        let compressed_size = (data.length() as Float * 0.4) as Int
        let compressed_data = []  // 实际实现会包含压缩后的数据
        {
          compressed_data,
          original_size: data.length(),
          compressed_size,
          compression_ratio: (data.length() as Float) / (compressed_size as Float)
        }
      }
      CompressionStrategy::Lz4 => {
        // 模拟LZ4压缩（实际压缩率约为40-60%，但速度更快）
        let compressed_size = (data.length() as Float * 0.5) as Int
        let compressed_data = []  // 实际实现会包含压缩后的数据
        {
          compressed_data,
          original_size: data.length(),
          compressed_size,
          compression_ratio: (data.length() as Float) / (compressed_size as Float)
        }
      }
      CompressionStrategy::Custom(algorithm) => {
        // 模拟自定义压缩算法
        let compressed_size = match algorithm {
          "fast" => (data.length() as Float * 0.7) as Int
          "balanced" => (data.length() as Float * 0.5) as Int
          "max" => (data.length() as Float * 0.3) as Int
          _ => data.length()
        }
        let compressed_data = []
        {
          compressed_data,
          original_size: data.length(),
          compressed_size,
          compression_ratio: (data.length() as Float) / (compressed_size as Float)
        }
      }
    }
  }
  
  // 选择最佳压缩策略
  let select_compression_strategy = fn(data_size: Int, performance_priority: String) {
    if data_size < 1024 {  // 小于1KB
      CompressionStrategy::NoCompression
    } else if performance_priority == "speed" {
      CompressionStrategy::Lz4
    } else if performance_priority == "ratio" {
      CompressionStrategy::Gzip
    } else {
      CompressionStrategy::Custom("balanced")
    }
  }
  
  // 测试压缩策略选择
  assert_eq(select_compression_strategy(512, "speed"), CompressionStrategy::NoCompression)
  assert_eq(select_compression_strategy(2048, "speed"), CompressionStrategy::Lz4)
  assert_eq(select_compression_strategy(2048, "ratio"), CompressionStrategy::Gzip)
  assert_eq(select_compression_strategy(2048, "balanced"), CompressionStrategy::Custom("balanced"))
  
  // 测试数据压缩
  let test_data = []  // 模拟2KB数据
  for i in 0..2048 {
    test_data = test_data.push((i % 256) as Byte)
  }
  
  // 测试不同压缩策略
  let no_compression = compress_data(test_data, CompressionStrategy::NoCompression)
  assert_eq(no_compression.original_size, 2048)
  assert_eq(no_compression.compressed_size, 2048)
  assert_eq(no_compression.compression_ratio, 1.0)
  
  let gzip_compression = compress_data(test_data, CompressionStrategy::Gzip)
  assert_eq(gzip_compression.original_size, 2048)
  assert_eq(gzip_compression.compressed_size, 819)  // 2048 * 0.4 ≈ 819
  assert_eq(gzip_compression.compression_ratio.round(), 2.5)  // 2048 / 819 ≈ 2.5
  
  let lz4_compression = compress_data(test_data, CompressionStrategy::Lz4)
  assert_eq(lz4_compression.original_size, 2048)
  assert_eq(lz4_compression.compressed_size, 1024)  // 2048 * 0.5 = 1024
  assert_eq(lz4_compression.compression_ratio, 2.0)  // 2048 / 1024 = 2.0
  
  let custom_fast_compression = compress_data(test_data, CompressionStrategy::Custom("fast"))
  assert_eq(custom_fast_compression.original_size, 2048)
  assert_eq(custom_fast_compression.compressed_size, 1433)  // 2048 * 0.7 ≈ 1433
  assert_eq(custom_fast_compression.compression_ratio.round(), 1.43)  // 2048 / 1433 ≈ 1.43
  
  // 压缩效率比较
  let compare_compression_efficiency = fn(strategies: Array<CompressionStrategy>) {
    let results = strategies.map(fn(strategy) {
      let result = compress_data(test_data, strategy)
      (strategy, result.compression_ratio)
    })
    
    // 按压缩比排序（降序）
    results.sort(fn(a, b) {
      let (_, ratio_a) = a
      let (_, ratio_b) = b
      if ratio_a > ratio_b { -1 } else if ratio_a < ratio_b { 1 } else { 0 }
    })
  }
  
  let efficiency_comparison = compare_compression_efficiency([
    CompressionStrategy::NoCompression,
    CompressionStrategy::Gzip,
    CompressionStrategy::Lz4,
    CompressionStrategy::Custom("fast"),
    CompressionStrategy::Custom("max")
  ])
  
  // Gzip应该有最高的压缩比
  assert_eq(efficiency_comparison[0].0, CompressionStrategy::Gzip)
  // NoCompression应该有最低的压缩比（1.0）
  assert_eq(efficiency_comparison[4].0, CompressionStrategy::NoCompression)
}

// 测试7: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    sample_rate: Float,
    attributes: Array[(String, String)]
  }
  
  // 定义采样器接口
  trait Sampler {
    should_sample(trace_id: String, name: String, attributes: Array[(String, String)>) -> SamplingDecision
  }
  
  // 基于概率的采样器
  let probability_sampler = fn(sample_rate: Float) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      // 简化的采样算法：基于trace_id的哈希值
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      let sampled = normalized <= sample_rate
      
      {
        sampled,
        sample_rate,
        attributes: attributes.push(("sampler.type", "probability"))
      }
    }
  }
  
  // 基于属性的采样器
  let attribute_sampler = fn(rules: Array<(String, String, Float)>) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      // 查找匹配的规则
      let matched_rules = rules.filter(fn(rule) {
        let (attr_key, attr_value, _) = rule
        attributes.any(fn(attr) {
          let (key, value) = attr
          key == attr_key && value == attr_value
        })
      })
      
      if matched_rules.length() > 0 {
        // 使用第一个匹配规则的采样率
        let (_, _, sample_rate) = matched_rules[0]
        {
          sampled: true,
          sample_rate,
          attributes: attributes.push(("sampler.type", "attribute"))
        }
      } else {
        // 没有匹配的规则，使用默认采样率
        {
          sampled: false,
          sample_rate: 0.0,
          attributes: attributes.push(("sampler.type", "attribute"))
        }
      }
    }
  }
  
  // 组合采样器
  let composite_sampler = fn(samplers: Array[(String) -> SamplingDecision>) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      let decisions = samplers.map(fn(sampler) {
        sampler(trace_id, name, attributes)
      })
      
      // 如果任何采样器决定采样，则采样
      let sampled = decisions.any(fn(decision) { decision.sampled })
      
      // 使用最高的采样率
      let max_sample_rate = decisions.reduce(0.0, fn(acc, decision) {
        if decision.sample_rate > acc {
          decision.sample_rate
        } else {
          acc
        }
      })
      
      // 合并所有属性
      let all_attributes = decisions.reduce([], fn(acc, decision) {
        acc + decision.attributes
      })
      
      {
        sampled,
        sample_rate: max_sample_rate,
        attributes: all_attributes
      }
    }
  }
  
  // 测试概率采样器
  let prob_sampler = probability_sampler(0.5)  // 50%采样率
  let decision1 = prob_sampler("trace-12345", "operation", [])
  
  // 注意：实际结果取决于哈希计算，这里只是示例
  assert_eq(decision1.sample_rate, 0.5)
  assert_true(decision1.attributes.contains(("sampler.type", "probability")))
  
  // 测试属性采样器
  let attr_rules = [
    ("service.name", "critical-service", 1.0),  // 总是采样
    ("service.name", "important-service", 0.8),  // 80%采样率
    ("environment", "production", 0.5)  // 50%采样率
  ]
  let attr_sampler_instance = attribute_sampler(attr_rules)
  
  let decision2 = attr_sampler_instance("trace-67890", "operation", [
    ("service.name", "critical-service"),
    ("environment", "production")
  ])
  assert_true(decision2.sampled)
  assert_eq(decision2.sample_rate, 1.0)
  
  let decision3 = attr_sampler_instance("trace-11111", "operation", [
    ("service.name", "regular-service"),
    ("environment", "development")
  ])
  assert_false(decision3.sampled)
  assert_eq(decision3.sample_rate, 0.0)
  
  // 测试组合采样器
  let comp_sampler = composite_sampler([
    probability_sampler(0.3),
    attribute_sampler(attr_rules)
  ])
  
  let decision4 = comp_sampler("trace-22222", "operation", [
    ("service.name", "important-service")
  ])
  // 应该被采样，因为属性采样器决定采样
  assert_true(decision4.sampled)
  assert_eq(decision4.sample_rate, 0.8)
  
  let decision5 = comp_sampler("trace-33333", "operation", [
    ("service.name", "regular-service")
  ])
  // 取决于概率采样器的决策
  assert_eq(decision5.sample_rate, 0.3)
  
  // 采样率限制器
  let rate_limiting_sampler = fn(base_sampler: (String) -> SamplingDecision, max_samples_per_second: Int) {
    let mut samples_this_second = 0
    let mut last_second = Time::now() / 1000
    
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      let current_second = Time::now() / 1000
      
      if current_second > last_second {
        samples_this_second = 0
        last_second = current_second
      }
      
      let base_decision = base_sampler(trace_id, name, attributes)
      
      if base_decision.sampled && samples_this_second < max_samples_per_second {
        samples_this_second = samples_this_second + 1
        {
          sampled: true,
          sample_rate: base_decision.sample_rate,
          attributes: base_decision.attributes.push(("sampler.type", "rate_limited"))
        }
      } else {
        {
          sampled: false,
          sample_rate: 0.0,
          attributes: base_decision.attributes.push(("sampler.type", "rate_limited"))
        }
      }
    }
  }
  
  // 测试速率限制采样器
  let rate_limited = rate_limiting_sampler(probability_sampler(1.0), 2)  // 每秒最多2个样本
  
  // 模拟在同一秒内的多次采样决策
  let rl_decision1 = rate_limited("trace-44444", "operation", [])
  let rl_decision2 = rate_limited("trace-55555", "operation", [])
  let rl_decision3 = rate_limited("trace-66666", "operation", [])
  
  // 前两个应该被采样，第三个应该被限制
  assert_true(rl_decision1.sampled)
  assert_true(rl_decision2.sampled)
  assert_false(rl_decision3.sampled)
}

// 测试8: 遥测数据导出和批处理
test "遥测数据导出和批处理" {
  // 定义导出项
  type ExportItem = {
    id: String,
    data: Array[Byte],
    timestamp: Int,
    retry_count: Int
  }
  
  // 定义导出结果
  enum ExportResult {
    Success(String)  // String表示导出ID
    RetryableError(String, Int)  // String表示错误信息，Int表示重试延迟(ms)
    FatalError(String)  // String表示错误信息
  }
  
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_batch_time_ms: Int,
    max_retry_attempts: Int
  }
  
  // 默认批处理配置
  let default_batch_config = {
    max_batch_size: 100,
    max_batch_time_ms: 5000,
    max_retry_attempts: 3
  }
  
  // 模拟导出函数
  let export_batch = fn(batch: Array[ExportItem]) {
    if batch.length() == 0 {
      return ExportResult::FatalError("空批次")
    }
    
    // 模拟导出失败（基于批次大小）
    if batch.length() > 150 {
      ExportResult::RetryableError("批次过大", 1000)
    } else if batch.length() > 120 {
      ExportResult::RetryableError("服务器繁忙", 2000)
    } else {
      let batch_id = "batch-" + Time::now().to_string()
      ExportResult::Success(batch_id)
    }
  }
  
  // 批处理器
  let batch_processor = fn(config: BatchConfig) {
    let mut pending_items = []
    let mut last_export_time = Time::now()
    
    fn(item: ExportItem) -> (Array[ExportResult], Array[ExportItem>) {
      // 添加到待处理队列
      pending_items = pending_items.push(item)
      
      let mut results = []
      let current_time = Time::now()
      let should_export = 
        pending_items.length() >= config.max_batch_size ||
        (current_time - last_export_time) >= config.max_batch_time_ms
      
      if should_export && pending_items.length() > 0 {
        // 处理重试项
        let retry_items = pending_items.filter(fn(item) { item.retry_count > 0 })
        let new_items = pending_items.filter(fn(item) { item.retry_count == 0 })
        
        // 导出新项
        if new_items.length() > 0 {
          let result = export_batch(new_items)
          results = results.push(result)
          
          match result {
            ExportResult::Success(_) => {
              // 成功，从队列中移除
              pending_items = retry_items
            }
            ExportResult::RetryableError(_, delay) => {
              // 重试错误，增加重试计数
              pending_items = retry_items + new_items.map(fn(item) {
                { id: item.id, data: item.data, timestamp: item.timestamp, retry_count: item.retry_count + 1 }
              })
            }
            ExportResult::FatalError(_) => {
              // 致命错误，从队列中移除
              pending_items = retry_items
            }
          }
        }
        
        // 处理重试项
        if retry_items.length() > 0 {
          let retry_result = export_batch(retry_items)
          results = results.push(retry_result)
          
          match retry_result {
            ExportResult::Success(_) => {
              pending_items = []
            }
            ExportResult::RetryableError(_, delay) => {
              pending_items = retry_items.map(fn(item) {
                { id: item.id, data: item.data, timestamp: item.timestamp, retry_count: item.retry_count + 1 }
              })
            }
            ExportResult::FatalError(_) => {
              pending_items = []
            }
          }
        }
        
        last_export_time = current_time
      }
      
      (results, pending_items)
    }
  }
  
  // 测试批处理
  let processor = batch_processor(default_batch_config)
  
  // 添加少量项目，不应触发导出
  let item1 = { id: "item-1", data: [], timestamp: Time::now(), retry_count: 0 }
  let (results1, pending1) = processor(item1)
  assert_eq(results1.length(), 0)
  assert_eq(pending1.length(), 1)
  
  // 添加更多项目，达到批处理大小
  let mut items = []
  for i in 2..=100 {
    items = items.push({ id: "item-" + i.to_string(), data: [], timestamp: Time::now(), retry_count: 0 })
  }
  
  let (results2, pending2) = processor(items[0])
  assert_eq(results2.length(), 1)  // 应该触发一次导出
  assert_eq(pending2.length(), 0)  // 所有项目应该被成功导出
  
  // 测试重试逻辑
  let large_batch = []
  for i in 1..=160 {
    large_batch = large_batch.push({ id: "large-item-" + i.to_string(), data: [], timestamp: Time::now(), retry_count: 0 })
  }
  
  let (results3, pending3) = processor(large_batch[0])
  assert_eq(results3.length(), 1)
  assert_eq(pending3.length(), 160)  // 所有项目应该保留在队列中，因为导出失败
  
  // 再次处理，应该增加重试计数
  let (results4, pending4) = processor(large_batch[1])
  assert_eq(results4.length(), 1)
  assert_eq(pending4.length(), 160)
  assert_eq(pending4[0].retry_count, 2)  // 重试计数应该增加
  
  // 测试导出结果分析
  let analyze_export_results = fn(results: Array[ExportResult>) {
    let mut success_count = 0
    let mut retry_count = 0
    let mut fatal_count = 0
    
    for result in results {
      match result {
        ExportResult::Success(_) => success_count = success_count + 1
        ExportResult::RetryableError(_, _) => retry_count = retry_count + 1
        ExportResult::FatalError(_) => fatal_count = fatal_count + 1
      }
    }
    
    {
      success_count,
      retry_count,
      fatal_count,
      total_count: results.length()
    }
  }
  
  // 测试结果分析
  let test_results = [
    ExportResult::Success("batch-1"),
    ExportResult::RetryableError("服务器繁忙", 1000),
    ExportResult::Success("batch-2"),
    ExportResult::FatalError("配置错误"),
    ExportResult::RetryableError("网络超时", 2000)
  ]
  
  let analysis = analyze_export_results(test_results)
  assert_eq(analysis.success_count, 2)
  assert_eq(analysis.retry_count, 2)
  assert_eq(analysis.fatal_count, 1)
  assert_eq(analysis.total_count, 5)
}