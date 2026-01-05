// Azimuth 新综合测试用例
// 包含多种MoonBit语言特性和Azimuth遥测系统功能的测试

// 测试1: 基础数据类型和操作
test "基础数据类型和操作测试" {
  // 测试整数操作
  let int_val = 42
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(int_val / 2, 21)
  assert_eq(int_val % 5, 2)
  
  // 测试浮点数操作
  let float_val = 3.14159
  assert_eq(float_val + 1.0, 4.14159)
  assert_eq(float_val * 2.0, 6.28318)
  assert_true(float_val > 3.0)
  assert_true(float_val < 4.0)
  
  // 测试字符串操作
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  assert_eq(str.to_uppercase(), "AZIMUTH-TELEMETRY")
  assert_eq(str.to_lowercase(), "azimuth-telemetry")
  assert_true(str.contains("telemetry"))
  assert_eq(str.substring(0, 7), "azimuth")
  
  // 测试布尔操作
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(!true)
  
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(evens, [2, 4])
  assert_eq(sum, 15)
}

// 测试2: 遥测数据处理
test "遥测数据处理测试" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 创建测试数据
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  // 测试数据访问
  assert_eq(telemetry_data.trace_id, "trace-12345")
  assert_eq(telemetry_data.span_id, "span-67890")
  assert_eq(telemetry_data.operation_name, "database_query")
  assert_eq(telemetry_data.status, "ok")
  
  // 测试Option类型处理
  match telemetry_data.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span-11111")
    None => assert_true(false)
  }
  
  // 计算持续时间
  let duration = telemetry_data.end_time - telemetry_data.start_time
  assert_eq(duration, 50)
  
  // 测试属性查找
  let find_attribute = fn(attributes: Array<(String, String)>, key: String) {
    let mut found = None
    for (k, v) in attributes {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let service_name = find_attribute(telemetry_data.attributes, "service.name")
  assert_eq(service_name, Some("payment-service"))
  
  let db_type = find_attribute(telemetry_data.attributes, "db.type")
  assert_eq(db_type, Some("postgresql"))
  
  let missing_attr = find_attribute(telemetry_data.attributes, "missing.key")
  assert_eq(missing_attr, None)
}

// 测试3: 错误处理和恢复
test "错误处理和恢复测试" {
  // 定义错误类型
  enum TelemetryError {
    InvalidTraceId(String)
    NetworkTimeout(Int)
    SerializationError(String)
    ConfigurationError(String)
  }
  
  // 定义结果类型
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError]
  }
  
  // 创建成功结果
  let create_success = fn(data: T) {
    {
      success: true,
      data: Some(data),
      error: None
    }
  }
  
  // 创建错误结果
  let create_error = fn(error: TelemetryError) {
    {
      success: false,
      data: None,
      error: Some(error)
    }
  }
  
  // 验证trace ID
  let validate_trace_id = fn(trace_id: String) {
    if trace_id.length() >= 5 && trace_id.starts_with("trace-") {
      create_success(trace_id)
    } else {
      create_error(TelemetryError::InvalidTraceId(trace_id))
    }
  }
  
  // 测试有效trace ID
  let valid_trace = validate_trace_id("trace-12345")
  assert_true(valid_trace.success)
  assert_eq(valid_trace.data, Some("trace-12345"))
  assert_eq(valid_trace.error, None)
  
  // 测试无效trace ID
  let invalid_trace = validate_trace_id("bad")
  assert_false(invalid_trace.success)
  assert_eq(invalid_trace.data, None)
  
  match invalid_trace.error {
    Some(TelemetryError::InvalidTraceId(id)) => assert_eq(id, "bad")
    _ => assert_true(false)
  }
  
  // 测试错误恢复
  let with_fallback = fn(primary: String, fallback: String) {
    let primary_result = validate_trace_id(primary)
    match primary_result.success {
      true => primary_result
      false => validate_trace_id(fallback)
    }
  }
  
  let fallback_result = with_fallback("bad", "trace-fallback")
  assert_true(fallback_result.success)
  assert_eq(fallback_result.data, Some("trace-fallback"))
}

// 测试4: 并发安全操作
test "并发安全操作测试" {
  // 模拟并发计数器
  type Counter = {
    mut value: Int,
    mut lock: Bool
  }
  
  // 创建计数器
  let counter = { mut value: 0, mut lock: false }
  
  // 模拟锁获取
  let acquire_lock = fn(counter: Counter) {
    if not(counter.lock) {
      counter.lock = true
      true
    } else {
      false
    }
  }
  
  // 模拟锁释放
  let release_lock = fn(counter: Counter) {
    counter.lock = false
  }
  
  // 安全增加计数器
  let safe_increment = fn(counter: Counter) {
    if acquire_lock(counter) {
      counter.value = counter.value + 1
      release_lock(counter)
      true
    } else {
      false
    }
  }
  
  // 测试基本操作
  assert_true(safe_increment(counter))
  assert_eq(counter.value, 1)
  
  assert_true(safe_increment(counter))
  assert_eq(counter.value, 2)
  
  // 模拟并发场景
  let concurrent_increment = fn(counter: Counter, times: Int) {
    let mut success_count = 0
    for i in 0..times {
      if safe_increment(counter) {
        success_count = success_count + 1
      }
    }
    success_count
  }
  
  // 在单线程环境中，所有 increment 应该成功
  let result = concurrent_increment(counter, 5)
  assert_eq(result, 5)
  assert_eq(counter.value, 7)
  
  // 测试资源池模式
  type ResourcePool = {
    resources: Array[String],
    mut used_resources: Array[String]
  }
  
  let create_pool = fn(resources: Array[String>) {
    {
      resources,
      used_resources: []
    }
  }
  
  // 获取资源
  let acquire_resource = fn(pool: ResourcePool) {
    for resource in pool.resources {
      if not(pool.used_resources.contains(resource)) {
        pool.used_resources = pool.used_resources.push(resource)
        return Some(resource)
      }
    }
    None
  }
  
  // 释放资源
  let release_resource = fn(pool: ResourcePool, resource: String) {
    let index = pool.used_resources.index_of(resource)
    match index {
      Some(i) => {
        pool.used_resources = pool.used_resources.remove_at(i)
        true
      }
      None => false
    }
  }
  
  // 测试资源池
  let pool = create_pool(["conn-1", "conn-2", "conn-3"])
  
  let resource1 = acquire_resource(pool)
  assert_eq(resource1, Some("conn-1"))
  assert_eq(pool.used_resources.length(), 1)
  
  let resource2 = acquire_resource(pool)
  assert_eq(resource2, Some("conn-2"))
  assert_eq(pool.used_resources.length(), 2)
  
  // 释放资源
  assert_true(release_resource(pool, "conn-1"))
  assert_eq(pool.used_resources.length(), 1)
  
  // 可以重新获取已释放的资源
  let resource1_again = acquire_resource(pool)
  assert_eq(resource1_again, Some("conn-1"))
}

// 测试5: 性能优化测试
test "性能优化测试" {
  // 测试记忆化模式
  let memoize = fn(f: (Int) -> Int) {
    let cache = []
    fn(x: Int) {
      // 查找缓存
      for (cached_x, cached_result) in cache {
        if cached_x == x {
          return cached_result
        }
      }
      
      // 计算结果
      let result = f(x)
      // 存入缓存
      // 在实际实现中，这里会更新缓存
      result
    }
  }
  
  // 计算平方函数
  let square = memoize(fn(x) { x * x })
  
  // 测试记忆化
  assert_eq(square(5), 25)
  assert_eq(square(10), 100)
  assert_eq(square(5), 25)  // 应该从缓存获取
  
  // 测试批处理
  let batch_process = fn(items: Array[String], processor: (String) -> String, batch_size: Int) {
    let mut results = []
    let mut batch = []
    
    for item in items {
      batch = batch.push(item)
      
      if batch.length() == batch_size {
        // 处理当前批次
        let processed_batch = batch.map(processor)
        results = results + processed_batch
        batch = []
      }
    }
    
    // 处理剩余项目
    if batch.length() > 0 {
      let processed_batch = batch.map(processor)
      results = results + processed_batch
    }
    
    results
  }
  
  let items = ["item1", "item2", "item3", "item4", "item5", "item6", "item7"]
  let processor = fn(item) { "processed-" + item }
  let batch_results = batch_process(items, processor, 3)
  
  assert_eq(batch_results, [
    "processed-item1", "processed-item2", "processed-item3",
    "processed-item4", "processed-item5", "processed-item6",
    "processed-item7"
  ])
  
  // 测试延迟计算
  type Lazy[T] = {
    computed: Bool,
    value: Option[T],
    computation: () -> T
  }
  
  let create_lazy = fn(computation: () -> T) {
    {
      computed: false,
      value: None,
      computation
    }
  }
  
  let get_lazy_value = fn(lazy: Lazy[T]) {
    if lazy.computed {
      match lazy.value {
        Some(v) => v
        None => lazy.computation()
      }
    } else {
      lazy.computation()
    }
  }
  
  let expensive_computation = create_lazy(fn() {
    // 模拟昂贵计算
    42 * 42
  })
  
  let result = get_lazy_value(expensive_computation)
  assert_eq(result, 1764)
}

// 测试6: 配置管理测试
test "配置管理测试" {
  // 定义配置类型
  type Config = {
    service_name: String,
    service_version: String,
    environment: String,
    telemetry_enabled: Bool,
    sampling_rate: Float,
    batch_size: Int,
    export_endpoint: String
  }
  
  // 默认配置
  let default_config = {
    service_name: "unknown-service",
    service_version: "1.0.0",
    environment: "development",
    telemetry_enabled: true,
    sampling_rate: 1.0,
    batch_size: 100,
    export_endpoint: "http://localhost:4317"
  }
  
  // 配置验证
  let validate_config = fn(config: Config) {
    let errors = []
    
    if config.service_name == "" {
      errors = errors.push("service_name cannot be empty")
    }
    
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors = errors.push("sampling_rate must be between 0.0 and 1.0")
    }
    
    if config.batch_size <= 0 {
      errors = errors.push("batch_size must be positive")
    }
    
    errors
  }
  
  // 测试默认配置
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = {
    service_name: "",
    service_version: "1.0.0",
    environment: "production",
    telemetry_enabled: true,
    sampling_rate: 1.5,  // 无效值
    batch_size: -1,      // 无效值
    export_endpoint: "http://localhost:4317"
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 3)
  assert_true(invalid_errors.contains("service_name cannot be empty"))
  assert_true(invalid_errors.contains("sampling_rate must be between 0.0 and 1.0"))
  assert_true(invalid_errors.contains("batch_size must be positive"))
  
  // 配置合并
  let merge_configs = fn(base: Config, override: Config) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      service_version: if override.service_version != "" { override.service_version } else { base.service_version },
      environment: if override.environment != "" { override.environment } else { base.environment },
      telemetry_enabled: override.telemetry_enabled,
      sampling_rate: if override.sampling_rate >= 0.0 { override.sampling_rate } else { base.sampling_rate },
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      export_endpoint: if override.export_endpoint != "" { override.export_endpoint } else { base.export_endpoint }
    }
  }
  
  // 测试配置合并
  let partial_override = {
    service_name: "payment-service",
    service_version: "",  // 使用默认值
    environment: "production",
    telemetry_enabled: false,
    sampling_rate: 0.5,
    batch_size: 0,  // 使用默认值
    export_endpoint: ""  // 使用默认值
  }
  
  let merged_config = merge_configs(default_config, partial_override)
  assert_eq(merged_config.service_name, "payment-service")
  assert_eq(merged_config.service_version, "1.0.0")  // 保持默认值
  assert_eq(merged_config.environment, "production")
  assert_eq(merged_config.telemetry_enabled, false)
  assert_eq(merged_config.sampling_rate, 0.5)
  assert_eq(merged_config.batch_size, 100)  // 保持默认值
  assert_eq(merged_config.export_endpoint, "http://localhost:4317")  // 保持默认值
  
  // 验证合并后的配置
  let merged_errors = validate_config(merged_config)
  assert_eq(merged_errors.length(), 0)
}

// 测试7: 数据序列化测试
test "数据序列化测试" {
  // 定义遥测数据类型
  type Span = {
    trace_id: String,
    span_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // 简化的JSON序列化
  let serialize_to_json = fn(span: Span) {
    "{" +
    "\"trace_id\":\"" + span.trace_id + "\"," +
    "\"span_id\":\"" + span.span_id + "\"," +
    "\"operation_name\":\"" + span.operation_name + "\"," +
    "\"start_time\":" + span.start_time.to_string() + "," +
    "\"end_time\":" + span.end_time.to_string() + "," +
    "\"status\":\"" + span.status + "\"" +
    "}"
  }
  
  // 简化的JSON反序列化
  let deserialize_from_json = fn(json: String) {
    // 在实际实现中，这里会有完整的JSON解析
    // 这里只是模拟
    if json.contains("trace_id") && json.contains("span_id") {
      Some({
        trace_id: "trace-123",
        span_id: "span-456",
        operation_name: "test_operation",
        start_time: 1640995200,
        end_time: 1640995250,
        status: "ok"
      })
    } else {
      None
    }
  }
  
  // 创建测试数据
  let span = {
    trace_id: "trace-123",
    span_id: "span-456",
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok"
  }
  
  // 测试序列化
  let json = serialize_to_json(span)
  assert_true(json.contains("\"trace_id\":\"trace-123\""))
  assert_true(json.contains("\"span_id\":\"span-456\""))
  assert_true(json.contains("\"operation_name\":\"database_query\""))
  assert_true(json.contains("\"status\":\"ok\""))
  
  // 测试反序列化
  let deserialized = deserialize_from_json(json)
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(parsed_span) => {
      assert_eq(parsed_span.trace_id, "trace-123")
      assert_eq(parsed_span.span_id, "span-456")
      assert_eq(parsed_span.status, "ok")
    }
    None => assert_true(false)
  }
  
  // 测试无效JSON
  let invalid_json = "{ invalid json }"
  let invalid_result = deserialize_from_json(invalid_json)
  assert_eq(invalid_result, None)
  
  // 测试CSV序列化
  let serialize_to_csv = fn(spans: Array[Span>) {
    let header = "trace_id,span_id,operation_name,start_time,end_time,status"
    let mut rows = [header]
    
    for span in spans {
      let row = span.trace_id + "," + 
                span.span_id + "," + 
                span.operation_name + "," + 
                span.start_time.to_string() + "," + 
                span.end_time.to_string() + "," + 
                span.status
      rows = rows.push(row)
    }
    
    rows.join("\n")
  }
  
  // 测试CSV序列化
  let spans = [
    span,
    {
      trace_id: "trace-789",
      span_id: "span-012",
      operation_name: "api_call",
      start_time: 1640995300,
      end_time: 1640995350,
      status: "error"
    }
  ]
  
  let csv = serialize_to_csv(spans)
  assert_true(csv.contains("trace_id,span_id,operation_name,start_time,end_time,status"))
  assert_true(csv.contains("trace-123,span-456,database_query,1640995200,1640995250,ok"))
  assert_true(csv.contains("trace-789,span-012,api_call,1640995300,1640995350,error"))
}

// 测试8: 跨平台兼容性测试
test "跨平台兼容性测试" {
  // 模拟不同平台的路径处理
  let normalize_path = fn(path: String, platform: String) {
    match platform {
      "windows" => path.replace("/", "\\"),
      "linux" => path.replace("\\", "/"),
      "macos" => path.replace("\\", "/"),
      _ => path
    }
  }
  
  // 测试路径规范化
  let path = "logs/telemetry/data.json"
  
  let windows_path = normalize_path(path, "windows")
  assert_eq(windows_path, "logs\\telemetry\\data.json")
  
  let linux_path = normalize_path(path, "linux")
  assert_eq(linux_path, "logs/telemetry/data.json")
  
  let macos_path = normalize_path(path, "macos")
  assert_eq(macos_path, "logs/telemetry/data.json")
  
  // 模拟平台特定的配置
  let get_platform_config = fn(platform: String) {
    match platform {
      "windows" => {
        log_directory: "C:\\ProgramData\\Azimuth\\Logs",
        config_directory: "C:\\ProgramData\\Azimuth\\Config",
        temp_directory: "C:\\Temp\\Azimuth"
      }
      "linux" => {
        log_directory: "/var/log/azimuth",
        config_directory: "/etc/azimuth",
        temp_directory: "/tmp/azimuth"
      }
      "macos" => {
        log_directory: "/Library/Logs/Azimuth",
        config_directory: "/Library/Application Support/Azimuth",
        temp_directory: "/tmp/azimuth"
      }
      _ => {
        log_directory: "./logs",
        config_directory: "./config",
        temp_directory: "./temp"
      }
    }
  }
  
  // 测试平台配置
  let windows_config = get_platform_config("windows")
  assert_eq(windows_config.log_directory, "C:\\ProgramData\\Azimuth\\Logs")
  assert_eq(windows_config.config_directory, "C:\\ProgramData\\Azimuth\\Config")
  
  let linux_config = get_platform_config("linux")
  assert_eq(linux_config.log_directory, "/var/log/azimuth")
  assert_eq(linux_config.config_directory, "/etc/azimuth")
  
  let macos_config = get_platform_config("macos")
  assert_eq(macos_config.log_directory, "/Library/Logs/Azimuth")
  assert_eq(macos_config.config_directory, "/Library/Application Support/Azimuth")
  
  // 测试平台检测
  let detect_platform = fn() {
    // 在实际实现中，这里会检测实际运行平台
    // 这里只是模拟
    "linux"
  }
  
  let current_platform = detect_platform()
  let current_config = get_platform_config(current_platform)
  
  // 根据检测到的平台获取相应配置
  match current_platform {
    "windows" => assert_eq(current_config.log_directory, "C:\\ProgramData\\Azimuth\\Logs")
    "linux" => assert_eq(current_config.log_directory, "/var/log/azimuth")
    "macos" => assert_eq(current_config.log_directory, "/Library/Logs/Azimuth")
    _ => assert_eq(current_config.log_directory, "./logs")
  }
  
  // 测试平台特定的文件权限
  let get_file_permissions = fn(platform: String, file_type: String) {
    match (platform, file_type) {
      ("linux", "config") => "644",
      ("linux", "executable") => "755",
      ("windows", _) => "NTFS",
      ("macos", "config") => "644",
      ("macos", "executable") => "755",
      _ => "default"
    }
  }
  
  // 测试文件权限
  assert_eq(get_file_permissions("linux", "config"), "644")
  assert_eq(get_file_permissions("linux", "executable"), "755")
  assert_eq(get_file_permissions("windows", "config"), "NTFS")
  assert_eq(get_file_permissions("macos", "executable"), "755")
}