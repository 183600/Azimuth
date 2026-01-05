// Azimuth 系统健康监控测试用例
// 专注于系统监控、健康检查和故障恢复机制

// 测试1: 递归函数和算法实现
test "递归函数和算法实现测试" {
  // 斐波那契数列递归实现
  let fibonacci = fn(n: Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  // 测试斐波那契数列
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(2), 1)
  assert_eq(fibonacci(3), 2)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(10), 55)
  
  // 阶乘递归实现
  let factorial = fn(n: Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  // 测试阶乘
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(3), 6)
  assert_eq(factorial(5), 120)
  
  // 最大公约数算法
  let gcd = fn(a: Int, b: Int) -> Int {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  // 测试最大公约数
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 5), 1)
  assert_eq(gcd(100, 25), 25)
}

// 测试2: 状态机实现和测试
test "状态机实现和测试" {
  // 定义服务状态
  enum ServiceState {
    Starting
    Running
    Stopping
    Stopped
    Error
  }
  
  // 定义状态转换
  let transition = fn(current: ServiceState, event: String) -> ServiceState {
    match (current, event) {
      (ServiceState::Stopped, "start") => ServiceState::Starting
      (ServiceState::Starting, "ready") => ServiceState::Running
      (ServiceState::Running, "stop") => ServiceState::Stopping
      (ServiceState::Stopping, "stopped") => ServiceState::Stopped
      (_, "error") => ServiceState::Error
      (ServiceState::Error, "recover") => ServiceState::Stopped
      _ => current // 无效转换，保持当前状态
    }
  }
  
  // 测试状态转换序列
  let mut state = ServiceState::Stopped
  
  // 正常启动流程
  state = transition(state, "start")
  assert_eq(state, ServiceState::Starting)
  
  state = transition(state, "ready")
  assert_eq(state, ServiceState::Running)
  
  // 正常停止流程
  state = transition(state, "stop")
  assert_eq(state, ServiceState::Stopping)
  
  state = transition(state, "stopped")
  assert_eq(state, ServiceState::Stopped)
  
  // 错误恢复流程
  state = transition(state, "start")
  state = transition(state, "error")
  assert_eq(state, ServiceState::Error)
  
  state = transition(state, "recover")
  assert_eq(state, ServiceState::Stopped)
  
  // 测试无效转换
  state = transition(state, "ready") // 从Stopped状态不能直接ready
  assert_eq(state, ServiceState::Stopped)
}

// 测试3: 缓存机制实现
test "缓存机制实现测试" {
  // 简单LRU缓存实现
  type CacheEntry[T] = {
    key: String,
    value: T,
    access_time: Int,
    access_count: Int
  }
  
  type Cache[T] = {
    entries: Array[CacheEntry[T]],
    max_size: Int,
    current_time: Int
  }
  
  let create_cache = fn(max_size: Int) -> Cache[T] {
    {
      entries: [],
      max_size,
      current_time: 0
    }
  }
  
  let get = fn(cache: Cache[T], key: String) -> Option[T] {
    let mut found = None
    let mut updated_entries = []
    
    for entry in cache.entries {
      if entry.key == key {
        found = Some(entry.value)
        updated_entries = updated_entries.push({
          entry | access_time: cache.current_time,
          access_count: entry.access_count + 1
        })
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    found
  }
  
  let put = fn(cache: Cache[T], key: String, value: T) -> Cache[T] {
    let mut new_entries = []
    let mut found = false
    
    // 检查是否已存在
    for entry in cache.entries {
      if entry.key == key {
        new_entries = new_entries.push({
          key,
          value,
          access_time: cache.current_time,
          access_count: entry.access_count + 1
        })
        found = true
      } else {
        new_entries = new_entries.push(entry)
      }
    }
    
    // 如果不存在且还有空间，添加新条目
    if not(found) and new_entries.length() < cache.max_size {
      new_entries = new_entries.push({
        key,
        value,
        access_time: cache.current_time,
        access_count: 1
      })
    }
    
    // 如果缓存已满，移除最少使用的条目
    if new_entries.length() > cache.max_size {
      let mut min_access = 999999
      let mut min_index = 0
      
      let mut i = 0
      while i < new_entries.length() {
        if new_entries[i].access_count < min_access {
          min_access = new_entries[i].access_count
          min_index = i
        }
        i = i + 1
      }
      
      new_entries = Array::remove_at(new_entries, min_index)
    }
    
    { cache | entries: new_entries, current_time: cache.current_time + 1 }
  }
  
  // 测试缓存操作
  let cache = create_cache(3)
  
  // 添加条目
  let cache1 = put(cache, "key1", "value1")
  let cache2 = put(cache1, "key2", "value2")
  let cache3 = put(cache2, "key3", "value3")
  
  // 测试获取
  assert_eq(get(cache3, "key1"), Some("value1"))
  assert_eq(get(cache3, "key2"), Some("value2"))
  assert_eq(get(cache3, "key3"), Some("value3"))
  assert_eq(get(cache3, "missing"), None)
  
  // 测试LRU淘汰
  let cache4 = put(cache3, "key4", "value4") // 应该淘汰最少使用的条目
  assert_eq(get(cache4, "key4"), Some("value4"))
  // 由于我们的简单实现，最后一个条目会被淘汰
}

// 测试4: 数据转换和序列化
test "数据转换和序列化测试" {
  // 定义遥测数据结构
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 简单的JSON序列化模拟
  let serialize_to_json = fn(point: TelemetryPoint) -> String {
    let tags_json = fn(tags: Array[(String, String)]) -> String {
      let mut result = "{"
      let mut first = true
      
      for (key, value) in tags {
        if not(first) {
          result = result + ","
        }
        result = result + "\"" + key + "\":\"" + value + "\""
        first = false
      }
      
      result + "}"
    }
    
    "{"
    + "\"timestamp\":" + point.timestamp.to_string() + ","
    + "\"metric_name\":\"" + point.metric_name + "\"," +
    + "\"value\":" + point.value.to_string() + ","
    + "\"tags\":" + tags_json(point.tags)
    + "}"
  }
  
  // 创建测试数据点
  let point = {
    timestamp: 1640995200,
    metric_name: "cpu.usage",
    value: 75.5,
    tags: [
      ("host", "server-01"),
      ("region", "us-west-2"),
      ("environment", "production")
    ]
  }
  
  // 序列化
  let json = serialize_to_json(point)
  
  // 验证JSON包含必要字段
  assert_true(json.contains("\"timestamp\":1640995200"))
  assert_true(json.contains("\"metric_name\":\"cpu.usage\""))
  assert_true(json.contains("\"value\":75.5"))
  assert_true(json.contains("\"host\":\"server-01\""))
  assert_true(json.contains("\"region\":\"us-west-2\""))
  assert_true(json.contains("\"environment\":\"production\""))
  
  // 简单的CSV序列化
  let serialize_to_csv = fn(points: Array[TelemetryPoint]) -> String {
    let header = "timestamp,metric_name,value,host,region,environment"
    let mut lines = [header]
    
    for point in points {
      let host = fn() {
        for (key, value) in point.tags {
          if key == "host" {
            value
          }
        }
        "unknown"
      }()
      
      let region = fn() {
        for (key, value) in point.tags {
          if key == "region" {
            value
          }
        }
        "unknown"
      }()
      
      let environment = fn() {
        for (key, value) in point.tags {
          if key == "environment" {
            value
          }
        }
        "unknown"
      }()
      
      let line = point.timestamp.to_string() + ","
        + point.metric_name + ","
        + point.value.to_string() + ","
        + host + ","
        + region + ","
        + environment
      
      lines = lines.push(line)
    }
    
    let mut result = ""
    let mut first = true
    for line in lines {
      if not(first) {
        result = result + "\n"
      }
      result = result + line
      first = false
    }
    result
  }
  
  // 测试CSV序列化
  let points = [point]
  let csv = serialize_to_csv(points)
  
  assert_true(csv.contains("timestamp,metric_name,value,host,region,environment"))
  assert_true(csv.contains("1640995200,cpu.usage,75.5,server-01,us-west-2,production"))
}

// 测试5: 异步操作模拟
test "异步操作模拟测试" {
  // 模拟异步任务状态
  enum TaskStatus {
    Pending
    Running
    Completed
    Failed
  }
  
  type AsyncTask[T] = {
    id: String,
    status: TaskStatus,
    result: Option[T],
    error: Option[String],
    progress: Float
  }
  
  let create_task = fn(id: String) -> AsyncTask[String] {
    {
      id,
      status: TaskStatus::Pending,
      result: None,
      error: None,
      progress: 0.0
    }
  }
  
  let start_task = fn(task: AsyncTask[String]) -> AsyncTask[String] {
    { task | status: TaskStatus::Running, progress: 0.1 }
  }
  
  let update_progress = fn(task: AsyncTask[String], progress: Float) -> AsyncTask[String] {
    { task | progress: progress }
  }
  
  let complete_task = fn(task: AsyncTask[String], result: String) -> AsyncTask[String] {
    {
      task |
      status: TaskStatus::Completed,
      result: Some(result),
      progress: 1.0
    }
  }
  
  let fail_task = fn(task: AsyncTask[String], error: String) -> AsyncTask[String] {
    {
      task |
      status: TaskStatus::Failed,
      error: Some(error)
    }
  }
  
  // 测试异步任务生命周期
  let task = create_task("task-001")
  assert_eq(task.id, "task-001")
  assert_eq(task.status, TaskStatus::Pending)
  assert_eq(task.progress, 0.0)
  
  let running_task = start_task(task)
  assert_eq(running_task.status, TaskStatus::Running)
  assert_eq(running_task.progress, 0.1)
  
  let progressing_task = update_progress(running_task, 0.5)
  assert_eq(progressing_task.status, TaskStatus::Running)
  assert_eq(progressing_task.progress, 0.5)
  
  let completed_task = complete_task(progressing_task, "task result")
  assert_eq(completed_task.status, TaskStatus::Completed)
  assert_eq(completed_task.result, Some("task result"))
  assert_eq(completed_task.progress, 1.0)
  
  // 测试失败场景
  let task2 = create_task("task-002")
  let running_task2 = start_task(task2)
  let failed_task = fail_task(running_task2, "network error")
  
  assert_eq(failed_task.status, TaskStatus::Failed)
  assert_eq(failed_task.error, Some("network error"))
  assert_eq(failed_task.result, None)
}

// 测试6: 流式处理模拟
test "流式处理模拟测试" {
  // 定义流处理器
  type StreamProcessor[T, U] = {
    process: fn(T) -> U,
    filter: fn(T) -> Bool,
    transform: fn(T) -> T
  }
  
  let create_processor = fn[T, U](
    process_fn: fn(T) -> U,
    filter_fn: fn(T) -> Bool,
    transform_fn: fn(T) -> T
  ) -> StreamProcessor[T, U] {
    {
      process: process_fn,
      filter: filter_fn,
      transform: transform_fn
    }
  }
  
  let process_stream = fn[T, U](
    processor: StreamProcessor[T, U],
    input: Array[T]
  ) -> Array[U] {
    let mut results = []
    
    for item in input {
      if processor.filter(item) {
        let transformed = processor.transform(item)
        let processed = processor.process(transformed)
        results = results.push(processed)
      }
    }
    
    results
  }
  
  // 创建遥测数据流处理器
  let telemetry_processor = create_processor(
    // 处理函数：提取指标值
    fn(point: (String, Float)) -> Float {
      point.1
    },
    // 过滤函数：只保留CPU和内存指标
    fn(point: (String, Float)) -> Bool {
      point.0 == "cpu.usage" or point.0 == "memory.usage"
    },
    // 转换函数：将百分比转换为小数
    fn(point: (String, Float)) -> (String, Float) {
      (point.0, point.1 / 100.0)
    }
  )
  
  // 创建测试数据流
  let telemetry_data = [
    ("cpu.usage", 75.5),
    ("memory.usage", 60.2),
    ("disk.usage", 45.0),
    ("network.throughput", 120.5),
    ("cpu.usage", 80.0),
    ("memory.usage", 65.5)
  ]
  
  // 处理数据流
  let processed = process_stream(telemetry_processor, telemetry_data)
  
  // 验证结果
  assert_eq(processed.length(), 4) // 只有cpu和memory指标
  assert_eq(processed[0], 0.755)   // 75.5% -> 0.755
  assert_eq(processed[1], 0.602)   // 60.2% -> 0.602
  assert_eq(processed[2], 0.8)     // 80.0% -> 0.8
  assert_eq(processed[3], 0.655)   // 65.5% -> 0.655
}

// 测试7: 排序和搜索算法
test "排序和搜索算法测试" {
  // 冒泡排序实现
  let bubble_sort = fn(arr: Array[Int]) -> Array[Int] {
    let mut result = arr
    let n = result.length()
    let mut i = 0
    
    while i < n - 1 {
      let mut j = 0
      while j < n - i - 1 {
        if result[j] > result[j + 1] {
          // 交换元素
          let temp = result[j]
          result = Array::set(result, j, result[j + 1])
          result = Array::set(result, j + 1, temp)
        }
        j = j + 1
      }
      i = i + 1
    }
    
    result
  }
  
  // 二分搜索实现
  let binary_search = fn(arr: Array[Int], target: Int) -> Bool {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = (left + right) / 2
      let mid_value = arr[mid]
      
      if mid_value == target {
        return true
      } else if mid_value < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    false
  }
  
  // 测试数据
  let unsorted = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  let sorted = bubble_sort(unsorted)
  
  // 验证排序结果
  assert_eq(sorted, [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90])
  
  // 测试搜索
  assert_true(binary_search(sorted, 25))
  assert_true(binary_search(sorted, 90))
  assert_true(binary_search(sorted, 11))
  assert_false(binary_search(sorted, 100))
  assert_false(binary_search(sorted, 0))
  
  // 测试空数组和单元素数组
  assert_eq(bubble_sort([]), [])
  assert_eq(bubble_sort([42]), [42])
  assert_true(binary_search([42], 42))
  assert_false(binary_search([42], 24))
}

// 测试8: 配置管理
test "配置管理测试" {
  // 配置项类型
  enum ConfigValue {
    StringVal(String)
    IntVal(Int)
    FloatVal(Float)
    BoolVal(Bool)
  }
  
  type Config = {
    values: Array[(String, ConfigValue)],
    defaults: Array[(String, ConfigValue)]
  }
  
  let create_config = fn() -> Config {
    {
      values: [],
      defaults: [
        ("timeout", IntVal(5000)),
        ("retries", IntVal(3)),
        ("debug", BoolVal(false)),
        ("max_connections", IntVal(100)),
        ("cache_size", IntVal(1000))
      ]
    }
  }
  
  let set_config = fn(config: Config, key: String, value: ConfigValue) -> Config {
    let mut new_values = []
    let mut found = false
    
    // 更新现有值
    for (k, v) in config.values {
      if k == key {
        new_values = new_values.push((key, value))
        found = true
      } else {
        new_values = new_values.push((k, v))
      }
    }
    
    // 添加新值
    if not(found) {
      new_values = new_values.push((key, value))
    }
    
    { config | values: new_values }
  }
  
  let get_config = fn(config: Config, key: String) -> ConfigValue {
    // 首先查找用户设置的值
    for (k, v) in config.values {
      if k == key {
        return v
      }
    }
    
    // 然后查找默认值
    for (k, v) in config.defaults {
      if k == key {
        return v
      }
    }
    
    // 如果找不到，返回默认值
    StringVal("")
  }
  
  let get_config_string = fn(config: Config, key: String) -> String {
    match get_config(config, key) {
      ConfigValue::StringVal(s) => s
      _ => ""
    }
  }
  
  let get_config_int = fn(config: Config, key: String) -> Int {
    match get_config(config, key) {
      ConfigValue::IntVal(i) => i
      _ => 0
    }
  }
  
  let get_config_bool = fn(config: Config, key: String) -> Bool {
    match get_config(config, key) {
      ConfigValue::BoolVal(b) => b
      _ => false
    }
  }
  
  // 测试配置管理
  let config = create_config()
  
  // 测试默认值
  assert_eq(get_config_int(config, "timeout"), 5000)
  assert_eq(get_config_int(config, "retries"), 3)
  assert_false(get_config_bool(config, "debug"))
  assert_eq(get_config_int(config, "max_connections"), 100)
  
  // 设置新值
  let config1 = set_config(config, "timeout", IntVal(10000))
  let config2 = set_config(config1, "debug", BoolVal(true))
  let config3 = set_config(config2, "service.name", StringVal("azimuth"))
  
  // 测试获取值
  assert_eq(get_config_int(config3, "timeout"), 10000)
  assert_true(get_config_bool(config3, "debug"))
  assert_eq(get_config_string(config3, "service.name"), "azimuth")
  
  // 未设置的值应该返回默认值
  assert_eq(get_config_int(config3, "retries"), 3)
  assert_eq(get_config_string(config3, "unknown"), "")
}

// 测试9: 资源池管理
test "资源池管理测试" {
  // 资源池状态
  enum ResourceState {
    Available
    InUse
    Maintenance
  }
  
  type Resource = {
    id: String,
    state: ResourceState,
    created_at: Int,
    last_used: Int
  }
  
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    current_time: Int
  }
  
  let create_pool = fn(max_size: Int) -> ResourcePool {
    {
      resources: [],
      max_size,
      current_time: 0
    }
  }
  
  let add_resource = fn(pool: ResourcePool, id: String) -> ResourcePool {
    if pool.resources.length() >= pool.max_size {
      pool // 池已满，不能添加
    } else {
      let resource = {
        id,
        state: ResourceState::Available,
        created_at: pool.current_time,
        last_used: pool.current_time
      }
      { pool | resources: pool.resources.push(resource) }
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool) -> (ResourcePool, Option[String]) {
    let mut updated_resources = []
    let mut acquired_id = None
    
    for resource in pool.resources {
      if resource.state == ResourceState::Available and acquired_id == None {
        updated_resources = updated_resources.push({
          resource |
          state: ResourceState::InUse,
          last_used: pool.current_time
        })
        acquired_id = Some(resource.id)
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    ({ pool | resources: updated_resources }, acquired_id)
  }
  
  let release_resource = fn(pool: ResourcePool, id: String) -> ResourcePool {
    let mut updated_resources = []
    
    for resource in pool.resources {
      if resource.id == id and resource.state == ResourceState::InUse {
        updated_resources = updated_resources.push({
          resource |
          state: ResourceState::Available,
          last_used: pool.current_time
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    { pool | resources: updated_resources }
  }
  
  let count_available = fn(pool: ResourcePool) -> Int {
    let mut count = 0
    for resource in pool.resources {
      if resource.state == ResourceState::Available {
        count = count + 1
      }
    }
    count
  }
  
  let count_in_use = fn(pool: ResourcePool) -> Int {
    let mut count = 0
    for resource in pool.resources {
      if resource.state == ResourceState::InUse {
        count = count + 1
      }
    }
    count
  }
  
  // 测试资源池管理
  let pool = create_pool(3)
  
  // 添加资源
  let pool1 = add_resource(pool, "resource-1")
  let pool2 = add_resource(pool1, "resource-2")
  let pool3 = add_resource(pool2, "resource-3")
  
  // 验证资源数量
  assert_eq(pool3.resources.length(), 3)
  assert_eq(count_available(pool3), 3)
  assert_eq(count_in_use(pool3), 0)
  
  // 获取资源
  let (pool4, resource1) = acquire_resource(pool3)
  assert_eq(resource1, Some("resource-1"))
  assert_eq(count_available(pool4), 2)
  assert_eq(count_in_use(pool4), 1)
  
  let (pool5, resource2) = acquire_resource(pool4)
  assert_eq(resource2, Some("resource-2"))
  assert_eq(count_available(pool5), 1)
  assert_eq(count_in_use(pool5), 2)
  
  // 释放资源
  let pool6 = release_resource(pool5, "resource-1")
  assert_eq(count_available(pool6), 2)
  assert_eq(count_in_use(pool6), 1)
  
  // 测试池满的情况
  let pool7 = add_resource(pool6, "resource-4") // 应该失败，池已满
  assert_eq(pool7.resources.length(), 3) // 数量不变
}

// 测试10: 错误恢复机制
test "错误恢复机制测试" {
  // 错误类型
  enum ErrorType {
    NetworkError
    DatabaseError
    TimeoutError
    ValidationError
  }
  
  // 重试策略
  enum RetryStrategy {
    NoRetry
    FixedDelay(Int) // 延迟毫秒数
    ExponentialBackoff(Int, Int) // 初始延迟，最大延迟
  }
  
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[ErrorType],
    attempts: Int
  }
  
  let execute_with_retry = fn[T](
    operation: fn() -> (Bool, Option[T], Option[ErrorType]),
    strategy: RetryStrategy,
    max_attempts: Int
  ) -> OperationResult[T] {
    let mut attempts = 0
    let mut result_data = None
    let mut result_error = None
    let mut success = false
    let mut delay = 0
    
    while attempts < max_attempts and not(success) {
      attempts = attempts + 1
      
      let (op_success, data, error) = operation()
      
      if op_success {
        success = true
        result_data = data
      } else {
        result_error = error
        
        // 根据策略计算延迟
        match strategy {
          RetryStrategy::NoRetry => {
            // 不重试
            break
          }
          RetryStrategy::FixedDelay(d) => {
            delay = d
          }
          RetryStrategy::ExponentialBackoff(initial, max) => {
            delay = initial * (2 ^ (attempts - 1))
            if delay > max {
              delay = max
            }
          }
        }
        
        // 在实际实现中，这里会等待delay毫秒
      }
    }
    
    {
      success,
      data: result_data,
      error: result_error,
      attempts
    }
  }
  
  // 模拟操作函数
  let create_failing_operation = fn(fail_count: Int, error_type: ErrorType) {
    let mut current_attempt = 0
    fn() -> (Bool, Option[String], Option[ErrorType]) {
      current_attempt = current_attempt + 1
      if current_attempt <= fail_count {
        (false, None, Some(error_type))
      } else {
        (true, Some("operation succeeded"), None)
      }
    }
  }
  
  // 测试成功操作（不需要重试）
  let success_op = fn() -> (Bool, Option[String], Option[ErrorType]) {
    (true, Some("immediate success"), None)
  }
  
  let result1 = execute_with_retry(success_op, RetryStrategy::NoRetry, 3)
  assert_true(result1.success)
  assert_eq(result1.data, Some("immediate success"))
  assert_eq(result1.attempts, 1)
  
  // 测试固定延迟重试
  let failing_op = create_failing_operation(2, ErrorType::NetworkError)
  let result2 = execute_with_retry(failing_op, RetryStrategy::FixedDelay(100), 5)
  assert_true(result2.success)
  assert_eq(result2.data, Some("operation succeeded"))
  assert_eq(result2.attempts, 3)
  
  // 测试指数退避重试
  let failing_op2 = create_failing_operation(3, ErrorType::DatabaseError)
  let result3 = execute_with_retry(failing_op2, RetryStrategy::ExponentialBackoff(100, 1000), 5)
  assert_true(result3.success)
  assert_eq(result3.data, Some("operation succeeded"))
  assert_eq(result3.attempts, 4)
  
  // 测试超过最大重试次数
  let always_failing_op = fn() -> (Bool, Option[String], Option[ErrorType]) {
    (false, None, Some(ErrorType::TimeoutError))
  }
  
  let result4 = execute_with_retry(always_failing_op, RetryStrategy::FixedDelay(200), 3)
  assert_false(result4.success)
  assert_eq(result4.data, None)
  assert_eq(result4.error, Some(ErrorType::TimeoutError))
  assert_eq(result4.attempts, 3)
  
  // 测试不重试策略
  let result5 = execute_with_retry(always_failing_op, RetryStrategy::NoRetry, 5)
  assert_false(result5.success)
  assert_eq(result5.attempts, 1)
}