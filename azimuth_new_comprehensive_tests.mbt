// Azimuth 新增综合测试用例
// 包含多种功能领域的测试，确保系统各个组件的可靠性

// 测试1: 错误处理和恢复机制
test "错误处理和恢复机制测试" {
  // 1. 测试基本错误类型
  let network_error = NetworkError({ message: "连接超时", code: 1001 })
  let validation_error = ValidationError({ field: "email", message: "邮箱格式不正确" })
  let system_error = SystemError({ component: "数据库", message: "连接失败" })
  
  // 2. 验证错误类型
  match network_error {
    NetworkError(message, code) => {
      assert_eq(message, "连接超时")
      assert_eq(code, 1001)
    }
    _ => assert_true(false)
  }
  
  match validation_error {
    ValidationError(field, message) => {
      assert_eq(field, "email")
      assert_eq(message, "邮箱格式不正确")
    }
    _ => assert_true(false)
  }
  
  match system_error {
    SystemError(component, message) => {
      assert_eq(component, "数据库")
      assert_eq(message, "连接失败")
    }
    _ => assert_true(false)
  }
  
  // 3. 测试错误恢复机制
  let recoverable_error = RecoverableError({ 
    error: network_error, 
    retry_count: 3, 
    max_retries: 5 
  })
  
  assert_eq(recoverable_error.retry_count, 3)
  assert_eq(recoverable_error.max_retries, 5)
  
  // 4. 测试错误链
  let chained_error = ChainedError({
    primary: validation_error,
    secondary: network_error
  })
  
  match chained_error.primary {
    ValidationError(field, message) => {
      assert_eq(field, "email")
      assert_eq(message, "邮箱格式不正确")
    }
    _ => assert_true(false)
  }
  
  match chained_error.secondary {
    NetworkError(message, code) => {
      assert_eq(message, "连接超时")
      assert_eq(code, 1001)
    }
    _ => assert_true(false)
  }
}

// 测试2: 数据序列化和反序列化
test "数据序列化和反序列化测试" {
  // 1. 测试基本数据类型序列化
  let int_data = 42
  let float_data = 3.14159
  let string_data = "azimuth-telemetry"
  let bool_data = true
  
  // 2. 序列化为JSON
  let int_json = serialize_to_json(int_data)
  let float_json = serialize_to_json(float_data)
  let string_json = serialize_to_json(string_data)
  let bool_json = serialize_to_json(bool_data)
  
  assert_eq(int_json, "42")
  assert_eq(float_json, "3.14159")
  assert_eq(string_json, "\"azimuth-telemetry\"")
  assert_eq(bool_json, "true")
  
  // 3. 反序列化
  let deserialized_int = deserialize_from_json(int_json, "int")
  let deserialized_float = deserialize_from_json(float_json, "float")
  let deserialized_string = deserialize_from_json(string_json, "string")
  let deserialized_bool = deserialize_from_json(bool_json, "bool")
  
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match deserialized_float {
    FloatValue(v) => assert_eq(v, 3.14159)
    _ => assert_true(false)
  }
  
  match deserialized_string {
    StringValue(v) => assert_eq(v, "azimuth-telemetry")
    _ => assert_true(false)
  }
  
  match deserialized_bool {
    BoolValue(v) => assert_eq(v, true)
    _ => assert_true(false)
  }
  
  // 4. 测试复杂数据结构序列化
  let complex_data = {
    name: "azimuth-service",
    version: "1.0.0",
    metrics: [
      { name: "request.count", value: 1000 },
      { name: "response.time", value: 250.5 }
    ],
    active: true
  }
  
  let complex_json = serialize_complex_to_json(complex_data)
  assert_true(complex_json.contains("azimuth-service"))
  assert_true(complex_json.contains("request.count"))
  assert_true(complex_json.contains("1000"))
  assert_true(complex_json.contains("250.5"))
  assert_true(complex_json.contains("true"))
}

// 测试3: 并发安全性
test "并发安全性测试" {
  // 1. 创建共享资源
  let shared_counter = AtomicCounter({ value: 0 })
  let shared_map = ConcurrentMap({ entries = [] })
  
  // 2. 模拟并发操作
  let task1 = fn() {
    for i = 0; i < 100; i = i + 1 {
      shared_counter.increment()
      shared_map.insert("key-" + i.to_string(), i)
    }
  }
  
  let task2 = fn() {
    for i = 0; i < 100; i = i + 1 {
      shared_counter.increment()
      shared_map.insert("key-" + (i + 100).to_string(), i + 100)
    }
  }
  
  // 3. 执行并发任务
  execute_concurrently([task1, task2])
  
  // 4. 验证结果
  assert_eq(shared_counter.get(), 200)
  assert_eq(shared_map.size(), 200)
  
  // 5. 验证特定键值
  let value1 = shared_map.get("key-50")
  match value1 {
    Some(v) => assert_eq(v, 50)
    None => assert_true(false)
  }
  
  let value2 = shared_map.get("key-150")
  match value2 {
    Some(v) => assert_eq(v, 150)
    None => assert_true(false)
  }
  
  // 6. 测试读写锁
  let rw_lock = ReadWriteLock({ 
    readers: 0, 
    writer: false, 
    data: "shared-data" 
  })
  
  // 读操作
  let read_result = rw_lock.read(fn(data) { data.length() })
  assert_eq(read_result, 11)
  
  // 写操作
  let write_success = rw_lock.write(fn(data) { data + "-updated" })
  assert_true(write_success)
  
  let updated_data = rw_lock.read(fn(data) { data })
  assert_eq(updated_data, "shared-data-updated")
}

// 测试4: 性能基准测试
test "性能基准测试" {
  // 1. 测试数组操作性能
  let large_array = create_large_array(10000)
  let start_time = current_time_millis()
  
  // 执行数组操作
  let filtered = large_array.filter(fn(x) { x % 2 == 0 })
  let mapped = filtered.map(fn(x) { x * 2 })
  let reduced = mapped.reduce(fn(acc, x) { acc + x }, 0)
  
  let end_time = current_time_millis()
  let elapsed = end_time - start_time
  
  // 验证结果
  assert_eq(filtered.length(), 5000)
  assert_eq(mapped.length(), 5000)
  assert_eq(reduced, 50050000)
  
  // 性能断言（操作应在合理时间内完成）
  assert_true(elapsed < 1000)  // 应在1秒内完成
  
  // 2. 测试字符串操作性能
  let string_builder = StringBuilder({ buffer = "" })
  let string_start = current_time_millis()
  
  for i = 0; i < 1000; i = i + 1 {
    string_builder.append("item-" + i.to_string() + ",")
  }
  
  let result_string = string_builder.to_string()
  let string_end = current_time_millis()
  let string_elapsed = string_end - string_start
  
  assert_true(result_string.contains("item-0"))
  assert_true(result_string.contains("item-999"))
  assert_true(string_elapsed < 500)  // 应在500ms内完成
  
  // 3. 测试哈希表性能
  let hash_map = HashMap({ buckets = create_array(1000) })
  let hash_start = current_time_millis()
  
  for i = 0; i < 1000; i = i + 1 {
    hash_map.insert("key-" + i.to_string(), "value-" + i.to_string())
  }
  
  for i = 0; i < 1000; i = i + 1 {
    let value = hash_map.get("key-" + i.to_string())
    match value {
      Some(v) => assert_eq(v, "value-" + i.to_string())
      None => assert_true(false)
    }
  }
  
  let hash_end = current_time_millis()
  let hash_elapsed = hash_end - hash_start
  
  assert_true(hash_elapsed < 200)  // 应在200ms内完成
}

// 测试5: 资源管理
test "资源管理测试" {
  // 1. 测试资源池
  let resource_pool = ResourcePool({
    resources: [
      Resource({ id: "res-1", in_use: false, data: "resource-data-1" }),
      Resource({ id: "res-2", in_use: false, data: "resource-data-2" }),
      Resource({ id: "res-3", in_use: false, data: "resource-data-3" })
    ],
    max_size: 5
  })
  
  // 2. 获取资源
  let resource1 = resource_pool.acquire()
  match resource1 {
    Some(res) => {
      assert_eq(res.id, "res-1")
      assert_true(res.in_use)
      assert_eq(res.data, "resource-data-1")
    }
    None => assert_true(false)
  }
  
  let resource2 = resource_pool.acquire()
  match resource2 {
    Some(res) => {
      assert_eq(res.id, "res-2")
      assert_true(res.in_use)
      assert_eq(res.data, "resource-data-2")
    }
    None => assert_true(false)
  }
  
  // 3. 释放资源
  resource_pool.release(resource1.unwrap())
  
  let resource3 = resource_pool.acquire()
  match resource3 {
    Some(res) => {
      assert_eq(res.id, "res-1")  // 应该重新获取到刚释放的资源
      assert_true(res.in_use)
    }
    None => assert_true(false)
  }
  
  // 4. 测试资源限制
  let resource4 = resource_pool.acquire()
  let resource5 = resource_pool.acquire()
  let resource6 = resource_pool.acquire()  // 应该失败，因为资源已用完
  
  match resource6 {
    Some(_) => assert_true(false)  // 不应该成功获取
    None => assert_true(true)      // 应该返回None
  }
  
  // 5. 测试资源清理
  let cleanup_count = resource_pool.cleanup_expired()
  assert_eq(cleanup_count, 0)  // 没有过期资源
  
  // 6. 测试内存资源管理
  let memory_manager = MemoryManager({
    allocated_blocks = [],
    total_allocated: 0,
    max_memory: 1000000  // 1MB
  })
  
  let block1 = memory_manager.allocate(100000)  // 100KB
  let block2 = memory_manager.allocate(200000)  // 200KB
  let block3 = memory_manager.allocate(300000)  // 300KB
  
  assert_eq(memory_manager.total_allocated, 600000)
  
  let block4 = memory_manager.allocate(500000)  // 500KB，应该失败
  match block4 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  memory_manager.deallocate(block2.unwrap())
  assert_eq(memory_manager.total_allocated, 400000)
  
  let block5 = memory_manager.allocate(500000)  // 现在应该成功
  match block5 {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
}

// 测试6: 配置管理
test "配置管理测试" {
  // 1. 创建配置管理器
  let config_manager = ConfigurationManager({
    configs = [
      ("service.name", "azimuth-service"),
      ("service.port", "8080"),
      ("service.host", "localhost"),
      ("database.url", "postgresql://localhost:5432/azimuth"),
      ("database.pool_size", "10"),
      ("logging.level", "INFO"),
      ("logging.format", "json"),
      ("telemetry.enabled", "true"),
      ("telemetry.sampling_rate", "0.1")
    ]
  })
  
  // 2. 获取配置值
  let service_name = config_manager.get_string("service.name")
  match service_name {
    Some(name) => assert_eq(name, "azimuth-service")
    None => assert_true(false)
  }
  
  let service_port = config_manager.get_int("service.port")
  match service_port {
    Some(port) => assert_eq(port, 8080)
    None => assert_true(false)
  }
  
  let telemetry_enabled = config_manager.get_bool("telemetry.enabled")
  match telemetry_enabled {
    Some(enabled) => assert_true(enabled)
    None => assert_true(false)
  }
  
  let sampling_rate = config_manager.get_float("telemetry.sampling_rate")
  match sampling_rate {
    Some(rate) => assert_eq(rate, 0.1)
    None => assert_true(false)
  }
  
  // 3. 测试默认值
  let non_existent = config_manager.get_string_or("non.existent.key", "default-value")
  assert_eq(non_existent, "default-value")
  
  let non_existent_int = config_manager.get_int_or("non.existent.int", 42)
  assert_eq(non_existent_int, 42)
  
  let non_existent_bool = config_manager.get_bool_or("non.existent.bool", false)
  assert_false(non_existent_bool)
  
  // 4. 动态更新配置
  let update_success = config_manager.set("service.port", "9090")
  assert_true(update_success)
  
  let updated_port = config_manager.get_int("service.port")
  match updated_port {
    Some(port) => assert_eq(port, 9090)
    None => assert_true(false)
  }
  
  // 5. 配置验证
  let validation_result = config_manager.validate()
  assert_true(validation_result.is_valid)
  
  // 6. 配置导出
  let exported_config = config_manager.export()
  assert_true(exported_config.contains("service.name"))
  assert_true(exported_config.contains("azimuth-service"))
  assert_true(exported_config.contains("telemetry.enabled"))
  assert_true(exported_config.contains("true"))
}

// 测试7: 时间序列数据处理
test "时间序列数据处理测试" {
  // 1. 创建时间序列数据点
  let data_points = [
    TimeSeriesPoint({ timestamp: 1609459200000, value: 10.5, tags = [("source", "sensor-A")] }),
    TimeSeriesPoint({ timestamp: 1609459260000, value: 12.3, tags = [("source", "sensor-A")] }),
    TimeSeriesPoint({ timestamp: 1609459320000, value: 11.8, tags = [("source", "sensor-A")] }),
    TimeSeriesPoint({ timestamp: 1609459380000, value: 13.2, tags = [("source", "sensor-A")] }),
    TimeSeriesPoint({ timestamp: 1609459440000, value: 14.7, tags = [("source", "sensor-A")] })
  ]
  
  // 2. 创建时间序列
  let time_series = TimeSeries({
    name: "temperature",
    data_points: data_points,
    aggregation: Average
  })
  
  // 3. 测试时间范围查询
  let start_time = 1609459200000
  let end_time = 1609459380000
  let filtered_points = time_series.query_by_time_range(start_time, end_time)
  
  assert_eq(filtered_points.length(), 4)
  assert_eq(filtered_points[0].value, 10.5)
  assert_eq(filtered_points[3].value, 13.2)
  
  // 4. 测试聚合操作
  let avg_value = time_series.aggregate(Average)
  assert_eq(avg_value, 12.5)  // (10.5 + 12.3 + 11.8 + 13.2 + 14.7) / 5
  
  let max_value = time_series.aggregate(Max)
  assert_eq(max_value, 14.7)
  
  let min_value = time_series.aggregate(Min)
  assert_eq(min_value, 10.5)
  
  let sum_value = time_series.aggregate(Sum)
  assert_eq(sum_value, 62.5)
  
  // 5. 测试时间窗口聚合
  let windowed_avg = time_series.window_aggregate(120000, Average)  // 2分钟窗口
  assert_eq(windowed_avg.length(), 3)  // 5个点，2分钟窗口，应该有3个窗口
  
  // 6. 测试标签过滤
  let tag_filtered = time_series.filter_by_tag("source", "sensor-A")
  assert_eq(tag_filtered.length(), 5)
  
  let non_existent_tag = time_series.filter_by_tag("source", "sensor-B")
  assert_eq(non_existent_tag.length(), 0)
  
  // 7. 测试时间序列压缩
  let compressed_series = time_series.compress(Downsample, 2)  // 每2个点压缩为1个
  assert_eq(compressed_series.data_points.length(), 3)
  
  // 8. 测试时间序列插值
  let interpolated_value = time_series.interpolate(1609459300000)  // 在两个点之间插值
  assert_true(interpolated_value > 11.8 && interpolated_value < 13.2)
}

// 测试8: 缓存机制
test "缓存机制测试" {
  // 1. 创建LRU缓存
  let lru_cache = LRUCache({ 
    capacity: 3, 
    entries = [] 
  })
  
  // 2. 添加缓存项
  lru_cache.put("key1", "value1")
  lru_cache.put("key2", "value2")
  lru_cache.put("key3", "value3")
  
  // 3. 获取缓存项
  let value1 = lru_cache.get("key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = lru_cache.get("key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // 4. 测试容量限制
  lru_cache.put("key4", "value4")  // 应该淘汰最久未使用的项
  
  let evicted_value = lru_cache.get("key3")  // key3应该被淘汰
  match evicted_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let value4 = lru_cache.get("key4")
  match value4 {
    Some(v) => assert_eq(v, "value4")
    None => assert_true(false)
  }
  
  // 5. 测试缓存统计
  let stats = lru_cache.get_stats()
  assert_eq(stats.hits, 3)
  assert_eq(stats.misses, 1)
  assert_eq(stats.evictions, 1)
  
  // 6. 测试TTL缓存
  let ttl_cache = TTLCache({ 
    entries = [],
    default_ttl: 1000  // 1秒TTL
  })
  
  ttl_cache.put_with_ttl("temp_key", "temp_value", 500)  // 0.5秒TTL
  
  let immediate_value = ttl_cache.get("temp_key")
  match immediate_value {
    Some(v) => assert_eq(v, "temp_value")
    None => assert_true(false)
  }
  
  // 模拟时间流逝
  advance_time(600)
  
  let expired_value = ttl_cache.get("temp_key")
  match expired_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 7. 测试多级缓存
  let l1_cache = LRUCache({ capacity: 2, entries = [] })
  let l2_cache = LRUCache({ capacity: 4, entries = [] })
  
  let multi_level_cache = MultiLevelCache({
    levels: [l1_cache, l2_cache]
  })
  
  multi_level_cache.put("ml_key1", "ml_value1")
  multi_level_cache.put("ml_key2", "ml_value2")
  multi_level_cache.put("ml_key3", "ml_value3")
  multi_level_cache.put("ml_key4", "ml_value4")
  multi_level_cache.put("ml_key5", "ml_value5")
  
  let ml_value1 = multi_level_cache.get("ml_key1")
  match ml_value1 {
    Some(v) => assert_eq(v, "ml_value1")
    None => assert_true(false)
  }
  
  let ml_stats = multi_level_cache.get_stats()
  assert_eq(ml_stats.l1_hits, 0)
  assert_eq(ml_stats.l2_hits, 1)
}

// 测试9: 日志记录和聚合
test "日志记录和聚合测试" {
  // 1. 创建日志记录器
  let logger = Logger({
    name: "azimuth-logger",
    level: INFO,
    handlers = [
      ConsoleHandler({ format: Json }),
      FileHandler({ path: "/tmp/azimuth.log", format: Text })
    ]
  })
  
  // 2. 记录不同级别的日志
  logger.debug("调试信息")
  logger.info("一般信息")
  logger.warn("警告信息")
  logger.error("错误信息")
  
  // 3. 创建结构化日志
  let structured_log = LogEntry({
    timestamp: current_time_millis(),
    level: ERROR,
    message: "数据库连接失败",
    fields = [
      ("component", "database"),
      ("operation", "connect"),
      ("retry_count", "3"),
      ("error_code", "1001")
    ]
  })
  
  logger.log_structured(structured_log)
  
  // 4. 测试日志聚合器
  let log_aggregator = LogAggregator({
    time_window: 60000,  // 1分钟窗口
    aggregation_rules = [
      CountByLevel(),
      GroupByComponent(),
      TopErrors(5)
    ]
  })
  
  // 添加更多日志条目
  for i = 0; i < 10; i = i + 1 {
    logger.info("处理请求 " + i.to_string())
    if i % 3 == 0 {
      logger.warn("处理请求 " + i.to_string() + " 时遇到警告")
    }
    if i % 5 == 0 {
      logger.error("处理请求 " + i.to_string() + " 时遇到错误")
    }
  }
  
  // 5. 获取聚合结果
  let aggregated_logs = log_aggregator.aggregate()
  
  let error_count = aggregated_logs.get_count_by_level(ERROR)
  let warn_count = aggregated_logs.get_count_by_level(WARN)
  let info_count = aggregated_logs.get_count_by_level(INFO)
  
  assert_eq(error_count, 3)  // 0, 5, 10
  assert_eq(warn_count, 4)   // 0, 3, 6, 9
  assert_eq(info_count, 11)  // 0-10
  
  // 6. 测试日志过滤
  let error_logs = log_aggregator.filter_by_level(ERROR)
  assert_eq(error_logs.length(), 3)
  
  let component_logs = log_aggregator.filter_by_field("component", "database")
  assert_eq(component_logs.length(), 1)
  
  // 7. 测试日志搜索
  let search_results = log_aggregator.search("处理请求")
  assert_eq(search_results.length(), 11)
  
  let error_search = log_aggregator.search("错误")
  assert_eq(error_search.length(), 3)
}

// 测试10: 跨平台兼容性
test "跨平台兼容性测试" {
  // 1. 测试平台检测
  let platform_info = detect_platform()
  
  assert_true(platform_info.os == "linux" || 
              platform_info.os == "windows" || 
              platform_info.os == "macos")
  
  assert_true(platform_info.arch == "x86_64" || 
              platform_info.arch == "arm64" || 
              platform_info.arch == "x86")
  
  // 2. 测试路径处理
  let path_handler = PathHandler({ platform: platform_info.os })
  
  let joined_path = path_handler.join("home", "user", "documents")
  if platform_info.os == "windows" {
    assert_true(joined_path.contains("\\"))
  } else {
    assert_true(joined_path.contains("/"))
  }
  
  let normalized_path = path_handler.normalize("home//user/./documents/../desktop")
  assert_false(normalized_path.contains("//"))
  assert_false(normalized_path.contains("./"))
  assert_false(normalized_path.contains("../"))
  
  // 3. 测试环境变量处理
  let env_handler = EnvironmentHandler({ platform: platform_info.os })
  
  env_handler.set("AZIMUTH_TEST", "test_value")
  let test_value = env_handler.get("AZIMUTH_TEST")
  match test_value {
    Some(v) => assert_eq(v, "test_value")
    None => assert_true(false)
  }
  
  // 4. 测试文件系统操作
  let fs_handler = FileSystemHandler({ platform: platform_info.os })
  
  let temp_dir = fs_handler.create_temp_directory()
  assert_true(fs_handler.exists(temp_dir))
  assert_true(fs_handler.is_directory(temp_dir))
  
  let temp_file = path_handler.join(temp_dir, "test.txt")
  let write_success = fs_handler.write_text_file(temp_file, "测试内容")
  assert_true(write_success)
  
  let read_content = fs_handler.read_text_file(temp_file)
  match read_content {
    Some(content) => assert_eq(content, "测试内容")
    None => assert_true(false)
  }
  
  // 5. 测试网络配置
  let network_handler = NetworkHandler({ platform: platform_info.os })
  
  let local_addresses = network_handler.get_local_addresses()
  assert_true(local_addresses.length() > 0)
  
  let is_localhost_reachable = network_handler.is_reachable("localhost", 80)
  // 结果可能因环境而异，不进行断言
  
  // 6. 测试进程管理
  let process_handler = ProcessHandler({ platform: platform_info.os })
  
  let current_pid = process_handler.get_current_pid()
  assert_true(current_pid > 0)
  
  let process_info = process_handler.get_process_info(current_pid)
  assert_true(process_info.is_running)
  
  // 7. 清理
  let cleanup_success = fs_handler.delete_directory(temp_dir)
  assert_true(cleanup_success)
  assert_false(fs_handler.exists(temp_dir))
}