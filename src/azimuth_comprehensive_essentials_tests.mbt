// Azimuth Comprehensive Essentials Test Suite
// 综合核心测试套件 - 涵盖关键功能的补充测试用例

// Test 1: 数据转换和序列化测试
test "data conversion and serialization" {
  // 测试基本类型转换
  let int_val = 42
  let float_val = 3.14159
  let bool_val = true
  let string_val = "test_string"
  
  // 测试数值转换
  let int_to_float = int_val.to_float()
  assert_true(int_to_float == 42.0)
  
  let float_to_int = float_val.to_int()
  assert_true(float_to_int == 3)
  
  // 测试布尔值转换
  let bool_to_string = bool_val.to_string()
  assert_eq(bool_to_string, "true")
  
  let string_to_bool = "true".to_bool()
  assert_true(string_to_bool)
  
  // 测试字符串转换
  let string_to_int = "123".to_int()
  assert_eq(string_to_int, 123)
  
  let string_to_float = "3.14".to_float()
  assert_true(string_to_float == 3.14)
  
  // 测试JSON序列化
  let test_data = {
    "id": 123,
    "name": "test_item",
    "active": true,
    "values": [1, 2, 3, 4, 5]
  }
  
  let json_string = Json::serialize(test_data)
  assert_true(json_string.contains("\"id\":123"))
  assert_true(json_string.contains("\"name\":\"test_item\""))
  assert_true(json_string.contains("\"active\":true"))
  
  // 测试JSON反序列化
  let deserialized_data = Json::deserialize(json_string)
  assert_eq(deserialized_data["id"], 123)
  assert_eq(deserialized_data["name"], "test_item")
  assert_eq(deserialized_data["active"], true)
  
  // 测试二进制序列化
  let binary_data = Binary::serialize(test_data)
  assert_true(binary_data.length() > 0)
  
  let binary_deserialized = Binary::deserialize(binary_data)
  assert_eq(binary_deserialized["id"], 123)
  assert_eq(binary_deserialized["name"], "test_item")
}

// Test 2: 并发处理和线程安全测试
test "concurrent processing and thread safety" {
  let concurrent_processor = ConcurrentProcessor::new(4) // 4个工作线程
  
  // 测试并发数据处理
  let test_data = []
  for i in 0..=1000 {
    test_data.push(("item_" + i.to_string(), i))
  }
  
  // 并发处理数据
  let results = ConcurrentProcessor::process_map(concurrent_processor, test_data, |(name, value)| {
    (name.toUpperCase(), value * 2)
  })
  
  // 验证结果
  assert_eq(results.length(), 1001)
  assert_eq(results[0].0, "ITEM_0")
  assert_eq(results[0].1, 0)
  assert_eq(results[1000].0, "ITEM_1000")
  assert_eq(results[1000].1, 2000)
  
  // 测试并发过滤
  let filtered_results = ConcurrentProcessor::process_filter(concurrent_processor, test_data, |(_, value)| {
    value % 100 == 0
  })
  
  assert_eq(filtered_results.length(), 11) // 0, 100, 200, ..., 1000
  
  // 测试并发聚合
  let sum_result = ConcurrentProcessor::process_reduce(concurrent_processor, test_data, 0, |acc, (_, value)| {
    acc + value
  })
  
  // 计算期望值: 0 + 1 + 2 + ... + 1000 = 1000 * 1001 / 2 = 500500
  assert_eq(sum_result, 500500)
  
  // 测试线程安全的计数器
  let safe_counter = ThreadSafeCounter::new()
  
  // 并发增加计数器
  let counter_tasks = []
  for i in 0..=100 {
    counter_tasks.push(|| => {
      for j in 0..=100 {
        safe_counter.increment()
      }
    })
  }
  
  ConcurrentProcessor::run_tasks(concurrent_processor, counter_tasks)
  
  // 验证计数器结果
  assert_eq(safe_counter.value(), 101 * 101) // 101个任务，每个增加101次
  
  // 测试线程安全的集合
  let safe_set = ThreadSafeSet::new()
  
  // 并发添加元素
  let set_tasks = []
  for i in 0..=1000 {
    set_tasks.push(|| => {
      safe_set.add("item_" + i.to_string())
    })
  }
  
  ConcurrentProcessor::run_tasks(concurrent_processor, set_tasks)
  
  // 验证集合大小
  assert_eq(safe_set.size(), 1001)
  assert_true(safe_set.contains("item_0"))
  assert_true(safe_set.contains("item_1000"))
  assert_false(safe_set.contains("item_1001"))
}

// Test 3: 缓存机制测试
test "caching mechanisms" {
  let cache = LRUCache::new(100) // 缓存容量100
  
  // 测试基本缓存操作
  cache.put("key1", "value1")
  cache.put("key2", "value2")
  cache.put("key3", "value3")
  
  assert_eq(cache.get("key1"), Some("value1"))
  assert_eq(cache.get("key2"), Some("value2"))
  assert_eq(cache.get("key3"), Some("value3"))
  assert_eq(cache.get("nonexistent"), None)
  
  // 测试缓存更新
  cache.put("key1", "updated_value1")
  assert_eq(cache.get("key1"), Some("updated_value1"))
  
  // 测试缓存删除
  cache.remove("key2")
  assert_eq(cache.get("key2"), None)
  assert_eq(cache.get("key1"), Some("updated_value1"))
  assert_eq(cache.get("key3"), Some("value3"))
  
  // 测试LRU淘汰策略
  for i in 0..=150 {
    cache.put("key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // 验证缓存大小不超过限制
  assert_eq(cache.size(), 100)
  
  // 最早的条目应该被淘汰
  assert_eq(cache.get("key1"), None)
  assert_eq(cache.get("key2"), None)
  assert_eq(cache.get("key3"), None)
  
  // 最新的条目应该存在
  assert_eq(cache.get("key_150"), Some("value_150"))
  assert_eq(cache.get("key_149"), Some("value_149"))
  
  // 测试缓存统计
  let stats = cache.get_stats()
  assert_eq(stats.hits, 0)
  assert_eq(stats.misses, 0)
  assert_eq(stats.evictions, 53) // 淘汰了153-100=53个条目
  
  // 测试缓存命中
  assert_eq(cache.get("key_150"), Some("value_150"))
  assert_eq(cache.get("key_149"), Some("value_149"))
  assert_eq(cache.get("nonexistent"), None)
  
  let updated_stats = cache.get_stats()
  assert_eq(updated_stats.hits, 2)
  assert_eq(updated_stats.misses, 1)
  
  // 测试带过期时间的缓存
  let ttl_cache = TTLCache::new(100, 1000) // 容量100，过期时间1000ms
  
  ttl_cache.put("ttl_key1", "ttl_value1")
  ttl_cache.put("ttl_key2", "ttl_value2")
  
  assert_eq(ttl_cache.get("ttl_key1"), Some("ttl_value1"))
  assert_eq(ttl_cache.get("ttl_key2"), Some("ttl_value2"))
  
  // 等待过期
  Time::sleep(1100)
  
  assert_eq(ttl_cache.get("ttl_key1"), None)
  assert_eq(ttl_cache.get("ttl_key2"), None)
  
  // 测试缓存清理
  ttl_cache.put("ttl_key3", "ttl_value3")
  assert_eq(ttl_cache.get("ttl_key3"), Some("ttl_value3"))
  
  ttl_cache.cleanup_expired()
  assert_eq(ttl_cache.get("ttl_key3"), None)
}

// Test 4: 配置管理测试
test "configuration management" {
  let config_manager = ConfigManager::new()
  
  // 测试基本配置操作
  config_manager.set("app.name", "Azimuth")
  config_manager.set("app.version", "1.0.0")
  config_manager.set("app.debug", true)
  config_manager.set("server.port", 8080)
  config_manager.set("server.host", "localhost")
  
  // 测试配置获取
  assert_eq(config_manager.get("app.name"), Some("Azimuth"))
  assert_eq(config_manager.get("app.version"), Some("1.0.0"))
  assert_eq(config_manager.get("app.debug"), Some("true"))
  assert_eq(config_manager.get("server.port"), Some("8080"))
  assert_eq(config_manager.get("server.host"), Some("localhost"))
  assert_eq(config_manager.get("nonexistent"), None)
  
  // 测试类型化配置获取
  assert_eq(config_manager.get_string("app.name"), Some("Azimuth"))
  assert_eq(config_manager.get_int("server.port"), Some(8080))
  assert_eq(config_manager.get_bool("app.debug"), Some(true))
  assert_eq(config_manager.get_float("nonexistent"), None)
  
  // 测试配置组
  let app_config = config_manager.get_group("app")
  assert_eq(app_config.get("name"), Some("Azimuth"))
  assert_eq(app_config.get("version"), Some("1.0.0"))
  assert_eq(app_config.get("debug"), Some("true"))
  
  let server_config = config_manager.get_group("server")
  assert_eq(server_config.get("port"), Some("8080"))
  assert_eq(server_config.get("host"), Some("localhost"))
  
  // 测试配置更新
  config_manager.set("app.version", "1.1.0")
  config_manager.set("server.port", 9090)
  
  assert_eq(config_manager.get("app.version"), Some("1.1.0"))
  assert_eq(config_manager.get("server.port"), Some("9090"))
  
  // 测试配置监听器
  let mut change_count = 0
  let mut last_key = ""
  let mut last_value = ""
  
  config_manager.add_listener(|key, value| {
    change_count = change_count + 1
    last_key = key
    last_value = value
  })
  
  config_manager.set("test.key", "test_value")
  
  assert_eq(change_count, 1)
  assert_eq(last_key, "test.key")
  assert_eq(last_value, "test_value")
  
  // 测试配置持久化
  let config_file = "/tmp/test_config.json"
  config_manager.save_to_file(config_file)
  
  let new_config_manager = ConfigManager::new()
  new_config_manager.load_from_file(config_file)
  
  assert_eq(new_config_manager.get("app.name"), Some("Azimuth"))
  assert_eq(new_config_manager.get("app.version"), Some("1.1.0"))
  assert_eq(new_config_manager.get("server.port"), Some("9090"))
  assert_eq(new_config_manager.get("test.key"), Some("test_value"))
  
  // 测试环境变量覆盖
  Environment::set("AZIMUTH_APP_NAME", "Azimuth-Override")
  Environment::set("AZIMUTH_SERVER_PORT", "9999")
  
  config_manager.load_from_env("AZIMUTH")
  
  assert_eq(config_manager.get("app.name"), Some("Azimuth-Override"))
  assert_eq(config_manager.get("server.port"), Some("9999"))
  assert_eq(config_manager.get("app.version"), Some("1.1.0")) // 未被覆盖
}

// Test 5: 网络通信测试
test "network communication" {
  let mock_server = MockHttpServer::new(8080)
  mock_server.start()
  
  // 配置模拟服务器响应
  mock_server.get("/api/test", {
    "status": 200,
    "headers": {"Content-Type": "application/json"},
    "body": "{\"message\":\"Hello from server\"}"
  })
  
  mock_server.post("/api/data", {
    "status": 201,
    "headers": {"Content-Type": "application/json"},
    "body": "{\"id\":123,\"status\":\"created\"}"
  })
  
  mock_server.get("/api/error", {
    "status": 500,
    "headers": {"Content-Type": "application/json"},
    "body": "{\"error\":\"Internal server error\"}"
  })
  
  // 测试HTTP客户端
  let http_client = HttpClient::new()
  http_client.set_timeout(5000) // 5秒超时
  
  // 测试GET请求
  let get_response = http_client.get("http://localhost:8080/api/test")
  
  assert_eq(get_response.status_code, 200)
  assert_eq(get_response.get_header("Content-Type"), Some("application/json"))
  
  let get_body = get_response.body
  assert_true(get_body.contains("Hello from server"))
  
  let get_json = Json::deserialize(get_body)
  assert_eq(get_json["message"], "Hello from server")
  
  // 测试POST请求
  let post_data = {
    "name": "test_item",
    "value": 42
  }
  
  let post_response = http_client.post(
    "http://localhost:8080/api/data",
    Json::serialize(post_data),
    {"Content-Type": "application/json"}
  )
  
  assert_eq(post_response.status_code, 201)
  assert_eq(post_response.get_header("Content-Type"), Some("application/json"))
  
  let post_body = post_response.body
  let post_json = Json::deserialize(post_body)
  assert_eq(post_json["id"], 123)
  assert_eq(post_json["status"], "created")
  
  // 测试错误处理
  let error_response = http_client.get("http://localhost:8080/api/error")
  
  assert_eq(error_response.status_code, 500)
  
  let error_body = error_response.body
  let error_json = Json::deserialize(error_body)
  assert_eq(error_json["error"], "Internal server error")
  
  // 测试超时处理
  mock_server.delay("/api/slow", 10000) // 10秒延迟
  
  let slow_start_time = Time::now()
  let slow_response = http_client.get("http://localhost:8080/api/slow")
  let slow_end_time = Time::now()
  
  assert_true(slow_response.is_error()) // 应该超时
  assert_true(slow_end_time - slow_start_time >= 5000) // 至少等待5秒
  
  // 测试重试机制
  let retry_config = RetryConfig::new(3, 1000) // 最多重试3次，间隔1秒
  http_client.set_retry_config(retry_config)
  
  let mut attempt_count = 0
  mock_server.get("/api/flaky", {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      {
        "status": 500,
        "headers": {"Content-Type": "application/json"},
        "body": "{\"error\":\"Temporary failure\"}"
      }
    } else {
      {
        "status": 200,
        "headers": {"Content-Type": "application/json"},
        "body": "{\"message\":\"Success after retries\"}"
      }
    }
  })
  
  let flaky_response = http_client.get("http://localhost:8080/api/flaky")
  
  assert_eq(flaky_response.status_code, 200)
  assert_eq(attempt_count, 3) // 应该尝试了3次
  
  let flaky_body = flaky_response.body
  let flaky_json = Json::deserialize(flaky_body)
  assert_eq(flaky_json["message"], "Success after retries")
  
  mock_server.stop()
}

// Test 6: 时间序列数据处理测试
test "time series data processing" {
  let time_series_processor = TimeSeriesProcessor::new()
  
  // 生成测试时间序列数据
  let base_time = Time::now() - 3600000 // 1小时前
  let time_series_data = []
  
  for i in 0..=3600 {
    let timestamp = base_time + i * 1000 // 每秒一个数据点
    let value = 50.0 + 10.0 * Math::sin(i * 0.01) + Random::next_float() * 5.0 // 带噪声的正弦波
    time_series_data.push(TimeSeriesPoint::new(timestamp, value))
  }
  
  // 测试数据聚合
  let aggregated_1min = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(1), AggregationType::Average)
  assert_eq(aggregated_1min.length(), 60) // 1小时 / 1分钟 = 60个数据点
  
  let aggregated_5min = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(5), AggregationType::Average)
  assert_eq(aggregated_5min.length(), 12) // 1小时 / 5分钟 = 12个数据点
  
  // 测试不同聚合类型
  let max_aggregated = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(5), AggregationType::Max)
  let min_aggregated = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(5), AggregationType::Min)
  let sum_aggregated = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(5), AggregationType::Sum)
  let count_aggregated = time_series_processor.aggregate(time_series_data, TimeWindow::Minutes(5), AggregationType::Count)
  
  for i in 0..=11 {
    assert_true(max_aggregated[i].value >= min_aggregated[i].value)
    assert_true(sum_aggregated[i].value >= max_aggregated[i].value)
    assert_eq(count_aggregated[i].value, 300.0) // 5分钟 * 60秒 = 300个数据点
  }
  
  // 测试数据平滑
  let smoothed_data = time_series_processor.smooth(time_series_data, SmoothingType::MovingAverage, 60) // 60点移动平均
  assert_eq(smoothed_data.length(), time_series_data.length())
  
  // 验证平滑效果：原始数据应该比平滑数据有更多变化
  let mut original_variance = 0.0
  let mut smoothed_variance = 0.0
  
  for i in 1..=time_series_data.length() - 1 {
    original_variance = original_variance + Math::abs(time_series_data[i].value - time_series_data[i-1].value)
    smoothed_variance = smoothed_variance + Math::abs(smoothed_data[i].value - smoothed_data[i-1].value)
  }
  
  assert_true(smoothed_variance < original_variance)
  
  // 测试趋势检测
  let increasing_trend = []
  for i in 0..=100 {
    increasing_trend.push(TimeSeriesPoint::new(base_time + i * 1000, i.to_float()))
  }
  
  let trend_result = time_series_processor.detect_trend(increasing_trend)
  assert_eq(trend_result.trend_type, TrendType::Increasing)
  assert_true(trend_result.confidence > 0.9)
  
  let decreasing_trend = []
  for i in 0..=100 {
    decreasing_trend.push(TimeSeriesPoint::new(base_time + i * 1000, (100 - i).to_float()))
  }
  
  let decreasing_result = time_series_processor.detect_trend(decreasing_trend)
  assert_eq(decreasing_result.trend_type, TrendType::Decreasing)
  assert_true(decreasing_result.confidence > 0.9)
  
  // 测试异常检测
  let normal_data_with_outlier = []
  for i in 0..=100 {
    let value = 50.0 + Random::next_float() * 10.0
    normal_data_with_outlier.push(TimeSeriesPoint::new(base_time + i * 1000, value))
  }
  
  // 添加异常值
  normal_data_with_outlier[50] = TimeSeriesPoint::new(base_time + 50 * 1000, 200.0)
  normal_data_with_outlier[75] = TimeSeriesPoint::new(base_time + 75 * 1000, -50.0)
  
  let anomalies = time_series_processor.detect_anomalies(normal_data_with_outlier, AnomalyDetectionMethod::ZScore, 2.0)
  
  assert_true(anomalies.length() >= 2)
  assert_true(anomalies.any(|a| a.index == 50))
  assert_true(anomalies.any(|a| a.index == 75))
  
  // 测试预测
  let prediction_result = time_series_processor.predict(time_series_data, PredictionMethod::LinearRegression, 300) // 预测5分钟
  
  assert_eq(prediction_result.predicted_values.length(), 300)
  assert_true(prediction_result.confidence_score > 0.0)
  assert_true(prediction_result.mean_squared_error >= 0.0)
}

// Test 7: 错误边界处理测试
test "error boundary handling" {
  let error_boundary = ErrorBoundary::new()
  
  // 测试基本错误捕获
  let result1 = error_boundary.execute(|| => {
    10 / 2 // 正常操作
  })
  
  assert_true(result1.is_success())
  assert_eq(result1.get_value(), 5)
  
  let result2 = error_boundary.execute(|| => {
    10 / 0 // 除零错误
  })
  
  assert_true(result2.is_error())
  assert_true(result2.get_error().contains("division by zero"))
  
  // 测试错误恢复策略
  error_boundary.set_recovery_strategy(RecoveryStrategy::Retry(3))
  
  let mut attempt_count = 0
  let result3 = error_boundary.execute(|| => {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error::raise("temporary failure")
    } else {
      "success after retries"
    }
  })
  
  assert_true(result3.is_success())
  assert_eq(result3.get_value(), "success after retries")
  assert_eq(attempt_count, 3)
  
  // 测试默认值恢复
  error_boundary.set_recovery_strategy(RecoveryStrategy::DefaultValue("default_value"))
  
  let result4 = error_boundary.execute(|| => {
    Error::raise("always fails")
  })
  
  assert_true(result4.is_success())
  assert_eq(result4.get_value(), "default_value")
  
  // 测试回调恢复
  let mut callback_called = false
  error_boundary.set_recovery_strategy(RecoveryStrategy::Callback(|| => {
    callback_called = true
    "callback recovery"
  }))
  
  let result5 = error_boundary.execute(|| => {
    Error::raise("always fails")
  })
  
  assert_true(result5.is_success())
  assert_eq(result5.get_value(), "callback recovery")
  assert_true(callback_called)
  
  // 测试错误分类和处理
  error_boundary.add_error_handler(ErrorType::NetworkError, || => {
    "network error handled"
  })
  
  error_boundary.add_error_handler(ErrorType::ValidationError, || => {
    "validation error handled"
  })
  
  let result6 = error_boundary.execute(|| => {
    Error::raise_with_type("network timeout", ErrorType::NetworkError)
  })
  
  assert_true(result6.is_success())
  assert_eq(result6.get_value(), "network error handled")
  
  let result7 = error_boundary.execute(|| => {
    Error::raise_with_type("invalid input", ErrorType::ValidationError)
  })
  
  assert_true(result7.is_success())
  assert_eq(result7.get_value(), "validation error handled")
  
  // 测试错误统计
  let stats = error_boundary.get_stats()
  assert_eq(stats.total_executions, 7)
  assert_eq(stats.successful_executions, 4)
  assert_eq(stats.failed_executions, 3)
  assert_true(stats.success_rate > 0.5)
  
  // 测试错误报告
  let error_report = error_boundary.generate_error_report()
  assert_true(error_report.contains("Error Statistics"))
  assert_true(error_report.contains("Total Executions: 7"))
  assert_true(error_report.contains("Successful: 4"))
  assert_true(error_report.contains("Failed: 3"))
}

// Test 8: 资源限制测试
test "resource limits" {
  let resource_limiter = ResourceLimiter::new()
  
  // 设置资源限制
  resource_limiter.set_memory_limit(100 * 1024 * 1024) // 100MB
  resource_limiter.set_cpu_limit(0.8) // 80% CPU使用率
  resource_limiter.set_network_limit(1024 * 1024) // 1MB网络带宽
  resource_limiter.set_file_handle_limit(100) // 100个文件句柄
  
  // 测试内存限制
  let memory_usage_before = resource_limiter.get_memory_usage()
  
  // 分配内存
  let large_data = ByteArray::new(50 * 1024 * 1024) // 50MB
  
  let memory_usage_after = resource_limiter.get_memory_usage()
  let memory_diff = memory_usage_after - memory_usage_before
  
  assert_true(memory_diff >= 50 * 1024 * 1024)
  assert_true(memory_diff <= 60 * 1024 * 1024) // 允许一些开销
  
  // 检查是否在限制内
  assert_true(resource_limiter.check_memory_limit())
  
  // 测试超过内存限制
  let oversized_data = ByteArray::new(120 * 1024 * 1024) // 120MB，超过限制
  
  assert_false(resource_limiter.check_memory_limit())
  
  // 清理内存
  large_data = []
  oversized_data = []
  
  // 测试网络限制
  let network_usage_before = resource_limiter.get_network_usage()
  
  // 模拟网络传输
  let network_data = ByteArray::new(512 * 1024) // 512KB
  resource_limiter.record_network_transfer(network_data.length())
  
  let network_usage_after = resource_limiter.get_network_usage()
  assert_eq(network_usage_after - network_usage_before, 512 * 1024)
  
  // 检查是否在限制内
  assert_true(resource_limiter.check_network_limit())
  
  // 测试超过网络限制
  let large_network_data = ByteArray::new(2 * 1024 * 1024) // 2MB，超过限制
  resource_limiter.record_network_transfer(large_network_data.length())
  
  assert_false(resource_limiter.check_network_limit())
  
  // 测试文件句柄限制
  for i in 0..=50 {
    let file = File::open("/tmp/test_file_" + i.to_string(), "w")
    resource_limiter.record_file_handle_open()
    file.close()
  }
  
  assert_true(resource_limiter.check_file_handle_limit())
  
  // 测试超过文件句柄限制
  for i in 51..=150 {
    let file = File::open("/tmp/test_file_" + i.to_string(), "w")
    resource_limiter.record_file_handle_open()
    file.close()
  }
  
  assert_false(resource_limiter.check_file_handle_limit())
  
  // 测试资源监控和警报
  let mut alert_triggered = false
  let mut alert_type = ""
  let mut alert_message = ""
  
  resource_limiter.set_alert_callback(|type, message| {
    alert_triggered = true
    alert_type = type
    alert_message = message
  })
  
  // 触发内存警报
  resource_limiter.check_all_limits()
  
  if !resource_limiter.check_memory_limit() {
    assert_true(alert_triggered)
    assert_eq(alert_type, "memory_limit_exceeded")
    assert_true(alert_message.contains("Memory usage exceeds limit"))
  }
  
  // 测试资源使用报告
  let usage_report = resource_limiter.generate_usage_report()
  assert_true(usage_report.contains("Resource Usage Report"))
  assert_true(usage_report.contains("Memory Usage:"))
  assert_true(usage_report.contains("Network Usage:"))
  assert_true(usage_report.contains("File Handles:"))
  
  // 测试资源清理
  resource_limiter.cleanup_resources()
  
  // 验证清理效果
  let cleanup_memory_usage = resource_limiter.get_memory_usage()
  assert_true(cleanup_memory_usage < memory_usage_after)
}

// Test 9: 数据一致性测试
test "data consistency" {
  let consistency_manager = ConsistencyManager::new()
  
  // 创建测试数据
  let original_data = {
    "id": 123,
    "name": "test_item",
    "value": 42,
    "timestamp": Time::now(),
    "checksum": ""
  }
  
  // 计算校验和
  let data_string = Json::serialize(original_data)
  let checksum = ConsistencyManager::calculate_checksum(data_string)
  let data_with_checksum = { ...original_data, "checksum": checksum }
  
  // 测试数据完整性验证
  let integrity_check = consistency_manager.verify_integrity(data_with_checksum)
  assert_true(integrity_check.is_valid)
  
  // 测试篡改检测
  let tampered_data = { ...data_with_checksum, "value": 999 }
  let tampered_check = consistency_manager.verify_integrity(tampered_data)
  assert_false(tampered_check.is_valid)
  assert_eq(tampered_check.violation_type, IntegrityViolation::ChecksumMismatch)
  
  // 测试数据版本控制
  let versioned_data = consistency_manager.create_version("data_123", data_with_checksum)
  assert_eq(versioned_data.version, 1)
  assert_eq(versioned_data.data, data_with_checksum)
  assert_true(versioned_data.timestamp > 0)
  
  // 更新数据
  let updated_data = { ...data_with_checksum, "value": 43 }
  let updated_checksum = ConsistencyManager::calculate_checksum(Json::serialize(updated_data))
  let updated_data_with_checksum = { ...updated_data, "checksum": updated_checksum }
  
  let new_version = consistency_manager.update_version("data_123", updated_data_with_checksum)
  assert_eq(new_version.version, 2)
  assert_eq(new_version.data, updated_data_with_checksum)
  assert_true(new_version.timestamp > versioned_data.timestamp)
  
  // 测试版本历史
  let version_history = consistency_manager.get_version_history("data_123")
  assert_eq(version_history.length(), 2)
  assert_eq(version_history[0].version, 1)
  assert_eq(version_history[1].version, 2)
  
  // 测试数据回滚
  let rollback_result = consistency_manager.rollback_to_version("data_123", 1)
  assert_true(rollback_result.success)
  assert_eq(rollback_result.data.version, 1)
  assert_eq(rollback_result.data.data["value"], 42)
  
  // 测试并发一致性
  let concurrent_data = consistency_manager.create_concurrent_data("concurrent_123", data_with_checksum)
  
  // 模拟并发修改
  let concurrent_processor = ConcurrentProcessor::new(4)
  let concurrent_tasks = []
  
  for i in 0..=10 {
    concurrent_tasks.push(|| => {
      let current_data = consistency_manager.get_concurrent_data("concurrent_123")
      let updated_value = current_data.data["value"] + 1
      let updated_data = { ...current_data.data, "value": updated_value }
      let updated_checksum = ConsistencyManager::calculate_checksum(Json::serialize(updated_data))
      let final_data = { ...updated_data, "checksum": updated_checksum }
      
      consistency_manager.update_concurrent_data("concurrent_123", final_data, current_data.version)
    })
  }
  
  ConcurrentProcessor::run_tasks(concurrent_processor, concurrent_tasks)
  
  // 验证最终一致性
  let final_data = consistency_manager.get_concurrent_data("concurrent_123")
  assert_eq(final_data.data["value"], 42 + 11) // 原值42 + 11次更新
  
  // 测试冲突解决
  let conflict_data1 = { ...data_with_checksum, "value": 100 }
  let conflict_data2 = { ...data_with_checksum, "value": 200 }
  
  let conflict_result = consistency_manager.resolve_conflict(
    "conflict_123",
    conflict_data1,
    conflict_data2,
    ConflictResolutionStrategy::LastWriterWins
  )
  
  assert_true(conflict_result.resolved)
  assert_eq(conflict_result.resolved_data["value"], 200) // 最后写入者获胜
  
  // 测试事务一致性
  let transaction = consistency_manager.begin_transaction()
  
  consistency_manager.transaction_set(transaction, "key1", "value1")
  consistency_manager.transaction_set(transaction, "key2", "value2")
  consistency_manager.transaction_set(transaction, "key3", "value3")
  
  // 验证事务中的数据
  assert_eq(consistency_manager.transaction_get(transaction, "key1"), Some("value1"))
  assert_eq(consistency_manager.transaction_get(transaction, "key2"), Some("value2"))
  assert_eq(consistency_manager.transaction_get(transaction, "key3"), Some("value3"))
  
  // 提交事务
  let commit_result = consistency_manager.commit_transaction(transaction)
  assert_true(commit_result.success)
  
  // 验证提交后的数据
  assert_eq(consistency_manager.get("key1"), Some("value1"))
  assert_eq(consistency_manager.get("key2"), Some("value2"))
  assert_eq(consistency_manager.get("key3"), Some("value3"))
  
  // 测试事务回滚
  let rollback_transaction = consistency_manager.begin_transaction()
  consistency_manager.transaction_set(rollback_transaction, "key4", "value4")
  consistency_manager.transaction_set(rollback_transaction, "key5", "value5")
  
  // 回滚事务
  consistency_manager.rollback_transaction(rollback_transaction)
  
  // 验证回滚后的数据
  assert_eq(consistency_manager.get("key4"), None)
  assert_eq(consistency_manager.get("key5"), None)
}

// Test 10: 生命周期管理测试
test "lifecycle management" {
  let lifecycle_manager = LifecycleManager::new()
  
  // 创建测试组件
  let component1 = TestComponent::new("component1")
  let component2 = TestComponent::new("component2")
  let component3 = TestComponent::new("component3")
  
  // 注册组件
  lifecycle_manager.register_component(component1)
  lifecycle_manager.register_component(component2)
  lifecycle_manager.register_component(component3)
  
  // 测试初始化
  let init_result = lifecycle_manager.initialize_all()
  assert_true(init_result.success)
  assert_eq(init_result.initialized_count, 3)
  assert_eq(init_result.failed_count, 0)
  
  // 验证组件状态
  assert_eq(component1.get_state(), ComponentState::Initialized)
  assert_eq(component2.get_state(), ComponentState::Initialized)
  assert_eq(component3.get_state(), ComponentState::Initialized)
  
  // 测试启动
  let start_result = lifecycle_manager.start_all()
  assert_true(start_result.success)
  assert_eq(start_result.started_count, 3)
  assert_eq(start_result.failed_count, 0)
  
  // 验证组件状态
  assert_eq(component1.get_state(), ComponentState::Running)
  assert_eq(component2.get_state(), ComponentState::Running)
  assert_eq(component3.get_state(), ComponentState::Running)
  
  // 测试依赖关系
  let dependent_component = DependentComponent::new("dependent_component", ["component1", "component2"])
  lifecycle_manager.register_component(dependent_component)
  
  // 初始化依赖组件
  let dependent_init_result = lifecycle_manager.initialize_component("dependent_component")
  assert_true(dependent_init_result.success)
  
  // 验证依赖关系
  assert_eq(dependent_component.get_state(), ComponentState::Initialized)
  assert_true(dependent_component.get_dependencies().contains("component1"))
  assert_true(dependent_component.get_dependencies().contains("component2"))
  
  // 测试停止
  let stop_result = lifecycle_manager.stop_all()
  assert_true(stop_result.success)
  assert_eq(stop_result.stopped_count, 4)
  assert_eq(stop_result.failed_count, 0)
  
  // 验证组件状态
  assert_eq(component1.get_state(), ComponentState::Stopped)
  assert_eq(component2.get_state(), ComponentState::Stopped)
  assert_eq(component3.get_state(), ComponentState::Stopped)
  assert_eq(dependent_component.get_state(), ComponentState::Stopped)
  
  // 测试重启
  let restart_result = lifecycle_manager.restart_all()
  assert_true(restart_result.success)
  assert_eq(restart_result.restarted_count, 4)
  assert_eq(restart_result.failed_count, 0)
  
  // 验证组件状态
  assert_eq(component1.get_state(), ComponentState::Running)
  assert_eq(component2.get_state(), ComponentState::Running)
  assert_eq(component3.get_state(), ComponentState::Running)
  assert_eq(dependent_component.get_state(), ComponentState::Running)
  
  // 测试组件健康检查
  let health_check_result = lifecycle_manager.health_check_all()
  assert_true(health_check_result.overall_health)
  assert_eq(health_check_result.healthy_count, 4)
  assert_eq(health_check_result.unhealthy_count, 0)
  
  // 模拟组件故障
  component2.simulate_failure()
  
  let fault_health_check = lifecycle_manager.health_check_all()
  assert_false(fault_health_check.overall_health)
  assert_eq(fault_health_check.healthy_count, 3)
  assert_eq(fault_health_check.unhealthy_count, 1)
  assert_true(fault_health_check.unhealthy_components.contains("component2"))
  
  // 测试故障恢复
  let recovery_result = lifecycle_manager.recover_component("component2")
  assert_true(recovery_result.success)
  
  let recovery_health_check = lifecycle_manager.health_check_all()
  assert_true(recovery_health_check.overall_health)
  assert_eq(recovery_health_check.healthy_count, 4)
  assert_eq(recovery_health_check.unhealthy_count, 0)
  
  // 测试优雅关闭
  let mut shutdown_sequence = []
  lifecycle_manager.set_shutdown_observer(|component_name, state| {
    shutdown_sequence.push((component_name, state))
  })
  
  let shutdown_result = lifecycle_manager.shutdown_all()
  assert_true(shutdown_result.success)
  assert_eq(shutdown_result.shutdown_count, 4)
  
  // 验证关闭顺序（依赖组件应该先关闭）
  assert_eq(shutdown_sequence[0].0, "dependent_component")
  assert_eq(shutdown_sequence[0].1, ComponentState::Shutdown)
  
  // 验证所有组件都已关闭
  assert_eq(component1.get_state(), ComponentState::Shutdown)
  assert_eq(component2.get_state(), ComponentState::Shutdown)
  assert_eq(component3.get_state(), ComponentState::Shutdown)
  assert_eq(dependent_component.get_state(), ComponentState::Shutdown)
  
  // 测试生命周期事件
  let mut lifecycle_events = []
  lifecycle_manager.add_event_observer(|event| {
    lifecycle_events.push(event)
  })
  
  // 重新初始化并启动组件以捕获事件
  lifecycle_manager.initialize_all()
  lifecycle_manager.start_all()
  
  // 验证生命周期事件
  assert_true(lifecycle_events.length() >= 6) // 至少每个组件2个事件（初始化和启动）
  
  // 测试生命周期指标
  let metrics = lifecycle_manager.get_metrics()
  
  assert_eq(metrics.total_components, 4)
  assert_eq(metrics.running_components, 4)
  assert_eq(metrics.stopped_components, 0)
  assert_true(metrics.uptime_ms > 0)
  assert_true(metrics.total_restarts >= 1)
}

// 辅助类和函数
class TestComponent {
  name : String
  state : ComponentState
  
  new(name : String) {
    { name: name, state: ComponentState::Created }
  }
  
  get_state() : ComponentState {
    self.state
  }
  
  initialize() : Bool {
    self.state = ComponentState::Initialized
    true
  }
  
  start() : Bool {
    if self.state == ComponentState::Initialized {
      self.state = ComponentState::Running
      true
    } else {
      false
    }
  }
  
  stop() : Bool {
    if self.state == ComponentState::Running {
      self.state = ComponentState::Stopped
      true
    } else {
      false
    }
  }
  
  shutdown() : Bool {
    self.state = ComponentState::Shutdown
    true
  }
  
  is_healthy() : Bool {
    self.state == ComponentState::Running
  }
  
  simulate_failure() : Unit {
    self.state = ComponentState::Failed
  }
  
  recover() : Bool {
    self.state = ComponentState::Stopped
    self.start()
  }
}

class DependentComponent {
  name : String
  state : ComponentState
  dependencies : Array[String]
  
  new(name : String, dependencies : Array[String]) {
    { name: name, state: ComponentState::Created, dependencies: dependencies }
  }
  
  get_state() : ComponentState {
    self.state
  }
  
  get_dependencies() : Array[String] {
    self.dependencies
  }
  
  initialize() : Bool {
    self.state = ComponentState::Initialized
    true
  }
  
  start() : Bool {
    if self.state == ComponentState::Initialized {
      self.state = ComponentState::Running
      true
    } else {
      false
    }
  }
  
  stop() : Bool {
    if self.state == ComponentState::Running {
      self.state = ComponentState::Stopped
      true
    } else {
      false
    }
  }
  
  shutdown() : Bool {
    self.state = ComponentState::Shutdown
    true
  }
  
  is_healthy() : Bool {
    self.state == ComponentState::Running
  }
}

enum ComponentState {
  Created
  Initialized
  Running
  Stopped
  Failed
  Shutdown
}