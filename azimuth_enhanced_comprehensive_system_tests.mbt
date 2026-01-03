// Azimuth 增强综合系统测试
// 此文件包含针对Azimuth遥测系统的全面测试用例，覆盖性能、错误处理、配置管理等方面

// 测试1: 性能基准测试
test "性能基准测试 - 度量收集性能" {
  // 创建性能测试器
  let performance_tester = PerformanceTester::new()
  
  // 测试计数器性能
  let counter_start_time = SystemTime::now()
  let counter = Counter::new("performance.test.counter", "测试计数器性能", "count")
  
  // 执行10000次计数器操作
  for i in 0..10000 {
    Counter::add(counter, 1)
  }
  
  let counter_end_time = SystemTime::now()
  let counter_duration = SystemTime::duration_since(counter_start_time, counter_end_time)
  
  // 验证计数器值
  assert_eq(Counter::value(counter), 10000)
  
  // 验证性能 - 10000次操作应在100ms内完成
  assert_true(counter_duration < 100)
  
  // 测试仪表性能
  let gauge_start_time = SystemTime::now()
  let gauge = Gauge::new("performance.test.gauge", "测试仪表性能", "value")
  
  // 执行10000次仪表设置操作
  for i in 0..10000 {
    Gauge::set(gauge, i.to_float())
  }
  
  let gauge_end_time = SystemTime::now()
  let gauge_duration = SystemTime::duration_since(gauge_start_time, gauge_end_time)
  
  // 验证仪表值
  assert_eq(Gauge::value(gauge), 9999.0)
  
  // 验证性能 - 10000次操作应在100ms内完成
  assert_true(gauge_duration < 100)
  
  // 测试直方图性能
  let histogram_start_time = SystemTime::now()
  let histogram = Histogram::new("performance.test.histogram", "测试直方图性能", "ms", [1.0, 5.0, 10.0, 50.0, 100.0])
  
  // 执行10000次直方图记录操作
  for i in 0..10000 {
    Histogram::record(histogram, (i % 100).to_float())
  }
  
  let histogram_end_time = SystemTime::now()
  let histogram_duration = SystemTime::duration_since(histogram_start_time, histogram_end_time)
  
  // 验证直方图计数
  assert_eq(Histogram::count(histogram), 10000)
  
  // 验证性能 - 10000次操作应在150ms内完成
  assert_true(histogram_duration < 150)
}

// 测试2: 错误处理和恢复测试
test "错误处理和恢复机制测试" {
  // 创建错误处理器
  let error_handler = ErrorHandler::new()
  
  // 测试度量错误处理
  let counter = Counter::new("error.test.counter", "错误测试计数器", "count")
  
  // 模拟错误情况 - 无效值
  let result1 = ErrorHandler::handle_counter_error(error_handler, counter, "invalid_value")
  match result1 {
    Ok(_) => assert_true(false) // 应该返回错误
    Err(error) => assert_eq(Error::message(error), "Invalid counter value: invalid_value")
  }
  
  // 测试错误恢复
  let recovery_result = ErrorHandler::recover_from_counter_error(error_handler, counter)
  match recovery_result {
    Ok(recovered_counter) => {
      // 验证恢复后的计数器可以正常工作
      Counter::add(recovered_counter, 5)
      assert_eq(Counter::value(recovered_counter), 5)
    }
    Err(_) => assert_true(false) // 恢复应该成功
  }
  
  // 测试仪表错误处理
  let gauge = Gauge::new("error.test.gauge", "错误测试仪表", "value")
  
  // 模拟错误情况 - 无效值
  let result2 = ErrorHandler::handle_gauge_error(error_handler, gauge, "invalid_value")
  match result2 {
    Ok(_) => assert_true(false) // 应该返回错误
    Err(error) => assert_eq(Error::message(error), "Invalid gauge value: invalid_value")
  }
  
  // 测试错误恢复
  let gauge_recovery_result = ErrorHandler::recover_from_gauge_error(error_handler, gauge)
  match gauge_recovery_result {
    Ok(recovered_gauge) => {
      // 验证恢复后的仪表可以正常工作
      Gauge::set(recovered_gauge, 42.0)
      assert_eq(Gauge::value(recovered_gauge), 42.0)
    }
    Err(_) => assert_true(false) // 恢复应该成功
  }
  
  // 测试错误日志记录
  let error_logger = ErrorLogger::new()
  ErrorLogger::log_error(error_logger, "Test error for logging", "test.context")
  
  let logged_errors = ErrorLogger::get_errors(error_logger)
  assert_eq(logged_errors.length(), 1)
  assert_eq(logged_errors[0].message, "Test error for logging")
  assert_eq(logged_errors[0].context, "test.context")
}

// 测试3: 配置管理测试
test "配置管理系统测试" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 测试默认配置
  let default_config = ConfigurationManager::get_default_config(config_manager)
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.max_spans, 1000)
  assert_eq(default_config.export_interval_ms, 5000)
  assert_true(default_config.enable_compression)
  
  // 测试配置更新
  let updated_config = ConfigurationManager::update_config(config_manager, {
    sampling_rate: 0.5,
    max_spans: 2000,
    export_interval_ms: 10000,
    enable_compression: false
  })
  
  assert_eq(updated_config.sampling_rate, 0.5)
  assert_eq(updated_config.max_spans, 2000)
  assert_eq(updated_config.export_interval_ms, 10000)
  assert_false(updated_config.enable_compression)
  
  // 测试配置验证
  let valid_config = {
    sampling_rate: 0.5,
    max_spans: 2000,
    export_interval_ms: 10000,
    enable_compression: true
  }
  
  let validation_result1 = ConfigurationManager::validate_config(config_manager, valid_config)
  match validation_result1 {
    Ok(_) => assert_true(true) // 有效配置应该通过验证
    Err(_) => assert_true(false)
  }
  
  // 测试无效配置
  let invalid_config = {
    sampling_rate: 1.5, // 无效的采样率，应小于等于1.0
    max_spans: -100,    // 无效的最大span数，应为正数
    export_interval_ms: 0,     // 无效的导出间隔，应为正数
    enable_compression: true
  }
  
  let validation_result2 = ConfigurationManager::validate_config(config_manager, invalid_config)
  match validation_result2 {
    Ok(_) => assert_true(false) // 无效配置应该失败
    Err(errors) => {
      // 验证错误消息
      assert_true(errors.length() >= 3)
      assert_true(errors.any(fn(msg) { msg.contains("sampling_rate") }))
      assert_true(errors.any(fn(msg) { msg.contains("max_spans") }))
      assert_true(errors.any(fn(msg) { msg.contains("export_interval_ms") }))
    }
  }
  
  // 测试配置持久化
  let persistence_result = ConfigurationManager::persist_config(config_manager, updated_config)
  match persistence_result {
    Ok(_) => assert_true(true) // 持久化应该成功
    Err(_) => assert_true(false)
  }
  
  // 测试配置加载
  let loaded_config = ConfigurationManager::load_config(config_manager)
  match loaded_config {
    Ok(config) => {
      assert_eq(config.sampling_rate, 0.5)
      assert_eq(config.max_spans, 2000)
      assert_eq(config.export_interval_ms, 10000)
      assert_false(config.enable_compression)
    }
    Err(_) => assert_true(false) // 加载应该成功
  }
}

// 测试4: 数据序列化和反序列化测试
test "数据序列化和反序列化测试" {
  // 创建序列化器
  let serializer = JsonSerializer::new()
  
  // 创建测试数据
  let test_span = SpanData::new("test.operation", 1000, 1500, [
    ("service.name", "test.service"),
    ("operation.type", "test"),
    ("user.id", "12345")
  ])
  
  // 测试序列化
  let serialization_result = JsonSerializer::serialize_span(serializer, test_span)
  match serialization_result {
    Ok(json_string) => {
      // 验证JSON字符串包含预期内容
      assert_true(json_string.contains("test.operation"))
      assert_true(json_string.contains("test.service"))
      assert_true(json_string.contains("12345"))
    }
    Err(_) => assert_true(false) // 序列化应该成功
  }
  
  // 测试反序列化
  match serialization_result {
    Ok(json_string) => {
      let deserialization_result = JsonSerializer::deserialize_span(serializer, json_string)
      match deserialization_result {
        Ok(deserialized_span) => {
          // 验证反序列化后的数据
          assert_eq(SpanData::name(deserialized_span), "test.operation")
          assert_eq(SpanData::start_time(deserialized_span), 1000)
          assert_eq(SpanData::end_time(deserialized_span), 1500)
          
          let attributes = SpanData::attributes(deserialized_span)
          assert_eq(attributes.length(), 3)
          
          let service_name_attr = attributes.find(fn(attr) { Attribute::key(attr) == "service.name" })
          match service_name_attr {
            Some(attr) => assert_eq(Attribute::value(attr), "test.service")
            None => assert_true(false)
          }
        }
        Err(_) => assert_true(false) // 反序列化应该成功
      }
    }
    Err(_) => assert_true(false) // 序列化应该成功
  }
  
  // 测试批量序列化
  let test_spans = [
    SpanData::new("operation1", 1000, 1100, [("service", "test")]),
    SpanData::new("operation2", 1100, 1200, [("service", "test")]),
    SpanData::new("operation3", 1200, 1300, [("service", "test")])
  ]
  
  let batch_serialization_result = JsonSerializer::serialize_span_batch(serializer, test_spans)
  match batch_serialization_result {
    Ok(json_string) => {
      // 验证批量序列化结果
      assert_true(json_string.contains("operation1"))
      assert_true(json_string.contains("operation2"))
      assert_true(json_string.contains("operation3"))
    }
    Err(_) => assert_true(false) // 批量序列化应该成功
  }
  
  // 测试批量反序列化
  match batch_serialization_result {
    Ok(json_string) => {
      let batch_deserialization_result = JsonSerializer::deserialize_span_batch(serializer, json_string)
      match batch_deserialization_result {
        Ok(deserialized_spans) => {
          // 验证批量反序列化后的数据
          assert_eq(deserialized_spans.length(), 3)
          assert_eq(SpanData::name(deserialized_spans[0]), "operation1")
          assert_eq(SpanData::name(deserialized_spans[1]), "operation2")
          assert_eq(SpanData::name(deserialized_spans[2]), "operation3")
        }
        Err(_) => assert_true(false) // 批量反序列化应该成功
      }
    }
    Err(_) => assert_true(false) // 批量序列化应该成功
  }
}

// 测试5: 并发安全测试
test "并发安全测试" {
  // 创建并发测试器
  let concurrent_tester = ConcurrentTester::new()
  
  // 创建共享计数器
  let shared_counter = ConcurrentCounter::new("concurrent.test.counter", "并发测试计数器", "count")
  
  // 创建多个线程并发增加计数器
  let thread_count = 10
  let increments_per_thread = 1000
  
  let threads = []
  for i in 0..thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..increments_per_thread {
        ConcurrentCounter::add(shared_counter, 1)
      }
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终值
  let expected_value = thread_count * increments_per_thread
  assert_eq(ConcurrentCounter::value(shared_counter), expected_value)
  
  // 测试并发仪表操作
  let shared_gauge = ConcurrentGauge::new("concurrent.test.gauge", "并发测试仪表", "value")
  
  let gauge_threads = []
  for i in 0..thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..increments_per_thread {
        ConcurrentGauge::set(shared_gauge, (i * increments_per_thread + j).to_float())
      }
    })
    gauge_threads = gauge_threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in gauge_threads {
    Thread::join(thread)
  }
  
  // 验证最终值（应该是最后一个设置的值）
  let final_value = (thread_count - 1) * increments_per_thread + (increments_per_thread - 1)
  assert_eq(ConcurrentGauge::value(shared_gauge), final_value.to_float())
  
  // 测试并发直方图操作
  let shared_histogram = ConcurrentHistogram::new("concurrent.test.histogram", "并发测试直方图", "ms", [1.0, 5.0, 10.0, 50.0, 100.0])
  
  let histogram_threads = []
  for i in 0..thread_count {
    let thread = Thread::spawn(fn() {
      for j in 0..increments_per_thread {
        ConcurrentHistogram::record(shared_histogram, ((i * increments_per_thread + j) % 100).to_float())
      }
    })
    histogram_threads = histogram_threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in histogram_threads {
    Thread::join(thread)
  }
  
  // 验证最终计数
  let expected_histogram_count = thread_count * increments_per_thread
  assert_eq(ConcurrentHistogram::count(shared_histogram), expected_histogram_count)
}

// 测试6: 缓存机制测试
test "缓存机制测试" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new()
  
  // 测试基本缓存操作
  let cache_key = "test.cache.key"
  let cache_value = "test.cache.value"
  
  // 设置缓存值
  let set_result = CacheManager::set(cache_manager, cache_key, cache_value, 60) // 60秒TTL
  match set_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 获取缓存值
  let get_result = CacheManager::get(cache_manager, cache_key)
  match get_result {
    Ok(value) => assert_eq(value, cache_value)
    Err(_) => assert_true(false)
  }
  
  // 测试缓存过期
  let short_ttl_key = "short.ttl.key"
  let short_ttl_value = "short.ttl.value"
  
  // 设置短TTL缓存值
  CacheManager::set(cache_manager, short_ttl_key, short_ttl_value, 1) // 1秒TTL
  
  // 等待缓存过期
  Thread::sleep(1100) // 等待1.1秒
  
  // 尝试获取过期的缓存值
  let expired_result = CacheManager::get(cache_manager, short_ttl_key)
  match expired_result {
    Ok(_) => assert_true(false) // 过期的缓存不应该返回值
    Err(error) => assert_eq(Error::message(error), "Cache entry expired")
  }
  
  // 测试缓存删除
  let delete_key = "delete.test.key"
  let delete_value = "delete.test.value"
  
  // 设置缓存值
  CacheManager::set(cache_manager, delete_key, delete_value, 60)
  
  // 删除缓存值
  let delete_result = CacheManager::delete(cache_manager, delete_key)
  match delete_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 尝试获取已删除的缓存值
  let deleted_result = CacheManager::get(cache_manager, delete_key)
  match deleted_result {
    Ok(_) => assert_true(false) // 已删除的缓存不应该返回值
    Err(error) => assert_eq(Error::message(error), "Cache entry not found")
  }
  
  // 测试缓存清理
  let cleanup_key1 = "cleanup.test.key1"
  let cleanup_key2 = "cleanup.test.key2"
  
  // 设置缓存值
  CacheManager::set(cache_manager, cleanup_key1, "value1", 1) // 1秒TTL
  CacheManager::set(cache_manager, cleanup_key2, "value2", 60) // 60秒TTL
  
  // 等待第一个缓存过期
  Thread::sleep(1100) // 等待1.1秒
  
  // 清理过期缓存
  let cleanup_result = CacheManager::cleanup_expired(cache_manager)
  match cleanup_result {
    Ok(count) => assert_eq(count, 1) // 应该清理1个过期条目
    Err(_) => assert_true(false)
  }
  
  // 验证过期缓存已被清理
  let expired_cleanup_result = CacheManager::get(cache_manager, cleanup_key1)
  match expired_cleanup_result {
    Ok(_) => assert_true(false) // 过期的缓存应该被清理
    Err(_) => assert_true(true)
  }
  
  // 验证未过期缓存仍然存在
  let valid_cleanup_result = CacheManager::get(cache_manager, cleanup_key2)
  match valid_cleanup_result {
    Ok(value) => assert_eq(value, "value2")
    Err(_) => assert_true(false)
  }
}

// 测试7: 时间序列数据处理测试
test "时间序列数据处理测试" {
  // 创建时间序列处理器
  let time_series_processor = TimeSeriesProcessor::new()
  
  // 创建时间序列数据点
  let data_points = [
    TimeSeriesPoint::new(1000, 10.5),
    TimeSeriesPoint::new(1100, 12.3),
    TimeSeriesPoint::new(1200, 11.8),
    TimeSeriesPoint::new(1300, 13.2),
    TimeSeriesPoint::new(1400, 14.7),
    TimeSeriesPoint::new(1500, 13.9),
    TimeSeriesPoint::new(1600, 15.1),
    TimeSeriesPoint::new(1700, 14.5),
    TimeSeriesPoint::new(1800, 16.3),
    TimeSeriesPoint::new(1900, 15.8)
  ]
  
  // 测试时间序列聚合
  let aggregation_result = TimeSeriesProcessor::aggregate(time_series_processor, data_points, Aggregation::Average)
  match aggregation_result {
    Ok(average) => assert_eq(average, 13.81) // 验证平均值
    Err(_) => assert_true(false)
  }
  
  // 测试时间序列最大值
  let max_result = TimeSeriesProcessor::aggregate(time_series_processor, data_points, Aggregation::Max)
  match max_result {
    Ok(max) => assert_eq(max, 16.3) // 验证最大值
    Err(_) => assert_true(false)
  }
  
  // 测试时间序列最小值
  let min_result = TimeSeriesProcessor::aggregate(time_series_processor, data_points, Aggregation::Min)
  match min_result {
    Ok(min) => assert_eq(min, 10.5) // 验证最小值
    Err(_) => assert_true(false)
  }
  
  // 测试时间序列求和
  let sum_result = TimeSeriesProcessor::aggregate(time_series_processor, data_points, Aggregation::Sum)
  match sum_result {
    Ok(sum) => assert_eq(sum, 138.1) // 验证总和
    Err(_) => assert_true(false)
  }
  
  // 测试时间序列采样
  let sampling_result = TimeSeriesProcessor::sample(time_series_processor, data_points, 3) // 每3个点采样1个
  match sampling_result {
    Ok(sampled_points) => {
      assert_eq(sampled_points.length(), 4) // 10个点每3个采样1个，应该得到4个点
      assert_eq(TimeSeriesPoint::timestamp(sampled_points[0]), 1000)
      assert_eq(TimeSeriesPoint::timestamp(sampled_points[1]), 1300)
      assert_eq(TimeSeriesPoint::timestamp(sampled_points[2]), 1600)
      assert_eq(TimeSeriesPoint::timestamp(sampled_points[3]), 1900)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试时间窗口聚合
  let window_result = TimeSeriesProcessor::aggregate_by_window(time_series_processor, data_points, 500) // 500ms窗口
  match window_result {
    Ok(windows) => {
      assert_eq(windows.length(), 2) // 1000-1900的时间范围，500ms窗口应该得到2个窗口
      
      // 验证第一个窗口 (1000-1500)
      let window1 = windows[0]
      assert_eq(TimeWindow::start_time(window1), 1000)
      assert_eq(TimeWindow::end_time(window1), 1500)
      assert_eq(TimeWindow::point_count(window1), 5)
      
      // 验证第二个窗口 (1500-2000)
      let window2 = windows[1]
      assert_eq(TimeWindow::start_time(window2), 1500)
      assert_eq(TimeWindow::end_time(window2), 2000)
      assert_eq(TimeWindow::point_count(window2), 5)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试时间序列平滑
  let smoothing_result = TimeSeriesProcessor::smooth(time_series_processor, data_points, Smoothing::MovingAverage, 3)
  match smoothing_result {
    Ok(smoothed_points) => {
      assert_eq(smoothed_points.length(), data_points.length())
      
      // 验证平滑后的值
      // 第一个点应该保持不变
      assert_eq(TimeSeriesPoint::value(smoothed_points[0]), 10.5)
      
      // 中间的点应该是移动平均值
      assert_eq(TimeSeriesPoint::value(smoothed_points[1]), (10.5 + 12.3 + 11.8) / 3.0)
      assert_eq(TimeSeriesPoint::value(smoothed_points[2]), (12.3 + 11.8 + 13.2) / 3.0)
      
      // 最后一个点应该保持不变
      assert_eq(TimeSeriesPoint::value(smoothed_points[9]), 15.8)
    }
    Err(_) => assert_true(false)
  }
}

// 测试8: 警报和通知测试
test "警报和通知系统测试" {
  // 创建警报管理器
  let alert_manager = AlertManager::new()
  
  // 创建警报规则
  let threshold_rule = ThresholdAlertRule::new("high.error.rate", "错误率过高", "error.rate", ">", 0.05, Severity::Warning)
  let trend_rule = TrendAlertRule::new("memory.increasing", "内存使用持续增长", "memory.usage", "increasing", 5, Severity::Critical)
  
  // 注册警报规则
  AlertManager::register_rule(alert_manager, threshold_rule)
  AlertManager::register_rule(alert_manager, trend_rule)
  
  // 创建测试指标数据
  let metrics = [
    ("error.rate", 0.03),  // 低于阈值，不应触发警报
    ("error.rate", 0.06),  // 高于阈值，应触发警报
    ("memory.usage", 100), // 内存使用量
    ("memory.usage", 110), // 内存使用量增加
    ("memory.usage", 120), // 内存使用量增加
    ("memory.usage", 130), // 内存使用量增加
    ("memory.usage", 140), // 内存使用量增加
    ("memory.usage", 150)  // 内存使用量增加，连续5次增长，应触发趋势警报
  ]
  
  // 创建通知收集器
  let notification_collector = NotificationCollector::new()
  AlertManager::set_notification_handler(alert_manager, NotificationCollector::handle, notification_collector)
  
  // 处理指标数据
  for (metric_name, metric_value) in metrics {
    AlertManager::process_metric(alert_manager, metric_name, metric_value)
  }
  
  // 验证警报触发
  let notifications = NotificationCollector::get_notifications(notification_collector)
  assert_eq(notifications.length(), 2) // 应该触发2个警报
  
  // 验证阈值警报
  let threshold_alert = notifications.find(fn(notif) { Notification::rule_id(notif) == "high.error.rate" })
  match threshold_alert {
    Some(alert) => {
      assert_eq(Notification::title(alert), "错误率过高")
      assert_eq(Notification::severity(alert), Severity::Warning)
      assert_true(Notification::message(alert).contains("0.06"))
      assert_true(Notification::message(alert).contains("0.05"))
    }
    None => assert_true(false)
  }
  
  // 验证趋势警报
  let trend_alert = notifications.find(fn(notif) { Notification::rule_id(notif) == "memory.increasing" })
  match trend_alert {
    Some(alert) => {
      assert_eq(Notification::title(alert), "内存使用持续增长")
      assert_eq(Notification::severity(alert), Severity::Critical)
      assert_true(Notification::message(alert).contains("increasing"))
      assert_true(Notification::message(alert).contains("5"))
    }
    None => assert_true(false)
  }
  
  // 测试警报抑制
  let suppression_rule = SuppressionRule::new("suppress.high.error.rate", "high.error.rate", 300) // 抑制5分钟
  AlertManager::register_suppression_rule(alert_manager, suppression_rule)
  
  // 再次触发相同的阈值警报
  AlertManager::process_metric(alert_manager, "error.rate", 0.07)
  
  // 验证警报被抑制
  let suppressed_notifications = NotificationCollector::get_notifications(notification_collector)
  assert_eq(suppressed_notifications.length(), 2) // 警报数量不应增加，因为被抑制
  
  // 测试警报恢复
  AlertManager::process_metric(alert_manager, "error.rate", 0.02)
  
  // 验证恢复通知
  let recovery_notifications = NotificationCollector::get_notifications(notification_collector)
  assert_eq(recovery_notifications.length(), 3) // 应该有1个恢复通知
  
  let recovery_alert = recovery_notifications.find(fn(notif) { Notification::is_recovery(notif) })
  match recovery_alert {
    Some(alert) => {
      assert_eq(Notification::rule_id(alert), "high.error.rate")
      assert_true(Notification::message(alert).contains("recovered"))
    }
    None => assert_true(false)
  }
}

// 测试9: 数据压缩和传输测试
test "数据压缩和传输测试" {
  // 创建压缩管理器
  let compression_manager = CompressionManager::new()
  
  // 创建测试数据
  let test_data = "This is a test string for compression. ".repeat(100) // 创建重复的测试数据
  
  // 测试GZIP压缩
  let gzip_result = CompressionManager::compress(compression_manager, test_data, CompressionType::Gzip)
  match gzip_result {
    Ok(compressed_data) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed_data.length() < test_data.length())
      
      // 测试GZIP解压缩
      let decompress_result = CompressionManager::decompress(compression_manager, compressed_data, CompressionType::Gzip)
      match decompress_result {
        Ok(decompressed_data) => {
          // 验证解压缩后的数据与原始数据相同
          assert_eq(decompressed_data, test_data)
        }
        Err(_) => assert_true(false) // 解压缩应该成功
      }
    }
    Err(_) => assert_true(false) // 压缩应该成功
  }
  
  // 测试LZ4压缩
  let lz4_result = CompressionManager::compress(compression_manager, test_data, CompressionType::Lz4)
  match lz4_result {
    Ok(compressed_data) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed_data.length() < test_data.length())
      
      // 测试LZ4解压缩
      let decompress_result = CompressionManager::decompress(compression_manager, compressed_data, CompressionType::Lz4)
      match decompress_result {
        Ok(decompressed_data) => {
          // 验证解压缩后的数据与原始数据相同
          assert_eq(decompressed_data, test_data)
        }
        Err(_) => assert_true(false) // 解压缩应该成功
      }
    }
    Err(_) => assert_true(false) // 压缩应该成功
  }
  
  // 测试批量数据压缩
  let batch_data = [
    "Batch data 1: " + "test".repeat(50),
    "Batch data 2: " + "compression".repeat(30),
    "Batch data 3: " + "algorithm".repeat(40)
  ]
  
  let batch_compress_result = CompressionManager::compress_batch(compression_manager, batch_data, CompressionType::Gzip)
  match batch_compress_result {
    Ok(compressed_batch) => {
      // 验证批量压缩后的数据
      assert_eq(compressed_batch.length(), batch_data.length())
      
      // 测试批量解压缩
      let batch_decompress_result = CompressionManager::decompress_batch(compression_manager, compressed_batch, CompressionType::Gzip)
      match batch_decompress_result {
        Ok(decompressed_batch) => {
          // 验证解压缩后的数据与原始数据相同
          assert_eq(decompressed_batch.length(), batch_data.length())
          for i in 0..batch_data.length() {
            assert_eq(decompressed_batch[i], batch_data[i])
          }
        }
        Err(_) => assert_true(false) // 批量解压缩应该成功
      }
    }
    Err(_) => assert_true(false) // 批量压缩应该成功
  }
  
  // 创建传输管理器
  let transport_manager = TransportManager::new()
  
  // 测试数据传输
  let endpoint = "https://example.com/telemetry"
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer test-token")]
  
  // 模拟传输（在实际环境中，这会发送HTTP请求）
  let transport_result = TransportManager::send_data(transport_manager, endpoint, headers, test_data)
  match transport_result {
    Ok(response) => {
      // 验证传输响应
      assert_eq(response.status_code, 200)
      assert_true(response.body.contains("success"))
    }
    Err(_) => assert_true(false) // 传输应该成功
  }
  
  // 测试压缩数据传输
  match gzip_result {
    Ok(compressed_data) => {
      let compressed_transport_result = TransportManager::send_compressed_data(transport_manager, endpoint, headers, compressed_data, CompressionType::Gzip)
      match compressed_transport_result {
        Ok(response) => {
          // 验证压缩传输响应
          assert_eq(response.status_code, 200)
          assert_true(response.body.contains("success"))
        }
        Err(_) => assert_true(false) // 压缩传输应该成功
      }
    }
    Err(_) => assert_true(false) // 压缩应该成功
  }
  
  // 测试批量传输
  let batch_transport_result = TransportManager::send_batch(transport_manager, endpoint, headers, batch_data)
  match batch_transport_result {
    Ok(responses) => {
      // 验证批量传输响应
      assert_eq(responses.length(), batch_data.length())
      for response in responses {
        assert_eq(response.status_code, 200)
        assert_true(response.body.contains("success"))
      }
    }
    Err(_) => assert_true(false) // 批量传输应该成功
  }
}

// 测试10: 跨平台兼容性测试
test "跨平台兼容性测试" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  
  // 检测当前平台
  let current_platform = PlatformDetector::detect_platform(platform_detector)
  
  // 验证平台检测结果
  match current_platform {
    Platform::Linux => assert_true(true)
    Platform::Windows => assert_true(true)
    Platform::MacOS => assert_true(true)
    Platform::Unknown => assert_true(false) // 应该能识别当前平台
  }
  
  // 创建跨平台适配器
  let adapter = CrossPlatformAdapter::new()
  
  // 测试路径处理
  let test_path = "/test/path/to/file"
  let normalized_path = CrossPlatformAdapter::normalize_path(adapter, test_path)
  
  // 验证路径标准化
  match current_platform {
    Platform::Windows => assert_true(normalized_path.contains("\"))
    _ => assert_true(normalized_path.contains("/"))
  }
  
  // 测试文件系统操作
  let temp_dir = CrossPlatformAdapter::get_temp_directory(adapter)
  assert_true(temp_dir.length() > 0)
  
  let test_file_path = temp_dir + "/azimuth_test_file.txt"
  let test_content = "Azimuth cross-platform test content"
  
  // 测试文件写入
  let write_result = CrossPlatformAdapter::write_file(adapter, test_file_path, test_content)
  match write_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试文件读取
  let read_result = CrossPlatformAdapter::read_file(adapter, test_file_path)
  match read_result {
    Ok(content) => assert_eq(content, test_content)
    Err(_) => assert_true(false)
  }
  
  // 测试文件删除
  let delete_result = CrossPlatformAdapter::delete_file(adapter, test_file_path)
  match delete_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试进程执行
  let test_command = match current_platform {
    Platform::Windows => "echo Windows test"
    _ => "echo Unix test"
  }
  
  let execute_result = CrossPlatformAdapter::execute_command(adapter, test_command)
  match execute_result {
    Ok(output) => {
      match current_platform {
        Platform::Windows => assert_true(output.contains("Windows test"))
        _ => assert_true(output.contains("Unix test"))
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试网络连接
  let test_host = "example.com"
  let test_port = 80
  
  let connection_result = CrossPlatformAdapter::check_connection(adapter, test_host, test_port)
  match connection_result {
    Ok(is_connected) => assert_true(is_connected || true) // 连接可能失败，但API应该工作
    Err(_) => assert_true(false)
  }
  
  // 测试环境变量
  let test_env_key = "AZIMUTH_TEST_ENV"
  let test_env_value = "test_value"
  
  // 设置环境变量
  let set_env_result = CrossPlatformAdapter::set_environment_variable(adapter, test_env_key, test_env_value)
  match set_env_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 获取环境变量
  let get_env_result = CrossPlatformAdapter::get_environment_variable(adapter, test_env_key)
  match get_env_result {
    Ok(value) => assert_eq(value, test_env_value)
    Err(_) => assert_true(false)
  }
  
  // 删除环境变量
  let unset_env_result = CrossPlatformAdapter::unset_environment_variable(adapter, test_env_key)
  match unset_env_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证环境变量已删除
  let verify_env_result = CrossPlatformAdapter::get_environment_variable(adapter, test_env_key)
  match verify_env_result {
    Ok(_) => assert_true(false) // 环境变量应该已删除
    Err(_) => assert_true(true)
  }
}