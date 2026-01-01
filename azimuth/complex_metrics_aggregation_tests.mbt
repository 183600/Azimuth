// 复杂度量聚合场景测试
// 测试各种度量聚合算法、时间窗口聚合和多维度聚合

pub test "基础度量聚合测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建基础度量聚合器
  let counter_aggregator = azimuth::AggregationManager::create_counter_aggregator(aggregation_manager)
  let histogram_aggregator = azimuth::AggregationManager::create_histogram_aggregator(aggregation_manager)
  let gauge_aggregator = azimuth::AggregationManager::create_gauge_aggregator(aggregation_manager)
  
  // 测试计数器聚合
  for i in 0..100 {
    azimuth::CounterAggregator::add(counter_aggregator, 1.0)
  }
  
  let counter_result = azimuth::CounterAggregator::get_aggregated_value(counter_aggregator)
  assert_eq(counter_result, 100.0)
  
  // 测试直方图聚合
  let histogram_values = [10.5, 20.3, 15.7, 30.1, 25.8, 18.2, 22.6, 12.9, 28.4, 19.7]
  
  for value in histogram_values {
    azimuth::HistogramAggregator::observe(histogram_aggregator, value)
  }
  
  let histogram_stats = azimuth::HistogramAggregator::get_statistics(histogram_aggregator)
  assert_eq(histogram_stats.count, 10)
  assert_eq(histogram_stats.sum, 203.2)
  assert_true(histogram_stats.min >= 10.0 && histogram_stats.min <= 11.0)
  assert_true(histogram_stats.max >= 30.0 && histogram_stats.max <= 31.0)
  assert_true(histogram_stats.average >= 20.0 && histogram_stats.average <= 21.0)
  
  // 测试仪表聚合
  for i in 0..50 {
    let gauge_value = 100.0 + (i.to_double() * 0.5)
    azimuth::GaugeAggregator::set(gauge_aggregator, gauge_value)
  }
  
  let gauge_result = azimuth::GaugeAggregator::get_current_value(gauge_aggregator)
  assert_true(gauge_result >= 124.0 && gauge_result <= 125.0)
  
  // 测试时间窗口聚合
  let windowed_aggregator = azimuth::AggregationManager::create_time_window_aggregator(
    aggregation_manager, 
    60000  // 1分钟窗口
  )
  
  // 在时间窗口内添加数据点
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..60 {
    let timestamp = base_time + (i.to_long() * 1000000000L)  // 每秒一个数据点
    let value = 10.0 + (i.to_double() * 0.1)
    azimuth::TimeWindowAggregator::add_data_point(windowed_aggregator, value, timestamp)
  }
  
  let window_stats = azimuth::TimeWindowAggregator::get_window_statistics(windowed_aggregator)
  assert_eq(window_stats.count, 60)
  assert_true(window_stats.sum >= 600.0 && window_stats.sum <= 700.0)
}

pub test "多维度度量聚合测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建多维度聚合器
  let multi_dim_aggregator = azimuth::AggregationManager::create_multi_dimensional_aggregator(aggregation_manager)
  
  // 定义维度
  let dimensions = ["service.name", "operation.name", "http.method", "http.status_code"]
  
  // 添加多维度数据
  let test_data = [
    { service: "user-service", operation: "login", method: "POST", status: "200", value: 150.5 },
    { service: "user-service", operation: "login", method: "POST", status: "401", value: 25.3 },
    { service: "user-service", operation: "register", method: "POST", status: "201", value: 200.7 },
    { service: "user-service", operation: "register", method: "POST", status: "400", value: 30.1 },
    { service: "order-service", operation: "create", method: "POST", status: "201", value: 350.2 },
    { service: "order-service", operation: "create", method: "POST", status: "500", value: 45.8 },
    { service: "order-service", operation: "list", method: "GET", status: "200", value: 80.4 },
    { service: "order-service", operation: "list", method: "GET", status: "404", value: 15.6 },
    { service: "payment-service", operation: "process", method: "POST", status: "200", value: 500.9 },
    { service: "payment-service", operation: "process", method: "POST", status: "402", value: 60.2 }
  ]
  
  for data in test_data {
    let attributes = azimuth::Attributes::new()
    azimuth::Attributes::set(attributes, "service.name", azimuth::StringValue(data.service))
    azimuth::Attributes::set(attributes, "operation.name", azimuth::StringValue(data.operation))
    azimuth::Attributes::set(attributes, "http.method", azimuth::StringValue(data.method))
    azimuth::Attributes::set(attributes, "http.status_code", azimuth::StringValue(data.status))
    
    azimuth::MultiDimensionalAggregator::add_observation(multi_dim_aggregator, data.value, attributes)
  }
  
  // 测试按服务维度聚合
  let service_aggregation = azimuth::MultiDimensionalAggregator::aggregate_by_dimension(
    multi_dim_aggregator, 
    "service.name"
  )
  
  assert_true(service_aggregation.length() >= 3)
  
  let user_service_stats = service_aggregation.find(fn(stats) { stats.dimension_value == "user-service" })
  assert_true(user_service_stats.total_count >= 4)
  assert_true(user_service_stats.total_sum >= 400.0)
  
  let order_service_stats = service_aggregation.find(fn(stats) { stats.dimension_value == "order-service" })
  assert_true(order_service_stats.total_count >= 4)
  assert_true(order_service_stats.total_sum >= 450.0)
  
  let payment_service_stats = service_aggregation.find(fn(stats) { stats.dimension_value == "payment-service" })
  assert_true(payment_service_stats.total_count >= 2)
  assert_true(payment_service_stats.total_sum >= 550.0)
  
  // 测试多维度组合聚合
  let multi_dim_aggregation = azimuth::MultiDimensionalAggregator::aggregate_by_dimensions(
    multi_dim_aggregator, 
    ["service.name", "http.status_code"]
  )
  
  assert_true(multi_dim_aggregation.length() >= 8)
  
  let user_success_stats = multi_dim_aggregation.find(fn(stats) { 
    stats.dimension_values["service.name"] == "user-service" && 
    stats.dimension_values["http.status_code"] == "200" 
  })
  assert_true(user_success_stats.total_count >= 1)
  assert_true(user_success_stats.total_sum >= 150.0)
  
  // 测试维度下钻分析
  let drilldown_stats = azimuth::MultiDimensionalAggregator::drill_down(
    multi_dim_aggregator, 
    "user-service", 
    ["operation.name", "http.status_code"]
  )
  
  assert_true(drilldown_stats.length() >= 4)
  
  let login_success_stats = drilldown_stats.find(fn(stats) { 
    stats.dimension_values["operation.name"] == "login" && 
    stats.dimension_values["http.status_code"] == "200" 
  })
  assert_true(login_success_stats.total_count >= 1)
  assert_eq(login_success_stats.total_sum, 150.5)
}

pub test "时间序列聚合测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建时间序列聚合器
  let time_series_aggregator = azimuth::AggregationManager::create_time_series_aggregator(aggregation_manager)
  
  // 生成时间序列数据
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let time_series_data = []
  
  for i in 0..1440 {  // 24小时，每分钟一个数据点
    let timestamp = base_time + (i.to_long() * 60000000000L)  // 每分钟
    let value = 100.0 + 50.0 * (i.to_double() / 1440.0) + 10.0 * (i.to_double() % 60.0) / 60.0
    time_series_data.push({ timestamp, value })
  }
  
  // 添加时间序列数据
  for data_point in time_series_data {
    azimuth::TimeSeriesAggregator::add_data_point(time_series_aggregator, data_point.value, data_point.timestamp)
  }
  
  // 测试不同时间窗口的聚合
  let minute_window = azimuth::TimeSeriesAggregator::aggregate_by_window(time_series_aggregator, 60000000000L)   // 1分钟
  let hour_window = azimuth::TimeSeriesAggregator::aggregate_by_window(time_series_aggregator, 3600000000000L)  // 1小时
  let day_window = azimuth::TimeSeriesAggregator::aggregate_by_window(time_series_aggregator, 86400000000000L)  // 1天
  
  assert_eq(minute_window.length(), 1440)
  assert_eq(hour_window.length(), 24)
  assert_eq(day_window.length(), 1)
  
  // 验证小时窗口聚合结果
  let first_hour_stats = hour_window[0]
  assert_eq(first_hour_stats.count, 60)
  assert_true(first_hour_stats.average >= 100.0 && first_hour_stats.average <= 110.0)
  
  let last_hour_stats = hour_window[23]
  assert_eq(last_hour_stats.count, 60)
  assert_true(last_hour_stats.average >= 140.0 && last_hour_stats.average <= 160.0)
  
  // 测试移动平均聚合
  let moving_avg_5min = azimuth::TimeSeriesAggregator::moving_average(time_series_aggregator, 5)
  let moving_avg_30min = azimuth::TimeSeriesAggregator::moving_average(time_series_aggregator, 30)
  
  assert_eq(moving_avg_5min.length(), 1436)  // 1440 - 5 + 1
  assert_eq(moving_avg_30min.length(), 1411)  // 1440 - 30 + 1
  
  // 验证移动平均的平滑效果
  let original_variance = azimuth::TimeSeriesAggregator::calculate_variance(time_series_aggregator)
  let smoothed_variance_5min = azimuth::TimeSeriesAggregator::calculate_variance_of_series(moving_avg_5min)
  let smoothed_variance_30min = azimuth::TimeSeriesAggregator::calculate_variance_of_series(moving_avg_30min)
  
  assert_true(smoothed_variance_5min < original_variance)
  assert_true(smoothed_variance_30min < smoothed_variance_5min)
  
  // 测试趋势分析
  let trend_analysis = azimuth::TimeSeriesAggregator::analyze_trend(time_series_aggregator)
  assert_true(trend_analysis.slope > 0)  # 应该是上升趋势
  assert_true(trend_analysis.correlation > 0.8)  # 强正相关
  
  // 测试季节性检测
  let seasonality = azimuth::TimeSeriesAggregator::detect_seasonality(time_series_aggregator, 60)  # 60分钟周期
  assert_true(seasonality.detected)
  assert_true(seasonality.strength > 0.1)
}

pub test "百分位数和分位数聚合测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建百分位数聚合器
  let percentile_aggregator = azimuth::AggregationManager::create_percentile_aggregator(aggregation_manager)
  
  // 生成正态分布的测试数据
  let normal_distribution_data = []
  
  for i in 0..10000 {
    // 使用Box-Muller变换生成正态分布随机数
    let u1 = (i + 1).to_double() / 10001.0
    let u2 = ((i * 7) % 10000 + 1).to_double() / 10001.0
    let z0 = (-2.0 * u1.ln()).sqrt() * (2.0 * 3.14159265359 * u2).cos()
    let value = 100.0 + 15.0 * z0  # 均值100，标准差15
    normal_distribution_data.push(value)
  }
  
  // 添加数据到聚合器
  for value in normal_distribution_data {
    azimuth::PercentileAggregator::observe(percentile_aggregator, value)
  }
  
  // 测试常用百分位数
  let p50 = azimuth::PercentileAggregator::get_percentile(percentile_aggregator, 0.5)
  let p90 = azimuth::PercentileAggregator::get_percentile(percentile_aggregator, 0.9)
  let p95 = azimuth::PercentileAggregator::get_percentile(percentile_aggregator, 0.95)
  let p99 = azimuth::PercentileAggregator::get_percentile(percentile_aggregator, 0.99)
  let p999 = azimuth::PercentileAggregator::get_percentile(percentile_aggregator, 0.999)
  
  // 验证百分位数的合理性（正态分布特性）
  assert_true(p50 >= 95.0 && p50 <= 105.0)    # 接近均值
  assert_true(p90 >= 115.0 && p90 <= 125.0)   # 均值 + 1.28σ
  assert_true(p95 >= 120.0 && p95 <= 130.0)   # 均值 + 1.64σ
  assert_true(p99 >= 130.0 && p99 <= 140.0)   # 均值 + 2.33σ
  assert_true(p999 >= 145.0 && p999 <= 155.0) # 均值 + 3.09σ
  
  // 测试多百分位数同时计算
  let percentiles = [0.25, 0.5, 0.75, 0.9, 0.95, 0.99]
  let percentile_results = azimuth::PercentileAggregator::get_multiple_percentiles(
    percentile_aggregator, 
    percentiles
  )
  
  assert_eq(percentile_results.length(), 6)
  assert_true(percentile_results[0].value <= percentile_results[1].value)  # P25 <= P50
  assert_true(percentile_results[1].value <= percentile_results[2].value)  # P50 <= P75
  assert_true(percentile_results[2].value <= percentile_results[3].value)  # P75 <= P90
  
  // 测试直方图桶统计
  let histogram_buckets = azimuth::PercentileAggregator::get_histogram_buckets(percentile_aggregator)
  assert_true(histogram_buckets.length() >= 10)
  
  let total_count = histogram_buckets.reduce(0, fn(acc, bucket) { acc + bucket.count })
  assert_eq(total_count, 10000)
  
  // 测试滑动窗口百分位数
  let sliding_percentile_aggregator = azimuth::AggregationManager::create_sliding_percentile_aggregator(
    aggregation_manager, 
    1000  # 窗口大小1000
  )
  
  // 添加数据并测试滑动窗口
  for i in 0..2000 {
    let value = 50.0 + (i.to_double() % 100.0)
    azimuth::SlidingPercentileAggregator::observe(sliding_percentile_aggregator, value)
    
    if i >= 1000 {
      let sliding_p95 = azimuth::SlidingPercentileAggregator::get_percentile(sliding_percentile_aggregator, 0.95)
      assert_true(sliding_p95 >= 140.0 && sliding_p95 <= 150.0)
    }
  }
  
  // 测试TDigest算法（高效的百分位数估算）
  let tdigest_aggregator = azimuth::AggregationManager::create_tdigest_aggregator(aggregation_manager, 100)
  
  for value in normal_distribution_data {
    azimuth::TDigestAggregator::observe(tdigest_aggregator, value)
  }
  
  let tdigest_p95 = azimuth::TDigestAggregator::get_percentile(tdigest_aggregator, 0.95)
  let exact_p95 = p95
  
  # TDigest应该提供合理的近似
  let relative_error = (tdigest_p95 - exact_p95).abs() / exact_p95
  assert_true(relative_error < 0.05)  # 误差小于5%
  
  // 测试内存效率
  let exact_memory = azimuth::PercentileAggregator::get_memory_usage(percentile_aggregator)
  let tdigest_memory = azimuth::TDigestAggregator::get_memory_usage(tdigest_aggregator)
  
  assert_true(tdigest_memory < exact_memory / 2)  # TDigest应该使用更少内存
}

pub test "聚合性能优化测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建高性能聚合器
  let high_perf_aggregator = azimuth::AggregationManager::create_high_performance_aggregator(aggregation_manager)
  
  // 性能测试参数
  let performance_test_size = 100000
  let batch_sizes = [10, 100, 1000, 10000]
  
  // 测试不同批量大小的聚合性能
  let batch_performance_results = []
  
  for batch_size in batch_sizes {
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    for batch_start in range(0, performance_test_size, batch_size) {
      let batch_end = min(batch_start + batch_size, performance_test_size)
      let batch_data = []
      
      for i in range(batch_start, batch_end) {
        batch_data.push(100.0 + (i.to_double() % 50.0))
      }
      
      azimuth::HighPerformanceAggregator::add_batch(high_perf_aggregator, batch_data)
    }
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let duration = end_time - start_time
    
    batch_performance_results.push({ batch_size, duration })
  }
  
  // 验证批量处理的优势
  let single_op_time = batch_performance_results[0].duration / performance_test_size.to_long()
  let best_batch_time = batch_performance_results[batch_performance_results.length() - 1].duration / performance_test_size.to_long()
  
  assert_true(best_batch_time < single_op_time)
  
  // 测试流式聚合性能
  let streaming_aggregator = azimuth::AggregationManager::create_streaming_aggregator(aggregation_manager)
  
  let stream_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..performance_test_size {
    let value = 50.0 + (i.to_double() % 100.0)
    azimuth::StreamingAggregator::observe(streaming_aggregator, value)
  }
  
  let stream_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let stream_duration = stream_end_time - stream_start_time
  
  // 流式聚合应该在合理时间内完成
  assert_true(stream_duration < 5000000000L)  # 小于5秒
  
  // 测试增量聚合性能
  let incremental_aggregator = azimuth::AggregationManager::create_incremental_aggregator(aggregation_manager)
  
  let incremental_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..performance_test_size {
    let value = 75.0 + (i.to_double() % 25.0)
    azimuth::IncrementalAggregator::observe(incremental_aggregator, value)
    
    // 每1000次操作获取一次聚合结果
    if i % 1000 == 0 {
      let stats = azimuth::IncrementalAggregator::get_current_statistics(incremental_aggregator)
      assert_true(stats.count > 0)
    }
  }
  
  let incremental_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let incremental_duration = incremental_end_time - incremental_start_time
  
  // 增量聚合性能应该合理
  assert_true(incremental_duration < 10000000000L)  # 小于10秒
  
  // 测试内存优化聚合
  let memory_optimized_aggregator = azimuth::AggregationManager::create_memory_optimized_aggregator(aggregation_manager)
  
  // 设置内存限制
  azimuth::MemoryOptimizedAggregator::set_memory_limit(memory_optimized_aggregator, 10 * 1024 * 1024)  # 10MB
  
  let memory_test_count = 50000
  for i in 0..memory_test_count {
    let value = 200.0 + (i.to_double() % 10.0)
    azimuth::MemoryOptimizedAggregator::observe(memory_optimized_aggregator, value)
  }
  
  let memory_usage = azimuth::MemoryOptimizedAggregator::get_memory_usage(memory_optimized_aggregator)
  assert_true(memory_usage <= 10 * 1024 * 1024)  # 不应超过内存限制
  
  // 验证聚合结果的准确性
  let memory_stats = azimuth::MemoryOptimizedAggregator::get_statistics(memory_optimized_aggregator)
  assert_eq(memory_stats.count, memory_test_count)
  assert_true(memory_stats.average >= 200.0 && memory_stats.average <= 210.0)
  
  // 测试并发聚合性能
  let concurrent_aggregator = azimuth::AggregationManager::create_concurrent_aggregator(aggregation_manager)
  let concurrent_threads = 8
  let operations_per_thread = 12500  # 总共100000次操作
  
  let concurrent_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 模拟并发聚合
  for thread_id in 0..concurrent_threads {
    for op_id in 0..operations_per_thread {
      let value = 300.0 + (thread_id.to_double() * 10.0) + (op_id.to_double() % 20.0)
      azimuth::ConcurrentAggregator::observe(concurrent_aggregator, value)
    }
  }
  
  let concurrent_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let concurrent_duration = concurrent_end_time - concurrent_start_time
  
  // 并发聚合应该提高性能
  let total_concurrent_ops = concurrent_threads * operations_per_thread
  let avg_concurrent_time = concurrent_duration / total_concurrent_ops.to_long()
  
  assert_true(avg_concurrent_time < 100000)  # 平均每次操作小于100微秒
  
  // 验证并发聚合的正确性
  let concurrent_stats = azimuth::ConcurrentAggregator::get_statistics(concurrent_aggregator)
  assert_eq(concurrent_stats.count, total_concurrent_ops)
  assert_true(concurrent_stats.average >= 300.0 && concurrent_stats.average <= 400.0)
}

pub test "聚合数据持久化和恢复测试" {
  let aggregation_manager = azimuth::MetricAggregationManager::new()
  
  // 创建支持持久化的聚合器
  let persistent_aggregator = azimuth::AggregationManager::create_persistent_aggregator(aggregation_manager)
  
  // 添加测试数据
  let test_data_points = []
  
  for i in 0..1000 {
    let timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i.to_long() * 1000000L)
    let value = 50.0 + (i.to_double() % 100.0)
    test_data_points.push({ timestamp, value })
  }
  
  for data_point in test_data_points {
    azimuth::PersistentAggregator::add_data_point(persistent_aggregator, data_point.value, data_point.timestamp)
  }
  
  // 获取聚合状态
  let original_stats = azimuth::PersistentAggregator::get_statistics(persistent_aggregator)
  assert_eq(original_stats.count, 1000)
  assert_true(original_stats.average >= 90.0 && original_stats.average <= 110.0)
  
  // 序列化聚合状态
  let serialized_state = azimuth::PersistentAggregator::serialize_state(persistent_aggregator)
  assert_true(serialized_state.length() > 0)
  
  // 创建新的聚合器并恢复状态
  let restored_aggregator = azimuth::AggregationManager::create_persistent_aggregator(aggregation_manager)
  azimuth::PersistentAggregator::deserialize_state(restored_aggregator, serialized_state)
  
  // 验证恢复后的状态
  let restored_stats = azimuth::PersistentAggregator::get_statistics(restored_aggregator)
  assert_eq(restored_stats.count, original_stats.count)
  assert_true((restored_stats.average - original_stats.average).abs() < 0.001)
  assert_true((restored_stats.sum - original_stats.sum).abs() < 0.001)
  
  // 测试增量持久化
  let incremental_aggregator = azimuth::AggregationManager::create_incremental_persistent_aggregator(aggregation_manager)
  
  // 添加初始数据
  for i in 0..500 {
    let value = 100.0 + (i.to_double() % 50.0)
    azimuth::IncrementalPersistentAggregator::observe(incremental_aggregator, value)
  }
  
  // 第一次持久化
  let checkpoint_1 = azimuth::IncrementalPersistentAggregator::create_checkpoint(incremental_aggregator)
  
  // 添加更多数据
  for i in 500..1000 {
    let value = 100.0 + (i.to_double() % 50.0)
    azimuth::IncrementalPersistentAggregator::observe(incremental_aggregator, value)
  }
  
  // 第二次持久化
  let checkpoint_2 = azimuth::IncrementalPersistentAggregator::create_checkpoint(incremental_aggregator)
  
  // 验证增量检查点
  assert_true(checkpoint_2.size > checkpoint_1.size)
  assert_true(checkpoint_2.incremental_data_count > checkpoint_1.incremental_data_count)
  
  // 从检查点恢复
  let restored_incremental = azimuth::AggregationManager::create_incremental_persistent_aggregator(aggregation_manager)
  azimuth::IncrementalPersistentAggregator::restore_from_checkpoint(restored_incremental, checkpoint_2)
  
  let restored_incremental_stats = azimuth::IncrementalPersistentAggregator::get_statistics(restored_incremental)
  assert_eq(restored_incremental_stats.count, 1000)
  
  // 测试压缩持久化
  let compressed_aggregator = azimuth::AggregationManager::create_compressed_persistent_aggregator(aggregation_manager)
  
  for i in 0..2000 {
    let value = 75.0 + (i.to_double() % 25.0)
    azimuth::CompressedPersistentAggregator::observe(compressed_aggregator, value)
  }
  
  let uncompressed_state = azimuth::PersistentAggregator::serialize_state(compressed_aggregator)
  let compressed_state = azimuth::CompressedPersistentAggregator::serialize_compressed(compressed_aggregator)
  
  // 验证压缩效果
  assert_true(compressed_state.length < uncompressed_state.length)
  
  // 从压缩状态恢复
  let decompressed_aggregator = azimuth::AggregationManager::create_compressed_persistent_aggregator(aggregation_manager)
  azimuth::CompressedPersistentAggregator::deserialize_compressed(decompressed_aggregator, compressed_state)
  
  let decompressed_stats = azimuth::CompressedPersistentAggregator::get_statistics(decompressed_aggregator)
  assert_eq(decompressed_stats.count, 2000)
  
  // 测试分布式聚合状态同步
  let distributed_aggregator = azimuth::AggregationManager::create_distributed_aggregator(aggregation_manager)
  
  // 模拟多个节点的聚合数据
  let node_data = [
    { node_id: "node-1", data_points: [10.5, 15.3, 20.7, 25.1, 30.8] },
    { node_id: "node-2", data_points: [12.2, 18.6, 22.4, 28.9, 32.1] },
    { node_id: "node-3", data_points: [11.8, 16.9, 21.3, 27.5, 31.6] }
  ]
  
  for node in node_data {
    for value in node.data_points {
      azimuth::DistributedAggregator::add_node_data(distributed_aggregator, node.node_id, value)
    }
  }
  
  // 创建分布式快照
  let distributed_snapshot = azimuth::DistributedAggregator::create_snapshot(distributed_aggregator)
  
  // 验证快照包含所有节点数据
  assert_true(distributed_snapshot.node_snapshots.length >= 3)
  
  // 在另一个节点恢复快照
  let remote_aggregator = azimuth::AggregationManager::create_distributed_aggregator(aggregation_manager)
  azimuth::DistributedAggregator::restore_from_snapshot(remote_aggregator, distributed_snapshot)
  
  let remote_stats = azimuth::DistributedAggregator::get_global_statistics(remote_aggregator)
  assert_eq(remote_stats.total_count, 15)  # 3个节点，每个5个数据点
  assert_true(remote_stats.global_average >= 15.0 && remote_stats.global_average <= 30.0)
}