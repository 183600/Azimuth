// Azimuth 时间序列数据处理测试用例
// 专注于时间序列数据的采集、存储、查询和分析功能测试

// 测试1: 时间序列数据采集
test "时间序列数据采集测试" {
  // 创建时间序列采集器
  let ts_collector = TimeSeriesCollector::new({
    buffer_size: 10000,
    flush_interval: 60000,  // 1分钟
    compression_enabled: true,
    validation_enabled: true
  })
  
  // 添加指标定义
  ts_collector.add_metric_definition({
    name: "cpu.usage",
    data_type: "gauge",
    unit: "percent",
    description: "CPU使用率",
    tags: ["host", "service"]
  })
  
  ts_collector.add_metric_definition({
    name: "request.count",
    data_type: "counter",
    unit: "count",
    description: "请求计数",
    tags: ["service", "endpoint", "method"]
  })
  
  ts_collector.add_metric_definition({
    name: "response.duration",
    data_type: "histogram",
    unit: "milliseconds",
    description: "响应时间分布",
    tags: ["service", "endpoint"],
    buckets: [10, 50, 100, 200, 500, 1000, 2000]
  })
  
  // 采集时间序列数据点
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // 采集CPU使用率数据
  for i in 0..=100 {
    ts_collector.collect_gauge({
      metric_name: "cpu.usage",
      timestamp: base_time + i * 60,  // 每分钟一个数据点
      value: 50.0 + (i % 20) * 2.0,   // 50-90%之间波动
      tags: [
        ("host", "server-" + (i % 5).to_string()),
        ("service", "web-api")
      ]
    })
  }
  
  // 采集请求计数数据
  for i in 0..=100 {
    ts_collector.collect_counter({
      metric_name: "request.count",
      timestamp: base_time + i * 60,
      value: 100 + (i % 10) * 10,    // 100-190之间
      tags: [
        ("service", "web-api"),
        ("endpoint", "/api/users"),
        ("method", "GET")
      ]
    })
  }
  
  // 采集响应时间分布数据
  for i in 0..=100 {
    let response_time = 50 + (i % 50) * 10  // 50-550ms之间
    ts_collector.collect_histogram({
      metric_name: "response.duration",
      timestamp: base_time + i * 60,
      value: response_time,
      tags: [
        ("service", "web-api"),
        ("endpoint", "/api/users")
      ]
    })
  }
  
  // 验证采集的数据点数量
  let buffer_stats = ts_collector.get_buffer_stats()
  assert_eq(buffer_stats.total_points, 303)  // 101 + 101 + 101
  assert_eq(buffer_stats.metrics.get("cpu.usage"), Some(101))
  assert_eq(buffer_stats.metrics.get("request.count"), Some(101))
  assert_eq(buffer_stats.metrics.get("response.duration"), Some(101))
  
  // 测试数据点查询
  let cpu_data = ts_collector.query_metric("cpu.usage", {
    start_time: base_time,
    end_time: base_time + 6000,  // 前100分钟
    tags: [("host", "server-1")]
  })
  
  assert_true(cpu_data.length() > 0)
  assert_true(cpu_data.length() <= 101)
  
  // 验证数据点结构
  let first_point = cpu_data[0]
  assert_eq(first_point.metric_name, "cpu.usage")
  assert_true(first_point.timestamp >= base_time)
  assert_true(first_point.value >= 50.0 and first_point.value <= 90.0)
  assert_eq(first_point.tags.get("host"), Some("server-1"))
  
  // 测试数据刷新
  let flush_result = ts_collector.flush()
  assert_true(flush_result.success)
  assert_eq(flush_result.points_flushed, 303)
  
  // 验证缓冲区已清空
  let empty_stats = ts_collector.get_buffer_stats()
  assert_eq(empty_stats.total_points, 0)
}

// 测试2: 时间序列数据存储
test "时间序列数据存储测试" {
  // 创建时间序列存储引擎
  let ts_storage = TimeSeriesStorage::new({
    engine: "influxdb",
    connection_string: "http://localhost:8086",
    database: "azimuth_telemetry",
    retention_policy: "30d",
    batch_size: 1000,
    flush_interval: 5000
  })
  
  // 测试存储连接
  let connection_result = ts_storage.test_connection()
  assert_true(connection_result.success)
  
  // 创建时间序列表
  let table_creation_result = ts_storage.create_table({
    name: "system_metrics",
    fields: [
      { name: "timestamp", type: "timestamp", primary: true },
      { name: "metric_name", type: "string", primary: true },
      { name: "value", type: "double" },
      { name: "tags", type: "json" }
    ],
    indexes: ["metric_name", "timestamp"]
  })
  
  assert_true(table_creation_result.success)
  
  // 准备测试数据
  let test_data = []
  let base_time = 1640995200
  
  for i in 0..=500 {
    test_data = test_data.push({
      timestamp: base_time + i * 60,
      metric_name: "memory.usage",
      value: 60.0 + (i % 30) * 1.0,
      tags: {
        "host": "server-" + (i % 3).to_string(),
        "service": "database"
      }
    })
  }
  
  // 测试批量写入
  let write_result = ts_storage.write_batch("system_metrics", test_data)
  assert_true(write_result.success)
  assert_eq(write_result.points_written, 501)
  
  // 测试单点写入
  let single_point = {
    timestamp: base_time + 501 * 60,
    metric_name: "memory.usage",
    value: 75.5,
    tags: {
      "host": "server-0",
      "service": "database"
    }
  }
  
  let single_write_result = ts_storage.write_point("system_metrics", single_point)
  assert_true(single_write_result.success)
  assert_eq(single_write_result.points_written, 1)
  
  // 测试时间范围查询
  let query_result = ts_storage.query_range({
    table: "system_metrics",
    start_time: base_time,
    end_time: base_time + 1000 * 60,
    metric_filter: "memory.usage",
    tag_filters: [("host", "server-1")]
  })
  
  assert_true(query_result.success)
  assert_true(query_result.points.length() > 0)
  
  // 验证查询结果
  for point in query_result.points {
    assert_eq(point.metric_name, "memory.usage")
    assert_eq(point.tags.get("host"), Some("server-1"))
    assert_eq(point.tags.get("service"), Some("database"))
    assert_true(point.value >= 60.0 and point.value <= 90.0)
  }
  
  // 测试聚合查询
  let aggregation_result = ts_storage.query_aggregation({
    table: "system_metrics",
    start_time: base_time,
    end_time: base_time + 1000 * 60,
    metric_filter: "memory.usage",
    aggregation: "avg",
    group_by: ["host"],
    interval: "1h"
  })
  
  assert_true(aggregation_result.success)
  assert_true(aggregation_result.aggregated_points.length() > 0)
  
  // 验证聚合结果
  for agg_point in aggregation_result.aggregated_points {
    assert_true(agg_point.value >= 60.0 and agg_point.value <= 90.0)
    assert_true(agg_point.tags.contains_key("host"))
  }
  
  // 测试最新值查询
  let latest_result = ts_storage.query_latest({
    table: "system_metrics",
    metric_filter: "memory.usage",
    tag_filters: [("host", "server-0")]
  })
  
  assert_true(latest_result.success)
  assert_eq(latest_result.points.length(), 1)
  
  let latest_point = latest_result.points[0]
  assert_eq(latest_point.metric_name, "memory.usage")
  assert_eq(latest_point.tags.get("host"), Some("server-0"))
  
  // 测试数据删除
  let delete_result = ts_storage.delete_range({
    table: "system_metrics",
    start_time: base_time,
    end_time: base_time + 10 * 60  // 删除前10分钟数据
  })
  
  assert_true(delete_result.success)
  assert_true(delete_result.points_deleted > 0)
  
  // 验证删除后的数据
  let after_delete_query = ts_storage.query_range({
    table: "system_metrics",
    start_time: base_time,
    end_time: base_time + 1000 * 60,
    metric_filter: "memory.usage"
  })
  
  assert_true(after_delete_query.success)
  assert_true(after_delete_query.points.length() < query_result.points.length())
}

// 测试3: 时间序列数据压缩
test "时间序列数据压缩测试" {
  // 创建数据压缩器
  let ts_compressor = TimeSeriesCompressor::new({
    algorithm: "gorilla",  // Gorilla压缩算法，适合时间序列数据
    compression_level: 6,
    block_size: 1000
  })
  
  // 生成测试时间序列数据
  let generate_ts_data = fn(count: Int, noise_factor: Float) {
    let mut data = []
    let base_time = 1640995200
    let base_value = 100.0
    
    for i in 0..=count - 1 {
      let timestamp = base_time + i * 60
      let trend = i.to_float() * 0.1  // 缓慢上升趋势
      let noise = (Math::random() - 0.5) * noise_factor
      let value = base_value + trend + noise
      
      data = data.push({
        timestamp: timestamp,
        value: value,
        metric: "test.metric",
        tags: [("host", "test-server")]
      })
    }
    data
  }
  
  // 生成不同特征的时间序列数据
  let stable_series = generate_ts_data(1000, 2.0)    // 稳定序列，噪声小
  let volatile_series = generate_ts_data(1000, 20.0)  // 波动序列，噪声大
  let sparse_series = generate_ts_data(100, 5.0)      // 稀疏序列，数据点少
  
  // 测试稳定序列压缩
  let stable_compression_result = ts_compressor.compress_series(stable_series)
  assert_true(stable_compression_result.success)
  
  let stable_compression_ratio = 1.0 - (stable_compression_result.compressed_size.to_float() / stable_compression_result.original_size.to_float())
  assert_true(stable_compression_ratio > 0.8)  // 稳定序列应该有高压缩率
  
  // 测试波动序列压缩
  let volatile_compression_result = ts_compressor.compress_series(volatile_series)
  assert_true(volatile_compression_result.success)
  
  let volatile_compression_ratio = 1.0 - (volatile_compression_result.compressed_size.to_float() / volatile_compression_result.original_size.to_float())
  assert_true(volatile_compression_ratio > 0.5)  // 波动序列压缩率较低
  
  // 测试稀疏序列压缩
  let sparse_compression_result = ts_compressor.compress_series(sparse_series)
  assert_true(sparse_compression_result.success)
  
  // 验证压缩和解压缩的数据一致性
  let stable_decompressed = ts_compressor.decompress_series(stable_compression_result.compressed_data)
  assert_eq(stable_decompressed.length(), stable_series.length())
  
  for i in 0..=stable_series.length() - 1 {
    let original = stable_series[i]
    let decompressed = stable_decompressed[i]
    
    assert_eq(original.timestamp, decompressed.timestamp)
    assert_eq(original.metric, decompressed.metric)
    assert_true(Math::abs(original.value - decompressed.value) < 0.001)  // 允许小的浮点误差
  }
  
  // 测试不同压缩算法
  let delta_compressor = TimeSeriesCompressor::new({
    algorithm: "delta",
    compression_level: 6,
    block_size: 1000
  })
  
  let delta_compression_result = delta_compressor.compress_series(stable_series)
  let delta_compression_ratio = 1.0 - (delta_compression_result.compressed_size.to_float() / delta_compression_result.original_size.to_float())
  
  // Gorilla算法通常比Delta算法更适合时间序列数据
  assert_true(stable_compression_ratio >= delta_compression_ratio)
  
  // 测试自适应压缩
  let adaptive_compressor = AdaptiveTimeSeriesCompressor::new({
    algorithms: ["gorilla", "delta", "simple8b"],
    analysis_window: 100,
    auto_switch: true
  })
  
  let adaptive_result = adaptive_compressor.compress_series(stable_series)
  assert_true(adaptive_result.success)
  assert_true(adaptive_result.algorithm_used != None)
  
  // 测试压缩性能
  let compression_start_time = Time::now()
  for i in 0..=10 {
    ts_compressor.compress_series(stable_series)
  }
  let compression_duration = Time::now() - compression_start_time
  
  let decompression_start_time = Time::now()
  for i in 0..=10 {
    ts_compressor.decompress_series(stable_compression_result.compressed_data)
  }
  let decompression_duration = Time::now() - decompression_start_time
  
  // 验证压缩和解压缩性能
  assert_true(compression_duration < 5000)    // 压缩应在5秒内完成
  assert_true(decompression_duration < 2000) // 解压缩应在2秒内完成
}

// 测试4: 时间序列数据查询优化
test "时间序列数据查询优化测试" {
  // 创建查询优化器
  let query_optimizer = TSQueryOptimizer::new({
    enable_caching: true,
    cache_size: 1000,
    cache_ttl: 300000,  // 5分钟
    enable_index_hints: true,
    enable_query_plan_cache: true
  })
  
  // 创建测试数据存储
  let test_storage = create_test_time_series_storage()
  
  // 生成大量测试数据
  let large_dataset = generate_large_time_series_dataset(10000)  // 10000个数据点
  
  // 批量写入测试数据
  let write_result = test_storage.write_batch("metrics", large_dataset)
  assert_true(write_result.success)
  
  // 测试基本查询
  let basic_query = {
    table: "metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 10000 * 60,
    metric_filter: "cpu.usage",
    limit: 1000
  }
  
  let basic_start_time = Time::now()
  let basic_result = test_storage.query(basic_query)
  let basic_duration = Time::now() - basic_start_time
  
  assert_true(basic_result.success)
  assert_true(basic_result.points.length() <= 1000)
  
  // 测试优化查询
  let optimized_query = query_optimizer.optimize_query(basic_query)
  let optimized_start_time = Time::now()
  let optimized_result = test_storage.query(optimized_query)
  let optimized_duration = Time::now() - optimized_start_time
  
  assert_true(optimized_result.success)
  assert_eq(optimized_result.points.length(), basic_result.points.length())
  
  // 优化查询应该更快（后续查询使用缓存）
  assert_true(optimized_duration <= basic_duration)
  
  // 测试索引提示
  let indexed_query = {
    table: "metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 10000 * 60,
    metric_filter: "memory.usage",
    index_hints: ["metric_name", "timestamp"],
    limit: 500
  }
  
  let indexed_start_time = Time::now()
  let indexed_result = test_storage.query(indexed_query)
  let indexed_duration = Time::now() - indexed_start_time
  
  assert_true(indexed_result.success)
  assert_true(indexed_result.points.length() <= 500)
  
  // 测试聚合查询优化
  let aggregation_query = {
    table: "metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 10000 * 60,
    metric_filter: "cpu.usage",
    aggregation: "avg",
    group_by: ["host"],
    interval: "1h",
    limit: 100
  }
  
  let agg_start_time = Time::now()
  let agg_result = test_storage.query_aggregation(aggregation_query)
  let agg_duration = Time::now() - agg_start_time
  
  assert_true(agg_result.success)
  assert_true(agg_result.aggregated_points.length() <= 100)
  
  // 测试预计算聚合
  query_optimizer.create_materialized_view({
    name: "cpu_usage_hourly",
    query: aggregation_query,
    refresh_interval: 3600000  // 1小时
  })
  
  let precomputed_start_time = Time::now()
  let precomputed_result = query_optimizer.query_materialized_view("cpu_usage_hourly")
  let precomputed_duration = Time::now() - precomputed_start_time
  
  assert_true(precomputed_result.success)
  assert_true(precomputed_duration < agg_duration)  // 预计算应该更快
  
  // 测试查询计划缓存
  let similar_query = {
    table: "metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 10000 * 60,
    metric_filter: "cpu.usage",
    limit: 2000  // 不同的limit
  }
  
  let cached_plan_start_time = Time::now()
  let cached_plan_result = test_storage.query(query_optimizer.optimize_query(similar_query))
  let cached_plan_duration = Time::now() - cached_plan_start_time
  
  // 使用缓存的查询计划应该更快
  assert_true(cached_plan_duration < basic_duration)
  
  // 测试查询性能统计
  let performance_stats = query_optimizer.get_performance_stats()
  assert_true(performance_stats.cache_hit_rate > 0.0)
  assert_true(performance_stats.avg_query_time > 0.0)
  assert_true(performance_stats.total_queries > 0)
}

// 测试5: 时间序列数据降采样
test "时间序列数据降采样测试" {
  // 创建降采样管理器
  let downsampler = TimeSeriesDownsampler::new({
    default_policy: "avg",
    retention_policies: [
      {
        name: "raw",
        duration: "1h",
        interval: "1m"
      },
      {
        name: "hourly",
        duration: "7d",
        interval: "1h",
        aggregation: "avg"
      },
      {
        name: "daily",
        duration: "30d",
        interval: "1d",
        aggregation: "avg"
      },
      {
        name: "monthly",
        duration: "1y",
        interval: "1M",
        aggregation: "avg"
      }
    ]
  })
  
  // 创建测试数据
  let raw_data = generate_high_frequency_time_series(1440)  // 24小时的分钟数据
  
  // 写入原始数据
  let storage = create_test_time_series_storage()
  let write_result = storage.write_batch("raw_metrics", raw_data)
  assert_true(write_result.success)
  
  // 测试小时级降采样
  let hourly_downsample_result = downsampler.downsample({
    source_table: "raw_metrics",
    target_table: "hourly_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,  // 24小时
    interval: "1h",
    aggregation: "avg"
  })
  
  assert_true(hourly_downsample_result.success)
  assert_eq(hourly_downsample_result.points_downsampled, 24)  // 24小时
  
  // 验证小时级数据
  let hourly_query_result = storage.query_range({
    table: "hourly_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    metric_filter: "cpu.usage"
  })
  
  assert_eq(hourly_query_result.points.length(), 24)
  
  // 验证降采样数据的正确性
  for hourly_point in hourly_query_result.points {
    // 计算对应时间范围内的原始数据平均值
    let hour_start = hourly_point.timestamp
    let hour_end = hour_start + 3600
    
    let raw_hour_data = storage.query_range({
      table: "raw_metrics",
      start_time: hour_start,
      end_time: hour_end,
      metric_filter: "cpu.usage"
    })
    
    let expected_avg = calculate_average(raw_hour_data.points.map(fn(p) { p.value }))
    assert_true(Math::abs(hourly_point.value - expected_avg) < 0.001)
  }
  
  // 测试日级降采样
  let daily_downsample_result = downsampler.downsample({
    source_table: "hourly_metrics",
    target_table: "daily_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    interval: "1d",
    aggregation: "avg"
  })
  
  assert_true(daily_downsample_result.success)
  assert_eq(daily_downsample_result.points_downsampled, 1)  // 1天
  
  // 验证日级数据
  let daily_query_result = storage.query_range({
    table: "daily_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    metric_filter: "cpu.usage"
  })
  
  assert_eq(daily_query_result.points.length(), 1)
  
  // 测试不同聚合方法
  let max_downsample_result = downsampler.downsample({
    source_table: "raw_metrics",
    target_table: "hourly_max_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    interval: "1h",
    aggregation: "max"
  })
  
  let min_downsample_result = downsampler.downsample({
    source_table: "raw_metrics",
    target_table: "hourly_min_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    interval: "1h",
    aggregation: "min"
  })
  
  assert_true(max_downsample_result.success)
  assert_true(min_downsample_result.success)
  
  // 验证最大值和最小值降采样
  let max_data = storage.query_range({
    table: "hourly_max_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 3600,
    metric_filter: "cpu.usage"
  })
  
  let min_data = storage.query_range({
    table: "hourly_min_metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 3600,
    metric_filter: "cpu.usage"
  })
  
  assert_eq(max_data.points.length(), 1)
  assert_eq(min_data.points.length(), 1)
  
  let max_point = max_data.points[0]
  let min_point = min_data.points[0]
  
  assert_true(max_point.value >= min_point.value)
  
  // 测试自动降采样策略
  downsampler.enable_auto_downsampling({
    schedule: "0 * * * *",  // 每小时执行
    retention_check: true
  })
  
  // 模拟自动降采样触发
  let auto_downsample_result = downsampler.run_auto_downsampling()
  assert_true(auto_downsample_result.success)
  assert_true(auto_downsample_result.policies_executed > 0)
  
  // 测试降采样数据查询
  let downsampled_query = downsampler.query_with_fallback({
    table: "metrics",
    start_time: 1640995200,
    end_time: 1640995200 + 86400,
    metric_filter: "cpu.usage",
    preferred_resolution: "1h"
  })
  
  assert_true(downsampled_query.success)
  assert_eq(downsampled_query.resolution_used, "1h")
}

// 测试6: 时间序列异常检测
test "时间序列异常检测测试" {
  // 创建异常检测器
  let anomaly_detector = TSAnomalyDetector::new({
    algorithms: ["statistical", "ml", "pattern"],
    sensitivity: 0.8,
    min_data_points: 50,
    confidence_threshold: 0.95
  })
  
  // 生成正常时间序列数据
  let normal_data = generate_normal_time_series(200, {
    trend: 0.1,
    seasonality: 24,
    noise_level: 5.0,
    base_value: 100.0
  })
  
  // 添加异常点
  let anomalous_data = add_anomalies_to_series(normal_data, [
    { index: 50, type: "spike", magnitude: 3.0 },
    { index: 100, type: "drop", magnitude: -2.5 },
    { index: 150, type: "trend_change", duration: 10 }
  ])
  
  // 训练异常检测模型
  let training_result = anomaly_detector.train_model(normal_data, {
    algorithm: "statistical",
    window_size: 20,
    confidence_interval: 0.95
  })
  
  assert_true(training_result.success)
  assert_true(training_result.model_accuracy > 0.9)
  
  // 检测异常
  let detection_result = anomaly_detector.detect_anomalies(anomalous_data, {
    algorithm: "statistical",
    threshold: 2.0  // 2个标准差
    min_anomaly_gap: 5  // 异常之间至少间隔5个点
  })
  
  assert_true(detection_result.success)
  assert_true(detection_result.anomalies.length() > 0)
  
  // 验证检测到的异常
  let spike_anomaly = detection_result.anomalies.find(fn(a) { a.index == 50 })
  assert_true(spike_anomaly != None)
  
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.type, "spike")
      assert_true(anomaly.score > 2.0)
      assert_true(anomaly.confidence > 0.95)
    }
    None => assert_true(false)
  }
  
  // 测试机器学习方法
  let ml_training_result = anomaly_detector.train_model(normal_data, {
    algorithm: "isolation_forest",
    contamination: 0.1,
    n_estimators: 100
  })
  
  assert_true(ml_training_result.success)
  
  let ml_detection_result = anomaly_detector.detect_anomalies(anomalous_data, {
    algorithm: "isolation_forest"
  })
  
  assert_true(ml_detection_result.success)
  assert_true(ml_detection_result.anomalies.length() > 0)
  
  // 测试模式检测
  let pattern_training_result = anomaly_detector.train_model(normal_data, {
    algorithm: "pattern_matching",
    pattern_length: 24,
    similarity_threshold: 0.9
  })
  
  assert_true(pattern_training_result.success)
  
  let pattern_detection_result = anomaly_detector.detect_anomalies(anomalous_data, {
    algorithm: "pattern_matching"
  })
  
  assert_true(pattern_detection_result.success)
  
  // 测试实时异常检测
  let realtime_detector = RealTimeAnomalyDetector::new({
    model: anomaly_detector.get_trained_model(),
    window_size: 20,
    alert_threshold: 0.8
  })
  
  let mut anomaly_count = 0
  for point in anomalous_data {
    let realtime_result = realtime_detector.process_point(point)
    if realtime_result.is_anomaly {
      anomaly_count = anomaly_count + 1
    }
  }
  
  assert_true(anomaly_count > 0)
  
  // 测试异常解释
  let anomaly_explanation = anomaly_detector.explain_anomaly({
    data_point: anomalous_data[50],
    context: anomalous_data.slice(30, 70),
    algorithm: "statistical"
  })
  
  assert_true(anomaly_explanation.factors.length() > 0)
  assert_true(anomaly_explanation.confidence > 0.0)
  
  // 测试异常修复建议
  let repair_suggestions = anomaly_detector.suggest_repair({
    anomalous_data: anomalous_data,
    anomaly_indices: [50, 100, 150],
    repair_methods: ["interpolation", "forecast", "replacement"]
  })
  
  assert_true(repair_suggestions.suggestions.length() > 0)
  
  // 应用修复建议
  let repaired_data = anomaly_detector.apply_repairs(anomalous_data, repair_suggestions.suggestions[0])
  assert_eq(repaired_data.length(), anomalous_data.length())
  
  // 验证修复后的数据
  let repaired_detection_result = anomaly_detector.detect_anomalies(repaired_data, {
    algorithm: "statistical",
    threshold: 2.0
  })
  
  assert_true(repaired_detection_result.anomalies.length() < detection_result.anomalies.length())
}

// 测试7: 时间序列预测
test "时间序列预测测试" {
  // 创建时间序列预测器
  let ts_forecaster = TSForecaster::new({
    algorithms: ["arima", "exponential_smoothing", "linear_regression"],
    forecast_horizon: 24,
    confidence_interval: 0.95
  })
  
  // 生成历史数据
  let historical_data = generate_time_series_with_patterns(500, {
    trend: 0.05,
    seasonality: {
      daily: true,
      weekly: true,
      monthly: false
    },
    noise_level: 2.0,
    base_value: 100.0
  })
  
  // 分割训练和测试数据
  let train_data = historical_data.slice(0, 400)
  let test_data = historical_data.slice(400, 500)
  
  // 训练ARIMA模型
  let arima_training_result = ts_forecaster.train_model(train_data, {
    algorithm: "arima",
    auto_arima: true,
    seasonal: true,
    seasonal_periods: [24, 168]  // 日、周季节性
  })
  
  assert_true(arima_training_result.success)
  assert_true(arima_training_result.model_metrics.aic > 0)
  
  // 进行预测
  let arima_forecast_result = ts_forecaster.forecast({
    model: arima_training_result.model,
    steps: 24,
    confidence_level: 0.95
  })
  
  assert_true(arima_forecast_result.success)
  assert_eq(arima_forecast_result.forecasts.length(), 24)
  assert_eq(arima_forecast_result.confidence_intervals.length(), 24)
  
  // 验证预测结果
  for forecast in arima_forecast_result.forecasts {
    assert_true(forecast.value > 0)
    assert_true(forecast.timestamp > 0)
  }
  
  // 评估预测准确性
  let accuracy_result = ts_forecaster.evaluate_forecast({
    forecasts: arima_forecast_result.forecasts.slice(0, 24),
    actual: test_data.slice(0, 24),
    metrics: ["mae", "rmse", "mape"]
  })
  
  assert_true(accuracy_result.success)
  assert_true(accuracy_result.metrics.mae > 0)
  assert_true(accuracy_result.metrics.rmse > 0)
  assert_true(accuracy_result.metrics.mape > 0)
  
  // 训练指数平滑模型
  let es_training_result = ts_forecaster.train_model(train_data, {
    algorithm: "exponential_smoothing",
    trend: "additive",
    seasonal: "additive",
    seasonal_periods: 24
  })
  
  assert_true(es_training_result.success)
  
  // 进行预测
  let es_forecast_result = ts_forecaster.forecast({
    model: es_training_result.model,
    steps: 24,
    confidence_level: 0.95
  })
  
  assert_true(es_forecast_result.success)
  
  // 比较不同模型的预测结果
  let model_comparison = ts_forecaster.compare_models({
    models: [
      { name: "ARIMA", model: arima_training_result.model },
      { name: "ExponentialSmoothing", model: es_training_result.model }
    ],
    test_data: test_data.slice(0, 24),
    metrics: ["mae", "rmse"]
  })
  
  assert_true(model_comparison.length() > 0)
  
  // 测试集成预测
  let ensemble_forecast_result = ts_forecaster.ensemble_forecast({
    models: [
      arima_training_result.model,
      es_training_result.model
    ],
    weights: [0.6, 0.4],
    steps: 24
  })
  
  assert_true(ensemble_forecast_result.success)
  assert_eq(ensemble_forecast_result.forecasts.length(), 24)
  
  // 测试异常值对预测的影响
  let data_with_outliers = add_outliers_to_time_series(train_data, [
    { index: 50, value: 500.0 },
    { index: 150, value: 10.0 }
  ])
  
  let outlier_robust_forecast = ts_forecaster.forecast_with_outlier_detection({
    data: data_with_outliers,
    outlier_method: "iqr",
    outlier_threshold: 1.5,
    forecast_steps: 24
  })
  
  assert_true(outlier_robust_forecast.success)
  
  // 测试预测区间
  let forecast_intervals = ts_forecaster.calculate_forecast_intervals({
    model: arima_training_result.model,
    steps: 24,
    confidence_levels: [0.8, 0.9, 0.95]
  })
  
  assert_true(forecast_intervals.success)
  assert_eq(forecast_intervals.intervals.length(), 24)
  
  for interval in forecast_intervals.intervals {
    assert_true(interval.lower_80 < interval.upper_80)
    assert_true(interval.lower_90 < interval.upper_90)
    assert_true(interval.lower_95 < interval.upper_95)
    assert_true(interval.lower_80 > interval.lower_90)
    assert_true(interval.upper_80 < interval.upper_90)
  }
}

// 测试8: 时间序列数据可视化
test "时间序列数据可视化测试" {
  // 创建时间序列可视化器
  let ts_visualizer = TSVisualizer::new({
    default_theme: "light",
    default_width: 800,
    default_height: 400,
    output_formats: ["svg", "png", "html"]
  })
  
  // 生成测试数据
  let multi_series_data = generate_multiple_time_series({
    series_count: 3,
    points_per_series: 100,
    start_time: 1640995200,
    interval: 3600  // 1小时间隔
  })
  
  // 创建线图
  let line_chart_result = ts_visualizer.create_line_chart({
    data: multi_series_data,
    title: "系统性能指标趋势",
    x_axis: { label: "时间", format: "datetime" },
    y_axis: { label: "值" },
    legend: { position: "top" },
    colors: ["#1f77b4", "#ff7f0e", "#2ca02c"],
    output_format: "svg"
  })
  
  assert_true(line_chart_result.success)
  assert_true(line_chart_result.output_size > 0)
  
  // 创建面积图
  let area_chart_result = ts_visualizer.create_area_chart({
    data: multi_series_data.slice(0, 2),  // 只使用前两个系列
    title: "资源使用率",
    x_axis: { label: "时间", format: "datetime" },
    y_axis: { label: "使用率 (%)" },
    stacked: true,
    opacity: 0.7,
    output_format: "png"
  })
  
  assert_true(area_chart_result.success)
  
  // 创建热力图
  let heatmap_data = generate_heatmap_data({
    rows: 24,  // 24小时
    columns: 7, // 7天
    start_time: 1640995200
  })
  
  let heatmap_result = ts_visualizer.create_heatmap({
    data: heatmap_data,
    title: "一周内每小时系统负载热力图",
    x_axis: { label: "星期" },
    y_axis: { label: "小时" },
    color_scheme: "viridis",
    output_format: "html"
  })
  
  assert_true(heatmap_result.success)
  
  // 创建仪表盘
  let dashboard_result = ts_visualizer.create_dashboard({
    title: "系统监控仪表盘",
    layout: "grid",
    panels: [
      {
        type: "line_chart",
        title: "CPU使用率",
        data: multi_series_data[0],
        span: { width: 12, height: 6 }
      },
      {
        type: "gauge",
        title: "当前内存使用率",
        value: 75.5,
        max: 100,
        thresholds: [
          { value: 50, color: "green" },
          { value: 80, color: "yellow" },
          { value: 90, color: "red" }
        ],
        span: { width: 6, height: 6 }
      },
      {
        type: "area_chart",
        title: "网络流量",
        data: multi_series_data[1],
        span: { width: 6, height: 6 }
      }
    ],
    refresh_interval: 30000,  // 30秒刷新
    output_format: "html"
  })
  
  assert_true(dashboard_result.success)
  
  // 测试交互式图表
  let interactive_chart_result = ts_visualizer.create_interactive_chart({
    data: multi_series_data[0],
    chart_type: "line",
    title: "交互式性能指标",
    features: ["zoom", "pan", "tooltip", "crosshair"],
    output_format: "html"
  })
  
  assert_true(interactive_chart_result.success)
  
  // 测试实时数据可视化
  let realtime_visualizer = RealTimeVisualizer::new({
    update_interval: 5000,  // 5秒更新
    max_points: 100,
    animation_duration: 500
  })
  
  let realtime_chart = realtime_visualizer.create_realtime_chart({
    title: "实时系统指标",
    metrics: ["cpu.usage", "memory.usage"],
    chart_type: "line",
    output_format: "html"
  })
  
  assert_true(realtime_chart.success)
  
  // 模拟实时数据更新
  for i in 0..=10 {
    let realtime_data = generate_realtime_data_point({
      timestamp: Time::now(),
      metrics: [
        { name: "cpu.usage", value: 50.0 + (i % 20) * 2.0 },
        { name: "memory.usage", value: 60.0 + (i % 15) * 1.5 }
      ]
    })
    
    let update_result = realtime_visualizer.update_chart(realtime_chart.chart_id, realtime_data)
    assert_true(update_result.success)
    
    Time::sleep(100)  // 短暂等待
  }
  
  // 测试图表导出
  let export_result = ts_visualizer.export_chart({
    chart_id: line_chart_result.chart_id,
    format: "png",
    width: 1200,
    height: 600,
    quality: 90
  })
  
  assert_true(export_result.success)
  assert_true(export_result.file_size > 0)
  
  // 测试图表模板
  let template_result = ts_visualizer.create_chart_from_template({
    template_name: "system_overview",
    data: multi_series_data,
    customizations: {
      title: "自定义系统概览",
      color_scheme: "dark"
    },
    output_format: "svg"
  })
  
  assert_true(template_result.success)
}

// 测试9: 时间序列数据流处理
test "时间序列数据流处理测试" {
  // 创建数据流处理器
  let stream_processor = TSStreamProcessor::new({
    buffer_size: 10000,
    processing_window: "1m",
    batch_size: 100,
    parallel_workers: 4
  })
  
  // 添加流处理算子
  stream_processor.add_operator({
    name: "filter",
    type: "filter",
    condition: "value > 50.0",
    output_stream: "high_value_stream"
  })
  
  stream_processor.add_operator({
    name: "aggregation",
    type: "windowed_aggregation",
    window_size: "5m",
    aggregation: "avg",
    group_by: ["metric_name"],
    output_stream: "aggregated_stream"
  })
  
  stream_processor.add_operator({
    name: "anomaly_detection",
    type: "anomaly_detection",
    algorithm: "statistical",
    threshold: 2.0,
    output_stream: "anomaly_stream"
  })
  
  // 创建数据源
  let data_source = MockTimeSeriesSource::new({
    metrics: ["cpu.usage", "memory.usage", "disk.usage"],
    interval: 1000,  // 1秒间隔
    value_range: (0.0, 100.0),
    anomaly_probability: 0.05
  })
  
  // 启动流处理
  let processing_result = stream_processor.start_processing(data_source)
  assert_true(processing_result.success)
  
  // 运行一段时间
  Time::sleep(10000)  // 运行10秒
  
  // 检查处理结果
  let processed_stats = stream_processor.get_processing_stats()
  assert_true(processed_stats.total_processed > 0)
  assert_true(processed_stats.output_streams.contains_key("high_value_stream"))
  assert_true(processed_stats.output_streams.contains_key("aggregated_stream"))
  assert_true(processed_stats.output_streams.contains_key("anomaly_stream"))
  
  // 获取高值流数据
  let high_value_data = stream_processor.get_stream_data("high_value_stream", {
    limit: 100
  })
  
  assert_true(high_value_data.length() > 0)
  
  for point in high_value_data {
    assert_true(point.value > 50.0)
  }
  
  // 获取聚合流数据
  let aggregated_data = stream_processor.get_stream_data("aggregated_stream", {
    limit: 100
  })
  
  assert_true(aggregated_data.length() > 0)
  
  // 获取异常流数据
  let anomaly_data = stream_processor.get_stream_data("anomaly_stream", {
    limit: 100
  })
  
  // 测试动态添加算子
  stream_processor.add_operator({
    name: "smoothing",
    type: "moving_average",
    window_size: 10,
    output_stream: "smoothed_stream"
  })
  
  // 验证新算子已添加
  let operators = stream_processor.get_operators()
  assert_true(operators.length() >= 4)
  assert_true(operators.any(fn(op) { op.name == "smoothing" }))
  
  // 测试流处理性能
  let performance_start_time = Time::now()
  let high_volume_source = MockTimeSeriesSource::new({
    metrics: ["test.metric"],
    interval: 10,  // 10ms间隔，高频率
    value_range: (0.0, 100.0),
    anomaly_probability: 0.0
  })
  
  let high_volume_result = stream_processor.start_processing(high_volume_source)
  Time::sleep(5000)  // 运行5秒
  
  let performance_end_time = Time::now()
  let performance_duration = performance_end_time - performance_start_time
  let throughput = processed_stats.total_processed.to_float() / (performance_duration.to_float() / 1000.0)
  
  assert_true(throughput > 1000)  // 至少每秒处理1000个数据点
  
  // 停止流处理
  let stop_result = stream_processor.stop_processing()
  assert_true(stop_result.success)
  
  // 测试状态恢复
  let checkpoint_result = stream_processor.create_checkpoint()
  assert_true(checkpoint_result.success)
  
  let restore_result = stream_processor.restore_from_checkpoint(checkpoint_result.checkpoint_id)
  assert_true(restore_result.success)
}

// 测试10: 时间序列数据质量监控
test "时间序列数据质量监控测试" {
  // 创建数据质量监控器
  let quality_monitor = TSDataQualityMonitor::new({
    check_interval: 60000,  // 1分钟检查一次
    alert_threshold: 0.8,
    retention_period: 86400000  // 保留24小时的质量报告
  })
  
  // 添加质量检查规则
  quality_monitor.add_check_rule({
    name: "completeness_check",
    type: "completeness",
    description: "检查数据完整性",
    parameters: {
      expected_interval: "1m",
      max_gap_duration: "5m",
      min_data_points: 100
    },
    severity: "high"
  })
  
  quality_monitor.add_check_rule({
    name: "timeliness_check",
    type: "timeliness",
    description: "检查数据时效性",
    parameters: {
      max_delay_seconds: 300,  // 5分钟
      acceptable_delay_percentage: 0.95
    },
    severity: "medium"
  })
  
  quality_monitor.add_check_rule({
    name: "accuracy_check",
    type: "accuracy",
    description: "检查数据准确性",
    parameters: {
      value_range: (0.0, 100.0),
      outlier_threshold: 3.0,
      outlier_method: "zscore"
    },
    severity: "medium"
  })
  
  quality_monitor.add_check_rule({
    name: "consistency_check",
    type: "consistency",
    description: "检查数据一致性",
    parameters: {
      monotonic_metrics: ["counters"],
      expected_value_changes: ["counters"],
      max_fluctuation_percentage: 0.1
    },
    severity: "low"
  })
  
  // 生成测试数据
  let good_quality_data = generate_time_series_with_quality({
    points_count: 1000,
    interval: 60,  // 1分钟间隔
    missing_data_percentage: 0.01,  // 1%缺失数据
    outlier_percentage: 0.02,        // 2%异常值
    delay_percentage: 0.05           // 5%延迟数据
  })
  
  let poor_quality_data = generate_time_series_with_quality({
    points_count: 1000,
    interval: 60,
    missing_data_percentage: 0.15,  // 15%缺失数据
    outlier_percentage: 0.10,        // 10%异常值
    delay_percentage: 0.20           // 20%延迟数据
  })
  
  // 测试高质量数据
  let good_quality_result = quality_monitor.assess_data_quality(good_quality_data)
  assert_true(good_quality_result.overall_score > 0.8)
  assert_eq(good_quality_result.grade, "A")
  assert_false(good_quality_result.has_critical_issues)
  
  // 验证各项检查结果
  let completeness_score = good_quality_result.scores.get("completeness")
  let timeliness_score = good_quality_result.scores.get("timeliness")
  let accuracy_score = good_quality_result.scores.get("accuracy")
  let consistency_score = good_quality_result.scores.get("consistency")
  
  assert_true(completeness_score > 0.9)
  assert_true(timeliness_score > 0.9)
  assert_true(accuracy_score > 0.9)
  assert_true(consistency_score > 0.9)
  
  // 测试低质量数据
  let poor_quality_result = quality_monitor.assess_data_quality(poor_quality_data)
  assert_true(poor_quality_result.overall_score < 0.7)
  assert_true(["C", "D", "F"].contains(poor_quality_result.grade))
  assert_true(poor_quality_result.has_critical_issues)
  
  // 验证质量问题
  assert_true(poor_quality_result.issues.length() > 0)
  
  let completeness_issues = poor_quality_result.issues.filter(fn(issue) { 
    issue.category == "completeness" 
  })
  assert_true(completeness_issues.length() > 0)
  
  // 测试实时质量监控
  quality_monitor.enable_realtime_monitoring({
    data_source: "telemetry_stream",
    alert_channels: ["email", "slack"]
  })
  
  // 模拟实时数据流
  for i in 0..=100 {
    let data_point = {
      timestamp: Time::now(),
      metric: "test.metric",
      value: 50.0 + (i % 20) * 2.0,
      tags: [("host", "test-server")]
    }
    
    let realtime_result = quality_monitor.process_realtime_data(data_point)
    assert_true(realtime_result.success)
    
    Time::sleep(100)
  }
  
  // 检查实时监控结果
  let realtime_stats = quality_monitor.get_realtime_stats()
  assert_true(realtime_stats.total_points_processed > 0)
  assert_true(realtime_stats.quality_score >= 0.0)
  
  // 测试质量趋势分析
  let quality_history = []
  for i in 0..=7 {  // 8天的质量数据
    let daily_data = generate_time_series_with_quality({
      points_count: 1440,  // 24小时，每分钟一个点
      interval: 60,
      missing_data_percentage: 0.01 + i * 0.02,  // 质量逐渐下降
      outlier_percentage: 0.02,
      delay_percentage: 0.05
    })
    
    let daily_quality = quality_monitor.assess_data_quality(daily_data)
    quality_history = quality_history.push({
      date: "2022-01-" + (i + 1).to_string(),
      score: daily_quality.overall_score,
      grade: daily_quality.grade
    })
  }
  
  let trend_analysis = quality_monitor.analyze_quality_trend(quality_history)
  assert_true(trend_analysis.trend_direction == "declining")  // 质量下降趋势
  assert_true(trend_analysis.correlation < 0)
  
  // 测试质量改进建议
  let improvement_recommendations = quality_monitor.get_improvement_recommendations(poor_quality_result)
  assert_true(improvement_recommendations.length() > 0)
  
  let completeness_recommendation = improvement_recommendations.find(fn(rec) { 
    rec.category == "completeness" 
  })
  assert_true(completeness_recommendation != None)
  
  match completeness_recommendation {
    Some(recommendation) => {
      assert_true(recommendation.priority == "high")
      assert_true(recommendation.description.contains("数据完整性"))
    }
    None => assert_true(false)
  }
  
  // 测试质量报告生成
  let quality_report = quality_monitor.generate_quality_report({
    period: "24h",
    format: "json",
    include_details: true,
    include_recommendations: true
  })
  
  assert_true(quality_report.success)
  assert_true(quality_report.report_size > 0)
  
  // 测试质量阈值告警
  quality_monitor.set_quality_threshold({
    metric: "overall_score",
    threshold: 0.7,
    operator: "less_than",
    severity: "critical",
    action: "alert"
  })
  
  // 触发阈值告警
  let alert_data = generate_time_series_with_quality({
    points_count: 100,
    interval: 60,
    missing_data_percentage: 0.30,  // 30%缺失数据，触发告警
    outlier_percentage: 0.10,
    delay_percentage: 0.20
  })
  
  let alert_result = quality_monitor.assess_data_quality(alert_data)
  assert_true(alert_result.overall_score < 0.7)
  
  let alert_triggered = quality_monitor.check_threshold_alerts(alert_result)
  assert_true(alert_triggered)
}

// 辅助函数：生成测试数据
fn generate_high_frequency_time_series(count: Int) -> Array[TimeSeriesPoint] {
  let mut data = []
  let base_time = 1640995200
  
  for i in 0..=count - 1 {
    data = data.push({
      timestamp: base_time + i * 60,  // 每分钟一个点
      metric: "cpu.usage",
      value: 50.0 + (i % 20) * 2.0 + (Math::random() - 0.5) * 5.0,
      tags: [("host", "server-" + (i % 5).to_string())]
    })
  }
  
  data
}

fn calculate_average(values: Array[Float]) -> Float {
  let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
  sum / values.length().to_float()
}

fn add_anomalies_to_series(data: Array[TimeSeriesPoint], anomalies: Array[AnomalySpec]) -> Array[TimeSeriesPoint] {
  let mut result = data
  
  for anomaly in anomalies {
    match anomaly.type {
      "spike" => {
        let point = result[anomaly.index]
        result[anomaly.index] = { point | value: point.value * anomaly.magnitude }
      }
      "drop" => {
        let point = result[anomaly.index]
        result[anomaly.index] = { point | value: point.value + anomaly.magnitude }
      }
      "trend_change" => {
        for i in anomaly.index..=anomaly.index + anomaly.duration - 1 {
          if i < result.length() {
            let point = result[i]
            result[i] = { point | value: point.value + 10.0 }
          }
        }
      }
      _ => {}
    }
  }
  
  result
}

// 更多辅助函数...（由于篇幅限制，这里省略其他辅助函数的实现）