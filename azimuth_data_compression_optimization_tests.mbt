// Azimuth Data Compression and Optimization Tests
// 遥测数据压缩和优化测试用例 - 专注于数据压缩、存储优化和传输效率

// Test 1: 遥测数据压缩算法比较
test "telemetry data compression algorithms comparison" {
  // 创建测试数据集
  let test_data = []
  let base_time = Time::now()
  
  // 生成多样化的遥测数据
  for i in 0..<1000 {
    let timestamp = base_time + i * 1000
    let service = ["web", "api", "database", "cache"][i % 4]
    let instance = ["server-01", "server-02", "server-03"][i % 3]
    
    // 度量数据
    let cpu_usage = 20.0 + (i % 80).to_float() + Math::random() * 10.0
    let memory_usage = 30.0 + (i % 60).to_float() + Math::random() * 15.0
    let request_count = 100 + (i % 500) + Math::random() * 100.0
    let error_count = (i % 20).to_float()
    
    let data_point = TelemetryDataPoint::new(timestamp)
    TelemetryDataPoint::add_tag(data_point, "service", service)
    TelemetryDataPoint::add_tag(data_point, "instance", instance)
    TelemetryDataPoint::add_field(data_point, "cpu_usage", cpu_usage)
    TelemetryDataPoint::add_field(data_point, "memory_usage", memory_usage)
    TelemetryDataPoint::add_field(data_point, "request_count", request_count)
    TelemetryDataPoint::add_field(data_point, "error_count", error_count)
    
    test_data = test_data.push(data_point)
  }
  
  // 序列化原始数据
  let serializer = TelemetrySerializer::new()
  let serialized_data = Serializer::serialize(serializer, test_data)
  let original_size = serialized_data.length()
  
  // 测试不同压缩算法
  let compression_algorithms = [
    CompressionAlgorithm::Gzip,
    CompressionAlgorithm::LZ4,
    CompressionAlgorithm::Snappy,
    CompressionAlgorithm::Zstandard
  ]
  
  let compression_results = []
  
  for algorithm in compression_algorithms {
    let compressor = Compressor::new(algorithm)
    
    // 压缩数据
    let start_time = Time::now()
    let compressed_data = Compressor::compress(compressor, serialized_data)
    let compression_time = Time::now() - start_time
    
    // 解压数据
    let decompress_start = Time::now()
    let decompressed_data = Compressor::decompress(compressor, compressed_data)
    let decompression_time = Time::now() - decompress_start
    
    // 验证数据完整性
    assert_eq(decompressed_data, serialized_data)
    
    // 计算压缩指标
    let compressed_size = compressed_data.length()
    let compression_ratio = original_size.to_float() / compressed_size.to_float()
    let space_saving = (1.0 - compressed_size.to_float() / original_size.to_float()) * 100.0
    
    let result = CompressionResult::new(
      algorithm = algorithm,
      original_size = original_size,
      compressed_size = compressed_size,
      compression_ratio = compression_ratio,
      space_saving_percent = space_saving,
      compression_time_ms = compression_time,
      decompression_time_ms = decompression_time
    )
    
    compression_results = compression_results.push(result)
  }
  
  // 验证压缩结果
  for result in compression_results {
    assert_true(result.compressed_size < result.original_size)
    assert_true(result.compression_ratio > 1.0)
    assert_true(result.space_saving_percent > 0.0)
    assert_true(result.compression_time_ms > 0)
    assert_true(result.decompression_time_ms > 0)
  }
  
  // 找出最佳压缩算法
  let best_compression = ArrayUtils::max_by(compression_results, fn(r) { r.space_saving_percent })
  let fastest_compression = ArrayUtils::min_by(compression_results, fn(r) { r.compression_time_ms })
  let fastest_decompression = ArrayUtils::min_by(compression_results, fn(r) { r.decompression_time_ms })
  
  assert_true(best_compression.space_saving_percent >= 0.0)
  assert_true(fastest_compression.compression_time_ms >= 0.0)
  assert_true(fastest_decompression.decompression_time_ms >= 0.0)
}

// Test 2: 自适应压缩策略
test "adaptive compression strategies" {
  // 创建自适应压缩管理器
  let adaptive_compressor = AdaptiveCompressionManager::new()
  
  // 配置压缩策略
  let strategies = [
    CompressionStrategy::new("high_compression", CompressionAlgorithm::Zstandard)
      .with_level(9)
      .with_condition(Condition::NetworkLatencyGreaterThan(100))
      .with_condition(Condition::DataSizeGreaterThan(10000)),
    
    CompressionStrategy::new("balanced", CompressionAlgorithm::LZ4)
      .with_level(5)
      .with_condition(Condition::NetworkLatencyBetween(10, 100))
      .with_condition(Condition::DataSizeBetween(1000, 10000)),
    
    CompressionStrategy::new("fast", CompressionAlgorithm::Snappy)
      .with_level(1)
      .with_condition(Condition::NetworkLatencyLessThan(10))
      .with_condition(Condition::DataSizeLessThan(1000))
  ]
  
  for strategy in strategies {
    AdaptiveCompressor::add_strategy(adaptive_compressor, strategy)
  }
  
  // 创建不同大小和类型的数据集
  let small_data = "small telemetry data".to_bytes()
  let medium_data = "medium telemetry data with more content".to_bytes()
  let large_data = "large telemetry data with significantly more content to test compression effectiveness".to_bytes() * 100
  
  let test_datasets = [
    ("small", small_data),
    ("medium", medium_data),
    ("large", large_data)
  ]
  
  // 模拟不同网络条件
  let network_conditions = [
    ("slow", NetworkCondition::new(latency_ms = 200, bandwidth_kbps = 100)),
    ("normal", NetworkCondition::new(latency_ms = 50, bandwidth_kbps = 1000)),
    ("fast", NetworkCondition::new(latency_ms = 5, bandwidth_kbps = 10000))
  ]
  
  // 测试自适应压缩
  for (data_name, data) in test_datasets {
    for (network_name, network) in network_conditions {
      // 设置网络条件
      AdaptiveCompressor::set_network_condition(adaptive_compressor, network)
      
      // 选择压缩策略
      let selected_strategy = AdaptiveCompressor::select_strategy(adaptive_compressor, data)
      assert_true(selected_strategy !== None)
      
      // 应用压缩
      let strategy = selected_strategy.unwrap()
      let compressor = Compressor::new(strategy.algorithm)
      Compressor::set_level(compressor, strategy.level)
      
      let compressed_data = Compressor::compress(compressor, data)
      let decompressed_data = Compressor::decompress(compressor, compressed_data)
      
      // 验证数据完整性
      assert_eq(decompressed_data, data)
      
      // 记录压缩统计
      AdaptiveCompressor::record_compression_stats(adaptive_compressor, CompressionStats::new(
        strategy_name = strategy.name,
        original_size = data.length(),
        compressed_size = compressed_data.length(),
        compression_time_ms = 0, // 实际应用中应该测量
        network_latency_ms = network.latency_ms
      ))
    }
  }
  
  // 验证自适应压缩统计
  let compression_stats = AdaptiveCompressor::get_compression_stats(adaptive_compressor)
  assert_true(compression_stats.total_compressions > 0)
  
  // 验证策略选择效果
  let strategy_effectiveness = AdaptiveCompressor::analyze_strategy_effectiveness(adaptive_compressor)
  for (strategy_name, effectiveness) in strategy_effectiveness {
    assert_true(effectiveness.average_compression_ratio > 1.0)
    assert_true(effectiveness.average_compression_time_ms >= 0)
  }
}

// Test 3: 增量数据压缩
test "incremental data compression" {
  // 创建增量压缩管理器
  let incremental_compressor = IncrementalCompressor::new()
  
  // 配置增量压缩参数
  IncrementalCompressor::set_window_size(incremental_compressor, 100) // 100个数据点窗口
  IncrementalCompressor::set_delta_threshold(incremental_compressor, 0.01) // 1%变化阈值
  
  // 生成时间序列数据
  let base_time = Time::now()
  let time_series_data = []
  
  for i in 0..<500 {
    let timestamp = base_time + i * 60000 // 每分钟一个数据点
    
    // 生成带有趋势和季节性的数据
    let trend = i.to_float() * 0.1
    let seasonal = Math::sin(i.to_float() * 0.1) * 5.0
    let noise = Math::random() * 2.0
    let value = 50.0 + trend + seasonal + noise
    
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    time_series_data = time_series_data.push(data_point)
  }
  
  // 分批处理数据
  let batch_size = 50
  let compression_results = []
  
  for i in 0..<(time_series_data.length() / batch_size) {
    let start_index = i * batch_size
    let end_index = start_index + batch_size
    let batch = ArrayUtils::slice(time_series_data, start_index, end_index)
    
    // 压缩批次数据
    let compression_result = IncrementalCompressor::compress_batch(incremental_compressor, batch)
    compression_results = compression_results.push(compression_result)
    
    // 验证压缩结果
    assert_true(compression_result.compressed_data.length() > 0)
    assert_true(compression_result.compression_ratio > 1.0)
  }
  
  // 测试增量解压
  let decompressed_data = []
  for result in compression_results {
    let batch = IncrementalCompressor::decompress_batch(incremental_compressor, result.compressed_data)
    decompressed_data = ArrayUtils::concat(decompressed_data, batch)
  }
  
  // 验证解压数据完整性
  assert_eq(decompressed_data.length(), time_series_data.length())
  for i in 0..<decompressed_data.length() {
    assert_eq(decompressed_data[i].timestamp, time_series_data[i].timestamp)
    assert_true(Math::abs(decompressed_data[i].value - time_series_data[i].value) < 0.001)
  }
  
  // 测试增量压缩效率
  let total_original_size = IncrementalCompressor::calculate_original_size(incremental_compressor, time_series_data)
  let total_compressed_size = ArrayUtils::sum(compression_results.map(fn(r) { r.compressed_data.length() }))
  let overall_compression_ratio = total_original_size.to_float() / total_compressed_size.to_float()
  
  assert_true(overall_compression_ratio > 1.0)
  
  // 测试增量更新
  let new_data_points = []
  for i in 500..<520 {
    let timestamp = base_time + i * 60000
    let value = 50.0 + i.to_float() * 0.1 + Math::sin(i.to_float() * 0.1) * 5.0
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    new_data_points = new_data_points.push(data_point)
  }
  
  // 增量压缩新数据
  let incremental_result = IncrementalCompressor::compress_incremental(incremental_compressor, new_data_points)
  assert_true(incremental_result.compressed_data.length() > 0)
  assert_true(incremental_result.compression_ratio > 1.0)
  
  // 验证增量压缩效果
  let regular_compression = IncrementalCompressor::compress_batch(incremental_compressor, new_data_points)
  assert_true(incremental_result.compressed_data.length() <= regular_compression.compressed_data.length())
}

// Test 4: 智能采样和降采样
test "intelligent sampling and downsampling" {
  // 创建智能采样管理器
  let sampling_manager = IntelligentSamplingManager::new()
  
  // 配置采样策略
  let sampling_strategies = [
    SamplingStrategy::new("uniform", SamplingType::Uniform)
      .with_rate(0.1) // 10% 采样率
      .with_condition(Condition::Always()),
    
    SamplingStrategy::new("adaptive", SamplingType::Adaptive)
      .with_base_rate(0.05) // 基础5%采样率
      .with_max_rate(0.5) // 最大50%采样率
      .with_condition(Condition::MetricVarianceGreaterThan("value", 1.0)),
    
    SamplingStrategy::new("priority", SamplingType::Priority)
      .with_priority_attributes(["error", "critical"])
      .with_base_rate(0.2) // 基础20%采样率
      .with_high_priority_rate(1.0) // 高优先级100%采样率
  ]
  
  for strategy in sampling_strategies {
    SamplingManager::add_strategy(sampling_manager, strategy)
  }
  
  // 生成测试数据
  let test_data = []
  let base_time = Time::now()
  
  for i in 0..<10000 {
    let timestamp = base_time + i * 1000
    
    // 生成不同特征的数据
    let value = if i % 100 == 0 {
      100.0 + Math::random() * 50.0 // 异常值
    } else if i % 10 == 0 {
      50.0 + Math::random() * 10.0 // 中等变化
    } else {
      25.0 + Math::random() * 2.0 // 小变化
    }
    
    let priority = if i % 100 == 0 { "critical" } else if i % 10 == 0 { "error" } else { "normal" }
    
    let data_point = TelemetryDataPoint::new(timestamp)
    TelemetryDataPoint::add_field(data_point, "value", value)
    TelemetryDataPoint::add_tag(data_point, "priority", priority)
    TelemetryDataPoint::add_tag(data_point, "service", "test-service")
    
    test_data = test_data.push(data_point)
  }
  
  // 测试均匀采样
  let uniform_strategy = SamplingManager::get_strategy(sampling_manager, "uniform").unwrap()
  let uniform_sampled = SamplingManager::apply_strategy(sampling_manager, test_data, uniform_strategy)
  
  assert_true(uniform_sampled.length() < test_data.length())
  let uniform_rate = uniform_sampled.length().to_float() / test_data.length().to_float()
  assert_true(Math::abs(uniform_rate - 0.1) < 0.05) // 允许5%误差
  
  // 测试自适应采样
  let adaptive_strategy = SamplingManager::get_strategy(sampling_manager, "adaptive").unwrap()
  let adaptive_sampled = SamplingManager::apply_strategy(sampling_manager, test_data, adaptive_strategy)
  
  assert_true(adaptive_sampled.length() < test_data.length())
  // 自适应采样应该保留更多的异常值
  let adaptive_critical_count = ArrayUtils::count(adaptive_sampled, fn(dp) {
    match TelemetryDataPoint::get_tag(dp, "priority") {
      Some(p) => p == "critical"
      None => false
    }
  })
  let uniform_critical_count = ArrayUtils::count(uniform_sampled, fn(dp) {
    match TelemetryDataPoint::get_tag(dp, "priority") {
      Some(p) => p == "critical"
      None => false
    }
  })
  assert_true(adaptive_critical_count >= uniform_critical_count)
  
  // 测试优先级采样
  let priority_strategy = SamplingManager::get_strategy(sampling_manager, "priority").unwrap()
  let priority_sampled = SamplingManager::apply_strategy(sampling_manager, test_data, priority_strategy)
  
  // 验证优先级采样效果
  let priority_critical = ArrayUtils::filter(priority_sampled, fn(dp) {
    match TelemetryDataPoint::get_tag(dp, "priority") {
      Some(p) => p == "critical"
      None => false
    }
  })
  
  let original_critical = ArrayUtils::filter(test_data, fn(dp) {
    match TelemetryDataPoint::get_tag(dp, "priority") {
      Some(p) => p == "critical"
      None => false
    }
  })
  
  assert_eq(priority_critical.length(), original_critical.length()) // 关键数据应该全部保留
  
  // 测试时间序列降采样
  let downsampler = TimeSeriesDownsampler::new()
  DownSampler::set_target_rate(downsampler, 60) // 每分钟一个数据点
  
  let downsampled_data = DownSampler::downsample(downsampler, test_data)
  assert_true(downsampled_data.length() < test_data.length())
  
  // 验证降采样数据的时间分布
  let time_intervals = []
  for i in 1..<downsampled_data.length() {
    let interval = downsampled_data[i].timestamp - downsampled_data[i-1].timestamp
    time_intervals = time_intervals.push(interval)
  }
  
  let avg_interval = ArrayUtils::sum(time_intervals).to_float() / time_intervals.length().to_float()
  assert_true(Math::abs(avg_interval - 60000.0) < 5000.0) // 允许5秒误差
  
  // 测试聚合降采样
  let aggregation_downsampler = AggregationDownsampler::new()
  DownSampler::set_target_rate(aggregation_downsampler, 300) // 每5分钟一个数据点
  DownSampler::set_aggregation_method(aggregation_downsampler, AggregationMethod::Average)
  
  let aggregated_data = DownSampler::downsample(aggregation_downsampler, test_data)
  assert_true(aggregated_data.length() < downsampled_data.length())
  
  // 验证聚合结果
  for data_point in aggregated_data {
    assert_true(data_point.aggregated)
    assert_true(data_point.aggregation_method == AggregationMethod::Average)
    assert_true(data_point.source_count > 1)
  }
}

// Test 5: 数据存储优化
test "data storage optimization" {
  // 创建存储优化管理器
  let storage_optimizer = StorageOptimizer::new()
  
  // 配置存储策略
  let storage_strategies = [
    StorageStrategy::new("hot_data", StorageTier::Memory)
      .with_retention_period(3600) // 1小时
      .with_compression_enabled(false)
      .with_indexing_enabled(true),
    
    StorageStrategy::new("warm_data", StorageTier::SSD)
      .with_retention_period(86400 * 7) // 7天
      .with_compression_enabled(true)
      .with_compression_algorithm(CompressionAlgorithm::LZ4)
      .with_indexing_enabled(true),
    
    StorageStrategy::new("cold_data", StorageTier::HDD)
      .with_retention_period(86400 * 30) // 30天
      .with_compression_enabled(true)
      .with_compression_algorithm(CompressionAlgorithm::Zstandard)
      .with_indexing_enabled(false)
  ]
  
  for strategy in storage_strategies {
    StorageOptimizer::add_strategy(storage_optimizer, strategy)
  }
  
  // 生成测试数据
  let test_data = []
  let base_time = Time::now() - 86400 * 10 // 10天前开始
  
  for i in 0..<10000 {
    let timestamp = base_time + i * 60000 // 每分钟一个数据点
    let age_hours = (Time::now() - timestamp) / 3600
    
    let data_point = TelemetryDataPoint::new(timestamp)
    TelemetryDataPoint::add_field(data_point, "cpu_usage", 50.0 + Math::random() * 30.0)
    TelemetryDataPoint::add_field(data_point, "memory_usage", 60.0 + Math::random() * 20.0)
    TelemetryDataPoint::add_tag(data_point, "service", "test-service")
    TelemetryDataPoint::add_tag(data_point, "instance", "test-instance")
    TelemetryDataPoint::add_metadata(data_point, "age_hours", age_hours)
    
    test_data = test_data.push(data_point)
  }
  
  // 应用存储优化
  let optimization_result = StorageOptimizer::optimize_storage(storage_optimizer, test_data)
  
  // 验证存储分层
  assert_eq(optimization_result.tiers.length(), 3)
  
  for tier in optimization_result.tiers {
    match tier.storage_type {
      StorageTier::Memory => {
        // 热数据应该是最近的数据
        for data_point in tier.data_points {
          let age_hours = Time::now() - data_point.timestamp / 3600
          assert_true(age_hours <= 1)
        }
        assert_false(tier.compressed)
        assert_true(tier.indexed)
      }
      StorageTier::SSD => {
        // 温数据应该是1小时到7天的数据
        for data_point in tier.data_points {
          let age_hours = Time::now() - data_point.timestamp / 3600
          assert_true(age_hours > 1 && age_hours <= 168) // 168小时 = 7天
        }
        assert_true(tier.compressed)
        assert_true(tier.indexed)
      }
      StorageTier::HDD => {
        // 冷数据应该是7天到30天的数据
        for data_point in tier.data_points {
          let age_hours = Time::now() - data_point.timestamp / 3600
          assert_true(age_hours > 168 && age_hours <= 720) // 720小时 = 30天
        }
        assert_true(tier.compressed)
        assert_false(tier.indexed)
      }
    }
  }
  
  // 验证存储空间节省
  let original_size = StorageOptimizer::calculate_original_size(storage_optimizer, test_data)
  let optimized_size = StorageOptimizer::calculate_optimized_size(storage_optimizer, optimization_result)
  let space_saving = (1.0 - optimized_size.to_float() / original_size.to_float()) * 100.0
  
  assert_true(space_saving > 0.0)
  
  // 测试数据生命周期管理
  let lifecycle_manager = DataLifecycleManager::new()
  LifecycleManager::set_retention_policy(lifecycle_manager, RetentionPolicy::new(
    hot_retention_hours = 1,
    warm_retention_days = 7,
    cold_retention_days = 30,
    archive_retention_days = 365
  ))
  
  // 模拟时间推进
  let time_advanced_data = []
  for data_point in test_data {
    let advanced_point = TelemetryDataPoint::clone(data_point)
    advanced_point.timestamp = data_point.timestamp - 86400 * 5 // 推进5天
    time_advanced_data = time_advanced_data.push(advanced_point)
  }
  
  // 应用生命周期管理
  let lifecycle_result = LifecycleManager::apply_retention_policy(lifecycle_manager, time_advanced_data)
  
  // 验证数据迁移
  assert_true(lifecycle_result.data_migrated > 0)
  assert_true(lifecycle_result.data_archived > 0)
  assert_true(lifecycle_result.data_deleted >= 0) // 可能没有数据被删除
  
  // 测试存储索引优化
  let index_optimizer = StorageIndexOptimizer::new()
  
  // 创建索引
  let index_fields = ["service", "instance", "timestamp"]
  for field in index_fields {
    IndexOptimizer::create_index(index_optimizer, field)
  }
  
  // 优化索引
  let index_optimization_result = IndexOptimizer::optimize_indexes(index_optimizer, optimization_result)
  
  assert_true(index_optimization_result.indexes_created > 0)
  assert_true(index_optimization_result.indexes_optimized > 0)
  
  // 测试查询性能
  let query_engine = StorageQueryEngine::new()
  QueryEngine::set_storage_layout(query_engine, optimization_result)
  QueryEngine::set_indexes(query_engine, index_optimization_result.indexes)
  
  // 执行测试查询
  let query_start = Time::now()
  let query_result = QueryEngine::execute(query_engine, "SELECT * FROM telemetry WHERE service = 'test-service' AND timestamp > " + (base_time + 86400 * 5).to_string())
  let query_time = Time::now() - query_start
  
  assert_true(query_result.rows.length() > 0)
  assert_true(query_time < 100) // 查询应该在100ms内完成
}