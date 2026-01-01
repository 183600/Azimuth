// 遥测数据压缩和优化测试
// 测试遥测数据的压缩算法、优化策略和存储效率

pub test "遥测数据基础压缩测试" {
  // 创建压缩管理器
  let compression_manager = azimuth::CompressionManager::new()
  
  // 创建测试遥测数据
  let trace_data = azimuth::TraceData::new("compression-test", "compression-operation")
  let trace_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(trace_attributes, "service.name", azimuth::StringValue("compression-test-service"))
  azimuth::Attributes::set(trace_attributes, "operation.name", azimuth::StringValue("compression-operation"))
  azimuth::Attributes::set(trace_attributes, "user.id", azimuth::StringValue("user-12345"))
  azimuth::Attributes::set(trace_attributes, "request.id", azimuth::StringValue("req-67890"))
  azimuth::Attributes::set(trace_attributes, "environment", azimuth::StringValue("production"))
  
  // 压缩前数据大小
  let original_size = azimuth::CompressionManager::calculate_data_size(compression_manager, trace_data)
  
  // 使用不同压缩算法进行压缩
  let gzip_compressed = azimuth::CompressionManager::compress_with_gzip(compression_manager, trace_data)
  let lz4_compressed = azimuth::CompressionManager::compress_with_lz4(compression_manager, trace_data)
  let zstd_compressed = azimuth::CompressionManager::compress_with_zstd(compression_manager, trace_data)
  
  // 获取压缩后大小
  let gzip_size = azimuth::CompressionManager::compressed_size(gzip_compressed)
  let lz4_size = azimuth::CompressionManager::compressed_size(lz4_compressed)
  let zstd_size = azimuth::CompressionManager::compressed_size(zstd_compressed)
  
  // 验证压缩效果
  assert_true(gzip_size < original_size)
  assert_true(lz4_size < original_size)
  assert_true(zstd_size < original_size)
  
  // 计算压缩比
  let gzip_ratio = original_size.to_double() / gzip_size.to_double()
  let lz4_ratio = original_size.to_double() / lz4_size.to_double()
  let zstd_ratio = original_size.to_double() / zstd_size.to_double()
  
  // 验证压缩比合理性
  assert_true(gzip_ratio > 1.0)
  assert_true(lz4_ratio > 1.0)
  assert_true(zstd_ratio > 1.0)
  
  // 测试解压缩正确性
  let gzip_decompressed = azimuth::CompressionManager::decompress_gzip(compression_manager, gzip_compressed)
  let lz4_decompressed = azimuth::CompressionManager::decompress_lz4(compression_manager, lz4_compressed)
  let zstd_decompressed = azimuth::CompressionManager::decompress_zstd(compression_manager, zstd_compressed)
  
  // 验证解压缩后数据完整性
  assert_true(azimuth::CompressionManager::verify_data_integrity(compression_manager, trace_data, gzip_decompressed))
  assert_true(azimuth::CompressionManager::verify_data_integrity(compression_manager, trace_data, lz4_decompressed))
  assert_true(azimuth::CompressionManager::verify_data_integrity(compression_manager, trace_data, zstd_decompressed))
}

pub test "遥测数据批量压缩优化测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 创建大量遥测数据用于批量压缩
  let telemetry_batch = []
  
  for i in 0..100 {
    let trace_data = azimuth::TraceData::new("batch-compression-test", "batch-operation-" + i.to_string())
    let trace_attributes = azimuth::Attributes::new()
    azimuth::Attributes::set(trace_attributes, "service.name", azimuth::StringValue("batch-service"))
    azimuth::Attributes::set(trace_attributes, "batch.id", azimuth::StringValue("batch-" + (i / 10).to_string()))
    azimuth::Attributes::set(trace_attributes, "operation.id", azimuth::StringValue(i.to_string()))
    
    telemetry_batch.push(trace_data)
  }
  
  // 计算原始数据总大小
  let original_batch_size = azimuth::CompressionManager::calculate_batch_size(compression_manager, telemetry_batch)
  
  // 单独压缩每个数据项
  let individual_compressed = []
  let individual_total_size = 0
  
  for data in telemetry_batch {
    let compressed = azimuth::CompressionManager::compress_with_gzip(compression_manager, data)
    individual_compressed.push(compressed)
    individual_total_size = individual_total_size + azimuth::CompressionManager::compressed_size(compressed)
  }
  
  // 批量压缩所有数据
  let batch_compressed = azimuth::CompressionManager::compress_batch_gzip(compression_manager, telemetry_batch)
  let batch_compressed_size = azimuth::CompressionManager::compressed_size(batch_compressed)
  
  // 验证批量压缩效率
  assert_true(batch_compressed_size < individual_total_size)
  
  // 计算压缩比提升
  let individual_ratio = original_batch_size.to_double() / individual_total_size.to_double()
  let batch_ratio = original_batch_size.to_double() / batch_compressed_size.to_double()
  
  // 批量压缩应该有更好的压缩比
  assert_true(batch_ratio >= individual_ratio)
  
  // 测试批量解压缩
  let batch_decompressed = azimuth::CompressionManager::decompress_batch_gzip(compression_manager, batch_compressed)
  
  // 验证批量解压缩后数据完整性
  assert_true(azimuth::CompressionManager::verify_batch_integrity(compression_manager, telemetry_batch, batch_decompressed))
}

pub test "遥测数据增量压缩测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 创建基础遥测数据
  let base_trace = azimuth::TraceData::new("incremental-test", "base-operation")
  let base_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(base_attributes, "service.name", azimuth::StringValue("incremental-service"))
  azimuth::Attributes::set(base_attributes, "version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(base_attributes, "environment", azimuth::StringValue("production"))
  
  // 压缩基础数据
  let base_compressed = azimuth::CompressionManager::compress_with_delta(compression_manager, base_trace)
  
  // 创建增量数据（大部分属性相同，只有少量变化）
  let incremental_trace = azimuth::TraceData::new("incremental-test", "incremental-operation")
  let incremental_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(incremental_attributes, "service.name", azimuth::StringValue("incremental-service"))
  azimuth::Attributes::set(incremental_attributes, "version", azimuth::StringValue("1.0.1"))  // 版本变化
  azimuth::Attributes::set(incremental_attributes, "environment", azimuth::StringValue("production"))
  azimuth::Attributes::set(incremental_attributes, "feature.flag", azimuth::StringValue("enabled"))  // 新增属性
  
  // 基于基础数据进行增量压缩
  let incremental_compressed = azimuth::CompressionManager::compress_incremental(
    compression_manager, 
    incremental_trace, 
    base_compressed
  )
  
  // 计算压缩效率
  let base_size = azimuth::CompressionManager::compressed_size(base_compressed)
  let incremental_size = azimuth::CompressionManager::compressed_size(incremental_compressed)
  let full_incremental_size = azimuth::CompressionManager::calculate_data_size(compression_manager, incremental_trace)
  
  // 增量压缩应该比完整压缩更小
  assert_true(incremental_size < full_incremental_size)
  
  // 测试增量解压缩
  let incremental_decompressed = azimuth::CompressionManager::decompress_incremental(
    compression_manager, 
    incremental_compressed, 
    base_compressed
  )
  
  // 验证增量解压缩正确性
  assert_true(azimuth::CompressionManager::verify_data_integrity(
    compression_manager, 
    incremental_trace, 
    incremental_decompressed
  ))
  
  // 测试多级增量压缩
  let second_incremental_trace = azimuth::TraceData::new("incremental-test", "second-incremental-operation")
  let second_incremental_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(second_incremental_attributes, "service.name", azimuth::StringValue("incremental-service"))
  azimuth::Attributes::set(second_incremental_attributes, "version", azimuth::StringValue("1.0.2"))
  azimuth::Attributes::set(second_incremental_attributes, "environment", azimuth::StringValue("staging"))  // 环境变化
  azimuth::Attributes::set(second_incremental_attributes, "feature.flag", azimuth::StringValue("enabled"))
  
  let second_incremental_compressed = azimuth::CompressionManager::compress_incremental(
    compression_manager, 
    second_incremental_trace, 
    incremental_compressed
  )
  
  let second_incremental_decompressed = azimuth::CompressionManager::decompress_incremental(
    compression_manager, 
    second_incremental_compressed, 
    incremental_compressed
  )
  
  // 验证多级增量压缩正确性
  assert_true(azimuth::CompressionManager::verify_data_integrity(
    compression_manager, 
    second_incremental_trace, 
    second_incremental_decompressed
  ))
}

pub test "遥测数据智能压缩策略测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 启用智能压缩策略
  azimuth::CompressionManager::enable_smart_compression(compression_manager, true)
  
  // 创建不同类型和大小的遥测数据
  let small_trace = azimuth::TraceData::new("smart-test", "small-operation")
  let small_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(small_attributes, "key", azimuth::StringValue("value"))
  
  let medium_trace = azimuth::TraceData::new("smart-test", "medium-operation")
  let medium_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(medium_attributes, "service.name", azimuth::StringValue("medium-service"))
  azimuth::Attributes::set(medium_attributes, "operation.name", azimuth::StringValue("medium-operation"))
  azimuth::Attributes::set(medium_attributes, "user.id", azimuth::StringValue("user-12345"))
  
  let large_trace = azimuth::TraceData::new("smart-test", "large-operation")
  let large_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(large_attributes, "service.name", azimuth::StringValue("large-service"))
  azimuth::Attributes::set(large_attributes, "operation.name", azimuth::StringValue("large-operation"))
  azimuth::Attributes::set(large_attributes, "user.id", azimuth::StringValue("user-12345"))
  azimuth::Attributes::set(large_attributes, "request.id", azimuth::StringValue("req-67890"))
  azimuth::Attributes::set(large_attributes, "session.id", azimuth::StringValue("session-abcdef"))
  azimuth::Attributes::set(large_attributes, "trace.id", azimuth::StringValue("trace-123456789"))
  azimuth::Attributes::set(large_attributes, "span.id", azimuth::StringValue("span-987654321"))
  
  // 使用智能压缩策略压缩不同大小的数据
  let small_compressed = azimuth::CompressionManager::smart_compress(compression_manager, small_trace)
  let medium_compressed = azimuth::CompressionManager::smart_compress(compression_manager, medium_trace)
  let large_compressed = azimuth::CompressionManager::smart_compress(compression_manager, large_trace)
  
  // 获取使用的压缩算法
  let small_algorithm = azimuth::CompressionManager::get_compression_algorithm(compression_manager, small_compressed)
  let medium_algorithm = azimuth::CompressionManager::get_compression_algorithm(compression_manager, medium_compressed)
  let large_algorithm = azimuth::CompressionManager::get_compression_algorithm(compression_manager, large_compressed)
  
  // 验证智能策略选择
  // 小数据可能使用轻量级压缩或跳过压缩
  // 中等数据使用标准压缩
  // 大数据使用高压缩比算法
  
  let small_size = azimuth::CompressionManager::calculate_data_size(compression_manager, small_trace)
  let medium_size = azimuth::CompressionManager::calculate_data_size(compression_manager, medium_trace)
  let large_size = azimuth::CompressionManager::calculate_data_size(compression_manager, large_trace)
  
  let small_compressed_size = azimuth::CompressionManager::compressed_size(small_compressed)
  let medium_compressed_size = azimuth::CompressionManager::compressed_size(medium_compressed)
  let large_compressed_size = azimuth::CompressionManager::compressed_size(large_compressed)
  
  // 验证压缩效果
  let small_ratio = small_size.to_double() / small_compressed_size.to_double()
  let medium_ratio = medium_size.to_double() / medium_compressed_size.to_double()
  let large_ratio = large_size.to_double() / large_compressed_size.to_double()
  
  // 大数据应该有更好的压缩比
  assert_true(large_ratio >= medium_ratio)
  assert_true(medium_ratio >= small_ratio)
  
  // 测试压缩性能
  let small_time = azimuth::CompressionManager::compression_time(compression_manager, small_compressed)
  let medium_time = azimuth::CompressionManager::compression_time(compression_manager, medium_compressed)
  let large_time = azimuth::CompressionManager::compression_time(compression_manager, large_compressed)
  
  // 验证智能策略的性能权衡
  assert_true(small_time < medium_time)
  assert_true(medium_time < large_time)
}

pub test "遥测数据压缩缓存优化测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 启用压缩缓存
  azimuth::CompressionManager::enable_compression_cache(compression_manager, true)
  azimuth::CompressionManager::set_cache_size(compression_manager, 100)
  
  // 创建重复的遥测数据模式
  let repeated_patterns = []
  
  for i in 0..50 {
    // 创建重复模式的数据
    let pattern_id = i % 5  // 5种不同模式
    let trace_data = azimuth::TraceData::new("cache-test", "pattern-" + pattern_id.to_string())
    let trace_attributes = azimuth::Attributes::new()
    azimuth::Attributes::set(trace_attributes, "pattern.id", azimuth::StringValue(pattern_id.to_string()))
    azimuth::Attributes::set(trace_attributes, "iteration", azimuth::StringValue(i.to_string()))
    
    repeated_patterns.push(trace_data)
  }
  
  // 压缩数据并利用缓存
  let compression_times = []
  let cache_hit_rates = []
  
  for (i, data) in repeated_patterns.enumerate() {
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let compressed = azimuth::CompressionManager::compress_with_cache(compression_manager, data)
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    compression_times.push(end_time - start_time)
    
    // 获取缓存统计
    let cache_stats = azimuth::CompressionManager::get_cache_stats(compression_manager)
    cache_hit_rates.push(cache_stats.hit_rate)
  }
  
  // 验证缓存效果
  // 后续压缩应该更快（由于缓存命中）
  let early_avg_time = compression_times[0..10].reduce(0L, fn(acc, time) { acc + time }) / 10L
  let late_avg_time = compression_times[40..50].reduce(0L, fn(acc, time) { acc + time }) / 10L
  
  assert_true(late_avg_time <= early_avg_time)
  
  // 验证缓存命中率
  let final_hit_rate = cache_hit_rates[cache_hit_rates.length() - 1]
  assert_true(final_hit_rate > 0.5)  // 至少50%的缓存命中率
  
  // 测试缓存清理
  azimuth::CompressionManager::clear_cache(compression_manager)
  let cleared_stats = azimuth::CompressionManager::get_cache_stats(compression_manager)
  
  assert_eq(cleared_stats.hit_rate, 0.0)
  assert_eq(cleared_stats.cache_size, 0)
}

pub test "遥测数据压缩质量测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 创建不同质量的遥测数据
  let high_quality_trace = azimuth::TraceData::new("quality-test", "high-quality-operation")
  let high_quality_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(high_quality_attributes, "precision", azimuth::StringValue("high"))
  azimuth::Attributes::set(high_quality_attributes, "timestamp", azimuth::StringValue("1735689600000000000"))
  azimuth::Attributes::set(high_quality_attributes, "measurements", azimuth::StringValue("1.23456789012345"))
  
  let low_quality_trace = azimuth::TraceData::new("quality-test", "low-quality-operation")
  let low_quality_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(low_quality_attributes, "precision", azimuth::StringValue("low"))
  azimuth::Attributes::set(low_quality_attributes, "timestamp", azimuth::StringValue("1735689600"))
  azimuth::Attributes::set(low_quality_attributes, "measurements", azimuth::StringValue("1.23"))
  
  // 测试不同压缩级别
  let high_quality_levels = [1, 3, 6, 9]  // 从快到好
  let low_quality_levels = [1, 3, 6, 9]
  
  let high_quality_results = []
  let low_quality_results = []
  
  for level in high_quality_levels {
    let compressed = azimuth::CompressionManager::compress_with_level(
      compression_manager, 
      high_quality_trace, 
      level
    )
    let size = azimuth::CompressionManager::compressed_size(compressed)
    let time = azimuth::CompressionManager::compression_time(compression_manager, compressed)
    
    high_quality_results.push({ level, size, time })
  }
  
  for level in low_quality_levels {
    let compressed = azimuth::CompressionManager::compress_with_level(
      compression_manager, 
      low_quality_trace, 
      level
    )
    let size = azimuth::CompressionManager::compressed_size(compressed)
    let time = azimuth::CompressionManager::compression_time(compression_manager, compressed)
    
    low_quality_results.push({ level, size, time })
  }
  
  // 验证压缩级别影响
  // 高级别应该有更好的压缩比但更慢
  let high_fast = high_quality_results[0]
  let high_best = high_quality_results[3]
  
  assert_true(high_best.size < high_fast.size)
  assert_true(high_best.time > high_fast.time)
  
  // 测试压缩质量评估
  let high_quality_score = azimuth::CompressionManager::evaluate_compression_quality(
    compression_manager, 
    high_quality_trace, 
    high_best.size
  )
  let low_quality_score = azimuth::CompressionManager::evaluate_compression_quality(
    compression_manager, 
    low_quality_trace, 
    low_quality_results[3].size
  )
  
  // 验证质量评估
  assert_true(high_quality_score > 0.0)
  assert_true(low_quality_score > 0.0)
}

pub test "遥测数据压缩并发安全测试" {
  let compression_manager = azimuth::CompressionManager::new()
  
  // 启用并发安全压缩
  azimuth::CompressionManager::enable_concurrent_compression(compression_manager, true)
  
  // 创建多个并发压缩任务
  let concurrent_tasks = 10
  let items_per_task = 20
  
  let compression_results = []
  
  for task_id in 0..concurrent_tasks {
    let task_results = []
    
    for item_id in 0..items_per_task {
      let trace_data = azimuth::TraceData::new(
        "concurrent-compression-test", 
        "task-" + task_id.to_string + "-item-" + item_id.to_string
      )
      
      let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      let compressed = azimuth::CompressionManager::compress_concurrent_safe(compression_manager, trace_data)
      let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      
      task_results.push({
        task_id,
        item_id,
        compressed,
        compression_time: end_time - start_time
      })
    }
    
    compression_results.push(task_results)
  }
  
  // 等待所有并发压缩完成
  azimuth::CompressionManager::wait_for_concurrent_compression(compression_manager)
  
  // 验证并发压缩结果
  let total_compressed = 0
  let total_time = 0L
  
  for task_results in compression_results {
    for result in task_results {
      total_compressed = total_compressed + 1
      total_time = total_time + result.compression_time
      
      // 验证压缩结果有效性
      assert_true(azimuth::CompressionManager::is_valid_compression(compression_manager, result.compressed))
      
      // 测试解压缩正确性
      let decompressed = azimuth::CompressionManager::decompress_concurrent_safe(
        compression_manager, 
        result.compressed
      )
      assert_true(azimuth::CompressionManager::verify_decompression_integrity(compression_manager, decompressed))
    }
  }
  
  // 验证总压缩数量
  let expected_total = concurrent_tasks * items_per_task
  assert_eq(total_compressed, expected_total)
  
  // 验证并发性能
  let avg_compression_time = total_time / total_compressed.to_long()
  assert_true(avg_compression_time < 10000000L)  // 平均每次压缩小于10毫秒
  
  // 获取并发压缩统计
  let concurrent_stats = azimuth::CompressionManager::get_concurrent_stats(compression_manager)
  assert_true(concurrent_stats.total_operations >= expected_total)
  assert_true(concurrent_stats.successful_operations >= expected_total * 0.95)
  assert_true(concurrent_stats.failed_operations < expected_total * 0.05)
}