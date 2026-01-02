// Azimuth Telemetry Data Compression and Storage Optimization Tests
// 遥测数据压缩和存储优化测试用例，专注于数据压缩算法和存储策略

// Test 1: 遥测数据GZIP压缩
test "telemetry data GZIP compression" {
  // 创建测试遥测数据
  let telemetry_data = "trace_id=abc123def45678901234567890123456,span_id=1234567890abcdef,parent_span_id=0123456789abcdef,operation_name=get_user,start_time=1640995200000000,end_time=1640995201000000,status=ok,service.name=user_service,service.version=1.0.0,user.id=12345,request.path=/api/users/12345,http.method=GET,http.status_code=200,db.statement=SELECT * FROM users WHERE id = ?,db.type=postgresql"
  
  // 使用GZIP压缩数据
  let compressed_data = azimuth::compression::gzip::compress(telemetry_data)
  
  // 验证压缩后的数据比原始数据小
  assert_true(compressed_data.length() < telemetry_data.length())
  
  // 解压数据并验证内容
  let decompressed_data = azimuth::compression::gzip::decompress(compressed_data)
  assert_eq(decompressed_data, telemetry_data)
  
  // 计算压缩率
  let compression_ratio = compressed_data.length().to_float() / telemetry_data.length().to_float()
  assert_true(compression_ratio < 0.8) // 压缩率应该小于80%
}

// Test 2: 遥测数据LZ4压缩
test "telemetry data LZ4 compression" {
  // 创建大型遥测数据集
  let large_telemetry_data = []
  for i in 0..=999 {
    let data_point = "trace_id=trace" + i.to_string() + ",span_id=span" + i.to_string() + ",operation_name=operation_" + i.to_string() + ",service.name=service_" + (i % 10).to_string() + ",metric.value=" + (i * 1.5).to_string()
    large_telemetry_data = large_telemetry_data.push(data_point)
  }
  
  let combined_data = large_telemetry_data.join("\n")
  
  // 使用LZ4压缩数据
  let compressed_data = azimuth::compression::lz4::compress(combined_data)
  
  // 验证压缩后的数据比原始数据小
  assert_true(compressed_data.length() < combined_data.length())
  
  // 解压数据并验证内容
  let decompressed_data = azimuth::compression::lz4::decompress(compressed_data)
  assert_eq(decompressed_data, combined_data)
  
  // LZ4应该提供更快的压缩速度
  let start_time = azimuth::time::now()
  let _ = azimuth::compression::lz4::compress(combined_data)
  let end_time = azimuth::time::now()
  let compression_time = end_time - start_time
  
  // 验证压缩时间在合理范围内（小于100ms）
  assert_true(compression_time < 100)
}

// Test 3: 遥测数据字典压缩
test "telemetry data dictionary compression" {
  // 创建包含重复模式的遥测数据
  let telemetry_data = [
    "trace_id=abc123,service.name=user_service,operation_name=get_user",
    "trace_id=def456,service.name=user_service,operation_name=create_user",
    "trace_id=ghi789,service.name=user_service,operation_name=update_user",
    "trace_id=jkl012,service.name=order_service,operation_name=create_order",
    "trace_id=mno345,service.name=order_service,operation_name=get_order",
    "trace_id=pqr678,service.name=user_service,operation_name=delete_user"
  ]
  
  let combined_data = telemetry_data.join("\n")
  
  // 使用字典压缩
  let dictionary = azimuth::compression::dictionary::build_from_data(combined_data)
  let compressed_data = azimuth::compression::dictionary::compress(combined_data, dictionary)
  
  // 验证压缩后的数据比原始数据小
  assert_true(compressed_data.length() < combined_data.length())
  
  // 解压数据并验证内容
  let decompressed_data = azimuth::compression::dictionary::decompress(compressed_data, dictionary)
  assert_eq(decompressed_data, combined_data)
  
  // 验证字典包含常见的键值对
  assert_true(azimuth::compression::dictionary::contains(dictionary, "service.name"))
  assert_true(azimuth::compression::dictionary::contains(dictionary, "operation_name"))
}

// Test 4: 基于时间的遥测数据存储优化
test "time-based telemetry data storage optimization" {
  // 创建存储管理器
  let storage_manager = azimuth::storage::TimeBasedManager::new()
  
  // 配置存储策略
  azimuth::storage::set_retention_policy(storage_manager, "hot", 1) // 热数据保留1天
  azimuth::storage::set_retention_policy(storage_manager, "warm", 7) // 温数据保留7天
  azimuth::storage::set_retention_policy(storage_manager, "cold", 30) // 冷数据保留30天
  
  // 添加不同时间的遥测数据
  let current_time = azimuth::time::now()
  let old_time = current_time - (86400 * 10) // 10天前
  
  let recent_data = azimuth::storage::TelemetryData::new("recent_trace", current_time)
  let old_data = azimuth::storage::TelemetryData::new("old_trace", old_time)
  
  azimuth::storage::store(storage_manager, recent_data)
  azimuth::storage::store(storage_manager, old_data)
  
  // 验证数据存储在正确的层级
  let recent_storage = azimuth::storage::get_storage_tier(storage_manager, "recent_trace")
  let old_storage = azimuth::storage::get_storage_tier(storage_manager, "old_trace")
  
  assert_eq(recent_storage, "hot")
  assert_eq(old_storage, "cold")
}

// Test 5: 基于重要性的数据存储策略
test "importance-based data storage strategy" {
  // 创建重要性感知的存储管理器
  let storage_manager = azimuth::storage::ImportanceAwareManager::new()
  
  // 配置重要性规则
  azimuth::storage::add_importance_rule(storage_manager, "error", 10) // 错误数据最重要
  azimuth::storage::add_importance_rule(storage_manager, "critical", 8) // 关键数据
  azimuth::storage::add_importance_rule(storage_manager, "normal", 5) // 普通数据
  azimuth::storage::add_importance_rule(storage_manager, "debug", 1) // 调试数据最不重要
  
  // 创建不同重要性的遥测数据
  let error_data = azimuth::storage::TelemetryData::with_importance("error_trace", "error", 10)
  let critical_data = azimuth::storage::TelemetryData::with_importance("critical_trace", "critical", 8)
  let normal_data = azimuth::storage::TelemetryData::with_importance("normal_trace", "normal", 5)
  let debug_data = azimuth::storage::TelemetryData::with_importance("debug_trace", "debug", 1)
  
  // 存储数据
  azimuth::storage::store(storage_manager, error_data)
  azimuth::storage::store(storage_manager, critical_data)
  azimuth::storage::store(storage_manager, normal_data)
  azimuth::storage::store(storage_manager, debug_data)
  
  // 验证存储优先级
  let error_priority = azimuth::storage::get_storage_priority(storage_manager, "error_trace")
  let debug_priority = azimuth::storage::get_storage_priority(storage_manager, "debug_trace")
  
  assert_true(error_priority > debug_priority)
}

// Test 6: 遥测数据批处理存储
test "telemetry data batch storage optimization" {
  // 创建批处理存储管理器
  let batch_manager = azimuth::storage::BatchManager::new()
  
  // 配置批处理参数
  azimuth::storage::set_batch_size(batch_manager, 100) // 每批100条记录
  azimuth::storage::set_flush_interval(batch_manager, 5) // 5秒刷新一次
  
  // 添加大量遥测数据
  for i in 0..=249 {
    let data = azimuth::storage::TelemetryData::new("batch_trace_" + i.to_string(), azimuth::time::now())
    azimuth::storage::add_to_batch(batch_manager, data)
  }
  
  // 验证批处理状态
  let pending_count = azimuth::storage::get_pending_count(batch_manager)
  assert_eq(pending_count, 50) // 250 % 100 = 50条记录等待批处理
  
  // 手动刷新批处理
  azimuth::storage::flush(batch_manager)
  
  // 验证所有数据已处理
  let flushed_count = azimuth::storage::get_flushed_count(batch_manager)
  assert_eq(flushed_count, 250)
}

// Test 7: 遥测数据增量存储
test "telemetry data incremental storage" {
  // 创建增量存储管理器
  let incremental_manager = azimuth::storage::IncrementalManager::new()
  
  // 存储基础数据
  let base_data = azimuth::storage::TelemetryData::new("base_trace", azimuth::time::now())
  azimuth::storage::store_base(incremental_manager, base_data)
  
  // 存储增量更新
  let update1 = azimuth::storage::TelemetryUpdate::new("base_trace", "status", "ok")
  let update2 = azimuth::storage::TelemetryUpdate::new("base_trace", "duration", "150ms")
  
  azimuth::storage::store_update(incremental_manager, update1)
  azimuth::storage::store_update(incremental_manager, update2)
  
  // 重建完整数据
  let reconstructed_data = azimuth::storage::reconstruct(incremental_manager, "base_trace")
  
  // 验证重建的数据包含所有更新
  assert_true(azimuth::storage::has_attribute(reconstructed_data, "status"))
  assert_true(azimuth::storage::has_attribute(reconstructed_data, "duration"))
  
  assert_eq(azimuth::storage::get_attribute(reconstructed_data, "status"), "ok")
  assert_eq(azimuth::storage::get_attribute(reconstructed_data, "duration"), "150ms")
}

// Test 8: 遥测数据压缩比优化
test "telemetry data compression ratio optimization" {
  // 创建压缩优化器
  let optimizer = azimuth::compression::Optimizer::new()
  
  // 测试不同压缩算法的效率
  let test_data = "service.name=user_service,service.version=1.0.0,trace.id=abc123def45678901234567890123456,span.id=1234567890abcdef,parent.span.id=0123456789abcdef,operation.name=get_user,start.time=1640995200000000,end.time=1640995201000000,status=ok,user.id=12345,request.path=/api/users/12345,http.method=GET,http.status.code=200,db.statement=SELECT * FROM users WHERE id = ?,db.type=postgresql,host=service-a,region=us-west-2,environment=production"
  
  // 测试GZIP压缩
  let gzip_result = azimuth::compression::optimize::test_algorithm(optimizer, "gzip", test_data)
  
  // 测试LZ4压缩
  let lz4_result = azimuth::compression::optimize::test_algorithm(optimizer, "lz4", test_data)
  
  // 测试字典压缩
  let dict_result = azimuth::compression::optimize::test_algorithm(optimizer, "dictionary", test_data)
  
  // 获取最佳压缩算法
  let best_algorithm = azimuth::compression::optimize::get_best_algorithm(optimizer)
  
  // 验证最佳算法提供了合理的压缩比
  let best_ratio = azimuth::compression::optimize::get_compression_ratio(optimizer, best_algorithm)
  assert_true(best_ratio < 0.7) // 最佳压缩比应该小于70%
  
  // 验证压缩时间在可接受范围内
  let best_time = azimuth::compression::optimize::get_compression_time(optimizer, best_algorithm)
  assert_true(best_time < 50) // 压缩时间应该小于50ms
}