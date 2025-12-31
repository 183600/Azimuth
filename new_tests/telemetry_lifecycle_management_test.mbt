// 遥测数据生命周期测试用例

test "telemetry_data_creation_and_initialization" {
  // 测试遥测数据的创建和初始化
  
  // 创建遥测数据记录
  let telemetry_data = azimuth::telemetry::api::trace::TelemetryData::new()
  assert_eq(telemetry_data.is_valid(), true)
  assert_eq(telemetry_data.is_empty(), true)
  
  // 初始化遥测数据
  let initialized_data = telemetry_data
    .with_trace_id("trace-12345")
    .with_span_id("span-67890")
    .with_timestamp(1640995200000)  // 2022-01-01 00:00:00 UTC
    .with_service_name("test-service")
  
  assert_eq(initialized_data.is_valid(), true)
  assert_eq(initialized_data.is_empty(), false)
  assert_eq(initialized_data.get_trace_id(), "trace-12345")
  assert_eq(initialized_data.get_span_id(), "span-67890")
  assert_eq(initialized_data.get_timestamp(), 1640995200000)
  assert_eq(initialized_data.get_service_name(), "test-service")
}

test "telemetry_data_collection_and_aggregation" {
  // 测试遥测数据的收集和聚合
  
  // 创建多个遥测数据点
  let data_points = [
    azimuth::telemetry::api::metrics::MetricPoint::new("response_time", 150.5, "ms"),
    azimuth::telemetry::api::metrics::MetricPoint::new("response_time", 200.0, "ms"),
    azimuth::telemetry::api::metrics::MetricPoint::new("response_time", 120.0, "ms"),
    azimuth::telemetry::api::metrics::MetricPoint::new("response_time", 180.0, "ms"),
    azimuth::telemetry::api::metrics::MetricPoint::new("response_time", 160.0, "ms")
  ]
  
  // 创建聚合器
  let aggregator = azimuth::telemetry::api::metrics::MetricAggregator::new()
  
  // 添加数据点
  let mut aggregated_data = aggregator.empty()
  for point in data_points {
    aggregated_data = aggregated_data.add_point(point)
  }
  
  // 验证聚合结果
  assert_eq(aggregated_data.count(), 5)
  assert_eq(aggregated_data.sum(), 810.5)
  assert_eq(aggregated_data.average(), 162.1)
  assert_eq(aggregated_data.min(), 120.0)
  assert_eq(aggregated_data.max(), 200.0)
}

test "telemetry_data_storage_and_persistence" {
  // 测试遥测数据的存储和持久化
  
  // 创建遥测数据集合
  let telemetry_collection = azimuth::telemetry::api::common::TelemetryCollection::new()
    .add_metric("cpu.usage", 75.5, "percent")
    .add_metric("memory.usage", 1024.0, "MB")
    .add_trace("request.processed", "trace-001", "span-001")
    .add_log("info", "Service started successfully")
  
  // 序列化数据
  let serialized_data = telemetry_collection.serialize()
  assert_eq(serialized_data.length() > 0, true)
  
  // 反序列化数据
  let deserialized_collection = azimuth::telemetry::api::common::TelemetryCollection::deserialize(serialized_data)
  assert_eq(deserialized_collection.metrics_count(), 2)
  assert_eq(deserialized_collection.traces_count(), 1)
  assert_eq(deserialized_collection.logs_count(), 1)
  
  // 验证数据完整性
  let cpu_metric = deserialized_collection.get_metric("cpu.usage").unwrap()
  assert_eq(cpu_metric.value, 75.5)
  assert_eq(cpu_metric.unit, "percent")
  
  let memory_metric = deserialized_collection.get_metric("memory.usage").unwrap()
  assert_eq(memory_metric.value, 1024.0)
  assert_eq(memory_metric.unit, "MB")
}

test "telemetry_data_retention_and_cleanup" {
  // 测试遥测数据的保留和清理
  
  // 创建数据保留策略
  let retention_policy = azimuth::telemetry::api::common::RetentionPolicy::new()
    .with_max_age(86400000)  // 24小时
    .with_max_size(1000)     // 最大1000条记录
    .with_cleanup_interval(3600000)  // 1小时清理一次
  
  // 创建数据存储
  let data_store = azimuth::telemetry::api::storage::DataStore::new(retention_policy)
  
  // 添加不同时间的数据
  let current_time = 1640995200000  // 2022-01-01 00:00:00 UTC
  let old_time = current_time - 86400000  // 24小时前
  let very_old_time = current_time - 172800000  // 48小时前
  
  data_store = data_store
    .add_record_with_timestamp("recent", current_time)
    .add_record_with_timestamp("old", old_time)
    .add_record_with_timestamp("very_old", very_old_time)
  
  assert_eq(data_store.record_count(), 3)
  
  // 执行清理
  let cleaned_store = data_store.cleanup(current_time)
  
  // 验证清理结果
  assert_eq(cleaned_store.record_count(), 2)  // very_old应该被清理
  assert_eq(cleaned_store.contains_record("recent"), true)
  assert_eq(cleaned_store.contains_record("old"), true)
  assert_eq(cleaned_store.contains_record("very_old"), false)
}

test "telemetry_data_archiving_and_compression" {
  // 测试遥测数据的归档和压缩
  
  // 创建大量遥测数据
  let mut large_collection = azimuth::telemetry::api::common::TelemetryCollection::new()
  
  // 添加1000个数据点
  for i = 0; i < 1000; i = i + 1 {
    large_collection = large_collection.add_metric(
      "metric." + i.to_string(),
      i.to_float() * 1.5,
      "unit"
    )
  }
  
  assert_eq(large_collection.metrics_count(), 1000)
  
  // 归档数据
  let archived_data = large_collection.archive()
  assert_eq(archived_data.is_compressed(), true)
  assert_eq(archived_data.original_size(), large_collection.size())
  assert_eq(archived_data.compressed_size() < large_collection.size(), true)
  
  // 解压并验证
  let decompressed_collection = archived_data.decompress()
  assert_eq(decompressed_collection.metrics_count(), 1000)
  
  // 验证数据完整性
  for i = 0; i < 1000; i = i + 1 {
    let metric_name = "metric." + i.to_string()
    let metric = decompressed_collection.get_metric(metric_name).unwrap()
    assert_eq(metric.value, i.to_float() * 1.5)
    assert_eq(metric.unit, "unit")
  }
}

test "telemetry_data_lifecycle_transitions" {
  // 测试遥测数据生命周期状态转换
  
  // 创建遥测数据生命周期管理器
  let lifecycle_manager = azimuth::telemetry::api::lifecycle::LifecycleManager::new()
  
  // 创建遥测数据
  let telemetry_data = azimuth::telemetry::api::trace::TelemetryData::new()
    .with_trace_id("lifecycle-test")
    .with_span_id("span-001")
  
  // 验证初始状态
  assert_eq(telemetry_data.get_state(), azimuth::telemetry::api::lifecycle::DataState::Created)
  
  // 转换到活跃状态
  let active_data = lifecycle_manager.activate(telemetry_data)
  assert_eq(active_data.get_state(), azimuth::telemetry::api::lifecycle::DataState::Active)
  
  // 转换到完成状态
  let completed_data = lifecycle_manager.complete(active_data)
  assert_eq(completed_data.get_state(), azimuth::telemetry::api::lifecycle::DataState::Completed)
  
  // 转换到归档状态
  let archived_data = lifecycle_manager.archive(completed_data)
  assert_eq(archived_data.get_state(), azimuth::telemetry::api::lifecycle::DataState::Archived)
  
  // 验证状态转换历史
  let state_history = archived_data.get_state_history()
  assert_eq(state_history.length(), 4)
  assert_eq(state_history[0], azimuth::telemetry::api::lifecycle::DataState::Created)
  assert_eq(state_history[1], azimuth::telemetry::api::lifecycle::DataState::Active)
  assert_eq(state_history[2], azimuth::telemetry::api::lifecycle::DataState::Completed)
  assert_eq(state_history[3], azimuth::telemetry::api::lifecycle::DataState::Archived)
}