// Azimuth Serialization and Performance Tests
// 测试序列化、反序列化和性能相关功能

test "JSON序列化和反序列化" {
  // 测试JSON序列化和反序列化功能
  let telemetry_data = {
    "trace_id": "abc123def456",
    "span_id": "789ghi012jkl",
    "operation_name": "user.login",
    "start_time": 1735689600000000000L,
    "duration": 125000000L,
    "status": "OK",
    "attributes": {
      "user.id": "user-12345",
      "service.name": "auth-service",
      "region": "us-west-2"
    }
  }
  
  // 序列化为JSON字符串
  let json_string = serialize_to_json(telemetry_data)
  assert_true(json_string.contains("trace_id"))
  assert_true(json_string.contains("user.login"))
  assert_true(json_string.contains("auth-service"))
  
  // 反序列化回对象
  let deserialized_data = deserialize_from_json(json_string)
  assert_eq(deserialized_data.operation_name, "user.login")
  assert_eq(deserialized_data.status, "OK")
  assert_eq(deserialized_data.attributes["user.id"], "user-12345")
  
  // 测试复杂数据结构的序列化
  let complex_data = {
    "metrics": [
      {"name": "cpu.usage", "value": 75.5, "unit": "percent"},
      {"name": "memory.usage", "value": 1024.0, "unit": "MB"},
      {"name": "disk.io", "value": 125.8, "unit": "MB/s"}
    ],
    "events": [
      {"timestamp": 1735689600000000000L, "name": "process.started"},
      {"timestamp": 1735689600500000000L, "name": "cache.loaded"}
    ],
    "links": [
      {"trace_id": "parent-trace-123", "span_id": "parent-span-456"}
    ]
  }
  
  let complex_json = serialize_to_json(complex_data)
  let complex_deserialized = deserialize_from_json(complex_json)
  assert_eq(complex_deserialized.metrics.length(), 3)
  assert_eq(complex_deserialized.events.length(), 2)
  assert_eq(complex_deserialized.links.length(), 1)
  assert_eq(complex_deserialized.metrics[0].name, "cpu.usage")
  assert_eq(complex_deserialized.metrics[0].value, 75.5)
}

test "二进制序列化性能优化" {
  // 测试二进制序列化性能优化
  let large_dataset = []
  
  // 生成大型数据集
  for i in 0..=1000 {
    let data_point = {
      "timestamp": 1735689600000000000L + (i * 1000000L),
      "metric_name": "performance.metric." + i.to_string(),
      "value": (i * 1.5),
      "tags": {
        "host": "server-" + (i % 10).to_string(),
        "region": ["us-west-2", "us-east-1", "eu-west-1"][i % 3],
        "environment": "production"
      }
    }
    large_dataset = large_dataset.push(data_point)
  }
  
  // 测试二进制序列化
  let binary_data = serialize_to_binary(large_dataset)
  assert_true(binary_data.length() > 0)
  
  // 测试二进制反序列化
  let deserialized_dataset = deserialize_from_binary(binary_data)
  assert_eq(deserialized_dataset.length(), 1001)
  assert_eq(deserialized_dataset[0].metric_name, "performance.metric.0")
  assert_eq(deserialized_dataset[1000].metric_name, "performance.metric.1000")
  
  // 测试压缩性能
  let compressed_data = compress_binary(binary_data)
  assert_true(compressed_data.length() < binary_data.length())
  
  // 测试解压缩和反序列化
  let decompressed_data = decompress_binary(compressed_data)
  let final_dataset = deserialize_from_binary(decompressed_data)
  assert_eq(final_dataset.length(), 1001)
  assert_eq(final_dataset[500].value, 500 * 1.5)
}

test "批量操作和流式处理" {
  // 测试批量操作和流式处理
  let batch_processor = BatchProcessor::new(100)
  let stream_processor = StreamProcessor::new()
  
  // 添加批量数据
  let telemetry_spans = []
  for i in 0..=250 {
    let span = {
      "trace_id": "trace-" + (i % 10).to_string(),
      "span_id": "span-" + i.to_string(),
      "operation": "operation." + (i % 5).to_string(),
      "duration": (i * 1000) + 5000,
      "status": if i % 20 == 0 { "ERROR" } else { "OK" }
    }
    telemetry_spans = telemetry_spans.push(span)
  }
  
  // 批量处理
  let batches = BatchProcessor::process(batch_processor, telemetry_spans)
  assert_eq(batches.length(), 3)  // 251个span，每批100个，应该有3批
  assert_eq(batches[0].length(), 100)
  assert_eq(batches[1].length(), 100)
  assert_eq(batches[2].length(), 51)
  
  // 流式处理
  let stream_results = []
  for span in telemetry_spans {
    let result = StreamProcessor::process_item(stream_processor, span)
    stream_results = stream_results.push(result)
  }
  
  assert_eq(stream_results.length(), 251)
  
  // 验证错误处理
  let error_count = stream_results.filter(fn(r) { r.status == "ERROR" }).length()
  assert_eq(error_count, 13)  // 251个span中每20个有1个错误，应该有13个错误
}

test "缓存策略和性能优化" {
  // 测试缓存策略和性能优化
  let cache = LRU_Cache::new(100)
  let metrics_collector = MetricsCollector::new()
  
  // 测试缓存操作
  for i in 0..=150 {
    let key = "metric-key-" + i.to_string()
    let value = {
      "timestamp": 1735689600000000000L + (i * 1000000L),
      "value": (i * 2.5),
      "metadata": {
        "source": "sensor-" + (i % 5).to_string(),
        "quality": "high"
      }
    }
    Cache::put(cache, key, value)
  }
  
  // 验证缓存大小限制
  assert_eq(Cache::size(cache), 100)  // LRU缓存应该保持100个条目
  
  // 测试缓存命中
  let cached_value = Cache::get(cache, "metric-key-125")
  match cached_value {
    Some(value) => {
      assert_eq(value.value, 125 * 2.5)
      assert_eq(value.metadata.source, "sensor-0")
    }
    None => assert_true(false)
  }
  
  // 测试缓存未命中
  let missing_value = Cache::get(cache, "metric-key-10")
  match missing_value {
    Some(_) => assert_true(false)  // 应该被LRU淘汰
    None => assert_true(true)
  }
  
  // 测试缓存性能统计
  let cache_stats = Cache::statistics(cache)
  assert_eq(cache_stats.hits, 1)
  assert_eq(cache_stats.misses, 1)
  assert_eq(cache_stats.evictions, 51)  // 150个条目中应该有51个被淘汰
}