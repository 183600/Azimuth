// Azimuth数据压缩和优化测试用例
// 测试遥测数据的压缩算法和存储优化策略

test "基础数据压缩算法测试" {
  // 创建测试数据集
  let original_data = "azimuth-telemetry-data-".repeat(1000)
  let original_size = original_data.length()
  
  // 测试LZ77压缩算法
  let compressed_lz77 = Compression::compress_lz77(original_data)
  let compressed_lz77_size = compressed_lz77.length()
  
  // 验证压缩效果
  assert_true(compressed_lz77_size < original_size)
  let compression_ratio_lz77 = compressed_lz77_size.to_float() / original_size.to_float()
  assert_true(compression_ratio_lz77 < 0.8)  // 至少20%的压缩率
  
  // 验证解压缩正确性
  let decompressed_lz77 = Compression::decompress_lz77(compressed_lz77)
  assert_eq(decompressed_lz77, original_data)
  
  // 测试Huffman编码
  let compressed_huffman = Compression::compress_huffman(original_data)
  let compressed_huffman_size = compressed_huffman.length()
  
  // 验证压缩效果
  assert_true(compressed_huffman_size < original_size)
  let compression_ratio_huffman = compressed_huffman_size.to_float() / original_size.to_float()
  assert_true(compression_ratio_huffman < 0.9)  // 至少10%的压缩率
  
  // 验证解压缩正确性
  let decompressed_huffman = Compression::decompress_huffman(compressed_huffman)
  assert_eq(decompressed_huffman, original_data)
}

test "遥测数据专用压缩算法" {
  // 创建模拟遥测数据
  let telemetry_points = []
  let base_timestamp = 1735689600000000000L
  
  // 生成1000个遥测数据点
  for i in 0..=999 {
    let timestamp = base_timestamp + (i * 1000000000L)  // 每秒一个点
    let metric_value = 100.0 + (@sin(i.to_float() * 0.1) * 10.0)  // 周期性变化
    let attributes = [("service", "auth"), ("region", "us-west")]
    let point = TelemetryPoint::new(timestamp, metric_value, attributes)
    telemetry_points = telemetry_points.push(point)
  }
  
  // 序列化遥测数据
  let serialized_data = TelemetrySerialization::serialize_points(telemetry_points)
  let original_size = serialized_data.length()
  
  // 使用遥测专用压缩算法
  let compressed_telemetry = TelemetryCompression::compress_telemetry_data(telemetry_points)
  let compressed_size = compressed_telemetry.length()
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.5)  // 遥测数据应该有更好的压缩率
  
  // 验证解压缩正确性
  let decompressed_points = TelemetryCompression::decompress_telemetry_data(compressed_telemetry)
  assert_eq(decompressed_points.length(), telemetry_points.length())
  
  // 验证数据点内容
  for i in 0..=999 {
    assert_eq(decompressed_points[i].timestamp, telemetry_points[i].timestamp)
    assert_eq(decompressed_points[i].value, telemetry_points[i].value)
    assert_eq(decompressed_points[i].attributes, telemetry_points[i].attributes)
  }
}

test "增量压缩和差异编码" {
  // 创建基础时间序列数据
  let base_series = TimeSeries::new("cpu.usage", "percent")
  let base_timestamp = 1735689600000000000L
  
  for i in 0..=99 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个点
    let value = 50.0 + (@sin(i.to_float() * 0.1) * 10.0)
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(base_series, data_point)
  }
  
  // 压缩基础数据
  let compressed_base = TimeSeriesCompression::compress_series(base_series)
  
  // 创建增量数据（在基础数据之后）
  let incremental_points = []
  for i in 100..=149 {
    let timestamp = base_timestamp + (i * 60000000000L)
    let value = 50.0 + (@sin(i.to_float() * 0.1) * 10.0)
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    incremental_points = incremental_points.push(data_point)
  }
  
  // 压缩增量数据
  let compressed_incremental = TimeSeriesCompression::compress_incremental(
    base_series, 
    incremental_points
  )
  
  // 验证增量压缩比完整压缩更高效
  let full_series_size = TimeSeriesCompression::compress_series(
    TimeSeries::with_points("cpu.usage", "percent", 
      TimeSeries::get_points(base_series) + incremental_points)
  ).length()
  
  let incremental_total_size = compressed_base.length() + compressed_incremental.length()
  assert_true(incremental_total_size < full_series_size)
  
  // 验证增量解压缩正确性
  let reconstructed_points = TimeSeriesCompression::decompress_incremental(
    compressed_base, 
    compressed_incremental
  )
  
  assert_eq(reconstructed_points.length(), 150)
  for i in 0..=149 {
    let expected_timestamp = base_timestamp + (i * 60000000000L)
    assert_eq(reconstructed_points[i].timestamp, expected_timestamp)
  }
}

test "属性压缩和字典编码" {
  // 创建具有重复属性的遥测数据
  let telemetry_data = []
  let common_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "2.1.0"),
    ("deployment.environment", "production"),
    ("region", "us-west-2")
  ]
  
  // 生成1000个具有相似属性的数据点
  for i in 0..=999 {
    let timestamp = 1735689600000000000L + (i * 1000000000L)
    let value = @random() * 100.0
    
    // 大部分数据点使用通用属性，少数有变化
    let attributes = if i % 10 == 0 {
      // 10%的数据点有额外属性
      common_attributes + [("request.id", "req-" + i.to_string())]
    } else {
      common_attributes
    }
    
    let point = TelemetryPoint::new(timestamp, value, attributes)
    telemetry_data = telemetry_data.push(point)
  }
  
  // 使用字典编码压缩属性
  let compressed_with_dict = AttributeCompression::compress_with_dictionary(telemetry_data)
  let compressed_without_dict = TelemetrySerialization::serialize_points(telemetry_data)
  
  // 验证字典编码的压缩效果
  assert_true(compressed_with_dict.length() < compressed_without_dict.length())
  let compression_improvement = (compressed_without_dict.length() - compressed_with_dict.length()).to_float() / 
                               compressed_without_dict.length().to_float()
  assert_true(compression_improvement > 0.1)  // 至少10%的改进
  
  // 验证解压缩正确性
  let decompressed_data = AttributeCompression::decompress_with_dictionary(compressed_with_dict)
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  // 验证属性内容
  for i in 0..=999 {
    assert_eq(decompressed_data[i].timestamp, telemetry_data[i].timestamp)
    assert_eq(decompressed_data[i].value, telemetry_data[i].value)
    assert_eq(decompressed_data[i].attributes, telemetry_data[i].attributes)
  }
}

test "自适应压缩策略选择" {
  // 创建不同类型的数据集测试自适应压缩
  
  // 1. 高重复性文本数据
  let repetitive_data = "error-message-".repeat(500)
  let compression_strategy1 = AdaptiveCompression::select_strategy(repetitive_data)
  assert_eq(compression_strategy1, "dictionary")  // 应该选择字典编码
  
  // 2. 时间序列数值数据
  let time_series_data = []
  for i in 0..=199 {
    let timestamp = 1735689600000000000L + (i * 60000000000L)
    let value = 100.0 + (i.to_float() * 0.5)
    time_series_data = time_series_data.push((timestamp, value))
  }
  let serialized_ts = TimeSeriesSerialization::serialize(time_series_data)
  let compression_strategy2 = AdaptiveCompression::select_strategy(serialized_ts)
  assert_eq(compression_strategy2, "delta")  // 应该选择增量编码
  
  // 3. 随机二进制数据
  let binary_data = []
  for i in 0..=999 {
    binary_data = binary_data.push((@random() * 256.0).to_int())
  }
  let compression_strategy3 = AdaptiveCompression::select_strategy(binary_data)
  assert_eq(compression_strategy3, "generic")  // 应该选择通用压缩
  
  // 验证自适应压缩的效果
  let compressed1 = AdaptiveCompression::compress(repetitive_data)
  let compressed2 = AdaptiveCompression::compress(serialized_ts)
  let compressed3 = AdaptiveCompression::compress(binary_data)
  
  assert_true(compressed1.length() < repetitive_data.length())
  assert_true(compressed2.length() < serialized_ts.length())
  assert_true(compressed3.length() < binary_data.length())
}

test "压缩性能和内存优化" {
  // 创建大数据集测试压缩性能
  let large_dataset = []
  let base_timestamp = 1735689600000000000L
  
  // 生成10000个遥测数据点
  for i in 0..=9999 {
    let timestamp = base_timestamp + (i * 1000000000L)
    let value = @random() * 1000.0
    let attributes = [
      ("service", "auth"),
      ("instance", "instance-" + ((i % 10).to_string())),
      ("region", ["us-west", "us-east", "eu-west"][i % 3])
    ]
    let point = TelemetryPoint::new(timestamp, value, attributes)
    large_dataset = large_dataset.push(point)
  }
  
  // 测试压缩性能
  let start_time = Time::now()
  let compressed_data = TelemetryCompression::compress_telemetry_data(large_dataset)
  let compression_time = Time::now() - start_time
  
  // 验证压缩时间在合理范围内（小于5秒）
  assert_true(compression_time < 5000000000L)  // 5秒（纳秒）
  
  // 测试内存使用优化
  let memory_before = Memory::usage()
  let compressed_optimized = TelemetryCompression::compress_with_memory_optimization(large_dataset)
  let memory_after = Memory::usage()
  let memory_usage = memory_after - memory_before
  
  // 验证内存使用在合理范围内（小于100MB）
  assert_true(memory_usage < 100 * 1024 * 1024)  // 100MB
  
  // 验证优化压缩的压缩率
  let original_size = TelemetrySerialization::serialize_points(large_dataset).length()
  let optimized_compression_ratio = compressed_optimized.length().to_float() / original_size.to_float()
  assert_true(optimized_compression_ratio < 0.3)  // 至少70%的压缩率
  
  // 验证解压缩性能
  let start_decompression = Time::now()
  let decompressed_data = TelemetryCompression::decompress_telemetry_data(compressed_optimized)
  let decompression_time = Time::now() - start_decompression
  
  // 验证解压缩时间在合理范围内（小于3秒）
  assert_true(decompression_time < 3000000000L)  // 3秒（纳秒）
  
  // 验证数据完整性
  assert_eq(decompressed_data.length(), large_dataset.length())
}

test "压缩数据索引和查询" {
  // 创建压缩的时间序列数据
  let time_series = TimeSeries::new("query.test.metric", "value")
  let base_timestamp = 1735689600000000000L
  
  // 生成1000个数据点
  for i in 0..=999 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个点
    let value = 50.0 + (@sin(i.to_float() * 0.05) * 20.0)
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 压缩时间序列数据
  let compressed_series = CompressedTimeSeries::compress(time_series)
  
  // 创建压缩数据的索引
  let index = CompressedTimeSeries::create_index(compressed_series)
  
  // 测试时间范围查询
  let query_start = base_timestamp + (200 * 60000000000L)
  let query_end = base_timestamp + (300 * 60000000000L)
  
  let query_result = CompressedTimeSeries::query_range(
    compressed_series, 
    index, 
    query_start, 
    query_end
  )
  
  // 验证查询结果
  assert_eq(query_result.length(), 101)  // 包含边界点
  for point in query_result {
    assert_true(point.timestamp >= query_start && point.timestamp <= query_end)
  }
  
  // 测试值范围查询
  let value_query_result = CompressedTimeSeries::query_value_range(
    compressed_series, 
    index, 
    60.0, 
    80.0
  )
  
  // 验证值范围查询结果
  for point in value_query_result {
    assert_true(point.value >= 60.0 && point.value <= 80.0)
  }
  
  // 测试聚合查询
  let aggregation_result = CompressedTimeSeries::aggregate_query(
    compressed_series, 
    index, 
    base_timestamp, 
    base_timestamp + (999 * 60000000000L),
    "average",
    100  // 100个桶
  )
  
  // 验证聚合查询结果
  assert_eq(aggregation_result.length(), 100)
  for bucket in aggregation_result {
    assert_true(bucket.count > 0)
    assert_true(bucket.average >= 30.0 && bucket.average <= 70.0)
  }
}

test "压缩数据流式处理" {
  // 创建模拟实时数据流
  let data_stream = DataStream::new()
  let base_timestamp = 1735689600000000000L
  
  // 启动流式压缩处理
  let compressed_stream = StreamCompression::start_compression(data_stream, 100)  // 每100个点压缩一次
  
  // 模拟实时数据输入
  for batch in 0..=9 {  // 10个批次，每个批次100个点
    for i in 0..=99 {
      let timestamp = base_timestamp + ((batch * 100 + i) * 1000000000L)
      let value = @random() * 100.0
      let attributes = [("batch", batch.to_string())]
      let point = TelemetryPoint::new(timestamp, value, attributes)
      DataStream::add_point(data_stream, point)
    }
    
    // 等待压缩完成
    @sleep(100)  // 100ms
  }
  
  // 完成流式压缩
  let final_compressed_data = StreamCompression::finish(compressed_stream)
  
  // 验证流式压缩的数据完整性
  let decompressed_data = TelemetryCompression::decompress_telemetry_data(final_compressed_data)
  assert_eq(decompressed_data.length(), 1000)  // 10个批次 × 100个点
  
  // 验证时间戳顺序
  for i in 1..=999 {
    assert_true(decompressed_data[i].timestamp > decompressed_data[i-1].timestamp)
  }
  
  // 验证批次属性
  for i in 0..=999 {
    let expected_batch = (i / 100).to_string()
    let batch_attr = decompressed_data[i].attributes.find(fn(attr) { attr.0 == "batch" })
    match batch_attr {
      Some(attr) => assert_eq(attr.1, expected_batch)
      None => assert_true(false)
    }
  }
  
  // 测试流式压缩的内存效率
  let peak_memory_usage = StreamCompression::get_peak_memory_usage(compressed_stream)
  let total_data_size = TelemetrySerialization::serialize_points(decompressed_data).length()
  let memory_efficiency = peak_memory_usage.to_float() / total_data_size.to_float()
  
  // 验证内存效率（峰值内存使用应该远小于总数据大小）
  assert_true(memory_efficiency < 0.3)  // 峰值内存使用小于总数据大小的30%
}