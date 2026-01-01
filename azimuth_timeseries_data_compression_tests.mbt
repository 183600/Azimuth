// Azimuth Time Series Data Compression Tests
// 时间序列数据压缩测试用例
// 测试时间序列数据的压缩、解压缩和存储优化

import "azimuth/azimuth"

// Test 1: 时间序列数据基础压缩测试
pub test "时间序列数据基础压缩测试" {
  // 创建时间序列压缩器
  let compressor = azimuth::TimeSeriesCompressor::new()
  
  // 创建测试时间序列数据
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let time_series_data = []
  
  // 生成1000个时间序列数据点
  for i in 0..1000 {
    let timestamp = base_timestamp + (i * 1000000L)  // 每毫秒一个数据点
    let value = 100.0 + (i % 100).to_double() * 0.5 + azimuth::Random::next_double(azimuth::Random::system()) * 10.0
    let attributes = [
      ("metric.name", "cpu.usage"),
      ("host.name", "server-" + (i % 10).to_string()),
      ("region", "us-west-" + (i % 3).to_string())
    ]
    
    time_series_data.push({
      "timestamp": timestamp,
      "value": value,
      "attributes": attributes
    })
  }
  
  // 压缩时间序列数据
  let compressed_data = azimuth::TimeSeriesCompressor::compress(compressor, time_series_data)
  
  // 验证压缩结果
  assert_true(azimuth::TimeSeriesCompressor::is_valid(compressed_data))
  assert_true(azimuth::TimeSeriesCompressor::get_compressed_size(compressed_data) > 0)
  
  // 计算压缩率
  let original_size = azimuth::TimeSeriesUtils::calculate_data_size(time_series_data)
  let compressed_size = azimuth::TimeSeriesCompressor::get_compressed_size(compressed_data)
  let compression_ratio = original_size.to_double() / compressed_size.to_double()
  
  // 验证压缩效果（压缩率应该大于1.5，即至少压缩33%）
  assert_true(compression_ratio > 1.5)
  
  // 解压缩数据
  let decompressed_data = azimuth::TimeSeriesCompressor::decompress(compressor, compressed_data)
  
  // 验证解压缩结果
  assert_eq(decompressed_data.length(), time_series_data.length())
  
  // 验证数据完整性
  for i in 0..time_series_data.length() {
    assert_eq(decompressed_data[i]["timestamp"], time_series_data[i]["timestamp"])
    assert_true(azimuth::FloatUtils::almost_equal(
      decompressed_data[i]["value"], 
      time_series_data[i]["value"], 
      0.001  // 允许0.001的误差
    ))
    assert_eq(decompressed_data[i]["attributes"], time_series_data[i]["attributes"])
  }
  
  // 创建压缩度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "compression-test")
  let compression_ratio_gauge = azimuth::Meter::create_gauge(meter, "compression.ratio")
  let compression_time_histogram = azimuth::Meter::create_histogram(meter, "compression.time", Some("Compression processing time"), Some("ms"))
  
  // 记录压缩度量
  azimuth::Gauge::record(compression_ratio_gauge, compression_ratio)
  azimuth::Histogram::record(compression_time_histogram, azimuth::TimeSeriesCompressor::get_compression_time(compressor).to_double())
}

// Test 2: 不同压缩算法性能对比测试
pub test "不同压缩算法性能对比测试" {
  // 创建不同类型的压缩器
  let gzip_compressor = azimuth::TimeSeriesCompressor::with_algorithm("gzip")
  let lz4_compressor = azimuth::TimeSeriesCompressor::with_algorithm("lz4")
  let zstd_compressor = azimuth::TimeSeriesCompressor::with_algorithm("zstd")
  let delta_compressor = azimuth::TimeSeriesCompressor::with_algorithm("delta")
  
  // 创建测试数据集
  let datasets = [
    {
      "name": "steady.values",
      "data": generate_steady_time_series(1000, 100.0, 0.1)
    },
    {
      "name": "linear.trend",
      "data": generate_linear_trend_time_series(1000, 50.0, 0.1)
    },
    {
      "name": "seasonal.pattern",
      "data": generate_seasonal_time_series(1000, 24, 60.0, 20.0)
    },
    {
      "name": "random.spikes",
      "data": generate_spike_time_series(1000, 25.0, 100.0, 0.05)
    }
  ]
  
  // 测试不同压缩算法在不同数据集上的性能
  let compression_results = []
  
  for dataset in datasets {
    for (algorithm, compressor) in [
      ("gzip", gzip_compressor),
      ("lz4", lz4_compressor),
      ("zstd", zstd_compressor),
      ("delta", delta_compressor)
    ] {
      // 压缩数据
      let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      let compressed_data = azimuth::TimeSeriesCompressor::compress(compressor, dataset["data"])
      let compression_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time
      
      // 解压缩数据
      let start_decompress_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      let decompressed_data = azimuth::TimeSeriesCompressor::decompress(compressor, compressed_data)
      let decompression_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_decompress_time
      
      // 计算性能指标
      let original_size = azimuth::TimeSeriesUtils::calculate_data_size(dataset["data"])
      let compressed_size = azimuth::TimeSeriesCompressor::get_compressed_size(compressed_data)
      let compression_ratio = original_size.to_double() / compressed_size.to_double()
      
      compression_results.push({
        "dataset": dataset["name"],
        "algorithm": algorithm,
        "compression.time": compression_time,
        "decompression.time": decompression_time,
        "compression.ratio": compression_ratio,
        "original.size": original_size,
        "compressed.size": compressed_size
      })
    }
  }
  
  // 分析压缩结果
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "compression-comparison")
  let compression_time_histogram = azimuth::Meter::create_histogram(meter, "compression.time.by.algorithm", Some("Compression time by algorithm"), Some("ms"))
  let compression_ratio_histogram = azimuth::Meter::create_histogram(meter, "compression.ratio.by.algorithm", Some("Compression ratio by algorithm"), None)
  
  // 记录压缩性能度量
  for result in compression_results {
    azimuth::Histogram::record(compression_time_histogram, result["compression.time"].to_double() / 1000000.0, Some([
      ("algorithm", azimuth::StringValue(result["algorithm"])),
      ("dataset", azimuth::StringValue(result["dataset"]))
    ]))
    
    azimuth::Histogram::record(compression_ratio_histogram, result["compression.ratio"], Some([
      ("algorithm", azimuth::StringValue(result["algorithm"])),
      ("dataset", azimuth::StringValue(result["dataset"]))
    ]))
  }
  
  // 验证压缩算法的有效性
  for dataset in datasets {
    let dataset_results = compression_results.filter(r => r["dataset"] == dataset["name"])
    
    // 验证所有算法都能成功压缩和解压缩
    for result in dataset_results {
      assert_true(result["compression.ratio"] > 1.0)  // 压缩率应该大于1
      assert_true(result["compression.time"] > 0L)    // 压缩时间应该大于0
      assert_true(result["decompression.time"] > 0L) // 解压缩时间应该大于0
    }
    
    // 找出最适合该数据集的算法
    let best_compression = dataset_results.reduce((best, current) => 
      current["compression.ratio"] > best["compression.ratio"] ? current : best
    )
    
    // 记录最佳算法
    let best_algorithm_gauge = azimuth::Meter::create_gauge(meter, "best.compression.algorithm")
    azimuth::Gauge::record(best_algorithm_gauge, match best_compression["algorithm"] {
      "gzip" => 1.0,
      "lz4" => 2.0,
      "zstd" => 3.0,
      "delta" => 4.0,
      _ => 0.0
    }, Some([
      ("dataset", azimuth::StringValue(dataset["name"])),
      ("algorithm", azimuth::StringValue(best_compression["algorithm"]))
    ]))
  }
}

// Test 3: 增量压缩和流式压缩测试
pub test "增量压缩和流式压缩测试" {
  // 创建流式压缩器
  let stream_compressor = azimuth::StreamTimeSeriesCompressor::new()
  
  // 创建增量压缩器
  let incremental_compressor = azimuth::IncrementalTimeSeriesCompressor::new()
  
  // 模拟实时数据流
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let stream_data_points = []
  
  // 生成流式数据点（模拟实时数据）
  for i in 0..500 {
    let timestamp = base_timestamp + (i * 2000000L)  // 每2毫秒一个数据点
    let value = 50.0 + azimuth::Math::sin(i.to_double() * 0.1) * 10.0 + azimuth::Random::next_double(azimuth::Random::system()) * 5.0
    
    let data_point = {
      "timestamp": timestamp,
      "value": value,
      "metric": "system.memory.usage"
    }
    
    stream_data_points.push(data_point)
    
    // 流式压缩（每100个数据点压缩一次）
    if (i + 1) % 100 == 0 {
      let batch = stream_data_points.slice(i - 99, i + 1)
      azimuth::StreamTimeSeriesCompressor::add_batch(stream_compressor, batch)
      
      // 增量压缩
      azimuth::IncrementalTimeSeriesCompressor::add_point(incremental_compressor, data_point)
    }
  }
  
  // 完成流式压缩
  let stream_compressed_data = azimuth::StreamTimeSeriesCompressor::finish(stream_compressor)
  
  // 完成增量压缩
  let incremental_compressed_data = azimuth::IncrementalTimeSeriesCompressor::finish(incremental_compressor)
  
  // 验证流式压缩结果
  assert_true(azimuth::StreamTimeSeriesCompressor::is_valid(stream_compressed_data))
  let stream_decompressed = azimuth::StreamTimeSeriesCompressor::decompress(stream_compressor)
  assert_eq(stream_decompressed.length(), stream_data_points.length())
  
  // 验证增量压缩结果
  assert_true(azimuth::IncrementalTimeSeriesCompressor::is_valid(incremental_compressed_data))
  let incremental_decompressed = azimuth::IncrementalTimeSeriesCompressor::decompress(incremental_compressor)
  assert_eq(incremental_decompressed.length(), stream_data_points.length())
  
  // 比较不同压缩方法的性能
  let stream_compression_ratio = stream_data_points.length().to_double() * 24.0 / azimuth::StreamTimeSeriesCompressor::get_compressed_size(stream_compressed_data).to_double()
  let incremental_compression_ratio = stream_data_points.length().to_double() * 24.0 / azimuth::IncrementalTimeSeriesCompressor::get_compressed_size(incremental_compressed_data).to_double()
  
  // 创建压缩性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "streaming-compression")
  let stream_compression_ratio_gauge = azimuth::Meter::create_gauge(meter, "stream.compression.ratio")
  let incremental_compression_ratio_gauge = azimuth::Meter::create_gauge(meter, "incremental.compression.ratio")
  
  azimuth::Gauge::record(stream_compression_ratio_gauge, stream_compression_ratio)
  azimuth::Gauge::record(incremental_compression_ratio_gauge, incremental_compression_ratio)
  
  // 验证压缩效果
  assert_true(stream_compression_ratio > 1.5)
  assert_true(incremental_compression_ratio > 1.5)
}

// Test 4: 压缩数据查询和聚合测试
pub test "压缩数据查询和聚合测试" {
  // 创建压缩存储引擎
  let compressed_storage = azimuth::CompressedTimeSeriesStorage::new()
  
  // 生成并压缩多个时间序列
  let time_series_list = []
  let metrics = ["cpu.usage", "memory.usage", "disk.io", "network.throughput"]
  let hosts = ["host-1", "host-2", "host-3"]
  
  for metric in metrics {
    for host in hosts {
      let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      let time_series_data = []
      
      // 生成24小时的数据（每分钟一个数据点）
      for i in 0..1440 {
        let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个数据点
        let value = generate_metric_value(metric, i)
        
        time_series_data.push({
          "timestamp": timestamp,
          "value": value,
          "metric": metric,
          "host": host
        })
      }
      
      // 压缩时间序列
      let compressed_data = azimuth::TimeSeriesCompressor::compress(
        azimuth::TimeSeriesCompressor::new(), 
        time_series_data
      )
      
      // 存储压缩数据
      azimuth::CompressedTimeSeriesStorage::store(compressed_storage, metric + ":" + host, compressed_data)
      
      time_series_list.push({
        "key": metric + ":" + host,
        "data": time_series_data,
        "compressed": compressed_data
      })
    }
  }
  
  // 测试压缩数据查询
  for series in time_series_list {
    // 查询完整时间序列
    let full_query = azimuth::TimeSeriesQuery::new()
      .with_metric(series["key"])
      .with_start_time(series["data"][0]["timestamp"])
      .with_end_time(series["data"][series["data"].length() - 1]["timestamp"])
    
    let full_result = azimuth::CompressedTimeSeriesStorage::query(compressed_storage, full_query)
    
    // 验证查询结果
    assert_eq(full_result.length(), series["data"].length())
    
    // 查询时间范围（前6小时）
    let partial_query = azimuth::TimeSeriesQuery::new()
      .with_metric(series["key"])
      .with_start_time(series["data"][0]["timestamp"])
      .with_end_time(series["data"][360]["timestamp"])  // 6小时 = 360分钟
    
    let partial_result = azimuth::CompressedTimeSeriesStorage::query(compressed_storage, partial_query)
    
    // 验证部分查询结果
    assert_eq(partial_result.length(), 361)  // 包含起始和结束点
    
    // 测试聚合查询
    let aggregation_query = azimuth::TimeSeriesQuery::new()
      .with_metric(series["key"])
      .with_start_time(series["data"][0]["timestamp"])
      .with_end_time(series["data"][series["data"].length() - 1]["timestamp"])
      .with_aggregation("avg", 360)  // 按6小时聚合
    
    let aggregation_result = azimuth::CompressedTimeSeriesStorage::query(compressed_storage, aggregation_query)
    
    // 验证聚合结果（24小时 / 6小时 = 4个聚合点）
    assert_eq(aggregation_result.length(), 4)
    
    // 验证聚合计算
    let expected_avg_1 = calculate_average(series["data"].slice(0, 361))
    let expected_avg_2 = calculate_average(series["data"].slice(360, 721))
    let expected_avg_3 = calculate_average(series["data"].slice(720, 1081))
    let expected_avg_4 = calculate_average(series["data"].slice(1080, 1440))
    
    assert_true(azimuth::FloatUtils::almost_equal(aggregation_result[0]["value"], expected_avg_1, 0.01))
    assert_true(azimuth::FloatUtils::almost_equal(aggregation_result[1]["value"], expected_avg_2, 0.01))
    assert_true(azimuth::FloatUtils::almost_equal(aggregation_result[2]["value"], expected_avg_3, 0.01))
    assert_true(azimuth::FloatUtils::almost_equal(aggregation_result[3]["value"], expected_avg_4, 0.01))
  }
  
  // 测试多时间序列查询
  let multi_query = azimuth::TimeSeriesQuery::new()
    .with_metrics(["cpu.usage:host-1", "cpu.usage:host-2", "cpu.usage:host-3"])
    .with_start_time(time_series_list[0]["data"][0]["timestamp"])
    .with_end_time(time_series_list[0]["data"][time_series_list[0]["data"].length() - 1]["timestamp"])
  
  let multi_result = azimuth::CompressedTimeSeriesStorage::query(compressed_storage, multi_query)
  
  // 验证多时间序列查询结果
  assert_eq(multi_result.length(), 3)
  for result in multi_result {
    assert_eq(result["data"].length(), 1440)
  }
  
  // 创建查询性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "compressed-query")
  let query_time_histogram = azimuth::Meter::create_histogram(meter, "query.time", Some("Query execution time"), Some("ms"))
  let decompression_time_histogram = azimuth::Meter::create_histogram(meter, "decompression.time", Some("Decompression time"), Some("ms"))
  
  // 记录查询性能度量
  azimuth::Histogram::record(query_time_histogram, azimuth::CompressedTimeSeriesStorage::get_last_query_time(compressed_storage).to_double() / 1000000.0)
  azimuth::Histogram::record(decompression_time_histogram, azimuth::CompressedTimeSeriesStorage::get_last_decompression_time(compressed_storage).to_double() / 1000000.0)
}

// Test 5: 压缩数据长期存储和检索测试
pub test "压缩数据长期存储和检索测试" {
  // 创建长期存储管理器
  let long_term_storage = azimuth::LongTermCompressedStorage::new()
  
  // 配置存储策略
  azimuth::LongTermCompressedStorage::set_retention_policy(long_term_storage, {
    "hot.tier.duration": "7d",     // 热数据保留7天
    "warm.tier.duration": "30d",   // 温数据保留30天
    "cold.tier.duration": "365d",  // 冷数据保留365天
    "archive.tier.duration": "indefinite"  // 归档数据无限期保留
  })
  
  // 生成历史数据（模拟过去2年的数据）
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let two_years_ago = current_time - (2 * 365 * 24 * 60 * 60 * 1000000000L)
  
  let historical_data = []
  let time_per_day = 24 * 60 * 60 * 1000000000L  // 一天的纳秒数
  
  // 生成每日汇总数据
  for day in 0..730 {  // 2年 = 730天
    let day_timestamp = two_years_ago + (day * time_per_day)
    
    // 生成当天的多个时间序列数据
    let daily_metrics = []
    let metrics = ["cpu.usage", "memory.usage", "disk.usage"]
    
    for metric in metrics {
      let daily_values = []
      
      // 生成当天的小时数据
      for hour in 0..24 {
        let hour_timestamp = day_timestamp + (hour * 60 * 60 * 1000000000L)
        let value = generate_metric_value(metric, hour)
        
        daily_values.push({
          "timestamp": hour_timestamp,
          "value": value
        })
      }
      
      daily_metrics.push({
        "metric": metric,
        "values": daily_values
      })
    }
    
    historical_data.push({
      "date": day_timestamp,
      "metrics": daily_metrics
    })
    
    // 每30天批量存储一次
    if (day + 1) % 30 == 0 {
      let batch = historical_data.slice(day - 29, day + 1)
      
      // 压缩批量数据
      let compressed_batch = azimuth::TimeSeriesCompressor::compress_batch(
        azimuth::TimeSeriesCompressor::new(),
        batch
      )
      
      // 存储到长期存储
      azimuth::LongTermCompressedStorage::store_batch(long_term_storage, compressed_batch)
    }
  }
  
  // 测试不同时间范围的数据检索
  let test_queries = [
    {
      "name": "recent.data",
      "start": current_time - (7 * 24 * 60 * 60 * 1000000000L),  // 最近7天
      "end": current_time,
      "expected.tier": "hot"
    },
    {
      "name": "month.old.data",
      "start": current_time - (30 * 24 * 60 * 60 * 1000000000L),  // 30天前
      "end": current_time - (23 * 24 * 60 * 60 * 1000000000L),   // 23天前
      "expected.tier": "warm"
    },
    {
      "name": "year.old.data",
      "start": current_time - (365 * 24 * 60 * 60 * 1000000000L),  // 1年前
      "end": current_time - (335 * 24 * 60 * 60 * 1000000000L),   // 335天前
      "expected.tier": "cold"
    },
    {
      "name": "two.years.old.data",
      "start": two_years_ago,
      "end": two_years_ago + (30 * 24 * 60 * 60 * 1000000000L),  // 前30天
      "expected.tier": "archive"
    }
  ]
  
  for query in test_queries {
    let retrieval_query = azimuth::TimeSeriesQuery::new()
      .with_start_time(query["start"])
      .with_end_time(query["end"])
      .with_metrics(["cpu.usage", "memory.usage", "disk.usage"])
    
    let retrieval_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let retrieved_data = azimuth::LongTermCompressedStorage::query(long_term_storage, retrieval_query)
    let retrieval_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - retrieval_start
    
    // 验证检索结果
    assert_true(retrieved_data.length() > 0)
    
    // 验证数据来自正确的存储层
    let actual_tier = azimuth::LongTermCompressedStorage::get_data_tier(long_term_storage, query["start"])
    assert_eq(actual_tier, query["expected.tier"])
    
    // 验证数据完整性
    for metric_data in retrieved_data {
      assert_true(metric_data["data"].length() > 0)
      
      // 验证时间戳在查询范围内
      for data_point in metric_data["data"] {
        assert_true(data_point["timestamp"] >= query["start"])
        assert_true(data_point["timestamp"] <= query["end"])
      }
    }
    
    // 记录检索性能
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "long-term-storage")
    let retrieval_time_histogram = azimuth::Meter::create_histogram(meter, "retrieval.time", Some("Data retrieval time"), Some("ms"))
    let tier_retrieval_time_histogram = azimuth::Meter::create_histogram(meter, "tier.retrieval.time", Some("Tier-specific retrieval time"), Some("ms"))
    
    azimuth::Histogram::record(retrieval_time_histogram, retrieval_time.to_double() / 1000000.0, Some([
      ("query.type", azimuth::StringValue(query["name"]))
    ]))
    
    azimuth::Histogram::record(tier_retrieval_time_histogram, retrieval_time.to_double() / 1000000.0, Some([
      ("storage.tier", azimuth::StringValue(query["expected.tier"]))
    ]))
  }
  
  // 测试数据生命周期管理
  let expired_data_start = current_time - (400 * 24 * 60 * 60 * 1000000000L)  // 400天前（应该被清理）
  let expired_data_end = current_time - (370 * 24 * 60 * 60 * 1000000000L)   // 370天前
  
  // 清理过期数据
  let cleaned_count = azimuth::LongTermCompressedStorage::cleanup_expired_data(long_term_storage)
  
  // 验证过期数据已被清理
  let cleanup_query = azimuth::TimeSeriesQuery::new()
    .with_start_time(expired_data_start)
    .with_end_time(expired_data_end)
  
  let cleanup_result = azimuth::LongTermCompressedStorage::query(long_term_storage, cleanup_query)
  assert_eq(cleanup_result.length(), 0)  // 过期数据应该已被清理
  
  // 验证清理统计
  assert_true(cleaned_count > 0)
  
  // 记录清理度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "long-term-storage")
  let cleanup_counter = azimuth::Meter::create_counter(meter, "data.cleanup.count")
  azimuth::Counter::add(cleanup_counter, cleaned_count.to_double())
}

// 辅助函数：生成稳定值的时间序列
fn generate_steady_time_series(count : Int, base_value : Double, noise : Double) -> Array<Map<String, Any>> {
  let result = []
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..count {
    let timestamp = base_timestamp + (i * 1000000L)
    let value = base_value + (azimuth::Random::next_double(azimuth::Random::system()) - 0.5) * noise * 2
    
    result.push({
      "timestamp": timestamp,
      "value": value,
      "attributes": [("metric.type", "steady")]
    })
  }
  
  result
}

// 辅助函数：生成线性趋势的时间序列
fn generate_linear_trend_time_series(count : Int, start_value : Double, increment : Double) -> Array<Map<String, Any>> {
  let result = []
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..count {
    let timestamp = base_timestamp + (i * 1000000L)
    let value = start_value + (i.to_double() * increment)
    
    result.push({
      "timestamp": timestamp,
      "value": value,
      "attributes": [("metric.type", "linear.trend")]
    })
  }
  
  result
}

// 辅助函数：生成季节性模式的时间序列
fn generate_seasonal_time_series(count : Int, period : Int, base_value : Double, amplitude : Double) -> Array<Map<String, Any>> {
  let result = []
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..count {
    let timestamp = base_timestamp + (i * 1000000L)
    let value = base_value + amplitude * azimuth::Math::sin(2.0 * azimuth::Math::pi() * i.to_double() / period.to_double())
    
    result.push({
      "timestamp": timestamp,
      "value": value,
      "attributes": [("metric.type", "seasonal")]
    })
  }
  
  result
}

// 辅助函数：生成尖峰时间序列
fn generate_spike_time_series(count : Int, base_value : Double, spike_value : Double, spike_probability : Double) -> Array<Map<String, Any>> {
  let result = []
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..count {
    let timestamp = base_timestamp + (i * 1000000L)
    let value = if azimuth::Random::next_double(azimuth::Random::system()) < spike_probability {
      spike_value
    } else {
      base_value
    }
    
    result.push({
      "timestamp": timestamp,
      "value": value,
      "attributes": [("metric.type", "spike")]
    })
  }
  
  result
}

// 辅助函数：根据指标类型生成值
fn generate_metric_value(metric : String, index : Int) -> Double {
  match metric {
    "cpu.usage" => 30.0 + 20.0 * azimuth::Math::sin(index.to_double() * 0.1) + azimuth::Random::next_double(azimuth::Random::system()) * 10.0
    "memory.usage" => 50.0 + 15.0 * azimuth::Math::sin(index.to_double() * 0.05) + azimuth::Random::next_double(azimuth::Random::system()) * 8.0
    "disk.io" => 10.0 + 40.0 * azimuth::Math::sin(index.to_double() * 0.2) + azimuth::Random::next_double(azimuth::Random::system()) * 20.0
    "network.throughput" => 25.0 + 25.0 * azimuth::Math::sin(index.to_double() * 0.15) + azimuth::Random::next_double(azimuth::Random::system()) * 15.0
    "disk.usage" => 60.0 + 5.0 * azimuth::Math::sin(index.to_double() * 0.01) + azimuth::Random::next_double(azimuth::Random::system()) * 2.0
    _ => 50.0 + azimuth::Random::next_double(azimuth::Random::system()) * 20.0
  }
}

// 辅助函数：计算平均值
fn calculate_average(values : Array<Map<String, Any>>) -> Double {
  let sum = 0.0
  for value in values {
    sum = sum + value["value"]
  }
  sum / values.length().to_double()
}