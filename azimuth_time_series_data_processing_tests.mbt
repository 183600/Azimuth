// Azimuth 时间序列数据处理测试用例
// 测试遥测系统的时间序列数据处理能力和分析功能

test "时间序列数据采集和存储" {
  // 创建时间序列数据管理器
  let ts_manager = TimeSeriesManager::new()
  TimeSeriesManager::set_retention_period(ts_manager, 86400000) // 24小时保留期
  TimeSeriesManager::set_compression_enabled(ts_manager, true)
  TimeSeriesManager::set_max_series_count(ts_manager, 10000)
  
  // 创建遥测提供者并启用时间序列支持
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_time_series_enabled(config, true)
  TelemetryConfiguration::set_time_series_manager(config, ts_manager)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "timeseries.test")
  
  // 创建时间序列度量
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries.test.meter")
  
  let cpu_counter = Meter::create_counter(meter, "cpu.usage", Some("CPU usage percentage"), Some("%"))
  let memory_gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("MB"))
  let response_time_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // 生成时间序列数据
  let base_timestamp = Timestamp::now()
  let data_points = []
  
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = Timestamp::add_millis(base_timestamp, i * 1000) // 每秒一个数据点
    
    // CPU使用率（模拟周期性变化）
    let cpu_value = 50.0 + 30.0 * (i / 100.0).sin()
    Counter::add_with_timestamp(cpu_counter, cpu_value / 100.0, timestamp, [
      ("host", "server-1"),
      ("instance", "prod")
    ])
    
    // 内存使用率（模拟逐渐增长）
    let memory_value = 1024.0 + i * 0.5
    Gauge::set_with_timestamp(memory_gauge, memory_value, timestamp, [
      ("host", "server-1"),
      ("instance", "prod")
    ])
    
    // 响应时间（模拟随机波动）
    let response_time = 100.0 + 50.0 * (Math::random() * 2.0 - 1.0)
    Histogram::record_with_timestamp(response_time_histogram, response_time, timestamp, [
      ("endpoint", "/api/v1/data"),
      ("method", "GET")
    ])
    
    // 记录数据点信息
    data_points = data_points + [{
      "timestamp": timestamp,
      "cpu": cpu_value,
      "memory": memory_value,
      "response_time": response_time
    }]
  }
  
  // 验证时间序列数据被正确采集
  let ts_data = TimeSeriesManager::get_all_series(ts_manager)
  assert_true(ts_data.length() >= 3, "应该至少有3个时间序列")
  
  // 验证CPU时间序列
  let cpu_series = TimeSeriesManager::get_series(ts_manager, "cpu.usage")
  assert_true(cpu_series.is_some(), "应该有CPU使用率时间序列")
  
  let cpu_series_data = TimeSeriesManager::get_series_data(ts_manager, "cpu.usage", 
    base_timestamp, Timestamp::add_millis(base_timestamp, 1000000))
  assert_true(cpu_series_data.length() == 1000, "CPU时间序列应该有1000个数据点")
  
  // 验证数据点时间戳递增
  let timestamps = cpu_series_data.map(fn(point) { point.timestamp })
  for i = 1; i < timestamps.length(); i = i + 1 {
    assert_true(timestamps[i] > timestamps[i-1], "时间戳应该递增")
  }
  
  // 验证数据压缩
  let compression_stats = TimeSeriesManager::get_compression_stats(ts_manager)
  assert_true(compression_stats.compression_enabled, "压缩应该被启用")
  assert_true(compression_stats.compression_ratio > 1.0, "应该有压缩效果")
  
  // 测试时间范围查询
  let start_time = Timestamp::add_millis(base_timestamp, 100000) // 100秒后
  let end_time = Timestamp::add_millis(base_timestamp, 200000) // 200秒后
  
  let range_data = TimeSeriesManager::get_series_data(ts_manager, "cpu.usage", start_time, end_time)
  assert_eq(range_data.length(), 101, "时间范围查询应该返回101个数据点")
  
  // 验证查询的数据点在正确范围内
  for point in range_data {
    assert_true(point.timestamp >= start_time && point.timestamp <= end_time, 
      "查询的数据点应该在时间范围内")
  }
  
  // 测试数据聚合
  let aggregation_result = TimeSeriesManager::aggregate_series(ts_manager, "cpu.usage", 
    start_time, end_time, Average, 10000) // 10秒间隔聚合
  
  assert_true(aggregation_result.aggregated_points.length() > 0, "聚合应该产生数据点")
  assert_true(aggregation_result.aggregated_points.length() <= 11, 
    "101个点按10秒聚合应该不超过11个点")
  
  // 验证聚合计算正确性
  let first_aggregated_point = aggregation_result.aggregated_points[0]
  let expected_sum = range_data.slice(0, 10).fold(0.0, fn(acc, point) { acc + point.value })
  let expected_avg = expected_sum / 10.0
  
  assert_true((first_aggregated_point.value - expected_avg).abs() < 0.01, 
    "聚合值应该正确计算")
  
  // 测试时间序列标签过滤
  let labeled_data = TimeSeriesManager::get_series_data_with_labels(ts_manager, "cpu.usage", 
    base_timestamp, Timestamp::add_millis(base_timestamp, 1000000), [
    ("host", "server-1"),
    ("instance", "prod")
  ])
  
  assert_eq(labeled_data.length(), 1000, "标签过滤应该返回所有匹配的数据点")
  
  let mismatched_label_data = TimeSeriesManager::get_series_data_with_labels(ts_manager, "cpu.usage", 
    base_timestamp, Timestamp::add_millis(base_timestamp, 1000000), [
    ("host", "server-2") // 不匹配的标签
  ])
  
  assert_eq(mismatched_label_data.length(), 0, "不匹配的标签应该返回空结果")
  
  // 测试时间序列数据导出
  let export_result = TimeSeriesManager::export_series(ts_manager, "cpu.usage", 
    base_timestamp, Timestamp::add_millis(base_timestamp, 1000000), "json")
  
  assert_true(export_result.success, "时间序列导出应该成功")
  assert_true(export_result.data.length() > 0, "导出数据不应为空")
  
  // 测试时间序列数据导入
  let import_result = TimeSeriesManager::import_series(ts_manager, "cpu.usage.backup", 
    export_result.data, "json")
  
  assert_true(import_result.success, "时间序列导入应该成功")
  
  let imported_data = TimeSeriesManager::get_series_data(ts_manager, "cpu.usage.backup", 
    base_timestamp, Timestamp::add_millis(base_timestamp, 1000000))
  assert_eq(imported_data.length(), 1000, "导入的数据应该与原始数据一致")
  
  assert_true(true)
}

test "时间序列数据分析和统计" {
  // 创建时间序列分析器
  let ts_analyzer = TimeSeriesAnalyzer::new()
  TimeSeriesAnalyzer::set_analysis_window(ts_analyzer, 3600000) // 1小时分析窗口
  TimeSeriesAnalyzer::set_statistical_methods(ts_analyzer, [Mean, Median, StdDev, Percentile])
  
  // 创建测试时间序列数据
  let test_data = []
  let base_timestamp = Timestamp::now()
  
  // 生成带有趋势和季节性的测试数据
  for i = 0; i < 1440; i = i + 1 { // 24小时，每分钟一个点
    let timestamp = Timestamp::add_millis(base_timestamp, i * 60000) // 每分钟
    
    // 基础值 + 趋势 + 季节性 + 噪声
    let trend = i * 0.01 // 线性增长趋势
    let seasonal = 10.0 * (i * 2 * 3.14159 / 60).sin() // 每小时的季节性
    let noise = 5.0 * (Math::random() * 2.0 - 1.0) // 随机噪声
    let value = 50.0 + trend + seasonal + noise
    
    test_data = test_data + [TimeSeriesPoint::new(timestamp, value, [
      ("metric", "test.metric"),
      ("unit", "percent")
    ])]
  }
  
  // 基本统计分析
  let basic_stats = TimeSeriesAnalyzer::basic_statistics(ts_analyzer, test_data)
  
  assert_true(basic_stats.count == 1440, "统计数据点数应该正确")
  assert_true(basic_stats.mean > 40.0 && basic_stats.mean < 60.0, "平均值应该在合理范围内")
  assert_true(basic_stats.min > 20.0, "最小值应该大于20")
  assert_true(basic_stats.max < 100.0, "最大值应该小于100")
  assert_true(basic_stats.std_dev > 5.0, "标准差应该大于5")
  
  // 百分位数分析
  let percentiles = TimeSeriesAnalyzer::percentiles(ts_analyzer, test_data, [50, 90, 95, 99])
  
  assert_true(percentiles[50] > basic_stats.min && percentiles[50] < basic_stats.max, 
    "50%百分位数应该在最小值和最大值之间")
  assert_true(percentiles[90] > percentiles[50], "90%百分位数应该大于50%百分位数")
  assert_true(percentiles[95] > percentiles[90], "95%百分位数应该大于90%百分位数")
  assert_true(percentiles[99] > percentiles[95], "99%百分位数应该大于95%百分位数")
  
  // 趋势分析
  let trend_analysis = TimeSeriesAnalyzer::trend_analysis(ts_analyzer, test_data)
  
  assert_eq(trend_analysis.direction, "increasing", "应该检测到上升趋势")
  assert_true(trend_analysis.slope > 0.005 && trend_analysis.slope < 0.015, "斜率应该在合理范围内")
  assert_true(trend_analysis.r_squared > 0.5, "拟合度应该大于0.5")
  
  // 季节性分析
  let seasonality_analysis = TimeSeriesAnalyzer::seasonality_analysis(ts_analyzer, test_data, 60) // 60分钟周期
  
  assert_true(seasonality_analysis.has_seasonality, "应该检测到季节性")
  assert_true(seasonality_analysis.period == 60, "应该检测到60分钟周期")
  assert_true(seasonality_analysis.strength > 0.5, "季节性强度应该大于0.5")
  
  // 异常检测
  let anomaly_detection = TimeSeriesAnalyzer::anomaly_detection(ts_analyzer, test_data, 2.0) // 2倍标准差阈值
  
  assert_true(anomaly_detected.anomalies.length() > 0, "应该检测到一些异常")
  assert_true(anomaly_detected.anomalies.length() < test_data.length() * 0.1, 
    "异常点数量应该少于总数据点的10%")
  
  // 验证异常点确实偏离正常值
  for anomaly in anomaly_detected.anomalies {
    let z_score = (anomaly.value - basic_stats.mean) / basic_stats.std_dev
    assert_true(z_score.abs() > 2.0, "异常点的Z分数应该大于2")
  }
  
  // 变化点检测
  let change_point_detection = TimeSeriesAnalyzer::change_point_detection(ts_analyzer, test_data)
  
  assert_true(change_point_detection.change_points.length() >= 0, "应该有变化点检测结果")
  
  // 如果检测到变化点，验证其合理性
  if change_point_detection.change_points.length() > 0 {
    for change_point in change_point_detection.change_points {
      assert_true(change_point.index > 0 && change_point.index < test_data.length(), 
        "变化点索引应该在有效范围内")
      assert_true(change_point.confidence > 0.5, "变化点置信度应该大于0.5")
    }
  }
  
  // 相关性分析
  // 创建第二个时间序列用于相关性测试
  let correlated_data = []
  for i = 0; i < test_data.length(); i = i + 1 {
    let original_value = test_data[i].value
    let correlated_value = original_value * 0.8 + 10.0 + 2.0 * (Math::random() * 2.0 - 1.0)
    
    correlated_data = correlated_data + [TimeSeriesPoint::new(
      test_data[i].timestamp, 
      correlated_value, 
      [("metric", "correlated.metric")]
    )]
  }
  
  let correlation_analysis = TimeSeriesAnalyzer::correlation_analysis(ts_analyzer, test_data, correlated_data)
  
  assert_true(correlation_analysis.correlation_coefficient > 0.7, "相关性系数应该大于0.7")
  assert_true(correlation_analysis.p_value < 0.05, "P值应该小于0.05")
  
  // 预测分析
  let forecast_result = TimeSeriesAnalyzer::forecast(ts_analyzer, test_data.slice(0, 1200), 240) // 用前20小时预测后4小时
  
  assert_eq(forecast_result.forecast_points.length(), 240, "应该生成240个预测点")
  
  // 验证预测点的合理性
  for forecast_point in forecast_result.forecast_points {
    assert_true(forecast_point.value > 20.0 && forecast_point.value < 100.0, 
      "预测值应该在合理范围内")
  }
  
  // 验证预测置信区间
  for i = 0; i < forecast_result.forecast_points.length(); i = i + 1 {
    let forecast_point = forecast_result.forecast_points[i]
    let confidence_interval = forecast_result.confidence_intervals[i]
    
    assert_true(confidence_interval.lower < forecast_point.value, 
      "置信区间下限应该小于预测值")
    assert_true(confidence_interval.upper > forecast_point.value, 
      "置信区间上限应该大于预测值")
    assert_true(confidence_interval.upper - confidence_interval.lower > 0, 
      "置信区间应该有宽度")
  }
  
  // 预测准确性评估
  let actual_values = test_data.slice(1200, 1440).map(fn(point) { point.value })
  let predicted_values = forecast_result.forecast_points.map(fn(point) { point.value })
  
  let accuracy_metrics = TimeSeriesAnalyzer::evaluate_forecast(ts_analyzer, actual_values, predicted_values)
  
  assert_true(accuracy_metrics.mae > 0, "平均绝对误差应该大于0")
  assert_true(accuracy_metrics.rmse > 0, "均方根误差应该大于0")
  assert_true(accuracy_metrics.mape < 50.0, "平均绝对百分比误差应该小于50%")
  
  // 生成分析报告
  let analysis_report = TimeSeriesAnalyzer::generate_analysis_report(ts_analyzer, test_data)
  
  assert_true(analysis_report.basic_statistics.is_some(), "报告应该包含基本统计")
  assert_true(analysis_report.trend_analysis.is_some(), "报告应该包含趋势分析")
  assert_true(analysis_report.seasonality_analysis.is_some(), "报告应该包含季节性分析")
  assert_true(analysis_report.anomaly_detection.is_some(), "报告应该包含异常检测")
  
  assert_true(true)
}

test "时间序列数据降采样和聚合" {
  // 创建降采样管理器
  let downsampler = TimeSeriesDownsampler::new()
  TimeSeriesDownsampler::set_default_strategy(downsampler, Average)
  TimeSeriesDownsampler::set_max_raw_points(downsampler, 10000) // 最大原始点数
  TimeSeriesDownsampler::set_retention_policy(downsampler, [
    RetentionLevel::new("raw", 0, 86400000), // 1天原始数据
    RetentionLevel::new("1m", 60000, 604800000), // 1分钟数据，保留7天
    RetentionLevel::new("5m", 300000, 2592000000), // 5分钟数据，保留30天
    RetentionLevel::new("1h", 3600000, 31536000000) // 1小时数据，保留1年
  ])
  
  // 创建高频率测试数据
  let high_freq_data = []
  let base_timestamp = Timestamp::now()
  
  // 生成1天的高频率数据（每5秒一个点）
  for i = 0; i < 17280; i = i + 1 { // 24小时 * 720点/小时
    let timestamp = Timestamp::add_millis(base_timestamp, i * 5000) // 每5秒
    
    // 模拟服务器指标
    let cpu_usage = 30.0 + 20.0 * (i / 360.0).sin() + 5.0 * (Math::random() * 2.0 - 1.0)
    let memory_usage = 4096.0 + i * 0.1 + 100.0 * (Math::random() * 2.0 - 1.0)
    let disk_io = 50.0 + 30.0 * (i / 720.0).cos() + 10.0 * (Math::random() * 2.0 - 1.0)
    
    high_freq_data = high_freq_data + [
      TimeSeriesPoint::new(timestamp, cpu_usage, [("metric", "cpu.usage"), ("unit", "percent")]),
      TimeSeriesPoint::new(timestamp, memory_usage, [("metric", "memory.usage"), ("unit", "MB")]),
      TimeSeriesPoint::new(timestamp, disk_io, [("metric", "disk.io"), ("unit", "MB/s")])
    ]
  }
  
  // 按指标分组数据
  let cpu_data = high_freq_data.filter(fn(point) { 
    point.labels.any(fn(label) { label.0 == "metric" && label.1 == "cpu.usage" }) 
  })
  
  let memory_data = high_freq_data.filter(fn(point) { 
    point.labels.any(fn(label) { label.0 == "metric" && label.1 == "memory.usage" }) 
  })
  
  let disk_io_data = high_freq_data.filter(fn(point) { 
    point.labels.any(fn(label) { label.0 == "metric" && label.1 == "disk.io" }) 
  })
  
  // 测试1分钟降采样
  let downsampled_1m_cpu = TimeSeriesDownsampler::downsample(downsampler, cpu_data, 60000, Average)
  assert_eq(downsampled_1m_cpu.length(), 1440, "1分钟降采样应该有1440个点")
  
  // 验证降采样计算正确性
  let first_minute_original = cpu_data.slice(0, 12) // 12个5秒点 = 1分钟
  let first_minute_avg = first_minute_original.fold(0.0, fn(acc, point) { acc + point.value }) / 12.0
  
  assert_true((downsampled_1m_cpu[0].value - first_minute_avg).abs() < 0.01, 
    "1分钟降采样平均值应该正确计算")
  
  // 测试不同降采样策略
  let downsampled_1m_cpu_max = TimeSeriesDownsampler::downsample(downsampler, cpu_data, 60000, Max)
  let downsampled_1m_cpu_min = TimeSeriesDownsampler::downsample(downsampler, cpu_data, 60000, Min)
  let downsampled_1m_cpu_sum = TimeSeriesDownsampler::downsample(downsampler, cpu_data, 60000, Sum)
  let downsampled_1m_cpu_count = TimeSeriesDownsampler::downsample(downsampler, cpu_data, 60000, Count)
  
  // 验证不同策略的结果
  for i = 0; i < downsampled_1m_cpu.length(); i = i + 1 {
    let minute_start = i * 12
    let minute_end = (i + 1) * 12
    let minute_data = cpu_data.slice(minute_start, minute_end)
    
    let expected_max = minute_data.fold(0.0, fn(acc, point) { Math::max(acc, point.value) })
    let expected_min = minute_data.fold(999999.0, fn(acc, point) { Math::min(acc, point.value) })
    let expected_sum = minute_data.fold(0.0, fn(acc, point) { acc + point.value })
    let expected_count = minute_data.length().to_float()
    
    assert_true((downsampled_1m_cpu_max[i].value - expected_max).abs() < 0.01, 
      "最大值降采样应该正确计算")
    assert_true((downsampled_1m_cpu_min[i].value - expected_min).abs() < 0.01, 
      "最小值降采样应该正确计算")
    assert_true((downsampled_1m_cpu_sum[i].value - expected_sum).abs() < 0.01, 
      "求和降采样应该正确计算")
    assert_eq(downsampled_1m_cpu_count[i].value, expected_count, 
      "计数降采样应该正确计算")
  }
  
  // 测试5分钟降采样
  let downsampled_5m_memory = TimeSeriesDownsampler::downsample(downsampler, memory_data, 300000, Average)
  assert_eq(downsampled_5m_memory.length(), 288, "5分钟降采样应该有288个点")
  
  // 测试1小时降采样
  let downsampled_1h_disk_io = TimeSeriesDownsampler::downsample(downsampler, disk_io_data, 3600000, Average)
  assert_eq(downsampled_1h_disk_io.length(), 24, "1小时降采样应该有24个点")
  
  // 测试多级降采样
  let multi_level_result = TimeSeriesDownsampler::multi_level_downsample(downsampler, cpu_data)
  
  assert_true(multi_level_result.contains_key("1m"), "应该有1分钟级别数据")
  assert_true(multi_level_result.contains_key("5m"), "应该有5分钟级别数据")
  assert_true(multi_level_result.contains_key("1h"), "应该有1小时级别数据")
  
  // 验证多级降采样的一致性
  let level_1m = multi_level_result.get("1m").unwrap()
  let level_5m = multi_level_result.get("5m").unwrap()
  let level_1h = multi_level_result.get("1h").unwrap()
  
  // 1小时数据应该是5分钟数据的进一步降采样
  assert_eq(level_1h.length() * 12, level_5m.length(), "1小时和5分钟数据应该有正确的关系")
  
  // 测试降采样数据查询
  let query_start = Timestamp::add_millis(base_timestamp, 3600000) // 1小时后
  let query_end = Timestamp::add_millis(base_timestamp, 7200000) // 2小时后
  
  let downsampled_query = TimeSeriesDownsampler::query_downsampled_data(
    downsampler, "cpu.usage", query_start, query_end, "5m")
  
  assert_true(downsampled_query.success, "降采样数据查询应该成功")
  assert_true(downsampled_query.data.length() > 0, "应该返回查询数据")
  
  // 验证查询数据在正确的时间范围内
  for point in downsampled_query.data {
    assert_true(point.timestamp >= query_start && point.timestamp <= query_end, 
      "查询的数据点应该在时间范围内")
  }
  
  // 测试降采样数据压缩
  let compression_result = TimeSeriesDownsampler::compress_downsampled_data(downsampler, level_5m)
  
  assert_true(compression_result.success, "降采样数据压缩应该成功")
  assert_true(compression_result.compressed_size < compression_result.original_size, 
    "压缩后大小应该减少")
  assert_true(compression_result.compression_ratio > 1.0, "压缩比应该大于1")
  
  // 测试压缩数据解压
  let decompression_result = TimeSeriesDownsampler::decompress_downsampled_data(
    downsampler, compression_result.compressed_data)
  
  assert_true(decompression_result.success, "数据解压应该成功")
  assert_eq(decompression_result.data.length(), level_5m.length(), 
    "解压后数据点数应该与原始数据一致")
  
  // 验证解压数据的正确性
  for i = 0; i < decompression_result.data.length(); i = i + 1 {
    assert_eq(decompression_result.data[i].timestamp, level_5m[i].timestamp, 
      "解压后时间戳应该与原始数据一致")
    assert_true((decompression_result.data[i].value - level_5m[i].value).abs() < 0.01, 
      "解压后值应该与原始数据一致")
  }
  
  // 测试降采样数据保留策略
  let retention_result = TimeSeriesDownsampler::apply_retention_policy(downsampler)
  
  assert_true(retention_result.success, "保留策略应用应该成功")
  assert_true(retention_result.deleted_raw_points > 0, "应该删除一些原始数据点")
  assert_true(retention_result.kept_downsampled_points > 0, "应该保留降采样数据点")
  
  // 验证保留策略效果
  let final_raw_count = TimeSeriesDownsampler::get_raw_points_count(downsampler)
  assert_true(final_raw_count <= 10000, "原始数据点数应该在限制内")
  
  // 生成降采样报告
  let downsample_report = TimeSeriesDownsampler::generate_report(downsampler)
  
  assert_true(downsample_report.total_original_points > 0, "应该有原始数据点统计")
  assert_true(downsample_report.total_downsampled_points > 0, "应该有降采样数据点统计")
  assert_true(downsample_report.compression_ratio > 1.0, "应该有压缩比统计")
  assert_true(downsample_report.space_saving_percentage > 0.0, "应该有空间节省百分比")
  
  assert_true(true)
}

test "实时时间序列流处理" {
  // 创建实时流处理器
  let stream_processor = RealTimeStreamProcessor::new()
  RealTimeStreamProcessor::set_buffer_size(stream_processor, 10000)
  RealTimeStreamProcessor::set_processing_interval(stream_processor, 1000) // 1秒处理间隔
  RealTimeStreamProcessor::set_late_data_tolerance(stream_processor, 30000) // 30秒延迟容忍
  
  // 创建流处理管道
  let pipeline = StreamPipeline::new()
  
  // 添加处理阶段
  StreamPipeline::add_stage(pipeline, ValidationStage::new())
  StreamPipeline::add_stage(pipeline, NormalizationStage::new())
  StreamPipeline::add_stage(pipeline, AggregationStage::new(60000)) // 1分钟聚合
  StreamPipeline::add_stage(pipeline, AnomalyDetectionStage::new(2.0)) // 2倍标准差异常检测
  StreamPipeline::add_stage(pipeline, AlertingStage::new())
  
  RealTimeStreamProcessor::set_pipeline(stream_processor, pipeline)
  
  // 创建实时数据生成器
  let data_generator = RealTimeDataGenerator::new()
  RealTimeDataGenerator::set_frequency(data_generator, 10) // 每秒10个点
  RealTimeDataGenerator::set_metrics(data_generator, [
    MetricConfig::new("cpu.usage", "percent", 0.0, 100.0),
    MetricConfig::new("memory.usage", "MB", 0.0, 16384.0),
    MetricConfig::new("response.time", "ms", 0.0, 10000.0)
  ])
  
  // 启动流处理器
  RealTimeStreamProcessor::start(stream_processor)
  RealTimeDataGenerator::start(data_generator, stream_processor)
  
  // 让系统运行一段时间
  simulate_delay(10000) // 运行10秒
  
  // 验证流处理状态
  let processing_stats = RealTimeStreamProcessor::get_processing_stats(stream_processor)
  
  assert_true(processing_stats.is_running, "流处理器应该正在运行")
  assert_true(processing_stats.total_points_processed > 0, "应该处理了一些数据点")
  assert_true(processing_stats.processing_rate > 0, "应该有处理速率")
  assert_true(processing_stats.average_processing_latency < 1000, 
    "平均处理延迟应该小于1秒")
  
  // 验证缓冲区状态
  let buffer_stats = RealTimeStreamProcessor::get_buffer_stats(stream_processor)
  
  assert_true(buffer_stats.current_size > 0, "缓冲区应该有数据")
  assert_true(buffer_stats.current_size <= buffer_stats.max_size, 
    "缓冲区大小不应超过最大值")
  assert_true(buffer_stats.overflow_count == 0, "不应该有缓冲区溢出")
  
  // 验证管道处理统计
  let pipeline_stats = StreamPipeline::get_stage_stats(pipeline)
  
  assert_true(pipeline_stats.length() == 5, "应该有5个处理阶段")
  
  for stage_stat in pipeline_stats {
    assert_true(stage_stat.processed_points > 0, "每个阶段应该处理了一些数据点")
    assert_true(stage_stat.error_count == 0, "每个阶段不应该有错误")
    assert_true(stage_stat.average_processing_time < 100, 
      "每个阶段平均处理时间应该小于100ms")
  }
  
  // 测试实时聚合
  let aggregation_results = RealTimeStreamProcessor::get_latest_aggregations(stream_processor)
  
  assert_true(aggregation_results.length() > 0, "应该有聚合结果")
  
  for aggregation in aggregation_results {
    assert_true(aggregation.metric_name.length() > 0, "聚合应该有指标名称")
    assert_true(aggregation.aggregation_window > 0, "聚合应该有时间窗口")
    assert_true(aggregation.value >= 0.0, "聚合值应该非负")
    assert_true(aggregation.timestamp > 0, "聚合应该有时间戳")
  }
  
  // 测试实时异常检测
  let anomaly_results = RealTimeStreamProcessor::get_latest_anomalies(stream_processor)
  
  // 如果有异常，验证其合理性
  if anomaly_results.length() > 0 {
    for anomaly in anomaly_results {
      assert_true(anomaly.metric_name.length() > 0, "异常应该有指标名称")
      assert_true(anomaly.anomaly_score > 2.0, "异常分数应该大于阈值")
      assert_true(anomaly.timestamp > 0, "异常应该有时间戳")
      assert_true(anomaly.description.length() > 0, "异常应该有描述")
    }
  }
  
  // 测试实时告警
  let alert_results = RealTimeStreamProcessor::get_latest_alerts(stream_processor)
  
  // 如果有告警，验证其合理性
  if alert_results.length() > 0 {
    for alert in alert_results {
      assert_true(alert.alert_name.length() > 0, "告警应该有名称")
      assert_true(alert.severity.length() > 0, "告警应该有严重级别")
      assert_true(alert.timestamp > 0, "告警应该有时间戳")
      assert_true(alert.message.length() > 0, "告警应该有消息")
    }
  }
  
  // 测试延迟数据处理
  let late_data = []
  let old_timestamp = Timestamp::sub_millis(Timestamp::now(), 20000) // 20秒前的时间戳
  
  for i = 0; i < 10; i = i + 1 {
    late_data = late_data + [TimeSeriesPoint::new(
      Timestamp::add_millis(old_timestamp, i * 1000),
      50.0 + i.to_float(),
      [("metric", "late.data"), ("unit", "percent")]
    )]
  }
  
  // 发送延迟数据
  for point in late_data {
    RealTimeStreamProcessor::process_point(stream_processor, point)
  }
  
  // 等待延迟数据处理
  simulate_delay(2000)
  
  // 验证延迟数据被处理
  let late_data_stats = RealTimeStreamProcessor::get_late_data_stats(stream_processor)
  
  assert_true(late_data_stats.late_points_received > 0, "应该接收到延迟数据")
  assert_true(late_data_stats.late_points_processed > 0, "应该处理了延迟数据")
  assert_true(late_data_stats.late_points_rejected == 0, 
    "在容忍范围内的延迟数据不应该被拒绝")
  
  // 测试高负载场景
  let high_load_generator = HighLoadDataGenerator::new()
  HighLoadDataGenerator::set_frequency(high_load_generator, 1000) // 每秒1000个点
  
  HighLoadDataGenerator::start(high_load_generator, stream_processor)
  simulate_delay(5000) // 运行5秒高负载
  
  // 验证高负载下的性能
  let high_load_stats = RealTimeStreamProcessor::get_processing_stats(stream_processor)
  
  assert_true(high_load_stats.processing_rate > 500, "高负载下处理速率应该保持较高")
  assert_true(high_load_stats.average_processing_latency < 5000, 
    "高负载下平均处理延迟应该可接受")
  assert_true(high_load_stats.error_rate < 0.01, "高负载下错误率应该很低")
  
  // 停止数据生成器
  RealTimeDataGenerator::stop(data_generator)
  HighLoadDataGenerator::stop(high_load_generator)
  
  // 等待剩余数据处理完成
  simulate_delay(3000)
  
  // 停止流处理器
  RealTimeStreamProcessor::stop(stream_processor)
  
  // 验证最终状态
  let final_stats = RealTimeStreamProcessor::get_processing_stats(stream_processor)
  
  assert_false(final_stats.is_running, "流处理器应该已停止")
  assert_true(final_stats.total_points_processed > 1000, "应该处理了大量数据点")
  assert_true(final_stats.buffer_size == 0, "缓冲区应该被清空")
  
  // 生成流处理报告
  let stream_report = RealTimeStreamProcessor::generate_report(stream_processor)
  
  assert_true(stream_report.total_processing_time > 0, "应该有总处理时间")
  assert_true(stream_report.average_throughput > 0, "应该有平均吞吐量")
  assert_true(stream_report.peak_throughput >= stream_report.average_throughput, 
    "峰值吞吐量应该大于等于平均吞吐量")
  assert_true(stream_report.total_anomalies_detected >= 0, "应该有异常检测统计")
  assert_true(stream_report.total_alerts_generated >= 0, "应该有告警生成统计")
  
  assert_true(true)
}

test "时间序列数据可视化和报告" {
  // 创建可视化管理器
  let viz_manager = TimeSeriesVisualizationManager::new()
  TimeSeriesVisualizationManager::set_default_chart_type(viz_manager, LineChart)
  TimeSeriesVisualizationManager::set_max_data_points(viz_manager, 1000) // 最大显示点数
  TimeSeriesVisualizationManager::set_theme(viz_manager, "light")
  
  // 创建测试时间序列数据
  let test_series = []
  let base_timestamp = Timestamp::now()
  
  // 生成多个指标的时间序列数据
  let metrics = ["cpu.usage", "memory.usage", "disk.io", "network.in", "network.out"]
  let base_values = [50.0, 4096.0, 100.0, 1000.0, 800.0]
  
  for metric_index = 0; metric_index < metrics.length(); metric_index = metric_index + 1 {
    let metric_data = []
    let metric_name = metrics[metric_index]
    let base_value = base_values[metric_index]
    
    for i = 0; i < 200; i = i + 1 {
      let timestamp = Timestamp::add_millis(base_timestamp, i * 300000) // 每5分钟
      
      // 生成带有趋势和季节性的数据
      let trend = i * 0.1
      let seasonal = base_value * 0.2 * (i * 2 * 3.14159 / 24).sin() // 24小时季节性
      let noise = base_value * 0.1 * (Math::random() * 2.0 - 1.0)
      let value = base_value + trend + seasonal + noise
      
      metric_data = metric_data + [TimeSeriesPoint::new(timestamp, value, [
        ("metric", metric_name),
        ("unit", if metric_name == "cpu.usage" { "percent" } else { "MB" })
      ])]
    }
    
    test_series = test_series + [TimeSeries::new(metric_name, metric_data)]
  }
  
  // 测试基本图表生成
  let chart_config = ChartConfig::new()
  ChartConfig::set_title(chart_config, "System Metrics Overview")
  ChartConfig::set_width(chart_config, 800)
  ChartConfig::set_height(chart_config, 600)
  ChartConfig::set_time_range(chart_config, base_timestamp, 
    Timestamp::add_millis(base_timestamp, 200 * 300000))
  
  let line_chart_result = TimeSeriesVisualizationManager::create_line_chart(
    viz_manager, test_series.slice(0, 2), chart_config) // 只显示CPU和内存
  
  assert_true(line_chart_result.success, "折线图生成应该成功")
  assert_true(line_chart_result.chart_data.length() > 0, "图表数据不应为空")
  assert_true(line_chart_result.metadata.width == 800, "图表宽度应该正确")
  assert_true(line_chart_result.metadata.height == 600, "图表高度应该正确")
  
  // 测试柱状图生成
  let bar_chart_result = TimeSeriesVisualizationManager::create_bar_chart(
    viz_manager, test_series.slice(2, 4), chart_config) // 显示磁盘IO和网络输入
  
  assert_true(bar_chart_result.success, "柱状图生成应该成功")
  assert_true(bar_chart_result.chart_data.length() > 0, "图表数据不应为空")
  
  // 测试面积图生成
  let area_chart_result = TimeSeriesVisualizationManager::create_area_chart(
    viz_manager, test_series.slice(3, 5), chart_config) // 显示网络输入输出
  
  assert_true(area_chart_result.success, "面积图生成应该成功")
  assert_true(area_chart_result.chart_data.length() > 0, "图表数据不应为空")
  
  // 测试散点图生成（需要两个系列）
  let scatter_chart_result = TimeSeriesVisualizationManager::create_scatter_chart(
    viz_manager, test_series.slice(0, 2), chart_config) // CPU vs 内存
  
  assert_true(scatter_chart_result.success, "散点图生成应该成功")
  assert_true(scatter_chart_result.chart_data.length() > 0, "图表数据不应为空")
  
  // 测试热力图生成
  let heatmap_data = []
  for i = 0; i < 24; i = i + 1 { // 24小时
    for j = 0; j < 7; j = j + 1 { // 7天
      let value = 50.0 + 20.0 * (i * 2 * 3.14159 / 24).sin() + 10.0 * (j * 2 * 3.14159 / 7).cos()
      heatmap_data = heatmap_data + [HeatmapPoint::new(i, j, value)]
    }
  }
  
  let heatmap_result = TimeSeriesVisualizationManager::create_heatmap(
    viz_manager, heatmap_data, chart_config)
  
  assert_true(heatmap_result.success, "热力图生成应该成功")
  assert_true(heatmap_result.chart_data.length() > 0, "图表数据不应为空")
  
  // 测试仪表盘生成
  let dashboard_config = DashboardConfig::new()
  DashboardConfig::set_title(dashboard_config, "System Monitoring Dashboard")
  DashboardConfig::set_layout(dashboard_config, GridLayout(2, 3)) // 2行3列布局
  
  let chart_configs = [
    ChartConfig::new_with_title("CPU Usage"),
    ChartConfig::new_with_title("Memory Usage"),
    ChartConfig::new_with_title("Disk I/O"),
    ChartConfig::new_with_title("Network In"),
    ChartConfig::new_with_title("Network Out"),
    ChartConfig::new_with_title("System Overview")
  ]
  
  let dashboard_result = TimeSeriesVisualizationManager::create_dashboard(
    viz_manager, test_series, chart_configs, dashboard_config)
  
  assert_true(dashboard_result.success, "仪表盘生成应该成功")
  assert_true(dashboard_result.dashboard.charts.length() == 6, "仪表盘应该有6个图表")
  
  // 测试图表交互功能
  let chart_interaction = ChartInteraction::new(line_chart_result.chart)
  
  // 测试缩放功能
  let zoom_result = ChartInteraction::zoom(chart_interaction, 
    Timestamp::add_millis(base_timestamp, 10 * 300000), // 50分钟处
    Timestamp::add_millis(base_timestamp, 50 * 300000)) // 250分钟处
  
  assert_true(zoom_result.success, "图表缩放应该成功")
  assert_true(zoom_result.visible_data_points < line_chart_result.chart_data.length(), 
    "缩放后可见数据点应该减少")
  
  // 测试平移功能
  let pan_result = ChartInteraction::pan(chart_interaction, 300000) // 向右平移5分钟
  
  assert_true(pan_result.success, "图表平移应该成功")
  
  // 测试数据点悬停信息
  let hover_result = ChartInteraction::get_point_info(chart_interaction, 
    Timestamp::add_millis(base_timestamp, 25 * 300000)) // 125分钟处
  
  assert_true(hover_result.success, "获取数据点信息应该成功")
  assert_true(hover_result.point_info.length() > 0, "数据点信息不应为空")
  
  // 测试图表导出
  let export_png_result = TimeSeriesVisualizationManager::export_chart(
    viz_manager, line_chart_result.chart, "png")
  
  assert_true(export_png_result.success, "PNG导出应该成功")
  assert_true(export_png_result.data.length() > 0, "导出数据不应为空")
  
  let export_svg_result = TimeSeriesVisualizationManager::export_chart(
    viz_manager, line_chart_result.chart, "svg")
  
  assert_true(export_svg_result.success, "SVG导出应该成功")
  assert_true(export_svg_result.data.length() > 0, "导出数据不应为空")
  
  let export_pdf_result = TimeSeriesVisualizationManager::export_chart(
    viz_manager, line_chart_result.chart, "pdf")
  
  assert_true(export_pdf_result.success, "PDF导出应该成功")
  assert_true(export_pdf_result.data.length() > 0, "导出数据不应为空")
  
  // 测试报告生成
  let report_config = ReportConfig::new()
  ReportConfig::set_title(report_config, "System Performance Report")
  ReportConfig::set_time_range(report_config, base_timestamp, 
    Timestamp::add_millis(base_timestamp, 200 * 300000))
  ReportConfig::set_include_charts(report_config, true)
  ReportConfig::set_include_statistics(report_config, true)
  ReportConfig::set_include_anomalies(report_config, true)
  
  let report_result = TimeSeriesVisualizationManager::generate_report(
    viz_manager, test_series, report_config)
  
  assert_true(report_result.success, "报告生成应该成功")
  assert_true(report_result.report.content.length() > 0, "报告内容不应为空")
  assert_true(report_result.report.charts.length() > 0, "报告应该包含图表")
  assert_true(report_result.report.statistics.length() > 0, "报告应该包含统计信息")
  
  // 测试报告导出
  let export_html_result = TimeSeriesVisualizationManager::export_report(
    viz_manager, report_result.report, "html")
  
  assert_true(export_html_result.success, "HTML报告导出应该成功")
  assert_true(export_html_result.data.length() > 0, "导出数据不应为空")
  assert_true(export_html_result.data.contains("<html>"), "导出数据应该是有效的HTML")
  
  let export_pdf_report_result = TimeSeriesVisualizationManager::export_report(
    viz_manager, report_result.report, "pdf")
  
  assert_true(export_pdf_report_result.success, "PDF报告导出应该成功")
  assert_true(export_pdf_report_result.data.length() > 0, "导出数据不应为空")
  
  // 测试实时图表更新
  let real_time_chart = RealTimeChart::new(line_chart_result.chart)
  RealTimeChart::set_update_interval(real_time_chart, 1000) // 1秒更新间隔
  
  RealTimeChart::start_updates(real_time_chart)
  
  // 添加新数据点
  for i = 0; i < 10; i = i + 1 {
    let new_timestamp = Timestamp::add_millis(
      Timestamp::add_millis(base_timestamp, 200 * 300000), 
      i * 1000)
    
    let new_value = 50.0 + 10.0 * (Math::random() * 2.0 - 1.0)
    let new_point = TimeSeriesPoint::new(new_timestamp, new_value, [
      ("metric", "cpu.usage"),
      ("unit", "percent")
    ])
    
    RealTimeChart::add_data_point(real_time_chart, new_point)
    simulate_delay(100) // 等待100ms
  }
  
  // 等待图表更新
  simulate_delay(2000)
  
  // 验证图表已更新
  let updated_chart = RealTimeChart::get_chart(real_time_chart)
  assert_true(updated_chart.data_points > line_chart_result.chart.data_points, 
    "实时图表应该有更多数据点")
  
  RealTimeChart::stop_updates(real_time_chart)
  
  // 测试图表主题切换
  let dark_theme_result = TimeSeriesVisualizationManager::apply_theme(
    viz_manager, line_chart_result.chart, "dark")
  
  assert_true(dark_theme_result.success, "主题切换应该成功")
  assert_true(dark_theme_result.theme.name == "dark", "应该应用深色主题")
  
  // 生成可视化报告
  let viz_report = TimeSeriesVisualizationManager::generate_visualization_report(viz_manager)
  
  assert_true(viz_report.total_charts_generated > 0, "应该有图表生成统计")
  assert_true(viz_report.total_reports_generated > 0, "应该有报告生成统计")
  assert_true(viz_report.average_rendering_time > 0, "应该有平均渲染时间统计")
  assert_true(viz_report.total_export_operations > 0, "应该有导出操作统计")
  
  assert_true(true)
}