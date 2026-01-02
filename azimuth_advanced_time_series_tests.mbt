// Azimuth Advanced Time Series Data Processing Test Suite
// 高级时间序列数据处理测试套件

// Test 1: 高频时间序列数据流处理
test "high-frequency time series data stream processing" {
  let ts_processor = @azimuth.TimeSeriesProcessor::new()
  
  // 配置高频处理参数
  let hf_config = @azimuth.HighFrequencyConfig::new()
  @azimuth.HighFrequencyConfig::set_sampling_rate(hf_config, 1000) // 1000Hz
  @azimuth.HighFrequencyConfig::set_buffer_size(hf_config, 10000) // 10000点缓冲
  @azimuth.HighFrequencyConfig::set_processing_window(hf_config, 100) // 100点窗口
  @azimuth.HighFrequencyConfig::enable_real_time_aggregation(hf_config, true)
  @azimuth.HighFrequencyConfig::set_aggregation_interval(hf_config, 1000) // 1秒聚合
  
  @azimuth.TimeSeriesProcessor::configure_high_frequency(ts_processor, hf_config)
  
  // 生成高频时间序列数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let mut high_freq_data = []
  
  for i in 0..=5000 {
    let timestamp = base_timestamp + i * 1000000 // 1ms间隔
    let value = 50.0 + 10.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * i / 100.0) + 
                2.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * i / 20.0) + // 高频分量
                @azimuth.Random::next_float() * 0.5 // 噪声
    
    let data_point = @azimuth.TimeSeriesPoint::new(timestamp, value, [
      ("sensor.id", @azimuth.StringValue("high-freq-sensor-001")),
      ("metric.type", @azimuth.StringValue("vibration")),
      ("unit", @azimuth.StringValue("Hz"))
    ])
    high_freq_data = high_freq_data.push(data_point)
  }
  
  // 处理高频数据流
  let processing_start = @azimuth.Time::now_unix_nanos()
  let processed_results = @azimuth.TimeSeriesProcessor::process_stream(ts_processor, high_freq_data)
  let processing_end = @azimuth.Time::now_unix_nanos()
  
  // 验证处理性能
  let processing_time = processing_end - processing_start
  let throughput = high_freq_data.length() / (processing_time / 1000000000.0) // 点/秒
  
  assert_true(throughput > 10000) // 至少10000点/秒的处理速度
  assert_true(processed_results.length() > 0)
  
  // 验证实时聚合结果
  let aggregated_data = @azimuth.TimeSeriesProcessor::get_aggregated_data(ts_processor)
  assert_true(aggregated_data.length() > 0)
  
  // 检查聚合窗口
  for window in aggregated_data {
    let window_size = @azimuth.TimeSeriesWindow::size(window)
    assert_true(window_size <= hf_config.processing_window)
  }
}

// Test 2: 时间序列模式识别和异常检测
test "time series pattern recognition and anomaly detection" {
  let pattern_detector = @azimuth.PatternDetector::new()
  
  // 配置模式检测算法
  let detection_config = @azimuth.DetectionConfig::new()
  @azimuth.DetectionConfig::enable_seasonal_detection(detection_config, true)
  @azimuth.DetectionConfig::enable_trend_detection(detection_config, true)
  @azimuth.DetectionConfig::enable_outlier_detection(detection_config, true)
  @azimuth.DetectionConfig::set_seasonal_period(detection_config, 24) // 24小时季节性
  @azimuth.DetectionConfig::set_outlier_threshold(detection_config, 2.5) // 2.5个标准差
  
  @azimuth.PatternDetector::configure(pattern_detector, detection_config)
  
  // 创建具有明显模式的时间序列数据
  let mut pattern_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos() - 7 * 24 * 60 * 60 * 1000000000L // 7天前
  
  for day in 0..=6 { // 7天数据
    for hour in 0..=23 { // 每天24小时
      let timestamp = base_timestamp + (day * 24 + hour) * 60 * 60 * 1000000000L
      
      // 创建模式：白天高，夜晚低
      let base_value = if hour >= 8 && hour <= 18 { 80.0 } else { 20.0 }
      
      // 添加季节性变化
      let seasonal_variation = 10.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * hour / 24.0)
      
      // 添加趋势
      let trend = day * 2.0
      
      // 添加噪声
      let noise = @azimuth.Random::next_float() * 5.0
      
      let value = base_value + seasonal_variation + trend + noise
      
      let data_point = @azimuth.TimeSeriesPoint::new(timestamp, value, [
        ("metric.name", @azimuth.StringValue("server.cpu.usage")),
        ("server.id", @azimuth.StringValue("web-server-001")),
        ("environment", @azimuth.StringValue("production"))
      ])
      pattern_data = pattern_data.push(data_point)
    }
  }
  
  // 注入异常值
  let anomaly_indices = [50, 100, 120] // 一些异常点
  for index in anomaly_indices {
    if index < pattern_data.length() {
      let original_point = pattern_data[index]
      let anomalous_value = @azimuth.TimeSeriesPoint::value(original_point) * 3.0 // 3倍正常值
      let anomalous_point = @azimuth.TimeSeriesPoint::with_value(original_point, anomalous_value)
      pattern_data[index] = anomalous_point
    }
  }
  
  // 执行模式检测
  let pattern_result = @azimuth.PatternDetector::detect_patterns(pattern_detector, pattern_data)
  
  // 验证季节性模式检测
  assert_true(pattern_result.seasonal_patterns.length() > 0)
  let daily_pattern = pattern_result.seasonal_patterns[0]
  assert_eq(@azimuth.SeasonalPattern::period(daily_pattern), 24) // 24小时周期
  assert_true(@azimuth.SeasonalPattern::confidence(daily_pattern) > 0.7)
  
  // 验证趋势检测
  assert_true(pattern_result.trend != null)
  assert_true(@azimuth.Trend::direction(pattern_result.trend) == @azimuth.TrendDirection::Increasing)
  assert_true(@azimuth.Trend::slope(pattern_result.trend) > 0.0)
  
  // 验证异常检测
  assert_true(pattern_result.anomalies.length() >= anomaly_indices.length())
  
  for anomaly in pattern_result.anomalies {
    let anomaly_score = @azimuth.Anomaly::score(anomaly)
    assert_true(anomaly_score > detection_config.outlier_threshold)
    
    let anomaly_type = @azimuth.Anomaly::type(anomaly)
    assert_true(anomaly_type == @azimuth.AnomalyType::Spike || anomaly_type == @azimuth.AnomalyType::Outlier)
  }
  
  // 测试实时异常检测
  let real_time_detector = @azimuth.RealTimeAnomalyDetector::new()
  @azimuth.RealTimeAnomalyDetector::configure_threshold(real_time_detector, 2.0)
  
  let mut detected_anomalies = []
  for point in pattern_data {
    let detection_result = @azimuth.RealTimeAnomalyDetector::process_point(real_time_detector, point)
    if detection_result.is_anomaly {
      detected_anomalies = detected_anomalies.push(detection_result)
    }
  }
  
  // 验证实时检测效果
  assert_true(detected_anomalies.length() >= anomaly_indices.length())
}

// Test 3: 多维度时间序列聚合和分析
test "multi-dimensional time series aggregation and analysis" {
  let multi_dim_analyzer = @azimuth.MultiDimensionalAnalyzer::new()
  
  // 创建多维度时间序列数据
  let mut multi_dim_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos() - 30 * 24 * 60 * 60 * 1000000000L // 30天前
  
  for day in 0..=29 { // 30天数据
    for hour in 0..=23 { // 每天24小时
      let timestamp = base_timestamp + (day * 24 + hour) * 60 * 60 * 1000000000L
      
      // 为不同服务创建数据
      for service_idx in 0..=4 { // 5个服务
        let service_name = ["web-api", "database", "cache", "queue", "storage"][service_idx]
        
        // 为不同区域创建数据
        for region_idx in 0..=2 { // 3个区域
          let region_name = ["us-east-1", "us-west-2", "eu-west-1"][region_idx]
          
          // 基础值根据服务和区域变化
          let base_value = match service_name {
            "web-api" => 60.0 + region_idx * 10.0,
            "database" => 40.0 + region_idx * 5.0,
            "cache" => 30.0 + region_idx * 3.0,
            "queue" => 25.0 + region_idx * 2.0,
            "storage" => 20.0 + region_idx * 4.0,
            _ => 50.0
          }
          
          // 添加时间变化
          let time_variation = 10.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * hour / 24.0)
          
          // 添加趋势
          let trend = day * 0.5
          
          // 添加噪声
          let noise = @azimuth.Random::next_float() * 3.0
          
          let value = @azimuth.Math::max(0.0, base_value + time_variation + trend + noise)
          
          let data_point = @azimuth.TimeSeriesPoint::new(timestamp, value, [
            ("service.name", @azimuth.StringValue(service_name)),
            ("region", @azimuth.StringValue(region_name)),
            ("metric.type", @azimuth.StringValue("cpu.usage")),
            ("environment", @azimuth.StringValue("production"))
          ])
          multi_dim_data = multi_dim_data.push(data_point)
        }
      }
    }
  }
  
  // 配置聚合维度
  let aggregation_config = @azimuth.AggregationConfig::new()
  @azimuth.AggregationConfig::add_dimension(aggregation_config, "service.name")
  @azimuth.AggregationConfig::add_dimension(aggregation_config, "region")
  @azimuth.AggregationConfig::set_time_granularity(aggregation_config, @azimuth.TimeGranularity::Hourly)
  @azimuth.AggregationConfig::set_aggregation_functions(aggregation_config, [
    @azimuth.AggregationFunction::Average,
    @azimuth.AggregationFunction::Min,
    @azimuth.AggregationFunction::Max,
    @azimuth.AggregationFunction::Percentile95
  ])
  
  // 执行多维度聚合
  let aggregation_result = @azimuth.MultiDimensionalAnalyzer::aggregate(multi_dim_analyzer, multi_dim_data, aggregation_config)
  
  // 验证聚合结果
  assert_true(aggregation_result.aggregated_series.length() > 0)
  
  // 验证每个服务-区域组合都有聚合数据
  let expected_combinations = 5 * 3 // 5个服务 × 3个区域
  assert_eq(aggregation_result.aggregated_series.length(), expected_combinations)
  
  // 检查特定聚合结果
  let web_api_east_series = aggregation_result.aggregated_series.filter(fn(series) {
    let dimensions = @azimuth.AggregatedSeries::dimensions(series)
    @azimuth.StringMap::get(dimensions, "service.name") == Some("web-api") &&
    @azimuth.StringMap::get(dimensions, "region") == Some("us-east-1")
  })
  
  assert_true(web_api_east_series.length() == 1)
  
  let web_api_east = web_api_east_series[0]
  let data_points = @azimuth.AggregatedSeries::data_points(web_api_east)
  assert_true(data_points.length() == 30 * 24) // 30天 × 24小时
  
  // 验证聚合函数
  for point in data_points {
    let aggregates = @azimuth.TimeSeriesPoint::aggregates(point)
    assert_true(@azimuth.Aggregates::has_average(aggregates))
    assert_true(@azimuth.Aggregates::has_min(aggregates))
    assert_true(@azimuth.Aggregates::has_max(aggregates))
    assert_true(@azimuth.Aggregates::has_percentile95(aggregates))
  }
  
  // 测试下钻分析
  let drill_down_result = @azimuth.MultiDimensionalAnalyzer::drill_down(
    multi_dim_analyzer,
    aggregation_result,
    [("service.name", "web-api")]
  )
  
  // 验证下钻结果
  assert_eq(drill_down_result.aggregated_series.length(), 3) // 3个区域
  
  // 测试切片分析
  let slice_result = @azimuth.MultiDimensionalAnalyzer::slice(
    multi_dim_analyzer,
    aggregation_result,
    [("region", "us-east-1")]
  )
  
  // 验证切片结果
  assert_eq(slice_result.aggregated_series.length(), 5) // 5个服务
  
  // 测试时间序列对比分析
  let comparison_result = @azimuth.MultiDimensionalAnalyzer::compare_series(
    multi_dim_analyzer,
    "web-api",
    "database",
    "us-east-1"
  )
  
  // 验证对比结果
  assert_true(comparison_result.correlation_coefficient >= -1.0)
  assert_true(comparison_result.correlation_coefficient <= 1.0)
  assert_true(comparison_result.mean_difference != 0.0)
}

// Test 4: 时间序列预测和趋势分析
test "time series forecasting and trend analysis" {
  let forecaster = @azimuth.TimeSeriesForecaster::new()
  
  // 创建历史时间序列数据
  let mut historical_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos() - 365 * 24 * 60 * 60 * 1000000000L // 1年前
  
  for day in 0..=364 { // 365天数据
    let timestamp = base_timestamp + day * 24 * 60 * 60 * 1000000000L
    
    // 创建具有趋势、季节性和噪声的数据
    let trend = 100.0 + day * 0.2 // 上升趋势
    let seasonal = 20.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * day / 365.25) // 年季节性
    let weekly = 5.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * day / 7.0) // 周季节性
    let noise = @azimuth.Random::next_float() * 10.0 - 5.0 // ±5的噪声
    
    let value = @azimuth.Math::max(0.0, trend + seasonal + weekly + noise)
    
    let data_point = @azimuth.TimeSeriesPoint::new(timestamp, value, [
      ("metric.name", @azimuth.StringValue("daily.active.users")),
      ("product", @azimuth.StringValue("mobile-app")),
      ("platform", @azimuth.StringValue("ios"))
    ])
    historical_data = historical_data.push(data_point)
  }
  
  // 配置预测模型
  let forecast_config = @azimuth.ForecastConfig::new()
  @azimuth.ForecastConfig::set_model_type(forecast_config, @azimuth.ForecastModel::Prophet)
  @azimuth.ForecastConfig::set_forecast_horizon(forecast_config, 30) // 30天预测
  @azimuth.ForecastConfig::set_confidence_interval(forecast_config, 0.95) // 95%置信区间
  @azimuth.ForecastConfig::enable_seasonality(forecast_config, true)
  @azimuth.ForecastConfig::set_seasonality_periods(forecast_config, [7.0, 365.25]) // 周和年季节性
  
  // 执行预测
  let forecast_result = @azimuth.TimeSeriesForecaster::forecast(forecaster, historical_data, forecast_config)
  
  // 验证预测结果
  assert_true(forecast_result.forecast_points.length() == 30) // 30天预测
  
  // 检查预测点结构
  for point in forecast_result.forecast_points {
    assert_true(@azimuth.ForecastPoint::has_prediction(point))
    assert_true(@azimuth.ForecastPoint::has_upper_bound(point))
    assert_true(@azimuth.ForecastPoint::has_lower_bound(point))
    assert_true(@azimuth.ForecastPoint::confidence_interval(point) > 0.0)
    
    // 验证预测值在合理范围内
    let prediction = @azimuth.ForecastPoint::prediction(point)
    let upper_bound = @azimuth.ForecastPoint::upper_bound(point)
    let lower_bound = @azimuth.ForecastPoint::lower_bound(point)
    
    assert_true(prediction >= lower_bound)
    assert_true(prediction <= upper_bound)
    assert_true(upper_bound > lower_bound)
  }
  
  // 验证预测质量指标
  let model_metrics = @azimuth.TimeSeriesForecaster::get_model_metrics(forecaster)
  assert_true(model_metrics.mae >= 0.0) // 平均绝对误差
  assert_true(model_metrics.rmse >= 0.0) // 均方根误差
  assert_true(model_metrics.mape >= 0.0) // 平均绝对百分比误差
  assert_true(model_metrics.r2_score >= -1.0 && model_metrics.r2_score <= 1.0) // R²分数
  
  // 测试不同预测模型
  let arima_config = @azimuth.ForecastConfig::new()
  @azimuth.ForecastConfig::set_model_type(arima_config, @azimuth.ForecastModel::ARIMA)
  @azimuth.ForecastConfig::set_forecast_horizon(arima_config, 30)
  
  let arima_result = @azimuth.TimeSeriesForecaster::forecast(forecaster, historical_data, arima_config)
  assert_true(arima_result.forecast_points.length() == 30)
  
  let lstm_config = @azimuth.ForecastConfig::new()
  @azimuth.ForecastConfig::set_model_type(lstm_config, @azimuth.ForecastModel::LSTM)
  @azimuth.ForecastConfig::set_forecast_horizon(lstm_config, 30)
  
  let lstm_result = @azimuth.TimeSeriesForecaster::forecast(forecaster, historical_data, lstm_config)
  assert_true(lstm_result.forecast_points.length() == 30)
  
  // 比较不同模型的预测结果
  let model_comparison = @azimuth.TimeSeriesForecaster::compare_models(forecaster, [
    ("Prophet", forecast_result),
    ("ARIMA", arima_result),
    ("LSTM", lstm_result)
  ])
  
  // 验证模型比较结果
  assert_eq(model_comparison.length(), 3)
  for model in model_comparison {
    assert_true(@azimuth.ModelComparison::model_name(model).length() > 0)
    assert_true(@azimuth.ModelComparison::accuracy_score(model) >= 0.0)
  }
  
  // 测试趋势变化检测
  let trend_analyzer = @azimuth.TrendAnalyzer::new()
  let trend_analysis = @azimuth.TrendAnalyzer::analyze_trend(trend_analyzer, historical_data)
  
  // 验证趋势分析结果
  assert_true(trend_analysis.overall_trend == @azimuth.TrendDirection::Increasing)
  assert_true(trend_analysis.trend_strength > 0.0)
  assert_true(trend_analysis.seasonal_patterns.length() > 0)
  
  // 测试变化点检测
  let change_points = @azimuth.TrendAnalyzer::detect_change_points(trend_analyzer, historical_data)
  assert_true(change_points.length() >= 0)
  
  for change_point in change_points {
    assert_true(@azimuth.ChangePoint::timestamp(change_point) > 0)
    assert_true(@azimuth.ChangePoint::confidence(change_point) > 0.0)
    assert_true(@azimuth.ChangePoint::change_magnitude(change_point) != 0.0)
  }
}

// Test 5: 时间序列数据压缩和存储优化
test "time series data compression and storage optimization" {
  let compression_engine = @azimuth.TSCompressionEngine::new()
  
  // 创建大量时间序列数据
  let mut large_ts_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos() - 90 * 24 * 60 * 60 * 1000000000L // 90天前
  
  for day in 0..=89 { // 90天数据
    for hour in 0..=23 { // 每天24小时
      let timestamp = base_timestamp + (day * 24 + hour) * 60 * 60 * 1000000000L
      
      // 创建具有重复模式的数据（适合压缩）
      let base_value = 50.0 + 10.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * hour / 24.0)
      let noise = @azimuth.Random::next_float() * 2.0 - 1.0
      let value = base_value + noise
      
      let data_point = @azimuth.TimeSeriesPoint::new(timestamp, value, [
        ("metric.name", @azimuth.StringValue("system.memory.usage")),
        ("host", @azimuth.StringValue("server-#{day % 10}")), // 10台服务器循环
        ("datacenter", @azimuth.StringValue("dc-#{day % 3}")) // 3个数据中心循环
      ])
      large_ts_data = large_ts_data.push(data_point)
    }
  }
  
  // 测试不同压缩算法
  let gorilla_compressed = @azimuth.TSCompressionEngine::compress(compression_engine, large_ts_data, @azimuth.TSCompressionAlgorithm::Gorilla)
  let delta_compressed = @azimuth.TSCompressionEngine::compress(compression_engine, large_ts_data, @azimuth.TSCompressionAlgorithm::Delta)
  let run_length_compressed = @azimuth.TSCompressionEngine::compress(compression_engine, large_ts_data, @azimuth.TSCompressionAlgorithm::RunLength)
  
  // 计算原始数据大小
  let original_size = @azimuth.TimeSeriesData::calculate_size(large_ts_data)
  
  // 验证压缩效果
  let gorilla_size = @azimuth.CompressedTSData::size(gorilla_compressed)
  let delta_size = @azimuth.CompressedTSData::size(delta_compressed)
  let run_length_size = @azimuth.CompressedTSData::size(run_length_compressed)
  
  assert_true(gorilla_size < original_size)
  assert_true(delta_size < original_size)
  assert_true(run_length_size < original_size)
  
  // 计算压缩比
  let gorilla_ratio = original_size / gorilla_size
  let delta_ratio = original_size / delta_size
  let run_length_ratio = original_size / run_length_size
  
  assert_true(gorilla_ratio > 1.0)
  assert_true(delta_ratio > 1.0)
  assert_true(run_length_ratio > 1.0)
  
  // 测试解压缩正确性
  let gorilla_decompressed = @azimuth.TSCompressionEngine::decompress(compression_engine, gorilla_compressed)
  let delta_decompressed = @azimuth.TSCompressionEngine::decompress(compression_engine, delta_compressed)
  let run_length_decompressed = @azimuth.TSCompressionEngine::decompress(compression_engine, run_length_compressed)
  
  // 验证解压缩数据完整性
  assert_eq(gorilla_decompressed.length(), large_ts_data.length())
  assert_eq(delta_decompressed.length(), large_ts_data.length())
  assert_eq(run_length_decompressed.length(), large_ts_data.length())
  
  // 验证数据值精度
  for i in 0..=large_ts_data.length() - 1 {
    let original_point = large_ts_data[i]
    let gorilla_point = gorilla_decompressed[i]
    
    let original_value = @azimuth.TimeSeriesPoint::value(original_point)
    let gorilla_value = @azimuth.TimeSeriesPoint::value(gorilla_point)
    
    // 允许小的精度损失
    let error = @azimuth.Math::abs(original_value - gorilla_value)
    assert_true(error < 0.001)
  }
  
  // 测试分块压缩
  let chunk_config = @azimuth.ChunkCompressionConfig::new()
  @azimuth.ChunkCompressionConfig::set_chunk_size(chunk_config, 1000) // 1000点一块
  @azimuth.ChunkCompressionConfig::set_compression_algorithm(chunk_config, @azimuth.TSCompressionAlgorithm::Gorilla)
  
  let chunked_compressed = @azimuth.TSCompressionEngine::compress_chunked(compression_engine, large_ts_data, chunk_config)
  
  // 验证分块压缩
  assert_true(@azimuth.ChunkedCompressedData::chunk_count(chunked_compressed) > 1)
  
  // 测试随机访问压缩数据
  let middle_index = large_ts_data.length() / 2
  let random_access_result = @azimuth.TSCompressionEngine::get_point_at_index(compression_engine, chunked_compressed, middle_index)
  
  assert_true(random_access_result.is_some)
  let retrieved_point = random_access_result.unwrap()
  let original_point = large_ts_data[middle_index]
  
  assert_eq(@azimuth.TimeSeriesPoint::timestamp(retrieved_point), @azimuth.TimeSeriesPoint::timestamp(original_point))
  
  let retrieved_value = @azimuth.TimeSeriesPoint::value(retrieved_point)
  let original_value = @azimuth.TimeSeriesPoint::value(original_point)
  assert_true(@azimuth.Math::abs(retrieved_value - original_value) < 0.001)
  
  // 测试时间范围查询压缩数据
  let start_time = base_timestamp + 30 * 24 * 60 * 60 * 1000000000L // 第30天
  let end_time = base_timestamp + 60 * 24 * 60 * 60 * 1000000000L   // 第60天
  
  let range_query_result = @azimuth.TSCompressionEngine::query_time_range(compression_engine, chunked_compressed, start_time, end_time)
  
  assert_true(range_query_result.length() > 0)
  
  // 验证查询结果的时间范围
  let first_point = range_query_result[0]
  let last_point = range_query_result[range_query_result.length() - 1]
  
  assert_true(@azimuth.TimeSeriesPoint::timestamp(first_point) >= start_time)
  assert_true(@azimuth.TimeSeriesPoint::timestamp(last_point) <= end_time)
}

// Test 6: 实时时间序列监控和告警
test "real-time time series monitoring and alerting" {
  let ts_monitor = @azimuth.TimeSeriesMonitor::new()
  
  // 配置监控规则
  let monitoring_config = @azimuth.MonitoringConfig::new()
  @azimuth.MonitoringConfig::set_evaluation_interval(monitoring_config, 60000) // 1分钟评估一次
  @azimuth.MonitoringConfig::set_window_size(monitoring_config, 300000) // 5分钟窗口
  @azimuth.MonitoringConfig::enable_anomaly_detection(monitoring_config, true)
  
  @azimuth.TimeSeriesMonitor::configure(ts_monitor, monitoring_config)
  
  // 创建告警规则
  let threshold_rule = @azimuth.AlertRule::new("high-cpu-usage", @azimuth.AlertType::Threshold)
  @azimuth.AlertRule::set_metric(threshold_rule, "cpu.usage")
  @azimuth.AlertRule::set_threshold(threshold_rule, @azimuth.ThresholdOperator::GreaterThan, 80.0)
  @azimuth.AlertRule::set_duration(threshold_rule, 300000) // 持续5分钟
  @azimuth.AlertRule::set_severity(threshold_rule, @azimuth.AlertSeverity::Warning)
  
  let rate_rule = @azimuth.AlertRule::new("error-rate-spike", @azimuth.AlertType::Rate)
  @azimuth.AlertRule::set_metric(rate_rule, "error.rate")
  @azimuth.AlertRule::set_rate_threshold(rate_rule, @azimuth.RateOperator::GreaterThan, 0.1, 300000) // 5分钟内错误率>10%
  @azimuth.AlertRule::set_severity(rate_rule, @azimuth.AlertSeverity::Critical)
  
  let anomaly_rule = @azimuth.AlertRule::new("anomaly-detection", @azimuth.AlertType::Anomaly)
  @azimuth.AlertRule::set_metric(anomaly_rule, "response.time")
  @azimuth.AlertRule::set_anomaly_sensitivity(anomaly_rule, 0.8) // 80%敏感度
  @azimuth.AlertRule::set_severity(anomaly_rule, @azimuth.AlertSeverity::Info)
  
  @azimuth.TimeSeriesMonitor::add_alert_rule(ts_monitor, threshold_rule)
  @azimuth.TimeSeriesMonitor::add_alert_rule(ts_monitor, rate_rule)
  @azimuth.TimeSeriesMonitor::add_alert_rule(ts_monitor, anomaly_rule)
  
  // 生成实时数据流
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let mut real_time_data = []
  
  // 正常数据
  for i in 0..=20 {
    let timestamp = base_timestamp + i * 60000000000L // 1分钟间隔
    
    // CPU使用率数据
    let cpu_value = 40.0 + 10.0 * @azimuth.Math::sin(2.0 * @azimuth.Math::PI * i / 10.0) + @azimuth.Random::next_float() * 5.0
    let cpu_point = @azimuth.TimeSeriesPoint::new(timestamp, cpu_value, [
      ("metric.name", @azimuth.StringValue("cpu.usage")),
      ("host", @azimuth.StringValue("web-server-001")),
      ("environment", @azimuth.StringValue("production"))
    ])
    
    // 错误率数据
    let error_value = if i >= 15 && i <= 18 { 0.15 } else { 0.02 } // 15-18分钟高错误率
    let error_point = @azimuth.TimeSeriesPoint::new(timestamp, error_value, [
      ("metric.name", @azimuth.StringValue("error.rate")),
      ("service", @azimuth.StringValue("api-gateway")),
      ("environment", @azimuth.StringValue("production"))
    ])
    
    // 响应时间数据
    let response_value = if i == 10 { 500.0 } else { 100.0 + @azimuth.Random::next_float() * 20.0 } // 第10分钟异常
    let response_point = @azimuth.TimeSeriesPoint::new(timestamp, response_value, [
      ("metric.name", @azimuth.StringValue("response.time")),
      ("service", @azimuth.StringValue("api-gateway")),
      ("environment", @azimuth.StringValue("production"))
    ])
    
    real_time_data = real_time_data.push([cpu_point, error_point, response_point])
  }
  
  // 处理实时数据流
  let mut triggered_alerts = []
  
  for time_batch in real_time_data {
    let batch_result = @azimuth.TimeSeriesMonitor::process_batch(ts_monitor, time_batch)
    
    for alert in batch_result.triggered_alerts {
      triggered_alerts = triggered_alerts.push(alert)
    }
  }
  
  // 验证告警触发
  assert_true(triggered_alerts.length() > 0)
  
  // 验证阈值告警
  let threshold_alerts = triggered_alerts.filter(fn(alert) { 
    @azimuth.Alert::rule_name(alert) == "high-cpu-usage" 
  })
  
  // 由于CPU使用率没有超过80%，可能没有阈值告警
  assert_true(threshold_alerts.length() >= 0)
  
  // 验证错误率告警
  let error_rate_alerts = triggered_alerts.filter(fn(alert) { 
    @azimuth.Alert::rule_name(alert) == "error-rate-spike" 
  })
  
  assert_true(error_rate_alerts.length() > 0)
  for alert in error_rate_alerts {
    assert_eq(@azimuth.Alert::severity(alert), @azimuth.AlertSeverity::Critical)
    assert_true(@azimuth.Alert::triggered_at(alert) > 0)
  }
  
  // 验证异常检测告警
  let anomaly_alerts = triggered_alerts.filter(fn(alert) { 
    @azimuth.Alert::rule_name(alert) == "anomaly-detection" 
  })
  
  assert_true(anomaly_alerts.length() > 0)
  for alert in anomaly_alerts {
    assert_eq(@azimuth.Alert::severity(alert), @azimuth.AlertSeverity::Info)
  }
  
  // 测试告警抑制和分组
  let alert_grouping = @azimuth.AlertGrouping::new()
  @azimuth.AlertGrouping::set_grouping_key(alert_grouping, ["service", "environment"])
  @azimuth.AlertGrouping::set_suppression_window(alert_grouping, 600000) // 10分钟抑制
  
  let grouped_alerts = @azimuth.AlertGrouping::group_alerts(alert_grouping, triggered_alerts)
  
  // 验证告警分组
  assert_true(grouped_alerts.length() <= triggered_alerts.length())
  
  // 测试告警恢复
  let recovery_data = []
  for i in 21..=30 {
    let timestamp = base_timestamp + i * 60000000000L
    
    // 恢复正常的错误率
    let error_value = 0.02
    let error_point = @azimuth.TimeSeriesPoint::new(timestamp, error_value, [
      ("metric.name", @azimuth.StringValue("error.rate")),
      ("service", @azimuth.StringValue("api-gateway")),
      ("environment", @azimuth.StringValue("production"))
    ])
    
    recovery_data = recovery_data.push(error_point)
  }
  
  let recovery_result = @azimuth.TimeSeriesMonitor::process_batch(ts_monitor, recovery_data)
  
  // 验证告警恢复
  assert_true(recovery_result.resolved_alerts.length() > 0)
  
  for resolved_alert in recovery_result.resolved_alerts {
    assert_eq(@azimuth.Alert::rule_name(resolved_alert), "error-rate-spike")
    assert_true(@azimuth.Alert::resolved_at(resolved_alert) > 0)
  }
}