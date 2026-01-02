// 时间序列数据处理测试
// 测试Azimuth遥测系统的时间序列数据处理功能

test "时间序列数据创建和基本操作" {
  // 创建时间序列数据点
  let base_timestamp = 1735689600000000000L  // 2025-01-02T10:00:00Z in nanoseconds
  
  let data_points = [
    TimeSeriesDataPoint::new(base_timestamp, 100.0, [("metric.name", "cpu.usage")]),
    TimeSeriesDataPoint::new(base_timestamp + 60000000000L, 105.5, [("metric.name", "cpu.usage")]),
    TimeSeriesDataPoint::new(base_timestamp + 120000000000L, 98.2, [("metric.name", "cpu.usage")]),
    TimeSeriesDataPoint::new(base_timestamp + 180000000000L, 112.3, [("metric.name", "cpu.usage")]),
    TimeSeriesDataPoint::new(base_timestamp + 240000000000L, 107.8, [("metric.name", "cpu.usage")])
  ]
  
  // 创建时间序列
  let time_series = TimeSeries::new("system.cpu.usage", "percent", data_points)
  
  // 验证时间序列基本属性
  assert_eq(time_series.get_name(), "system.cpu.usage")
  assert_eq(time_series.get_unit(), "percent")
  assert_eq(time_series.get_length(), 5)
  
  // 验证时间范围
  let start_time = time_series.get_start_time()
  let end_time = time_series.get_end_time()
  
  assert_eq(start_time, base_timestamp)
  assert_eq(end_time, base_timestamp + 240000000000L)
  
  // 验证数据点值
  let first_point = time_series.get_point(0)
  let last_point = time_series.get_point(4)
  
  assert_eq(first_point.get_timestamp(), base_timestamp)
  assert_eq(first_point.get_value(), 100.0)
  assert_eq(last_point.get_timestamp(), base_timestamp + 240000000000L)
  assert_eq(last_point.get_value(), 107.8)
  
  // 测试数据点查询
  let point_at_2min = time_series.get_point_at_or_before(base_timestamp + 120000000000L)
  assert_true(point_at_2min.is_some())
  assert_eq(point_at_2min.some().get_value(), 98.2)
  
  let point_before_90sec = time_series.get_point_at_or_before(base_timestamp + 90000000000L)
  assert_true(point_before_90sec.is_some())
  assert_eq(point_before_90sec.some().get_value(), 105.5)
}

test "时间序列数据插值和外推" {
  // 创建不规则间隔的时间序列数据
  let base_timestamp = 1735689600000000000L
  
  let irregular_points = [
    TimeSeriesDataPoint::new(base_timestamp, 50.0, [("metric.name", "memory.usage")]),
    TimeSeriesDataPoint::new(base_timestamp + 180000000000L, 80.0, [("metric.name", "memory.usage")]),  // 3分钟后
    TimeSeriesDataPoint::new(base_timestamp + 420000000000L, 65.0, [("metric.name", "memory.usage")]),  // 7分钟后
    TimeSeriesDataPoint::new(base_timestamp + 600000000000L, 90.0, [("metric.name", "memory.usage")])   // 10分钟后
  ]
  
  let time_series = TimeSeries::new("system.memory.usage", "percent", irregular_points)
  let interpolator = TimeSeriesInterpolator::new(time_series)
  
  // 测试线性插值
  let interpolated_at_1min = interpolator.interpolate_linear(base_timestamp + 60000000000L)  // 1分钟后
  let interpolated_at_5min = interpolator.interpolate_linear(base_timestamp + 300000000000L)  // 5分钟后
  let interpolated_at_8min = interpolator.interpolate_linear(base_timestamp + 480000000000L)  // 8分钟后
  
  // 验证插值结果
  assert_true(interpolated_at_1min.is_some())
  assert_true(interpolated_at_5min.is_some())
  assert_true(interpolated_at_8min.is_some())
  
  // 1分钟后的值应该在50和80之间 (线性插值: 50 + (80-50)*(1/3) = 60)
  let val_1min = interpolated_at_1min.some()
  assert_true(val_1min > 50.0 && val_1min < 80.0)
  
  // 5分钟后的值应该在80和65之间 (线性插值: 80 + (65-80)*(2/4) = 72.5)
  let val_5min = interpolated_at_5min.some()
  assert_true(val_5min > 65.0 && val_5min < 80.0)
  
  // 测试样条插值
  let spline_interpolated = interpolator.interpolate_spline(base_timestamp + 90000000000L)  // 1.5分钟后
  assert_true(spline_interpolated.is_some())
  
  // 测试外推
  let extrapolated_before = interpolator.extrapolate_linear(base_timestamp - 60000000000L)  // 前1分钟
  let extrapolated_after = interpolator.extrapolate_linear(base_timestamp + 720000000000L)  // 12分钟后
  
  assert_true(extrapolated_before.is_some())
  assert_true(extrapolated_after.is_some())
  
  // 外推值应该基于趋势进行计算
  let val_before = extrapolated_before.some()
  let val_after = extrapolated_after.some()
  assert_true(val_before < 50.0)  // 前推应该小于第一个值
  assert_true(val_after > 90.0)   // 后推应该大于最后一个值
}

test "时间序列数据聚合和降采样" {
  // 创建高频率时间序列数据
  let base_timestamp = 1735689600000000000L
  let high_freq_points = []
  
  // 每分钟一个数据点，持续1小时
  for i in 0..60 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟
    let value = 100.0 + (i.to_float() * 0.5) + (Math::random() * 10.0 - 5.0)  // 基础值 + 趋势 + 噪声
    let point = TimeSeriesDataPoint::new(timestamp, value, [("metric.name", "high.freq.metric")])
    high_freq_points = high_freq_points.push(point)
  }
  
  let high_freq_series = TimeSeries::new("high.frequency.metric", "units", high_freq_points)
  let aggregator = TimeSeriesAggregator::new(high_freq_series)
  
  // 测试5分钟聚合
  let aggregated_5min = aggregator.aggregate_by_interval(300000000000L)  // 5分钟
  assert_eq(aggregated_5min.get_length(), 12)  // 60分钟 / 5分钟 = 12个点
  
  // 验证聚合类型
  let avg_aggregated = aggregator.aggregate_by_interval_with_method(300000000000L, "average")
  let max_aggregated = aggregator.aggregate_by_interval_with_method(300000000000L, "maximum")
  let min_aggregated = aggregator.aggregate_by_interval_with_method(300000000000L, "minimum")
  let sum_aggregated = aggregator.aggregate_by_interval_with_method(300000000000L, "sum")
  
  // 验证聚合结果
  for i in 0..avg_aggregated.get_length() {
    let avg_point = avg_aggregated.get_point(i)
    let max_point = max_aggregated.get_point(i)
    let min_point = min_aggregated.get_point(i)
    let sum_point = sum_aggregated.get_point(i)
    
    // 最大值应该大于等于平均值，平均值应该大于等于最小值
    assert_true(max_point.get_value() >= avg_point.get_value())
    assert_true(avg_point.get_value() >= min_point.get_value())
    
    // 总和应该是平均值乘以数据点数量（5个点）
    assert_true(Math::abs(sum_point.get_value() - (avg_point.get_value() * 5.0)) < 0.001)
  }
  
  // 测试10分钟聚合
  let aggregated_10min = aggregator.aggregate_by_interval(600000000000L)  // 10分钟
  assert_eq(aggregated_10min.get_length(), 6)  // 60分钟 / 10分钟 = 6个点
  
  // 测试自定义聚合函数
  let custom_aggregated = aggregator.aggregate_by_interval_with_function(
    300000000000L,  // 5分钟间隔
    fn(points) {
      // 计算中位数
      let sorted_values = points.map(fn(p) { p.get_value() }).sort()
      let len = sorted_values.length()
      if len % 2 == 0 {
        (sorted_values.get(len / 2 - 1) + sorted_values.get(len / 2)) / 2.0
      } else {
        sorted_values.get(len / 2)
      }
    }
  )
  
  assert_eq(custom_aggregated.get_length(), 12)
}

test "时间序列数据模式识别和异常检测" {
  // 创建包含模式的时间序列数据
  let base_timestamp = 1735689600000000000L
  let pattern_points = []
  
  // 创建周期性模式（每日模式）
  for day in 0..7 {  // 7天数据
    for hour in 0..24 {  // 每天24小时
      let timestamp = base_timestamp + (day * 86400000000000L) + (hour * 3600000000000L)
      
      // 模拟日模式：白天高，夜晚低
      let base_value = if hour >= 8 && hour <= 20 {
        80.0 + (Math::sin((hour - 8).to_float() * Math::PI() / 12.0) * 20.0)
      } else {
        30.0 + (Math::random() * 10.0)
      }
      
      // 在第3天添加一些异常值
      let value = if day == 3 && hour == 14 {
        base_value + 100.0  // 异常高峰
      } else if day == 5 && hour == 3 {
        base_value - 20.0   // 异常低谷
      } else {
        base_value + (Math::random() * 5.0 - 2.5)  // 正常噪声
      }
      
      let point = TimeSeriesDataPoint::new(timestamp, value, [("metric.name", "daily.pattern")])
      pattern_points = pattern_points.push(point)
    }
  }
  
  let pattern_series = TimeSeries::new("daily.pattern.metric", "units", pattern_points)
  let pattern_detector = TimeSeriesPatternDetector::new(pattern_series)
  
  // 检测周期性模式
  let daily_pattern = pattern_detector.detect_periodicity(86400000000000L)  // 24小时周期
  assert_true(daily_pattern.is_some())
  
  let pattern_strength = daily_pattern.some().get_strength()
  assert_true(pattern_strength > 0.5)  // 应该有较强的周期性
  
  // 检测趋势
  let trend = pattern_detector.detect_trend()
  assert_true(trend == "stable" || trend == "increasing" || trend == "decreasing")
  
  // 异常检测
  let anomaly_detector = TimeSeriesAnomalyDetector::new(pattern_series)
  let anomalies = anomaly_detector.detect_statistical_outliers(2.0)  // 2个标准差
  
  // 应该检测到我们植入的异常
  assert_true(anomalies.length() >= 2)
  
  // 验证异常点的时间戳
  let anomaly_times = anomalies.map(fn(a) { a.get_timestamp() })
  let day_3_14 = base_timestamp + (3 * 86400000000000L) + (14 * 3600000000000L)
  let day_5_3 = base_timestamp + (5 * 86400000000000L) + (3 * 3600000000000L)
  
  assert_true(anomaly_times.contains(day_3_14))
  assert_true(anomaly_times.contains(day_5_3))
  
  // 测试季节性分解
  let decomposition = pattern_detector.decompose_seasonal(86400000000000L)  // 日季节性
  let trend_component = decomposition.get_trend()
  let seasonal_component = decomposition.get_seasonal()
  let residual_component = decomposition.get_residual()
  
  assert_eq(trend_component.get_length(), pattern_series.get_length())
  assert_eq(seasonal_component.get_length(), pattern_series.get_length())
  assert_eq(residual_component.get_length(), pattern_series.get_length())
}

test "时间序列数据压缩和存储优化" {
  // 创建大量时间序列数据用于压缩测试
  let base_timestamp = 1735689600000000000L
  let large_dataset = []
  
  // 生成1000个数据点
  for i in 0..1000 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟
    let value = 100.0 + Math::sin(i.to_float() * 0.1) * 10.0 + (Math::random() * 2.0 - 1.0)
    let point = TimeSeriesDataPoint::new(timestamp, value, [("metric.name", "large.dataset")])
    large_dataset = large_dataset.push(point)
  }
  
  let large_series = TimeSeries::new("large.dataset.metric", "units", large_dataset)
  let compressor = TimeSeriesCompressor::new(large_series)
  
  // 测试不同压缩算法
  let delta_compressed = compressor.compress_delta()
  let gorilla_compressed = compressor.compress_gorilla()
  let lttb_compressed = compressor.compress_lttb(100)  // 降采样到100个点
  
  // 验证压缩效果
  let original_size = large_series.get_size_bytes()
  let delta_size = delta_compressed.get_size_bytes()
  let gorilla_size = gorilla_compressed.get_size_bytes()
  let lttb_size = lttb_compressed.get_size_bytes()
  
  assert_true(delta_size < original_size)
  assert_true(gorilla_size < original_size)
  assert_true(lttb_size < original_size)
  
  // Gorilla压缩通常比Delta压缩更好
  assert_true(gorilla_size <= delta_size)
  
  // 验证解压缩后的数据完整性
  let delta_decompressed = delta_compressed.decompress()
  let gorilla_decompressed = gorilla_compressed.decompress()
  
  assert_eq(delta_decompressed.get_length(), large_series.get_length())
  assert_eq(gorilla_decompressed.get_length(), large_series.get_length())
  
  // 验证解压缩后的数据精度（允许小的浮点误差）
  for i in 0..10 {  // 检查前10个点
    let original_val = large_series.get_point(i).get_value()
    let delta_val = delta_decompressed.get_point(i).get_value()
    let gorilla_val = gorilla_decompressed.get_point(i).get_value()
    
    assert_true(Math::abs(original_val - delta_val) < 0.001)
    assert_true(Math::abs(original_val - gorilla_val) < 0.001)
  }
  
  // 测试LTTB降采样的可视化效果
  assert_eq(lttb_compressed.get_length(), 100)
  
  // 验证降采样保留了关键特征
  let lttb_max = lttb_compressed.get_max_value()
  let lttb_min = lttb_compressed.get_min_value()
  let original_max = large_series.get_max_value()
  let original_min = large_series.get_min_value()
  
  assert_true(Math::abs(lttb_max - original_max) < 5.0)  // 允许一定误差
  assert_true(Math::abs(lttb_min - original_min) < 5.0)
}

test "时间序列数据查询和分析" {
  // 创建测试时间序列数据
  let base_timestamp = 1735689600000000000L
  let query_points = []
  
  for i in 0..72 {  // 3天，每小时一个点
    let timestamp = base_timestamp + (i * 3600000000000L)
    let hour_of_day = i % 24
    
    // 模拟业务指标：工作时间高，非工作时间低
    let base_value = if hour_of_day >= 9 && hour_of_day <= 17 {
      150.0 + Math::sin((hour_of_day - 9).to_float() * Math::PI() / 8.0) * 50.0
    } else {
      20.0 + Math::random() * 10.0
    }
    
    let value = base_value + (Math::random() * 10.0 - 5.0)
    let point = TimeSeriesDataPoint::new(timestamp, value, [
      ("metric.name", "business.metric"),
      ("hour.of.day", hour_of_day.to_string()),
      ("day.of.week", ((i / 24) % 7).to_string())
    ])
    query_points = query_points.push(point)
  }
  
  let query_series = TimeSeries::new("business.metric", "units", query_points)
  let query_engine = TimeSeriesQueryEngine::new(query_series)
  
  // 测试时间范围查询
  let first_day_data = query_engine.query_by_time_range(
    base_timestamp, 
    base_timestamp + 86400000000000L
  )
  assert_eq(first_day_data.get_length(), 24)
  
  let business_hours_data = query_engine.query_by_time_range(
    base_timestamp + 3600000000000L * 9,  // 第1天9点
    base_timestamp + 3600000000000L * 18  // 第1天18点
  )
  assert_eq(business_hours_data.get_length(), 10)  // 9点到17点
  
  // 测试值范围查询
  let high_value_data = query_engine.query_by_value_range(100.0, 200.0)
  let low_value_data = query_engine.query_by_value_range(0.0, 50.0)
  
  assert_true(high_value_data.length() > 0)
  assert_true(low_value_data.length() > 0)
  assert_true(high_value_data.length() > low_value_data.length())  // 高值应该更多
  
  // 测试属性查询
  let business_hours_query = query_engine.query_by_attribute("hour.of.day", "9")
  assert_eq(business_hours_query.get_length(), 3)  // 3天中每天的9点
  
  // 测试复合查询
  let complex_query = query_engine.query_complex(
    fn(point) {
      let hour = point.get_attribute("hour.of.day").parse_int().unwrap_or(0)
      let value = point.get_value()
      hour >= 9 && hour <= 17 && value > 100.0
    }
  )
  
  assert_true(complex_query.get_length() > 0)
  
  // 测试统计分析
  let stats = query_engine.calculate_statistics()
  let mean = stats.get_mean()
  let median = stats.get_median()
  let std_dev = stats.get_standard_deviation()
  let percentiles = stats.get_percentiles([25.0, 50.0, 75.0, 90.0, 95.0])
  
  assert_true(mean > 0.0)
  assert_true(median > 0.0)
  assert_true(std_dev > 0.0)
  assert_eq(percentiles.length(), 5)
  
  // 验证百分位数顺序
  assert_true(percentiles.get(0) <= percentiles.get(1))  // 25% <= 50%
  assert_true(percentiles.get(1) <= percentiles.get(2))  // 50% <= 75%
  assert_true(percentiles.get(2) <= percentiles.get(3))  // 75% <= 90%
  assert_true(percentiles.get(3) <= percentiles.get(4))  // 90% <= 95%
}