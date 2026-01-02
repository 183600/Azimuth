// Azimuth Time Series Processing Tests
// 测试时间序列数据处理功能

test "time series data point creation and validation" {
  // 创建时间序列数据点
  let timestamp = @azimuth.Time::from_epoch_millis(1672531200000)  // 2023-01-01 00:00:00 UTC
  let value = 42.5
  let attributes = @azimuth.Attributes::builder()
    .with("service", "api")
    .with("endpoint", "/users")
    .build()
  
  let data_point = @azimuth.TimeSeriesDataPoint::new(timestamp, value, attributes)
  
  // 验证数据点属性
  assert_eq(data_point.timestamp, timestamp)
  assert_eq(data_point.value, value)
  assert_eq(data_point.attributes.get("service"), Some("api"))
  assert_eq(data_point.attributes.get("endpoint"), Some("/users"))
}

test "time series aggregation by time windows" {
  // 创建时间序列聚合器
  let window_size = @azimuth.Duration::from_minutes(5)  // 5分钟窗口
  let aggregator = @azimuth.TimeSeriesAggregator::new(window_size)
  
  // 添加数据点
  let base_time = @azimuth.Time::from_epoch_millis(1672531200000)  // 2023-01-01 00:00:00 UTC
  
  // 第一个窗口的数据点
  aggregator.add_data_point(@azimuth.TimeSeriesDataPoint::new(
    base_time, 10.0, @azimuth.Attributes::empty()
  ))
  aggregator.add_data_point(@azimuth.TimeSeriesDataPoint::new(
    base_time + @azimuth.Duration::from_minutes(2), 20.0, @azimuth.Attributes::empty()
  ))
  
  // 第二个窗口的数据点
  aggregator.add_data_point(@azimuth.TimeSeriesDataPoint::new(
    base_time + @azimuth.Duration::from_minutes(6), 30.0, @azimuth.Attributes::empty()
  ))
  aggregator.add_data_point(@azimuth.TimeSeriesDataPoint::new(
    base_time + @azimuth.Duration::from_minutes(8), 40.0, @azimuth.Attributes::empty()
  ))
  
  // 获取聚合结果
  let windows = aggregator.get_aggregated_windows()
  
  // 验证有两个窗口
  assert_eq(windows.length(), 2)
  
  // 验证第一个窗口的聚合结果
  let first_window = windows[0]
  assert_eq(first_window.count, 2)
  assert_eq(first_window.sum, 30.0)
  assert_eq(first_window.average, 15.0)
  assert_eq(first_window.min, 10.0)
  assert_eq(first_window.max, 20.0)
  
  // 验证第二个窗口的聚合结果
  let second_window = windows[1]
  assert_eq(second_window.count, 2)
  assert_eq(second_window.sum, 70.0)
  assert_eq(second_window.average, 35.0)
  assert_eq(second_window.min, 30.0)
  assert_eq(second_window.max, 40.0)
}

test "time series downsampling" {
  // 创建高频率时间序列数据
  let base_time = @azimuth.Time::from_epoch_millis(1672531200000)
  let high_freq_data = []
  
  // 每秒一个数据点，持续10分钟
  for i = 0; i < 600; i = i + 1 {
    let data_point = @azimuth.TimeSeriesDataPoint::new(
      base_time + @azimuth.Duration::from_seconds(i),
      100.0 + @azimuth.Math::sin(i.to_float() * 0.1) * 10.0,  // 带有噪声的信号
      @azimuth.Attributes::empty()
    )
    high_freq_data.push(data_point)
  }
  
  // 创建下采样器，目标为1分钟间隔
  let downsampler = @azimuth.TimeSeriesDownsampler::new(@azimuth.Duration::from_minutes(1))
  
  // 执行下采样
  let downsampled_data = downsampler.downsample(high_freq_data, @azimuth.Aggregation::AVERAGE)
  
  // 验证下采样后的数据点数量
  assert_eq(downsampled_data.length(), 10)  // 10分钟，每分钟一个点
  
  // 验证第一个下采样点
  let first_point = downsampled_data[0]
  assert_eq(first_point.timestamp, base_time + @azimuth.Duration::from_minutes(0.5))  // 窗口中点
  assert_true(first_point.value > 95.0 && first_point.value < 105.0)  // 平均值应在合理范围内
}

test "time series interpolation" {
  // 创建稀疏时间序列数据
  let base_time = @azimuth.Time::from_epoch_millis(1672531200000)
  let sparse_data = [
    @azimuth.TimeSeriesDataPoint::new(base_time, 10.0, @azimuth.Attributes::empty()),
    @azimuth.TimeSeriesDataPoint::new(base_time + @azimuth.Duration::from_hours(1), 20.0, @azimuth.Attributes::empty()),
    @azimuth.TimeSeriesDataPoint::new(base_time + @azimuth.Duration::from_hours(2), 15.0, @azimuth.Attributes::empty())
  ]
  
  // 创建插值器
  let interpolator = @azimuth.TimeSeriesInterpolator::new(@azimuth.InterpolationMethod::LINEAR)
  
  // 在数据点之间进行插值
  let interpolated_points = []
  for i = 0; i < 120; i = i + 1 {  // 每30秒一个点，持续1小时
    let timestamp = base_time + @azimuth.Duration::from_minutes(i * 0.5)
    let interpolated_value = interpolator.interpolate(sparse_data, timestamp)
    
    match interpolated_value {
      Some(value) => {
        interpolated_points.push(@azimuth.TimeSeriesDataPoint::new(
          timestamp, value, @azimuth.Attributes::empty()
        ))
      }
      None => ()  // 超出数据范围
    }
  }
  
  // 验证插值结果
  assert_true(interpolated_points.length() > 0)
  
  // 验证第一个插值点（应该在10和20之间）
  let first_interpolated = interpolated_points[0]
  assert_true(first_interpolated.value > 10.0 && first_interpolated.value < 20.0)
  
  // 验证中间插值点（应该接近15）
  let middle_index = interpolated_points.length() / 2
  let middle_interpolated = interpolated_points[middle_index]
  assert_true(middle_interpolated.value > 14.0 && middle_interpolated.value < 16.0)
}

test "time series anomaly detection" {
  // 创建带有异常值的时间序列数据
  let base_time = @azimuth.Time::from_epoch_millis(1672531200000)
  let data_with_anomalies = []
  
  // 正常数据：在100左右波动
  for i = 0; i < 50; i = i + 1 {
    let value = 100.0 + @azimuth.Math::random() * 10.0 - 5.0  // 95-105之间
    data_with_anomalies.push(@azimuth.TimeSeriesDataPoint::new(
      base_time + @azimuth.Duration::from_minutes(i),
      value,
      @azimuth.Attributes::empty()
    ))
  }
  
  // 添加异常值
  data_with_anomalies.push(@azimuth.TimeSeriesDataPoint::new(
    base_time + @azimuth.Duration::from_minutes(25),
    200.0,  // 异常高值
    @azimuth.Attributes::empty()
  ))
  
  data_with_anomalies.push(@azimuth.TimeSeriesDataPoint::new(
    base_time + @azimuth.Duration::from_minutes(40),
    0.0,    // 异常低值
    @azimuth.Attributes::empty()
  ))
  
  // 创建异常检测器
  let anomaly_detector = @azimuth.TimeSeriesAnomalyDetector::new(
    @azimuth.AnomalyDetectionMethod::STATISTICAL,
    2.0  // 2个标准差阈值
  )
  
  // 检测异常
  let anomalies = anomaly_detector.detect_anomalies(data_with_anomalies)
  
  // 验证检测到异常
  assert_true(anomalies.length() >= 2)
  
  // 验证异常点的时间戳
  let anomaly_timestamps = anomalies.map(fn(a) { a.timestamp })
  assert_true(anomaly_timestamps.contains(base_time + @azimuth.Duration::from_minutes(25)))
  assert_true(anomaly_timestamps.contains(base_time + @azimuth.Duration::from_minutes(40)))
}

test "time series trend analysis" {
  // 创建有明显趋势的时间序列数据
  let base_time = @azimuth.Time::from_epoch_millis(1672531200000)
  let trending_data = []
  
  // 上升趋势数据
  for i = 0; i < 100; i = i + 1 {
    let value = 10.0 + i * 0.5 + @azimuth.Math::random() * 5.0 - 2.5  // 线性增长加噪声
    trending_data.push(@azimuth.TimeSeriesDataPoint::new(
      base_time + @azimuth.Duration::from_hours(i),
      value,
      @azimuth.Attributes::empty()
    ))
  }
  
  // 创建趋势分析器
  let trend_analyzer = @azimuth.TimeSeriesTrendAnalyzer::new()
  
  // 分析趋势
  let trend_result = trend_analyzer.analyze(trending_data)
  
  // 验证趋势分析结果
  assert_eq(trend_result.direction, @azimuth.TrendDirection::INCREASING)
  assert_true(trend_result.slope > 0.4 && trend_result.slope < 0.6)  // 应该接近0.5
  assert_true(trend_result.confidence > 0.8)  // 高置信度
  
  // 创建下降趋势数据
  let decreasing_data = []
  for i = 0; i < 100; i = i + 1 {
    let value = 60.0 - i * 0.3 + @azimuth.Math::random() * 5.0 - 2.5  // 线性下降加噪声
    decreasing_data.push(@azimuth.TimeSeriesDataPoint::new(
      base_time + @azimuth.Duration::from_hours(i),
      value,
      @azimuth.Attributes::empty()
    ))
  }
  
  // 分析下降趋势
  let decreasing_trend = trend_analyzer.analyze(decreasing_data)
  
  // 验证下降趋势分析结果
  assert_eq(decreasing_trend.direction, @azimuth.TrendDirection::DECREASING)
  assert_true(decreasing_trend.slope < -0.2 && decreasing_trend.slope > -0.4)  // 应该接近-0.3
  assert_true(decreasing_trend.confidence > 0.8)  // 高置信度
}