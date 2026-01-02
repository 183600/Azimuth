// 时间序列数据处理测试用例
// 测试Azimuth遥测系统的时间序列数据处理功能

test "时间序列数据点创建和基本操作" {
  // 创建时间序列数据点
  let timestamp1 = Timestamp::from_unix_millis(1641024000000)  // 2022-01-01 00:00:00 UTC
  let timestamp2 = Timestamp::from_unix_millis(1641024060000)  // 2022-01-01 00:01:00 UTC
  let timestamp3 = Timestamp::from_unix_millis(1641024120000)  // 2022-01-01 00:02:00 UTC
  
  let data_point1 = TimeSeriesDataPoint::new(timestamp1, 25.5)
  let data_point2 = TimeSeriesDataPoint::new(timestamp2, 27.8)
  let data_point3 = TimeSeriesDataPoint::new(timestamp3, 26.2)
  
  // 验证数据点
  assert_eq(TimeSeriesDataPoint::value(data_point1), 25.5)
  assert_eq(TimeSeriesDataPoint::value(data_point2), 27.8)
  assert_eq(TimeSeriesDataPoint::value(data_point3), 26.2)
  
  assert_eq(TimeSeriesDataPoint::timestamp(data_point1), timestamp1)
  assert_eq(TimeSeriesDataPoint::timestamp(data_point2), timestamp2)
  assert_eq(TimeSeriesDataPoint::timestamp(data_point3), timestamp3)
  
  // 创建时间序列
  let time_series = TimeSeries::new("cpu.usage", "percent")
  TimeSeries::add_data_point(time_series, data_point1)
  TimeSeries::add_data_point(time_series, data_point2)
  TimeSeries::add_data_point(time_series, data_point3)
  
  // 验证时间序列
  assert_eq(TimeSeries::name(time_series), "cpu.usage")
  assert_eq(TimeSeries::unit(time_series), "percent")
  assert_eq(TimeSeries::data_points_count(time_series), 3)
  
  assert_true(true)
}

test "时间序列聚合操作" {
  // 创建时间序列数据
  let base_timestamp = Timestamp::from_unix_millis(1641024000000)
  let time_series = TimeSeries::new("memory.usage", "percent")
  
  // 添加多个数据点
  for i = 0; i < 10; i = i + 1 {
    let timestamp = Timestamp::from_unix_millis(1641024000000 + i * 60000)  // 每分钟一个点
    let value = 50.0 + i.to_int().to_float() * 2.5  // 50.0, 52.5, 55.0, ...
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_data_point(time_series, data_point)
  }
  
  // 测试平均值聚合
  let avg_value = TimeSeriesAggregator::average(time_series)
  assert_true(avg_value > 60.0 && avg_value < 65.0)  // 大约62.5
  
  // 测试最大值聚合
  let max_value = TimeSeriesAggregator::max(time_series)
  assert_eq(max_value, 72.5)  // 最后一个值
  
  // 测试最小值聚合
  let min_value = TimeSeriesAggregator::min(time_series)
  assert_eq(min_value, 50.0)  // 第一个值
  
  // 测试总和聚合
  let sum_value = TimeSeriesAggregator::sum(time_series)
  assert_eq(sum_value, 625.0)  // 50.0 + 52.5 + ... + 72.5
  
  // 测试计数聚合
  let count_value = TimeSeriesAggregator::count(time_series)
  assert_eq(count_value, 10)
  
  assert_true(true)
}

test "时间窗口聚合" {
  // 创建跨越多小时的时序数据
  let time_series = TimeSeries::new("request.rate", "requests_per_second")
  
  // 添加24小时的数据，每小时一个点
  for hour = 0; hour < 24; hour = hour + 1 {
    let timestamp = Timestamp::from_unix_millis(1641024000000 + hour * 3600000)  // 每小时
    let value = 100.0 + (hour.to_int().to_float() * 5.0)  // 递增的请求率
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_data_point(time_series, data_point)
  }
  
  // 定义6小时的时间窗口
  let window_start = Timestamp::from_unix_millis(1641024000000 + 6 * 3600000)  // 从第6小时开始
  let window_end = Timestamp::from_unix_millis(1641024000000 + 12 * 3600000)    // 到第12小时结束
  
  // 在时间窗口内进行聚合
  let window_avg = TimeSeriesAggregator::average_in_window(time_series, window_start, window_end)
  let window_max = TimeSeriesAggregator::max_in_window(time_series, window_start, window_end)
  let window_min = TimeSeriesAggregator::min_in_window(time_series, window_start, window_end)
  
  // 验证窗口聚合结果
  assert_true(window_avg > 135.0 && window_avg < 140.0)  // 大约137.5
  assert_eq(window_max, 160.0)  // 第12小时的值
  assert_eq(window_min, 130.0)  // 第6小时的值
  
  // 测试滑动窗口聚合
  let sliding_avg = TimeSeriesAggregator::sliding_window_average(time_series, 3600000 * 4)  // 4小时窗口
  assert_true(sliding_avg.length() > 0)
  
  assert_true(true)
}

test "时间序列下采样" {
  // 创建高频率时间序列数据
  let high_freq_series = TimeSeries::new("cpu.temperature", "celsius")
  
  // 添加每分钟一个点，持续24小时
  for minute = 0; minute < 1440; minute = minute + 1 {  // 24 * 60 分钟
    let timestamp = Timestamp::from_unix_millis(1641024000000 + minute * 60000)
    // 模拟温度变化：基础温度 + 时间相关的波动 + 随机噪声
    let base_temp = 60.0
    let daily_variation = 10.0 * (minute.to_int().to_float() / 1440.0)  // 24小时内的线性增长
    let noise = 5.0 * (minute % 10).to_int().to_float() / 10.0  // 每10分钟的周期性变化
    let temperature = base_temp + daily_variation + noise
    
    let data_point = TimeSeriesDataPoint::new(timestamp, temperature)
    TimeSeries::add_data_point(high_freq_series, data_point)
  }
  
  // 下采样到小时级别
  let hourly_series = TimeSeriesDownsampler::downsample(
    high_freq_series, 
    3600000,  // 1小时窗口
    DownsamplingStrategy::Average
  )
  
  // 验证下采样结果
  assert_eq(TimeSeries::data_points_count(hourly_series), 24)  // 24小时，24个点
  
  // 验证第一个小时的平均值
  let first_hour_points = TimeSeries::get_data_points_in_range(
    hourly_series,
    Timestamp::from_unix_millis(1641024000000),
    Timestamp::from_unix_millis(1641024000000 + 3600000)
  )
  assert_eq(first_hour_points.length(), 1)
  
  // 测试不同下采样策略
  let max_downsampled = TimeSeriesDownsampler::downsample(
    high_freq_series,
    3600000,
    DownsamplingStrategy::Max
  )
  
  let min_downsampled = TimeSeriesDownsampler::downsample(
    high_freq_series,
    3600000,
    DownsamplingStrategy::Min
  )
  
  assert_eq(TimeSeries::data_points_count(max_downsampled), 24)
  assert_eq(TimeSeries::data_points_count(min_downsampled), 24)
  
  assert_true(true)
}

test "时间序列异常检测" {
  // 创建包含异常的时间序列数据
  let anomaly_series = TimeSeries::new("network.latency", "milliseconds")
  
  // 添加正常数据点
  for i = 0; i < 20; i = i + 1 {
    let timestamp = Timestamp::from_unix_millis(1641024000000 + i * 60000)
    let normal_latency = 50.0 + 10.0 * (i % 5).to_int().to_float()  // 50-90ms的正常范围
    let data_point = TimeSeriesDataPoint::new(timestamp, normal_latency)
    TimeSeries::add_data_point(anomaly_series, data_point)
  }
  
  // 添加异常数据点
  let anomaly_timestamp1 = Timestamp::from_unix_millis(1641024000000 + 20 * 60000)
  let anomaly_point1 = TimeSeriesDataPoint::new(anomaly_timestamp1, 500.0)  // 异常高延迟
  TimeSeries::add_data_point(anomaly_series, anomaly_point1)
  
  let anomaly_timestamp2 = Timestamp::from_unix_millis(1641024000000 + 22 * 60000)
  let anomaly_point2 = TimeSeriesDataPoint::new(anomaly_timestamp2, 2.0)    // 异常低延迟
  TimeSeries::add_data_point(anomaly_series, anomaly_point2)
  
  // 继续添加正常数据点
  for i = 24; i < 30; i = i + 1 {
    let timestamp = Timestamp::from_unix_millis(1641024000000 + i * 60000)
    let normal_latency = 50.0 + 10.0 * (i % 5).to_int().to_float()
    let data_point = TimeSeriesDataPoint::new(timestamp, normal_latency)
    TimeSeries::add_data_point(anomaly_series, data_point)
  }
  
  // 使用统计方法检测异常
  let anomalies = TimeSeriesAnomalyDetector::detect_statistical_outliers(
    anomaly_series, 
    2.0  // 2个标准差阈值
  )
  
  // 验证异常检测结果
  assert_true(anomalies.length() >= 2)  // 至少检测到2个异常点
  
  // 验证检测到的异常点
  let anomaly_values = anomalies.map(fn(a) { TimeSeriesDataPoint::value(a) })
  assert_true(anomaly_values.contains(500.0))
  assert_true(anomaly_values.contains(2.0))
  
  // 测试滑动窗口异常检测
  let sliding_anomalies = TimeSeriesAnomalyDetector::detect_sliding_window_outliers(
    anomaly_series,
    600000,  // 10分钟窗口
    3.0      // 3个标准差阈值
  )
  
  assert_true(sliding_anomalies.length() > 0)
  
  assert_true(true)
}