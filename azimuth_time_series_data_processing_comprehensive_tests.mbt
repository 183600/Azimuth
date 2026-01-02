// Azimuth 时间序列数据处理综合测试
// 测试时间序列数据的处理、聚合和分析功能

test "时间序列数据创建和基本操作" {
  // 测试时间序列数据的创建和基本操作
  
  // 创建时间序列数据点
  let base_timestamp = 1735689600000000000L  // 2025-01-02T00:00:00Z
  let time_series = TimeSeries::new("cpu.usage", "percent")
  
  // 添加数据点
  for i in 0..=23 {
    let timestamp = base_timestamp + (i * 3600000000000L)  // 每小时一个数据点
    let value = 50.0 + (10.0 * (i / 6.0).sin())  // 模拟CPU使用率波动
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 验证时间序列基本属性
  assert_eq(TimeSeries::metric_name(time_series), "cpu.usage")
  assert_eq(TimeSeries::unit(time_series), "percent")
  assert_eq(TimeSeries::point_count(time_series), 24)
  
  // 验证数据点
  let points = TimeSeries::get_points(time_series)
  assert_eq(points.length(), 24)
  
  // 验证第一个和最后一个数据点
  let first_point = TimeSeries::get_first_point(time_series)
  let last_point = TimeSeries::get_last_point(time_series)
  
  match first_point {
    Some(point) => {
      assert_eq(TimeSeriesDataPoint::timestamp(point), base_timestamp)
      assert_eq(TimeSeriesDataPoint::value(point), 50.0)
    }
    None => assert_true(false)
  }
  
  match last_point {
    Some(point) => {
      let expected_timestamp = base_timestamp + (23 * 3600000000000L)
      assert_eq(TimeSeriesDataPoint::timestamp(point), expected_timestamp)
    }
    None => assert_true(false)
  }
}

test "时间序列数据聚合操作" {
  // 测试时间序列数据的聚合操作
  
  // 创建时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("request.count", "count")
  
  // 添加每分钟的数据点，持续24小时
  for i in 0..=1439 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个数据点
    let value = 100.0 + (50.0 * (i / 60.0).sin()) + (20.0 * Random::next_float())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试按小时聚合
  let hourly_series = TimeSeries::aggregate_by_interval(time_series, 3600000000000L, AggregationType::Average)
  assert_eq(TimeSeries::point_count(hourly_series), 24)
  
  // 测试按天聚合
  let daily_series = TimeSeries::aggregate_by_interval(time_series, 86400000000000L, AggregationType::Sum)
  assert_eq(TimeSeries::point_count(daily_series), 1)
  
  // 测试最大值聚合
  let max_series = TimeSeries::aggregate_by_interval(time_series, 3600000000000L, AggregationType::Max)
  let max_points = TimeSeries::get_points(max_series)
  
  // 测试最小值聚合
  let min_series = TimeSeries::aggregate_by_interval(time_series, 3600000000000L, AggregationType::Min)
  let min_points = TimeSeries::get_points(min_series)
  
  // 验证聚合结果
  for i in 0..=23 {
    let max_point = max_points[i]
    let min_point = min_points[i]
    
    assert_true(TimeSeriesDataPoint::value(max_point) >= TimeSeriesDataPoint::value(min_point))
  }
  
  // 测试计数聚合
  let count_series = TimeSeries::aggregate_by_interval(time_series, 3600000000000L, AggregationType::Count)
  let count_points = TimeSeries::get_points(count_series)
  
  for point in count_points {
    assert_eq(TimeSeriesDataPoint::value(point), 60.0)  // 每小时60个数据点
  }
}

test "时间序列数据窗口操作" {
  // 测试时间序列数据的窗口操作
  
  // 创建时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("memory.usage", "percent")
  
  // 添加每分钟的数据点，持续4小时
  for i in 0..=239 {
    let timestamp = base_timestamp + (i * 60000000000L)
    let value = 60.0 + (20.0 * (i / 30.0).sin())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试滑动窗口平均
  let window_size = 300000000000L  // 5分钟窗口
  let smoothed_series = TimeSeries::sliding_window_average(time_series, window_size)
  
  assert_eq(TimeSeries::point_count(smoothed_series), TimeSeries::point_count(time_series))
  
  // 测试滑动窗口最大值
  let max_window_series = TimeSeries::sliding_window_max(time_series, window_size)
  
  // 测试滑动窗口最小值
  let min_window_series = TimeSeries::sliding_window_min(time_series, window_size)
  
  // 测试滑动窗口求和
  let sum_window_series = TimeSeries::sliding_window_sum(time_series, window_size)
  
  // 验证窗口操作结果
  let original_points = TimeSeries::get_points(time_series)
  let smoothed_points = TimeSeries::get_points(smoothed_series)
  let max_points = TimeSeries::get_points(max_window_series)
  let min_points = TimeSeries::get_points(min_window_series)
  let sum_points = TimeSeries::get_points(sum_window_series)
  
  for i in 0..=239 {
    let original_value = TimeSeriesDataPoint::value(original_points[i])
    let smoothed_value = TimeSeriesDataPoint::value(smoothed_points[i])
    let max_value = TimeSeriesDataPoint::value(max_points[i])
    let min_value = TimeSeriesDataPoint::value(min_points[i])
    let sum_value = TimeSeriesDataPoint::value(sum_points[i])
    
    // 验证窗口操作的逻辑关系
    assert_true(max_value >= smoothed_value)
    assert_true(smoothed_value >= min_value)
    assert_true(sum_value >= max_value)
  }
}

test "时间序列数据重采样" {
  // 测试时间序列数据的重采样操作
  
  // 创建高频时间序列数据
  let base_timestamp = 1735689600000000000L
  let high_freq_series = TimeSeries::new("temperature", "celsius")
  
  // 添加每秒的数据点，持续1小时
  for i in 0..=3599 {
    let timestamp = base_timestamp + (i * 1000000000L)
    let value = 20.0 + (5.0 * (i / 300.0).sin()) + (2.0 * Random::next_float())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(high_freq_series, data_point)
  }
  
  // 测试上采样（从秒到分钟）
  let upsampled_series = TimeSeries::upsample(high_freq_series, 60000000000L, ResamplingMethod::Linear)
  assert_eq(TimeSeries::point_count(upsampled_series), 60)
  
  // 测试下采样（从秒到分钟）
  let downsampled_series = TimeSeries::downsample(high_freq_series, 60000000000L, ResamplingMethod::Average)
  assert_eq(TimeSeries::point_count(downsampled_series), 60)
  
  // 测试不同重采样方法
  let max_downsampled = TimeSeries::downsample(high_freq_series, 60000000000L, ResamplingMethod::Max)
  let min_downsampled = TimeSeries::downsample(high_freq_series, 60000000000L, ResamplingMethod::Min)
  let first_downsampled = TimeSeries::downsample(high_freq_series, 60000000000L, ResamplingMethod::First)
  let last_downsampled = TimeSeries::downsample(high_freq_series, 60000000000L, ResamplingMethod::Last)
  
  // 验证重采样结果
  let downsampled_points = TimeSeries::get_points(downsampled_series)
  let max_points = TimeSeries::get_points(max_downsampled)
  let min_points = TimeSeries::get_points(min_downsampled)
  
  for i in 0..=59 {
    let avg_value = TimeSeriesDataPoint::value(downsampled_points[i])
    let max_value = TimeSeriesDataPoint::value(max_points[i])
    let min_value = TimeSeriesDataPoint::value(min_points[i])
    
    assert_true(max_value >= avg_value)
    assert_true(avg_value >= min_value)
  }
}

test "时间序列数据统计分析" {
  // 测试时间序列数据的统计分析功能
  
  // 创建时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("response.time", "ms")
  
  // 添加具有统计特征的数据点
  for i in 0..=999 {
    let timestamp = base_timestamp + (i * 60000000000L)
    // 生成正态分布的数据
    let value = 100.0 + (20.0 * Random::normal())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 计算基本统计量
  let stats = TimeSeries::calculate_statistics(time_series)
  
  // 验证统计量
  assert_true(stats.count == 1000)
  assert_true(stats.mean > 80.0 && stats.mean < 120.0)  // 应该在100±20范围内
  assert_true(stats.std_dev > 15.0 && stats.std_dev < 25.0)  // 标准差应该在20左右
  assert_true(stats.min < stats.mean)
  assert_true(stats.max > stats.mean)
  assert_true(stats.median >= stats.min && stats.median <= stats.max)
  
  // 计算百分位数
  let p50 = TimeSeries::percentile(time_series, 0.5)
  let p95 = TimeSeries::percentile(time_series, 0.95)
  let p99 = TimeSeries::percentile(time_series, 0.99)
  
  // 验证百分位数关系
  assert_true(p50 <= p95)
  assert_true(p95 <= p99)
  
  // 计算移动平均
  let ma_10 = TimeSeries::moving_average(time_series, 10)
  let ma_50 = TimeSeries::moving_average(time_series, 50)
  let ma_100 = TimeSeries::moving_average(time_series, 100)
  
  assert_eq(TimeSeries::point_count(ma_10), 1000)
  assert_eq(TimeSeries::point_count(ma_50), 1000)
  assert_eq(TimeSeries::point_count(ma_100), 1000)
  
  // 计算指数移动平均
  let ema_0_1 = TimeSeries::exponential_moving_average(time_series, 0.1)
  let ema_0_3 = TimeSeries::exponential_moving_average(time_series, 0.3)
  
  assert_eq(TimeSeries::point_count(ema_0_1), 1000)
  assert_eq(TimeSeries::point_count(ema_0_3), 1000)
}

test "时间序列数据异常检测" {
  // 测试时间序列数据的异常检测功能
  
  // 创建包含异常的时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("network.traffic", "mbps")
  
  // 添加正常数据点
  for i in 0..=199 {
    let timestamp = base_timestamp + (i * 60000000000L)
    let value = 50.0 + (10.0 * (i / 20.0).sin()) + (5.0 * Random::next_float())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 添加异常数据点
  let anomaly_timestamp_1 = base_timestamp + (50 * 60000000000L)
  let anomaly_1 = TimeSeriesDataPoint::new(anomaly_timestamp_1, 150.0)  // 异常高值
  TimeSeries::add_point(time_series, anomaly_1)
  
  let anomaly_timestamp_2 = base_timestamp + (120 * 60000000000L)
  let anomaly_2 = TimeSeriesDataPoint::new(anomaly_timestamp_2, -10.0)  // 异常低值
  TimeSeries::add_point(time_series, anomaly_2)
  
  let anomaly_timestamp_3 = base_timestamp + (180 * 60000000000L)
  let anomaly_3 = TimeSeriesDataPoint::new(anomaly_timestamp_3, 200.0)  // 极端高值
  TimeSeries::add_point(time_series, anomaly_3)
  
  // 使用Z-score方法检测异常
  let z_score_anomalies = TimeSeries::detect_anomalies_zscore(time_series, 2.0)
  assert_true(z_score_anomalies.length() >= 3)
  
  // 使用IQR方法检测异常
  let iqr_anomalies = TimeSeries::detect_anomalies_iqr(time_series, 1.5)
  assert_true(iqr_anomalies.length() >= 3)
  
  // 使用移动平均方法检测异常
  let ma_anomalies = TimeSeries::detect_anomalies_moving_average(time_series, 10, 2.0)
  assert_true(ma_anomalies.length() >= 3)
  
  // 验证检测到的异常包含我们插入的异常点
  let anomaly_timestamps = [anomaly_timestamp_1, anomaly_timestamp_2, anomaly_timestamp_3]
  
  for anomaly_timestamp in anomaly_timestamps {
    let z_score_found = z_score_anomalies.any(fn(a) { TimeSeriesDataPoint::timestamp(a) == anomaly_timestamp })
    let iqr_found = iqr_anomalies.any(fn(a) { TimeSeriesDataPoint::timestamp(a) == anomaly_timestamp })
    let ma_found = ma_anomalies.any(fn(a) { TimeSeriesDataPoint::timestamp(a) == anomaly_timestamp })
    
    // 至少有一种方法应该检测到异常
    assert_true(z_score_found || iqr_found || ma_found)
  }
}

test "时间序列数据预测" {
  // 测试时间序列数据的预测功能
  
  // 创建具有趋势和季节性的时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("sales.revenue", "dollars")
  
  // 生成30天的数据
  for i in 0..=29 {
    let timestamp = base_timestamp + (i * 86400000000000L)
    // 添加趋势（每天增长100）
    let trend = 1000.0 + (i * 100.0)
    // 添加季节性（7天周期）
    let seasonal = 200.0 * (2.0 * 3.14159 * i / 7.0).sin()
    // 添加噪声
    let noise = 50.0 * Random::next_float()
    let value = trend + seasonal + noise
    
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 使用线性回归预测未来7天
  let linear_forecast = TimeSeries::forecast_linear_regression(time_series, 7)
  assert_eq(TimeSeries::point_count(linear_forecast), 7)
  
  // 使用移动平均预测未来7天
  let ma_forecast = TimeSeries::forecast_moving_average(time_series, 7, 7)
  assert_eq(TimeSeries::point_count(ma_forecast), 7)
  
  // 使用指数平滑预测未来7天
  let ets_forecast = TimeSeries::forecast_exponential_smoothing(time_series, 7, 0.3)
  assert_eq(TimeSeries::point_count(ets_forecast), 7)
  
  // 验证预测时间戳
  let last_timestamp = base_timestamp + (29 * 86400000000000L)
  let linear_points = TimeSeries::get_points(linear_forecast)
  
  for i in 0..=6 {
    let expected_timestamp = last_timestamp + ((i + 1) * 86400000000000L)
    let actual_timestamp = TimeSeriesDataPoint::timestamp(linear_points[i])
    assert_eq(actual_timestamp, expected_timestamp)
  }
  
  // 验证预测值的合理性
  let last_value = TimeSeriesDataPoint::value(TimeSeries::get_last_point(time_series))
  let first_forecast_value = TimeSeriesDataPoint::value(TimeSeries::get_first_point(linear_forecast))
  
  // 预测值应该基于历史数据，不应该偏离太远
  assert_true(first_forecast_value > last_value * 0.5 && first_forecast_value < last_value * 2.0)
}

test "时间序列数据相关性分析" {
  // 测试时间序列数据的相关性分析
  
  // 创建两个相关的的时间序列
  let base_timestamp = 1735689600000000000L
  let series_a = TimeSeries::new("cpu.usage", "percent")
  let series_b = TimeSeries::new("memory.usage", "percent")
  
  // 生成相关的数据
  for i in 0..=99 {
    let timestamp = base_timestamp + (i * 60000000000L)
    let base_value = 50.0 + (20.0 * (i / 10.0).sin())
    
    // 系列A
    let value_a = base_value + (5.0 * Random::next_float())
    let data_point_a = TimeSeriesDataPoint::new(timestamp, value_a)
    TimeSeries::add_point(series_a, data_point_a)
    
    // 系列B（与A相关，但有一些噪声）
    let value_b = base_value * 0.8 + (10.0 * Random::next_float())
    let data_point_b = TimeSeriesDataPoint::new(timestamp, value_b)
    TimeSeries::add_point(series_b, data_point_b)
  }
  
  // 创建一个不相关的系列
  let series_c = TimeSeries::new("network.latency", "ms")
  for i in 0..=99 {
    let timestamp = base_timestamp + (i * 60000000000L)
    let value_c = 20.0 + (10.0 * Random::next_float())
    let data_point_c = TimeSeriesDataPoint::new(timestamp, value_c)
    TimeSeries::add_point(series_c, data_point_c)
  }
  
  // 计算相关性
  let correlation_ab = TimeSeries::correlation(series_a, series_b)
  let correlation_ac = TimeSeries::correlation(series_a, series_c)
  let correlation_bc = TimeSeries::correlation(series_b, series_c)
  
  // 验证相关性结果
  assert_true(correlation_ab > 0.5)  // A和B应该有较强的正相关
  assert_true(correlation_ac.abs() < 0.3)  // A和C应该相关性较弱
  assert_true(correlation_bc.abs() < 0.3)  // B和C应该相关性较弱
  
  // 计算自相关
  let autocorr_a_lag1 = TimeSeries::autocorrelation(series_a, 1)
  let autocorr_a_lag5 = TimeSeries::autocorrelation(series_a, 5)
  let autocorr_a_lag10 = TimeSeries::autocorrelation(series_a, 10)
  
  // 验证自相关性
  assert_true(autocorr_a_lag1.abs() > autocorr_a_lag5.abs())
  assert_true(autocorr_a_lag5.abs() > autocorr_a_lag10.abs())
  
  // 计算交叉相关
  let cross_corr = TimeSeries::cross_correlation(series_a, series_b, 5)
  assert_eq(cross_corr.length(), 11)  // 从-5到+5的滞后
  
  // 找到最大交叉相关的滞后
  let max_lag = TimeSeries::max_cross_correlation_lag(series_a, series_b, 5)
  assert_true(max_lag >= -5 && max_lag <= 5)
}

test "时间序列数据压缩和存储" {
  // 测试时间序列数据的压缩和存储
  
  // 创建高频时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("sensor.data", "value")
  
  // 添加大量数据点
  for i in 0..=9999 {
    let timestamp = base_timestamp + (i * 1000000000L)
    let value = 100.0 + (50.0 * (i / 1000.0).sin()) + (10.0 * Random::next_float())
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试原始数据大小
  let original_size = TimeSeries::serialize(time_series).length()
  
  // 测试不同压缩方法
  let delta_compressed = TimeSeries::compress_delta(time_series)
  let gorilla_compressed = TimeSeries::compress_gorilla(time_series)
  let lzw_compressed = TimeSeries::compress_lzw(time_series)
  
  // 验证压缩效果
  assert_true(delta_compressed.length() < original_size)
  assert_true(gorilla_compressed.length() < original_size)
  assert_true(lzw_compressed.length() < original_size)
  
  // 计算压缩比
  let delta_ratio = delta_compressed.length().to_float() / original_size.to_float()
  let gorilla_ratio = gorilla_compressed.length().to_float() / original_size.to_float()
  let lzw_ratio = lzw_compressed.length().to_float() / original_size.to_float()
  
  // 验证压缩比
  assert_true(delta_ratio < 0.9)
  assert_true(gorilla_ratio < 0.8)
  assert_true(lzw_ratio < 0.9)
  
  // 解压缩并验证数据完整性
  let delta_decompressed = TimeSeries::decompress_delta(delta_compressed)
  let gorilla_decompressed = TimeSeries::decompress_gorilla(gorilla_compressed)
  let lzw_decompressed = TimeSeries::decompress_lzw(lzw_compressed)
  
  // 验证解压缩后的数据
  assert_eq(TimeSeries::point_count(delta_decompressed), TimeSeries::point_count(time_series))
  assert_eq(TimeSeries::point_count(gorilla_decompressed), TimeSeries::point_count(time_series))
  assert_eq(TimeSeries::point_count(lzw_decompressed), TimeSeries::point_count(time_series))
  
  // 验证数据点值的一致性
  let original_points = TimeSeries::get_points(time_series)
  let delta_points = TimeSeries::get_points(delta_decompressed)
  let gorilla_points = TimeSeries::get_points(gorilla_decompressed)
  let lzw_points = TimeSeries::get_points(lzw_decompressed)
  
  for i in 0..=9999 {
    let original_value = TimeSeriesDataPoint::value(original_points[i])
    let delta_value = TimeSeriesDataPoint::value(delta_points[i])
    let gorilla_value = TimeSeriesDataPoint::value(gorilla_points[i])
    let lzw_value = TimeSeriesDataPoint::value(lzw_points[i])
    
    // 允许微小的浮点误差
    assert_true((original_value - delta_value).abs() < 0.001)
    assert_true((original_value - gorilla_value).abs() < 0.001)
    assert_true((original_value - lzw_value).abs() < 0.001)
  }
}