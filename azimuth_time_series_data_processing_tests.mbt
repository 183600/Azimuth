// Azimuth 时间序列数据处理测试
// 专注于测试时间序列数据的收集、处理和分析功能

// 时间序列数据点类型
type TimeSeriesPoint = {
  timestamp : Int,
  value : Float,
  metadata : Array[(String, String)]
}

// 时间序列类型
type TimeSeries = {
  name : String,
  points : Array[TimeSeriesPoint],
  interval : Int // 数据点间隔（秒）
  unit : String
}

// 聚合类型
type AggregationType {
  Average
  Sum
  Min
  Max
  Count
  Median
  Percentile(Int)
}

// 测试1: 时间序列数据创建和基本操作
test "时间序列数据创建和基本操作测试" {
  // 创建时间序列
  let cpu_series = create_time_series("cpu_usage", 60, "percent") // 每分钟一个数据点
  let memory_series = create_time_series("memory_usage", 300, "bytes") // 每5分钟一个数据点
  
  // 添加数据点
  let base_time = 1640995200 // 2022-01-01 00:00:00 UTC
  
  add_point(cpu_series, base_time, 45.2)
  add_point(cpu_series, base_time + 60, 52.8)
  add_point(cpu_series, base_time + 120, 38.9)
  add_point(cpu_series, base_time + 180, 61.3)
  
  add_point(memory_series, base_time, 1024 * 1024 * 1024) // 1GB
  add_point(memory_series, base_time + 300, 1024 * 1024 * 1200) // 1.2GB
  add_point(memory_series, base_time + 600, 1024 * 1024 * 950) // 950MB
  
  // 验证数据点数量
  assert_eq(cpu_series.points.length(), 4)
  assert_eq(memory_series.points.length(), 3)
  
  // 验证数据点内容
  assert_eq(cpu_series.points[0].timestamp, base_time)
  assert_eq(cpu_series.points[0].value, 45.2)
  
  assert_eq(cpu_series.points[3].timestamp, base_time + 180)
  assert_eq(cpu_series.points[3].value, 61.3)
  
  // 测试获取最新数据点
  let latest_cpu = get_latest_point(cpu_series)
  match latest_cpu {
    Some(point) => {
      assert_eq(point.timestamp, base_time + 180)
      assert_eq(point.value, 61.3)
    }
    None => assert_true(false)
  }
  
  // 测试获取指定时间范围的数据点
  let range_points = get_points_in_range(cpu_series, base_time + 60, base_time + 180)
  assert_eq(range_points.length(), 3)
  
  // 测试删除数据点
  remove_point(cpu_series, base_time + 60)
  assert_eq(cpu_series.points.length(), 3)
  
  // 验证删除后的数据点
  let updated_range_points = get_points_in_range(cpu_series, base_time, base_time + 180)
  assert_eq(updated_range_points.length(), 2)
}

// 测试2: 时间序列数据聚合
test "时间序列数据聚合测试" {
  // 创建测试数据
  let series = create_time_series("test_series", 60, "units")
  let base_time = 1640995200
  
  // 添加测试数据点
  let values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  let i = 0
  while i < values.length() {
    add_point(series, base_time + i * 60, values[i])
    i = i + 1
  }
  
  // 测试平均值聚合
  let avg_result = aggregate_series(series, Average, base_time, base_time + 540) // 前6个点
  match avg_result {
    Some(value) => assert_eq(value, 35.0) // (10+20+30+40+50+60)/6
    None => assert_true(false)
  }
  
  // 测试总和聚合
  let sum_result = aggregate_series(series, Sum, base_time, base_time + 540)
  match sum_result {
    Some(value) => assert_eq(value, 210.0) // 10+20+30+40+50+60
    None => assert_true(false)
  }
  
  // 测试最小值聚合
  let min_result = aggregate_series(series, Min, base_time, base_time + 540)
  match min_result {
    Some(value) => assert_eq(value, 10.0)
    None => assert_true(false)
  }
  
  // 测试最大值聚合
  let max_result = aggregate_series(series, Max, base_time, base_time + 540)
  match max_result {
    Some(value) => assert_eq(value, 60.0)
    None => assert_true(false)
  }
  
  // 测试计数聚合
  let count_result = aggregate_series(series, Count, base_time, base_time + 540)
  match count_result {
    Some(value) => assert_eq(value, 6.0)
    None => assert_true(false)
  }
  
  // 测试中位数聚合
  let median_result = aggregate_series(series, Median, base_time, base_time + 540)
  match median_result {
    Some(value) => assert_eq(value, 35.0) // (30+40)/2
    None => assert_true(false)
  }
  
  // 测试百分位数聚合
  let p75_result = aggregate_series(series, Percentile(75), base_time, base_time + 540)
  match p75_result {
    Some(value) => assert_eq(value, 50.0) // 75th percentile
    None => assert_true(false)
  }
}

// 测试3: 时间序列数据重采样
test "时间序列数据重采样测试" {
  // 创建高频数据（每秒一个点）
  let high_freq_series = create_time_series("high_freq", 1, "units")
  let base_time = 1640995200
  
  // 添加24小时的数据点（86400个点）
  let i = 0
  while i < 3600 { // 只添加1小时的数据点以简化测试
    let value = 50.0 + 10.0 * (i % 60) as Float / 60.0 // 模拟波动
    add_point(high_freq_series, base_time + i, value)
    i = i + 1
  }
  
  // 验证高频数据点数量
  assert_eq(high_freq_series.points.length(), 3600)
  
  // 下采样到每分钟（60秒）
  let minute_series = downsample_series(high_freq_series, 60, Average)
  assert_eq(minute_series.points.length(), 60) // 3600秒 / 60秒 = 60个点
  
  // 验证下采样结果
  let first_minute_point = minute_series.points[0]
  assert_eq(first_minute_point.timestamp, base_time + 60) // 第一个分钟结束时间
  
  // 上采样到每30秒
  let thirty_sec_series = upsample_series(high_freq_series, 30, Linear)
  assert_eq(thirty_sec_series.points.length(), 7200) // 3600秒 / 30秒 = 120个点
  
  // 测试不同聚合方法的重采样
  let max_minute_series = downsample_series(high_freq_series, 60, Max)
  let min_minute_series = downsample_series(high_freq_series, 60, Min)
  
  // 验证不同聚合方法的结果
  assert_true(max_minute_series.points[0].value >= min_minute_series.points[0].value)
  
  // 测试时间对齐
  let aligned_series = align_series_time(high_freq_series, 60, "start")
  assert_eq(aligned_series.points.length(), 60)
  
  // 验证时间对齐
  let i = 0
  while i < aligned_series.points.length() {
    assert_eq(aligned_series.points[i].timestamp, base_time + i * 60)
    i = i + 1
  }
}

// 测试4: 时间序列数据平滑和滤波
test "时间序列数据平滑和滤波测试" {
  // 创建带有噪声的数据
  let noisy_series = create_time_series("noisy_data", 60, "units")
  let base_time = 1640995200
  
  // 添加带有噪声的数据点（基础值 + 随机噪声）
  let i = 0
  while i < 24 {
    let base_value = 100.0 + i as Float * 5.0 // 线性增长
    let noise = (i % 3 - 1) as Float * 10.0 // -10, 0, 10的噪声
    add_point(noisy_series, base_time + i * 3600, base_value + noise)
    i = i + 1
  }
  
  // 移动平均平滑
  let ma_smoothed = apply_moving_average(noisy_series, 3)
  assert_eq(ma_smoothed.points.length(), noisy_series.points.length())
  
  // 验证平滑效果：平滑后的数据应该比原始数据波动小
  let original_variance = calculate_variance(extract_values(noisy_series.points))
  let smoothed_variance = calculate_variance(extract_values(ma_smoothed.points))
  assert_true(smoothed_variance <= original_variance)
  
  // 指数平滑
  let exp_smoothed = apply_exponential_smoothing(noisy_series, 0.3)
  assert_eq(exp_smoothed.points.length(), noisy_series.points.length())
  
  // 验证指数平滑效果
  let exp_variance = calculate_variance(extract_values(exp_smoothed.points))
  assert_true(exp_variance <= original_variance)
  
  // 中值滤波
  let median_filtered = apply_median_filter(noisy_series, 3)
  assert_eq(median_filtered.points.length(), noisy_series.points.length())
  
  // 验证中值滤波效果
  let median_variance = calculate_variance(extract_values(median_filtered.points))
  assert_true(median_variance <= original_variance)
  
  // 测试异常值检测
  let outliers = detect_outliers(noisy_series, 2.0) // 2个标准差
  assert_true(outliers.length() > 0)
  
  // 移除异常值
  let cleaned_series = remove_outliers(noisy_series, 2.0)
  assert_true(cleaned_series.points.length() <= noisy_series.points.length())
}

// 测试5: 时间序列趋势分析
test "时间序列趋势分析测试" {
  // 创建具有明显趋势的数据
  let trend_series = create_time_series("trend_data", 3600, "units") // 每小时一个数据点
  let base_time = 1640995200
  
  // 添加线性增长趋势数据
  let i = 0
  while i < 24 {
    let trend_value = 100.0 + i as Float * 5.0 // 每小时增长5个单位
    let seasonal = 10.0 * (2 * 3.14159 * i / 24).sin() // 添加季节性波动
    add_point(trend_series, base_time + i * 3600, trend_value + seasonal)
    i = i + 1
  }
  
  // 线性趋势检测
  let linear_trend = detect_linear_trend(trend_series)
  match linear_trend {
    Some((slope, intercept)) => {
      // 验证斜率接近5（每单位时间增长5个单位）
      assert_true(slope > 4.5 && slope < 5.5)
    }
    None => assert_true(false)
  }
  
  // 季节性检测
  let seasonality = detect_seasonality(trend_series, 24) // 24小时周期
  match seasonality {
    Some(amplitude) => {
      // 验证季节性振幅接近10
      assert_true(amplitude > 8.0 && amplitude < 12.0)
    }
    None => assert_true(false)
  }
  
  // 趋势分解
  let decomposition = decompose_timeseries(trend_series, 24)
  match decomposition {
    Some((trend, seasonal, residual)) => {
      assert_eq(trend.points.length(), trend_series.points.length())
      assert_eq(seasonal.points.length(), trend_series.points.length())
      assert_eq(residual.points.length(), trend_series.points.length())
      
      // 验证趋势数据是单调递增的
      let i = 1
      while i < trend.points.length() {
        assert_true(trend.points[i].value >= trend.points[i-1].value)
        i = i + 1
      }
    }
    None => assert_true(false)
  }
  
  // 变化点检测
  let change_points = detect_change_points(trend_series)
  assert_true(change_points.length() >= 0)
  
  // 创建有变化点的数据
  let change_series = create_time_series("change_data", 3600, "units")
  let i = 0
  while i < 24 {
    let value = if i < 12 {
      100.0 + i as Float * 2.0 // 前12小时缓慢增长
    } else {
      200.0 + (i - 12) as Float * 8.0 // 后12小时快速增长
    }
    add_point(change_series, base_time + i * 3600, value)
    i = i + 1
  }
  
  let detected_changes = detect_change_points(change_series)
  assert_true(detected_changes.length() > 0)
  
  // 验证检测到的变化点在合理范围内
  for change_point in detected_changes {
    assert_true(change_point >= base_time + 10 * 3600 && change_point <= base_time + 14 * 3600)
  }
}

// 测试6: 时间序列预测
test "时间序列预测测试" {
  // 创建历史数据
  let historical_series = create_time_series("historical_data", 86400, "units") // 每天一个数据点
  let base_time = 1640995200
  
  // 添加30天的历史数据
  let i = 0
  while i < 30 {
    let trend = 100.0 + i as Float * 2.0 // 每天增长2个单位
    let weekly_pattern = 10.0 * (2 * 3.14159 * i / 7).sin() // 周期性模式
    let noise = (i % 5 - 2) as Float * 3.0 // 随机噪声
    add_point(historical_series, base_time + i * 86400, trend + weekly_pattern + noise)
    i = i + 1
  }
  
  // 简单移动平均预测
  let ma_forecast = forecast_with_moving_average(historical_series, 7) // 使用7天移动平均预测未来7天
  assert_eq(ma_forecast.points.length(), 7)
  
  // 验证预测时间戳
  let last_timestamp = historical_series.points[historical_series.points.length() - 1].timestamp
  let i = 0
  while i < ma_forecast.points.length() {
    assert_eq(ma_forecast.points[i].timestamp, last_timestamp + (i + 1) * 86400)
    i = i + 1
  }
  
  // 线性回归预测
  let lr_forecast = forecast_with_linear_regression(historical_series, 7)
  assert_eq(lr_forecast.points.length(), 7)
  
  // 验证线性回归预测的趋势
  let i = 0
  while i < lr_forecast.points.length() - 1 {
    assert_true(lr_forecast.points[i + 1].value > lr_forecast.points[i].value) // 应该是递增的
    i = i + 1
  }
  
  // 指数平滑预测
  let exp_forecast = forecast_with_exponential_smoothing(historical_series, 7, 0.3)
  assert_eq(exp_forecast.points.length(), 7)
  
  // 季节性预测
  let seasonal_forecast = forecast_with_seasonality(historical_series, 7, 7) // 7天周期
  assert_eq(seasonal_forecast.points.length(), 7)
  
  // 预测精度评估
  let train_size = 20 // 使用前20天训练，后10天测试
  let train_series = slice_series(historical_series, 0, train_size)
  let test_series = slice_series(historical_series, train_size, historical_series.points.length())
  
  let test_forecast = forecast_with_linear_regression(train_series, test_series.points.length())
  let accuracy = evaluate_forecast_accuracy(test_series, test_forecast)
  
  // 验证预测精度在合理范围内
  assert_true(accuracy.mae > 0.0) // 平均绝对误差应该大于0
  assert_true(accuracy.rmse > 0.0) // 均方根误差应该大于0
  assert_true(accuracy.mape < 50.0) // 平均绝对百分比误差应该小于50%
}

// 测试7: 时间序列相关性分析
test "时间序列相关性分析测试" {
  // 创建两个相关的时间序列
  let series1 = create_time_series("series1", 3600, "units")
  let series2 = create_time_series("series2", 3600, "units")
  let base_time = 1640995200
  
  // 添加相关数据（series2 = 2 * series1 + 噪声）
  let i = 0
  while i < 24 {
    let base_value = 50.0 + i as Float * 2.0
    let noise1 = (i % 3 - 1) as Float * 2.0
    let noise2 = (i % 4 - 1.5) as Float * 3.0
    
    add_point(series1, base_time + i * 3600, base_value + noise1)
    add_point(series2, base_time + i * 3600, 2.0 * base_value + noise2)
    i = i + 1
  }
  
  // 计算相关系数
  let correlation = calculate_correlation(series1, series2)
  
  // 验证相关系数接近1（强正相关）
  assert_true(correlation > 0.8 && correlation <= 1.0)
  
  // 创建负相关的时间序列
  let series3 = create_time_series("series3", 3600, "units")
  let i = 0
  while i < 24 {
    let base_value = 50.0 + i as Float * 2.0
    let noise = (i % 3 - 1) as Float * 2.0
    add_point(series3, base_time + i * 3600, 200.0 - base_value + noise) // 负相关
    i = i + 1
  }
  
  let negative_correlation = calculate_correlation(series1, series3)
  
  // 验证相关系数接近-1（强负相关）
  assert_true(negative_correlation >= -1.0 && negative_correlation < -0.8)
  
  // 创建不相关的时间序列
  let series4 = create_time_series("series4", 3600, "units")
  let i = 0
  while i < 24 {
    let random_value = 100.0 + (i * 7 % 13) as Float * 5.0 // 随机模式
    add_point(series4, base_time + i * 3600, random_value)
    i = i + 1
  }
  
  let no_correlation = calculate_correlation(series1, series4)
  
  // 验证相关系数接近0（不相关）
  assert_true(no_correlation > -0.3 && no_correlation < 0.3)
  
  // 交叉相关分析
  let cross_correlation = calculate_cross_correlation(series1, series2, 5) // 最大滞后5个点
  assert_eq(cross_correlation.length(), 11) // -5到+5的滞后
  
  // 验证最大交叉相关在滞后0附近
  let max_lag = find_max_correlation_lag(cross_correlation)
  assert_true(max_lag >= -2 && max_lag <= 2)
  
  // 滞后相关性测试
  let lagged_series = create_lagged_series(series1, 2) // 延迟2个点
  let lagged_correlation = calculate_correlation(lagged_series, series2)
  
  // 验证滞后相关性
  assert_true(lagged_correlation > 0.0)
}

// 测试8: 时间序列数据压缩和存储优化
test "时间序列数据压缩和存储优化测试" {
  // 创建高精度时间序列
  let high_precision_series = create_time_series("high_precision", 60, "units")
  let base_time = 1640995200
  
  // 添加高精度数据点
  let i = 0
  while i < 1440 { // 24小时，每分钟一个点
    let value = 100.123456789 + i as Float * 0.001234567
    add_point(high_precision_series, base_time + i * 60, value)
    i = i + 1
  }
  
  // 计算原始数据大小
  let original_size = calculate_series_size(high_precision_series)
  
  // 应用精度降低压缩
  let reduced_precision = reduce_precision(high_precision_series, 4) // 保留4位小数
  let reduced_size = calculate_series_size(reduced_precision)
  
  // 验证压缩效果
  assert_true(reduced_size < original_size)
  
  // 计算精度损失
  let precision_loss = calculate_precision_loss(high_precision_series, reduced_precision)
  assert_true(precision_loss < 0.001) // 精度损失应该很小
  
  // 应用增量编码压缩
  let delta_encoded = apply_delta_encoding(high_precision_series)
  let delta_size = calculate_series_size(delta_encoded)
  
  // 验证增量编码的压缩效果
  assert_true(delta_size < original_size)
  
  // 应用旋转门压缩
  let swing_door_compressed = apply_swing_door_compression(high_precision_series, 0.1)
  let swing_door_size = calculate_series_size(swing_door_compressed)
  
  // 验证旋转门压缩效果
  assert_true(swing_door_size < original_size)
  
  // 计算压缩比
  let compression_ratio = original_size as Float / swing_door_size as Float
  assert_true(compression_ratio > 1.0)
  
  // 验证压缩后的数据质量
  let reconstruction_error = calculate_reconstruction_error(high_precision_series, swing_door_compressed)
  assert_true(reconstruction_error < 0.5) // 重建误差应该很小
  
  // 测试时间序列分块存储
  let chunks = split_into_chunks(high_precision_series, 3600) // 每小时一块
  assert_eq(chunks.length(), 24) // 24小时应该分成24块
  
  // 验证每个块的大小
  for chunk in chunks {
    assert_true(chunk.points.length() <= 61) // 每块最多61个点（60个点+1个边界点）
  }
  
  // 测试数据恢复
  let recovered_series = merge_chunks(chunks)
  assert_eq(recovered_series.points.length(), high_precision_series.points.length())
  
  // 验证恢复的数据质量
  let recovery_error = calculate_reconstruction_error(high_precision_series, recovered_series)
  assert_true(recovery_error < 0.001) // 恢复误差应该非常小
}

// 辅助函数：创建时间序列
fn create_time_series(name : String, interval : Int, unit : String) -> TimeSeries {
  {
    name: name,
    points: [],
    interval: interval,
    unit: unit
  }
}

// 辅助函数：添加数据点
fn add_point(series : TimeSeries, timestamp : Int, value : Float) -> Unit {
  let point = {
    timestamp: timestamp,
    value: value,
    metadata: []
  }
  series.points.push(point)
}

// 辅助函数：获取最新数据点
fn get_latest_point(series : TimeSeries) -> Option[TimeSeriesPoint] {
  if series.points.length() > 0 {
    Some(series.points[series.points.length() - 1])
  } else {
    None
  }
}

// 辅助函数：获取指定时间范围的数据点
fn get_points_in_range(series : TimeSeries, start_time : Int, end_time : Int) -> Array[TimeSeriesPoint] {
  let result = []
  for point in series.points {
    if point.timestamp >= start_time && point.timestamp <= end_time {
      result.push(point)
    }
  }
  result
}

// 辅助函数：删除数据点
fn remove_point(series : TimeSeries, timestamp : Int) -> Bool {
  let i = 0
  while i < series.points.length() {
    if series.points[i].timestamp == timestamp {
      series.points.splice(i, 1)
      return true
    }
    i = i + 1
  }
  false
}

// 辅助函数：聚合时间序列
fn aggregate_series(series : TimeSeries, aggregation_type : AggregationType, start_time : Int, end_time : Int) -> Option[Float] {
  let points = get_points_in_range(series, start_time, end_time)
  
  if points.length() == 0 {
    return None
  }
  
  match aggregation_type {
    Average => {
      let sum = 0.0
      for point in points {
        sum = sum + point.value
      }
      Some(sum / points.length() as Float)
    }
    Sum => {
      let sum = 0.0
      for point in points {
        sum = sum + point.value
      }
      Some(sum)
    }
    Min => {
      let min = points[0].value
      for point in points {
        if point.value < min {
          min = point.value
        }
      }
      Some(min)
    }
    Max => {
      let max = points[0].value
      for point in points {
        if point.value > max {
          max = point.value
        }
      }
      Some(max)
    }
    Count => Some(points.length() as Float)
    Median => {
      let sorted_values = sort_values(extract_values(points))
      let mid = sorted_values.length() / 2
      
      if sorted_values.length() % 2 == 0 {
        Some((sorted_values[mid - 1] + sorted_values[mid]) / 2.0)
      } else {
        Some(sorted_values[mid])
      }
    }
    Percentile(p) => {
      let sorted_values = sort_values(extract_values(points))
      let index = (sorted_values.length() * p) / 100
      
      if index >= sorted_values.length() {
        Some(sorted_values[sorted_values.length() - 1])
      } else {
        Some(sorted_values[index])
      }
    }
  }
}

// 辅助函数：提取值
fn extract_values(points : Array[TimeSeriesPoint]) -> Array[Float] {
  let values = []
  for point in points {
    values.push(point.value)
  }
  values
}

// 辅助函数：排序值
fn sort_values(values : Array[Float]) -> Array[Float] {
  // 简化的排序实现
  let sorted = []
  let i = 0
  while i < values.length() {
    sorted.push(values[i])
    i = i + 1
  }
  
  // 冒泡排序
  let i = 0
  while i < sorted.length() {
    let j = 0
    while j < sorted.length() - i - 1 {
      if sorted[j] > sorted[j + 1] {
        let temp = sorted[j]
        sorted[j] = sorted[j + 1]
        sorted[j + 1] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
  
  sorted
}

// 辅助函数：下采样时间序列
fn downsample_series(series : TimeSeries, new_interval : Int, aggregation : AggregationType) -> TimeSeries {
  let downsampled = create_time_series(series.name + "_downsampled", new_interval, series.unit)
  
  if series.points.length() == 0 {
    return downsampled
  }
  
  let start_time = series.points[0].timestamp
  let end_time = series.points[series.points.length() - 1].timestamp
  
  let current_time = start_time
  while current_time <= end_time {
    let window_end = current_time + new_interval
    match aggregate_series(series, aggregation, current_time, window_end - 1) {
      Some(value) => {
        add_point(downsampled, window_end - 1, value)
      }
      None => {}
    }
    current_time = current_time + new_interval
  }
  
  downsampled
}

// 辅助函数：上采样时间序列
fn upsample_series(series : TimeSeries, new_interval : Int, interpolation : String) -> TimeSeries {
  let upsampled = create_time_series(series.name + "_upsampled", new_interval, series.unit)
  
  if series.points.length() < 2 {
    return upsampled
  }
  
  let i = 0
  while i < series.points.length() - 1 {
    let current_point = series.points[i]
    let next_point = series.points[i + 1]
    
    // 添加原始点
    add_point(upsampled, current_point.timestamp, current_point.value)
    
    // 在两点之间插值
    let current_time = current_point.timestamp + new_interval
    while current_time < next_point.timestamp {
      let ratio = (current_time - current_point.timestamp) as Float / (next_point.timestamp - current_point.timestamp) as Float
      let interpolated_value = current_point.value + ratio * (next_point.value - current_point.value)
      add_point(upsampled, current_time, interpolated_value)
      current_time = current_time + new_interval
    }
    i = i + 1
  }
  
  // 添加最后一个点
  let last_point = series.points[series.points.length() - 1]
  add_point(upsampled, last_point.timestamp, last_point.value)
  
  upsampled
}

// 辅助函数：对齐时间序列时间
fn align_series_time(series : TimeSeries, interval : Int, alignment : String) -> TimeSeries {
  let aligned = create_time_series(series.name + "_aligned", interval, series.unit)
  
  if series.points.length() == 0 {
    return aligned
  }
  
  let start_time = series.points[0].timestamp
  let aligned_start = match alignment {
    "start" => start_time
    "end" => start_time + (interval - (start_time % interval))
    _ => start_time
  }
  
  let current_time = aligned_start
  let i = 0
  while i < series.points.length() {
    let point = series.points[i]
    
    // 找到最近的对齐时间点
    let aligned_time = (point.timestamp / interval) * interval
    
    // 查找或创建对齐时间点的数据
    let existing_point = find_point_at_timestamp(aligned, aligned_time)
    match existing_point {
      Some(_) => {
        // 如果已存在，更新为平均值
        update_point_value(aligned, aligned_time, (existing_point.value + point.value) / 2.0)
      }
      None => {
        // 如果不存在，添加新点
        add_point(aligned, aligned_time, point.value)
      }
    }
    i = i + 1
  }
  
  aligned
}

// 辅助函数：查找指定时间戳的数据点
fn find_point_at_timestamp(series : TimeSeries, timestamp : Int) -> Option[TimeSeriesPoint] {
  let i = 0
  while i < series.points.length() {
    if series.points[i].timestamp == timestamp {
      return Some(series.points[i])
    }
    i = i + 1
  }
  None
}

// 辅助函数：更新数据点值
fn update_point_value(series : TimeSeries, timestamp : Int, new_value : Float) -> Bool {
  let i = 0
  while i < series.points.length() {
    if series.points[i].timestamp == timestamp {
      series.points[i] = { series.points[i] with value: new_value }
      return true
    }
    i = i + 1
  }
  false
}

// 辅助函数：应用移动平均
fn apply_moving_average(series : TimeSeries, window_size : Int) -> TimeSeries {
  let smoothed = create_time_series(series.name + "_ma", series.interval, series.unit)
  
  let i = 0
  while i < series.points.length() {
    let start = if i - window_size / 2 < 0 { 0 } else { i - window_size / 2 }
    let end = if i + window_size / 2 >= series.points.length() { series.points.length() - 1 } else { i + window_size / 2 }
    
    let sum = 0.0
    let count = 0
    let j = start
    while j <= end {
      sum = sum + series.points[j].value
      count = count + 1
      j = j + 1
    }
    
    let avg_value = sum / count as Float
    add_point(smoothed, series.points[i].timestamp, avg_value)
    i = i + 1
  }
  
  smoothed
}

// 辅助函数：应用指数平滑
fn apply_exponential_smoothing(series : TimeSeries, alpha : Float) -> TimeSeries {
  let smoothed = create_time_series(series.name + "_exp", series.interval, series.unit)
  
  if series.points.length() == 0 {
    return smoothed
  }
  
  // 第一个点的平滑值等于原始值
  add_point(smoothed, series.points[0].timestamp, series.points[0].value)
  
  let i = 1
  while i < series.points.length() {
    let current_value = series.points[i].value
    let previous_smoothed = smoothed.points[i - 1].value
    let smoothed_value = alpha * current_value + (1.0 - alpha) * previous_smoothed
    
    add_point(smoothed, series.points[i].timestamp, smoothed_value)
    i = i + 1
  }
  
  smoothed
}

// 辅助函数：应用中值滤波
fn apply_median_filter(series : TimeSeries, window_size : Int) -> TimeSeries {
  let filtered = create_time_series(series.name + "_median", series.interval, series.unit)
  
  let i = 0
  while i < series.points.length() {
    let start = if i - window_size / 2 < 0 { 0 } else { i - window_size / 2 }
    let end = if i + window_size / 2 >= series.points.length() { series.points.length() - 1 } else { i + window_size / 2 }
    
    let window_values = []
    let j = start
    while j <= end {
      window_values.push(series.points[j].value)
      j = j + 1
    }
    
    let sorted_values = sort_values(window_values)
    let median_value = sorted_values[sorted_values.length() / 2]
    
    add_point(filtered, series.points[i].timestamp, median_value)
    i = i + 1
  }
  
  filtered
}

// 辅助函数：计算方差
fn calculate_variance(values : Array[Float]) -> Float {
  if values.length() == 0 {
    return 0.0
  }
  
  let mean = calculate_mean(values)
  let sum_squared_diff = 0.0
  
  for value in values {
    let diff = value - mean
    sum_squared_diff = sum_squared_diff + diff * diff
  }
  
  sum_squared_diff / values.length() as Float
}

// 辅助函数：计算平均值
fn calculate_mean(values : Array[Float]) -> Float {
  if values.length() == 0 {
    return 0.0
  }
  
  let sum = 0.0
  for value in values {
    sum = sum + value
  }
  
  sum / values.length() as Float
}

// 辅助函数：检测异常值
fn detect_outliers(series : TimeSeries, threshold : Float) -> Array[TimeSeriesPoint] {
  let values = extract_values(series.points)
  let mean = calculate_mean(values)
  let variance = calculate_variance(values)
  let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
  
  let outliers = []
  for point in series.points {
    let z_score = if std_dev > 0.0 { (point.value - mean) / std_dev } else { 0.0 }
    if z_score.abs() > threshold {
      outliers.push(point)
    }
  }
  
  outliers
}

// 辅助函数：移除异常值
fn remove_outliers(series : TimeSeries, threshold : Float) -> TimeSeries {
  let outliers = detect_outliers(series, threshold)
  let cleaned = create_time_series(series.name + "_cleaned", series.interval, series.unit)
  
  for point in series.points {
    let is_outlier = false
    let i = 0
    while i < outliers.length() {
      if outliers[i].timestamp == point.timestamp {
        is_outlier = true
        break
      }
      i = i + 1
    }
    
    if !is_outlier {
      add_point(cleaned, point.timestamp, point.value)
    }
  }
  
  cleaned
}

// 辅助函数：检测线性趋势
fn detect_linear_trend(series : TimeSeries) -> Option[(Float, Float)] {
  if series.points.length() < 2 {
    return None
  }
  
  let n = series.points.length() as Float
  let sum_x = 0.0
  let sum_y = 0.0
  let sum_xy = 0.0
  let sum_x2 = 0.0
  
  let i = 0
  while i < series.points.length() {
    let x = i as Float
    let y = series.points[i].value
    
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
    i = i + 1
  }
  
  let denominator = n * sum_x2 - sum_x * sum_x
  if denominator.abs() < 0.0001 {
    return None
  }
  
  let slope = (n * sum_xy - sum_x * sum_y) / denominator
  let intercept = (sum_y - slope * sum_x) / n
  
  Some((slope, intercept))
}

// 辅助函数：检测季节性
fn detect_seasonality(series : TimeSeries, period : Int) -> Option[Float] {
  if series.points.length() < period * 2 {
    return None
  }
  
  let seasonal_values = []
  let i = 0
  while i < period {
    seasonal_values.push(0.0)
    i = i + 1
  }
  
  let counts = []
  let i = 0
  while i < period {
    counts.push(0)
    i = i + 1
  }
  
  // 计算每个季节位置的平均值
  let i = 0
  while i < series.points.length() {
    let season_index = i % period
    seasonal_values[season_index] = seasonal_values[season_index] + series.points[i].value
    counts[season_index] = counts[season_index] + 1
    i = i + 1
  }
  
  let i = 0
  while i < period {
    if counts[i] > 0 {
      seasonal_values[i] = seasonal_values[i] / counts[i] as Float
    }
    i = i + 1
  }
  
  // 计算季节性振幅
  let mean = calculate_mean(seasonal_values)
  let max_diff = 0.0
  
  let i = 0
  while i < seasonal_values.length() {
    let diff = (seasonal_values[i] - mean).abs()
    if diff > max_diff {
      max_diff = diff
    }
    i = i + 1
  }
  
  Some(max_diff)
}

// 时间序列分解结果类型
type TimeSeriesDecomposition = (TimeSeries, TimeSeries, TimeSeries) // (trend, seasonal, residual)

// 辅助函数：分解时间序列
fn decompose_timeseries(series : TimeSeries, period : Int) -> Option[TimeSeriesDecomposition] {
  if series.points.length() < period * 2 {
    return None
  }
  
  // 计算趋势（使用移动平均）
  let trend = apply_moving_average(series, period)
  
  // 计算季节性
  let seasonal = create_time_series(series.name + "_seasonal", series.interval, series.unit)
  let seasonal_values = []
  let i = 0
  while i < period {
    seasonal_values.push(0.0)
    i = i + 1
  }
  
  let counts = []
  let i = 0
  while i < period {
    counts.push(0)
    i = i + 1
  }
  
  // 计算去趋势后的季节性
  let i = 0
  while i < series.points.length() {
    if i < trend.points.length() {
      let detrended = series.points[i].value - trend.points[i].value
      let season_index = i % period
      seasonal_values[season_index] = seasonal_values[season_index] + detrended
      counts[season_index] = counts[season_index] + 1
    }
    i = i + 1
  }
  
  let i = 0
  while i < period {
    if counts[i] > 0 {
      seasonal_values[i] = seasonal_values[i] / counts[i] as Float
    }
    i = i + 1
  }
  
  // 构建季节性序列
  let i = 0
  while i < series.points.length() {
    let season_index = i % period
    add_point(seasonal, series.points[i].timestamp, seasonal_values[season_index])
    i = i + 1
  }
  
  // 计算残差
  let residual = create_time_series(series.name + "_residual", series.interval, series.unit)
  let i = 0
  while i < series.points.length() {
    if i < trend.points.length() {
      let residual_value = series.points[i].value - trend.points[i].value - seasonal.points[i].value
      add_point(residual, series.points[i].timestamp, residual_value)
    }
    i = i + 1
  }
  
  Some((trend, seasonal, residual))
}

// 辅助函数：检测变化点
fn detect_change_points(series : TimeSeries) -> Array[Int] {
  let change_points = []
  
  if series.points.length() < 4 {
    return change_points
  }
  
  let window_size = series.points.length() / 4
  if window_size < 2 {
    window_size = 2
  }
  
  let i = window_size
  while i < series.points.length() - window_size {
    // 计算窗口前的均值
    let left_mean = calculate_mean(extract_values(
      series.points.slice(i - window_size, window_size)
    ))
    
    // 计算窗口后的均值
    let right_mean = calculate_mean(extract_values(
      series.points.slice(i, window_size)
    ))
    
    // 计算变化程度
    let change_magnitude = (right_mean - left_mean).abs()
    
    // 如果变化超过阈值，标记为变化点
    if change_magnitude > left_mean * 0.2 { // 20%的变化阈值
      change_points.push(series.points[i].timestamp)
    }
    
    i = i + 1
  }
  
  change_points
}

// 辅助函数：使用移动平均预测
fn forecast_with_moving_average(series : TimeSeries, periods : Int) -> TimeSeries {
  let forecast = create_time_series(series.name + "_ma_forecast", series.interval, series.unit)
  
  if series.points.length() == 0 {
    return forecast
  }
  
  let window_size = if series.points.length() < 7 { series.points.length() } else { 7 }
  let last_timestamp = series.points[series.points.length() - 1].timestamp
  
  // 计算最近窗口的平均值
  let recent_values = []
  let i = series.points.length() - window_size
  while i < series.points.length() {
    recent_values.push(series.points[i].value)
    i = i + 1
  }
  
  let avg_value = calculate_mean(recent_values)
  
  // 生成预测值
  let i = 0
  while i < periods {
    add_point(forecast, last_timestamp + (i + 1) * series.interval, avg_value)
    i = i + 1
  }
  
  forecast
}

// 辅助函数：使用线性回归预测
fn forecast_with_linear_regression(series : TimeSeries, periods : Int) -> TimeSeries {
  let forecast = create_time_series(series.name + "_lr_forecast", series.interval, series.unit)
  
  match detect_linear_trend(series) {
    Some((slope, intercept)) => {
      let last_timestamp = series.points[series.points.length() - 1].timestamp
      let last_x = (series.points.length() - 1) as Float
      
      let i = 0
      while i < periods {
        let x = last_x + (i + 1) as Float
        let predicted_value = slope * x + intercept
        add_point(forecast, last_timestamp + (i + 1) * series.interval, predicted_value)
        i = i + 1
      }
    }
    None => {
      // 如果无法检测趋势，使用最后一个值
      let last_value = series.points[series.points.length() - 1].value
      let last_timestamp = series.points[series.points.length() - 1].timestamp
      
      let i = 0
      while i < periods {
        add_point(forecast, last_timestamp + (i + 1) * series.interval, last_value)
        i = i + 1
      }
    }
  }
  
  forecast
}

// 辅助函数：使用指数平滑预测
fn forecast_with_exponential_smoothing(series : TimeSeries, periods : Int, alpha : Float) -> TimeSeries {
  let forecast = create_time_series(series.name + "_exp_forecast", series.interval, series.unit)
  
  if series.points.length() == 0 {
    return forecast
  }
  
  // 应用指数平滑
  let smoothed = apply_exponential_smoothing(series, alpha)
  let last_smoothed_value = smoothed.points[smoothed.points.length() - 1].value
  let last_timestamp = series.points[series.points.length() - 1].timestamp
  
  // 生成预测值（使用最后一个平滑值）
  let i = 0
  while i < periods {
    add_point(forecast, last_timestamp + (i + 1) * series.interval, last_smoothed_value)
    i = i + 1
  }
  
  forecast
}

// 辅助函数：使用季节性预测
fn forecast_with_seasonality(series : TimeSeries, periods : Int, season_length : Int) -> TimeSeries {
  let forecast = create_time_series(series.name + "_seasonal_forecast", series.interval, series.unit)
  
  if series.points.length() < season_length {
    return forecast
  }
  
  match detect_seasonality(series, season_length) {
    Some(_) => {
      // 计算趋势
      match detect_linear_trend(series) {
        Some((slope, intercept)) => {
          let last_timestamp = series.points[series.points.length() - 1].timestamp
          let last_x = (series.points.length() - 1) as Float
          
          // 计算季节性模式
          let seasonal_pattern = []
          let i = 0
          while i < season_length {
            seasonal_pattern.push(0.0)
            i = i + 1
          }
          
          let counts = []
          let i = 0
          while i < season_length {
            counts.push(0)
            i = i + 1
          }
          
          // 计算去趋势后的季节性
          let i = 0
          while i < series.points.length() {
            let x = i as Float
            let trend_value = slope * x + intercept
            let detrended = series.points[i].value - trend_value
            let season_index = i % season_length
            seasonal_pattern[season_index] = seasonal_pattern[season_index] + detrended
            counts[season_index] = counts[season_index] + 1
            i = i + 1
          }
          
          let i = 0
          while i < season_length {
            if counts[i] > 0 {
              seasonal_pattern[i] = seasonal_pattern[i] / counts[i] as Float
            }
            i = i + 1
          }
          
          // 生成预测值
          let i = 0
          while i < periods {
            let x = last_x + (i + 1) as Float
            let trend_value = slope * x + intercept
            let season_index = (series.points.length() + i) % season_length
            let seasonal_value = seasonal_pattern[season_index]
            let predicted_value = trend_value + seasonal_value
            
            add_point(forecast, last_timestamp + (i + 1) * series.interval, predicted_value)
            i = i + 1
          }
        }
        None => {
          // 如果无法检测趋势，只使用季节性
          let last_timestamp = series.points[series.points.length() - 1].timestamp
          
          // 计算季节性模式
          let seasonal_pattern = []
          let i = 0
          while i < season_length {
            seasonal_pattern.push(0.0)
            i = i + 1
          }
          
          let counts = []
          let i = 0
          while i < season_length {
            counts.push(0)
            i = i + 1
          }
          
          let i = 0
          while i < series.points.length() {
            let season_index = i % season_length
            seasonal_pattern[season_index] = seasonal_pattern[season_index] + series.points[i].value
            counts[season_index] = counts[season_index] + 1
            i = i + 1
          }
          
          let i = 0
          while i < season_length {
            if counts[i] > 0 {
              seasonal_pattern[i] = seasonal_pattern[i] / counts[i] as Float
            }
            i = i + 1
          }
          
          // 生成预测值
          let i = 0
          while i < periods {
            let season_index = (series.points.length() + i) % season_length
            let predicted_value = seasonal_pattern[season_index]
            
            add_point(forecast, last_timestamp + (i + 1) * series.interval, predicted_value)
            i = i + 1
          }
        }
      }
    }
    None => {
      // 如果无法检测季节性，使用线性回归
      return forecast_with_linear_regression(series, periods)
    }
  }
  
  forecast
}

// 预测精度评估结果类型
type ForecastAccuracy = {
  mae : Float, // 平均绝对误差
  rmse : Float, // 均方根误差
  mape : Float // 平均绝对百分比误差
}

// 辅助函数：评估预测精度
fn evaluate_forecast_accuracy(actual : TimeSeries, forecast : TimeSeries) -> ForecastAccuracy {
  if actual.points.length() != forecast.points.length() || actual.points.length() == 0 {
    return { mae: 0.0, rmse: 0.0, mape: 0.0 }
  }
  
  let mae_sum = 0.0
  let mse_sum = 0.0
  let mape_sum = 0.0
  
  let i = 0
  while i < actual.points.length() {
    let actual_value = actual.points[i].value
    let forecast_value = forecast.points[i].value
    let error = (actual_value - forecast_value).abs()
    
    mae_sum = mae_sum + error
    mse_sum = mse_sum + error * error
    
    if actual_value != 0.0 {
      mape_sum = mape_sum + error / actual_value.abs()
    }
    i = i + 1
  }
  
  let n = actual.points.length() as Float
  
  {
    mae: mae_sum / n,
    rmse: (mse_sum / n).sqrt(),
    mape: (mape_sum / n) * 100.0
  }
}

// 辅助函数：计算相关系数
fn calculate_correlation(series1 : TimeSeries, series2 : TimeSeries) -> Float {
  if series1.points.length() != series2.points.length() || series1.points.length() == 0 {
    return 0.0
  }
  
  let values1 = extract_values(series1.points)
  let values2 = extract_values(series2.points)
  
  let mean1 = calculate_mean(values1)
  let mean2 = calculate_mean(values2)
  
  let sum_xy = 0.0
  let sum_x2 = 0.0
  let sum_y2 = 0.0
  
  let i = 0
  while i < values1.length() {
    let x_diff = values1[i] - mean1
    let y_diff = values2[i] - mean2
    
    sum_xy = sum_xy + x_diff * y_diff
    sum_x2 = sum_x2 + x_diff * x_diff
    sum_y2 = sum_y2 + y_diff * y_diff
    i = i + 1
  }
  
  let denominator = (sum_x2 * sum_y2).sqrt()
  if denominator.abs() < 0.0001 {
    return 0.0
  }
  
  sum_xy / denominator
}

// 辅助函数：计算交叉相关
fn calculate_cross_correlation(series1 : TimeSeries, series2 : TimeSeries, max_lag : Int) -> Array[Float] {
  let correlations = []
  
  let n = series1.points.length()
  if n == 0 || series2.points.length() != n {
    return correlations
  }
  
  let values1 = extract_values(series1.points)
  let values2 = extract_values(series2.points)
  
  let lag = -max_lag
  while lag <= max_lag {
    let sum_xy = 0.0
    let sum_x2 = 0.0
    let sum_y2 = 0.0
    let count = 0
    
    let i = 0
    while i < n {
      let j = i + lag
      
      if j >= 0 && j < n {
        sum_xy = sum_xy + values1[i] * values2[j]
        sum_x2 = sum_x2 + values1[i] * values1[i]
        sum_y2 = sum_y2 + values2[j] * values2[j]
        count = count + 1
      }
      i = i + 1
    }
    
    if count > 0 {
      let denominator = (sum_x2 * sum_y2).sqrt()
      if denominator.abs() > 0.0001 {
        correlations.push(sum_xy / denominator)
      } else {
        correlations.push(0.0)
      }
    } else {
      correlations.push(0.0)
    }
    
    lag = lag + 1
  }
  
  correlations
}

// 辅助函数：找到最大相关性的滞后
fn find_max_correlation_lag(correlations : Array[Float]) -> Int {
  if correlations.length() == 0 {
    return 0
  }
  
  let max_correlation = correlations[0]
  let max_lag = -correlations.length() / 2
  
  let i = 0
  while i < correlations.length() {
    if correlations[i] > max_correlation {
      max_correlation = correlations[i]
      max_lag = i - correlations.length() / 2
    }
    i = i + 1
  }
  
  max_lag
}

// 辅助函数：创建滞后序列
fn create_lagged_series(series : TimeSeries, lag : Int) -> TimeSeries {
  let lagged = create_time_series(series.name + "_lagged", series.interval, series.unit)
  
  if series.points.length() <= lag {
    return lagged
  }
  
  let i = lag
  while i < series.points.length() {
    add_point(lagged, series.points[i].timestamp, series.points[i - lag].value)
    i = i + 1
  }
  
  lagged
}

// 辅助函数：计算序列大小（简化）
fn calculate_series_size(series : TimeSeries) -> Int {
  // 简化的大小计算，基于数据点数量和精度
  let base_size = series.points.length() * 16 // 每个点16字节
  let name_size = series.name.length() * 1
  let unit_size = series.unit.length() * 1
  
  base_size + name_size + unit_size
}

// 辅助函数：降低精度
fn reduce_precision(series : TimeSeries, decimal_places : Int) -> TimeSeries {
  let reduced = create_time_series(series.name + "_reduced", series.interval, series.unit)
  
  let factor = (10.0 as Float).pow(decimal_places as Float)
  
  for point in series.points {
    let reduced_value = (point.value * factor).round() / factor
    add_point(reduced, point.timestamp, reduced_value)
  }
  
  reduced
}

// 辅助函数：计算精度损失
fn calculate_precision_loss(original : TimeSeries, reduced : TimeSeries) -> Float {
  if original.points.length() != reduced.points.length() {
    return Float::infinity
  }
  
  let total_error = 0.0
  let i = 0
  while i < original.points.length() {
    let error = (original.points[i].value - reduced.points[i].value).abs()
    total_error = total_error + error
    i = i + 1
  }
  
  total_error / original.points.length() as Float
}

// 辅助函数：应用增量编码
fn apply_delta_encoding(series : TimeSeries) -> TimeSeries {
  let encoded = create_time_series(series.name + "_delta", series.interval, series.unit)
  
  if series.points.length() == 0 {
    return encoded
  }
  
  // 第一个点保持原值
  add_point(encoded, series.points[0].timestamp, series.points[0].value)
  
  // 后续点存储与前一点的差值
  let i = 1
  while i < series.points.length() {
    let delta = series.points[i].value - series.points[i - 1].value
    add_point(encoded, series.points[i].timestamp, delta)
    i = i + 1
  }
  
  encoded
}

// 辅助函数：应用旋转门压缩
fn apply_swing_door_compression(series : TimeSeries, tolerance : Float) -> TimeSeries {
  let compressed = create_time_series(series.name + "_compressed", series.interval, series.unit)
  
  if series.points.length() == 0 {
    return compressed
  }
  
  // 添加第一个点
  add_point(compressed, series.points[0].timestamp, series.points[0].value)
  
  let i = 1
  while i < series.points.length() {
    let current_point = series.points[i]
    let last_compressed = compressed.points[compressed.points.length() - 1]
    
    // 检查当前点是否超出容忍范围
    let deviation = (current_point.value - last_compressed.value).abs()
    
    if deviation > tolerance {
      // 如果超出容忍范围，添加新点
      add_point(compressed, current_point.timestamp, current_point.value)
    }
    // 如果在容忍范围内，跳过该点
    
    i = i + 1
  }
  
  // 确保最后一个点被包含
  let last_original = series.points[series.points.length() - 1]
  let last_compressed = compressed.points[compressed.points.length() - 1]
  
  if last_original.timestamp != last_compressed.timestamp {
    add_point(compressed, last_original.timestamp, last_original.value)
  }
  
  compressed
}

// 辅助函数：计算重建误差
fn calculate_reconstruction_error(original : TimeSeries, compressed : TimeSeries) -> Float {
  // 简化的重建误差计算
  // 在实际项目中，需要先解压缩再计算误差
  
  if original.points.length() == 0 || compressed.points.length() == 0 {
    return Float::infinity
  }
  
  let total_error = 0.0
  let count = 0
  
  let i = 0
  while i < original.points.length() {
    // 找到压缩序列中最接近的点
    let closest_value = find_closest_compressed_value(compressed, original.points[i])
    let error = (original.points[i].value - closest_value).abs()
    
    total_error = total_error + error
    count = count + 1
    i = i + 1
  }
  
  total_error / count as Float
}

// 辅助函数：查找压缩序列中最接近的值（简化实现）
fn find_closest_compressed_value(compressed : TimeSeries, original_point : TimeSeriesPoint) -> Float {
  if compressed.points.length() == 0 {
    return 0.0
  }
  
  let closest_value = compressed.points[0].value
  let min_diff = (compressed.points[0].value - original_point.value).abs()
  
  let i = 1
  while i < compressed.points.length() {
    let diff = (compressed.points[i].value - original_point.value).abs()
    if diff < min_diff {
      min_diff = diff
      closest_value = compressed.points[i].value
    }
    i = i + 1
  }
  
  closest_value
}

// 辅助函数：分割成块
fn split_into_chunks(series : TimeSeries, chunk_duration : Int) -> Array[TimeSeries] {
  let chunks = []
  
  if series.points.length() == 0 {
    return chunks
  }
  
  let start_time = series.points[0].timestamp
  let end_time = series.points[series.points.length() - 1].timestamp
  
  let current_start = start_time
  while current_start <= end_time {
    let current_end = current_start + chunk_duration
    
    let chunk = create_time_series(series.name + "_chunk", series.interval, series.unit)
    
    let i = 0
    while i < series.points.length() {
      let point = series.points[i]
      if point.timestamp >= current_start && point.timestamp < current_end {
        add_point(chunk, point.timestamp, point.value)
      }
      i = i + 1
    }
    
    if chunk.points.length() > 0 {
      chunks.push(chunk)
    }
    
    current_start = current_end
  }
  
  chunks
}

// 辅助函数：合并块
fn merge_chunks(chunks : Array[TimeSeries]) -> TimeSeries {
  if chunks.length() == 0 {
    return create_time_series("merged", 0, "")
  }
  
  let merged = create_time_series(chunks[0].name, chunks[0].interval, chunks[0].unit)
  
  for chunk in chunks {
    for point in chunk.points {
      add_point(merged, point.timestamp, point.value)
    }
  }
  
  // 按时间戳排序
  let i = 0
  while i < merged.points.length() {
    let j = i + 1
    while j < merged.points.length() {
      if merged.points[i].timestamp > merged.points[j].timestamp {
        let temp = merged.points[i]
        merged.points[i] = merged.points[j]
        merged.points[j] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
  
  merged
}

// 辅助函数：切片序列
fn slice_series(series : TimeSeries, start_index : Int, end_index : Int) -> TimeSeries {
  let sliced = create_time_series(series.name + "_slice", series.interval, series.unit)
  
  if start_index < 0 || end_index > series.points.length() || start_index >= end_index {
    return sliced
  }
  
  let i = start_index
  while i < end_index {
    add_point(sliced, series.points[i].timestamp, series.points[i].value)
    i = i + 1
  }
  
  sliced
}