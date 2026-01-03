// Azimuth 时间序列分析测试
// 专注于遥测数据的时间序列分析和预测功能

// 测试1: 时间序列趋势分析
test "时间序列趋势分析" {
  // 模拟时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 100.0, metric: "cpu_usage" },
    { timestamp: 1640995260, value: 105.0, metric: "cpu_usage" },
    { timestamp: 1640995320, value: 110.0, metric: "cpu_usage" },
    { timestamp: 1640995380, value: 108.0, metric: "cpu_usage" },
    { timestamp: 1640995440, value: 115.0, metric: "cpu_usage" },
    { timestamp: 1640995500, value: 120.0, metric: "cpu_usage" },
    { timestamp: 1640995560, value: 118.0, metric: "cpu_usage" },
    { timestamp: 1640995620, value: 125.0, metric: "cpu_usage" },
    { timestamp: 1640995680, value: 130.0, metric: "cpu_usage" },
    { timestamp: 1640995740, value: 135.0, metric: "cpu_usage" }
  ]
  
  // 计算基本趋势指标
  let n = time_series_data.length()
  let first_value = time_series_data[0].value
  let last_value = time_series_data[n-1].value
  
  // 计算线性趋势
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut sum_xy = 0.0
  let mut sum_x2 = 0.0
  
  for i in 0..n {
    let x = i.to_float()
    let y = time_series_data[i].value
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
  }
  
  let slope = (n.to_float() * sum_xy - sum_x * sum_y) / (n.to_float() * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n.to_float()
  
  // 计算趋势强度（R²）
  let mut ss_total = 0.0
  let mut ss_residual = 0.0
  let y_mean = sum_y / n.to_float()
  
  for i in 0..n {
    let x = i.to_float()
    let y_actual = time_series_data[i].value
    let y_predicted = slope * x + intercept
    
    ss_total = ss_total + (y_actual - y_mean) * (y_actual - y_mean)
    ss_residual = ss_residual + (y_actual - y_predicted) * (y_actual - y_predicted)
  }
  
  let r_squared = if ss_total > 0.0 { 1.0 - ss_residual / ss_total } else { 0.0 }
  
  // 计算变化率
  let overall_change = last_value - first_value
  let overall_change_rate = overall_change / first_value * 100.0
  
  // 计算移动平均
  let window_size = 3
  let mut moving_averages = []
  
  for i in window_size-1..n {
    let mut sum = 0.0
    for j in i-window_size+1..i+1 {
      sum = sum + time_series_data[j].value
    }
    let avg = sum / window_size.to_float()
    moving_averages = moving_averages.push({
      index: i,
      moving_average: avg
    })
  }
  
  // 验证趋势分析结果
  assert_true(slope > 0.0) // 应该是上升趋势
  assert_eq(r_squared > 0.8, true) // 趋势应该比较强
  
  // 验证变化率
  assert_true(overall_change > 0.0) // 应该有正向变化
  assert_true(overall_change_rate > 30.0) // 变化率应该超过30%
  
  // 验证移动平均
  assert_eq(moving_averages.length(), n - window_size + 1)
  
  // 移动平均应该平滑数据
  for i in 1..moving_averages.length() {
    assert_true(moving_averages[i].moving_average >= moving_averages[i-1].moving_average - 5.0)
  }
  
  // 验证趋势预测
  let future_points = 5
  let mut predictions = []
  
  for i in 0..future_points {
    let x = (n + i).to_float()
    let predicted_value = slope * x + intercept
    predictions = predictions.push(predicted_value)
  }
  
  // 预测值应该继续上升趋势
  for i in 1..predictions.length() {
    assert_true(predictions[i] > predictions[i-1])
  }
  
  // 最后一个预测值应该大于最后一个实际值
  assert_true(predictions[predictions.length()-1] > last_value)
}

// 测试2: 时间序列季节性分析
test "时间序列季节性分析" {
  // 模拟具有季节性的时间序列数据（24小时周期）
  let seasonal_data = [
    { timestamp: 1640995200, hour: 8, value: 45.0, day_of_week: 1 }, // 周一早晨
    { timestamp: 1640995260, hour: 9, value: 55.0, day_of_week: 1 },
    { timestamp: 1640995320, hour: 10, value: 65.0, day_of_week: 1 },
    { timestamp: 1640995380, hour: 11, value: 70.0, day_of_week: 1 },
    { timestamp: 1640995440, hour: 12, value: 75.0, day_of_week: 1 },
    { timestamp: 1640995500, hour: 13, value: 72.0, day_of_week: 1 },
    { timestamp: 1640995560, hour: 14, value: 68.0, day_of_week: 1 },
    { timestamp: 1640995620, hour: 15, value: 65.0, day_of_week: 1 },
    { timestamp: 1640995680, hour: 16, value: 60.0, day_of_week: 1 },
    { timestamp: 1640995740, hour: 17, value: 55.0, day_of_week: 1 },
    { timestamp: 1640995800, hour: 18, value: 50.0, day_of_week: 1 },
    { timestamp: 1640995860, hour: 19, value: 40.0, day_of_week: 1 },
    { timestamp: 1640995920, hour: 20, value: 35.0, day_of_week: 1 },
    { timestamp: 1640995980, hour: 21, value: 30.0, day_of_week: 1 },
    { timestamp: 1640996040, hour: 22, value: 25.0, day_of_week: 1 },
    { timestamp: 1640996100, hour: 23, value: 20.0, day_of_week: 1 },
    { timestamp: 1640996160, hour: 0, value: 15.0, day_of_week: 1 },
    { timestamp: 1640996220, hour: 1, value: 12.0, day_of_week: 1 },
    { timestamp: 1640996280, hour: 2, value: 10.0, day_of_week: 1 },
    { timestamp: 1640996340, hour: 3, value: 8.0, day_of_week: 1 },
    { timestamp: 1640996400, hour: 4, value: 10.0, day_of_week: 1 },
    { timestamp: 1640996460, hour: 5, value: 15.0, day_of_week: 1 },
    { timestamp: 1640996520, hour: 6, value: 25.0, day_of_week: 1 },
    { timestamp: 1640996580, hour: 7, value: 35.0, day_of_week: 1 },
    // 第二天的数据（周二）
    { timestamp: 1641081600, hour: 8, value: 48.0, day_of_week: 2 },
    { timestamp: 1641081660, hour: 9, value: 58.0, day_of_week: 2 },
    { timestamp: 1641081720, hour: 10, value: 68.0, day_of_week: 2 },
    { timestamp: 1641081780, hour: 11, value: 73.0, day_of_week: 2 },
    { timestamp: 1641081840, hour: 12, value: 78.0, day_of_week: 2 }
  ]
  
  // 按小时分组分析季节性
  let mut hourly_patterns = []
  
  for hour in 0..24 {
    let hour_data = seasonal_data.filter_fn(d => d.hour == hour)
    if hour_data.length() > 0 {
      let mut sum = 0.0
      for d in hour_data {
        sum = sum + d.value
      }
      let avg_value = sum / hour_data.length().to_float()
      
      hourly_patterns = hourly_patterns.push({
        hour: hour,
        avg_value: avg_value,
        data_points: hour_data.length()
      })
    }
  }
  
  // 按天分组分析周模式
  let mut daily_patterns = []
  
  for day in 1..8 {
    let day_data = seasonal_data.filter_fn(d => d.day_of_week == day)
    if day_data.length() > 0 {
      let mut sum = 0.0
      for d in day_data {
        sum = sum + d.value
      }
      let avg_value = sum / day_data.length().to_float()
      
      daily_patterns = daily_patterns.push({
        day_of_week: day,
        avg_value: avg_value,
        data_points: day_data.length()
      })
    }
  }
  
  // 验证小时季节性模式
  assert_eq(hourly_patterns.length(), 24)
  
  // 找到峰值和谷值时间
  let sorted_by_value = hourly_patterns.sort(fn(a, b) { a.avg_value <= b.avg_value })
  let lowest_hour = sorted_by_value[0]
  let highest_hour = sorted_by_value[sorted_by_value.length()-1]
  
  // 验证最低值在凌晨
  assert_true(lowest_hour.hour >= 2 && lowest_hour.hour <= 4)
  assert_true(lowest_hour.avg_value < 15.0)
  
  // 验证最高值在中午
  assert_true(highest_hour.hour >= 11 && highest_hour.hour <= 13)
  assert_true(highest_hour.avg_value > 70.0)
  
  // 验证日间变化模式
  let morning_hours = hourly_patterns.filter_fn(p => p.hour >= 6 && p.hour <= 10)
  let afternoon_hours = hourly_patterns.filter_fn(p => p.hour >= 14 && p.hour <= 18)
  let evening_hours = hourly_patterns.filter_fn(p => p.hour >= 19 && p.hour <= 23)
  let night_hours = hourly_patterns.filter_fn(p => p.hour >= 0 && p.hour <= 5)
  
  // 计算各时段平均值
  let morning_avg = morning_hours.reduce(fn(acc, p) { acc + p.avg_value }, 0.0) / morning_hours.length().to_float()
  let afternoon_avg = afternoon_hours.reduce(fn(acc, p) { acc + p.avg_value }, 0.0) / afternoon_hours.length().to_float()
  let evening_avg = evening_hours.reduce(fn(acc, p) { acc + p.avg_value }, 0.0) / evening_hours.length().to_float()
  let night_avg = night_hours.reduce(fn(acc, p) { acc + p.avg_value }, 0.0) / night_hours.length().to_float()
  
  // 验证时段差异
  assert_true(morning_avg > night_avg)
  assert_true(afternoon_avg > morning_avg)
  assert_true(evening_avg < afternoon_avg)
  assert_true(evening_avg > night_avg)
  
  // 验证周模式
  assert_eq(daily_patterns.length(), 2) // 只有周一和周二的数据
  
  // 计算季节性强度
  let overall_mean = seasonal_data.reduce(fn(acc, d) { acc + d.value }, 0.0) / seasonal_data.length().to_float()
  let mut seasonal_variance = 0.0
  
  for pattern in hourly_patterns {
    let deviation = pattern.avg_value - overall_mean
    seasonal_variance = seasonal_variance + deviation * deviation
  }
  
  let seasonal_strength = seasonal_variance / hourly_patterns.length().to_float()
  assert_true(seasonal_strength > 100.0) // 应该有明显的季节性
  
  // 验证季节性预测
  let next_day_hourly_forecast = []
  for pattern in hourly_patterns {
    next_day_hourly_forecast = next_day_hourly_forecast.push({
      hour: pattern.hour,
      predicted_value: pattern.avg_value
    })
  }
  
  // 验证预测的合理性
  let forecast_morning = next_day_hourly_forecast.filter_fn(p => p.hour >= 8 && p.hour <= 10)
  let forecast_night = next_day_hourly_forecast.filter_fn(p => p.hour >= 0 && p.hour <= 5)
  
  let forecast_morning_avg = forecast_morning.reduce(fn(acc, p) { acc + p.predicted_value }, 0.0) / forecast_morning.length().to_float()
  let forecast_night_avg = forecast_night.reduce(fn(acc, p) { acc + p.predicted_value }, 0.0) / forecast_night.length().to_float()
  
  assert_true(forecast_morning_avg > forecast_night_avg)
}

// 测试3: 时间序列异常检测
test "时间序列异常检测" {
  // 模拟包含异常的时间序列数据
  let anomaly_data = [
    { timestamp: 1640995200, value: 50.0, is_anomaly: false },
    { timestamp: 1640995260, value: 52.0, is_anomaly: false },
    { timestamp: 1640995320, value: 48.0, is_anomaly: false },
    { timestamp: 1640995380, value: 51.0, is_anomaly: false },
    { timestamp: 1640995440, value: 150.0, is_anomaly: true }, // 尖峰异常
    { timestamp: 1640995500, value: 49.0, is_anomaly: false },
    { timestamp: 1640995560, value: 53.0, is_anomaly: false },
    { timestamp: 1640995620, value: 47.0, is_anomaly: false },
    { timestamp: 1640995680, value: 5.0, is_anomaly: true }, // 谷值异常
    { timestamp: 1640995740, value: 52.0, is_anomaly: false },
    { timestamp: 1640995800, value: 48.0, is_anomaly: false },
    { timestamp: 1640995860, value: 51.0, is_anomaly: false },
    { timestamp: 1640995920, value: 49.0, is_anomaly: false },
    { timestamp: 1640995980, value: 53.0, is_anomaly: false },
    { timestamp: 1640996040, value: 47.0, is_anomaly: false }
  ]
  
  // 使用Z-score方法检测异常
  let normal_data = anomaly_data.filter_fn(d => !d.is_anomaly)
  
  // 计算正常数据的均值和标准差
  let n = normal_data.length()
  let mean = normal_data.reduce(fn(acc, d) { acc + d.value }, 0.0) / n.to_float()
  
  let mut variance = 0.0
  for d in normal_data {
    let diff = d.value - mean
    variance = variance + diff * diff
  }
  let std_dev = (variance / n.to_float()).sqrt()
  
  // 设置异常阈值（3个标准差）
  let upper_threshold = mean + 3.0 * std_dev
  let lower_threshold = mean - 3.0 * std_dev
  
  // 检测异常
  let mut detected_anomalies = []
  let mut normal_points = []
  
  for d in anomaly_data {
    let z_score = (d.value - mean) / std_dev
    let is_anomaly = d.value > upper_threshold || d.value < lower_threshold
    
    if is_anomaly {
      detected_anomalies = detected_anomalies.push({
        timestamp: d.timestamp,
        value: d.value,
        z_score: z_score,
        threshold_exceeded: if d.value > upper_threshold { "upper" } else { "lower" }
      })
    } else {
      normal_points = normal_points.push(d)
    }
  }
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 2)
  assert_eq(normal_points.length(), 13)
  
  // 验证检测到的异常
  let spike_anomaly = detected_anomalies.filter_fn(a => a.threshold_exceeded == "upper")[0]
  assert_eq(spike_anomaly.value, 150.0)
  assert_true(spike_anomaly.z_score > 3.0)
  
  let dip_anomaly = detected_anomalies.filter_fn(a => a.threshold_exceeded == "lower")[0]
  assert_eq(dip_anomaly.value, 5.0)
  assert_true(dip_anomaly.z_score < -3.0)
  
  // 使用移动平均方法检测异常
  let window_size = 3
  let mut ma_anomalies = []
  
  for i in window_size..anomaly_data.length() {
    let mut sum = 0.0
    for j in i-window_size..i {
      sum = sum + anomaly_data[j].value
    }
    let ma = sum / window_size.to_float()
    let current_value = anomaly_data[i].value
    let deviation = (current_value - ma).abs() / ma
    
    if deviation > 0.5 { // 50%偏差阈值
      ma_anomalies = ma_anomalies.push({
        timestamp: anomaly_data[i].timestamp,
        value: current_value,
        moving_average: ma,
        deviation_percent: deviation * 100.0
      })
    }
  }
  
  // 验证移动平均异常检测
  assert_true(ma_anomalies.length() >= 1) // 至少检测到一个异常
  
  // 使用指数平滑方法检测异常
  let alpha = 0.3 // 平滑因子
  let mut ema = anomaly_data[0].value
  let mut ema_anomalies = []
  
  for i in 1..anomaly_data.length() {
    ema = alpha * anomaly_data[i].value + (1.0 - alpha) * ema
    let deviation = (anomaly_data[i].value - ema).abs() / ema
    
    if deviation > 0.4 { // 40%偏差阈值
      ema_anomalies = ema_anomalies.push({
        timestamp: anomaly_data[i].timestamp,
        value: anomaly_data[i].value,
        ema: ema,
        deviation_percent: deviation * 100.0
      })
    }
  }
  
  // 验证指数平滑异常检测
  assert_true(ema_anomalies.length() >= 1) // 至少检测到一个异常
  
  // 计算异常检测性能指标
  let true_anomalies = anomaly_data.filter_fn(d => d.is_anomaly)
  let false_positives = detected_anomalies.length() - true_anomalies.length()
  let false_negatives = true_anomalies.length() - detected_anomalies.filter_fn(da => 
    true_anomalies.some(ta => ta.timestamp == da.timestamp)
  ).length()
  
  let precision = if detected_anomalies.length() > 0 {
    (detected_anomalies.length() - false_positives).to_float() / detected_anomalies.length().to_float()
  } else { 0.0 }
  
  let recall = if true_anomalies.length() > 0 {
    (true_anomalies.length() - false_negatives).to_float() / true_anomalies.length().to_float()
  } else { 0.0 }
  
  // 验证检测性能
  assert_true(precision > 0.8) // 精确率应该高于80%
  assert_true(recall > 0.8)  // 召回率应该高于80%
}

// 测试4: 时间序列分解
test "时间序列分解" {
  // 模拟可分解的时间序列数据
  let decomposable_data = [
    { timestamp: 1640995200, value: 120.0, trend: 100.0, seasonal: 20.0, residual: 0.0 },
    { timestamp: 1640995260, value: 125.0, trend: 101.0, seasonal: 24.0, residual: 0.0 },
    { timestamp: 1640995320, value: 130.0, trend: 102.0, seasonal: 28.0, residual: 0.0 },
    { timestamp: 1640995380, value: 135.0, trend: 103.0, seasonal: 32.0, residual: 0.0 },
    { timestamp: 1640995440, value: 140.0, trend: 104.0, seasonal: 36.0, residual: 0.0 },
    { timestamp: 1640995500, value: 135.0, trend: 105.0, seasonal: 30.0, residual: 0.0 },
    { timestamp: 1640995560, value: 130.0, trend: 106.0, seasonal: 24.0, residual: 0.0 },
    { timestamp: 1640995620, value: 125.0, trend: 107.0, seasonal: 18.0, residual: 0.0 },
    { timestamp: 1640995680, value: 120.0, trend: 108.0, seasonal: 12.0, residual: 0.0 },
    { timestamp: 1640995740, value: 115.0, trend: 109.0, seasonal: 6.0, residual: 0.0 },
    { timestamp: 1640995800, value: 110.0, trend: 110.0, seasonal: 0.0, residual: 0.0 },
    { timestamp: 1640995860, value: 115.0, trend: 111.0, seasonal: 4.0, residual: 0.0 },
    { timestamp: 1640995920, value: 120.0, trend: 112.0, seasonal: 8.0, residual: 0.0 },
    { timestamp: 1640995980, value: 125.0, trend: 113.0, seasonal: 12.0, residual: 0.0 },
    { timestamp: 1640996040, value: 130.0, trend: 114.0, seasonal: 16.0, residual: 0.0 }
  ]
  
  // 执行时间序列分解
  let n = decomposable_data.length()
  
  // 提取趋势分量（使用移动平均）
  let trend_window = 5
  let mut extracted_trend = []
  
  for i in 0..n {
    if i < trend_window/2 || i >= n - trend_window/2 {
      // 边界点使用简单平均
      extracted_trend = extracted_trend.push(decomposable_data[i].trend)
    } else {
      let mut sum = 0.0
      for j in i-trend_window/2..i+trend_window/2+1 {
        sum = sum + decomposable_data[j].value
      }
      extracted_trend = extracted_trend.push(sum / trend_window.to_float())
    }
  }
  
  // 提取季节性分量（假设已知周期）
  let seasonal_period = 10
  let mut seasonal_pattern = []
  
  for i in 0..seasonal_period {
    let mut sum = 0.0
    let count = 0
    for j in i..n {
      if j % seasonal_period == i {
        sum = sum + (decomposable_data[j].value - extracted_trend[j])
        count = count + 1
      }
    }
    if count > 0 {
      seasonal_pattern = seasonal_pattern.push(sum / count.to_float())
    }
  }
  
  // 计算残差分量
  let mut residuals = []
  for i in 0..n {
    let seasonal_index = i % seasonal_period
    let reconstructed = extracted_trend[i] + seasonal_pattern[seasonal_index]
    let residual = decomposable_data[i].value - reconstructed
    residuals = residuals.push(residual)
  }
  
  // 验证分解结果
  assert_eq(extracted_trend.length(), n)
  assert_eq(seasonal_pattern.length(), seasonal_period)
  assert_eq(residuals.length(), n)
  
  // 验证趋势分量的单调性
  for i in 1..extracted_trend.length() {
    assert_true(extracted_trend[i] >= extracted_trend[i-1] - 1.0) // 允许小幅波动
  }
  
  // 验证季节性分量
  let seasonal_sum = seasonal_pattern.reduce(fn(acc, s) { acc + s }, 0.0)
  assert_true(seasonal_sum.abs() < 1.0) // 季节性分量应该接近零和
  
  // 验证残差分量
  let residual_mean = residuals.reduce(fn(acc, r) { acc + r }, 0.0) / residuals.length().to_float()
  assert_true(residual_mean.abs() < 1.0) // 残差均值应该接近零
  
  // 验证重构准确性
  let mut reconstruction_errors = []
  for i in 0..n {
    let seasonal_index = i % seasonal_period
    let reconstructed = extracted_trend[i] + seasonal_pattern[seasonal_index] + residuals[i]
    let error = (reconstructed - decomposable_data[i].value).abs()
    reconstruction_errors = reconstruction_errors.push(error)
  }
  
  let max_error = reconstruction_errors.reduce(fn(acc, e) { if e > acc { e } else { acc } }, 0.0)
  assert_true(max_error < 1.0) // 重构误差应该很小
  
  // 计算各分量的方差贡献
  let total_variance = {
    let mean = decomposable_data.reduce(fn(acc, d) { acc + d.value }, 0.0) / n.to_float()
    let mut sum = 0.0
    for d in decomposable_data {
      let diff = d.value - mean
      sum = sum + diff * diff
    }
    sum / n.to_float()
  }
  
  let trend_variance = {
    let mean = extracted_trend.reduce(fn(acc, t) { acc + t }, 0.0) / n.to_float()
    let mut sum = 0.0
    for t in extracted_trend {
      let diff = t - mean
      sum = sum + diff * diff
    }
    sum / n.to_float()
  }
  
  let seasonal_variance = {
    let mean = 0.0
    let mut sum = 0.0
    for i in 0..n {
      let seasonal_value = seasonal_pattern[i % seasonal_period]
      let diff = seasonal_value - mean
      sum = sum + diff * diff
    }
    sum / n.to_float()
  }
  
  let residual_variance = {
    let mean = residuals.reduce(fn(acc, r) { acc + r }, 0.0) / n.to_float()
    let mut sum = 0.0
    for r in residuals {
      let diff = r - mean
      sum = sum + diff * diff
    }
    sum / n.to_float()
  }
  
  // 验证方差分解
  let explained_variance = trend_variance + seasonal_variance
  let explained_ratio = explained_variance / total_variance
  
  assert_true(explained_ratio > 0.8) // 趋势和季节性应该解释大部分方差
  assert_true(residual_variance < total_variance * 0.2) // 残差方差应该较小
}

// 测试5: 时间序列预测
test "时间序列预测" {
  // 模拟历史时间序列数据
  let historical_data = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995260, value: 105.0 },
    { timestamp: 1640995320, value: 110.0 },
    { timestamp: 1640995380, value: 108.0 },
    { timestamp: 1640995440, value: 115.0 },
    { timestamp: 1640995500, value: 120.0 },
    { timestamp: 1640995560, value: 118.0 },
    { timestamp: 1640995620, value: 125.0 },
    { timestamp: 1640995680, value: 130.0 },
    { timestamp: 1640995740, value: 135.0 }
  ]
  
  // 使用简单指数平滑进行预测
  let alpha = 0.3
  let mut ses_predictions = []
  let mut ses = historical_data[0].value
  
  for i in 1..historical_data.length() {
    ses = alpha * historical_data[i].value + (1.0 - alpha) * ses
    ses_predictions = ses_predictions.push(ses)
  }
  
  // 预测未来5个点
  let future_ses_predictions = []
  for i in 0..5 {
    future_ses_predictions = future_ses_predictions.push(ses)
  }
  
  // 使用Holt线性趋势方法进行预测
  let beta = 0.2
  let mut level = historical_data[0].value
  let mut trend = historical_data[1].value - historical_data[0].value
  let mut holt_predictions = []
  
  for i in 1..historical_data.length() {
    let prev_level = level
    level = alpha * historical_data[i].value + (1.0 - alpha) * (prev_level + trend)
    trend = beta * (level - prev_level) + (1.0 - beta) * trend
    holt_predictions = holt_predictions.push(level + trend)
  }
  
  // 预测未来5个点
  let future_holt_predictions = []
  for i in 1..6 {
    future_holt_predictions = future_holt_predictions.push(level + i.to_float() * trend)
  }
  
  // 使用ARIMA模拟进行预测（简化版）
  let arima_predictions = []
  let ar_coefficient = 0.6
  let ma_coefficient = 0.3
  
  for i in 2..historical_data.length() {
    let ar_part = ar_coefficient * historical_data[i-1].value
    let ma_part = ma_coefficient * (historical_data[i-1].value - historical_data[i-2].value)
    let prediction = ar_part + ma_part
    arima_predictions = arima_predictions.push(prediction)
  }
  
  // 预测未来5个点
  let future_arima_predictions = []
  let last_value = historical_data[historical_data.length()-1].value
  let prev_diff = historical_data[historical_data.length()-1].value - historical_data[historical_data.length()-2].value
  
  for i in 0..5 {
    let ar_part = if i == 0 { ar_coefficient * last_value } else { ar_coefficient * future_arima_predictions[i-1] }
    let ma_part = ma_coefficient * prev_diff
    let prediction = ar_part + ma_part
    future_arima_predictions = future_arima_predictions.push(prediction)
  }
  
  // 验证预测结果
  assert_eq(ses_predictions.length(), historical_data.length() - 1)
  assert_eq(holt_predictions.length(), historical_data.length() - 1)
  assert_eq(arima_predictions.length(), historical_data.length() - 2)
  
  assert_eq(future_ses_predictions.length(), 5)
  assert_eq(future_holt_predictions.length(), 5)
  assert_eq(future_arima_predictions.length(), 5)
  
  // 验证预测的合理性
  let last_actual_value = historical_data[historical_data.length()-1].value
  
  // 简单指数平滑预测应该接近最后一个实际值
  for prediction in future_ses_predictions {
    assert_true(prediction > last_actual_value * 0.8)
    assert_true(prediction < last_actual_value * 1.2)
  }
  
  // Holt线性趋势预测应该显示趋势
  for i in 1..future_holt_predictions.length() {
    assert_true(future_holt_predictions[i] >= future_holt_predictions[i-1] - 1.0)
  }
  
  // ARIMA预测应该显示自相关特性
  for i in 1..future_arima_predictions.length() {
    assert_true(future_arima_predictions[i] > 0)
  }
  
  // 计算预测准确性（使用历史数据）
  let ses_errors = []
  let holt_errors = []
  
  for i in 0..ses_predictions.length() {
    let actual = historical_data[i+1].value
    let predicted = ses_predictions[i]
    ses_errors = ses_errors.push((actual - predicted).abs())
  }
  
  for i in 0..holt_predictions.length() {
    let actual = historical_data[i+1].value
    let predicted = holt_predictions[i]
    holt_errors = holt_errors.push((actual - predicted).abs())
  }
  
  let ses_mae = ses_errors.reduce(fn(acc, e) { acc + e }, 0.0) / ses_errors.length().to_float()
  let holt_mae = holt_errors.reduce(fn(acc, e) { acc + e }, 0.0) / holt_errors.length().to_float()
  
  // 验证预测准确性
  assert_true(ses_mae < 20.0) // 简单指数平滑的平均绝对误差应该小于20
  assert_true(holt_mae < 15.0) // Holt方法的平均绝对误差应该小于15
  
  // Holt方法应该比简单指数平滑更准确（对于有趋势的数据）
  assert_true(holt_mae <= ses_mae)
}

// 测试6: 多元时间序列分析
test "多元时间序列分析" {
  // 模拟多个相关的时间序列
  let multivariate_data = [
    {
      timestamp: 1640995200,
      cpu_usage: 45.0,
      memory_usage: 60.0,
      disk_io: 100.0,
      network_io: 80.0
    },
    {
      timestamp: 1640995260,
      cpu_usage: 50.0,
      memory_usage: 65.0,
      disk_io: 120.0,
      network_io: 90.0
    },
    {
      timestamp: 1640995320,
      cpu_usage: 55.0,
      memory_usage: 70.0,
      disk_io: 140.0,
      network_io: 100.0
    },
    {
      timestamp: 1640995380,
      cpu_usage: 60.0,
      memory_usage: 75.0,
      disk_io: 160.0,
      network_io: 110.0
    },
    {
      timestamp: 1640995440,
      cpu_usage: 58.0,
      memory_usage: 72.0,
      disk_io: 150.0,
      network_io: 105.0
    },
    {
      timestamp: 1640995500,
      cpu_usage: 52.0,
      memory_usage: 68.0,
      disk_io: 130.0,
      network_io: 95.0
    },
    {
      timestamp: 1640995560,
      cpu_usage: 48.0,
      memory_usage: 64.0,
      disk_io: 110.0,
      network_io: 85.0
    },
    {
      timestamp: 1640995620,
      cpu_usage: 65.0,
      memory_usage: 80.0,
      disk_io: 180.0,
      network_io: 120.0
    }
  ]
  
  // 计算各变量的基本统计
  let variables = ["cpu_usage", "memory_usage", "disk_io", "network_io"]
  let mut variable_stats = []
  
  for var in variables {
    let mut values = []
    for data_point in multivariate_data {
      match var {
        "cpu_usage" => values = values.push(data_point.cpu_usage)
        "memory_usage" => values = values.push(data_point.memory_usage)
        "disk_io" => values = values.push(data_point.disk_io)
        "network_io" => values = values.push(data_point.network_io)
        _ => ()
      }
    }
    
    let n = values.length()
    let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / n.to_float()
    
    let mut variance = 0.0
    for v in values {
      let diff = v - mean
      variance = variance + diff * diff
    }
    let std_dev = (variance / n.to_float()).sqrt()
    
    variable_stats = variable_stats.push({
      variable: var,
      mean: mean,
      std_dev: std_dev,
      min: values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0]),
      max: values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    })
  }
  
  // 计算变量间的相关系数
  let mut correlations = []
  
  for i in 0..variables.length() {
    for j in i..variables.length() {
      if i != j {
        let var1 = variables[i]
        let var2 = variables[j]
        
        let mut values1 = []
        let mut values2 = []
        
        for data_point in multivariate_data {
          match var1 {
            "cpu_usage" => values1 = values1.push(data_point.cpu_usage)
            "memory_usage" => values1 = values1.push(data_point.memory_usage)
            "disk_io" => values1 = values1.push(data_point.disk_io)
            "network_io" => values1 = values1.push(data_point.network_io)
            _ => ()
          }
          
          match var2 {
            "cpu_usage" => values2 = values2.push(data_point.cpu_usage)
            "memory_usage" => values2 = values2.push(data_point.memory_usage)
            "disk_io" => values2 = values2.push(data_point.disk_io)
            "network_io" => values2 = values2.push(data_point.network_io)
            _ => ()
          }
        }
        
        let n = values1.length()
        let mean1 = values1.reduce(fn(acc, v) { acc + v }, 0.0) / n.to_float()
        let mean2 = values2.reduce(fn(acc, v) { acc + v }, 0.0) / n.to_float()
        
        let mut numerator = 0.0
        let mut variance1 = 0.0
        let mut variance2 = 0.0
        
        for k in 0..n {
          let diff1 = values1[k] - mean1
          let diff2 = values2[k] - mean2
          numerator = numerator + diff1 * diff2
          variance1 = variance1 + diff1 * diff1
          variance2 = variance2 + diff2 * diff2
        }
        
        let correlation = numerator / (variance1.sqrt() * variance2.sqrt())
        
        correlations = correlations.push({
          variable1: var1,
          variable2: var2,
          correlation: correlation,
          strength: if correlation.abs() > 0.7 { "strong" } else if correlation.abs() > 0.3 { "moderate" } else { "weak" }
        })
      }
    }
  }
  
  // 验证基本统计
  assert_eq(variable_stats.length(), 4)
  
  // 验证相关系数计算
  assert_eq(correlations.length(), 6) // 4个变量的组合数 C(4,2)
  
  // 验证强相关性
  let strong_correlations = correlations.filter_fn(c => c.strength == "strong")
  assert_true(strong_correlations.length() >= 2) // 至少有两对强相关变量
  
  // 验证CPU和内存使用率的相关性
  let cpu_memory_corr = correlations.filter_fn(c => 
    (c.variable1 == "cpu_usage" && c.variable2 == "memory_usage") ||
    (c.variable1 == "memory_usage" && c.variable2 == "cpu_usage")
  )[0]
  
  assert_true(cpu_memory_corr.correlation > 0.7) // CPU和内存应该强相关
  assert_eq(cpu_memory_corr.strength, "strong")
  
  // 验证磁盘IO和网络IO的相关性
  let disk_network_corr = correlations.filter_fn(c => 
    (c.variable1 == "disk_io" && c.variable2 == "network_io") ||
    (c.variable1 == "network_io" && c.variable2 == "disk_io")
  )[0]
  
  assert_true(disk_network_corr.correlation > 0.7) // 磁盘IO和网络IO应该强相关
  assert_eq(disk_network_corr.strength, "strong")
  
  // 执行主成分分析（简化版）
  let n = multivariate_data.length()
  let mut data_matrix = []
  
  for data_point in multivariate_data {
    let row = [
      data_point.cpu_usage,
      data_point.memory_usage,
      data_point.disk_io,
      data_point.network_io
    ]
    data_matrix = data_matrix.push(row)
  }
  
  // 标准化数据
  let mut standardized_matrix = []
  for i in 0..n {
    let mut standardized_row = []
    for j in 0..variables.length() {
      let var = variables[j]
      let stats = variable_stats.filter_fn(s => s.variable == var)[0]
      let original_value = data_matrix[i][j]
      let standardized_value = (original_value - stats.mean) / stats.std_dev
      standardized_row = standardized_row.push(standardized_value)
    }
    standardized_matrix = standardized_matrix.push(standardized_row)
  }
  
  // 计算协方差矩阵
  let mut covariance_matrix = []
  for i in 0..variables.length() {
    let mut cov_row = []
    for j in 0..variables.length() {
      let mut cov = 0.0
      for k in 0..n {
        cov = cov + standardized_matrix[k][i] * standardized_matrix[k][j]
      }
      cov = cov / (n - 1).to_float()
      cov_row = cov_row.push(cov)
    }
    covariance_matrix = covariance_matrix.push(cov_row)
  }
  
  // 验证协方差矩阵
  assert_eq(covariance_matrix.length(), 4)
  assert_eq(covariance_matrix[0].length(), 4)
  
  // 协方差矩阵应该是对称的
  for i in 0..variables.length() {
    for j in 0..variables.length() {
      assert_eq(covariance_matrix[i][j], covariance_matrix[j][i])
    }
  }
  
  // 对角线元素应该是1（标准化数据的方差）
  for i in 0..variables.length() {
    assert_true((covariance_matrix[i][i] - 1.0).abs() < 0.01)
  }
  
  // 验证多元时间序列的预测能力
  let future_predictions = []
  let prediction_horizon = 3
  
  // 使用简单的线性回归进行多元预测
  for h in 1..prediction_horizon+1 {
    let predicted_cpu = variable_stats.filter_fn(s => s.variable == "cpu_usage")[0].mean + h.to_float() * 2.0
    let predicted_memory = variable_stats.filter_fn(s => s.variable == "memory_usage")[0].mean + h.to_float() * 2.5
    let predicted_disk = variable_stats.filter_fn(s => s.variable == "disk_io")[0].mean + h.to_float() * 10.0
    let predicted_network = variable_stats.filter_fn(s => s.variable == "network_io")[0].mean + h.to_float() * 5.0
    
    future_predictions = future_predictions.push({
      horizon: h,
      cpu_usage: predicted_cpu,
      memory_usage: predicted_memory,
      disk_io: predicted_disk,
      network_io: predicted_network
    })
  }
  
  // 验证预测的合理性
  assert_eq(future_predictions.length(), prediction_horizon)
  
  for prediction in future_predictions {
    // 预测值应该在合理范围内
    assert_true(prediction.cpu_usage > 0 && prediction.cpu_usage < 100)
    assert_true(prediction.memory_usage > 0 && prediction.memory_usage < 100)
    assert_true(prediction.disk_io > 0)
    assert_true(prediction.network_io > 0)
    
    // 预测值应该保持相关性
    let cpu_memory_ratio = prediction.memory_usage / prediction.cpu_usage
    assert_true(cpu_memory_ratio > 1.2 && cpu_memory_ratio < 1.5)
  }
}