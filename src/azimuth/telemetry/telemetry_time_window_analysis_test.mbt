// 遥测时间窗口分析测试用例
// 测试时间窗口内的数据聚合、趋势分析和异常检测功能

test "telemetry_time_window_aggregation" {
  // 测试时间窗口数据聚合功能
  
  let window_size_seconds = 60L
  let current_timestamp = 1640995200L
  let metric_values = [10.5, 15.2, 12.8, 18.9, 22.1, 19.7, 25.3, 30.0]
  let metric_timestamps = [
    current_timestamp,
    current_timestamp + 10L,
    current_timestamp + 20L,
    current_timestamp + 30L,
    current_timestamp + 40L,
    current_timestamp + 50L,
    current_timestamp + 55L,
    current_timestamp + 58L
  ]
  
  // 验证时间窗口内数据点数量
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_timestamps.length(), 8)
  
  // 验证所有时间戳都在窗口内
  let mut i = 0
  let mut all_in_window = true
  while i < metric_timestamps.length() {
    let time_diff = metric_timestamps[i] - current_timestamp
    if time_diff > window_size_seconds {
      all_in_window = false
    }
    i = i + 1
  }
  assert_eq(all_in_window, true)
  
  // 计算平均值
  let mut sum = 0.0
  i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let average = sum / metric_values.length().to_double()
  assert_eq(average > 15.0, true)
  assert_eq(average < 25.0, true)
  
  // 计算最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  assert_eq(max_value, 30.0)
  assert_eq(min_value, 10.5)
  
  // 计算趋势（简单线性回归斜率）
  let mut numerator = 0.0
  let mut denominator = 0.0
  let mean_x = metric_timestamps.length().to_double() / 2.0
  i = 0
  while i < metric_timestamps.length() {
    let x = i.to_double()
    let y = metric_values[i]
    numerator = numerator + (x - mean_x) * (y - average)
    denominator = denominator + (x - mean_x) * (x - mean_x)
    i = i + 1
  }
  let slope = numerator / denominator
  assert_eq(slope > 0.0, true) // 上升趋势
  assert_eq(slope < 5.0, true)  // 但不是过于陡峭
}

test "telemetry_time_window_anomaly_detection" {
  // 测试时间窗口异常检测功能
  
  let baseline_values = [20.0, 21.0, 19.5, 20.5, 20.8, 19.8, 20.2, 20.0]
  let anomaly_values = [20.0, 21.0, 19.5, 75.0, 20.8, 19.8, 20.2, 20.0] // 75.0是异常值
  
  // 计算基准统计
  let mut baseline_sum = 0.0
  let mut i = 0
  while i < baseline_values.length() {
    baseline_sum = baseline_sum + baseline_values[i]
    i = i + 1
  }
  let baseline_mean = baseline_sum / baseline_values.length().to_double()
  
  // 计算基准标准差
  let mut variance_sum = 0.0
  i = 0
  while i < baseline_values.length() {
    let diff = baseline_values[i] - baseline_mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let baseline_std = (variance_sum / baseline_values.length().to_double()).sqrt()
  
  // 验证基准统计
  assert_eq(baseline_mean > 19.0, true)
  assert_eq(baseline_mean < 21.0, true)
  assert_eq(baseline_std > 0.0, true)
  assert_eq(baseline_std < 2.0, true)
  
  // 检测异常值（3-sigma规则）
  let threshold = 3.0 * baseline_std
  let mut anomaly_count = 0
  i = 0
  while i < anomaly_values.length() {
    let diff = (anomaly_values[i] - baseline_mean).abs()
    if diff > threshold {
      anomaly_count = anomaly_count + 1
    }
    i = i + 1
  }
  
  // 应该检测到1个异常值
  assert_eq(anomaly_count, 1)
  
  // 验证异常值位置
  i = 0
  let mut found_anomaly = false
  while i < anomaly_values.length() {
    let diff = (anomaly_values[i] - baseline_mean).abs()
    if diff > threshold && i == 3 {
      found_anomaly = true
    }
    i = i + 1
  }
  assert_eq(found_anomaly, true)
}

test "telemetry_time_window_sliding_analysis" {
  // 测试滑动时间窗口分析
  
  let data_points = [
    (1640995200L, 10.0), // t=0
    (1640995210L, 15.0), // t=10
    (1640995220L, 12.0), // t=20
    (1640995230L, 18.0), // t=30
    (1640995240L, 25.0), // t=40
    (1640995250L, 22.0), // t=50
    (1640995260L, 28.0), // t=60
    (1640995270L, 30.0), // t=70
  ]
  
  let window_size = 30L // 30秒窗口
  
  // 第一个窗口 (t=0 到 t=30)
  let window1_data = [10.0, 15.0, 12.0, 18.0]
  let mut window1_sum = 0.0
  let mut i = 0
  while i < window1_data.length() {
    window1_sum = window1_sum + window1_data[i]
    i = i + 1
  }
  let window1_avg = window1_sum / window1_data.length().to_double()
  
  // 第二个窗口 (t=10 到 t=40)
  let window2_data = [15.0, 12.0, 18.0, 25.0]
  let mut window2_sum = 0.0
  i = 0
  while i < window2_data.length() {
    window2_sum = window2_sum + window2_data[i]
    i = i + 1
  }
  let window2_avg = window2_sum / window2_data.length().to_double()
  
  // 第三个窗口 (t=20 到 t=50)
  let window3_data = [12.0, 18.0, 25.0, 22.0]
  let mut window3_sum = 0.0
  i = 0
  while i < window3_data.length() {
    window3_sum = window3_sum + window3_data[i]
    i = i + 1
  }
  let window3_avg = window3_sum / window3_data.length().to_double()
  
  // 验证窗口平均值趋势
  assert_eq(window1_avg < window2_avg, true) // 上升趋势
  assert_eq(window2_avg < window3_avg, true) // 继续上升
  
  // 验证具体数值
  assert_eq(window1_avg > 10.0, true)
  assert_eq(window1_avg < 20.0, true)
  assert_eq(window2_avg > 15.0, true)
  assert_eq(window2_avg < 20.0, true)
  assert_eq(window3_avg > 15.0, true)
  assert_eq(window3_avg < 25.0, true)
}

test "telemetry_time_window_percentile_analysis" {
  // 测试时间窗口百分位数分析
  
  let response_times = [
    10.0, 15.0, 20.0, 25.0, 30.0,
    35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0,
    85.0, 90.0, 95.0, 100.0, 500.0 // 异常高值
  ]
  
  // 排序数据（已经排好序）
  let sorted_times = response_times
  
  // 计算百分位数
  let p50_index = sorted_times.length() / 2
  let p90_index = (sorted_times.length() * 9) / 10
  let p95_index = (sorted_times.length() * 95) / 100
  let p99_index = (sorted_times.length() * 99) / 100
  
  let p50 = sorted_times[p50_index]
  let p90 = sorted_times[p90_index]
  let p95 = sorted_times[p95_index]
  let p99 = sorted_times[p99_index]
  
  // 验证百分位数关系
  assert_eq(p50 < p90, true)
  assert_eq(p90 < p95, true)
  assert_eq(p95 < p99, true)
  
  // 验证具体值
  assert_eq(p50 >= 50.0, true)  // 中位数应该在50左右
  assert_eq(p50 <= 60.0, true)
  assert_eq(p90 >= 85.0, true)  // 90%应该在85以上
  assert_eq(p90 <= 95.0, true)
  assert_eq(p95 >= 90.0, true)  // 95%应该在90以上
  assert_eq(p95 <= 100.0, true)
  assert_eq(p99 >= 95.0, true)  // 99%应该包括异常值
  assert_eq(p99 <= 500.0, true)
  
  // 计算异常值影响（去除最大值后的平均值）
  let mut normal_sum = 0.0
  let mut i = 0
  while i < sorted_times.length() - 1 {
    normal_sum = normal_sum + sorted_times[i]
    i = i + 1
  }
  let normal_avg = normal_sum / (sorted_times.length() - 1).to_double()
  
  let total_sum = normal_sum + sorted_times[sorted_times.length() - 1]
  let total_avg = total_sum / sorted_times.length().to_double()
  
  // 验证异常值影响
  assert_eq(total_avg > normal_avg, true)
  assert_eq(total_avg - normal_avg > 10.0, true) // 异常值显著影响平均值
}

test "telemetry_time_window_rate_calculation" {
  // 测试时间窗口速率计算
  
  let event_counts = [
    (1640995200L, 100), // t=0, 100 events
    (1640995210L, 150), // t=10, 150 events
    (1640995220L, 120), // t=20, 120 events
    (1640995230L, 180), // t=30, 180 events
    (1640995240L, 200), // t=40, 200 events
  ]
  
  let window_size = 30L // 30秒窗口
  
  // 计算第一个窗口的速率 (t=0 到 t=30)
  let window1_events = 100 + 150 + 120
  let window1_rate = window1_events.to_double() / window_size.to_double()
  
  // 计算第二个窗口的速率 (t=10 到 t=40)
  let window2_events = 150 + 120 + 180
  let window2_rate = window2_events.to_double() / window_size.to_double()
  
  // 计算第三个窗口的速率 (t=20 到 t=50，假设t=50有220个事件)
  let window3_events = 120 + 180 + 200
  let window3_rate = window3_events.to_double() / window_size.to_double()
  
  // 验证速率计算
  assert_eq(window1_rate > 10.0, true)  // 至少10 events/sec
  assert_eq(window1_rate < 20.0, true)  // 但不超过20 events/sec
  assert_eq(window2_rate > window1_rate, true) // 速率上升
  assert_eq(window3_rate > window2_rate, true) // 继续上升
  
  // 计算速率变化百分比
  let rate_change_1_to_2 = ((window2_rate - window1_rate) / window1_rate) * 100.0
  let rate_change_2_to_3 = ((window3_rate - window2_rate) / window2_rate) * 100.0
  
  // 验证速率变化
  assert_eq(rate_change_1_to_2 > 0.0, true)  // 正增长
  assert_eq(rate_change_2_to_3 > 0.0, true)  // 继续正增长
  assert_eq(rate_change_1_to_2 < 50.0, true)  // 但增长不过快
  assert_eq(rate_change_2_to_3 < 50.0, true)  // 但增长不过快
  
  // 计算累计平均值
  let total_events = window1_events + window2_events + window3_events
  let total_time = window_size.to_double() * 3.0
  let overall_rate = total_events.to_double() / total_time
  
  // 验证整体速率
  assert_eq(overall_rate > window1_rate, true)
  assert_eq(overall_rate < window3_rate, true)
}

test "telemetry_time_window_burst_detection" {
  // 测试时间窗口突发检测
  
  let request_counts = [
    (1640995200L, 50),   // 正常
    (1640995210L, 45),   // 正常
    (1640995220L, 55),   // 正常
    (1640995230L, 200),  // 突发！
    (1640995240L, 180),  // 突发持续
    (1640995250L, 60),   // 恢复正常
    (1640995260L, 52),   // 正常
    (1640995270L, 48),   // 正常
  ]
  
  // 计算基准平均值（排除明显的突发值）
  let baseline_values = [50, 45, 55, 60, 52, 48]
  let mut baseline_sum = 0
  let mut i = 0
  while i < baseline_values.length() {
    baseline_sum = baseline_sum + baseline_values[i]
    i = i + 1
  }
  let baseline_avg = baseline_sum / baseline_values.length()
  
  // 计算基准标准差
  let mut variance_sum = 0
  i = 0
  while i < baseline_values.length() {
    let diff = baseline_values[i] - baseline_avg
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let baseline_std = (variance_sum / baseline_values.length()).to_double().sqrt()
  
  // 设置突发阈值（平均值 + 2倍标准差）
  let burst_threshold = baseline_avg.to_double() + (2.0 * baseline_std)
  
  // 检测突发
  let mut burst_count = 0
  let mut burst_indices = []
  i = 0
  while i < request_counts.length() {
    let count = request_counts[i].1.to_double()
    if count > burst_threshold {
      burst_count = burst_count + 1
      burst_indices.push(i)
    }
    i = i + 1
  }
  
  // 验证突发检测结果
  assert_eq(burst_count, 2)  // 应该检测到2个突发点
  assert_eq(burst_indices.length(), 2)
  assert_eq(burst_indices[0], 3)  // 第4个点
  assert_eq(burst_indices[1], 4)  // 第5个点
  
  // 验证突发值
  assert_eq(request_counts[3].1, 200)
  assert_eq(request_counts[4].1, 180)
  assert_eq(request_counts[3].1.to_double() > burst_threshold, true)
  assert_eq(request_counts[4].1.to_double() > burst_threshold, true)
  
  // 验证非突发值
  assert_eq(request_counts[0].1.to_double() < burst_threshold, true)
  assert_eq(request_counts[6].1.to_double() < burst_threshold, true)
  assert_eq(request_counts[7].1.to_double() < burst_threshold, true)
}

test "telemetry_time_window_correlation_analysis" {
  // 测试时间窗口相关性分析
  
  let cpu_usage = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0]
  let memory_usage = [20.0, 22.0, 25.0, 28.0, 32.0, 36.0, 41.0, 46.0]
  let response_time = [100.0, 105.0, 115.0, 130.0, 150.0, 175.0, 205.0, 240.0]
  
  // 计算CPU和内存的相关系数
  let cpu_mean = cpu_usage.reduce(fn(acc, x) { acc + x }, 0.0) / cpu_usage.length().to_double()
  let mem_mean = memory_usage.reduce(fn(acc, x) { acc + x }, 0.0) / memory_usage.length().to_double()
  
  let mut cpu_mem_covariance = 0.0
  let mut cpu_variance = 0.0
  let mut mem_variance = 0.0
  let mut i = 0
  while i < cpu_usage.length() {
    let cpu_diff = cpu_usage[i] - cpu_mean
    let mem_diff = memory_usage[i] - mem_mean
    cpu_mem_covariance = cpu_mem_covariance + cpu_diff * mem_diff
    cpu_variance = cpu_variance + cpu_diff * cpu_diff
    mem_variance = mem_variance + mem_diff * mem_diff
    i = i + 1
  }
  
  let cpu_mem_correlation = cpu_mem_covariance / (cpu_variance.sqrt() * mem_variance.sqrt())
  
  // 计算CPU和响应时间的相关系数
  let resp_mean = response_time.reduce(fn(acc, x) { acc + x }, 0.0) / response_time.length().to_double()
  
  let mut cpu_resp_covariance = 0.0
  let mut resp_variance = 0.0
  i = 0
  while i < cpu_usage.length() {
    let cpu_diff = cpu_usage[i] - cpu_mean
    let resp_diff = response_time[i] - resp_mean
    cpu_resp_covariance = cpu_resp_covariance + cpu_diff * resp_diff
    resp_variance = resp_variance + resp_diff * resp_diff
    i = i + 1
  }
  
  let cpu_resp_correlation = cpu_resp_covariance / (cpu_variance.sqrt() * resp_variance.sqrt())
  
  // 验证相关性
  assert_eq(cpu_mem_correlation > 0.9, true)  // CPU和内存高度正相关
  assert_eq(cpu_mem_correlation <= 1.0, true)  // 相关系数不超过1
  assert_eq(cpu_resp_correlation > 0.8, true)  // CPU和响应时间也高度正相关
  assert_eq(cpu_resp_correlation <= 1.0, true)  // 相关系数不超过1
  
  // 验证响应时间对CPU的敏感性更高
  assert_eq(cpu_resp_correlation > cpu_mem_correlation, true)
  
  // 计算预测值（简单线性回归）
  let cpu_resp_slope = cpu_resp_covariance / cpu_variance
  let cpu_resp_intercept = resp_mean - cpu_resp_slope * cpu_mean
  
  // 预测CPU=50时的响应时间
  let predicted_resp_time = cpu_resp_slope * 50.0 + cpu_resp_intercept
  
  // 验证预测合理性
  assert_eq(predicted_resp_time > 200.0, true)  // 应该高于当前最大值
  assert_eq(predicted_resp_time < 500.0, true)  // 但不应该过高
}

test "telemetry_time_window_seasonality_detection" {
  // 测试时间窗口季节性检测
  
  // 模拟一天内的流量模式（24小时，每小时一个数据点）
  let hourly_traffic = [
    20, 15, 10, 8, 12, 25, 45, 80,  // 0-7点（夜间到早晨）
    120, 150, 180, 200, 190, 170,   // 8-13点（上午到中午）
    160, 140, 130, 125, 135, 145,   // 14-19点（下午到傍晚）
    110, 85, 60, 40, 30, 25         // 20-23点（晚上到深夜）
  ]
  
  // 计算日平均值
  let daily_total = hourly_traffic.reduce(fn(acc, x) { acc + x }, 0)
  let daily_avg = daily_total / hourly_traffic.length()
  
  // 验证基本统计
  assert_eq(daily_avg > 80, true)   // 日平均应该在合理范围
  assert_eq(daily_avg < 150, true)
  
  // 检测高峰时段（应该在工作时间）
  let mut peak_hours = []
  let mut i = 0
  while i < hourly_traffic.length() {
    if hourly_traffic[i] > daily_avg * 3 / 2 {  // 超过平均值1.5倍
      peak_hours.push(i)
    }
    i = i + 1
  }
  
  // 验证高峰时段
  assert_eq(peak_hours.length() > 0, true)  // 应该有高峰时段
  assert_eq(peak_hours.length() < 8, true)   // 但不应该太多
  
  // 检测低峰时段
  let mut low_hours = []
  i = 0
  while i < hourly_traffic.length() {
    if hourly_traffic[i] < daily_avg / 2 {   // 低于平均值一半
      low_hours.push(i)
    }
    i = i + 1
  }
  
  // 验证低峰时段
  assert_eq(low_hours.length() > 0, true)   // 应该有低峰时段
  assert_eq(low_hours.length() < 8, true)    // 但不应该太多
  
  // 验证时间模式（凌晨应该是低峰）
  let mut has_early_morning_low = false
  i = 0
  while i < low_hours.length() {
    if low_hours[i] >= 0 && low_hours[i] <= 6 {
      has_early_morning_low = true
    }
    i = i + 1
  }
  assert_eq(has_early_morning_low, true)
  
  // 验证时间模式（上午应该是高峰）
  let mut has_morning_peak = false
  i = 0
  while i < peak_hours.length() {
    if peak_hours[i] >= 8 && peak_hours[i] <= 12 {
      has_morning_peak = true
    }
    i = i + 1
  }
  assert_eq(has_morning_peak, true)
  
  // 计算波动性（标准差）
  let mut variance_sum = 0
  i = 0
  while i < hourly_traffic.length() {
    let diff = hourly_traffic[i] - daily_avg
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let std_dev = (variance_sum / hourly_traffic.length()).to_double().sqrt()
  
  // 验证波动性
  assert_eq(std_dev > 30.0, true)   // 应该有明显波动
  assert_eq(std_dev < 100.0, true)  // 但波动不应该过大
  
  // 计算变异系数
  let coefficient_of_variation = std_dev / daily_avg.to_double()
  assert_eq(coefficient_of_variation > 0.3, true)  // 应该有明显季节性
  assert_eq(coefficient_of_variation < 1.0, true)  // 但不应该过于极端
}