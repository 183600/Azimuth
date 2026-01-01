// Advanced Time Series Operations Tests
// 高级时序操作测试

test "时间戳精度和单调性测试" {
  // 测试时间戳的精度和单调性
  let clock = Clock::system()
  
  // 生成多个时间戳
  let timestamps = []
  for i in 0..100 {
    let timestamp = Clock::now_unix_nanos(clock)
    timestamps.push(timestamp)
  }
  
  // 验证时间戳的精度（纳秒级）
  for timestamp in timestamps {
    assert_true(timestamp > 0L)
    assert_true(timestamp.toString().length() >= 16)  // 至少16位数字
  }
  
  // 验证时间戳的单调递增性
  for i in 1..timestamps.length() {
    assert_true(timestamps[i] >= timestamps[i-1])
  }
  
  // 测试时间差计算
  let time_diffs = []
  for i in 1..timestamps.length() {
    let diff = timestamps[i] - timestamps[i-1]
    time_diffs.push(diff)
  }
  
  // 验证时间差为非负数
  for diff in time_diffs {
    assert_true(diff >= 0L)
  }
  
  // 计算平均时间差
  let total_diff = 0L
  for diff in time_diffs {
    total_diff = total_diff + diff
  }
  let avg_diff = total_diff / time_diffs.length().to_long()
  
  // 验证平均时间差在合理范围内（纳秒级）
  assert_true(avg_diff >= 0L)
  assert_true(avg_diff < 1000000000L)  // 小于1秒
  
  assert_true(true)
}

test "时间窗口聚合测试" {
  // 测试时间窗口聚合操作
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 创建时间序列数据点
  let data_points = []
  for i in 0..100 {
    let timestamp = base_time + (i * 1000000L)  // 每个点间隔1毫秒
    let value = i.to_double() * 1.5  // 递增值
    data_points.push((timestamp, value))
  }
  
  // 定义时间窗口（10毫秒）
  let window_size = 10000000L  // 10毫秒（纳秒）
  
  // 按时间窗口聚合数据
  let windowed_data = []
  let current_window = []
  let current_window_start = data_points[0].0
  
  for (timestamp, value) in data_points {
    if timestamp < current_window_start + window_size {
      current_window.push((timestamp, value))
    } else {
      if current_window.length() > 0 {
        windowed_data.push(current_window)
        current_window = []
        current_window_start = timestamp
      }
      current_window.push((timestamp, value))
    }
  }
  
  // 添加最后一个窗口
  if current_window.length() > 0 {
    windowed_data.push(current_window)
  }
  
  // 验证时间窗口聚合结果
  assert_true(windowed_data.length() > 0)
  
  // 计算每个窗口的统计信息
  for window in windowed_data {
    // 计算窗口内的平均值
    let sum = 0.0
    for (_, value) in window {
      sum = sum + value
    }
    let avg = sum / window.length().to_double()
    
    // 验证平均值在合理范围内
    assert_true(avg >= 0.0)
    
    // 计算窗口内的最大值和最小值
    let max_value = window[0].1
    let min_value = window[0].1
    
    for (_, value) in window {
      if value > max_value {
        max_value = value
      }
      if value < min_value {
        min_value = value
      }
    }
    
    // 验证最大值和最小值的关系
    assert_true(max_value >= min_value)
    assert_true(avg >= min_value)
    assert_true(avg <= max_value)
  }
  
  assert_true(true)
}

test "时序数据采样测试" {
  // 测试时序数据的采样操作
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 创建高频率时间序列数据（1000个点，每0.1毫秒一个点）
  let high_freq_data = []
  for i in 0..1000 {
    let timestamp = base_time + (i * 100000L)  // 每0.1毫秒
    let value = 100.0 + (i.to_double() * 0.1) + (Float::sin(i.to_double() * 0.1) * 10.0)
    high_freq_data.push((timestamp, value))
  }
  
  // 定义采样率（每10个点采样1个）
  let sampling_rate = 10
  
  // 执行采样
  let sampled_data = []
  for i in 0..high_freq_data.length() {
    if i % sampling_rate == 0 {
      sampled_data.push(high_freq_data[i])
    }
  }
  
  // 验证采样结果
  assert_true(sampled_data.length() == high_freq_data.length() / sampling_rate)
  
  // 验证采样数据的时间戳递增
  for i in 1..sampled_data.length() {
    assert_true(sampled_data[i].0 > sampled_data[i-1].0)
  }
  
  // 验证采样数据的值在原始数据范围内
  let original_values = []
  for (_, value) in high_freq_data {
    original_values.push(value)
  }
  
  let original_min = original_values.reduce(original_values[0], fn(acc, val) { if val < acc { val } else { acc } })
  let original_max = original_values.reduce(original_values[0], fn(acc, val) { if val > acc { val } else { acc } })
  
  for (_, value) in sampled_data {
    assert_true(value >= original_min)
    assert_true(value <= original_max)
  }
  
  // 测试不同的采样率
  let sampling_rates = [2, 5, 20, 50, 100]
  
  for rate in sampling_rates {
    let rate_sampled_data = []
    for i in 0..high_freq_data.length() {
      if i % rate == 0 {
        rate_sampled_data.push(high_freq_data[i])
      }
    }
    
    // 验证不同采样率的结果
    assert_true(rate_sampled_data.length() == high_freq_data.length() / rate)
  }
  
  assert_true(true)
}

test "时序数据插值测试" {
  // 测试时序数据的插值操作
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 创建稀疏时间序列数据
  let sparse_data = [
    (base_time, 10.0),
    (base_time + 10000000L, 20.0),  // 10毫秒后
    (base_time + 20000000L, 15.0),  // 20毫秒后
    (base_time + 30000000L, 25.0),  // 30毫秒后
    (base_time + 40000000L, 30.0)   // 40毫秒后
  ]
  
  // 定义插值目标时间点
  let interpolation_targets = [
    base_time + 5000000L,   // 5毫秒
    base_time + 15000000L,  // 15毫秒
    base_time + 25000000L,  // 25毫秒
    base_time + 35000000L   // 35毫秒
  ]
  
  // 执行线性插值
  let interpolated_values = []
  for target_time in interpolation_targets {
    // 找到插值区间
    let lower_index = 0
    let upper_index = 1
    
    for i in 0..sparse_data.length() - 1 {
      if sparse_data[i].0 <= target_time && target_time <= sparse_data[i+1].0 {
        lower_index = i
        upper_index = i + 1
        break
      }
    }
    
    // 计算插值
    let (t1, v1) = sparse_data[lower_index]
    let (t2, v2) = sparse_data[upper_index]
    
    let ratio = (target_time - t1).to_double() / (t2 - t1).to_double()
    let interpolated_value = v1 + (v2 - v1) * ratio
    
    interpolated_values.push(interpolated_value)
  }
  
  // 验证插值结果
  assert_true(interpolated_values.length() == interpolation_targets.length())
  
  // 验证插值值在合理范围内
  // 5毫秒处的插值应该在10.0和20.0之间
  assert_true(interpolated_values[0] >= 10.0 && interpolated_values[0] <= 20.0)
  
  // 15毫秒处的插值应该在20.0和15.0之间
  assert_true(interpolated_values[1] >= 15.0 && interpolated_values[1] <= 20.0)
  
  // 25毫秒处的插值应该在15.0和25.0之间
  assert_true(interpolated_values[2] >= 15.0 && interpolated_values[2] <= 25.0)
  
  // 35毫秒处的插值应该在25.0和30.0之间
  assert_true(interpolated_values[3] >= 25.0 && interpolated_values[3] <= 30.0)
  
  assert_true(true)
}

test "时序数据平滑测试" {
  // 测试时序数据的平滑操作
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 创建带噪声的时间序列数据
  let noisy_data = []
  for i in 0..100 {
    let timestamp = base_time + (i * 1000000L)  // 每毫秒一个点
    let trend = i.to_double() * 0.5  // 趋势：线性增长
    let noise = (Float::random() - 0.5) * 10.0  // 噪声：±5范围内的随机值
    let value = trend + noise
    noisy_data.push((timestamp, value))
  }
  
  // 定义移动平均窗口大小
  let window_size = 5
  
  // 执行移动平均平滑
  let smoothed_data = []
  for i in 0..noisy_data.length() {
    if i < window_size - 1 {
      // 边界条件：使用前面所有点的平均值
      let sum = 0.0
      for j in 0..(i + 1) {
        sum = sum + noisy_data[j].1
      }
      let avg = sum / (i + 1).to_double()
      smoothed_data.push((noisy_data[i].0, avg))
    } else {
      // 使用窗口内的平均值
      let sum = 0.0
      for j in (i - window_size + 1)..(i + 1) {
        sum = sum + noisy_data[j].1
      }
      let avg = sum / window_size.to_double()
      smoothed_data.push((noisy_data[i].0, avg))
    }
  }
  
  // 验证平滑结果
  assert_true(smoothed_data.length() == noisy_data.length())
  
  // 计算原始数据和平滑数据的方差
  let original_variance = calculate_variance(noisy_data.map(fn(point) { point.1 }))
  let smoothed_variance = calculate_variance(smoothed_data.map(fn(point) { point.1 }))
  
  // 验证平滑后方差减小（数据更平滑）
  assert_true(smoothed_variance <= original_variance)
  
  // 验证平滑数据保持了整体趋势
  let original_first_half_avg = calculate_average(noisy_data.slice(0, 50).map(fn(point) { point.1 }))
  let original_second_half_avg = calculate_average(noisy_data.slice(50, 100).map(fn(point) { point.1 }))
  let smoothed_first_half_avg = calculate_average(smoothed_data.slice(0, 50).map(fn(point) { point.1 }))
  let smoothed_second_half_avg = calculate_average(smoothed_data.slice(50, 100).map(fn(point) { point.1 }))
  
  // 验证趋势保持：后半部分的平均值应该大于前半部分
  assert_true(original_second_half_avg > original_first_half_avg)
  assert_true(smoothed_second_half_avg > smoothed_first_half_avg)
  
  assert_true(true)
}

test "时序数据趋势分析测试" {
  // 测试时序数据的趋势分析
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 创建具有明显趋势的时间序列数据
  let trend_data = []
  for i in 0..100 {
    let timestamp = base_time + (i * 1000000L)  // 每毫秒一个点
    let trend = 10.0 + i.to_double() * 2.0  // 线性增长趋势
    let seasonal = 5.0 * Float::sin(i.to_double() * 0.2)  // 季节性波动
    let noise = (Float::random() - 0.5) * 2.0  // 小噪声
    let value = trend + seasonal + noise
    trend_data.push((timestamp, value))
  }
  
  // 计算简单线性回归趋势
  let n = trend_data.length().to_double()
  let sum_x = 0.0
  let sum_y = 0.0
  let sum_xy = 0.0
  let sum_x2 = 0.0
  
  for i in 0..trend_data.length() {
    let x = i.to_double()
    let y = trend_data[i].1
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
  }
  
  // 计算回归系数
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // 验证趋势分析结果
  // 由于我们创建的数据具有明显的上升趋势（斜率约为2.0）
  // 回归斜率应该为正数
  assert_true(slope > 0.0)
  
  // 计算预测值和实际值的误差
  let errors = []
  for i in 0..trend_data.length() {
    let x = i.to_double()
    let predicted = slope * x + intercept
    let actual = trend_data[i].1
    let error = actual - predicted
    errors.push(error)
  }
  
  // 计算均方误差
  let mse = errors.map(fn(e) { e * e }).reduce(0.0, fn(acc, val) { acc + val }) / errors.length().to_double()
  
  // 验证均方误差在合理范围内
  assert_true(mse >= 0.0)
  
  // 计算R平方值（决定系数）
  let y_mean = sum_y / n
  let total_sum_squares = 0.0
  let residual_sum_squares = 0.0
  
  for i in 0..trend_data.length() {
    let x = i.to_double()
    let actual = trend_data[i].1
    let predicted = slope * x + intercept
    
    total_sum_squares = total_sum_squares + (actual - y_mean) * (actual - y_mean)
    residual_sum_squares = residual_sum_squares + (actual - predicted) * (actual - predicted)
  }
  
  let r_squared = 1.0 - (residual_sum_squares / total_sum_squares)
  
  // 验证R平方值在合理范围内（0到1之间）
  assert_true(r_squared >= 0.0 && r_squared <= 1.0)
  
  assert_true(true)
}

// 辅助函数：计算方差
fn calculate_variance(values: Array[Double]) -> Double {
  let mean = calculate_average(values)
  let sum_squared_diff = 0.0
  
  for value in values {
    let diff = value - mean
    sum_squared_diff = sum_squared_diff + diff * diff
  }
  
  sum_squared_diff / values.length().to_double()
}

// 辅助函数：计算平均值
fn calculate_average(values: Array[Double]) -> Double {
  let sum = 0.0
  for value in values {
    sum = sum + value
  }
  sum / values.length().to_double()
}