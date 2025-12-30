// 遥测智能过滤和降噪测试用例
// 测试智能数据过滤、噪声去除和信号增强算法

test "telemetry_statistical_outlier_filtering" {
  // 测试统计学异常值过滤
  
  let raw_metrics = [10.2, 11.5, 9.8, 10.7, 11.2, 10.5, 9.9, 10.8, 45.6, 10.3, 11.1, 9.5, 10.9]
  let outlier_threshold = 3.0  // 3σ规则
  
  // 计算均值和标准差
  let mut sum = 0.0
  let mut i = 0
  while i < raw_metrics.length() {
    sum = sum + raw_metrics[i]
    i = i + 1
  }
  let mean = sum / raw_metrics.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < raw_metrics.length() {
    let diff = raw_metrics[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let variance = variance_sum / raw_metrics.length().to_double()
  let std_dev = @sqrt(variance)
  
  // 过滤异常值
  let mut filtered_metrics = []
  let mut outlier_count = 0
  
  i = 0
  while i < raw_metrics.length() {
    let value = raw_metrics[i]
    let z_score = @abs(value - mean) / std_dev
    
    if z_score <= outlier_threshold {
      filtered_metrics.push(value)
    } else {
      outlier_count = outlier_count + 1
    }
    
    i = i + 1
  }
  
  // 验证异常值过滤结果
  assert_eq(filtered_metrics.length(), raw_metrics.length() - outlier_count)
  assert_eq(outlier_count, 1)  // 应该检测到1个异常值
  assert_eq(filtered_metrics.length(), 12)
  
  // 过滤后的数据应该更集中
  let mut filtered_sum = 0.0
  i = 0
  while i < filtered_metrics.length() {
    filtered_sum = filtered_sum + filtered_metrics[i]
    i = i + 1
  }
  let filtered_mean = filtered_sum / filtered_metrics.length().to_double()
  
  assert_eq(@abs(filtered_mean - 10.5) < 1.0, true)  // 过滤后均值应该接近10.5
}

test "telemetry_moving_average_smoothing" {
  // 测试移动平均平滑
  
  let noisy_data = [25.3, 52.8, 28.1, 48.5, 32.7, 45.2, 29.8, 51.3, 35.6, 42.9, 31.2, 47.8]
  let window_sizes = [3, 5, 7]
  
  // 对不同窗口大小进行移动平均平滑
  let mut smoothed_results = []
  
  let mut w = 0
  while w < window_sizes.length() {
    let window_size = window_sizes[w]
    let mut smoothed_values = []
    
    let mut i = window_size - 1
    while i < noisy_data.length() {
      // 计算窗口平均值
      let mut window_sum = 0.0
      let mut j = i - (window_size - 1)
      while j <= i {
        window_sum = window_sum + noisy_data[j]
        j = j + 1
      }
      let avg = window_sum / window_size.to_double()
      smoothed_values.push(avg)
      
      i = i + 1
    }
    
    smoothed_results.push((window_size, smoothed_values))
    w = w + 1
  }
  
  // 验证移动平均平滑结果
  assert_eq(smoothed_results.length(), window_sizes.length())
  
  // 窗口越大，平滑效果越强，数据点越少
  assert_eq(smoothed_results[0].1.length() > smoothed_results[1].1.length(), true)
  assert_eq(smoothed_results[1].1.length() > smoothed_results[2].1.length(), true)
  
  // 平滑后的数据应该更加稳定
  let three_window_data = smoothed_results[0].1
  let mut variance = 0.0
  let mut mean = 0.0
  
  let mut i = 0
  while i < three_window_data.length() {
    mean = mean + three_window_data[i]
    i = i + 1
  }
  mean = mean / three_window_data.length().to_double()
  
  i = 0
  while i < three_window_data.length() {
    let diff = three_window_data[i] - mean
    variance = variance + diff * diff
    i = i + 1
  }
  variance = variance / three_window_data.length().to_double()
  
  assert_eq(variance < 100.0, true)  // 平滑后方差应该较小
}

test "telemetry_median_filter_denoising" {
  // 测试中值滤波降噪
  
  let signal_with_spikes = [10.5, 11.2, 10.8, 95.3, 10.9, 11.5, 10.3, 89.7, 10.7, 11.1, 10.4]
  let filter_window = 3
  
  // 应用中值滤波
  let mut median_filtered = []
  
  let mut i = filter_window - 1
  while i < signal_with_spikes.length() {
    // 提取窗口数据
    let window_data = []
    let mut j = i - (filter_window - 1)
    while j <= i {
      window_data.push(signal_with_spikes[j])
      j = j + 1
    }
    
    // 找中值
    let sorted_window = []
    let mut j = 0
    while j < window_data.length() {
      sorted_window.push(window_data[j])
      j = j + 1
    }
    
    // 简单排序（冒泡）
    let mut j = 0
    while j < sorted_window.length() - 1 {
      let mut k = 0
      while k < sorted_window.length() - 1 - j {
        if sorted_window[k] > sorted_window[k + 1] {
          let temp = sorted_window[k]
          sorted_window[k] = sorted_window[k + 1]
          sorted_window[k + 1] = temp
        }
        k = k + 1
      }
      j = j + 1
    }
    
    let median_value = sorted_window[1]  // 窗口大小为3，中值在索引1
    median_filtered.push(median_value)
    
    i = i + 1
  }
  
  // 验证中值滤波结果
  assert_eq(median_filtered.length(), signal_with_spikes.length() - filter_window + 1)
  
  // 中值滤波应该有效去除尖峰
  let mut spike_count = 0
  let mut i = 0
  while i < median_filtered.length() {
    if median_filtered[i] > 50.0 {
      spike_count = spike_count + 1
    }
    i = i + 1
  }
  
  assert_eq(spike_count, 0)  // 中值滤波后应该没有尖峰
  
  // 滤波后的值应该在正常范围内
  let mut i = 0
  while i < median_filtered.length() {
    assert_eq(median_filtered[i] > 8.0 && median_filtered[i] < 15.0, true)
    i = i + 1
  }
}

test "telemetry_exponential_smoothing" {
  // 测试指数平滑
  
  let raw_series = [100, 120, 95, 140, 125, 160, 145, 180, 165, 190]
  let alpha = 0.3  // 平滑因子
  
  // 应用指数平滑
  let mut smoothed_values = []
  let mut previous_smoothed = raw_series[0].to_double()  // 初始值
  
  smoothed_values.push(previous_smoothed)
  
  let mut i = 1
  while i < raw_series.length() {
    let current_value = raw_series[i].to_double()
    let smoothed_value = alpha * current_value + (1.0 - alpha) * previous_smoothed
    
    smoothed_values.push(smoothed_value)
    previous_smoothed = smoothed_value
    
    i = i + 1
  }
  
  // 验证指数平滑结果
  assert_eq(smoothed_values.length(), raw_series.length())
  
  // 平滑后的数据应该比原始数据更平滑
  let mut original_variance = 0.0
  let mut smoothed_variance = 0.0
  
  // 计算原始数据方差
  let mut original_sum = 0.0
  let mut i = 0
  while i < raw_series.length() {
    original_sum = original_sum + raw_series[i].to_double()
    i = i + 1
  }
  let original_mean = original_sum / raw_series.length().to_double()
  
  i = 0
  while i < raw_series.length() {
    let diff = raw_series[i].to_double() - original_mean
    original_variance = original_variance + diff * diff
    i = i + 1
  }
  original_variance = original_variance / raw_series.length().to_double()
  
  // 计算平滑数据方差
  let mut smoothed_sum = 0.0
  i = 0
  while i < smoothed_values.length() {
    smoothed_sum = smoothed_sum + smoothed_values[i]
    i = i + 1
  }
  let smoothed_mean = smoothed_sum / smoothed_values.length().to_double()
  
  i = 0
  while i < smoothed_values.length() {
    let diff = smoothed_values[i] - smoothed_mean
    smoothed_variance = smoothed_variance + diff * diff
    i = i + 1
  }
  smoothed_variance = smoothed_variance / smoothed_values.length().to_double()
  
  assert_eq(smoothed_variance < original_variance, true)
}

test "telemetry_adaptive_noise_filtering" {
  // 测试自适应噪声过滤
  
  let sensor_readings = [20.5, 21.2, 19.8, 35.7, 20.9, 21.5, 19.3, 42.1, 20.7, 21.1, 19.5]
  let noise_sensitivity = 0.15  // 噪声敏感度
  
  // 自适应噪声检测和过滤
  let mut adaptive_filtered = []
  let mut noise_levels = []
  
  let mut i = 0
  while i < sensor_readings.length() {
    let current_value = sensor_readings[i]
    
    if i == 0 {
      // 第一个值直接通过
      adaptive_filtered.push(current_value)
      noise_levels.push(0.0)
    } else {
      // 计算与前一值的差异
      let previous_value = adaptive_filtered[i - 1]
      let difference = @abs(current_value - previous_value)
      
      // 计算局部噪声水平
      let mut local_noise = 0.0
      let sample_count = @min(i, 5)
      let mut j = @max(0, i - sample_count)
      let mut local_sum = 0.0
      
      while j < i {
        local_sum = local_sum + sensor_readings[j]
        j = j + 1
      }
      let local_mean = local_sum / sample_count.to_double()
      
      let mut local_variance = 0.0
      j = @max(0, i - sample_count)
      while j < i {
        let diff = sensor_readings[j] - local_mean
        local_variance = local_variance + diff * diff
        j = j + 1
      }
      local_variance = local_variance / sample_count.to_double()
      local_noise = @sqrt(local_variance)
      
      // 自适应阈值
      let adaptive_threshold = local_noise * (1.0 + noise_sensitivity)
      
      // 过滤决策
      let filtered_value = 
        if difference > adaptive_threshold {
          previous_value  // 差异过大，认为是噪声，使用前一值
        } else {
          current_value   // 正常值
        }
      
      adaptive_filtered.push(filtered_value)
      noise_levels.push(local_noise)
    }
    
    i = i + 1
  }
  
  // 验证自适应噪声过滤结果
  assert_eq(adaptive_filtered.length(), sensor_readings.length())
  assert_eq(noise_levels.length(), sensor_readings.length())
  
  // 过滤后的数据应该更平滑
  let mut spike_count = 0
  let mut i = 1
  while i < adaptive_filtered.length() {
    let difference = @abs(adaptive_filtered[i] - adaptive_filtered[i - 1])
    if difference > 10.0 {
      spike_count = spike_count + 1
    }
    i = i + 1
  }
  
  assert_eq(spike_count < 2, true)  // 过滤后尖峰应该显著减少
  
  // 大部分值应该在合理范围内
  let mut normal_count = 0
  let mut i = 0
  while i < adaptive_filtered.length() {
    if adaptive_filtered[i] > 18.0 && adaptive_filtered[i] < 23.0 {
      normal_count = normal_count + 1
    }
    i = i + 1
  }
  
  assert_eq(normal_count > adaptive_filtered.length() * 0.7, true)
}