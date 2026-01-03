// Azimuth 高级实时流处理测试
// 专注于遥测系统的实时数据处理和流分析功能

// 测试1: 实时数据流过滤与转换
test "实时数据流过滤与转换" {
  // 1. 创建输入数据流
  let input_stream = [
    (1, "metric1", 100.5),
    (2, "metric2", 200.3),
    (3, "metric1", 150.7),
    (4, "metric3", 75.2),
    (5, "metric2", 180.9),
    (6, "metric1", 120.1)
  ]
  
  // 2. 验证输入流
  assert_eq(input_stream.length(), 6)
  
  // 3. 过滤metric1数据
  let metric1_data = input_stream.filter(fn(item) { item.1 == "metric1" })
  assert_eq(metric1_data.length(), 3)
  
  // 4. 验证过滤结果
  assert_eq(metric1_data[0].0, 1)
  assert_eq(metric1_data[0].1, "metric1")
  assert_eq(metric1_data[0].2, 100.5)
  
  assert_eq(metric1_data[1].0, 3)
  assert_eq(metric1_data[1].1, "metric1")
  assert_eq(metric1_data[1].2, 150.7)
  
  assert_eq(metric1_data[2].0, 6)
  assert_eq(metric1_data[2].1, "metric1")
  assert_eq(metric1_data[2].2, 120.1)
  
  // 5. 转换数据 - 只保留值
  let metric1_values = metric1_data.map(fn(item) { item.2 })
  assert_eq(metric1_values, [100.5, 150.7, 120.1])
  
  // 6. 计算平均值
  let sum = metric1_values.reduce(fn(acc, val) { acc + val }, 0.0)
  let avg = sum / metric1_values.length()
  assert_true(avg > 120.0 && avg < 125.0)
  
  // 7. 过滤大于120的值
  let high_values = metric1_values.filter(fn(val) { val > 120.0 })
  assert_eq(high_values.length(), 2)
  assert_eq(high_values, [150.7, 120.1])
}

// 测试2: 时间窗口聚合分析
test "时间窗口聚合分析" {
  // 1. 创建时间序列数据
  let time_series = [
    (1000L, "cpu", 45.5),
    (2000L, "cpu", 50.2),
    (3000L, "memory", 78.3),
    (4000L, "cpu", 48.7),
    (5000L, "memory", 82.1),
    (6000L, "cpu", 52.3),
    (7000L, "memory", 79.8),
    (8000L, "cpu", 46.9)
  ]
  
  // 2. 验证时间序列
  assert_eq(time_series.length(), 8)
  
  // 3. 按指标类型分组
  let cpu_data = time_series.filter(fn(item) { item.1 == "cpu" })
  let memory_data = time_series.filter(fn(item) { item.1 == "memory" })
  
  assert_eq(cpu_data.length(), 4)
  assert_eq(memory_data.length(), 4)
  
  // 4. 计算CPU指标统计
  let cpu_sum = cpu_data.reduce(fn(acc, item) { acc + item.2 }, 0.0)
  let cpu_avg = cpu_sum / cpu_data.length()
  let cpu_max = cpu_data.reduce(fn(acc, item) { 
    if item.2 > acc { item.2 } else { acc } 
  }, 0.0)
  let cpu_min = cpu_data.reduce(fn(acc, item) { 
    if item.2 < acc { item.2 } else { acc } 
  }, 100.0)
  
  assert_true(cpu_avg > 48.0 && cpu_avg < 50.0)
  assert_eq(cpu_max, 52.3)
  assert_eq(cpu_min, 45.5)
  
  // 5. 计算内存指标统计
  let memory_sum = memory_data.reduce(fn(acc, item) { acc + item.2 }, 0.0)
  let memory_avg = memory_sum / memory_data.length()
  
  assert_true(memory_avg > 79.0 && memory_avg < 81.0)
  
  // 6. 时间窗口分析 - 前4秒
  let early_window = time_series.filter(fn(item) { item.0 <= 4000L })
  let late_window = time_series.filter(fn(item) { item.0 > 4000L })
  
  assert_eq(early_window.length(), 4)
  assert_eq(late_window.length(), 4)
  
  // 7. 计算窗口平均值
  let early_sum = early_window.reduce(fn(acc, item) { acc + item.2 }, 0.0)
  let early_avg = early_sum / early_window.length()
  
  let late_sum = late_window.reduce(fn(acc, item) { acc + item.2 }, 0.0)
  let late_avg = late_sum / late_window.length()
  
  assert_true(early_avg > 50.0 && early_avg < 60.0)
  assert_true(late_avg > 60.0 && late_avg < 70.0)
}

// 测试3: 异常检测算法
test "异常检测算法" {
  // 1. 创建正常和异常数据点
  let data_points = [
    (1, 45.2),   // 正常
    (2, 47.8),   // 正常
    (3, 46.5),   // 正常
    (4, 48.1),   // 正常
    (5, 95.3),   // 异常
    (6, 47.3),   // 正常
    (7, 46.9),   // 正常
    (8, 12.5),   // 异常
    (9, 48.7),   // 正常
    (10, 47.1)   // 正常
  ]
  
  // 2. 验证数据点
  assert_eq(data_points.length(), 10)
  
  // 3. 计算基准统计（使用前4个正常点）
  let baseline_points = data_points.slice(0, 4)
  let baseline_sum = baseline_points.reduce(fn(acc, point) { acc + point.1 }, 0.0)
  let baseline_avg = baseline_sum / baseline_points.length()
  
  // 4. 计算标准差
  let variance = baseline_points.reduce(fn(acc, point) { 
    let diff = point.1 - baseline_avg
    acc + diff * diff 
  }, 0.0) / baseline_points.length()
  
  let std_dev = variance.sqrt()
  
  assert_true(baseline_avg > 46.0 && baseline_avg < 48.0)
  assert_true(std_dev > 0.5 && std_dev < 2.0)
  
  // 5. 定义异常阈值（3倍标准差）
  let upper_threshold = baseline_avg + 3.0 * std_dev
  let lower_threshold = baseline_avg - 3.0 * std_dev
  
  // 6. 检测异常点
  let anomalies = data_points.filter(fn(point) { 
    point.1 > upper_threshold || point.1 < lower_threshold 
  })
  
  assert_eq(anomalies.length(), 2)
  
  // 7. 验证异常点
  assert_eq(anomalies[0].0, 5)
  assert_eq(anomalies[0].1, 95.3)
  
  assert_eq(anomalies[1].0, 8)
  assert_eq(anomalies[1].1, 12.5)
  
  // 8. 计算异常率
  let anomaly_rate = anomalies.length().to_float() / data_points.length().to_float()
  assert_eq(anomaly_rate, 0.2)
}

// 测试4: 流式数据聚合
test "流式数据聚合" {
  // 1. 创建流式数据批次
  let batch1 = [
    ("server1", "cpu", 45.2),
    ("server2", "cpu", 50.1),
    ("server1", "memory", 78.5)
  ]
  
  let batch2 = [
    ("server2", "memory", 82.3),
    ("server1", "cpu", 48.7),
    ("server3", "cpu", 35.9)
  ]
  
  let batch3 = [
    ("server3", "memory", 65.2),
    ("server2", "cpu", 52.4),
    ("server1", "memory", 80.1)
  ]
  
  // 2. 验证批次
  assert_eq(batch1.length(), 3)
  assert_eq(batch2.length(), 3)
  assert_eq(batch3.length(), 3)
  
  // 3. 合并所有批次
  let all_batches = []
  let all_data = all_batches.concat(batch1).concat(batch2).concat(batch3)
  
  assert_eq(all_data.length(), 9)
  
  // 4. 按服务器分组
  let server1_data = all_data.filter(fn(item) { item.0 == "server1" })
  let server2_data = all_data.filter(fn(item) { item.0 == "server2" })
  let server3_data = all_data.filter(fn(item) { item.0 == "server3" })
  
  assert_eq(server1_data.length(), 3)
  assert_eq(server2_data.length(), 3)
  assert_eq(server3_data.length(), 3)
  
  // 5. 按指标类型分组
  let cpu_data = all_data.filter(fn(item) { item.1 == "cpu" })
  let memory_data = all_data.filter(fn(item) { item.1 == "memory" })
  
  assert_eq(cpu_data.length(), 5)
  assert_eq(memory_data.length(), 4)
  
  // 6. 计算每个服务器的CPU平均值
  let server1_cpu = server1_data.filter(fn(item) { item.1 == "cpu" })
  let server1_cpu_avg = server1_cpu.reduce(fn(acc, item) { acc + item.2 }, 0.0) / server1_cpu.length()
  
  let server2_cpu = server2_data.filter(fn(item) { item.1 == "cpu" })
  let server2_cpu_avg = server2_cpu.reduce(fn(acc, item) { acc + item.2 }, 0.0) / server2_cpu.length()
  
  let server3_cpu = server3_data.filter(fn(item) { item.1 == "cpu" })
  let server3_cpu_avg = server3_cpu.reduce(fn(acc, item) { acc + item.2 }, 0.0) / server3_cpu.length()
  
  assert_true(server1_cpu_avg > 46.0 && server1_cpu_avg < 48.0)
  assert_true(server2_cpu_avg > 50.0 && server2_cpu_avg < 52.0)
  assert_eq(server3_cpu_avg, 35.9)
  
  // 7. 计算全局CPU平均值
  let global_cpu_avg = cpu_data.reduce(fn(acc, item) { acc + item.2 }, 0.0) / cpu_data.length()
  assert_true(global_cpu_avg > 45.0 && global_cpu_avg < 48.0)
}

// 测试5: 实时趋势分析
test "实时趋势分析" {
  // 1. 创建趋势数据
  let trend_data = [
    (1, 100.0),
    (2, 105.0),
    (3, 102.0),
    (4, 108.0),
    (5, 110.0),
    (6, 107.0),
    (7, 112.0),
    (8, 115.0),
    (9, 113.0),
    (10, 118.0)
  ]
  
  // 2. 验证趋势数据
  assert_eq(trend_data.length(), 10)
  
  // 3. 计算移动平均（窗口大小为3）
  let moving_averages = []
  for i in 2..trend_data.length() {
    let window_sum = trend_data[i-2].1 + trend_data[i-1].1 + trend_data[i].1
    let window_avg = window_sum / 3.0
    moving_averages = moving_averages.push(window_avg)
  }
  
  assert_eq(moving_averages.length(), 8)
  
  // 4. 验证移动平均计算
  assert_eq(moving_averages[0], (100.0 + 105.0 + 102.0) / 3.0)
  assert_eq(moving_averages[1], (105.0 + 102.0 + 108.0) / 3.0)
  assert_eq(moving_averages[7], (112.0 + 115.0 + 113.0) / 3.0)
  
  // 5. 计算趋势斜率（简单线性回归）
  let n = trend_data.length().to_float()
  let sum_x = trend_data.reduce(fn(acc, point) { acc + point.0.to_float() }, 0.0)
  let sum_y = trend_data.reduce(fn(acc, point) { acc + point.1 }, 0.0)
  let sum_xy = trend_data.reduce(fn(acc, point) { 
    acc + point.0.to_float() * point.1 
  }, 0.0)
  let sum_x2 = trend_data.reduce(fn(acc, point) { 
    acc + point.0.to_float() * point.0.to_float() 
  }, 0.0)
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  
  // 6. 验证趋势为正增长
  assert_true(slope > 1.5 && slope < 2.5)
  
  // 7. 检测趋势变化点
  let trend_changes = []
  for i in 1..trend_data.length() - 1 {
    let prev_diff = trend_data[i].1 - trend_data[i-1].1
    let next_diff = trend_data[i+1].1 - trend_data[i].1
    
    // 如果趋势方向改变
    if (prev_diff > 0.0 && next_diff < 0.0) || (prev_diff < 0.0 && next_diff > 0.0) {
      trend_changes = trend_changes.push(i)
    }
  }
  
  // 8. 验证趋势变化
  assert_eq(trend_changes.length(), 2)  // 在点5和点9有趋势变化
}

// 测试6: 数据质量监控
test "数据质量监控" {
  // 1. 创建包含质量问题的数据
  let quality_data = [
    (1, "metric1", 45.2, 0.95),   // 高质量
    (2, "metric2", 0.0, 0.88),    // 零值
    (3, "metric3", -1.5, 0.92),   // 负值
    (4, "metric1", 999.9, 0.75),  // 异常高值
    (5, "metric2", 52.3, 0.98),   // 高质量
    (6, "metric3", 0.0, 0.85),    // 零值
    (7, "metric1", 48.7, 0.94),   // 高质量
    (8, "metric2", -5.2, 0.82),   // 负值
    (9, "metric3", 75.1, 0.97)    // 高质量
  ]
  
  // 2. 验证数据
  assert_eq(quality_data.length(), 9)
  
  // 3. 计算整体质量分数
  let total_quality = quality_data.reduce(fn(acc, item) { acc + item.3 }, 0.0)
  let avg_quality = total_quality / quality_data.length()
  
  assert_true(avg_quality > 0.85 && avg_quality < 0.95)
  
  // 4. 识别低质量数据（质量分数 < 0.9）
  let low_quality_data = quality_data.filter(fn(item) { item.3 < 0.9 })
  assert_eq(low_quality_data.length(), 4)
  
  // 5. 识别零值数据
  let zero_values = quality_data.filter(fn(item) { item.2 == 0.0 })
  assert_eq(zero_values.length(), 2)
  
  // 6. 识别负值数据
  let negative_values = quality_data.filter(fn(item) { item.2 < 0.0 })
  assert_eq(negative_values.length(), 2)
  
  // 7. 按指标类型分组质量分析
  let metric1_data = quality_data.filter(fn(item) { item.1 == "metric1" })
  let metric2_data = quality_data.filter(fn(item) { item.1 == "metric2" })
  let metric3_data = quality_data.filter(fn(item) { item.1 == "metric3" })
  
  assert_eq(metric1_data.length(), 3)
  assert_eq(metric2_data.length(), 3)
  assert_eq(metric3_data.length(), 3)
  
  // 8. 计算各指标的平均质量
  let metric1_quality = metric1_data.reduce(fn(acc, item) { acc + item.3 }, 0.0) / metric1_data.length()
  let metric2_quality = metric2_data.reduce(fn(acc, item) { acc + item.3 }, 0.0) / metric2_data.length()
  let metric3_quality = metric3_data.reduce(fn(acc, item) { acc + item.3 }, 0.0) / metric3_data.length()
  
  assert_true(metric1_quality > 0.85 && metric1_quality < 0.95)
  assert_true(metric2_quality > 0.80 && metric2_quality < 0.90)
  assert_true(metric3_quality > 0.85 && metric3_quality < 0.95)
  
  // 9. 生成质量报告
  let quality_issues = []
  if zero_values.length() > 0 {
    quality_issues = quality_issues.push("Zero values detected: " + zero_values.length().to_string())
  }
  if negative_values.length() > 0 {
    quality_issues = quality_issues.push("Negative values detected: " + negative_values.length().to_string())
  }
  if low_quality_data.length() > 0 {
    quality_issues = quality_issues.push("Low quality data points: " + low_quality_data.length().to_string())
  }
  
  assert_eq(quality_issues.length(), 3)
}

// 测试7: 实时性能监控
test "实时性能监控" {
  // 1. 创建性能指标数据
  let performance_metrics = [
    (1000L, "response_time", 120.5),
    (2000L, "throughput", 1500.0),
    (3000L, "response_time", 95.3),
    (4000L, "error_rate", 0.02),
    (5000L, "response_time", 180.7),
    (6000L, "throughput", 1650.0),
    (7000L, "response_time", 110.2),
    (8000L, "error_rate", 0.01),
    (9000L, "response_time", 85.9),
    (10000L, "throughput", 1750.0)
  ]
  
  // 2. 验证性能数据
  assert_eq(performance_metrics.length(), 10)
  
  // 3. 按指标类型分组
  let response_times = performance_metrics.filter(fn(item) { item.1 == "response_time" })
  let throughput = performance_metrics.filter(fn(item) { item.1 == "throughput" })
  let error_rates = performance_metrics.filter(fn(item) { item.1 == "error_rate" })
  
  assert_eq(response_times.length(), 5)
  assert_eq(throughput.length(), 3)
  assert_eq(error_rates.length(), 2)
  
  // 4. 计算响应时间统计
  let rt_sum = response_times.reduce(fn(acc, item) { acc + item.2 }, 0.0)
  let rt_avg = rt_sum / response_times.length()
  let rt_max = response_times.reduce(fn(acc, item) { 
    if item.2 > acc { item.2 } else { acc } 
  }, 0.0)
  let rt_min = response_times.reduce(fn(acc, item) { 
    if item.2 < acc { item.2 } else { acc } 
  }, 1000.0)
  
  assert_true(rt_avg > 110.0 && rt_avg < 130.0)
  assert_eq(rt_max, 180.7)
  assert_eq(rt_min, 85.9)
  
  // 5. 计算吞吐量趋势
  let throughput_values = throughput.map(fn(item) { item.2 })
  assert_true(throughput_values[0] < throughput_values[1])
  assert_true(throughput_values[1] < throughput_values[2])
  
  // 6. 计算错误率平均值
  let er_avg = error_rates.reduce(fn(acc, item) { acc + item.2 }, 0.0) / error_rates.length()
  assert_eq(er_avg, 0.015)
  
  // 7. 检测性能异常（响应时间超过150ms）
  let slow_responses = response_times.filter(fn(item) { item.2 > 150.0 })
  assert_eq(slow_responses.length(), 1)
  assert_eq(slow_responses[0].2, 180.7)
  
  // 8. 计算性能分数（综合指标）
  let performance_score = 100.0 - (rt_avg / 10.0) - (er_avg * 1000.0) + (throughput_values[2] / 50.0)
  assert_true(performance_score > 80.0 && performance_score < 100.0)
}

// 测试8: 流式数据关联分析
test "流式数据关联分析" {
  // 1. 创建关联数据流
  let stream_a = [
    (1, "event_a", 100.0),
    (3, "event_a", 120.0),
    (5, "event_a", 110.0),
    (7, "event_a", 130.0),
    (9, "event_a", 115.0)
  ]
  
  let stream_b = [
    (2, "event_b", 200.0),
    (4, "event_b", 180.0),
    (6, "event_b", 220.0),
    (8, "event_b", 190.0),
    (10, "event_b", 210.0)
  ]
  
  // 2. 验证数据流
  assert_eq(stream_a.length(), 5)
  assert_eq(stream_b.length(), 5)
  
  // 3. 合并数据流
  let merged_stream = []
  let i = 0
  let j = 0
  
  while i < stream_a.length() && j < stream_b.length() {
    if stream_a[i].0 < stream_b[j].0 {
      merged_stream = merged_stream.push(stream_a[i])
      i = i + 1
    } else {
      merged_stream = merged_stream.push(stream_b[j])
      j = j + 1
    }
  }
  
  // 添加剩余元素
  while i < stream_a.length() {
    merged_stream = merged_stream.push(stream_a[i])
    i = i + 1
  }
  
  while j < stream_b.length() {
    merged_stream = merged_stream.push(stream_b[j])
    j = j + 1
  }
  
  assert_eq(merged_stream.length(), 10)
  
  // 4. 验证时间顺序
  for k in 1..merged_stream.length() {
    assert_true(merged_stream[k].0 > merged_stream[k-1].0)
  }
  
  // 5. 计算相关性（简化版本：检查相反趋势）
  let a_values = stream_a.map(fn(item) { item.2 })
  let b_values = stream_b.map(fn(item) { item.2 })
  
  // 6. 计算趋势变化
  let a_trends = []
  for k in 1..a_values.length() {
    let diff = a_values[k] - a_values[k-1]
    a_trends = a_trends.push(diff)
  }
  
  let b_trends = []
  for k in 1..b_values.length() {
    let diff = b_values[k] - b_values[k-1]
    b_trends = b_trends.push(diff)
  }
  
  // 7. 检测负相关性（趋势相反）
  let opposite_trends = 0
  for k in 0..a_trends.length() {
    if (a_trends[k] > 0.0 && b_trends[k] < 0.0) || (a_trends[k] < 0.0 && b_trends[k] > 0.0) {
      opposite_trends = opposite_trends + 1
    }
  }
  
  assert_true(opposite_trends >= 2)
  
  // 8. 计算时间窗口内的相关性
  let window_size = 3
  let correlations = []
  
  for start in 0..(stream_a.length() - window_size + 1) {
    let a_window = stream_a.slice(start, start + window_size)
    let b_window = stream_b.slice(start, start + window_size)
    
    let a_avg = a_window.reduce(fn(acc, item) { acc + item.2 }, 0.0) / a_window.length()
    let b_avg = b_window.reduce(fn(acc, item) { acc + item.2 }, 0.0) / b_window.length()
    
    // 简化的相关性计算
    let a_diff = a_window[window_size - 1].2 - a_window[0].2
    let b_diff = b_window[window_size - 1].2 - b_window[0].2
    
    let correlation = if a_diff * b_diff < 0.0 { -1.0 } else { 1.0 }
    correlations = correlations.push(correlation)
  }
  
  assert_eq(correlations.length(), 3)
  
  // 9. 验证负相关性
  let negative_correlations = correlations.filter(fn(c) { c < 0.0 })
  assert_true(negative_correlations.length() >= 1)
}