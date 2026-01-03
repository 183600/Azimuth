// Azimuth 遥测系统数据聚合分析测试
// 专注于遥测数据的聚合操作和分析功能

// 测试1: 时间序列数据聚合
test "时间序列数据聚合" {
  // 模拟时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "auth" },
    { timestamp: 1640995260, metric: "cpu", value: 50.0, service: "auth" },
    { timestamp: 1640995320, metric: "cpu", value: 55.0, service: "auth" },
    { timestamp: 1640995380, metric: "cpu", value: 60.0, service: "auth" },
    { timestamp: 1640995440, metric: "cpu", value: 58.0, service: "auth" },
    { timestamp: 1640995200, metric: "memory", value: 1024.0, service: "auth" },
    { timestamp: 1640995260, metric: "memory", value: 1050.0, service: "auth" },
    { timestamp: 1640995320, metric: "memory", value: 1080.0, service: "auth" },
    { timestamp: 1640995380, metric: "memory", value: 1100.0, service: "auth" },
    { timestamp: 1640995440, metric: "memory", value: 1120.0, service: "auth" }
  ]
  
  // 按指标类型分组
  let mut grouped_data = {}
  for data in time_series_data {
    let current_values = grouped_data[data.metric]
    match current_values {
      Some(values) => {
        grouped_data[data.metric] = Some(values.push(data.value))
      }
      None => {
        grouped_data[data.metric] = Some([data.value])
      }
    }
  }
  
  // 计算每个指标的聚合统计
  let mut aggregation_results = []
  for (metric, values) in grouped_data {
    match values {
      Some(value_list) => {
        // 计算平均值
        let mut sum = 0.0
        for v in value_list {
          sum = sum + v
        }
        let avg = sum / value_list.length().to_float()
        
        // 计算最大值和最小值
        let mut max_val = value_list[0]
        let mut min_val = value_list[0]
        for v in value_list {
          if v > max_val { max_val = v }
          if v < min_val { min_val = v }
        }
        
        aggregation_results = aggregation_results.push({
          metric: metric,
          count: value_list.length(),
          avg: avg,
          min: min_val,
          max: max_val,
          sum: sum
        })
      }
      None => ()
    }
  }
  
  // 验证聚合结果
  assert_eq(aggregation_results.length(), 2)
  
  // 验证CPU指标聚合
  let cpu_result = aggregation_results[0]
  assert_eq(cpu_result.metric, "cpu")
  assert_eq(cpu_result.count, 5)
  assert_eq(cpu_result.avg, 53.6) // (45+50+55+60+58)/5
  assert_eq(cpu_result.min, 45.0)
  assert_eq(cpu_result.max, 60.0)
  assert_eq(cpu_result.sum, 268.0)
  
  // 验证内存指标聚合
  let memory_result = aggregation_results[1]
  assert_eq(memory_result.metric, "memory")
  assert_eq(memory_result.count, 5)
  assert_eq(memory_result.avg, 1074.8) // (1024+1050+1080+1100+1120)/5
  assert_eq(memory_result.min, 1024.0)
  assert_eq(memory_result.max, 1120.0)
  assert_eq(memory_result.sum, 5374.0)
}

// 测试2: 多维度数据聚合
test "多维度数据聚合" {
  // 模拟多维度遥测数据
  let multi_dimension_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "auth", region: "us-east", env: "prod" },
    { timestamp: 1640995260, metric: "cpu", value: 50.0, service: "auth", region: "us-east", env: "prod" },
    { timestamp: 1640995320, metric: "cpu", value: 40.0, service: "auth", region: "us-west", env: "prod" },
    { timestamp: 1640995380, metric: "cpu", value: 35.0, service: "auth", region: "us-west", env: "staging" },
    { timestamp: 1640995440, metric: "cpu", value: 55.0, service: "db", region: "us-east", env: "prod" },
    { timestamp: 1640995500, metric: "cpu", value: 30.0, service: "db", region: "us-west", env: "staging" },
    { timestamp: 1640995560, metric: "memory", value: 1024.0, service: "auth", region: "us-east", env: "prod" },
    { timestamp: 1640995620, metric: "memory", value: 2048.0, service: "db", region: "us-east", env: "prod" }
  ]
  
  // 按服务和环境维度分组聚合
  let mut service_env_groups = {}
  for data in multi_dimension_data {
    let group_key = data.service + ":" + data.env
    let current_values = service_env_groups[group_key]
    match current_values {
      Some(values) => {
        service_env_groups[group_key] = Some(values.push(data.value))
      }
      None => {
        service_env_groups[group_key] = Some([data.value])
      }
    }
  }
  
  // 计算各组的平均值
  let mut service_env_avg = {}
  for (group_key, values) in service_env_groups {
    match values {
      Some(value_list) => {
        let mut sum = 0.0
        for v in value_list {
          sum = sum + v
        }
        let avg = sum / value_list.length().to_float()
        service_env_avg[group_key] = Some(avg)
      }
      None => ()
    }
  }
  
  // 按区域维度分组聚合
  let mut region_groups = {}
  for data in multi_dimension_data {
    let current_values = region_groups[data.region]
    match current_values {
      Some(values) => {
        region_groups[data.region] = Some(values.push(data.value))
      }
      None => {
        region_groups[data.region] = Some([data.value])
      }
    }
  }
  
  // 计算各区域的平均值
  let mut region_avg = {}
  for (region, values) in region_groups {
    match values {
      Some(value_list) => {
        let mut sum = 0.0
        for v in value_list {
          sum = sum + v
        }
        let avg = sum / value_list.length().to_float()
        region_avg[region] = Some(avg)
      }
      None => ()
    }
  }
  
  // 验证服务和环境维度聚合
  assert_eq(service_env_avg["auth:prod"], Some(706.3333333333334)) // (45+50+1024)/3
  assert_eq(service_env_avg["auth:staging"], Some(35.0)) // 35
  assert_eq(service_env_avg["db:prod"], Some(1051.5)) // (55+2048)/2
  assert_eq(service_env_avg["db:staging"], Some(30.0)) // 30
  
  // 验证区域维度聚合
  assert_eq(region_avg["us-east"], Some(794.4)) // (45+50+55+1024+2048)/5
  assert_eq(region_avg["us-west"], Some(35.0)) // (40+35+30)/3
}

// 测试3: 滑动窗口数据聚合
test "滑动窗口数据聚合" {
  // 模拟时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995210, value: 20.0 },
    { timestamp: 1640995220, value: 30.0 },
    { timestamp: 1640995230, value: 40.0 },
    { timestamp: 1640995240, value: 50.0 },
    { timestamp: 1640995250, value: 60.0 },
    { timestamp: 1640995260, value: 70.0 },
    { timestamp: 1640995270, value: 80.0 }
  ]
  
  // 滑动窗口聚合函数
  let sliding_window_aggregation = fn(data, window_size) {
    let mut results = []
    let mut i = 0
    while i <= data.length() - window_size {
      let window_data = data.slice(i, i + window_size)
      
      // 计算窗口内的平均值
      let mut sum = 0.0
      for point in window_data {
        sum = sum + point.value
      }
      let avg = sum / window_size.to_float()
      
      // 计算窗口内的最大值和最小值
      let mut max_val = window_data[0].value
      let mut min_val = window_data[0].value
      for point in window_data {
        if point.value > max_val { max_val = point.value }
        if point.value < min_val { min_val = point.value }
      }
      
      results = results.push({
        start_time: window_data[0].timestamp,
        end_time: window_data[window_size-1].timestamp,
        avg: avg,
        min: min_val,
        max: max_val,
        count: window_size
      })
      
      i = i + 1
    }
    results
  }
  
  // 使用3个数据点的滑动窗口
  let window_results = sliding_window_aggregation(time_series_data, 3)
  
  // 验证滑动窗口聚合结果
  assert_eq(window_results.length(), 6) // 8-3+1=6个窗口
  
  // 验证第一个窗口 (10, 20, 30)
  assert_eq(window_results[0].start_time, 1640995200)
  assert_eq(window_results[0].end_time, 1640995220)
  assert_eq(window_results[0].avg, 20.0) // (10+20+30)/3
  assert_eq(window_results[0].min, 10.0)
  assert_eq(window_results[0].max, 30.0)
  assert_eq(window_results[0].count, 3)
  
  // 验证最后一个窗口 (60, 70, 80)
  assert_eq(window_results[5].start_time, 1640995250)
  assert_eq(window_results[5].end_time, 1640995270)
  assert_eq(window_results[5].avg, 70.0) // (60+70+80)/3
  assert_eq(window_results[5].min, 60.0)
  assert_eq(window_results[5].max, 80.0)
  assert_eq(window_results[5].count, 3)
}

// 测试4: 分位数数据聚合
test "分位数数据聚合" {
  // 模拟遥测数据
  let telemetry_data = [
    { metric: "response_time", value: 10.0 },
    { metric: "response_time", value: 20.0 },
    { metric: "response_time", value: 30.0 },
    { metric: "response_time", value: 40.0 },
    { metric: "response_time", value: 50.0 },
    { metric: "response_time", value: 60.0 },
    { metric: "response_time", value: 70.0 },
    { metric: "response_time", value: 80.0 },
    { metric: "response_time", value: 90.0 },
    { metric: "response_time", value: 100.0 }
  ]
  
  // 提取值并排序
  let mut values = []
  for data in telemetry_data {
    values = values.push(data.value)
  }
  
  // 简化的排序（冒泡排序）
  let mut i = 0
  while i < values.length() {
    let mut j = 0
    while j < values.length() - i - 1 {
      if values[j] > values[j+1] {
        let temp = values[j]
        values[j] = values[j+1]
        values[j+1] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 计算分位数
  let calculate_percentile = fn(sorted_values, percentile) {
    let index = ((sorted_values.length() - 1).to_float() * percentile / 100.0).to_int()
    sorted_values[index]
  }
  
  // 计算各种分位数
  let p50 = calculate_percentile(values, 50) // 中位数
  let p90 = calculate_percentile(values, 90) // 90分位数
  let p95 = calculate_percentile(values, 95) // 95分位数
  let p99 = calculate_percentile(values, 99) // 99分位数
  
  // 计算基本统计
  let mut sum = 0.0
  for v in values {
    sum = sum + v
  }
  let mean = sum / values.length().to_float()
  
  let min_val = values[0]
  let max_val = values[values.length()-1]
  
  // 验证分位数计算
  assert_eq(min_val, 10.0)
  assert_eq(max_val, 100.0)
  assert_eq(mean, 55.0) // (10+20+...+100)/10
  assert_eq(p50, 55.0) // 中位数
  assert_eq(p90, 90.0) // 90分位数
  assert_eq(p95, 95.0) // 95分位数
  assert_eq(p99, 100.0) // 99分位数
  
  // 验证分位数关系
  assert_true(p50 <= p90)
  assert_true(p90 <= p95)
  assert_true(p95 <= p99)
  assert_true(p99 <= max_val)
}

// 测试5: 趋势分析聚合
test "趋势分析聚合" {
  // 模拟时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995260, value: 15.0 },
    { timestamp: 1640995320, value: 20.0 },
    { timestamp: 1640995380, value: 25.0 },
    { timestamp: 1640995440, value: 30.0 },
    { timestamp: 1640995500, value: 28.0 },
    { timestamp: 1640995560, value: 26.0 },
    { timestamp: 1640995620, value: 24.0 }
  ]
  
  // 计算趋势（简单线性回归）
  let calculate_trend = fn(data) {
    let n = data.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    let mut i = 0
    while i < data.length() {
      let x = i.to_float()
      let y = data[i].value
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + x * y
      sum_x2 = sum_x2 + x * x
      
      i = i + 1
    }
    
    // 计算斜率和截距
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    { slope: slope, intercept: intercept }
  }
  
  // 计算趋势
  let trend = calculate_trend(time_series_data)
  
  // 计算预测值
  let predict = fn(x, slope, intercept) {
    slope * x + intercept
  }
  
  // 计算趋势方向
  let trend_direction = 
    if trend.slope > 0.5 { "strong_up" }
    else if trend.slope > 0.1 { "up" }
    else if trend.slope > -0.1 { "stable" }
    else if trend.slope > -0.5 { "down" }
    else { "strong_down" }
  
  // 验证趋势分析
  assert_true(trend.slope < 0.0) // 负斜率，表示下降趋势
  assert_eq(trend_direction, "stable") // 趋势接近稳定
  
  // 验证预测值
  let predicted_0 = predict(0.0, trend.slope, trend.intercept)
  let predicted_7 = predict(7.0, trend.slope, trend.intercept)
  
  assert_eq(predicted_0, time_series_data[0].value)
  assert_true(predicted_7 >= 20.0 && predicted_7 <= 30.0)
  
  // 计算变化率
  let first_value = time_series_data[0].value
  let last_value = time_series_data[time_series_data.length()-1].value
  let change_rate = ((last_value - first_value) / first_value) * 100.0
  
  assert_eq(change_rate, 140.0) // (24-10)/10*100
}

// 测试6: 采样数据聚合
test "采样数据聚合" {
  // 模拟高频遥测数据
  let high_frequency_data = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995201, value: 12.0 },
    { timestamp: 1640995202, value: 11.0 },
    { timestamp: 1640995203, value: 13.0 },
    { timestamp: 1640995204, value: 15.0 },
    { timestamp: 1640995205, value: 14.0 },
    { timestamp: 1640995206, value: 16.0 },
    { timestamp: 1640995207, value: 18.0 },
    { timestamp: 1640995208, value: 17.0 },
    { timestamp: 1640995209, value: 19.0 },
    { timestamp: 1640995210, value: 20.0 },
    { timestamp: 1640995211, value: 22.0 }
  ]
  
  // 采样函数
  let sample_data = fn(data, interval) {
    let mut sampled_data = []
    let mut i = 0
    while i < data.length() {
      sampled_data = sampled_data.push(data[i])
      i = i + interval
    }
    sampled_data
  }
  
  // 聚合采样函数
  let aggregate_sampled_data = fn(data, window_size) {
    let mut aggregated_data = []
    let mut i = 0
    while i < data.length() {
      let end_index = if i + window_size > data.length() { data.length() } else { i + window_size }
      let window_data = data.slice(i, end_index)
      
      // 计算窗口内的平均值
      let mut sum = 0.0
      for point in window_data {
        sum = sum + point.value
      }
      let avg = sum / window_data.length().to_float()
      
      aggregated_data = aggregated_data.push({
        timestamp: window_data[0].timestamp,
        avg_value: avg,
        count: window_data.length()
      })
      
      i = i + window_size
    }
    aggregated_data
  }
  
  // 每3个数据点采样一次
  let sampled_data = sample_data(high_frequency_data, 3)
  
  // 验证采样结果
  assert_eq(sampled_data.length(), 4) // 12/3=4
  assert_eq(sampled_data[0].timestamp, 1640995200)
  assert_eq(sampled_data[0].value, 10.0)
  assert_eq(sampled_data[1].timestamp, 1640995203)
  assert_eq(sampled_data[1].value, 13.0)
  assert_eq(sampled_data[2].timestamp, 1640995206)
  assert_eq(sampled_data[2].value, 16.0)
  assert_eq(sampled_data[3].timestamp, 1640995209)
  assert_eq(sampled_data[3].value, 19.0)
  
  // 对采样数据进行聚合（窗口大小为2）
  let aggregated_data = aggregate_sampled_data(sampled_data, 2)
  
  // 验证聚合结果
  assert_eq(aggregated_data.length(), 2)
  
  // 验证第一个聚合窗口 (10.0, 13.0)
  assert_eq(aggregated_data[0].timestamp, 1640995200)
  assert_eq(aggregated_data[0].avg_value, 11.5) // (10+13)/2
  assert_eq(aggregated_data[0].count, 2)
  
  // 验证第二个聚合窗口 (16.0, 19.0)
  assert_eq(aggregated_data[1].timestamp, 1640995206)
  assert_eq(aggregated_data[1].avg_value, 17.5) // (16+19)/2
  assert_eq(aggregated_data[1].count, 2)
  
  // 计算数据压缩率
  let original_size = high_frequency_data.length()
  let final_size = aggregated_data.length()
  let compression_rate = ((original_size - final_size).to_float() / original_size.to_float()) * 100.0
  
  assert_eq(compression_rate, 83.33333333333334) // (12-2)/12*100
}