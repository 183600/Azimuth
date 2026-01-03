// Azimuth 高级遥测数据处理测试用例
// 专注于测试遥测数据的高级处理、转换和分析功能

// 测试1: 遥测数据流处理管道
test "遥测数据流处理管道测试" {
  // 创建模拟遥测数据点
  let data_points = [
    { timestamp: 1640995200000, metric_name: "cpu.usage", value: 45.2, tags: ["host:server1", "region:us-east"] },
    { timestamp: 1640995260000, metric_name: "cpu.usage", value: 48.7, tags: ["host:server1", "region:us-east"] },
    { timestamp: 1640995320000, metric_name: "memory.usage", value: 67.3, tags: ["host:server1", "region:us-east"] },
    { timestamp: 1640995380000, metric_name: "cpu.usage", value: 52.1, tags: ["host:server2", "region:us-west"] },
    { timestamp: 1640995440000, metric_name: "memory.usage", value: 71.8, tags: ["host:server2", "region:us-west"] }
  ]
  
  // 验证数据点数量
  assert_eq(data_points.length(), 5)
  
  // 按指标名称分组
  let grouped_by_metric = data_points.group_by(|point| point.metric_name)
  
  // 验证分组结果
  assert_eq(grouped_by_metric.size(), 2)
  assert_true(grouped_by_metric.contains_key("cpu.usage"))
  assert_true(grouped_by_metric.contains_key("memory.usage"))
  
  // 验证CPU使用率数据点
  let cpu_points = grouped_by_metric.get("cpu.usage")
  match cpu_points {
    Some(points) => {
      assert_eq(points.length(), 3)
      assert_eq(points[0].value, 45.2)
      assert_eq(points[1].value, 48.7)
      assert_eq(points[2].value, 52.1)
    }
    None => assert_true(false)
  }
  
  // 验证内存使用率数据点
  let memory_points = grouped_by_metric.get("memory.usage")
  match memory_points {
    Some(points) => {
      assert_eq(points.length(), 2)
      assert_eq(points[0].value, 67.3)
      assert_eq(points[1].value, 71.8)
    }
    None => assert_true(false)
  }
}

// 测试2: 时间窗口聚合计算
test "时间窗口聚合计算测试" {
  // 创建时间序列数据
  let time_series = [
    { timestamp: 1640995200000, value: 10.5 },
    { timestamp: 1640995260000, value: 12.3 },
    { timestamp: 1640995320000, value: 11.8 },
    { timestamp: 1640995380000, value: 13.2 },
    { timestamp: 1640995440000, value: 14.7 },
    { timestamp: 1640995500000, value: 15.1 }
  ]
  
  // 计算时间窗口内的平均值（窗口大小：3个数据点）
  let window_size = 3
  let moving_averages = []
  
  for i = 0; i <= time_series.length() - window_size; i = i + 1 {
    let window_sum = 0.0
    for j = i; j < i + window_size; j = j + 1 {
      window_sum = window_sum + time_series[j].value
    }
    let avg = window_sum / window_size.to_int()
    moving_averages.push(avg)
  }
  
  // 验证移动平均结果
  assert_eq(moving_averages.length(), 4)
  assert_eq(moving_averages[0], (10.5 + 12.3 + 11.8) / 3.0)
  assert_eq(moving_averages[1], (12.3 + 11.8 + 13.2) / 3.0)
  assert_eq(moving_averages[2], (11.8 + 13.2 + 14.7) / 3.0)
  assert_eq(moving_averages[3], (13.2 + 14.7 + 15.1) / 3.0)
}

// 测试3: 异常检测算法
test "异常检测算法测试" {
  // 创建包含异常的指标数据
  let metrics_data = [23.5, 24.1, 23.8, 24.2, 23.9, 45.7, 24.0, 23.7, 24.3, 23.6]
  
  // 计算均值和标准差
  let sum = 0.0
  for value in metrics_data {
    sum = sum + value
  }
  let mean = sum / metrics_data.length().to_int()
  
  let variance_sum = 0.0
  for value in metrics_data {
    let diff = value - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / metrics_data.length().to_int()
  let std_dev = sqrt(variance)
  
  // 使用3σ规则检测异常
  let threshold = 3.0 * std_dev
  let anomalies = []
  
  for i = 0; i < metrics_data.length(); i = i + 1 {
    let value = metrics_data[i]
    let z_score = abs(value - mean) / std_dev
    if z_score > 3.0 {
      anomalies.push({ index: i, value: value, z_score: z_score })
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].index, 5)  // 第6个数据点是异常
  assert_eq(anomalies[0].value, 45.7)
  assert_true(anomalies[0].z_score > 3.0)
}

// 测试4: 指标数据降采样
test "指标数据降采样测试" {
  // 创建高频率时间序列数据（每分钟一个数据点）
  let high_freq_data = []
  let base_timestamp = 1640995200000  // 2022-01-01 00:00:00
  
  for i = 0; i < 60; i = i + 1 {
    high_freq_data.push({
      timestamp: base_timestamp + i * 60000,  // 每分钟
      value: 50.0 + (i % 10).to_int().to_float() * 2.0
    })
  }
  
  // 降采样到每小时（取每小时的最大值）
  let downsampled_data = []
  let hour_in_ms = 3600000  // 一小时的毫秒数
  
  for i = 0; i < high_freq_data.length(); i = i + 60 {
    let hour_start = high_freq_data[i].timestamp
    let hour_end = hour_start + hour_in_ms
    
    // 获取当前小时的所有数据点
    let hour_data = []
    for j = i; j < high_freq_data.length() && high_freq_data[j].timestamp < hour_end; j = j + 1 {
      hour_data.push(high_freq_data[j].value)
    }
    
    // 计算最大值
    let max_value = hour_data[0]
    for value in hour_data {
      if value > max_value {
        max_value = value
      }
    }
    
    downsampled_data.push({
      timestamp: hour_start,
      value: max_value
    })
  }
  
  // 验证降采样结果
  assert_eq(downsampled_data.length(), 1)  // 60分钟的数据降采样为1小时
  assert_eq(downsampled_data[0].timestamp, 1640995200000)
  assert_eq(downsampled_data[0].value, 68.0)  // 50 + 9*2 = 68
}

// 测试5: 多维度指标分析
test "多维度指标分析测试" {
  // 创建带有多维标签的指标数据
  let multi_dim_metrics = [
    { metric: "response_time", value: 120, tags: { service: "api", region: "us-east", version: "v1.0" } },
    { metric: "response_time", value: 95, tags: { service: "api", region: "us-east", version: "v1.1" } },
    { metric: "response_time", value: 150, tags: { service: "web", region: "us-east", version: "v1.0" } },
    { metric: "response_time", value: 85, tags: { service: "api", region: "us-west", version: "v1.0" } },
    { metric: "throughput", value: 1000, tags: { service: "api", region: "us-east", version: "v1.0" } },
    { metric: "throughput", value: 1200, tags: { service: "api", region: "us-east", version: "v1.1" } }
  ]
  
  // 按服务分组计算平均响应时间
  let response_time_by_service = {}
  
  for metric_data in multi_dim_metrics {
    if metric_data.metric == "response_time" {
      let service = metric_data.tags.service
      let current_sum = response_time_by_service.get(service)
      match current_sum {
        Some((sum, count)) => {
          response_time_by_service.set(service, (sum + metric_data.value, count + 1))
        }
        None => {
          response_time_by_service.set(service, (metric_data.value, 1))
        }
      }
    }
  }
  
  // 计算平均值
  let avg_response_time_by_service = {}
  for (service, (sum, count)) in response_time_by_service {
    avg_response_time_by_service.set(service, sum / count.to_int())
  }
  
  // 验证分组聚合结果
  assert_eq(avg_response_time_by_service.size(), 2)
  
  let api_avg = avg_response_time_by_service.get("api")
  match api_avg {
    Some(avg) => assert_eq(avg, (120 + 95 + 85) / 3.0)
    None => assert_true(false)
  }
  
  let web_avg = avg_response_time_by_service.get("web")
  match web_avg {
    Some(avg) => assert_eq(avg, 150.0)
    None => assert_true(false)
  }
}