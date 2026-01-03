// Azimuth 遥测数据处理测试用例
// 专注于遥测数据的收集、处理和分析功能

// 测试1: 遥测数据点创建和验证
test "遥测数据点创建和验证" {
  // 创建CPU使用率数据点
  let cpu_metric = {
    "name": "cpu.usage",
    "value": 75.5,
    "timestamp": 1640995200,
    "tags": ["service:api", "env:production"],
    "type": "gauge"
  }
  
  // 验证数据点字段
  assert_eq(cpu_metric["name"], "cpu.usage")
  assert_eq(cpu_metric["value"], 75.5)
  assert_eq(cpu_metric["timestamp"], 1640995200)
  assert_eq(cpu_metric["type"], "gauge")
  assert_true(cpu_metric["tags"].length() > 0)
  
  // 创建内存使用率数据点
  let memory_metric = {
    "name": "memory.usage",
    "value": 60.2,
    "timestamp": 1640995200,
    "tags": ["service:api", "env:production"],
    "type": "gauge"
  }
  
  // 验证不同指标的数据点
  assert_eq(memory_metric["name"], "memory.usage")
  assert_eq(memory_metric["value"], 60.2)
  assert_eq(memory_metric["timestamp"], cpu_metric["timestamp"])
  assert_eq(memory_metric["type"], cpu_metric["type"])
}

// 测试2: 遥测数据聚合
test "遥测数据聚合计算" {
  // 创建多个CPU使用率数据点
  let cpu_metrics = [
    {"name": "cpu.usage", "value": 70.5, "timestamp": 1640995200},
    {"name": "cpu.usage", "value": 75.5, "timestamp": 1640995260},
    {"name": "cpu.usage", "value": 80.2, "timestamp": 1640995320},
    {"name": "cpu.usage", "value": 65.8, "timestamp": 1640995380},
    {"name": "cpu.usage", "value": 72.1, "timestamp": 1640995440}
  ]
  
  // 计算平均值
  let total = cpu_metrics.reduce(fn(acc, metric) { acc + metric["value"] }, 0.0)
  let average = total / (cpu_metrics.length() as Float)
  assert_eq(average.round(), 73.0)
  
  // 计算最大值
  let max_value = cpu_metrics.reduce(fn(acc, metric) { 
    if metric["value"] > acc { metric["value"] } else { acc } 
  }, 0.0)
  assert_eq(max_value, 80.2)
  
  // 计算最小值
  let min_value = cpu_metrics.reduce(fn(acc, metric) { 
    if metric["value"] < acc { metric["value"] } else { acc } 
  }, 999.0)
  assert_eq(min_value, 65.8)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range.round(), 14.0)
}

// 测试3: 遥测数据过滤和选择
test "遥测数据过滤和选择" {
  // 创建混合类型的遥测数据
  let metrics = [
    {"name": "cpu.usage", "value": 75.5, "service": "api", "env": "prod"},
    {"name": "memory.usage", "value": 60.2, "service": "api", "env": "prod"},
    {"name": "response.time", "value": 120.0, "service": "web", "env": "prod"},
    {"name": "error.rate", "value": 0.5, "service": "api", "env": "staging"},
    {"name": "throughput", "value": 1500.0, "service": "web", "env": "prod"}
  ]
  
  // 按服务过滤
  let api_metrics = metrics.filter(fn(metric) { metric["service"] == "api" })
  assert_eq(api_metrics.length(), 3)
  
  // 按环境过滤
  let prod_metrics = metrics.filter(fn(metric) { metric["env"] == "prod" })
  assert_eq(prod_metrics.length(), 4)
  
  // 按值范围过滤
  let high_usage = metrics.filter(fn(metric) { 
    metric["name"] == "cpu.usage" && metric["value"] > 70.0 
  })
  assert_eq(high_usage.length(), 1)
  
  // 复合条件过滤
  let api_prod_metrics = metrics.filter(fn(metric) { 
    metric["service"] == "api" && metric["env"] == "prod" 
  })
  assert_eq(api_prod_metrics.length(), 2)
}

// 测试4: 遥测数据转换和映射
test "遥测数据转换和映射" {
  // 创建原始遥测数据
  let raw_metrics = [
    {"name": "cpu.usage", "value": 75.5, "unit": "percent"},
    {"name": "memory.total", "value": 8589934592, "unit": "bytes"},
    {"name": "disk.free", "value": 107374182400, "unit": "bytes"},
    {"name": "network.in", "value": 1048576, "unit": "bytes"}
  ]
  
  // 转换字节数据为GB
  let converted_metrics = raw_metrics.map(fn(metric) {
    if metric["unit"] == "bytes" {
      {
        "name": metric["name"],
        "value": metric["value"] / (1024.0 * 1024.0 * 1024.0),
        "unit": "GB"
      }
    } else {
      metric
    }
  })
  
  // 验证转换结果
  let memory_gb = converted_metrics.filter(fn(m) { m["name"] == "memory.total" })[0]
  assert_eq(memory_gb["unit"], "GB")
  assert_eq(memory_gb["value"].round(), 8.0)
  
  let disk_gb = converted_metrics.filter(fn(m) { m["name"] == "disk.free" })[0]
  assert_eq(disk_gb["unit"], "GB")
  assert_eq(disk_gb["value"].round(), 100.0)
  
  // CPU指标应该保持不变
  let cpu_metric = converted_metrics.filter(fn(m) { m["name"] == "cpu.usage" })[0]
  assert_eq(cpu_metric["unit"], "percent")
  assert_eq(cpu_metric["value"], 75.5)
}

// 测试5: 遥测数据时间窗口处理
test "遥测数据时间窗口处理" {
  // 创建时间序列数据
  let time_series = [
    {"timestamp": 1640995200, "value": 70.5},
    {"timestamp": 1640995260, "value": 75.5},
    {"timestamp": 1640995320, "value": 80.2},
    {"timestamp": 1640995380, "value": 65.8},
    {"timestamp": 1640995440, "value": 72.1},
    {"timestamp": 1640995500, "value": 78.9},
    {"timestamp": 1640995560, "value": 82.3},
    {"timestamp": 1640995620, "value": 68.7}
  ]
  
  // 定义时间窗口 (开始时间到结束时间)
  let window_start = 1640995260
  let window_end = 1640995500
  
  // 过滤时间窗口内的数据
  let window_data = time_series.filter(fn(point) { 
    point["timestamp"] >= window_start && point["timestamp"] <= window_end 
  })
  
  // 验证时间窗口数据
  assert_eq(window_data.length(), 4)
  assert_eq(window_data[0]["timestamp"], 1640995260)
  assert_eq(window_data[3]["timestamp"], 1640995500)
  
  // 计算窗口内的平均值
  let window_avg = window_data.reduce(fn(acc, point) { 
    acc + point["value"] 
  }, 0.0) / (window_data.length() as Float)
  
  assert_eq(window_avg.round(), 74.0)
  
  // 计算窗口内的趋势 (简单线性趋势)
  let first_value = window_data[0]["value"]
  let last_value = window_data[window_data.length() - 1]["value"]
  let trend = last_value - first_value
  
  assert_eq(trend.round(), -3.0)
}

// 测试6: 遥测数据异常检测
test "遥测数据异常检测" {
  // 创建包含异常值的数据集
  let metrics = [
    {"name": "response.time", "value": 120.0},
    {"name": "response.time", "value": 125.0},
    {"name": "response.time", "value": 118.0},
    {"name": "response.time", "value": 500.0}, // 异常值
    {"name": "response.time", "value": 122.0},
    {"name": "response.time", "value": 119.0},
    {"name": "response.time", "value": 800.0}, // 异常值
    {"name": "response.time", "value": 121.0}
  ]
  
  // 计算平均值和标准差 (简化版)
  let values = metrics.map(fn(m) { m["value"] })
  let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
  
  // 计算方差
  let variance = values.reduce(fn(acc, v) { 
    let diff = v - mean
    acc + diff * diff
  }, 0.0) / (values.length() as Float)
  
  // 标准差
  let std_dev = variance.sqrt()
  
  // 定义异常阈值 (平均值 ± 2倍标准差)
  let anomaly_threshold = 2.0 * std_dev
  let upper_bound = mean + anomaly_threshold
  let lower_bound = mean - anomaly_threshold
  
  // 检测异常值
  let anomalies = metrics.filter(fn(m) { 
    m["value"] > upper_bound || m["value"] < lower_bound 
  })
  
  // 验证异常检测
  assert_eq(anomalies.length(), 2)
  assert_true(anomalies[0]["value"] > upper_bound)
  assert_true(anomalies[1]["value"] > upper_bound)
  
  // 验证正常值
  let normal_values = metrics.filter(fn(m) { 
    m["value"] <= upper_bound && m["value"] >= lower_bound 
  })
  assert_eq(normal_values.length(), 6)
}

// 测试7: 遥测数据采样策略
test "遥测数据采样策略" {
  // 创建高频遥测数据
  let high_frequency_data = []
  for i in 0..=100 {
    high_frequency_data = high_frequency_data.push({
      "timestamp": 1640995200 + i * 10,
      "value": 70.0 + (i % 20) as Float * 0.5
    })
  }
  
  assert_eq(high_frequency_data.length(), 101)
  
  // 实现简单随机采样 (采样率10%)
  let sample_rate = 0.1
  let sampled_data = high_frequency_data.filter(fn(_) { 
    // 简化的伪随机采样，实际应用中应使用真正的随机数
    true 
  }).filter_with_index(fn(i, _) { i % 10 == 0 })
  
  assert_eq(sampled_data.length(), 11)
  
  // 实现系统采样
  let systematic_interval = 10
  let systematic_sampled = high_frequency_data.filter_with_index(fn(i, _) { 
    i % systematic_interval == 0 
  })
  
  assert_eq(systematic_sampled.length(), 11)
  assert_eq(systematic_sampled[0]["timestamp"], 1640995200)
  assert_eq(systematic_sampled[10]["timestamp"], 1640995200 + 100 * 10)
  
  // 实现基于值的采样 (只保留高值)
  let value_threshold = 75.0
  let value_sampled = high_frequency_data.filter(fn(point) { 
    point["value"] >= value_threshold 
  })
  
  assert_true(value_sampled.length() > 0)
  assert_true(value_sampled.length() < high_frequency_data.length())
  
  // 验证所有采样值都超过阈值
  let all_above_threshold = value_sampled.all(fn(point) { 
    point["value"] >= value_threshold 
  })
  assert_true(all_above_threshold)
}