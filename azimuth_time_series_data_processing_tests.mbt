// Azimuth 时间序列数据处理测试用例
// 专注于时间序列数据的存储、查询和分析功能

// 测试1: 时间序列数据点创建
test "时间序列数据点创建和验证" {
  // 创建时间序列数据点
  let timestamp = 1640995200 // 2022-01-01 00:00:00
  let data_point = {
    "metric_name": "cpu.usage",
    "value": 75.5,
    "timestamp": timestamp,
    "tags": {
      "service": "api",
      "environment": "production",
      "instance": "api-server-1"
    },
    "type": "gauge"
  }
  
  // 验证数据点结构
  assert_eq(data_point["metric_name"], "cpu.usage")
  assert_eq(data_point["value"], 75.5)
  assert_eq(data_point["timestamp"], timestamp)
  assert_eq(data_point["type"], "gauge")
  assert_eq(data_point["tags"]["service"], "api")
  assert_eq(data_point["tags"]["environment"], "production")
  assert_eq(data_point["tags"]["instance"], "api-server-1")
  
  // 创建另一个时间点
  let next_timestamp = timestamp + 60 // 1分钟后
  let next_data_point = {
    "metric_name": "cpu.usage",
    "value": 78.2,
    "timestamp": next_timestamp,
    "tags": data_point["tags"],
    "type": "gauge"
  }
  
  // 验证时间递增
  assert_true(next_data_point["timestamp"] > data_point["timestamp"])
  assert_eq(next_data_point["timestamp"] - data_point["timestamp"], 60)
}

// 测试2: 时间序列数据排序
test "时间序列数据排序" {
  // 创建无序时间序列数据
  let unordered_series = [
    {"timestamp": 1640995380, "value": 65.8},
    {"timestamp": 1640995200, "value": 70.5},
    {"timestamp": 1640995320, "value": 80.2},
    {"timestamp": 1640995440, "value": 72.1},
    {"timestamp": 1640995260, "value": 75.5}
  ]
  
  // 按时间戳排序
  let ordered_series = unordered_series.sort_by(fn(a, b) { 
    if a["timestamp"] < b["timestamp"] { -1 } 
    else if a["timestamp"] > b["timestamp"] { 1 } 
    else { 0 } 
  })
  
  // 验证排序结果
  assert_eq(ordered_series[0]["timestamp"], 1640995200)
  assert_eq(ordered_series[1]["timestamp"], 1640995260)
  assert_eq(ordered_series[2]["timestamp"], 1640995320)
  assert_eq(ordered_series[3]["timestamp"], 1640995380)
  assert_eq(ordered_series[4]["timestamp"], 1640995440)
  
  // 验证值对应正确
  assert_eq(ordered_series[0]["value"], 70.5)
  assert_eq(ordered_series[1]["value"], 75.5)
  assert_eq(ordered_series[2]["value"], 80.2)
  assert_eq(ordered_series[3]["value"], 65.8)
  assert_eq(ordered_series[4]["value"], 72.1)
}

// 测试3: 时间范围查询
test "时间序列数据时间范围查询" {
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
  
  // 查询时间范围内的数据
  let start_time = 1640995260
  let end_time = 1640995500
  
  let range_data = time_series.filter(fn(point) { 
    point["timestamp"] >= start_time && point["timestamp"] <= end_time 
  })
  
  // 验证查询结果
  assert_eq(range_data.length(), 4)
  assert_eq(range_data[0]["timestamp"], 1640995260)
  assert_eq(range_data[3]["timestamp"], 1640995500)
  
  // 验证范围边界
  assert_true(range_data[0]["timestamp"] >= start_time)
  assert_true(range_data[range_data.length() - 1]["timestamp"] <= end_time)
  
  // 查询特定时间点之前的数据
  let before_data = time_series.filter(fn(point) { 
    point["timestamp"] < start_time 
  })
  
  assert_eq(before_data.length(), 1)
  assert_eq(before_data[0]["timestamp"], 1640995200)
  
  // 查询特定时间点之后的数据
  let after_data = time_series.filter(fn(point) { 
    point["timestamp"] > end_time 
  })
  
  assert_eq(after_data.length(), 3)
  assert_eq(after_data[0]["timestamp"], 1640995560)
}

// 测试4: 时间序列数据聚合
test "时间序列数据聚合计算" {
  // 创建时间序列数据
  let time_series = [
    {"timestamp": 1640995200, "value": 70.5},
    {"timestamp": 1640995260, "value": 75.5},
    {"timestamp": 1640995320, "value": 80.2},
    {"timestamp": 1640995380, "value": 65.8},
    {"timestamp": 1640995440, "value": 72.1},
    {"timestamp": 1640995500, "value": 78.9}
  ]
  
  // 计算总和
  let total = time_series.reduce(fn(acc, point) { acc + point["value"] }, 0.0)
  assert_eq(total.round(), 443.0)
  
  // 计算平均值
  let average = total / (time_series.length() as Float)
  assert_eq(average.round(), 74.0)
  
  // 计算最大值
  let max_value = time_series.reduce(fn(acc, point) { 
    if point["value"] > acc { point["value"] } else { acc } 
  }, 0.0)
  assert_eq(max_value, 80.2)
  
  // 计算最小值
  let min_value = time_series.reduce(fn(acc, point) { 
    if point["value"] < acc { point["value"] } else { acc } 
  }, 999.0)
  assert_eq(min_value, 65.8)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range.round(), 14.0)
  
  // 查找最大值时间戳
  let max_point = time_series.filter(fn(point) { point["value"] == max_value })[0]
  assert_eq(max_point["timestamp"], 1640995320)
  
  // 查找最小值时间戳
  let min_point = time_series.filter(fn(point) { point["value"] == min_value })[0]
  assert_eq(min_point["timestamp"], 1640995380)
}

// 测试5: 时间序列数据插值
test "时间序列数据插值处理" {
  // 创建有缺失的时间序列数据
  let sparse_series = [
    {"timestamp": 1640995200, "value": 70.5},
    {"timestamp": 1640995320, "value": 80.2}, // 缺失1640995260
    {"timestamp": 1640995440, "value": 72.1},
    {"timestamp": 1640995560, "value": 82.3}, // 缺失1640995500
    {"timestamp": 1640995680, "value": 68.7}
  ]
  
  // 线性插值函数 (简化版)
  let linear_interpolate = fn(point1, point2, target_timestamp) {
    let time_diff = point2["timestamp"] - point1["timestamp"]
    let value_diff = point2["value"] - point1["value"]
    let ratio = (target_timestamp - point1["timestamp"]) as Float / (time_diff as Float)
    point1["value"] + value_diff * ratio
  }
  
  // 检测缺失的时间点
  let expected_interval = 120 // 2分钟
  let interpolated_points = []
  
  for i in 0..=(sparse_series.length() - 2) {
    let current = sparse_series[i]
    let next = sparse_series[i + 1]
    
    // 添加当前点
    interpolated_points = interpolated_points.push(current)
    
    // 检查是否有缺失点
    let time_gap = next["timestamp"] - current["timestamp"]
    if time_gap > expected_interval {
      let missing_timestamp = current["timestamp"] + expected_interval
      let interpolated_value = linear_interpolate(current, next, missing_timestamp)
      
      interpolated_points = interpolated_points.push({
        "timestamp": missing_timestamp,
        "value": interpolated_value,
        "interpolated": true
      })
    }
  }
  
  // 添加最后一个点
  interpolated_points = interpolated_points.push(sparse_series[sparse_series.length() - 1])
  
  // 验证插值结果
  assert_eq(interpolated_points.length(), 7) // 原始5个点 + 2个插值点
  
  // 验证插值点
  let interpolated_data = interpolated_points.filter(fn(p) { p["interpolated"] == true })
  assert_eq(interpolated_data.length(), 2)
  
  // 验证第一个插值点 (1640995260)
  let first_interp = interpolated_data[0]
  assert_eq(first_interp["timestamp"], 1640995260)
  
  // 计算期望值: (70.5 + 80.2) / 2 = 75.35
  assert_eq(first_interp["value"].round(), 75.0)
}

// 测试6: 时间序列数据滑动窗口计算
test "时间序列数据滑动窗口计算" {
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
  
  // 定义滑动窗口大小 (3个点)
  let window_size = 3
  
  // 计算滑动平均值
  let moving_averages = []
  for i in window_size..=(time_series.length() - 1) {
    let window_start = i - window_size
    let window_data = time_series.slice(window_start, window_size)
    
    let window_sum = window_data.reduce(fn(acc, point) { acc + point["value"] }, 0.0)
    let window_avg = window_sum / (window_size as Float)
    
    moving_averages = moving_averages.push({
      "timestamp": time_series[i]["timestamp"],
      "moving_average": window_avg
    })
  }
  
  // 验证滑动平均值计算
  assert_eq(moving_averages.length(), 5)
  
  // 验证第一个滑动平均值 (点0,1,2)
  let first_ma = moving_averages[0]
  assert_eq(first_ma["timestamp"], 1640995320)
  // (70.5 + 75.5 + 80.2) / 3 = 75.4
  assert_eq(first_ma["moving_average"].round(), 75.0)
  
  // 验证最后一个滑动平均值 (点5,6,7)
  let last_ma = moving_averages[moving_averages.length() - 1]
  assert_eq(last_ma["timestamp"], 1640995620)
  // (78.9 + 82.3 + 68.7) / 3 = 76.63
  assert_eq(last_ma["moving_average"].round(), 77.0)
}

// 测试7: 时间序列数据趋势分析
test "时间序列数据趋势分析" {
  // 创建有明显趋势的时间序列数据
  let trending_series = [
    {"timestamp": 1640995200, "value": 70.5},
    {"timestamp": 1640995260, "value": 72.1},
    {"timestamp": 1640995320, "value": 75.5},
    {"timestamp": 1640995380, "value": 78.9},
    {"timestamp": 1640995440, "value": 82.3},
    {"timestamp": 1640995500, "value": 85.7},
    {"timestamp": 1640995560, "value": 89.1},
    {"timestamp": 1640995620, "value": 92.5}
  ]
  
  // 计算简单线性回归 (简化版)
  let n = trending_series.length() as Float
  let sum_x = trending_series.reduce(fn(acc, point, i) { acc + i as Float }, 0.0)
  let sum_y = trending_series.reduce(fn(acc, point) { acc + point["value"] }, 0.0)
  let sum_xy = trending_series.reduce(fn(acc, point, i) { 
    acc + (i as Float) * point["value"] 
  }, 0.0)
  let sum_x2 = trending_series.reduce(fn(acc, point, i) { 
    acc + (i as Float) * (i as Float) 
  }, 0.0)
  
  // 计算斜率和截距
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // 验证上升趋势 (正斜率)
  assert_true(slope > 0.0)
  assert_eq(slope.round(), 3.0)
  
  // 预测下一个值
  let next_index = trending_series.length()
  let predicted_value = slope * (next_index as Float) + intercept
  assert_eq(predicted_value.round(), 96.0)
  
  // 计算相关系数 (简化版)
  let mean_x = sum_x / n
  let mean_y = sum_y / n
  
  let numerator = trending_series.reduce(fn(acc, point, i) { 
    let x_dev = (i as Float) - mean_x
    let y_dev = point["value"] - mean_y
    acc + x_dev * y_dev
  }, 0.0)
  
  let sum_x_dev_sq = trending_series.reduce(fn(acc, point, i) { 
    let x_dev = (i as Float) - mean_x
    acc + x_dev * x_dev
  }, 0.0)
  
  let sum_y_dev_sq = trending_series.reduce(fn(acc, point) { 
    let y_dev = point["value"] - mean_y
    acc + y_dev * y_dev
  }, 0.0)
  
  let correlation = numerator / (sum_x_dev_sq * sum_y_dev_sq).sqrt()
  
  // 验证强正相关
  assert_true(correlation > 0.9)
  assert_eq(correlation.round(), 1.0)
}