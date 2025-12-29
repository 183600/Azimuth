// 时间窗口和滑动窗口测试用例
// 验证遥测数据的时间窗口处理和滑动窗口计算功能

test "fixed_time_window_aggregation" {
  // 测试固定时间窗口聚合
  
  let telemetry_events = [
    {"timestamp": 1640995200000L, "metric": "response_time", "value": 100.0, "service": "api"},
    {"timestamp": 1640995201000L, "metric": "response_time", "value": 120.0, "service": "api"},
    {"timestamp": 1640995202000L, "metric": "response_time", "value": 80.0, "service": "api"},
    {"timestamp": 1640995203000L, "metric": "response_time", "value": 150.0, "service": "api"},
    {"timestamp": 1640995204000L, "metric": "response_time", "value": 90.0, "service": "api"},
    {"timestamp": 1640995205000L, "metric": "response_time", "value": 110.0, "service": "api"},
    {"timestamp": 1640995206000L, "metric": "response_time", "value": 130.0, "service": "api"},
    {"timestamp": 1640995207000L, "metric": "response_time", "value": 95.0, "service": "api"}
  ]
  
  let window_size_ms = 3000L // 3秒窗口
  let window_start = 1640995202000L
  let window_end = window_start + window_size_ms
  
  // 聚合窗口内的事件
  let window_events = []
  let mut i = 0
  while i < telemetry_events.length() {
    let event = telemetry_events[i]
    let timestamp = event["timestamp"]
    
    if timestamp >= window_start && timestamp < window_end {
      window_events.push(event)
    }
    i = i + 1
  }
  
  // 计算窗口统计
  let mut sum_values = 0.0
  let mut count_values = 0
  let mut min_value = 999999.0
  let mut max_value = -1.0
  
  let mut j = 0
  while j < window_events.length() {
    let value = window_events[j]["value"]
    sum_values = sum_values + value
    count_values = count_values + 1
    
    if value < min_value {
      min_value = value
    }
    if value > max_value {
      max_value = value
    }
    
    j = j + 1
  }
  
  let average_value = sum_values / count_values.to_double()
  
  // 验证窗口聚合结果
  assert_eq(window_events.length(), 3) // 应该有3个事件在窗口内
  assert_eq(window_events[0]["timestamp"], 1640995202000L)
  assert_eq(window_events[1]["timestamp"], 1640995203000L)
  assert_eq(window_events[2]["timestamp"], 1640995204000L)
  
  assert_eq(average_value, (80.0 + 150.0 + 90.0) / 3.0) // 106.666...
  assert_eq(min_value, 80.0)
  assert_eq(max_value, 150.0)
}

test "sliding_window_computation" {
  // 测试滑动窗口计算
  
  let metric_values = [
    {"timestamp": 1000L, "value": 10.0},
    {"timestamp": 2000L, "value": 15.0},
    {"timestamp": 3000L, "value": 12.0},
    {"timestamp": 4000L, "value": 18.0},
    {"timestamp": 5000L, "value": 14.0},
    {"timestamp": 6000L, "value": 20.0},
    {"timestamp": 7000L, "value": 16.0},
    {"timestamp": 8000L, "value": 22.0}
  ]
  
  let window_size_ms = 4000L // 4秒滑动窗口
  let slide_step_ms = 1000L  // 每秒滑动一次
  
  let sliding_windows = []
  let mut current_time = 2000L // 从第2秒开始
  
  while current_time <= 7000L { // 到第7秒结束
    let window_start = current_time - window_size_ms
    let window_end = current_time
    
    // 收集窗口内的数据
    let window_data = []
    let mut i = 0
    while i < metric_values.length() {
      let data_point = metric_values[i]
      let timestamp = data_point["timestamp"]
      
      if timestamp > window_start && timestamp <= window_end {
        window_data.push(data_point["value"])
      }
      
      i = i + 1
    }
    
    // 计算窗口统计
    let mut sum_values = 0.0
    let mut j = 0
    while j < window_data.length() {
      sum_values = sum_values + window_data[j]
      j = j + 1
    }
    
    let average_value = if window_data.length() > 0 { 
      sum_values / window_data.length().to_double() 
    } else { 
      0.0 
    }
    
    sliding_windows.push({
      "window_time": current_time,
      "window_start": window_start,
      "window_end": window_end,
      "data_count": window_data.length(),
      "average": average_value,
      "data": window_data
    })
    
    current_time = current_time + slide_step_ms
  }
  
  // 验证滑动窗口结果
  assert_eq(sliding_windows.length(), 6) // 应该有6个滑动窗口
  
  // 验证第一个窗口 (时间2s, 窗口范围-2s到2s)
  assert_eq(sliding_windows[0]["window_time"], 2000L)
  assert_eq(sliding_windows[0]["data_count"], 2) // 10.0, 15.0
  assert_eq(sliding_windows[0]["average"], (10.0 + 15.0) / 2.0)
  
  // 验证中间窗口 (时间5s, 窗口范围1s到5s)
  assert_eq(sliding_windows[3]["window_time"], 5000L)
  assert_eq(sliding_windows[3]["data_count"], 4) // 15.0, 12.0, 18.0, 14.0
  assert_eq(sliding_windows[3]["average"], (15.0 + 12.0 + 18.0 + 14.0) / 4.0)
  
  // 验证最后一个窗口 (时间7s, 窗口范围3s到7s)
  assert_eq(sliding_windows[5]["window_time"], 7000L)
  assert_eq(sliding_windows[5]["data_count"], 4) // 12.0, 18.0, 14.0, 20.0
  assert_eq(sliding_windows[5]["average"], (12.0 + 18.0 + 14.0 + 20.0) / 4.0)
}

test "time_window_percentile_calculation" {
  // 测试时间窗口百分位数计算
  
  let response_times = [
    {"timestamp": 1640995200000L, "value": 45.0},
    {"timestamp": 1640995201000L, "value": 78.0},
    {"timestamp": 1640995202000L, "value": 23.0},
    {"timestamp": 1640995203000L, "value": 89.0},
    {"timestamp": 1640995204000L, "value": 56.0},
    {"timestamp": 1640995205000L, "value": 34.0},
    {"timestamp": 1640995206000L, "value": 67.0},
    {"timestamp": 1640995207000L, "value": 12.0},
    {"timestamp": 1640995208000L, "value": 90.0},
    {"timestamp": 1640995209000L, "value": 41.0}
  ]
  
  let window_start = 1640995202000L
  let window_end = 1640995208000L
  
  // 收集窗口内的响应时间
  let window_values = []
  let mut i = 0
  while i < response_times.length() {
    let data_point = response_times[i]
    let timestamp = data_point["timestamp"]
    
    if timestamp >= window_start && timestamp <= window_end {
      window_values.push(data_point["value"])
    }
    
    i = i + 1
  }
  
  // 排序响应时间
  let mut j = 0
  while j < window_values.length() {
    let mut k = j + 1
    while k < window_values.length() {
      if window_values[j] > window_values[k] {
        let temp = window_values[j]
        window_values[j] = window_values[k]
        window_values[k] = temp
      }
      k = k + 1
    }
    j = j + 1
  }
  
  // 计算百分位数
  let p50_index = (window_values.length() * 50) / 100
  let p95_index = (window_values.length() * 95) / 100
  let p99_index = (window_values.length() * 99) / 100
  
  let p50_value = if p50_index < window_values.length() { window_values[p50_index] } else { 0.0 }
  let p95_value = if p95_index < window_values.length() { window_values[p95_index] } else { 0.0 }
  let p99_value = if p99_index < window_values.length() { window_values[p99_index] } else { 0.0 }
  
  // 验证百分位数计算
  assert_eq(window_values.length(), 6) // 应该有6个值在窗口内
  
  // 手动验证排序结果
  let expected_sorted = [12.0, 23.0, 34.0, 56.0, 67.0, 89.0]
  let mut j = 0
  while j < window_values.length() {
    assert_eq(window_values[j], expected_sorted[j])
    j = j + 1
  }
  
  // 验证百分位数
  assert_eq(p50_value, 34.0) // 第50百分位数
  assert_eq(p95_value, 89.0) // 第95百分位数
  assert_eq(p99_value, 89.0) // 第99百分位数
}

test "time_window_rate_calculation" {
  // 测试时间窗口速率计算
  
  let request_events = [
    {"timestamp": 1640995200000L, "status": 200},
    {"timestamp": 1640995200100L, "status": 200},
    {"timestamp": 1640995200200L, "status": 404},
    {"timestamp": 1640995200300L, "status": 200},
    {"timestamp": 1640995200400L, "status": 500},
    {"timestamp": 1640995200500L, "status": 200},
    {"timestamp": 1640995200600L, "status": 200},
    {"timestamp": 1640995200700L, "status": 200},
    {"timestamp": 1640995200800L, "status": 404},
    {"timestamp": 1640995200900L, "status": 200}
  ]
  
  let window_duration_ms = 5000L // 5秒窗口
  let window_start = 1640995200200L
  let window_end = window_start + window_duration_ms
  
  // 统计窗口内的事件
  let mut total_requests = 0
  let mut success_requests = 0
  let mut error_requests = 0
  
  let mut i = 0
  while i < request_events.length() {
    let event = request_events[i]
    let timestamp = event["timestamp"]
    
    if timestamp >= window_start && timestamp <= window_end {
      total_requests = total_requests + 1
      
      if event["status"] == 200 {
        success_requests = success_requests + 1
      } else {
        error_requests = error_requests + 1
      }
    }
    
    i = i + 1
  }
  
  // 计算速率
  let window_duration_seconds = window_duration_ms.to_double() / 1000.0
  let total_rate = total_requests.to_double() / window_duration_seconds
  let success_rate = success_requests.to_double() / window_duration_seconds
  let error_rate = error_requests.to_double() / window_duration_seconds
  let success_percentage = if total_requests > 0 { 
    (success_requests.to_double() / total_requests.to_double()) * 100.0 
  } else { 
    0.0 
  }
  
  // 验证速率计算
  assert_eq(total_requests, 7) // 应该有7个请求在5秒窗口内
  assert_eq(success_requests, 5) // 5个成功请求
  assert_eq(error_requests, 2)   // 2个错误请求
  
  assert_eq(total_rate, 7.0 / 5.0) // 1.4 requests per second
  assert_eq(success_rate, 5.0 / 5.0) // 1.0 success per second
  assert_eq(error_rate, 2.0 / 5.0)   // 0.4 error per second
  assert_eq(success_percentage, (5.0 / 7.0) * 100.0) // 71.428%
}

test "time_window_memory_efficiency" {
  // 测试时间窗口内存效率
  
  let event_stream_size = 10000
  let window_size_ms = 60000L // 1分钟窗口
  let max_memory_events = 1000 // 最大内存中事件数
  
  // 生成大量事件流
  let event_stream = []
  let mut i = 0
  while i < event_stream_size {
    event_stream.push({
      "id": i,
      "timestamp": 1640995200000L + (i * 100L), // 每100ms一个事件
      "value": (i % 100).to_double() // 0-99的循环值
    })
    i = i + 1
  }
  
  // 模拟内存高效的窗口处理
  let memory_window = []
  let processed_windows = []
  let mut current_window_start = 1640995200000L
  
  while current_window_start < 1640995200000L + (event_stream_size * 100L) {
    let current_window_end = current_window_start + window_size_ms
    
    // 清理超出窗口的事件
    let mut j = 0
    while j < memory_window.length() {
      if memory_window[j]["timestamp"] < current_window_start {
        memory_window.remove_at(j)
      } else {
        j = j + 1
      }
    }
    
    // 添加新窗口内的事件
    let mut k = 0
    while k < event_stream.length() {
      let event = event_stream[k]
      let timestamp = event["timestamp"]
      
      if timestamp >= current_window_start && timestamp < current_window_end {
        // 检查内存限制
        if memory_window.length() < max_memory_events {
          memory_window.push(event)
        }
      }
      
      k = k + 1
    }
    
    // 计算窗口统计
    let mut sum_values = 0.0
    let mut j = 0
    while j < memory_window.length() {
      sum_values = sum_values + memory_window[j]["value"]
      j = j + 1
    }
    
    let average_value = if memory_window.length() > 0 { 
      sum_values / memory_window.length().to_double() 
    } else { 
      0.0 
    }
    
    processed_windows.push({
      "window_start": current_window_start,
      "window_end": current_window_end,
      "event_count": memory_window.length(),
      "average": average_value,
      "memory_usage": memory_window.length()
    })
    
    current_window_start = current_window_end
  }
  
  // 验证内存效率
  assert_eq(processed_windows.length() > 0, true)
  
  // 验证内存使用不超过限制
  let mut j = 0
  while j < processed_windows.length() {
    assert_eq(processed_windows[j]["memory_usage"] <= max_memory_events, true)
    j = j + 1
  }
  
  // 验证第一个窗口的统计
  let first_window = processed_windows[0]
  assert_eq(first_window["window_start"], 1640995200000L)
  assert_eq(first_window["window_end"], 1640995200000L + window_size_ms)
  assert_eq(first_window["event_count"], 600) // 60秒/0.1秒 = 600个事件
}

test "time_window_boundary_handling" {
  // 测试时间窗口边界处理
  
  let boundary_events = [
    {"timestamp": 1640995200000L, "value": 10.0}, // 窗口开始边界
    {"timestamp": 1640995200999L, "value": 15.0}, // 窗口结束边界前1ms
    {"timestamp": 1640995201000L, "value": 20.0}, // 窗口结束边界（不包含）
    {"timestamp": 1640995201001L, "value": 25.0}, // 窗口外1ms
    {"timestamp": 1640995201999L, "value": 30.0}, // 下一个窗口结束边界前1ms
    {"timestamp": 1640995202000L, "value": 35.0}  // 下一个窗口结束边界（不包含）
  ]
  
  // 测试半开区间 [start, end)
  let window1_start = 1640995200000L
  let window1_end = 1640995201000L
  
  let window1_events = []
  let mut i = 0
  while i < boundary_events.length() {
    let event = boundary_events[i]
    let timestamp = event["timestamp"]
    
    if timestamp >= window1_start && timestamp < window1_end {
      window1_events.push(event)
    }
    
    i = i + 1
  }
  
  // 测试下一个窗口
  let window2_start = 1640995201000L
  let window2_end = 1640995202000L
  
  let window2_events = []
  i = 0
  while i < boundary_events.length() {
    let event = boundary_events[i]
    let timestamp = event["timestamp"]
    
    if timestamp >= window2_start && timestamp < window2_end {
      window2_events.push(event)
    }
    
    i = i + 1
  }
  
  // 验证边界处理
  assert_eq(window1_events.length(), 2) // 包含开始边界，不包含结束边界
  assert_eq(window1_events[0]["timestamp"], 1640995200000L) // 包含开始边界
  assert_eq(window1_events[1]["timestamp"], 1640995200999L) // 包含结束边界前1ms
  
  assert_eq(window2_events.length(), 2)
  assert_eq(window2_events[0]["timestamp"], 1640995201001L) // 不包含边界点
  assert_eq(window2_events[1]["timestamp"], 1640995201999L) // 包含结束边界前1ms
  
  // 验证边界点被正确分配
  let boundary_point_found = false
  i = 0
  while i < window1_events.length() {
    if window1_events[i]["timestamp"] == 1640995201000L {
      boundary_point_found = true
      break
    }
    i = i + 1
  }
  assert_eq(boundary_point_found, false) // 边界点不在第一个窗口
  
  boundary_point_found = false
  i = 0
  while i < window2_events.length() {
    if window2_events[i]["timestamp"] == 1640995201000L {
      boundary_point_found = true
      break
    }
    i = i + 1
  }
  assert_eq(boundary_point_found, false) // 边界点也不在第二个窗口（半开区间）
}