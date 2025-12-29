// 遥测数据实时流处理测试用例

test "telemetry_stream_window_processing" {
  // 测试流窗口处理
  
  let event_stream = [
    (1000, "request", 200),
    (1500, "request", 200),
    (2000, "error", 500),
    (2500, "request", 200),
    (3000, "request", 200),
    (3500, "request", 404),
    (4000, "request", 200),
    (4500, "error", 503),
    (5000, "request", 200),
    (5500, "request", 200)
  ]
  
  let window_size = 2000  // 2秒窗口
  
  // 流窗口处理
  let mut window_results = []
  let mut start_time = event_stream[0].0
  
  while start_time <= event_stream[event_stream.length() - 1].0 {
    let end_time = start_time + window_size
    
    // 统计窗口内的事件
    let mut request_count = 0
    let mut error_count = 0
    let mut success_count = 0
    
    let mut i = 0
    while i < event_stream.length() {
      let event_time = event_stream[i].0
      let event_type = event_stream[i].1
      let status_code = event_stream[i].2
      
      if event_time >= start_time && event_time < end_time {
        if event_type == "request" {
          request_count = request_count + 1
          if status_code >= 200 && status_code < 300 {
            success_count = success_count + 1
          } else if status_code >= 400 {
            error_count = error_count + 1
          }
        } else if event_type == "error" {
          error_count = error_count + 1
        }
      }
      i = i + 1
    }
    
    if request_count > 0 || error_count > 0 {
      let error_rate = if request_count > 0 {
        error_count.to_double() / request_count.to_double()
      } else {
        0.0
      }
      
      window_results = window_results.push((start_time, end_time, request_count, error_count, error_rate))
    }
    
    start_time = start_time + 1000  // 滑动窗口，每次前进1秒
  }
  
  // 验证流窗口处理结果
  assert_eq(window_results.length() > 0, true)
  
  // 验证第一个窗口 (1000-3000)
  assert_eq(window_results[0].0, 1000)
  assert_eq(window_results[0].1, 3000)
  assert_eq(window_results[0].2, 4)  // 4个请求
  assert_eq(window_results[0].3, 1)  // 1个错误
  assert_eq(window_results[0].4 > 0.2 && window_results[0].4 < 0.3, true)  // 错误率约25%
}

test "telemetry_stream_aggregation" {
  // 测试流聚合
  
  let metric_stream = [
    (100, "cpu", 45.2),
    (200, "memory", 67.8),
    (300, "cpu", 48.1),
    (400, "disk", 23.5),
    (500, "memory", 69.2),
    (600, "cpu", 46.8),
    (700, "disk", 24.1),
    (800, "memory", 68.5),
    (900, "cpu", 47.3),
    (1000, "memory", 70.1)
  ]
  
  // 流聚合：按metric类型聚合
  let mut aggregated_metrics = @hashmap.new()
  
  let mut i = 0
  while i < metric_stream.length() {
    let timestamp = metric_stream[i].0
    let metric_type = metric_stream[i].1
    let value = metric_stream[i].2
    
    if aggregated_metrics.contains(metric_type) {
      let (sum, count, min_val, max_val, last_timestamp) = aggregated_metrics[metric_type]
      let new_sum = sum + value
      let new_count = count + 1
      let new_min = if value < min_val { value } else { min_val }
      let new_max = if value > max_val { value } else { max_val }
      
      aggregated_metrics[metric_type] = (new_sum, new_count, new_min, new_max, timestamp)
    } else {
      aggregated_metrics[metric_type] = (value, 1, value, value, timestamp)
    }
    i = i + 1
  }
  
  // 验证流聚合结果
  assert_eq(aggregated_metrics.keys().length(), 3)  // 应该有3种metric类型
  
  // 验证CPU聚合结果
  let (cpu_sum, cpu_count, cpu_min, cpu_max, cpu_last_time) = aggregated_metrics["cpu"]
  assert_eq(cpu_count, 4)  // 4个CPU数据点
  assert_eq(cpu_min > 45.0 && cpu_min < 48.0, true)  // 最小值约45.2
  assert_eq(cpu_max > 47.0 && cpu_max < 49.0, true)  // 最大值约48.1
  assert_eq(cpu_last_time, 900)  // 最后一个时间戳
  
  // 验证内存聚合结果
  let (mem_sum, mem_count, mem_min, mem_max, mem_last_time) = aggregated_metrics["memory"]
  assert_eq(mem_count, 4)  // 4个内存数据点
  assert_eq(mem_min > 67.0 && mem_min < 69.0, true)  // 最小值约67.8
  assert_eq(mem_max > 69.0 && mem_max < 71.0, true)  // 最大值约70.1
  
  // 验证磁盘聚合结果
  let (disk_sum, disk_count, disk_min, disk_max, disk_last_time) = aggregated_metrics["disk"]
  assert_eq(disk_count, 2)  // 2个磁盘数据点
  assert_eq(disk_min > 23.0 && disk_min < 24.0, true)  // 最小值约23.5
  assert_eq(disk_max > 24.0 && disk_max < 25.0, true)  // 最大值约24.1
}

test "telemetry_stream_filtering" {
  // 测试流过滤
  
  let log_stream = [
    (1000, "INFO", "User login successful"),
    (1500, "DEBUG", "Cache hit for user profile"),
    (2000, "WARN", "High memory usage detected"),
    (2500, "ERROR", "Database connection failed"),
    (3000, "INFO", "User logout"),
    (3500, "ERROR", "Payment processing failed"),
    (4000, "DEBUG", "API response cached"),
    (4500, "CRITICAL", "System overload detected"),
    (5000, "INFO", "User registration completed")
  ]
  
  // 过滤条件：只保留ERROR和CRITICAL级别的日志
  let mut filtered_stream = []
  let mut i = 0
  while i < log_stream.length() {
    let timestamp = log_stream[i].0
    let level = log_stream[i].1
    let message = log_stream[i].2
    
    if level == "ERROR" || level == "CRITICAL" {
      filtered_stream = filtered_stream.push((timestamp, level, message))
    }
    i = i + 1
  }
  
  // 验证流过滤结果
  assert_eq(filtered_stream.length(), 3)  // 应该有3条ERROR/CRITICAL日志
  
  // 验证过滤内容
  assert_eq(filtered_stream[0].0, 2500)
  assert_eq(filtered_stream[0].1, "ERROR")
  assert_eq(filtered_stream[0].2, "Database connection failed")
  
  assert_eq(filtered_stream[1].0, 3500)
  assert_eq(filtered_stream[1].1, "ERROR")
  assert_eq(filtered_stream[1].2, "Payment processing failed")
  
  assert_eq(filtered_stream[2].0, 4500)
  assert_eq(filtered_stream[2].1, "CRITICAL")
  assert_eq(filtered_stream[2].2, "System overload detected")
}

test "telemetry_stream_transformation" {
  // 测试流转换
  
  let raw_metrics = [
    (1000, "response_time_ms", 250),
    (2000, "response_time_ms", 180),
    (3000, "response_time_ms", 320),
    (4000, "response_time_ms", 150),
    (5000, "response_time_ms", 280)
  ]
  
  // 流转换：将毫秒转换为秒，并添加性能等级
  let mut transformed_stream = []
  let mut i = 0
  while i < raw_metrics.length() {
    let timestamp = raw_metrics[i].0
    let metric_name = raw_metrics[i].1
    let value_ms = raw_metrics[i].2
    
    // 转换为秒
    let value_seconds = value_ms.to_double() / 1000.0
    
    // 添加性能等级
    let performance_grade = if value_ms < 200 {
      "excellent"
    } else if value_ms < 300 {
      "good"
    } else {
      "poor"
    }
    
    transformed_stream = transformed_stream.push((timestamp, metric_name, value_seconds, performance_grade))
    i = i + 1
  }
  
  // 验证流转换结果
  assert_eq(transformed_stream.length(), raw_metrics.length())
  
  // 验证转换后的值
  assert_eq(transformed_stream[0].2, 0.25)  // 250ms -> 0.25s
  assert_eq(transformed_stream[0].3, "good")  // 250ms属于"good"
  
  assert_eq(transformed_stream[1].2, 0.18)  // 180ms -> 0.18s
  assert_eq(transformed_stream[1].3, "excellent")  // 180ms属于"excellent"
  
  assert_eq(transformed_stream[2].2, 0.32)  // 320ms -> 0.32s
  assert_eq(transformed_stream[2].3, "poor")  // 320ms属于"poor"
  
  // 验证性能等级分布
  let mut excellent_count = 0
  let mut good_count = 0
  let mut poor_count = 0
  let mut i = 0
  while i < transformed_stream.length() {
    let grade = transformed_stream[i].3
    if grade == "excellent" {
      excellent_count = excellent_count + 1
    } else if grade == "good" {
      good_count = good_count + 1
    } else if grade == "poor" {
      poor_count = poor_count + 1
    }
    i = i + 1
  }
  
  assert_eq(excellent_count, 2)  // 2个excellent
  assert_eq(good_count, 2)       // 2个good
  assert_eq(poor_count, 1)       // 1个poor
}

test "telemetry_stream_sessionization" {
  // 测试流会话化
  
  let user_events = [
    (1000, "user_123", "login"),
    (1500, "user_123", "view_page"),
    (2000, "user_456", "login"),
    (2500, "user_123", "add_to_cart"),
    (3000, "user_456", "view_page"),
    (3500, "user_123", "checkout"),
    (4000, "user_789", "login"),
    (4500, "user_456", "logout"),
    (5000, "user_123", "logout"),
    (6000, "user_789", "view_page")
  ]
  
  let session_timeout = 3000  // 3秒超时
  
  // 流会话化：将事件分组到会话中
  let mut sessions = @hashmap.new()
  
  let mut i = 0
  while i < user_events.length() {
    let timestamp = user_events[i].0
    let user_id = user_events[i].1
    let event = user_events[i].2
    
    if sessions.contains(user_id) {
      let (last_time, event_count) = sessions[user_id]
      
      // 检查是否超时
      if timestamp - last_time > session_timeout {
        // 新会话
        sessions[user_id] = (timestamp, 1)
      } else {
        // 继续当前会话
        sessions[user_id] = (timestamp, event_count + 1)
      }
    } else {
      // 新用户/新会话
      sessions[user_id] = (timestamp, 1)
    }
    i = i + 1
  }
  
  // 验证流会话化结果
  assert_eq(sessions.keys().length(), 3)  // 应该有3个用户
  
  // 验证user_123的会话
  let (user_123_last_time, user_123_events) = sessions["user_123"]
  assert_eq(user_123_events, 5)  // user_123应该有5个事件
  assert_eq(user_123_last_time, 5000)  // 最后一个事件时间
  
  // 验证user_456的会话
  let (user_456_last_time, user_456_events) = sessions["user_456"]
  assert_eq(user_456_events, 4)  // user_456应该有4个事件
  assert_eq(user_456_last_time, 4500)  // 最后一个事件时间
  
  // 验证user_789的会话
  let (user_789_last_time, user_789_events) = sessions["user_789"]
  assert_eq(user_789_events, 2)  // user_789应该有2个事件
  assert_eq(user_789_last_time, 6000)  // 最后一个事件时间
  
  // 计算总事件数
  let mut total_events = 0
  let keys = sessions.keys()
  let mut i = 0
  while i < keys.length() {
    let user_id = keys[i]
    let (_, event_count) = sessions[user_id]
    total_events = total_events + event_count
    i = i + 1
  }
  
  assert_eq(total_events, 11)  // 总共11个事件
}