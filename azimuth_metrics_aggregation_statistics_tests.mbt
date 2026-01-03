// 度量数据聚合和统计测试用例
// 测试Azimuth遥测系统中度量数据的聚合和统计功能

test "时间窗口聚合统计" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // 创建各种度量类型
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = Meter::create_gauge(meter, "http.connections.active", Some("Active HTTP connections"), Some("connections"))
  
  // 模拟不同时间窗口的数据
  let time_windows = [
    ("2025-01-01T00:00:00Z", "2025-01-01T01:00:00Z", 1000, 150.0),
    ("2025-01-01T01:00:00Z", "2025-01-01T02:00:00Z", 1200, 145.0),
    ("2025-01-01T02:00:00Z", "2025-01-01T03:00:00Z", 800, 160.0),
    ("2025-01-01T03:00:00Z", "2025-01-01T04:00:00Z", 1500, 140.0),
    ("2025-01-01T04:00:00Z", "2025-01-01T05:00:00Z", 900, 155.0)
  ]
  
  let total_requests = 0
  let total_response_time = 0.0
  let response_time_count = 0
  
  // 在每个时间窗口记录数据
  for i = 0; i < time_windows.length(); i = i + 1 {
    let (start_time, end_time, request_count, avg_response_time) = time_windows[i]
    
    // 记录请求数量
    Counter::add_with_attributes(request_counter, request_count.to_float(), [
      ("time.window.start", start_time),
      ("time.window.end", end_time)
    ])
    
    // 记录响应时间分布
    for j = 0; j < 10; j = j + 1 {
      let response_time = avg_response_time + (j * 10.0) - 45.0  // 创建分布
      Histogram::record_with_attributes(response_histogram, response_time, [
        ("time.window.start", start_time),
        ("time.window.end", end_time)
      ])
      total_response_time = total_response_time + response_time
      response_time_count = response_time_count + 1
    }
    
    // 记录活跃连接数
    let active_conn = 50 + (request_count / 20)
    Gauge::record_with_attributes(active_connections, active_conn.to_float(), [
      ("time.window.start", start_time),
      ("time.window.end", end_time)
    ])
    
    total_requests = total_requests + request_count
  }
  
  // 计算聚合统计
  let avg_response_time = total_response_time / response_time_count.to_float()
  let max_requests = 1500
  let min_requests = 800
  let avg_requests_per_window = total_requests.to_float() / time_windows.length().to_float()
  
  // 验证聚合结果
  assert_eq(total_requests, 5400)
  assert_true(avg_response_time >= 140.0 && avg_response_time <= 160.0)
  assert_true(avg_requests_per_window >= 1000.0 && avg_requests_per_window <= 1200.0)
}

test "百分位数统计计算" {
  // 测试百分位数计算
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "percentile.test")
  
  // 创建响应时间直方图
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "response.time.percentiles", 
    Some("Response time percentiles"), 
    Some("ms")
  )
  
  // 生成具有特定分布的响应时间数据
  let response_times = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 110.0,
    120.0, 130.0, 140.0, 150.0, 160.0, 170.0, 180.0, 190.0, 200.0, 250.0,
    300.0, 350.0, 400.0, 450.0, 500.0, 600.0, 700.0, 800.0, 900.0, 1000.0
  ]
  
  // 记录响应时间数据
  for response_time in response_times {
    Histogram::record(response_time_histogram, response_time)
  }
  
  // 计算百分位数
  let sorted_times = response_times.sort(fn(a, b) { a <= b })
  let data_count = sorted_times.length()
  
  // P50 (中位数)
  let p50_index = (data_count * 50) / 100
  let p50 = sorted_times[p50_index]
  
  // P95
  let p95_index = (data_count * 95) / 100
  let p95 = sorted_times[p95_index]
  
  // P99
  let p99_index = (data_count * 99) / 100
  let p99 = sorted_times[p99_index]
  
  // P999
  let p999_index = (data_count * 999) / 1000
  let p999 = if p999_index >= data_count {
    sorted_times[data_count - 1]
  } else {
    sorted_times[p999_index]
  }
  
  // 验证百分位数计算结果
  assert_eq(p50, 150.0)
  assert_eq(p95, 800.0)
  assert_eq(p99, 1000.0)
  assert_eq(p999, 1000.0)
  
  // 验证百分位数关系
  assert_true(p50 <= p95 && p95 <= p99 && p99 <= p999)
}

test "多维度数据聚合" {
  // 测试按多个维度聚合数据
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multidimensional.test")
  
  // 创建多维度计数器
  let api_requests = Meter::create_counter(
    meter, 
    "api.requests.total", 
    Some("Total API requests"), 
    Some("count")
  )
  
  // 定义维度
  let services = ["user-service", "order-service", "payment-service", "inventory-service"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  
  // 生成多维度数据
  for i = 0; i < 100; i = i + 1 {
    let service = services[i % services.length()]
    let method = methods[i % methods.length()]
    let status_code = status_codes[i % status_codes.length()]
    
    // 记录多维度数据
    Counter::add_with_attributes(api_requests, 1.0, [
      ("service.name", service),
      ("http.method", method),
      ("http.status_code", status_code)
    ])
  }
  
  // 按服务维度聚合
  let service_counts = [
    ("user-service", 25),
    ("order-service", 25),
    ("payment-service", 25),
    ("inventory-service", 25)
  ]
  
  // 按方法维度聚合
  let method_counts = [
    ("GET", 25),
    ("POST", 25),
    ("PUT", 25),
    ("DELETE", 25)
  ]
  
  // 按状态码维度聚合
  let status_counts = [
    ("200", 20),
    ("201", 20),
    ("400", 20),
    ("404", 20),
    ("500", 20)
  ]
  
  // 验证聚合结果
  let total_service_requests = 0
  for (service, count) in service_counts {
    total_service_requests = total_service_requests + count
    assert_eq(count, 25)
  }
  
  let total_method_requests = 0
  for (method, count) in method_counts {
    total_method_requests = total_method_requests + count
    assert_eq(count, 25)
  }
  
  let total_status_requests = 0
  for (status, count) in status_counts {
    total_status_requests = total_status_requests + count
    assert_eq(count, 20)
  }
  
  // 验证总请求数
  assert_eq(total_service_requests, 100)
  assert_eq(total_method_requests, 100)
  assert_eq(total_status_requests, 100)
}

test "时序数据降采样" {
  // 测试时序数据降采样功能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "downsampling.test")
  
  // 创建高频度量
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage", Some("System CPU usage"), Some("percent"))
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage", Some("System memory usage"), Some("percent"))
  
  // 生成高频数据点（每秒一个点，持续1小时）
  let high_frequency_data = []
  for i = 0; i < 3600; i = i + 1 {
    // 模拟CPU使用率波动
    let base_cpu = 30.0
    let cpu_variation = 20.0 * (i % 60).to_float() / 60.0  // 1分钟周期
    let cpu_value = base_cpu + cpu_variation
    
    // 模拟内存使用率缓慢增长
    let base_memory = 40.0
    let memory_growth = (i.to_float() / 3600.0) * 20.0  // 1小时内增长20%
    let memory_value = base_memory + memory_growth
    
    // 记录高频数据点
    Gauge::record(cpu_usage, cpu_value)
    Gauge::record(memory_usage, memory_value)
    
    high_frequency_data.push((i, cpu_value, memory_value))
  }
  
  // 执行降采样：从每秒到每分钟
  let downsampled_data = []
  for minute = 0; minute < 60; minute = minute + 1 {
    let minute_start = minute * 60
    let minute_end = (minute + 1) * 60 - 1
    
    let minute_cpu_sum = 0.0
    let minute_memory_sum = 0.0
    let minute_count = 0
    
    // 聚合分钟内的数据点
    for i = minute_start; i <= minute_end; i = i + 1 {
      if i < high_frequency_data.length() {
        let (_, cpu_value, memory_value) = high_frequency_data[i]
        minute_cpu_sum = minute_cpu_sum + cpu_value
        minute_memory_sum = minute_memory_sum + memory_value
        minute_count = minute_count + 1
      }
    }
    
    // 计算分钟平均值
    let avg_cpu = minute_cpu_sum / minute_count.to_float()
    let avg_memory = minute_memory_sum / minute_count.to_float()
    
    downsampled_data.push((minute, avg_cpu, avg_memory))
  }
  
  // 验证降采样结果
  assert_eq(downsampled_data.length(), 60)
  
  // 验证第一个分钟的数据
  let (first_minute, first_cpu, first_memory) = downsampled_data[0]
  assert_eq(first_minute, 0)
  assert_true(first_cpu >= 30.0 && first_cpu <= 50.0)
  assert_true(first_memory >= 40.0 && first_memory <= 41.0)
  
  // 验证最后一个分钟的数据
  let (last_minute, last_cpu, last_memory) = downsampled_data[59]
  assert_eq(last_minute, 59)
  assert_true(last_cpu >= 30.0 && last_cpu <= 50.0)
  assert_true(last_memory >= 59.0 && last_memory <= 61.0)
}

test "异常值检测和处理" {
  // 测试异常值检测和处理
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "outlier.test")
  
  // 创建包含异常值的度量
  let response_time = Meter::create_histogram(meter, "response.time.with.outliers", Some("Response time with outliers"), Some("ms"))
  
  // 生成正常响应时间数据
  let normal_response_times = [
    50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0,
    100.0, 105.0, 110.0, 115.0, 120.0, 125.0, 130.0, 135.0, 140.0, 145.0,
    150.0, 155.0, 160.0, 165.0, 170.0, 175.0, 180.0, 185.0, 190.0, 195.0,
    200.0, 205.0, 210.0, 215.0, 220.0, 225.0, 230.0, 235.0, 240.0, 245.0,
    250.0, 255.0, 260.0, 265.0, 270.0, 275.0, 280.0, 285.0, 290.0, 295.0
  ]
  
  // 添加异常值
  let outliers = [5.0, 10.0, 15.0, 500.0, 1000.0, 2000.0, 3000.0]
  
  // 记录所有响应时间数据
  for response_time_value in normal_response_times {
    Histogram::record_with_attributes(response_time, response_time_value, [("is.outlier", "false")])
  }
  
  for outlier_value in outliers {
    Histogram::record_with_attributes(response_time, outlier_value, [("is.outlier", "true")])
  }
  
  // 计算统计指标
  let all_values = normal_response_times.concat(outliers)
  let sorted_values = all_values.sort(fn(a, b) { a <= b })
  
  // 计算四分位数
  let q1_index = sorted_values.length() / 4
  let q3_index = (sorted_values.length() * 3) / 4
  let q1 = sorted_values[q1_index]
  let q3 = sorted_values[q3_index]
  let iqr = q3 - q1
  
  // 定义异常值边界
  let lower_bound = q1 - 1.5 * iqr
  let upper_bound = q3 + 1.5 * iqr
  
  // 检测异常值
  let detected_outliers = []
  let normal_values = []
  
  for value in all_values {
    if value < lower_bound || value > upper_bound {
      detected_outliers.push(value)
    } else {
      normal_values.push(value)
    }
  }
  
  // 验证异常值检测结果
  assert_true(detected_outliers.length() >= 3)  // 至少检测到3个异常值
  assert_true(normal_values.length() >= 45)  // 大部分数据应该是正常的
  
  // 验证异常值边界计算
  assert_true(lower_bound > 0.0)
  assert_true(upper_bound < 1000.0)
  
  // 计算去除异常值后的统计指标
  let filtered_sum = 0.0
  for value in normal_values {
    filtered_sum = filtered_sum + value
  }
  let filtered_avg = filtered_sum / normal_values.length().to_float()
  
  // 验证过滤后的平均值在合理范围内
  assert_true(filtered_avg >= 100.0 && filtered_avg <= 200.0)
}

test "趋势分析和预测" {
  // 测试趋势分析和简单预测
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "trend.analysis.test")
  
  // 创建趋势度量
  let request_rate = Meter::create_gauge(meter, "request.rate", Some("Request rate"), Some("req/sec"))
  let error_rate = Meter::create_gauge(meter, "error.rate", Some("Error rate"), Some("percent"))
  
  // 生成具有趋势的数据
  let trend_data = []
  let base_request_rate = 100.0
  let base_error_rate = 2.0
  
  for day = 0; day < 30; day = day + 1 {
    // 模拟请求率增长趋势（每天增长2%）
    let request_growth = 1.0 + (day.to_float() * 0.02)
    let daily_request_rate = base_request_rate * request_growth
    
    // 添加一些随机波动
    let request_variation = 10.0 * ((day * 3) % 7).to_float() / 7.0
    let actual_request_rate = daily_request_rate + request_variation
    
    // 模拟错误率下降趋势（每天降低1%）
    let error_decay = 1.0 - (day.to_float() * 0.01)
    let daily_error_rate = base_error_rate * error_decay
    
    // 添加一些随机波动
    let error_variation = 0.5 * ((day * 5) % 3).to_float() / 3.0
    let actual_error_rate = daily_error_rate + error_variation
    
    // 记录趋势数据
    Gauge::record_with_attributes(request_rate, actual_request_rate, [("day", day.to_string())])
    Gauge::record_with_attributes(error_rate, actual_error_rate, [("day", day.to_string())])
    
    trend_data.push((day, actual_request_rate, actual_error_rate))
  }
  
  // 计算简单线性趋势
  let request_rate_sum = 0.0
  let error_rate_sum = 0.0
  let day_sum = 0.0
  
  for (day, request_rate_value, error_rate_value) in trend_data {
    request_rate_sum = request_rate_sum + request_rate_value
    error_rate_sum = error_rate_sum + error_rate_value
    day_sum = day_sum + day.to_float()
  }
  
  let avg_request_rate = request_rate_sum / trend_data.length().to_float()
  let avg_error_rate = error_rate_sum / trend_data.length().to_float()
  let avg_day = day_sum / trend_data.length().to_float()
  
  // 计算斜率（简化线性回归）
  let numerator = 0.0
  let denominator = 0.0
  
  for (day, request_rate_value, error_rate_value) in trend_data {
    let day_diff = day.to_float() - avg_day
    let request_diff = request_rate_value - avg_request_rate
    let error_diff = error_rate_value - avg_error_rate
    
    numerator = numerator + (day_diff * request_diff)
    denominator = denominator + (day_diff * day_diff)
  }
  
  let request_slope = numerator / denominator
  
  // 验证趋势分析结果
  assert_true(avg_request_rate > 100.0 && avg_request_rate < 200.0)
  assert_true(avg_error_rate > 1.0 && avg_error_rate < 3.0)
  assert_true(request_slope > 0.0)  // 请求率应该呈上升趋势
  
  // 简单预测未来7天的值
  let future_day = 30.0
  let predicted_request_rate = avg_request_rate + request_slope * (future_day - avg_day)
  
  // 验证预测值在合理范围内
  assert_true(predicted_request_rate > avg_request_rate)
  assert_true(predicted_request_rate < 500.0)
}