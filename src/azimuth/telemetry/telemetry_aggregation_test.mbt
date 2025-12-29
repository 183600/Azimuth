// 遥测数据聚合和统计测试用例

test "telemetry_response_time_aggregation" {
  // 测试响应时间聚合统计
  
  let response_times = [25.3, 45.6, 78.9, 12.1, 156.8, 89.2, 34.5, 67.8, 23.4, 98.7]
  
  // 计算平均响应时间
  let mut total_time = 0.0
  let mut i = 0
  while i < response_times.length() {
    total_time = total_time + response_times[i]
    i = i + 1
  }
  let average_time = total_time / response_times.length().to_double()
  
  // 验证平均响应时间
  assert_eq(average_time > 50.0, true)
  assert_eq(average_time < 70.0, true)
  
  // 计算最大响应时间
  let mut max_time = response_times[0]
  i = 1
  while i < response_times.length() {
    if response_times[i] > max_time {
      max_time = response_times[i]
    }
    i = i + 1
  }
  
  // 验证最大响应时间
  assert_eq(max_time, 156.8)
  
  // 计算最小响应时间
  let mut min_time = response_times[0]
  i = 1
  while i < response_times.length() {
    if response_times[i] < min_time {
      min_time = response_times[i]
    }
    i = i + 1
  }
  
  // 验证最小响应时间
  assert_eq(min_time, 12.1)
}

test "telemetry_request_count_aggregation" {
  // 测试请求计数聚合
  
  let endpoint_stats = [
    ("GET /api/users", 150),
    ("POST /api/users", 45),
    ("GET /api/products", 230),
    ("PUT /api/products", 78),
    ("DELETE /api/products", 12),
    ("GET /api/orders", 189)
  ]
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < endpoint_stats.length() {
    total_requests = total_requests + endpoint_stats[i].1
    i = i + 1
  }
  
  // 验证总请求数
  assert_eq(total_requests, 704)
  
  // 按HTTP方法分组统计
  let mut get_requests = 0
  let mut post_requests = 0
  let mut put_requests = 0
  let mut delete_requests = 0
  
  i = 0
  while i < endpoint_stats.length() {
    if endpoint_stats[i].0.has_prefix("GET") {
      get_requests = get_requests + endpoint_stats[i].1
    } else if endpoint_stats[i].0.has_prefix("POST") {
      post_requests = post_requests + endpoint_stats[i].1
    } else if endpoint_stats[i].0.has_prefix("PUT") {
      put_requests = put_requests + endpoint_stats[i].1
    } else if endpoint_stats[i].0.has_prefix("DELETE") {
      delete_requests = delete_requests + endpoint_stats[i].1
    }
    i = i + 1
  }
  
  // 验证分组统计结果
  assert_eq(get_requests, 569)
  assert_eq(post_requests, 45)
  assert_eq(put_requests, 78)
  assert_eq(delete_requests, 12)
}

test "telemetry_error_rate_calculation" {
  // 测试错误率计算
  
  let request_data = [
    ("api", 200, 150),
    ("api", 404, 25),
    ("api", 500, 8),
    ("api", 200, 89),
    ("api", 401, 3),
    ("api", 200, 234),
    ("api", 503, 2)
  ]
  
  // 计算总请求数和错误请求数
  let mut total_requests = 0
  let mut error_requests = 0
  let mut i = 0
  while i < request_data.length() {
    total_requests = total_requests + request_data[i].2
    if request_data[i].1 != 200 {
      error_requests = error_requests + request_data[i].2
    }
    i = i + 1
  }
  
  // 计算错误率
  let error_rate = (error_requests.to_double() / total_requests.to_double()) * 100.0
  
  // 验证错误率计算
  assert_eq(total_requests, 511)
  assert_eq(error_requests, 38)
  assert_eq(error_rate > 5.0, true)
  assert_eq(error_rate < 10.0, true)
  
  // 按错误类型分组
  let mut client_errors = 0  // 4xx
  let mut server_errors = 0  // 5xx
  
  i = 0
  while i < request_data.length() {
    if request_data[i].1 >= 400 && request_data[i].1 < 500 {
      client_errors = client_errors + request_data[i].2
    } else if request_data[i].1 >= 500 {
      server_errors = server_errors + request_data[i].2
    }
    i = i + 1
  }
  
  // 验证错误分类
  assert_eq(client_errors, 28)
  assert_eq(server_errors, 10)
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [12.1, 23.4, 25.3, 34.5, 45.6, 67.8, 78.9, 89.2, 98.7, 156.8]
  
  // 验证数据已排序
  assert_eq(response_times[0], 12.1)
  assert_eq(response_times[9], 156.8)
  
  // 计算P50 (中位数)
  let p50_index = response_times.length() / 2
  let p50 = response_times[p50_index]
  
  // 验证P50
  assert_eq(p50, 67.8)
  
  // 计算P90
  let p90_index = (response_times.length() * 90) / 100
  let p90 = response_times[p90_index]
  
  // 验证P90
  assert_eq(p90, 98.7)
  
  // 计算P95
  let p95_index = (response_times.length() * 95) / 100
  let p95 = response_times[p95_index]
  
  // 验证P95
  assert_eq(p95, 156.8)
  
  // 计算P99
  let p99_index = (response_times.length() * 99) / 100
  let p99 = response_times[p99_index]
  
  // 验证P99
  assert_eq(p99, 156.8)
}

test "telemetry_time_bucket_aggregation" {
  // 测试时间桶聚合
  
  let time_series_data = [
    (1640995200L, 15),  // 2022-01-01 00:00:00
    (1640995260L, 23),  // 2022-01-01 00:01:00
    (1640995320L, 18),  // 2022-01-01 00:02:00
    (1640995380L, 31),  // 2022-01-01 00:03:00
    (1640995440L, 27),  // 2022-01-01 00:04:00
    (1640995500L, 19),  // 2022-01-01 00:05:00
    (1640995560L, 25),  // 2022-01-01 00:06:00
    (1640995620L, 22)   // 2022-01-01 00:07:00
  ]
  
  // 按5分钟桶聚合
  let bucket_size = 300L  // 5分钟 = 300秒
  let mut buckets = []
  
  // 创建时间桶
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    let bucket_start = (timestamp / bucket_size) * bucket_size
    
    // 查找或创建桶
    let mut bucket_found = false
    let mut j = 0
    while j < buckets.length() {
      if buckets[j].0 == bucket_start {
        buckets[j] = (buckets[j].0, buckets[j].1 + value)
        bucket_found = true
        break
      }
      j = j + 1
    }
    
    // 如果桶不存在，创建新桶
    if not bucket_found {
      buckets.push((bucket_start, value))
    }
    
    i = i + 1
  }
  
  // 验证时间桶聚合结果
  assert_eq(buckets.length(), 2)  // 应该有2个5分钟桶
  
  // 验证第一个桶 (00:00-00:05)
  assert_eq(buckets[0].0, 1640995200L)
  assert_eq(buckets[0].1, 114)  // 15+23+18+31+27
  
  // 验证第二个桶 (00:05-00:10)
  assert_eq(buckets[1].0, 1640995500L)
  assert_eq(buckets[1].1, 66)   // 19+25+22
}

test "telemetry_metric_aggregation_by_tags" {
  // 测试按标签聚合指标
  
  let tagged_metrics = [
    ("cpu_usage", 75.5, ["service:api", "env:production"]),
    ("cpu_usage", 45.2, ["service:db", "env:production"]),
    ("cpu_usage", 89.7, ["service:api", "env:staging"]),
    ("memory_usage", 156.8, ["service:api", "env:production"]),
    ("memory_usage", 78.9, ["service:db", "env:production"]),
    ("cpu_usage", 23.4, ["service:cache", "env:development"])
  ]
  
  // 按服务名称聚合CPU使用率
  let mut service_cpu_usage = []
  let mut i = 0
  while i < tagged_metrics.length() {
    if tagged_metrics[i].0 == "cpu_usage" {
      let tags = tagged_metrics[i].2
      let mut service_name = ""
      
      // 提取服务名称
      let mut j = 0
      while j < tags.length() {
        if tags[j].has_prefix("service:") {
          service_name = tags[j].substring(8)  // 跳过"service:"
          break
        }
        j = j + 1
      }
      
      // 查找或创建服务聚合
      let mut service_found = false
      let mut k = 0
      while k < service_cpu_usage.length() {
        if service_cpu_usage[k].0 == service_name {
          service_cpu_usage[k] = (service_cpu_usage[k].0, 
                                 service_cpu_usage[k].1 + tagged_metrics[i].1,
                                 service_cpu_usage[k].2 + 1)
          service_found = true
          break
        }
        k = k + 1
      }
      
      // 如果服务不存在，创建新聚合
      if not service_found {
        service_cpu_usage.push((service_name, tagged_metrics[i].1, 1))
      }
    }
    i = i + 1
  }
  
  // 验证服务CPU使用率聚合
  assert_eq(service_cpu_usage.length(), 3)
  
  // 验证api服务CPU使用率
  let mut api_cpu = 0.0
  let mut api_count = 0
  i = 0
  while i < service_cpu_usage.length() {
    if service_cpu_usage[i].0 == "api" {
      api_cpu = service_cpu_usage[i].1
      api_count = service_cpu_usage[i].2
      break
    }
    i = i + 1
  }
  assert_eq(api_cpu, 165.2)  // 75.5 + 89.7
  assert_eq(api_count, 2)
}