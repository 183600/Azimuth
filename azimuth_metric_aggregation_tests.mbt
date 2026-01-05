// Azimuth 指标聚合测试套件
// 专注于遥测指标的聚合、统计和分析功能

// 测试1: 基础指标聚合
test "基础指标聚合" {
  // 模拟时间序列指标数据
  let metric_data = [
    { name: "cpu_usage", value: 45.2, timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-east")] },
    { name: "cpu_usage", value: 50.1, timestamp: 1640995260, tags: [("host", "server1"), ("region", "us-east")] },
    { name: "cpu_usage", value: 48.7, timestamp: 1640995320, tags: [("host", "server1"), ("region", "us-east")] },
    { name: "cpu_usage", value: 52.3, timestamp: 1640995380, tags: [("host", "server1"), ("region", "us-east")] },
    { name: "cpu_usage", value: 47.9, timestamp: 1640995440, tags: [("host", "server1"), ("region", "us-east")] }
  ]
  
  // 计算基础统计指标
  let mut sum = 0.0
  let mut min_value = metric_data[0].value
  let mut max_value = metric_data[0].value
  
  for metric in metric_data {
    sum = sum + metric.value
    if metric.value < min_value {
      min_value = metric.value
    }
    if metric.value > max_value {
      max_value = metric.value
    }
  }
  
  let count = metric_data.length().to_float()
  let avg_value = sum / count
  
  // 验证基础聚合结果
  assert_eq(avg_value, 48.84)
  assert_eq(min_value, 45.2)
  assert_eq(max_value, 52.3)
  
  // 计算方差和标准差
  let mut variance_sum = 0.0
  for metric in metric_data {
    let diff = metric.value - avg_value
    variance_sum = variance_sum + (diff * diff)
  }
  let variance = variance_sum / count
  let std_deviation = variance.sqrt()
  
  // 验证方差和标准差
  assert_true(variance > 0.0)
  assert_true(std_deviation > 0.0)
  
  // 计算百分位数
  let sorted_values = [45.2, 47.9, 48.7, 50.1, 52.3]
  let p50 = sorted_values[2]  // 中位数
  let p90 = sorted_values[4]  // 90%百分位数
  
  assert_eq(p50, 48.7)
  assert_eq(p90, 52.3)
}

// 测试2: 多维度指标聚合
test "多维度指标聚合" {
  // 模拟多维度的指标数据
  let multi_dimensional_metrics = [
    { name: "response_time", value: 120.0, timestamp: 1640995200, tags: [("service", "api"), ("endpoint", "/users"), ("method", "GET")] },
    { name: "response_time", value: 85.0, timestamp: 1640995260, tags: [("service", "api"), ("endpoint", "/users"), ("method", "GET")] },
    { name: "response_time", value: 200.0, timestamp: 1640995320, tags: [("service", "api"), ("endpoint", "/orders"), ("method", "POST")] },
    { name: "response_time", value: 150.0, timestamp: 1640995380, tags: [("service", "api"), ("/endpoint", "/orders"), ("method", "POST")] },
    { name: "response_time", value: 95.0, timestamp: 1640995440, tags: [("service", "auth"), ("endpoint", "/login"), ("method", "POST")] },
    { name: "response_time", value: 110.0, timestamp: 1640995500, tags: [("service", "auth"), ("endpoint", "/login"), ("method", "POST")] }
  ]
  
  // 按服务分组聚合
  let mut service_aggregates = []
  let mut processed_services = []
  
  for metric in multi_dimensional_metrics {
    let service_name = ""
    for (key, value) in metric.tags {
      if key == "service" {
        service_name = value
        break
      }
    }
    
    let already_processed = false
    for service in processed_services {
      if service == service_name {
        already_processed = true
        break
      }
    }
    
    if not already_processed and service_name != "" {
      processed_services = processed_services.push(service_name)
      
      // 收集该服务的所有指标
      let mut service_metrics = []
      for m in multi_dimensional_metrics {
        let current_service = ""
        for (key, value) in m.tags {
          if key == "service" {
            current_service = value
            break
          }
        }
        if current_service == service_name {
          service_metrics = service_metrics.push(m)
        }
      }
      
      // 计算服务级别的聚合统计
      let mut sum = 0.0
      let mut count = 0
      for m in service_metrics {
        sum = sum + m.value
        count = count + 1
      }
      
      service_aggregates = service_aggregates.push({
        service: service_name,
        avg_response_time: sum / count.to_float(),
        request_count: count,
        min_response_time: {
          let mut min_val = service_metrics[0].value
          for m in service_metrics {
            if m.value < min_val {
              min_val = m.value
            }
          }
          min_val
        },
        max_response_time: {
          let mut max_val = service_metrics[0].value
          for m in service_metrics {
            if m.value > max_val {
              max_val = m.value
            }
          }
          max_val
        }
      })
    }
  }
  
  // 验证服务级别聚合结果
  assert_eq(service_aggregates.length(), 2)  // api 和 auth 服务
  
  // 验证 api 服务聚合
  let api_aggregate = service_aggregates[0]
  assert_eq(api_aggregate.service, "api")
  assert_eq(api_aggregate.request_count, 4)
  assert_eq(api_aggregate.avg_response_time, 138.75)
  assert_eq(api_aggregate.min_response_time, 85.0)
  assert_eq(api_aggregate.max_response_time, 200.0)
  
  // 验证 auth 服务聚合
  let auth_aggregate = service_aggregates[1]
  assert_eq(auth_aggregate.service, "auth")
  assert_eq(auth_aggregate.request_count, 2)
  assert_eq(auth_aggregate.avg_response_time, 102.5)
  assert_eq(auth_aggregate.min_response_time, 95.0)
  assert_eq(auth_aggregate.max_response_time, 110.0)
}

// 测试3: 时间窗口聚合
test "时间窗口聚合" {
  // 模拟时间序列指标数据
  let time_series_metrics = []
  let mut i = 0
  while i < 60 {  // 60个数据点，每分钟一个
    time_series_metrics = time_series_metrics.push({
      name: "request_rate",
      value: (50 + (i % 20)).to_int().to_float(),  // 50-70之间的值
      timestamp: 1640995200 + i * 60,  // 每分钟一个数据点
      tags: [("service", "web")]
    })
    i = i + 1
  }
  
  // 定义时间窗口（5分钟窗口）
  let window_size = 5 * 60  // 5分钟，以秒为单位
  
  // 时间窗口聚合
  let mut windowed_aggregates = []
  let mut current_window_start = time_series_metrics[0].timestamp
  
  while current_window_start <= time_series_metrics[time_series_metrics.length() - 1].timestamp {
    let current_window_end = current_window_start + window_size
    
    // 收集当前窗口内的数据
    let mut window_data = []
    for metric in time_series_metrics {
      if metric.timestamp >= current_window_start and metric.timestamp < current_window_end {
        window_data = window_data.push(metric)
      }
    }
    
    if window_data.length() > 0 {
      // 计算窗口聚合统计
      let mut sum = 0.0
      let mut count = 0
      for data in window_data {
        sum = sum + data.value
        count = count + 1
      }
      
      windowed_aggregates = windowed_aggregates.push({
        window_start: current_window_start,
        window_end: current_window_end,
        avg_value: sum / count.to_float(),
        data_point_count: count,
        min_value: {
          let mut min_val = window_data[0].value
          for data in window_data {
            if data.value < min_val {
              min_val = data.value
            }
          }
          min_val
        },
        max_value: {
          let mut max_val = window_data[0].value
          for data in window_data {
            if data.value > max_val {
              max_val = data.value
            }
          }
          max_val
        }
      })
    }
    
    current_window_start = current_window_end
  }
  
  // 验证时间窗口聚合结果
  assert_eq(windowed_aggregates.length(), 12)  // 60分钟 / 5分钟窗口 = 12个窗口
  
  // 验证每个窗口的数据点数量
  for window in windowed_aggregates {
    assert_eq(window.data_point_count, 5)  // 每个窗口应该有5个数据点
    assert_true(window.avg_value >= 50.0)
    assert_true(window.avg_value <= 70.0)
    assert_true(window.min_value >= 50.0)
    assert_true(window.max_value <= 70.0)
  }
  
  // 验证窗口时间连续性
  let mut i = 0
  while i < windowed_aggregates.length() - 1 {
    let current_window = windowed_aggregates[i]
    let next_window = windowed_aggregates[i + 1]
    assert_eq(current_window.window_end, next_window.window_start)
    i = i + 1
  }
}

// 测试4: 指标聚合的容错处理
test "指标聚合的容错处理" {
  // 模拟包含异常值的指标数据
  let metrics_with_outliers = [
    { name: "memory_usage", value: 1024.0, timestamp: 1640995200, tags: [("host", "server1")] },
    { name: "memory_usage", value: 1080.0, timestamp: 1640995260, tags: [("host", "server1")] },
    { name: "memory_usage", value: 999999.0, timestamp: 1640995320, tags: [("host", "server1")] },  // 异常值
    { name: "memory_usage", value: 1100.0, timestamp: 1640995380, tags: [("host", "server1")] },
    { name: "memory_usage", value: 1050.0, timestamp: 1640995440, tags: [("host", "server1")] },
    { name: "memory_usage", value: 0.0, timestamp: 1640995500, tags: [("host", "server1")] },  // 异常值
    { name: "memory_usage", value: 1070.0, timestamp: 1640995560, tags: [("host", "server1")] }
  ]
  
  // 计算基础统计（包含异常值）
  let mut sum_with_outliers = 0.0
  let mut count = 0
  for metric in metrics_with_outliers {
    sum_with_outliers = sum_with_outliers + metric.value
    count = count + 1
  }
  let avg_with_outliers = sum_with_outliers / count.to_float()
  
  // 异常值检测和过滤
  let detect_outliers = fn(data: Array[Dynamic]) {
    let mut values = []
    for item in data {
      values = values.push(item.value)
    }
    
    // 计算四分位数
    let sorted_values = values  // 假设已排序
    let q1 = sorted_values[sorted_values.length() / 4]
    let q3 = sorted_values[sorted_values.length() * 3 / 4]
    let iqr = q3 - q1
    
    // 定义异常值边界
    let lower_bound = q1 - 1.5 * iqr
    let upper_bound = q3 + 1.5 * iqr
    
    // 过滤异常值
    let mut filtered_data = []
    for item in data {
      if item.value >= lower_bound and item.value <= upper_bound {
        filtered_data = filtered_data.push(item)
      }
    }
    
    filtered_data
  }
  
  let filtered_metrics = detect_outliers(metrics_with_outliers)
  
  // 计算过滤后的统计
  let mut sum_filtered = 0.0
  let mut filtered_count = 0
  for metric in filtered_metrics {
    sum_filtered = sum_filtered + metric.value
    filtered_count = filtered_count + 1
  }
  let avg_filtered = if filtered_count > 0 { sum_filtered / filtered_count.to_float() } else { 0.0 }
  
  // 验证异常值过滤效果
  assert_true(filtered_count < count)  // 过滤后数据点应该减少
  assert_true(avg_filtered < avg_with_outliers)  // 过滤后平均值应该更合理
  assert_true(avg_filtered > 1000.0)  // 过滤后平均值应该在合理范围内
  
  // 验证过滤后的数据都是合理的
  for metric in filtered_metrics {
    assert_true(metric.value > 0.0)
    assert_true(metric.value < 10000.0)  // 排除明显异常的大值
  }
  
  // 容错处理：缺失值处理
  let metrics_with_missing = [
    { name: "cpu_usage", value: 45.0, timestamp: 1640995200, tags: [("host", "server1")] },
    { name: "cpu_usage", value: None, timestamp: 1640995260, tags: [("host", "server1")] },  // 缺失值
    { name: "cpu_usage", value: 50.0, timestamp: 1640995320, tags: [("host", "server1")] },
    { name: "cpu_usage", value: None, timestamp: 1640995380, tags: [("host", "server1")] },  // 缺失值
    { name: "cpu_usage", value: 48.0, timestamp: 1640995440, tags: [("host", "server1")] }
  ]
  
  // 缺失值处理策略：插值
  let handle_missing_values = fn(data: Array[Dynamic]) {
    let mut processed_data = []
    let mut last_valid_value = None
    
    for item in data {
      match item.value {
        Some(val) => {
          processed_data = processed_data.push(val)
          last_valid_value = Some(val)
        }
        None => {
          match last_valid_value {
            Some(last_val) => {
              // 使用前一个有效值
              processed_data = processed_data.push(last_val)
            }
            None => {
              // 如果没有前一个有效值，跳过
            }
          }
        }
      }
    }
    
    processed_data
  }
  
  let processed_values = handle_missing_values(metrics_with_missing)
  
  // 验证缺失值处理
  assert_eq(processed_values.length(), 5)  // 所有数据点都应该有值
  assert_eq(processed_values[0], 45.0)
  assert_eq(processed_values[1], 45.0)  // 插值
  assert_eq(processed_values[2], 50.0)
  assert_eq(processed_values[3], 50.0)  // 插值
  assert_eq(processed_values[4], 48.0)
}

// 测试5: 高级指标聚合算法
test "高级指标聚合算法" {
  // 模拟复杂的指标数据
  let complex_metrics = [
    { name: "error_rate", value: 0.01, timestamp: 1640995200, tags: [("service", "api"), ("status", "2xx")] },
    { name: "error_rate", value: 0.05, timestamp: 1640995260, tags: [("service", "api"), ("status", "4xx")] },
    { name: "error_rate", value: 0.02, timestamp: 1640995320, tags: [("service", "api"), ("status", "2xx")] },
    { name: "error_rate", value: 0.10, timestamp: 1640995380, tags: [("service", "api"), ("status", "5xx")] },
    { name: "error_rate", value: 0.03, timestamp: 1640995440, tags: [("service", "web"), ("status", "2xx")] },
    { name: "error_rate", value: 0.08, timestamp: 1640995500, tags: [("service", "web"), ("status", "4xx")] },
    { name: "error_rate", value: 0.01, timestamp: 1640995560, tags: [("service", "web"), ("status", "2xx")] },
    { name: "error_rate", value: 0.15, timestamp: 1640995620, tags: [("service", "web"), ("status", "5xx")] }
  ]
  
  // 加权平均聚合（按请求数加权）
  let weighted_avg = fn(data: Array[Dynamic], weights: Array[Float]) {
    let mut weighted_sum = 0.0
    let mut total_weight = 0.0
    
    let mut i = 0
    while i < data.length() and i < weights.length() {
      weighted_sum = weighted_sum + (data[i].value * weights[i])
      total_weight = total_weight + weights[i]
      i = i + 1
    }
    
    if total_weight > 0.0 {
      weighted_sum / total_weight
    } else {
      0.0
    }
  }
  
  // 模拟请求数权重
  let request_weights = [100.0, 20.0, 150.0, 5.0, 200.0, 30.0, 180.0, 3.0]
  
  // 按服务分组计算加权平均错误率
  let mut service_weighted_errors = []
  let mut processed_services = []
  
  for metric in complex_metrics {
    let service_name = ""
    for (key, value) in metric.tags {
      if key == "service" {
        service_name = value
        break
      }
    }
    
    let already_processed = false
    for service in processed_services {
      if service == service_name {
        already_processed = true
        break
      }
    }
    
    if not already_processed and service_name != "" {
      processed_services = processed_services.push(service_name)
      
      // 收集该服务的所有指标和权重
      let mut service_values = []
      let mut service_weights = []
      let mut i = 0
      while i < complex_metrics.length() {
        let current_service = ""
        for (key, value) in complex_metrics[i].tags {
          if key == "service" {
            current_service = value
            break
          }
        }
        if current_service == service_name {
          service_values = service_values.push(complex_metrics[i].value)
          service_weights = service_weights.push(request_weights[i])
        }
        i = i + 1
      }
      
      let weighted_error_rate = weighted_avg(service_values, service_weights)
      
      service_weighted_errors = service_weighted_errors.push({
        service: service_name,
        weighted_error_rate: weighted_error_rate,
        total_requests: {
          let mut total = 0.0
          for weight in service_weights {
            total = total + weight
          }
          total
        }
      })
    }
  }
  
  // 验证加权平均结果
  assert_eq(service_weighted_errors.length(), 2)
  
  // 验证 api 服务
  let api_service = service_weighted_errors[0]
  assert_eq(api_service.service, "api")
  assert_true(api_service.weighted_error_rate > 0.0)
  assert_true(api_service.weighted_error_rate < 0.1)
  assert_eq(api_service.total_requests, 275.0)  // 100 + 20 + 150 + 5
  
  // 验证 web 服务
  let web_service = service_weighted_errors[1]
  assert_eq(web_service.service, "web")
  assert_true(web_service.weighted_error_rate > 0.0)
  assert_true(web_service.weighted_error_rate < 0.1)
  assert_eq(web_service.total_requests, 413.0)  // 200 + 30 + 180 + 3
  
  // 指数移动平均（EMA）计算
  let ema = fn(data: Array[Float], alpha: Float) {
    if data.length() == 0 {
      0.0
    } else {
      let mut ema_value = data[0]
      let mut i = 1
      while i < data.length() {
        ema_value = alpha * data[i] + (1.0 - alpha) * ema_value
        i = i + 1
      }
      ema_value
    }
  }
  
  // 计算错误率的指数移动平均
  let error_rate_values = [0.01, 0.05, 0.02, 0.10, 0.03, 0.08, 0.01, 0.15]
  let ema_alpha = 0.3
  let ema_error_rate = ema(error_rate_values, ema_alpha)
  
  // 验证 EMA 结果
  assert_true(ema_error_rate > 0.0)
  assert_true(ema_error_rate < 0.15)
  
  // 简单平均
  let mut simple_sum = 0.0
  for value in error_rate_values {
    simple_sum = simple_sum + value
  }
  let simple_avg = simple_sum / error_rate_values.length().to_float()
  
  // EMA 应该更接近最近的值
  assert_true(ema_error_rate > simple_avg * 0.8)
  assert_true(ema_error_rate < simple_avg * 1.2)
}

// 测试6: 分布式指标聚合
test "分布式指标聚合" {
  // 模拟来自不同节点的指标数据
  let node_metrics = [
    { node_id: "node-1", service: "api", metric: "response_time", value: 120.0, timestamp: 1640995200 },
    { node_id: "node-1", service: "api", metric: "response_time", value: 130.0, timestamp: 1640995260 },
    { node_id: "node-2", service: "api", metric: "response_time", value: 110.0, timestamp: 1640995200 },
    { node_id: "node-2", service: "api", metric: "response_time", value: 115.0, timestamp: 1640995260 },
    { node_id: "node-3", service: "web", metric: "response_time", value: 80.0, timestamp: 1640995200 },
    { node_id: "node-3", service: "web", metric: "response_time", value: 85.0, timestamp: 1640995260 },
    { node_id: "node-4", service: "web", metric: "response_time", value: 90.0, timestamp: 1640995200 },
    { node_id: "node-4", service: "web", metric: "response_time", value: 95.0, timestamp: 1640995260 }
  ]
  
  // 分布式聚合策略：两阶段聚合
  // 第一阶段：节点本地聚合
  let local_aggregation = fn(node_data: Array[Dynamic]) {
    let mut sum = 0.0
    let mut count = 0
    let mut min_val = node_data[0].value
    let mut max_val = node_data[0].value
    
    for data in node_data {
      sum = sum + data.value
      count = count + 1
      if data.value < min_val {
        min_val = data.value
      }
      if data.value > max_val {
        max_val = data.value
      }
    }
    
    {
      avg_value: sum / count.to_float(),
      min_value: min_val,
      max_value: max_val,
      count: count
    }
  }
  
  // 第二阶段：全局聚合
  let global_aggregation = fn(local_results: Array[Dynamic]) {
    let mut total_sum = 0.0
    let mut total_count = 0
    let mut global_min = local_results[0].min_value
    let mut global_max = local_results[0].max_value
    
    for result in local_results {
      total_sum = total_sum + (result.avg_value * result.count.to_float())
      total_count = total_count + result.count
      if result.min_value < global_min {
        global_min = result.min_value
      }
      if result.max_value > global_max {
        global_max = result.max_value
      }
    }
    
    {
      global_avg: total_sum / total_count.to_float(),
      global_min: global_min,
      global_max: global_max,
      total_count: total_count
    }
  }
  
  // 执行分布式聚合
  let mut nodes = []
  let mut processed_nodes = []
  
  for metric in node_metrics {
    let already_processed = false
    for node in processed_nodes {
      if node == metric.node_id {
        already_processed = true
        break
      }
    }
    
    if not already_processed {
      processed_nodes = processed_nodes.push(metric.node_id)
      
      // 收集该节点的所有指标
      let mut node_data = []
      for m in node_metrics {
        if m.node_id == metric.node_id and m.service == metric.service and m.metric == metric.metric {
          node_data = node_data.push(m)
        }
      }
      
      if node_data.length() > 0 {
        let local_result = local_aggregation(node_data)
        nodes = nodes.push({
          node_id: metric.node_id,
          service: metric.service,
          metric: metric.metric,
          local_result: local_result
        })
      }
    }
  }
  
  // 按服务和指标分组进行全局聚合
  let mut global_results = []
  let mut processed_services_metrics = []
  
  for node in nodes {
    let service_metric_key = node.service + ":" + node.metric
    
    let already_processed = false
    for key in processed_services_metrics {
      if key == service_metric_key {
        already_processed = true
        break
      }
    }
    
    if not already_processed {
      processed_services_metrics = processed_service_metrics.push(service_metric_key)
      
      // 收集同一服务指标的所有节点结果
      let mut node_results = []
      for n in nodes {
        let current_key = n.service + ":" + n.metric
        if current_key == service_metric_key {
          node_results = node_results.push(n.local_result)
        }
      }
      
      let global_result = global_aggregation(node_results)
      
      global_results = global_results.push({
        service_metric: service_metric_key,
        global_result: global_result,
        node_count: node_results.length()
      })
    }
  }
  
  // 验证分布式聚合结果
  assert_eq(global_results.length(), 2)  // api:response_time 和 web:response_time
  
  // 验证 api:response_time 聚合
  let api_result = global_results[0]
  assert_eq(api_result.service_metric, "api:response_time")
  assert_eq(api_result.node_count, 2)  // node-1 和 node-2
  assert_eq(api_result.global_result.total_count, 4)  // 每个节点2个数据点
  assert_eq(api_result.global_result.global_avg, 118.75)  // (120+130+110+115)/4
  assert_eq(api_result.global_result.global_min, 110.0)
  assert_eq(api_result.global_result.global_max, 130.0)
  
  // 验证 web:response_time 聚合
  let web_result = global_results[1]
  assert_eq(web_result.service_metric, "web:response_time")
  assert_eq(web_result.node_count, 2)  // node-3 和 node-4
  assert_eq(web_result.global_result.total_count, 4)  // 每个节点2个数据点
  assert_eq(web_result.global_result.global_avg, 87.5)  // (80+85+90+95)/4
  assert_eq(web_result.global_result.global_min, 80.0)
  assert_eq(web_result.global_result.global_max, 95.0)
  
  // 验证分布式聚合的优势：减少网络传输
  // 原始数据传输：8个数据点
  // 本地聚合后传输：4个聚合结果（每个节点一个）
  // 全局聚合后：2个最终结果（每个服务指标一个）
  assert_true(global_results.length() < nodes.length())
  assert_true(nodes.length() < node_metrics.length())
}