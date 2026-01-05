// Azimuth 遥测数据分析测试用例
// 专注于遥测数据的分析、处理和可视化功能

// 测试1: 遥测数据聚合功能
test "遥测数据聚合功能测试" {
  // 创建模拟的遥测数据点
  let telemetry_data = [
    { service: "api-gateway", latency: 120, status: 200, timestamp: 1640995200 },
    { service: "api-gateway", latency: 95, status: 200, timestamp: 1640995260 },
    { service: "api-gateway", latency: 250, status: 500, timestamp: 1640995320 },
    { service: "user-service", latency: 80, status: 200, timestamp: 1640995380 },
    { service: "user-service", latency: 65, status: 200, timestamp: 1640995440 },
    { service: "payment-service", latency: 300, status: 200, timestamp: 1640995500 },
    { service: "payment-service", latency: 450, status: 500, timestamp: 1640995560 },
    { service: "notification-service", latency: 40, status: 200, timestamp: 1640995620 }
  ]
  
  // 按服务名称分组聚合
  let group_by_service = fn(data: Array[ServiceMetric]) {
    let mut groups = {}
    for metric in data {
      let service_name = metric.service
      let existing_metrics = groups.get(service_name, [])
      groups = groups.set(service_name, existing_metrics.push(metric))
    }
    groups
  }
  
  let grouped_data = group_by_service(telemetry_data)
  
  // 验证分组结果
  assert_eq(grouped_data.get("api-gateway", []).length(), 3)
  assert_eq(grouped_data.get("user-service", []).length(), 2)
  assert_eq(grouped_data.get("payment-service", []).length(), 2)
  assert_eq(grouped_data.get("notification-service", []).length(), 1)
  
  // 计算每个服务的平均延迟
  let calculate_avg_latency = fn(metrics: Array[ServiceMetric]) {
    let mut total = 0
    for metric in metrics {
      total = total + metric.latency
    }
    total / metrics.length()
  }
  
  let api_gateway_avg = calculate_avg_latency(grouped_data.get("api-gateway", []))
  let user_service_avg = calculate_avg_latency(grouped_data.get("user-service", []))
  let payment_service_avg = calculate_avg_latency(grouped_data.get("payment-service", []))
  
  // 验证平均延迟计算
  assert_eq(api_gateway_avg, (120 + 95 + 250) / 3)
  assert_eq(user_service_avg, (80 + 65) / 2)
  assert_eq(payment_service_avg, (300 + 450) / 2)
  
  // 计算错误率
  let calculate_error_rate = fn(metrics: Array[ServiceMetric]) {
    let mut error_count = 0
    for metric in metrics {
      if metric.status != 200 {
        error_count = error_count + 1
      }
    }
    (error_count * 100) / metrics.length()
  }
  
  let api_gateway_error_rate = calculate_error_rate(grouped_data.get("api-gateway", []))
  let user_service_error_rate = calculate_error_rate(grouped_data.get("user-service", []))
  let payment_service_error_rate = calculate_error_rate(grouped_data.get("payment-service", []))
  
  // 验证错误率计算
  assert_eq(api_gateway_error_rate, 33)  // 1 error out of 3 requests ≈ 33%
  assert_eq(user_service_error_rate, 0)  // 0 errors out of 2 requests = 0%
  assert_eq(payment_service_error_rate, 50)  // 1 error out of 2 requests = 50%
}

// 测试2: 遥测数据过滤功能
test "遥测数据过滤功能测试" {
  // 创建模拟的遥测数据点
  let telemetry_data = [
    { service: "api-gateway", latency: 120, status: 200, timestamp: 1640995200, region: "us-east-1" },
    { service: "api-gateway", latency: 95, status: 200, timestamp: 1640995260, region: "us-east-1" },
    { service: "api-gateway", latency: 250, status: 500, timestamp: 1640995320, region: "us-west-2" },
    { service: "user-service", latency: 80, status: 200, timestamp: 1640995380, region: "us-east-1" },
    { service: "user-service", latency: 65, status: 200, timestamp: 1640995440, region: "eu-west-1" },
    { service: "payment-service", latency: 300, status: 200, timestamp: 1640995500, region: "us-west-2" },
    { service: "payment-service", latency: 450, status: 500, timestamp: 1640995560, region: "us-west-2" },
    { service: "notification-service", latency: 40, status: 200, timestamp: 1640995620, region: "eu-west-1" }
  ]
  
  // 按服务名称过滤
  let filter_by_service = fn(data: Array[ServiceMetricWithRegion], service_name: String) {
    let mut filtered = []
    for metric in data {
      if metric.service == service_name {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let api_gateway_metrics = filter_by_service(telemetry_data, "api-gateway")
  assert_eq(api_gateway_metrics.length(), 3)
  
  // 按状态码过滤
  let filter_by_status = fn(data: Array[ServiceMetricWithRegion], status_code: Int) {
    let mut filtered = []
    for metric in data {
      if metric.status == status_code {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let success_metrics = filter_by_status(telemetry_data, 200)
  let error_metrics = filter_by_status(telemetry_data, 500)
  
  assert_eq(success_metrics.length(), 6)
  assert_eq(error_metrics.length(), 2)
  
  // 按区域过滤
  let filter_by_region = fn(data: Array[ServiceMetricWithRegion], region_name: String) {
    let mut filtered = []
    for metric in data {
      if metric.region == region_name {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let us_east_metrics = filter_by_region(telemetry_data, "us-east-1")
  let eu_west_metrics = filter_by_region(telemetry_data, "eu-west-1")
  let us_west_metrics = filter_by_region(telemetry_data, "us-west-2")
  
  assert_eq(us_east_metrics.length(), 3)
  assert_eq(eu_west_metrics.length(), 2)
  assert_eq(us_west_metrics.length(), 3)
  
  // 组合过滤条件
  let complex_filter = fn(data: Array[ServiceMetricWithRegion], service_name: String, status_code: Int, region_name: String) {
    let mut filtered = []
    for metric in data {
      if metric.service == service_name and metric.status == status_code and metric.region == region_name {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let api_gateway_success_us_east = complex_filter(telemetry_data, "api-gateway", 200, "us-east-1")
  assert_eq(api_gateway_success_us_east.length(), 2)
  
  // 按延迟阈值过滤
  let filter_by_latency_threshold = fn(data: Array[ServiceMetricWithRegion], threshold: Int) {
    let mut filtered = []
    for metric in data {
      if metric.latency > threshold {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let high_latency_metrics = filter_by_latency_threshold(telemetry_data, 200)
  assert_eq(high_latency_metrics.length(), 3)
  
  // 验证高延迟指标
  for metric in high_latency_metrics {
    assert_true(metric.latency > 200)
  }
  
  // 按时间范围过滤
  let filter_by_time_range = fn(data: Array[ServiceMetricWithRegion], start_time: Int, end_time: Int) {
    let mut filtered = []
    for metric in data {
      if metric.timestamp >= start_time and metric.timestamp <= end_time {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  let time_filtered_metrics = filter_by_time_range(telemetry_data, 1640995300, 1640995500)
  assert_eq(time_filtered_metrics.length(), 3)
}

// 测试3: 遥测数据分析功能
test "遥测数据分析功能测试" {
  // 创建模拟的遥测数据点
  let telemetry_data = [
    { service: "api-gateway", latency: 120, status: 200, timestamp: 1640995200, cpu_usage: 65.5, memory_usage: 70.2 },
    { service: "api-gateway", latency: 95, status: 200, timestamp: 1640995260, cpu_usage: 68.1, memory_usage: 72.5 },
    { service: "api-gateway", latency: 250, status: 500, timestamp: 1640995320, cpu_usage: 85.3, memory_usage: 85.7 },
    { service: "user-service", latency: 80, status: 200, timestamp: 1640995380, cpu_usage: 45.2, memory_usage: 60.3 },
    { service: "user-service", latency: 65, status: 200, timestamp: 1640995440, cpu_usage: 42.8, memory_usage: 61.1 },
    { service: "payment-service", latency: 300, status: 200, timestamp: 1640995500, cpu_usage: 75.6, memory_usage: 78.9 },
    { service: "payment-service", latency: 450, status: 500, timestamp: 1640995560, cpu_usage: 92.1, memory_usage: 90.4 },
    { service: "notification-service", latency: 40, status: 200, timestamp: 1640995620, cpu_usage: 30.5, memory_usage: 45.7 }
  ]
  
  // 计算百分位数
  let calculate_percentile = fn(values: Array[Int], percentile: Float) {
    let sorted_values = values.sort(fn(a, b) { a - b })
    let index = (sorted_values.length() * percentile / 100.0).to_int()
    if index >= sorted_values.length() {
      sorted_values[sorted_values.length() - 1]
    } else {
      sorted_values[index]
    }
  }
  
  let latencies = telemetry_data.map(fn(m) { m.latency })
  let p50_latency = calculate_percentile(latencies, 50.0)
  let p90_latency = calculate_percentile(latencies, 90.0)
  let p95_latency = calculate_percentile(latencies, 95.0)
  let p99_latency = calculate_percentile(latencies, 99.0)
  
  // 验证百分位数计算
  assert_eq(p50_latency, 120)  // 中位数
  assert_eq(p90_latency, 450)
  assert_eq(p95_latency, 450)
  assert_eq(p99_latency, 450)
  
  // 计算相关系数
  let calculate_correlation = fn(x_values: Array[Float], y_values: Array[Float]) {
    if x_values.length() != y_values.length() or x_values.length() == 0 {
      0.0
    } else {
      let n = x_values.length().to_float()
      let x_sum = x_values.reduce(fn(acc, x) { acc + x }, 0.0)
      let y_sum = y_values.reduce(fn(acc, y) { acc + y }, 0.0)
      
      let x_mean = x_sum / n
      let y_mean = y_sum / n
      
      let mut numerator = 0.0
      let mut x_sum_sq = 0.0
      let mut y_sum_sq = 0.0
      
      for i in 0..x_values.length() {
        let x_diff = x_values[i] - x_mean
        let y_diff = y_values[i] - y_mean
        
        numerator = numerator + (x_diff * y_diff)
        x_sum_sq = x_sum_sq + (x_diff * x_diff)
        y_sum_sq = y_sum_sq + (y_diff * y_diff)
      }
      
      let denominator = (x_sum_sq * y_sum_sq).sqrt()
      if denominator == 0.0 {
        0.0
      } else {
        numerator / denominator
      }
    }
  }
  
  let cpu_usage_values = telemetry_data.map(fn(m) { m.cpu_usage })
  let memory_usage_values = telemetry_data.map(fn(m) { m.memory_usage })
  let cpu_memory_correlation = calculate_correlation(cpu_usage_values, memory_usage_values)
  
  // 验证相关系数（应该在0到1之间）
  assert_true(cpu_memory_correlation >= 0.0 and cpu_memory_correlation <= 1.0)
  
  // 计算延迟与CPU使用率的相关性
  let latency_cpu_correlation = calculate_correlation(
    telemetry_data.map(fn(m) { m.latency.to_float() }),
    cpu_usage_values
  )
  
  // 验证相关系数（应该在0到1之间）
  assert_true(latency_cpu_correlation >= 0.0 and latency_cpu_correlation <= 1.0)
  
  // 计算标准差
  let calculate_standard_deviation = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      let n = values.length().to_float()
      let mean = values.reduce(fn(acc, x) { acc + x }, 0.0) / n
      
      let mut sum_sq_diff = 0.0
      for value in values {
        let diff = value - mean
        sum_sq_diff = sum_sq_diff + (diff * diff)
      }
      
      (sum_sq_diff / n).sqrt()
    }
  }
  
  let cpu_std_dev = calculate_standard_deviation(cpu_usage_values)
  let memory_std_dev = calculate_standard_deviation(memory_usage_values)
  
  // 验证标准差为正数
  assert_true(cpu_std_dev > 0.0)
  assert_true(memory_std_dev > 0.0)
  
  // 计算变异系数
  let calculate_coefficient_of_variation = fn(values: Array[Float]) {
    if values.length() == 0 {
      0.0
    } else {
      let mean = values.reduce(fn(acc, x) { acc + x }, 0.0) / values.length().to_float()
      let std_dev = calculate_standard_deviation(values)
      
      if mean == 0.0 {
        0.0
      } else {
        std_dev / mean
      }
    }
  }
  
  let cpu_cv = calculate_coefficient_of_variation(cpu_usage_values)
  let memory_cv = calculate_coefficient_of_variation(memory_usage_values)
  
  // 验证变异系数为正数
  assert_true(cpu_cv > 0.0)
  assert_true(memory_cv > 0.0)
}

// 测试4: 遥测时间序列分析功能
test "遥测时间序列分析功能测试" {
  // 创建模拟的时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 120.5, metric: "response_time" },
    { timestamp: 1640995260, value: 115.2, metric: "response_time" },
    { timestamp: 1640995320, value: 130.8, metric: "response_time" },
    { timestamp: 1640995380, value: 125.3, metric: "response_time" },
    { timestamp: 1640995440, value: 140.7, metric: "response_time" },
    { timestamp: 1640995500, value: 135.9, metric: "response_time" },
    { timestamp: 1640995560, value: 150.2, metric: "response_time" },
    { timestamp: 1640995620, value: 145.6, metric: "response_time" }
  ]
  
  // 计算移动平均
  let calculate_moving_average = fn(data: Array[TimeSeriesPoint], window_size: Int) {
    if window_size <= 0 or data.length() < window_size {
      []
    } else {
      let mut result = []
      for i in window_size - 1..data.length() {
        let mut sum = 0.0
        for j in i - window_size + 1..=i {
          sum = sum + data[j].value
        }
        let avg = sum / window_size.to_float()
        result = result.push({ timestamp: data[i].timestamp, value: avg, metric: data[i].metric + "_ma" })
      }
      result
    }
  }
  
  let ma_3 = calculate_moving_average(time_series_data, 3)
  let ma_5 = calculate_moving_average(time_series_data, 5)
  
  // 验证移动平均计算
  assert_eq(ma_3.length(), 6)  // 8 - 3 + 1
  assert_eq(ma_5.length(), 4)  // 8 - 5 + 1
  
  // 验证第一个移动平均值
  let first_ma_3 = (120.5 + 115.2 + 130.8) / 3.0
  assert_true((ma_3[0].value - first_ma_3).abs() < 0.01)
  
  // 计算趋势
  let calculate_trend = fn(data: Array[TimeSeriesPoint]) {
    if data.length() < 2 {
      "stable"
    } else {
      let first_value = data[0].value
      let last_value = data[data.length() - 1].value
      let change_percent = ((last_value - first_value) / first_value) * 100.0
      
      if change_percent > 10.0 {
        "increasing"
      } else if change_percent < -10.0 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  let trend = calculate_trend(time_series_data)
  
  // 验证趋势计算
  let first_value = time_series_data[0].value
  let last_value = time_series_data[time_series_data.length() - 1].value
  let change_percent = ((last_value - first_value) / first_value) * 100.0
  
  if change_percent > 10.0 {
    assert_eq(trend, "increasing")
  } else if change_percent < -10.0 {
    assert_eq(trend, "decreasing")
  } else {
    assert_eq(trend, "stable")
  }
  
  // 计算季节性（简化版）
  let detect_seasonality = fn(data: Array[TimeSeriesPoint], period: Int) {
    if data.length() < period * 2 {
      false
    } else {
      let mut seasonal_patterns = 0
      for i in period..data.length() {
        let current_value = data[i].value
        let previous_period_value = data[i - period].value
        
        // 简单的季节性检测：当前值与上一周期值的差异是否在合理范围内
        let diff_percent = ((current_value - previous_period_value) / previous_period_value) * 100.0
        if diff_percent.abs() < 20.0 {
          seasonal_patterns = seasonal_patterns + 1
        }
      }
      
      let seasonal_ratio = seasonal_patterns.to_float() / (data.length() - period).to_float()
      seasonal_ratio > 0.7
    }
  }
  
  // 检测周期为4的季节性
  let has_seasonality = detect_seasonality(time_series_data, 4)
  
  // 计算自相关
  let calculate_autocorrelation = fn(data: Array[TimeSeriesPoint], lag: Int) {
    if data.length() <= lag or lag <= 0 {
      0.0
    } else {
      let n = (data.length() - lag).to_float()
      let values = data.map(fn(p) { p.value })
      
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
      
      let mut numerator = 0.0
      let mut denominator = 0.0
      
      for i in lag..data.length() {
        let x_diff = data[i].value - mean
        let y_diff = data[i - lag].value - mean
        
        numerator = numerator + (x_diff * y_diff)
      }
      
      for i in 0..data.length() {
        let diff = data[i].value - mean
        denominator = denominator + (diff * diff)
      }
      
      if denominator == 0.0 {
        0.0
      } else {
        numerator / denominator
      }
    }
  }
  
  let autocorr_lag_1 = calculate_autocorrelation(time_series_data, 1)
  let autocorr_lag_2 = calculate_autocorrelation(time_series_data, 2)
  
  // 验证自相关系数在-1到1之间
  assert_true(autocorr_lag_1 >= -1.0 and autocorr_lag_1 <= 1.0)
  assert_true(autocorr_lag_2 >= -1.0 and autocorr_lag_2 <= 1.0)
}

// 测试5: 遥测异常检测功能
test "遥测异常检测功能测试" {
  // 创建包含异常的模拟数据
  let telemetry_data = [
    { timestamp: 1640995200, metric: "cpu_usage", value: 45.2, service: "api-gateway" },
    { timestamp: 1640995260, metric: "cpu_usage", value: 48.7, service: "api-gateway" },
    { timestamp: 1640995320, metric: "cpu_usage", value: 52.1, service: "api-gateway" },
    { timestamp: 1640995380, metric: "cpu_usage", value: 95.3, service: "api-gateway" },  // 异常点
    { timestamp: 1640995440, metric: "cpu_usage", value: 49.8, service: "api-gateway" },
    { timestamp: 1640995500, metric: "cpu_usage", value: 46.5, service: "api-gateway" },
    { timestamp: 1640995560, metric: "cpu_usage", value: 44.9, service: "api-gateway" },
    { timestamp: 1640995620, metric: "cpu_usage", value: 47.3, service: "api-gateway" }
  ]
  
  // 使用Z-Score方法检测异常
  let detect_anomalies_zscore = fn(data: Array[MetricPoint], threshold: Float) {
    if data.length() < 3 {
      []
    } else {
      let values = data.map(fn(p) { p.value })
      let n = values.length().to_float()
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / n
      
      let mut sum_sq_diff = 0.0
      for value in values {
        let diff = value - mean
        sum_sq_diff = sum_sq_diff + (diff * diff)
      }
      let std_dev = (sum_sq_diff / n).sqrt()
      
      let mut anomalies = []
      for point in data {
        let z_score = (point.value - mean) / std_dev
        if z_score.abs() > threshold {
          anomalies = anomalies.push({ point, z_score })
        }
      }
      anomalies
    }
  }
  
  let anomalies = detect_anomalies_zscore(telemetry_data, 2.0)
  
  // 验证异常检测
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].point.timestamp, 1640995380)
  assert_eq(anomalies[0].point.value, 95.3)
  assert_true(anomalies[0].z_score > 2.0)
  
  // 使用IQR方法检测异常
  let detect_anomalies_iqr = fn(data: Array[MetricPoint]) {
    if data.length() < 4 {
      []
    } else {
      let values = data.map(fn(p) { p.value }).sort(fn(a, b) { a - b })
      let n = values.length()
      
      let q1_index = (n / 4).to_int()
      let q3_index = (3 * n / 4).to_int()
      
      let q1 = values[q1_index]
      let q3 = values[q3_index]
      let iqr = q3 - q1
      
      let lower_bound = q1 - 1.5 * iqr
      let upper_bound = q3 + 1.5 * iqr
      
      let mut anomalies = []
      for point in data {
        if point.value < lower_bound or point.value > upper_bound {
          anomalies = anomalies.push(point)
        }
      }
      anomalies
    }
  }
  
  let iqr_anomalies = detect_anomalies_iqr(telemetry_data)
  
  // 验证IQR异常检测
  assert_eq(iqr_anomalies.length(), 1)
  assert_eq(iqr_anomalies[0].timestamp, 1640995380)
  assert_eq(iqr_anomalies[0].value, 95.3)
  
  // 使用移动平均方法检测异常
  let detect_anomalies_moving_avg = fn(data: Array[MetricPoint], window_size: Int, threshold: Float) {
    if data.length() < window_size + 1 {
      []
    } else {
      let mut anomalies = []
      
      for i in window_size..data.length() {
        let mut sum = 0.0
        for j in i - window_size..i - 1 {
          sum = sum + data[j].value
        }
        let moving_avg = sum / window_size.to_float()
        
        let current_point = data[i]
        let deviation_percent = ((current_point.value - moving_avg) / moving_avg) * 100.0
        
        if deviation_percent.abs() > threshold {
          anomalies = anomalies.push({ point: current_point, moving_avg, deviation_percent })
        }
      }
      anomalies
    }
  }
  
  let ma_anomalies = detect_anomalies_moving_avg(telemetry_data, 3, 50.0)
  
  // 验证移动平均异常检测
  assert_eq(ma_anomalies.length(), 1)
  assert_eq(ma_anomalies[0].point.timestamp, 1640995380)
  assert_eq(ma_anomalies[0].point.value, 95.3)
  assert_true(ma_anomalies[0].deviation_percent > 50.0)
  
  // 创建多服务异常检测
  let multi_service_data = [
    { timestamp: 1640995200, metric: "cpu_usage", value: 45.2, service: "api-gateway" },
    { timestamp: 1640995260, metric: "cpu_usage", value: 48.7, service: "api-gateway" },
    { timestamp: 1640995320, metric: "cpu_usage", value: 52.1, service: "api-gateway" },
    { timestamp: 1640995380, metric: "cpu_usage", value: 95.3, service: "api-gateway" },  // 异常点
    { timestamp: 1640995440, metric: "cpu_usage", value: 49.8, service: "api-gateway" },
    { timestamp: 1640995500, metric: "cpu_usage", value: 30.5, service: "user-service" },
    { timestamp: 1640995560, metric: "cpu_usage", value: 32.1, service: "user-service" },
    { timestamp: 1640995620, metric: "cpu_usage", value: 35.7, service: "user-service" },
    { timestamp: 1640995680, metric: "cpu_usage", value: 31.2, service: "user-service" }
  ]
  
  // 按服务分组并检测异常
  let group_by_service = fn(data: Array[MetricPoint]) {
    let mut groups = {}
    for point in data {
      let service_name = point.service
      let existing_points = groups.get(service_name, [])
      groups = groups.set(service_name, existing_points.push(point))
    }
    groups
  }
  
  let grouped_data = group_by_service(multi_service_data)
  
  // 检测每个服务的异常
  let detect_service_anomalies = fn(service_data: Array[MetricPoint], threshold: Float) {
    detect_anomalies_zscore(service_data, threshold)
  }
  
  let api_gateway_anomalies = detect_service_anomalies(grouped_data.get("api-gateway", []), 2.0)
  let user_service_anomalies = detect_service_anomalies(grouped_data.get("user-service", []), 2.0)
  
  // 验证服务级异常检测
  assert_eq(api_gateway_anomalies.length(), 1)
  assert_eq(user_service_anomalies.length(), 0)
}

// 测试6: 遥测报告生成功能
test "遥测报告生成功能测试" {
  // 创建模拟的遥测数据
  let telemetry_data = [
    { service: "api-gateway", latency: 120, status: 200, timestamp: 1640995200, region: "us-east-1" },
    { service: "api-gateway", latency: 95, status: 200, timestamp: 1640995260, region: "us-east-1" },
    { service: "api-gateway", latency: 250, status: 500, timestamp: 1640995320, region: "us-west-2" },
    { service: "user-service", latency: 80, status: 200, timestamp: 1640995380, region: "us-east-1" },
    { service: "user-service", latency: 65, status: 200, timestamp: 1640995440, region: "eu-west-1" },
    { service: "payment-service", latency: 300, status: 200, timestamp: 1640995500, region: "us-west-2" },
    { service: "payment-service", latency: 450, status: 500, timestamp: 1640995560, region: "us-west-2" },
    { service: "notification-service", latency: 40, status: 200, timestamp: 1640995620, region: "eu-west-1" }
  ]
  
  // 生成服务摘要报告
  let generate_service_summary = fn(data: Array[ServiceMetricWithRegion]) {
    // 按服务分组
    let mut groups = {}
    for metric in data {
      let service_name = metric.service
      let existing_metrics = groups.get(service_name, [])
      groups = groups.set(service_name, existing_metrics.push(metric))
    }
    
    // 为每个服务生成摘要
    let mut summaries = []
    for (service_name, metrics) in groups {
      let total_requests = metrics.length()
      let mut error_count = 0
      let mut total_latency = 0
      
      for metric in metrics {
        if metric.status != 200 {
          error_count = error_count + 1
        }
        total_latency = total_latency + metric.latency
      }
      
      let avg_latency = total_latency / total_requests
      let error_rate = (error_count * 100) / total_requests
      
      let summary = {
        service: service_name,
        total_requests,
        error_count,
        avg_latency,
        error_rate
      }
      
      summaries = summaries.push(summary)
    }
    
    summaries
  }
  
  let service_summaries = generate_service_summary(telemetry_data)
  
  // 验证服务摘要
  assert_eq(service_summaries.length(), 4)
  
  // 验证api-gateway摘要
  let api_gateway_summary = service_summaries.find(fn(s) { s.service == "api-gateway" })
  assert_eq(api_gateway_summary.service, "api-gateway")
  assert_eq(api_gateway_summary.total_requests, 3)
  assert_eq(api_gateway_summary.error_count, 1)
  assert_eq(api_gateway_summary.avg_latency, (120 + 95 + 250) / 3)
  assert_eq(api_gateway_summary.error_rate, 33)
  
  // 验证user-service摘要
  let user_service_summary = service_summaries.find(fn(s) { s.service == "user-service" })
  assert_eq(user_service_summary.service, "user-service")
  assert_eq(user_service_summary.total_requests, 2)
  assert_eq(user_service_summary.error_count, 0)
  assert_eq(user_service_summary.avg_latency, (80 + 65) / 2)
  assert_eq(user_service_summary.error_rate, 0)
  
  // 生成区域摘要报告
  let generate_region_summary = fn(data: Array[ServiceMetricWithRegion]) {
    // 按区域分组
    let mut groups = {}
    for metric in data {
      let region_name = metric.region
      let existing_metrics = groups.get(region_name, [])
      groups = groups.set(region_name, existing_metrics.push(metric))
    }
    
    // 为每个区域生成摘要
    let mut summaries = []
    for (region_name, metrics) in groups {
      let total_requests = metrics.length()
      let mut error_count = 0
      let mut total_latency = 0
      let mut services = []
      
      for metric in metrics {
        if metric.status != 200 {
          error_count = error_count + 1
        }
        total_latency = total_latency + metric.latency
        
        if not(services.contains(metric.service)) {
          services = services.push(metric.service)
        }
      }
      
      let avg_latency = total_latency / total_requests
      let error_rate = (error_count * 100) / total_requests
      
      let summary = {
        region: region_name,
        total_requests,
        error_count,
        avg_latency,
        error_rate,
        service_count: services.length()
      }
      
      summaries = summaries.push(summary)
    }
    
    summaries
  }
  
  let region_summaries = generate_region_summary(telemetry_data)
  
  // 验证区域摘要
  assert_eq(region_summaries.length(), 3)
  
  // 验证us-east-1摘要
  let us_east_summary = region_summaries.find(fn(s) { s.region == "us-east-1" })
  assert_eq(us_east_summary.region, "us-east-1")
  assert_eq(us_east_summary.total_requests, 3)
  assert_eq(us_east_summary.error_count, 0)
  assert_eq(us_east_summary.service_count, 2)
  
  // 生成时间范围报告
  let generate_time_range_report = fn(data: Array[ServiceMetricWithRegion], start_time: Int, end_time: Int) {
    let filtered_data = []
    for metric in data {
      if metric.timestamp >= start_time and metric.timestamp <= end_time {
        filtered_data = filtered_data.push(metric)
      }
    }
    
    let total_requests = filtered_data.length()
    let mut error_count = 0
    let mut total_latency = 0
    
    for metric in filtered_data {
      if metric.status != 200 {
        error_count = error_count + 1
      }
      total_latency = total_latency + metric.latency
    }
    
    let avg_latency = if total_requests > 0 {
      total_latency / total_requests
    } else {
      0
    }
    
    let error_rate = if total_requests > 0 {
      (error_count * 100) / total_requests
    } else {
      0
    }
    
    {
      time_range: { start: start_time, end: end_time },
      total_requests,
      error_count,
      avg_latency,
      error_rate
    }
  }
  
  let time_report = generate_time_range_report(telemetry_data, 1640995300, 1640995500)
  
  // 验证时间范围报告
  assert_eq(time_report.time_range.start, 1640995300)
  assert_eq(time_report.time_range.end, 1640995500)
  assert_eq(time_report.total_requests, 3)
  assert_eq(time_report.error_count, 1)
  
  // 生成性能排名报告
  let generate_performance_ranking = fn(data: Array[ServiceMetricWithRegion]) {
    // 按服务分组
    let mut groups = {}
    for metric in data {
      let service_name = metric.service
      let existing_metrics = groups.get(service_name, [])
      groups = groups.set(service_name, existing_metrics.push(metric))
    }
    
    // 计算每个服务的平均延迟
    let mut service_performance = []
    for (service_name, metrics) in groups {
      let mut total_latency = 0
      for metric in metrics {
        total_latency = total_latency + metric.latency
      }
      let avg_latency = total_latency / metrics.length()
      
      service_performance = service_performance.push({
        service: service_name,
        avg_latency,
        request_count: metrics.length()
      })
    }
    
    // 按平均延迟排序（升序，性能好的在前）
    service_performance.sort(fn(a, b) { a.avg_latency - b.avg_latency })
  }
  
  let performance_ranking = generate_performance_ranking(telemetry_data)
  
  // 验证性能排名
  assert_eq(performance_ranking.length(), 4)
  assert_eq(performance_ranking[0].service, "notification-service")  // 最低延迟
  assert_eq(performance_ranking[3].service, "payment-service")  // 最高延迟
  
  // 验证排序
  for i in 1..performance_ranking.length() {
    assert_true(performance_ranking[i - 1].avg_latency <= performance_ranking[i].avg_latency)
  }
}

// 测试7: 遥测数据可视化功能
test "遥测数据可视化功能测试" {
  // 创建模拟的遥测数据
  let telemetry_data = [
    { timestamp: 1640995200, value: 120, metric: "response_time" },
    { timestamp: 1640995260, value: 95, metric: "response_time" },
    { timestamp: 1640995320, value: 130, metric: "response_time" },
    { timestamp: 1640995380, value: 125, metric: "response_time" },
    { timestamp: 1640995440, value: 140, metric: "response_time" },
    { timestamp: 1640995500, value: 135, metric: "response_time" },
    { timestamp: 1640995560, value: 150, metric: "response_time" },
    { timestamp: 1640995620, value: 145, metric: "response_time" }
  ]
  
  // 生成线图数据
  let generate_line_chart_data = fn(data: Array[TimeSeriesPoint]) {
    let mut chart_data = []
    for point in data {
      chart_data = chart_data.push({
        x: point.timestamp,
        y: point.value
      })
    }
    chart_data
  }
  
  let line_chart_data = generate_line_chart_data(telemetry_data)
  
  // 验证线图数据
  assert_eq(line_chart_data.length(), 8)
  assert_eq(line_chart_data[0].x, 1640995200)
  assert_eq(line_chart_data[0].y, 120)
  assert_eq(line_chart_data[7].x, 1640995620)
  assert_eq(line_chart_data[7].y, 145)
  
  // 生成柱状图数据
  let generate_bar_chart_data = fn(data: Array[TimeSeriesPoint], bin_size: Int) {
    // 找出最小和最大值
    let values = data.map(fn(p) { p.value })
    let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    // 计算bins
    let num_bins = ((max_value - min_value) / bin_size).to_int() + 1
    let mut bins = []
    
    // 初始化bins
    for i in 0..num_bins {
      let bin_min = min_value + (i * bin_size)
      let bin_max = bin_min + bin_size - 1
      bins = bins.push({
        range: { min: bin_min, max: bin_max },
        count: 0
      })
    }
    
    // 统计每个bin的计数
    for point in data {
      let bin_index = ((point.value - min_value) / bin_size).to_int()
      if bin_index >= 0 and bin_index < bins.length() {
        let bin = bins[bin_index]
        bins = bins.set(bin_index, { bin | count: bin.count + 1 })
      }
    }
    
    bins
  }
  
  let bar_chart_data = generate_bar_chart_data(telemetry_data, 20)
  
  // 验证柱状图数据
  assert_true(bar_chart_data.length() > 0)
  
  // 验证所有bins的计数总和等于数据点数量
  let total_count = bar_chart_data.reduce(fn(acc, bin) { acc + bin.count }, 0)
  assert_eq(total_count, telemetry_data.length())
  
  // 生成饼图数据
  let generate_pie_chart_data = fn(data: Array[ServiceMetricWithRegion]) {
    // 按服务分组
    let mut groups = {}
    for metric in data {
      let service_name = metric.service
      let existing_count = groups.get(service_name, 0)
      groups = groups.set(service_name, existing_count + 1)
    }
    
    // 转换为饼图数据格式
    let mut pie_data = []
    let total = data.length()
    
    for (service_name, count) in groups {
      let percentage = (count * 100) / total
      pie_data = pie_data.push({
        label: service_name,
        value: count,
        percentage
      })
    }
    
    // 按值排序
    pie_data.sort(fn(a, b) { b.value - a.value })
  }
  
  let service_data = [
    { service: "api-gateway", latency: 120, status: 200, timestamp: 1640995200, region: "us-east-1" },
    { service: "api-gateway", latency: 95, status: 200, timestamp: 1640995260, region: "us-east-1" },
    { service: "api-gateway", latency: 250, status: 500, timestamp: 1640995320, region: "us-west-2" },
    { service: "user-service", latency: 80, status: 200, timestamp: 1640995380, region: "us-east-1" },
    { service: "user-service", latency: 65, status: 200, timestamp: 1640995440, region: "eu-west-1" },
    { service: "payment-service", latency: 300, status: 200, timestamp: 1640995500, region: "us-west-2" },
    { service: "payment-service", latency: 450, status: 500, timestamp: 1640995560, region: "us-west-2" },
    { service: "notification-service", latency: 40, status: 200, timestamp: 1640995620, region: "eu-west-1" }
  ]
  
  let pie_chart_data = generate_pie_chart_data(service_data)
  
  // 验证饼图数据
  assert_eq(pie_chart_data.length(), 4)
  
  // 验证百分比总和为100%
  let total_percentage = pie_chart_data.reduce(fn(acc, item) { acc + item.percentage }, 0)
  assert_eq(total_percentage, 100)
  
  // 验证api-gateway的数据
  let api_gateway_data = pie_chart_data.find(fn(item) { item.label == "api-gateway" })
  assert_eq(api_gateway_data.label, "api-gateway")
  assert_eq(api_gateway_data.value, 3)
  assert_eq(api_gateway_data.percentage, 37)  // 3/8 * 100 ≈ 37%
  
  // 生成热力图数据
  let generate_heatmap_data = fn(data: Array[ServiceMetricWithRegion]) {
    // 按服务和区域分组
    let mut groups = {}
    for metric in data {
      let service_name = metric.service
      let region_name = metric.region
      
      let key = service_name + ":" + region_name
      let existing_data = groups.get(key, { count: 0, total_latency: 0 })
      
      groups = groups.set(key, {
        count: existing_data.count + 1,
        total_latency: existing_data.total_latency + metric.latency
      })
    }
    
    // 计算每个组合的平均延迟
    let mut heatmap_data = []
    for (key, data) in groups {
      let parts = key.split(":")
      let service_name = parts[0]
      let region_name = parts[1]
      let avg_latency = data.total_latency / data.count
      
      heatmap_data = heatmap_data.push({
        x: service_name,
        y: region_name,
        value: avg_latency
      })
    }
    
    heatmap_data
  }
  
  let heatmap_data = generate_heatmap_data(service_data)
  
  // 验证热力图数据
  assert_eq(heatmap_data.length(), 5)  // 5个不同的服务-区域组合
  
  // 验证api-gateway在us-east-1的数据
  let api_gateway_us_east = heatmap_data.find(fn(item) { item.x == "api-gateway" and item.y == "us-east-1" })
  assert_eq(api_gateway_us_east.x, "api-gateway")
  assert_eq(api_gateway_us_east.y, "us-east-1")
  assert_eq(api_gateway_us_east.value, (120 + 95) / 2)  // 平均延迟
}

// 测试8: 遥测预测分析功能
test "遥测预测分析功能测试" {
  // 创建模拟的时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 120.5, metric: "response_time" },
    { timestamp: 1640995260, value: 115.2, metric: "response_time" },
    { timestamp: 1640995320, value: 130.8, metric: "response_time" },
    { timestamp: 1640995380, value: 125.3, metric: "response_time" },
    { timestamp: 1640995440, value: 140.7, metric: "response_time" },
    { timestamp: 1640995500, value: 135.9, metric: "response_time" },
    { timestamp: 1640995560, value: 150.2, metric: "response_time" },
    { timestamp: 1640995620, value: 145.6, metric: "response_time" }
  ]
  
  // 简单线性回归预测
  let linear_regression_predict = fn(data: Array[TimeSeriesPoint], forecast_steps: Int) {
    if data.length() < 2 {
      []
    } else {
      let n = data.length().to_float()
      let values = data.map(fn(p) { p.value })
      
      // 计算线性回归参数 y = ax + b
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for i in 0..data.length() {
        let x = i.to_float()
        let y = values[i]
        
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + (x * y)
        sum_x2 = sum_x2 + (x * x)
      }
      
      let a = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let b = (sum_y - a * sum_x) / n
      
      // 生成预测
      let mut predictions = []
      let last_timestamp = data[data.length() - 1].timestamp
      let time_interval = if data.length() > 1 {
        data[1].timestamp - data[0].timestamp
      } else {
        60  // 默认60秒间隔
      }
      
      for i in 1..=forecast_steps {
        let x = (data.length() + i - 1).to_float()
        let predicted_value = a * x + b
        let predicted_timestamp = last_timestamp + (i * time_interval)
        
        predictions = predictions.push({
          timestamp: predicted_timestamp,
          value: predicted_value,
          metric: data[0].metric + "_predicted"
        })
      }
      
      predictions
    }
  }
  
  let predictions = linear_regression_predict(time_series_data, 3)
  
  // 验证预测结果
  assert_eq(predictions.length(), 3)
  
  // 验证预测时间戳
  let last_timestamp = time_series_data[time_series_data.length() - 1].timestamp
  let time_interval = time_series_data[1].timestamp - time_series_data[0].timestamp
  
  assert_eq(predictions[0].timestamp, last_timestamp + time_interval)
  assert_eq(predictions[1].timestamp, last_timestamp + 2 * time_interval)
  assert_eq(predictions[2].timestamp, last_timestamp + 3 * time_interval)
  
  // 验证预测指标名称
  for prediction in predictions {
    assert_eq(prediction.metric, "response_time_predicted")
  }
  
  // 移动平均预测
  let moving_average_predict = fn(data: Array[TimeSeriesPoint], window_size: Int, forecast_steps: Int) {
    if data.length() < window_size or window_size <= 0 {
      []
    } else {
      let mut predictions = []
      let last_timestamp = data[data.length() - 1].timestamp
      let time_interval = if data.length() > 1 {
        data[1].timestamp - data[0].timestamp
      } else {
        60  // 默认60秒间隔
      }
      
      // 计算最后一个窗口的平均值
      let mut sum = 0.0
      for i in data.length() - window_size..data.length() {
        sum = sum + data[i].value
      }
      let last_ma = sum / window_size.to_float()
      
      // 使用最后一个移动平均值作为预测
      for i in 1..=forecast_steps {
        let predicted_timestamp = last_timestamp + (i * time_interval)
        
        predictions = predictions.push({
          timestamp: predicted_timestamp,
          value: last_ma,
          metric: data[0].metric + "_ma_predicted"
        })
      }
      
      predictions
    }
  }
  
  let ma_predictions = moving_average_predict(time_series_data, 3, 2)
  
  // 验证移动平均预测
  assert_eq(ma_predictions.length(), 2)
  
  // 验证预测值等于最后一个移动平均值
  let expected_ma = (time_series_data[5].value + time_series_data[6].value + time_series_data[7].value) / 3.0
  for prediction in ma_predictions {
    assert_eq(prediction.value, expected_ma)
    assert_eq(prediction.metric, "response_time_ma_predicted")
  }
  
  // 季节性预测（简化版）
  let seasonal_predict = fn(data: Array[TimeSeriesPoint], period: Int, forecast_steps: Int) {
    if data.length() < period or period <= 0 {
      []
    } else {
      let mut predictions = []
      let last_timestamp = data[data.length() - 1].timestamp
      let time_interval = if data.length() > 1 {
        data[1].timestamp - data[0].timestamp
      } else {
        60  // 默认60秒间隔
      }
      
      // 使用历史周期数据进行预测
      for i in 1..=forecast_steps {
        let historical_index = (data.length() - period + i - 1) % data.length()
        let predicted_value = data[historical_index].value
        let predicted_timestamp = last_timestamp + (i * time_interval)
        
        predictions = predictions.push({
          timestamp: predicted_timestamp,
          value: predicted_value,
          metric: data[0].metric + "_seasonal_predicted"
        })
      }
      
      predictions
    }
  }
  
  let seasonal_predictions = seasonal_predict(time_series_data, 4, 2)
  
  // 验证季节性预测
  assert_eq(seasonal_predictions.length(), 2)
  
  // 验证预测值来自历史周期数据
  let historical_index_1 = (time_series_data.length() - 4 + 1 - 1) % time_series_data.length()
  let historical_index_2 = (time_series_data.length() - 4 + 2 - 1) % time_series_data.length()
  
  assert_eq(seasonal_predictions[0].value, time_series_data[historical_index_1].value)
  assert_eq(seasonal_predictions[1].value, time_series_data[historical_index_2].value)
  
  // 预测准确性评估
  let evaluate_prediction_accuracy = fn(actual_data: Array[TimeSeriesPoint], predicted_data: Array[TimeSeriesPoint]) {
    if actual_data.length() != predicted_data.length() or actual_data.length() == 0 {
      { mae: 0.0, mse: 0.0, rmse: 0.0, mape: 0.0 }
    } else {
      let mut mae = 0.0  // 平均绝对误差
      let mut mse = 0.0  // 均方误差
      let mut mape = 0.0  // 平均绝对百分比误差
      
      for i in 0..actual_data.length() {
        let actual = actual_data[i].value
        let predicted = predicted_data[i].value
        
        let abs_error = (actual - predicted).abs()
        let squared_error = abs_error * abs_error
        let percentage_error = if actual != 0.0 {
          (abs_error / actual.abs()) * 100.0
        } else {
          0.0
        }
        
        mae = mae + abs_error
        mse = mse + squared_error
        mape = mape + percentage_error
      }
      
      let n = actual_data.length().to_float()
      mae = mae / n
      mse = mse / n
      let rmse = mse.sqrt()
      mape = mape / n
      
      { mae, mse, rmse, mape }
    }
  }
  
  // 使用部分数据进行测试
  let training_data = []
  for i in 0..5 {
    training_data = training_data.push(time_series_data[i])
  }
  
  let test_data = []
  for i in 5..7 {
    test_data = test_data.push(time_series_data[i])
  }
  
  let test_predictions = linear_regression_predict(training_data, 2)
  let accuracy_metrics = evaluate_prediction_accuracy(test_data, test_predictions)
  
  // 验证准确性指标
  assert_true(accuracy_metrics.mae >= 0.0)
  assert_true(accuracy_metrics.mse >= 0.0)
  assert_true(accuracy_metrics.rmse >= 0.0)
  assert_true(accuracy_metrics.mape >= 0.0)
}