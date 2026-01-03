// Azimuth 遥测数据多维度分析测试用例
// 专注于遥测系统中的多维度数据分析和洞察功能

// 测试1: 多维度数据聚合分析
test "多维度数据聚合分析" {
  // 模拟多维度遥测数据
  let multi_dimensional_data = [
    { timestamp: 1640995200, service: "auth", region: "us-east", env: "prod", metric: "cpu", value: 45.0 },
    { timestamp: 1640995260, service: "auth", region: "us-east", env: "prod", metric: "memory", value: 1024.0 },
    { timestamp: 1640995320, service: "auth", region: "us-west", env: "staging", metric: "cpu", value: 35.0 },
    { timestamp: 1640995380, service: "db", region: "us-east", env: "prod", metric: "cpu", value: 70.0 },
    { timestamp: 1640995440, service: "db", region: "us-east", env: "prod", metric: "disk_io", value: 150.0 },
    { timestamp: 1640995500, service: "db", region: "us-west", env: "staging", metric: "cpu", value: 60.0 },
    { timestamp: 1640995560, service: "api", region: "us-east", env: "prod", metric: "cpu", value: 30.0 },
    { timestamp: 1640995620, service: "api", region: "us-east", env: "prod", metric: "memory", value: 512.0 },
    { timestamp: 1640995680, service: "api", region: "eu-west", env: "prod", metric: "cpu", value: 25.0 },
    { timestamp: 1640995740, service: "cache", region: "us-east", env: "prod", metric: "cpu", value: 20.0 },
    { timestamp: 1640995800, service: "cache", region: "us-east", env: "prod", metric: "memory", value: 256.0 },
    { timestamp: 1640995860, service: "cache", region: "eu-west", env: "dev", metric: "cpu", value: 15.0 }
  ]
  
  // 按服务维度聚合
  let mut service_aggregation = {}
  for data_point in multi_dimensional_data {
    let service_data = service_aggregation.get(data_point.service).unwrap_or({
      metrics: {},
      count: 0,
      regions: {},
      environments: {}
    })
    
    // 按指标聚合
    let metrics = service_data.metrics
    let metric_data = metrics.get(data_point.metric).unwrap_or({
      values: [],
      sum: 0.0,
      count: 0
    })
    
    let updated_values = metric_data.values.push(data_point.value)
    let updated_sum = metric_data.sum + data_point.value
    let updated_count = metric_data.count + 1
    
    let updated_metrics = metrics.set(data_point.metric, {
      values: updated_values,
      sum: updated_sum,
      count: updated_count,
      avg: updated_sum / updated_count.to_float()
    })
    
    // 按区域聚合
    let regions = service_data.regions
    let region_count = regions.get(data_point.region).unwrap_or(0) + 1
    let updated_regions = regions.set(data_point.region, region_count)
    
    // 按环境聚合
    let environments = service_data.environments
    let env_count = environments.get(data_point.env).unwrap_or(0) + 1
    let updated_environments = environments.set(data_point.env, env_count)
    
    service_aggregation = service_aggregation.set(data_point.service, {
      metrics: updated_metrics,
      count: service_data.count + 1,
      regions: updated_regions,
      environments: updated_environments
    })
  }
  
  // 验证服务维度聚合结果
  assert_eq(service_aggregation.size(), 4) // 应该有4个服务
  
  // 验证auth服务聚合
  let auth_service = service_aggregation.get("auth").unwrap()
  assert_eq(auth_service.count, 3)
  assert_eq(auth_service.metrics.size(), 2) // cpu和memory
  assert_eq(auth_service.metrics.get("cpu").unwrap().avg, 40.0) // (45+35)/2
  assert_eq(auth_service.metrics.get("memory").unwrap().avg, 1024.0)
  assert_eq(auth_service.regions.get("us-east").unwrap(), 2)
  assert_eq(auth_service.regions.get("us-west").unwrap(), 1)
  assert_eq(auth_service.environments.get("prod").unwrap(), 2)
  assert_eq(auth_service.environments.get("staging").unwrap(), 1)
  
  // 验证db服务聚合
  let db_service = service_aggregation.get("db").unwrap()
  assert_eq(db_service.count, 3)
  assert_eq(db_service.metrics.get("cpu").unwrap().avg, 65.0) // (70+60)/2
  
  // 按区域维度聚合
  let mut region_aggregation = {}
  for data_point in multi_dimensional_data {
    let region_data = region_aggregation.get(data_point.region).unwrap_or({
      services: {},
      metrics: {},
      count: 0
    })
    
    // 按服务聚合
    let services = region_data.services
    let service_count = services.get(data_point.service).unwrap_or(0) + 1
    let updated_services = services.set(data_point.service, service_count)
    
    // 按指标聚合
    let metrics = region_data.metrics
    let metric_data = metrics.get(data_point.metric).unwrap_or({
      values: [],
      sum: 0.0,
      count: 0
    })
    
    let updated_values = metric_data.values.push(data_point.value)
    let updated_sum = metric_data.sum + data_point.value
    let updated_count = metric_data.count + 1
    
    let updated_metrics = metrics.set(data_point.metric, {
      values: updated_values,
      sum: updated_sum,
      count: updated_count,
      avg: updated_sum / updated_count.to_float()
    })
    
    region_aggregation = region_aggregation.set(data_point.region, {
      services: updated_services,
      metrics: updated_metrics,
      count: region_data.count + 1
    })
  }
  
  // 验证区域维度聚合结果
  assert_eq(region_aggregation.size(), 3) // 应该有3个区域
  
  // 验证us-east区域聚合
  let us_east_region = region_aggregation.get("us-east").unwrap()
  assert_eq(us_east_region.count, 8)
  assert_eq(us_east_region.services.size(), 4) // auth, db, api, cache
  assert_eq(us_east_region.metrics.get("cpu").unwrap().avg, 41.25) // (45+70+30+20)/4
}

// 测试2: 多维度关联性分析
test "多维度关联性分析" {
  // 模拟时间序列多维度数据
  let time_series_data = [
    { timestamp: 1640995200, cpu: 30.0, memory: 60.0, request_count: 100, response_time: 50.0 },
    { timestamp: 1640995260, cpu: 35.0, memory: 65.0, request_count: 120, response_time: 55.0 },
    { timestamp: 1640995320, cpu: 40.0, memory: 70.0, request_count: 150, response_time: 65.0 },
    { timestamp: 1640995380, cpu: 50.0, memory: 75.0, request_count: 200, response_time: 80.0 },
    { timestamp: 1640995440, cpu: 60.0, memory: 80.0, request_count: 250, response_time: 95.0 },
    { timestamp: 1640995500, cpu: 70.0, memory: 85.0, request_count: 300, response_time: 120.0 },
    { timestamp: 1640995560, cpu: 65.0, memory: 82.0, request_count: 280, response_time: 110.0 },
    { timestamp: 1640995620, cpu: 55.0, memory: 78.0, request_count: 220, response_time: 85.0 },
    { timestamp: 1640995680, cpu: 45.0, memory: 72.0, request_count: 180, response_time: 70.0 },
    { timestamp: 1640995740, cpu: 35.0, memory: 68.0, request_count: 140, response_time: 60.0 }
  ]
  
  // 计算相关性矩阵
  let dimensions = ["cpu", "memory", "request_count", "response_time"]
  let mut correlation_matrix = {}
  
  // 计算各维度的平均值
  let mut dimension_means = {}
  for dim in dimensions {
    let mut sum = 0.0
    for data_point in time_series_data {
      let value = match dim {
        "cpu" => data_point.cpu
        "memory" => data_point.memory
        "request_count" => data_point.request_count.to_float()
        "response_time" => data_point.response_time
        _ => 0.0
      }
      sum = sum + value
    }
    dimension_means = dimension_means.set(dim, sum / time_series_data.length().to_float())
  }
  
  // 计算维度间的相关性
  for i in 0..dimensions.length() {
    for j in i..dimensions.length() {
      let dim1 = dimensions[i]
      let dim2 = dimensions[j]
      
      let mean1 = dimension_means.get(dim1).unwrap()
      let mean2 = dimension_means.get(dim2).unwrap()
      
      // 计算协方差
      let mut covariance = 0.0
      let mut variance1 = 0.0
      let mut variance2 = 0.0
      
      for data_point in time_series_data {
        let value1 = match dim1 {
          "cpu" => data_point.cpu
          "memory" => data_point.memory
          "request_count" => data_point.request_count.to_float()
          "response_time" => data_point.response_time
          _ => 0.0
        }
        
        let value2 = match dim2 {
          "cpu" => data_point.cpu
          "memory" => data_point.memory
          "request_count" => data_point.request_count.to_float()
          "response_time" => data_point.response_time
          _ => 0.0
        }
        
        let diff1 = value1 - mean1
        let diff2 = value2 - mean2
        
        covariance = covariance + diff1 * diff2
        variance1 = variance1 + diff1 * diff1
        variance2 = variance2 + diff2 * diff2
      }
      
      covariance = covariance / time_series_data.length().to_float()
      variance1 = variance1 / time_series_data.length().to_float()
      variance2 = variance2 / time_series_data.length().to_float()
      
      // 计算相关系数
      let correlation = if variance1 > 0.0 && variance2 > 0.0 {
        covariance / (variance1.sqrt() * variance2.sqrt())
      } else {
        0.0
      }
      
      let pair_key = dim1 + ":" + dim2
      correlation_matrix = correlation_matrix.set(pair_key, correlation)
    }
  }
  
  // 验证相关性分析结果
  assert_eq(correlation_matrix.size(), 10) // 4个维度的组合数 C(4,2)+4
  
  // CPU和响应时间应该有强正相关
  let cpu_response_corr = correlation_matrix.get("cpu:response_time").unwrap()
  assert_true(cpu_response_corr > 0.8) // 强正相关
  
  // 请求量和响应时间应该有强正相关
  let request_response_corr = correlation_matrix.get("request_count:response_time").unwrap()
  assert_true(request_response_corr > 0.8) // 强正相关
  
  // CPU和内存应该有正相关
  let cpu_memory_corr = correlation_matrix.get("cpu:memory").unwrap()
  assert_true(cpu_memory_corr > 0.5) // 正相关
  
  // 识别强相关关系（相关系数 > 0.7）
  let mut strong_correlations = []
  for (pair, correlation) in correlation_matrix {
    if correlation.abs() > 0.7 {
      let dims = pair.split(":")
      strong_correlations = strong_correlations.push({
        dimension1: dims[0],
        dimension2: dims[1],
        correlation: correlation,
        correlation_type: if correlation > 0 { "positive" } else { "negative" }
      })
    }
  }
  
  // 验证强相关关系
  assert_eq(strong_correlations.length(), 3) // 应该有3对强相关关系
  
  // 验证相关关系类型
  let positive_correlations = strong_correlations.filter(fn(c) { c.correlation_type == "positive" })
  assert_eq(positive_correlations.length(), 3) // 所有强相关都应该是正相关
}

// 测试3: 多维度趋势分析
test "多维度趋势分析" {
  // 模拟多个时间点的多维度数据
  let trend_data = [
    { timestamp: 1640995200, service: "auth", cpu: 30.0, memory: 60.0, error_rate: 0.1 },
    { timestamp: 1640995260, service: "auth", cpu: 32.0, memory: 62.0, error_rate: 0.1 },
    { timestamp: 1640995320, service: "auth", cpu: 35.0, memory: 65.0, error_rate: 0.2 },
    { timestamp: 1640995380, service: "auth", cpu: 38.0, memory: 68.0, error_rate: 0.3 },
    { timestamp: 1640995440, service: "auth", cpu: 42.0, memory: 70.0, error_rate: 0.5 },
    { timestamp: 1640995500, service: "auth", cpu: 45.0, memory: 73.0, error_rate: 0.8 },
    { timestamp: 1640995560, service: "auth", cpu: 48.0, memory: 75.0, error_rate: 1.2 },
    { timestamp: 1640995620, service: "auth", cpu: 50.0, memory: 78.0, error_rate: 1.8 },
    { timestamp: 1640995680, service: "auth", cpu: 52.0, memory: 80.0, error_rate: 2.5 },
    { timestamp: 1640995740, service: "auth", cpu: 55.0, memory: 82.0, error_rate: 3.5 }
  ]
  
  // 计算各维度的趋势
  let dimensions = ["cpu", "memory", "error_rate"]
  let mut trend_analysis = {}
  
  for dim in dimensions {
    let values = trend_data.map(fn(d) {
      match dim {
        "cpu" => d.cpu
        "memory" => d.memory
        "error_rate" => d.error_rate
        _ => 0.0
      }
    })
    
    // 计算线性趋势（简单线性回归）
    let n = values.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    for i in 0..values.length() {
      let x = i.to_float()
      let y = values[i]
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + x * y
      sum_x2 = sum_x2 + x * x
    }
    
    // 计算斜率和截距
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // 计算R²（决定系数）
    let mut ss_res = 0.0
    let mut ss_tot = 0.0
    let y_mean = sum_y / n
    
    for i in 0..values.length() {
      let x = i.to_float()
      let y = values[i]
      let y_pred = slope * x + intercept
      
      ss_res = ss_res + (y - y_pred) * (y - y_pred)
      ss_tot = ss_tot + (y - y_mean) * (y - y_mean)
    }
    
    let r_squared = if ss_tot > 0.0 { 1.0 - ss_res / ss_tot } else { 0.0 }
    
    // 确定趋势类型
    let trend_type = 
      if slope.abs() < 0.1 { "stable" }
      else if slope > 0.0 { "increasing" }
      else { "decreasing" }
    
    // 计算变化率
    let first_value = values[0]
    let last_value = values[values.length() - 1]
    let change_rate = (last_value - first_value) / first_value * 100.0
    
    trend_analysis = trend_analysis.set(dim, {
      slope: slope,
      intercept: intercept,
      r_squared: r_squared,
      trend_type: trend_type,
      change_rate: change_rate,
      first_value: first_value,
      last_value: last_value
    })
  }
  
  // 验证趋势分析结果
  assert_eq(trend_analysis.size(), 3)
  
  // 验证CPU趋势（应该是上升趋势）
  let cpu_trend = trend_analysis.get("cpu").unwrap()
  assert_eq(cpu_trend.trend_type, "increasing")
  assert_true(cpu_trend.slope > 0.0)
  assert_true(cpu_trend.change_rate > 80.0) // 从30增长到55，约83%
  assert_true(cpu_trend.r_squared > 0.9) // 强线性关系
  
  // 验证内存趋势（应该是上升趋势）
  let memory_trend = trend_analysis.get("memory").unwrap()
  assert_eq(memory_trend.trend_type, "increasing")
  assert_true(memory_trend.slope > 0.0)
  assert_true(memory_trend.change_rate > 30.0) // 从60增长到82，约37%
  assert_true(memory_trend.r_squared > 0.9) // 强线性关系
  
  // 验证错误率趋势（应该是急剧上升趋势）
  let error_rate_trend = trend_analysis.get("error_rate").unwrap()
  assert_eq(error_rate_trend.trend_type, "increasing")
  assert_true(error_rate_trend.slope > cpu_trend.slope) // 错误率增长更快
  assert_true(error_rate_trend.change_rate > 3000.0) // 从0.1增长到3.5，约3400%
  assert_true(error_rate_trend.r_squared > 0.8) // 强线性关系
  
  // 识别异常趋势（变化率超过阈值）
  let trend_change_threshold = 100.0 // 100%变化率阈值
  let mut anomalous_trends = []
  for (dim, trend) in trend_analysis {
    if trend.change_rate.abs() > trend_change_threshold {
      anomalous_trends = anomalous_trends.push({
        dimension: dim,
        trend_type: trend.trend_type,
        change_rate: trend.change_rate,
        severity: if trend.change_rate.abs() > 500.0 { "critical" } else { "warning" }
      })
    }
  }
  
  // 验证异常趋势识别
  assert_eq(anomalous_trends.length(), 1) // 只有错误率变化率超过100%
  assert_eq(anomalous_trends[0].dimension, "error_rate")
  assert_eq(anomalous_trends[0].severity, "critical")
}

// 测试4: 多维度异常模式识别
test "多维度异常模式识别" {
  // 模拟包含多维异常的数据
  let anomaly_pattern_data = [
    { timestamp: 1640995200, cpu: 30.0, memory: 60.0, network: 50.0, disk_io: 20.0, pattern: "normal" },
    { timestamp: 1640995260, cpu: 32.0, memory: 62.0, network: 52.0, disk_io: 22.0, pattern: "normal" },
    { timestamp: 1640995320, cpu: 95.0, memory: 98.0, network: 15.0, disk_io: 80.0, pattern: "resource_exhaustion" },
    { timestamp: 1640995380, cpu: 35.0, memory: 65.0, network: 55.0, disk_io: 25.0, pattern: "normal" },
    { timestamp: 1640995440, cpu: 25.0, memory: 40.0, network: 95.0, disk_io: 10.0, pattern: "network_bottleneck" },
    { timestamp: 1640995500, cpu: 30.0, memory: 62.0, network: 53.0, disk_io: 23.0, pattern: "normal" },
    { timestamp: 1640995560, cpu: 20.0, memory: 35.0, network: 20.0, disk_io: 90.0, pattern: "disk_bottleneck" },
    { timestamp: 1640995620, cpu: 33.0, memory: 64.0, network: 54.0, disk_io: 24.0, pattern: "normal" },
    { timestamp: 1640995680, cpu: 98.0, memory: 99.0, network: 10.0, disk_io: 85.0, pattern: "resource_exhaustion" },
    { timestamp: 1640995740, cpu: 31.0, memory: 63.0, network: 52.0, disk_io: 21.0, pattern: "normal" }
  ]
  
  // 定义正常基准
  let normal_data = anomaly_pattern_data.filter(fn(d) { d.pattern == "normal" })
  
  let mut baseline = {}
  let dimensions = ["cpu", "memory", "network", "disk_io"]
  
  for dim in dimensions {
    let values = normal_data.map(fn(d) {
      match dim {
        "cpu" => d.cpu
        "memory" => d.memory
        "network" => d.network
        "disk_io" => d.disk_io
        _ => 0.0
      }
    })
    
    let mut sum = 0.0
    for v in values {
      sum = sum + v
    }
    let mean = sum / values.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for v in values {
      let diff = v - mean
      variance_sum = variance_sum + diff * diff
    }
    let variance = variance_sum / values.length().to_float()
    let std_dev = variance.sqrt()
    
    baseline = baseline.set(dim, {
      mean: mean,
      std_dev: std_dev,
      upper_threshold: mean + 2.0 * std_dev,
      lower_threshold: mean - 2.0 * std_dev
    })
  }
  
  // 识别异常模式
  let mut detected_patterns = []
  for data_point in anomaly_pattern_data {
    let mut anomalous_dimensions = []
    let mut dimension_deviations = {}
    
    for dim in dimensions {
      let value = match dim {
        "cpu" => data_point.cpu
        "memory" => data_point.memory
        "network" => data_point.network
        "disk_io" => data_point.disk_io
        _ => 0.0
      }
      
      let dim_baseline = baseline.get(dim).unwrap()
      let deviation = (value - dim_baseline.mean) / dim_baseline.std_dev
      
      if deviation.abs() > 2.0 {
        anomalous_dimensions = anomalous_dimensions.push(dim)
        dimension_deviations = dimension_deviations.set(dim, deviation)
      }
    }
    
    // 基于异常维度组合识别模式
    let detected_pattern = 
      if anomalous_dimensions.length() >= 3 {
        let has_cpu_high = dimension_deviations.get("cpu").map_or(false, fn(d) { d > 2.0 })
        let has_mem_high = dimension_deviations.get("memory").map_or(false, fn(d) { d > 2.0 })
        let has_network_low = dimension_deviations.get("network").map_or(false, fn(d) { d < -2.0 })
        let has_disk_high = dimension_deviations.get("disk_io").map_or(false, fn(d) { d > 2.0 })
        
        if has_cpu_high && has_mem_high && has_network_low && has_disk_high {
          "resource_exhaustion"
        } else if has_cpu_high && has_mem_high {
          "memory_cpu_pressure"
        } else {
          "multi_dimension_anomaly"
        }
      } else if anomalous_dimensions.length() == 2 {
        let has_network_high = dimension_deviations.get("network").map_or(false, fn(d) { d > 2.0 })
        let has_disk_low = dimension_deviations.get("disk_io").map_or(false, fn(d) { d < -2.0 })
        
        if has_network_high && has_disk_low {
          "network_bottleneck"
        } else if dimension_deviations.contains("disk_io") && dimension_deviations.get("disk_io").unwrap() > 2.0 {
          "disk_bottleneck"
        } else {
          "dual_dimension_anomaly"
        }
      } else if anomalous_dimensions.length() == 1 {
        "single_dimension_anomaly"
      } else {
        "normal"
      }
    
    detected_patterns = detected_patterns.push({
      timestamp: data_point.timestamp,
      actual_pattern: data_point.pattern,
      detected_pattern: detected_pattern,
      anomalous_dimensions: anomalous_dimensions,
      is_correct: detected_pattern == data_point.pattern
    })
  }
  
  // 验证异常模式识别结果
  assert_eq(detected_patterns.length(), 10)
  
  // 验证正确识别的模式
  let correct_detections = detected_patterns.filter(fn(p) { p.is_correct })
  assert_true(correct_detections.length() >= 7) // 至少70%正确率
  
  // 验证资源耗尽模式识别
  let resource_exhaustion_detections = detected_patterns.filter(fn(p) { 
    p.detected_pattern == "resource_exhaustion" 
  })
  assert_eq(resource_exhaustion_detections.length(), 2)
  
  // 验证网络瓶颈模式识别
  let network_bottleneck_detections = detected_patterns.filter(fn(p) { 
    p.detected_pattern == "network_bottleneck" 
  })
  assert_eq(network_bottleneck_detections.length(), 1)
  
  // 验证磁盘瓶颈模式识别
  let disk_bottleneck_detections = detected_patterns.filter(fn(p) { 
    p.detected_pattern == "disk_bottleneck" 
  })
  assert_eq(disk_bottleneck_detections.length(), 1)
  
  // 验证正常模式识别
  let normal_detections = detected_patterns.filter(fn(p) { 
    p.detected_pattern == "normal" 
  })
  assert_eq(normal_detections.length(), 5)
}

// 测试5: 多维度下钻分析
test "多维度下钻分析" {
  // 模拟层次化多维度数据
  let hierarchical_data = [
    { timestamp: 1640995200, region: "us-east", az: "us-east-1a", service: "auth", instance: "auth-1", cpu: 30.0 },
    { timestamp: 1640995260, region: "us-east", az: "us-east-1a", service: "auth", instance: "auth-2", cpu: 32.0 },
    { timestamp: 1640995320, region: "us-east", az: "us-east-1b", service: "auth", instance: "auth-3", cpu: 35.0 },
    { timestamp: 1640995380, region: "us-east", az: "us-east-1b", service: "db", instance: "db-1", cpu: 70.0 },
    { timestamp: 1640995440, region: "us-east", az: "us-east-1b", service: "db", instance: "db-2", cpu: 75.0 },
    { timestamp: 1640995500, region: "us-west", az: "us-west-2a", service: "auth", instance: "auth-4", cpu: 25.0 },
    { timestamp: 1640995560, region: "us-west", az: "us-west-2a", service: "db", instance: "db-3", cpu: 60.0 },
    { timestamp: 1640995620, region: "us-west", az: "us-west-2b", service: "auth", instance: "auth-5", cpu: 28.0 },
    { timestamp: 1640995680, region: "us-west", az: "us-west-2b", service: "db", instance: "db-4", cpu: 65.0 },
    { timestamp: 1640995740, region: "eu-west", az: "eu-west-1a", service: "auth", instance: "auth-6", cpu: 20.0 }
  ]
  
  // 层次化维度定义
  let dimension_hierarchy = {
    "region": [],
    "az": ["region"],
    "service": [],
    "instance": ["service"]
  }
  
  // 构建层次化聚合数据
  let mut hierarchy_aggregations = {}
  
  // 按区域聚合（顶层）
  let mut region_aggregation = {}
  for data_point in hierarchical_data {
    let region_data = region_aggregation.get(data_point.region).unwrap_or({
      cpu_values: [],
      count: 0,
      services: {},
      availability_zones: {}
    })
    
    let updated_cpu_values = region_data.cpu_values.push(data_point.cpu)
    let updated_count = region_data.count + 1
    
    // 按服务聚合
    let services = region_data.services
    let service_data = services.get(data_point.service).unwrap_or({
      cpu_values: [],
      count: 0
    })
    let updated_service_cpu = service_data.cpu_values.push(data_point.cpu)
    let updated_service_count = service_data.count + 1
    let updated_services = services.set(data_point.service, {
      cpu_values: updated_service_cpu,
      count: updated_service_count,
      avg_cpu: updated_service_cpu.reduce(fn(acc, v) { acc + v }, 0.0) / updated_service_count.to_float()
    })
    
    // 按可用区聚合
    let azs = region_data.availability_zones
    let az_count = azs.get(data_point.az).unwrap_or(0) + 1
    let updated_azs = azs.set(data_point.az, az_count)
    
    region_aggregation = region_aggregation.set(data_point.region, {
      cpu_values: updated_cpu_values,
      count: updated_count,
      avg_cpu: updated_cpu_values.reduce(fn(acc, v) { acc + v }, 0.0) / updated_count.to_float(),
      services: updated_services,
      availability_zones: updated_azs
    })
  }
  
  hierarchy_aggregations = hierarchy_aggregations.set("region", region_aggregation)
  
  // 验证顶层聚合（区域级别）
  let region_level = hierarchy_aggregations.get("region").unwrap()
  assert_eq(region_level.size(), 3) // 应该有3个区域
  
  // 验证us-east区域聚合
  let us_east = region_level.get("us-east").unwrap()
  assert_eq(us_east.count, 5)
  assert_eq(us_east.avg_cpu, 48.4) // (30+32+35+70+75)/5
  assert_eq(us_east.services.size(), 2) // auth和db
  assert_eq(us_east.availability_zones.size(), 2) // us-east-1a和us-east-1b
  
  // 验证服务级别聚合
  let us_east_auth = us_east.services.get("auth").unwrap()
  assert_eq(us_east_auth.count, 3)
  assert_eq(us_east_auth.avg_cpu, 32.33) // (30+32+35)/3
  
  // 下钻分析：找出高CPU使用的区域
  let high_cpu_regions = []
  for (region, data) in region_level {
    if data.avg_cpu > 40.0 {
      high_cpu_regions = high_cpu_regions.push({
        region: region,
        avg_cpu: data.avg_cpu,
        count: data.count
      })
    }
  }
  
  // 验证高CPU区域识别
  assert_eq(high_cpu_regions.length(), 2) // us-east和us-west
  assert_eq(high_cpu_regions[0].region, "us-east")
  assert_eq(high_cpu_regions[1].region, "us-west")
  
  // 进一步下钻：在高CPU区域中找出高CPU服务
  let mut high_cpu_services = []
  for region_data in high_cpu_regions {
    let region_details = region_level.get(region_data.region).unwrap()
    for (service, service_data) in region_details.services {
      if service_data.avg_cpu > 40.0 {
        high_cpu_services = high_cpu_services.push({
          region: region_data.region,
          service: service,
          avg_cpu: service_data.avg_cpu,
          count: service_data.count
        })
      }
    }
  }
  
  // 验证高CPU服务识别
  assert_eq(high_cpu_services.length(), 3) // us-east的db，us-west的db
  assert_eq(high_cpu_services[0].service, "db")
  assert_eq(high_cpu_services[1].service, "db")
  
  // 最终下钻：实例级别分析
  let mut instance_analysis = []
  for data_point in hierarchical_data {
    if high_cpu_services.any(fn(s) { s.region == data_point.region && s.service == data_point.service }) {
      instance_analysis = instance_analysis.push({
        region: data_point.region,
        service: data_point.service,
        instance: data_point.instance,
        cpu: data_point.cpu
      })
    }
  }
  
  // 验证实例级别分析
  assert_eq(instance_analysis.length(), 4) // 应该有4个高CPU实例
  
  // 找出CPU最高的实例
  let highest_cpu_instance = instance_analysis.sort_by(fn(a, b) {
    if a.cpu > b.cpu { -1 }
    else if a.cpu < b.cpu { 1 }
    else { 0 }
  })[0]
  
  assert_eq(highest_cpu_instance.instance, "db-2")
  assert_eq(highest_cpu_instance.cpu, 75.0)
  assert_eq(highest_cpu_instance.region, "us-east")
  assert_eq(highest_cpu_instance.service, "db")
}