// Azimuth 异常检测增强测试套件
// 专注于遥测数据中的异常检测和识别功能

// 测试1: 基于统计的异常检测
test "基于统计的异常检测" {
  // 模拟正常范围内的遥测数据
  let normal_metrics = [
    { name: "cpu_usage", value: 45.0, timestamp: 1640995200 },
    { name: "cpu_usage", value: 48.0, timestamp: 1640995260 },
    { name: "cpu_usage", value: 52.0, timestamp: 1640995320 },
    { name: "cpu_usage", value: 47.0, timestamp: 1640995380 },
    { name: "cpu_usage", value: 50.0, timestamp: 1640995440 }
  ]
  
  // 添加异常数据点
  let metrics_with_anomalies = normal_metrics + [
    { name: "cpu_usage", value: 95.0, timestamp: 1640995500 },  // 异常高值
    { name: "cpu_usage", value: 5.0, timestamp: 1640995560 }    // 异常低值
  ]
  
  // 计算基础统计信息
  let calculate_stats = fn(data: Array[Dynamic]) {
    let mut sum = 0.0
    let mut count = 0
    let mut min_val = data[0].value
    let mut max_val = data[0].value
    
    for item in data {
      sum = sum + item.value
      count = count + 1
      if item.value < min_val {
        min_val = item.value
      }
      if item.value > max_val {
        max_val = item.value
      }
    }
    
    let mean = sum / count.to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for item in data {
      let diff = item.value - mean
      variance_sum = variance_sum + (diff * diff)
    }
    let variance = variance_sum / count.to_float()
    let std_dev = variance.sqrt()
    
    { mean, std_dev, min_val, max_val, count }
  }
  
  // 使用3-sigma规则检测异常
  let detect_anomalies_3sigma = fn(data: Array[Dynamic]) {
    let stats = calculate_stats(data)
    let threshold = 3.0 * stats.std_dev
    
    let mut anomalies = []
    let mut normal_points = []
    
    for item in data {
      let z_score = (item.value - stats.mean).abs() / stats.std_dev
      if z_score > 3.0 {
        anomalies = anomalies.push({
          data: item,
          z_score: z_score,
          anomaly_type: if item.value > stats.mean { "high" } else { "low" }
        })
      } else {
        normal_points = normal_points.push(item)
      }
    }
    
    { anomalies, normal_points, stats }
  }
  
  // 执行异常检测
  let result = detect_anomalies_3sigma(metrics_with_anomalies)
  
  // 验证异常检测结果
  assert_eq(result.normal_points.length(), 5)  // 正常数据点
  assert_eq(result.anomalies.length(), 2)     // 异常数据点
  
  // 验证高值异常
  let high_anomaly = result.anomalies[0]
  assert_eq(high_anomaly.data.value, 95.0)
  assert_eq(high_anomaly.anomaly_type, "high")
  assert_true(high_anomaly.z_score > 3.0)
  
  // 验证低值异常
  let low_anomaly = result.anomalies[1]
  assert_eq(low_anomaly.data.value, 5.0)
  assert_eq(low_anomaly.anomaly_type, "low")
  assert_true(low_anomaly.z_score > 3.0)
  
  // 验证统计信息
  assert_true(result.stats.mean > 40.0)
  assert_true(result.stats.mean < 60.0)
  assert_true(result.stats.std_dev > 0.0)
}

// 测试2: 基于时间序列的异常检测
test "基于时间序列的异常检测" {
  // 模拟时间序列数据，包含周期性模式
  let time_series_data = []
  let mut i = 0
  while i < 24 {  // 24小时数据
    let base_value = 50.0
    let hourly_variation = 10.0 * ((i % 12).to_float() / 12.0)  // 周期性变化
    let noise = (i % 5).to_float()  // 小幅随机变化
    
    time_series_data = time_series_data.push({
      hour: i,
      value: base_value + hourly_variation + noise,
      timestamp: 1640995200 + i * 3600
    })
    i = i + 1
  }
  
  // 添加异常点
  let data_with_anomalies = time_series_data.slice(0, 12) + [
    { hour: 12, value: 120.0, timestamp: 1640995200 + 12 * 3600 }  // 异常高值
  ] + time_series_data.slice(13, 24)
  
  // 移动平均异常检测
  let moving_average_anomaly_detection = fn(data: Array[Dynamic], window_size: Int, threshold: Float) {
    let mut anomalies = []
    
    let mut i = window_size
    while i < data.length() {
      // 计算移动平均
      let mut sum = 0.0
      let mut j = i - window_size
      while j < i {
        sum = sum + data[j].value
        j = j + 1
      }
      let moving_avg = sum / window_size.to_float()
      
      // 检测偏差
      let current_value = data[i].value
      let deviation = (current_value - moving_avg).abs() / moving_avg
      
      if deviation > threshold {
        anomalies = anomalies.push({
          index: i,
          hour: data[i].hour,
          value: current_value,
          expected: moving_avg,
          deviation_percent: deviation * 100.0
        })
      }
      
      i = i + 1
    }
    
    anomalies
  }
  
  // 执行移动平均异常检测
  let anomalies = moving_average_anomaly_detection(data_with_anomalies, 6, 0.5)  // 6小时窗口，50%偏差阈值
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)  // 应该检测到一个异常点
  
  // 验证异常点详情
  let anomaly = anomalies[0]
  assert_eq(anomaly.hour, 12)  // 第12小时的异常
  assert_eq(anomaly.value, 120.0)
  assert_true(anomaly.expected > 50.0)
  assert_true(anomaly.expected < 70.0)
  assert_true(anomaly.deviation_percent > 50.0)
  
  // 测试季节性分解异常检测
  let seasonal_decomposition = fn(data: Array[Dynamic], period: Int) {
    // 简化的季节性分解：计算季节性模式
    let mut seasonal_pattern = []
    let mut i = 0
    while i < period {
      let mut period_sum = 0.0
      let mut period_count = 0
      
      let mut j = i
      while j < data.length() {
        period_sum = period_sum + data[j].value
        period_count = period_count + 1
        j = j + period
      }
      
      let seasonal_avg = period_sum / period_count.to_float()
      seasonal_pattern = seasonal_pattern.push(seasonal_avg)
      i = i + 1
    }
    
    // 去除季节性成分
    let mut deseasonalized = []
    let mut i = 0
    while i < data.length() {
      let seasonal_index = i % period
      let seasonal_value = seasonal_pattern[seasonal_index]
      let deseasonalized_value = data[i].value - seasonal_value
      
      deseasonalized = deseasonalized.push({
        hour: data[i].hour,
        original_value: data[i].value,
        seasonal_value: seasonal_value,
        deseasonalized_value: deseasonalized_value
      })
      i = i + 1
    }
    
    { seasonal_pattern, deseasonalized }
  }
  
  // 执行季节性分解
  let decomposition = seasonal_decomposition(data_with_anomalies, 12)  // 12小时周期
  
  // 验证季节性分解结果
  assert_eq(decomposition.seasonal_pattern.length(), 12)
  assert_eq(decomposition.deseasonalized.length(), 24)
  
  // 验证异常点在去季节化数据中仍然明显
  let anomaly_hour = 12
  let mut found_anomaly = false
  for item in decomposition.deseasonalized {
    if item.hour == anomaly_hour {
      assert_true(item.deseasonalized_value > 50.0)  // 去季节化后仍然异常
      found_anomaly = true
      break
    }
  }
  assert_true(found_anomaly)
}

// 测试3: 多维度异常检测
test "多维度异常检测" {
  // 模拟多维度指标数据
  let multi_dimensional_data = [
    { service: "api", endpoint: "/users", method: "GET", response_time: 120.0, error_rate: 0.01, throughput: 1000.0 },
    { service: "api", endpoint: "/users", method: "GET", response_time: 125.0, error_rate: 0.02, throughput: 980.0 },
    { service: "api", endpoint: "/users", method: "GET", response_time: 118.0, error_rate: 0.01, throughput: 1020.0 },
    { service: "api", endpoint: "/orders", method: "POST", response_time: 350.0, error_rate: 0.05, throughput: 200.0 },
    { service: "api", endpoint: "/orders", method: "POST", response_time: 360.0, error_rate: 0.04, throughput: 210.0 },
    { service: "api", endpoint: "/orders", method: "POST", response_time: 340.0, error_rate: 0.06, throughput: 190.0 },
    // 异常数据点
    { service: "api", endpoint: "/users", method: "GET", response_time: 500.0, error_rate: 0.15, throughput: 300.0 }
  ]
  
  // 多维度异常检测算法
  let multivariate_anomaly_detection = fn(data: Array[Dynamic]) {
    // 计算各维度的均值和标准差
    let dimensions = ["response_time", "error_rate", "throughput"]
    let mut dimension_stats = []
    
    for dim in dimensions {
      let mut values = []
      for item in data {
        match dim {
          "response_time" => values = values.push(item.response_time)
          "error_rate" => values = values.push(item.error_rate)
          "throughput" => values = values.push(item.throughput)
          _ => ()
        }
      }
      
      // 计算统计量
      let mut sum = 0.0
      for v in values {
        sum = sum + v
      }
      let mean = sum / values.length().to_float()
      
      let mut variance_sum = 0.0
      for v in values {
        let diff = v - mean
        variance_sum = variance_sum + (diff * diff)
      }
      let std_dev = (variance_sum / values.length().to_float()).sqrt()
      
      dimension_stats = dimension_stats.push({ dimension: dim, mean, std_dev })
    }
    
    // 计算每个数据点的马氏距离（简化版）
    let mut anomalies = []
    for item in data {
      let mut normalized_distance = 0.0
      
      for stats in dimension_stats {
        let value = match stats.dimension {
          "response_time" => item.response_time
          "error_rate" => item.error_rate
          "throughput" => item.throughput
          _ => 0.0
        }
        
        let z_score = (value - stats.mean) / stats.std_dev
        normalized_distance = normalized_distance + (z_score * z_score)
      }
      
      let mahalanobis_distance = normalized_distance.sqrt()
      
      // 如果距离超过阈值，标记为异常
      if mahalanobis_distance > 3.0 {
        anomalies = anomalies.push({
          data: item,
          distance: mahalanobis_distance,
          anomaly_score: mahalanobis_distance
        })
      }
    }
    
    { anomalies, dimension_stats }
  }
  
  // 执行多维度异常检测
  let result = multivariate_anomaly_detection(multi_dimensional_data)
  
  // 验证异常检测结果
  assert_eq(result.anomalies.length(), 1)  // 应该检测到一个异常点
  
  // 验证异常点详情
  let anomaly = result.anomalies[0]
  assert_eq(anomaly.data.service, "api")
  assert_eq(anomaly.data.endpoint, "/users")
  assert_eq(anomaly.data.method, "GET")
  assert_eq(anomaly.data.response_time, 500.0)
  assert_eq(anomaly.data.error_rate, 0.15)
  assert_eq(anomaly.data.throughput, 300.0)
  assert_true(anomaly.distance > 3.0)
  
  // 验证维度统计信息
  assert_eq(result.dimension_stats.length(), 3)
  
  // 验证响应时间统计
  let response_time_stats = result.dimension_stats[0]
  assert_eq(response_time_stats.dimension, "response_time")
  assert_true(response_time_stats.mean > 200.0)
  assert_true(response_time_stats.mean < 300.0)
  assert_true(response_time_stats.std_dev > 0.0)
  
  // 验证错误率统计
  let error_rate_stats = result.dimension_stats[1]
  assert_eq(error_rate_stats.dimension, "error_rate")
  assert_true(error_rate_stats.mean > 0.01)
  assert_true(error_rate_stats.mean < 0.1)
  assert_true(error_rate_stats.std_dev > 0.0)
  
  // 验证吞吐量统计
  let throughput_stats = result.dimension_stats[2]
  assert_eq(throughput_stats.dimension, "throughput")
  assert_true(throughput_stats.mean > 400.0)
  assert_true(throughput_stats.mean < 800.0)
  assert_true(throughput_stats.std_dev > 0.0)
}

// 测试4: 上下文感知异常检测
test "上下文感知异常检测" {
  // 模拟带有上下文信息的遥测数据
  let contextual_data = [
    { service: "payment", environment: "production", region: "us-east", time_of_day: "peak", response_time: 200.0 },
    { service: "payment", environment: "production", region: "us-east", time_of_day: "peak", response_time: 210.0 },
    { service: "payment", environment: "production", region: "us-east", time_of_day: "peak", response_time: 195.0 },
    { service: "payment", environment: "staging", region: "us-west", time_of_day: "off-peak", response_time: 150.0 },
    { service: "payment", environment: "staging", region: "us-west", time_of_day: "off-peak", response_time: 160.0 },
    { service: "payment", environment: "staging", region: "us-west", time_of_day: "off-peak", response_time: 145.0 },
    // 上下文异常：在非高峰时段出现高峰时段的响应时间
    { service: "payment", environment: "production", region: "us-east", time_of_day: "off-peak", response_time: 205.0 }
  ]
  
  // 上下文感知异常检测
  let contextual_anomaly_detection = fn(data: Array[Dynamic]) {
    // 按上下文分组
    let mut context_groups = []
    let mut processed_contexts = []
    
    for item in data {
      let context_key = item.environment + ":" + item.region + ":" + item.time_of_day
      
      let already_processed = false
      for ctx in processed_contexts {
        if ctx == context_key {
          already_processed = true
          break
        }
      }
      
      if not already_processed {
        processed_contexts = processed_contexts.push(context_key)
        
        // 收集同一上下文的所有数据
        let mut context_data = []
        for d in data {
          let current_context = d.environment + ":" + d.region + ":" + d.time_of_day
          if current_context == context_key {
            context_data = context_data.push(d)
          }
        }
        
        // 计算上下文统计
        let mut sum = 0.0
        let mut count = 0
        let mut min_val = context_data[0].response_time
        let mut max_val = context_data[0].response_time
        
        for d in context_data {
          sum = sum + d.response_time
          count = count + 1
          if d.response_time < min_val {
            min_val = d.response_time
          }
          if d.response_time > max_val {
            max_val = d.response_time
          }
        }
        
        let mean = sum / count.to_float()
        
        // 计算标准差
        let mut variance_sum = 0.0
        for d in context_data {
          let diff = d.response_time - mean
          variance_sum = variance_sum + (diff * diff)
        }
        let std_dev = (variance_sum / count.to_float()).sqrt()
        
        context_groups = context_groups.push({
          context: context_key,
          data: context_data,
          stats: { mean, std_dev, min_val, max_val, count }
        })
      }
    }
    
    // 检测上下文异常
    let mut contextual_anomalies = []
    
    for group in context_groups {
      for item in group.data {
        let z_score = (item.response_time - group.stats.mean) / group.stats.std_dev
        
        if z_score.abs() > 2.0 {  // 使用2-sigma规则
          contextual_anomalies = contextual_anomalies.push({
            data: item,
            context: group.context,
            z_score: z_score,
            context_mean: group.stats.mean,
            context_std_dev: group.stats.std_dev
          })
        }
      }
    }
    
    { context_groups, contextual_anomalies }
  }
  
  // 执行上下文感知异常检测
  let result = contextual_anomaly_detection(contextual_data)
  
  // 验证上下文分组结果
  assert_eq(result.context_groups.length(), 2)  // 两个不同的上下文
  
  // 验证生产环境高峰时段上下文
  let production_peak_context = result.context_groups[0]
  assert_eq(production_peak_context.context, "production:us-east:peak")
  assert_eq(production_peak_context.stats.count, 3)
  assert_true(production_peak_context.stats.mean > 190.0)
  assert_true(production_peak_context.stats.mean < 210.0)
  
  // 验证预发布环境非高峰时段上下文
  let staging_off_peak_context = result.context_groups[1]
  assert_eq(staging_off_peak_context.context, "staging:us-west:off-peak")
  assert_eq(staging_off_peak_context.stats.count, 3)
  assert_true(staging_off_peak_context.stats.mean > 140.0)
  assert_true(staging_off_peak_context.stats.mean < 160.0)
  
  // 验证上下文异常检测结果
  assert_eq(result.contextual_anomalies.length(), 1)  // 应该检测到一个上下文异常
  
  // 验证上下文异常详情
  let anomaly = result.contextual_anomalies[0]
  assert_eq(anomaly.data.service, "payment")
  assert_eq(anomaly.data.environment, "production")
  assert_eq(anomaly.data.region, "us-east")
  assert_eq(anomaly.data.time_of_day, "off-peak")
  assert_eq(anomaly.data.response_time, 205.0)
  assert_eq(anomaly.context, "production:us-east:off-peak")
  assert_true(anomaly.z_score.abs() > 2.0)
  
  // 上下文解释：在非高峰时段，响应时间应该接近高峰时段的基线
  // 但实际上这个值更像是高峰时段的响应时间，因此是上下文异常
}