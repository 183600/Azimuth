// Azimuth Time Series Data Aggregation Tests
// 时间序列数据聚合测试用例，专注于各种时间窗口和聚合函数

test "时间窗口聚合函数测试" {
  // 创建24小时的时间序列数据（每分钟一个数据点）
  let time_series_data = []
  let base_timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  for i = 0; i < 1440; i = i + 1 { // 24小时 * 60分钟
    let hour = i / 60
    let minute = i % 60
    
    // 模拟CPU使用率，白天高，夜晚低
    let base_cpu = if hour >= 8 && hour <= 20 { 60.0 } else { 20.0 }
    let cpu_variation = (minute % 20).to_double() * 2.0
    let cpu_usage = base_cpu + cpu_variation
    
    // 模拟内存使用率，逐渐增长
    let memory_usage = 40.0 + (i.to_double() / 1440.0) * 20.0 + (minute % 15).to_double()
    
    // 模拟请求率，高峰期高
    let base_requests = if hour >= 9 && hour <= 17 && hour != 12 { 500.0 } else { 100.0 }
    let request_variation = (minute % 10).to_double() * 50.0
    let request_rate = base_requests + request_variation
    
    let data_point = {
      "timestamp": base_timestamp + i.to_long() * 60000L, // 每分钟
      "cpu_usage": cpu_usage,
      "memory_usage": memory_usage,
      "request_rate": request_rate,
      "error_rate": if minute % 30 == 0 { 5.0 } else { 0.5 },
      "response_time": 100.0 + (minute % 25).to_double() * 10.0,
      "host": "server_" + (i % 5).to_string(),
      "region": "us-east-1"
    }
    time_series_data.push(data_point)
  }
  
  // 1. 按小时聚合
  let hourly_aggregates = {}
  
  for point in time_series_data {
    let timestamp = point["timestamp"]
    let hour_offset = ((timestamp - base_timestamp) / 3600000L).to_int() // 小时偏移
    let hour_key = "hour_" + hour_offset.to_string()
    
    if hourly_aggregates[hour_key] is None {
      hourly_aggregates[hour_key] = {
        "cpu_values": [],
        "memory_values": [],
        "request_values": [],
        "error_values": [],
        "response_values": [],
        "count": 0
      }
    }
    
    let aggregate = hourly_aggregates[hour_key]
    aggregate["cpu_values"].push(point["cpu_usage"])
    aggregate["memory_values"].push(point["memory_usage"])
    aggregate["request_values"].push(point["request_rate"])
    aggregate["error_values"].push(point["error_rate"])
    aggregate["response_values"].push(point["response_time"])
    aggregate["count"] = aggregate["count"] + 1
  }
  
  // 计算每小时的平均值
  let hourly_averages = {}
  for (hour_key, aggregate) in hourly_aggregates {
    let cpu_avg = aggregate["cpu_values"].reduce(|acc, val| acc + val, 0.0) / aggregate["count"].to_double()
    let memory_avg = aggregate["memory_values"].reduce(|acc, val| acc + val, 0.0) / aggregate["count"].to_double()
    let request_avg = aggregate["request_values"].reduce(|acc, val| acc + val, 0.0) / aggregate["count"].to_double()
    let error_avg = aggregate["error_values"].reduce(|acc, val| acc + val, 0.0) / aggregate["count"].to_double()
    let response_avg = aggregate["response_values"].reduce(|acc, val| acc + val, 0.0) / aggregate["count"].to_double()
    
    hourly_averages[hour_key] = {
      "cpu_avg": cpu_avg,
      "memory_avg": memory_avg,
      "request_avg": request_avg,
      "error_avg": error_avg,
      "response_avg": response_avg,
      "count": aggregate["count"]
    }
  }
  
  // 验证每小时聚合结果
  assert_eq(hourly_averages.length(), 24) // 24小时
  
  // 验证工作时间（8-20点）的CPU使用率更高
  let work_hours_avg_cpu = 0.0
  let work_hours_count = 0
  let non_work_hours_avg_cpu = 0.0
  let non_work_hours_count = 0
  
  for (hour_key, avg) in hourly_averages {
    let hour = hour_key.split("_")[1].to_int()
    if hour >= 8 && hour <= 20 {
      work_hours_avg_cpu = work_hours_avg_cpu + avg["cpu_avg"]
      work_hours_count = work_hours_count + 1
    } else {
      non_work_hours_avg_cpu = non_work_hours_avg_cpu + avg["cpu_avg"]
      non_work_hours_count = non_work_hours_count + 1
    }
  }
  
  work_hours_avg_cpu = work_hours_avg_cpu / work_hours_count.to_double()
  non_work_hours_avg_cpu = non_work_hours_avg_cpu / non_work_hours_count.to_double()
  
  assert_true(work_hours_avg_cpu > non_work_hours_avg_cpu)
  
  // 2. 计算最大值和最小值
  let daily_max_cpu = 0.0
  let daily_min_cpu = 100.0
  let daily_max_memory = 0.0
  let daily_min_memory = 100.0
  
  for point in time_series_data {
    let cpu = point["cpu_usage"]
    let memory = point["memory_usage"]
    
    if cpu > daily_max_cpu {
      daily_max_cpu = cpu
    }
    if cpu < daily_min_cpu {
      daily_min_cpu = cpu
    }
    
    if memory > daily_max_memory {
      daily_max_memory = memory
    }
    if memory < daily_min_memory {
      daily_min_memory = memory
    }
  }
  
  assert_true(daily_max_cpu > daily_min_cpu)
  assert_true(daily_max_memory > daily_min_memory)
  assert_true(daily_max_cpu >= 60.0) // 工作时间应该有较高的CPU使用率
  assert_true(daily_min_cpu <= 40.0) // 非工作时间应该有较低的CPU使用率
  
  assert_true(true)
}

test "多维度时间序列聚合测试" {
  // 创建多维度时间序列数据（多个主机、多个指标）
  let multi_dimensional_data = []
  let base_timestamp = 1640995200000L
  
  let hosts = ["web-server-01", "web-server-02", "db-server-01", "db-server-02", "cache-server-01"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1"]
  let services = ["web-service", "db-service", "cache-service"]
  
  // 生成7天的数据，每小时一个点
  for day = 0; day < 7; day = day + 1 {
    for hour = 0; hour < 24; hour = hour + 1 {
      for host_idx = 0; host_idx < hosts.length(); host_idx = host_idx + 1 {
        let host = hosts[host_idx]
        let region = regions[host_idx % regions.length()]
        let service = services[host_idx % services.length()]
        
        // 根据服务类型生成不同的指标模式
        let (cpu_usage, memory_usage, request_rate, disk_io) = match service {
          "web-service" => (
            50.0 + (hour % 12).to_double() * 5.0, // 白天高
            60.0 + (day % 3).to_double() * 5.0, // 逐渐增长
            100.0 + (hour % 8).to_double() * 50.0, // 波动较大
            20.0 + (hour % 24).to_double() * 2.0 // 相对稳定
          ),
          "db-service" => (
            70.0 + (hour % 6).to_double() * 3.0, // 持续较高
            80.0 + (day % 2).to_double() * 5.0, // 逐渐增长
            200.0 + (hour % 4).to_double() * 30.0, // 高且波动
            50.0 + (hour % 12).to_double() * 10.0 // 中等波动
          ),
          "cache-service" => (
            30.0 + (hour % 24).to_double() * 1.5, // 相对稳定
            40.0 + (day % 4).to_double() * 3.0, // 缓慢增长
            500.0 + (hour % 3).to_double() * 100.0, // 高频访问
            10.0 + (hour % 24).to_double() * 1.0 // 很低且稳定
          ),
          _ => (50.0, 50.0, 100.0, 20.0)
        }
        
        let timestamp = base_timestamp + (day * 24 + hour).to_long() * 3600000L
        let data_point = {
          "timestamp": timestamp,
          "host": host,
          "region": region,
          "service": service,
          "cpu_usage": cpu_usage,
          "memory_usage": memory_usage,
          "request_rate": request_rate,
          "disk_io": disk_io,
          "network_io": 30.0 + (hour % 10).to_double() * 5.0,
          "error_rate": if hour % 8 == 0 { 2.0 } else { 0.1 }
        }
        multi_dimensional_data.push(data_point)
      }
    }
  }
  
  // 按服务聚合
  let service_aggregates = {}
  
  for point in multi_dimensional_data {
    let service = point["service"]
    
    if service_aggregates[service] is None {
      service_aggregates[service] = {
        "cpu_values": [],
        "memory_values": [],
        "request_values": [],
        "disk_io_values": [],
        "network_io_values": [],
        "error_values": [],
        "count": 0
      }
    }
    
    let aggregate = service_aggregates[service]
    aggregate["cpu_values"].push(point["cpu_usage"])
    aggregate["memory_values"].push(point["memory_usage"])
    aggregate["request_values"].push(point["request_rate"])
    aggregate["disk_io_values"].push(point["disk_io"])
    aggregate["network_io_values"].push(point["network_io"])
    aggregate["error_values"].push(point["error_rate"])
    aggregate["count"] = aggregate["count"] + 1
  }
  
  // 计算每个服务的平均指标
  let service_averages = {}
  for (service, aggregate) in service_aggregates {
    let count = aggregate["count"].to_double()
    let cpu_avg = aggregate["cpu_values"].reduce(|acc, val| acc + val, 0.0) / count
    let memory_avg = aggregate["memory_values"].reduce(|acc, val| acc + val, 0.0) / count
    let request_avg = aggregate["request_values"].reduce(|acc, val| acc + val, 0.0) / count
    let disk_io_avg = aggregate["disk_io_values"].reduce(|acc, val| acc + val, 0.0) / count
    let network_io_avg = aggregate["network_io_values"].reduce(|acc, val| acc + val, 0.0) / count
    let error_avg = aggregate["error_values"].reduce(|acc, val| acc + val, 0.0) / count
    
    service_averages[service] = {
      "cpu_avg": cpu_avg,
      "memory_avg": memory_avg,
      "request_avg": request_avg,
      "disk_io_avg": disk_io_avg,
      "network_io_avg": network_io_avg,
      "error_avg": error_avg
    }
  }
  
  // 验证服务间差异
  let web_service = service_averages["web-service"]
  let db_service = service_averages["db-service"]
  let cache_service = service_averages["cache-service"]
  
  // DB服务应该有更高的CPU和内存使用率
  assert_true(db_service["cpu_avg"] > web_service["cpu_avg"])
  assert_true(db_service["memory_avg"] > web_service["memory_avg"])
  
  // 缓存服务应该有更高的请求率
  assert_true(cache_service["request_avg"] > web_service["request_avg"])
  assert_true(cache_service["request_avg"] > db_service["request_avg"])
  
  // 按区域聚合
  let region_aggregates = {}
  
  for point in multi_dimensional_data {
    let region = point["region"]
    
    if region_aggregates[region] is None {
      region_aggregates[region] = {
        "cpu_values": [],
        "memory_values": [],
        "request_values": [],
        "count": 0
      }
    }
    
    let aggregate = region_aggregates[region]
    aggregate["cpu_values"].push(point["cpu_usage"])
    aggregate["memory_values"].push(point["memory_usage"])
    aggregate["request_values"].push(point["request_rate"])
    aggregate["count"] = aggregate["count"] + 1
  }
  
  // 计算每个区域的平均指标
  let region_averages = {}
  for (region, aggregate) in region_aggregates {
    let count = aggregate["count"].to_double()
    let cpu_avg = aggregate["cpu_values"].reduce(|acc, val| acc + val, 0.0) / count
    let memory_avg = aggregate["memory_values"].reduce(|acc, val| acc + val, 0.0) / count
    let request_avg = aggregate["request_values"].reduce(|acc, val| acc + val, 0.0) / count
    
    region_averages[region] = {
      "cpu_avg": cpu_avg,
      "memory_avg": memory_avg,
      "request_avg": request_avg
    }
  }
  
  // 验证区域数据完整性
  assert_eq(region_averages.length(), 3)
  for (region, avg) in region_averages {
    assert_true(avg["cpu_avg"] > 0.0)
    assert_true(avg["memory_avg"] > 0.0)
    assert_true(avg["request_avg"] > 0.0)
  }
  
  assert_true(true)
}

test "时间序列降采样和上采样测试" {
  // 创建高频时间序列数据（每秒一个点，持续1小时）
  let high_frequency_data = []
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 3600; i = i + 1 { // 1小时 = 3600秒
    // 模拟一个有趋势和季节性的指标
    let trend = i.to_double() * 0.01 // 线性增长趋势
    let seasonal = 10.0 * (i.to_double() / 300.0 * 3.14159).sin() // 5分钟周期的季节性
    let noise = (i % 7).to_double() * 2.0 // 随机噪声
    
    let value = 50.0 + trend + seasonal + noise
    
    let data_point = {
      "timestamp": base_timestamp + i.to_long() * 1000L, // 每秒
      "value": value,
      "metric_name": "cpu_usage",
      "host": "server-01"
    }
    high_frequency_data.push(data_point)
  }
  
  // 1. 降采样：从每秒到每分钟
  let minute_downsampled = {}
  
  for point in high_frequency_data {
    let timestamp = point["timestamp"]
    let minute_bucket = ((timestamp - base_timestamp) / 60000L).to_int() // 分钟桶
    let minute_key = "minute_" + minute_bucket.to_string()
    
    if minute_downsampled[minute_key] is None {
      minute_downsampled[minute_key] = {
        "values": [],
        "timestamps": [],
        "count": 0
      }
    }
    
    let bucket = minute_downsampled[minute_key]
    bucket["values"].push(point["value"])
    bucket["timestamps"].push(timestamp)
    bucket["count"] = bucket["count"] + 1
  }
  
  // 计算每分钟的平均值
  let minute_averages = []
  for i = 0; i < 60; i = i + 1 { // 60分钟
    let minute_key = "minute_" + i.to_string()
    match minute_downsampled[minute_key] {
      Some(bucket) => {
        let avg_value = bucket["values"].reduce(|acc, val| acc + val, 0.0) / bucket["count"].to_double()
        let timestamp = base_timestamp + i.to_long() * 60000L
        minute_averages.push({
          "timestamp": timestamp,
          "value": avg_value
        })
      }
      None => () // 忽略空桶
    }
  }
  
  // 验证降采样结果
  assert_eq(minute_averages.length(), 60)
  
  // 验证每分钟的值在合理范围内
  for avg_point in minute_averages {
    assert_true(avg_point["value"] >= 30.0) // 最小值应该大于30
    assert_true(avg_point["value"] <= 80.0) // 最大值应该小于80
  }
  
  // 2. 进一步降采样：从每分钟到每10分钟
  let ten_minute_downsampled = {}
  
  for point in minute_averages {
    let timestamp = point["timestamp"]
    let ten_minute_bucket = ((timestamp - base_timestamp) / 600000L).to_int() // 10分钟桶
    let ten_minute_key = "ten_minute_" + ten_minute_bucket.to_string()
    
    if ten_minute_downsampled[ten_minute_key] is None {
      ten_minute_downsampled[ten_minute_key] = {
        "values": [],
        "timestamps": [],
        "count": 0
      }
    }
    
    let bucket = ten_minute_downsampled[ten_minute_key]
    bucket["values"].push(point["value"])
    bucket["timestamps"].push(timestamp)
    bucket["count"] = bucket["count"] + 1
  }
  
  // 计算每10分钟的平均值
  let ten_minute_averages = []
  for i = 0; i < 6; i = i + 1 { // 6个10分钟段
    let ten_minute_key = "ten_minute_" + i.to_string()
    match ten_minute_downsampled[ten_minute_key] {
      Some(bucket) => {
        let avg_value = bucket["values"].reduce(|acc, val| acc + val, 0.0) / bucket["count"].to_double()
        let timestamp = base_timestamp + i.to_long() * 600000L
        ten_minute_averages.push({
          "timestamp": timestamp,
          "value": avg_value
        })
      }
      None => () // 忽略空桶
    }
  }
  
  // 验证10分钟降采样结果
  assert_eq(ten_minute_averages.length(), 6)
  
  // 3. 上采样：从每10分钟到每分钟（使用线性插值）
  let minute_upsampled = []
  
  for i = 0; i < ten_minute_averages.length() - 1; i = i + 1 {
    let current_point = ten_minute_averages[i]
    let next_point = ten_minute_averages[i + 1]
    
    let current_timestamp = current_point["timestamp"]
    let next_timestamp = next_point["timestamp"]
    let current_value = current_point["value"]
    let next_value = next_point["value"]
    
    // 在两个10分钟点之间进行线性插值，生成每分钟的数据
    for j = 0; j < 10; j = j + 1 {
      let interpolated_timestamp = current_timestamp + j.to_long() * 60000L
      let ratio = j.to_double() / 10.0
      let interpolated_value = current_value + (next_value - current_value) * ratio
      
      minute_upsampled.push({
        "timestamp": interpolated_timestamp,
        "value": interpolated_value
      })
    }
  }
  
  // 添加最后一个10分钟段
  let last_point = ten_minute_averages[ten_minute_averages.length() - 1]
  for j = 0; j < 10; j = j + 1 {
    let interpolated_timestamp = last_point["timestamp"] + j.to_long() * 60000L
    minute_upsampled.push({
      "timestamp": interpolated_timestamp,
      "value": last_point["value"]
    })
  }
  
  // 验证上采样结果
  assert_eq(minute_upsampled.length(), 60)
  
  // 验证上采样数据的平滑性（相邻点之间的差异应该较小）
  for i = 1; i < minute_upsampled.length(); i = i + 1 {
    let current_value = minute_upsampled[i]["value"]
    let prev_value = minute_upsampled[i-1]["value"]
    let diff = (current_value - prev_value).abs()
    
    // 由于是线性插值，相邻点之间的差异应该相对较小
    assert_true(diff < 5.0)
  }
  
  assert_true(true)
}

test "时间序列异常检测和标记测试" {
  // 创建包含异常的时间序列数据
  let time_series_with_anomalies = []
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 1440; i = i + 1 { // 24小时，每分钟一个点
    let hour = i / 60
    let minute = i % 60
    
    // 基础模式：白天高，夜晚低
    let base_value = if hour >= 8 && hour <= 20 { 60.0 } else { 20.0 }
    let normal_variation = (minute % 10).to_double() * 2.0
    let value = base_value + normal_variation
    
    // 插入异常值
    let is_anomaly = false
    let anomaly_type = ""
    
    if i == 360 { // 6:00 AM - 突然峰值
      value = 95.0
      is_anomaly = true
      anomaly_type = "spike"
    } else if i == 720 { // 12:00 PM - 突然下降
      value = 5.0
      is_anomaly = true
      anomaly_type = "drop"
    } else if i >= 1080 && i < 1090 { // 6:00-6:10 PM - 持续异常
      value = 90.0
      is_anomaly = true
      anomaly_type = "sustained"
    } else if i % 100 == 0 && i > 0 { // 随机异常
      value = if value > 50.0 { value - 40.0 } else { value + 40.0 }
      is_anomaly = true
      anomaly_type = "random"
    }
    
    let data_point = {
      "timestamp": base_timestamp + i.to_long() * 60000L,
      "value": value,
      "is_anomaly": is_anomaly,
      "anomaly_type": anomaly_type,
      "metric_name": "cpu_usage",
      "host": "server-01"
    }
    time_series_with_anomalies.push(data_point)
  }
  
  // 1. 使用移动平均检测异常
  let window_size = 30 // 30分钟窗口
  let detected_anomalies = []
  
  for i = window_size; i < time_series_with_anomalies.length() - window_size; i = i + 1 {
    let current_value = time_series_with_anomalies[i]["value"]
    
    // 计算前后窗口的平均值和标准差
    let window_values = []
    for j = i - window_size; j < i + window_size + 1; j = j + 1 {
      if j != i { // 排除当前点
        window_values.push(time_series_with_anomalies[j]["value"])
      }
    }
    
    let window_avg = window_values.reduce(|acc, val| acc + val, 0.0) / window_values.length().to_double()
    
    // 计算标准差
    let variance = window_values.reduce(|acc, val| acc + (val - window_avg) * (val - window_avg), 0.0) / window_values.length().to_double()
    let std_dev = variance.sqrt()
    
    // 如果当前值偏离平均值超过3个标准差，则认为是异常
    let threshold = 3.0 * std_dev
    if (current_value - window_avg).abs() > threshold {
      detected_anomalies.push({
        "index": i,
        "timestamp": time_series_with_anomalies[i]["timestamp"],
        "value": current_value,
        "window_avg": window_avg,
        "std_dev": std_dev,
        "deviation": (current_value - window_avg).abs()
      })
    }
  }
  
  // 验证检测到的异常数量
  assert_true(detected_anomalies.length() >= 4) // 至少应该检测到4个主要异常
  
  // 验证检测到的异常包含已知的异常点
  let detected_indices = detected_anomalies.map(|anomaly| anomaly["index"])
  assert_true(detected_indices.contains(360)) // 6:00 AM 峰值
  assert_true(detected_indices.contains(720)) // 12:00 PM 下降
  
  // 2. 使用Z-score检测异常
  let z_score_anomalies = []
  
  // 计算全局平均值和标准差
  let all_values = time_series_with_anomalies.map(|point| point["value"])
  let global_avg = all_values.reduce(|acc, val| acc + val, 0.0) / all_values.length().to_double()
  let global_variance = all_values.reduce(|acc, val| acc + (val - global_avg) * (val - global_avg), 0.0) / all_values.length().to_double()
  let global_std_dev = global_variance.sqrt()
  
  for i = 0; i < time_series_with_anomalies.length(); i = i + 1 {
    let value = time_series_with_anomalies[i]["value"]
    let z_score = (value - global_avg) / global_std_dev
    
    // Z-score绝对值大于3认为是异常
    if z_score.abs() > 3.0 {
      z_score_anomalies.push({
        "index": i,
        "timestamp": time_series_with_anomalies[i]["timestamp"],
        "value": value,
        "z_score": z_score
      })
    }
  }
  
  // 验证Z-score方法检测到的异常
  assert_true(z_score_anomalies.length() >= 3)
  
  // 3. 标记和分类异常
  let anomaly_classifications = {}
  
  for anomaly in detected_anomalies {
    let index = anomaly["index"]
    let value = anomaly["value"]
    let window_avg = anomaly["window_avg"]
    
    let classification = if value > window_avg * 1.5 {
      "spike"
    } else if value < window_avg * 0.5 {
      "drop"
    } else {
      "moderate"
    }
    
    if anomaly_classifications[classification] is None {
      anomaly_classifications[classification] = []
    }
    anomaly_classifications[classification].push(anomaly)
  }
  
  // 验证异常分类
  assert_true(anomaly_classifications.contains("spike"))
  assert_true(anomaly_classifications.contains("drop"))
  
  // 4. 验证检测到的异常与实际异常的一致性
  let true_positives = 0
  let false_positives = 0
  let false_negatives = 0
  
  let detected_set = detected_anomalies.map(|anomaly| anomaly["index"])
  let actual_anomaly_indices = []
  
  for i = 0; i < time_series_with_anomalies.length(); i = i + 1 {
    if time_series_with_anomalies[i]["is_anomaly"] {
      actual_anomaly_indices.push(i)
    }
  }
  
  // 计算真正例、假正例和假负例
  for idx in detected_set {
    if actual_anomaly_indices.contains(idx) {
      true_positives = true_positives + 1
    } else {
      false_positives = false_positives + 1
    }
  }
  
  for idx in actual_anomaly_indices {
    if !detected_set.contains(idx) {
      false_negatives = false_negatives + 1
    }
  }
  
  // 计算精确率和召回率
  let precision = true_positives.to_double() / (true_positives + false_positives).to_double()
  let recall = true_positives.to_double() / (true_positives + false_negatives).to_double()
  
  // 验证检测质量
  assert_true(precision > 0.5) // 至少50%的精确率
  assert_true(recall > 0.3) // 至少30%的召回率
  
  assert_true(true)
}