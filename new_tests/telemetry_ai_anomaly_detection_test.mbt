// 遥测AI异常检测测试用例

test "telemetry_ai_anomaly_detection_statistical" {
  // 测试基于统计学的AI异常检测算法
  
  let normal_data = [10.0, 12.0, 11.5, 10.8, 11.2, 12.1, 10.9, 11.0]
  let anomaly_data = [10.0, 12.0, 11.5, 10.8, 50.0, 11.2, 12.1, 10.9]
  let threshold = 2.0  // 2个标准差
  
  // 验证测试数据
  assert_eq(normal_data.length(), 8)
  assert_eq(anomaly_data.length(), 8)
  assert_eq(threshold, 2.0)
  
  // 计算正常数据的均值和标准差
  let mut sum = 0.0
  let mut i = 0
  while i < normal_data.length() {
    sum = sum + normal_data[i]
    i = i + 1
  }
  let mean = sum / normal_data.length().to_double()
  
  // 计算标准差
  let mut variance = 0.0
  i = 0
  while i < normal_data.length() {
    let diff = normal_data[i] - mean
    variance = variance + diff * diff
    i = i + 1
  }
  variance = variance / normal_data.length().to_double()
  let std_dev = sqrt(variance)
  
  // 验证统计参数
  assert_eq(mean > 10.0, true)
  assert_eq(mean < 12.0, true)
  assert_eq(std_dev > 0.0, true)
  
  // 检测异常数据中的异常点
  let mut anomaly_count = 0
  i = 0
  while i < anomaly_data.length() {
    let z_score = abs(anomaly_data[i] - mean) / std_dev
    if z_score > threshold {
      anomaly_count = anomaly_count + 1
    }
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(anomaly_count > 0, true)
  assert_eq(anomaly_count <= 2, true)  // 最多2个异常点
}

test "telemetry_ai_anomaly_detection_ml" {
  // 测试基于机器学习的异常检测
  
  let training_data = [
    (1.0, 2.0, "normal"),
    (1.1, 2.1, "normal"),
    (0.9, 1.9, "normal"),
    (1.2, 2.2, "normal"),
    (1.0, 2.0, "normal")
  ]
  let test_data = [
    (1.05, 2.05, "normal"),
    (5.0, 10.0, "anomaly"),
    (0.8, 1.8, "normal")
  ]
  let confidence_threshold = 0.8
  
  // 验证ML数据
  assert_eq(training_data.length(), 5)
  assert_eq(test_data.length(), 3)
  assert_eq(confidence_threshold, 0.8)
  
  // 简化的ML模型训练（计算正常簇的中心）
  let mut center_x = 0.0
  let mut center_y = 0.0
  let mut normal_count = 0
  
  let mut i = 0
  while i < training_data.length() {
    if training_data[i].2 == "normal" {
      center_x = center_x + training_data[i].0
      center_y = center_y + training_data[i].1
      normal_count = normal_count + 1
    }
    i = i + 1
  }
  
  center_x = center_x / normal_count.to_double()
  center_y = center_y / normal_count.to_double()
  
  // 计算正常簇的半径
  let mut max_distance = 0.0
  i = 0
  while i < training_data.length() {
    if training_data[i].2 == "normal" {
      let distance = sqrt(
        (training_data[i].0 - center_x) * (training_data[i].0 - center_x) +
        (training_data[i].1 - center_y) * (training_data[i].1 - center_y)
      )
      if distance > max_distance {
        max_distance = distance
      }
    }
    i = i + 1
  }
  
  // 验证ML模型参数
  assert_eq(center_x > 0.5, true)
  assert_eq(center_x < 1.5, true)
  assert_eq(center_y > 1.5, true)
  assert_eq(center_y < 2.5, true)
  assert_eq(max_distance > 0.0, true)
  
  // 使用ML模型进行异常检测
  let mut detected_anomalies = 0
  i = 0
  while i < test_data.length() {
    let distance = sqrt(
      (test_data[i].0 - center_x) * (test_data[i].0 - center_x) +
      (test_data[i].1 - center_y) * (test_data[i].1 - center_y)
    )
    
    // 计算异常置信度
    let confidence = if distance <= max_distance {
      1.0 - (distance / max_distance)
    } else {
      0.0
    }
    
    if confidence < confidence_threshold {
      detected_anomalies = detected_anomalies + 1
    }
    
    i = i + 1
  }
  
  // 验证ML异常检测结果
  assert_eq(detected_anomalies >= 1, true)
  assert_eq(detected_anomalies <= 2, true)
}

test "telemetry_ai_anomaly_detection_time_series" {
  // 测试时间序列异常检测
  
  let time_series_data = [
    (1000, 10.5),
    (2000, 11.0),
    (3000, 10.8),
    (4000, 11.2),
    (5000, 10.9),
    (6000, 11.1),
    (7000, 10.7),
    (8000, 50.0),  // 异常点
    (9000, 11.0),
    (10000, 10.8)
  ]
  let window_size = 4
  let anomaly_threshold = 3.0
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 10)
  assert_eq(window_size, 4)
  assert_eq(anomaly_threshold, 3.0)
  
  // 滑动窗口异常检测
  let mut anomalies_detected = 0
  let mut i = window_size
  
  while i < time_series_data.length() {
    // 计算窗口内的统计特征
    let mut window_sum = 0.0
    let mut j = i - window_size
    
    while j < i {
      window_sum = window_sum + time_series_data[j].1
      j = j + 1
    }
    
    let window_mean = window_sum / window_size.to_double()
    
    // 计算窗口方差
    let mut window_variance = 0.0
    j = i - window_size
    while j < i {
      let diff = time_series_data[j].1 - window_mean
      window_variance = window_variance + diff * diff
      j = j + 1
    }
    window_variance = window_variance / window_size.to_double()
    let window_std = sqrt(window_variance)
    
    // 检测当前点是否异常
    let current_value = time_series_data[i].1
    let z_score = abs(current_value - window_mean) / window_std
    
    if z_score > anomaly_threshold {
      anomalies_detected = anomalies_detected + 1
    }
    
    i = i + 1
  }
  
  // 验证时间序列异常检测结果
  assert_eq(anomalies_detected >= 1, true)
  assert_eq(anomalies_detected <= 3, true)
}

test "telemetry_ai_anomaly_detection_adaptive" {
  // 测试自适应异常检测
  
  let initial_threshold = 2.0
  let adaptation_rate = 0.1
  let data_stream = [
    10.0, 11.0, 10.5, 12.0, 11.5,  // 正常数据
    15.0, 16.0, 14.5,              // 渐变异常
    10.0, 11.0, 10.5               // 恢复正常
  ]
  
  // 验证自适应检测参数
  assert_eq(initial_threshold, 2.0)
  assert_eq(adaptation_rate, 0.1)
  assert_eq(data_stream.length(), 11)
  
  // 自适应阈值异常检测
  let mut current_threshold = initial_threshold
  let mut base_value = data_stream[0]
  let mut anomalies_detected = 0
  
  let mut i = 1
  while i < data_stream.length() {
    let current_value = data_stream[i]
    let deviation = abs(current_value - base_value)
    
    // 检测异常
    if deviation > current_threshold {
      anomalies_detected = anomalies_detected + 1
      
      // 自适应调整阈值
      current_threshold = current_threshold + adaptation_rate * deviation
    } else {
      // 缓慢恢复到初始阈值
      current_threshold = current_threshold * (1.0 - adaptation_rate) + 
                         initial_threshold * adaptation_rate
    }
    
    // 更新基准值（使用指数移动平均）
    base_value = base_value * 0.9 + current_value * 0.1
    
    i = i + 1
  }
  
  // 验证自适应检测结果
  assert_eq(anomalies_detected >= 2, true)
  assert_eq(anomalies_detected <= 5, true)
  assert_eq(current_threshold > initial_threshold, true)
  assert_eq(current_threshold < initial_threshold * 3.0, true)
}