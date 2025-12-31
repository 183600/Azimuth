// 遥测异常检测测试用例

test "telemetry_statistical_anomaly_detection" {
  // 测试遥测统计异常检测
  
  let statistical_config = {
    "algorithm": "z_score",
    "threshold": 2.5,
    "window_size": 100,
    "min_samples": 30
  }
  
  // 验证统计检测配置
  assert_eq(statistical_config["algorithm"], "z_score")
  assert_eq(statistical_config["threshold"], "2.5")
  assert_eq(statistical_config["window_size"], "100")
  assert_eq(statistical_config["min_samples"], "30")
  
  // 模拟正常和异常数据
  let metric_data = [
    45.2, 46.8, 44.9, 47.1, 45.6, 46.3, 44.7, 45.9, 46.5, 45.1,  // 正常范围
    46.2, 45.8, 44.3, 47.5, 45.4, 46.7, 44.1, 45.7, 46.9, 45.3,  // 正常范围
    95.2,  // 异常值
    46.1, 45.5, 44.8, 47.2, 45.0, 46.6, 44.5, 45.8, 46.4, 45.2,  // 恢复正常
    12.3,  // 异常值
    46.0, 45.9, 44.6, 47.3, 45.7, 46.8, 44.9, 45.6, 46.2, 45.4   // 恢复正常
  ]
  
  // 验证数据
  assert_eq(metric_data.length(), 32)
  
  // 使用前20个数据点作为训练集
  let training_size = 20
  let mut sum = 0.0
  let mut i = 0
  
  while i < training_size {
    sum = sum + metric_data[i]
    i = i + 1
  }
  
  let mean = sum / training_size.to_float()
  
  // 计算标准差
  let mut variance_sum = 0.0
  let mut i = 0
  
  while i < training_size {
    let value = metric_data[i]
    let diff = value - mean
    variance_sum = variance_sum + (diff * diff)
    i = i + 1
  }
  
  let variance = variance_sum / training_size.to_float()
  let std_dev = variance.sqrt()
  
  // 验证基础统计量
  assert_eq(mean > 40.0 and mean < 50.0, true)
  assert_eq(std_dev > 0.0 and std_dev < 5.0, true)
  
  // 检测异常
  let threshold = statistical_config["threshold"].to_float()
  let mut anomalies = []
  
  let mut i = training_size
  while i < metric_data.length() {
    let value = metric_data[i]
    let z_score = (value - mean) / std_dev
    let is_anomaly = z_score.abs() > threshold
    
    if is_anomaly {
      anomalies.push({
        "index": i,
        "value": value,
        "z_score": z_score,
        "mean": mean,
        "std_dev": std_dev
      })
    }
    
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)  // 检测到2个异常
  
  // 验证第一个异常（高值）
  assert_eq(anomalies[0]["index"], 20)
  assert_eq(anomalies[0]["value"], 95.2)
  assert_eq(anomalies[0]["z_score"] > threshold, true)
  
  // 验证第二个异常（低值）
  assert_eq(anomalies[1]["index"], 26)
  assert_eq(anomalies[1]["value"], 12.3)
  assert_eq(anomalies[1]["z_score"].abs() > threshold, true)
  
  // 测试动态阈值调整
  let dynamic_threshold_enabled = true
  let mut adjusted_threshold = threshold
  
  if dynamic_threshold_enabled {
    // 根据历史误报率调整阈值
    let false_positive_rate = 0.05  // 5%误报率
    let target_false_positive_rate = 0.01  // 1%目标误报率
    
    if false_positive_rate > target_false_positive_rate {
      adjusted_threshold = threshold * 1.2  // 提高阈值
    }
  }
  
  // 验证动态阈值调整
  assert_eq(adjusted_threshold > threshold, true)
}

test "telemetry_pattern_based_anomaly_detection" {
  // 测试遥测基于模式的异常检测
  
  let pattern_config = {
    "algorithm": "seasonal_decomposition",
    "season_length": 24,  // 24小时季节周期
    "trend_window": 168,  // 7天趋势窗口
    "threshold": 3.0
  }
  
  // 验证模式检测配置
  assert_eq(pattern_config["algorithm"], "seasonal_decomposition")
  assert_eq(pattern_config["season_length"], "24")
  assert_eq(pattern_config["trend_window"], "168")
  assert_eq(pattern_config["threshold"], "3.0")
  
  // 模拟具有日周期性的数据
  let seasonal_data = [
    // 第1天（正常模式）
    20.5, 19.8, 18.2, 17.5, 16.9, 17.2, 18.5, 22.1, 28.3, 35.7,
    42.8, 48.9, 52.3, 54.7, 55.2, 53.8, 49.6, 43.7, 37.2, 31.5,
    26.8, 23.1, 21.4, 20.8,
    // 第2天（正常模式）
    20.2, 19.5, 18.0, 17.3, 16.8, 17.0, 18.3, 21.9, 28.1, 35.5,
    42.6, 48.7, 52.1, 54.5, 55.0, 53.6, 49.4, 43.5, 37.0, 31.3,
    26.6, 22.9, 21.2, 20.6,
    // 第3天（异常模式）
    20.3, 19.6, 18.1, 17.4, 75.2,  // 异常：早高峰异常高
    17.1, 18.4, 21.8, 28.0, 35.4, 42.5, 48.6, 52.0, 54.4, 54.9,
    53.5, 49.3, 43.4, 36.9, 31.2, 26.5, 22.8, 21.1, 20.5
  ]
  
  // 验证季节性数据
  assert_eq(seasonal_data.length(), 69)
  
  let season_length = pattern_config["season_length"].to_int()
  
  // 计算季节性模式（使用前两天数据）
  let mut seasonal_pattern = []
  
  let mut i = 0
  while i < season_length {
    let day1_val = seasonal_data[i]
    let day2_val = seasonal_data[i + season_length]
    let seasonal_avg = (day1_val + day2_val) / 2.0
    seasonal_pattern.push(seasonal_avg)
    i = i + 1
  }
  
  // 验证季节性模式
  assert_eq(seasonal_pattern.length(), 24)
  
  // 检测第三天的异常
  let day3_start = season_length * 2
  let threshold = pattern_config["threshold"].to_float()
  let mut pattern_anomalies = []
  
  let mut i = day3_start
  while i < seasonal_data.length() {
    let hour_of_day = i - day3_start
    let expected_value = seasonal_pattern[hour_of_day]
    let actual_value = seasonal_data[i]
    
    // 计算偏差
    let deviation = (actual_value - expected_value).abs()
    let relative_deviation = deviation / expected_value
    
    // 检查是否为异常
    if relative_deviation > (threshold / 10.0) {  // 转换为相对阈值
      pattern_anomalies.push({
        "index": i,
        "hour_of_day": hour_of_day,
        "expected": expected_value,
        "actual": actual_value,
        "deviation": deviation,
        "relative_deviation": relative_deviation
      })
    }
    
    i = i + 1
  }
  
  // 验证模式异常检测结果
  assert_eq(pattern_anomalies.length(), 1)  // 检测到1个异常
  
  // 验证异常详情
  assert_eq(pattern_anomalies[0]["hour_of_day"], 4)  // 第4小时
  assert_eq(pattern_anomalies[0]["expected"] > 16.0 and pattern_anomalies[0]["expected"] < 18.0, true)
  assert_eq(pattern_anomalies[0]["actual"], 75.2)
  assert_eq(pattern_anomalies[0]["relative_deviation"] > 3.0, true)
  
  // 测试趋势检测
  let trend_window = pattern_config["trend_window"].to_int()
  
  // 简化的趋势检测：检查最近几个点的趋势
  let recent_data = seasonal_data.slice(seasonal_data.length() - 5, 5)
  let mut trend_sum = 0.0
  
  let mut i = 1
  while i < recent_data.length() {
    let diff = recent_data[i] - recent_data[i - 1]
    trend_sum = trend_sum + diff
    i = i + 1
  }
  
  let avg_trend = trend_sum / (recent_data.length() - 1).to_float()
  
  // 验证趋势计算
  assert_eq(avg_trend > -5.0 and avg_trend < 5.0, true)  // 趋势应该在合理范围内
}

test "telemetry_machine_learning_anomaly_detection" {
  // 测试遥测机器学习异常检测
  
  let ml_config = {
    "algorithm": "isolation_forest",
    "contamination_rate": 0.1,
    "n_estimators": 100,
    "max_samples": "auto"
  }
  
  // 验证机器学习检测配置
  assert_eq(ml_config["algorithm"], "isolation_forest")
  assert_eq(ml_config["contamination_rate"], "0.1")
  assert_eq(ml_config["n_estimators"], "100")
  assert_eq(ml_config["max_samples"], "auto")
  
  // 模拟多维度指标数据
  let multidimensional_data = [
    {"cpu": 45.2, "memory": 60.5, "network": 120.3, "disk": 25.8},  // 正常
    {"cpu": 46.8, "memory": 62.1, "network": 118.7, "disk": 26.2},  // 正常
    {"cpu": 44.9, "memory": 59.8, "network": 122.1, "disk": 25.1},  // 正常
    {"cpu": 47.1, "memory": 63.4, "network": 119.5, "disk": 27.3},  // 正常
    {"cpu": 45.6, "memory": 61.2, "network": 121.8, "disk": 24.9},  // 正常
    {"cpu": 95.2, "memory": 85.7, "network": 450.6, "disk": 95.3},  // 异常
    {"cpu": 46.3, "memory": 60.8, "network": 120.9, "disk": 26.1},  // 正常
    {"cpu": 44.7, "memory": 58.9, "network": 123.4, "disk": 24.7},  // 正常
    {"cpu": 15.3, "memory": 35.2, "network": 45.8, "disk": 12.1},   // 异常
    {"cpu": 45.9, "memory": 61.7, "network": 119.8, "disk": 25.6}   // 正常
  ]
  
  // 验证多维度数据
  assert_eq(multidimensional_data.length(), 10)
  
  // 计算每个维度的统计量
  let dimensions = ["cpu", "memory", "network", "disk"]
  let mut dimension_stats = {}
  
  let mut d = 0
  while d < dimensions.length() {
    let dim = dimensions[d]
    
    // 计算平均值
    let mut sum = 0.0
    let mut i = 0
    while i < multidimensional_data.length() {
      sum = sum + multidimensional_data[i][dim].to_float()
      i = i + 1
    }
    let mean = sum / multidimensional_data.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    let mut i = 0
    while i < multidimensional_data.length() {
      let value = multidimensional_data[i][dim].to_float()
      let diff = value - mean
      variance_sum = variance_sum + (diff * diff)
      i = i + 1
    }
    let variance = variance_sum / multidimensional_data.length().to_float()
    let std_dev = variance.sqrt()
    
    dimension_stats[dim] = {"mean": mean, "std_dev": std_dev}
    d = d + 1
  }
  
  // 验证维度统计量
  assert_eq(dimension_stats.size(), 4)
  assert_eq(dimension_stats["cpu"]["mean"] > 40.0, true)
  assert_eq(dimension_stats["memory"]["mean"] > 50.0, true)
  
  // 模拟孤立森林异常检测（简化版）
  let contamination_rate = ml_config["contamination_rate"].to_float()
  let expected_anomalies = Int::from_float(multidimensional_data.length().to_float() * contamination_rate)
  
  // 计算每个数据点的异常分数（简化的马氏距离）
  let mut anomaly_scores = []
  
  let mut i = 0
  while i < multidimensional_data.length() {
    let data_point = multidimensional_data[i]
    let mut score = 0.0
    
    // 计算各维度的z-score平方和
    let mut d = 0
    while d < dimensions.length() {
      let dim = dimensions[d]
      let value = data_point[dim].to_float()
      let mean = dimension_stats[dim]["mean"]
      let std_dev = dimension_stats[dim]["std_dev"]
      
      if std_dev > 0.0 {
        let z_score = (value - mean) / std_dev
        score = score + (z_score * z_score)
      }
      
      d = d + 1
    }
    
    anomaly_scores.push(score)
    i = i + 1
  }
  
  // 根据分数确定异常（分数最高的为异常）
  let mut indexed_scores = []
  let mut i = 0
  while i < anomaly_scores.length() {
    indexed_scores.push({"index": i, "score": anomaly_scores[i]})
    i = i + 1
  }
  
  // 按分数排序（简化：手动识别最高分）
  let mut max_score = 0.0
  let mut max_score_index = 0
  let mut second_max_score = 0.0
  let mut second_max_score_index = 0
  
  let mut i = 0
  while i < indexed_scores.length() {
    let score = indexed_scores[i]["score"]
    if score > max_score {
      second_max_score = max_score
      second_max_score_index = max_score_index
      max_score = score
      max_score_index = indexed_scores[i]["index"]
    } else if score > second_max_score {
      second_max_score = score
      second_max_score_index = indexed_scores[i]["index"]
    }
    i = i + 1
  }
  
  // 验证异常检测结果
  let detected_anomalies = [max_score_index, second_max_score_index]
  assert_eq(detected_anomalies.length(), 2)
  
  // 验证检测到的异常点
  assert_eq(detected_anomalies.contains(5), true)  // 第5个点（高值异常）
  assert_eq(detected_anomalies.contains(8), true)  // 第8个点（低值异常）
  
  // 测试模型更新
  let model_update_enabled = true
  let update_threshold = 50  // 50个新样本后更新模型
  
  if model_update_enabled {
    let new_samples_count = 60
    let should_update_model = new_samples_count >= update_threshold
    
    // 验证模型更新逻辑
    assert_eq(should_update_model, true)
    
    // 模拟模型更新过程
    let model_update_steps = [
      "collect_new_samples",
      "validate_data_quality",
      "retrain_model",
      "validate_model_performance",
      "deploy_updated_model"
    ]
    
    // 验证模型更新步骤
    assert_eq(model_update_steps.length(), 5)
  }
}