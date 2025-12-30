// 遥测数据异常检测测试用例

test "telemetry_statistical_anomaly_detection" {
  // 测试遥测统计异常检测
  
  let statistical_methods = [
    {
      "method_name": "z_score",
      "threshold": 3.0,
      "window_size": 100,
      "sensitivity": "medium",
      "use_case": "metric_spikes"
    },
    {
      "method_name": "iqr",
      "multiplier": 1.5,
      "window_size": 200,
      "sensitivity": "low",
      "use_case": "distribution_outliers"
    },
    {
      "method_name": "moving_average",
      "period": 20,
      "deviation_threshold": 2.0,
      "sensitivity": "high",
      "use_case": "trend_deviation"
    },
    {
      "method_name": "exponential_smoothing",
      "alpha": 0.3,
      "std_threshold": 2.5,
      "sensitivity": "medium",
      "use_case": "time_series_anomaly"
    }
  ]
  
  // 验证统计方法
  assert_eq(statistical_methods.length(), 4)
  
  // 生成测试时间序列数据
  let mut time_series_data = []
  let mut i = 0
  let data_points = 500
  
  while i < data_points {
    let timestamp = 1641018000000L + (i * 60000)  // 每分钟一个数据点
    let base_value = 100.0
    let trend = i.to_double() * 0.1  // 轻微上升趋势
    let seasonal = 10.0 * (i.to_double() * 0.1).sin()  // 季节性变化
    let noise = (i * 7) % 20 - 10.0  // 随机噪声 (-10 到 +10)
    
    let normal_value = base_value + trend + seasonal + noise.to_double()
    
    // 插入一些异常值
    let value = if i == 100 {
      250.0  // 高异常值
    } else if i == 200 {
      20.0   // 低异常值
    } else if i == 300 {
      180.0  // 中等异常值
    } else if i == 400 {
      -10.0  // 极低异常值
    } else {
      normal_value
    }
    
    time_series_data.push((timestamp, value))
    i = i + 1
  }
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 500)
  
  // 模拟统计异常检测
  let mut anomaly_detection_results = []
  
  for method in statistical_methods {
    let method_name = method["method_name"]
    let sensitivity = method["sensitivity"]
    let use_case = method["use_case"]
    
    // 模拟异常检测算法
    let mut detected_anomalies = []
    let mut false_positives = 0
    let mut true_positives = 0
    let mut false_negatives = 0
    
    // 已知的真实异常位置
    let true_anomaly_positions = [100, 200, 300, 400]
    
    // 滑动窗口检测
    let window_size = method["window_size"]
    let mut i = window_size
    while i < time_series_data.length() {
      // 提取窗口数据
      let mut window_values = []
      let mut j = i - window_size
      while j < i {
        window_values.push(time_series_data[j].1)
        j = j + 1
      }
      
      // 计算窗口统计量
      let mut sum = 0.0
      for value in window_values {
        sum = sum + value
      }
      let mean = sum / window_values.length().to_double()
      
      let mut sum_squared_diff = 0.0
      for value in window_values {
        let diff = value - mean
        sum_squared_diff = sum_squared_diff + diff * diff
      }
      let std_dev = (sum_squared_diff / window_values.length().to_double()).sqrt()
      
      // 检测当前点是否异常
      let current_value = time_series_data[i].1
      let is_anomaly = match method_name {
        "z_score" => {
          let threshold = method["threshold"]
          (current_value - mean).abs() / std_dev > threshold
        }
        "iqr" => {
          let mut sorted_values = window_values
          // 简化排序（实际应该实现完整排序）
          let q1 = sorted_values[window_values.length() / 4]
          let q3 = sorted_values[window_values.length() * 3 / 4]
          let iqr = q3 - q1
          let multiplier = method["multiplier"]
          current_value < q1 - iqr * multiplier or current_value > q3 + iqr * multiplier
        }
        "moving_average" => {
          let period = method["period"]
          let mut sum_recent = 0.0
          let mut j = 0
          while j < period {
            sum_recent = sum_recent + time_series_data[i - period + j].1
            j = j + 1
          }
          let moving_avg = sum_recent / period.to_double()
          let threshold = method["deviation_threshold"]
          (current_value - moving_avg).abs() > threshold * std_dev
        }
        "exponential_smoothing" => {
          let alpha = method["alpha"]
          let ema = if i > 0 {
            let prev_ema = mean  // 简化，使用窗口均值作为前一个EMA
            alpha * current_value + (1.0 - alpha) * prev_ema
          } else {
            current_value
          }
          let threshold = method["std_threshold"]
          (current_value - ema).abs() > threshold * std_dev
        }
        _ => false
      }
      
      if is_anomaly {
        detected_anomalies.push((i, current_value, mean, std_dev))
        
        // 检查是否是真实异常
        let is_true_anomaly = true_anomaly_positions.contains(i)
        if is_true_anomaly {
          true_positives = true_positives + 1
        } else {
          false_positives = false_positives + 1
        }
      }
      
      i = i + 1
    }
    
    // 计算漏检的异常
    for pos in true_anomaly_positions {
      let detected = false
      for anomaly in detected_anomalies {
        if anomaly.0 == pos {
          detected = true
          break
        }
      }
      if not detected {
        false_negatives = false_negatives + 1
      }
    }
    
    // 计算性能指标
    let precision = if true_positives + false_positives > 0 {
      true_positives.to_double() / (true_positives + false_positives).to_double()
    } else {
      0.0
    }
    
    let recall = if true_positives + false_negatives > 0 {
      true_positives.to_double() / (true_positives + false_negatives).to_double()
    } else {
      0.0
    }
    
    let f1_score = if precision + recall > 0.0 {
      2.0 * precision * recall / (precision + recall)
    } else {
      0.0
    }
    
    anomaly_detection_results.push((
      method_name,
      sensitivity,
      use_case,
      detected_anomalies.length(),
      true_positives,
      false_positives,
      false_negatives,
      precision,
      recall,
      f1_score
    ))
  }
  
  // 验证异常检测结果
  assert_eq(anomaly_detection_results.length(), 4)
  
  // 验证Z-Score方法
  let z_score_result = anomaly_detection_results[0]
  assert_eq(z_score_result.0, "z_score")
  assert_eq(z_score_result.1, "medium")
  assert_eq(z_score_result.2, "metric_spikes")
  assert_eq(z_score_result.4 >= 2, true)  // 应该检测到至少2个真实异常
  assert_eq(z_score_result.8 > 0.5, true)  // 召回率应该大于50%
  
  // 验证所有方法都有一定的检测能力
  for result in anomaly_detection_results {
    assert_eq(result.3 >= 0, true)  // 检测到的异常数应该非负
    assert_eq(result.9 >= 0.0, true)  // F1分数应该非负
    assert_eq(result.9 <= 1.0, true)  // F1分数应该不超过1.0
    
    // 至少应该检测到一些异常或有一些误报
    assert_eq(result.3 > 0 or result.5 > 0, true)
  }
}

test "telemetry_ml_based_anomaly_detection" {
  // 测试基于机器学习的异常检测
  
  let ml_models = [
    {
      "model_name": "isolation_forest",
      "algorithm_type": "unsupervised",
      "contamination_rate": 0.1,
      "training_samples": 1000,
      "feature_count": 5
    },
    {
      "model_name": "one_class_svm",
      "algorithm_type": "unsupervised",
      "nu_parameter": 0.05,
      "training_samples": 800,
      "feature_count": 8
    },
    {
      "model_name": "autoencoder",
      "algorithm_type": "deep_learning",
      "encoding_dim": 3,
      "training_samples": 2000,
      "feature_count": 10
    },
    {
      "model_name": "lstm_autoencoder",
      "algorithm_type": "sequence_model",
      "sequence_length": 20,
      "training_samples": 1500,
      "feature_count": 6
    }
  ]
  
  // 验证ML模型
  assert_eq(ml_models.length(), 4)
  
  // 生成多维度特征数据
  let mut feature_data = []
  let mut i = 0
  let sample_count = 1200
  
  while i < sample_count {
    let timestamp = 1641018000000L + (i * 60000)
    
    // 生成正常特征
    let cpu_usage = 30.0 + (i % 40).to_double() + (i * 13) % 10 / 10.0
    let memory_usage = 50.0 + (i % 30).to_double() + (i * 7) % 15 / 10.0
    let disk_io = 100.0 + (i % 50).to_double() + (i * 11) % 20 / 10.0
    let network_io = 200.0 + (i % 60).to_double() + (i * 17) % 25 / 10.0
    let response_time = 50.0 + (i % 20).to_double() + (i * 9) % 12 / 10.0
    
    // 插入异常
    let is_anomaly_sample = i == 200 or i == 400 or i == 600 or i == 800 or i == 1000
    
    let features = if is_anomaly_sample {
      [
        cpu_usage * 3.0,      // CPU异常高
        memory_usage * 2.5,   // 内存异常高
        disk_io * 0.1,        // 磁盘IO异常低
        network_io * 4.0,     // 网络IO异常高
        response_time * 5.0   // 响应时间异常高
      ]
    } else {
      [cpu_usage, memory_usage, disk_io, network_io, response_time]
    }
    
    feature_data.push((timestamp, features, is_anomaly_sample))
    i = i + 1
  }
  
  // 验证特征数据
  assert_eq(feature_data.length(), 1200)
  
  // 模拟ML模型训练和预测
  let mut ml_detection_results = []
  
  for model in ml_models {
    let model_name = model["model_name"]
    let algorithm_type = model["algorithm_type"]
    let training_samples = model["training_samples"]
    let feature_count = model["feature_count"]
    
    // 分割训练和测试数据
    let training_data = feature_data.slice(0, training_samples)
    let test_data = feature_data.slice(training_samples, feature_data.length() - training_samples)
    
    // 模拟模型训练
    let training_time_seconds = match algorithm_type {
      "unsupervised" => 10.0,
      "deep_learning" => 120.0,
      "sequence_model" => 180.0,
      _ => 30.0
    }
    
    // 模拟模型预测
    let mut predictions = []
    let mut true_positives = 0
    let mut false_positives = 0
    let mut false_negatives = 0
    
    for sample in test_data {
      let features = sample.1
      let is_true_anomaly = sample.2
      
      // 模拟异常分数计算
      let anomaly_score = match model_name {
        "isolation_forest" => {
          let sum_features = 0.0
          for f in features {
            sum_features = sum_features + f
          }
          if sum_features > 500.0 { 0.9 } else { 0.1 }
        }
        "one_class_svm" => {
          let max_feature = features[0]
          for f in features {
            if f > max_feature {
              max_feature = f
            }
          }
          if max_feature > 150.0 { 0.85 } else { 0.15 }
        }
        "autoencoder" => {
          let reconstruction_error = features[0] * features[1] / 1000.0
          if reconstruction_error > 10.0 { 0.95 } else { 0.05 }
        }
        "lstm_autoencoder" => {
          let sequence_score = (features[0] + features[1] + features[2]) / 3.0
          if sequence_score > 200.0 { 0.88 } else { 0.12 }
        }
        _ => 0.5
      }
      
      // 根据分数预测是否异常
      let threshold = match model_name {
        "isolation_forest" => 0.5,
        "one_class_svm" => 0.4,
        "autoencoder" => 0.3,
        "lstm_autoencoder" => 0.35,
        _ => 0.5
      }
      
      let predicted_anomaly = anomaly_score > threshold
      predictions.push((anomaly_score, predicted_anomaly))
      
      // 计算混淆矩阵
      if predicted_anomaly and is_true_anomaly {
        true_positives = true_positives + 1
      } else if predicted_anomaly and not is_true_anomaly {
        false_positives = false_positives + 1
      } else if not predicted_anomaly and is_true_anomaly {
        false_negatives = false_negatives + 1
      }
    }
    
    // 计算性能指标
    let precision = if true_positives + false_positives > 0 {
      true_positives.to_double() / (true_positives + false_positives).to_double()
    } else {
      0.0
    }
    
    let recall = if true_positives + false_negatives > 0 {
      true_positives.to_double() / (true_positives + false_negatives).to_double()
    } else {
      0.0
    }
    
    let f1_score = if precision + recall > 0.0 {
      2.0 * precision * recall / (precision + recall)
    } else {
      0.0
    }
    
    // 计算模型复杂度指标
    let model_complexity = match algorithm_type {
      "unsupervised" => "low",
      "deep_learning" => "high",
      "sequence_model" => "very_high",
      _ => "medium"
    }
    
    ml_detection_results.push((
      model_name,
      algorithm_type,
      training_samples,
      test_data.length(),
      training_time_seconds,
      true_positives,
      false_positives,
      false_negatives,
      precision,
      recall,
      f1_score,
      model_complexity
    ))
  }
  
  // 验证ML检测结果
  assert_eq(ml_detection_results.length(), 4)
  
  // 验证深度学习模型
  let autoencoder_result = ml_detection_results[2]
  assert_eq(autoencoder_result.0, "autoencoder")
  assert_eq(autoencoder_result.1, "deep_learning")
  assert_eq(autoencoder_result.4, 120.0)  // 训练时间应该较长
  assert_eq(autoencoder_result.11, "high")  // 复杂度高
  assert_eq(autoencoder_result.10 > 0.6, true)  // F1分数应该较好
  
  // 验证无监督学习模型
  let isolation_forest_result = ml_detection_results[0]
  assert_eq(isolation_forest_result.0, "isolation_forest")
  assert_eq(isolation_forest_result.1, "unsupervised")
  assert_eq(isolation_forest_result.4, 10.0)  // 训练时间应该较短
  assert_eq(isolation_forest_result.11, "low")  // 复杂度低
  
  // 验证所有模型都有合理的性能
  for result in ml_detection_results {
    assert_eq(result.5 >= 0, true)  // 真正例应该非负
    assert_eq(result.6 >= 0, true)  // 假正例应该非负
    assert_eq(result.7 >= 0, true)  // 假负例应该非负
    assert_eq(result.10 >= 0.0, true)  // F1分数应该非负
    assert_eq(result.10 <= 1.0, true)  // F1分数应该不超过1.0
    assert_eq(result.4 > 0.0, true)   // 训练时间应该大于0
  }
}

test "telemetry_seasonal_anomaly_detection" {
  // 测试遥测季节性异常检测
  
  let seasonal_patterns = [
    {
      "pattern_name": "daily_traffic",
      "period_hours": 24,
      "peak_hours": [9, 10, 11, 14, 15, 16],
      "low_hours": [2, 3, 4, 5],
      "expected_amplitude": 2.0
    },
    {
      "pattern_name": "weekly_business",
      "period_days": 7,
      "peak_days": [1, 2, 3, 4, 5],  // 工作日
      "low_days": [0, 6],  // 周末
      "expected_amplitude": 1.5
    },
    {
      "pattern_name": "monthly_billing",
      "period_days": 30,
      "peak_days": [1, 2, 3, 28, 29, 30],
      "low_days": [15, 16, 17],
      "expected_amplitude": 3.0
    }
  ]
  
  // 验证季节性模式
  assert_eq(seasonal_patterns.length(), 3)
  
  // 生成季节性时间序列数据
  let mut seasonal_data = []
  let mut i = 0
  let days = 60  // 2个月的数据
  let hours_per_day = 24
  
  while i < days * hours_per_day {
    let timestamp = 1641018000000L + (i * 3600000)  // 每小时一个数据点
    let day_of_week = (i / hours_per_day) % 7
    let hour_of_day = i % 24
    let day_of_month = (i / hours_per_day) % 30 + 1
    
    // 基础流量
    let base_traffic = 100.0
    
    // 日模式
    let daily_factor = if hour_of_day >= 9 and hour_of_day <= 16 {
      2.0  // 工作时间高峰
    } else if hour_of_day >= 2 and hour_of_day <= 5 {
      0.3  // 深夜低谷
    } else {
      1.0  // 正常时间
    }
    
    // 周模式
    let weekly_factor = if day_of_week >= 1 and day_of_week <= 5 {
      1.5  // 工作日
    } else {
      0.7  // 周末
    }
    
    // 月模式
    let monthly_factor = if day_of_month <= 3 or day_of_month >= 28 {
      2.5  // 月初月末高峰
    } else if day_of_month >= 15 and day_of_month <= 17 {
      0.5  // 月中低谷
    } else {
      1.0  // 正常日期
    }
    
    // 计算季节性流量
    let seasonal_traffic = base_traffic * daily_factor * weekly_factor * monthly_factor
    
    // 添加随机噪声
    let noise = (i * 13) % 20 - 10.0
    let traffic = seasonal_traffic + noise
    
    // 插入季节性异常
    let anomalous_traffic = if i == 240 {  // 第10天凌晨3点（应该是低谷）
      500.0  // 异常高峰
    } else if i == 600 {  // 第25天下午2点（应该是高峰）
      20.0   // 异常低谷
    } else if i == 1000 { // 第42天上午10点（应该是高峰）
      15.0   // 异常低谷
    } else {
      traffic
    }
    
    seasonal_data.push((
      timestamp,
      day_of_week,
      hour_of_day,
      day_of_month,
      anomalous_traffic
    ))
    
    i = i + 1
  }
  
  // 验证季节性数据
  assert_eq(seasonal_data.length(), 1440)  // 60天 × 24小时
  
  // 模拟季节性异常检测
  let mut seasonal_detection_results = []
  
  for pattern in seasonal_patterns {
    let pattern_name = pattern["pattern_name"]
    let period = match pattern_name {
      "daily_traffic" => 24,
      "weekly_business" => 7 * 24,
      "monthly_billing" => 30 * 24,
      _ => 24
    }
    
    let expected_amplitude = pattern["expected_amplitude"]
    
    // 计算季节性基线
    let mut seasonal_baselines = {}
    let mut i = 0
    while i < period {
      let mut period_values = []
      let mut j = i
      while j < seasonal_data.length() {
        period_values.push(seasonal_data[j].4)
        j = j + period
      }
      
      // 计算该时间点的基线统计量
      let mut sum = 0.0
      for value in period_values {
        sum = sum + value
      }
      let mean = sum / period_values.length().to_double()
      
      let mut sum_squared_diff = 0.0
      for value in period_values {
        let diff = value - mean
        sum_squared_diff = sum_squared_diff + diff * diff
      }
      let std_dev = (sum_squared_diff / period_values.length().to_double()).sqrt()
      
      seasonal_baselines[i] = (mean, std_dev)
      i = i + 1
    }
    
    // 检测季节性异常
    let mut detected_anomalies = []
    let mut i = period
    while i < seasonal_data.length() {
      let period_index = i % period
      let baseline = seasonal_baselines[period_index]
      let baseline_mean = baseline.0
      let baseline_std = baseline.1
      
      let current_value = seasonal_data[i].4
      
      // 季节性异常检测：与历史同期比较
      let z_score = (current_value - baseline_mean) / baseline_std
      let is_anomaly = z_score.abs() > 3.0  // 3-sigma规则
      
      if is_anomaly {
        detected_anomalies.push((
          i,
          current_value,
          baseline_mean,
          baseline_std,
          z_score
        ))
      }
      
      i = i + 1
    }
    
    // 验证检测到的异常
    let mut true_anomalies_detected = 0
    let known_anomaly_positions = [240, 600, 1000]
    
    for anomaly in detected_anomalies {
      let position = anomaly.0
      if known_anomaly_positions.contains(position) {
        true_anomalies_detected = true_anomalies_detected + 1
      }
    }
    
    // 计算检测率
    let detection_rate = true_anomalies_detected.to_double() / known_anomaly_positions.length().to_double()
    
    seasonal_detection_results.push((
      pattern_name,
      period,
      seasonal_baselines.length(),
      detected_anomalies.length(),
      true_anomalies_detected,
      detection_rate
    ))
  }
  
  // 验证季节性检测结果
  assert_eq(seasonal_detection_results.length(), 3)
  
  // 验证日模式检测
  let daily_result = seasonal_detection_results[0]
  assert_eq(daily_result.0, "daily_traffic")
  assert_eq(daily_result.1, 24)  // 24小时周期
  assert_eq(daily_result.2, 24)  // 24个基线点
  assert_eq(daily_result.4 >= 1, true)  // 应该检测到至少1个真实异常
  
  // 验证周模式检测
  let weekly_result = seasonal_detection_results[1]
  assert_eq(weekly_result.0, "weekly_business")
  assert_eq(weekly_result.1, 168)  // 7天 × 24小时
  assert_eq(weekly_result.2, 168)  // 168个基线点
  
  // 验证月模式检测
  let monthly_result = seasonal_detection_results[2]
  assert_eq(monthly_result.0, "monthly_billing")
  assert_eq(monthly_result.1, 720)  // 30天 × 24小时
  assert_eq(monthly_result.2, 720)  // 720个基线点
  
  // 验证所有模式都有一定的检测能力
  for result in seasonal_detection_results {
    assert_eq(result.3 >= 0, true)  // 检测到的异常数应该非负
    assert_eq(result.5 >= 0.0, true)  // 检测率应该非负
    assert_eq(result.5 <= 1.0, true)  // 检测率应该不超过1.0
  }
}

test "telemetry_multivariate_anomaly_detection" {
  // 测试遥测多变量异常检测
  
  let multivariate_methods = [
    {
      "method_name": "mahalanobis_distance",
      "correlation_threshold": 0.8,
      "covariance_regularization": true,
      "dimensionality": 5
    },
    {
      "method_name": "principal_component_analysis",
      "variance_explained_threshold": 0.95,
      "components_retained": 3,
      "dimensionality": 8
    },
    {
      "method_name": "hotelling_t2",
      "significance_level": 0.05,
      "degrees_of_freedom": 6,
      "dimensionality": 6
    },
    {
      "method_name": "isolation_forest_mv",
      "contamination_rate": 0.05,
      "tree_count": 100,
      "dimensionality": 10
    }
  ]
  
  // 验证多变量方法
  assert_eq(multivariate_methods.length(), 4)
  
  // 生成多变量相关数据
  let mut multivariate_data = []
  let mut i = 0
  let sample_count = 800
  
  while i < sample_count {
    let timestamp = 1641018000000L + (i * 60000)
    
    // 生成相关的正常数据
    let base_load = 50.0 + (i % 30).to_double()
    let cpu_usage = base_load + (i * 7) % 10 / 10.0
    let memory_usage = base_load * 1.5 + (i * 11) % 15 / 10.0
    let disk_io = base_load * 0.8 + (i * 13) % 12 / 10.0
    let network_io = base_load * 2.0 + (i * 17) % 20 / 10.0
    let response_time = base_load * 0.3 + (i * 9) % 8 / 10.0
    
    // 插入多变量异常
    let is_multivariate_anomaly = i == 150 or i == 300 or i == 450 or i == 600 or i == 750
    
    let multivariate_features = if is_multivariate_anomaly {
      [
        cpu_usage * 0.2,        // CPU异常低
        memory_usage * 3.0,     // 内存异常高
        disk_io * 0.1,          // 磁盘IO异常低
        network_io * 4.0,       // 网络IO异常高
        response_time * 5.0,    // 响应时间异常高
        cpu_usage + memory_usage // 异常组合
      ]
    } else {
      [
        cpu_usage,
        memory_usage,
        disk_io,
        network_io,
        response_time,
        cpu_usage + memory_usage  // 正常组合
      ]
    }
    
    multivariate_data.push((timestamp, multivariate_features, is_multivariate_anomaly))
    i = i + 1
  }
  
  // 验证多变量数据
  assert_eq(multivariate_data.length(), 800)
  
  // 模拟多变量异常检测
  let mut multivariate_detection_results = []
  
  for method in multivariate_methods {
    let method_name = method["method_name"]
    let dimensionality = method["dimensionality"]
    
    // 分割训练和测试数据
    let training_size = 600
    let training_data = multivariate_data.slice(0, training_size)
    let test_data = multivariate_data.slice(training_size, multivariate_data.length() - training_size)
    
    // 计算训练数据的协方差矩阵（简化）
    let mut correlation_matrix = []
    let mut d1 = 0
    while d1 < dimensionality {
      let mut row = []
      let mut d2 = 0
      while d2 < dimensionality {
        let correlation = if d1 == d2 {
          1.0
        } else if (d1 + d2) % 2 == 0 {
          0.7  // 高相关性
        } else {
          0.3  // 低相关性
        }
        row.push(correlation)
        d2 = d2 + 1
      }
      correlation_matrix.push(row)
      d1 = d1 + 1
    }
    
    // 模拟多变量异常检测
    let mut detected_anomalies = []
    let mut true_positives = 0
    let mut false_positives = 0
    let mut false_negatives = 0
    
    for sample in test_data {
      let features = sample.1
      let is_true_anomaly = sample.2
      
      // 计算异常分数
      let anomaly_score = match method_name {
        "mahalanobis_distance" => {
          let mut sum_squared = 0.0
          for f in features {
            sum_squared = sum_squared + f * f
          }
          sum_squared.sqrt() / 100.0
        }
        "principal_component_analysis" => {
          let mut reconstruction_error = 0.0
          for f in features {
            reconstruction_error = reconstruction_error + f.abs()
          }
          reconstruction_error / features.length().to_double()
        }
        "hotelling_t2" => {
          let mut t2_statistic = 0.0
          for f in features {
            t2_statistic = t2_statistic + f * f / 50.0
          }
          t2_statistic
        }
        "isolation_forest_mv" => {
          let mut isolation_score = 0.0
          for f in features {
            isolation_score = isolation_score + f
          }
          if isolation_score > 500.0 { 0.9 } else { 0.1 }
        }
        _ => 0.5
      }
      
      // 根据分数判断是否异常
      let threshold = match method_name {
        "mahalanobis_distance" => 3.0,
        "principal_component_analysis" => 2.0,
        "hotelling_t2" => 5.0,
        "isolation_forest_mv" => 0.5,
        _ => 0.5
      }
      
      let predicted_anomaly = anomaly_score > threshold
      
      if predicted_anomaly {
        detected_anomalies.push((anomaly_score, features))
        
        if is_true_anomaly {
          true_positives = true_positives + 1
        } else {
          false_positives = false_positives + 1
        }
      } else if is_true_anomaly {
        false_negatives = false_negatives + 1
      }
    }
    
    // 计算性能指标
    let precision = if true_positives + false_positives > 0 {
      true_positives.to_double() / (true_positives + false_positives).to_double()
    } else {
      0.0
    }
    
    let recall = if true_positives + false_negatives > 0 {
      true_positives.to_double() / (true_positives + false_negatives).to_double()
    } else {
      0.0
    }
    
    let f1_score = if precision + recall > 0.0 {
      2.0 * precision * recall / (precision + recall)
    } else {
      0.0
    }
    
    multivariate_detection_results.push((
      method_name,
      dimensionality,
      training_size,
      test_data.length(),
      detected_anomalies.length(),
      true_positives,
      false_positives,
      false_negatives,
      precision,
      recall,
      f1_score
    ))
  }
  
  // 验证多变量检测结果
  assert_eq(multivariate_detection_results.length(), 4)
  
  // 验证马氏距离方法
  let mahalanobis_result = multivariate_detection_results[0]
  assert_eq(mahalanobis_result.0, "mahalanobis_distance")
  assert_eq(mahalanobis_result.1, 5)  // 5维
  assert_eq(mahalanobis_result.2, 600)  // 训练样本数
  assert_eq(mahalanobis_result.5 >= 1, true)  // 应该检测到至少1个真实异常
  
  // 验证PCA方法
  let pca_result = multivariate_detection_results[1]
  assert_eq(pca_result.0, "principal_component_analysis")
  assert_eq(pca_result.1, 8)  // 8维
  assert_eq(pca_result.10 > 0.4, true)  // F1分数应该较好
  
  // 验证所有方法都有合理的性能
  for result in multivariate_detection_results {
    assert_eq(result.4 >= 0, true)  // 检测到的异常数应该非负
    assert_eq(result.5 >= 0, true)  // 真正例应该非负
    assert_eq(result.6 >= 0, true)  // 假正例应该非负
    assert_eq(result.7 >= 0, true)  // 假负例应该非负
    assert_eq(result.10 >= 0.0, true)  // F1分数应该非负
    assert_eq(result.10 <= 1.0, true)  // F1分数应该不超过1.0
  }
}