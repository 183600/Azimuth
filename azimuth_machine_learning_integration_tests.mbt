// Azimuth 机器学习集成测试用例
// 专注于遥测数据与机器学习模型的集成，包括异常检测、预测分析和智能告警

// 测试1: 遥测数据异常检测
test "基于机器学习的遥测数据异常检测" {
  // 定义异常检测算法
  enum AnomalyDetectionAlgorithm {
    StatisticalOutlier    // 统计离群点检测
    IsolationForest      // 孤立森林
    Autoencoder          // 自编码器
    LSTM                 // 长短期记忆网络
    ClusteringBased      // 基于聚类的检测
  }
  
  // 定义数据点
  type DataPoint = {
    timestamp: Int,
    features: Array[Float],
    labels: Array[String],
    metadata: Array[(String, String)]
  }
  
  // 定义异常检测结果
  type AnomalyResult = {
    is_anomaly: Bool,
    anomaly_score: Float,  // 0.0-1.0，越高越异常
    confidence: Float,     // 0.0-1.0，置信度
    algorithm: AnomalyDetectionAlgorithm,
    detected_at: Int,
    features_contributing: Array[(String, Float)]
  }
  
  // 定义异常检测模型
  type AnomalyDetectionModel = {
    id: String,
    algorithm: AnomalyDetectionAlgorithm,
    trained_at: Int,
    feature_names: Array[String],
    threshold: Float,
    accuracy: Float,
    precision: Float,
    recall: Float
  }
  
  // 统计离群点检测
  let statistical_outlier_detection = fn(data_points: Array[DataPoint], threshold: Float) {
    if data_points.length() < 3 {
      return []
    }
    
    let mut anomaly_results = []
    
    // 对每个特征进行检测
    for point in data_points {
      let mut feature_scores = []
      
      for i in 0..point.features.length() {
        // 提取所有数据点的第i个特征
        let feature_values = data_points.map(fn(p) { p.features[i] })
        
        // 计算均值和标准差
        let mean = feature_values.reduce(fn(acc, v) { acc + v }, 0.0) / (feature_values.length() as Float)
        let variance = feature_values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / (feature_values.length() as Float)
        let std_dev = Math::sqrt(variance)
        
        // 计算Z-score
        let z_score = if std_dev > 0.0 {
          Math::abs(point.features[i] - mean) / std_dev
        } else {
          0.0
        }
        
        // 转换为异常分数
        let anomaly_score = if z_score > threshold { 1.0 } else { z_score / threshold }
        feature_scores = feature_scores.push((point.labels[i], anomaly_score))
      }
      
      // 计算整体异常分数
      let overall_score = feature_scores.reduce(fn(acc, fs) { acc + fs.1 }, 0.0) / (feature_scores.length() as Float)
      
      let result = {
        is_anomaly: overall_score > 0.7,
        anomaly_score: overall_score,
        confidence: if overall_score > 0.5 { overall_score } else { 1.0 - overall_score },
        algorithm: AnomalyDetectionAlgorithm::StatisticalOutlier,
        detected_at: Time::now(),
        features_contributing: feature_scores.filter(fn(fs) { fs.1 > 0.5 })
      }
      
      anomaly_results = anomaly_results.push(result)
    }
    
    anomaly_results
  }
  
  // 孤立森林检测（简化版）
  let isolation_forest_detection = fn(data_points: Array[DataPoint], num_trees: Int, threshold: Float) {
    if data_points.length() < 2 {
      return []
    }
    
    let mut anomaly_results = []
    
    for point in data_points {
      // 简化的孤立森林实现
      // 在实际实现中，会构建多个随机树并计算平均路径长度
      let mut path_lengths = []
      
      for tree in 0..num_trees {
        // 简化的随机分割过程
        let mut current_data = data_points
        let mut path_length = 0
        let mut remaining_features = Array::range(0, point.features.length())
        
        while current_data.length() > 1 && remaining_features.length() > 0 {
          // 随机选择特征
          let feature_index = remaining_features[Time::now() % remaining_features.length()]
          remaining_features = remaining_features.filter(fn(i) { i != feature_index })
          
          // 计算分割点
          let feature_values = current_data.map(fn(p) { p.features[feature_index] })
          let min_val = feature_values.reduce(fn(acc, v) { if acc < v { acc } else { v } }, feature_values[0])
          let max_val = feature_values.reduce(fn(acc, v) { if acc > v { acc } else { v } }, feature_values[0])
          let split_point = min_val + (max_val - min_val) * 0.5
          
          // 分割数据
          current_data = current_data.filter(fn(p) { p.features[feature_index] <= split_point })
          path_length = path_length + 1
          
          // 如果当前点被分离，停止
          if current_data.length() == 1 && current_data[0].timestamp == point.timestamp {
            break
          }
        }
        
        path_lengths = path_lengths.push(path_length)
      }
      
      // 计算平均路径长度
      let avg_path_length = path_lengths.reduce(fn(acc, pl) { acc + pl }, 0) / (path_lengths.length() as Float)
      
      // 转换为异常分数（路径越短越异常）
      let max_path_length = Math::log(data_points.length() as Float, 2.0)
      let anomaly_score = if max_path_length > 0.0 {
        1.0 - (avg_path_length / max_path_length)
      } else {
        0.0
      }
      
      let feature_contributions = point.labels.map_with_index(fn(i, label) {
        let feature_value = point.features[i]
        let feature_values = data_points.map(fn(p) { p.features[i] })
        let mean = feature_values.reduce(fn(acc, v) { acc + v }, 0.0) / (feature_values.length() as Float)
        let deviation = Math::abs(feature_value - mean) / (mean + 0.001)  // 避免除零
        (label, deviation)
      })
      
      let result = {
        is_anomaly: anomaly_score > threshold,
        anomaly_score: anomaly_score,
        confidence: if anomaly_score > 0.5 { anomaly_score } else { 1.0 - anomaly_score },
        algorithm: AnomalyDetectionAlgorithm::IsolationForest,
        detected_at: Time::now(),
        features_contributing: feature_contributions.filter(fn(fc) { fc.1 > 0.5 })
      }
      
      anomaly_results = anomaly_results.push(result)
    }
    
    anomaly_results
  }
  
  // 基于聚类的异常检测
  let clustering_based_detection = fn(data_points: Array[DataPoint], num_clusters: Int, threshold: Float) {
    if data_points.length() < num_clusters {
      return []
    }
    
    // 简化的K-means聚类
    let mut centroids = Array::new(num_clusters, fn(i) {
      let initial_index = (i * data_points.length()) / num_clusters
      data_points[initial_index].features
    })
    
    // 迭代优化质心（简化版）
    for iteration in 0..10 {
      // 分配点到最近的质心
      let mut clusters = Array::new(num_clusters, fn(_) { [] })
      
      for point in data_points {
        let mut min_distance = Float::max_value()
        let mut closest_cluster = 0
        
        for i in 0..centroids.length() {
          let distance = point.features.map_with_index(fn(j, f) {
            let diff = f - centroids[i][j]
            diff * diff
          }).reduce(fn(acc, d) { acc + d }, 0.0)
          
          if distance < min_distance {
            min_distance = distance
            closest_cluster = i
          }
        }
        
        clusters[closest_cluster] = clusters[closest_cluster].push(point)
      }
      
      // 更新质心
      for i in 0..centroids.length() {
        if clusters[i].length() > 0 {
          let new_centroid = Array::new(point.features.length(), fn(j) {
            let sum = clusters[i].reduce(fn(acc, p) { acc + p.features[j] }, 0.0)
            sum / (clusters[i].length() as Float)
          })
          centroids[i] = new_centroid
        }
      }
    }
    
    // 计算异常分数
    let mut anomaly_results = []
    
    for point in data_points {
      // 找到最近的质心和距离
      let mut min_distance = Float::max_value()
      let mut closest_cluster = 0
      
      for i in 0..centroids.length() {
        let distance = point.features.map_with_index(fn(j, f) {
          let diff = f - centroids[i][j]
          diff * diff
        }).reduce(fn(acc, d) { acc + d }, 0.0)
        
        if distance < min_distance {
          min_distance = distance
          closest_cluster = i
        }
      }
      
      // 计算聚类内平均距离
      let cluster_distances = clusters[closest_cluster].map(fn(p) {
        p.features.map_with_index(fn(j, f) {
          let diff = f - centroids[closest_cluster][j]
          diff * diff
        }).reduce(fn(acc, d) { acc + d }, 0.0)
      })
      
      let avg_cluster_distance = cluster_distances.reduce(fn(acc, d) { acc + d }, 0.0) / (cluster_distances.length() as Float)
      
      // 转换为异常分数
      let anomaly_score = if avg_cluster_distance > 0.0 {
        min_distance / avg_cluster_distance
      } else {
        0.0
      }
      
      let feature_contributions = point.labels.map_with_index(fn(i, label) {
        let feature_value = point.features[i]
        let centroid_value = centroids[closest_cluster][i]
        let deviation = Math::abs(feature_value - centroid_value) / (Math::abs(centroid_value) + 0.001)
        (label, deviation)
      })
      
      let result = {
        is_anomaly: anomaly_score > threshold,
        anomaly_score: anomaly_score,
        confidence: if anomaly_score > 0.5 { anomaly_score } else { 1.0 - anomaly_score },
        algorithm: AnomalyDetectionAlgorithm::ClusteringBased,
        detected_at: Time::now(),
        features_contributing: feature_contributions.filter(fn(fc) { fc.1 > 0.5 })
      }
      
      anomaly_results = anomaly_results.push(result)
    }
    
    anomaly_results
  }
  
  // 创建测试数据
  let base_timestamp = 1640995200
  let normal_data = Array::new(20, fn(i) {
    {
      timestamp: base_timestamp + i * 60,
      features: [
        50.0 + Math::random() * 10.0,  // CPU使用率: 45-55
        1000.0 + Math::random() * 200.0,  // 内存使用: 900-1100MB
        100.0 + Math::random() * 20.0  // 响应时间: 90-110ms
      ],
      labels: ["cpu", "memory", "response_time"],
      metadata: [
        ("service", "api-service"),
        ("environment", "production")
      ]
    }
  })
  
  // 添加异常点
  let anomaly_data = [
    {
      timestamp: base_timestamp + 21 * 60,
      features: [95.0, 800.0, 500.0],  // CPU异常高，响应时间异常高
      labels: ["cpu", "memory", "response_time"],
      metadata: [
        ("service", "api-service"),
        ("environment", "production")
      ]
    },
    {
      timestamp: base_timestamp + 22 * 60,
      features: [10.0, 2500.0, 50.0],  // CPU异常低，内存异常高
      labels: ["cpu", "memory", "response_time"],
      metadata: [
        ("service", "api-service"),
        ("environment", "production")
      ]
    }
  ]
  
  let all_data = normal_data + anomaly_data
  
  // 测试统计离群点检测
  let statistical_results = statistical_outlier_detection(all_data, 2.0)
  assert_eq(statistical_results.length(), all_data.length())
  
  // 验证异常点被检测到
  let detected_anomalies = statistical_results.filter(fn(r) { r.is_anomaly })
  assert_true(detected_anomalies.length() >= 2)  // 至少检测到2个异常
  
  // 验证异常点的分数较高
  for i in 0..statistical_results.length() {
    if i >= 20 {  // 异常点在索引20和21
      assert_true(statistical_results[i].anomaly_score > 0.5)
      assert_true(statistical_results[i].is_anomaly)
    }
  }
  
  // 测试孤立森林检测
  let isolation_results = isolation_forest_detection(all_data, 10, 0.6)
  assert_eq(isolation_results.length(), all_data.length())
  
  let isolation_anomalies = isolation_results.filter(fn(r) { r.is_anomaly })
  assert_true(isolation_anomalies.length() >= 1)  // 至少检测到1个异常
  
  // 测试基于聚类的检测
  let clustering_results = clustering_based_detection(all_data, 3, 1.5)
  assert_eq(clustering_results.length(), all_data.length())
  
  let clustering_anomalies = clustering_results.filter(fn(r) { r.is_anomaly })
  assert_true(clustering_anomalies.length() >= 1)  // 至少检测到1个异常
  
  // 验证不同算法检测到不同的异常
  let statistical_anomaly_indices = statistical_results
    .map_with_index(fn(i, r) { if r.is_anomaly { Some(i) } else { None } })
    .filter(fn(i) { i.is_some() })
    .map(fn(i) { i.unwrap() })
  
  let isolation_anomaly_indices = isolation_results
    .map_with_index(fn(i, r) { if r.is_anomaly { Some(i) } else { None } })
    .filter(fn(i) { i.is_some() })
    .map(fn(i) { i.unwrap() })
  
  let clustering_anomaly_indices = clustering_results
    .map_with_index(fn(i, r) { if r.is_anomaly { Some(i) } else { None } })
    .filter(fn(i) { i.is_some() })
    .map(fn(i) { i.unwrap() })
  
  // 不同算法应该检测到不同的异常点
  assert_true(statistical_anomaly_indices != isolation_anomaly_indices ||
             statistical_anomaly_indices != clustering_anomaly_indices)
}

// 测试2: 遥测数据预测分析
test "遥测数据时序预测分析" {
  // 定义预测模型类型
  enum PredictionModelType {
    LinearRegression      // 线性回归
    ExponentialSmoothing  // 指数平滑
    ARIMA                 // 自回归积分移动平均
    LSTM                  // 长短期记忆网络
    Prophet               // Facebook Prophet模型
  }
  
  // 定义预测结果
  type PredictionResult = {
    predicted_values: Array[Float],
    confidence_intervals: Array[(Float, Float)],  // (下限, 上限)
    model_type: PredictionModelType,
    mse: Float,           // 均方误差
    mae: Float,           // 平均绝对误差
    r2_score: Float,      // R平方分数
    created_at: Int
  }
  
  // 定义时序数据
  type TimeSeriesData = {
    timestamps: Array[Int],
    values: Array[Float],
    metadata: Array[(String, String)]
  }
  
  // 线性回归预测
  let linear_regression_predict = fn(data: TimeSeriesData, forecast_steps: Int) {
    if data.timestamps.length() < 2 || data.values.length() < 2 {
      return {
        predicted_values: [],
        confidence_intervals: [],
        model_type: PredictionModelType::LinearRegression,
        mse: 0.0,
        mae: 0.0,
        r2_score: 0.0,
        created_at: Time::now()
      }
    }
    
    // 简单线性回归: y = ax + b
    let n = data.timestamps.length() as Float
    let sum_x = data.timestamps.reduce(fn(acc, t) { acc + t as Float }, 0.0)
    let sum_y = data.values.reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_xy = data.timestamps.map_with_index(fn(i, t) { t as Float * data.values[i] }).reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_x2 = data.timestamps.reduce(fn(acc, t) { acc + (t as Float) * (t as Float) }, 0.0)
    
    let denominator = n * sum_x2 - sum_x * sum_x
    let slope = if denominator != 0.0 { (n * sum_xy - sum_x * sum_y) / denominator } else { 0.0 }
    let intercept = if n != 0.0 { (sum_y - slope * sum_x) / n } else { 0.0 }
    
    // 预测未来值
    let last_timestamp = data.timestamps[data.timestamps.length() - 1]
    let predicted_values = Array::new(forecast_steps, fn(i) {
      let future_timestamp = (last_timestamp + (i + 1) * 60) as Float  // 假设每分钟一个数据点
      slope * future_timestamp + intercept
    })
    
    // 计算置信区间（简化）
    let residuals = data.values.map_with_index(fn(i, v) {
      let predicted = slope * (data.timestamps[i] as Float) + intercept
      v - predicted
    })
    
    let mse = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0) / (residuals.length() as Float)
    let std_error = Math::sqrt(mse)
    
    let confidence_intervals = predicted_values.map(fn(pv) {
      let margin = 1.96 * std_error  // 95%置信区间
      (pv - margin, pv + margin)
    })
    
    // 计算模型评估指标
    let predicted_train = data.timestamps.map(fn(t) { slope * (t as Float) + intercept })
    let mae = residuals.map(fn(r) { Math::abs(r) }).reduce(fn(acc, r) { acc + r }, 0.0) / (residuals.length() as Float)
    
    let ss_res = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0)
    let mean_y = sum_y / n
    let ss_tot = data.values.reduce(fn(acc, v) { acc + (v - mean_y) * (v - mean_y) }, 0.0)
    let r2_score = if ss_tot != 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    
    {
      predicted_values: predicted_values,
      confidence_intervals: confidence_intervals,
      model_type: PredictionModelType::LinearRegression,
      mse: mse,
      mae: mae,
      r2_score: r2_score,
      created_at: Time::now()
    }
  }
  
  // 指数平滑预测
  let exponential_smoothing_predict = fn(data: TimeSeriesData, forecast_steps: Int, alpha: Float) {
    if data.values.length() == 0 {
      return {
        predicted_values: [],
        confidence_intervals: [],
        model_type: PredictionModelType::ExponentialSmoothing,
        mse: 0.0,
        mae: 0.0,
        r2_score: 0.0,
        created_at: Time::now()
      }
    }
    
    // 简单指数平滑
    let mut smoothed_values = []
    let mut current_smooth = data.values[0]
    
    smoothed_values = smoothed_values.push(current_smooth)
    
    for i in 1..data.values.length() {
      current_smooth = alpha * data.values[i] + (1.0 - alpha) * current_smooth
      smoothed_values = smoothed_values.push(current_smooth)
    }
    
    // 预测未来值（使用最后一个平滑值）
    let last_smoothed = smoothed_values[smoothed_values.length() - 1]
    let predicted_values = Array::new(forecast_steps, fn(_) { last_smoothed })
    
    // 计算置信区间（简化）
    let residuals = data.values.map_with_index(fn(i, v) { v - smoothed_values[i] })
    let mse = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0) / (residuals.length() as Float)
    let std_error = Math::sqrt(mse)
    
    let confidence_intervals = predicted_values.map(fn(pv) {
      let margin = 1.96 * std_error
      (pv - margin, pv + margin)
    })
    
    // 计算模型评估指标
    let mae = residuals.map(fn(r) { Math::abs(r) }).reduce(fn(acc, r) { acc + r }, 0.0) / (residuals.length() as Float)
    
    let mean_y = data.values.reduce(fn(acc, v) { acc + v }, 0.0) / (data.values.length() as Float)
    let ss_res = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0)
    let ss_tot = data.values.reduce(fn(acc, v) { acc + (v - mean_y) * (v - mean_y) }, 0.0)
    let r2_score = if ss_tot != 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    
    {
      predicted_values: predicted_values,
      confidence_intervals: confidence_intervals,
      model_type: PredictionModelType::ExponentialSmoothing,
      mse: mse,
      mae: mae,
      r2_score: r2_score,
      created_at: Time::now()
    }
  }
  
  // 移动平均预测
  let moving_average_predict = fn(data: TimeSeriesData, forecast_steps: Int, window_size: Int) {
    if data.values.length() < window_size {
      return {
        predicted_values: [],
        confidence_intervals: [],
        model_type: PredictionModelType::ARIMA,
        mse: 0.0,
        mae: 0.0,
        r2_score: 0.0,
        created_at: Time::now()
      }
    }
    
    // 计算移动平均
    let mut moving_averages = []
    
    for i in window_size..data.values.length() {
      let window = data.values.slice(i - window_size, i)
      let avg = window.reduce(fn(acc, v) { acc + v }, 0.0) / (window.length() as Float)
      moving_averages = moving_averages.push(avg)
    }
    
    // 预测未来值（使用最后一个移动平均值）
    let last_ma = if moving_averages.length() > 0 {
      moving_averages[moving_averages.length() - 1]
    } else {
      data.values.reduce(fn(acc, v) { acc + v }, 0.0) / (data.values.length() as Float)
    }
    
    let predicted_values = Array::new(forecast_steps, fn(_) { last_ma })
    
    // 计算置信区间（简化）
    let residuals = data.values.map_with_index(fn(i, v) {
      if i >= window_size - 1 {
        v - moving_averages[i - (window_size - 1)]
      } else {
        0.0
      }
    }).slice(window_size - 1, data.values.length())
    
    let mse = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0) / (residuals.length() as Float)
    let std_error = Math::sqrt(mse)
    
    let confidence_intervals = predicted_values.map(fn(pv) {
      let margin = 1.96 * std_error
      (pv - margin, pv + margin)
    })
    
    // 计算模型评估指标
    let mae = residuals.map(fn(r) { Math::abs(r) }).reduce(fn(acc, r) { acc + r }, 0.0) / (residuals.length() as Float)
    
    let mean_y = data.values.reduce(fn(acc, v) { acc + v }, 0.0) / (data.values.length() as Float)
    let ss_res = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0)
    let ss_tot = data.values.reduce(fn(acc, v) { acc + (v - mean_y) * (v - mean_y) }, 0.0)
    let r2_score = if ss_tot != 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    
    {
      predicted_values: predicted_values,
      confidence_intervals: confidence_intervals,
      model_type: PredictionModelType::ARIMA,
      mse: mse,
      mae: mae,
      r2_score: r2_score,
      created_at: Time::now()
    }
  }
  
  // 创建测试数据（带有趋势和季节性）
  let base_timestamp = 1640995200
  let timestamps = Array::new(24, fn(i) { base_timestamp + i * 3600 })  // 每小时一个数据点，共24小时
  
  // 生成带有趋势（每小时增加0.5）和季节性（正弦波）的数据
  let values = Array::new(24, fn(i) {
    let trend = i as Float * 0.5
    let seasonal = 10.0 * Math::sin(2.0 * 3.14159 * i as Float / 12.0)  // 12小时周期
    let noise = (Math::random() - 0.5) * 5.0  // 随机噪声
    50.0 + trend + seasonal + noise
  })
  
  let time_series_data = {
    timestamps: timestamps,
    values: values,
    metadata: [
      ("metric", "cpu_usage"),
      ("service", "api-service"),
      ("unit", "percent")
    ]
  }
  
  // 测试线性回归预测
  let linear_result = linear_regression_predict(time_series_data, 6)
  assert_eq(linear_result.predicted_values.length(), 6)
  assert_eq(linear_result.confidence_intervals.length(), 6)
  assert_eq(linear_result.model_type, PredictionModelType::LinearRegression)
  
  // 验证预测值是递增的（因为有趋势）
  for i in 1..linear_result.predicted_values.length() {
    assert_true(linear_result.predicted_values[i] >= linear_result.predicted_values[i-1])
  }
  
  // 验证置信区间
  for i in 0..linear_result.confidence_intervals.length() {
    let (lower, upper) = linear_result.confidence_intervals[i]
    assert_true(lower <= linear_result.predicted_values[i])
    assert_true(upper >= linear_result.predicted_values[i])
  }
  
  // 测试指数平滑预测
  let exp_result = exponential_smoothing_predict(time_series_data, 6, 0.3)
  assert_eq(exp_result.predicted_values.length(), 6)
  assert_eq(exp_result.model_type, PredictionModelType::ExponentialSmoothing)
  
  // 指数平滑预测应该接近最近的值
  let last_actual = values[values.length() - 1]
  let first_predicted = exp_result.predicted_values[0]
  assert_true(Math::abs(first_predicted - last_actual) < 10.0)
  
  // 测试移动平均预测
  let ma_result = moving_average_predict(time_series_data, 6, 6)
  assert_eq(ma_result.predicted_values.length(), 6)
  assert_eq(ma_result.model_type, PredictionModelType::ARIMA)
  
  // 移动平均预测应该是一个常数
  for i in 1..ma_result.predicted_values.length() {
    assert_eq(ma_result.predicted_values[i], ma_result.predicted_values[0])
  }
  
  // 比较不同模型的性能
  let models = [linear_result, exp_result, ma_result]
  let best_model_by_mse = models.reduce(fn(best, current) {
    if current.mse < best.mse { current } else { best }
  }, models[0])
  
  let best_model_by_mae = models.reduce(fn(best, current) {
    if current.mae < best.mae { current } else { best }
  }, models[0])
  
  let best_model_by_r2 = models.reduce(fn(best, current) {
    if current.r2_score > best.r2_score { current } else { best }
  }, models[0])
  
  // 验证最佳模型的选择
  for model in models {
    assert_true(model.mse <= best_model_by_mse.mse || model == best_model_by_mse)
    assert_true(model.mae <= best_model_by_mae.mae || model == best_model_by_mae)
    assert_true(model.r2_score <= best_model_by_r2.r2_score || model == best_model_by_r2)
  }
  
  // 验证模型指标的合理性
  for model in models {
    assert_true(model.mse >= 0.0)
    assert_true(model.mae >= 0.0)
    assert_true(model.r2_score <= 1.0)
  }
}

// 测试3: 智能告警和根因分析
test "智能告警和根因分析系统" {
  // 定义告警级别
  enum AlertSeverity {
    Info
    Warning
    Error
    Critical
  }
  
  // 定义告警状态
  enum AlertStatus {
    Open
    Acknowledged
    Resolved
    Suppressed
  }
  
  // 定义告警规则
  type AlertRule = {
    id: String,
    name: String,
    description: String,
    condition: String,  // 简化的条件表达式
    severity: AlertSeverity,
    threshold: Float,
    duration_minutes: Int,
    enabled: Bool
  }
  
  // 定义告警
  type Alert = {
    id: String,
    rule_id: String,
    severity: AlertSeverity,
    status: AlertStatus,
    message: String,
    source: String,
    triggered_at: Int,
    acknowledged_at: Option[Int>,
    resolved_at: Option[Int],
    metadata: Array[(String, String)]
  }
  
  // 定义根因分析结果
  type RootCauseAnalysis = {
    alert_id: String,
    likely_causes: Array[(String, Float)],  // (原因, 置信度)
    correlated_metrics: Array[String],
    correlation_scores: Array[(String, Float)],
    analysis_timestamp: Int,
    confidence: Float
  }
  
  // 定义相关性分析
  type CorrelationResult = {
    metric_a: String,
    metric_b: String,
    correlation_coefficient: Float,
    p_value: Float,
    time_lag: Int  // 时间延迟（秒）
  }
  
  // 评估告警规则
  let evaluate_alert_rule = fn(rule: AlertRule, metrics: Array[(String, Float)]) {
    if not(rule.enabled) {
      return None
    }
    
    // 简化的条件评估
    let condition_met = match rule.condition {
      "cpu > threshold" => {
        match metrics.find(fn(m) { m.0 == "cpu" }) {
          Some((_, value)) => value > rule.threshold
          None => false
        }
      }
      "memory > threshold" => {
        match metrics.find(fn(m) { m.0 == "memory" }) {
          Some((_, value)) => value > rule.threshold
          None => false
        }
      }
      "response_time > threshold" => {
        match metrics.find(fn(m) { m.0 == "response_time" }) {
          Some((_, value)) => value > rule.threshold
          None => false
        }
      }
      "error_rate > threshold" => {
        match metrics.find(fn(m) { m.0 == "error_rate" }) {
          Some((_, value)) => value > rule.threshold
          None => false
        }
      }
      _ => false
    }
    
    if condition_met {
      let alert_id = "alert-" + Time::now().to_string()
      let message = rule.name + ": " + rule.description + " (当前值: " + 
                   match metrics.find(fn(m) { rule.condition.contains(m.0) }) {
                     Some((name, value)) => name + " = " + value.to_string()
                     None => "未知"
                   } + ")"
      
      Some({
        id: alert_id,
        rule_id: rule.id,
        severity: rule.severity,
        status: AlertStatus::Open,
        message: message,
        source: "telemetry-system",
        triggered_at: Time::now(),
        acknowledged_at: None,
        resolved_at: None,
        metadata: [
          ("threshold", rule.threshold.to_string()),
          ("condition", rule.condition)
        ]
      })
    } else {
      None
    }
  }
  
  // 计算皮尔逊相关系数
  let calculate_correlation = fn(series_a: Array[Float], series_b: Array[Float]) {
    if series_a.length() != series_b.length() || series_a.length() < 2 {
      return 0.0
    }
    
    let n = series_a.length() as Float
    let sum_a = series_a.reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_b = series_b.reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_ab = series_a.map_with_index(fn(i, v) { v * series_b[i] }).reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_a2 = series_a.reduce(fn(acc, v) { acc + v * v }, 0.0)
    let sum_b2 = series_b.reduce(fn(acc, v) { acc + v * v }, 0.0)
    
    let numerator = n * sum_ab - sum_a * sum_b
    let denominator = Math::sqrt((n * sum_a2 - sum_a * sum_a) * (n * sum_b2 - sum_b * sum_b))
    
    if denominator == 0.0 {
      0.0
    } else {
      numerator / denominator
    }
  }
  
  // 计算时间延迟相关性
  let calculate_time_lag_correlation = fn(series_a: Array[Float], series_b: Array[Float], max_lag: Int) {
    if series_a.length() != series_b.length() || series_a.length() <= max_lag {
      return {
        metric_a: "",
        metric_b: "",
        correlation_coefficient: 0.0,
        p_value: 1.0,
        time_lag: 0
      }
    }
    
    let mut best_correlation = 0.0
    let mut best_lag = 0
    
    for lag in 0..max_lag {
      let truncated_a = series_a.slice(0, series_a.length() - lag)
      let truncated_b = series_b.slice(lag, series_b.length())
      
      let correlation = calculate_correlation(truncated_a, truncated_b)
      
      if Math::abs(correlation) > Math::abs(best_correlation) {
        best_correlation = correlation
        best_lag = lag
      }
    }
    
    {
      metric_a: "",
      metric_b: "",
      correlation_coefficient: best_correlation,
      p_value: 0.05,  // 简化的p值
      time_lag: best_lag
    }
  }
  
  // 执行根因分析
  let perform_root_cause_analysis = fn(alert: Alert, metrics_history: Array[(String, Array[Float])]) {
    // 提取告警时间前的指标历史
    let alert_time = alert.triggered_at
    let analysis_window = 3600  // 1小时
    
    // 简化的根因分析：找出与告警相关的指标
    let alert_metric = match alert.metadata.find(fn(m) { m.0 == "condition" }) {
      Some((_, condition)) => {
        if condition.contains("cpu") { "cpu" }
        else if condition.contains("memory") { "memory" }
        else if condition.contains("response_time") { "response_time" }
        else if condition.contains("error_rate") { "error_rate" }
        else { "unknown" }
      }
      None => "unknown"
    }
    
    // 计算相关性
    let mut correlations = []
    
    match metrics_history.find(fn(mh) { mh.0 == alert_metric }) {
      Some((_, alert_series)) => {
        for (metric_name, metric_series) in metrics_history {
          if metric_name != alert_metric {
            let correlation_result = calculate_time_lag_correlation(alert_series, metric_series, 10)
            let correlation_with_names = {
              metric_a: alert_metric,
              metric_b: metric_name,
              correlation_coefficient: correlation_result.correlation_coefficient,
              p_value: correlation_result.p_value,
              time_lag: correlation_result.time_lag
            }
            correlations = correlations.push(correlation_with_names)
          }
        }
      }
      None => {}
    }
    
    // 按相关性排序
    let sorted_correlations = correlations.sort(fn(a, b) {
      let abs_a = Math::abs(a.correlation_coefficient)
      let abs_b = Math::abs(b.correlation_coefficient)
      if abs_a > abs_b { -1 } else if abs_a < abs_b { 1 } else { 0 }
    })
    
    // 提取最相关的指标
    let top_correlations = sorted_correlations.slice(0, 5)
    let correlated_metrics = top_correlations.map(fn(c) { c.metric_b })
    let correlation_scores = top_correlations.map(fn(c) { (c.metric_b, Math::abs(c.correlation_coefficient)) })
    
    // 生成可能的原因
    let likely_causes = top_correlations.map_with_index(fn(i, c) {
      let cause = match c.metric_b {
        "cpu" => "CPU使用率异常"
        "memory" => "内存使用率异常"
        "response_time" => "响应时间异常"
        "error_rate" => "错误率异常"
        "network_io" => "网络I/O异常"
        "disk_io" => "磁盘I/O异常"
        _ => "指标 " + c.metric_b + " 异常"
      }
      
      let confidence = Math::abs(c.correlation_coefficient)
      (cause, confidence)
    })
    
    // 计算整体置信度
    let confidence = if likely_causes.length() > 0 {
      likely_causes.reduce(fn(acc, cause) { acc + cause.1 }, 0.0) / (likely_causes.length() as Float)
    } else {
      0.0
    }
    
    {
      alert_id: alert.id,
      likely_causes: likely_causes,
      correlated_metrics: correlated_metrics,
      correlation_scores: correlation_scores,
      analysis_timestamp: Time::now(),
      confidence: confidence
    }
  }
  
  // 创建告警规则
  let alert_rules = [
    {
      id: "rule-001",
      name: "高CPU使用率",
      description: "CPU使用率超过阈值",
      condition: "cpu > threshold",
      severity: AlertSeverity::Warning,
      threshold: 80.0,
      duration_minutes: 5,
      enabled: true
    },
    {
      id: "rule-002",
      name: "高内存使用率",
      description: "内存使用率超过阈值",
      condition: "memory > threshold",
      severity: AlertSeverity::Warning,
      threshold: 85.0,
      duration_minutes: 5,
      enabled: true
    },
    {
      id: "rule-003",
      name: "高响应时间",
      description: "响应时间超过阈值",
      condition: "response_time > threshold",
      severity: AlertSeverity::Error,
      threshold: 500.0,
      duration_minutes: 2,
      enabled: true
    },
    {
      id: "rule-004",
      name: "高错误率",
      description: "错误率超过阈值",
      condition: "error_rate > threshold",
      severity: AlertSeverity::Critical,
      threshold: 5.0,
      duration_minutes: 1,
      enabled: true
    }
  ]
  
  // 创建测试指标
  let test_metrics = [
    ("cpu", 85.5),        // 触发规则1
    ("memory", 60.0),     // 不触发
    ("response_time", 450.0),  // 不触发
    ("error_rate", 2.0)    // 不触发
  ]
  
  // 评估告警规则
  let triggered_alerts = alert_rules.map(fn(rule) {
    evaluate_alert_rule(rule, test_metrics)
  }).filter(fn(alert) { alert.is_some() }).map(fn(alert) { alert.unwrap() })
  
  assert_eq(triggered_alerts.length(), 1)  // 只有一个规则触发
  assert_eq(triggered_alerts[0].rule_id, "rule-001")
  assert_eq(triggered_alerts[0].severity, AlertSeverity::Warning)
  
  // 创建指标历史数据
  let metrics_history = [
    ("cpu", Array::new(60, fn(i) { 50.0 + i as Float * 0.5 + (Math::random() - 0.5) * 5.0 })),
    ("memory", Array::new(60, fn(i) { 60.0 + (Math::random() - 0.5) * 10.0 })),
    ("response_time", Array::new(60, fn(i) { 200.0 + (Math::random() - 0.5) * 50.0 })),
    ("error_rate", Array::new(60, fn(i) { 1.0 + (Math::random() - 0.5) * 2.0 })),
    ("network_io", Array::new(60, fn(i) { 100.0 + i as Float * 2.0 + (Math::random() - 0.5) * 20.0 })),
    ("disk_io", Array::new(60, fn(i) { 50.0 + (Math::random() - 0.5) * 15.0 }))
  ]
  
  // 执行根因分析
  let root_cause_analysis = perform_root_cause_analysis(triggered_alerts[0], metrics_history)
  
  assert_eq(root_cause_analysis.alert_id, triggered_alerts[0].id)
  assert_true(root_cause_analysis.likely_causes.length() > 0)
  assert_true(root_cause_analysis.correlated_metrics.length() > 0)
  assert_true(root_cause_analysis.confidence >= 0.0 && root_cause_analysis.confidence <= 1.0)
  
  // 验证相关性分数
  for (_, score) in root_cause_analysis.correlation_scores {
    assert_true(score >= 0.0 && score <= 1.0)
  }
  
  // 验证可能的原因按置信度排序
  for i in 1..root_cause_analysis.likely_causes.length() {
    assert_true(root_cause_analysis.likely_causes[i-1].1 >= root_cause_analysis.likely_causes[i].1)
  }
  
  // 测试多个告警的根因分析
  let multi_test_metrics = [
    ("cpu", 90.0),
    ("memory", 90.0),
    ("response_time", 600.0),
    ("error_rate", 6.0)
  ]
  
  let multi_triggered_alerts = alert_rules.map(fn(rule) {
    evaluate_alert_rule(rule, multi_test_metrics)
  }).filter(fn(alert) { alert.is_some() }).map(fn(alert) { alert.unwrap() })
  
  assert_eq(multi_triggered_alerts.length(), 4)  // 所有规则都触发
  
  // 为每个告警执行根因分析
  let multi_root_cause_analyses = multi_triggered_alerts.map(fn(alert) {
    perform_root_cause_analysis(alert, metrics_history)
  })
  
  assert_eq(multi_root_cause_analyses.length(), 4)
  
  // 验证每个分析都有结果
  for analysis in multi_root_cause_analyses {
    assert_true(analysis.likely_causes.length() > 0)
    assert_true(analysis.confidence >= 0.0 && analysis.confidence <= 1.0)
  }
}

// 测试4: 模型训练和评估
test "机器学习模型训练和评估" {
  // 定义模型类型
  enum MLModelType {
    Classification
    Regression
    Clustering
    AnomalyDetection
    TimeSeries
  }
  
  // 定义训练状态
  enum TrainingStatus {
    NotStarted
    InProgress
    Completed
    Failed
  }
  
  // 定义数据集
  type Dataset = {
    id: String,
    name: String,
    features: Array[Array[Float]],  // 特征矩阵
    labels: Option[Array[Float]>,   // 标签（可选，用于无监督学习）
    feature_names: Array[String],
    description: String,
    created_at: Int
  }
  
  // 定义模型
  type MLModel = {
    id: String,
    name: String,
    model_type: MLModelType,
    algorithm: String,
    parameters: Array[(String, String)],
    training_status: TrainingStatus,
    trained_at: Option[Int>,
    metrics: Option[ModelMetrics],
    dataset_id: String
  }
  
  // 定义模型评估指标
  type ModelMetrics = {
    accuracy: Option<Float>,      // 分类准确率
    precision: Option<Float>,     // 精确率
    recall: Option<Float],        // 召回率
    f1_score: Option<Float],      // F1分数
    mse: Option<Float],           // 均方误差（回归）
    mae: Option<Float],           // 平均绝对误差（回归）
    r2_score: Option<Float],      // R平方（回归）
    silhouette_score: Option<Float>, // 轮廓系数（聚类）
    training_time_ms: Int,
    inference_time_ms: Float
  }
  
  // 简化的K-means聚类
  let train_kmeans = fn(dataset: Dataset, k: Int, max_iterations: Int) {
    if dataset.features.length() < k {
      return {
        status: TrainingStatus::Failed,
        metrics: None,
        model: None
      }
    }
    
    let start_time = Time::now()
    
    // 初始化质心
    let mut centroids = Array::new(k, fn(i) {
      let initial_index = (i * dataset.features.length()) / k
      dataset.features[initial_index]
    })
    
    // K-means迭代
    for iteration in 0..max_iterations {
      // 分配点到最近的质心
      let mut clusters = Array::new(k, fn(_) { [] })
      
      for point in dataset.features {
        let mut min_distance = Float::max_value()
        let mut closest_cluster = 0
        
        for i in 0..centroids.length() {
          let distance = point.map_with_index(fn(j, f) {
            let diff = f - centroids[i][j]
            diff * diff
          }).reduce(fn(acc, d) { acc + d }, 0.0)
          
          if distance < min_distance {
            min_distance = distance
            closest_cluster = i
          }
        }
        
        clusters[closest_cluster] = clusters[closest_cluster].push(point)
      }
      
      // 更新质心
      let mut new_centroids = []
      let mut converged = true
      
      for i in 0..centroids.length() {
        if clusters[i].length() > 0 {
          let new_centroid = Array::new(point.features.length(), fn(j) {
            let sum = clusters[i].reduce(fn(acc, p) { acc + p[j] }, 0.0)
            sum / (clusters[i].length() as Float)
          })
          
          // 检查收敛
          let centroid_distance = new_centroid.map_with_index(fn(j, c) {
            let diff = c - centroids[i][j]
            diff * diff
          }).reduce(fn(acc, d) { acc + d }, 0.0)
          
          if centroid_distance > 0.001 {  // 收敛阈值
            converged = false
          }
          
          new_centroids = new_centroids.push(new_centroid)
        } else {
          new_centroids = new_centroids.push(centroids[i])
        }
      }
      
      centroids = new_centroids
      
      if converged {
        break
      }
    }
    
    let end_time = Time::now()
    let training_time = end_time - start_time
    
    // 计算轮廓系数（简化版）
    let mut silhouette_sum = 0.0
    let mut point_count = 0
    
    for point in dataset.features {
      // 找到最近的质心
      let mut min_distance = Float::max_value()
      let mut closest_cluster = 0
      
      for i in 0..centroids.length() {
        let distance = point.map_with_index(fn(j, f) {
          let diff = f - centroids[i][j]
          diff * diff
        }).reduce(fn(acc, d) { acc + d }, 0.0)
        
        if distance < min_distance {
          min_distance = distance
          closest_cluster = i
        }
      }
      
      // 找到第二近的质心
      let mut second_min_distance = Float::max_value()
      
      for i in 0..centroids.length() {
        if i != closest_cluster {
          let distance = point.map_with_index(fn(j, f) {
            let diff = f - centroids[i][j]
            diff * diff
          }).reduce(fn(acc, d) { acc + d }, 0.0)
          
          if distance < second_min_distance {
            second_min_distance = distance
          }
        }
      }
      
      // 计算轮廓系数
      let silhouette = if second_min_distance > 0.0 {
        (second_min_distance - min_distance) / second_min_distance
      } else {
        0.0
      }
      
      silhouette_sum = silhouette_sum + silhouette
      point_count = point_count + 1
    }
    
    let silhouette_score = if point_count > 0 {
      silhouette_sum / (point_count as Float)
    } else {
      0.0
    }
    
    // 测试推理时间
    let inference_start = Time::now()
    for _ in 0..100 {
      // 模拟推理过程
      let test_point = dataset.features[0]
      let mut min_distance = Float::max_value()
      
      for centroid in centroids {
        let distance = test_point.map_with_index(fn(j, f) {
          let diff = f - centroid[j]
          diff * diff
        }).reduce(fn(acc, d) { acc + d }, 0.0)
        
        if distance < min_distance {
          min_distance = distance
        }
      }
    }
    let inference_end = Time::now()
    let inference_time = (inference_end - inference_start) as Float / 100.0
    
    let metrics = {
      accuracy: None,
      precision: None,
      recall: None,
      f1_score: None,
      mse: None,
      mae: None,
      r2_score: None,
      silhouette_score: Some(silhouette_score),
      training_time_ms: training_time,
      inference_time_ms: inference_time
    }
    
    {
      status: TrainingStatus::Completed,
      metrics: Some(metrics),
      model: Some({
        id: "model-kmeans-" + Time::now().to_string(),
        name: "K-means Clustering",
        model_type: MLModelType::Clustering,
        algorithm: "K-means",
        parameters: [
          ("k", k.to_string()),
          ("max_iterations", max_iterations.to_string())
        ],
        training_status: TrainingStatus::Completed,
        trained_at: Some(end_time),
        metrics: Some(metrics),
        dataset_id: dataset.id
      })
    }
  }
  
  // 简化的线性回归训练
  let train_linear_regression = fn(dataset: Dataset) {
    if dataset.features.length() < 2 || dataset.labels.is_none() {
      return {
        status: TrainingStatus::Failed,
        metrics: None,
        model: None
      }
    }
    
    let start_time = Time::now()
    let labels = dataset.labels.unwrap()
    
    if dataset.features[0].length() != 1 || labels.length() != dataset.features.length() {
      return {
        status: TrainingStatus::Failed,
        metrics: None,
        model: None
      }
    }
    
    // 简单线性回归: y = ax + b
    let n = dataset.features.length() as Float
    let sum_x = dataset.features.reduce(fn(acc, f) { acc + f[0] }, 0.0)
    let sum_y = labels.reduce(fn(acc, l) { acc + l }, 0.0)
    let sum_xy = dataset.features.map_with_index(fn(i, f) { f[0] * labels[i] }).reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_x2 = dataset.features.reduce(fn(acc, f) { acc + f[0] * f[0] }, 0.0)
    
    let denominator = n * sum_x2 - sum_x * sum_x
    let slope = if denominator != 0.0 { (n * sum_xy - sum_x * sum_y) / denominator } else { 0.0 }
    let intercept = if n != 0.0 { (sum_y - slope * sum_x) / n } else { 0.0 }
    
    let end_time = Time::now()
    let training_time = end_time - start_time
    
    // 计算预测值
    let predictions = dataset.features.map(fn(f) { slope * f[0] + intercept })
    
    // 计算评估指标
    let residuals = predictions.map_with_index(fn(i, p) { labels[i] - p })
    let mse = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0) / (residuals.length() as Float)
    let mae = residuals.map(fn(r) { Math::abs(r) }).reduce(fn(acc, r) { acc + r }, 0.0) / (residuals.length() as Float)
    
    let mean_y = sum_y / n
    let ss_res = residuals.reduce(fn(acc, r) { acc + r * r }, 0.0)
    let ss_tot = labels.reduce(fn(acc, y) { acc + (y - mean_y) * (y - mean_y) }, 0.0)
    let r2_score = if ss_tot != 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    
    // 测试推理时间
    let inference_start = Time::now()
    for _ in 0..100 {
      let test_x = dataset.features[0][0]
      let _prediction = slope * test_x + intercept
    }
    let inference_end = Time::now()
    let inference_time = (inference_end - inference_start) as Float / 100.0
    
    let metrics = {
      accuracy: None,
      precision: None,
      recall: None,
      f1_score: None,
      mse: Some(mse),
      mae: Some(mae),
      r2_score: Some(r2_score),
      silhouette_score: None,
      training_time_ms: training_time,
      inference_time_ms: inference_time
    }
    
    {
      status: TrainingStatus::Completed,
      metrics: Some(metrics),
      model: Some({
        id: "model-linear-regression-" + Time::now().to_string(),
        name: "Linear Regression",
        model_type: MLModelType::Regression,
        algorithm: "Ordinary Least Squares",
        parameters: [
          ("slope", slope.to_string()),
          ("intercept", intercept.to_string())
        ],
        training_status: TrainingStatus::Completed,
        trained_at: Some(end_time),
        metrics: Some(metrics),
        dataset_id: dataset.id
      })
    }
  }
  
  // 创建聚类数据集
  let cluster_dataset = {
    id: "dataset-cluster-001",
    name: "服务性能聚类数据",
    features: Array::new(100, fn(i) {
      let cluster = i / 33  // 3个聚类
      let base_x = (cluster as Float) * 10.0
      let base_y = (cluster as Float) * 10.0
      [
        base_x + (Math::random() - 0.5) * 5.0,
        base_y + (Math::random() - 0.5) * 5.0
      ]
    }),
    labels: None,
    feature_names: ["cpu_usage", "memory_usage"],
    description: "服务性能指标数据，用于聚类分析",
    created_at: Time::now()
  }
  
  // 创建回归数据集
  let regression_dataset = {
    id: "dataset-regression-001",
    name: "CPU与响应时间关系",
    features: Array::new(50, fn(i) {
      let cpu = 20.0 + i as Float * 1.5 + (Math::random() - 0.5) * 5.0
      [cpu]
    }),
    labels: Some(Array::new(50, fn(i) {
      let cpu = 20.0 + i as Float * 1.5
      50.0 + cpu * 2.0 + (Math::random() - 0.5) * 20.0
    })),
    feature_names: ["cpu_usage"],
    description: "CPU使用率与响应时间的关系数据",
    created_at: Time::now()
  }
  
  // 训练K-means模型
  let kmeans_result = train_kmeans(cluster_dataset, 3, 100)
  assert_eq(kmeans_result.status, TrainingStatus::Completed)
  assert_true(kmeans_result.metrics.is_some())
  assert_true(kmeans_result.model.is_some())
  
  let kmeans_metrics = kmeans_result.metrics.unwrap()
  let kmeans_model = kmeans_result.model.unwrap()
  
  assert_true(kmeans_metrics.silhouette_score.is_some())
  assert_true(kmeans_metrics.silhouette_score.unwrap() >= -1.0 && kmeans_metrics.silhouette_score.unwrap() <= 1.0)
  assert_true(kmeans_metrics.training_time_ms > 0)
  assert_true(kmeans_metrics.inference_time_ms > 0.0)
  
  assert_eq(kmeans_model.model_type, MLModelType::Clustering)
  assert_eq(kmeans_model.algorithm, "K-means")
  assert_eq(kmeans_model.training_status, TrainingStatus::Completed)
  assert_true(kmeans_model.trained_at.is_some())
  
  // 训练线性回归模型
  let regression_result = train_linear_regression(regression_dataset)
  assert_eq(regression_result.status, TrainingStatus::Completed)
  assert_true(regression_result.metrics.is_some())
  assert_true(regression_result.model.is_some())
  
  let regression_metrics = regression_result.metrics.unwrap()
  let regression_model = regression_result.model.unwrap()
  
  assert_true(regression_metrics.mse.is_some())
  assert_true(regression_metrics.mae.is_some())
  assert_true(regression_metrics.r2_score.is_some())
  assert_true(regression_metrics.mse.unwrap() >= 0.0)
  assert_true(regression_metrics.mae.unwrap() >= 0.0)
  assert_true(regression_metrics.r2_score.unwrap() <= 1.0)
  assert_true(regression_metrics.training_time_ms > 0)
  assert_true(regression_metrics.inference_time_ms > 0.0)
  
  assert_eq(regression_model.model_type, MLModelType::Regression)
  assert_eq(regression_model.algorithm, "Ordinary Least Squares")
  assert_eq(regression_model.training_status, TrainingStatus::Completed)
  assert_true(regression_model.trained_at.is_some())
  
  // 比较模型性能
  let models = [kmeans_model, regression_model]
  
  // 验证模型ID唯一性
  let model_ids = models.map(fn(m) { m.id })
  let unique_ids = model_ids.unique()
  assert_eq(model_ids.length(), unique_ids.length())
  
  // 验证训练时间合理性
  for model in models {
    match model.metrics {
      Some(metrics) => {
        assert_true(metrics.training_time_ms > 0)
        assert_true(metrics.inference_time_ms > 0.0)
      }
      None => assert_true(false)
    }
  }
  
  // 测试模型参数
  assert_eq(kmeans_model.parameters.length(), 2)
  assert_eq(regression_model.parameters.length(), 2)
  
  // 验证参数值
  let k_param = kmeans_model.parameters.find(fn(p) { p.0 == "k" })
  match k_param {
    Some((_, value)) => assert_eq(value, "3")
    None => assert_true(false)
  }
  
  let slope_param = regression_model.parameters.find(fn(p) { p.0 == "slope" })
  match slope_param {
    Some((_, value)) => {
      let slope_value = value.to_float()
      assert_true(slope_value > 0.0)  // CPU增加，响应时间应该增加
    }
    None => assert_true(false)
  }
}