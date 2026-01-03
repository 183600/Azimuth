// Azimuth 时间序列数据分析测试
// 专注于测试时间序列数据的分析、聚合和异常检测能力

// 测试1: 时间序列数据聚合和统计
test "时间序列数据聚合和统计" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Map[String, String]
  }
  
  // 定义聚合窗口
  enum AggregationWindow {
    Minute
    Hour
    Day
    Week
    Month
  }
  
  // 定义聚合函数
  enum AggregationFunction {
    Avg
    Sum
    Min
    Max
    Count
    P50  // 50th percentile
    P95  // 95th percentile
    P99  // 99th percentile
  }
  
  // 定义聚合结果
  type AggregationResult = {
    window_start: Int,
    window_end: Int,
    value: Float,
    point_count: Int,
    function: AggregationFunction
  }
  
  // 创建时间序列分析器
  let create_time_series_analyzer = fn() {
    {
      // 按时间窗口聚合数据
      aggregate_by_window: fn(points: Array[TimeSeriesPoint], window: AggregationWindow, function: AggregationFunction) {
        if points.length() == 0 {
          return []
        }
        
        // 计算窗口大小（毫秒）
        let window_size_ms = match window {
          Minute => 60 * 1000
          Hour => 60 * 60 * 1000
          Day => 24 * 60 * 60 * 1000
          Week => 7 * 24 * 60 * 60 * 1000
          Month => 30 * 24 * 60 * 60 * 1000
        }
        
        // 按时间戳排序
        let sorted_points = points.sort(fn(a, b) { a.timestamp - b.timestamp })
        
        // 分组到窗口
        let window_groups = sorted_points.reduce(Map::empty(), fn(groups, point) {
          let window_start = (point.timestamp / window_size_ms) * window_size_ms
          let window_end = window_start + window_size_ms
          let window_key = window_start.to_string() + "-" + window_end.to_string()
          
          let window_points = match Map::get(groups, window_key) {
            Some(existing) => existing.push(point)
            None => [point]
          }
          
          Map::insert(groups, window_key, window_points)
        })
        
        // 对每个窗口应用聚合函数
        let results = []
        for (window_key, window_points) in window_groups {
          let window_parts = window_key.split("-")
          let window_start = window_parts[0].to_int()
          let window_end = window_parts[1].to_int()
          
          let aggregated_value = match function {
            Avg => {
              let sum = window_points.reduce(0.0, fn(acc, point) { acc + point.value })
              sum / (window_points.length() as Float)
            }
            Sum => {
              window_points.reduce(0.0, fn(acc, point) { acc + point.value })
            }
            Min => {
              window_points.reduce(Float::infinity, fn(min, point) { 
                if point.value < min { point.value } else { min } 
              })
            }
            Max => {
              window_points.reduce(0.0, fn(max, point) { 
                if point.value > max { point.value } else { max } 
              })
            }
            Count => {
              window_points.length() as Float
            }
            P50 => {
              // 简化的百分位计算
              let sorted_values = window_points.map(fn(point) { point.value }).sort(fn(a, b) { a - b })
              let index = ((sorted_values.length() as Float) * 0.5).to_int()
              sorted_values[index]
            }
            P95 => {
              let sorted_values = window_points.map(fn(point) { point.value }).sort(fn(a, b) { a - b })
              let index = ((sorted_values.length() as Float) * 0.95).to_int()
              sorted_values[index]
            }
            P99 => {
              let sorted_values = window_points.map(fn(point) { point.value }).sort(fn(a, b) { a - b })
              let index = ((sorted_values.length() as Float) * 0.99).to_int()
              sorted_values[index]
            }
          }
          
          let result = {
            window_start,
            window_end,
            value: aggregated_value,
            point_count: window_points.length(),
            function
          }
          
          results = results.push(result)
        }
        
        // 按窗口开始时间排序结果
        results.sort(fn(a, b) { a.window_start - b.window_start })
      },
      
      // 计算统计指标
      calculate_statistics: fn(points: Array[TimeSeriesPoint]) {
        if points.length() == 0 {
          return None
        }
        
        let values = points.map(fn(point) { point.value })
        let sorted_values = values.sort(fn(a, b) { a - b })
        
        let count = values.length()
        let sum = values.reduce(0.0, fn(acc, value) { acc + value })
        let mean = sum / (count as Float)
        
        // 计算标准差
        let variance = values.reduce(0.0, fn(acc, value) { 
          acc + (value - mean) * (value - mean) 
        }) / (count as Float)
        let std_dev = Float::sqrt(variance)
        
        // 计算百分位数
        let p50_index = ((count as Float) * 0.5).to_int()
        let p95_index = ((count as Float) * 0.95).to_int()
        let p99_index = ((count as Float) * 0.99).to_int()
        
        let min = sorted_values[0]
        let max = sorted_values[count - 1]
        let p50 = sorted_values[p50_index]
        let p95 = sorted_values[p95_index]
        let p99 = sorted_values[p99_index]
        
        Some({
          count,
          min,
          max,
          mean,
          std_dev,
          p50,
          p95,
          p99
        })
      }
    }
  }
  
  // 创建测试时间序列数据
  let create_test_time_series = fn(point_count: Int, base_value: Float, variance: Float, trend: Float) {
    let points = []
    let base_time = Time::now() - (point_count * 60000)  // 从1小时前开始，每分钟一个点
    
    for i in 0..point_count {
      let trend_value = base_value + (trend * (i as Float))
      let random_variation = (Random::float() - 0.5) * 2.0 * variance
      let value = trend_value + random_variation
      
      let point = {
        timestamp: base_time + (i * 60000),  // 每分钟一个点
        value: value,
        tags: [
          ("service", "api-gateway"),
          ("metric", "response_time"),
          ("environment", "production")
        ]
      }
      
      points = points.push(point)
    }
    
    points
  }
  
  // 测试时间序列聚合
  let analyzer = create_time_series_analyzer()
  
  // 创建测试数据：基础值100ms，方差20ms，上升趋势0.5ms/分钟
  let test_points = create_test_time_series(1440, 100.0, 20.0, 0.5)  // 24小时的数据
  
  assert_eq(test_points.length(), 1440)
  
  // 测试按小时聚合平均值
  let hourly_avg = analyzer.aggregate_by_window(test_points, Hour, Avg)
  assert_eq(hourly_avg.length(), 24)  // 24小时
  
  for result in hourly_avg {
    assert_true(result.window_end > result.window_start)
    assert_true(result.point_count > 0)
    assert_true(result.value > 0.0)
    assert_eq(result.function, Avg)
    
    // 验证窗口大小为1小时
    assert_eq(result.window_end - result.window_start, 60 * 60 * 1000)
  }
  
  // 测试按天聚合最大值
  let daily_max = analyzer.aggregate_by_window(test_points, Day, Max)
  assert_eq(daily_max.length(), 1)  // 1天
  
  let day_result = daily_max[0]
  assert_eq(day_result.function, Max)
  assert_eq(day_result.point_count, 1440)
  assert_true(day_result.value > 100.0)  // 应该大于基础值，因为有上升趋势
  
  // 测试百分位聚合
  let hourly_p95 = analyzer.aggregate_by_window(test_points, Hour, P95)
  assert_eq(hourly_p95.length(), 24)
  
  // 验证P95值通常大于平均值
  for i in 0..hourly_avg.length() {
    assert_true(hourly_p95[i].value >= hourly_avg[i].value)
  }
  
  // 测试统计计算
  let stats = analyzer.calculate_statistics(test_points)
  match stats {
    Some(statistics) => {
      assert_eq(statistics.count, 1440)
      assert_true(statistics.min < statistics.mean)
      assert_true(statistics.max > statistics.mean)
      assert_true(statistics.std_dev > 0.0)
      assert_true(statistics.p50 <= statistics.p95)
      assert_true(statistics.p95 <= statistics.p99)
      
      // 验证上升趋势反映在统计中
      assert_true(statistics.mean > 100.0)  // 应该大于基础值
    }
    None => assert_true(false)
  }
  
  // 测试空数据集
  let empty_points = []
  let empty_aggregation = analyzer.aggregate_by_window(empty_points, Hour, Avg)
  assert_eq(empty_aggregation.length(), 0)
  
  let empty_stats = analyzer.calculate_statistics(empty_points)
  assert_eq(empty_stats, None)
}

// 测试2: 时间序列异常检测
test "时间序列异常检测" {
  // 定义异常类型
  enum AnomalyType {
    Spike  // 突增
    Drop   // 突降
    Trend  // 趋势变化
    Outlier  // 离群点
    Pattern  // 模式异常
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    timestamp: Int,
    value: Float,
    anomaly_type: AnomalyType,
    severity: Float,  // 0.0 to 1.0
    expected_value: Float,
    confidence: Float
  }
  
  // 定义异常检测算法
  enum AnomalyDetectionAlgorithm {
    StatisticalThreshold
    MovingAverage
    ExponentialSmoothing
    SeasonalDecomposition
    IsolationForest
  }
  
  // 创建异常检测器
  let create_anomaly_detector = fn(algorithm: AnomalyDetectionAlgorithm) {
    {
      algorithm,
      
      // 检测异常
      detect_anomalies: fn(points: Array[TimeSeriesPoint], sensitivity: Float) {
        if points.length() < 10 {
          return []  // 数据点太少，无法检测
        }
        
        match algorithm {
          StatisticalThreshold => {
            // 基于统计阈值的异常检测
            let values = points.map(fn(point) { point.value })
            let sorted_values = values.sort(fn(a, b) { a - b })
            
            let count = values.length()
            let sum = values.reduce(0.0, fn(acc, value) { acc + value })
            let mean = sum / (count as Float)
            
            // 计算标准差
            let variance = values.reduce(0.0, fn(acc, value) { 
              acc + (value - mean) * (value - mean) 
            }) / (count as Float)
            let std_dev = Float::sqrt(variance)
            
            // 计算阈值
            let threshold_multiplier = 3.0 - (sensitivity * 2.0)  // sensitivity 0.0-1.0
            let upper_threshold = mean + (threshold_multiplier * std_dev)
            let lower_threshold = mean - (threshold_multiplier * std_dev)
            
            // 检测异常
            let anomalies = []
            for point in points {
              let is_anomaly = point.value > upper_threshold || point.value < lower_threshold
              
              if is_anomaly {
                let anomaly_type = if point.value > upper_threshold { Spike } else { Drop }
                let severity = if point.value > upper_threshold {
                  (point.value - upper_threshold) / (std_dev + 0.001)
                } else {
                  (lower_threshold - point.value) / (std_dev + 0.001)
                }
                
                let anomaly = {
                  timestamp: point.timestamp,
                  value: point.value,
                  anomaly_type: anomaly_type,
                  severity: Float::min(severity / 3.0, 1.0),  // 归一化到0-1
                  expected_value: mean,
                  confidence: 0.8  // 统计方法有较高置信度
                }
                
                anomalies = anomalies.push(anomaly)
              }
            }
            
            anomalies
          }
          MovingAverage => {
            // 基于移动平均的异常检测
            let window_size = 10
            let anomalies = []
            
            for i in window_size..points.length() {
              let current_point = points[i]
              
              // 计算移动平均
              let window_points = points.slice(i - window_size, i)
              let window_values = window_points.map(fn(point) { point.value })
              let window_sum = window_values.reduce(0.0, fn(acc, value) { acc + value })
              let moving_avg = window_sum / (window_values.length() as Float)
              
              // 计算移动标准差
              let variance = window_values.reduce(0.0, fn(acc, value) { 
                acc + (value - moving_avg) * (value - moving_avg) 
              }) / (window_values.length() as Float)
              let moving_std = Float::sqrt(variance)
              
              // 检测异常
              let threshold_multiplier = 2.5 - (sensitivity * 1.5)
              let upper_threshold = moving_avg + (threshold_multiplier * moving_std)
              let lower_threshold = moving_avg - (threshold_multiplier * moving_std)
              
              let is_anomaly = current_point.value > upper_threshold || current_point.value < lower_threshold
              
              if is_anomaly {
                let anomaly_type = if current_point.value > upper_threshold { Spike } else { Drop }
                let severity = if current_point.value > upper_threshold {
                  (current_point.value - upper_threshold) / (moving_std + 0.001)
                } else {
                  (lower_threshold - current_point.value) / (moving_std + 0.001)
                }
                
                let anomaly = {
                  timestamp: current_point.timestamp,
                  value: current_point.value,
                  anomaly_type: anomaly_type,
                  severity: Float::min(severity / 2.5, 1.0),
                  expected_value: moving_avg,
                  confidence: 0.7
                }
                
                anomalies = anomalies.push(anomaly)
              }
            }
            
            anomalies
          }
          ExponentialSmoothing => {
            // 基于指数平滑的异常检测
            let alpha = 0.3  // 平滑因子
            let mut smoothed_value = points[0].value
            let mut smoothed_errors = []
            let anomalies = []
            
            // 计算指数平滑值
            for i in 1..points.length() {
              let current_value = points[i].value
              let previous_smoothed = smoothed_value
              smoothed_value = alpha * current_value + (1.0 - alpha) * previous_smoothed
              
              // 计算平滑误差
              let error = current_value - smoothed_value
              smoothed_errors = smoothed_errors.push(error)
            }
            
            // 计算误差的标准差
            if smoothed_errors.length() > 0 {
              let error_sum = smoothed_errors.reduce(0.0, fn(acc, error) { acc + error })
              let error_mean = error_sum / (smoothed_errors.length() as Float)
              let error_variance = smoothed_errors.reduce(0.0, fn(acc, error) { 
                acc + (error - error_mean) * (error - error_mean) 
              }) / (smoothed_errors.length() as Float)
              let error_std = Float::sqrt(error_variance)
              
              // 检测异常
              let threshold_multiplier = 2.0 - (sensitivity * 1.0)
              let error_threshold = threshold_multiplier * error_std
              
              for i in 0..smoothed_errors.length() {
                let point_index = i + 1
                let error = smoothed_errors[i]
                
                if Float::abs(error) > error_threshold {
                  let point = points[point_index]
                  let anomaly_type = if error > 0 { Spike } else { Drop }
                  let severity = Float::abs(error) / (error_std + 0.001)
                  
                  let anomaly = {
                    timestamp: point.timestamp,
                    value: point.value,
                    anomaly_type: anomaly_type,
                    severity: Float::min(severity / 2.0, 1.0),
                    expected_value: point.value - error,
                    confidence: 0.75
                  }
                  
                  anomalies = anomalies.push(anomaly)
                }
              }
            }
            
            anomalies
          }
          SeasonalDecomposition => {
            // 简化的季节性分解异常检测
            let season_length = 24  // 假设24小时季节性
            let anomalies = []
            
            // 计算季节性模式
            if points.length() >= season_length * 2 {
              let seasonal_patterns = Map::empty()
              
              // 计算每个季节位置的平均值
              for i in 0..points.length() {
                let season_index = i % season_length
                let current_value = points[i].value
                
                let (sum, count) = match Map::get(seasonal_patterns, season_index.to_string()) {
                  Some((existing_sum, existing_count)) => {
                    (existing_sum + current_value, existing_count + 1)
                  }
                  None => {
                    (current_value, 1)
                  }
                }
                
                seasonal_patterns = Map::insert(seasonal_patterns, season_index.to_string(), (sum, count))
              }
              
              // 计算季节性期望值
              for i in 0..points.length() {
                let season_index = i % season_length
                let current_point = points[i]
                
                match Map::get(seasonal_patterns, season_index.to_string()) {
                  Some((sum, count)) => {
                    let seasonal_avg = sum / count
                    let deviation = current_point.value - seasonal_avg
                    
                    // 计算所有偏差的标准差
                    let all_deviations = []
                    for j in 0..points.length() {
                      let j_season_index = j % season_length
                      match Map::get(seasonal_patterns, j_season_index.to_string()) {
                        Some((j_sum, j_count)) => {
                          let j_seasonal_avg = j_sum / j_count
                          all_deviations = all_deviations.push(points[j].value - j_seasonal_avg)
                        }
                        None => ()
                      }
                    }
                    
                    if all_deviations.length() > 0 {
                      let dev_sum = all_deviations.reduce(0.0, fn(acc, dev) { acc + dev })
                      let dev_mean = dev_sum / (all_deviations.length() as Float)
                      let dev_variance = all_deviations.reduce(0.0, fn(acc, dev) { 
                        acc + (dev - dev_mean) * (dev - dev_mean) 
                      }) / (all_deviations.length() as Float)
                      let dev_std = Float::sqrt(dev_variance)
                      
                      // 检测异常
                      let threshold_multiplier = 2.5 - (sensitivity * 1.5)
                      let threshold = threshold_multiplier * dev_std
                      
                      if Float::abs(deviation) > threshold {
                        let anomaly_type = if deviation > 0 { Spike } else { Drop }
                        let severity = Float::abs(deviation) / (dev_std + 0.001)
                        
                        let anomaly = {
                          timestamp: current_point.timestamp,
                          value: current_point.value,
                          anomaly_type: anomaly_type,
                          severity: Float::min(severity / 2.5, 1.0),
                          expected_value: seasonal_avg,
                          confidence: 0.85
                        }
                        
                        anomalies = anomalies.push(anomaly)
                      }
                    }
                  }
                  None => ()
                }
              }
            }
            
            anomalies
          }
          IsolationForest => {
            // 简化的孤立森林异常检测
            let anomalies = []
            
            // 计算数据的统计特征
            let values = points.map(fn(point) { point.value })
            let sorted_values = values.sort(fn(a, b) { a - b })
            
            let count = values.length()
            let sum = values.reduce(0.0, fn(acc, value) { acc + value })
            let mean = sum / (count as Float)
            
            // 计算四分位数
            let q1_index = ((count as Float) * 0.25).to_int()
            let q3_index = ((count as Float) * 0.75).to_int()
            let q1 = sorted_values[q1_index]
            let q3 = sorted_values[q3_index]
            let iqr = q3 - q1
            
            // 使用IQR方法检测异常
            let outlier_threshold = 1.5 + (1.0 - sensitivity)  // sensitivity越高，阈值越严格
            let lower_bound = q1 - (outlier_threshold * iqr)
            let upper_bound = q3 + (outlier_threshold * iqr)
            
            for point in points {
              if point.value < lower_bound || point.value > upper_bound {
                let anomaly_type = if point.value > upper_bound { Spike } else { Drop }
                let severity = if point.value > upper_bound {
                  (point.value - upper_bound) / (iqr + 0.001)
                } else {
                  (lower_bound - point.value) / (iqr + 0.001)
                }
                
                let anomaly = {
                  timestamp: point.timestamp,
                  value: point.value,
                  anomaly_type: anomaly_type,
                  severity: Float::min(severity / 3.0, 1.0),
                  expected_value: mean,
                  confidence: 0.9
                }
                
                anomalies = anomalies.push(anomaly)
              }
            }
            
            anomalies
          }
        }
      }
    }
  }
  
  // 创建带异常的测试数据
  let create_test_data_with_anomalies = fn() {
    let base_points = create_test_time_series(100, 50.0, 5.0, 0.1)  // 基础数据
    let anomaly_points = []
    
    // 在基础数据中插入异常
    for i in 0..base_points.length() {
      let point = base_points[i]
      let modified_point = 
        if i == 20 {
          // 突增异常
          { timestamp: point.timestamp, value: point.value + 50.0, tags: point.tags }
        } else if i == 40 {
          // 突降异常
          { timestamp: point.timestamp, value: point.value - 30.0, tags: point.tags }
        } else if i == 60 {
          // 中等异常
          { timestamp: point.timestamp, value: point.value + 20.0, tags: point.tags }
        } else {
          point
        }
      
      anomaly_points = anomaly_points.push(modified_point)
    }
    
    anomaly_points
  }
  
  // 测试异常检测
  let anomaly_data = create_test_data_with_anomalies()
  
  // 测试统计阈值方法
  let statistical_detector = create_anomaly_detector(StatisticalThreshold)
  let statistical_anomalies = statistical_detector.detect_anomalies(anomaly_data, 0.5)
  
  assert_true(statistical_anomalies.length() > 0)
  
  // 验证检测到的异常包含我们插入的异常点
  let anomaly_timestamps = statistical_anomalies.map(fn(anomaly) { anomaly.timestamp })
  let expected_anomaly_timestamps = [
    anomaly_data[20].timestamp,
    anomaly_data[40].timestamp,
    anomaly_data[60].timestamp
  ]
  
  for expected_timestamp in expected_anomaly_timestamps {
    assert_true(anomaly_timestamps.some(fn(timestamp) { timestamp == expected_timestamp }))
  }
  
  // 验证异常属性
  for anomaly in statistical_anomalies {
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.value > 0.0)
    assert_true(anomaly.severity >= 0.0)
    assert_true(anomaly.severity <= 1.0)
    assert_true(anomaly.expected_value > 0.0)
    assert_true(anomaly.confidence >= 0.0)
    assert_true(anomaly.confidence <= 1.0)
  }
  
  // 测试移动平均方法
  let moving_avg_detector = create_anomaly_detector(MovingAverage)
  let moving_avg_anomalies = moving_avg_detector.detect_anomalies(anomaly_data, 0.7)
  
  assert_true(moving_avg_anomalies.length() > 0)
  
  // 测试指数平滑方法
  let exp_smoothing_detector = create_anomaly_detector(ExponentialSmoothing)
  let exp_smoothing_anomalies = exp_smoothing_detector.detect_anomalies(anomaly_data, 0.6)
  
  assert_true(exp_smoothing_anomalies.length() > 0)
  
  // 测试不同敏感度的影响
  let low_sensitivity_anomalies = statistical_detector.detect_anomalies(anomaly_data, 0.2)  // 低敏感度
  let high_sensitivity_anomalies = statistical_detector.detect_anomalies(anomaly_data, 0.8)  // 高敏感度
  
  assert_true(high_sensitivity_anomalies.length() >= low_sensitivity_anomalies.length())
  
  // 测试季节性分解方法
  let seasonal_data = create_test_time_series(168, 100.0, 10.0, 0.0)  // 7天数据，每小时一个点
  let seasonal_detector = create_anomaly_detector(SeasonalDecomposition)
  let seasonal_anomalies = seasonal_detector.detect_anomalies(seasonal_data, 0.5)
  
  // 季节性数据应该没有异常（或者很少）
  assert_true(seasonal_anomalies.length() < 5)
}

// 测试3: 时间序列预测和趋势分析
test "时间序列预测和趋势分析" {
  // 定义预测模型类型
  enum PredictionModel {
    LinearRegression
    ExponentialSmoothing
    ARIMA
    SeasonalNaive
    MovingAverage
  }
  
  // 定义预测结果
  type PredictionResult = {
    timestamp: Int,
    predicted_value: Float,
    confidence_interval_lower: Float,
    confidence_interval_upper: Float,
    model: PredictionModel
  }
  
  // 定义趋势分析结果
  type TrendAnalysisResult = {
    trend_direction: String,  // "increasing", "decreasing", "stable"
    trend_strength: Float,    // 0.0 to 1.0
    slope: Float,             // 变化率
    r_squared: Float,         // 拟合度
    seasonality_detected: Bool
    seasonal_period: Option[Int]
  }
  
  // 创建时间序列预测器
  let create_time_series_predictor = fn(model: PredictionModel) {
    {
      model,
      
      // 训练模型并预测
      predict: fn(points: Array[TimeSeriesPoint], forecast_horizon: Int) {
        if points.length() < 5 {
          return []  // 数据点太少，无法预测
        }
        
        match model {
          LinearRegression => {
            // 简单线性回归预测
            let n = points.length() as Float
            let x_values = []  // 时间索引
            let y_values = []  // 观测值
            
            for i in 0..points.length() {
              x_values = x_values.push(i as Float)
              y_values = y_values.push(points[i].value)
            }
            
            // 计算线性回归参数
            let sum_x = x_values.reduce(0.0, fn(acc, x) { acc + x })
            let sum_y = y_values.reduce(0.0, fn(acc, y) { acc + y })
            let sum_xy = x_values.reduce(0.0, fn(acc, x) { 
              acc + x * y_values[x_values.index_of(x).unwrap_or(0)]
            })
            let sum_x2 = x_values.reduce(0.0, fn(acc, x) { acc + x * x })
            
            let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
            let intercept = (sum_y - slope * sum_x) / n
            
            // 计算预测值
            let predictions = []
            let last_timestamp = points[points.length() - 1].timestamp
            let time_interval = if points.length() > 1 {
              points[1].timestamp - points[0].timestamp
            } else {
              60000  // 默认1分钟间隔
            }
            
            for i in 1..(forecast_horizon + 1) {
              let future_x = (points.length() + i - 1) as Float
              let predicted_value = slope * future_x + intercept
              
              // 简化的置信区间计算
              let residual_sum_of_squares = y_values.reduce(0.0, fn(acc, y) {
                let x = x_values[y_values.index_of(y).unwrap_or(0)]
                let predicted = slope * x + intercept
                acc + (y - predicted) * (y - predicted)
              })
              let mse = residual_sum_of_squares / (n - 2.0)
              let standard_error = Float::sqrt(mse)
              
              let t_value = 1.96  // 95%置信区间
              let margin_of_error = t_value * standard_error * Float::sqrt(1.0 + 1.0 / n + (future_x - sum_x / n) * (future_x - sum_x / n) / (sum_x2 - sum_x * sum_x / n))
              
              let prediction = {
                timestamp: last_timestamp + (i * time_interval),
                predicted_value: predicted_value,
                confidence_interval_lower: predicted_value - margin_of_error,
                confidence_interval_upper: predicted_value + margin_of_error,
                model: LinearRegression
              }
              
              predictions = predictions.push(prediction)
            }
            
            predictions
          }
          ExponentialSmoothing => {
            // 指数平滑预测
            let alpha = 0.3  // 平滑因子
            let mut smoothed_value = points[0].value
            
            // 计算平滑值
            for i in 1..points.length() {
              smoothed_value = alpha * points[i].value + (1.0 - alpha) * smoothed_value
            }
            
            // 生成预测
            let predictions = []
            let last_timestamp = points[points.length() - 1].timestamp
            let time_interval = if points.length() > 1 {
              points[1].timestamp - points[0].timestamp
            } else {
              60000
            }
            
            for i in 1..(forecast_horizon + 1) {
              // 指数平滑的预测就是最后的平滑值
              let predicted_value = smoothed_value
              
              // 简化的置信区间
              let confidence_range = predicted_value * 0.2  // 20%的置信区间
              
              let prediction = {
                timestamp: last_timestamp + (i * time_interval),
                predicted_value: predicted_value,
                confidence_interval_lower: predicted_value - confidence_range,
                confidence_interval_upper: predicted_value + confidence_range,
                model: ExponentialSmoothing
              }
              
              predictions = predictions.push(prediction)
            }
            
            predictions
          }
          SeasonalNaive => {
            // 季节性朴素预测
            let season_length = 24  // 假设24小时季节性
            
            if points.length() < season_length {
              return []  // 数据不足以检测季节性
            }
            
            let predictions = []
            let last_timestamp = points[points.length() - 1].timestamp
            let time_interval = if points.length() > 1 {
              points[1].timestamp - points[0].timestamp
            } else {
              60000
            }
            
            for i in 1..(forecast_horizon + 1) {
              // 使用上个季节对应时间点的值作为预测
              let seasonal_index = (points.length() - season_length + i - 1) % season_length
              let predicted_value = points[seasonal_index].value
              
              // 简化的置信区间
              let confidence_range = predicted_value * 0.15
              
              let prediction = {
                timestamp: last_timestamp + (i * time_interval),
                predicted_value: predicted_value,
                confidence_interval_lower: predicted_value - confidence_range,
                confidence_interval_upper: predicted_value + confidence_range,
                model: SeasonalNaive
              }
              
              predictions = predictions.push(prediction)
            }
            
            predictions
          }
          MovingAverage => {
            // 移动平均预测
            let window_size = 10
            let predictions = []
            let last_timestamp = points[points.length() - 1].timestamp
            let time_interval = if points.length() > 1 {
              points[1].timestamp - points[0].timestamp
            } else {
              60000
            }
            
            // 计算最后的移动平均值
            let recent_points = points.slice(points.length() - window_size, points.length())
            let sum = recent_points.reduce(0.0, fn(acc, point) { acc + point.value })
            let moving_avg = sum / (recent_points.length() as Float)
            
            for i in 1..(forecast_horizon + 1) {
              let predicted_value = moving_avg
              
              // 简化的置信区间
              let variance = recent_points.reduce(0.0, fn(acc, point) { 
                acc + (point.value - moving_avg) * (point.value - moving_avg) 
              }) / (recent_points.length() as Float)
              let std_dev = Float::sqrt(variance)
              let confidence_range = 1.96 * std_dev
              
              let prediction = {
                timestamp: last_timestamp + (i * time_interval),
                predicted_value: predicted_value,
                confidence_interval_lower: predicted_value - confidence_range,
                confidence_interval_upper: predicted_value + confidence_range,
                model: MovingAverage
              }
              
              predictions = predictions.push(prediction)
            }
            
            predictions
          }
          ARIMA => {
            // 简化的ARIMA预测（实际上是指数平滑的变体）
            // 在实际实现中，ARIMA会更复杂
            let alpha = 0.5  // AR参数
            let beta = 0.3   // MA参数
            
            let predictions = []
            let last_timestamp = points[points.length() - 1].timestamp
            let time_interval = if points.length() > 1 {
              points[1].timestamp - points[0].timestamp
            } else {
              60000
            }
            
            // 使用最后几个点进行预测
            let last_value = points[points.length() - 1].value
            let second_last_value = if points.length() > 1 {
              points[points.length() - 2].value
            } else {
              last_value
            }
            
            for i in 1..(forecast_horizon + 1) {
              let predicted_value = alpha * last_value + beta * (last_value - second_last_value)
              
              // 简化的置信区间
              let confidence_range = predicted_value * 0.25
              
              let prediction = {
                timestamp: last_timestamp + (i * time_interval),
                predicted_value: predicted_value,
                confidence_interval_lower: predicted_value - confidence_range,
                confidence_interval_upper: predicted_value + confidence_range,
                model: ARIMA
              }
              
              predictions = predictions.push(prediction)
            }
            
            predictions
          }
        }
      },
      
      // 趋势分析
      analyze_trend: fn(points: Array[TimeSeriesPoint]) {
        if points.length() < 3 {
          return None
        }
        
        // 简单线性回归分析趋势
        let n = points.length() as Float
        let x_values = []
        let y_values = []
        
        for i in 0..points.length() {
          x_values = x_values.push(i as Float)
          y_values = y_values.push(points[i].value)
        }
        
        let sum_x = x_values.reduce(0.0, fn(acc, x) { acc + x })
        let sum_y = y_values.reduce(0.0, fn(acc, y) { acc + y })
        let sum_xy = x_values.reduce(0.0, fn(acc, x) { 
          acc + x * y_values[x_values.index_of(x).unwrap_or(0)]
        })
        let sum_x2 = x_values.reduce(0.0, fn(acc, x) { acc + x * x })
        let sum_y2 = y_values.reduce(0.0, fn(acc, y) { acc + y * y })
        
        let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
        let intercept = (sum_y - slope * sum_x) / n
        
        // 计算R²
        let ss_tot = sum_y2 - (sum_y * sum_y) / n
        let ss_res = y_values.reduce(0.0, fn(acc, y) {
          let x = x_values[y_values.index_of(y).unwrap_or(0)]
          let predicted = slope * x + intercept
          acc + (y - predicted) * (y - predicted)
        })
        let r_squared = if ss_tot > 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
        
        // 确定趋势方向
        let trend_direction = if slope > 0.1 {
          "increasing"
        } else if slope < -0.1 {
          "decreasing"
        } else {
          "stable"
        }
        
        // 计算趋势强度（基于斜率和R²）
        let trend_strength = Float::min(Float::abs(slope) * r_squared * 10.0, 1.0)
        
        // 简化的季节性检测
        let seasonality_detected = points.length() >= 24  // 假设至少24个点才能检测季节性
        let seasonal_period = if seasonality_detected { Some(24) } else { None }
        
        Some({
          trend_direction,
          trend_strength,
          slope,
          r_squared,
          seasonality_detected,
          seasonal_period
        })
      }
    }
  }
  
  // 测试时间序列预测
  let historical_data = create_test_time_series(100, 50.0, 5.0, 0.2)  // 上升趋势数据
  
  // 测试线性回归预测
  let linear_predictor = create_time_series_predictor(LinearRegression)
  let linear_predictions = linear_predictor.predict(historical_data, 10)
  
  assert_eq(linear_predictions.length(), 10)
  
  for prediction in linear_predictions {
    assert_true(prediction.timestamp > 0)
    assert_true(prediction.predicted_value > 0.0)
    assert_true(prediction.confidence_interval_lower <= prediction.predicted_value)
    assert_true(prediction.confidence_interval_upper >= prediction.predicted_value)
    assert_eq(prediction.model, LinearRegression)
  }
  
  // 验证预测值的趋势（应该是递增的，因为历史数据是上升趋势）
  for i in 1..linear_predictions.length() {
    assert_true(linear_predictions[i].predicted_value >= linear_predictions[i-1].predicted_value - 0.1)
  }
  
  // 测试趋势分析
  let trend_analysis = linear_predictor.analyze_trend(historical_data)
  match trend_analysis {
    Some(analysis) => {
      assert_eq(analysis.trend_direction, "increasing")
      assert_true(analysis.trend_strength > 0.0)
      assert_true(analysis.slope > 0.0)
      assert_true(analysis.r_squared >= 0.0)
      assert_true(analysis.r_squared <= 1.0)
    }
    None => assert_true(false)
  }
  
  // 测试季节性数据
  let seasonal_data = create_test_time_series(168, 100.0, 10.0, 0.0)  // 7天数据，无趋势
  let seasonal_predictor = create_time_series_predictor(SeasonalNaive)
  let seasonal_predictions = seasonal_predictor.predict(seasonal_data, 24)
  
  assert_eq(seasonal_predictions.length(), 24)
  
  for prediction in seasonal_predictions {
    assert_eq(prediction.model, SeasonalNaive)
  }
  
  // 测试移动平均预测
  let ma_predictor = create_time_series_predictor(MovingAverage)
  let ma_predictions = ma_predictor.predict(historical_data, 5)
  
  assert_eq(ma_predictions.length(), 5)
  
  for prediction in ma_predictions {
    assert_eq(prediction.model, MovingAverage)
  }
  
  // 测试平稳数据（无趋势）
  let stable_data = create_test_time_series(50, 75.0, 3.0, 0.0)
  let stable_trend = ma_predictor.analyze_trend(stable_data)
  match stable_trend {
    Some(analysis) => {
      assert_eq(analysis.trend_direction, "stable")
      assert_true(Float::abs(analysis.slope) < 0.1)
    }
    None => assert_true(false)
  }
}