// Azimuth 时间序列数据处理测试
// 专注于测试时间序列数据的收集、处理、聚合和分析功能

// 测试1: 时间序列数据点处理
test "时间序列数据点处理和验证" {
  // 定义时间序列数据点类型
  type DataPoint = {
    timestamp: Int,
    value: Float,
    labels: Array[(String, String)],
    metric_type: String  // "counter", "gauge", "histogram", "summary"
  }
  
  // 定义时间序列类型
  type TimeSeries = {
    name: String,
    data_points: Array[DataPoint],
    labels: Array[(String, String)],
    metric_type: String
  }
  
  // 创建数据点验证器
  let validate_data_point = fn(point: DataPoint) {
    let errors = []
    
    // 检查时间戳有效性
    if point.timestamp <= 0 {
      errors = errors.push("时间戳必须大于0")
    }
    
    // 检查值的有效性
    if point.value.is_nan() {
      errors = errors.push("值不能为NaN")
    }
    
    if point.value.is_infinite() {
      errors = errors.push("值不能为无限大")
    }
    
    // 检查度量类型有效性
    let valid_types = ["counter", "gauge", "histogram", "summary"]
    if not valid_types.contains(point.metric_type) {
      errors = errors.push("无效的度量类型: " + point.metric_type)
    }
    
    // 检查标签有效性
    for label in point.labels {
      if label.0 == "" {
        errors = errors.push("标签名不能为空")
      }
      
      if label.0.contains(" ") {
        errors = errors.push("标签名不能包含空格: " + label.0)
      }
    }
    
    errors
  }
  
  // 创建时间序列聚合器
  let aggregate_time_series = fn(series: TimeSeries, time_window_ms: Int, aggregation_type: String) {
    if series.data_points.length() == 0 {
      return {
        name: series.name,
        data_points: [],
        labels: series.labels,
        metric_type: series.metric_type
      }
    }
    
    // 按时间窗口分组
    let sorted_points = series.data_points.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    let first_timestamp = sorted_points[0].timestamp
    let last_timestamp = sorted_points[sorted_points.length() - 1].timestamp
    
    let windows = []
    let mut current_window_start = first_timestamp
    
    while current_window_start <= last_timestamp {
      let window_end = current_window_start + time_window_ms
      let window_points = sorted_points.filter(fn(point) { 
        point.timestamp >= current_window_start && point.timestamp < window_end 
      })
      
      if window_points.length() > 0 {
        windows = windows.push(window_points)
      }
      
      current_window_start = window_end
    }
    
    // 聚合每个窗口
    let aggregated_points = []
    for window in windows {
      let window_timestamp = window[0].timestamp  // 使用窗口开始时间
      let values = window.map(fn(point) { point.value })
      
      let aggregated_value = match aggregation_type {
        "avg" => values.reduce(0.0, fn(sum, v) { sum + v }) / (values.length() as Float)
        "sum" => values.reduce(0.0, fn(sum, v) { sum + v })
        "min" => values.reduce(values[0], fn(min, v) { if v < min { v } else { min } })
        "max" => values.reduce(values[0], fn(max, v) { if v > max { v } else { max } })
        "count" => values.length() as Float
        _ => values.reduce(0.0, fn(sum, v) { sum + v })  // 默认求和
      }
      
      aggregated_points = aggregated_points.push({
        timestamp: window_timestamp,
        value: aggregated_value,
        labels: series.labels,
        metric_type: series.metric_type
      })
    }
    
    {
      name: series.name,
      data_points: aggregated_points,
      labels: series.labels,
      metric_type: series.metric_type
    }
  }
  
  // 创建测试数据
  let create_test_time_series = fn() {
    let base_timestamp = Time::now() - 300000  // 5分钟前
    
    let data_points = []
    for i in 0..60 {
      let timestamp = base_timestamp + i * 5000  // 每5秒一个数据点
      let value = 10.0 + (i as Float) * 0.5 + (Math::random() * 2.0 - 1.0)  // 基础值+趋势+随机噪声
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        labels: [
          ("service", "api-gateway"),
          ("endpoint", "/api/users"),
          ("method", "GET")
        ],
        metric_type: "gauge"
      })
    }
    
    {
      name: "request_duration",
      data_points: data_points,
      labels: [
        ("service", "api-gateway"),
        ("unit", "milliseconds")
      ],
      metric_type: "gauge"
    }
  }
  
  // 测试数据点验证
  let valid_point = {
    timestamp: Time::now(),
    value: 42.5,
    labels: [("service", "test"), ("env", "prod")],
    metric_type: "gauge"
  }
  
  let valid_errors = validate_data_point(valid_point)
  assert_eq(valid_errors.length(), 0)
  
  // 测试无效数据点
  let invalid_point = {
    timestamp: -1,  // 无效时间戳
    value: Float::nan(),  // 无效值
    labels: [("", "empty_name"), ("invalid name", "value")],  // 无效标签
    metric_type: "invalid_type"  // 无效类型
  }
  
  let invalid_errors = validate_data_point(invalid_point)
  assert_true(invalid_errors.length() >= 4)
  assert_true(invalid_errors.any(fn(err) { err.contains("时间戳必须大于0") }))
  assert_true(invalid_errors.any(fn(err) { err.contains("值不能为NaN") }))
  assert_true(invalid_errors.any(fn(err) { err.contains("标签名不能为空") }))
  assert_true(invalid_errors.any(fn(err) { err.contains("无效的度量类型") }))
  
  // 测试时间序列聚合
  let test_series = create_test_time_series()
  
  // 测试不同聚合类型
  let aggregation_types = ["avg", "sum", "min", "max", "count"]
  let aggregation_results = []
  
  for agg_type in aggregation_types {
    let aggregated = aggregate_time_series(test_series, 30000, agg_type)  // 30秒窗口
    aggregation_results = aggregation_results.push((agg_type, aggregated))
  }
  
  // 验证聚合结果
  for (agg_type, result) in aggregation_results {
    assert_true(result.data_points.length() > 0)
    assert_eq(result.name, test_series.name)
    assert_eq(result.metric_type, test_series.metric_type)
    
    // 验证聚合后的时间戳是递增的
    for i in 1..result.data_points.length() {
      assert_true(result.data_points[i].timestamp > result.data_points[i-1].timestamp)
    }
    
    // 验证聚合值的合理性
    for point in result.data_points {
      assert_false(point.value.is_nan())
      assert_false(point.value.is_infinite())
      
      match agg_type {
        "count" => assert_true(point.value >= 0.0)
        "min" | "max" => assert_true(point.value >= 0.0)
        _ => {}  // 其他类型不需要特殊验证
      }
    }
  }
  
  // 验证不同聚合类型产生不同的结果
  let avg_result = aggregation_results.filter(fn(t, _) { t == "avg" })[0].1
  let max_result = aggregation_results.filter(fn(t, _) { t == "max" })[0].1
  
  // 同一时间窗口的最大值应该大于等于平均值
  assert_true(max_result.data_points[0].value >= avg_result.data_points[0].value)
}

// 测试2: 时间序列数据降采样
test "时间序列数据降采样和压缩" {
  // 定义降采样策略
  enum DownsamplingStrategy {
    Average
    Sum
    Min
    Max
    First
    Last
  }
  
  // 定义降采样配置
  type DownsamplingConfig = {
    target_interval_ms: Int,
    strategy: DownsamplingStrategy,
    preserve_extremes: Bool  // 是否保留极值点
  }
  
  // 创建降采样器
  let create_downsampler = fn(config: DownsamplingConfig) {
    let downsample = fn(series: TimeSeries) {
      if series.data_points.length() == 0 {
        return series
      }
      
      let sorted_points = series.data_points.sort(fn(a, b) { 
        if a.timestamp < b.timestamp { -1 } 
        else if a.timestamp > b.timestamp { 1 } 
        else { 0 } 
      })
      
      let first_timestamp = sorted_points[0].timestamp
      let last_timestamp = sorted_points[sorted_points.length() - 1].timestamp
      
      let downsampled_points = []
      let mut current_window_start = first_timestamp
      
      while current_window_start <= last_timestamp {
        let window_end = current_window_start + config.target_interval_ms
        let window_points = sorted_points.filter(fn(point) { 
          point.timestamp >= current_window_start && point.timestamp < window_end 
        })
        
        if window_points.length() > 0 {
          let window_timestamp = current_window_start + config.target_interval_ms / 2
          
          let downsampled_value = match config.strategy {
            DownsamplingStrategy::Average => {
              let values = window_points.map(fn(p) { p.value })
              values.reduce(0.0, fn(sum, v) { sum + v }) / (values.length() as Float)
            }
            DownsamplingStrategy::Sum => {
              let values = window_points.map(fn(p) { p.value })
              values.reduce(0.0, fn(sum, v) { sum + v })
            }
            DownsamplingStrategy::Min => {
              window_points.reduce(window_points[0], fn(min, p) { 
                if p.value < min.value { p } else { min } 
              }).value
            }
            DownsamplingStrategy::Max => {
              window_points.reduce(window_points[0], fn(max, p) { 
                if p.value > max.value { p } else { max } 
              }).value
            }
            DownsamplingStrategy::First => window_points[0].value
            DownsamplingStrategy::Last => window_points[window_points.length() - 1].value
          }
          
          downsampled_points = downsampled_points.push({
            timestamp: window_timestamp,
            value: downsampled_value,
            labels: series.labels,
            metric_type: series.metric_type
          })
          
          // 如果需要保留极值点
          if config.preserve_extremes && window_points.length() > 1 {
            let values = window_points.map(fn(p) { p.value })
            let min_value = values.reduce(values[0], fn(min, v) { if v < min { v } else { min } })
            let max_value = values.reduce(values[0], fn(max, v) { if v > max { v } else { max } })
            
            // 添加极值点（如果它们与降采样值不同）
            if min_value != downsampled_value {
              let min_point = window_points.find(fn(p) { p.value == min_value }).unwrap()
              downsampled_points = downsampled_points.push({
                timestamp: min_point.timestamp,
                value: min_value,
                labels: series.labels,
                metric_type: series.metric_type
              })
            }
            
            if max_value != downsampled_value && max_value != min_value {
              let max_point = window_points.find(fn(p) { p.value == max_value }).unwrap()
              downsampled_points = downsampled_points.push({
                timestamp: max_point.timestamp,
                value: max_value,
                labels: series.labels,
                metric_type: series.metric_type
              })
            }
          }
        }
        
        current_window_start = window_end
      }
      
      // 对降采样后的点重新排序
      let final_points = downsampled_points.sort(fn(a, b) { 
        if a.timestamp < b.timestamp { -1 } 
        else if a.timestamp > b.timestamp { 1 } 
        else { 0 } 
      })
      
      {
        name: series.name,
        data_points: final_points,
        labels: series.labels,
        metric_type: series.metric_type
      }
    }
    
    { downsample }
  }
  
  // 创建高频测试数据
  let create_high_frequency_data = fn() {
    let base_timestamp = Time::now() - 60000  // 1分钟前
    let data_points = []
    
    // 每秒一个数据点，共60个点
    for i in 0..60 {
      let timestamp = base_timestamp + i * 1000
      let value = 100.0 + Math::sin(i as Float * 0.1) * 20.0 + (Math::random() * 5.0)
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        labels: [("service", "high-frequency")],
        metric_type: "gauge"
      })
    }
    
    {
      name: "high_frequency_metric",
      data_points: data_points,
      labels: [("unit", "bytes")],
      metric_type: "gauge"
    }
  }
  
  // 测试降采样
  let high_freq_data = create_high_frequency_data()
  assert_eq(high_freq_data.data_points.length(), 60)
  
  // 测试不同降采样策略
  let strategies = [
    (DownsamplingStrategy::Average, "平均降采样"),
    (DownsamplingStrategy::Max, "最大值降采样"),
    (DownsamplingStrategy::Min, "最小值降采样"),
    (DownsamplingStrategy::First, "首值降采样"),
    (DownsamplingStrategy::Last, "末值降采样")
  ]
  
  let downsample_results = []
  
  for (strategy, description) in strategies {
    let config = {
      target_interval_ms: 10000,  // 10秒间隔
      strategy: strategy,
      preserve_extremes: false
    }
    
    let downsampler = create_downsampler(config)
    let downsampled = downsampler.downsample(high_freq_data)
    downsample_results = downsample_results.push((description, downsampled))
  }
  
  // 验证降采样结果
  for (description, result) in downsample_results {
    // 降采样后的点数应该少于原始数据
    assert_true(result.data_points.length() < high_freq_data.data_points.length())
    
    // 验证时间戳递增
    for i in 1..result.data_points.length() {
      assert_true(result.data_points[i].timestamp > result.data_points[i-1].timestamp)
    }
    
    // 验证时间间隔大致符合目标
    for i in 1..result.data_points.length() {
      let interval = result.data_points[i].timestamp - result.data_points[i-1].timestamp
      assert_true(interval >= 8000 && interval <= 12000)  // 允许20%的误差
    }
  }
  
  // 验证不同策略产生不同结果
  let avg_result = downsample_results.filter(fn(d, _) { d.contains("平均") })[0].1
  let max_result = downsample_results.filter(fn(d, _) { d.contains("最大") })[0].1
  let min_result = downsample_results.filter(fn(d, _) { d.contains("最小") })[0].1
  
  // 同一时间窗口的最大值应该大于等于平均值，平均值应该大于等于最小值
  assert_true(max_result.data_points[0].value >= avg_result.data_points[0].value)
  assert_true(avg_result.data_points[0].value >= min_result.data_points[0].value)
  
  // 测试保留极值点的降采样
  let extreme_preserving_config = {
    target_interval_ms: 15000,  // 15秒间隔
    strategy: DownsamplingStrategy::Average,
    preserve_extremes: true
  }
  
  let extreme_downsampler = create_downsampler(extreme_preserving_config)
  let extreme_result = extreme_downsampler.downsample(high_freq_data)
  
  // 保留极值点的情况下，点数可能略多于基本降采样
  assert_true(extreme_result.data_points.length() >= avg_result.data_points.length())
}

// 测试3: 时间序列数据异常检测
test "时间序列数据异常检测" {
  // 定义异常检测结果
  type AnomalyResult = {
    timestamp: Int,
    value: Float,
    expected_value: Float,
    anomaly_score: Float,
    is_anomaly: Bool,
    anomaly_type: String  // "spike", "drop", "trend", "seasonality"
  }
  
  // 定义异常检测器
  type AnomalyDetector = {
    detect_anomalies: fn(TimeSeries) -> Array[AnomalyResult],
    get_threshold: fn() -> Float
  }
  
  // 创建统计异常检测器
  let create_statistical_detector = fn(z_threshold: Float) {
    let detect_anomalies = fn(series: TimeSeries) {
      if series.data_points.length() < 10 {
        return []
      }
      
      let values = series.data_points.map(fn(point) { point.value })
      
      // 计算统计量
      let mean = values.reduce(0.0, fn(sum, v) { sum + v }) / (values.length() as Float)
      let variance = values.reduce(0.0, fn(sum, v) { sum + (v - mean) * (v - mean) }) / (values.length() as Float)
      let std_dev = Math::sqrt(variance)
      
      let anomalies = []
      
      for point in series.data_points {
        let z_score = (point.value - mean) / std_dev
        let anomaly_score = Math::abs(z_score)
        let is_anomaly = anomaly_score > z_threshold
        
        let anomaly_type = if is_anomaly {
          if point.value > mean + z_threshold * std_dev {
            "spike"  // 峰值异常
          } else {
            "drop"   // 下降异常
          }
        } else {
          "normal"
        }
        
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          value: point.value,
          expected_value: mean,
          anomaly_score: anomaly_score,
          is_anomaly: is_anomaly,
          anomaly_type: anomaly_type
        })
      }
      
      anomalies
    }
    
    let get_threshold = fn() { z_threshold }
    
    { detect_anomalies, get_threshold }
  }
  
  // 创建移动平均异常检测器
  let create_moving_average_detector = fn(window_size: Int, threshold_factor: Float) {
    let detect_anomalies = fn(series: TimeSeries) {
      if series.data_points.length() < window_size * 2 {
        return []
      }
      
      let anomalies = []
      
      for i in window_size..(series.data_points.length() - window_size) {
        let current_point = series.data_points[i]
        
        // 计算移动平均
        let window_start = i - window_size
        let window_end = i + window_size
        let window_values = []
        
        for j in window_start..window_end {
          if j != i {  // 排除当前点
            window_values = window_values.push(series.data_points[j].value)
          }
        }
        
        let moving_avg = window_values.reduce(0.0, fn(sum, v) { sum + v }) / (window_values.length() as Float)
        let moving_std = Math::sqrt(
          window_values.reduce(0.0, fn(sum, v) { sum + (v - moving_avg) * (v - moving_avg) }) / 
          (window_values.length() as Float)
        )
        
        let deviation = Math::abs(current_point.value - moving_avg)
        let threshold = threshold_factor * moving_std
        let anomaly_score = deviation / moving_std
        let is_anomaly = deviation > threshold
        
        let anomaly_type = if is_anomaly {
          if current_point.value > moving_avg {
            "spike"
          } else {
            "drop"
          }
        } else {
          "normal"
        }
        
        if is_anomaly {
          anomalies = anomalies.push({
            timestamp: current_point.timestamp,
            value: current_point.value,
            expected_value: moving_avg,
            anomaly_score: anomaly_score,
            is_anomaly: is_anomaly,
            anomaly_type: anomaly_type
          })
        }
      }
      
      anomalies
    }
    
    let get_threshold = fn() { threshold_factor }
    
    { detect_anomalies, get_threshold }
  }
  
  // 创建包含异常的测试数据
  let create_anomaly_test_data = fn() {
    let base_timestamp = Time::now() - 120000  // 2分钟前
    let data_points = []
    
    // 生成正常数据（均值50，标准差5）
    for i in 0..100 {
      let timestamp = base_timestamp + i * 1200  // 每1.2秒一个点
      let mut value = 50.0 + Math::random() * 10.0 - 5.0  // 50 ± 5
      
      // 添加一些异常点
      if i == 20 {
        value = 80.0  // 峰值异常
      } else if i == 50 {
        value = 20.0  // 下降异常
      } else if i == 75 {
        value = 95.0  // 极端峰值
      }
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        labels: [("sensor", "temperature")],
        metric_type: "gauge"
      })
    }
    
    {
      name: "temperature_sensor",
      data_points: data_points,
      labels: [("unit", "celsius")],
      metric_type: "gauge"
    }
  }
  
  // 测试异常检测
  let anomaly_data = create_anomaly_test_data()
  
  // 测试统计异常检测器
  let stat_detector = create_statistical_detector(2.5)  // 2.5个标准差阈值
  let stat_anomalies = stat_detector.detect_anomalies(anomaly_data)
  
  // 验证异常检测结果
  assert_true(stat_anomalies.length() > 0)
  
  // 应该检测到我们插入的异常点
  let spike_anomalies = stat_anomalies.filter(fn(a) { a.anomaly_type == "spike" })
  let drop_anomalies = stat_anomalies.filter(fn(a) { a.anomaly_type == "drop" })
  
  assert_true(spike_anomalies.length() >= 2)  // 至少检测到2个峰值异常
  assert_true(drop_anomalies.length() >= 1)  // 至少检测到1个下降异常
  
  // 验证异常分数
  for anomaly in stat_anomalies {
    assert_true(anomaly.anomaly_score > 2.5)
    assert_true(anomaly.is_anomaly)
    assert_true(anomaly.anomaly_type != "normal")
  }
  
  // 测试移动平均异常检测器
  let ma_detector = create_moving_average_detector(10, 2.0)  // 10个点窗口，2倍标准差阈值
  let ma_anomalies = ma_detector.detect_anomalies(anomaly_data)
  
  // 验证移动平均检测结果
  assert_true(ma_anomalies.length() > 0)
  
  // 移动平均检测器应该对局部异常更敏感
  for anomaly in ma_anomalies {
    assert_true(anomaly.anomaly_score > 2.0)
    assert_true(anomaly.is_anomaly)
    
    // 验证预期值的合理性
    assert_true(anomaly.expected_value > 0.0)
    assert_false(anomaly.expected_value.is_nan())
  }
  
  // 比较两种检测器的结果
  let stat_spike_count = stat_anomalies.filter(fn(a) { a.anomaly_type == "spike" }).length()
  let ma_spike_count = ma_anomalies.filter(fn(a) { a.anomaly_type == "spike" }).length()
  
  // 移动平均检测器可能检测到更多的局部异常
  assert_true(ma_spike_count >= stat_spike_count - 1)  // 允许一些差异
  
  // 测试异常检测器的阈值
  assert_eq(stat_detector.get_threshold(), 2.5)
  assert_eq(ma_detector.get_threshold(), 2.0)
}

// 测试4: 时间序列数据趋势分析
test "时间序列数据趋势分析" {
  // 定义趋势类型
  enum TrendDirection {
    Increasing
    Decreasing
    Stable
    Volatile
  }
  
  // 定义趋势分析结果
  type TrendAnalysis = {
    direction: TrendDirection,
    slope: Float,  // 趋势斜率
    correlation: Float,  // 相关系数
    confidence: Float,  // 置信度
    seasonal_period: Option[Int>,  // 季节性周期（如果有）
    seasonal_strength: Float  // 季节性强度
  }
  
  // 创建趋势分析器
  let create_trend_analyzer = fn() {
    let analyze_trend = fn(series: TimeSeries) {
      if series.data_points.length() < 10 {
        return {
          direction: TrendDirection::Stable,
          slope: 0.0,
          correlation: 0.0,
          confidence: 0.0,
          seasonal_period: None,
          seasonal_strength: 0.0
        }
      }
      
      let n = series.data_points.length() as Float
      let points = series.data_points
      
      // 线性回归计算趋势
      let sum_x = points.reduce_indexed(0.0, fn(acc, point, i) { acc + (i as Float) })
      let sum_y = points.reduce(0.0, fn(acc, point) { acc + point.value })
      let sum_xy = points.reduce_indexed(0.0, fn(acc, point, i) { acc + (i as Float) * point.value })
      let sum_x2 = points.reduce_indexed(0.0, fn(acc, point, i) { acc + (i as Float) * (i as Float) })
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // 计算相关系数
      let mean_x = sum_x / n
      let mean_y = sum_y / n
      
      let numerator = points.reduce_indexed(0.0, fn(acc, point, i) { 
        acc + ((i as Float) - mean_x) * (point.value - mean_y) 
      })
      
      let sum_xx = points.reduce_indexed(0.0, fn(acc, point, i) { 
        acc + ((i as Float) - mean_x) * ((i as Float) - mean_x) 
      })
      
      let sum_yy = points.reduce(0.0, fn(acc, point) { 
        acc + (point.value - mean_y) * (point.value - mean_y) 
      })
      
      let correlation = if sum_xx > 0.0 && sum_yy > 0.0 {
        numerator / Math::sqrt(sum_xx * sum_yy)
      } else {
        0.0
      }
      
      // 确定趋势方向
      let direction = if Math::abs(slope) < 0.1 {
        TrendDirection::Stable
      } else if slope > 0.0 {
        TrendDirection::Increasing
      } else {
        TrendDirection::Decreasing
      }
      
      // 计算置信度（基于相关系数）
      let confidence = Math::abs(correlation)
      
      // 简单的季节性检测（自相关）
      let max_lag = points.length() / 4
      let autocorrelations = []
      
      for lag in 1..max_lag {
        let mut sum_product = 0.0
        let count = points.length() - lag
        
        for i in 0..count {
          sum_product = sum_product + points[i].value * points[i + lag].value
        }
        
        let autocorr = sum_product / (count as Float)
        autocorrelations = autocorrelations.push(autocorr)
      }
      
      // 寻找季节性周期
      let seasonal_period = if autocorrelations.length() > 0 {
        let max_autocorr = autocorrelations.reduce(autocorrelations[0], fn(max, ac) { 
          if ac > max { ac } else { max } 
        })
        
        if max_autocorr > 0.5 {
          let period_index = autocorrelations.find_index(fn(ac) { ac == max_autocorr }).unwrap()
          Some(period_index + 1)
        } else {
          None
        }
      } else {
        None
      }
      
      let seasonal_strength = match seasonal_period {
        Some(period) => {
          if autocorrelations.length() >= period {
            autocorrelations[period - 1]
          } else {
            0.0
          }
        }
        None => 0.0
      }
      
      {
        direction: direction,
        slope: slope,
        correlation: correlation,
        confidence: confidence,
        seasonal_period: seasonal_period,
        seasonal_strength: seasonal_strength
      }
    }
    
    { analyze_trend }
  }
  
  // 创建不同趋势的测试数据
  let create_increasing_data = fn() {
    let base_timestamp = Time::now() - 60000
    let data_points = []
    
    for i in 0..50 {
      let timestamp = base_timestamp + i * 1200
      let value = 10.0 + (i as Float) * 2.0 + (Math::random() * 4.0 - 2.0)  // 线性增长 + 噪声
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        labels: [("metric", "increasing")],
        metric_type: "counter"
      })
    }
    
    {
      name: "increasing_metric",
      data_points: data_points,
      labels: [("type", "trend_test")],
      metric_type: "counter"
    }
  }
  
  let create_decreasing_data = fn() {
    let base_timestamp = Time::now() - 60000
    let data_points = []
    
    for i in 0..50 {
      let timestamp = base_timestamp + i * 1200
      let value = 100.0 - (i as Float) * 1.5 + (Math::random() * 3.0 - 1.5)  // 线性下降 + 噪声
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        labels: [("metric", "decreasing")],
        metric_type: "gauge"
      })
    }
    
    {
      name: "decreasing_metric",
      data_points: data_points,
      labels: [("type", "trend_test")],
      metric_type: "gauge"
    }
  }
  
  let create_seasonal_data = fn() {
    let base_timestamp = Time::now() - 120000
    let data_points = []
    
    for i in 0..100 {
      let timestamp = base_timestamp + i * 1200
      let seasonal_value = 50.0 + 20.0 * Math::sin((i as Float) * 0.2)  // 周期性模式
      let noise = Math::random() * 5.0 - 2.5
      let trend = (i as Float) * 0.1  // 轻微上升趋势
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: seasonal_value + trend + noise,
        labels: [("metric", "seasonal")],
        metric_type: "gauge"
      })
    }
    
    {
      name: "seasonal_metric",
      data_points: data_points,
      labels: [("type", "seasonal_test")],
      metric_type: "gauge"
    }
  }
  
  // 测试趋势分析
  let analyzer = create_trend_analyzer()
  
  // 测试增长趋势
  let increasing_data = create_increasing_data()
  let increasing_analysis = analyzer.analyze_trend(increasing_data)
  
  match increasing_analysis.direction {
    TrendDirection::Increasing => assert_true(true)
    _ => assert_true(false)  // 应该检测到增长趋势
  }
  
  assert_true(increasing_analysis.slope > 0.0)
  assert_true(Math::abs(increasing_analysis.correlation) > 0.7)  // 强相关
  assert_true(increasing_analysis.confidence > 0.7)
  
  // 测试下降趋势
  let decreasing_data = create_decreasing_data()
  let decreasing_analysis = analyzer.analyze_trend(decreasing_data)
  
  match decreasing_analysis.direction {
    TrendDirection::Decreasing => assert_true(true)
    _ => assert_true(false)  // 应该检测到下降趋势
  }
  
  assert_true(decreasing_analysis.slope < 0.0)
  assert_true(Math::abs(decreasing_analysis.correlation) > 0.7)  // 强相关
  assert_true(decreasing_analysis.confidence > 0.7)
  
  // 测试季节性数据
  let seasonal_data = create_seasonal_data()
  let seasonal_analysis = analyzer.analyze_trend(seasonal_data)
  
  // 季节性数据可能检测到季节性周期
  match seasonal_analysis.seasonal_period {
    Some(period) => {
      assert_true(period > 0)
      assert_true(seasonal_analysis.seasonal_strength > 0.3)
    }
    None => {
      // 如果没有检测到季节性，可能是因为噪声太大或周期不明显
      // 这种情况下，我们检查趋势方向
      match seasonal_analysis.direction {
        TrendDirection::Increasing => assert_true(seasonal_analysis.slope > 0.0)
        TrendDirection::Decreasing => assert_true(seasonal_analysis.slope < 0.0)
        TrendDirection::Stable => assert_true(Math::abs(seasonal_analysis.slope) < 0.5)
        TrendDirection::Volatile => assert_true(true)  // 波动数据
      }
    }
  }
  
  // 验证相关系数的有效范围
  assert_true(increasing_analysis.correlation >= -1.0 && increasing_analysis.correlation <= 1.0)
  assert_true(decreasing_analysis.correlation >= -1.0 && decreasing_analysis.correlation <= 1.0)
  assert_true(seasonal_analysis.correlation >= -1.0 && seasonal_analysis.correlation <= 1.0)
  
  // 验证置信度的有效范围
  assert_true(increasing_analysis.confidence >= 0.0 && increasing_analysis.confidence <= 1.0)
  assert_true(decreasing_analysis.confidence >= 0.0 && decreasing_analysis.confidence <= 1.0)
  assert_true(seasonal_analysis.confidence >= 0.0 && seasonal_analysis.confidence <= 1.0)
}