// Azimuth 高级遥测量数据分析测试用例
// 专注于复杂的遥测量数据分析、聚合和异常检测

// 测试1: 多维度遥测量数据分析
test "多维度遥测量数据分析" {
  // 定义多维遥测量数据结构
  type MetricPoint = {
    timestamp: Int,
    value: Float,
    dimensions: Array[(String, String)],
    quality_score: Float
  }
  
  type MetricSeries = {
    name: String,
    points: Array[MetricPoint],
    aggregation_state: String
  }
  
  // 创建测试数据
  let create_test_metrics = fn() {
    let cpu_metrics = MetricSeries {
      name: "cpu_usage",
      points: [
        { timestamp: 1000, value: 45.2, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.95 },
        { timestamp: 2000, value: 52.8, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.92 },
        { timestamp: 3000, value: 38.1, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.98 },
        { timestamp: 1000, value: 67.3, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.88 },
        { timestamp: 2000, value: 71.5, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.85 },
        { timestamp: 3000, value: 63.9, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.90 }
      ],
      aggregation_state: "raw"
    }
    
    let memory_metrics = MetricSeries {
      name: "memory_usage",
      points: [
        { timestamp: 1000, value: 1024.5, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.97 },
        { timestamp: 2000, value: 1089.2, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.94 },
        { timestamp: 3000, value: 998.7, dimensions: [("service", "api"), ("host", "server1")], quality_score: 0.96 },
        { timestamp: 1000, value: 2048.1, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.91 },
        { timestamp: 2000, value: 2156.3, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.89 },
        { timestamp: 3000, value: 1987.6, dimensions: [("service", "worker"), ("host", "server2")], quality_score: 0.93 }
      ],
      aggregation_state: "raw"
    }
    
    [cpu_metrics, memory_metrics]
  }
  
  let metrics = create_test_metrics()
  
  // 实现维度聚合函数
  let aggregate_by_dimension = fn(series: MetricSeries, dimension_key: String) {
    let mut groups = []
    
    // 按维度值分组
    for point in series.points {
      let dimension_value = match series.points.find(fn(p) { p == point }) {
        Some(p) => {
          match p.dimensions.find(fn(d) { d.0 == dimension_key }) {
            Some(d) => d.1
            None => "unknown"
          }
        }
        None => "unknown"
      }
      
      let existing_group = groups.find(fn(g) { g.0 == dimension_value })
      match existing_group {
        Some(group) => {
          // 更新现有组
          let updated_points = group.1.push(point)
          groups = groups.map(fn(g) {
            if g.0 == dimension_value {
              (g.0, updated_points)
            } else {
              g
            }
          })
        }
        None => {
          // 创建新组
          groups = groups.push((dimension_value, [point]))
        }
      }
    }
    
    // 计算每组的统计信息
    let aggregated = groups.map(fn(g) {
      let points = g.1
      let values = points.map(fn(p) { p.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / values.length().to_float()
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      
      let avg_quality = points.map(fn(p) { p.quality_score })
        .reduce(fn(acc, q) { acc + q }, 0.0) / points.length().to_float()
      
      {
        dimension_value: g.0,
        count: points.length(),
        average: avg,
        maximum: max,
        minimum: min,
        quality_score: avg_quality
      }
    })
    
    aggregated
  }
  
  // 测试CPU指标按服务维度聚合
  let cpu_by_service = aggregate_by_dimension(metrics[0], "service")
  assert_eq(cpu_by_service.length(), 2)
  
  let api_service = cpu_by_service.find(fn(r) { r.dimension_value == "api" })
  match api_service {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.average, (45.2 + 52.8 + 38.1) / 3.0)
      assert_eq(result.maximum, 52.8)
      assert_eq(result.minimum, 38.1)
      assert_eq(result.quality_score, (0.95 + 0.92 + 0.98) / 3.0)
    }
    None => assert_true(false)
  }
  
  let worker_service = cpu_by_service.find(fn(r) { r.dimension_value == "worker" })
  match worker_service {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.average, (67.3 + 71.5 + 63.9) / 3.0)
      assert_eq(result.maximum, 71.5)
      assert_eq(result.minimum, 63.9)
    }
    None => assert_true(false)
  }
  
  // 测试内存指标按主机维度聚合
  let memory_by_host = aggregate_by_dimension(metrics[1], "host")
  assert_eq(memory_by_host.length(), 2)
  
  let server1_host = memory_by_host.find(fn(r) { r.dimension_value == "server1" })
  match server1_host {
    Some(result) => {
      assert_eq(result.count, 3)
      assert_eq(result.average, (1024.5 + 1089.2 + 998.7) / 3.0)
    }
    None => assert_true(false)
  }
}

// 测试2: 时间序列数据趋势分析
test "时间序列数据趋势分析" {
  // 定义时间序列点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    anomaly_score: Float
  }
  
  type TrendAnalysis = {
    direction: String, // "increasing", "decreasing", "stable"
    slope: Float,
    confidence: Float,
    seasonal_pattern: Bool
  }
  
  // 创建测试数据 - 上升趋势
  let increasing_series = [
    { timestamp: 1000, value: 10.5, anomaly_score: 0.1 },
    { timestamp: 2000, value: 15.2, anomaly_score: 0.1 },
    { timestamp: 3000, value: 18.7, anomaly_score: 0.1 },
    { timestamp: 4000, value: 24.1, anomaly_score: 0.1 },
    { timestamp: 5000, value: 29.8, anomaly_score: 0.1 },
    { timestamp: 6000, value: 35.3, anomaly_score: 0.1 }
  ]
  
  // 创建测试数据 - 下降趋势
  let decreasing_series = [
    { timestamp: 1000, value: 85.2, anomaly_score: 0.1 },
    { timestamp: 2000, value: 78.9, anomaly_score: 0.1 },
    { timestamp: 3000, value: 71.4, anomaly_score: 0.1 },
    { timestamp: 4000, value: 65.7, anomaly_score: 0.1 },
    { timestamp: 5000, value: 59.1, anomaly_score: 0.1 },
    { timestamp: 6000, value: 52.6, anomaly_score: 0.1 }
  ]
  
  // 创建测试数据 - 稳定趋势
  let stable_series = [
    { timestamp: 1000, value: 50.1, anomaly_score: 0.1 },
    { timestamp: 2000, value: 49.8, anomaly_score: 0.1 },
    { timestamp: 3000, value: 50.3, anomaly_score: 0.1 },
    { timestamp: 4000, value: 49.9, anomaly_score: 0.1 },
    { timestamp: 5000, value: 50.2, anomaly_score: 0.1 },
    { timestamp: 6000, value: 50.0, anomaly_score: 0.1 }
  ]
  
  // 实现趋势分析函数
  let analyze_trend = fn(series: Array[TimeSeriesPoint]) {
    if series.length() < 3 {
      return {
        direction: "insufficient_data",
        slope: 0.0,
        confidence: 0.0,
        seasonal_pattern: false
      }
    }
    
    // 计算简单线性回归斜率
    let n = series.length().to_float()
    let sum_x = series.map(fn(p) { p.timestamp.to_float() }).reduce(fn(acc, x) { acc + x }, 0.0)
    let sum_y = series.map(fn(p) { p.value }).reduce(fn(acc, y) { acc + y }, 0.0)
    let sum_xy = series.map(fn(p) { p.timestamp.to_float() * p.value }).reduce(fn(acc, xy) { acc + xy }, 0.0)
    let sum_x2 = series.map(fn(p) { p.timestamp.to_float() * p.timestamp.to_float() }).reduce(fn(acc, x2) { acc + x2 }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    // 确定趋势方向
    let direction = if slope > 0.5 {
      "increasing"
    } else if slope < -0.5 {
      "decreasing"
    } else {
      "stable"
    }
    
    // 计算R²作为置信度
    let mean_y = sum_y / n
    let ss_tot = series.map(fn(p) { 
      let diff = p.value - mean_y
      diff * diff 
    }).reduce(fn(acc, ss) { acc + ss }, 0.0)
    
    let y_pred = fn(x: Float) {
      let intercept = (sum_y - slope * sum_x) / n
      intercept + slope * x
    }
    
    let ss_res = series.map(fn(p) {
      let predicted = y_pred(p.timestamp.to_float())
      let diff = p.value - predicted
      diff * diff
    }).reduce(fn(acc, ss) { acc + ss }, 0.0)
    
    let r_squared = if ss_tot > 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    let confidence = if r_squared < 0.0 { 0.0 } else if r_squared > 1.0 { 1.0 } else { r_squared }
    
    // 简单的季节性检测
    let has_seasonality = fn(points: Array[TimeSeriesPoint]) {
      if points.length() < 6 {
        false
      } else {
        // 简单的周期性检测：检查是否有重复的模式
        let first_half = points.slice(0, points.length() / 2)
        let second_half = points.slice(points.length() / 2, points.length() - points.length() / 2)
        
        let first_avg = first_half.map(fn(p) { p.value }).reduce(fn(acc, v) { acc + v }, 0.0) / first_half.length().to_float()
        let second_avg = second_half.map(fn(p) { p.value }).reduce(fn(acc, v) { acc + v }, 0.0) / second_half.length().to_float()
        
        // 如果两半的平均值相似，可能存在季节性
        @abs(first_avg - second_avg) < 5.0
      }
    }
    
    {
      direction,
      slope,
      confidence,
      seasonal_pattern: has_seasonality(series)
    }
  }
  
  // 测试上升趋势分析
  let increasing_trend = analyze_trend(increasing_series)
  assert_eq(increasing_trend.direction, "increasing")
  assert_true(increasing_trend.slope > 0.0)
  assert_true(increasing_trend.confidence > 0.5)
  assert_false(increasing_trend.seasonal_pattern)
  
  // 测试下降趋势分析
  let decreasing_trend = analyze_trend(decreasing_series)
  assert_eq(decreasing_trend.direction, "decreasing")
  assert_true(decreasing_trend.slope < 0.0)
  assert_true(decreasing_trend.confidence > 0.5)
  assert_false(decreasing_trend.seasonal_pattern)
  
  // 测试稳定趋势分析
  let stable_trend = analyze_trend(stable_series)
  assert_eq(stable_trend.direction, "stable")
  assert_true(@abs(stable_trend.slope) < 0.5)
  assert_true(stable_trend.confidence > 0.5)
  assert_false(stable_trend.seasonal_pattern)
}

// 测试3: 异常检测算法
test "异常检测算法" {
  // 定义异常检测结果
  type AnomalyResult = {
    is_anomaly: Bool,
    anomaly_score: Float,
    threshold: Float,
    method: String
  }
  
  // 创建测试数据 - 包含异常点
  let normal_data = [
    10.5, 11.2, 10.8, 11.5, 10.9, 11.1, 10.7, 11.3, 10.6, 11.0
  ]
  
  let anomaly_data = [
    10.5, 11.2, 10.8, 25.7, 10.9, 11.1, 10.7, 11.3, 10.6, 11.0
  ]
  
  // 实现统计异常检测（Z-Score方法）
  let detect_anomaly_zscore = fn(value: Float, data: Array[Float], threshold: Float) {
    if data.length() < 3 {
      return {
        is_anomaly: false,
        anomaly_score: 0.0,
        threshold,
        method: "zscore"
      }
    }
    
    let mean = data.reduce(fn(acc, v) { acc + v }, 0.0) / data.length().to_float()
    let variance = data.map(fn(v) {
      let diff = v - mean
      diff * diff
    }).reduce(fn(acc, v) { acc + v }, 0.0) / data.length().to_float()
    let std_dev = @sqrt(variance)
    
    let z_score = if std_dev > 0.0 { @abs(value - mean) / std_dev } else { 0.0 }
    
    {
      is_anomaly: z_score > threshold,
      anomaly_score: z_score,
      threshold,
      method: "zscore"
    }
  }
  
  // 实现IQR（四分位距）异常检测
  let detect_anomaly_iqr = fn(value: Float, data: Array[Float], multiplier: Float) {
    if data.length() < 4 {
      return {
        is_anomaly: false,
        anomaly_score: 0.0,
        threshold: multiplier,
        method: "iqr"
      }
    }
    
    // 排序数据
    let sorted_data = data.sort(fn(a, b) { a <= b })
    let n = sorted_data.length()
    
    // 计算四分位数
    let q1_index = n / 4
    let q3_index = (3 * n) / 4
    let q1 = sorted_data[q1_index]
    let q3 = sorted_data[q3_index]
    let iqr = q3 - q1
    
    let lower_bound = q1 - multiplier * iqr
    let upper_bound = q3 + multiplier * iqr
    
    let is_anomaly = value < lower_bound or value > upper_bound
    let anomaly_score = if is_anomaly {
      if value < lower_bound {
        (lower_bound - value) / iqr
      } else {
        (value - upper_bound) / iqr
      }
    } else {
      0.0
    }
    
    {
      is_anomaly,
      anomaly_score,
      threshold: multiplier,
      method: "iqr"
    }
  }
  
  // 测试正常数据点
  let normal_result_zscore = detect_anomaly_zscore(11.0, normal_data, 2.0)
  assert_false(normal_result_zscore.is_anomaly)
  assert_true(normal_result_zscore.anomaly_score < 2.0)
  
  let normal_result_iqr = detect_anomaly_iqr(11.0, normal_data, 1.5)
  assert_false(normal_result_iqr.is_anomaly)
  assert_eq(normal_result_iqr.anomaly_score, 0.0)
  
  // 测试异常数据点
  let anomaly_result_zscore = detect_anomaly_zscore(25.7, normal_data, 2.0)
  assert_true(anomaly_result_zscore.is_anomaly)
  assert_true(anomaly_result_zscore.anomaly_score > 2.0)
  assert_eq(anomaly_result_zscore.method, "zscore")
  
  let anomaly_result_iqr = detect_anomaly_iqr(25.7, normal_data, 1.5)
  assert_true(anomaly_result_iqr.is_anomaly)
  assert_true(anomaly_result_iqr.anomaly_score > 0.0)
  assert_eq(anomaly_result_iqr.method, "iqr")
  
  // 批量异常检测
  let batch_anomaly_detection = fn(data: Array[Float], method: String, threshold: Float) {
    data.map(fn(value) {
      if method == "zscore" {
        detect_anomaly_zscore(value, data, threshold)
      } else if method == "iqr" {
        detect_anomaly_iqr(value, data, threshold)
      } else {
        { is_anomaly: false, anomaly_score: 0.0, threshold, method: "unknown" }
      }
    })
  }
  
  // 测试批量检测
  let batch_results_zscore = batch_anomaly_detection(anomaly_data, "zscore", 2.0)
  let anomaly_count = batch_results_zscore.filter(fn(r) { r.is_anomaly }).length()
  assert_eq(anomaly_count, 1) // 只有一个异常点
  
  let anomaly_index = batch_results_zscore.index_of(fn(r) { r.is_anomaly })
  assert_eq(anomaly_index, 3) // 异常点在索引3处
}

// 测试4: 遥测量数据质量评估
test "遥测量数据质量评估" {
  // 定义数据质量指标
  type DataQualityMetrics = {
    completeness: Float,    // 完整性 (0-1)
    accuracy: Float,        // 准确性 (0-1)
    consistency: Float,     // 一致性 (0-1)
    timeliness: Float,      // 及时性 (0-1)
    validity: Float,        // 有效性 (0-1)
    overall_score: Float    // 总体评分 (0-1)
  }
  
  // 定义遥测量点
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    source: String,
    quality_flags: Array[String]
  }
  
  // 创建测试数据集
  let create_test_dataset = fn() {
    // 高质量数据集
    let high_quality_data = [
      { timestamp: 1000, metric_name: "cpu_usage", value: 45.2, source: "server1", quality_flags: [] },
      { timestamp: 2000, metric_name: "cpu_usage", value: 52.8, source: "server1", quality_flags: [] },
      { timestamp: 3000, metric_name: "cpu_usage", value: 38.1, source: "server1", quality_flags: [] },
      { timestamp: 4000, metric_name: "cpu_usage", value: 47.5, source: "server1", quality_flags: [] },
      { timestamp: 5000, metric_name: "cpu_usage", value: 51.3, source: "server1", quality_flags: [] }
    ]
    
    // 低质量数据集
    let low_quality_data = [
      { timestamp: 1000, metric_name: "cpu_usage", value: 45.2, source: "server1", quality_flags: [] },
      { timestamp: 0, metric_name: "cpu_usage", value: -1.0, source: "server1", quality_flags: ["invalid_value"] },
      { timestamp: 3000, metric_name: "memory_usage", value: 38.1, source: "server1", quality_flags: [] },
      { timestamp: 0, metric_name: "cpu_usage", value: 0.0, source: "", quality_flags: ["missing_timestamp", "missing_source"] },
      { timestamp: 5000, metric_name: "cpu_usage", value: 151.3, source: "server1", quality_flags: ["out_of_range"] }
    ]
    
    (high_quality_data, low_quality_data)
  }
  
  let (high_quality, low_quality) = create_test_dataset()
  
  // 实现数据质量评估函数
  let assess_data_quality = fn(data: Array[TelemetryPoint]) {
    if data.length() == 0 {
      return {
        completeness: 0.0,
        accuracy: 0.0,
        consistency: 0.0,
        timeliness: 0.0,
        validity: 0.0,
        overall_score: 0.0
      }
    }
    
    // 完整性评估 - 检查缺失值
    let completeness_issues = data.filter(fn(point) {
      point.timestamp == 0 or point.source == "" or point.metric_name == ""
    }).length()
    
    let completeness = 1.0 - (completeness_issues.to_float() / data.length().to_float())
    
    // 准确性评估 - 检查无效值和异常值
    let accuracy_issues = data.filter(fn(point) {
      point.value < 0.0 or point.quality_flags.contains("invalid_value")
    }).length()
    
    let accuracy = 1.0 - (accuracy_issues.to_float() / data.length().to_float())
    
    // 一致性评估 - 检查指标名称一致性
    let metric_names = data.map(fn(p) { p.metric_name })
    let unique_metrics = metric_names.filter(fn(name, index) {
      metric_names.index_of(fn(n) { n == name }) == index
    })
    
    let consistency = if unique_metrics.length() <= 1 {
      1.0
    } else {
      // 计算主要指标的占比
      let primary_metric = unique_metrics.reduce(fn(acc, name) {
        let acc_count = data.filter(fn(p) { p.metric_name == acc }).length()
        let name_count = data.filter(fn(p) { p.metric_name == name }).length()
        if name_count > acc_count { name } else { acc }
      }, unique_metrics[0])
      
      let primary_count = data.filter(fn(p) { p.metric_name == primary_metric }).length()
      primary_count.to_float() / data.length().to_float()
    }
    
    // 及时性评估 - 检查时间戳的时效性
    let current_time = 5000  // 假设当前时间
    let timeliness_issues = data.filter(fn(point) {
      point.timestamp == 0 or (current_time - point.timestamp) > 10000
    }).length()
    
    let timeliness = 1.0 - (timeliness_issues.to_float() / data.length().to_float())
    
    // 有效性评估 - 检查数据范围和类型
    let validity_issues = data.filter(fn(point) {
      point.quality_flags.contains("out_of_range") or 
      point.quality_flags.contains("invalid_type")
    }).length()
    
    let validity = 1.0 - (validity_issues.to_float() / data.length().to_float())
    
    // 计算总体评分
    let overall_score = (completeness + accuracy + consistency + timeliness + validity) / 5.0
    
    {
      completeness,
      accuracy,
      consistency,
      timeliness,
      validity,
      overall_score
    }
  }
  
  // 测试高质量数据集
  let high_quality_result = assess_data_quality(high_quality)
  assert_true(high_quality_result.completeness > 0.9)
  assert_true(high_quality_result.accuracy > 0.9)
  assert_eq(high_quality_result.consistency, 1.0)
  assert_true(high_quality_result.timeliness > 0.9)
  assert_true(high_quality_result.validity > 0.9)
  assert_true(high_quality_result.overall_score > 0.9)
  
  // 测试低质量数据集
  let low_quality_result = assess_data_quality(low_quality)
  assert_true(low_quality_result.completeness < 0.9)
  assert_true(low_quality_result.accuracy < 0.9)
  assert_true(low_quality_result.consistency < 1.0)
  assert_true(low_quality_result.timeliness < 0.9)
  assert_true(low_quality_result.validity < 0.9)
  assert_true(low_quality_result.overall_score < 0.9)
  
  // 验证低质量数据的具体问题
  assert_true(low_quality_result.completeness < 1.0) // 有缺失的时间戳和源
  assert_true(low_quality_result.accuracy < 1.0)     // 有无效值
  assert_true(low_quality_result.consistency < 1.0)  // 有不一致的指标名称
  assert_true(low_quality_result.validity < 1.0)     // 有超出范围的值
}

// 测试5: 遥测量数据关联分析
test "遥测量数据关联分析" {
  // 定义关联分析结果
  type CorrelationResult = {
    metric_a: String,
    metric_b: String,
    correlation_coefficient: Float,
    correlation_strength: String, // "strong", "moderate", "weak", "none"
    p_value: Float,
    significant: Bool
  }
  
  // 定义多指标时间序列数据点
  type MultiMetricPoint = {
    timestamp: Int,
    metrics: Array[(String, Float)]
  }
  
  // 创建测试数据 - 正相关
  let positive_correlation_data = [
    { timestamp: 1000, metrics: [("cpu_usage", 20.0), ("response_time", 100.0), ("error_rate", 0.5)] },
    { timestamp: 2000, metrics: [("cpu_usage", 30.0), ("response_time", 150.0), ("error_rate", 0.8)] },
    { timestamp: 3000, metrics: [("cpu_usage", 40.0), ("response_time", 200.0), ("error_rate", 1.2)] },
    { timestamp: 4000, metrics: [("cpu_usage", 50.0), ("response_time", 250.0), ("error_rate", 1.5)] },
    { timestamp: 5000, metrics: [("cpu_usage", 60.0), ("response_time", 300.0), ("error_rate", 1.8)] }
  ]
  
  // 创建测试数据 - 负相关
  let negative_correlation_data = [
    { timestamp: 1000, metrics: [("cpu_usage", 80.0), ("throughput", 1000.0), ("cache_hit_rate", 0.9)] },
    { timestamp: 2000, metrics: [("cpu_usage", 70.0), ("throughput", 1200.0), ("cache_hit_rate", 0.85)] },
    { timestamp: 3000, metrics: [("cpu_usage", 60.0), ("throughput", 1400.0), ("cache_hit_rate", 0.8)] },
    { timestamp: 4000, metrics: [("cpu_usage", 50.0), ("throughput", 1600.0), ("cache_hit_rate", 0.75)] },
    { timestamp: 5000, metrics: [("cpu_usage", 40.0), ("throughput", 1800.0), ("cache_hit_rate", 0.7)] }
  ]
  
  // 实现相关系数计算
  let calculate_correlation = fn(data: Array[MultiMetricPoint], metric_a: String, metric_b: String) {
    // 提取两个指标的时间序列
    let series_a = data.filter_map(fn(point) {
      match point.metrics.find(fn(m) { m.0 == metric_a }) {
        Some(metric) => Some(metric.1)
        None => None
      }
    })
    
    let series_b = data.filter_map(fn(point) {
      match point.metrics.find(fn(m) { m.0 == metric_b }) {
        Some(metric) => Some(metric.1)
        None => None
      }
    })
    
    if series_a.length() != series_b.length() or series_a.length() < 3 {
      return {
        metric_a,
        metric_b,
        correlation_coefficient: 0.0,
        correlation_strength: "none",
        p_value: 1.0,
        significant: false
      }
    }
    
    let n = series_a.length().to_float()
    
    // 计算均值
    let mean_a = series_a.reduce(fn(acc, v) { acc + v }, 0.0) / n
    let mean_b = series_b.reduce(fn(acc, v) { acc + v }, 0.0) / n
    
    // 计算协方差和方差
    let covariance = series_a.map(fn(a, i) {
      let b = series_b[i]
      (a - mean_a) * (b - mean_b)
    }).reduce(fn(acc, v) { acc + v }, 0.0)
    
    let variance_a = series_a.map(fn(a) {
      let diff = a - mean_a
      diff * diff
    }).reduce(fn(acc, v) { acc + v }, 0.0)
    
    let variance_b = series_b.map(fn(b) {
      let diff = b - mean_b
      diff * diff
    }).reduce(fn(acc, v) { acc + v }, 0.0)
    
    // 计算皮尔逊相关系数
    let correlation = if variance_a > 0.0 and variance_b > 0.0 {
      covariance / (@sqrt(variance_a) * @sqrt(variance_b))
    } else {
      0.0
    }
    
    // 确定相关强度
    let abs_correlation = @abs(correlation)
    let correlation_strength = if abs_correlation >= 0.7 {
      "strong"
    } else if abs_correlation >= 0.3 {
      "moderate"
    } else if abs_correlation >= 0.1 {
      "weak"
    } else {
      "none"
    }
    
    // 简化的p值计算（实际应用中应使用更精确的统计检验）
    let t_statistic = abs_correlation * @sqrt((n - 2.0) / (1.0 - abs_correlation * abs_correlation))
    let p_value = if t_statistic > 0.0 {
      1.0 / (1.0 + t_statistic)  // 简化计算
    } else {
      1.0
    }
    
    let significant = p_value < 0.05 and abs_correlation > 0.1
    
    {
      metric_a,
      metric_b,
      correlation_coefficient: correlation,
      correlation_strength,
      p_value,
      significant
    }
  }
  
  // 测试正相关
  let positive_corr = calculate_correlation(positive_correlation_data, "cpu_usage", "response_time")
  assert_true(positive_corr.correlation_coefficient > 0.8)
  assert_eq(positive_corr.correlation_strength, "strong")
  assert_true(positive_corr.significant)
  
  // 测试负相关
  let negative_corr = calculate_correlation(negative_correlation_data, "cpu_usage", "throughput")
  assert_true(negative_corr.correlation_coefficient < -0.8)
  assert_eq(negative_corr.correlation_strength, "strong")
  assert_true(negative_corr.significant)
  
  // 测试弱相关或无相关
  let weak_corr = calculate_correlation(positive_correlation_data, "response_time", "error_rate")
  // 由于数据是线性增长的，这些指标也会有相关性，但可能不如CPU和响应时间之间的相关性强
  assert_true(@abs(weak_corr.correlation_coefficient) > 0.5)
  
  // 批量关联分析
  let batch_correlation_analysis = fn(data: Array[MultiMetricPoint]) {
    // 提取所有指标名称
    let all_metrics = data.fold(fn(acc, point) {
      let point_metrics = point.metrics.map(fn(m) { m.0 })
      let unique_point_metrics = point_metrics.filter(fn(metric) {
        not(acc.contains(metric))
      })
      acc + unique_point_metrics
    }, [])
    
    // 计算所有指标对的关联
    let mut correlations = []
    for i in 0..all_metrics.length() {
      for j in (i + 1)..all_metrics.length() {
        let metric_a = all_metrics[i]
        let metric_b = all_metrics[j]
        let correlation = calculate_correlation(data, metric_a, metric_b)
        correlations = correlations.push(correlation)
      }
    }
    
    // 按相关系数绝对值排序
    correlations.sort(fn(a, b) {
      @abs(a.correlation_coefficient) >= @abs(b.correlation_coefficient)
    })
  }
  
  // 测试批量关联分析
  let positive_correlations = batch_correlation_analysis(positive_correlation_data)
  assert_true(positive_correlations.length() > 0)
  
  // 最强的相关应该是CPU使用率和响应时间
  let strongest = positive_correlations[0]
  assert_eq(strongest.metric_a, "cpu_usage")
  assert_eq(strongest.metric_b, "response_time")
  assert_eq(strongest.correlation_strength, "strong")
  
  // 测试负相关数据的批量分析
  let negative_correlations = batch_correlation_analysis(negative_correlation_data)
  assert_true(negative_correlations.length() > 0)
  
  // 最强的相关应该是CPU使用率和吞吐量（负相关）
  let strongest_negative = negative_correlations[0]
  assert_true(strongest_negative.correlation_coefficient < -0.8)
  assert_eq(strongest_negative.correlation_strength, "strong")
}