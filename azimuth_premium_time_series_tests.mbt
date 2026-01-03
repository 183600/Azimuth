// Azimuth Premium Time Series Data Processing Tests
// 高级时间序列数据处理测试用例，专注于遥测时间序列数据的分析和处理

// 测试 1: 时间序列数据结构
test "time series data structures" {
  // 定义时间点
  type TimePoint = {
    timestamp: Int,
    value: Float,
    metadata: Map[String, String]
  }
  
  // 定义时间序列
  type TimeSeries = {
    name: String,
    unit: String,
    points: Array[TimePoint],
    start_time: Int,
    end_time: Int,
    tags: Map[String, String]
  }
  
  // 创建时间点
  let create_time_point = fn(timestamp: Int, value: Float, metadata: Map[String, String>) {
    {
      timestamp,
      value,
      metadata
    }
  }
  
  // 创建时间序列
  let create_time_series = fn(name: String, unit: String, tags: Map[String, String>) {
    {
      name,
      unit,
      points: [],
      start_time: 0,
      end_time: 0,
      tags
    }
  }
  
  // 添加时间点
  let add_point = fn(series: TimeSeries, point: TimePoint) {
    let updated_points = series.points.push(point)
    
    let new_start_time = if series.points.length() == 0 {
      point.timestamp
    } else {
      series.start_time.min(point.timestamp)
    }
    
    let new_end_time = if series.points.length() == 0 {
      point.timestamp
    } else {
      series.end_time.max(point.timestamp)
    }
    
    {
      name: series.name,
      unit: series.unit,
      points: updated_points,
      start_time: new_start_time,
      end_time: new_end_time,
      tags: series.tags
    }
  }
  
  // 测试时间序列数据结构
  let series = create_time_series("cpu.usage", "percent", Map::from_array([("host", "server1"), ("region", "us-west")]))
  
  // 添加时间点
  let point1 = create_time_point(1000, 45.5, Map::empty())
  let point2 = create_time_point(2000, 52.3, Map::from_array([("source", "system")]))
  let point3 = create_time_point(3000, 48.7, Map::empty())
  
  let series1 = add_point(series, point1)
  let series2 = add_point(series1, point2)
  let series3 = add_point(series2, point3)
  
  // 验证时间序列
  assert_eq(series3.name, "cpu.usage")
  assert_eq(series3.unit, "percent")
  assert_eq(series3.points.length(), 3)
  assert_eq(series3.start_time, 1000)
  assert_eq(series3.end_time, 3000)
  assert_eq(Map::get(series3.tags, "host"), Some("server1"))
  assert_eq(Map::get(series3.tags, "region"), Some("us-west"))
  
  // 验证时间点
  assert_eq(series3.points[0].timestamp, 1000)
  assert_eq(series3.points[0].value, 45.5)
  assert_eq(series3.points[1].timestamp, 2000)
  assert_eq(series3.points[1].value, 52.3)
  assert_eq(Map::get(series3.points[1].metadata, "source"), Some("system"))
}

// 测试 2: 时间序列聚合
test "time series aggregation" {
  // 定义时间点
  type TimePoint = {
    timestamp: Int,
    value: Float
  }
  
  // 定义聚合结果
  type AggregationResult = {
    timestamp: Int,
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    variance: Float
  }
  
  // 创建时间序列聚合器
  let create_time_series_aggregator = fn() {
    // 按时间窗口聚合
    let aggregate_by_window = fn(points: Array[TimePoint], window_size: Int) {
      if points.length() == 0 {
        return []
      }
      
      // 按时间戳排序
      let sorted_points = points.sort(fn(a, b) { 
        if a.timestamp < b.timestamp { -1 } 
        else if a.timestamp > b.timestamp { 1 } 
        else { 0 } 
      })
      
      let mut results = []
      let mut window_start = sorted_points[0].timestamp
      let mut window_end = window_start + window_size
      let mut window_points = []
      
      for point in sorted_points {
        if point.timestamp >= window_start && point.timestamp < window_end {
          // 在当前窗口内
          window_points = window_points.push(point)
        } else {
          // 超出当前窗口，处理当前窗口
          if window_points.length() > 0 {
            let aggregation = calculate_aggregation(window_points, window_start + window_size / 2)
            results = results.push(aggregation)
          }
          
          // 开始新窗口
          window_start = (point.timestamp / window_size) * window_size
          window_end = window_start + window_size
          window_points = [point]
        }
      }
      
      // 处理最后一个窗口
      if window_points.length() > 0 {
        let aggregation = calculate_aggregation(window_points, window_start + window_size / 2)
        results = results.push(aggregation)
      }
      
      results
    }
    
    // 计算聚合统计
    let calculate_aggregation = fn(window_points: Array[TimePoint], timestamp: Int) {
      let count = window_points.length()
      let values = window_points.map(fn(p) { p.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (count as Float)
      
      let min = values.reduce(fn(acc, v) { acc.min(v) }, values[0])
      let max = values.reduce(fn(acc, v) { acc.max(v) }, values[0])
      
      // 计算方差
      let variance = if count > 1 {
        let variance_sum = values.reduce(fn(acc, v) { acc + (v - avg) * (v - avg) }, 0.0)
        variance_sum / ((count - 1) as Float)
      } else {
        0.0
      }
      
      {
        timestamp,
        count,
        sum,
        min,
        max,
        avg,
        variance
      }
    }
    
    // 下采样
    let downsample = fn(points: Array[TimePoint], target_count: Int) {
      if points.length() <= target_count {
        return points
      }
      
      let sorted_points = points.sort(fn(a, b) { 
        if a.timestamp < b.timestamp { -1 } 
        else if a.timestamp > b.timestamp { 1 } 
        else { 0 } 
      })
      
      let step = sorted_points.length() / target_count
      let mut result = []
      
      for i in 0..target_count {
        let index = i * step
        if index < sorted_points.length() {
          result = result.push(sorted_points[index])
        }
      }
      
      result
    }
    
    {
      aggregate_by_window,
      downsample
    }
  }
  
  // 测试时间序列聚合
  let aggregator = create_time_series_aggregator()
  
  // 创建测试数据
  let points = [
    { timestamp: 1000, value: 10.5 },
    { timestamp: 1500, value: 12.3 },
    { timestamp: 2000, value: 11.8 },
    { timestamp: 2500, value: 13.2 },
    { timestamp: 3000, value: 14.5 },
    { timestamp: 3500, value: 12.9 },
    { timestamp: 4000, value: 15.1 },
    { timestamp: 4500, value: 13.7 },
    { timestamp: 5000, value: 16.2 },
    { timestamp: 5500, value: 14.8 }
  ]
  
  // 测试窗口聚合
  let windowed_results = aggregator.aggregate_by_window(points, 2000)  // 2秒窗口
  
  // 验证聚合结果
  assert_eq(windowed_results.length(), 3)  // 10秒数据，2秒窗口，应该有5个窗口，但有些可能为空
  
  // 验证第一个窗口 (1000-3000)
  let first_window = windowed_results[0]
  assert_eq(first_window.timestamp, 2000)  // 窗口中点
  assert_eq(first_window.count, 4)  // 1000, 1500, 2000, 2500
  assert_eq(first_window.sum.round(), 47.8)  // 10.5 + 12.3 + 11.8 + 13.2
  assert_eq(first_window.min, 10.5)
  assert_eq(first_window.max, 13.2)
  assert_eq(first_window.avg.round(), 12.0)  // 47.8 / 4
  
  // 测试下采样
  let downsampled = aggregator.downsample(points, 3)
  
  // 验证下采样结果
  assert_eq(downsampled.length(), 3)
  assert_eq(downsampled[0].timestamp, 1000)
  assert_eq(downsampled[1].timestamp, 3000)
  assert_eq(downsampled[2].timestamp, 5000)
}

// 测试 3: 时间序列查询
test "time series querying" {
  // 定义时间点
  type TimePoint = {
    timestamp: Int,
    value: Float,
    tags: Map[String, String]
  }
  
  // 定义时间序列
  type TimeSeries = {
    name: String,
    points: Array[TimePoint]
  }
  
  // 创建查询器
  let create_time_series_queryer = fn() {
    // 时间范围查询
    let query_by_time_range = fn(series: TimeSeries, start_time: Int, end_time: Int) {
      series.points.filter(fn(point) { 
        point.timestamp >= start_time && point.timestamp <= end_time 
      })
    }
    
    // 标签查询
    let query_by_tags = fn(series: TimeSeries, tag_filters: Map[String, String>) {
      series.points.filter(fn(point) {
        tag_filters.all(fn(key, value) {
          match Map::get(point.tags, key) {
            Some(point_value) => point_value == value
            None => false
          }
        })
      })
    }
    
    // 值范围查询
    let query_by_value_range = fn(series: TimeSeries, min_value: Float, max_value: Float) {
      series.points.filter(fn(point) { 
        point.value >= min_value && point.value <= max_value 
      })
    }
    
    // 复合查询
    let query = fn(series: TimeSeries, time_range: Option[(Int, Int)], 
                   tag_filters: Map[String, String], value_range: Option[(Float, Float)]) {
      let mut results = series.points
      
      // 应用时间范围过滤
      match time_range {
        Some((start, end)) => {
          results = results.filter(fn(point) { 
            point.timestamp >= start && point.timestamp <= end 
          })
        }
        None => {}
      }
      
      // 应用标签过滤
      if tag_filters.size() > 0 {
        results = results.filter(fn(point) {
          tag_filters.all(fn(key, value) {
            match Map::get(point.tags, key) {
              Some(point_value) => point_value == value
              None => false
            }
          })
        })
      }
      
      // 应用值范围过滤
      match value_range {
        Some((min, max)) => {
          results = results.filter(fn(point) { 
            point.value >= min && point.value <= max 
          })
        }
        None => {}
      }
      
      results
    }
    
    {
      query_by_time_range,
      query_by_tags,
      query_by_value_range,
      query
    }
  }
  
  // 测试时间序列查询
  let queryer = create_time_series_queryer()
  
  // 创建测试数据
  let points = [
    { timestamp: 1000, value: 10.5, tags: Map::from_array([("host", "server1"), ("region", "us-west")]) },
    { timestamp: 2000, value: 15.2, tags: Map::from_array([("host", "server1"), ("region", "us-east")]) },
    { timestamp: 3000, value: 12.8, tags: Map::from_array([("host", "server2"), ("region", "us-west")]) },
    { timestamp: 4000, value: 18.3, tags: Map::from_array([("host", "server2"), ("region", "us-east")]) },
    { timestamp: 5000, value: 14.7, tags: Map::from_array([("host", "server1"), ("region", "us-west")]) }
  ]
  
  let series = {
    name: "cpu.usage",
    points
  }
  
  // 测试时间范围查询
  let time_range_results = queryer.query_by_time_range(series, 2000, 4000)
  
  assert_eq(time_range_results.length(), 3)  // 2000, 3000, 4000
  assert_eq(time_range_results[0].timestamp, 2000)
  assert_eq(time_range_results[1].timestamp, 3000)
  assert_eq(time_range_results[2].timestamp, 4000)
  
  // 测试标签查询
  let tag_results = queryer.query_by_tags(series, Map::from_array([("host", "server1")]))
  
  assert_eq(tag_results.length(), 3)  // server1在1000, 2000, 5000
  assert_eq(tag_results[0].timestamp, 1000)
  assert_eq(tag_results[1].timestamp, 2000)
  assert_eq(tag_results[2].timestamp, 5000)
  
  // 测试值范围查询
  let value_results = queryer.query_by_value_range(series, 12.0, 15.0)
  
  assert_eq(value_results.length(), 3)  // 12.8, 14.7, 15.2(15.2 > 15.0，所以不包括)
  assert_eq(value_results[0].timestamp, 3000)
  assert_eq(value_results[1].timestamp, 5000)
  
  // 测试复合查询
  let composite_results = queryer(
    series, 
    Some((1500, 4500)),  // 时间范围
    Map::from_array([("region", "us-west")]),  // 标签过滤
    Some((10.0, 13.0))  // 值范围
  )
  
  assert_eq(composite_results.length(), 1)  // 只有3000的时间点满足所有条件
  assert_eq(composite_results[0].timestamp, 3000)
  assert_eq(composite_results[0].value, 12.8)
  assert_eq(Map::get(composite_results[0].tags, "host"), Some("server2"))
}

// 测试 4: 时间序列分析
test "time series analysis" {
  // 定义时间点
  type TimePoint = {
    timestamp: Int,
    value: Float
  }
  
  // 定义分析结果
  type AnalysisResult = {
    trend: String,  // "increasing", "decreasing", "stable"
    seasonality: Bool,
    outliers: Array[Int],  // 异常点的索引
    forecast: Array[TimePoint]  // 预测点
  }
  
  // 创建时间序列分析器
  let create_time_series_analyzer = fn() {
    // 趋势分析
    let analyze_trend = fn(points: Array[TimePoint]) {
      if points.length() < 2 {
        return "stable"
      }
      
      // 简单线性回归
      let n = points.length() as Float
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for i in 0..points.length() {
        let x = i as Float
        let y = points[i].value
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      
      if slope > 0.1 {
        "increasing"
      } else if slope < -0.1 {
        "decreasing"
      } else {
        "stable"
      }
    }
    
    // 异常检测
    let detect_outliers = fn(points: Array[TimePoint], threshold: Float) {
      if points.length() < 3 {
        return []
      }
      
      // 计算均值和标准差
      let values = points.map(fn(p) { p.value })
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / (values.length() as Float)
      let std_dev = variance.sqrt()
      
      // 检测异常点
      let mut outliers = []
      
      for i in 0..points.length() {
        let z_score = (points[i].value - mean).abs() / std_dev
        if z_score > threshold {
          outliers = outliers.push(i)
        }
      }
      
      outliers
    }
    
    // 简单预测（线性外推）
    let forecast = fn(points: Array[TimePoint], steps: Int) {
      if points.length() < 2 {
        return []
      }
      
      // 简单线性回归
      let n = points.length() as Float
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for i in 0..points.length() {
        let x = i as Float
        let y = points[i].value
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // 生成预测点
      let mut forecast_points = []
      let last_timestamp = points[points.length() - 1].timestamp
      let time_interval = if points.length() > 1 {
        points[1].timestamp - points[0].timestamp
      } else {
        1000  // 默认1秒间隔
      }
      
      for i in 1..=steps {
        let x = (points.length() + i) as Float
        let y = slope * x + intercept
        
        let forecast_point = {
          timestamp: last_timestamp + i * time_interval,
          value: y
        }
        
        forecast_points = forecast_points.push(forecast_point)
      }
      
      forecast_points
    }
    
    // 季节性检测
    let detect_seasonality = fn(points: Array[TimePoint], period: Int) {
      if points.length() < period * 2 {
        return false
      }
      
      // 简单自相关检测
      let mut correlation = 0.0
      let count = points.length() - period
      
      if count <= 0 {
        return false
      }
      
      let values = points.map(fn(p) { p.value })
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let mut numerator = 0.0
      let mut denominator = 0.0
      
      for i in 0..count {
        let deviation = values[i] - mean
        numerator = numerator + deviation * (values[i + period] - mean)
        denominator = denominator + deviation * deviation
      }
      
      if denominator > 0.0 {
        correlation = numerator / denominator
      }
      
      correlation.abs() > 0.5  // 阈值
    }
    
    // 综合分析
    let analyze = fn(points: Array[TimePoint]) {
      let trend = analyze_trend(points)
      let outliers = detect_outliers(points, 2.0)  // 2倍标准差阈值
      let forecast_points = forecast(points, 3)  // 预测3个点
      let seasonality = detect_seasonality(points, 5)  // 检测周期为5的季节性
      
      {
        trend,
        seasonality,
        outliers,
        forecast: forecast_points
      }
    }
    
    {
      analyze,
      analyze_trend,
      detect_outliers,
      forecast,
      detect_seasonality
    }
  }
  
  // 测试时间序列分析
  let analyzer = create_time_series_analyzer()
  
  // 创建测试数据（上升趋势）
  let increasing_points = [
    { timestamp: 1000, value: 10.0 },
    { timestamp: 2000, value: 12.0 },
    { timestamp: 3000, value: 14.5 },
    { timestamp: 4000, value: 16.0 },
    { timestamp: 5000, value: 18.5 },
    { timestamp: 6000, value: 20.0 }
  ]
  
  // 测试趋势分析
  let trend = analyzer.analyze_trend(increasing_points)
  assert_eq(trend, "increasing")
  
  // 创建测试数据（包含异常值）
  let outlier_points = [
    { timestamp: 1000, value: 10.0 },
    { timestamp: 2000, value: 12.0 },
    { timestamp: 3000, value: 50.0 },  // 异常值
    { timestamp: 4000, value: 14.0 },
    { timestamp: 5000, value: 16.0 }
  ]
  
  // 测试异常检测
  let outliers = analyzer.detect_outliers(outlier_points, 2.0)
  assert_eq(outliers.length(), 1)
  assert_eq(outliers[0], 2)  // 第3个点是异常
  
  // 测试预测
  let forecast_points = analyzer.forecast(increasing_points, 2)
  assert_eq(forecast_points.length(), 2)
  assert_true(forecast_points[0].timestamp > 6000)
  assert_true(forecast_points[0].value > 20.0)
  
  // 创建测试数据（季节性）
  let seasonal_points = [
    { timestamp: 1000, value: 10.0 },
    { timestamp: 2000, value: 15.0 },
    { timestamp: 3000, value: 20.0 },
    { timestamp: 4000, value: 15.0 },
    { timestamp: 5000, value: 10.0 },
    { timestamp: 6000, value: 15.0 },
    { timestamp: 7000, value: 20.0 },
    { timestamp: 8000, value: 15.0 },
    { timestamp: 9000, value: 10.0 }
  ]
  
  // 测试季节性检测
  let seasonality = analyzer.detect_seasonality(seasonal_points, 4)
  assert_true(seasonality)
  
  // 测试综合分析
  let analysis_result = analyzer.analyze(increasing_points)
  
  assert_eq(analysis_result.trend, "increasing")
  assert_eq(analysis_result.outliers.length(), 0)
  assert_eq(analysis_result.forecast.length(), 3)
}

// 测试 5: 时间序列压缩
test "time series compression" {
  // 定义时间点
  type TimePoint = {
    timestamp: Int,
    value: Float
  }
  
  // 创建时间序列压缩器
  let create_time_series_compressor = fn() {
    // 三角函数压缩（基于变化率）
    let triangular_compression = fn(points: Array[TimePoint], threshold: Float) {
      if points.length() <= 2 {
        return points
      }
      
      let mut compressed = [points[0]]  // 保留第一个点
      
      let mut last_kept = points[0]
      
      for i in 1..points.length() - 1 {
        let current = points[i]
        let next = points[i + 1]
        
        // 计算三角形面积
        let area = ((current.timestamp - last_kept.timestamp) * (next.value - last_kept.value) - 
                   (next.timestamp - last_kept.timestamp) * (current.value - last_kept.value)).abs() / 2.0
        
        if area > threshold {
          compressed = compressed.push(current)
          last_kept = current
        }
      }
      
      // 保留最后一个点
      compressed = compressed.push(points[points.length() - 1])
      
      compressed
    }
    
    // 增量编码压缩
    let delta_compression = fn(points: Array[TimePoint>) {
      if points.length() <= 1 {
        return points
      }
      
      let mut compressed = [points[0]]  // 第一个点保持不变
      
      for i in 1..points.length() {
        let prev = points[i - 1]
        let current = points[i]
        
        let delta_point = {
          timestamp: current.timestamp - prev.timestamp,
          value: current.value - prev.value
        }
        
        compressed = compressed.push(delta_point)
      }
      
      compressed
    }
    
    // 增量解码
    let delta_decompression = fn(compressed: Array[TimePoint>) {
      if compressed.length() <= 1 {
        return compressed
      }
      
      let mut decompressed = [compressed[0]]
      
      for i in 1..compressed.length() {
        let prev = decompressed[i - 1]
        let delta = compressed[i]
        
        let point = {
          timestamp: prev.timestamp + delta.timestamp,
          value: prev.value + delta.value
        }
        
        decompressed = decompressed.push(point)
      }
      
      decompressed
    }
    
    // 计算压缩率
    let calculate_compression_ratio = fn(original: Array[TimePoint], compressed: Array[TimePoint>) {
      let original_size = original.length() * 16  // 假设每个点16字节
      let compressed_size = compressed.length() * 16  // 简化计算
      
      (original_size - compressed_size) as Float / original_size as Float
    }
    
    {
      triangular_compression,
      delta_compression,
      delta_decompression,
      calculate_compression_ratio
    }
  }
  
  // 测试时间序列压缩
  let compressor = create_time_series_compressor()
  
  // 创建测试数据
  let points = [
    { timestamp: 1000, value: 10.0 },
    { timestamp: 2000, value: 10.1 },
    { timestamp: 3000, value: 10.2 },
    { timestamp: 4000, value: 15.0 },  // 显著变化
    { timestamp: 5000, value: 15.1 },
    { timestamp: 6000, value: 15.2 },
    { timestamp: 7000, value: 20.0 },  // 显著变化
    { timestamp: 8000, value: 20.1 },
    { timestamp: 9000, value: 20.2 }
  ]
  
  // 测试三角函数压缩
  let triangular_compressed = compressor.triangular_compression(points, 5.0)
  
  // 验证压缩结果
  assert_true(triangular_compressed.length() < points.length())
  assert_eq(triangular_compressed[0], points[0])  // 第一个点应该保留
  assert_eq(triangular_compressed[triangular_compressed.length() - 1], points[points.length() - 1])  // 最后一个点应该保留
  
  // 验证显著变化的点被保留
  let significant_change_indices = triangular_compressed.map(fn(p) { 
    points.find_index(fn(op) { op.timestamp == p.timestamp })
  })
  
  assert_true(significant_change_indices.contains(Some(3)))  // 时间戳4000的点
  assert_true(significant_change_indices.contains(Some(6)))  // 时间戳7000的点
  
  // 测试增量压缩
  let delta_compressed = compressor.delta_compression(points)
  
  // 验证增量压缩结果
  assert_eq(delta_compressed.length(), points.length())
  assert_eq(delta_compressed[0], points[0])  // 第一个点保持不变
  assert_eq(delta_compressed[1].timestamp, 1000)  // 2000 - 1000
  assert_eq(delta_compressed[1].value, 0.1)     // 10.1 - 10.0
  
  // 测试增量解压缩
  let delta_decompressed = compressor.delta_decompression(delta_compressed)
  
  // 验证解压缩结果
  assert_eq(delta_decompressed.length(), points.length())
  for i in 0..points.length() {
    assert_eq(delta_decompressed[i].timestamp, points[i].timestamp)
    assert_eq(delta_decompressed[i].value, points[i].value)
  }
  
  // 测试压缩率计算
  let compression_ratio = compressor.calculate_compression_ratio(points, triangular_compressed)
  
  // 验证压缩率
  assert_true(compression_ratio > 0.0)  // 应该有压缩效果
  assert_true(compression_ratio < 1.0)  // 但不会超过100%
}