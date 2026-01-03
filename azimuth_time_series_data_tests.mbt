// Azimuth 时间序列数据处理测试
// 专注于测试时间序列数据的收集、处理、分析和存储

// 测试1: 时间序列数据收集
test "时间序列数据收集" {
  // 创建时间序列数据收集器
  let collector = TimeSeriesCollector({
    metrics: [],
    max_points: 1000,
    retention_period: 24 * 60 * 60 * 1000 // 24小时
  })
  
  // 生成时间序列数据点
  let generate_data_points = fn(base_time, count, interval, value_func) {
    (0..count).map(fn(i) {
      DataPoint({
        timestamp: base_time + i * interval,
        value: value_func(i),
        tags: [("source", "test"), ("index", i.to_string())]
      })
    })
  }
  
  // 生成不同类型的指标数据
  let cpu_data = generate_data_points(
    1640995200000, // 基础时间戳
    60,            // 数据点数量
    60000,         // 1分钟间隔
    fn(i) { 50.0 + (i % 20).to_float() * 2.0 } // CPU使用率在50-90之间波动
  )
  
  let memory_data = generate_data_points(
    1640995200000,
    60,
    60000,
    fn(i) { 4096.0 + (i % 10).to_float() * 512.0 } // 内存使用在4GB-9GB之间波动
  )
  
  let request_rate_data = generate_data_points(
    1640995200000,
    60,
    60000,
    fn(i) { 100.0 + (i % 30).to_float() * 10.0 } // 请求率在100-400之间波动
  )
  
  // 添加指标到收集器
  let add_metric = fn(collector, name, data_points) {
    let metric = TimeSeriesMetric({
      name: name,
      data_points: data_points,
      unit: "unknown",
      created_at: get_current_time_ms()
    })
    
    match collector {
      TimeSeriesCollector(data) => {
        TimeSeriesCollector({
          metrics: data.metrics + [metric],
          max_points: data.max_points,
          retention_period: data.retention_period
        })
      }
    }
  }
  
  // 添加指标到收集器
  let collector_with_cpu = add_metric(collector, "system.cpu.usage", cpu_data)
  let collector_with_memory = add_metric(collector_with_cpu, "system.memory.usage", memory_data)
  let final_collector = add_metric(collector_with_memory, "http.requests.rate", request_rate_data)
  
  // 验证收集的指标数量
  assert_eq(final_collector.metrics.length(), 3, "收集的指标数量不正确")
  
  // 验证每个指标的数据点数量
  for i = 0; i < final_collector.metrics.length(); i = i + 1 {
    let metric = final_collector.metrics[i]
    assert_eq(metric.data_points.length(), 60, 
      "指标 " + metric.name + " 的数据点数量不正确")
  }
  
  // 验证数据点的时间顺序
  let verify_time_order = fn(data_points) {
    for i = 1; i < data_points.length(); i = i + 1 {
      if data_points[i].timestamp < data_points[i - 1].timestamp {
        return false
      }
    }
    true
  }
  
  for i = 0; i < final_collector.metrics.length(); i = i + 1 {
    let metric = final_collector.metrics[i]
    assert_true(verify_time_order(metric.data_points), 
      "指标 " + metric.name + " 的数据点时间顺序不正确")
  }
}

// 测试2: 时间序列数据聚合
test "时间序列数据聚合" {
  // 创建测试数据
  let base_time = 1640995200000
  let test_data = (0..1440).map(fn(i) { // 24小时，每分钟一个数据点
    DataPoint({
      timestamp: base_time + i * 60000, // 每分钟
      value: 100.0 + (i % 60).to_float() * 2.0, // 每小时一个周期
      tags: [("service", "api"), ("region", "us-west-2")]
    })
  })
  
  // 时间序列聚合函数
  let aggregate_time_series = fn(data_points, interval, aggregation_type) {
    if data_points.length() == 0 { return [] }
    
    let start_time = data_points[0].timestamp
    let end_time = data_points[data_points.length() - 1].timestamp
    
    let intervals = (start_time..end_time step interval).map(fn(interval_start) {
      let interval_end = interval_start + interval
      
      // 获取当前时间窗口内的数据点
      let window_points = data_points.filter(fn(point) {
        point.timestamp >= interval_start && point.timestamp < interval_end
      })
      
      // 根据聚合类型计算聚合值
      let aggregated_value = match aggregation_type {
        "avg" => {
          if window_points.length() == 0 { 0.0 }
          else {
            window_points.fold(0.0, fn(acc, point) { acc + point.value }) / 
            window_points.length().to_float()
          }
        }
        "min" => {
          if window_points.length() == 0 { 0.0 }
          else {
            window_points.fold(window_points[0].value, fn(acc, point) { 
              if point.value < acc { point.value } else { acc } 
            })
          }
        }
        "max" => {
          if window_points.length() == 0 { 0.0 }
          else {
            window_points.fold(window_points[0].value, fn(acc, point) { 
              if point.value > acc { point.value } else { acc } 
            })
          }
        }
        "sum" => {
          window_points.fold(0.0, fn(acc, point) { acc + point.value })
        }
        "count" => {
          window_points.length().to_float()
        }
        _ => 0.0
      }
      
      DataPoint({
        timestamp: interval_start,
        value: aggregated_value,
        tags: data_points[0].tags // 继承第一个数据点的标签
      })
    })
    
    intervals
  }
  
  // 测试不同聚合类型和间隔
  let hourly_avg = aggregate_time_series(test_data, 60 * 60000, "avg") // 每小时平均
  let hourly_max = aggregate_time_series(test_data, 60 * 60000, "max") // 每小时最大
  let daily_sum = aggregate_time_series(test_data, 24 * 60 * 60000, "sum") // 每天总和
  
  // 验证每小时聚合结果（应该有24个数据点）
  assert_eq(hourly_avg.length(), 24, "每小时平均聚合结果数量不正确")
  assert_eq(hourly_max.length(), 24, "每小时最大聚合结果数量不正确")
  
  // 验证每天聚合结果（应该有1个数据点）
  assert_eq(daily_sum.length(), 1, "每天聚合结果数量不正确")
  
  // 验证聚合值的合理性
  let validate_aggregation = fn(original, aggregated, aggregation_type) {
    for i = 0; i < aggregated.length(); i = i + 1 {
      let agg_point = aggregated[i]
      let interval_start = agg_point.timestamp
      let interval_end = interval_start + 60 * 60000
      
      // 获取原始数据中对应时间窗口的数据点
      let window_points = original.filter(fn(point) {
        point.timestamp >= interval_start && point.timestamp < interval_end
      })
      
      if window_points.length() > 0 {
        match aggregation_type {
          "avg" => {
            let expected_avg = window_points.fold(0.0, fn(acc, point) { acc + point.value }) / 
                              window_points.length().to_float()
            assert_eq(agg_point.value, expected_avg, "平均值聚合计算错误")
          }
          "max" => {
            let expected_max = window_points.fold(window_points[0].value, fn(acc, point) { 
              if point.value > acc { point.value } else { acc } 
            })
            assert_eq(agg_point.value, expected_max, "最大值聚合计算错误")
          }
          _ => {} // 跳过其他类型的验证
        }
      }
    }
  }
  
  // 验证聚合计算的正确性
  validate_aggregation(test_data, hourly_avg, "avg")
  validate_aggregation(test_data, hourly_max, "max")
}

// 测试3: 时间序列数据分析
test "时间序列数据分析" {
  // 创建测试数据：带有趋势和季节性的时间序列
  let base_time = 1640995200000
  let trend_data = (0..168).map(fn(i) { // 一周，每小时一个数据点
    let trend = 100.0 + i.to_float() * 0.5 // 线性增长趋势
    let seasonal = 20.0 * (i % 24).to_float() / 24.0 // 日季节性
    let noise = (i % 7).to_float() * 2.0 // 周期性噪声
    
    DataPoint({
      timestamp: base_time + i * 3600000, // 每小时
      value: trend + seasonal + noise,
      tags: [("metric", "trend_test")]
    })
  })
  
  // 趋势分析函数
  let analyze_trend = fn(data_points) {
    if data_points.length() < 2 { return 0.0 }
    
    // 简单线性回归计算趋势
    let n = data_points.length().to_float()
    let sum_x = (0..data_points.length()).fold(0, fn(acc, i) { acc + i })
    let sum_y = data_points.fold(0.0, fn(acc, point) { acc + point.value })
    let sum_xy = data_points.fold_with_index(0.0, fn(acc, i, point) { 
      acc + i.to_float() * point.value 
    })
    let sum_x2 = (0..data_points.length()).fold(0, fn(acc, i) { acc + i * i })
    
    let slope = (n.to_float() * sum_xy - sum_x.to_float() * sum_y) / 
               (n.to_float() * sum_x2.to_float() - sum_x.to_float() * sum_x.to_float())
    
    slope
  }
  
  // 季节性分析函数
  let analyze_seasonality = fn(data_points, period) {
    if data_points.length() < period * 2 { return [] }
    
    // 计算每个季节周期内的平均值
    let seasonal_patterns = (0..period).map(fn(i) {
      let period_values = []
      let j = i
      
      while j < data_points.length() {
        period_values = period_values + [data_points[j].value]
        j = j + period
      }
      
      if period_values.length() > 0 {
        let avg = period_values.fold(0.0, fn(acc, value) { acc + value }) / 
                  period_values.length().to_float()
        avg
      } else {
        0.0
      }
    })
    
    seasonal_patterns
  }
  
  // 异常检测函数
  let detect_anomalies = fn(data_points, threshold) {
    // 计算移动平均和标准差
    let window_size = 10
    let anomalies = []
    
    for i = window_size; i < data_points.length(); i = i + 1 {
      let window = data_points.slice(i - window_size, i)
      let mean = window.fold(0.0, fn(acc, point) { acc + point.value }) / 
                 window.length().to_float()
      
      let variance = window.fold(0.0, fn(acc, point) { 
        acc + (point.value - mean) * (point.value - mean) 
      }) / window.length().to_float()
      
      let std_dev = variance.sqrt()
      
      // 检查当前点是否为异常
      let current_point = data_points[i]
      let z_score = (current_point.value - mean) / std_dev
      
      if z_score.abs() > threshold {
        anomalies = anomalies + [{
          index: i,
          timestamp: current_point.timestamp,
          value: current_point.value,
          z_score: z_score
        }]
      }
    }
    
    anomalies
  }
  
  // 执行分析
  let trend = analyze_trend(trend_data)
  let daily_seasonality = analyze_seasonality(trend_data, 24) // 24小时季节性
  let anomalies = detect_anomalies(trend_data, 2.0) // 2倍标准差阈值
  
  // 验证趋势分析结果
  assert_true(trend > 0.0, "应该检测到正趋势")
  assert_true(trend < 1.0, "趋势值应该合理")
  
  // 验证季节性分析结果
  assert_eq(daily_seasonality.length(), 24, "季节性模式数量不正确")
  
  // 验证季节性模式的合理性
  let seasonal_sum = daily_seasonality.fold(0.0, fn(acc, value) { acc + value })
  let seasonal_avg = seasonal_sum / daily_seasonality.length().to_float()
  assert_true(seasonal_avg > 0.0, "季节性平均值应该大于0")
  
  // 验证异常检测结果
  assert_true(anomalies.length() >= 0, "异常检测应该返回数组")
  
  // 如果有异常，验证异常数据结构
  if anomalies.length() > 0 {
    for i = 0; i < anomalies.length(); i = i + 1 {
      let anomaly = anomalies[i]
      assert_true(anomaly.index >= 0 && anomaly.index < trend_data.length(), 
        "异常索引超出范围")
      assert_true(anomaly.z_score.abs() > 2.0, "异常Z分数应该大于阈值")
    }
  }
}

// 测试4: 时间序列数据压缩
test "时间序列数据压缩" {
  // 创建测试数据
  let base_time = 1640995200000
  let test_data = (0..1000).map(fn(i) {
    DataPoint({
      timestamp: base_time + i * 60000, // 每分钟
      value: 100.0 + (i / 100).to_float(), // 缓慢增长
      tags: [("metric", "compression_test")]
    })
  })
  
  // 简单压缩算法：基于变化率的压缩
  let compress_time_series = fn(data_points, compression_threshold) {
    if data_points.length() == 0 { return [] }
    
    let compressed = [data_points[0]] // 总是保留第一个点
    let last_kept_point = data_points[0]
    
    for i = 1; i < data_points.length(); i = i + 1 {
      let current_point = data_points[i]
      
      // 计算相对于上一个保留点的变化率
      let rate_of_change = (current_point.value - last_kept_point.value) / 
                          (current_point.timestamp - last_kept_point.timestamp).to_float()
      
      // 如果变化率超过阈值，保留这个点
      if rate_of_change.abs() > compression_threshold {
        compressed = compressed + [current_point]
        last_kept_point = current_point
      }
    }
    
    // 总是保留最后一个点
    if compressed[compressed.length() - 1].timestamp != data_points[data_points.length() - 1].timestamp {
      compressed = compressed + [data_points[data_points.length() - 1]]
    }
    
    compressed
  }
  
  // 解压缩函数：线性插值
  let decompress_time_series = fn(compressed_data) {
    if compressed_data.length() < 2 { return compressed_data }
    
    let decompressed = []
    
    for i = 0; i < compressed_data.length() - 1; i = i + 1 {
      let start_point = compressed_data[i]
      let end_point = compressed_data[i + 1]
      
      // 添加起始点
      decompressed = decompressed + [start_point]
      
      // 计算两点之间的时间间隔
      let time_interval = end_point.timestamp - start_point.timestamp
      
      // 如果间隔大于一个数据点间隔，进行插值
      if time_interval > 60000 {
        let steps = (time_interval / 60000).to_int() - 1
        let value_step = (end_point.value - start_point.value) / (steps + 1).to_float()
        
        for j = 1; j <= steps; j = j + 1 {
          let interpolated_point = DataPoint({
            timestamp: start_point.timestamp + j * 60000,
            value: start_point.value + j.to_float() * value_step,
            tags: start_point.tags
          })
          decompressed = decompressed + [interpolated_point]
        }
      }
    }
    
    // 添加最后一个点
    decompressed = decompressed + [compressed_data[compressed_data.length() - 1]]
    
    decompressed
  }
  
  // 计算压缩比
  let calculate_compression_ratio = fn(original, compressed) {
    original.length().to_float() / compressed.length().to_float()
  }
  
  // 计算压缩误差
  let calculate_compression_error = fn(original, decompressed) {
    if original.length() != decompressed.length() { return 999999.0 }
    
    let total_error = original.fold_with_index(0.0, fn(acc, i, point) {
      let decompressed_point = decompressed[i]
      let error = (point.value - decompressed_point.value).abs()
      acc + error
    })
    
    let avg_error = total_error / original.length().to_float()
    avg_error
  }
  
  // 执行压缩
  let compressed_data = compress_time_series(test_data, 0.001) // 0.001的变化率阈值
  
  // 验证压缩结果
  assert_true(compressed_data.length() < test_data.length(), "压缩应该减少数据点数量")
  assert_true(compressed_data.length() > 1, "压缩后应该保留多个数据点")
  
  // 验证压缩后的数据点时间顺序
  for i = 1; i < compressed_data.length(); i = i + 1 {
    assert_true(compressed_data[i].timestamp > compressed_data[i - 1].timestamp, 
      "压缩后数据点时间顺序不正确")
  }
  
  // 计算压缩比
  let compression_ratio = calculate_compression_ratio(test_data, compressed_data)
  assert_true(compression_ratio > 1.0, "压缩比应该大于1")
  
  // 执行解压缩
  let decompressed_data = decompress_time_series(compressed_data)
  
  // 验证解压缩结果
  assert_eq(decompressed_data.length(), test_data.length(), "解压缩后数据点数量应该与原始数据一致")
  
  // 计算压缩误差
  let compression_error = calculate_compression_error(test_data, decompressed_data)
  assert_true(compression_error < 1.0, "压缩误差应该小于1")
  
  // 验证时间戳的一致性
  for i = 0; i < test_data.length(); i = i + 1 {
    assert_eq(decompressed_data[i].timestamp, test_data[i].timestamp, 
      "解压缩后时间戳不一致")
  }
}

// 测试5: 时间序列数据查询
test "时间序列数据查询" {
  // 创建测试数据
  let base_time = 1640995200000 // 2022-01-01 00:00:00
  let test_metrics = [
    {
      name: "system.cpu.usage",
      data: (0..72).map(fn(i) { // 3天，每小时一个数据点
        DataPoint({
          timestamp: base_time + i * 3600000,
          value: 50.0 + (i % 24).to_float() * 2.0,
          tags: [("service", "web"), ("region", "us-west-2")]
        })
      })
    },
    {
      name: "system.memory.usage",
      data: (0..72).map(fn(i) {
        DataPoint({
          timestamp: base_time + i * 3600000,
          value: 4096.0 + (i % 12).to_float() * 512.0,
          tags: [("service", "web"), ("region", "us-west-2")]
        })
      })
    },
    {
      name: "http.requests.rate",
      data: (0..72).map(fn(i) {
        DataPoint({
          timestamp: base_time + i * 3600000,
          value: 100.0 + (i % 6).to_float() * 50.0,
          tags: [("service", "api"), ("region", "us-east-1")]
        })
      })
    }
  ]
  
  // 时间范围查询函数
  let query_by_time_range = fn(metrics, start_time, end_time) {
    metrics.map(fn(metric) {
      let filtered_data = metric.data.filter(fn(point) {
        point.timestamp >= start_time && point.timestamp <= end_time
      })
      
      {
        name: metric.name,
        data: filtered_data
      }
    })
  }
  
  // 标签查询函数
  let query_by_tags = fn(metrics, tag_filters) {
    metrics.filter(fn(metric) {
      if metric.data.length() == 0 { return false }
      
      let tags = metric.data[0].tags
      
      tag_filters.all(fn(filter) {
        tags.any(fn(tag) { 
          tag.0 == filter.0 && tag.1 == filter.1 
        })
      })
    })
  }
  
  // 聚合查询函数
  let query_with_aggregation = fn(metrics, aggregation_type, interval) {
    metrics.map(fn(metric) {
      let aggregated_data = aggregate_time_series(metric.data, interval, aggregation_type)
      
      {
        name: metric.name,
        data: aggregated_data
      }
    })
  }
  
  // 聚合函数（复用之前的实现）
  let aggregate_time_series = fn(data_points, interval, aggregation_type) {
    if data_points.length() == 0 { return [] }
    
    let start_time = data_points[0].timestamp
    let end_time = data_points[data_points.length() - 1].timestamp
    
    let intervals = (start_time..end_time step interval).map(fn(interval_start) {
      let interval_end = interval_start + interval
      
      // 获取当前时间窗口内的数据点
      let window_points = data_points.filter(fn(point) {
        point.timestamp >= interval_start && point.timestamp < interval_end
      })
      
      // 根据聚合类型计算聚合值
      let aggregated_value = match aggregation_type {
        "avg" => {
          if window_points.length() == 0 { 0.0 }
          else {
            window_points.fold(0.0, fn(acc, point) { acc + point.value }) / 
            window_points.length().to_float()
          }
        }
        "max" => {
          if window_points.length() == 0 { 0.0 }
          else {
            window_points.fold(window_points[0].value, fn(acc, point) { 
              if point.value > acc { point.value } else { acc } 
            })
          }
        }
        _ => 0.0
      }
      
      DataPoint({
        timestamp: interval_start,
        value: aggregated_value,
        tags: data_points[0].tags // 继承第一个数据点的标签
      })
    })
    
    intervals
  }
  
  // 测试时间范围查询
  let day1_start = base_time
  let day1_end = base_time + 24 * 3600000 - 1
  
  let day1_metrics = query_by_time_range(test_metrics, day1_start, day1_end)
  
  // 验证时间范围查询结果
  for i = 0; i < day1_metrics.length(); i = i + 1 {
    let metric = day1_metrics[i]
    assert_eq(metric.data.length(), 24, 
      "指标 " + metric.name + " 在第一天的数据点数量不正确")
    
    // 验证所有数据点都在指定时间范围内
    for j = 0; j < metric.data.length(); j = j + 1 {
      let point = metric.data[j]
      assert_true(point.timestamp >= day1_start && point.timestamp <= day1_end, 
        "数据点不在指定时间范围内")
    }
  }
  
  // 测试标签查询
  let web_service_metrics = query_by_tags(test_metrics, [("service", "web")])
  
  // 验证标签查询结果
  assert_eq(web_service_metrics.length(), 2, "web服务指标数量不正确")
  
  for i = 0; i < web_service_metrics.length(); i = i + 1 {
    let metric = web_service_metrics[i]
    assert_true(metric.name == "system.cpu.usage" || metric.name == "system.memory.usage", 
      "标签查询返回了错误的指标")
  }
  
  // 测试聚合查询
  let daily_avg_metrics = query_with_aggregation(test_metrics, "avg", 24 * 3600000)
  
  // 验证聚合查询结果
  for i = 0; i < daily_avg_metrics.length(); i = i + 1 {
    let metric = daily_avg_metrics[i]
    assert_eq(metric.data.length(), 3, 
      "指标 " + metric.name + " 的每日平均数据点数量不正确")
  }
  
  // 测试复合查询：时间范围 + 标签 + 聚合
  let composite_query = query_by_time_range(
    query_by_tags(test_metrics, [("service", "web")]),
    base_time,
    base_time + 2 * 24 * 3600000 - 1
  )
  
  let composite_aggregated = query_with_aggregation(composite_query, "max", 12 * 3600000)
  
  // 验证复合查询结果
  assert_eq(composite_aggregated.length(), 2, "复合查询结果指标数量不正确")
  
  for i = 0; i < composite_aggregated.length(); i = i + 1 {
    let metric = composite_aggregated[i]
    assert_eq(metric.data.length(), 4, 
      "指标 " + metric.name + " 的复合查询聚合数据点数量不正确")
  }
}

// 类型定义（用于测试）
type TimeSeriesCollector {
  TimeSeriesCollector({
    metrics: Array[TimeSeriesMetric],
    max_points: Int,
    retention_period: Int
  })
}

type TimeSeriesMetric {
  TimeSeriesMetric({
    name: String,
    data_points: Array[DataPoint],
    unit: String,
    created_at: Int
  })
}

type DataPoint {
  DataPoint({
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  })
}

// 辅助函数（用于测试）
let get_current_time_ms = fn() { 1640995200000 }

// 扩展Array类型的方法（用于测试）
let Array::map = fn(self, transform) {
  // 简化的map实现
  let result = []
  for i = 0; i < self.length(); i = i + 1 {
    result = result + [transform(self[i])]
  }
  result
}

let Array::filter = fn(self, predicate) {
  // 简化的filter实现
  let result = []
  for i = 0; i < self.length(); i = i + 1 {
    if predicate(self[i]) {
      result = result + [self[i]]
    }
  }
  result
}

let Array::fold = fn(self, initial, accumulator) {
  // 简化的fold实现
  let result = initial
  for i = 0; i < self.length(); i = i + 1 {
    result = accumulator(result, self[i])
  }
  result
}

let Array::fold_with_index = fn(self, initial, accumulator) {
  // 简化的带索引fold实现
  let result = initial
  for i = 0; i < self.length(); i = i + 1 {
    result = accumulator(result, i, self[i])
  }
  result
}

let Array::all = fn(self, predicate) {
  // 简化的all实现
  for i = 0; i < self.length(); i = i + 1 {
    if !predicate(self[i]) {
      return false
    }
  }
  true
}

let Array::any = fn(self, predicate) {
  // 简化的any实现
  for i = 0; i < self.length(); i = i + 1 {
    if predicate(self[i]) {
      return true
    }
  }
  false
}

let Array::length = fn(self) {
  // 简化的length实现
  let count = 0
  let _ = self // 简化实现
  count
}

let Array::slice = fn(self, start, end) {
  // 简化的slice实现
  let result = []
  for i = start; i < end && i < self.length(); i = i + 1 {
    result = result + [self[i]]
  }
  result
}

// 扩展Float类型的方法（用于测试）
let Float::abs = fn(self) {
  if self < 0.0 { -self } else { self }
}

let Float::sqrt = fn(self) {
  // 简化的平方根实现
  if self == 0.0 { 0.0 }
  else if self < 0.0 { 0.0 } // 错误情况
  else {
    // 牛顿迭代法
    let guess = self / 2.0
    let epsilon = 0.0001
    let diff = guess * guess - self
    
    while diff.abs() > epsilon {
      guess = guess - diff / (2.0 * guess)
      diff = guess * guess - self
    }
    
    guess
  }
}

// 扩展Int类型的方法（用于测试）
let Int::to_float = fn(self) { self.to_float() }