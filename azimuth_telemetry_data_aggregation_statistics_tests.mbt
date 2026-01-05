// Azimuth 遥测数据聚合和统计分析测试用例
// 专注于遥测数据的聚合操作和统计功能

// 测试1: 遥测数据时间窗口聚合
test "遥测数据时间窗口聚合" {
  // 创建时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "auth" },
    { timestamp: 1640995210, metric: "cpu", value: 50.0, service: "auth" },
    { timestamp: 1640995220, metric: "cpu", value: 55.0, service: "auth" },
    { timestamp: 1640995230, metric: "cpu", value: 60.0, service: "auth" },
    { timestamp: 1640995240, metric: "cpu", value: 58.0, service: "auth" },
    { timestamp: 1640995250, metric: "cpu", value: 52.0, service: "auth" },
    { timestamp: 1640995260, metric: "cpu", value: 48.0, service: "auth" },
    { timestamp: 1640995270, metric: "cpu", value: 42.0, service: "auth" },
    { timestamp: 1640995280, metric: "cpu", value: 40.0, service: "auth" },
    { timestamp: 1640995290, metric: "cpu", value: 38.0, service: "auth" }
  ]
  
  // 定义时间窗口聚合函数
  let aggregate_by_time_window = fn(data, window_size_seconds) {
    let mut windows = {}
    
    for data_point in data {
      // 计算时间窗口
      let window_start = (data_point.timestamp / window_size_seconds) * window_size_seconds
      
      if not(windows.contains(window_start.to_string())) {
        windows[window_start.to_string()] = {
          window_start,
          window_end: window_start + window_size_seconds,
          values: [],
          count: 0,
          sum: 0.0,
          min: 999999.0,
          max: -1.0
        }
      }
      
      let window = windows[window_start.to_string()]
      window.values = window.values.push(data_point.value)
      window.count = window.count + 1
      window.sum = window.sum + data_point.value
      
      if data_point.value < window.min {
        window.min = data_point.value
      }
      
      if data_point.value > window.max {
        window.max = data_point.value
      }
      
      windows[window_start.to_string()] = window
    }
    
    // 计算平均值
    let mut result = []
    for (_, window) in windows {
      let avg = if window.count > 0 { window.sum / window.count.to_float() } else { 0.0 }
      result = result.push({
        window_start: window.window_start,
        window_end: window.window_end,
        count: window.count,
        sum: window.sum,
        avg,
        min: window.min,
        max: window.max
      })
    }
    
    // 按窗口开始时间排序
    result.sort(fn(a, b) { a.window_start - b.window_start })
  }
  
  // 使用30秒窗口进行聚合
  let aggregated_windows = aggregate_by_time_window(time_series_data, 30)
  
  // 验证时间窗口聚合结果
  assert_eq(aggregated_windows.length(), 4)
  
  // 验证第一个窗口 (1640995200-1640995230)
  let window1 = aggregated_windows[0]
  assert_eq(window1.window_start, 1640995200)
  assert_eq(window1.window_end, 1640995230)
  assert_eq(window1.count, 3)
  assert_eq(window1.sum, 150.0)
  assert_eq(window1.avg, 50.0)
  assert_eq(window1.min, 45.0)
  assert_eq(window1.max, 55.0)
  
  // 验证最后一个窗口 (1640995280-1640995310)
  let window4 = aggregated_windows[3]
  assert_eq(window4.window_start, 1640995280)
  assert_eq(window4.count, 3)
  assert_eq(window4.sum, 120.0)
  assert_eq(window4.avg, 40.0)
  assert_eq(window4.min, 38.0)
  assert_eq(window4.max, 42.0)
}

// 测试2: 遥测数据多维度聚合
test "遥测数据多维度聚合" {
  // 创建多维度的遥测数据
  let multi_dimensional_data = [
    { timestamp: 1640995200, service: "auth", region: "us-east", metric: "cpu", value: 45.0 },
    { timestamp: 1640995200, service: "auth", region: "us-west", metric: "cpu", value: 50.0 },
    { timestamp: 1640995200, service: "db", region: "us-east", metric: "cpu", value: 60.0 },
    { timestamp: 1640995200, service: "db", region: "us-west", metric: "cpu", value: 55.0 },
    { timestamp: 1640995210, service: "auth", region: "us-east", metric: "memory", value: 1024.0 },
    { timestamp: 1640995210, service: "auth", region: "us-west", metric: "memory", value: 2048.0 },
    { timestamp: 1640995210, service: "db", region: "us-east", metric: "memory", value: 4096.0 },
    { timestamp: 1640995210, service: "db", region: "us-west", metric: "memory", value: 3072.0 }
  ]
  
  // 定义多维度聚合函数
  let aggregate_by_dimensions = fn(data, dimensions) {
    let mut groups = {}
    
    for data_point in data {
      // 创建复合键
      let mut key_parts = []
      for dimension in dimensions {
        match dimension {
          "service" => key_parts = key_parts.push(data_point.service)
          "region" => key_parts = key_parts.push(data_point.region)
          "metric" => key_parts = key_parts.push(data_point.metric)
          _ => ()
        }
      }
      let key = key_parts.join(":")
      
      if not(groups.contains(key)) {
        groups[key] = {
          dimensions: key_parts,
          values: [],
          count: 0,
          sum: 0.0,
          min: 999999.0,
          max: -1.0
        }
      }
      
      let group = groups[key]
      group.values = group.values.push(data_point.value)
      group.count = group.count + 1
      group.sum = group.sum + data_point.value
      
      if data_point.value < group.min {
        group.min = data_point.value
      }
      
      if data_point.value > group.max {
        group.max = data_point.value
      }
      
      groups[key] = group
    }
    
    // 计算统计信息并转换为数组
    let mut result = []
    for (_, group) in groups {
      let avg = if group.count > 0 { group.sum / group.count.to_float() } else { 0.0 }
      result = result.push({
        dimensions: group.dimensions,
        count: group.count,
        sum: group.sum,
        avg,
        min: group.min,
        max: group.max
      })
    }
    
    result
  }
  
  // 按服务+区域+指标进行聚合
  let service_region_metric_aggregation = aggregate_by_dimensions(multi_dimensional_data, ["service", "region", "metric"])
  
  // 验证多维度聚合结果
  assert_eq(service_region_metric_aggregation.length(), 8)
  
  // 验证具体聚合项
  let auth_east_cpu = service_region_metric_aggregation.find(fn(g) { 
    g.dimensions[0] == "auth" and g.dimensions[1] == "us-east" and g.dimensions[2] == "cpu" 
  })
  assert_eq(auth_east_cpu.dimensions, ["auth", "us-east", "cpu"])
  assert_eq(auth_east_cpu.count, 1)
  assert_eq(auth_east_cpu.sum, 45.0)
  assert_eq(auth_east_cpu.avg, 45.0)
  
  // 按服务进行聚合
  let service_aggregation = aggregate_by_dimensions(multi_dimensional_data, ["service"])
  
  // 验证服务级聚合结果
  assert_eq(service_aggregation.length(), 2)
  
  let auth_service = service_aggregation.find(fn(g) { g.dimensions[0] == "auth" })
  assert_eq(auth_service.count, 4)
  assert_eq(auth_service.sum, 3167.0)  // 45 + 50 + 1024 + 2048
  assert_eq(auth_service.avg, 791.75)
  
  // 按区域进行聚合
  let region_aggregation = aggregate_by_dimensions(multi_dimensional_data, ["region"])
  
  // 验证区域级聚合结果
  assert_eq(region_aggregation.length(), 2)
  
  let east_region = region_aggregation.find(fn(g) { g.dimensions[0] == "us-east" })
  assert_eq(east_region.count, 4)
  assert_eq(east_region.sum, 5225.0)  // 45 + 60 + 1024 + 4096
  assert_eq(east_region.avg, 1306.25)
}

// 测试3: 遥测数据百分位数统计
test "遥测数据百分位数统计" {
  // 创建响应时间数据
  let response_time_data = [
    { request_id: "1", response_time: 10 },
    { request_id: "2", response_time: 25 },
    { request_id: "3", response_time: 50 },
    { request_id: "4", response_time: 75 },
    { request_id: "5", response_time: 100 },
    { request_id: "6", response_time: 150 },
    { request_id: "7", response_time: 200 },
    { request_id: "8", response_time: 300 },
    { request_id: "9", response_time: 500 },
    { request_id: "10", response_time: 1000 }
  ]
  
  // 定义百分位数计算函数
  let calculate_percentiles = fn(data, percentiles) {
    // 提取值并排序
    let mut values = []
    for data_point in data {
      values = values.push(data_point.response_time)
    }
    values.sort(fn(a, b) { a - b })
    
    // 计算百分位数
    let mut results = {}
    for percentile in percentiles {
      let index = (percentile / 100.0 * (values.length() - 1).to_float()).to_int()
      let percentile_value = values[index]
      results[percentile.to_string()] = percentile_value
    }
    
    // 计算基本统计信息
    let sum = values.reduce(fn(acc, val) { acc + val }, 0)
    let avg = sum.to_float() / values.length().to_float()
    let min = values[0]
    let max = values[values.length() - 1]
    
    {
      count: values.length(),
      sum,
      avg,
      min,
      max,
      percentiles: results
    }
  }
  
  // 计算百分位数
  let percentile_results = calculate_percentiles(response_time_data, [50.0, 90.0, 95.0, 99.0])
  
  // 验证百分位数计算结果
  assert_eq(percentile_results.count, 10)
  assert_eq(percentile_results.sum, 2410)
  assert_eq(percentile_results.avg, 241.0)
  assert_eq(percentile_results.min, 10)
  assert_eq(percentile_results.max, 1000)
  
  // 验证百分位数
  assert_eq(percentile_results.percentiles["50"], 100)  // 中位数
  assert_eq(percentile_results.percentiles["90"], 500)
  assert_eq(percentile_results.percentiles["95"], 700)  // 插值: 500 + (1000-500)*(0.5)
  assert_eq(percentile_results.percentiles["99"], 940)  // 插值: 500 + (1000-500)*(0.88)
}

// 测试4: 遥测数据趋势分析
test "遥测数据趋势分析" {
  // 创建时间序列数据（包含趋势）
  let trend_data = [
    { timestamp: 1640995200, value: 100 },
    { timestamp: 1640995210, value: 105 },
    { timestamp: 1640995220, value: 110 },
    { timestamp: 1640995230, value: 115 },
    { timestamp: 1640995240, value: 120 },
    { timestamp: 1640995250, value: 125 },
    { timestamp: 1640995260, value: 130 },
    { timestamp: 1640995270, value: 135 },
    { timestamp: 1640995280, value: 140 },
    { timestamp: 1640995290, value: 145 }
  ]
  
  // 定义趋势分析函数
  let analyze_trend = fn(data) {
    if data.length() < 2 {
      return { trend: "insufficient_data", slope: 0.0, correlation: 0.0 }
    }
    
    // 计算线性回归
    let n = data.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    let mut sum_y2 = 0.0
    
    for i in 0..data.length() {
      let x = i.to_float()
      let y = data[i].value.to_float()
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
      sum_y2 = sum_y2 + (y * y)
    }
    
    // 计算斜率和截距
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // 计算相关系数
    let correlation = (n * sum_xy - sum_x * sum_y) / 
                     (((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt())
    
    // 确定趋势类型
    let trend = if slope > 0.5 {
      "strong_increasing"
    } else if slope > 0.1 {
      "moderate_increasing"
    } else if slope < -0.5 {
      "strong_decreasing"
    } else if slope < -0.1 {
      "moderate_decreasing"
    } else {
      "stable"
    }
    
    {
      trend,
      slope,
      intercept,
      correlation: if correlation.is_nan() { 0.0 } else { correlation }
    }
  }
  
  // 分析趋势
  let trend_analysis = analyze_trend(trend_data)
  
  // 验证趋势分析结果
  assert_eq(trend_analysis.trend, "strong_increasing")
  assert_eq(trend_analysis.slope, 5.0)  // 每个时间点增加5
  assert_eq(trend_analysis.intercept, 100.0)  // 起始值
  assert_eq(trend_analysis.correlation, 1.0)  // 完美相关
  
  // 测试下降趋势
  let decreasing_trend_data = [
    { timestamp: 1640995200, value: 200 },
    { timestamp: 1640995210, value: 180 },
    { timestamp: 1640995220, value: 160 },
    { timestamp: 1640995230, value: 140 },
    { timestamp: 1640995240, value: 120 }
  ]
  
  let decreasing_trend = analyze_trend(decreasing_trend_data)
  assert_eq(decreasing_trend.trend, "strong_decreasing")
  assert_eq(decreasing_trend.slope, -20.0)
  
  // 测试稳定趋势
  let stable_trend_data = [
    { timestamp: 1640995200, value: 100 },
    { timestamp: 1640995210, value: 102 },
    { timestamp: 1640995220, value: 98 },
    { timestamp: 1640995230, value: 101 },
    { timestamp: 1640995240, value: 99 }
  ]
  
  let stable_trend = analyze_trend(stable_trend_data)
  assert_eq(stable_trend.trend, "stable")
  assert_true(stable_trend.slope >= -0.5 and stable_trend.slope <= 0.5)
}

// 测试5: 遥测数据异常值检测
test "遥测数据异常值检测" {
  // 创建包含异常值的数据
  let data_with_outliers = [
    { timestamp: 1640995200, value: 50 },
    { timestamp: 1640995210, value: 55 },
    { timestamp: 1640995220, value: 45 },
    { timestamp: 1640995230, value: 60 },
    { timestamp: 1640995240, value: 52 },
    { timestamp: 1640995250, value: 48 },
    { timestamp: 1640995260, value: 150 },  // 异常值
    { timestamp: 1640995270, value: 53 },
    { timestamp: 1640995280, value: 47 },
    { timestamp: 1640995290, value: 58 },
    { timestamp: 1640995300, value: 5 },    // 异常值
    { timestamp: 1640995310, value: 51 }
  ]
  
  // 定义异常值检测函数（使用IQR方法）
  let detect_outliers = fn(data) {
    // 提取值并排序
    let mut values = []
    for data_point in data {
      values = values.push(data_point.value)
    }
    values.sort(fn(a, b) { a - b })
    
    // 计算四分位数
    let q1_index = (values.length() * 25) / 100
    let q3_index = (values.length() * 75) / 100
    let q1 = values[q1_index]
    let q3 = values[q3_index]
    let iqr = q3 - q1
    
    // 定义异常值边界
    let lower_bound = q1 - 1.5 * iqr
    let upper_bound = q3 + 1.5 * iqr
    
    // 检测异常值
    let mut outliers = []
    let mut normal_values = []
    
    for data_point in data {
      if data_point.value < lower_bound or data_point.value > upper_bound {
        outliers = outliers.push({
          data_point,
          reason: if data_point.value < lower_bound { "below_lower_bound" } else { "above_upper_bound" },
          lower_bound,
          upper_bound
        })
      } else {
        normal_values = normal_values.push(data_point)
      }
    }
    
    {
      normal_values,
      outliers,
      outlier_count: outliers.length(),
      outlier_percentage: outliers.length().to_float() / data.length().to_float() * 100.0,
      q1,
      q3,
      iqr,
      lower_bound,
      upper_bound
    }
  }
  
  // 检测异常值
  let outlier_detection = detect_outliers(data_with_outliers)
  
  // 验证异常值检测结果
  assert_eq(outlier_detection.normal_values.length(), 10)
  assert_eq(outlier_detection.outliers.length(), 2)
  assert_eq(outlier_detection.outlier_count, 2)
  assert_eq(outlier_detection.outlier_percentage, 16.67)  // 2/12 * 100
  
  // 验证异常值
  let outlier1 = outlier_detection.outliers[0]
  assert_eq(outlier1.data_point.value, 150)
  assert_eq(outlier1.reason, "above_upper_bound")
  
  let outlier2 = outlier_detection.outliers[1]
  assert_eq(outlier2.data_point.value, 5)
  assert_eq(outlier2.reason, "below_lower_bound")
  
  // 验证统计信息
  assert_eq(outlier_detection.q1, 47)
  assert_eq(outlier_detection.q3, 55)
  assert_eq(outlier_detection.iqr, 8)
  assert_eq(outlier_detection.lower_bound, 35.0)  // 47 - 1.5*8
  assert_eq(outlier_detection.upper_bound, 67.0)  // 47 + 1.5*8
}

// 测试6: 遥测数据相关性分析
test "遥测数据相关性分析" {
  // 创建多指标时间序列数据
  let multi_metric_data = [
    { timestamp: 1640995200, cpu: 30, memory: 512, requests: 100 },
    { timestamp: 1640995210, cpu: 35, memory: 640, requests: 120 },
    { timestamp: 1640995220, cpu: 40, memory: 768, requests: 150 },
    { timestamp: 1640995230, cpu: 50, memory: 896, requests: 200 },
    { timestamp: 1640995240, cpu: 60, memory: 1024, requests: 250 },
    { timestamp: 1640995250, cpu: 55, memory: 896, requests: 220 },
    { timestamp: 1640995260, cpu: 45, memory: 768, requests: 180 },
    { timestamp: 1640995270, cpu: 35, memory: 640, requests: 130 },
    { timestamp: 1640995280, cpu: 30, memory: 512, requests: 110 }
  ]
  
  // 定义相关性分析函数
  let calculate_correlation = fn(data, metric1, metric2) {
    if data.length() < 2 {
      return 0.0
    }
    
    let n = data.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    let mut sum_y2 = 0.0
    
    for data_point in data {
      let x = get_metric_value(data_point, metric1).to_float()
      let y = get_metric_value(data_point, metric2).to_float()
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
      sum_y2 = sum_y2 + (y * y)
    }
    
    let correlation = (n * sum_xy - sum_x * sum_y) / 
                     (((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt())
    
    if correlation.is_nan() { 0.0 } else { correlation }
  }
  
  // 辅助函数：获取指标值
  let get_metric_value = fn(data_point, metric) {
    match metric {
      "cpu" => data_point.cpu
      "memory" => data_point.memory
      "requests" => data_point.requests
      _ => 0
    }
  }
  
  // 定义相关性强度分类
  let classify_correlation = fn(correlation) {
    let abs_corr = if correlation < 0 { -correlation } else { correlation }
    
    if abs_corr >= 0.8 {
      "strong"
    } else if abs_corr >= 0.5 {
      "moderate"
    } else if abs_corr >= 0.3 {
      "weak"
    } else {
      "negligible"
    }
  }
  
  // 计算各指标间的相关性
  let cpu_memory_correlation = calculate_correlation(multi_metric_data, "cpu", "memory")
  let cpu_requests_correlation = calculate_correlation(multi_metric_data, "cpu", "requests")
  let memory_requests_correlation = calculate_correlation(multi_metric_data, "memory", "requests")
  
  // 验证相关性计算结果
  assert_eq(classify_correlation(cpu_memory_correlation), "strong")
  assert_eq(classify_correlation(cpu_requests_correlation), "strong")
  assert_eq(classify_correlation(memory_requests_correlation), "strong")
  
  // 验证相关性方向
  assert_true(cpu_memory_correlation > 0.8)  // 正相关
  assert_true(cpu_requests_correlation > 0.8)  // 正相关
  assert_true(memory_requests_correlation > 0.8)  // 正相关
  
  // 创建负相关数据测试
  let negative_correlation_data = [
    { timestamp: 1640995200, cpu: 20, cache_hit_rate: 95 },
    { timestamp: 1640995210, cpu: 30, cache_hit_rate: 85 },
    { timestamp: 1640995220, cpu: 40, cache_hit_rate: 75 },
    { timestamp: 1640995230, cpu: 50, cache_hit_rate: 65 },
    { timestamp: 1640995240, cpu: 60, cache_hit_rate: 55 },
    { timestamp: 1640995250, cpu: 70, cache_hit_rate: 45 }
  ]
  
  // 修改get_metric_value函数以支持cache_hit_rate
  let get_metric_value_v2 = fn(data_point, metric) {
    match metric {
      "cpu" => data_point.cpu
      "cache_hit_rate" => data_point.cache_hit_rate
      _ => 0
    }
  }
  
  let cpu_cache_correlation = calculate_correlation_with_extractor(negative_correlation_data, "cpu", "cache_hit_rate", get_metric_value_v2)
  
  // 验证负相关
  assert_eq(classify_correlation(cpu_cache_correlation), "strong")
  assert_true(cpu_cache_correlation < -0.8)  // 负相关
}

// 测试7: 遥测数据分布分析
test "遥测数据分布分析" {
  // 创建响应时间数据
  let response_time_distribution = [
    { response_time: 10 },
    { response_time: 15 },
    { response_time: 20 },
    { response_time: 25 },
    { response_time: 30 },
    { response_time: 35 },
    { response_time: 40 },
    { response_time: 45 },
    { response_time: 50 },
    { response_time: 60 },
    { response_time: 80 },
    { response_time: 100 },
    { response_time: 150 },
    { response_time: 200 },
    { response_time: 300 },
    { response_time: 500 }
  ]
  
  // 定义分布分析函数
  let analyze_distribution = fn(data, bin_size) {
    // 提取值并找到范围
    let mut values = []
    for data_point in data {
      values = values.push(data_point.response_time)
    }
    values.sort(fn(a, b) { a - b })
    
    let min_val = values[0]
    let max_val = values[values.length() - 1]
    
    // 创建直方图
    let mut bins = {}
    let bin_count = ((max_val - min_val) / bin_size) + 1
    
    for i in 0..bin_count {
      let bin_start = min_val + i * bin_size
      let bin_end = bin_start + bin_size
      let bin_key = bin_start.to_string() + "-" + bin_end.to_string()
      bins[bin_key] = {
        range: (bin_start, bin_end),
        count: 0,
        frequency: 0.0
      }
    }
    
    // 分配值到各个bin
    for value in values {
      let bin_index = (value - min_val) / bin_size
      let bin_start = min_val + bin_index * bin_size
      let bin_end = bin_start + bin_size
      let bin_key = bin_start.to_string() + "-" + bin_end.to_string()
      
      if bins.contains(bin_key) {
        let bin = bins[bin_key]
        bin.count = bin.count + 1
        bin.frequency = bin.count.to_float() / values.length().to_float()
        bins[bin_key] = bin
      }
    }
    
    // 转换为数组格式
    let mut histogram = []
    for (_, bin) in bins {
      histogram = histogram.push(bin)
    }
    
    // 计算分布特征
    let sum = values.reduce(fn(acc, val) { acc + val }, 0)
    let mean = sum.to_float() / values.length().to_float()
    
    // 计算方差和标准差
    let mut variance_sum = 0.0
    for value in values {
      let diff = value.to_float() - mean
      variance_sum = variance_sum + (diff * diff)
    }
    let variance = variance_sum / values.length().to_float()
    let std_dev = variance.sqrt()
    
    // 计算偏度和峰度
    let mut skewness_sum = 0.0
    let mut kurtosis_sum = 0.0
    
    for value in values {
      let diff = value.to_float() - mean
      let standardized_diff = diff / std_dev
      skewness_sum = skewness_sum + (standardized_diff * standardized_diff * standardized_diff)
      kurtosis_sum = kurtosis_sum + (standardized_diff * standardized_diff * 
                                   standardized_diff * standardized_diff)
    }
    
    let skewness = skewness_sum / values.length().to_float()
    let kurtosis = kurtosis_sum / values.length().to_float() - 3.0
    
    {
      histogram,
      distribution_stats: {
        count: values.length(),
        min: min_val,
        max: max_val,
        mean,
        variance,
        std_dev,
        skewness,
        kurtosis
      }
    }
  }
  
  // 分析分布
  let distribution_analysis = analyze_distribution(response_time_distribution, 50)
  
  // 验证分布分析结果
  assert_eq(distribution_analysis.histogram.length(), 10)
  
  // 验证统计信息
  let stats = distribution_analysis.distribution_stats
  assert_eq(stats.count, 16)
  assert_eq(stats.min, 10)
  assert_eq(stats.max, 500)
  assert_eq(stats.mean, 108.75)  // sum = 1740, count = 16
  
  // 验证标准差计算
  assert_true(stats.std_dev > 0.0)
  
  // 验证偏度（正偏态，因为有一些大的异常值）
  assert_true(stats.skewness > 0.0)
  
  // 验证直方图分布
  let first_bin = distribution_analysis.histogram[0]
  assert_eq(first_bin.range, (10, 60))
  assert_eq(first_bin.count, 9)  // 10, 15, 20, 25, 30, 35, 40, 45, 50
  assert_eq(first_bin.frequency, 0.5625)  // 9/16
  
  let last_bin = distribution_analysis.histogram[9]
  assert_eq(last_bin.range, (460, 510))
  assert_eq(last_bin.count, 1)  // 500
  assert_eq(last_bin.frequency, 0.0625)  // 1/16
}

// 测试8: 遥测数据比较分析
test "遥测数据比较分析" {
  // 创建两个不同时间段的遥测数据
  let baseline_data = [
    { timestamp: 1640995200, metric: "cpu", value: 40.0, service: "auth" },
    { timestamp: 1640995210, metric: "cpu", value: 45.0, service: "auth" },
    { timestamp: 1640995220, metric: "cpu", value: 50.0, service: "auth" },
    { timestamp: 1640995230, metric: "cpu", value: 42.0, service: "auth" },
    { timestamp: 1640995240, metric: "cpu", value: 48.0, service: "auth" }
  ]
  
  let comparison_data = [
    { timestamp: 1640995200, metric: "cpu", value: 60.0, service: "auth" },
    { timestamp: 1640995210, metric: "cpu", value: 65.0, service: "auth" },
    { timestamp: 1640995220, metric: "cpu", value: 70.0, service: "auth" },
    { timestamp: 1640995230, metric: "cpu", value: 55.0, service: "auth" },
    { timestamp: 1640995240, metric: "cpu", value: 68.0, service: "auth" }
  ]
  
  // 定义比较分析函数
  let compare_datasets = fn(baseline, comparison) {
    // 计算基线统计
    let baseline_values = baseline.map(fn(d) { d.value })
    let baseline_sum = baseline_values.reduce(fn(acc, val) { acc + val }, 0.0)
    let baseline_mean = baseline_sum / baseline_values.length().to_float()
    
    let baseline_min = baseline_values.reduce(fn(acc, val) { if val < acc { val } else { acc }, 999999.0)
    let baseline_max = baseline_values.reduce(fn(acc, val) { if val > acc { val } else { acc }, -1.0)
    
    // 计算基线方差
    let mut baseline_variance_sum = 0.0
    for value in baseline_values {
      let diff = value - baseline_mean
      baseline_variance_sum = baseline_variance_sum + (diff * diff)
    }
    let baseline_variance = baseline_variance_sum / baseline_values.length().to_float()
    let baseline_std_dev = baseline_variance.sqrt()
    
    // 计算比较数据统计
    let comparison_values = comparison.map(fn(d) { d.value })
    let comparison_sum = comparison_values.reduce(fn(acc, val) { acc + val }, 0.0)
    let comparison_mean = comparison_sum / comparison_values.length().to_float()
    
    let comparison_min = comparison_values.reduce(fn(acc, val) { if val < acc { val } else { acc }, 999999.0)
    let comparison_max = comparison_values.reduce(fn(acc, val) { if val > acc { val } else { acc }, -1.0)
    
    // 计算比较数据方差
    let mut comparison_variance_sum = 0.0
    for value in comparison_values {
      let diff = value - comparison_mean
      comparison_variance_sum = comparison_variance_sum + (diff * diff)
    }
    let comparison_variance = comparison_variance_sum / comparison_values.length().to_float()
    let comparison_std_dev = comparison_variance.sqrt()
    
    // 计算变化
    let mean_change = comparison_mean - baseline_mean
    let mean_change_percentage = (mean_change / baseline_mean) * 100.0
    let min_change = comparison_min - baseline_min
    let max_change = comparison_max - baseline_max
    let std_dev_change = comparison_std_dev - baseline_std_dev
    
    // 计算效应大小（Cohen's d）
    let pooled_std_dev = ((baseline_variance + comparison_variance) / 2.0).sqrt()
    let cohens_d = if pooled_std_dev > 0.0 { mean_change / pooled_std_dev } else { 0.0 }
    
    // 分类效应大小
    let effect_size_magnitude = if cohens_d.abs() >= 0.8 {
      "large"
    } else if cohens_d.abs() >= 0.5 {
      "medium"
    } else if cohens_d.abs() >= 0.2 {
      "small"
    } else {
      "negligible"
    }
    
    {
      baseline: {
        mean: baseline_mean,
        min: baseline_min,
        max: baseline_max,
        std_dev: baseline_std_dev
      },
      comparison: {
        mean: comparison_mean,
        min: comparison_min,
        max: comparison_max,
        std_dev: comparison_std_dev
      },
      changes: {
        mean_change,
        mean_change_percentage,
        min_change,
        max_change,
        std_dev_change
      },
      effect_size: {
        cohens_d,
        magnitude: effect_size_magnitude
      }
    }
  }
  
  // 执行比较分析
  let comparison_result = compare_datasets(baseline_data, comparison_data)
  
  // 验证比较分析结果
  assert_eq(comparison_result.baseline.mean, 45.0)
  assert_eq(comparison_result.comparison.mean, 63.6)
  
  // 验证变化
  assert_eq(comparison_result.changes.mean_change, 18.6)
  assert_eq(comparison_result.changes.mean_change_percentage, 41.33)
  
  // 验证效应大小
  assert_eq(comparison_result.effect_size.magnitude, "large")
  assert_true(comparison_result.effect_size.cohens_d > 0.8)
  
  // 测试无显著变化的情况
  let similar_data = [
    { timestamp: 1640995200, metric: "cpu", value: 42.0, service: "auth" },
    { timestamp: 1640995210, metric: "cpu", value: 47.0, service: "auth" },
    { timestamp: 1640995220, metric: "cpu", value: 48.0, service: "auth" },
    { timestamp: 1640995230, metric: "cpu", value: 44.0, service: "auth" },
    { timestamp: 1640995240, metric: "cpu", value: 46.0, service: "auth" }
  ]
  
  let similar_comparison = compare_datasets(baseline_data, similar_data)
  assert_eq(similar_comparison.effect_size.magnitude, "small")
  assert_true(similar_comparison.effect_size.cohens_d < 0.5)
}