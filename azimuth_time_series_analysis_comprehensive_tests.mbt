// Azimuth 时间序列分析功能测试用例
// 专注于测试遥测数据的时间序列分析和聚合功能

// 测试1: 时间序列数据创建和基本操作
test "时间序列数据创建和基本操作" {
  // 创建时间序列数据点
  let data_point1 = {
    timestamp: 1640995200000L,  // 2022-01-01 00:00:00 UTC
    value: 100.0,
    attributes: [("service", "auth"), ("region", "us-east-1")]
  }
  
  let data_point2 = {
    timestamp: 1640995260000L,  // 2022-01-01 00:01:00 UTC
    value: 150.0,
    attributes: [("service", "auth"), ("region", "us-east-1")]
  }
  
  let data_point3 = {
    timestamp: 1640995320000L,  // 2022-01-01 00:02:00 UTC
    value: 120.0,
    attributes: [("service", "auth"), ("region", "us-east-1")]
  }
  
  // 创建时间序列
  let time_series = [data_point1, data_point2, data_point3]
  
  // 验证时间序列长度
  assert_eq(time_series.length(), 3)
  
  // 验证数据点按时间排序
  assert_true(time_series[0].timestamp <= time_series[1].timestamp)
  assert_true(time_series[1].timestamp <= time_series[2].timestamp)
  
  // 验证数据值
  assert_eq(time_series[0].value, 100.0)
  assert_eq(time_series[1].value, 150.0)
  assert_eq(time_series[2].value, 120.0)
}

// 测试2: 时间序列聚合计算
test "时间序列聚合计算" {
  // 创建测试数据
  let data_points = [
    {timestamp: 1640995200000L, value: 10.0, attributes: []},
    {timestamp: 1640995260000L, value: 20.0, attributes: []},
    {timestamp: 1640995320000L, value: 30.0, attributes: []},
    {timestamp: 1640995380000L, value: 40.0, attributes: []},
    {timestamp: 1640995440000L, value: 50.0, attributes: []}
  ]
  
  // 计算总和
  let sum = data_points.fold(0.0, fn(acc, point) { acc + point.value })
  assert_eq(sum, 150.0)
  
  // 计算平均值
  let avg = sum / data_points.length().to_double()
  assert_eq(avg, 30.0)
  
  // 计算最大值
  let max_value = data_points.fold(0.0, fn(acc, point) { 
    if point.value > acc { point.value } else { acc }
  })
  assert_eq(max_value, 50.0)
  
  // 计算最小值
  let min_value = data_points.fold(999999.0, fn(acc, point) { 
    if point.value < acc { point.value } else { acc }
  })
  assert_eq(min_value, 10.0)
}

// 测试3: 时间窗口聚合
test "时间窗口聚合" {
  // 创建跨越多个时间窗口的数据点
  let data_points = [
    {timestamp: 1640995200000L, value: 10.0, attributes: []},  // 00:00:00
    {timestamp: 1640995230000L, value: 15.0, attributes: []},  // 00:00:30
    {timestamp: 1640995260000L, value: 20.0, attributes: []},  // 00:01:00
    {timestamp: 1640995290000L, value: 25.0, attributes: []},  // 00:01:30
    {timestamp: 1640995320000L, value: 30.0, attributes: []},  // 00:02:00
    {timestamp: 1640995350000L, value: 35.0, attributes: []}   // 00:02:30
  ]
  
  // 定义1分钟时间窗口
  let window_size = 60000L  // 60秒
  
  // 按时间窗口分组聚合
  let window_start = 1640995200000L
  let window1_end = window_start + window_size
  let window2_end = window1_end + window_size
  let window3_end = window2_end + window_size
  
  // 窗口1: 00:00:00 - 00:01:00
  let window1_data = data_points.filter(fn(point) { 
    point.timestamp >= window_start && point.timestamp < window1_end 
  })
  let window1_sum = window1_data.fold(0.0, fn(acc, point) { acc + point.value })
  assert_eq(window1_sum, 25.0)  // 10.0 + 15.0
  
  // 窗口2: 00:01:00 - 00:02:00
  let window2_data = data_points.filter(fn(point) { 
    point.timestamp >= window1_end && point.timestamp < window2_end 
  })
  let window2_sum = window2_data.fold(0.0, fn(acc, point) { acc + point.value })
  assert_eq(window2_sum, 45.0)  // 20.0 + 25.0
  
  // 窗口3: 00:02:00 - 00:03:00
  let window3_data = data_points.filter(fn(point) { 
    point.timestamp >= window2_end && point.timestamp < window3_end 
  })
  let window3_sum = window3_data.fold(0.0, fn(acc, point) { acc + point.value })
  assert_eq(window3_sum, 65.0)  // 30.0 + 35.0
}

// 测试4: 时间序列趋势分析
test "时间序列趋势分析" {
  // 创建有明显趋势的数据
  let increasing_data = [
    {timestamp: 1640995200000L, value: 10.0, attributes: []},
    {timestamp: 1640995260000L, value: 20.0, attributes: []},
    {timestamp: 1640995320000L, value: 30.0, attributes: []},
    {timestamp: 1640995380000L, value: 40.0, attributes: []},
    {timestamp: 1640995440000L, value: 50.0, attributes: []}
  ]
  
  // 计算简单线性趋势（斜率）
  let n = increasing_data.length().to_int()
  let sum_x = (n * (n - 1) / 2).to_double()  // 0 + 1 + 2 + 3 + 4 = 10
  let sum_y = increasing_data.fold(0.0, fn(acc, point) { acc + point.value })  // 150.0
  let sum_xy = increasing_data.fold(0.0, fn(acc, point) { 
    let index = increasing_data.index_of(point).unwrap_or(0)
    acc + (index.to_double() * point.value)
  })
  
  // 验证上升趋势
  assert_true(increasing_data[0].value < increasing_data[4].value)
  assert_eq(sum_y, 150.0)
  
  // 创建下降趋势数据
  let decreasing_data = [
    {timestamp: 1640995200000L, value: 50.0, attributes: []},
    {timestamp: 1640995260000L, value: 40.0, attributes: []},
    {timestamp: 1640995320000L, value: 30.0, attributes: []},
    {timestamp: 1640995380000L, value: 20.0, attributes: []},
    {timestamp: 1640995440000L, value: 10.0, attributes: []}
  ]
  
  // 验证下降趋势
  assert_true(decreasing_data[0].value > decreasing_data[4].value)
}

// 测试5: 时间序列异常检测
test "时间序列异常检测" {
  // 创建包含异常值的数据
  let normal_data = [
    {timestamp: 1640995200000L, value: 100.0, attributes: []},
    {timestamp: 1640995260000L, value: 105.0, attributes: []},
    {timestamp: 1640995320000L, value: 98.0, attributes: []},
    {timestamp: 1640995380000L, value: 102.0, attributes: []},
    {timestamp: 1640995440000L, value: 500.0, attributes: []},  // 异常值
    {timestamp: 1640995500000L, value: 99.0, attributes: []},
    {timestamp: 1640995560000L, value: 101.0, attributes: []}
  ]
  
  // 计算平均值和标准差
  let mean = normal_data.fold(0.0, fn(acc, point) { acc + point.value }) / normal_data.length().to_double()
  
  // 计算方差
  let variance = normal_data.fold(0.0, fn(acc, point) { 
    let diff = point.value - mean
    acc + (diff * diff)
  }) / normal_data.length().to_double()
  
  let std_dev = variance.sqrt()
  
  // 检测异常值（超过3个标准差）
  let anomaly_threshold = 3.0 * std_dev
  let anomalies = normal_data.filter(fn(point) { 
    (point.value - mean).abs() > anomaly_threshold
  })
  
  // 应该检测到1个异常值
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].value, 500.0)
}

// 测试6: 多维度时间序列分析
test "多维度时间序列分析" {
  // 创建带有不同属性的时间序列数据
  let multi_dim_data = [
    {timestamp: 1640995200000L, value: 100.0, attributes: [("service", "auth"), ("region", "us-east-1")]},
    {timestamp: 1640995200000L, value: 200.0, attributes: [("service", "api"), ("region", "us-east-1")]},
    {timestamp: 1640995200000L, value: 150.0, attributes: [("service", "auth"), ("region", "us-west-1")]},
    {timestamp: 1640995200000L, value: 250.0, attributes: [("service", "api"), ("region", "us-west-1")]},
    {timestamp: 1640995260000L, value: 110.0, attributes: [("service", "auth"), ("region", "us-east-1")]},
    {timestamp: 1640995260000L, value: 210.0, attributes: [("service", "api"), ("region", "us-east-1")]}
  ]
  
  // 按服务维度分组
  let auth_data = multi_dim_data.filter(fn(point) { 
    point.attributes.contains(fn(attr) { attr.0 == "service" && attr.1 == "auth" })
  })
  
  let api_data = multi_dim_data.filter(fn(point) { 
    point.attributes.contains(fn(attr) { attr.0 == "service" && attr.1 == "api" })
  })
  
  // 验证分组结果
  assert_eq(auth_data.length(), 3)
  assert_eq(api_data.length(), 3)
  
  // 计算各服务的平均值
  let auth_avg = auth_data.fold(0.0, fn(acc, point) { acc + point.value }) / auth_data.length().to_double()
  let api_avg = api_data.fold(0.0, fn(acc, point) { acc + point.value }) / api_data.length().to_double()
  
  // 验证API服务平均值高于认证服务
  assert_true(api_avg > auth_avg)
}