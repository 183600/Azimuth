// Metrics模块Measurement复杂场景测试用例
// 测试Measurement的复杂使用场景

test "measurement_with_various_attribute_types" {
  // 测试包含各种属性类型的Measurement
  
  // 创建包含不同类型属性的Measurement
  let measurement = Measurement::{
    value: 123.45,
    attributes: [
      ("string.attr", AttributeValue::string("test_value")),
      ("int.attr", AttributeValue::int(42L)),
      ("float.attr", AttributeValue::float(3.14159)),
      ("bool.attr", AttributeValue::bool(true)),
      ("string.array.attr", AttributeValue::array_string(["a", "b", "c"])),
      ("int.array.attr", AttributeValue::array_int([1L, 2L, 3L])),
      ("float.array.attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
      ("bool.array.attr", AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 验证Measurement基本属性
  assert_eq(measurement.value, 123.45)
  assert_eq(measurement.attributes.length(), 8)
  
  // 验证字符串属性
  let mut found_string_attr = false
  let mut i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    if key == "string.attr" {
      match value {
        AttributeValue::StringValue(str_val) => {
          assert_eq(str_val, "test_value")
          found_string_attr = true
        }
        _ => @test.fail("Expected StringValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_string_attr, true)
  
  // 验证整数属性
  let mut found_int_attr = false
  i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    if key == "int.attr" {
      match value {
        AttributeValue::IntValue(int_val) => {
          assert_eq(int_val, 42L)
          found_int_attr = true
        }
        _ => @test.fail("Expected IntValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_int_attr, true)
  
  // 验证浮点属性
  let mut found_float_attr = false
  i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    if key == "float.attr" {
      match value {
        AttributeValue::FloatValue(float_val) => {
          assert_eq(float_val, 3.14159)
          found_float_attr = true
        }
        _ => @test.fail("Expected FloatValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_float_attr, true)
  
  // 验证布尔属性
  let mut found_bool_attr = false
  i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    if key == "bool.attr" {
      match value {
        AttributeValue::BoolValue(bool_val) => {
          assert_eq(bool_val, true)
          found_bool_attr = true
        }
        _ => @test.fail("Expected BoolValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_bool_attr, true)
  
  // 验证字符串数组属性
  let mut found_string_array_attr = false
  i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    if key == "string.array.attr" {
      match value {
        AttributeValue::ArrayStringValue(str_array) => {
          assert_eq(str_array.length(), 3)
          assert_eq(str_array[0], "a")
          assert_eq(str_array[1], "b")
          assert_eq(str_array[2], "c")
          found_string_array_attr = true
        }
        _ => @test.fail("Expected ArrayStringValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_string_array_attr, true)
}

test "measurement_boundary_and_extreme_values" {
  // 测试Measurement的边界值和极值
  
  // 测试极小值
  let min_measurement = Measurement::{
    value: 0.0,
    attributes: [
      ("min.int", AttributeValue::int(-9223372036854775808L)),  // Int64最小值
      ("min.float", AttributeValue::float(-1.7976931348623157e+308)),  // Double最小值
      ("min.bool", AttributeValue::bool(false))
    ]
  }
  
  assert_eq(min_measurement.value, 0.0)
  
  let mut found_min_int = false
  let mut i = 0
  while i < min_measurement.attributes.length() {
    let (key, value) = min_measurement.attributes[i]
    if key == "min.int" {
      match value {
        AttributeValue::IntValue(int_val) => {
          assert_eq(int_val, -9223372036854775808L)
          found_min_int = true
        }
        _ => @test.fail("Expected IntValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_min_int, true)
  
  // 测试极大值
  let max_measurement = Measurement::{
    value: 1.7976931348623157e+308,  // Double最大值
    attributes: [
      ("max.int", AttributeValue::int(9223372036854775807L)),  // Int64最大值
      ("max.float", AttributeValue::float(1.7976931348623157e+308)),  // Double最大值
      ("max.bool", AttributeValue::bool(true))
    ]
  }
  
  assert_eq(max_measurement.value, 1.7976931348623157e+308)
  
  let mut found_max_float = false
  i = 0
  while i < max_measurement.attributes.length() {
    let (key, value) = max_measurement.attributes[i]
    if key == "max.float" {
      match value {
        AttributeValue::FloatValue(float_val) => {
          assert_eq(float_val, 1.7976931348623157e+308)
          found_max_float = true
        }
        _ => @test.fail("Expected FloatValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_max_float, true)
  
  // 测试特殊浮点值
  let special_float_measurement = Measurement::{
    value: 0.0,
    attributes: [
      ("infinity", AttributeValue::float(1.0/0.0)),  // 正无穷
      ("neg.infinity", AttributeValue::float(-1.0/0.0)),  // 负无穷
      ("nan", AttributeValue::float(0.0/0.0))  // NaN
    ]
  }
  
  // 验证特殊浮点值存在
  let mut found_infinity = false
  i = 0
  while i < special_float_measurement.attributes.length() {
    let (key, value) = special_float_measurement.attributes[i]
    if key == "infinity" {
      match value {
        AttributeValue::FloatValue(float_val) => {
          assert_eq(float_val > 1.0e+300, true)  // 基本的无穷大检查
          found_infinity = true
        }
        _ => @test.fail("Expected FloatValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_infinity, true)
  
  // 测试空数组属性
  let empty_arrays_measurement = Measurement::{
    value: 42.0,
    attributes: [
      ("empty.string.array", AttributeValue::array_string([])),
      ("empty.int.array", AttributeValue::array_int([])),
      ("empty.float.array", AttributeValue::array_float([])),
      ("empty.bool.array", AttributeValue::array_bool([]))
    ]
  }
  
  // 验证空数组
  let mut found_empty_string_array = false
  i = 0
  while i < empty_arrays_measurement.attributes.length() {
    let (key, value) = empty_arrays_measurement.attributes[i]
    if key == "empty.string.array" {
      match value {
        AttributeValue::ArrayStringValue(str_array) => {
          assert_eq(str_array.length(), 0)
          found_empty_string_array = true
        }
        _ => @test.fail("Expected ArrayStringValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_empty_string_array, true)
  
  // 测试大型数组属性
  let large_string_array = []
  let mut j = 0
  while j < 100 {
    large_string_array.push("item" + j.to_string())
    j = j + 1
  }
  
  let large_arrays_measurement = Measurement::{
    value: 100.0,
    attributes: [
      ("large.string.array", AttributeValue::array_string(large_string_array))
    ]
  }
  
  // 验证大型数组
  let mut found_large_array = false
  i = 0
  while i < large_arrays_measurement.attributes.length() {
    let (key, value) = large_arrays_measurement.attributes[i]
    if key == "large.string.array" {
      match value {
        AttributeValue::ArrayStringValue(str_array) => {
          assert_eq(str_array.length(), 100)
          assert_eq(str_array[0], "item0")
          assert_eq(str_array[99], "item99")
          found_large_array = true
        }
        _ => @test.fail("Expected ArrayStringValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_large_array, true)
}

test "measurement_aggregation_and_statistics" {
  // 测试Measurement的聚合和统计功能
  
  // 创建多个Measurement用于聚合
  let measurements = [
    Measurement::{
      value: 10.5,
      attributes: [("service", AttributeValue::string("api")), ("status", AttributeValue::string("200"))]
    },
    Measurement::{
      value: 15.2,
      attributes: [("service", AttributeValue::string("api")), ("status", AttributeValue::string("200"))]
    },
    Measurement::{
      value: 8.7,
      attributes: [("service", AttributeValue::string("api")), ("status", AttributeValue::string("500"))]
    },
    Measurement::{
      value: 12.3,
      attributes: [("service", AttributeValue::string("worker")), ("status", AttributeValue::string("200"))]
    },
    Measurement::{
      value: 20.1,
      attributes: [("service", AttributeValue::string("worker")), ("status", AttributeValue::string("200"))]
    }
  ]
  
  // 按属性过滤Measurement
  let filter_by_attributes = fn(ms : Array[Measurement], filter_key : String, filter_value : String) -> Array[Measurement] {
    let filtered = []
    let mut i = 0
    
    while i < ms.length() {
      let measurement = ms[i]
      let mut matches = false
      let mut j = 0
      
      while j < measurement.attributes.length() && !matches {
        let (key, value) = measurement.attributes[j]
        if key == filter_key {
          match value {
            AttributeValue::StringValue(str_val) => {
              matches = str_val == filter_value
            }
            _ => matches = false
          }
        }
        j = j + 1
      }
      
      if matches {
        filtered.push(measurement)
      }
      i = i + 1
    }
    
    filtered
  }
  
  // 过滤API服务的Measurement
  let api_measurements = filter_by_attributes(measurements, "service", "api")
  assert_eq(api_measurements.length(), 3)
  
  // 过滤Worker服务的Measurement
  let worker_measurements = filter_by_attributes(measurements, "service", "worker")
  assert_eq(worker_measurements.length(), 2)
  
  // 过滤状态码为200的Measurement
  let success_measurements = filter_by_attributes(measurements, "status", "200")
  assert_eq(success_measurements.length(), 4)
  
  // 计算Measurement的统计信息
  let calculate_stats = fn(ms : Array[Measurement]) -> (Double, Double, Double, Double, Int) {
    if ms.length() == 0 {
      (0.0, 0.0, 0.0, 0.0, 0)
    } else {
      let mut sum = 0.0
      let mut min = ms[0].value
      let mut max = ms[0].value
      let mut i = 0
      
      while i < ms.length() {
        let value = ms[i].value
        sum = sum + value
        
        if value < min {
          min = value
        }
        if value > max {
          max = value
        }
        i = i + 1
      }
      
      let avg = sum / ms.length().to_double()
      (sum, avg, min, max, ms.length())
    }
  }
  
  // 计算所有Measurement的统计
  let (total_sum, total_avg, total_min, total_max, total_count) = calculate_stats(measurements)
  assert_eq(total_count, 5)
  assert_eq(total_sum, 66.8)  // 10.5 + 15.2 + 8.7 + 12.3 + 20.1 = 66.8
  assert_eq(total_avg, 66.8 / 5.0)
  assert_eq(total_min, 8.7)
  assert_eq(total_max, 20.1)
  
  // 计算API服务的统计
  let (api_sum, api_avg, api_min, api_max, api_count) = calculate_stats(api_measurements)
  assert_eq(api_count, 3)
  assert_eq(api_sum, 34.4)  // 10.5 + 15.2 + 8.7 = 34.4
  assert_eq(api_avg, 34.4 / 3.0)
  assert_eq(api_min, 8.7)
  assert_eq(api_max, 15.2)
  
  // 计算Worker服务的统计
  let (worker_sum, worker_avg, worker_min, worker_max, worker_count) = calculate_stats(worker_measurements)
  assert_eq(worker_count, 2)
  assert_eq(worker_sum, 32.4)  // 12.3 + 20.1 = 32.4
  assert_eq(worker_avg, 16.2)
  assert_eq(worker_min, 12.3)
  assert_eq(worker_max, 20.1)
  
  // 计算成功请求的统计
  let (success_sum, success_avg, success_min, success_max, success_count) = calculate_stats(success_measurements)
  assert_eq(success_count, 4)
  assert_eq(success_sum, 58.1)  // 10.5 + 15.2 + 12.3 + 20.1 = 58.1
  assert_eq(success_avg, 58.1 / 4.0)
  assert_eq(success_min, 10.5)
  assert_eq(success_max, 20.1)
  
  // 百分位数计算（简化版本）
  let calculate_percentile = fn(ms : Array[Measurement], percentile : Double) -> Double {
    if ms.length() == 0 {
      0.0
    } else {
      // 简化实现：排序后选择相应位置的值
      let sorted_values = []
      let mut i = 0
      
      // 复制值
      while i < ms.length() {
        sorted_values.push(ms[i].value)
        i = i + 1
      }
      
      // 简化的冒泡排序
      let mut j = 0
      while j < sorted_values.length() - 1 {
        let mut k = 0
        while k < sorted_values.length() - j - 1 {
          if sorted_values[k] > sorted_values[k + 1] {
            let temp = sorted_values[k]
            sorted_values[k] = sorted_values[k + 1]
            sorted_values[k + 1] = temp
          }
          k = k + 1
        }
        j = j + 1
      }
      
      // 计算百分位数位置
      let index = (percentile * (sorted_values.length() - 1).to_double()).to_int()
      sorted_values[index]
    }
  }
  
  // 计算中位数（50%百分位数）
  let median = calculate_percentile(measurements, 0.5)
  assert_eq(median, 12.3)  // 排序后：[8.7, 10.5, 12.3, 15.2, 20.1]，中位数是12.3
  
  // 计算第95百分位数
  let p95 = calculate_percentile(measurements, 0.95)
  assert_eq(p95, 20.1)  // 接近最大值
}

test "measurement_time_series_and_temporal_analysis" {
  // 测试Measurement的时间序列和时间分析
  
  // 模拟时间序列Measurement
  let time_series_measurements = [
    Measurement::{
      value: 100.0,
      attributes: [
        ("timestamp", AttributeValue::int(1640995200L)),  // 2022-01-01 00:00:00
        ("metric.name", AttributeValue::string("cpu.usage"))
      ]
    },
    Measurement::{
      value: 120.5,
      attributes: [
        ("timestamp", AttributeValue::int(1640995260L)),  // 2022-01-01 00:01:00
        ("metric.name", AttributeValue::string("cpu.usage"))
      ]
    },
    Measurement::{
      value: 95.2,
      attributes: [
        ("timestamp", AttributeValue::int(1640995320L)),  // 2022-01-01 00:02:00
        ("metric.name", AttributeValue::string("cpu.usage"))
      ]
    },
    Measurement::{
      value: 110.8,
      attributes: [
        ("timestamp", AttributeValue::int(1640995380L)),  // 2022-01-01 00:03:00
        ("metric.name", AttributeValue::string("cpu.usage"))
      ]
    },
    Measurement::{
      value: 105.3,
      attributes: [
        ("timestamp", AttributeValue::int(1640995440L)),  // 2022-01-01 00:04:00
        ("metric.name", AttributeValue::string("cpu.usage"))
      ]
    }
  ]
  
  // 按时间戳排序Measurement
  let sort_by_timestamp = fn(ms : Array[Measurement]) -> Array[Measurement] {
    let sorted = []
    let mut i = 0
    
    // 复制Measurement
    while i < ms.length() {
      sorted.push(ms[i])
      i = i + 1
    }
    
    // 简化的冒泡排序按时间戳
    let mut j = 0
    while j < sorted.length() - 1 {
      let mut k = 0
      while k < sorted.length() - j - 1 {
        // 获取时间戳
        let mut timestamp1 = 0L
        let mut timestamp2 = 0L
        let mut m = 0
        
        while m < sorted[k].attributes.length() {
          let (key, value) = sorted[k].attributes[m]
          if key == "timestamp" {
            match value {
              AttributeValue::IntValue(ts) => timestamp1 = ts
              _ => {}
            }
          }
          m = m + 1
        }
        
        m = 0
        while m < sorted[k + 1].attributes.length() {
          let (key, value) = sorted[k + 1].attributes[m]
          if key == "timestamp" {
            match value {
              AttributeValue::IntValue(ts) => timestamp2 = ts
              _ => {}
            }
          }
          m = m + 1
        }
        
        if timestamp1 > timestamp2 {
          let temp = sorted[k]
          sorted[k] = sorted[k + 1]
          sorted[k + 1] = temp
        }
        k = k + 1
      }
      j = j + 1
    }
    
    sorted
  }
  
  let sorted_measurements = sort_by_timestamp(time_series_measurements)
  assert_eq(sorted_measurements.length(), 5)
  
  // 验证排序结果
  let get_timestamp = fn(m : Measurement) -> Int64 {
    let mut timestamp = 0L
    let mut i = 0
    while i < m.attributes.length() {
      let (key, value) = m.attributes[i]
      if key == "timestamp" {
        match value {
          AttributeValue::IntValue(ts) => timestamp = ts
          _ => {}
        }
      }
      i = i + 1
    }
    timestamp
  }
  
  assert_eq(get_timestamp(sorted_measurements[0]), 1640995200L)
  assert_eq(get_timestamp(sorted_measurements[4]), 1640995440L)
  
  // 计算时间窗口内的统计
  let calculate_time_window_stats = fn(ms : Array[Measurement], start_time : Int64, end_time : Int64) -> (Double, Double, Int) {
    let window_values = []
    let mut i = 0
    
    while i < ms.length() {
      let timestamp = get_timestamp(ms[i])
      if timestamp >= start_time && timestamp <= end_time {
        window_values.push(ms[i].value)
      }
      i = i + 1
    }
    
    if window_values.length() == 0 {
      (0.0, 0.0, 0)
    } else {
      let mut sum = 0.0
      let mut j = 0
      while j < window_values.length() {
        sum = sum + window_values[j]
        j = j + 1
      }
      let avg = sum / window_values.length().to_double()
      (sum, avg, window_values.length())
    }
  }
  
  // 计算前2分钟的统计
  let (sum_2min, avg_2min, count_2min) = calculate_time_window_stats(
    sorted_measurements, 
    1640995200L, 
    1640995320L
  )
  
  assert_eq(count_2min, 3)  // 前3个数据点
  assert_eq(sum_2min, 315.7)  // 100.0 + 120.5 + 95.2
  assert_eq(avg_2min, 315.7 / 3.0)
  
  // 计算趋势（简单线性回归）
  let calculate_trend = fn(ms : Array[Measurement]) -> Double {
    if ms.length() < 2 {
      0.0
    } else {
      let n = ms.length().to_double()
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      let mut i = 0
      
      while i < ms.length() {
        let x = i.to_double()  // 使用索引作为x值
        let y = ms[i].value
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
        i = i + 1
      }
      
      // 计算斜率
      (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    }
  }
  
  let trend = calculate_trend(sorted_measurements)
  // 趋势应该接近0，因为值在波动：100.0 -> 120.5 -> 95.2 -> 110.8 -> 105.3
  assert_eq(trend > -5.0 && trend < 5.0, true)
  
  // 检测异常值（使用简单的标准差方法）
  let detect_anomalies = fn(ms : Array[Measurement], threshold : Double) -> Array[Int] {
    if ms.length() < 2 {
      []
    } else {
      // 计算平均值
      let mut sum = 0.0
      let mut i = 0
      while i < ms.length() {
        sum = sum + ms[i].value
        i = i + 1
      }
      let mean = sum / ms.length().to_double()
      
      // 计算标准差
      let mut variance_sum = 0.0
      i = 0
      while i < ms.length() {
        let diff = ms[i].value - mean
        variance_sum = variance_sum + diff * diff
        i = i + 1
      }
      let std_dev = (variance_sum / ms.length().to_double()).sqrt()
      
      // 检测异常值
      let anomalies = []
      i = 0
      while i < ms.length() {
        let z_score = (ms[i].value - mean).abs() / std_dev
        if z_score > threshold {
          anomalies.push(i)
        }
        i = i + 1
      }
      
      anomalies
    }
  }
  
  let anomalies = detect_anomalies(sorted_measurements, 1.5)  // 1.5个标准差阈值
  // 根据数据分布，可能没有或很少有异常值
  assert_eq(anomalies.length() <= 2, true)
}