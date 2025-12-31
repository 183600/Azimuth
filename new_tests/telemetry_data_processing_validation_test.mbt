// 遥测数据处理验证测试用例

test "data_aggregation_accuracy" {
  // 测试数据聚合准确性
  
  let raw_metrics = [
    {"name": "cpu_usage", "value": 45.5, "timestamp": 1000000},
    {"name": "cpu_usage", "value": 50.2, "timestamp": 1001000},
    {"name": "cpu_usage", "value": 48.7, "timestamp": 1002000},
    {"name": "cpu_usage", "value": 52.1, "timestamp": 1003000},
    {"name": "cpu_usage", "value": 47.9, "timestamp": 1004000}
  ]
  
  // 计算聚合统计
  let mut sum = 0.0
  let mut count = 0
  let mut min_value = raw_metrics[0].value
  let mut max_value = raw_metrics[0].value
  
  let mut i = 0
  while i < raw_metrics.length() {
    let value = raw_metrics[i].value
    sum = sum + value
    count = count + 1
    
    if value < min_value {
      min_value = value
    }
    if value > max_value {
      max_value = value
    }
    i = i + 1
  }
  
  let average = sum / count
  
  // 验证聚合结果
  assert_eq(count, 5)
  assert_eq(sum > 200.0, true)
  assert_eq(min_value, 45.5)
  assert_eq(max_value, 52.1)
  assert_eq(average > 45.0 and average < 55.0, true)
  
  // 验证精确计算
  let expected_sum = 45.5 + 50.2 + 48.7 + 52.1 + 47.9
  let expected_avg = expected_sum / 5.0
  assert_eq(sum, expected_sum)
  assert_eq(average, expected_avg)
}

test "data_filtering_logic" {
  // 测试数据过滤逻辑
  
  let telemetry_data = [
    {"metric": "response_time", "value": 150, "service": "api"},
    {"metric": "response_time", "value": 2500, "service": "api"},
    {"metric": "response_time", "value": 80, "service": "web"},
    {"metric": "error_rate", "value": 0.05, "service": "api"},
    {"metric": "error_rate", "value": 0.02, "service": "web"},
    {"metric": "throughput", "value": 1000, "service": "api"},
    {"metric": "throughput", "value": 500, "service": "web"}
  ]
  
  // 过滤条件：响应时间小于1000ms的服务数据
  let mut filtered_data = []
  let mut i = 0
  while i < telemetry_data.length() {
    let data_point = telemetry_data[i]
    if data_point.metric == "response_time" and data_point.value < 1000 {
      filtered_data.push(data_point)
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(filtered_data.length(), 2)
  assert_eq(filtered_data[0].value, 150)
  assert_eq(filtered_data[1].value, 80)
  assert_eq(filtered_data[0].service, "api")
  assert_eq(filtered_data[1].service, "web")
  
  // 按服务分组过滤
  let mut api_data = []
  let mut web_data = []
  let mut j = 0
  while j < telemetry_data.length() {
    let data_point = telemetry_data[j]
    if data_point.service == "api" {
      api_data.push(data_point)
    } else if data_point.service == "web" {
      web_data.push(data_point)
    }
    j = j + 1
  }
  
  assert_eq(api_data.length(), 4)
  assert_eq(web_data.length(), 3)
}

test "data_transformation_correctness" {
  // 测试数据转换正确性
  
  let raw_telemetry = [
    {"timestamp": 1000000, "duration_ms": 150, "status": "success"},
    {"timestamp": 1001000, "duration_ms": 250, "status": "error"},
    {"timestamp": 1002000, "duration_ms": 80, "status": "success"},
    {"timestamp": 1003000, "duration_ms": 500, "status": "timeout"}
  ]
  
  // 转换：将毫秒转换为秒，状态转换为数值
  let mut transformed_data = []
  let mut i = 0
  while i < raw_telemetry.length() {
    let raw = raw_telemetry[i]
    let status_code = match raw.status {
      "success" => 0,
      "error" => 1,
      "timeout" => 2,
      _ => 3
    }
    
    let transformed = {
      "timestamp": raw.timestamp,
      "duration_s": raw.duration_ms / 1000.0,
      "status_code": status_code
    }
    transformed_data.push(transformed)
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(transformed_data.length(), 4)
  assert_eq(transformed_data[0].duration_s, 0.15)
  assert_eq(transformed_data[1].duration_s, 0.25)
  assert_eq(transformed_data[2].duration_s, 0.08)
  assert_eq(transformed_data[3].duration_s, 0.5)
  
  assert_eq(transformed_data[0].status_code, 0)  // success
  assert_eq(transformed_data[1].status_code, 1)  // error
  assert_eq(transformed_data[2].status_code, 0)  // success
  assert_eq(transformed_data[3].status_code, 2)  // timeout
}

test "data_sampling_strategy" {
  // 测试数据采样策略
  
  let large_dataset = []
  let mut i = 0
  while i < 1000 {
    let data_point = {
      "id": i,
      "value": i * 2,
      "category": i % 5
    }
    large_dataset.push(data_point)
    i = i + 1
  }
  
  // 采样策略：每10个数据点采样1个
  let mut sampled_data = []
  let mut j = 0
  while j < large_dataset.length() {
    if j % 10 == 0 {
      sampled_data.push(large_dataset[j])
    }
    j = j + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_data.length(), 100)
  assert_eq(sampled_data[0].id, 0)
  assert_eq(sampled_data[1].id, 10)
  assert_eq(sampled_data[99].id, 990)
  
  // 验证采样代表性
  let mut category_counts = [0, 0, 0, 0, 0]
  let mut k = 0
  while k < sampled_data.length() {
    let category = sampled_data[k].category
    category_counts[category] = category_counts[category] + 1
    k = k + 1
  }
  
  // 每个类别应该大约有20个样本
  let mut l = 0
  while l < category_counts.length() {
    assert_eq(category_counts[l] >= 18 and category_counts[l] <= 22, true)
    l = l + 1
  }
}

test "data_quality_validation" {
  // 测试数据质量验证
  
  let test_data = [
    {"metric": "cpu", "value": 75.5, "timestamp": 1000000, "valid": true},
    {"metric": "memory", "value": -10.0, "timestamp": 1001000, "valid": false},  // 负值无效
    {"metric": "disk", "value": 50.0, "timestamp": 0, "valid": false},  // 无效时间戳
    {"metric": "network", "value": 100.5, "timestamp": 1003000, "valid": true},
    {"metric": "", "value": 25.0, "timestamp": 1004000, "valid": false},  // 空指标名
    {"metric": "cpu", "value": 0.0, "timestamp": 1005000, "valid": true}
  ]
  
  // 数据质量检查规则
  let mut valid_data = []
  let mut invalid_data = []
  let mut quality_issues = []
  
  let mut i = 0
  while i < test_data.length() {
    let data_point = test_data[i]
    let mut is_valid = true
    let mut issues = []
    
    // 检查数值有效性
    if data_point.value < 0.0 {
      is_valid = false
      issues.push("negative_value")
    }
    
    // 检查时间戳有效性
    if data_point.timestamp <= 0 {
      is_valid = false
      issues.push("invalid_timestamp")
    }
    
    // 检查指标名有效性
    if data_point.metric == "" {
      is_valid = false
      issues.push("empty_metric_name")
    }
    
    let quality_result = {
      "data_index": i,
      "is_valid": is_valid,
      "issues": issues
    }
    
    if is_valid {
      valid_data.push(data_point)
    } else {
      invalid_data.push(data_point)
      quality_issues.push(quality_result)
    }
    
    i = i + 1
  }
  
  // 验证数据质量检查结果
  assert_eq(valid_data.length(), 3)
  assert_eq(invalid_data.length(), 3)
  assert_eq(quality_issues.length(), 3)
  
  // 验证具体质量问题
  assert_eq(quality_issues[0].issues.contains("negative_value"), true)
  assert_eq(quality_issues[1].issues.contains("invalid_timestamp"), true)
  assert_eq(quality_issues[2].issues.contains("empty_metric_name"), true)
  
  // 验证有效数据
  assert_eq(valid_data[0].metric, "cpu")
  assert_eq(valid_data[1].metric, "network")
  assert_eq(valid_data[2].metric, "cpu")
}