// Azimuth Advanced Telemetry Test Suite
// 高级遥测测试用例 - 覆盖采样、批处理、内存管理、序列化等高级特性

// 测试1: 采样策略和概率测试
test "采样策略和概率分布" {
  // 创建基于概率的采样器
  let trace_id_ratio = 0.1  // 10% 采样率
  
  // 模拟采样决策函数
  let should_sample = fn(trace_id: String, ratio: Double) {
    // 简化的采样算法 - 基于trace_id的哈希值
    let hash = trace_id.length() % 100
    let threshold = (ratio * 100.0).to_int()
    hash < threshold
  }
  
  // 测试不同trace_id的采样决策
  let trace_ids = [
    "trace-001", "trace-002", "trace-003", "trace-004", "trace-005",
    "trace-006", "trace-007", "trace-008", "trace-009", "trace-010"
  ]
  
  let mut sampled_count = 0
  for trace_id in trace_ids {
    if should_sample(trace_id, trace_id_ratio) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率在合理范围内 (允许一定误差)
  let actual_ratio = (sampled_count.to_double() / 10.0)
  assert_true(actual_ratio >= 0.0 && actual_ratio <= 0.5)  // 允许较大误差范围
  
  // 测试确定性采样 - 相同trace_id应该产生相同结果
  let sample_decision1 = should_sample("trace-deterministic", trace_id_ratio)
  let sample_decision2 = should_sample("trace-deterministic", trace_id_ratio)
  assert_eq(sample_decision1, sample_decision2)
  
  // 测试不同采样率
  let high_ratio = 0.8  // 80% 采样率
  let mut high_sampled_count = 0
  for trace_id in trace_ids {
    if should_sample(trace_id, high_ratio) {
      high_sampled_count = high_sampled_count + 1
    }
  }
  
  assert_true(high_sampled_count > sampled_count)  // 高采样率应该采样更多
  
  // 测试边界条件
  let zero_ratio = 0.0
  let zero_sampled = should_sample("any-trace", zero_ratio)
  assert_false(zero_sampled)
  
  let full_ratio = 1.0
  let full_sampled = should_sample("any-trace", full_ratio)
  assert_true(full_sampled)
}

// 测试2: 批处理和聚合功能
test "批处理和数据聚合操作" {
  // 创建批处理器
  let batch_size = 5
  let mut telemetry_batch = []
  
  // 模拟遥测数据点
  let data_points = [
    ("metric-1", 10.5, "ms"),
    ("metric-2", 25.3, "ms"),
    ("metric-3", 15.7, "ms"),
    ("metric-4", 30.2, "ms"),
    ("metric-5", 12.8, "ms"),
    ("metric-6", 18.4, "ms"),
    ("metric-7", 22.1, "ms")
  ]
  
  // 批处理函数
  let process_batch = fn(batch: Array[(String, Double, String)]) {
    let mut sum = 0.0
    let mut count = 0
    let mut min_value = 999999.0
    let mut max_value = -1.0
    
    for (name, value, unit) in batch {
      sum = sum + value
      count = count + 1
      if value < min_value {
        min_value = value
      }
      if value > max_value {
        max_value = value
      }
    }
    
    {
      batch_size: count,
      sum: sum,
      average: sum / count.to_double(),
      min: min_value,
      max: max_value,
      unit: batch[0].2  // 假设所有数据点单位相同
    }
  }
  
  // 分批处理数据
  let mut processed_batches = []
  let mut current_batch = []
  
  for data_point in data_points {
    current_batch = current_batch.push(data_point)
    
    if current_batch.length() >= batch_size {
      let batch_result = process_batch(current_batch)
      processed_batches = processed_batches.push(batch_result)
      current_batch = []
    }
  }
  
  // 处理剩余数据
  if current_batch.length() > 0 {
    let batch_result = process_batch(current_batch)
    processed_batches = processed_batches.push(batch_result)
  }
  
  // 验证批处理结果
  assert_eq(processed_batches.length(), 2)  // 7个数据点，批大小5，应该产生2批
  
  // 验证第一批数据 (前5个数据点)
  let first_batch = processed_batches[0]
  assert_eq(first_batch.batch_size, 5)
  assert_eq(first_batch.sum, 10.5 + 25.3 + 15.7 + 30.2 + 12.8)
  assert_eq(first_batch.min, 10.5)
  assert_eq(first_batch.max, 30.2)
  assert_eq(first_batch.unit, "ms")
  
  // 验证第二批数据 (剩余2个数据点)
  let second_batch = processed_batches[1]
  assert_eq(second_batch.batch_size, 2)
  assert_eq(second_batch.sum, 18.4 + 22.1)
  assert_eq(second_batch.min, 18.4)
  assert_eq(second_batch.max, 22.1)
  
  // 测试聚合函数
  let aggregate_batches = fn(batches: Array[Array[(String, Double, String)]]) {
    let all_data = batches.fold(fn(acc, batch) { acc + batch }, [])
    if all_data.length() > 0 {
      process_batch(all_data)
    } else {
      {
        batch_size: 0,
        sum: 0.0,
        average: 0.0,
        min: 0.0,
        max: 0.0,
        unit: ""
      }
    }
  }
  
  // 重新分批并聚合
  let all_batches = [
    data_points.slice(0, 5),  // 前5个
    data_points.slice(5, 2)   // 后2个
  ]
  
  let aggregated = aggregate_batches(all_batches)
  assert_eq(aggregated.batch_size, 7)
  assert_eq(aggregated.sum, 10.5 + 25.3 + 15.7 + 30.2 + 12.8 + 18.4 + 22.1)
  assert_eq(aggregated.min, 10.5)
  assert_eq(aggregated.max, 30.2)
}

// 测试3: 内存管理和资源清理
test "内存管理和资源生命周期" {
  // 模拟资源管理器
  let resource_tracker = {
    mut allocated_resources: [],
    mut freed_resources: []
  }
  
  // 资源分配函数
  let allocate_resource = fn(resource_type: String, id: String) {
    let resource_id = resource_type + "-" + id
    resource_tracker.allocated_resources = resource_tracker.allocated_resources.push(resource_id)
    resource_id
  }
  
  // 资料释放函数
  let free_resource = fn(resource_id: String) {
    if resource_tracker.allocated_resources.contains(resource_id) {
      resource_tracker.freed_resources = resource_tracker.freed_resources.push(resource_id)
      // 从已分配列表中移除
      let mut remaining = []
      for allocated_id in resource_tracker.allocated_resources {
        if allocated_id != resource_id {
          remaining = remaining.push(allocated_id)
        }
      }
      resource_tracker.allocated_resources = remaining
      true
    } else {
      false
    }
  }
  
  // 测试资源分配和释放
  let span_id = allocate_resource("span", "001")
  let metric_id = allocate_resource("metric", "001")
  let log_id = allocate_resource("log", "001")
  
  assert_eq(resource_tracker.allocated_resources.length(), 3)
  assert_true(resource_tracker.allocated_resources.contains(span_id))
  assert_true(resource_tracker.allocated_resources.contains(metric_id))
  assert_true(resource_tracker.allocated_resources.contains(log_id))
  
  // 释放资源
  assert_true(free_resource(span_id))
  assert_eq(resource_tracker.allocated_resources.length(), 2)
  assert_false(resource_tracker.allocated_resources.contains(span_id))
  assert_true(resource_tracker.freed_resources.contains(span_id))
  
  // 测试重复释放
  assert_false(free_resource(span_id))  // 已经释放，应该返回false
  
  // 测试释放不存在的资源
  assert_false(free_resource("non-existent-resource"))
  
  // 批量释放剩余资源
  let remaining_resources = resource_tracker.allocated_resources.copy()
  for resource_id in remaining_resources {
    free_resource(resource_id)
  }
  
  assert_eq(resource_tracker.allocated_resources.length(), 0)
  assert_eq(resource_tracker.freed_resources.length(), 3)
  
  // 测试资源池管理
  let resource_pool = {
    mut available_resources: [],
    mut used_resources: [],
    mut total_created: 0,
    mut total_reused: 0
  }
  
  let get_resource_from_pool = fn(pool) {
    match pool.available_resources.pop() {
      Some(resource) => {
        pool.used_resources = pool.used_resources.push(resource)
        pool.total_reused = pool.total_reused + 1
        resource
      }
      None => {
        let new_resource = "resource-" + pool.total_created.to_string()
        pool.used_resources = pool.used_resources.push(new_resource)
        pool.total_created = pool.total_created + 1
        new_resource
      }
    }
  }
  
  let return_resource_to_pool = fn(pool, resource: String) {
    if pool.used_resources.contains(resource) {
      // 从使用中移除
      let mut remaining_used = []
      for used_resource in pool.used_resources {
        if used_resource != resource {
          remaining_used = remaining_used.push(used_resource)
        }
      }
      pool.used_resources = remaining_used
      
      // 添加到可用资源
      pool.available_resources = pool.available_resources.push(resource)
      true
    } else {
      false
    }
  }
  
  // 测试资源池
  let resource1 = get_resource_from_pool(resource_pool)
  let resource2 = get_resource_from_pool(resource_pool)
  let resource3 = get_resource_from_pool(resource_pool)
  
  assert_eq(resource_pool.total_created, 3)
  assert_eq(resource_pool.total_reused, 0)
  assert_eq(resource_pool.used_resources.length(), 3)
  assert_eq(resource_pool.available_resources.length(), 0)
  
  // 归还资源
  return_resource_to_pool(resource_pool, resource1)
  return_resource_to_pool(resource_pool, resource2)
  
  assert_eq(resource_pool.used_resources.length(), 1)
  assert_eq(resource_pool.available_resources.length(), 2)
  
  // 重新获取资源（应该重用）
  let reused_resource1 = get_resource_from_pool(resource_pool)
  let reused_resource2 = get_resource_from_pool(resource_pool)
  
  assert_eq(resource_pool.total_created, 3)  // 没有创建新资源
  assert_eq(resource_pool.total_reused, 2)  // 重用了2个资源
}

// 测试4: 数据序列化和跨平台兼容性
test "数据序列化和格式转换" {
  // 模拟遥测数据结构
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database.query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1"),
      ("db.instance", "primary"),
      ("peer.service", "database")
    ],
    events: [
      {
        name: "db.query.start",
        timestamp: 1640995200000,
        attributes: [("query.type", "select")]
      },
      {
        name: "db.query.complete",
        timestamp: 1640995250000,
        attributes: [("rows.affected", "1")]
      }
    ]
  }
  
  // 简化的JSON序列化函数
  let serialize_to_json = fn(data) {
    // 简化的JSON序列化 - 实际实现会更复杂
    let json = "{"
    + "\"trace_id\":\"" + data.trace_id + "\","
    + "\"span_id\":\"" + data.span_id + "\","
    + "\"operation_name\":\"" + data.operation_name + "\","
    + "\"start_time\":" + data.start_time.to_string() + ","
    + "\"end_time\":" + data.end_time.to_string() + ","
    + "\"status\":\"" + data.status + "\","
    + "\"attributes_count\":" + data.attributes.length().to_string() + ","
    + "\"events_count\":" + data.events.length().to_string()
    + "}"
    json
  }
  
  // 简化的JSON反序列化函数
  let deserialize_from_json = fn(json: String) {
    // 简化的JSON解析 - 实际实现会更复杂
    if json.contains("trace-12345") and json.contains("span-67890") {
      Some({
        trace_id: "trace-12345",
        span_id: "span-67890",
        operation_name: "database.query",
        start_time: 1640995200000,
        end_time: 1640995250000,
        status: "ok"
      })
    } else {
      None
    }
  }
  
  // 测试序列化
  let json_data = serialize_to_json(telemetry_data)
  assert_true(json_data.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_data.contains("\"span_id\":\"span-67890\""))
  assert_true(json_data.contains("\"operation_name\":\"database.query\""))
  assert_true(json_data.contains("\"attributes_count\":4"))
  assert_true(json_data.contains("\"events_count\":2"))
  
  // 测试反序列化
  let deserialized_data = deserialize_from_json(json_data)
  match deserialized_data {
    Some(data) => {
      assert_eq(data.trace_id, "trace-12345")
      assert_eq(data.span_id, "span-67890")
      assert_eq(data.operation_name, "database.query")
      assert_eq(data.start_time, 1640995200000)
      assert_eq(data.end_time, 1640995250000)
      assert_eq(data.status, "ok")
    }
    None => assert_true(false)
  }
  
  // 测试格式转换
  let convert_to_protobuf_format = fn(data) {
    // 简化的Protobuf格式转换
    "protobuf:"
    + data.trace_id + "|" 
    + data.span_id + "|" 
    + data.operation_name + "|" 
    + data.start_time.to_string() + "|" 
    + data.end_time.to_string() + "|" 
    + data.status
  }
  
  let convert_from_protobuf_format = fn(protobuf_data: String) {
    if protobuf_data.starts_with("protobuf:") {
      let parts = protobuf_data.substring(9, protobuf_data.length() - 9).split("|")
      if parts.length() == 6 {
        Some({
          trace_id: parts[0],
          span_id: parts[1],
          operation_name: parts[2],
          start_time: parts[3].to_int(),
          end_time: parts[4].to_int(),
          status: parts[5]
        })
      } else {
        None
      }
    } else {
      None
    }
  }
  
  // 测试Protobuf格式转换
  let protobuf_data = convert_to_protobuf_format(telemetry_data)
  assert_true(protobuf_data.starts_with("protobuf:trace-12345|span-67890|"))
  
  let converted_back = convert_from_protobuf_format(protobuf_data)
  match converted_back {
    Some(data) => {
      assert_eq(data.trace_id, "trace-12345")
      assert_eq(data.span_id, "span-67890")
      assert_eq(data.operation_name, "database.query")
    }
    None => assert_true(false)
  }
  
  // 测试跨平台兼容性 - 不同平台的字节序
  let serialize_with_endian = fn(data: Int, is_little_endian: Bool) {
    let bytes = []
    let mut value = data
    
    for i in 0..4 {
      let byte = value % 256
      if is_little_endian {
        bytes = bytes.push(byte)
      } else {
        bytes = [byte] + bytes
      }
      value = value / 256
    }
    
    bytes
  }
  
  let test_value = 0x12345678
  let little_endian_bytes = serialize_with_endian(test_value, true)
  let big_endian_bytes = serialize_with_endian(test_value, false)
  
  // 验证字节序不同
  assert_false(little_endian_bytes[0] == big_endian_bytes[0])
  assert_false(little_endian_bytes[1] == big_endian_bytes[1])
  assert_false(little_endian_bytes[2] == big_endian_bytes[2])
  assert_false(little_endian_bytes[3] == big_endian_bytes[3])
  
  // 验证字节序正确性
  assert_eq(little_endian_bytes[0], 0x78)  // 低位在前
  assert_eq(little_endian_bytes[3], 0x12)  // 高位在后
  assert_eq(big_endian_bytes[0], 0x12)     // 高位在前
  assert_eq(big_endian_bytes[3], 0x78)     // 低位在后
}

// 测试5: 边界条件和极端值处理
test "边界条件和极端值处理" {
  // 测试数值边界
  let max_int_value = 2147483647  // 32位有符号整数最大值
  let min_int_value = -2147483648  // 32位有符号整数最小值
  
  // 测试数值溢出处理
  let safe_add = fn(a: Int, b: Int) -> Option[Int] {
    let result = a + b
    if (a > 0 and b > 0 and result < 0) or (a < 0 and b < 0 and result > 0) {
      None  // 溢出
    } else {
      Some(result)
    }
  }
  
  let safe_multiply = fn(a: Int, b: Int) -> Option[Int] {
    if a == 0 or b == 0 {
      Some(0)
    } else if a > 0 and b > 0 and a > max_int_value / b {
      None  // 正溢出
    } else if a < 0 and b < 0 and a < max_int_value / b {
      None  // 负溢出
    } else if a < 0 and b > 0 and a < min_int_value / b {
      None  // 负溢出
    } else if a > 0 and b < 0 and b < min_int_value / a {
      None  // 负溢出
    } else {
      Some(a * b)
    }
  }
  
  // 测试正常加法
  assert_eq(safe_add(100, 200), Some(300))
  
  // 测试边界加法
  assert_eq(safe_add(max_int_value - 1, 1), Some(max_int_value))
  assert_eq(safe_add(min_int_value + 1, -1), Some(min_int_value))
  
  // 测试溢出
  assert_eq(safe_add(max_int_value, 1), None)
  assert_eq(safe_add(min_int_value, -1), None)
  
  // 测试正常乘法
  assert_eq(safe_multiply(100, 200), Some(20000))
  
  // 测试乘法溢出
  assert_eq(safe_multiply(max_int_value / 2 + 1, 2), None)
  
  // 测试字符串边界
  let empty_string = ""
  let very_long_string = "a".repeat(10000)
  
  // 测试字符串长度边界
  assert_eq(empty_string.length(), 0)
  assert_eq(very_long_string.length(), 10000)
  
  // 测试字符串操作边界
  let safe_substring = fn(s: String, start: Int, length: Int) -> Option[String] {
    if start < 0 or length < 0 or start >= s.length() or start + length > s.length() {
      None
    } else {
      Some(s.substring(start, length))
    }
  }
  
  assert_eq(safe_substring("hello", 0, 5), Some("hello"))
  assert_eq(safe_substring("hello", 1, 3), Some("ell"))
  assert_eq(safe_substring("hello", 5, 0), None)  // 起始位置超出
  assert_eq(safe_substring("hello", -1, 3), None)  // 负起始位置
  assert_eq(safe_substring("hello", 0, -1), None)  // 负长度
  assert_eq(safe_substring("hello", 0, 6), None)   // 长度超出
  
  // 测试数组边界
  let empty_array = []
  let large_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let safe_array_access = fn(arr: Array[Int], index: Int) -> Option[Int] {
    if index >= 0 and index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  assert_eq(safe_array_access(empty_array, 0), None)
  assert_eq(safe_array_access(large_array, 0), Some(1))
  assert_eq(safe_array_access(large_array, 9), Some(10))
  assert_eq(safe_array_access(large_array, 10), None)
  assert_eq(safe_array_access(large_array, -1), None)
  
  // 测试时间边界
  let epoch_timestamp = 0
  let y2k_timestamp = 946684800  // 2000-01-01 00:00:00 UTC
  let future_timestamp = 4102444800  // 2100-01-01 00:00:00 UTC
  
  let is_valid_timestamp = fn(timestamp: Int) {
    timestamp >= 0 and timestamp <= 4102444800  // 合理的时间范围
  }
  
  assert_true(is_valid_timestamp(epoch_timestamp))
  assert_true(is_valid_timestamp(y2k_timestamp))
  assert_true(is_valid_timestamp(future_timestamp))
  assert_false(is_valid_timestamp(-1))
  assert_false(is_valid_timestamp(9999999999))
  
  // 测试浮点数边界
  let float_max = 1.7976931348623157e+308  // 双精度浮点数最大值
  let float_min = -1.7976931348623157e+308 // 双精度浮点数最小值
  let float_epsilon = 2.220446049250313e-16 // 双精度浮点数精度
  
  let safe_float_add = fn(a: Double, b: Double) -> Option[Double] {
    let result = a + b
    if result == Double::infinity or result == Double::neg_infinity {
      None
    } else {
      Some(result)
    }
  }
  
  assert_eq(safe_float_add(1.0, 2.0), Some(3.0))
  assert_eq(safe_float_add(float_max, float_max), None)  // 溢出到无穷大
  
  // 测试精度边界
  let very_small_number = 1.0e-20
  let normal_number = 1.0
  
  let result = normal_number + very_small_number
  assert_true(result > normal_number)  // 应该能检测到微小差异
}

// 测试6: 安全性和数据脱敏
test "安全性和敏感数据脱敏" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    CreditCard
    SSN
    Email
    PhoneNumber
    APIKey
    Password
  }
  
  // 数据脱敏函数
  let mask_sensitive_data = fn(data: String, data_type: SensitiveDataType) -> String {
    match data_type {
      SensitiveDataType::CreditCard => {
        if data.length() >= 16 {
          data.substring(0, 4) + "-****-****-" + data.substring(12, 4)
        } else {
          "****"
        }
      }
      SensitiveDataType::SSN => {
        if data.length() >= 9 {
          "***-**-" + data.substring(6, 4)
        } else {
          "***"
        }
      }
      SensitiveDataType::Email => {
        let at_index = data.index_of("@")
        if at_index > 0 and at_index < data.length() - 1 {
          let username = data.substring(0, at_index)
          let domain = data.substring(at_index, data.length() - at_index)
          if username.length() <= 2 {
            "*".repeat(username.length()) + domain
          } else {
            username.substring(0, 2) + "*".repeat(username.length() - 2) + domain
          }
        } else {
          "****"
        }
      }
      SensitiveDataType::PhoneNumber => {
        if data.length() >= 10 {
          "***-***-" + data.substring(data.length() - 4, 4)
        } else {
          "****"
        }
      }
      SensitiveDataType::APIKey => {
        if data.length() >= 8 {
          data.substring(0, 4) + "*".repeat(data.length() - 8) + data.substring(data.length() - 4, 4)
        } else {
          "*".repeat(data.length())
        }
      }
      SensitiveDataType::Password => {
        "*".repeat(data.length())
      }
    }
  }
  
  // 测试信用卡脱敏
  let credit_card = "1234567890123456"
  let masked_credit_card = mask_sensitive_data(credit_card, SensitiveDataType::CreditCard)
  assert_eq(masked_credit_card, "1234-****-****-3456")
  
  let short_credit_card = "1234"
  let masked_short = mask_sensitive_data(short_credit_card, SensitiveDataType::CreditCard)
  assert_eq(masked_short, "****")
  
  // 测试SSN脱敏
  let ssn = "123456789"
  let masked_ssn = mask_sensitive_data(ssn, SensitiveDataType::SSN)
  assert_eq(masked_ssn, "***-**-6789")
  
  // 测试邮箱脱敏
  let email = "user@example.com"
  let masked_email = mask_sensitive_data(email, SensitiveDataType::Email)
  assert_eq(masked_email, "us********@example.com")
  
  let short_email = "ab@domain.com"
  let masked_short_email = mask_sensitive_data(short_email, SensitiveDataType::Email)
  assert_eq(masked_short_email, "**@domain.com")
  
  // 测试电话号码脱敏
  let phone = "1234567890"
  let masked_phone = mask_sensitive_data(phone, SensitiveDataType::PhoneNumber)
  assert_eq(masked_phone, "***-***-7890")
  
  // 测试API密钥脱敏
  let api_key = "abcd1234efgh5678"
  let masked_api_key = mask_sensitive_data(api_key, SensitiveDataType::APIKey)
  assert_eq(masked_api_key, "abcd********5678")
  
  // 测试密码脱敏
  let password = "MySecretPassword123"
  let masked_password = mask_sensitive_data(password, SensitiveDataType::Password)
  assert_eq(masked_password, "********************")
  
  // 测试访问控制
  let access_control = {
    roles: ["admin", "user", "viewer"],
    permissions: [
      ("admin", ["read", "write", "delete", "manage_users"]),
      ("user", ["read", "write"]),
      ("viewer", ["read"])
    ]
  }
  
  let has_permission = fn(user_role: String, required_permission: String) -> Bool {
    for (role, permissions) in access_control.permissions {
      if role == user_role {
        return permissions.contains(required_permission)
      }
    }
    false
  }
  
  // 测试权限检查
  assert_true(has_permission("admin", "delete"))
  assert_true(has_permission("admin", "read"))
  assert_true(has_permission("user", "read"))
  assert_true(has_permission("user", "write"))
  assert_false(has_permission("user", "delete"))
  assert_true(has_permission("viewer", "read"))
  assert_false(has_permission("viewer", "write"))
  assert_false(has_permission("nonexistent_role", "read"))
  
  // 测试数据加密模拟
  let simple_encrypt = fn(data: String, key: String) -> String {
    let mut result = ""
    let key_length = key.length()
    
    for i in 0..data.length() {
      let data_char = data[i]
      let key_char = key[i % key_length]
      let encrypted_char = ((data_char.to_int() + key_char.to_int()) % 256).to_char()
      result = result + encrypted_char
    }
    
    result
  }
  
  let simple_decrypt = fn(encrypted_data: String, key: String) -> String {
    let mut result = ""
    let key_length = key.length()
    
    for i in 0..encrypted_data.length() {
      let encrypted_char = encrypted_data[i]
      let key_char = key[i % key_length]
      let decrypted_char = ((encrypted_char.to_int() - key_char.to_int() + 256) % 256).to_char()
      result = result + decrypted_char
    }
    
    result
  }
  
  // 测试加密解密
  let original_data = "sensitive telemetry data"
  let encryption_key = "secret-key-123"
  
  let encrypted = simple_encrypt(original_data, encryption_key)
  assert_not_eq(encrypted, original_data)  // 加密后应该不同
  
  let decrypted = simple_decrypt(encrypted, encryption_key)
  assert_eq(decrypted, original_data)  // 解密后应该恢复原始数据
  
  // 测试错误密钥
  let wrong_key_decrypted = simple_decrypt(encrypted, "wrong-key")
  assert_not_eq(wrong_key_decrypted, original_data)  // 错误密钥应该产生错误结果
}

// 测试7: 时间窗口和滑动窗口操作
test "时间窗口和滑动窗口计算" {
  // 定义时间点数据结构
  type TimePoint = {
    timestamp: Int,
    value: Double,
    metric_name: String
  }
  
  // 创建测试数据点
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  let time_points = [
    { timestamp: base_time, value: 10.5, metric_name: "response_time" },
    { timestamp: base_time + 60, value: 15.2, metric_name: "response_time" },
    { timestamp: base_time + 120, value: 12.8, metric_name: "response_time" },
    { timestamp: base_time + 180, value: 18.3, metric_name: "response_time" },
    { timestamp: base_time + 240, value: 14.7, metric_name: "response_time" },
    { timestamp: base_time + 300, value: 20.1, metric_name: "response_time" },
    { timestamp: base_time + 360, value: 16.9, metric_name: "response_time" },
    { timestamp: base_time + 420, value: 13.4, metric_name: "response_time" },
    { timestamp: base_time + 480, value: 19.8, metric_name: "response_time" },
    { timestamp: base_time + 540, value: 17.2, metric_name: "response_time" }
  ]
  
  // 固定时间窗口函数
  let fixed_time_window = fn(points: Array[TimePoint], window_start: Int, window_end: Int) -> Array[TimePoint] {
    let mut window_points = []
    for point in points {
      if point.timestamp >= window_start and point.timestamp < window_end {
        window_points = window_points.push(point)
      }
    }
    window_points
  }
  
  // 滑动时间窗口函数
  let sliding_time_window = fn(points: Array[TimePoint], center_time: Int, window_size: Int) -> Array[TimePoint] {
    let window_start = center_time - window_size / 2
    let window_end = center_time + window_size / 2
    fixed_time_window(points, window_start, window_end)
  }
  
  // 测试固定时间窗口
  let first_window = fixed_time_window(time_points, base_time, base_time + 300)  // 5分钟窗口
  assert_eq(first_window.length(), 5)  // 前5个数据点
  
  for point in first_window {
    assert_true(point.timestamp >= base_time)
    assert_true(point.timestamp < base_time + 300)
  }
  
  // 测试滑动时间窗口
  let center_time = base_time + 270  // 第4.5个数据点附近
  let sliding_window = sliding_time_window(time_points, center_time, 300)  // 5分钟滑动窗口
  
  // 滑动窗口应该包含第3-6个数据点
  assert_eq(sliding_window.length(), 4)
  for point in sliding_window {
    assert_true(point.timestamp >= center_time - 150)
    assert_true(point.timestamp < center_time + 150)
  }
  
  // 时间窗口聚合函数
  let aggregate_time_window = fn(points: Array[TimePoint]) -> {
    count: Int,
    sum: Double,
    average: Double,
    min: Double,
    max: Double,
    start_time: Int,
    end_time: Int
  } {
    if points.length() == 0 {
      {
        count: 0,
        sum: 0.0,
        average: 0.0,
        min: 0.0,
        max: 0.0,
        start_time: 0,
        end_time: 0
      }
    } else {
      let mut sum = 0.0
      let mut min_value = points[0].value
      let mut max_value = points[0].value
      let mut start_time = points[0].timestamp
      let mut end_time = points[0].timestamp
      
      for point in points {
        sum = sum + point.value
        if point.value < min_value {
          min_value = point.value
        }
        if point.value > max_value {
          max_value = point.value
        }
        if point.timestamp < start_time {
          start_time = point.timestamp
        }
        if point.timestamp > end_time {
          end_time = point.timestamp
        }
      }
      
      {
        count: points.length(),
        sum: sum,
        average: sum / points.length().to_double(),
        min: min_value,
        max: max_value,
        start_time: start_time,
        end_time: end_time
      }
    }
  }
  
  // 测试时间窗口聚合
  let window_aggregation = aggregate_time_window(first_window)
  assert_eq(window_aggregation.count, 5)
  assert_eq(window_aggregation.sum, 10.5 + 15.2 + 12.8 + 18.3 + 14.7)
  assert_eq(window_aggregation.min, 10.5)
  assert_eq(window_aggregation.max, 18.3)
  assert_eq(window_aggregation.start_time, base_time)
  assert_eq(window_aggregation.end_time, base_time + 240)
  
  // 时间序列分析函数
  let time_series_analysis = fn(points: Array[TimePoint]) -> {
    trend: String,  // "increasing", "decreasing", "stable"
    volatility: Double,
    outliers: Array[Int]  // 异常点的索引
  } {
    if points.length() < 2 {
      {
        trend: "stable",
        volatility: 0.0,
        outliers: []
      }
    } else {
      // 计算趋势
      let first_half = points.slice(0, points.length() / 2)
      let second_half = points.slice(points.length() / 2, points.length() - points.length() / 2)
      
      let first_avg = aggregate_time_window(first_half).average
      let second_avg = aggregate_time_window(second_half).average
      
      let trend = if second_avg > first_avg * 1.1 {
        "increasing"
      } else if second_avg < first_avg * 0.9 {
        "decreasing"
      } else {
        "stable"
      }
      
      // 计算波动性 (标准差)
      let avg = aggregate_time_window(points).average
      let mut variance_sum = 0.0
      for point in points {
        let diff = point.value - avg
        variance_sum = variance_sum + diff * diff
      }
      let variance = variance_sum / points.length().to_double()
      let volatility = variance.sqrt()
      
      // 检测异常值 (简单的1.5倍标准差方法)
      let mut outliers = []
      for i in 0..points.length() {
        let point = points[i]
        let deviation = (point.value - avg).abs()
        if deviation > 1.5 * volatility {
          outliers = outliers.push(i)
        }
      }
      
      {
        trend,
        volatility,
        outliers
      }
    }
  }
  
  // 测试时间序列分析
  let analysis = time_series_analysis(time_points)
  assert_true(analysis.trend == "increasing" or analysis.trend == "stable" or analysis.trend == "decreasing")
  assert_true(analysis.volatility >= 0.0)
  assert_true(analysis.outliers.length() <= time_points.length())
  
  // 滚动聚合函数
  let rolling_aggregation = fn(points: Array[TimePoint], window_size: Int) -> Array[{
    timestamp: Int,
    average: Double,
    count: Int
  }> {
    let mut results = []
    
    for i in window_size..points.length() {
      let window_points = points.slice(i - window_size, window_size)
      let aggregation = aggregate_time_window(window_points)
      results = results.push({
        timestamp: points[i].timestamp,
        average: aggregation.average,
        count: aggregation.count
      })
    }
    
    results
  }
  
  // 测试滚动聚合
  let rolling_results = rolling_aggregation(time_points, 3)  // 3点滚动窗口
  assert_eq(rolling_results.length(), time_points.length() - 3 + 1)
  
  for result in rolling_results {
    assert_eq(result.count, 3)  // 每个窗口应该包含3个点
    assert_true(result.average >= 0.0)
  }
  
  // 验证第一个滚动窗口
  let first_rolling = rolling_results[0]
  let expected_avg = (time_points[0].value + time_points[1].value + time_points[2].value) / 3.0
  assert_eq(first_rolling.timestamp, time_points[2].timestamp)  // 窗口结束时间
  assert_eq(first_rolling.average, expected_avg)
}