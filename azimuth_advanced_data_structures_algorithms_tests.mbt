// Azimuth Advanced Data Structures and Algorithms Test Suite
// 高级数据结构和算法测试套件，验证遥测系统中的复杂数据操作和算法实现

// 测试1: 高效属性索引结构
test "高效属性索引结构测试" {
  // 创建一个高效的属性索引结构，支持快速查找
  let mut attr_index = azimuth::AttributeIndex::new()
  
  // 添加1000个属性到索引
  for i in 0..1000 {
    let key = "attr.key." + i.to_string()
    let value = azimuth::AttributeValue::StringValue("value." + i.to_string())
    attr_index = azimuth::AttributeIndex::add(attr_index, key, value)
  }
  
  // 测试快速查找功能
  let result = azimuth::AttributeIndex::get(attr_index, "attr.key.500")
  match result {
    Some(azimuth::AttributeValue::StringValue(v)) => assert_eq(v, "value.500")
    _ => assert_true(false)
  }
  
  // 测试不存在的键
  let non_existent = azimuth::AttributeIndex::get(attr_index, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试前缀查找
  let prefix_results = azimuth::AttributeIndex::get_by_prefix(attr_index, "attr.key.5")
  assert_eq(prefix_results.length(), 100) // attr.key.500到attr.key.599
  
  // 验证前缀查找结果
  match prefix_results[0] {
    (key, azimuth::AttributeValue::StringValue(value)) => {
      assert_eq(key, "attr.key.500")
      assert_eq(value, "value.500")
    }
    _ => assert_true(false)
  }
}

// 测试2: 时间窗口数据聚合算法
test "时间窗口数据聚合算法测试" {
  // 创建时间窗口聚合器
  let window_size = 60000 // 60秒窗口
  let mut aggregator = azimuth::TimeWindowAggregator::new(window_size)
  
  // 添加不同时间点的数据点
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 在窗口内添加数据点
  for i in 0..100 {
    let timestamp = base_time + (i * 600) // 每600ms一个数据点
    let value = i.to_float() * 1.5
    let data_point = azimuth::DataPoint::new(timestamp, value)
    aggregator = azimuth::TimeWindowAggregator::add(aggregator, data_point)
  }
  
  // 测试窗口内数据点计数
  let count = azimuth::TimeWindowAggregator::count(aggregator)
  assert_eq(count, 100)
  
  // 测试窗口内数据求和
  let sum = azimuth::TimeWindowAggregator::sum(aggregator)
  let expected_sum = 0.0 + 1.5 + 3.0 + ... + 148.5 // 等差数列求和
  assert_eq(sum, expected_sum)
  
  // 测试窗口内数据平均值
  let avg = azimuth::TimeWindowAggregator::average(aggregator)
  assert_eq(avg, expected_sum / 100.0)
  
  // 测试窗口内数据最大值和最小值
  let max = azimuth::TimeWindowAggregator::max(aggregator)
  let min = azimuth::TimeWindowAggregator::min(aggregator)
  assert_eq(max, 148.5)
  assert_eq(min, 0.0)
  
  // 测试窗口外数据点（应该被忽略）
  let out_of_window_point = azimuth::DataPoint::new(base_time + window_size + 1000, 200.0)
  aggregator = azimuth::TimeWindowAggregator::add(aggregator, out_of_window_point)
  
  // 窗口内数据点计数应该不变
  let count_after = azimuth::TimeWindowAggregator::count(aggregator)
  assert_eq(count_after, 100)
}

// 测试3: 层级属性树结构
test "层级属性树结构测试" {
  // 创建一个层级属性树
  let mut tree = azimuth::AttributeTree::new()
  
  // 添加层级属性
  tree = azimuth::AttributeTree::set(tree, ["service", "name"], azimuth::AttributeValue::StringValue("azimuth-service"))
  tree = azimuth::AttributeTree::set(tree, ["service", "version"], azimuth::AttributeValue::StringValue("1.0.0"))
  tree = azimuth::AttributeTree::set(tree, ["service", "instance", "id"], azimuth::AttributeValue::StringValue("instance-123"))
  tree = azimuth::AttributeTree::set(tree, ["http", "method"], azimuth::AttributeValue::StringValue("GET"))
  tree = azimuth::AttributeTree::set(tree, ["http", "status"], azimuth::AttributeValue::IntValue(200))
  tree = azimuth::AttributeTree::set(tree, ["http", "duration"], azimuth::AttributeValue::FloatValue(125.5))
  
  // 测试获取单个属性
  let service_name = azimuth::AttributeTree::get(tree, ["service", "name"])
  match service_name {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let http_status = azimuth::AttributeTree::get(tree, ["http", "status"])
  match http_status {
    Some(azimuth::AttributeValue::IntValue(status)) => assert_eq(status, 200)
    _ => assert_true(false)
  }
  
  // 测试获取子树
  let service_subtree = azimuth::AttributeTree::get_subtree(tree, ["service"])
  match service_subtree {
    Some(subtree) => {
      let name = azimuth::AttributeTree::get(subtree, ["name"])
      let version = azimuth::AttributeTree::get(subtree, ["version"])
      let instance = azimuth::AttributeTree::get(subtree, ["instance", "id"])
      
      match name {
        Some(azimuth::AttributeValue::StringValue(n)) => assert_eq(n, "azimuth-service")
        _ => assert_true(false)
      }
      
      match version {
        Some(azimuth::AttributeValue::StringValue(v)) => assert_eq(v, "1.0.0")
        _ => assert_true(false)
      }
      
      match instance {
        Some(azimuth::AttributeValue::StringValue(i)) => assert_eq(i, "instance-123")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试获取所有键
  let all_keys = azimuth::AttributeTree::get_all_keys(tree)
  assert_eq(all_keys.length(), 6)
  assert_true(all_keys.contains(["service", "name"]))
  assert_true(all_keys.contains(["service", "version"]))
  assert_true(all_keys.contains(["service", "instance", "id"]))
  assert_true(all_keys.contains(["http", "method"]))
  assert_true(all_keys.contains(["http", "status"]))
  assert_true(all_keys.contains(["http", "duration"]))
}

// 测试4: 高效采样算法
test "高效采样算法测试" {
  // 创建概率采样器
  let probability = 0.1 // 10%采样率
  let sampler = azimuth::ProbabilitySampler::new(probability)
  
  // 测试采样决策一致性（相同trace_id应该得到相同决策）
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let decision1 = azimuth::ProbabilitySampler::should_sample(sampler, trace_id)
  let decision2 = azimuth::ProbabilitySampler::should_sample(sampler, trace_id)
  assert_eq(decision1, decision2) // 相同trace_id应该得到相同决策
  
  // 测试不同trace_id的采样分布
  let mut sample_count = 0
  let total_traces = 10000
  
  for i in 0..total_traces {
    let test_trace_id = "trace-" + i.to_string()
    if azimuth::ProbabilitySampler::should_sample(sampler, test_trace_id) {
      sample_count = sample_count + 1
    }
  }
  
  // 采样率应该接近10%（允许一定误差）
  let actual_rate = sample_count.to_float() / total_traces.to_float()
  assert_true(actual_rate > 0.09 && actual_rate < 0.11)
  
  // 测试确定性采样器
  let deterministic_sampler = azimuth::DeterministicSampler::new()
  
  // 测试已知trace_id的采样决策
  let sampled_trace = "4bf92f3577b34da6a3ce929d0e0e4736"
  let not_sampled_trace = "4bf92f3577b34da6a3ce929d0e0e4737"
  
  assert_true(azimuth::DeterministicSampler::should_sample(deterministic_sampler, sampled_trace))
  assert_false(azimuth::DeterministicSampler::should_sample(deterministic_sampler, not_sampled_trace))
}

// 测试5: 属性值高效序列化算法
test "属性值高效序列化算法测试" {
  // 测试不同类型属性值的序列化
  let string_attr = azimuth::AttributeValue::StringValue("test string")
  let int_attr = azimuth::AttributeValue::IntValue(42)
  let float_attr = azimuth::AttributeValue::FloatValue(3.14159)
  let bool_attr = azimuth::AttributeValue::BoolValue(true)
  let array_string_attr = azimuth::AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_int_attr = azimuth::AttributeValue::ArrayIntValue([1, 2, 3])
  
  // 序列化属性值
  let serialized_string = azimuth::AttributeValue::serialize(string_attr)
  let serialized_int = azimuth::AttributeValue::serialize(int_attr)
  let serialized_float = azimuth::AttributeValue::serialize(float_attr)
  let serialized_bool = azimuth::AttributeValue::serialize(bool_attr)
  let serialized_array_string = azimuth::AttributeValue::serialize(array_string_attr)
  let serialized_array_int = azimuth::AttributeValue::serialize(array_int_attr)
  
  // 验证序列化结果
  assert_eq(serialized_string, "{\"type\":\"string\",\"value\":\"test string\"}")
  assert_eq(serialized_int, "{\"type\":\"int\",\"value\":42}")
  assert_eq(serialized_float, "{\"type\":\"float\",\"value\":3.14159}")
  assert_eq(serialized_bool, "{\"type\":\"bool\",\"value\":true}")
  assert_eq(serialized_array_string, "{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}")
  assert_eq(serialized_array_int, "{\"type\":\"array_int\",\"value\":[1,2,3]}")
  
  // 测试反序列化
  let deserialized_string = azimuth::AttributeValue::deserialize(serialized_string)
  let deserialized_int = azimuth::AttributeValue::deserialize(serialized_int)
  let deserialized_float = azimuth::AttributeValue::deserialize(serialized_float)
  let deserialized_bool = azimuth::AttributeValue::deserialize(serialized_bool)
  let deserialized_array_string = azimuth::AttributeValue::deserialize(serialized_array_string)
  let deserialized_array_int = azimuth::AttributeValue::deserialize(serialized_array_int)
  
  // 验证反序列化结果
  match deserialized_string {
    Some(azimuth::AttributeValue::StringValue(s)) => assert_eq(s, "test string")
    _ => assert_true(false)
  }
  
  match deserialized_int {
    Some(azimuth::AttributeValue::IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match deserialized_float {
    Some(azimuth::AttributeValue::FloatValue(f)) => assert_eq(f, 3.14159)
    _ => assert_true(false)
  }
  
  match deserialized_bool {
    Some(azimuth::AttributeValue::BoolValue(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  match deserialized_array_string {
    Some(azimuth::AttributeValue::ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  match deserialized_array_int {
    Some(azimuth::AttributeValue::ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false)
  }
}

// 测试6: 高效属性过滤器
test "高效属性过滤器测试" {
  // 创建属性集合
  let attrs = azimuth::Attributes::new()
  attrs = azimuth::Attributes::set(attrs, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  attrs = azimuth::Attributes::set(attrs, "service.version", azimuth::AttributeValue::StringValue("2.1.0"))
  attrs = azimuth::Attributes::set(attrs, "http.method", azimuth::AttributeValue::StringValue("POST"))
  attrs = azimuth::Attributes::set(attrs, "http.status", azimuth::AttributeValue::IntValue(200))
  attrs = azimuth::Attributes::set(attrs, "http.duration", azimuth::AttributeValue::FloatValue(150.5))
  attrs = azimuth::Attributes::set(attrs, "user.id", azimuth::AttributeValue::StringValue("user-12345"))
  attrs = azimuth::Attributes::set(attrs, "transaction.id", azimuth::AttributeValue::StringValue("txn-abcde"))
  
  // 测试精确匹配过滤器
  let exact_filter = azimuth::ExactMatchFilter::new("service.name", "payment-service")
  assert_true(azimuth::ExactMatchFilter::matches(exact_filter, attrs))
  
  let non_match_filter = azimuth::ExactMatchFilter::new("service.name", "order-service")
  assert_false(azimuth::ExactMatchFilter::matches(non_match_filter, attrs))
  
  // 测试范围过滤器
  let status_range_filter = azimuth::RangeFilter::new("http.status", 200, 299)
  assert_true(azimuth::RangeFilter::matches(status_range_filter, attrs))
  
  let high_status_filter = azimuth::RangeFilter::new("http.status", 400, 499)
  assert_false(azimuth::RangeFilter::matches(high_status_filter, attrs))
  
  let duration_filter = azimuth::RangeFilter::new("http.duration", 100.0, 200.0)
  assert_true(azimuth::RangeFilter::matches(duration_filter, attrs))
  
  // 测试前缀过滤器
  let prefix_filter = azimuth::PrefixFilter::new("user.id", "user-")
  assert_true(azimuth::PrefixFilter::matches(prefix_filter, attrs))
  
  let wrong_prefix_filter = azimuth::PrefixFilter::new("user.id", "admin-")
  assert_false(azimuth::PrefixFilter::matches(wrong_prefix_filter, attrs))
  
  // 测试复合过滤器（AND逻辑）
  let and_filter = azimuth::AndFilter::new([
    azimuth::ExactMatchFilter::new("service.name", "payment-service"),
    azimuth::RangeFilter::new("http.status", 200, 299),
    azimuth::PrefixFilter::new("user.id", "user-")
  ])
  assert_true(azimuth::AndFilter::matches(and_filter, attrs))
  
  let and_filter_fail = azimuth::AndFilter::new([
    azimuth::ExactMatchFilter::new("service.name", "payment-service"),
    azimuth::RangeFilter::new("http.status", 400, 499),
    azimuth::PrefixFilter::new("user.id", "user-")
  ])
  assert_false(azimuth::AndFilter::matches(and_filter_fail, attrs))
  
  // 测试复合过滤器（OR逻辑）
  let or_filter = azimuth::OrFilter::new([
    azimuth::ExactMatchFilter::new("service.name", "order-service"),
    azimuth::RangeFilter::new("http.status", 200, 299),
    azimuth::PrefixFilter::new("admin.id", "admin-")
  ])
  assert_true(azimuth::OrFilter::matches(or_filter, attrs))
  
  let or_filter_fail = azimuth::OrFilter::new([
    azimuth::ExactMatchFilter::new("service.name", "order-service"),
    azimuth::RangeFilter::new("http.status", 400, 499),
    azimuth::PrefixFilter::new("admin.id", "admin-")
  ])
  assert_false(azimuth::OrFilter::matches(or_filter_fail, attrs))
}

// 测试7: 高效属性聚合算法
test "高效属性聚合算法测试" {
  // 创建多个属性集合
  let attrs1 = azimuth::Attributes::new()
  attrs1 = azimuth::Attributes::set(attrs1, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  attrs1 = azimuth::Attributes::set(attrs1, "http.method", azimuth::AttributeValue::StringValue("POST"))
  attrs1 = azimuth::Attributes::set(attrs1, "http.status", azimuth::AttributeValue::IntValue(200))
  attrs1 = azimuth::Attributes::set(attrs1, "http.duration", azimuth::AttributeValue::FloatValue(120.5))
  
  let attrs2 = azimuth::Attributes::new()
  attrs2 = azimuth::Attributes::set(attrs2, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  attrs2 = azimuth::Attributes::set(attrs2, "http.method", azimuth::AttributeValue::StringValue("GET"))
  attrs2 = azimuth::Attributes::set(attrs2, "http.status", azimuth::AttributeValue::IntValue(200))
  attrs2 = azimuth::Attributes::set(attrs2, "http.duration", azimuth::AttributeValue::FloatValue(85.2))
  
  let attrs3 = azimuth::Attributes::new()
  attrs3 = azimuth::Attributes::set(attrs3, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  attrs3 = azimuth::Attributes::set(attrs3, "http.method", azimuth::AttributeValue::StringValue("POST"))
  attrs3 = azimuth::Attributes::set(attrs3, "http.status", azimuth::AttributeValue::IntValue(400))
  attrs3 = azimuth::Attributes::set(attrs3, "http.duration", azimuth::AttributeValue::FloatValue(45.8))
  
  let attrs4 = azimuth::Attributes::new()
  attrs4 = azimuth::Attributes::set(attrs4, "service.name", azimuth::AttributeValue::StringValue("order-service"))
  attrs4 = azimuth::Attributes::set(attrs4, "http.method", azimuth::AttributeValue::StringValue("POST"))
  attrs4 = azimuth::Attributes::set(attrs4, "http.status", azimuth::AttributeValue::IntValue(200))
  attrs4 = azimuth::Attributes::set(attrs4, "http.duration", azimuth::AttributeValue::FloatValue(200.3))
  
  let attributes_list = [attrs1, attrs2, attrs3, attrs4]
  
  // 测试按属性值分组聚合
  let grouped_by_status = azimuth::AttributeAggregator::group_by(attributes_list, "http.status")
  assert_eq(grouped_by_status.size(), 2) // 200和400两种状态码
  
  let status_200_group = grouped_by_status.get("200")
  match status_200_group {
    Some(group) => assert_eq(group.length(), 3)
    None => assert_true(false)
  }
  
  let status_400_group = grouped_by_status.get("400")
  match status_400_group {
    Some(group) => assert_eq(group.length(), 1)
    None => assert_true(false)
  }
  
  // 测试数值属性统计聚合
  let duration_stats = azimuth::AttributeAggregator::aggregate_numeric(attributes_list, "http.duration")
  assert_eq(duration_stats.count, 4)
  assert_eq(duration_stats.sum, 120.5 + 85.2 + 45.8 + 200.3)
  assert_eq(duration_stats.min, 45.8)
  assert_eq(duration_stats.max, 200.3)
  assert_eq(duration_stats.average, (120.5 + 85.2 + 45.8 + 200.3) / 4.0)
  
  // 测试分位数计算
  let durations = [45.8, 85.2, 120.5, 200.3]
  let p50 = azimuth::AttributeAggregator::percentile(durations, 0.5)
  let p95 = azimuth::AttributeAggregator::percentile(durations, 0.95)
  let p99 = azimuth::AttributeAggregator::percentile(durations, 0.99)
  
  assert_eq(p50, 102.85) // (85.2 + 120.5) / 2
  assert_eq(p95, 200.3)
  assert_eq(p99, 200.3)
  
  // 测试频率统计
  let method_freq = azimuth::AttributeAggregator::frequency(attributes_list, "http.method")
  assert_eq(method_freq.get("POST"), Some(3))
  assert_eq(method_freq.get("GET"), Some(1))
  assert_eq(method_freq.get("PUT"), None)
}

// 测试8: 高效属性转换算法
test "高效属性转换算法测试" {
  // 创建原始属性集合
  let raw_attrs = azimuth::Attributes::new()
  raw_attrs = azimuth::Attributes::set(raw_attrs, "http.status_code", azimuth::AttributeValue::IntValue(200))
  raw_attrs = azimuth::Attributes::set(raw_attrs, "http.response_time_ms", azimuth::AttributeValue::IntValue(150))
  raw_attrs = azimuth::Attributes::set(raw_attrs, "service.version", azimuth::AttributeValue::StringValue("1.2.3"))
  raw_attrs = azimuth::Attributes::set(raw_attrs, "error.count", azimuth::AttributeValue::IntValue(0))
  raw_attrs = azimuth::Attributes::set(raw_attrs, "memory.usage", azimuth::AttributeValue::IntValue(5242880)) // 字节
  
  // 定义转换规则
  let transform_rules = [
    // 将状态码转换为状态类别
    ("http.status_code", azimuth::TransformRule::Category([
      (100, 199, "informational"),
      (200, 299, "success"),
      (300, 399, "redirection"),
      (400, 499, "client_error"),
      (500, 599, "server_error")
    ])),
    
    // 将响应时间从毫秒转换为秒
    ("http.response_time_ms", azimuth::TransformRule::UnitConversion("ms", "s", 0.001)),
    
    // 从版本字符串提取主版本号
    ("service.version", azimuth::TransformRule::RegexExtract(r"(\d+)\.\d+\.\d+", "$1")),
    
    // 将错误计数转换为布尔标志
    ("error.count", azimuth::TransformRule::Threshold(0, "no_error", "has_error")),
    
    // 将内存使用从字节转换为MB
    ("memory.usage", azimuth::TransformRule::UnitConversion("bytes", "MB", 0.000001))
  ]
  
  // 应用转换规则
  let transformed_attrs = azimuth::AttributeTransformer::transform(raw_attrs, transform_rules)
  
  // 验证转换结果
  let status_category = azimuth::Attributes::get(transformed_attrs, "http.status_code.category")
  match status_category {
    Some(azimuth::AttributeValue::StringValue(category)) => assert_eq(category, "success")
    _ => assert_true(false)
  }
  
  let response_time_sec = azimuth::Attributes::get(transformed_attrs, "http.response_time_ms.s")
  match response_time_sec {
    Some(azimuth::AttributeValue::FloatValue(seconds)) => assert_eq(seconds, 0.15)
    _ => assert_true(false)
  }
  
  let major_version = azimuth::Attributes::get(transformed_attrs, "service.version.major")
  match major_version {
    Some(azimuth::AttributeValue::StringValue(version)) => assert_eq(version, "1")
    _ => assert_true(false)
  }
  
  let error_flag = azimuth::Attributes::get(transformed_attrs, "error.count.flag")
  match error_flag {
    Some(azimuth::AttributeValue::StringValue(flag)) => assert_eq(flag, "no_error")
    _ => assert_true(false)
  }
  
  let memory_mb = azimuth::Attributes::get(transformed_attrs, "memory.usage.MB")
  match memory_mb {
    Some(azimuth::AttributeValue::FloatValue(mb)) => assert_eq(mb, 5.24288)
    _ => assert_true(false)
  }
}

// 测试9: 高效属性合并算法
test "高效属性合并算法测试" {
  // 创建多个属性集合，模拟不同来源的数据
  let base_attrs = azimuth::Attributes::new()
  base_attrs = azimuth::Attributes::set(base_attrs, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  base_attrs = azimuth::Attributes::set(base_attrs, "service.version", azimuth::AttributeValue::StringValue("1.0.0"))
  base_attrs = azimuth::Attributes::set(base_attrs, "host.name", azimuth::AttributeValue::StringValue("host-001"))
  
  let runtime_attrs = azimuth::Attributes::new()
  runtime_attrs = azimuth::Attributes::set(runtime_attrs, "service.version", azimuth::AttributeValue::StringValue("1.0.1")) // 覆盖基础版本
  runtime_attrs = azimuth::Attributes::set(runtime_attrs, "runtime.start_time", azimuth::AttributeValue::StringValue("2023-01-01T00:00:00Z"))
  runtime_attrs = azimuth::Attributes::set(runtime_attrs, "runtime.pid", azimuth::AttributeValue::IntValue(12345))
  
  let request_attrs = azimuth::Attributes::new()
  request_attrs = azimuth::Attributes::set(request_attrs, "http.method", azimuth::AttributeValue::StringValue("POST"))
  request_attrs = azimuth::Attributes::set(request_attrs, "http.url", azimuth::AttributeValue::StringValue("/api/payments"))
  request_attrs = azimuth::Attributes::set(request_attrs, "http.status", azimuth::AttributeValue::IntValue(200))
  
  let user_attrs = azimuth::Attributes::new()
  user_attrs = azimuth::Attributes::set(user_attrs, "user.id", azimuth::AttributeValue::StringValue("user-123"))
  user_attrs = azimuth::Attributes::set(user_attrs, "user.role", azimuth::AttributeValue::StringValue("customer"))
  user_attrs = azimuth::Attributes::set(user_attrs, "user.region", azimuth::AttributeValue::StringValue("us-west"))
  
  // 测试简单合并策略（后添加的覆盖先添加的）
  let simple_merged = azimuth::AttributeMerger::merge_simple([base_attrs, runtime_attrs, request_attrs, user_attrs])
  
  // 验证合并结果
  let service_name = azimuth::Attributes::get(simple_merged, "service.name")
  match service_name {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  let service_version = azimuth::Attributes::get(simple_merged, "service.version")
  match service_version {
    Some(azimuth::AttributeValue::StringValue(version)) => assert_eq(version, "1.0.1") // 应该是runtime_attrs中的版本
    _ => assert_true(false)
  }
  
  // 测试优先级合并策略
  let priority_rules = [
    ("user.", 10), // 用户属性优先级最高
    ("request.", 8), // 请求属性次之
    ("runtime.", 6), // 运行时属性再次之
    ("base.", 4) // 基础属性优先级最低
  ]
  
  let priority_merged = azimuth::AttributeMerger::merge_with_priority(
    [base_attrs, runtime_attrs, request_attrs, user_attrs],
    priority_rules
  )
  
  // 验证优先级合并结果（应该与简单合并相同，因为属性键没有冲突）
  let user_id = azimuth::Attributes::get(priority_merged, "user.id")
  match user_id {
    Some(azimuth::AttributeValue::StringValue(id)) => assert_eq(id, "user-123")
    _ => assert_true(false)
  }
  
  // 测试数组合并策略
  let attrs_with_arrays1 = azimuth::Attributes::new()
  attrs_with_arrays1 = azimuth::Attributes::set(attrs_with_arrays1, "tags", azimuth::AttributeValue::ArrayStringValue(["payment", "api", "v1"]))
  
  let attrs_with_arrays2 = azimuth::Attributes::new()
  attrs_with_arrays2 = azimuth::Attributes::set(attrs_with_arrays2, "tags", azimuth::AttributeValue::ArrayStringValue(["critical", "production"]))
  
  let array_merged = azimuth::AttributeMerger::merge_with_array_strategy(
    [attrs_with_arrays1, attrs_with_arrays2],
    "union" // 合并策略：并集
  )
  
  let merged_tags = azimuth::Attributes::get(array_merged, "tags")
  match merged_tags {
    Some(azimuth::AttributeValue::ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 5)
      assert_true(tags.contains("payment"))
      assert_true(tags.contains("api"))
      assert_true(tags.contains("v1"))
      assert_true(tags.contains("critical"))
      assert_true(tags.contains("production"))
    }
    _ => assert_true(false)
  }
}

// 测试10: 高效属性缓存算法
test "高效属性缓存算法测试" {
  // 创建LRU缓存
  let cache_size = 100
  let mut cache = azimuth::AttributeCache::new_lru(cache_size)
  
  // 测试缓存存取
  let key1 = "trace-001"
  let value1 = azimuth::Attributes::new()
  let value1_with_data = azimuth::Attributes::set(value1, "service.name", azimuth::AttributeValue::StringValue("payment-service"))
  
  // 存入缓存
  cache = azimuth::AttributeCache::put(cache, key1, value1_with_data)
  
  // 从缓存获取
  let cached_value1 = azimuth::AttributeCache::get(cache, key1)
  match cached_value1 {
    Some(attrs) => {
      let service_name = azimuth::Attributes::get(attrs, "service.name")
      match service_name {
        Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "payment-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试缓存未命中
  let non_existent = azimuth::AttributeCache::get(cache, "non-existent-key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试缓存容量限制
  for i in 0..150 {
    let key = "trace-" + i.to_string()
    let value = azimuth::Attributes::new()
    let value_with_data = azimuth::Attributes::set(value, "index", azimuth::AttributeValue::IntValue(i))
    cache = azimuth::AttributeCache::put(cache, key, value_with_data)
  }
  
  // 验证缓存大小不超过限制
  let cache_size_after = azimuth::AttributeCache::size(cache)
  assert_eq(cache_size_after, 100)
  
  // 验证最早的条目被淘汰（trace-0应该不在缓存中）
  let evicted_value = azimuth::AttributeCache::get(cache, "trace-0")
  match evicted_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 验证最新的条目仍在缓存中（trace-149应该在缓存中）
  let latest_value = azimuth::AttributeCache::get(cache, "trace-149")
  match latest_value {
    Some(attrs) => {
      let index = azimuth::Attributes::get(attrs, "index")
      match index {
        Some(azimuth::AttributeValue::IntValue(i)) => assert_eq(i, 149)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试缓存统计
  let stats = azimuth::AttributeCache::stats(cache)
  assert_eq(stats.hits, 1) // 我们之前成功获取了一次
  assert_eq(stats.misses, 1) // 我们有一次未命中
  assert_eq(stats.evictions, 50) // 应该淘汰了50个条目
  
  // 测试缓存清空
  let empty_cache = azimuth::AttributeCache::clear(cache)
  assert_eq(azimuth::AttributeCache::size(empty_cache), 0)
  
  let value_after_clear = azimuth::AttributeCache::get(empty_cache, "trace-149")
  match value_after_clear {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}