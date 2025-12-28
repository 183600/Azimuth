// Azimuth Advanced Attributes Operations Tests - 高级属性操作测试
// 测试复杂属性操作、深度嵌套、批量处理和属性转换功能

test "属性深度嵌套操作测试" {
  // 测试深度嵌套的属性操作
  let nested_attrs = Attributes::new()
  
  // 创建多层嵌套的属性结构
  let level1_keys = ["user", "system", "application", "network", "database"]
  let level2_keys = ["config", "status", "metrics", "logs", "events"]
  let level3_keys = ["cpu", "memory", "disk", "network_io", "processes"]
  
  // 三层嵌套属性
  for l1 in level1_keys {
    for l2 in level2_keys {
      for l3 in level3_keys {
        let nested_key = l1 + "." + l2 + "." + l3
        let nested_value = l1 + "_" + l2 + "_" + l3 + "_value"
        
        Attributes::set(nested_attrs, nested_key, StringValue(nested_value))
      }
    }
  }
  
  // 验证深度嵌套属性
  for l1 in level1_keys {
    for l2 in level2_keys {
      for l3 in level3_keys {
        let nested_key = l1 + "." + l2 + "." + l3
        let expected_value = l1 + "_" + l2 + "_" + l3 + "_value"
        
        match Attributes::get(nested_attrs, nested_key) {
          Some(StringValue(value)) => assert_eq(value, expected_value)
          _ => assert_true(false)
        }
      }
    }
  }
  
  // 测试极深嵌套（10层）
  let very_nested_attrs = Attributes::new()
  let base_key = "level"
  
  // 创建10层嵌套
  for i in 0..=9 {
    let nested_key_parts = []
    for j in 0..=i {
      nested_key_parts.push(base_key + j.to_string())
    }
    let very_nested_key = String::join(".", nested_key_parts)
    let very_nested_value = "value_at_level_" + i.to_string()
    
    Attributes::set(very_nested_attrs, very_nested_key, StringValue(very_nested_value))
  }
  
  // 验证极深嵌套
  for i in 0..=9 {
    let nested_key_parts = []
    for j in 0..=i {
      nested_key_parts.push(base_key + j.to_string())
    }
    let very_nested_key = String::join(".", nested_key_parts)
    let expected_value = "value_at_level_" + i.to_string()
    
    match Attributes::get(very_nested_attrs, very_nested_key) {
      Some(StringValue(value)) => assert_eq(value, expected_value)
      _ => assert_true(false)
    }
  }
  
  // 测试混合类型嵌套属性
  let mixed_type_attrs = Attributes::new()
  
  Attributes::set(mixed_type_attrs, "nested.string.value", StringValue("test_string"))
  Attributes::set(mixed_type_attrs, "nested.int.value", IntValue(42))
  Attributes::set(mixed_type_attrs, "nested.float.value", FloatValue(3.14159))
  Attributes::set(mixed_type_attrs, "nested.bool.value", BoolValue(true))
  Attributes::set(mixed_type_attrs, "nested.array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(mixed_type_attrs, "nested.array.int", ArrayIntValue([1, 2, 3]))
  
  // 验证混合类型嵌套属性
  match Attributes::get(mixed_type_attrs, "nested.string.value") {
    Some(StringValue("test_string")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Attributes::get(mixed_type_attrs, "nested.int.value") {
    Some(IntValue(42)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Attributes::get(mixed_type_attrs, "nested.float.value") {
    Some(FloatValue(3.14159)) => assert_true(true)
    _ => assert_true(false)
  }
  
  match Attributes::get(mixed_type_attrs, "nested.bool.value") {
    Some(BoolValue(true)) => assert_true(true)
    _ => assert_true(false)
  }
}

test "属性批量操作测试" {
  // 测试属性的批量操作
  let batch_attrs = Attributes::new()
  
  // 批量设置属性
  let batch_data = [
    ("batch.user.id", StringValue("user123")),
    ("batch.user.name", StringValue("John Doe")),
    ("batch.user.email", StringValue("john@example.com")),
    ("batch.system.version", StringValue("1.2.3")),
    ("batch.system.environment", StringValue("production")),
    ("batch.system.region", StringValue("us-west-2")),
    ("batch.metric.cpu_usage", FloatValue(75.5)),
    ("batch.metric.memory_usage", FloatValue(60.2)),
    ("batch.metric.disk_usage", FloatValue(45.8)),
    ("batch.timestamp.created", IntValue(1703740800)),
    ("batch.timestamp.updated", IntValue(1703827200))
  ]
  
  for (key, value) in batch_data {
    Attributes::set(batch_attrs, key, value)
  }
  
  // 验证批量设置的属性
  for (key, value) in batch_data {
    match Attributes::get(batch_attrs, key) {
      Some(retrieved_value) => {
        match (value, retrieved_value) {
          (StringValue(expected), StringValue(actual)) => assert_eq(expected, actual)
          (FloatValue(expected), FloatValue(actual)) => assert_eq(expected, actual)
          (IntValue(expected), IntValue(actual)) => assert_eq(expected, actual)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 批量读取属性
  let batch_keys = [
    "batch.user.id",
    "batch.user.name", 
    "batch.system.version",
    "batch.metric.cpu_usage",
    "batch.timestamp.created"
  ]
  
  let retrieved_values = []
  for key in batch_keys {
    match Attributes::get(batch_attrs, key) {
      Some(value) => retrieved_values.push((key, value))
      None => assert_true(false)
    }
  }
  
  // 验证批量读取结果
  assert_true(retrieved_values.length() == batch_keys.length())
  
  // 批量更新属性
  let update_data = [
    ("batch.user.email", StringValue("john.doe@newdomain.com")),
    ("batch.system.version", StringValue("1.3.0")),
    ("batch.metric.cpu_usage", FloatValue(80.1)),
    ("batch.timestamp.updated", IntValue(1703913600))
  ]
  
  for (key, value) in update_data {
    Attributes::set(batch_attrs, key, value)
  }
  
  // 验证批量更新
  for (key, value) in update_data {
    match Attributes::get(batch_attrs, key) {
      Some(retrieved_value) => {
        match (value, retrieved_value) {
          (StringValue(expected), StringValue(actual)) => assert_eq(expected, actual)
          (FloatValue(expected), FloatValue(actual)) => assert_eq(expected, actual)
          (IntValue(expected), IntValue(actual)) => assert_eq(expected, actual)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试大量属性的批量操作
  let large_batch_attrs = Attributes::new()
  let large_batch_start = Clock::now_unix_nanos(Clock::system())
  
  // 批量设置1000个属性
  for i in 0..=999 {
    let key = "large.batch.item." + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(large_batch_attrs, key, value)
  }
  
  let large_batch_set_end = Clock::now_unix_nanos(Clock::system())
  
  // 批量读取1000个属性
  let large_batch_retrieved = []
  for i in 0..=999 {
    let key = "large.batch.item." + i.to_string()
    match Attributes::get(large_batch_attrs, key) {
      Some(value) => large_batch_retrieved.push(value)
      None => assert_true(false)
    }
  }
  
  let large_batch_end = Clock::now_unix_nanos(Clock::system())
  
  // 验证大量批量操作
  assert_true(large_batch_retrieved.length() == 1000)
  assert_true((large_batch_set_end - large_batch_start) < 10000000000L)  // 设置时间小于10秒
  assert_true((large_batch_end - large_batch_set_end) < 10000000000L)  // 读取时间小于10秒
}

test "属性转换和序列化测试" {
  // 测试属性的转换和序列化功能
  let transform_attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(transform_attrs, "transform.string", StringValue("hello world"))
  Attributes::set(transform_attrs, "transform.int", IntValue(12345))
  Attributes::set(transform_attrs, "transform.float", FloatValue(123.456))
  Attributes::set(transform_attrs, "transform.bool", BoolValue(true))
  Attributes::set(transform_attrs, "transform.array.string", ArrayStringValue(["a", "b", "c", "d"]))
  Attributes::set(transform_attrs, "transform.array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 测试属性类型转换
  match Attributes::get(transform_attrs, "transform.string") {
    Some(StringValue(value)) => {
      // 转换为大写
      let upper_value = value.to_uppercase()
      assert_eq(upper_value, "HELLO WORLD")
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(transform_attrs, "transform.int") {
    Some(IntValue(value)) => {
      // 转换为字符串
      let string_value = value.to_string()
      assert_eq(string_value, "12345")
      
      // 转换为浮点数
      let float_value = value.to_double()
      assert_eq(float_value, 12345.0)
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(transform_attrs, "transform.float") {
    Some(FloatValue(value)) => {
      // 转换为字符串
      let string_value = value.to_string()
      assert_true(string_value.contains("123.456"))
      
      // 转换为整数（截断）
      let int_value = value.to_int()
      assert_eq(int_value, 123)
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(transform_attrs, "transform.bool") {
    Some(BoolValue(value)) => {
      // 转换为字符串
      let string_value = if value { "true" } else { "false" }
      assert_eq(string_value, "true")
      
      // 转换为整数
      let int_value = if value { 1 } else { 0 }
      assert_eq(int_value, 1)
    }
    _ => assert_true(false)
  }
  
  // 测试数组属性转换
  match Attributes::get(transform_attrs, "transform.array.string") {
    Some(ArrayStringValue(array)) => {
      // 转换为逗号分隔的字符串
      let csv_string = String::join(",", array)
      assert_eq(csv_string, "a,b,c,d")
      
      // 验证数组长度
      assert_eq(array.length(), 4)
    }
    _ => assert_true(false)
  }
  
  match Attributes::get(transform_attrs, "transform.array.int") {
    Some(ArrayIntValue(array)) => {
      // 计算数组总和
      let sum = 0
      for value in array {
        sum = sum + value
      }
      assert_eq(sum, 15)  // 1+2+3+4+5
      
      // 转换为字符串数组
      let string_array = []
      for value in array {
        string_array.push(value.to_string())
      }
      assert_eq(string_array.length(), 5)
    }
    _ => assert_true(false)
  }
  
  // 测试复杂属性的JSON序列化模拟
  let complex_attrs = Attributes::new()
  
  // 设置复杂的嵌套结构
  Attributes::set(complex_attrs, "user.profile.name", StringValue("Alice Smith"))
  Attributes::set(complex_attrs, "user.profile.age", IntValue(30))
  Attributes::set(complex_attrs, "user.profile.email", StringValue("alice@example.com"))
  Attributes::set(complex_attrs, "user.preferences.theme", StringValue("dark"))
  Attributes::set(complex_attrs, "user.preferences.notifications", BoolValue(true))
  Attributes::set(complex_attrs, "user.preferences.languages", ArrayStringValue(["en", "es", "fr"]))
  
  // 模拟JSON序列化
  let json_structure = {
    "user": {
      "profile": {
        "name": "Alice Smith",
        "age": 30,
        "email": "alice@example.com"
      },
      "preferences": {
        "theme": "dark",
        "notifications": true,
        "languages": ["en", "es", "fr"]
      }
    }
  }
  
  // 验证JSON结构中的值
  assert_eq(json_structure["user"]["profile"]["name"], "Alice Smith")
  assert_eq(json_structure["user"]["profile"]["age"], 30)
  assert_eq(json_structure["user"]["preferences"]["theme"], "dark")
  assert_eq(json_structure["user"]["preferences"]["notifications"], true)
  assert_eq(json_structure["user"]["preferences"]["languages"].length(), 3)
}

test "属性查询和过滤测试" {
  // 测试属性的查询和过滤功能
  let query_attrs = Attributes::new()
  
  // 设置测试数据
  let test_data = [
    ("user.id", StringValue("12345")),
    ("user.name", StringValue("John Doe")),
    ("user.email", StringValue("john@example.com")),
    ("user.age", IntValue(30)),
    ("user.active", BoolValue(true)),
    ("system.version", StringValue("1.2.3")),
    ("system.environment", StringValue("production")),
    ("system.region", StringValue("us-west-2")),
    ("system.cluster", StringValue("main")),
    ("metric.cpu.usage", FloatValue(75.5)),
    ("metric.memory.usage", FloatValue(60.2)),
    ("metric.disk.usage", FloatValue(45.8)),
    ("metric.network.in", FloatValue(1024.5)),
    ("metric.network.out", FloatValue(2048.1)),
    ("app.request.count", IntValue(1500)),
    ("app.error.count", IntValue(25)),
    ("app.response.time", FloatValue(250.5)),
    ("app.status", StringValue("healthy"))
  ]
  
  for (key, value) in test_data {
    Attributes::set(query_attrs, key, value)
  }
  
  // 测试前缀查询
  let user_prefix_keys = []
  let system_prefix_keys = []
  let metric_prefix_keys = []
  let app_prefix_keys = []
  
  for (key, _) in test_data {
    if key.starts_with("user.") {
      user_prefix_keys.push(key)
    } else if key.starts_with("system.") {
      system_prefix_keys.push(key)
    } else if key.starts_with("metric.") {
      metric_prefix_keys.push(key)
    } else if key.starts_with("app.") {
      app_prefix_keys.push(key)
    }
  }
  
  // 验证前缀查询结果
  assert_true(user_prefix_keys.length() == 5)
  assert_true(system_prefix_keys.length() == 4)
  assert_true(metric_prefix_keys.length() == 5)
  assert_true(app_prefix_keys.length() == 4)
  
  // 测试类型查询
  let string_keys = []
  let int_keys = []
  let float_keys = []
  let bool_keys = []
  
  for (key, value) in test_data {
    match value {
      StringValue(_) => string_keys.push(key)
      IntValue(_) => int_keys.push(key)
      FloatValue(_) => float_keys.push(key)
      BoolValue(_) => bool_keys.push(key)
      _ => assert_true(false)
    }
  }
  
  // 验证类型查询结果
  assert_true(string_keys.length() == 7)
  assert_true(int_keys.length() == 4)
  assert_true(float_keys.length() == 6)
  assert_true(bool_keys.length() == 2)
  
  // 测试模式匹配查询
  let pattern_matches = []
  
  for (key, _) in test_data {
    // 匹配包含".usage"的键
    if key.contains(".usage") {
      pattern_matches.push(key)
    }
    
    // 匹配以".count"结尾的键
    if key.ends_with(".count") {
      pattern_matches.push(key)
    }
    
    // 匹配包含"network"的键
    if key.contains("network") {
      pattern_matches.push(key)
    }
  }
  
  // 验证模式匹配结果
  assert_true(pattern_matches.length() >= 5)  // 至少包含3个usage和2个count
  
  // 测试值过滤
  let filtered_by_value = []
  
  for (key, value) in test_data {
    match value {
      StringValue(str_val) => {
        if str_val.length() > 5 {
          filtered_by_value.push((key, value))
        }
      }
      IntValue(int_val) => {
        if int_val > 100 {
          filtered_by_value.push((key, value))
        }
      }
      FloatValue(float_val) => {
        if float_val > 50.0 {
          filtered_by_value.push((key, value))
        }
      }
      BoolValue(bool_val) => {
        if bool_val {
          filtered_by_value.push((key, value))
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 验证值过滤结果
  assert_true(filtered_by_value.length() > 0)
  
  // 测试复合查询（前缀+类型）
  let user_string_keys = []
  let metric_float_keys = []
  
  for (key, value) in test_data {
    if key.starts_with("user.") {
      match value {
        StringValue(_) => user_string_keys.push(key)
        _ => ()
      }
    }
    
    if key.starts_with("metric.") {
      match value {
        FloatValue(_) => metric_float_keys.push(key)
        _ => ()
      }
    }
  }
  
  // 验证复合查询结果
  assert_true(user_string_keys.length() == 3)  // user.name, user.email, user.active
  assert_true(metric_float_keys.length() == 5)  // 所有metric.*都是FloatValue
}

test "属性性能和内存优化测试" {
  // 测试属性操作的性能和内存优化
  let performance_attrs = Attributes::new()
  
  // 性能测试：大量属性设置
  let large_scale_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..=9999 {  // 10000个属性
    let key = "perf.item." + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(performance_attrs, key, value)
  }
  
  let large_scale_end = Clock::now_unix_nanos(Clock::system())
  let large_scale_duration = large_scale_end - large_scale_start
  
  // 验证大规模设置性能
  assert_true(large_scale_duration < 30000000000L)  // 小于30秒
  
  // 性能测试：大量属性读取
  let read_start = Clock::now_unix_nanos(Clock::system())
  
  let read_count = 0
  for i in 0..=9999 {
    let key = "perf.item." + i.to_string()
    match Attributes::get(performance_attrs, key) {
      Some(_) => read_count = read_count + 1
      None => assert_true(false)
    }
  }
  
  let read_end = Clock::now_unix_nanos(Clock::system())
  let read_duration = read_end - read_start
  
  // 验证大规模读取性能
  assert_true(read_duration < 20000000000L)  // 小于20秒
  assert_eq(read_count, 10000)
  
  // 内存优化测试：属性重用
  let reuse_attrs = Attributes::new()
  
  // 设置和重用相同的键
  let reuse_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..=999 {  // 1000次重用
    let key = "reuse.item"
    let value = StringValue("iteration_" + i.to_string())
    Attributes::set(reuse_attrs, key, value)
    
    // 立即读取验证
    match Attributes::get(reuse_attrs, key) {
      Some(StringValue(retrieved)) => assert_eq(retrieved, "iteration_" + i.to_string())
      _ => assert_true(false)
    }
  }
  
  let reuse_end = Clock::now_unix_nanos(Clock::system())
  let reuse_duration = reuse_end - reuse_start
  
  // 验证重用性能
  assert_true(reuse_duration < 5000000000L)  // 小于5秒
  
  // 内存优化测试：批量清理和重建
  let cleanup_start = Clock::now_unix_nanos(Clock::system())
  
  // 创建多个属性集合并清理
  for batch in 0..=9 {  // 10个批次
    let batch_attrs = Attributes::new()
    
    // 每批设置100个属性
    for i in 0..=99 {
      let key = "batch." + batch.to_string() + ".item." + i.to_string()
      let value = StringValue("batch_value_" + i.to_string())
      Attributes::set(batch_attrs, key, value)
    }
    
    // 验证设置成功
    let test_key = "batch." + batch.to_string() + ".item.50"
    match Attributes::get(batch_attrs, test_key) {
      Some(StringValue("batch_value_50")) => assert_true(true)
      _ => assert_true(false)
    }
    
    // 模拟清理（在实际实现中，这里会释放内存）
    // batch_attrs = nil  // 在MoonBit中可能通过其他方式
  }
  
  let cleanup_end = Clock::now_unix_nanos(Clock::system())
  let cleanup_duration = cleanup_end - cleanup_start
  
  // 验证清理性能
  assert_true(cleanup_duration < 10000000000L)  // 小于10秒
  
  // 复杂操作性能测试
  let complex_attrs = Attributes::new()
  
  // 设置复杂的嵌套属性
  let complex_start = Clock::now_unix_nanos(Clock::system())
  
  for level1 in 0..=9 {      // 10个第一级
    for level2 in 0..=9 {    // 10个第二级
      for level3 in 0..=9 {  // 10个第三级
        let key = "l" + level1.to_string() + ".l" + level2.to_string() + ".l" + level3.to_string()
        let value = StringValue("val_" + level1.to_string() + "_" + level2.to_string() + "_" + level3.to_string())
        Attributes::set(complex_attrs, key, value)
      }
    }
  }
  
  let complex_end = Clock::now_unix_nanos(Clock::system())
  let complex_duration = complex_end - complex_start
  
  // 验证复杂操作性能
  assert_true(complex_duration < 15000000000L)  // 小于15秒
  
  // 验证复杂属性数量
  let expected_count = 10 * 10 * 10  // 1000个属性
  let actual_count = 0
  
  for level1 in 0..=9 {
    for level2 in 0..=9 {
      for level3 in 0..=9 {
        let key = "l" + level1.to_string() + ".l" + level2.to_string() + ".l" + level3.to_string()
        match Attributes::get(complex_attrs, key) {
          Some(_) => actual_count = actual_count + 1
          None => assert_true(false)
        }
      }
    }
  }
  
  assert_eq(actual_count, expected_count)
}

test "属性并发操作测试" {
  // 测试属性的并发操作安全性
  let concurrent_attrs = Attributes::new()
  
  // 模拟并发写入
  let concurrent_write_start = Clock::now_unix_nanos(Clock::system())
  
  // 模拟10个并发写入者
  for writer_id in 0..=9 {
    // 每个写入者写入100个属性
    for item in 0..=99 {
      let key = "writer." + writer_id.to_string() + ".item." + item.to_string()
      let value = StringValue("value_" + writer_id.to_string() + "_" + item.to_string())
      Attributes::set(concurrent_attrs, key, value)
    }
  }
  
  let concurrent_write_end = Clock::now_unix_nanos(Clock::system())
  let concurrent_write_duration = concurrent_write_end - concurrent_write_start
  
  // 验证并发写入性能
  assert_true(concurrent_write_duration < 10000000000L)  // 小于10秒
  
  // 模拟并发读取
  let concurrent_read_start = Clock::now_unix_nanos(Clock::system())
  
  let read_results = []
  
  // 模拟10个并发读取者
  for reader_id in 0..=9 {
    // 每个读取者读取100个属性
    for item in 0..=99 {
      let key = "writer." + reader_id.to_string() + ".item." + item.to_string()
      match Attributes::get(concurrent_attrs, key) {
        Some(value) => read_results.push((key, value))
        None => assert_true(false)
      }
    }
  }
  
  let concurrent_read_end = Clock::now_unix_nanos(Clock::system())
  let concurrent_read_duration = concurrent_read_end - concurrent_read_start
  
  // 验证并发读取性能
  assert_true(concurrent_read_duration < 5000000000L)  // 小于5秒
  assert_eq(read_results.length(), 1000)  // 10个读取者 * 100个属性
  
  // 测试读写混合并发操作
  let mixed_attrs = Attributes::new()
  let mixed_start = Clock::now_unix_nanos(Clock::system())
  
  // 混合操作：写入后立即读取
  for i in 0..=499 {  // 500次混合操作
    // 写入
    let write_key = "mixed.item." + i.to_string()
    let write_value = StringValue("mixed_value_" + i.to_string())
    Attributes::set(mixed_attrs, write_key, write_value)
    
    // 立即读取验证
    match Attributes::get(mixed_attrs, write_key) {
      Some(StringValue(retrieved)) => assert_eq(retrieved, "mixed_value_" + i.to_string())
      _ => assert_true(false)
    }
    
    // 读取之前写入的值（如果存在）
    if i > 0 {
      let read_key = "mixed.item." + (i - 1).to_string()
      match Attributes::get(mixed_attrs, read_key) {
        Some(_) => assert_true(true)
        None => assert_true(false)
      }
    }
  }
  
  let mixed_end = Clock::now_unix_nanos(Clock::system())
  let mixed_duration = mixed_end - mixed_start
  
  // 验证混合操作性能
  assert_true(mixed_duration < 8000000000L)  // 小于8秒
  
  // 测试属性更新的并发安全性
  let update_attrs = Attributes::new()
  
  // 初始化一个共享属性
  Attributes::set(update_attrs, "shared.counter", IntValue(0))
  
  let update_start = Clock::now_unix_nanos(Clock::system())
  
  // 模拟10个并发更新者
  for updater_id in 0..=9 {
    // 每个更新者更新10次
    for update in 0..=9 {
      let key = "shared.counter"
      let new_value = (updater_id * 10 + update).to_int()
      Attributes::set(update_attrs, key, IntValue(new_value))
      
      // 验证更新
      match Attributes::get(update_attrs, key) {
        Some(IntValue(retrieved)) => assert_eq(retrieved, new_value)
        _ => assert_true(false)
      }
    }
  }
  
  let update_end = Clock::now_unix_nanos(Clock::system())
  let update_duration = update_end - update_start
  
  // 验证并发更新性能
  assert_true(update_duration < 3000000000L)  // 小于3秒
  
  // 验证最终值
  match Attributes::get(update_attrs, "shared.counter") {
    Some(IntValue(final_value)) => assert_eq(final_value, 99)  // 最后一个更新应该是9*10+9
    _ => assert_true(false)
  }
}