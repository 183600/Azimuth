// Azimuth 基础类型测试用例
// 测试AttributeValue、Attributes、Resource等核心类型

// 测试1: AttributeValue类型创建和操作
test "AttributeValue类型创建和操作" {
  // 测试字符串值
  let string_val = AttributeValue::StringValue("test")
  match string_val {
    AttributeValue::StringValue(s) => assert_eq(s, "test")
    _ => assert_false(true) // 不应该到达这里
  }
  
  // 测试整数值
  let int_val = AttributeValue::IntValue(42)
  match int_val {
    AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_false(true) // 不应该到达这里
  }
  
  // 测试浮点数值
  let float_val = AttributeValue::FloatValue(3.14)
  match float_val {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_false(true) // 不应该到达这里
  }
  
  // 测试布尔值
  let bool_val = AttributeValue::BoolValue(true)
  match bool_val {
    AttributeValue::BoolValue(b) => assert_eq(b, true)
    _ => assert_false(true) // 不应该到达这里
  }
  
  // 测试字符串数组
  let string_array_val = AttributeValue::ArrayStringValue(["a", "b", "c"])
  match string_array_val {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_false(true) // 不应该到达这里
  }
  
  // 测试整数数组
  let int_array_val = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  match int_array_val {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_false(true) // 不应该到达这里
  }
}

// 测试2: Attributes类型操作
test "Attributes类型操作" {
  // 创建空属性集合
  let empty_attrs = { values : [] }
  assert_eq(empty_attrs.values.length(), 0)
  
  // 创建带有属性的集合
  let attrs = { 
    values : [
      ("key1", AttributeValue::StringValue("value1")),
      ("key2", AttributeValue::IntValue(100)),
      ("key3", AttributeValue::BoolValue(true))
    ]
  }
  
  assert_eq(attrs.values.length(), 3)
  
  // 验证第一个属性
  assert_eq(attrs.values[0].0, "key1")
  match attrs.values[0].1 {
    AttributeValue::StringValue(v) => assert_eq(v, "value1")
    _ => assert_false(true)
  }
  
  // 验证第二个属性
  assert_eq(attrs.values[1].0, "key2")
  match attrs.values[1].1 {
    AttributeValue::IntValue(v) => assert_eq(v, 100)
    _ => assert_false(true)
  }
  
  // 验证第三个属性
  assert_eq(attrs.values[2].0, "key3")
  match attrs.values[2].1 {
    AttributeValue::BoolValue(v) => assert_eq(v, true)
    _ => assert_false(true)
  }
}

// 测试3: Resource类型操作
test "Resource类型操作" {
  // 创建空资源
  let empty_resource = { attributes : [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 创建带有属性的资源
  let resource = {
    attributes : [
      ("service.name", AttributeValue::StringValue("azimuth-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123")),
      ("host.name", AttributeValue::StringValue("localhost")),
      ("process.pid", AttributeValue::IntValue(12345))
    ]
  }
  
  assert_eq(resource.attributes.length(), 5)
  
  // 验证服务名称
  assert_eq(resource.attributes[0].0, "service.name")
  match resource.attributes[0].1 {
    AttributeValue::StringValue(v) => assert_eq(v, "azimuth-service")
    _ => assert_false(true)
  }
  
  // 验证进程ID
  assert_eq(resource.attributes[4].0, "process.pid")
  match resource.attributes[4].1 {
    AttributeValue::IntValue(v) => assert_eq(v, 12345)
    _ => assert_false(true)
  }
}

// 测试4: InstrumentationScope类型操作
test "InstrumentationScope类型操作" {
  // 创建完整的instrumentation scope
  let full_scope = {
    name : "azimuth.telemetry",
    version : Some("1.0.0"),
    schema_url : Some("https://example.com/schema")
  }
  
  assert_eq(full_scope.name, "azimuth.telemetry")
  match full_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_false(true)
  }
  match full_scope.schema_url {
    Some(u) => assert_eq(u, "https://example.com/schema")
    None => assert_false(true)
  }
  
  // 创建最小instrumentation scope
  let min_scope = {
    name : "azimuth.minimal",
    version : None,
    schema_url : None
  }
  
  assert_eq(min_scope.name, "azimuth.minimal")
  assert_eq(min_scope.version, None)
  assert_eq(min_scope.schema_url, None)
  
  // 创建带版本但无schema的scope
  let partial_scope = {
    name : "azimuth.partial",
    version : Some("0.9.0"),
    schema_url : None
  }
  
  assert_eq(partial_scope.name, "azimuth.partial")
  match partial_scope.version {
    Some(v) => assert_eq(v, "0.9.0")
    None => assert_false(true)
  }
  assert_eq(partial_scope.schema_url, None)
}

// 测试5: 复杂属性值组合
test "复杂属性值组合" {
  // 创建包含各种类型属性的复杂资源
  let complex_resource = {
    attributes : [
      ("string.attr", AttributeValue::StringValue("complex string")),
      ("int.attr", AttributeValue::IntValue(-999)),
      ("float.attr", AttributeValue::FloatValue(-3.14159)),
      ("bool.attr", AttributeValue::BoolValue(false)),
      ("string.array", AttributeValue::ArrayStringValue(["x", "y", "z"])),
      ("int.array", AttributeValue::ArrayIntValue([-1, 0, 1, 2, 3]))
    ]
  }
  
  assert_eq(complex_resource.attributes.length(), 6)
  
  // 验证负整数值
  match complex_resource.attributes[1].1 {
    AttributeValue::IntValue(v) => assert_eq(v, -999)
    _ => assert_false(true)
  }
  
  // 验证负浮点数值
  match complex_resource.attributes[2].1 {
    AttributeValue::FloatValue(v) => assert_eq(v, -3.14159)
    _ => assert_false(true)
  }
  
  // 验证假布尔值
  match complex_resource.attributes[3].1 {
    AttributeValue::BoolValue(v) => assert_eq(v, false)
    _ => assert_false(true)
  }
  
  // 验证包含负数的整数数组
  match complex_resource.attributes[5].1 {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], -1)
      assert_eq(arr[1], 0)
      assert_eq(arr[4], 3)
    }
    _ => assert_false(true)
  }
}

// 测试6: 边界值测试
test "属性值边界值测试" {
  // 测试空字符串
  let empty_string = AttributeValue::StringValue("")
  match empty_string {
    AttributeValue::StringValue(s) => assert_eq(s, "")
    _ => assert_false(true)
  }
  
  // 测试零值
  let zero_int = AttributeValue::IntValue(0)
  match zero_int {
    AttributeValue::IntValue(i) => assert_eq(i, 0)
    _ => assert_false(true)
  }
  
  let zero_float = AttributeValue::FloatValue(0.0)
  match zero_float {
    AttributeValue::FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_false(true)
  }
  
  // 测试空数组
  let empty_string_array = AttributeValue::ArrayStringValue([])
  match empty_string_array {
    AttributeValue::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_false(true)
  }
  
  let empty_int_array = AttributeValue::ArrayIntValue([])
  match empty_int_array {
    AttributeValue::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_false(true)
  }
  
  // 测试单元素数组
  let single_string_array = AttributeValue::ArrayStringValue(["single"])
  match single_string_array {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => assert_false(true)
  }
}