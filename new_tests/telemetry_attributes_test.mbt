// 遥测属性操作测试用例

test "attribute_value_type_handling" {
  // 测试属性值类型的处理
  
  // 创建不同类型的属性值
  let int_attr = azimuth::telemetry::api::common::AttributeValue::Int64(42)
  let float_attr = azimuth::telemetry::api::common::AttributeValue::Float64(3.14159)
  let bool_attr = azimuth::telemetry::api::common::AttributeValue::Bool(true)
  let string_attr = azimuth::telemetry::api::common::AttributeValue::String("test_value")
  
  // 测试类型转换和比较
  assert_eq(int_attr.to_string(), "42")
  assert_eq(float_attr.to_string(), "3.14159")
  assert_eq(bool_attr.to_string(), "true")
  assert_eq(string_attr.to_string(), "test_value")
  
  // 测试数组类型
  let int_array_attr = azimuth::telemetry::api::common::AttributeValue::ArrayInt64([1, 2, 3, 4, 5])
  let string_array_attr = azimuth::telemetry::api::common::AttributeValue::ArrayString(["a", "b", "c"])
  
  assert_eq(int_array_attr.to_string(), "[1, 2, 3, 4, 5]")
  assert_eq(string_array_attr.to_string(), "[a, b, c]")
}

test "attributes_basic_operations" {
  // 测试属性的基本操作
  
  // 创建空属性集合
  let empty_attrs = azimuth::telemetry::api::common::Attributes::empty()
  assert_eq(empty_attrs.size(), 0)
  assert_eq(empty_attrs.is_empty(), true)
  
  // 添加属性
  let attrs1 = empty_attrs.with("user.id", azimuth::telemetry::api::common::AttributeValue::String("12345"))
  assert_eq(attrs1.size(), 1)
  assert_eq(attrs1.is_empty(), false)
  assert_eq(attrs1.get("user.id").unwrap().to_string(), "12345")
  
  let attrs2 = attrs1.with("request.count", azimuth::telemetry::api::common::AttributeValue::Int64(10))
  assert_eq(attrs2.size(), 2)
  assert_eq(attrs2.get("user.id").unwrap().to_string(), "12345")
  assert_eq(attrs2.get("request.count").unwrap().to_string(), "10")
  
  // 测试覆盖现有属性
  let attrs3 = attrs2.with("user.id", azimuth::telemetry::api::common::AttributeValue::String("67890"))
  assert_eq(attrs3.size(), 2)  // 覆盖不增加大小
  assert_eq(attrs3.get("user.id").unwrap().to_string(), "67890")
  assert_eq(attrs3.get("request.count").unwrap().to_string(), "10")
}

test "attributes_merging_and_combination" {
  // 测试属性的合并和组合
  
  // 创建第一个属性集合
  let attrs1 = azimuth::telemetry::api::common::Attributes::empty()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("auth-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.0.0"))
    .with("service.instance", azimuth::telemetry::api::common::AttributeValue::String("instance-1"))
  
  // 创建第二个属性集合
  let attrs2 = azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user123"))
    .with("user.role", azimuth::telemetry::api::common::AttributeValue::String("admin"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("2.0.0"))  // 覆盖版本
  
  // 合并属性
  let merged_attrs = attrs1.merge(attrs2)
  assert_eq(merged_attrs.size(), 4)  // service.version被覆盖
  
  // 验证合并结果
  assert_eq(merged_attrs.get("service.name").unwrap().to_string(), "auth-service")
  assert_eq(merged_attrs.get("service.instance").unwrap().to_string(), "instance-1")
  assert_eq(merged_attrs.get("user.id").unwrap().to_string(), "user123")
  assert_eq(merged_attrs.get("user.role").unwrap().to_string(), "admin")
  assert_eq(merged_attrs.get("service.version").unwrap().to_string(), "2.0.0")  // 被覆盖的值
}

test "attributes_filtering_and_selection" {
  // 测试属性的过滤和选择
  
  // 创建包含多种属性的集合
  let attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("12345"))
    .with("user.name", azimuth::telemetry::api::common::AttributeValue::String("张三"))
    .with("user.age", azimuth::telemetry::api::common::AttributeValue::Int64(25))
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("api-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.2.3"))
    .with("request.id", azimuth::telemetry::api::common::AttributeValue::String("req-abc-123"))
    .with("request.duration", azimuth::telemetry::api::common::AttributeValue::Float64(150.5))
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::Int64(500))
  
  assert_eq(attrs.size(), 8)
  
  // 按前缀过滤
  let user_attrs = attrs.filter_by_prefix("user.")
  assert_eq(user_attrs.size(), 3)
  assert_eq(user_attrs.get("user.id").unwrap().to_string(), "12345")
  assert_eq(user_attrs.get("user.name").unwrap().to_string(), "张三")
  assert_eq(user_attrs.get("user.age").unwrap().to_string(), "25")
  
  let service_attrs = attrs.filter_by_prefix("service.")
  assert_eq(service_attrs.size(), 2)
  assert_eq(service_attrs.get("service.name").unwrap().to_string(), "api-service")
  assert_eq(service_attrs.get("service.version").unwrap().to_string(), "1.2.3")
  
  let request_attrs = attrs.filter_by_prefix("request.")
  assert_eq(request_attrs.size(), 2)
  assert_eq(request_attrs.get("request.id").unwrap().to_string(), "req-abc-123")
  assert_eq(request_attrs.get("request.duration").unwrap().to_string(), "150.5")
  
  // 按键选择
  let selected_attrs = attrs.select(["user.id", "service.name", "error.code"])
  assert_eq(selected_attrs.size(), 3)
  assert_eq(selected_attrs.get("user.id").unwrap().to_string(), "12345")
  assert_eq(selected_attrs.get("service.name").unwrap().to_string(), "api-service")
  assert_eq(selected_attrs.get("error.code").unwrap().to_string(), "500")
}

test "attributes_serialization_and_deserialization" {
  // 测试属性的序列化和反序列化
  
  // 创建复杂属性集合
  let original_attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("string.value", azimuth::telemetry::api::common::AttributeValue::String("测试字符串"))
    .with("int.value", azimuth::telemetry::api::common::AttributeValue::Int64(42))
    .with("float.value", azimuth::telemetry::api::common::AttributeValue::Float64(3.14159))
    .with("bool.value", azimuth::telemetry::api::common::AttributeValue::Bool(true))
    .with("int.array", azimuth::telemetry::api::common::AttributeValue::ArrayInt64([1, 2, 3, 4, 5]))
    .with("string.array", azimuth::telemetry::api::common::AttributeValue::ArrayString(["a", "b", "c", "测试"]))
  
  // 序列化为字符串
  let serialized = original_attrs.to_json()
  assert_eq(serialized.length() > 0, true)
  
  // 反序列化
  let deserialized_attrs = azimuth::telemetry::api::common::Attributes::from_json(serialized)
  assert_eq(deserialized_attrs.size(), original_attrs.size())
  
  // 验证反序列化的值
  assert_eq(deserialized_attrs.get("string.value").unwrap().to_string(), "测试字符串")
  assert_eq(deserialized_attrs.get("int.value").unwrap().to_string(), "42")
  assert_eq(deserialized_attrs.get("float.value").unwrap().to_string(), "3.14159")
  assert_eq(deserialized_attrs.get("bool.value").unwrap().to_string(), "true")
  assert_eq(deserialized_attrs.get("int.array").unwrap().to_string(), "[1, 2, 3, 4, 5]")
  assert_eq(deserialized_attrs.get("string.array").unwrap().to_string(), "[a, b, c, 测试]")
}

test "attributes_edge_cases_and_validation" {
  // 测试属性的边界情况和验证
  
  // 测试空键和空值
  let attrs1 = azimuth::telemetry::api::common::Attributes::empty()
    .with("", azimuth::telemetry::api::common::AttributeValue::String("empty_key_value"))
    .with("empty_value", azimuth::telemetry::api::common::AttributeValue::String(""))
    .with("normal", azimuth::telemetry::api::common::AttributeValue::String("normal_value"))
  
  assert_eq(attrs1.size(), 3)
  assert_eq(attrs1.get("").unwrap().to_string(), "empty_key_value")
  assert_eq(attrs1.get("empty_value").unwrap().to_string(), "")
  assert_eq(attrs1.get("normal").unwrap().to_string(), "normal_value")
  
  // 测试特殊字符键
  let special_key = "key.with.special@chars#and$symbols"
  let attrs2 = attrs1.with(special_key, azimuth::telemetry::api::common::AttributeValue::String("special_value"))
  assert_eq(attrs2.size(), 4)
  assert_eq(attrs2.get(special_key).unwrap().to_string(), "special_value")
  
  // 测试Unicode键和值
  let unicode_key = "键名.包含.中文"
  let unicode_value = "值.包含.中文.和.🚀.emoji"
  let attrs3 = attrs2.with(unicode_key, azimuth::telemetry::api::common::AttributeValue::String(unicode_value))
  assert_eq(attrs3.size(), 5)
  assert_eq(attrs3.get(unicode_key).unwrap().to_string(), unicode_value)
  
  // 测试极长的键和值
  let long_key = "this.is.a.very.long.key.name.that.exceeds.normal.expectations.and.tests.system.behavior.with.extremely.long.identifiers"
  let long_value = "this.is.a.very.long.value.that.contains.a.lot.of.information.and.might.be.used.to.test.the.system.ability.to.handle.extremely.long.strings.without.issues.or.performance.problems"
  let attrs4 = attrs3.with(long_key, azimuth::telemetry::api::common::AttributeValue::String(long_value))
  assert_eq(attrs4.size(), 6)
  assert_eq(attrs4.get(long_key).unwrap().to_string(), long_value)
  
  // 测试数值边界
  let max_int = azimuth::telemetry::api::common::AttributeValue::Int64(9223372036854775807)  // Int64最大值
  let min_int = azimuth::telemetry::api::common::AttributeValue::Int64(-9223372036854775808)  // Int64最小值
  let max_float = azimuth::telemetry::api::common::AttributeValue::Float64(1.7976931348623157e+308)  // Float64最大值
  let min_float = azimuth::telemetry::api::common::AttributeValue::Float64(-1.7976931348623157e+308)  // Float64最小值
  
  let attrs5 = attrs4
    .with("max.int", max_int)
    .with("min.int", min_int)
    .with("max.float", max_float)
    .with("min.float", min_float)
  
  assert_eq(attrs5.size(), 10)
  assert_eq(attrs5.get("max.int").unwrap().to_string(), "9223372036854775807")
  assert_eq(attrs5.get("min.int").unwrap().to_string(), "-9223372036854775808")
  assert_eq(attrs5.get("max.float").unwrap().to_string(), "1.7976931348623157e+308")
  assert_eq(attrs5.get("min.float").unwrap().to_string(), "-1.7976931348623157e+308")
}