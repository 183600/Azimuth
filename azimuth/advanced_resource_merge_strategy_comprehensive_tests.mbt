// 资源合并策略高级测试
// 测试资源的复杂合并操作、优先级处理和冲突解决

test "资源基本合并策略测试" {
  // 创建基础资源
  let base_resource = Resource::new()
  let base_attributes = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("development"))
  ]
  let enriched_base = Resource::with_attributes(base_resource, base_attributes)
  
  // 创建覆盖资源
  let override_resource = Resource::new()
  let override_attributes = [
    ("service.name", StringValue("override-service")), // 应该覆盖
    ("service.instance.id", StringValue("instance-123")), // 新增
    ("deployment.region", StringValue("us-west-2")) // 新增
  ]
  let enriched_override = Resource::with_attributes(override_resource, override_attributes)
  
  // 执行合并
  let merged_resource = Resource::merge(enriched_base, enriched_override)
  
  // 验证合并结果
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  let environment = Resource::get_attribute(merged_resource, "environment")
  let instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  let deployment_region = Resource::get_attribute(merged_resource, "deployment.region")
  
  // 验证覆盖行为
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "override-service", "service.name应该被覆盖")
    _ => assert_true(false, "期望service.name为字符串值")
  }
  
  // 验证保留行为
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0", "service.version应该被保留")
    _ => assert_true(false, "期望service.version为字符串值")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "development", "environment应该被保留")
    _ => assert_true(false, "期望environment为字符串值")
  }
  
  // 验证新增属性
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-123", "service.instance.id应该被新增")
    _ => assert_true(false, "期望service.instance.id为字符串值")
  }
  
  match deployment_region {
    Some(StringValue(region)) => assert_eq(region, "us-west-2", "deployment.region应该被新增")
    _ => assert_true(false, "期望deployment.region为字符串值")
  }
}

test "资源多级合并链式测试" {
  // 创建第一级资源
  let resource1 = Resource::new()
  let attributes1 = [
    ("level1.attr1", StringValue("level1-value1")),
    ("shared.attr", StringValue("level1-shared")),
    ("common.attr", StringValue("original-value"))
  ]
  let enriched1 = Resource::with_attributes(resource1, attributes1)
  
  // 创建第二级资源
  let resource2 = Resource::new()
  let attributes2 = [
    ("level2.attr1", StringValue("level2-value1")),
    ("shared.attr", StringValue("level2-shared")), // 覆盖shared.attr
    ("new.attr2", StringValue("level2-new"))
  ]
  let enriched2 = Resource::with_attributes(resource2, attributes2)
  
  // 创建第三级资源
  let resource3 = Resource::new()
  let attributes3 = [
    ("level3.attr1", StringValue("level3-value1")),
    ("common.attr", StringValue("final-value")), // 覆盖common.attr
    ("new.attr3", StringValue("level3-new"))
  ]
  let enriched3 = Resource::with_attributes(resource3, attributes3)
  
  // 执行链式合并
  let merged12 = Resource::merge(enriched1, enriched2)
  let final_merged = Resource::merge(merged12, enriched3)
  
  // 验证链式合并结果
  let level1_attr1 = Resource::get_attribute(final_merged, "level1.attr1")
  let level2_attr1 = Resource::get_attribute(final_merged, "level2.attr1")
  let level3_attr1 = Resource::get_attribute(final_merged, "level3.attr1")
  let shared_attr = Resource::get_attribute(final_merged, "shared.attr")
  let common_attr = Resource::get_attribute(final_merged, "common.attr")
  let new_attr2 = Resource::get_attribute(final_merged, "new.attr2")
  let new_attr3 = Resource::get_attribute(final_merged, "new.attr3")
  
  // 验证各级属性保留
  match level1_attr1 {
    Some(StringValue(value)) => assert_eq(value, "level1-value1")
    _ => assert_true(false, "期望level1.attr1")
  }
  
  match level2_attr1 {
    Some(StringValue(value)) => assert_eq(value, "level2-value1")
    _ => assert_true(false, "期望level2.attr1")
  }
  
  match level3_attr1 {
    Some(StringValue(value)) => assert_eq(value, "level3-value1")
    _ => assert_true(false, "期望level3.attr1")
  }
  
  // 验证覆盖链
  match shared_attr {
    Some(StringValue(value)) => assert_eq(value, "level2-shared", "shared.attr应该是level2的值")
    _ => assert_true(false, "期望shared.attr")
  }
  
  match common_attr {
    Some(StringValue(value)) => assert_eq(value, "final-value", "common.attr应该是最终值")
    _ => assert_true(false, "期望common.attr")
  }
  
  // 验证新增属性
  match new_attr2 {
    Some(StringValue(value)) => assert_eq(value, "level2-new")
    _ => assert_true(false, "期望new.attr2")
  }
  
  match new_attr3 {
    Some(StringValue(value)) => assert_eq(value, "level3-new")
    _ => assert_true(false, "期望new.attr3")
  }
}

test "资源类型混合合并测试" {
  // 创建包含不同类型属性的基础资源
  let base_resource = Resource::new()
  let mixed_base_attributes = [
    ("string.attr", StringValue("base-string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(["item1", "item2"])),
    ("int.array.attr", ArrayIntValue([1, 2, 3]))
  ]
  let mixed_base = Resource::with_attributes(base_resource, mixed_base_attributes)
  
  // 创建覆盖资源（部分类型不同）
  let override_resource = Resource::new()
  let mixed_override_attributes = [
    ("string.attr", StringValue("override-string")), // 字符串覆盖
    ("int.attr", IntValue(100)), // 整数覆盖
    ("new.bool.attr", BoolValue(false)), // 新增布尔
    ("float.array.attr", ArrayStringValue(["float1", "float2"])), // 新增数组
    ("shared.array.attr", ArrayIntValue([10, 20, 30])) // 新增整数数组
  ]
  let mixed_override = Resource::with_attributes(override_resource, mixed_override_attributes)
  
  // 执行类型混合合并
  let mixed_merged = Resource::merge(mixed_base, mixed_override)
  
  // 验证类型混合合并结果
  let string_attr = Resource::get_attribute(mixed_merged, "string.attr")
  let int_attr = Resource::get_attribute(mixed_merged, "int.attr")
  let float_attr = Resource::get_attribute(mixed_merged, "float.attr")
  let bool_attr = Resource::get_attribute(mixed_merged, "bool.attr")
  let new_bool_attr = Resource::get_attribute(mixed_merged, "new.bool.attr")
  let string_array_attr = Resource::get_attribute(mixed_merged, "string.array.attr")
  let int_array_attr = Resource::get_attribute(mixed_merged, "int.array.attr")
  let float_array_attr = Resource::get_attribute(mixed_merged, "float.array.attr")
  let shared_array_attr = Resource::get_attribute(mixed_merged, "shared.array.attr")
  
  // 验证类型覆盖
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "override-string", "字符串属性应该被覆盖")
    _ => assert_true(false, "期望字符串属性")
  }
  
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 100, "整数属性应该被覆盖")
    _ => assert_true(false, "期望整数属性")
  }
  
  // 验证类型保留
  match float_attr {
    Some(FloatValue(value)) => assert_true(value > 3.14 && value < 3.15, "浮点属性应该被保留")
    _ => assert_true(false, "期望浮点属性")
  }
  
  match bool_attr {
    Some(BoolValue(value)) => assert_true(value, "布尔属性应该被保留")
    _ => assert_true(false, "期望布尔属性")
  }
  
  // 验证新增属性
  match new_bool_attr {
    Some(BoolValue(value)) => assert_false(value, "新增布尔属性应该存在")
    _ => assert_true(false, "期望新增布尔属性")
  }
  
  // 验证数组属性保留
  match string_array_attr {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 2, "字符串数组应该被保留")
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
    }
    _ => assert_true(false, "期望字符串数组属性")
  }
  
  match int_array_attr {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3, "整数数组应该被保留")
      assert_eq(arr[0], 1)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false, "期望整数数组属性")
  }
  
  // 验证新增数组属性
  match float_array_attr {
    Some(ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 2, "新增数组应该存在")
      assert_eq(arr[0], "float1")
      assert_eq(arr[1], "float2")
    }
    _ => assert_true(false, "期望新增数组属性")
  }
  
  match shared_array_attr {
    Some(ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 3, "共享数组应该存在")
      assert_eq(arr[0], 10)
      assert_eq(arr[2], 30)
    }
    _ => assert_true(false, "期望共享数组属性")
  }
}

test "资源边界条件和异常处理测试" {
  // 测试空资源合并
  let empty_resource1 = Resource::new()
  let empty_resource2 = Resource::new()
  let empty_merged = Resource::merge(empty_resource1, empty_resource2)
  
  // 验证空资源合并结果
  assert_eq(empty_merged.attributes.length(), 0, "空资源合并应该产生空资源")
  
  // 测试单侧空资源合并
  let non_empty_resource = Resource::new()
  let non_empty_attributes = [
    ("only.attr", StringValue("only-value")),
    ("number.attr", IntValue(999))
  ]
  let enriched_non_empty = Resource::with_attributes(non_empty_resource, non_empty_attributes)
  
  let empty_to_non_empty = Resource::merge(empty_resource1, enriched_non_empty)
  let non_empty_to_empty = Resource::merge(enriched_non_empty, empty_resource1)
  
  // 验证单侧空资源合并
  let only_attr1 = Resource::get_attribute(empty_to_non_empty, "only.attr")
  let only_attr2 = Resource::get_attribute(non_empty_to_empty, "only.attr")
  
  match only_attr1 {
    Some(StringValue(value)) => assert_eq(value, "only-value", "空到非空合并应该保留非空属性")
    _ => assert_true(false, "期望only.attr")
  }
  
  match only_attr2 {
    Some(StringValue(value)) => assert_eq(value, "only-value", "非空到空合并应该保留非空属性")
    _ => assert_true(false, "期望only.attr")
  }
  
  // 测试极值属性合并
  let extreme_resource = Resource::new()
  let extreme_attributes = [
    ("max.int", IntValue(2147483647)),
    ("min.int", IntValue(-2147483648)),
    ("max.float", FloatValue(1.7976931348623157e+308)),
    ("min.float", FloatValue(2.2250738585072014e-308)),
    ("empty.string", StringValue("")),
    ("long.string", StringValue("这是一个非常长的字符串，用于测试极值情况。" * 5))
  ]
  let enriched_extreme = Resource::with_attributes(extreme_resource, extreme_attributes)
  
  let extreme_merged = Resource::merge(enriched_non_empty, enriched_extreme)
  
  // 验证极值属性合并
  let max_int = Resource::get_attribute(extreme_merged, "max.int")
  let min_int = Resource::get_attribute(extreme_merged, "min.int")
  let max_float = Resource::get_attribute(extreme_merged, "max.float")
  let min_float = Resource::get_attribute(extreme_merged, "min.float")
  let empty_string = Resource::get_attribute(extreme_merged, "empty.string")
  let long_string = Resource::get_attribute(extreme_merged, "long.string")
  
  match max_int {
    Some(IntValue(value)) => assert_eq(value, 2147483647, "最大整数应该被保留")
    _ => assert_true(false, "期望max.int")
  }
  
  match min_int {
    Some(IntValue(value)) => assert_eq(value, -2147483648, "最小整数应该被保留")
    _ => assert_true(false, "期望min.int")
  }
  
  match max_float {
    Some(FloatValue(value)) => assert_true(value > 1.0e+307, "最大浮点数应该被保留")
    _ => assert_true(false, "期望max.float")
  }
  
  match min_float {
    Some(FloatValue(value)) => assert_true(value > 0.0 && value < 1.0e-307, "最小浮点数应该被保留")
    _ => assert_true(false, "期望min.float")
  }
  
  match empty_string {
    Some(StringValue(value)) => assert_eq(value.length(), 0, "空字符串应该被保留")
    _ => assert_true(false, "期望empty.string")
  }
  
  match long_string {
    Some(StringValue(value)) => assert_true(value.length() > 50, "长字符串应该被保留")
    _ => assert_true(false, "期望long.string")
  }
}

test "资源合并性能和大规模测试" {
  // 创建大规模基础资源
  let large_base_resource = Resource::new()
  let mut large_base_attributes = []
  
  // 添加大量属性
  for i = 0; i < 20; i = i + 1 {
    large_base_attributes.push(("base.attr" + i.to_string(), StringValue("base.value" + i.to_string())))
  }
  
  let large_enriched_base = Resource::with_attributes(large_base_resource, large_base_attributes)
  
  // 创建大规模覆盖资源
  let large_override_resource = Resource::new()
  let mut large_override_attributes = []
  
  // 添加大量覆盖属性
  for i = 0; i < 15; i = i + 1 {
    large_override_attributes.push(("base.attr" + i.to_string(), StringValue("override.value" + i.to_string())))
  }
  
  // 添加大量新属性
  for i = 0; i < 10; i = i + 1 {
    large_override_attributes.push(("new.attr" + i.to_string(), StringValue("new.value" + i.to_string())))
  }
  
  let large_enriched_override = Resource::with_attributes(large_override_resource, large_override_attributes)
  
  // 执行大规模合并
  let large_merged = Resource::merge(large_enriched_base, large_enriched_override)
  
  // 验证大规模合并结果
  let base_attr_0 = Resource::get_attribute(large_merged, "base.attr0")
  let base_attr_14 = Resource::get_attribute(large_merged, "base.attr14")
  let base_attr_19 = Resource::get_attribute(large_merged, "base.attr19")
  let new_attr_0 = Resource::get_attribute(large_merged, "new.attr0")
  let new_attr_9 = Resource::get_attribute(large_merged, "new.attr9")
  
  // 验证覆盖属性
  match base_attr_0 {
    Some(StringValue(value)) => assert_eq(value, "override.value0", "大规模覆盖应该成功")
    _ => assert_true(false, "期望base.attr0")
  }
  
  match base_attr_14 {
    Some(StringValue(value)) => assert_eq(value, "override.value14", "大规模覆盖应该成功")
    _ => assert_true(false, "期望base.attr14")
  }
  
  // 验证保留属性
  match base_attr_19 {
    Some(StringValue(value)) => assert_eq(value, "base.value19", "大规模保留应该成功")
    _ => assert_true(false, "期望base.attr19")
  }
  
  // 验证新增属性
  match new_attr_0 {
    Some(StringValue(value)) => assert_eq(value, "new.value0", "大规模新增应该成功")
    _ => assert_true(false, "期望new.attr0")
  }
  
  match new_attr_9 {
    Some(StringValue(value)) => assert_eq(value, "new.value9", "大规模新增应该成功")
    _ => assert_true(false, "期望new.attr9")
  }
  
  // 测试连续多次合并的性能
  let mut performance_resource = Resource::new()
  for i = 0; i < 5; i = i + 1 {
    let temp_resource = Resource::new()
    let temp_attributes = [
      ("performance.attr" + i.to_string(), StringValue("performance.value" + i.to_string()))
    ]
    let temp_enriched = Resource::with_attributes(temp_resource, temp_attributes)
    performance_resource = Resource::merge(performance_resource, temp_enriched)
  }
  
  // 验证性能测试结果
  let performance_attr_0 = Resource::get_attribute(performance_resource, "performance.attr0")
  let performance_attr_4 = Resource::get_attribute(performance_resource, "performance.attr4")
  
  match performance_attr_0 {
    Some(StringValue(value)) => assert_eq(value, "performance.value0", "性能测试属性0应该存在")
    _ => assert_true(false, "期望performance.attr0")
  }
  
  match performance_attr_4 {
    Some(StringValue(value)) => assert_eq(value, "performance.value4", "性能测试属性4应该存在")
    _ => assert_true(false, "期望performance.attr4")
  }
}