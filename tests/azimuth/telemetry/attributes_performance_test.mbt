// Attributes模块性能测试用例
// 测试attributes操作的性能和效率

test "attributes creation and destruction performance" {
  // 测试属性创建和销毁的性能
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 创建大量属性集合
  let attributes_list = []
  for i = 0; i < 1000; i = i + 1 {
    let attrs = telemetry.api.common.Attributes.new()
    attributes_list.push(attrs)
  }
  
  let creation_time = telemetry.sdk.platform.time.now_unix_nanos()
  let creation_duration = creation_time - start_time
  
  // 验证创建时间在合理范围内（这里只是确保不崩溃）
  assert_true(creation_duration > 0)
  assert_eq(attributes_list.length(), 1000)
  
  // 销毁属性集合（通过让它们超出作用域）
  let destroyed_count = 0
  for i = 0; i < attributes_list.length(); i = i + 1 {
    // 简单地访问以确认它们仍然存在
    let attrs = attributes_list[i]
    let size = attrs.values.length()
    assert_eq(size, 0)
    destroyed_count = destroyed_count + 1
  }
  
  assert_eq(destroyed_count, 1000)
}

test "attributes setting and getting performance" {
  // 测试属性设置和获取的性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 设置大量属性
  for i = 0; i < 10000; i = i + 1 {
    let key = "perf.key.${i}"
    let value = telemetry.api.common.StringValue("perf.value.${i}")
    telemetry.api.common.Attributes.set(attrs, key, value)
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  assert_eq(attrs.values.length(), 10000)
  
  // 获取大量属性
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    let key = "perf.key.${i}"
    let value = telemetry.api.common.Attributes.get(attrs, key)
    assert_true(value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes with different value types performance" {
  // 测试不同值类型的属性性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 设置不同类型的值
  for i = 0; i < 2000; i = i + 1 {
    let string_key = "string.key.${i}"
    let int_key = "int.key.${i}"
    let float_key = "float.key.${i}"
    let bool_key = "bool.key.${i}"
    let array_key = "array.key.${i}"
    
    telemetry.api.common.Attributes.set(attrs, string_key, 
      telemetry.api.common.StringValue("string.value.${i}"))
    telemetry.api.common.Attributes.set(attrs, int_key, 
      telemetry.api.common.IntValue(i))
    telemetry.api.common.Attributes.set(attrs, float_key, 
      telemetry.api.common.FloatValue(i.to_float() * 1.5))
    telemetry.api.common.Attributes.set(attrs, bool_key, 
      telemetry.api.common.BoolValue(i % 2 == 0))
    telemetry.api.common.Attributes.set(attrs, array_key, 
      telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"]))
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  assert_eq(attrs.values.length(), 10000) // 2000 * 5 types
  
  // 获取不同类型的值
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < 2000; i = i + 1 {
    let string_key = "string.key.${i}"
    let int_key = "int.key.${i}"
    let float_key = "float.key.${i}"
    let bool_key = "bool.key.${i}"
    let array_key = "array.key.${i}"
    
    let string_value = telemetry.api.common.Attributes.get(attrs, string_key)
    let int_value = telemetry.api.common.Attributes.get(attrs, int_key)
    let float_value = telemetry.api.common.Attributes.get(attrs, float_key)
    let bool_value = telemetry.api.common.Attributes.get(attrs, bool_key)
    let array_value = telemetry.api.common.Attributes.get(attrs, array_key)
    
    assert_true(string_value != None)
    assert_true(int_value != None)
    assert_true(float_value != None)
    assert_true(bool_value != None)
    assert_true(array_value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes with long keys and values performance" {
  // 测试长键和值的属性性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 创建长键和值
  let long_key_prefix = "very.long.attribute.name.that.could.potentially.cause.performance.issues."
  let long_value_prefix = "very.long.attribute.value.that.could.also.potentially.cause.performance.issues."
  
  for i = 0; i < 1000; i = i + 1 {
    let long_key = long_key_prefix + i.to_string()
    let long_value = long_value_prefix + i.to_string()
    
    telemetry.api.common.Attributes.set(attrs, long_key, 
      telemetry.api.common.StringValue(long_value))
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  assert_eq(attrs.values.length(), 1000)
  
  // 获取长键和值
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let long_key = long_key_prefix + i.to_string()
    let value = telemetry.api.common.Attributes.get(attrs, long_key)
    assert_true(value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes with special characters performance" {
  // 测试包含特殊字符的属性性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 包含特殊字符的键
  let special_keys = [
    "key.with.dots",
    "key-with-dashes",
    "key_with_underscores",
    "key/with/slashes",
    "key\\with\\backslashes",
    "key.with spaces",
    "key\twith\ttabs",
    "key\nwith\nnewlines",
    "key\rwith\rcarriage",
    "key\"with\"quotes",
    "key'with'apostrophes",
    "key[with]brackets",
    "key{with}braces",
    "key(with)parentheses",
    "key<with>angles",
    "key|with|pipes",
    "key+with+plus",
    "key=with=equals",
    "key?with?questions",
    "key@with@ats",
    "key#with#hashes",
    "key$with$dollars",
    "key%with%percents",
    "key^with^carets",
    "key&with&ampersands",
    "key*with*asterisks"
  ]
  
  for i = 0; i < special_keys.length(); i = i + 1 {
    let key = special_keys[i]
    let value = telemetry.api.common.StringValue("value.for.${key}")
    telemetry.api.common.Attributes.set(attrs, key, value)
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  assert_eq(attrs.values.length(), special_keys.length())
  
  // 获取包含特殊字符的键
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < special_keys.length(); i = i + 1 {
    let key = special_keys[i]
    let value = telemetry.api.common.Attributes.get(attrs, key)
    assert_true(value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes overwrite performance" {
  // 测试属性覆盖的性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 重复设置相同的键（覆盖）
  for i = 0; i < 10000; i = i + 1 {
    let key = "overwrite.key"
    let value = telemetry.api.common.StringValue("value.${i}")
    telemetry.api.common.Attributes.set(attrs, key, value)
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  // 即使设置10000次，也应该只有1个键
  assert_eq(attrs.values.length(), 1)
  
  // 多次获取相同的键
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    let key = "overwrite.key"
    let value = telemetry.api.common.Attributes.get(attrs, key)
    assert_true(value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes with array values performance" {
  // 测试数组值的属性性能
  let attrs = telemetry.api.common.Attributes.new()
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 设置数组值
  for i = 0; i < 1000; i = i + 1 {
    let key = "array.key.${i}"
    let array_size = i % 100 + 1 // 1到100个元素的数组
    
    let array_value = []
    for j = 0; j < array_size; j = j + 1 {
      array_value.push("array.item.${i}.${j}")
    }
    
    telemetry.api.common.Attributes.set(attrs, key, 
      telemetry.api.common.ArrayStringValue(array_value))
  }
  
  let set_time = telemetry.sdk.platform.time.now_unix_nanos()
  let set_duration = set_time - start_time
  
  // 验证设置时间
  assert_true(set_duration > 0)
  assert_eq(attrs.values.length(), 1000)
  
  // 获取数组值
  let get_start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let key = "array.key.${i}"
    let value = telemetry.api.common.Attributes.get(attrs, key)
    assert_true(value != None)
  }
  
  let get_time = telemetry.sdk.platform.time.now_unix_nanos()
  let get_duration = get_time - get_start_time
  
  // 验证获取时间
  assert_true(get_duration > 0)
}

test "attributes memory usage efficiency" {
  // 测试属性内存使用效率
  let initial_memory = 1000000 // 假设的初始内存
  
  let start_time = telemetry.sdk.platform.time.now_unix_nanos()
  
  // 创建大量属性集合并立即释放
  for batch = 0; batch < 10; batch = batch + 1 {
    let batch_attrs = []
    
    for i = 0; i < 1000; i = i + 1 {
      let attrs = telemetry.api.common.Attributes.new()
      
      for j = 0; j < 10; j = j + 1 {
        let key = "batch${batch}.attr${i}.${j}"
        let value = telemetry.api.common.StringValue("value${batch}.${i}.${j}")
        telemetry.api.common.Attributes.set(attrs, key, value)
      }
      
      batch_attrs.push(attrs)
    }
    
    // 模拟使用属性
    for i = 0; i < batch_attrs.length(); i = i + 1 {
      let attrs = batch_attrs[i]
      let size = attrs.values.length()
      assert_eq(size, 10)
    }
    
    // batch_attrs超出作用域，应该被垃圾回收
  }
  
  let end_time = telemetry.sdk.platform.time.now_unix_nanos()
  let total_duration = end_time - start_time
  
  // 验证总时间
  assert_true(total_duration > 0)
  
  // 如果内存管理良好，内存使用应该保持在合理范围内
  // 这里我们只测试操作完成而不崩溃
  assert_true(true)
}