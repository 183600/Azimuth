// Azimuth Premium Data Type Conversion Tests
// 高质量数据类型转换测试用例
// 专注于测试遥测系统中各种数据类型的转换和一致性

// 测试1: AttributeValue类型转换和验证
test "AttributeValue类型转换完整性测试" {
  // 1. 字符串值转换测试
  let string_attr = AttributeValue::StringValue("test_value")
  match string_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "test_value")
      assert_eq(s.length(), 11)
    }
    _ => assert_true(false, "字符串值转换失败")
  }
  
  // 2. 整数值转换测试
  let int_attr = AttributeValue::IntValue(42)
  match int_attr {
    AttributeValue::IntValue(i) => {
      assert_eq(i, 42)
      assert_true(i > 0)
    }
    _ => assert_true(false, "整数值转换失败")
  }
  
  // 3. 浮点数值转换测试
  let float_attr = AttributeValue::FloatValue(3.14159)
  match float_attr {
    AttributeValue::FloatValue(f) => {
      assert_true(f > 3.14 && f < 3.15)
      assert_true(f > 0.0)
    }
    _ => assert_true(false, "浮点数值转换失败")
  }
  
  // 4. 布尔值转换测试
  let bool_attr = AttributeValue::BoolValue(true)
  match bool_attr {
    AttributeValue::BoolValue(b) => {
      assert_true(b)
    }
    _ => assert_true(false, "布尔值转换失败")
  }
  
  // 5. 字符串数组转换测试
  let string_array_attr = AttributeValue::ArrayStringValue(["value1", "value2", "value3"])
  match string_array_attr {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "value1")
      assert_eq(arr[1], "value2")
      assert_eq(arr[2], "value3")
    }
    _ => assert_true(false, "字符串数组转换失败")
  }
  
  // 6. 整数数组转换测试
  let int_array_attr = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  match int_array_attr {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
      // 验证数组总和
      let mut sum = 0
      for i in arr {
        sum = sum + i
      }
      assert_eq(sum, 15)
    }
    _ => assert_true(false, "整数数组转换失败")
  }
}

// 测试2: Attributes结构体操作和类型转换
test "Attributes结构体高级操作测试" {
  // 1. 创建包含多种类型的Attributes
  let attributes = Attributes({
    values: [
      ("string_key", AttributeValue::StringValue("string_value")),
      ("int_key", AttributeValue::IntValue(100)),
      ("float_key", AttributeValue::FloatValue(2.71828)),
      ("bool_key", AttributeValue::BoolValue(false)),
      ("string_array_key", AttributeValue::ArrayStringValue(["a", "b", "c"])),
      ("int_array_key", AttributeValue::ArrayIntValue([10, 20, 30]))
    ]
  })
  
  // 2. 验证Attributes长度
  assert_eq(attributes.values.length(), 6)
  
  // 3. 验证特定属性的存在和类型
  let mut found_string = false
  let mut found_int = false
  let mut found_float = false
  let mut found_bool = false
  let mut found_string_array = false
  let mut found_int_array = false
  
  for (key, value) in attributes.values {
    match key {
      "string_key" => {
        match value {
          AttributeValue::StringValue(s) => {
            assert_eq(s, "string_value")
            found_string = true
          }
          _ => assert_true(false, "字符串属性类型不匹配")
        }
      }
      "int_key" => {
        match value {
          AttributeValue::IntValue(i) => {
            assert_eq(i, 100)
            found_int = true
          }
          _ => assert_true(false, "整数属性类型不匹配")
        }
      }
      "float_key" => {
        match value {
          AttributeValue::FloatValue(f) => {
            assert_true(f > 2.71 && f < 2.72)
            found_float = true
          }
          _ => assert_true(false, "浮点数属性类型不匹配")
        }
      }
      "bool_key" => {
        match value {
          AttributeValue::BoolValue(b) => {
            assert_false(b)
            found_bool = true
          }
          _ => assert_true(false, "布尔属性类型不匹配")
        }
      }
      "string_array_key" => {
        match value {
          AttributeValue::ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            found_string_array = true
          }
          _ => assert_true(false, "字符串数组属性类型不匹配")
        }
      }
      "int_array_key" => {
        match value {
          AttributeValue::ArrayIntValue(arr) => {
            assert_eq(arr.length(), 3)
            found_int_array = true
          }
          _ => assert_true(false, "整数数组属性类型不匹配")
        }
      }
      _ => assert_true(false, "未知的属性键: " + key)
    }
  }
  
  // 4. 验证所有属性都被找到
  assert_true(found_string, "字符串属性未找到")
  assert_true(found_int, "整数属性未找到")
  assert_true(found_float, "浮点数属性未找到")
  assert_true(found_bool, "布尔属性未找到")
  assert_true(found_string_array, "字符串数组属性未找到")
  assert_true(found_int_array, "整数数组属性未找到")
}

// 测试3: Resource属性类型转换和验证
test "Resource属性类型转换验证测试" {
  // 1. 创建包含多种资源属性的Resource
  let resource = Resource({
    attributes: [
      ("service.name", AttributeValue::StringValue("azimuth-telemetry")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-12345")),
      ("host.name", AttributeValue::StringValue("production-server")),
      ("host.cpu.count", AttributeValue::IntValue(8)),
      ("host.memory.total", AttributeValue::IntValue(16777216)),
      ("host.cpu.utilization", AttributeValue::FloatValue(0.75)),
      ("process.pid", AttributeValue::IntValue(1234)),
      ("process.executable.name", AttributeValue::StringValue("azimuth")),
      ("deployment.environment", AttributeValue::StringValue("production"))
    ]
  })
  
  // 2. 验证资源属性数量
  assert_eq(resource.attributes.length(), 10)
  
  // 3. 验证关键服务属性
  let mut service_name_found = false
  let mut service_version_found = false
  let mut environment_found = false
  
  for (key, value) in resource.attributes {
    match key {
      "service.name" => {
        match value {
          AttributeValue::StringValue(name) => {
            assert_eq(name, "azimuth-telemetry")
            service_name_found = true
          }
          _ => assert_true(false, "服务名称类型错误")
        }
      }
      "service.version" => {
        match value {
          AttributeValue::StringValue(version) => {
            assert_eq(version, "1.0.0")
            service_version_found = true
          }
          _ => assert_true(false, "服务版本类型错误")
        }
      }
      "deployment.environment" => {
        match value {
          AttributeValue::StringValue(env) => {
            assert_eq(env, "production")
            environment_found = true
          }
          _ => assert_true(false, "部署环境类型错误")
        }
      }
      "host.cpu.count" => {
        match value {
          AttributeValue::IntValue(count) => {
            assert_eq(count, 8)
            assert_true(count > 0)
          }
          _ => assert_true(false, "CPU核心数类型错误")
        }
      }
      "host.memory.total" => {
        match value {
          AttributeValue::IntValue(memory) => {
            assert_eq(memory, 16777216)
            assert_true(memory > 0)
          }
          _ => assert_true(false, "内存总量类型错误")
        }
      }
      _ => () // 忽略其他属性
    }
  }
  
  // 4. 验证关键属性存在
  assert_true(service_name_found, "服务名称属性未找到")
  assert_true(service_version_found, "服务版本属性未找到")
  assert_true(environment_found, "部署环境属性未找到")
}

// 测试4: 边界条件和特殊值转换测试
test "边界条件和特殊值转换测试" {
  // 1. 空字符串处理
  let empty_string = AttributeValue::StringValue("")
  match empty_string {
    AttributeValue::StringValue(s) => {
      assert_eq(s, "")
      assert_eq(s.length(), 0)
    }
    _ => assert_true(false, "空字符串处理失败")
  }
  
  // 2. 零值处理
  let zero_int = AttributeValue::IntValue(0)
  match zero_int {
    AttributeValue::IntValue(i) => {
      assert_eq(i, 0)
    }
    _ => assert_true(false, "零值处理失败")
  }
  
  // 3. 负数处理
  let negative_int = AttributeValue::IntValue(-42)
  match negative_int {
    AttributeValue::IntValue(i) => {
      assert_eq(i, -42)
      assert_true(i < 0)
    }
    _ => assert_true(false, "负数处理失败")
  }
  
  // 4. 零浮点数处理
  let zero_float = AttributeValue::FloatValue(0.0)
  match zero_float {
    AttributeValue::FloatValue(f) => {
      assert_eq(f, 0.0)
    }
    _ => assert_true(false, "零浮点数处理失败")
  }
  
  // 5. 负浮点数处理
  let negative_float = AttributeValue::FloatValue(-3.14159)
  match negative_float {
    AttributeValue::FloatValue(f) => {
      assert_true(f < -3.14 && f > -3.15)
    }
    _ => assert_true(false, "负浮点数处理失败")
  }
  
  // 6. 极大数值处理
  let large_int = AttributeValue::IntValue(9223372036854775807) // Int64最大值
  match large_int {
    AttributeValue::IntValue(i) => {
      assert_true(i > 9223372036854775806)
    }
    _ => assert_true(false, "极大数值处理失败")
  }
  
  // 7. 极小数值处理
  let small_float = AttributeValue::FloatValue(0.0000001)
  match small_float {
    AttributeValue::FloatValue(f) => {
      assert_true(f > 0.0 && f < 0.000001)
    }
    _ => assert_true(false, "极小数值处理失败")
  }
  
  // 8. 空数组处理
  let empty_string_array = AttributeValue::ArrayStringValue([])
  match empty_string_array {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => assert_true(false, "空字符串数组处理失败")
  }
  
  let empty_int_array = AttributeValue::ArrayIntValue([])
  match empty_int_array {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 0)
    }
    _ => assert_true(false, "空整数数组处理失败")
  }
}

// 测试5: 复杂嵌套数据类型转换测试
test "复杂嵌套数据类型转换测试" {
  // 1. 创建包含长字符串的属性
  let long_string = "这是一个非常长的字符串，用于测试系统对长文本的处理能力。"
                  + "它包含了多个字符和特殊符号：!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
                  + "以及中文字符：测试数据类型转换的完整性和正确性。"
  let long_string_attr = AttributeValue::StringValue(long_string)
  match long_string_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s.length(), 78)
      assert_true(s.contains("测试"))
      assert_true(s.contains("!@#$%^&*"))
    }
    _ => assert_true(false, "长字符串处理失败")
  }
  
  // 2. 创建包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./~`"
  let special_chars_attr = AttributeValue::StringValue(special_chars)
  match special_chars_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s.length(), 29)
      assert_true(s.contains("!@#$%^&*"))
      assert_true(s.contains("{}[]|\\"))
    }
    _ => assert_true(false, "特殊字符字符串处理失败")
  }
  
  // 3. 创建包含Unicode字符的字符串
  let unicode_string = "Unicode测试: 🌟 遥测系统 📊 数据收集 🔄"
  let unicode_attr = AttributeValue::StringValue(unicode_string)
  match unicode_attr {
    AttributeValue::StringValue(s) => {
      assert_eq(s.length(), 19)
      assert_true(s.contains("🌟"))
      assert_true(s.contains("📊"))
      assert_true(s.contains("🔄"))
    }
    _ => assert_true(false, "Unicode字符串处理失败")
  }
  
  // 4. 创建包含混合类型的大数组
  let large_string_array = AttributeValue::ArrayStringValue([
    "item1", "item2", "item3", "item4", "item5",
    "item6", "item7", "item8", "item9", "item10"
  ])
  match large_string_array {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 10)
      assert_eq(arr[0], "item1")
      assert_eq(arr[9], "item10")
    }
    _ => assert_true(false, "大字符串数组处理失败")
  }
  
  // 5. 创建包含大数值的数组
  let large_int_array = AttributeValue::ArrayIntValue([
    1000000, 2000000, 3000000, 4000000, 5000000
  ])
  match large_int_array {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1000000)
      assert_eq(arr[4], 5000000)
      
      // 验证数组总和
      let mut sum = 0
      for i in arr {
        sum = sum + i
      }
      assert_eq(sum, 15000000)
    }
    _ => assert_true(false, "大整数数组处理失败")
  }
}