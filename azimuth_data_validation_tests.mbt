// Azimuth 数据验证测试用例
// 测试数据验证和完整性检查功能

// 测试1: 输入数据验证
test "输入数据验证" {
  // 测试有效数据
  let valid_data = [
    ("name", StringValue("azimuth")),
    ("version", StringValue("1.0.0")),
    ("port", IntValue(8080)),
    ("enabled", BoolValue(true)),
    ("threshold", FloatValue(0.95))
  ]
  
  // 验证数据完整性
  assert_eq(valid_data.length(), 5)
  
  // 验证字符串字段
  for data in valid_data {
    match data.0 {
      "name" => {
        match data.1 {
          StringValue(v) => {
            assert_true(v.length() > 0)
            assert_eq(v, "azimuth")
          }
          _ => assert_true(false)
        }
      }
      "version" => {
        match data.1 {
          StringValue(v) => {
            assert_true(v.contains("."))
            assert_eq(v, "1.0.0")
          }
          _ => assert_true(false)
        }
      }
      "port" => {
        match data.1 {
          IntValue(v) => {
            assert_true(v > 0)
            assert_true(v < 65536)
            assert_eq(v, 8080)
          }
          _ => assert_true(false)
        }
      }
      "enabled" => {
        match data.1 {
          BoolValue(v) => assert_true(v)
          _ => assert_true(false)
        }
      }
      "threshold" => {
        match data.1 {
          FloatValue(v) => {
            assert_true(v >= 0.0)
            assert_true(v <= 1.0)
            assert_eq(v, 0.95)
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
}

// 测试2: 边界条件验证
test "边界条件验证" {
  // 测试数值边界
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let zero_int = IntValue(0)
  
  match max_int {
    IntValue(v) => assert_eq(v, 2147483647)
    _ => assert_true(false)
  }
  
  match min_int {
    IntValue(v) => assert_eq(v, -2147483648)
    _ => assert_true(false)
  }
  
  match zero_int {
    IntValue(v) => assert_eq(v, 0)
    _ => assert_true(false)
  }
  
  // 测试浮点数边界
  let max_float = FloatValue(3.4028235e+38)
  let min_float = FloatValue(1.17549435e-38)
  let inf_float = FloatValue(1.0/0.0)  // 无穷大
  let nan_float = FloatValue(0.0/0.0)  // 非数字
  
  match max_float {
    FloatValue(v) => assert_true(v > 1.0e+38)
    _ => assert_true(false)
  }
  
  match min_float {
    FloatValue(v) => assert_true(v > 0.0 && v < 1.0e-37)
    _ => assert_true(false)
  }
  
  // 测试字符串边界
  let empty_string = StringValue("")
  let long_string = StringValue("a" * 1000)  // 1000个字符的字符串
  let unicode_string = StringValue("测试Unicode字符🚀")
  
  match empty_string {
    StringValue(v) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
  
  match long_string {
    StringValue(v) => assert_eq(v.length(), 1000)
    _ => assert_true(false)
  }
  
  match unicode_string {
    StringValue(v) => {
      assert_true(v.contains("测试"))
      assert_true(v.contains("🚀"))
    }
    _ => assert_true(false)
  }
}

// 测试3: 数据类型转换验证
test "数据类型转换验证" {
  // 测试字符串到整数转换
  let str_int = StringValue("123")
  let valid_int_result = convert_to_int(str_int)
  match valid_int_result {
    Ok(int_val) => assert_eq(int_val, 123)
    Err(_) => assert_true(false)
  }
  
  let invalid_str_int = StringValue("abc")
  let invalid_int_result = convert_to_int(invalid_str_int)
  match invalid_int_result {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  // 测试字符串到浮点数转换
  let str_float = StringValue("3.14159")
  let valid_float_result = convert_to_float(str_float)
  match valid_float_result {
    Ok(float_val) => assert_true(float_val > 3.14 && float_val < 3.15)
    Err(_) => assert_true(false)
  }
  
  // 测试字符串到布尔值转换
  let str_bool_true = StringValue("true")
  let str_bool_false = StringValue("false")
  let str_bool_invalid = StringValue("maybe")
  
  let true_result = convert_to_bool(str_bool_true)
  match true_result {
    Ok(bool_val) => assert_true(bool_val)
    Err(_) => assert_true(false)
  }
  
  let false_result = convert_to_bool(str_bool_false)
  match false_result {
    Ok(bool_val) => assert_false(bool_val)
    Err(_) => assert_true(false)
  }
  
  let invalid_bool_result = convert_to_bool(str_bool_invalid)
  match invalid_bool_result {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
}

// 测试4: 数组数据验证
test "数组数据验证" {
  // 测试整数数组
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
      
      // 验证数组元素类型
      for val in arr {
        assert_true(val > 0)
        assert_true(val <= 5)
      }
      
      // 验证数组总和
      let sum = arr.reduce(fn(acc, x) { acc + x }, 0)
      assert_eq(sum, 15)
    }
    _ => assert_true(false)
  }
  
  // 测试字符串数组
  let string_array = ArrayStringValue(["apple", "banana", "cherry"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "apple")
      assert_eq(arr[2], "cherry")
      
      // 验证字符串非空
      for str in arr {
        assert_true(str.length() > 0)
      }
      
      // 验证特定字符串存在
      assert_true(arr.contains("banana"))
      assert_false(arr.contains("orange"))
    }
    _ => assert_true(false)
  }
  
  // 测试混合类型数组
  let mixed_array = ArrayMixedValue([
    IntValue(42),
    StringValue("hello"),
    BoolValue(true),
    FloatValue(3.14)
  ])
  match mixed_array {
    ArrayMixedValue(arr) => {
      assert_eq(arr.length(), 4)
      
      // 验证第一个元素是整数
      match arr[0] {
        IntValue(v) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      // 验证第二个元素是字符串
      match arr[1] {
        StringValue(v) => assert_eq(v, "hello")
        _ => assert_true(false)
      }
      
      // 验证第三个元素是布尔值
      match arr[2] {
        BoolValue(v) => assert_true(v)
        _ => assert_true(false)
      }
      
      // 验证第四个元素是浮点数
      match arr[3] {
        FloatValue(v) => assert_true(v > 3.1 && v < 3.2)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 测试5: 结构化数据验证
test "结构化数据验证" {
  // 创建嵌套数据结构
  let user_data = StructValue({
    fields = [
      ("id", IntValue(123)),
      ("name", StringValue("John Doe")),
      ("email", StringValue("john@example.com")),
      ("profile", StructValue({
        fields = [
          ("age", IntValue(30)),
          ("active", BoolValue(true)),
          ("preferences", StructValue({
            fields = [
              ("theme", StringValue("dark")),
              ("notifications", BoolValue(false))
            ]
          }))
        ]
      }))
    ]
  })
  
  // 验证顶层结构
  match user_data {
    StructValue(struct) => {
      assert_eq(struct.fields.length(), 4)
      
      // 验证ID字段
      let id_field = struct.fields[0]
      assert_eq(id_field.0, "id")
      match id_field.1 {
        IntValue(v) => assert_eq(v, 123)
        _ => assert_true(false)
      }
      
      // 验证name字段
      let name_field = struct.fields[1]
      assert_eq(name_field.0, "name")
      match name_field.1 {
        StringValue(v) => assert_eq(v, "John Doe")
        _ => assert_true(false)
      }
      
      // 验证email字段格式
      let email_field = struct.fields[2]
      assert_eq(email_field.0, "email")
      match email_field.1 {
        StringValue(v) => {
          assert_true(v.contains("@"))
          assert_true(v.contains("."))
        }
        _ => assert_true(false)
      }
      
      // 验证嵌套profile结构
      let profile_field = struct.fields[3]
      assert_eq(profile_field.0, "profile")
      match profile_field.1 {
        StructValue(profile) => {
          assert_eq(profile.fields.length(), 3)
          
          // 验证profile中的age
          let age_field = profile.fields[0]
          assert_eq(age_field.0, "age")
          match age_field.1 {
            IntValue(v) => {
              assert_true(v >= 0)
              assert_true(v <= 150)
              assert_eq(v, 30)
            }
            _ => assert_true(false)
          }
          
          // 验证profile中的active
          let active_field = profile.fields[1]
          assert_eq(active_field.0, "active")
          match active_field.1 {
            BoolValue(v) => assert_true(v)
            _ => assert_true(false)
          }
          
          // 验证嵌套preferences结构
          let preferences_field = profile.fields[2]
          assert_eq(preferences_field.0, "preferences")
          match preferences_field.1 {
            StructValue(preferences) => {
              assert_eq(preferences.fields.length(), 2)
              
              // 验证theme
              let theme_field = preferences.fields[0]
              assert_eq(theme_field.0, "theme")
              match theme_field.1 {
                StringValue(v) => assert_eq(v, "dark")
                _ => assert_true(false)
              }
              
              // 验证notifications
              let notifications_field = preferences.fields[1]
              assert_eq(notifications_field.0, "notifications")
              match notifications_field.1 {
                BoolValue(v) => assert_false(v)
                _ => assert_true(false)
              }
            }
            _ => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 测试6: 数据完整性验证
test "数据完整性验证" {
  // 创建测试数据集
  let dataset = [
    ("record1", [
      ("id", IntValue(1)),
      ("name", StringValue("Record 1")),
      ("checksum", StringValue("abc123"))
    ]),
    ("record2", [
      ("id", IntValue(2)),
      ("name", StringValue("Record 2")),
      ("checksum", StringValue("def456"))
    ]),
    ("record3", [
      ("id", IntValue(3)),
      ("name", StringValue("Record 3")),
      ("checksum", StringValue("ghi789"))
    ])
  ]
  
  // 验证数据集完整性
  assert_eq(dataset.length(), 3)
  
  // 验证每个记录的完整性
  for record in dataset {
    let record_name = record.0
    let record_fields = record.1
    
    // 验证记录名称
    assert_true(record_name.contains("record"))
    assert_true(record_name.length() > 6)
    
    // 验证记录字段数量
    assert_eq(record_fields.length(), 3)
    
    // 验证ID字段存在且唯一
    let mut id_found = false
    let mut name_found = false
    let mut checksum_found = false
    
    for field in record_fields {
      match field.0 {
        "id" => {
          id_found = true
          match field.1 {
            IntValue(v) => assert_true(v > 0 && v <= 3)
            _ => assert_true(false)
          }
        }
        "name" => {
          name_found = true
          match field.1 {
            StringValue(v) => {
              assert_true(v.contains("Record"))
              assert_true(v.length() > 6)
            }
            _ => assert_true(false)
          }
        }
        "checksum" => {
          checksum_found = true
          match field.1 {
            StringValue(v) => assert_eq(v.length(), 6)
            _ => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    
    // 确保所有必需字段都存在
    assert_true(id_found)
    assert_true(name_found)
    assert_true(checksum_found)
  }
  
  // 验证ID唯一性
  let ids = dataset.map(fn(record) {
    for field in record.1 {
      if field.0 == "id" {
        match field.1 {
          IntValue(v) => return v
          _ => assert_true(false)
        }
      }
    }
    return -1  // 不应该到达这里
  })
  
  assert_eq(ids.length(), 3)
  assert_eq(ids[0], 1)
  assert_eq(ids[1], 2)
  assert_eq(ids[2], 3)
  
  // 验证ID唯一性
  assert_true(ids[0] != ids[1])
  assert_true(ids[1] != ids[2])
  assert_true(ids[0] != ids[2])
}