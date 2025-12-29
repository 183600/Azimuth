// AttributeValue类型转换和验证测试
test "attribute_value_type_conversion_and_validation" {
  // 测试字符串类型的各种转换
  let string_attr = AttributeValue::string("test_value")
  match string_attr {
    StringValue(s) => {
      assert_eq(s, "test_value")
      // 测试空字符串
      let empty_string = AttributeValue::string("")
      match empty_string {
        StringValue(empty) => assert_eq(empty, "")
        _ => @test.fail("Expected StringValue for empty string")
      }
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试整数类型的边界值
  let int_attr = AttributeValue::int(42L)
  match int_attr {
    IntValue(i) => {
      assert_eq(i, 42L)
      // 测试边界值
      let zero_int = AttributeValue::int(0L)
      match zero_int {
        IntValue(zero) => assert_eq(zero, 0L)
        _ => @test.fail("Expected IntValue for zero")
      }
      
      let max_int = AttributeValue::int(9223372036854775807L)
      match max_int {
        IntValue(max) => assert_eq(max, 9223372036854775807L)
        _ => @test.fail("Expected IntValue for max value")
      }
      
      let min_int = AttributeValue::int(-9223372036854775808L)
      match min_int {
        IntValue(min) => assert_eq(min, -9223372036854775808L)
        _ => @test.fail("Expected IntValue for min value")
      }
    }
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试浮点数类型的特殊值
  let float_attr = AttributeValue::float(3.14159)
  match float_attr {
    FloatValue(f) => {
      assert_eq(f, 3.14159)
      // 测试特殊浮点值
      let zero_float = AttributeValue::float(0.0)
      match zero_float {
        FloatValue(zero) => assert_eq(zero, 0.0)
        _ => @test.fail("Expected FloatValue for zero")
      }
      
      let negative_float = AttributeValue::float(-1.0)
      match negative_float {
        FloatValue(neg) => assert_eq(neg, -1.0)
        _ => @test.fail("Expected FloatValue for negative")
      }
      
      let infinity = AttributeValue::float(1.0/0.0)
      match infinity {
        FloatValue(inf) => {
          // 验证无穷大
          assert_eq(inf > 1000000.0, true)
        }
        _ => @test.fail("Expected FloatValue for infinity")
      }
      
      let nan = AttributeValue::float(0.0/0.0)
      match nan {
        FloatValue(n) => {
          // NaN不等于任何值，包括自身
          assert_eq(n != n, true)
        }
        _ => @test.fail("Expected FloatValue for NaN")
      }
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试布尔类型
  let bool_attr = AttributeValue::bool(true)
  match bool_attr {
    BoolValue(b) => {
      assert_eq(b, true)
      let false_attr = AttributeValue::bool(false)
      match false_attr {
        BoolValue(false_val) => assert_eq(false_val, false)
        _ => @test.fail("Expected BoolValue for false")
      }
    }
    _ => @test.fail("Expected BoolValue")
  }
}

test "attribute_value_array_operations_and_validation" {
  // 测试字符串数组
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
      
      // 测试空数组
      let empty_string_array = AttributeValue::array_string([])
      match empty_string_array {
        ArrayStringValue(empty) => assert_eq(empty.length(), 0)
        _ => @test.fail("Expected ArrayStringValue for empty array")
      }
      
      // 测试单元素数组
      let single_string_array = AttributeValue::array_string(["single"])
      match single_string_array {
        ArrayStringValue(single) => {
          assert_eq(single.length(), 1)
          assert_eq(single[0], "single")
        }
        _ => @test.fail("Expected ArrayStringValue for single element")
      }
      
      // 测试包含特殊字符的字符串数组
      let special_string_array = AttributeValue::array_string(["", " ", "\t", "\n", "特殊字符", "!@#$%^&*()"])
      match special_string_array {
        ArrayStringValue(special) => {
          assert_eq(special.length(), 6)
          assert_eq(special[0], "")
          assert_eq(special[1], " ")
          assert_eq(special[2], "\t")
          assert_eq(special[3], "\n")
          assert_eq(special[4], "特殊字符")
          assert_eq(special[5], "!@#$%^&*()")
        }
        _ => @test.fail("Expected ArrayStringValue for special characters")
      }
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 测试整数数组
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
      
      // 测试边界值数组
      let boundary_int_array = AttributeValue::array_int([
        0L, 
        1L, 
        -1L, 
        9223372036854775807L, 
        -9223372036854775808L
      ])
      match boundary_int_array {
        ArrayIntValue(boundary) => {
          assert_eq(boundary.length(), 5)
          assert_eq(boundary[0], 0L)
          assert_eq(boundary[1], 1L)
          assert_eq(boundary[2], -1L)
          assert_eq(boundary[3], 9223372036854775807L)
          assert_eq(boundary[4], -9223372036854775808L)
        }
        _ => @test.fail("Expected ArrayIntValue for boundary values")
      }
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试浮点数数组
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
      
      // 测试特殊浮点值数组
      let special_float_array = AttributeValue::array_float([
        0.0, 
        -0.0, 
        1.0/0.0,  // Infinity
        -1.0/0.0, // -Infinity
        0.0/0.0   // NaN
      ])
      match special_float_array {
        ArrayFloatValue(special) => {
          assert_eq(special.length(), 5)
          assert_eq(special[0], 0.0)
          assert_eq(special[1], -0.0)
          assert_eq(special[2] > 1000000.0, true)  // Infinity
          assert_eq(special[3] < -1000000.0, true) // -Infinity
          assert_eq(special[4] != special[4], true) // NaN
        }
        _ => @test.fail("Expected ArrayFloatValue for special values")
      }
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  // 测试布尔数组
  let bool_array = AttributeValue::array_bool([true, false, true])
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
      
      // 测试全true和全false数组
      let all_true_array = AttributeValue::array_bool([true, true, true, true])
      match all_true_array {
        ArrayBoolValue(all_true) => {
          assert_eq(all_true.length(), 4)
          let mut i = 0
          while i < all_true.length() {
            assert_eq(all_true[i], true)
            i = i + 1
          }
        }
        _ => @test.fail("Expected ArrayBoolValue for all true")
      }
      
      let all_false_array = AttributeValue::array_bool([false, false, false])
      match all_false_array {
        ArrayBoolValue(all_false) => {
          assert_eq(all_false.length(), 3)
          let mut i = 0
          while i < all_false.length() {
            assert_eq(all_false[i], false)
            i = i + 1
          }
        }
        _ => @test.fail("Expected ArrayBoolValue for all false")
      }
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "resource_with_comprehensive_attributes_validation" {
  // 创建包含所有类型属性的Resource
  let comprehensive_attributes = [
    ("string.simple", AttributeValue::string("simple_value")),
    ("string.empty", AttributeValue::string("")),
    ("string.special", AttributeValue::string("特殊字符!@#$%^&*()")),
    ("int.zero", AttributeValue::int(0L)),
    ("int.positive", AttributeValue::int(42L)),
    ("int.negative", AttributeValue::int(-42L)),
    ("int.max", AttributeValue::int(9223372036854775807L)),
    ("int.min", AttributeValue::int(-9223372036854775808L)),
    ("float.zero", AttributeValue::float(0.0)),
    ("float.positive", AttributeValue::float(3.14159)),
    ("float.negative", AttributeValue::float(-3.14159)),
    ("float.infinity", AttributeValue::float(1.0/0.0)),
    ("float.nan", AttributeValue::float(0.0/0.0)),
    ("bool.true", AttributeValue::bool(true)),
    ("bool.false", AttributeValue::bool(false)),
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool", AttributeValue::array_bool([true, false, true]))
  ]
  
  let resource = Resource::default("comprehensive-test-service")
  
  // 验证基本属性
  assert_eq(resource.service_name, "comprehensive-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  match resource.service_version {
    Some(_) => @test.fail("Expected None for default resource version")
    None => assert_eq(true, true)
  }
  
  // 验证默认资源属性为空
  assert_eq(resource.attributes.length(), 0)
  
  // 创建带有复杂属性的资源（通过创建新的Resource实例）
  let complex_resource = Resource::default("complex-service")
  
  // 验证复杂属性的结构
  assert_eq(comprehensive_attributes.length(), 20)
  
  // 验证字符串属性
  match comprehensive_attributes[0].1 {
    StringValue(simple) => assert_eq(simple, "simple_value")
    _ => @test.fail("Expected StringValue")
  }
  
  match comprehensive_attributes[1].1 {
    StringValue(empty) => assert_eq(empty, "")
    _ => @test.fail("Expected StringValue for empty")
  }
  
  // 验证整数属性
  match comprehensive_attributes[3].1 {
    IntValue(zero) => assert_eq(zero, 0L)
    _ => @test.fail("Expected IntValue for zero")
  }
  
  match comprehensive_attributes[6].1 {
    IntValue(max) => assert_eq(max, 9223372036854775807L)
    _ => @test.fail("Expected IntValue for max")
  }
  
  // 验证浮点数属性
  match comprehensive_attributes[9].1 {
    FloatValue(zero) => assert_eq(zero, 0.0)
    _ => @test.fail("Expected FloatValue for zero")
  }
  
  match comprehensive_attributes[11].1 {
    FloatValue(negative) => assert_eq(negative, -3.14159)
    _ => @test.fail("Expected FloatValue for negative")
  }
  
  // 验证布尔属性
  match comprehensive_attributes[13].1 {
    BoolValue(true_val) => assert_eq(true_val, true)
    _ => @test.fail("Expected BoolValue for true")
  }
  
  match comprehensive_attributes[14].1 {
    BoolValue(false_val) => assert_eq(false_val, false)
    _ => @test.fail("Expected BoolValue for false")
  }
  
  // 验证数组属性
  match comprehensive_attributes[15].1 {
    ArrayStringValue(str_array) => {
      assert_eq(str_array.length(), 3)
      assert_eq(str_array[0], "a")
      assert_eq(str_array[1], "b")
      assert_eq(str_array[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match comprehensive_attributes[16].1 {
    ArrayIntValue(int_array) => {
      assert_eq(int_array.length(), 3)
      assert_eq(int_array[0], 1L)
      assert_eq(int_array[1], 2L)
      assert_eq(int_array[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match comprehensive_attributes[17].1 {
    ArrayFloatValue(float_array) => {
      assert_eq(float_array.length(), 3)
      assert_eq(float_array[0], 1.1)
      assert_eq(float_array[1], 2.2)
      assert_eq(float_array[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match comprehensive_attributes[18].1 {
    ArrayBoolValue(bool_array) => {
      assert_eq(bool_array.length(), 3)
      assert_eq(bool_array[0], true)
      assert_eq(bool_array[1], false)
      assert_eq(bool_array[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "instrumentation_scope_comprehensive_validation" {
  // 测试基本的InstrumentationScope
  let basic_scope = InstrumentationScope::{
    name: "test-instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(basic_scope.name, "test-instrument")
  match basic_scope.version {
    Some(_) => @test.fail("Expected None for version")
    None => assert_eq(true, true)
  }
  match basic_scope.schema_url {
    Some(_) => @test.fail("Expected None for schema_url")
    None => assert_eq(true, true)
  }
  
  // 测试带有版本和schema的InstrumentationScope
  let full_scope = InstrumentationScope::{
    name: "full-instrument",
    version: Some("1.2.3"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(full_scope.name, "full-instrument")
  match full_scope.version {
    Some(v) => assert_eq(v, "1.2.3")
    None => @test.fail("Expected Some(version)")
  }
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema/v1")
    None => @test.fail("Expected Some(schema_url)")
  }
  
  // 测试边界情况
  let empty_name_scope = InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  
  assert_eq(empty_name_scope.name, "")
  match empty_name_scope.version {
    Some(v) => assert_eq(v, "")
    None => @test.fail("Expected Some(empty version)")
  }
  match empty_name_scope.schema_url {
    Some(url) => assert_eq(url, "")
    None => @test.fail("Expected Some(empty schema_url)")
  }
  
  // 测试特殊字符
  let special_scope = InstrumentationScope::{
    name: "special-instrument!@#$%^&*()",
    version: Some("版本-1.0.0"),
    schema_url: Some("https://测试.example.com/schema?param=value#fragment")
  }
  
  assert_eq(special_scope.name, "special-instrument!@#$%^&*()")
  match special_scope.version {
    Some(v) => assert_eq(v, "版本-1.0.0")
    None => @test.fail("Expected Some(special version)")
  }
  match special_scope.schema_url {
    Some(url) => assert_eq(url, "https://测试.example.com/schema?param=value#fragment")
    None => @test.fail("Expected Some(special schema_url)")
  }
  
  // 测试长字符串
  let long_name = "a".repeat(1000)
  let long_scope = InstrumentationScope::{
    name: long_name,
    version: Some("b".repeat(100)),
    schema_url: Some("c".repeat(500))
  }
  
  assert_eq(long_scope.name.length(), 1000)
  match long_scope.version {
    Some(v) => assert_eq(v.length(), 100)
    None => @test.fail("Expected Some(long version)")
  }
  match long_scope.schema_url {
    Some(url) => assert_eq(url.length(), 500)
    None => @test.fail("Expected Some(long schema_url)")
  }
}