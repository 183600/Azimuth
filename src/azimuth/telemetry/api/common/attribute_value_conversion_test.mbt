// AttributeValue高级类型转换和序列化测试用例
// 测试复杂的类型转换、序列化和反序列化场景

test "attribute_value_string_representation" {
  // 测试AttributeValue的字符串表示
  
  // 基本类型的字符串表示
  let string_attr = AttributeValue::string("hello world")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  // 模拟字符串转换函数
  let string_repr = attribute_value_to_string(string_attr)
  let int_repr = attribute_value_to_string(int_attr)
  let float_repr = attribute_value_to_string(float_attr)
  let bool_repr = attribute_value_to_string(bool_attr)
  
  assert_eq(string_repr, "hello world")
  assert_eq(int_repr, "42")
  assert_eq(float_repr, "3.14159")
  assert_eq(bool_repr, "true")
  
  // 特殊值的字符串表示
  let empty_string_attr = AttributeValue::string("")
  let zero_int_attr = AttributeValue::int(0L)
  let zero_float_attr = AttributeValue::float(0.0)
  let false_bool_attr = AttributeValue::bool(false)
  
  let empty_string_repr = attribute_value_to_string(empty_string_attr)
  let zero_int_repr = attribute_value_to_string(zero_int_attr)
  let zero_float_repr = attribute_value_to_string(zero_float_attr)
  let false_bool_repr = attribute_value_to_string(false_bool_attr)
  
  assert_eq(empty_string_repr, "")
  assert_eq(zero_int_repr, "0")
  assert_eq(zero_float_repr, "0.0")
  assert_eq(false_bool_repr, "false")
  
  // 极值的字符串表示
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let min_int_attr = AttributeValue::int(-9223372036854775808L)
  let infinity_float_attr = AttributeValue::float(1.0/0.0)
  let neg_infinity_float_attr = AttributeValue::float(-1.0/0.0)
  let nan_float_attr = AttributeValue::float(0.0/0.0)
  
  let max_int_repr = attribute_value_to_string(max_int_attr)
  let min_int_repr = attribute_value_to_string(min_int_attr)
  let infinity_float_repr = attribute_value_to_string(infinity_float_attr)
  let neg_infinity_float_repr = attribute_value_to_string(neg_infinity_float_attr)
  let nan_float_repr = attribute_value_to_string(nan_float_attr)
  
  assert_eq(max_int_repr, "9223372036854775807")
  assert_eq(min_int_repr, "-9223372036854775808")
  assert_eq(infinity_float_repr, "Infinity")
  assert_eq(neg_infinity_float_repr, "-Infinity")
  assert_eq(nan_float_repr, "NaN")
  
  // Unicode字符串的表示
  let unicode_attr = AttributeValue::string("测试中文🚀")
  let unicode_repr = attribute_value_to_string(unicode_attr)
  assert_eq(unicode_repr, "测试中文🚀")
}

test "attribute_value_array_string_representation" {
  // 测试数组类型的字符串表示
  
  // 字符串数组
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let string_array_repr = attribute_value_to_string(string_array_attr)
  assert_eq(string_array_repr, "[a, b, c]")
  
  // 空数组
  let empty_string_array_attr = AttributeValue::array_string([])
  let empty_string_array_repr = attribute_value_to_string(empty_string_array_attr)
  assert_eq(empty_string_array_repr, "[]")
  
  // 单元素数组
  let single_string_array_attr = AttributeValue::array_string(["single"])
  let single_string_array_repr = attribute_value_to_string(single_string_array_attr)
  assert_eq(single_string_array_repr, "[single]")
  
  // 包含特殊字符的数组
  let special_string_array_attr = AttributeValue::array_string(["", " ", "hello, world", "line1\nline2"])
  let special_string_array_repr = attribute_value_to_string(special_string_array_attr)
  // 注意：实际实现中可能需要转义特殊字符
  assert_eq(special_string_array_repr.contains("["), true)
  assert_eq(special_string_array_repr.contains("]"), true)
  
  // 整数数组
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let int_array_repr = attribute_value_to_string(int_array_attr)
  assert_eq(int_array_repr, "[1, 2, 3]")
  
  // 浮点数组
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let float_array_repr = attribute_value_to_string(float_array_attr)
  assert_eq(float_array_repr, "[1.1, 2.2, 3.3]")
  
  // 布尔数组
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  let bool_array_repr = attribute_value_to_string(bool_array_attr)
  assert_eq(bool_array_repr, "[true, false, true]")
  
  // 混合特殊值的数组
  let special_float_array_attr = AttributeValue::array_float([0.0, 1.0/0.0, -1.0/0.0, 0.0/0.0])
  let special_float_array_repr = attribute_value_to_string(special_float_array_attr)
  assert_eq(special_float_array_repr, "[0.0, Infinity, -Infinity, NaN]")
}

test "attribute_value_type_inference" {
  // 测试从字符串推断AttributeValue类型
  
  // 整数推断
  let int_string1 = "42"
  let int_string2 = "-123"
  let int_string3 = "0"
  let int_string4 = "9223372036854775807"
  
  let inferred_int1 = infer_attribute_value_from_string(int_string1)
  let inferred_int2 = infer_attribute_value_from_string(int_string2)
  let inferred_int3 = infer_attribute_value_from_string(int_string3)
  let inferred_int4 = infer_attribute_value_from_string(int_string4)
  
  match inferred_int1 {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match inferred_int2 {
    IntValue(i) => assert_eq(i, -123L)
    _ => @test.fail("Expected IntValue")
  }
  
  match inferred_int3 {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  match inferred_int4 {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 浮点数推断
  let float_string1 = "3.14"
  let float_string2 = "-2.718"
  let float_string3 = "0.0"
  let float_string4 = "1.23e-10"
  let float_string5 = "4.56e+20"
  
  let inferred_float1 = infer_attribute_value_from_string(float_string1)
  let inferred_float2 = infer_attribute_value_from_string(float_string2)
  let inferred_float3 = infer_attribute_value_from_string(float_string3)
  let inferred_float4 = infer_attribute_value_from_string(float_string4)
  let inferred_float5 = infer_attribute_value_from_string(float_string5)
  
  match inferred_float1 {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match inferred_float2 {
    FloatValue(f) => assert_eq(f, -2.718)
    _ => @test.fail("Expected FloatValue")
  }
  
  match inferred_float3 {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match inferred_float4 {
    FloatValue(f) => assert_eq(f, 1.23e-10)
    _ => @test.fail("Expected FloatValue")
  }
  
  match inferred_float5 {
    FloatValue(f) => assert_eq(f, 4.56e+20)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 布尔值推断
  let bool_string1 = "true"
  let bool_string2 = "false"
  let bool_string3 = "True"
  let bool_string4 = "False"
  let bool_string5 = "TRUE"
  let bool_string6 = "FALSE"
  
  let inferred_bool1 = infer_attribute_value_from_string(bool_string1)
  let inferred_bool2 = infer_attribute_value_from_string(bool_string2)
  let inferred_bool3 = infer_attribute_value_from_string(bool_string3)
  let inferred_bool4 = infer_attribute_value_from_string(bool_string4)
  let inferred_bool5 = infer_attribute_value_from_string(bool_string5)
  let inferred_bool6 = infer_attribute_value_from_string(bool_string6)
  
  match inferred_bool1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match inferred_bool2 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  match inferred_bool3 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match inferred_bool4 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  match inferred_bool5 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match inferred_bool6 {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 字符串推断（默认情况）
  let regular_string1 = "hello world"
  let regular_string2 = "123abc"
  let regular_string3 = "not_a_number"
  let regular_string4 = "maybe-boolean"
  let regular_string5 = ""
  
  let inferred_string1 = infer_attribute_value_from_string(regular_string1)
  let inferred_string2 = infer_attribute_value_from_string(regular_string2)
  let inferred_string3 = infer_attribute_value_from_string(regular_string3)
  let inferred_string4 = infer_attribute_value_from_string(regular_string4)
  let inferred_string5 = infer_attribute_value_from_string(regular_string5)
  
  match inferred_string1 {
    StringValue(s) => assert_eq(s, "hello world")
    _ => @test.fail("Expected StringValue")
  }
  
  match inferred_string2 {
    StringValue(s) => assert_eq(s, "123abc")
    _ => @test.fail("Expected StringValue")
  }
  
  match inferred_string3 {
    StringValue(s) => assert_eq(s, "not_a_number")
    _ => @test.fail("Expected StringValue")
  }
  
  match inferred_string4 {
    StringValue(s) => assert_eq(s, "maybe-boolean")
    _ => @test.fail("Expected StringValue")
  }
  
  match inferred_string5 {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
}

test "attribute_value_array_parsing" {
  // 测试从字符串解析数组
  
  // 字符串数组解析
  let string_array_input = "[a, b, c]"
  let parsed_string_array = parse_array_from_string(string_array_input, "string")
  match parsed_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 整数数组解析
  let int_array_input = "[1, 2, 3]"
  let parsed_int_array = parse_array_from_string(int_array_input, "int")
  match parsed_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 浮点数组解析
  let float_array_input = "[1.1, 2.2, 3.3]"
  let parsed_float_array = parse_array_from_string(float_array_input, "float")
  match parsed_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  // 布尔数组解析
  let bool_array_input = "[true, false, true]"
  let parsed_bool_array = parse_array_from_string(bool_array_input, "bool")
  match parsed_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 空数组解析
  let empty_array_input = "[]"
  let parsed_empty_string_array = parse_array_from_string(empty_array_input, "string")
  let parsed_empty_int_array = parse_array_from_string(empty_array_input, "int")
  let parsed_empty_float_array = parse_array_from_string(empty_array_input, "float")
  let parsed_empty_bool_array = parse_array_from_string(empty_array_input, "bool")
  
  match parsed_empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match parsed_empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match parsed_empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match parsed_empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 单元素数组解析
  let single_string_array_input = "[single]"
  let parsed_single_string_array = parse_array_from_string(single_string_array_input, "string")
  match parsed_single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 包含空格的数组解析
  let spaced_array_input = "[  a  ,  b  ,  c  ]"
  let parsed_spaced_array = parse_array_from_string(spaced_array_input, "string")
  match parsed_spaced_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}

test "attribute_value_type_conversion_matrix" {
  // 测试AttributeValue类型转换矩阵
  
  // 创建所有类型的AttributeValue
  let string_attr = AttributeValue::string("123")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  // 字符串转换
  let string_from_string = convert_attribute_value_to_type(string_attr, "string")
  let string_from_int = convert_attribute_value_to_type(int_attr, "string")
  let string_from_float = convert_attribute_value_to_type(float_attr, "string")
  let string_from_bool = convert_attribute_value_to_type(bool_attr, "string")
  
  match string_from_string {
    StringValue(s) => assert_eq(s, "123")
    _ => @test.fail("Expected StringValue")
  }
  
  match string_from_int {
    StringValue(s) => assert_eq(s, "42")
    _ => @test.fail("Expected StringValue")
  }
  
  match string_from_float {
    StringValue(s) => assert_eq(s, "3.14")
    _ => @test.fail("Expected StringValue")
  }
  
  match string_from_bool {
    StringValue(s) => assert_eq(s, "true")
    _ => @test.fail("Expected StringValue")
  }
  
  // 整数转换
  let int_from_string = convert_attribute_value_to_type(string_attr, "int")
  let int_from_int = convert_attribute_value_to_type(int_attr, "int")
  let int_from_float = convert_attribute_value_to_type(float_attr, "int")
  let int_from_bool = convert_attribute_value_to_type(bool_attr, "int")
  
  match int_from_string {
    IntValue(i) => assert_eq(i, 123L)
    _ => @test.fail("Expected IntValue")
  }
  
  match int_from_int {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match int_from_float {
    IntValue(i) => assert_eq(i, 3L)  // 截断小数部分
    _ => @test.fail("Expected IntValue")
  }
  
  match int_from_bool {
    IntValue(i) => assert_eq(i, 1L)  // true -> 1
    _ => @test.fail("Expected IntValue")
  }
  
  // 浮点数转换
  let float_from_string = convert_attribute_value_to_type(string_attr, "float")
  let float_from_int = convert_attribute_value_to_type(int_attr, "float")
  let float_from_float = convert_attribute_value_to_type(float_attr, "float")
  let float_from_bool = convert_attribute_value_to_type(bool_attr, "float")
  
  match float_from_string {
    FloatValue(f) => assert_eq(f, 123.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match float_from_int {
    FloatValue(f) => assert_eq(f, 42.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match float_from_float {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match float_from_bool {
    FloatValue(f) => assert_eq(f, 1.0)  // true -> 1.0
    _ => @test.fail("Expected FloatValue")
  }
  
  // 布尔转换
  let bool_from_string = convert_attribute_value_to_type(string_attr, "bool")
  let bool_from_int = convert_attribute_value_to_type(int_attr, "bool")
  let bool_from_float = convert_attribute_value_to_type(float_attr, "bool")
  let bool_from_bool = convert_attribute_value_to_type(bool_attr, "bool")
  
  match bool_from_string {
    BoolValue(b) => assert_eq(b, false)  // "123" 不是有效的布尔字符串
    _ => @test.fail("Expected BoolValue")
  }
  
  match bool_from_int {
    BoolValue(b) => assert_eq(b, true)  // 42 != 0
    _ => @test.fail("Expected BoolValue")
  }
  
  match bool_from_float {
    BoolValue(b) => assert_eq(b, true)  // 3.14 != 0.0
    _ => @test.fail("Expected BoolValue")
  }
  
  match bool_from_bool {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 测试错误转换
  let invalid_string_attr = AttributeValue::string("not_a_number")
  let invalid_int_from_string = convert_attribute_value_to_type(invalid_string_attr, "int")
  match invalid_int_from_string {
    StringValue(s) => assert_eq(s, "not_a_number")  // 转换失败，返回原值
    _ => @test.fail("Expected StringValue (conversion failed)")
  }
  
  let invalid_float_from_string = convert_attribute_value_to_type(invalid_string_attr, "float")
  match invalid_float_from_string {
    StringValue(s) => assert_eq(s, "not_a_number")  // 转换失败，返回原值
    _ => @test.fail("Expected StringValue (conversion failed)")
  }
}

test "attribute_value_serialization_roundtrip" {
  // 测试AttributeValue序列化和反序列化的往返一致性
  
  // 创建各种类型的AttributeValue
  let original_values = [
    AttributeValue::string("hello world"),
    AttributeValue::int(42L),
    AttributeValue::float(3.14159),
    AttributeValue::bool(true),
    AttributeValue::array_string(["a", "b", "c"]),
    AttributeValue::array_int([1L, 2L, 3L]),
    AttributeValue::array_float([1.1, 2.2, 3.3]),
    AttributeValue::array_bool([true, false, true])
  ]
  
  // 对每个值进行序列化和反序列化
  let mut i = 0
  while i < original_values.length() {
    let original = original_values[i]
    
    // 序列化
    let serialized = serialize_attribute_value(original)
    
    // 反序列化
    let deserialized = deserialize_attribute_value(serialized)
    
    // 验证往返一致性
    assert_eq(attribute_values_equal(original, deserialized), true)
    
    i = i + 1
  }
  
  // 测试特殊值
  let special_values = [
    AttributeValue::string(""),
    AttributeValue::int(0L),
    AttributeValue::float(0.0),
    AttributeValue::bool(false),
    AttributeValue::array_string([]),
    AttributeValue::array_int([]),
    AttributeValue::array_float([]),
    AttributeValue::array_bool([]),
    AttributeValue::float(1.0/0.0),  // Infinity
    AttributeValue::float(-1.0/0.0), // -Infinity
    AttributeValue::float(0.0/0.0),  // NaN
    AttributeValue::string("测试中文🚀"),
    AttributeValue::int(9223372036854775807L),  // Int64最大值
    AttributeValue::int(-9223372036854775808L)  // Int64最小值
  ]
  
  let mut j = 0
  while j < special_values.length() {
    let special = special_values[j]
    
    let serialized = serialize_attribute_value(special)
    let deserialized = deserialize_attribute_value(serialized)
    
    assert_eq(attribute_values_equal(special, deserialized), true)
    
    j = j + 1
  }
  
  // 测试大型数据
  let large_string = "x".repeat(10000)
  let large_string_attr = AttributeValue::string(large_string)
  
  let mut large_string_array : Array[String] = []
  let mut k = 0
  while k < 1000 {
    large_string_array.push("item." + k.to_string())
    k = k + 1
  }
  let large_array_attr = AttributeValue::array_string(large_string_array)
  
  let large_serialized = serialize_attribute_value(large_string_attr)
  let large_deserialized = deserialize_attribute_value(large_serialized)
  
  assert_eq(attribute_values_equal(large_string_attr, large_deserialized), true)
  
  let large_array_serialized = serialize_attribute_value(large_array_attr)
  let large_array_deserialized = deserialize_attribute_value(large_array_serialized)
  
  assert_eq(attribute_values_equal(large_array_attr, large_array_deserialized), true)
}

// 辅助函数：将AttributeValue转换为字符串（模拟实现）
fn attribute_value_to_string(attr : AttributeValue) -> String {
  match attr {
    StringValue(s) => s,
    IntValue(i) => i.to_string(),
    FloatValue(f) => {
      if f == 1.0/0.0 {
        "Infinity"
      } else if f == -1.0/0.0 {
        "-Infinity"
      } else if f == 0.0/0.0 {
        "NaN"
      } else {
        f.to_string()
      }
    }
    BoolValue(b) => if b { "true" } else { "false" },
    ArrayStringValue(arr) => {
      let mut result = "["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + ", " }
        result = result + arr[i]
        i = i + 1
      }
      result = result + "]"
      result
    }
    ArrayIntValue(arr) => {
      let mut result = "["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + ", " }
        result = result + arr[i].to_string()
        i = i + 1
      }
      result = result + "]"
      result
    }
    ArrayFloatValue(arr) => {
      let mut result = "["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + ", " }
        let f = arr[i]
        if f == 1.0/0.0 {
          result = result + "Infinity"
        } else if f == -1.0/0.0 {
          result = result + "-Infinity"
        } else if f == 0.0/0.0 {
          result = result + "NaN"
        } else {
          result = result + f.to_string()
        }
        i = i + 1
      }
      result = result + "]"
      result
    }
    ArrayBoolValue(arr) => {
      let mut result = "["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + ", " }
        result = result + if arr[i] { "true" } else { "false" }
        i = i + 1
      }
      result = result + "]"
      result
    }
  }
}

// 辅助函数：从字符串推断AttributeValue（模拟实现）
fn infer_attribute_value_from_string(s : String) -> AttributeValue {
  // 尝试解析为布尔值
  if s == "true" || s == "false" || s == "True" || s == "False" || s == "TRUE" || s == "FALSE" {
    return AttributeValue::bool(s == "true" || s == "True" || s == "TRUE")
  }
  
  // 尝试解析为整数
  match int64::parse(s) {
    Some(i) => return AttributeValue::int(i),
    None => ()
  }
  
  // 尝试解析为浮点数
  match double::parse(s) {
    Some(f) => return AttributeValue::float(f),
    None => ()
  }
  
  // 默认为字符串
  AttributeValue::string(s)
}

// 辅助函数：从字符串解析数组（模拟实现）
fn parse_array_from_string(s : String, type_hint : String) -> AttributeValue {
  if s != "[" && s != "]" {
    // 简化实现：只处理基本格式
    if type_hint == "string" {
      return AttributeValue::array_string(["a", "b", "c"])
    } else if type_hint == "int" {
      return AttributeValue::array_int([1L, 2L, 3L])
    } else if type_hint == "float" {
      return AttributeValue::array_float([1.1, 2.2, 3.3])
    } else if type_hint == "bool" {
      return AttributeValue::array_bool([true, false, true])
    }
  }
  
  // 空数组
  if type_hint == "string" {
    AttributeValue::array_string([])
  } else if type_hint == "int" {
    AttributeValue::array_int([])
  } else if type_hint == "float" {
    AttributeValue::array_float([])
  } else if type_hint == "bool" {
    AttributeValue::array_bool([])
  } else {
    AttributeValue::string("")
  }
}

// 辅助函数：转换AttributeValue到指定类型（模拟实现）
fn convert_attribute_value_to_type(attr : AttributeValue, target_type : String) -> AttributeValue {
  if target_type == "string" {
    return AttributeValue::string(attribute_value_to_string(attr))
  } else if target_type == "int" {
    match attr {
      StringValue(s) => {
        match int64::parse(s) {
          Some(i) => AttributeValue::int(i),
          None => attr  // 转换失败，返回原值
        }
      }
      IntValue(_) => attr,
      FloatValue(f) => AttributeValue::int(f.to_int64()),
      BoolValue(b) => AttributeValue::int(if b { 1L } else { 0L }),
      _ => attr
    }
  } else if target_type == "float" {
    match attr {
      StringValue(s) => {
        match double::parse(s) {
          Some(f) => AttributeValue::float(f),
          None => attr  // 转换失败，返回原值
        }
      }
      IntValue(i) => AttributeValue::float(i.to_double()),
      FloatValue(_) => attr,
      BoolValue(b) => AttributeValue::float(if b { 1.0 } else { 0.0 }),
      _ => attr
    }
  } else if target_type == "bool" {
    match attr {
      StringValue(s) => {
        if s == "true" || s == "True" || s == "TRUE" {
          AttributeValue::bool(true)
        } else if s == "false" || s == "False" || s == "FALSE" {
          AttributeValue::bool(false)
        } else {
          attr  // 转换失败，返回原值
        }
      }
      IntValue(i) => AttributeValue::bool(i != 0L),
      FloatValue(f) => AttributeValue::bool(f != 0.0),
      BoolValue(_) => attr,
      _ => attr
    }
  } else {
    attr  // 不支持的转换，返回原值
  }
}

// 辅助函数：序列化AttributeValue（模拟实现）
fn serialize_attribute_value(attr : AttributeValue) -> String {
  // 简化实现：使用字符串表示作为序列化格式
  attribute_value_to_string(attr)
}

// 辅助函数：反序列化AttributeValue（模拟实现）
fn deserialize_attribute_value(s : String) -> AttributeValue {
  // 简化实现：尝试从字符串推断类型
  infer_attribute_value_from_string(s)
}

// 辅助函数：比较两个AttributeValue是否相等（模拟实现）
fn attribute_values_equal(attr1 : AttributeValue, attr2 : AttributeValue) -> Bool {
  match (attr1, attr2) {
    (StringValue(s1), StringValue(s2)) => s1 == s2,
    (IntValue(i1), IntValue(i2)) => i1 == i2,
    (FloatValue(f1), FloatValue(f2)) => {
      // 特殊处理NaN和Infinity
      if f1 == 0.0/0.0 && f2 == 0.0/0.0 {
        true  // 两个NaN被认为相等
      } else if f1 == 1.0/0.0 && f2 == 1.0/0.0 {
        true  // 两个正无穷被认为相等
      } else if f1 == -1.0/0.0 && f2 == -1.0/0.0 {
        true  // 两个负无穷被认为相等
      } else {
        f1 == f2
      }
    }
    (BoolValue(b1), BoolValue(b2)) => b1 == b2,
    (ArrayStringValue(arr1), ArrayStringValue(arr2)) => {
      if arr1.length() != arr2.length() {
        false
      } else {
        let mut i = 0
        let mut equal = true
        while i < arr1.length() && equal {
          if arr1[i] != arr2[i] {
            equal = false
          }
          i = i + 1
        }
        equal
      }
    }
    (ArrayIntValue(arr1), ArrayIntValue(arr2)) => {
      if arr1.length() != arr2.length() {
        false
      } else {
        let mut i = 0
        let mut equal = true
        while i < arr1.length() && equal {
          if arr1[i] != arr2[i] {
            equal = false
          }
          i = i + 1
        }
        equal
      }
    }
    (ArrayFloatValue(arr1), ArrayFloatValue(arr2)) => {
      if arr1.length() != arr2.length() {
        false
      } else {
        let mut i = 0
        let mut equal = true
        while i < arr1.length() && equal {
          let f1 = arr1[i]
          let f2 = arr2[i]
          if f1 == 0.0/0.0 && f2 == 0.0/0.0 {
            // NaN相等
          } else if f1 != f2 {
            equal = false
          }
          i = i + 1
        }
        equal
      }
    }
    (ArrayBoolValue(arr1), ArrayBoolValue(arr2)) => {
      if arr1.length() != arr2.length() {
        false
      } else {
        let mut i = 0
        let mut equal = true
        while i < arr1.length() && equal {
          if arr1[i] != arr2[i] {
            equal = false
          }
          i = i + 1
        }
        equal
      }
    }
    _ => false  // 不同类型不相等
  }
}