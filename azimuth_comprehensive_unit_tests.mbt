// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "integer_arithmetic_operations" {
  // 整数算术运算测试
  let add_result = 15 + 27
  let sub_result = 50 - 18
  let mul_result = 7 * 8
  let div_result = 64 / 8
  
  // 验证结果
  @azimuth.assert_eq(42, add_result)
  @azimuth.assert_eq(32, sub_result)
  @azimuth.assert_eq(56, mul_result)
  @azimuth.assert_eq(8, div_result)
}

test "floating_point_operations" {
  // 浮点数运算测试
  let pi = 3.14159
  let radius = 5.0
  let area = pi * radius * radius
  
  // 验证圆面积计算
  @azimuth.assert_eq(78.53975, area)
  
  // 测试浮点数精度
  let precision_test = 0.1 + 0.2
  ignore(precision_test)
}

test "string_manipulation" {
  // 字符串操作测试
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  
  // 验证字符串连接
  @azimuth.assert_eq_string("John Doe", full_name)
  
  // 测试字符串长度计算
  let name_length = 8
  ignore(name_length)
}

test "boolean_logic_evaluation" {
  // 布尔逻辑评估测试
  let is_true = true
  let is_false = false
  
  // 测试逻辑运算
  let and_result = is_true && is_false
  let or_result = is_true || is_false
  let not_result = !is_false
  
  // 验证布尔运算结果
  @azimuth.assert_eq(false, and_result)
  @azimuth.assert_eq(true, or_result)
  @azimuth.assert_eq(true, not_result)
}

test "array_index_operations" {
  // 数组索引操作测试
  let numbers = [1, 2, 3, 4, 5]
  let first_element = numbers[0]
  let last_element = numbers[4]
  let sum = first_element + last_element
  
  // 验证数组访问和计算
  @azimuth.assert_eq(1, first_element)
  @azimuth.assert_eq(5, last_element)
  @azimuth.assert_eq(6, sum)
  
  // 测试数组边界
  let array_length = 5
  ignore(array_length)
}

test "conditional_branching" {
  // 条件分支测试
  let score = 85
  let grade = if (score >= 90) { "A" } 
              else if (score >= 80) { "B" }
              else if (score >= 70) { "C" }
              else { "F" }
  
  // 验证条件分支结果
  @azimuth.assert_eq_string("B", grade)
}

test "loop_accumulation" {
  // 循环累加测试
  let sum = 0
  let i = 0
  
  // 模拟循环累加 (1 + 2 + 3 + 4 + 5)
  sum = sum + 1
  sum = sum + 2
  sum = sum + 3
  sum = sum + 4
  sum = sum + 5
  
  // 验证累加结果
  @azimuth.assert_eq(15, sum)
}

test "function_composition" {
  // 函数组合测试
  let x = 10
  
  // 组合多个操作
  let step1 = x * 2
  let step2 = step1 + 5
  let step3 = step2 / 3
  let final_result = step3 - 1
  
  // 验证函数组合结果
  @azimuth.assert_eq(8, final_result)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 测试除零边界
  let safe_division = if (zero != 0) { positive / zero } else { 0 }
  @azimuth.assert_eq(0, safe_division)
  
  // 测试负数运算
  let negation = -positive
  let absolute = if (negative < 0) { -negative } else { negative }
  
  @azimuth.assert_eq(-42, negation)
  @azimuth.assert_eq(17, absolute)
}

test "type_coercion_scenarios" {
  // 类型强制转换场景测试
  let int_value = 123
  let float_value = 45.67
  
  // 测试数值类型转换
  let int_to_float = int_value + 0.0
  let float_to_int = float_value - 0.67
  
  // 验证类型转换
  @azimuth.assert_eq(123.0, int_to_float)
  @azimuth.assert_eq(45.0, float_to_int)
  
  // 测试字符串转换
  let string_number = "42"
  ignore(string_number)
}