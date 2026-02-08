// 新增的 MoonBit 测试用例
// 包含 10 个标准测试用例，使用标准 MoonBit 测试语法

test "simple_arithmetic_addition" {
  // 简单加法测试
  let result = 2 + 3
  assert_eq(result, 5)
}

test "simple_arithmetic_multiplication" {
  // 简单乘法测试
  let result = 4 * 6
  assert_eq(result, 24)
}

test "simple_arithmetic_subtraction" {
  // 简单减法测试
  let result = 10 - 7
  assert_eq(result, 3)
}

test "integer_division" {
  // 整数除法测试
  let result = 10 / 3
  assert_eq(result, 3)
}

test "modulo_operation" {
  // 取模运算测试
  let result = 10 % 3
  assert_eq(result, 1)
}

test "boolean_operations" {
  // 布尔运算测试
  let result1 = true && false
  let result2 = true || false
  let result3 = not true
  
  assert_eq(result1, false)
  assert_eq(result2, true)
  assert_eq(result3, false)
}

test "comparison_operations" {
  // 比较运算测试
  let result1 = 5 > 3
  let result2 = 5 < 3
  let result3 = 5 >= 5
  let result4 = 5 <= 5
  let result5 = 5 == 5
  let result6 = 5 != 3
  
  assert_eq(result1, true)
  assert_eq(result2, false)
  assert_eq(result3, true)
  assert_eq(result4, true)
  assert_eq(result5, true)
  assert_eq(result6, true)
}

test "variable_assignment" {
  // 变量赋值测试
  let x = 10
  let y = 20
  let result = x + y
  
  assert_eq(result, 30)
}

test "nested_operations" {
  // 嵌套运算测试
  let result = (2 + 3) * (4 - 1)
  assert_eq(result, 15)
}

test "sequential_operations" {
  // 顺序运算测试
  let step1 = 10
  let step2 = step1 + 5
  let step3 = step2 * 2
  let step4 = step3 - 10
  
  assert_eq(step1, 10)
  assert_eq(step2, 15)
  assert_eq(step3, 30)
  assert_eq(step4, 20)
}