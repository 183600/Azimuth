// 简单的标准 MoonBit 测试用例
// 不依赖外部函数，只使用基本语言特性

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, 2 + 3)
  assert_eq(10, 4 * 2 + 2)
  assert_eq(15, 3 * 5)
  assert_eq(8, 10 - 2)
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, World!", "Hello, " + "World!")
  assert_eq_string("Test", "Test")
  assert_eq_string("", "")
  assert_eq_string("A", "A")
}

test "boolean_logic" {
  // 测试布尔逻辑
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_true(5 >= 5)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_false(5 != 5)
}

test "conditional_expressions" {
  // 测试条件表达式
  let result1 = if (5 > 3) { "greater" } else { "less" }
  assert_eq_string("greater", result1)
  
  let result2 = if (2 > 3) { "greater" } else { "less" }
  assert_eq_string("less", result2)
  
  let result3 = if (true) { 42 } else { 0 }
  assert_eq(42, result3)
}

test "variable_assignments" {
  // 测试变量赋值
  let x = 10
  let y = 20
  let sum = x + y
  assert_eq(30, sum)
  
  let product = x * y
  assert_eq(200, product)
  
  let message = "Result: " + sum.to_string()
  assert_eq_string("Result: 30", message)
}

test "function_composition" {
  // 测试函数组合
  let add_one = fn(x : Int) -> Int { x + 1 }
  let multiply_by_two = fn(x : Int) -> Int { x * 2 }
  
  // 组合函数：(x + 1) * 2
  let compose = fn(x : Int) -> Int { multiply_by_two(add_one(x)) }
  
  assert_eq(6, compose(2))  // (2 + 1) * 2 = 6
  assert_eq(10, compose(4)) // (4 + 1) * 2 = 10
}

test "nested_expressions" {
  // 测试嵌套表达式
  let result = (2 + 3) * (4 - 1) + 5
  assert_eq(20, result)  // 5 * 3 + 5 = 20
  
  let nested = if ((2 + 3) > 4) { 
    (10 * 2) + 5 
  } else { 
    0 
  }
  assert_eq(25, nested)
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(0, 0)
  assert_eq(1, 1)
  assert_eq(-1, -1)
  assert_eq_string("", "")
  assert_eq_string(" ", " ")
  assert_true(true)
  assert_false(false)
}

test "complex_calculations" {
  // 测试复杂计算
  let a = 5
  let b = 10
  let c = 15
  
  // (a + b) * c - a * b
  let result = (a + b) * c - a * b
  assert_eq(175, result)  // (5 + 10) * 15 - 5 * 10 = 225 - 50 = 175
  
  // a² + b² + c²
  let sum_of_squares = a * a + b * b + c * c
  assert_eq(350, sum_of_squares)  // 25 + 100 + 225 = 350
}

test "string_and_number_conversion" {
  // 测试字符串和数字转换
  let num = 42
  let num_str = num.to_string()
  assert_eq_string("42", num_str)
  
  let message = "The answer is " + num_str
  assert_eq_string("The answer is 42", message)
  
  let negative = -10
  let negative_str = negative.to_string()
  assert_eq_string("-10", negative_str)
}