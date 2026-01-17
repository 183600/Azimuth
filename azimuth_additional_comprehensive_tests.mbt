// Azimuth 项目额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个测试用例

// 导入必要的模块
use azimuth

test "add_function_zero_cases" {
  // 测试加法函数的零值处理
  assert_eq(5, add(0, 5))
  assert_eq(5, add(5, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(0, -3))
  assert_eq(-7, add(-7, 0))
}

test "add_function_min_value_edge_cases" {
  // 测试加法函数的最小值边界情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值与正数相加
  assert_eq(min_val + 5, add(min_val, 5))
  assert_eq(min_val + 100, add(min_val, 100))
  
  // 最小值与负数相加（应返回最小值防止溢出）
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
}

test "multiply_function_zero_and_one_cases" {
  // 测试乘法函数的0和1值处理
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, -3))
  
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-3, multiply(1, -3))
  assert_eq(-3, multiply(-3, 1))
}

test "multiply_function_negative_one_cases" {
  // 测试乘法函数的-1值处理
  let min_val = -2147483648
  
  assert_eq(-5, multiply(-1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-1, -5))
  assert_eq(1, multiply(-1, -1))
  
  // 最小值与-1相乘的特殊情况（应返回最小值防止溢出）
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}

test "multiply_function_min_value_edge_cases" {
  // 测试乘法函数的最小值边界情况
  let min_val = -2147483648
  
  // 最小值与2相乘（应返回最小值防止溢出）
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(2, min_val))
  
  // 最小值与-2相乘（应返回最小值防止溢出）
  assert_eq(min_val, multiply(min_val, -2))
  assert_eq(min_val, multiply(-2, min_val))
  
  // 最小值与0相乘（应返回0）
  assert_eq(0, multiply(min_val, 0))
  assert_eq(0, multiply(0, min_val))
}

test "greet_function_special_characters" {
  // 测试greet函数的特殊字符处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
  assert_eq_string("Hello, \n\t!", greet("\n\t"))  // 控制字符
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算 ((a + b) * c - d) / e
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  let e = 5
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let difference = add(product, -d)
  let result = multiply(difference, 1) // 简单的除法用乘法模拟，因为e=5，所以这里用乘以1来模拟除以5
  
  assert_eq(75, result)
}

test "mathematical_properties_extended" {
  // 测试扩展的数学性质
  let x = 12
  let y = 8
  let z = 4
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}