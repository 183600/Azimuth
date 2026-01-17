// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，基于实际存在的函数

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-3, 3))
  assert_eq(-10, add(-5, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(5, 0))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试最小值特殊处理
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_combinations" {
  // 测试数学运算组合
  // 计算 (2 + 3) * 4 = 20
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
  
  // 计算 5 * (3 + 2) = 25
  let sum2 = add(3, 2)
  let result2 = multiply(5, sum2)
  assert_eq(25, result2)
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值与0和1的运算
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试边界值加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-1, add(max_val, min_val))
}

test "assert_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(add(x, y) == 30)
  assert_false(multiply(x, y) == 200)
}