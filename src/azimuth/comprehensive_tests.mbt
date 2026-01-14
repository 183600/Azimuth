// 综合测试用例 - 覆盖 azimuth 模块的主要功能
// 使用标准的 MoonBit 测试语法

// 导入 azimuth 模块
use azimuth

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
  
  // 测试边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_mathematical_calculations" {
  // 测试复杂数学计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
  
  // (a + b + c) * 2
  let result3 = multiply(add(add(a, b), c), 2)
  assert_eq(120, result3)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = multiply(2, add(length, width))
  assert_eq(50, perimeter)
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(160, discounted_price)
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 正常除法
  assert_eq(2, divide_with_ceil(10, 5))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(4, divide_with_ceil(10, 3))
  
  // 有余数的情况
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-2, divide_with_ceil(-11, 5))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "assertion_functions_test" {
  // 测试断言函数
  // 布尔断言
  assert_true(true)
  assert_false(false)
  
  // 相等断言
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 复合断言
  let x = 5
  let y = 10
  assert_true(add(x, y) == 15)
  assert_false(add(x, y) == 20)
  assert_eq(50, multiply(x, y))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}