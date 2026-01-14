// 用户自定义的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_mixed_signs" {
  // 测试不同符号的加法
  assert_eq(0, add(5, -5))
  assert_eq(3, add(10, -7))
  assert_eq(-3, add(-10, 7))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
}

test "multiply_function_negative" {
  // 测试负数乘法
  assert_eq(12, multiply(-3, -4))
  assert_eq(-20, multiply(4, -5))
  assert_eq(25, multiply(-5, -5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)  // 5 + 3 = 8
  let product = multiply(sum_ab, c)  // 8 * 4 = 32
  let result = add(product, -d)  // 32 - 10 = 22
  
  assert_eq(22, result)
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  assert_eq(697, cart_total)
}

test "assert_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}