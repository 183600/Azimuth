// MoonBit 标准测试用例
// 覆盖 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(-5, 15))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试边界情况
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试特殊值
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法单位元
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 乘法单位元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  
  // 乘法零元
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的乘法
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(42, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试0的加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "complex_calculations" {
  // 测试复杂计算
  // (2 + 3) * 4 = 20
  let temp = add(2, 3)
  let result = multiply(temp, 4)
  assert_eq(20, result)
  
  // 5 * (6 + 7) = 65
  let temp2 = add(6, 7)
  let result2 = multiply(5, temp2)
  assert_eq(65, result2)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(10, 10)
  assert_eq_string("test", "test")
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格总和
  let item1 = 199
  let item2 = 299
  let item3 = 399
  let total = add(add(item1, item2), item3)
  assert_eq(897, total)
  
  // 折扣计算：原价8折
  let original_price = 1000
  let discounted_price = multiply(original_price, 8) / 10
  assert_eq(800, discounted_price)
}