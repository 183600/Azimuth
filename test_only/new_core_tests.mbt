// 新的 MoonBit 核心测试用例
// 使用 test_only 模块的函数进行测试
// 包含 8 个核心功能测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(10, add(5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  assert_eq(6, multiply(2, 3))
  assert_eq(25, multiply(5, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "identity_properties" {
  // 测试单位元性质
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "commutative_properties" {
  // 测试交换律
  assert_eq(add(3, 5), add(5, 3))
  assert_eq(add(-2, 8), add(8, -2))
  assert_eq(multiply(3, 5), multiply(5, 3))
  assert_eq(multiply(-2, 8), multiply(8, -2))
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(max_val, min_val + 1))  // 避免溢出
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  assert_eq(3000000, add(large1, large2))
  assert_eq(2000000000000, multiply(large1, large2))
  
  let large_negative = -1000000
  assert_eq(0, add(large1, large_negative))
  assert_eq(-1000000000000, multiply(large1, large_negative))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !", greet(""))
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_true(3 <= 3)
  assert_false(5 < 3)
  assert_false(3 != 3)
}