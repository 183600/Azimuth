// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_standard_cases" {
  // 标准加法测试用例
  assert_eq(10, add(6, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(100, add(50, 50))
}

test "multiply_function_standard_cases" {
  // 标准乘法测试用例
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(7, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(64, multiply(8, 8))
}

test "divide_with_ceil_standard_cases" {
  // 标准向上取整除法测试用例
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_standard_cases" {
  // 标准问候函数测试用例
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_operations_combination" {
  // 数学运算组合测试
  let x = 5
  let y = 3
  let z = 2
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(16, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(17, result2)
}

test "negative_number_operations_standard" {
  // 负数运算标准测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_handling_standard" {
  // 零值处理标准测试
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "large_number_operations_standard" {
  // 大数运算标准测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(0, add(1000, -1000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1000, divide_with_ceil(2000, 2))
}

test "precision_division_standard" {
  // 精度除法标准测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(7, divide_with_ceil(13, 2))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(999, 10))
}

test "real_world_calculation_standard" {
  // 实际应用计算标准测试
  // 计算购物总价：商品价格 × 数量 + 运费
  let price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}