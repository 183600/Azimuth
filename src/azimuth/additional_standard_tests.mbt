// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(2, divide_with_ceil(7, -3))
}

test "divide_with_ceil_zero" {
  // 测试包含0的向上取整除法
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_calculations" {
  // 测试复杂计算场景：计算平均值的向上取整
  let numbers_sum = add(add(15, 25), 35)  // 75
  let count = 4
  let average_ceil = divide_with_ceil(numbers_sum, count)
  assert_eq(19, average_ceil)  // 75/4 = 18.75，向上取整为19
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "add_chained_operations" {
  // 测试链式加法运算
  let result = add(add(10, 20), add(30, 40))
  assert_eq(100, result)
  
  let result2 = add(add(add(1, 2), 3), 4)
  assert_eq(10, result2)
}

test "multiply_power_calculation" {
  // 测试幂次计算：2^4 = 16
  let power_of_2 = multiply(multiply(2, 2), multiply(2, 2))
  assert_eq(16, power_of_2)
  
  // 测试 3^3 = 27
  let power_of_3 = multiply(multiply(3, 3), 3)
  assert_eq(27, power_of_3)
}

test "mixed_operations" {
  // 测试混合运算：(a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(32, result)
}

test "boundary_value_division" {
  // 测试边界值的除法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(1, divide_with_ceil(min_val, min_val))
  assert_eq(0, divide_with_ceil(1, max_val))
}