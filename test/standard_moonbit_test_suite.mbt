// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(20, multiply(4, 5))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "boundary_values" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculations" {
  // 复杂计算测试
  let result = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, result)  // 3*4 + 5*6 = 12 + 30 = 42
  
  let complex = divide_with_ceil(multiply(add(10, 5), 3), 4)
  assert_eq(12, complex)  // ceil((10+5)*3/4) = ceil(45/4) = 12
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 计算购物总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let total = add(multiply(unit_price, quantity), shipping)
  assert_eq(215, total)  // 25*8 + 15 = 200 + 15 = 215
  
  // 计算包装箱数量
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "string_processing" {
  // 字符串处理测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "division_precision" {
  // 除法精度测试
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(7, divide_with_ceil(13, 2))  // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(100, divide_with_ceil(999, 10))  // 99.9 -> 100
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(500000, divide_with_ceil(1000000, 2))
}

test "algorithmic_scenarios" {
  // 算法场景测试
  // 模拟斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 模拟等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 6
  let sum = multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))) / 2
  assert_eq(78, sum)  // 6 * (10 + 15) / 2 = 6 * 25 / 2 = 75
}