// 标准 MoonBit 测试用例 - 针对 azimuth 库的10个核心测试
// 使用标准的 MoonBit 测试语法编写

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(4, subtract(7, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-8, subtract(-2, 6))
  assert_eq(10, subtract(15, 5))
}

test "divide_with_ceil_boundary_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除零安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, áéíóú!", greet("áéíóú"))
  assert_eq_string("Hello, !", greet(""))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 8
  let y = 12
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (8 + 12) * 5 = 20 * 5 = 100
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)  // 8 * 12 + 5 = 96 + 5 = 101
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 25
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "practical_business_calculation" {
  // 测试实际业务计算场景
  let unit_price = 45
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(360, subtotal)
  assert_eq(375, total_before_discount)
  assert_eq(355, final_total)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  assert_eq(0, multiply(0, -1000))
  assert_eq(1000000, add(500000, 500000))
}

test "performance_boundary_values" {
  // 测试性能和边界值
  // 测试大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
}