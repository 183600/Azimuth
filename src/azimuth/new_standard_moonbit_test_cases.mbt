// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试、边界情况测试和实际应用场景

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiplication_boundary_cases" {
  // 测试乘法边界情况
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(1, multiply(1, 100))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(100, multiply(-10, -10))
}

test "subtraction_comprehensive" {
  // 测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_various_scenarios" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除零处理
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, !", greet(""))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 500
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 170
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "real_world_business_calculation" {
  // 测试实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
  assert_eq_string("Hello, 123456!", greet("123456"))  // 纯数字
}