// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含基本功能、边界情况和实际应用场景的测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
}

test "multiply_function_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 8))
  assert_eq(1, multiply(1, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
}

test "subtract_function_basic_operations" {
  // 测试减法基本操作
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(-5, -20))
  assert_eq(10, subtract(0, -10))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))    // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.33 -> 4
  assert_eq(0, divide_with_ceil(5, 0))     // 除零安全处理
  assert_eq(1, divide_with_ceil(1, 100))   // 1/100 = 0.01 -> 1
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let discount = 20
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(216, total_before_discount)
  assert_eq(196, final_total)
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (7 + 9) = 4 * 7 + 4 * 9 = 64
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数
  assert_eq(1, divide_with_ceil(-1, -1))       // 负数相等
  assert_eq(2147483647, multiply(46340, 46340)) // 接近平方根的最大乘积
  assert_eq_string("Hello, A!", greet("A"))     // 单字符
}

test "real_world_packaging_scenario" {
  // 实际包装场景计算
  let total_items = 47
  let items_per_box = 12
  let cost_per_box = 15
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(4, boxes_needed)    // ceil(47/12) = 4
  assert_eq(60, total_cost)     // 4 * 15 = 60
}

test "combined_operations_validation" {
  // 组合操作验证
  let x = 12
  let y = 8
  let z = 5
  
  // 测试复杂表达式: (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(99, result)  // (12 + 8) * 5 - ceil(12/8) = 20 * 5 - 2 = 100 - 2 = 98
  
  // 测试嵌套计算
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 3)
  assert_eq(52, nested)  // ceil((12 + 8*5) * 2 / 3) = ceil(52 * 2 / 3) = ceil(104/3) = 35
}