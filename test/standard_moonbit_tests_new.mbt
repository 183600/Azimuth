// 标准 MoonBit 测试用例 - 新增的10个测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  let test_values = [0, 1, -1, 10, -10, 100, -100]
  for value in test_values {
    assert_eq(value, multiply(value, 1))
  }
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 2))  // 0.5 -> 1
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let x = 8
  let y = 12
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (8 + 12) * 5 = 20 * 5 = 100
  
  // 测试 x * y + x * z
  let result2 = add(multiply(x, y), multiply(x, z))
  assert_eq(136, result2)  // 8 * 12 + 8 * 5 = 96 + 40 = 136
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的全面情况
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  let shipping_cost = 10
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total_before_shipping = add(subtotal1, subtotal2)
  let final_total = add(total_before_shipping, shipping_cost)
  
  assert_eq(75, subtotal1)      // 25 * 3 = 75
  assert_eq(30, subtotal2)      // 15 * 2 = 30
  assert_eq(105, total_before_shipping)  // 75 + 30 = 105
  assert_eq(115, final_total)   // 105 + 10 = 115
}

test "packaging_optimization" {
  // 测试包装优化计算
  let total_products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let empty_spaces = add(total_capacity, -total_products)
  
  assert_eq(4, boxes_needed)    // ceil(47/12) = 4
  assert_eq(48, total_capacity) // 4 * 12 = 48
  assert_eq(1, empty_spaces)    // 48 - 47 = 1
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 35
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let minutes_needed = multiply(hours_needed, 60)
  
  assert_eq(9, hours_needed)    // ceil(35/4) = 9
  assert_eq(540, minutes_needed) // 9 * 60 = 540
}