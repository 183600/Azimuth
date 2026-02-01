// Azimuth 额外的标准 MoonBit 测试用例
// 包含核心功能的全面测试，使用标准的 MoonBit 测试语法

test "azimuth_core_addition_comprehensive" {
  // 测试加法的各种情况
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-8, 13))
  assert_eq(1000000, add(500000, 500000))
}

test "azimuth_core_multiplication_comprehensive" {
  // 测试乘法的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
  assert_eq(10000, multiply(100, 100))
}

test "azimuth_core_subtraction_comprehensive" {
  // 测试减法的各种情况
  assert_eq(5, subtract(8, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(3, 8))
  assert_eq(10, subtract(-5, -15))
  assert_eq(1000, subtract(1500, 500))
}

test "azimuth_core_greet_functionality" {
  // 测试问候功能的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "azimuth_core_divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(34, divide_with_ceil(100, 3))
}

test "azimuth_core_divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "azimuth_core_mathematical_properties" {
  // 测试数学性质
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

test "azimuth_core_combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = subtract(multiply(10, 5), divide_with_ceil(25, 3))
  assert_eq(42, result3)  // 10*5 - ceil(25/3) = 50 - 9 = 41
}

test "azimuth_core_practical_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 4
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(135, total)  // 25*3 + 15*4 = 75 + 60 = 135
  
  // 计算需要的包装箱数量
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "azimuth_core_boundary_values" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}