// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "standard_add_basic_functionality" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-1, azimuth::add(-2, 1))
  azimuth::assert_eq(100, azimuth::add(50, 50))
}

test "standard_multiply_basic_functionality" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-4, azimuth::multiply(-2, 2))
  azimuth::assert_eq(25, azimuth::multiply(5, 5))
}

test "standard_greet_basic_functionality" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "standard_divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(3, 2))
}

test "standard_divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-4, 2))
}

test "standard_zero_operations" {
  // 测试与零相关的运算
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
  azimuth::assert_eq(5, azimuth::add(5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
}

test "standard_mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 15
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(x, 0))
  azimuth::assert_eq(x, azimuth::add(x, 0))
}

test "standard_combined_operations" {
  // 测试组合运算
  let result1 = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth::multiply(azimuth::add(5, 3), 2)
  azimuth::assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "standard_practical_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = azimuth::add(azimuth::multiply(item1_price, item1_qty), azimuth::multiply(item2_price, item2_qty))
  azimuth::assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  azimuth::assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}