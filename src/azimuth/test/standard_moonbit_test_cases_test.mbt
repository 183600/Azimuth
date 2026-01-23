// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

// 导入 azimuth 库中的函数
test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  assert_eq(add(7, 13), add(13, 7))
  // 乘法交换律
  assert_eq(multiply(6, 8), multiply(8, 6))
  // 加法单位元
  assert_eq(5, add(5, 0))
  // 乘法单位元
  assert_eq(7, multiply(7, 1))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = add(divide_with_ceil(10, 3), multiply(2, 2))
  assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "practical_application" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(103, total)  // 29*2 + 15*3 = 58 + 45 = 103
}

test "packaging_calculation" {
  // 测试包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}