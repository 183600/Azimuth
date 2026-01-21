// Azimuth 标准测试套件 - 10个核心测试用例
// 测试 azimuth 库的主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(3, 4), multiply(2, 5))
  assert_eq(22, result1)  // 3*4 + 2*5 = 12 + 10 = 22
  
  let result2 = multiply(add(2, 3), 4)
  assert_eq(20, result2)  // (2 + 3) * 4 = 5 * 4 = 20
}

test "business_calculation_scenario" {
  // 业务计算场景：购物车总价
  let item_price = 25
  let quantity = 4
  let shipping = 10
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(110, total)  // 25*4 + 10 = 100 + 10 = 110
}

test "packaging_calculation_scenario" {
  // 包装计算场景：所需箱子数量
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let x = 10
  let y = 15
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
}