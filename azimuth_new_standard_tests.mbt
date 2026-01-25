// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_add_function_comprehensive" {
  // 全面测试加法函数
  @assert_eq(10, add(7, 3))
  @assert_eq(0, add(0, 0))
  @assert_eq(-5, add(-2, -3))
  @assert_eq(1000, add(500, 500))
  @assert_eq(0, add(-10, 10))
}

test "azimuth_multiply_function_comprehensive" {
  // 全面测试乘法函数
  @assert_eq(15, multiply(3, 5))
  @assert_eq(0, multiply(0, 100))
  @assert_eq(-6, multiply(-2, 3))
  @assert_eq(25, multiply(5, 5))
  @assert_eq(100, multiply(10, 10))
}

test "azimuth_greet_function_multilingual" {
  // 测试多语言问候功能
  @assert_eq_string("Hello, 世界!", greet("世界"))
  @assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  @assert_eq_string("Hello, !", greet(""))
  @assert_eq_string("Hello, 123!", greet("123"))
}

test "azimuth_divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  @assert_eq(4, divide_with_ceil(10, 3))
  @assert_eq(3, divide_with_ceil(9, 3))
  @assert_eq(-3, divide_with_ceil(-10, 3))
  @assert_eq(0, divide_with_ceil(5, 0))
  @assert_eq(1, divide_with_ceil(1, 100))
}

test "azimuth_mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  @assert_true(add(a, b) == add(b, a))
  
  // 乘法交换律
  @assert_true(multiply(a, b) == multiply(b, a))
  
  // 零元素性质
  @assert_eq(0, multiply(a, 0))
  @assert_eq(a, add(a, 0))
}

test "azimuth_business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  @assert_eq(270, subtotal)
  @assert_eq(285, total_before_discount)
  @assert_eq(265, final_total)
}

test "azimuth_packaging_calculation" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  @assert_eq(4, boxes_needed)
}

test "azimuth_complex_calculations" {
  // 复杂计算测试
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  @assert_eq(500, result1)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  @assert_eq(170, result2)
}

test "azimuth_negative_number_operations" {
  // 负数运算测试
  @assert_eq(-10, add(-3, -7))
  @assert_eq(-5, add(-8, 3))
  @assert_eq(15, multiply(-3, -5))
  @assert_eq(-15, multiply(3, -5))
  @assert_eq(-3, divide_with_ceil(-10, 3))
}

test "azimuth_edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  @assert_eq(0, multiply(0, 0))
  @assert_eq(0, multiply(5, 0))
  @assert_eq(0, divide_with_ceil(0, 5))
  @assert_eq(1, divide_with_ceil(1, 1))
  @assert_eq(0, divide_with_ceil(0, 1))
}