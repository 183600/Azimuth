// 新增的标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-8, 12), @azimuth.add(12, -8))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "divide_with_ceil_real_world_applications" {
  // 测试实际应用场景：计算需要的容器数量
  let apples = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(apples, capacity_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 测试页面分页
  let total_items = 158
  let items_per_page = 25
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(7, pages_needed)
}

test "arithmetic_operations_identity_elements" {
  // 测试算术运算的单位元
  // 加法单位元：0
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 乘法单位元：1
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  
  // 乘法零元：0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算管道
  // 场景：计算折扣后的总价
  let original_price = 1000
  let discount_amount = 150
  let tax_rate = 2
  
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  let final_price = @azimuth.multiply(discounted_price, tax_rate)
  
  @azimuth.assert_eq(1700, final_price)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界值的运算
  @azimuth.assert_eq(max_int - 1, @azimuth.add(max_int - 2, 1))
  @azimuth.assert_eq(min_int + 1, @azimuth.add(min_int + 2, -1))
  
  // 测试边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含数字和特殊字符的组合
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test_case!", @azimuth.greet("test_case"))
  @azimuth.assert_eq_string("Hello, CAPSLOCK!", @azimuth.greet("CAPSLOCK"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, sum4)
  
  // 计算等比数列前n项积：2 * 4 * 8 = 64
  let product1 = @azimuth.multiply(2, 4)
  let product2 = @azimuth.multiply(product1, 8)
  @azimuth.assert_eq(64, product2)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极端值的组合运算
  let extreme_result = @azimuth.add(@azimuth.multiply(100000, 100), @azimuth.multiply(-50000, 200))
  @azimuth.assert_eq(0, extreme_result)
}