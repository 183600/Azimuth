// 新增的高质量 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法的全面测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))   // 除数为0，返回0
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -3.333... -> -3
}

test "greet_function_variations" {
  // 问候函数的各种变体测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))  // 数字字符串
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))  // 中文字符
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 8
  let y = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律：x * (y + y) = x * y + x * y
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, y)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, y)))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax_amount)
  @azimuth.assert_eq(228, total)
}

test "edge_case_boundary_values" {
  // 边界值测试
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  
  // 负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let a = 5
  let b = 3
  let c = 2
  let d = 7
  
  // 计算：(a + b) * c - d / b
  let step1 = @azimuth.add(a, b)
  let step2 = @azimuth.multiply(step1, c)
  let step3 = @azimuth.divide_with_ceil(d, b)
  let result = @azimuth.add(step2, -step3)
  
  @azimuth.assert_eq(13, result)  // (5 + 3) * 2 - ceil(7/3) = 8 * 2 - 3 = 16 - 3 = 13
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_items = 47
  let container_capacity = 8
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  
  let total_cost_per_container = 15
  let total_cost = @azimuth.multiply(containers_needed, total_cost_per_container)
  
  @azimuth.assert_eq(6, containers_needed)  // ceil(47/8) = 6
  @azimuth.assert_eq(90, total_cost)  // 6 * 15 = 90
}

test "string_processing_with_numbers" {
  // 数字和字符串混合处理
  let number_as_string = "123"
  let mixed_string = "User123"
  let special_chars = "!@#$%"
  
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet(number_as_string))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(mixed_string))
  @azimuth.assert_eq_string("Hello, !@#$%!", @azimuth.greet(special_chars))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}