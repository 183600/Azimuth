// 标准 MoonBit 测试用例 - 针对 azimuth 库的高质量测试
// 使用标准的 MoonBit 测试语法编写

test "quality_addition_test" {
  // 全面测试加法函数
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(100, -100))
  assert_eq(2147483647, add(2147483646, 1))
}

test "quality_multiplication_test" {
  // 测试乘法函数的边界情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 2147483647))
  assert_eq(-2147483647, multiply(-1, 2147483647))
}

test "quality_subtraction_test" {
  // 测试减法函数的数学性质
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(20, subtract(-5, -25))
}

test "quality_division_ceil_test" {
  // 全面测试向上取整除法的各种场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "quality_greeting_test" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, María!", greet("María"))
}

test "quality_business_calculation_test" {
  // 模拟复杂的业务计算工作流
  let unit_price = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(1800, subtotal)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(144, tax_amount)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  assert_eq(1969, total_before_discount)
  
  // 应用折扣
  let final_total = add(total_before_discount, subtract(0, discount))
  assert_eq(1919, final_total)
}

test "quality_mathematical_properties_test" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "quality_packaging_test" {
  // 真实世界的包装优化问题
  let product_a_weight = 3
  let product_b_weight = 5
  let product_c_weight = 2
  let max_box_capacity = 25
  let safety_margin = 2
  
  // 计算总重量
  let total_weight = add(add(product_a_weight, product_b_weight), product_c_weight)
  assert_eq(10, total_weight)
  
  // 计算实际可用容量
  let effective_capacity = subtract(max_box_capacity, safety_margin)
  assert_eq(23, effective_capacity)
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_weight, effective_capacity)
  assert_eq(1, boxes_needed)
}

test "quality_string_processing_test" {
  // 测试字符串处理与问候函数的结合
  let first_name = "John"
  let last_name = "Doe"
  let title = "Dr."
  
  // 构建完整姓名
  let full_name = first_name + " " + last_name
  let formal_name = title + " " + full_name
  
  // 测试问候
  let casual_greeting = greet(full_name)
  let formal_greeting = greet(formal_name)
  
  assert_eq_string("Hello, John Doe!", casual_greeting)
  assert_eq_string("Hello, Dr. John Doe!", formal_greeting)
}

test "quality_performance_test" {
  // 模拟性能基准测试
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  assert_eq(3000, optimized_operations)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  assert_eq(450, overhead)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, subtract(0, overhead))
  assert_eq(2550, actual_performance)
}