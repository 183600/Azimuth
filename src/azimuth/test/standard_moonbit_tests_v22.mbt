// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个高质量的测试用例，使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  
  // 测试负数情况
  @azimuth.assert_eq(@azimuth.add(-8, 12), @azimuth.add(12, -8))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 3
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 零除数处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x + y) * z - x
  let expression1 = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -x)
  @azimuth.assert_eq(88, expression1)  // (12 + 8) * 5 - 12 = 20 * 5 - 12 = 100 - 12 = 88
  
  // 测试 x * y + z * (x - y)
  let expression2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(z, @azimuth.add(x, -y)))
  @azimuth.assert_eq(116, expression2)  // 12 * 8 + 5 * (12 - 8) = 96 + 5 * 4 = 96 + 20 = 116
}

test "real_world_business_calculation" {
  // 实际业务计算：订单总价计算
  let product_price = 89
  let quantity = 6
  let tax_rate = 8   // 8%
  let shipping = 15
  let discount = 25
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(534, subtotal)            // 89 * 6 = 534
  @azimuth.assert_eq(43, tax_amount)           // ceil(534 * 8 / 100) = ceil(42.72) = 43
  @azimuth.assert_eq(592, total_before_discount) // 534 + 43 + 15 = 592
  @azimuth.assert_eq(567, final_total)         // 592 - 25 = 567
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 7
  let common_difference = 4
  let number_of_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(168, sum)  // 8/2 * (14 + 28) = 4 * 42 = 168
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算最少需要的包装箱数量
  let box_capacity = 40
  
  // 计算每种尺寸需要的箱子数量
  let boxes_15 = @azimuth.divide_with_ceil(15, box_capacity)
  let boxes_22 = @azimuth.divide_with_ceil(22, box_capacity)
  let boxes_18 = @azimuth.divide_with_ceil(18, box_capacity)
  let boxes_31 = @azimuth.divide_with_ceil(31, box_capacity)
  let boxes_12 = @azimuth.divide_with_ceil(12, box_capacity)
  let boxes_25 = @azimuth.divide_with_ceil(25, box_capacity)
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(boxes_15, boxes_22), boxes_18), boxes_31), boxes_12), boxes_25)
  
  @azimuth.assert_eq(1, boxes_15)  // ceil(15/40) = 1
  @azimuth.assert_eq(1, boxes_22)  // ceil(22/40) = 1
  @azimuth.assert_eq(1, boxes_18)  // ceil(18/40) = 1
  @azimuth.assert_eq(1, boxes_31)  // ceil(31/40) = 1
  @azimuth.assert_eq(1, boxes_12)  // ceil(12/40) = 1
  @azimuth.assert_eq(1, boxes_25)  // ceil(25/40) = 1
  @azimuth.assert_eq(6, total_boxes)
}

test "time_and_resource_management" {
  // 时间和资源管理：项目排期计算
  let total_tasks = 67
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_work_hours = @azimuth.multiply(work_days_needed, 8)
  
  @azimuth.assert_eq(9, work_days_needed)   // ceil(67/8) = 9
  @azimuth.assert_eq(2, weeks_needed)       // ceil(9/5) = 2
  @azimuth.assert_eq(72, total_work_hours)  // 9 * 8 = 72
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和特殊情况测试
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -456))
  @azimuth.assert_eq(123, @azimuth.add(123, 0))
  @azimuth.assert_eq(-456, @azimuth.add(-456, 0))
  
  // 测试单位元运算
  @azimuth.assert_eq(789, @azimuth.multiply(789, 1))
  @azimuth.assert_eq(789, @azimuth.multiply(1, 789))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  
  // 测试相等数运算
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(20, @azimuth.add(10, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(25, 25))
  
  // 测试相反数运算
  @azimuth.assert_eq(-100, @azimuth.add(100, -200))
  @azimuth.assert_eq(0, @azimuth.add(150, -150))
  @azimuth.assert_eq(-200, @azimuth.multiply(-20, 10))
}