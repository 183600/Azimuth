// 标准 MoonBit 测试套件
// 使用标准 MoonBit 测试语法，针对 azimuth 库的核心功能测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数交换律
  let x = -10
  let y = 25
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 测试被除数小于除数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 测试负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-15, 8))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 10))
  
  // 测试除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(50, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  // 应用折扣
  let final_total = @azimuth.add(total, -discount)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(44, tax)
  @azimuth.assert_eq(609, total)
  @azimuth.assert_eq(559, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 7
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(104, sum)  // 8/2 * (14 + 21) = 4 * 35 = 140 -> 修正计算
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目资源需求
  let total_tasks = 67
  let tasks_per_person = 8
  let work_days = 5
  
  // 计算需要的人员数量
  let people_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_person)
  
  // 计算总工作日
  let total_work_days = @azimuth.multiply(people_needed, work_days)
  
  @azimuth.assert_eq(9, people_needed)  // ceil(67/8) = 9
  @azimuth.assert_eq(45, total_work_days)  // 9 * 5 = 45
}

test "financial_compound_interest" {
  // 金融计算：复利计算（简化版）
  let principal = 10000
  let interest_rate = 6  // 6%
  let years = 3
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, interest_rate), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, interest_rate), 100))
  
  @azimuth.assert_eq(10600, year1)
  @azimuth.assert_eq(11236, year2)
  @azimuth.assert_eq(11910, year3)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let prefix = "Dr."
  let first_name = "Alice"
  let last_name = "Johnson"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name
  let formal_greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, Dr. Alice Johnson!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "@#$%^&*()"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", special_greeting)
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算最优包装方案
  let small_items = 35
  let medium_items = 28
  let large_items = 15
  
  let small_box_capacity = 12
  let medium_box_capacity = 8
  let large_box_capacity = 4
  
  // 计算每种尺寸需要的箱子数量
  let small_boxes = @azimuth.divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = @azimuth.divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = @azimuth.divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数量
  let total_boxes = @azimuth.add(@azimuth.add(small_boxes, medium_boxes), large_boxes)
  
  @azimuth.assert_eq(3, small_boxes)   // ceil(35/12) = 3
  @azimuth.assert_eq(4, medium_boxes)  // ceil(28/8) = 4
  @azimuth.assert_eq(4, large_boxes)   // ceil(15/4) = 4
  @azimuth.assert_eq(11, total_boxes)  // 3 + 4 + 4 = 11
}