// 标准 Azimuth 增强测试用例
// 包含8个高质量测试用例，覆盖核心功能

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let initial = 20
  let step1 = @azimuth.subtract(initial, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)          // 30
  let step3 = @azimuth.divide_with_ceil(step2, 7)  // 5
  let step4 = @azimuth.add(step3, 3)               // 8
  let result = @azimuth.subtract(step4, 2)         // 6
  
  @azimuth.assert_eq(6, result)
}

test "financial_calculation_scenario" {
  // 金融计算场景：投资回报计算
  let initial_investment = 10000
  let first_year_return = 1500
  let second_year_return = 1800
  let third_year_return = 2200
  
  let total_returns = @azimuth.add(@azimuth.add(first_year_return, second_year_return), third_year_return)
  let final_amount = @azimuth.add(initial_investment, total_returns)
  let roi_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(total_returns, 100), initial_investment)
  
  @azimuth.assert_eq(15500, final_amount)
  @azimuth.assert_eq(55, roi_percentage)  // 55% 总回报率
}

test "inventory_management_optimization" {
  // 库存管理优化测试
  let current_stock = 156
  let daily_sales = 12
  let safety_stock = 20
  let reorder_point = @azimuth.multiply(daily_sales, 7)  // 一周销量
  
  // 计算是否需要补货
  let should_reorder = current_stock <= reorder_point
  let days_of_supply = @azimuth.divide_with_ceil(current_stock, daily_sales)
  let minimum_reorder_quantity = @azimuth.subtract(reorder_point, current_stock)
  
  @azimuth.assert_false(should_reorder)  // 156 > 84，不需要补货
  @azimuth.assert_eq(13, days_of_supply)  // ceil(156/12) = 13天供应
  @azimuth.assert_true(minimum_reorder_quantity < 0)  // 库存充足
}

test "mathematical_identity_verification" {
  // 数学恒等式验证
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  
  // 减法恒等式
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(x, @azimuth.subtract(@azimuth.add(x, y), y))
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 250
  let team_a_size = 8
  let team_b_size = 12
  let team_c_size = 5
  
  // 计算基础分配
  let base_allocation_a = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 40), 100)  // 40%
  let base_allocation_b = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 35), 100)  // 35%
  let base_allocation_c = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 25), 100)  // 25%
  
  // 计算人均资源
  let per_person_a = @azimuth.divide_with_ceil(base_allocation_a, team_a_size)
  let per_person_b = @azimuth.divide_with_ceil(base_allocation_b, team_b_size)
  let per_person_c = @azimuth.divide_with_ceil(base_allocation_c, team_c_size)
  
  @azimuth.assert_eq(100, base_allocation_a)  // ceil(250*40/100) = 100
  @azimuth.assert_eq(88, base_allocation_b)   // ceil(250*35/100) = 88
  @azimuth.assert_eq(63, base_allocation_c)   // ceil(250*25/100) = 63
  
  @azimuth.assert_eq(13, per_person_a)  // ceil(100/8) = 13
  @azimuth.assert_eq(8, per_person_b)   // ceil(88/12) = 8
  @azimuth.assert_eq(13, per_person_c)  // ceil(63/5) = 13
}

test "string_processing_with_numbers" {
  // 字符串与数字处理结合测试
  let number1 = 42
  let number2 = 17
  let sum = @azimuth.add(number1, number2)
  let product = @azimuth.multiply(number1, number2)
  let difference = @azimuth.subtract(number1, number2)
  
  // 测试数字转字符串的问候功能
  @azimuth.assert_eq_string("Hello, 42!", @azimuth.greet("42"))
  @azimuth.assert_eq_string("Hello, 17!", @azimuth.greet("17"))
  @azimuth.assert_eq_string("Hello, 59!", @azimuth.greet("59"))  // 42 + 17
  @azimuth.assert_eq_string("Hello, 25!", @azimuth.greet("25"))  // 42 - 17
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况综合测试
  // 除零错误的多种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  let max_safe = 10000
  let min_safe = -10000
  
  @azimuth.assert_eq(20000, @azimuth.add(max_safe, max_safe))
  @azimuth.assert_eq(-20000, @azimuth.add(min_safe, min_safe))
  @azimuth.assert_eq(0, @azimuth.add(max_safe, min_safe))
  
  @azimuth.assert_eq(100000000, @azimuth.multiply(max_safe, max_safe))
  @azimuth.assert_eq(100000000, @azimuth.multiply(min_safe, min_safe))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(max_safe, min_safe))
  
  // 减法极值
  @azimuth.assert_eq(20000, @azimuth.subtract(max_safe, min_safe))
  @azimuth.assert_eq(-20000, @azimuth.subtract(min_safe, max_safe))
  @azimuth.assert_eq(0, @azimuth.subtract(max_safe, max_safe))
}