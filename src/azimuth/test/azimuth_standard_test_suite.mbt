// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能和实际应用场景

test "add_function_commutativity" {
  // 测试加法交换律
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_distributive" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3)) // 3.666... -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0)) // 除数为0的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let _years = 3
  
  // 简化的复利计算
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(principal, year1_interest), annual_rate), 100)
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(@azimuth.add(principal, year1_interest), year2_interest), annual_rate), 100)
  
  let total_amount = @azimuth.add(@azimuth.add(@azimuth.add(principal, year1_interest), year2_interest), year3_interest)
  @azimuth.assert_eq(11576, total_amount)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 200
  let days = 7
  
  // 计算一周后的库存
  let total_sold = @azimuth.multiply(daily_sales, days)
  let stock_before_restock = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_restock, restock_amount)
  
  @azimuth.assert_eq(525, final_stock)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 50000
  let team_members = 8
  let project_duration_days = 30
  
  // 计算每人每天的预算
  let daily_budget = @azimuth.divide_with_ceil(total_budget, project_duration_days)
  let per_person_daily = @azimuth.divide_with_ceil(daily_budget, team_members)
  
  // 验证总预算分配
  let allocated_budget = @azimuth.multiply(@azimuth.multiply(per_person_daily, team_members), project_duration_days)
  @azimuth.assert_eq(50480, allocated_budget)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let terms_count = 10
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(terms_count, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(terms_count - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(185, sum)
}

test "shipping_cost_calculator" {
  // 运费计算器测试
  let base_rate = 10
  let weight_kg = 15
  let distance_km = 250
  let weight_threshold = 10
  
  // 计算基础运费
  let weight_surcharge = if weight_kg > weight_threshold {
    @azimuth.multiply(weight_kg - weight_threshold, 2)
  } else {
    0
  }
  
  let distance_cost = @azimuth.divide_with_ceil(@azimuth.multiply(distance_km, base_rate), 100)
  let total_shipping = @azimuth.add(@azimuth.add(base_rate, weight_surcharge), distance_cost)
  
  @azimuth.assert_eq(50, total_shipping)
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let input_data = 1000
  let _processing_steps = 4
  let error_rate = 2  // 2%
  
  // 模拟数据处理步骤
  let step1_result = @azimuth.add(input_data, @azimuth.divide_with_ceil(@azimuth.multiply(input_data, 10), 100))
  let step2_result = @azimuth.add(step1_result, @azimuth.divide_with_ceil(@azimuth.multiply(step1_result, 15), 100))
  let step3_result = @azimuth.add(step2_result, @azimuth.divide_with_ceil(@azimuth.multiply(step2_result, 20), 100))
  let step4_result = @azimuth.add(step3_result, @azimuth.divide_with_ceil(@azimuth.multiply(step3_result, 25), 100))
  
  // 计算错误损失
  let error_loss = @azimuth.divide_with_ceil(@azimuth.multiply(step4_result, error_rate), 100)
  let final_result = @azimuth.add(step4_result, -error_loss)
  
  @azimuth.assert_eq(1983, final_result)
}