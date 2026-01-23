// 标准 Azimuth 综合测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(5, -5))
  
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(0, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greeting_function_various_inputs" {
  // 问候函数多种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律验证
  let c = 5
  let distrib_left = multiply(a, add(b, c))
  let distrib_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distrib_left, distrib_right)
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax)
  assert_eq(534, final_total)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值操作
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 除法边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(9999, 100))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 时间计算场景
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(5, days_needed)
  
  // 库存管理场景
  let initial_stock = 100
  let daily_sales = 15
  let days = 5
  let remaining_stock = add(initial_stock, -multiply(daily_sales, days))
  assert_eq(25, remaining_stock)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数运算健壮性
  assert_eq(-15, add(-8, -7))
  assert_eq(20, multiply(-4, -5))
  assert_eq(-20, multiply(4, -5))
}

test "string_processing_integration" {
  // 字符串处理集成测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张 三!", greeting)
  
  // 测试特殊字符组合
  let company = "TechCorp"
  let department = "研发部"
  let team_id = company + " " + department
  let team_greeting = greet(team_id)
  
  assert_eq_string("Hello, TechCorp 研发部!", team_greeting)
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 批量计算模拟
  let base_value = 100
  let iterations = 50
  let multiplier = 3
  
  // 模拟批量处理
  let batch_result = multiply(base_value, iterations)
  let enhanced_result = add(batch_result, multiply(multiplier, divide_with_ceil(iterations, 10)))
  
  assert_eq(5000, batch_result)
  assert_eq(5150, enhanced_result)
  
  // 资源分配计算
  let total_budget = 10000
  let project_cost = multiply(1200, 4)
  let remaining = add(total_budget, -project_cost)
  let small_projects = divide_with_ceil(remaining, 800)
  
  assert_eq(4800, project_cost)
  assert_eq(5200, remaining)
  assert_eq(7, small_projects)
}