// 标准 MoonBit 测试用例样本
// 为 azimuth 库提供高质量的标准测试

test "basic_arithmetic_sequence" {
  // 测试基本算术序列
  let start = 5
  let difference = 3
  
  // 计算等差数列：5, 8, 11, 14
  let term1 = start
  let term2 = @azimuth.add(term1, difference)
  let term3 = @azimuth.add(term2, difference)
  let term4 = @azimuth.add(term3, difference)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(38, sum)  // 5 + 8 + 11 + 14 = 38
}

test "string_greeting_with_emoji" {
  // 测试包含表情符号的问候
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "division_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 10))
  
  // 负数情况
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 2))
  
  // 零除数处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
}

test "business_invoice_calculation" {
  // 商业发票计算场景
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let discount = 50
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal, tax)
  // 应用折扣
  let final_total = @azimuth.add(total_before_discount, @azimuth.multiply(-1, discount))
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(44, tax)
  @azimuth.assert_eq(584, total_before_discount)
  @azimuth.assert_eq(534, final_total)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + b) = a * b + a * b
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, b))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, b))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "resource_allocation_optimization" {
  // 资源分配优化场景
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  // 计算已分配资源
  let allocated = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  // 计算剩余资源
  let remaining = @azimuth.add(total_resources, @azimuth.multiply(-1, allocated))
  
  // 计算可以支持的小项目数量（每个小项目需要3个资源）
  let small_projects = @azimuth.divide_with_ceil(remaining, 3)
  
  @azimuth.assert_eq(85, allocated)
  @azimuth.assert_eq(15, remaining)
  @azimuth.assert_eq(5, small_projects)
}

test "time_management_scenario" {
  // 时间管理场景
  let daily_working_hours = 8
  let total_tasks = 67
  
  // 计算需要的工作日
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, daily_working_hours)
  
  // 计算总工作时间（小时）
  let total_hours = @azimuth.multiply(workdays_needed, daily_working_hours)
  
  // 计算总工作时间（分钟）
  let total_minutes = @azimuth.multiply(total_hours, 60)
  
  @azimuth.assert_eq(9, workdays_needed)
  @azimuth.assert_eq(72, total_hours)
  @azimuth.assert_eq(4320, total_minutes)
}

test "error_handling_boundary_cases" {
  // 错误处理边界情况
  // 测试极端数值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
}

test "complex_string_combinations" {
  // 复杂字符串组合测试
  let name = "MoonBit"
  let suffix = "2024"
  
  // 构建复合字符串
  let full_name = name + " " + suffix
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, MoonBit 2024!", greeting)
  
  // 测试特殊字符组合
  let special_chars = @azimuth.greet("@#$%&*")
  @azimuth.assert_eq_string("Hello, @#$%&*!", special_chars)
}

test "scientific_calculation_simulation" {
  // 科学计算模拟
  let base_value = 42
  let multiplier = 7
  let divisor = 3
  let adjustment = 15
  
  // 模拟科学计算公式
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let final_result = @azimuth.add(step2, adjustment)
  
  @azimuth.assert_eq(294, step1)
  @azimuth.assert_eq(98, step2)
  @azimuth.assert_eq(113, final_result)
}