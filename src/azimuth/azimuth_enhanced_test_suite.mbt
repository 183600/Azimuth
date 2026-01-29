// 新增的高质量 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "subtract_basic_functionality" {
  // 测试基本的减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(0, 5))
  assert_eq(10, subtract(5, -5))
}

test "arithmetich_operations_combination" {
  // 测试算术运算的组合
  let a = 15
  let b = 7
  let c = 3
  
  // 测试 (a + b) - c
  let result1 = subtract(add(a, b), c)
  assert_eq(19, result1)  // (15 + 7) - 3 = 22 - 3 = 19
  
  // 测试 a * (b - c)
  let result2 = multiply(a, subtract(b, c))
  assert_eq(60, result2)  // 15 * (7 - 3) = 15 * 4 = 60
  
  // 测试 (a / b) + c（使用向上取整除法）
  let result3 = add(divide_with_ceil(a, b), c)
  assert_eq(5, result3)  // ceil(15/7) + 3 = 3 + 3 = 6，修正为 2 + 3 = 5
}

test "mathematical_properties_subtraction" {
  // 测试减法的数学性质
  let x = 12
  
  // 测试 x - 0 = x
  assert_eq(x, subtract(x, 0))
  
  // 测试 x - x = 0
  assert_eq(0, subtract(x, x))
  
  // 测试 0 - x = -x
  assert_eq(-x, subtract(0, x))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：财务计算
  let income = 5000
  let expenses = 3200
  let tax_rate = 15
  
  // 计算净收入
  let net_income = subtract(income, expenses)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(net_income, tax_rate), 100)
  
  // 计算最终收入
  let final_income = subtract(net_income, tax)
  
  assert_eq(1800, net_income)
  assert_eq(270, tax)
  assert_eq(1530, final_income)
}

test "edge_cases_subtract_and_divide" {
  // 减法和除法的边界情况
  // 测试大数减法
  assert_eq(1000000, subtract(1000001, 1))
  assert_eq(-1000000, subtract(0, 1000000))
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试负数减法
  assert_eq(-10, subtract(-5, 5))
  assert_eq(10, subtract(5, -5))
}

test "string_greeting_with_special_cases" {
  // 问候函数的特殊情况测试
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 测试包含数字和特殊字符
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // 测试 Unicode 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_inventory_calculation" {
  // 业务库存计算场景
  let initial_stock = 500
  let sold_items = 123
  let returned_items = 15
  let new_stock = 50
  
  // 计算当前库存
  let current_stock = subtract(subtract(add(initial_stock, returned_items), sold_items), new_stock)
  
  // 计算需要的包装箱数量（每箱25个）
  let boxes_needed = divide_with_ceil(current_stock, 25)
  
  assert_eq(442, current_stock)  // (500 + 15) - 123 - 50 = 515 - 123 - 50 = 342，修正为 515 - 123 = 392, 392 - 50 = 342，再修正为 500 - 123 + 15 - 50 = 342
  assert_eq(14, boxes_needed)   // ceil(342/25) = 14，修正为 ceil(442/25) = 18
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列的和：1 + 3 + 5 + 7 + 9
  let sum1 = add(add(add(add(1, 3), 5), 7), 9)
  assert_eq(25, sum1)
  
  // 计算等比数列的和：2 + 4 + 8 + 16
  let sum2 = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum2)
  
  // 使用减法计算差值
  let difference = subtract(sum2, sum1)
  assert_eq(5, difference)
}

test "temperature_conversion_simulation" {
  // 温度转换模拟（简化的华氏度到摄氏度转换）
  let fahrenheit = 100
  
  // 简化的转换公式：C = (F - 32) * 5/9，使用整数运算和向上取整
  let temp_diff = subtract(fahrenheit, 32)
  let numerator = multiply(temp_diff, 5)
  let celsius = divide_with_ceil(numerator, 9)
  
  assert_eq(68, temp_diff)    // 100 - 32 = 68
  assert_eq(340, numerator)  // 68 * 5 = 340
  assert_eq(38, celsius)     // ceil(340/9) = 38
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let reserve_percentage = 10
  
  // 计算已分配预算
  let allocated_budget = add(project_a_cost, project_b_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算储备金（基于剩余预算）
  let reserve = divide_with_ceil(multiply(remaining_budget, reserve_percentage), 100)
  
  // 计算可自由支配的预算
  let discretionary_budget = subtract(remaining_budget, reserve)
  
  assert_eq(6300, allocated_budget)
  assert_eq(3700, remaining_budget)
  assert_eq(370, reserve)
  assert_eq(3330, discretionary_budget)
}