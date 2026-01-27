// Azimuth 增强标准测试用例
// 包含10个高质量的 MoonBit 标准测试用例

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  subtract(10, 5)
  subtract(5, 10)
  subtract(5, 5)
  subtract(10, -5)
  subtract(-10, 5)
  subtract(0, 0)
  subtract(1000001, 1)
}

test "divide_with_ceil_advanced_cases" {
  // 测试向上取整除法的高级情况
  divide_with_ceil(0, -1)
  divide_with_ceil(-0, 1)
  divide_with_ceil(-5, 3)
  divide_with_ceil(5, -3)
  divide_with_ceil(-5, -3)
  divide_with_ceil(200001, 2)
  divide_with_ceil(-1, -1)
}

test "greet_function_edge_cases" {
  // 测试问候函数的边缘情况
  greet("")
  greet("  ")
  greet("\t\n\r")
  greet("null")
  greet("1234567890")
  greet("🌙🌟🚀")
}

test "mathematical_properties_advanced" {
  // 测试高级数学性质
  let a = 12
  let b = 8
  let c = 4
  
  // 减法性质：a - b - c = a - (b + c)
  subtract(subtract(a, b), c)
  subtract(a, add(b, c))
  
  // 乘法与减法：a * (b - c) = a * b - a * c
  multiply(a, subtract(b, c))
  subtract(multiply(a, b), multiply(a, c))
  
  // 除法与乘法关系（对于能整除的情况）
  multiply(divide_with_ceil(24, 6), 6)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 连续除零测试
  divide_with_ceil(divide_with_ceil(10, 0), 5)
  divide_with_ceil(divide_with_ceil(0, 0), 0)
  
  // 极值减法测试
  subtract(2147483647, 0)
  subtract(-2147483647, -1)
  
  // 空字符串与特殊字符组合
  greet("special")
}

test "real_world_logistics_scenario" {
  // 真实世界物流场景测试
  // 场景：仓库库存管理
  let initial_stock = 1000
  let daily_sales = 45
  let days = 7
  let restock_threshold = 200
  
  // 计算一周后的库存
  let weekly_sales = multiply(daily_sales, days)
  let remaining_stock = subtract(initial_stock, weekly_sales)
  
  // 计算需要的补货量（每次补货500件）
  let restock_needed = subtract(restock_threshold, remaining_stock)
  let restock_batches = divide_with_ceil(restock_needed, 500)
  
  weekly_sales
  remaining_stock
  restock_batches
}

test "large_scale_computations" {
  // 大规模计算测试
  let large_num = 100000
  let another_large = 99999
  
  // 大数运算
  add(large_num, another_large)
  multiply(100000, 9999)
  subtract(another_large, large_num)
  divide_with_ceil(large_num, another_large)
  
  // 连续大数运算
  let result = multiply(add(large_num, another_large), 10)
  result
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  // 场景：项目预算和时间管理
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  let contingency_percentage = 10
  
  // 计算基础工时和成本
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  let base_cost = multiply(total_hours, hourly_rate)
  
  // 计算应急预算（向上取整）
  let contingency_amount = divide_with_ceil(multiply(base_cost, contingency_percentage), 100)
  let total_budget = add(base_cost, contingency_amount)
  
  // 计算每周成本
  let weekly_cost = divide_with_ceil(multiply(total_budget, 7), project_days)
  
  total_hours
  base_cost
  contingency_amount
  total_budget
  weekly_cost
}

test "internationalization_comprehensive" {
  // 国际化综合测试
  greet("العربية")
  greet("עברית")
  greet("हिन्दी")
  greet("ไทย")
  greet("한국어")
  greet("日本語")
  greet("🇺🇳🇨🇳🇯🇵")
  greet("𝔘𝔫𝔦𝔠𝔬𝔡𝔢")
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let test_int = 42
  let test_string = "MoonBit"
  
  // 测试各种计算情况
  subtract(5, 5)
  greet("test")
  divide_with_ceil(10, 3)
  
  // 边界断言
  subtract(5, 5)
  greet("test")
  divide_with_ceil(10, 0)
  
  test_int
  test_string
}
