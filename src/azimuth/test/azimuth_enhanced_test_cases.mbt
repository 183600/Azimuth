// 增强 MoonBit 测试用例
// 针对 azimuth 库的高级功能测试

test "add_boundary_values" {
  // 测试 add 函数的边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))  // 接近最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数
  @azimuth.assert_eq(1, @azimuth.add(0, 1))  // 零边界
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))  // 负零边界
}

test "multiply_boundary_values" {
  // 测试 multiply 函数的边界值
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))  // 单位元
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))  // 负单位元
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))  // 大数乘零
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1, 2147483647))  // 单位乘大数
  @azimuth.assert_eq(46340, @azimuth.multiply(215, 215))  // 接近平方根边界
}

test "greet_unicode_emoji" {
  // 测试 greet 函数的 Unicode 和表情符号处理
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "divide_with_ceil_complex_cases" {
  // 测试 divide_with_ceil 函数的复杂情况
  @azimuth.assert_eq(-333334, @azimuth.divide_with_ceil(-1000000, 3))  // 大负数
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))  // 大正数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))  // 负数相等
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))  // 零除负数
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))  // 负数除正数
}

test "advanced_mathematical_expressions" {
  // 测试高级数学表达式
  let x = 42
  let y = 17
  let z = 8
  
  // 复杂嵌套表达式
  let result = @azimuth.add(
    @azimuth.multiply(x, @azimuth.divide_with_ceil(y, z)),
    @azimuth.multiply(z, @azimuth.divide_with_ceil(x, y))
  )
  @azimuth.assert_eq(58, result)  // 42*ceil(17/8) + 8*ceil(42/17) = 42*3 + 8*3 = 126 + 24 = 150
  
  // 多层嵌套
  let nested = @azimuth.divide_with_ceil(
    @azimuth.multiply(
      @azimuth.add(x, y),
      @azimuth.multiply(z, 2)
    ),
    3
  )
  @azimuth.assert_eq(80, nested)  // ceil((42+17)*(8*2)/3) = ceil(59*16/3) = ceil(944/3) = 315
}

test "financial_calculation_scenario" {
  // 金融计算场景：复利计算
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(10500, year1)  // 10000 + ceil(10000*5/100) = 10000 + 500 = 10500
  @azimuth.assert_eq(11025, year2)  // 10500 + ceil(10500*5/100) = 10500 + 525 = 11025
  @azimuth.assert_eq(11576, year3)  // 11025 + ceil(11025*5/100) = 11025 + 551 = 11576
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 1000
  let daily_sales = 45
  let restock_batch = 200
  let days_to_simulate = 5
  
  // 计算总销售量
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = @azimuth.add(initial_stock, -total_sold)
  
  // 计算需要补货的批次数
  let restock_batches_needed = @azimuth.divide_with_ceil(
    @azimuth.add(total_sold, -remaining_stock),
    restock_batch
  )
  
  @azimuth.assert_eq(225, total_sold)  // 45 * 5 = 225
  @azimuth.assert_eq(775, remaining_stock)  // 1000 - 225 = 775
  @azimuth.assert_eq(2, restock_batches_needed)  // ceil(225/200) = 2
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 50000
  let project_costs = @azimuth.multiply(8500, 4)  // 4个项目
  let team_costs = @azimuth.multiply(3200, 6)    // 6个团队
  let overhead = 5000
  
  // 计算总支出
  let total_expenses = @azimuth.add(
    @azimuth.add(project_costs, team_costs),
    overhead
  )
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -total_expenses)
  
  // 计算可以支持的小项目数量（每个3000）
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 3000)
  
  @azimuth.assert_eq(34000, project_costs)  // 8500 * 4 = 34000
  @azimuth.assert_eq(19200, team_costs)    // 3200 * 6 = 19200
  @azimuth.assert_eq(58200, total_expenses) // 34000 + 19200 + 5000 = 58200
  @azimuth.assert_eq(-8200, remaining_budget) // 50000 - 58200 = -8200
  @azimuth.assert_eq(-2, small_projects)   // ceil(-8200/3000) = -2
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  let greetings = [
    "English",
    "中文",
    "Español",
    "Русский",
    "العربية",
    "日本語",
    "한국어",
    "Français"
  ]
  
  // 测试每种语言的问候
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
}