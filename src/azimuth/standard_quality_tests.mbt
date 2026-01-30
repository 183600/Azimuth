// 标准 MoonBit 测试用例 - azimuth 库质量测试
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "mathematical_identity_laws" {
  // 测试数学恒等律
  let x = 42
  
  // 加法单位元：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "divide_with_ceil_comprehensive_coverage" {
  // 全面测试向上取整除法功能
  // 正数情况
  assert_eq(1, divide_with_ceil(5, 5))    // 整除
  assert_eq(2, divide_with_ceil(6, 5))    // 1.2 -> 2
  assert_eq(1, divide_with_ceil(1, 100))  // 小于除数
  
  // 负数情况
  assert_eq(-1, divide_with_ceil(-5, 5))   // -1
  assert_eq(-1, divide_with_ceil(-6, 5))   // -1.2 -> -1
  assert_eq(2, divide_with_ceil(-10, -5))  // 负负得正
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))     // 零除以正数
  assert_eq(0, divide_with_ceil(100, 0))   // 除数为零的安全处理
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello,   !", greet("  "))       // 空格字符串
  
  // 特殊字符和Unicode
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂算术组合
  let a = 10
  let b = 15
  let c = 20
  
  // 分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律验证
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
}

test "real_world_business_calculations" {
  // 真实业务场景计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(534, subtotal)
  assert_eq(43, tax)
  assert_eq(602, total_before_discount)
  assert_eq(552, final_total)
}

test "edge_case_numerical_operations" {
  // 边界值数值运算
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(10000, 100000))
  
  // 负数边界
  assert_eq(0, add(1000, -1000))
  assert_eq(-1000, add(-500, -500))
  assert_eq(1000000, multiply(-1000, -1000))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "algorithmic_pattern_simulation" {
  // 算法模式模拟
  // 模拟累加和：1 + 2 + 3 + 4 + 5
  let sum_1_to_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_1_to_5)
  
  // 模拟阶乘：5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial_5)
  
  // 模拟等差数列求和：首项3，公差2，前5项和
  let arithmetic_sum = add(add(add(add(3, 5), 7), 9), 11)
  assert_eq(35, arithmetic_sum)
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  // 多语言问候
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  
  // 特殊符号和表情
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 😀😃😄!", greet("😀😃😄"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "performance_efficiency_calculations" {
  // 性能效率计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 12
  
  // 优化后的操作数
  let optimized_ops = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  
  // 实际性能
  let actual_performance = add(optimized_ops, -overhead)
  
  // 性能提升倍数
  let improvement_factor = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(3000, optimized_ops)
  assert_eq(360, overhead)
  assert_eq(2640, actual_performance)
  assert_eq(3, improvement_factor)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个800）
  let small_projects_count = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(2, small_projects_count)
}