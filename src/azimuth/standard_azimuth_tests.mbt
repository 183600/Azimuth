// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法编写的高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_identity_elements" {
  // 测试乘法单位元：a * 1 = a, a * 0 = 0
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(999, 0))
  assert_eq(0, multiply(-123, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的各种情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(14, 2))  // 14/2 = 7 -> 7
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_multilingual_support" {
  // 测试多语言和特殊字符支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：订单总价计算
  let unit_price = 85
  let quantity = 6
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(510, subtotal)           // 85 * 6 = 510
  assert_eq(41, tax_amount)          // ceil(510 * 8 / 100) = ceil(40.8) = 41
  assert_eq(576, total_before_discount)  // 510 + 41 + 25 = 576
  assert_eq(526, final_total)        // 576 - 50 = 526
}

test "mathematical_laws_verification" {
  // 验证基本数学定律
  let a = 12
  let b = 7
  let c = 5
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  let total_tasks = 67
  let tasks_per_day = 8
  let work_days_per_week = 5
  let team_members = 3
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  // 计算每人平均工作小时数
  let hours_per_person = divide_with_ceil(total_hours, team_members)
  
  assert_eq(9, work_days_needed)    // ceil(67/8) = 9
  assert_eq(2, weeks_needed)        // ceil(9/5) = 2
  assert_eq(72, total_hours)        // 9 * 8 = 72
  assert_eq(24, hours_per_person)   // ceil(72/3) = 24
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, a!", greet("a"))                    // 单字符
  assert_eq_string("Hello, ABC!", greet("ABC"))                // 大写字母
  assert_eq_string("Hello, xyz!", greet("xyz"))                // 小写字母
  assert_eq_string("Hello, 12345!", greet("12345"))            // 纯数字
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))            // 包含运算符
  assert_eq_string("Hello, space separated!", greet("space separated"))  // 包含空格
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))          // 控制字符
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  let iterations = 5
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -overhead)
  
  // 计算迭代后的总性能
  let total_performance = multiply(actual_performance, iterations)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(12750, total_performance)
}

test "data_structure_manipulation" {
  // 数据结构操作测试（模拟数组/列表操作）
  let initial_size = 10
  let elements_to_add = 7
  let batch_size = 3
  
  // 计算添加元素后的总大小
  let final_size = add(initial_size, elements_to_add)
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(elements_to_add, batch_size)
  
  // 计算剩余空间（假设最大容量为25）
  let max_capacity = 25
  let remaining_space = add(max_capacity, -final_size)
  
  // 计算使用率百分比
  let usage_percentage = divide_with_ceil(multiply(final_size, 100), max_capacity)
  
  assert_eq(17, final_size)
  assert_eq(3, batches_needed)
  assert_eq(8, remaining_space)
  assert_eq(68, usage_percentage)
}
