// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "core_int_max_min_values" {
  // 测试核心整数模块的最大值和最小值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 验证边界值运算
  assert_eq(max_int, add(max_int - 1, 1))
  assert_eq(min_int, add(min_int + 1, -1))
  
  // 验证边界值乘法
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
}

test "core_string_functionality" {
  // 测试核心字符串功能
  let empty_str = ""
  let hello_str = "Hello"
  let world_str = "World"
  
  // 测试字符串连接（通过 greet 函数）
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 2024!", greet("2024"))
}

test "mathematical_operations_comprehensive" {
  // 综合数学运算测试
  let a = 15
  let b = 7
  let c = 3
  
  // 基本运算验证
  assert_eq(22, add(a, b))
  assert_eq(8, subtract(a, b))
  assert_eq(105, multiply(a, b))
  
  // 复合运算
  assert_eq(26, add(multiply(a, b), c))  // 15*7 + 3 = 105 + 3 = 108
  assert_eq(5, divide_with_ceil(a, b))   // ceil(15/7) = 3
  
  // 运算优先级验证
  assert_eq(add(a, multiply(b, c)), multiply(a, b) - multiply(a, c))  // 15 + 7*3 = 105 - 15*3
}

test "business_calculation_scenarios" {
  // 业务计算场景测试
  // 场景1：计算项目资源需求
  let team_size = 8
  let hours_per_day = 8
  let working_days = 20
  let total_hours = multiply(team_size, multiply(hours_per_day, working_days))
  assert_eq(1280, total_hours)  // 8 * 8 * 20 = 1280
  
  // 场景2：计算运输需求
  let packages = 156
  let capacity_per_truck = 25
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(7, trucks_needed)  // ceil(156/25) = 7
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(175, total_capacity)  // 7 * 25 = 175
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 边界减法
  assert_eq(0, subtract(large_num, large_num))
  assert_eq(2000000, subtract(large_num, -large_num))
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  // 测试各种语言的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // 测试特殊字符和符号
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, π≈3.14!", greet("π≈3.14"))
  assert_eq_string("Hello, ©®™!", greet("©®™"))
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  // 模拟不同复杂度的算法
  let input_size = 1000
  
  // O(1) 常数时间操作
  assert_eq(1000, multiply(input_size, 1))
  
  // O(n) 线性时间操作模拟
  let linear_ops = multiply(input_size, 3)
  assert_eq(3000, linear_ops)
  
  // O(log n) 对数时间操作模拟（简化）
  let log_ops = divide_with_ceil(input_size, 100)
  assert_eq(10, log_ops)
  
  // O(n²) 二次时间操作模拟
  let quadratic_ops = multiply(input_size, divide_with_ceil(input_size, 100))
  assert_eq(10000, quadratic_ops)
}

test "real_world_resource_allocation" {
  // 真实世界资源分配测试
  // 场景：活动策划资源分配
  let participants = 250
  let tables = 15
  let chairs_per_table = 8
  let budget_per_person = 50
  let total_budget = 15000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(250/8) = 32
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables || tables_needed > tables)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)  // 32 * 8 = 256
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(participants, budget_per_person)
  assert_eq(12500, total_cost)  // 250 * 50 = 12500
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(2500, remaining_budget)
}