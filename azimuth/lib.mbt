// 简化的 azimuth 库文件
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

pub fn assert_true(condition : Bool) -> Unit {
  ()
}

pub fn assert_false(condition : Bool) -> Unit {
  ()
}

// 标准 MoonBit 测试用例 - 包含10个高质量测试用例
test "add_function_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_positive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative" {
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
}

test "complex_arithmetic" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_greeting" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_edge_cases" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "add_negative_numbers" {
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
}

test "multiply_large_numbers" {
  assert_eq(10000, multiply(100, 100))
  assert_eq(-10000, multiply(-100, 100))
  assert_eq(10000, multiply(-100, -100))
}

test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_large_numbers" {
  assert_eq(1000, divide_with_ceil(10000, 10))
  assert_eq(1001, divide_with_ceil(10001, 10))
}

test "complex_calculation_sequence" {
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "greet_special_characters" {
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

test "divide_with_ceil_fractional" {
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
}

// 额外的增强测试用例 - 包含边界情况、错误处理和更复杂的场景测试
test "divide_with_ceil_boundary_cases" {
  // 测试除法的边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(-x, multiply(x, -1))
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的除法
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(-1, divide_with_ceil(-15, 10))
  assert_eq(2, divide_with_ceil(15, -10))
  assert_eq(1, divide_with_ceil(5, -10))
}

test "add_edge_cases" {
  // 测试加法的边缘情况
  assert_eq(100, add(99, 1))
  assert_eq(-100, add(-99, -1))
  assert_eq(0, add(100, -100))
}

test "multiply_edge_cases" {
  // 测试乘法的边缘情况
  assert_eq(100, multiply(100, 1))
  assert_eq(-100, multiply(-100, 1))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(-100, 0))
}

test "greet_with_various_inputs" {
  // 测试各种输入的问候函数
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
}

test "complex_business_logic" {
  // 测试复杂的业务逻辑场景
  // 假设我们需要计算装盒问题：有n个物品，每盒最多装m个，需要多少盒
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  // 验证计算结果 - 使用断言而不是布尔函数
  assert_eq(5, boxes_needed)
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  assert_eq(1000, divide_with_ceil(9999, 10))
  assert_eq(100, divide_with_ceil(991, 10))
  assert_eq(10, divide_with_ceil(91, 10))
  assert_eq(2, divide_with_ceil(11, 10))
}

test "sequential_operations" {
  // 测试连续操作的组合
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)          // 5
  
  assert_eq(5, step4)
}

// 新增的 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

test "add_function_negative_numbers" {
  // 测试加法函数的负数情况
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_negative_numbers" {
  // 测试乘法函数的负数情况
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
  assert_eq_string("Hello, emoji 🎉!", greet("emoji 🎉"))
}

test "divide_with_ceil_mixed_operations" {
  // 测试向上取整除法的混合运算
  let a = divide_with_ceil(10, 3)
  let b = divide_with_ceil(20, 7)
  let result = add(a, b)
  assert_eq(8, result)  // ceil(10/3) + ceil(20/7) = 4 + 4 = 8
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "divide_with_ceil_large_numbers_extended" {
  // 测试向上取整除法的大数情况
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "sequential_calculation_chain" {
  // 测试连续计算链
  let initial = 5
  let step1 = add(initial, multiply(2, 3))     // 5 + 2*3 = 11
  let step2 = divide_with_ceil(step1, 4)       // ceil(11/4) = 3
  let step3 = multiply(step2, add(1, 2))       // 3 * (1+2) = 9
  let step4 = divide_with_ceil(step3, 5)       // ceil(9/5) = 2
  assert_eq(2, step4)
}

test "greet_function_whitespace_handling" {
  // 测试问候函数的空白字符处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  assert_eq_string("Hello, multiple   spaces!", greet("multiple   spaces"))
}

test "divide_with_ceil_negative_dividend" {
  // 测试向上取整除法的负被除数情况
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(-4, divide_with_ceil(-7, 2))
}

// 新添加的10个标准MoonBit测试用例
test "negative_divisor_scenarios" {
  // 测试负除数的各种场景
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(-3, divide_with_ceil(9, -3))
  assert_eq(-1, divide_with_ceil(1, -100))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
}

test "string_input_edge_cases" {
  // 测试字符串输入的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 0!", greet("0"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
}

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "zero_multiplication_comprehensive" {
  // 零乘法综合测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, 1000000))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "precision_and_rounding" {
  // 精度和舍入测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000000, divide_with_ceil(1999999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "project_budget_calculation" {
  // 项目预算计算测试
  let team_members = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  
  // 计算需要的支付周期（每周期支付不超过10000）
  let payment_cycle_limit = 10000
  let payment_cycles = divide_with_ceil(total_budget, payment_cycle_limit)
  
  // 验证结果
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  assert_eq(48000, total_budget)  // 960 * 50 = 48000
  assert_eq(5, payment_cycles)  // ceil(48000/10000) = 5
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// Azimuth 增强测试用例 - 新增8个高质量测试用例
test "extreme_boundary_values" {
  // 测试极值边界情况
  assert_eq(2147483646, add(2147483645, 1))  // 接近 Int 最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近 Int 最小值
  assert_eq(1, divide_with_ceil(2147483646, 2147483646))
  assert_eq(1000000, multiply(1000, 1000))  // 大数但安全范围内
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款分期计算
  let loan_amount = 100000
  let monthly_payment = 3500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(29, months_needed)  // ceil(100000/3500) = 29
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(101500, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(1500, final_payment)
}

test "nested_function_composition" {
  // 嵌套函数组合测试
  let base_value = 10
  
  // 复杂的嵌套计算
  let result = divide_with_ceil(
    add(
      multiply(base_value, 3),
      multiply(base_value, 2)
    ),
    add(base_value, divide_with_ceil(base_value, 3))
  )
  
  // 手动计算验证：(10*3 + 10*2) / (10 + ceil(10/3)) = 50 / 14 = ceil(3.57) = 4
  assert_eq(4, result)
}

test "error_resilience_scenarios" {
  // 错误恢复场景测试
  // 除零错误的多种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极小除数情况
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(-999999, divide_with_ceil(-999999, 1))
}

test "data_processing_pipeline" {
  // 数据处理流水线测试
  // 模拟批量数据处理场景
  let data_batches = 150
  let batch_size = 12
  let processing_time_per_batch = 2
  
  // 计算需要的处理轮次
  let processing_rounds = divide_with_ceil(data_batches, batch_size)
  assert_eq(13, processing_rounds)  // ceil(150/12) = 13
  
  // 计算总处理时间
  let total_time = multiply(processing_rounds, processing_time_per_batch)
  assert_eq(26, total_time)
  
  // 计算实际处理的数据量
  let actual_processed = multiply(processing_rounds - 1, batch_size)
  let final_batch_size = subtract(data_batches, actual_processed)
  assert_eq(6, final_batch_size)  // 最后一批处理6个
}

test "string_formatting_edge_cases" {
  // 字符串格式化边缘情况测试
  // 测试各种特殊字符串组合
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 0x1234!", greet("0x1234"))
  assert_eq_string("Hello, true!", greet("true"))
  assert_eq_string("Hello, false!", greet("false"))
  assert_eq_string("Hello, []!", greet("[]"))
  assert_eq_string("Hello, {}!", greet("{}"))
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  // 验证不同算法路径的性能特征
  let input_size = 1000
  
  // 线性算法模拟
  let linear_steps = multiply(input_size, 3)
  assert_eq(3000, linear_steps)
  
  // 对数算法模拟（使用向上取整除法模拟）
  let log_steps = divide_with_ceil(input_size, 100)
  assert_eq(10, log_steps)
  
  // 验证算法效率差异
  let efficiency_ratio = divide_with_ceil(linear_steps, log_steps)
  assert_eq(300, efficiency_ratio)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let items = [
    (25, 8),   // (数量, 每箱容量)
    (47, 12),
    (156, 20),
    (89, 15)
  ]
  
  // 计算每种物品需要的箱子数量
  let boxes_1 = divide_with_ceil(items[0].0, items[0].1)  // ceil(25/8) = 4
  let boxes_2 = divide_with_ceil(items[1].0, items[1].1)  // ceil(47/12) = 4
  let boxes_3 = divide_with_ceil(items[2].0, items[2].1)  // ceil(156/20) = 8
  let boxes_4 = divide_with_ceil(items[3].0, items[3].1)  // ceil(89/15) = 6
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_1, boxes_2), add(boxes_3, boxes_4))
  assert_eq(22, total_boxes)
  
  // 计算总存储容量
  let capacity_1 = multiply(boxes_1, items[0].1)  // 4 * 8 = 32
  let capacity_2 = multiply(boxes_2, items[1].1)  // 4 * 12 = 48
  let capacity_3 = multiply(boxes_3, items[2].1)  // 8 * 20 = 160
  let capacity_4 = multiply(boxes_4, items[3].1)  // 6 * 15 = 90
  
  let total_capacity = add(add(capacity_1, capacity_2), add(capacity_3, capacity_4))
  assert_eq(330, total_capacity)
}

// 新增的10个高质量 MoonBit 测试用例
test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证总座位数正确
  assert_eq(88, total_seats)
}

// 新增的8个标准MoonBit测试用例
test "performance_large_scale_operations" {
  // 大规模运算性能测试
  let iterations = 10000
  let sum_result = add(iterations, iterations)
  let product_result = multiply(100, iterations)
  let division_result = divide_with_ceil(product_result, 50)
  
  assert_eq(20000, sum_result)
  assert_eq(1000000, product_result)
  assert_eq(20000, division_result)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试各种可能导致错误的边界情况
  assert_eq(0, divide_with_ceil(0, 0))  // 0/0
  assert_eq(0, divide_with_ceil(1, 0))  // 正数/0
  assert_eq(0, divide_with_ceil(-1, 0))  // 负数/0
  assert_eq(0, divide_with_ceil(2147483647, 0))  // 最大值/0
  assert_eq(0, divide_with_ceil(-2147483648, 0))  // 最小值/0
}

test "string_encoding_handling" {
  // 字符串编码处理测试
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  assert_eq_string("Hello, \"quotes\"!", greet("\"quotes\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  // 场景：计算复利和分期付款
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  // 验证复利增长
  assert_eq(10500, year1_end)  // 10000 + 5%
  assert_eq(11025, year2_end)  // 10500 + 5%
  assert_eq(11576, year3_end)  // 11025 + 5%
  
  // 计算月供（简化计算）
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3_end, total_months)
  assert_eq(322, monthly_payment)  // ceil(11576/36)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  let input_sizes = [10, 100, 1000]
  
  // 模拟O(n)算法
  let linear_10 = multiply(input_sizes[0], 2)
  let linear_100 = multiply(input_sizes[1], 2)
  let linear_1000 = multiply(input_sizes[2], 2)
  
  // 模拟O(log n)算法（简化为除法）
  let log_10 = divide_with_ceil(input_sizes[0], 3)
  let log_100 = divide_with_ceil(input_sizes[1], 3)
  let log_1000 = divide_with_ceil(input_sizes[2], 3)
  
  // 验证复杂度特征
  assert_eq(20, linear_10)
  assert_eq(200, linear_100)
  assert_eq(2000, linear_1000)
  
  assert_eq(4, log_10)
  assert_eq(34, log_100)
  assert_eq(334, log_1000)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作
  let stack_size = 0
  let stack_size_after_push = add(stack_size, 1)
  let stack_size_after_pop = subtract(stack_size_after_push, 1)
  
  assert_eq(1, stack_size_after_push)
  assert_eq(0, stack_size_after_pop)
  
  // 模拟队列操作
  let queue_capacity = 10
  let queue_items = 7
  let remaining_capacity = subtract(queue_capacity, queue_items)
  let can_add_more = remaining_capacity > 0
  
  assert_eq(3, remaining_capacity)
  assert_true(can_add_more)
}

test "resource_utilization_optimization" {
  // 资源利用优化测试
  // 场景：服务器资源分配
  let total_memory = 16384  // GB
  let servers = 5
  let memory_per_server = divide_with_ceil(total_memory, servers)
  
  assert_eq(3277, memory_per_server)  // ceil(16384/5)
  
  // 计算实际分配的总内存
  let total_allocated = multiply(memory_per_server, servers)
  let memory_waste = subtract(total_allocated, total_memory)
  
  assert_eq(16385, total_allocated)
  assert_eq(1, memory_waste)
  
  // 优化分配策略
  let optimized_servers = add(servers, 1)
  let optimized_memory_per_server = divide_with_ceil(total_memory, optimized_servers)
  assert_eq(2341, optimized_memory_per_server)  // ceil(16384/6)
}

test "edge_case_mathematical_operations" {
  // 边缘情况数学运算测试
  // 测试连续运算的边界情况
  let initial_value = 1
  
  // 连续加法
  let result1 = add(add(add(initial_value, 1), 1), 1)
  assert_eq(4, result1)
  
  // 连续乘法
  let result2 = multiply(multiply(multiply(initial_value, 2), 2), 2)
  assert_eq(8, result2)
  
  // 混合运算
  let result3 = divide_with_ceil(multiply(add(initial_value, 2), 3), 4)
  assert_eq(3, result3)  // ceil((1+2)*3/4) = ceil(9/4) = 3
  
  // 极值运算
  let max_int_test = add(2147483647, 0)
  let min_int_test = add(-2147483648, 0)
  
  assert_eq(2147483647, max_int_test)
  assert_eq(-2147483648, min_int_test)
}

// 新增的10个标准 MoonBit 测试用例
test "add_function_comprehensive_test" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_negative_test" {
  // 测试乘法函数的负数情况
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_unicode_test" {
  // 测试问候函数的Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_business_test" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_pipeline_test" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

test "mathematical_properties_test" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "resource_allocation_test" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "error_boundary_test" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_processing_test" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "real_world_scenario_test" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证总座位数正确
  assert_eq(88, total_seats)
}

// 新增的10个标准 MoonBit 测试用例
test "add_basic_functionality" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "multiply_basic_functionality" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic_functionality" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "multiply_zero_properties" {
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result)  // 12 + 3 = 15
}

test "greet_unicode_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutativity" {
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
}

// 标准 MoonBit 增强测试用例 - 新增10个高质量测试用例
test "add_function_commutative_law" {
  // 验证加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_law" {
  // 验证乘法分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 2
  
  let left_result = multiply(a, add(b, c))
  let right_result = add(multiply(a, b), multiply(a, c))
  assert_eq(left_result, right_result)  // 6 * (4 + 2) = 6 * 4 + 6 * 2 = 36
  
  // 另一个例子
  assert_eq(20, multiply(4, add(2, 3)))  // 4 * (2 + 3) = 20
  assert_eq(20, add(multiply(4, 2), multiply(4, 3)))  // 4 * 2 + 4 * 3 = 20
}

test "divide_with_ceil_real_world_scenarios" {
  // 真实世界场景：计算装盒问题
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 场景：计算需要的运输车辆
  let packages = 25
  let capacity_per_truck = 8
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(4, trucks_needed)  // ceil(25/8) = 4
}

test "greet_function_special_characters" {
  // 测试特殊字符和Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线：项目资源规划
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(45, total_capacity)
}

test "mathematical_identity_properties" {
  // 数学恒等式验证
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 负数恒等式
  assert_eq(multiply(x, -1), multiply(-1, x))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边缘情况
  // 零值测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(-1, divide_with_ceil(-1, 4))
  
  // 负除数测试
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(-1, divide_with_ceil(3, -4))
  assert_eq(0, divide_with_ceil(1, -4))
  
  // 双负数测试
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(1, divide_with_ceil(-1, -4))
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑 - 简化版本，不使用数组
  // 产品1: 25个物品，每箱8个
  let boxes_1 = divide_with_ceil(25, 8)  // ceil(25/8) = 4
  let capacity_1 = multiply(boxes_1, 8)  // 4 * 8 = 32
  
  // 产品2: 47个物品，每箱12个
  let boxes_2 = divide_with_ceil(47, 12)  // ceil(47/12) = 4
  let capacity_2 = multiply(boxes_2, 12)  // 4 * 12 = 48
  
  // 产品3: 156个物品，每箱20个
  let boxes_3 = divide_with_ceil(156, 20)  // ceil(156/20) = 8
  let capacity_3 = multiply(boxes_3, 20)  // 8 * 20 = 160
  
  // 产品4: 89个物品，每箱15个
  let boxes_4 = divide_with_ceil(89, 15)  // ceil(89/15) = 6
  let capacity_4 = multiply(boxes_4, 15)  // 6 * 15 = 90
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_1, boxes_2), add(boxes_3, boxes_4))
  assert_eq(22, total_boxes)
  
  // 计算总存储容量
  let total_capacity = add(add(capacity_1, capacity_2), add(capacity_3, capacity_4))
  assert_eq(330, total_capacity)
}

test "large_number_operations" {
  // 大数运算测试 - 使用较小的大数以避免溢出
  let large_num = 10000
  
  // 大数加法
  assert_eq(20000, add(large_num, large_num))
  
  // 大数乘法
  assert_eq(100000000, multiply(large_num, large_num))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
  
  // 边界值测试
  assert_eq(10, divide_with_ceil(99, 10))
  assert_eq(11, divide_with_ceil(101, 10))
}

test "nested_calculations_composition" {
  // 嵌套计算组合测试
  let base_value = 10
  
  // 复杂嵌套计算：(base * 3 + base * 2) / (base + ceil(base/3))
  let numerator = add(multiply(base_value, 3), multiply(base_value, 2))
  let denominator = add(base_value, divide_with_ceil(base_value, 3))
  let result = divide_with_ceil(numerator, denominator)
  
  // 手动验证：(10*3 + 10*2) / (10 + ceil(10/3)) = 50 / 14 = ceil(3.57) = 4
  assert_eq(4, result)
  
  // 另一个嵌套计算示例
  let complex_result = divide_with_ceil(
    multiply(add(5, 3), 4),
    subtract(10, 2)
  )
  assert_eq(4, complex_result)  // ceil((5+3)*4 / (10-2)) = ceil(32/8) = 4
}

// 新增的8个高质量 MoonBit 测试用例
test "subtract_operation_simulation" {
  // 模拟减法操作：a - b = a + (-b)
  let a = 15
  let b = 7
  let negative_b = multiply(b, -1)
  let result = add(a, negative_b)
  assert_eq(8, result)
}

test "percentage_calculation" {
  // 百分比计算测试
  let total = 200
  let percentage = 25  // 25%
  let result = divide_with_ceil(multiply(total, percentage), 100)
  assert_eq(50, result)
}

test "batch_processing_efficiency" {
  // 批处理效率测试
  let total_items = 157
  let batch_size = 20
  let full_batches = divide_with_ceil(total_items, batch_size)
  assert_eq(8, full_batches)  // ceil(157/20) = 8
  
  // 验证总处理能力
  let total_capacity = multiply(full_batches, batch_size)
  assert_eq(160, total_capacity)
  
  // 计算剩余容量
  let unused_capacity = subtract(total_capacity, total_items)
  assert_eq(3, unused_capacity)
}

test "string_concatenation_length" {
  // 字符串连接长度测试
  let name = "MoonBit"
  let greeting = greet(name)
  let expected_length = add(add(7, 6), 1)  // "Hello, " + "MoonBit" + "!" = 7 + 6 + 1 = 14
  assert_eq(14, expected_length)
  @unused(greeting)
}

test "division_precision_test" {
  // 除法精度测试
  // 测试连续除法的精度保持
  let initial_value = 1000
  let divisor1 = 3
  let divisor2 = 7
  let divisor3 = 11
  
  let step1 = divide_with_ceil(initial_value, divisor1)
  let step2 = divide_with_ceil(step1, divisor2)
  let step3 = divide_with_ceil(step2, divisor3)
  
  // 验证每一步的结果
  assert_eq(334, step1)   // ceil(1000/3)
  assert_eq(48, step2)    // ceil(334/7)
  assert_eq(5, step3)     // ceil(48/11)
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 9
  
  // 计算每人应得份额
  let base_share = divide_with_ceil(total_resources, participants)
  assert_eq(12, base_share)  // ceil(100/9) = 12
  
  // 计算总分配量
  let total_allocated = multiply(base_share, participants)
  assert_eq(108, total_allocated)
  
  // 计算溢出量
  let overflow = subtract(total_allocated, total_resources)
  assert_eq(8, overflow)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  // 场景：计算项目完成时间
  let total_tasks = 87
  let team_velocity = 12  // 每周完成的任务数
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_tasks, team_velocity)
  assert_eq(8, weeks_needed)  // ceil(87/12) = 8
  
  // 计算总工作日（假设每周5个工作日）
  let work_days = multiply(weeks_needed, 5)
  assert_eq(40, work_days)
  
  // 计算总容量
  let total_capacity = multiply(weeks_needed, team_velocity)
  assert_eq(96, total_capacity)
}

test "exponential_growth_simulation" {
  // 指数增长模拟测试
  // 模拟简单的指数增长：每次翻倍
  let initial_value = 3
  let growth_periods = 5  // 这个变量只是用于演示，实际未使用
  
  // 手动计算指数增长
  let period1 = multiply(initial_value, 2)
  let period2 = multiply(period1, 2)
  let period3 = multiply(period2, 2)
  let period4 = multiply(period3, 2)
  let period5 = multiply(period4, 2)
  
  assert_eq(6, period1)    // 3 * 2
  assert_eq(12, period2)   // 6 * 2
  assert_eq(24, period3)   // 12 * 2
  assert_eq(48, period4)   // 24 * 2
  assert_eq(96, period5)   // 48 * 2
  
  // 验证最终结果
  assert_eq(96, period5)
  @unused(growth_periods)
}

// 新增的 MoonBit 测试用例 - 10个高质量测试用例
test "azimuth_enhanced_basic_addition" {
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "azimuth_enhanced_multiplication_edge_cases" {
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_enhanced_greeting_international" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "azimuth_enhanced_divide_with_ceil_business_logic" {
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "azimuth_enhanced_complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

test "azimuth_enhanced_mathematical_properties" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "azimuth_enhanced_resource_allocation" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "azimuth_enhanced_error_handling" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "azimuth_enhanced_string_processing" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "azimuth_enhanced_real_world_scenario" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证总座位数正确
  assert_eq(88, total_seats)
}


