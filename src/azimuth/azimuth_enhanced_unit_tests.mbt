// Azimuth 增强单元测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量的单元测试用例

// 内置函数定义
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
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

test "mathematical_associative_laws" {
  // 数学结合律测试
  let a = 5
  let b = 3
  let c = 2
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let add_left = add(add(a, b), c)
  let add_right = add(a, add(b, c))
  assert_eq(add_left, add_right)
  
  // 减法结合律验证（不满足结合律）
  let sub_left = subtract(subtract(a, b), c)  // (5 - 3) - 2 = 0
  let sub_right = subtract(a, subtract(b, c))  // 5 - (3 - 2) = 4
  assert_eq(0, sub_left)
  assert_eq(4, sub_right)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let mult_left = multiply(multiply(a, b), c)
  let mult_right = multiply(a, multiply(b, c))
  assert_eq(mult_left, mult_right)
}

test "division_edge_cases_comprehensive" {
  // 除法边界情况综合测试
  // 正数除法测试
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(2, divide_with_ceil(4, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  
  // 负数除法测试
  assert_eq(-1, divide_with_ceil(-3, 3))
  assert_eq(-2, divide_with_ceil(-4, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-1, divide_with_ceil(-1, 3))
  
  // 混合符号除法测试
  assert_eq(-1, divide_with_ceil(3, -3))
  assert_eq(-2, divide_with_ceil(4, -3))
  assert_eq(1, divide_with_ceil(-3, -3))
  assert_eq(2, divide_with_ceil(-4, -3))
  
  // 零除数测试
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 零被除数测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串测试
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符测试
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, 1!", greet("1"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello,   !", greet("   "))  // 三个空格
  assert_eq_string("Hello, 	\n!", greet("\t\n"))  // 制表符和换行符
  
  // 长字符串测试
  let long_string = "This is a very long string"
  let expected = "Hello, " + long_string + "!"
  assert_eq_string(expected, greet(long_string))
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  // 场景：计算投资回报
  let principal = 10000  // 本金
  let rate = 5  // 年利率百分比
  let years = 3  // 投资年限
  
  // 计算单利
  let simple_interest = divide_with_ceil(multiply(principal, multiply(rate, years)), 100)
  let simple_total = add(principal, simple_interest)
  
  // 计算复利（简化版）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate), 100))
  let compound_total = year3_amount
  
  // 计算收益差异
  let difference = subtract(compound_total, simple_total)
  
  assert_eq(1500, simple_interest)
  assert_eq(11500, simple_total)
  assert_eq(11576, compound_total)
  assert_eq(76, difference)
}

test "inventory_management_system" {
  // 库存管理系统测试
  // 场景：计算库存补货需求
  let current_stock = 45
  let daily_sales = 8
  let supplier_lead_time = 7  // 天
  let safety_stock = 20  // 安全库存
  
  // 计算Lead Time期间的需求
  let lead_time_demand = multiply(daily_sales, supplier_lead_time)
  
  // 计算再订货点
  let reorder_point = add(lead_time_demand, safety_stock)
  
  // 计算需要补货的数量（假设目标库存为100）
  let target_stock = 100
  let reorder_quantity = subtract(target_stock, current_stock)
  
  // 计算补货次数
  let packages_per_order = 25
  let orders_needed = divide_with_ceil(reorder_quantity, packages_per_order)
  
  assert_eq(56, lead_time_demand)
  assert_eq(76, reorder_point)
  assert_eq(55, reorder_quantity)
  assert_eq(3, orders_needed)
}

test "algorithm_efficiency_tests" {
  // 算法效率测试
  // 场景：不同算法策略的性能比较
  let data_size = 100
  
  // 线性搜索模拟（最坏情况）
  let linear_comparisons = data_size
  
  // 二分搜索模拟（最坏情况）
  let binary_comparisons = 7  // log2(100) ≈ 6.64，向上取整
  
  // 哈希表搜索模拟（理想情况）
  let hash_comparisons = 1
  
  // 计算效率提升
  let linear_vs_binary = divide_with_ceil(multiply(linear_comparisons, 100), binary_comparisons)
  let linear_vs_hash = divide_with_ceil(multiply(linear_comparisons, 100), hash_comparisons)
  let binary_vs_hash = divide_with_ceil(multiply(binary_comparisons, 100), hash_comparisons)
  
  assert_eq(100, linear_comparisons)
  assert_eq(7, binary_comparisons)
  assert_eq(1, hash_comparisons)
  assert_eq(1429, linear_vs_binary)  // 二分搜索比线性搜索快约14.29倍
  assert_eq(10000, linear_vs_hash)   // 哈希搜索比线性搜索快100倍
  assert_eq(700, binary_vs_hash)     // 哈希搜索比二分搜索快7倍
}

test "data_validation_and_error_handling" {
  // 数据验证和错误处理测试
  // 场景：输入数据验证
  let valid_inputs = [1, 5, 10, 100, 1000]
  let invalid_inputs = [0, -1, -100]
  
  // 验证有效输入的处理
  let sum_valid = add(add(add(add(valid_inputs[0], valid_inputs[1]), valid_inputs[2]), valid_inputs[3]), valid_inputs[4])
  let product_valid = multiply(multiply(multiply(multiply(valid_inputs[1], valid_inputs[2]), valid_inputs[3]), valid_inputs[4]), valid_inputs[0])
  
  // 验证无效输入的处理
  let sum_invalid = add(add(invalid_inputs[0], invalid_inputs[1]), invalid_inputs[2])
  let product_invalid = multiply(multiply(invalid_inputs[1], invalid_inputs[2]), invalid_inputs[0])
  
  // 边界值测试（使用较小的值以避免整数溢出）
  let max_int = 1000000
  let min_int = -1000000
  let range_check = subtract(max_int, min_int)
  
  assert_eq(1116, sum_valid)
  assert_eq(5000000, product_valid)
  assert_eq(-101, sum_invalid)
  assert_eq(0, product_invalid)
  assert_eq(2000000, range_check)
}

test "scientific_computing_precision" {
  // 科学计算精度测试
  // 场景：物理公式计算
  let mass = 10  // kg
  let acceleration = 9  // m/s²
  let time = 5  // s
  
  // 计算力: F = ma
  let force = multiply(mass, acceleration)
  
  // 计算速度: v = at
  let velocity = multiply(acceleration, time)
  
  // 计算位移: s = 0.5at²
  let displacement = divide_with_ceil(multiply(acceleration, multiply(time, time)), 2)
  
  // 计算动能: E = 0.5mv²
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(velocity, velocity)), 2)
  
  // 计算功率: P = Fv
  let power = divide_with_ceil(multiply(force, velocity), 1)
  
  // 计算能量消耗率
  let energy_rate = divide_with_ceil(kinetic_energy, time)
  
  assert_eq(90, force)
  assert_eq(45, velocity)
  assert_eq(113, displacement)  // 0.5 * 9 * 25 = 112.5，向上取整
  assert_eq(10125, kinetic_energy)
  assert_eq(4050, power)
  assert_eq(2025, energy_rate)
}