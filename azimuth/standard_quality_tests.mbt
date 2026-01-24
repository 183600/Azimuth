// 标准 MoonBit 测试用例 - 高质量测试集合
// 为 azimuth 库添加10个新的标准测试用例

test "standard_quality_tests_verification" {
  // 验证测试文件被正确包含
  assert_eq(42, add(21, 21))
  assert_eq_string("Hello, Standard Quality Tests!", greet("Standard Quality Tests"))
}

test "add_function_comprehensive_coverage" {
  // 全面测试加法函数的各种情况
  assert_eq(100, add(50, 50))
  assert_eq(0, add(1000, -1000))
  assert_eq(-200, add(-100, -100))
  assert_eq(999999, add(500000, 499999))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_comprehensive_coverage" {
  // 全面测试乘法函数的各种情况
  assert_eq(10000, multiply(100, 100))
  assert_eq(-10000, multiply(-100, 100))
  assert_eq(10000, multiply(-100, -100))
  assert_eq(0, multiply(123456, 0))
  assert_eq(2147483647, multiply(46340, 46340))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度和准确性
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(500000, divide_with_ceil(999999, 2))
  assert_eq(1000000, divide_with_ceil(1999999, 2))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, Español!", greet("Español"))
}

test "complex_financial_calculations" {
  // 复杂金融计算场景测试
  let principal = 50000
  let annual_rate = 8
  let years = 5
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  let year4 = add(year3, divide_with_ceil(multiply(year3, annual_rate), 100))
  let year5 = add(year4, divide_with_ceil(multiply(year4, annual_rate), 100))
  
  assert_eq(54000, year1)  // 50000 + 8%
  assert_eq(58320, year2)  // 54000 + 8%
  assert_eq(62986, year3)  // 58320 + 8%
  assert_eq(68025, year4)  // 62986 + 8%
  assert_eq(73467, year5)  // 68025 + 8%
}

test "logistics_optimization_problem" {
  // 物流优化问题测试
  let total_packages = 1234
  let truck_capacity = 75
  let daily_trips = 3
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(17, trucks_needed)  // ceil(1234/75) = 17
  
  // 计算总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(1275, total_capacity)
  
  // 计算需要的运输天数
  let days_needed = divide_with_ceil(trucks_needed, daily_trips)
  assert_eq(6, days_needed)  // ceil(17/3) = 6
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  let start_value = 1
  let common_difference = 3
  let terms = 10
  
  // 计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let first_term = start_value
  let last_term = add(start_value, multiply(common_difference, subtract(terms, 1)))
  let sequence_sum = divide_with_ceil(multiply(terms, add(first_term, last_term)), 2)
  
  assert_eq(28, last_term)  // 1 + (10-1)*3 = 28
  assert_eq(145, sequence_sum)  // 10/2 * (2*1 + 9*3) = 5 * 29 = 145
}

test "resource_scheduling_algorithm" {
  // 资源调度算法测试
  let total_tasks = 256
  let parallel_workers = 8
  let tasks_per_worker = 32
  
  // 验证任务分配
  let total_capacity = multiply(parallel_workers, tasks_per_worker)
  assert_eq(256, total_capacity)
  
  // 计算需要的轮次
  let rounds_needed = divide_with_ceil(total_tasks, total_capacity)
  assert_eq(1, rounds_needed)
  
  // 模拟部分任务完成后的重新调度
  let completed_tasks = 100
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  let additional_rounds = divide_with_ceil(remaining_tasks, total_capacity)
  assert_eq(1, additional_rounds)
}

test "error_resilience_boundary_testing" {
  // 错误恢复和边界测试
  // 测试各种边界条件
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 测试极值运算
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
  assert_eq(1, divide_with_ceil(-2147483648, -2147483648))
}

test "string_security_validation" {
  // 字符串安全性验证测试
  // 测试各种特殊输入的安全性
  assert_eq_string("Hello, \0!", greet("\0"))
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  assert_eq_string("Hello, \"!'!", greet("\"!'"))
  assert_eq_string("Hello, <script>alert('xss')!</script>!", greet("<script>alert('xss')</script>"))
  assert_eq_string("Hello, DROP TABLE users;!", greet("DROP TABLE users;"))
  
  // 测试长字符串处理
  assert_eq_string("Hello, verylongstringwithoutspaces!", greet("verylongstringwithoutspaces"))
  assert_eq_string("Hello, a b c d e f g h i j k l m n o p q r s t u v w x y z!", greet("a b c d e f g h i j k l m n o p q r s t u v w x y z"))
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}